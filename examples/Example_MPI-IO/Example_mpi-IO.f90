!
!     Copyright (C) 2016  Adam Jirasek
! 
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU Lesser General Public License as published by
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
! 
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU Lesser General Public License for more details.
! 
!     You should have received a copy of the GNU Lesser General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.
!     
!     contact: libm3l@gmail.com
! 
!

!
!     Sample program
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: test of MPI I-O operations
!
!
!     Input parameters:
! 
!
!     Return value:
! 
! 
!
!     Modifications:
!     Date		Version		Patch number		CLA 
!
!
!     Description
!
!
PROGRAM  EXAMPLE_MPI_IO

    USE MPI
    USE FLL_MODS_M
    USE FLL_MPI_MODS_M
    USE READ_INPUT_M
    USE CREATE_DATA_SET_M

    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   TYPE(DNODE), POINTER  :: FLL_MPI_STRUCT, PDATA_SET, PNEW,PTMP,PMPI
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IERR,WORLD_RANK,world_group_id,NPROC,ISTAT

  INTEGER(LINT) :: NFILES,BYTESN,NSIZE,I
  integer :: EVEN_COMM_ID,EVEN_P,EVEN_GROUP_ID,ODD_GROUP_ID,ODD_COMM_ID
  CHARACTER(LEN=FILE_NAME_LENGTH) :: NAME_OF_FILE
  LOGICAL :: OK

  INTEGER, ALLOCATABLE :: EVEN_RANK(:),ODD_RANK(:)
  INTEGER :: E_RANK,O_RANK,COMM
  REAL :: START, FINISH

  TYPE(DNODE), POINTER ::PIOSTR,PSUBPROC,PIO
!
!  Initialize MPI
!
   CALL MPI_INIT(IERR)   
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, WORLD_RANK, IERR )
   CALL  MPI_Comm_size ( MPI_COMM_WORLD, NPROC, ierr )

   FLL_MPI_STRUCT => NULL()
!
! initiate MPI structure with all info
!
   PMPI => FLL_MPI_PROC_STRUCT(FPAR)
!
!  define how to save files for N-M saving model
!
   CALL  FLL_IO_STRUCT(PMPI,'ada','bmpi',2_LINT, FPAR)
!
!  print the strucute on the screen and save into ASCII file
!
   IF(WORLD_RANK == 0)THEN
    CALL FLL_CAT(PMPI,6,.FALSE., FPAR)
    IF(.NOT.FLL_WRITE(PMPI,"io.str", 9, 'A', FPAR))STOP'Error writing file'
   END IF
!
!   create sample data se
!
   NSIZE = 1000000 !+ 100*WORLD_RANK

   CALL CREATE_DATA_SET(PDATA_SET,NSIZE, WORLD_RANK)
   BYTESN = FLL_GETNBYTES(PDATA_SET,FPAR)
   WRITE(*,*)' Partition created data set size of ', WORLD_RANK,BYTESN
!
!  save to one file, all partitions at the same time
!
   CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
   IF(WORLD_RANK == 0)CALL CPU_TIME(START)
   
   OK = FLL_MPI_WRITE(PDATA_SET,'PartitionedFile',10,0, world_rank, MPI_COMM_WORLD, 'A', FPAR)
   
   IF(WORLD_RANK == 0)THEN 
     CALL CPU_TIME(FINISH)
     WRITE(*,*)' SAVING TIME IS ', FINISH-START
   END IF
   CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
!
!  save to several separate files
!  all partitions at the same time
!
   IF(WORLD_RANK == 0)CALL CPU_TIME(START)

   OK = FLL_MPI_WRITE_NM(PDATA_SET,PMPI,FPAR)

   IF(WORLD_RANK == 0)THEN 
     CALL CPU_TIME(FINISH)
     WRITE(*,*)' SAVING TIME IS ', FINISH-START
   END IF
   CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

   IF(WORLD_RANK == 0) THEN 
     CALL READ_INPUT(NAME_OF_FILE,NFILES,1_LINT*NPROC)
   END IF

   CALL FLL_RM(PMPI,FPAR)
   PMPI => FLL_MPI_PROC_STRUCT(FPAR)
   CALL  FLL_IO_STRUCT(PMPI,'beda','bmpi',4_LINT, FPAR)

   IF(WORLD_RANK == 0)CALL CPU_TIME(START)

   OK = FLL_MPI_WRITE_NM(PDATA_SET,PMPI,FPAR)

   IF(WORLD_RANK == 0)THEN 
     CALL CPU_TIME(FINISH)
     WRITE(*,*)' SAVING TIME IS ', FINISH-START
   END IF
   CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

!
!  Copy FLL_MPI_STRUCT date set which now exists on root partition onlyc
!  to all other partitions
!  Upon return, the function will return pointer to newly allocated data
!  for all other partition then root partition
!  On root partition, the PNEW pointer is pointing on FLL_MPI_STRUCT
!
!   PNEW => FLL_MPI_CP_ALL(FLL_MPI_STRUCT,MPI_COMM_WORLD,0,FPAR)
!   IF(WORLD_RANK /= 0)THEN
!
!  make FLL_MPI_STRUCT point to PNEW so that we cane use the same names for all partitions
!  
!     FLL_MPI_STRUCT => PNEW
!   END IF
  
!   PTMP => FLL_MPI_READ('PartitionedFile',10,0, world_rank, MPI_COMM_WORLD, 'A', FPAR)
!   BYTESN = FLL_GETNBYTES(PTMP,FPAR)
!   WRITE(*,*)' Partition all-reads data set size of ', WORLD_RANK,BYTESN
!   CALL FLL_RM(PTMP,FPAR)
!
!  CLEAN MEMORY
!
   IF(WORLD_RANK==0)write(*,*)' Releasing memory'
   CALL FLL_RM(FLL_MPI_STRUCT,FPAR)
   CALL FLL_RM(PDATA_SET,FPAR)

  
   CALL MPI_FINALIZE(IERR)
  
END PROGRAM

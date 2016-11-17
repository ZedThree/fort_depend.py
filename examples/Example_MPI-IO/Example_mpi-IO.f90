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
    USE CREATE_MPI_STRUCT_M
    USE READ_INPUT_M
    USE CREATE_DATA_SET_M

    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE
   TYPE(DNODE), POINTER  :: PNODE, FLL_MPI_STRUCT, PDATA_SET, PNEW,PTMP,PMPI
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT,I,IERR,WORLD_RANK,world_group_id,NPROC,ISTAT
   CHARACTER :: FMT

  INTEGER(LINT) :: NFILES,BYTES,BYTESN,NSIZE
  integer :: EVEN_COMM_ID,EVEN_P,EVEN_GROUP_ID
  CHARACTER(LEN=FILE_NAME_LENGTH) :: NAME_OF_FILE
  LOGICAL :: OK

  INTEGER(LINT), ALLOCATABLE :: POS(:)
  INTEGER, ALLOCATABLE :: EVEN_RANK(:)
  INTEGER :: E_RANK
  REAL :: RANDNUM
!
!  Initialize MPI
!
   CALL MPI_INIT(IERR)
   
   FLL_MPI_STRUCT => NULL()

   PMPI => FLL_MPI_PROC_STRUCT(FPAR)
   
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, WORLD_RANK, IERR )

   CALL  MPI_Comm_size ( MPI_COMM_WORLD, NPROC, ierr )

   IF(WORLD_RANK == 0) THEN 
     CALL READ_INPUT(NAME_OF_FILE,NFILES,1_LINT*NPROC)
!
!  initialize MPI
!
     FLL_MPI_STRUCT => NULL()
     CALL CREATE_MPI_STRUCT(FLL_MPI_STRUCT,NAME_OF_FILE,NFILES,1_LINT*NPROC)

   END IF
!
!  Copy FLL_MPI_STRUCT date set which now exists on root partition onlyc
!  to all other partitions
!  Upon return, the function will return pointer to newly allocated data
!  for all other partition then root partition
!  On root partition, the PNEW pointer is pointing on FLL_MPI_STRUCT
!
   PNEW => FLL_MPI_CP_ALL(FLL_MPI_STRUCT,MPI_COMM_WORLD,0,FPAR)
   IF(WORLD_RANK /= 0)THEN
!
!  make FLL_MPI_STRUCT point to PNEW so that we cane use the same names for all partitions
!  
     FLL_MPI_STRUCT => PNEW
   END IF
!
!  make some data set similar to solution
  NSIZE = 100000

  CALL CREATE_DATA_SET(PDATA_SET,NSIZE, WORLD_RANK)
  BYTESN = FLL_GETNBYTES(PDATA_SET,FPAR)
  WRITE(*,*)' Partition created data set size of ', WORLD_RANK,BYTESN
!
! Write - each partition to one common file
!
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

  OK = FLL_MPI_WRITE(PDATA_SET,'PartitionedFile',10,0, world_rank, MPI_COMM_WORLD, 'A', FPAR)

  WRITE(*,*)' READING FILES'
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  
  PTMP => FLL_MPI_READ('PartitionedFile',10,0, world_rank, MPI_COMM_WORLD, 'A', FPAR)
  BYTESN = FLL_GETNBYTES(PTMP,FPAR)
  WRITE(*,*)' Partition reads data set size of ', WORLD_RANK,BYTESN
  CALL FLL_RM(PTMP,FPAR)

!
!  make a group and save to a separate file 
!
!
!  Get a group identifier for MPI_COMM_WORLD.
!
  CALL MPI_Comm_group ( MPI_COMM_WORLD, WORLD_GROUP_ID, IERR )
!
  EVEN_P = ( NPROC + 1 ) / 2
  ALLOCATE ( EVEN_RANK(1:EVEN_P), STAT = ISTAT )
   IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY'

  ISTAT = 0
  DO I = 0, NPROC - 1, 2
    ISTAT = ISTAT + 1
    EVEN_RANK(ISTAT) = I+1
  END DO
!
!  create group in parent group:  world_group_id, number of processes 
!  in that group is: even_p
!  the processesare: even_rank(:)
!  the group ID is then 
!
  CALL MPI_Group_incl(WORLD_GROUP_ID, EVEN_P, EVEN_RANK, EVEN_GROUP_ID, IERR )
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  CALL MPI_Comm_create(MPI_COMM_WORLD, EVEN_GROUP_ID, EVEN_COMM_ID, IERR )

  IF(EVEN_COMM_ID /= MPI_COMM_NULL)THEN
   CALL MPI_Comm_rank(EVEN_COMM_ID, E_RANK, IERR )
  END IF
   OK = FLL_MPI_WRITE(PDATA_SET,'PartitionedFile_group',10,0, E_RANK, EVEN_COMM_ID, 'A', FPAR)


  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

  PTMP => NULL()
  PTMP => FLL_MPI_READ('PartitionedFile',10,0, E_RANK, EVEN_COMM_ID, 'A', FPAR)
  BYTESN = FLL_GETNBYTES(PTMP,FPAR)
  WRITE(*,*)' Partition reads data set size of ', WORLD_RANK,BYTESN
  CALL FLL_RM(PTMP,FPAR)

!
!  CLEAN MEMORY
!
  IF(WORLD_RANK==0)write(*,*)' Releasing memory'
  CALL FLL_RM(FLL_MPI_STRUCT,FPAR)
  CALL FLL_RM(PDATA_SET,FPAR)

  
   CALL MPI_FINALIZE(IERR)
  
END PROGRAM

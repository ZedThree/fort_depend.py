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
   TYPE(DNODE), POINTER  :: PNODE, FLL_MPI_STRUCT, PDATA_SET, PNEW,PTMP
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT,I,IERR,WORLD_RANK,world_group_id,NPROC
   CHARACTER :: FMT

  INTEGER(LINT) :: NFILES,BYTES
  CHARACTER(LEN=FILE_NAME_LENGTH) :: NAME_OF_FILE
  LOGICAL :: OK

  INTEGER(LINT), ALLOCATABLE :: POS(:)
!
!  Initialize MPI
!
   CALL MPI_INIT(IERR)
   
   FLL_MPI_STRUCT => NULL()
   
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, WORLD_RANK, IERR )

   CALL  MPI_Comm_size ( MPI_COMM_WORLD, NPROC, ierr )

   IF(WORLD_RANK == 0) THEN 
     CALL READ_INPUT(NAME_OF_FILE,NFILES,1_LINT*NPROC)
!
!  initialize MPI
!
     FLL_MPI_STRUCT => NULL()
     CALL CREATE_MPI_STRUCT(FLL_MPI_STRUCT,NAME_OF_FILE,NFILES,1_LINT*NPROC)

!     WRITE(*,*)'              duplicating'
!     PNEW => FLL_CP(FLL_MPI_STRUCT, NULL(), FPAR)
!     WRITE(*,*)'              duplicate'    
!     OK = FLL_MV(PNEW, FLL_MPI_STRUCT,FPAR)         
!     CALL FLL_CAT(FLL_MPI_STRUCT,6,.false., FPAR)

   END IF
!
!  Copy FLL_MPI_STRUCT date set which now exists on root partition only
!  to all other partitions
!  upon return, the function will return pointer to newly allocated data
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
!
  IF(WORLD_RANK==0)WRITE(*,*)' creating data set'
 !CALL CREATE_DATA_SET(PDATA_SET,1000000_LINT+100000*WORLD_RANK, WORLD_RANK)

  CALL CREATE_DATA_SET(PDATA_SET,10_LINT, WORLD_RANK)

  ! CALL MPI_Comm_group ( MPI_COMM_WORLD, world_group_id, ierr )
!
!  MPI_Barrier does not need to be here, 
!  just for testing purposes 
!
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

  OK = FLL_MPI_WRITE(PDATA_SET,'PartitionedFile',10,0, world_rank, MPI_COMM_WORLD, 'A', FPAR)

  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
  
   PTMP => FLL_MPI_READ('PartitionedFile',10,0, world_rank, MPI_COMM_WORLD, 'A', FPAR)
!
!  CLEAN MEMORY
!
  IF(WORLD_RANK==0)write(*,*)' Releasing memory'
  CALL FLL_RM(FLL_MPI_STRUCT,FPAR)
  CALL FLL_RM(PDATA_SET,FPAR)
  CALL FLL_RM(PTMP,FPAR)

  
   CALL MPI_FINALIZE(IERR)
  
END PROGRAM

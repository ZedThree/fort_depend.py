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
   TYPE(DNODE), POINTER  :: PNODE, FLL_MPI_STRUCT, PDATA_SET, PNEW
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
   write(*,*)' IRANK is ', WORLD_RANK

   call MPI_Comm_size ( MPI_COMM_WORLD, NPROC, ierr )

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
!  just test - if partition #1, print received data set
!
   IF(WORLD_RANK == 1)THEN
      CALL FLL_CAT(FLL_MPI_STRUCT,6,.FALSE., FPAR)
   END IF
!
!  make some data set similar to solution
!
  IF(WORLD_RANK==0)WRITE(*,*)' creating data set'
  CALL CREATE_DATA_SET(PDATA_SET,100000_LINT+10000*WORLD_RANK, WORLD_RANK)

  ALLOCATE(POS(NPROC))
  POS = 0
  POS(WORLD_RANK+1) = FLL_GETNBYTES(PDATA_SET,FPAR)
  WRITE(*,*)' --------   Size of data is ',POS

  ! CALL MPI_Comm_group ( MPI_COMM_WORLD, world_group_id, ierr )
!
!  MPI_Barrier does not need to be here, 
!  just for testing purposes 
!
  CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

  CALL FLL_MPI_SUM(MPI_COMM_WORLD, 1_LINT*NPROC,L1=POS)
  WRITE(*,*)' SUM is ', POS
!
!  CLEAN MEMORY
!
  IF(WORLD_RANK==0)write(*,*)' Releasing memory'
  CALL FLL_RM(FLL_MPI_STRUCT,FPAR)
  CALL FLL_RM(PDATA_SET,FPAR)

  deallocate(pos)
  
   CALL MPI_FINALIZE(IERR)
  
END PROGRAM

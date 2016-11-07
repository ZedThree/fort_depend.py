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
    USE CREATE_MPI_STRUCT_M
    USE READ_INPUT_M
    USE CREATE_DATA_SET_M

    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE
   TYPE(DNODE), POINTER  :: PNODE, FLL_MPI_STRUCT, PDATA_SET
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT,I,IERR,WORLD_RANK,world_group_id
   CHARACTER :: FMT

  INTEGER(LINT) :: NFILES,NPROC
  CHARACTER(LEN=FILE_NAME_LENGTH) :: NAME_OF_FILE
!
!  Initialize MPI
!
   CALL MPI_INIT(IERR)
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, WORLD_RANK, IERR )

   IF(WORLD_RANK == 0) THEN 
     CALL READ_INPUT(NAME_OF_FILE,NFILES,NPROC)
!
!  initialize MPI
!
     FLL_MPI_STRUCT => NULL()
     CALL CREATE_MPI_STRUCT(FLL_MPI_STRUCT,NAME_OF_FILE,NFILES,NPROC)

     CALL FLL_RM(FLL_MPI_STRUCT,FPAR)

   END IF
!
!  make some data set
!
   CALL CREATE_DATA_SET(PDATA_SET,100_LINT, WORLD_RANK)

   CALL MPI_Comm_group ( MPI_COMM_WORLD, world_group_id, ierr )

   CALL MPI_FINALIZE(IERR)
  
END PROGRAM

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
    PROGRAM  MPI-IO

    USE FLL_MODS_M
    USE CREATE_MPI_STRUCT_M

    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE
   TYPE(DNODE), POINTER  :: PNODE, FLL_MPI_STRUCT
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT,I
   CHARACTER :: FMT
!
!  read TEST file and store it in PNODE
!  file is an ASCII file
!
   FLL_MPI_STRUCT => NULL()
   CALL CREATE_MPI_STRUCT(FLL_MPI_STRUCT)

   CALL FLL_RM(FLL_MPI_STRUCT,FPAR)

  
END PROGRAM

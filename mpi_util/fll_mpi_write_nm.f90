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
MODULE FLL_MPI_WRITE_NM_M
!
! Description: contains subroutine writing file in paralell mode from N processors to M files
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used
!
CONTAINS

  FUNCTION FLL_MPI_WRITE_NM(PNODE,FILE,IOUNIT,FILE_TAB,FPAR) RESULT(OK)
!
! Description: contains subroutine writing file in paralell mode from N processors to M files
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used
!
  USE MPI
  USE FLL_MODS_M
  USE FLL_MPI_WRITE_M
  IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! FILE         In         Name of file
! PNODE        Out        Node to a first node in list from a file
! IOUNIT       In         Number of unit
! FILE_TAB     In         Specifies which partition saves to which file
! FPAR         In/Out     structure containing function specific data
! OK           Out        Success or fail
!
! Arguments declaration
!
   CHARACTER(*) :: FILE
   TYPE(DNODE), POINTER  :: PNODE, FILE_TAB
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   LOGICAL OK
!
! local declarations
!
  
  END FUNCTION FLL_MPI_WRITE_NM

END MODULE FLL_MPI_WRITE_NM_M

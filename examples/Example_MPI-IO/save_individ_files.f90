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
!     Description: Saves individual files
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
MODULE SAVE_INDIVID_FILES_M
CONTAINS

  SUBROUTINE SAVE_INDIVID_FILES(PNODE,RANK,FPAR)

  USE FLL_MODS_M
  IMPLICIT NONE

  TYPE(DNODE), POINTER  :: PNODE
!
!   Local declarations
!
   CHARACTER(LEN=NAME_LENGTH) :: FILENAME
   CHARACTER(LEN=6) :: STR
   LOGICAL :: OK
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: RANK
   
   WRITE(STR,'(I6)')RANK+1
   WRITE(FILENAME,'(A16)')"Ifiles_"//TRIM(ADJUSTL(TRIM(STR)))//".bsol"   
   OK =  FLL_WRITE(PNODE,ADJUSTL(TRIM(FILENAME)),RANK+10,'B',FPAR)

  END SUBROUTINE SAVE_INDIVID_FILES
END MODULE SAVE_INDIVID_FILES_M

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
!     Date: 2017-06-15
! 
! 
!
!
!     Description: 
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
    PROGRAM  FLL_TEST

    USE FLL_MODS_M
    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE
   TYPE(DNODE), POINTER  :: PNODE,ptmp
   TYPE(FUNC_DATA_SET) :: FPAR
!
!  reading record
!
   WRITE(*,*)' READING RECORD FROM THE FILE'
   PNODE => FLL_READ_RECORD('Mesh.fll',8,'element_group','*',-1_LINT,3_LINT,.TRUE.,FPAR)

   WRITE(*,*)'WRITING RECORD +++++++++++++++++++++'

   CALL FLL_CAT(PNODE, 6, .TRUE.,FPAR)
   WRITE(*,*)

   IF(.NOT.ASSOCIATED(PNODE))THEN
     WRITE(*,*)' DID NOT FIND RECORD'
     STOP
   END IF

   CALL FLL_RM(PNODE,FPAR)  
  
END PROGRAM

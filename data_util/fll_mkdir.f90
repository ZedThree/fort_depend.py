!
!     Copyright (C) 2016  Adam Jirasek
! 
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU Lesser General Public License as published by
!     the Rree Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
! 
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or RITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU Lesser General Public License for more details.
! 
!     You should have received a copy of the GNU Lesser General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.
!     
!     contact: libm3l@gmail.com
! 
!
MODULE FLL_MKDIR_M
!
! Description: creates node
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
   FUNCTION FLL_MKDIR(NAME,FPAR,ERRMSG) RESULT(PNEW)
!
! Description: function creates node specified by name, type and dimensions
!
! External Modules used
! 
       USE FLL_TYPE_M
       USE FLL_MK_M
       
       IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! NAME         In         name of node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
       TYPE(FUNC_DATA_SET)   :: FPAR
       TYPE(DNODE), POINTER  :: PNEW
       CHARACTER(LEN=*) :: NAME
       CHARACTER(LEN=*), OPTIONAL :: ERRMSG       
!
! Local declarations
!              
       CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
       IF(.NOT.PRESENT(ERRMSG))THEN
         LOC_ERRMSG='ALL'
       ELSE
         LOC_ERRMSG = ERRMSG
       END IF
       PNEW => NULL()
!
! Body
!
       PNEW => FLL_MK(NAME,'DIR',0_LINT, 0_LINT, FPAR,ERRMSG=LOC_ERRMSG)

    RETURN
   END FUNCTION FLL_MKDIR
   
END MODULE FLL_MKDIR_M

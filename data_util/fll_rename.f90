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
!     Subroutine FLL_RENAME
!
!
MODULE FLL_RENAME_M
!
! Description: Contains function fll_rename
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/08/17                         Initial implementation
!
!
! External Modules used
!

CONTAINS
   FUNCTION FLL_RENAME(PWHAT,INAME,FPAR,ERRMSG) RESULT(OK)
!
! Description: Renames node
!
! External Modules used
!                     
       USE FLL_TYPE_M
       USE FLL_OUT_M
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PWHAT        In         pointer which is to be copied
! FPAR         In/Out     structure containing function specific data
! NAME         In         New node name
! OK           Out        Return parameter
!
! Arguments declaration
!
       IMPLICIT NONE
       TYPE(DNODE), POINTER  :: PWHAT
       TYPE(FUNC_DATA_SET)   :: FPAR
       CHARACTER(LEN=*) :: INAME
       LOGICAL :: OK
       CHARACTER(*), OPTIONAL :: ERRMSG
       CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
       IF(.NOT.PRESENT(ERRMSG))THEN
         LOC_ERRMSG='ALL'
       ELSE
         LOC_ERRMSG = ERRMSG
       END IF
!
!  check that PWHAT is not NULL
!      
       IF(.NOT.ASSOCIATED(PWHAT))THEN
          WRITE(*,*)' Rename - SOURCE IS NULL NODE'
          WRITE(FPAR%MESG,'(A,A)')' Rename  - Pwhat null node '
          CALL FLL_OUT(LOC_ERRMSG,FPAR)
          FPAR%SUCCESS = .FALSE.
          OK = .FALSE.
          RETURN
       END IF

       PWHAT%LNAME = ADJUSTL(TRIM(INAME))

       OK = .TRUE.
 
       RETURN
   END FUNCTION FLL_RENAME  

END MODULE FLL_RENAME_M

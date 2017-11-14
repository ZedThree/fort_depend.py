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
!     Subroutine FLL_DEATTACH
!
!
MODULE FLL_DEATTACH_M
!
! Description: Deattach node from the list
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used

CONTAINS
   FUNCTION FLL_DEATTACH(PWHAT,FPAR,ERRMSG) RESULT(OK)
!
! Description: Deattach PWHAT node from the list
!              upon return, PWHAT parent is set to NULL and 
!              the nodes is moved outside the list it is currently in
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used

       USE FLL_TYPE_M
       USE FLL_STICH_M
       USE FLL_OUT_M
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PWHAT        In         node to deattach from the list
! OK           In         return parameter, of .TRUE. - success, if .FALSE. - fail
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
       IMPLICIT NONE
       TYPE(DNODE), POINTER  :: PWHAT
       TYPE(FUNC_DATA_SET) :: FPAR
       LOGICAL::  OK
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
!  check that PWAT is not null node
!
       OK = .FALSE.
       IF(.NOT.ASSOCIATED(PWHAT))THEN
         WRITE(FPAR%MESG,'(A,A)')' Deattach - null node '
         CALL FLL_OUT(LOC_ERRMSG,FPAR)
         FPAR%SUCCESS = .FALSE.
         RETURN
       END IF
!
!   STICH THE LIST WHERE THE SOURCE WILL BE TAKEN FROM 
!
      CALL FLL_STICH(PWHAT,FPAR)
!
!  set PWHAT pointers
!
      PWHAT%PPAR => NULL()
      PWHAT%PNEXT => NULL()
      PWHAT%PPREV => NULL()

      FPAR%SUCCESS = .TRUE.
      OK = .TRUE.
      RETURN
   END FUNCTION FLL_DEATTACH

END MODULE FLL_DEATTACH_M

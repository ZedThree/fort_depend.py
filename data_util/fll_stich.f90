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
MODULE FLL_STICH_M
!
! Description: Contains function fll_stich
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
   SUBROUTINE FLL_STICH(PNODE,FPAR, ERRMSG)
!
! Description: subroutine stiches list after PNODE
!              is taken away
!
! External Modules used
!
   
    USE FLL_TYPE_M
    USE FLL_OUT_M
    
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be take away from the list
!                         the list has to be stiched in place where the 
!                         node PNODE will be missing
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
    CHARACTER(*), OPTIONAL :: ERRMSG
!
! Local declarations
!
    TYPE(DNODE), POINTER  :: PNEXT=>NULL(), PPREV=>NULL(),PPAR=>NULL()
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
!   BODY OF SUBROUTINE
!      
   IF(.NOT.ASSOCIATED(PNODE))THEN
     WRITE(FPAR%MESG,'(A)')' Stich  - null node '
     CALL FLL_OUT(LOC_ERRMSG,FPAR)
     FPAR%SUCCESS = .FALSE.
     RETURN
   END IF

   PNEXT    => PNODE%PNEXT
   PPREV    => PNODE%PPREV
   PPAR     => PNODE%PPAR
   
   IF(.NOT.ASSOCIATED(PNEXT)  .AND. .NOT.ASSOCIATED(PPREV) .AND. .NOT.ASSOCIATED(PPAR)) RETURN

   IF(.NOT.ASSOCIATED(PNODE%PPAR))THEN
     WRITE(FPAR%MESG,'(A,A)')' Stich - null node '
     CALL FLL_OUT(LOC_ERRMSG,FPAR)
     FPAR%SUCCESS = .FALSE.
     RETURN
   END IF
!
!  NODE DOES NOT HAVE ANY NEIGHBOURS
!
   IF(.NOT.ASSOCIATED(PNEXT) .AND. .NOT.ASSOCIATED(PPREV))THEN
     PPAR%NDIM = 0
     PPAR%PCHILD => NULL()
     FPAR%SUCCESS = .TRUE.
!
!  STICH AND SUBSTRACT FROM PARENT 
!
   ELSE IF(ASSOCIATED(PPREV)) THEN
      PPREV%PNEXT => PNEXT
      IF(ASSOCIATED(PNEXT)) PNEXT%PPREV => PPREV
      PPAR%NDIM = PPAR%NDIM -1
   ELSE
!
!  FIRST CHILD NODE
!
     IF(ASSOCIATED(PNEXT))THEN
       PPAR%PCHILD => PNEXT
       PNEXT%PPREV => NULL()
       PNEXT%PPAR => PPAR
       PPAR%NDIM = PPAR%NDIM - 1
     ELSE
       PPAR%PCHILD => NULL()
       PPAR%NDIM = 0
     END IF
   END IF
  
  END SUBROUTINE FLL_STICH
  
END MODULE FLL_STICH_M

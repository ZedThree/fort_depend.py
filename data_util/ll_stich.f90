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
!     Subroutine LL_MV
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: stiches list
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
MODULE LL_STICH_M
CONTAINS

   SUBROUTINE LL_STICH(PNODE,FPAR)
   
    USE LL_TYPE_M
    
    IMPLICIT NONE
!
!   STICHES THE LIST AFTER REMOVING NODES
!
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
!
!   LOCAL TYPES
!
    TYPE(DNODE), POINTER  :: PNEXT=>NULL(), PPREV=>NULL(),PPAR=>NULL()
!   
!   BODY OF SUBROUTINE
!      
   IF(.NOT.ASSOCIATED(PNODE%PPAR))THEN
     WRITE(FPAR%MESG,'(A)')' Stich  - null node '
     FPAR%SUCCESS = .FALSE.
     RETURN
   END IF

   PNEXT    => PNODE%PNEXT
   PPREV    => PNODE%PPREV
   PPAR     => PNODE%PPAR

   IF(.NOT.ASSOCIATED(PNODE%PPAR))THEN
     WRITE(FPAR%MESG,'(A,A)')' Stich - null node '
     FPAR%SUCCESS = .FALSE.
     RETURN
   END IF
!
!  NODE DOES NOT HAVE ANY NEIGHBOURS
!
   IF(.NOT.ASSOCIATED(PNEXT) .AND. .NOT.ASSOCIATED(PPREV))THEN
     PPAR%NDIM = 0
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
  
  END SUBROUTINE LL_STICH
  
END MODULE LL_STICH_M

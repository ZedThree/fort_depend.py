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
!     Subroutine LL_DEATTACH
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: DEATTACHES NODE FROM THE LIST
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
MODULE LL_DEATTACH_M
CONTAINS

   FUNCTION LL_DEATTACH(PWHAT,FPAR) RESULT(OK)
       USE LL_TYPE_M
       USE LL_STICH_M
       IMPLICIT NONE
       TYPE(DNODE), POINTER  :: PWHAT
       TYPE(FUNC_DATA_SET) :: FPAR
       LOGICAL::  OK

       OK = .FALSE.
       IF(.NOT.ASSOCIATED(PWHAT))THEN
         WRITE(FPAR%MESG,'(A,A)')' Deattach - null node '
         FPAR%SUCCESS = .FALSE.
         RETURN
       END IF
!
!   STICH THE LIST WHERE THE SOURCE WILL BE TAKEN FROM 
!
      CALL LL_STICH(PWHAT,FPAR)
      PWHAT%PPAR => NULL()
      PWHAT%PNEXT => NULL()
      PWHAT%PPREV => NULL()

      FPAR%SUCCESS = .TRUE.
      OK = .TRUE.
      RETURN
   END FUNCTION LL_DEATTACH

END MODULE LL_DEATTACH_M

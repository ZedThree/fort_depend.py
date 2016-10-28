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
!     Subroutine FLL_CP
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: moves or copies node 
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
MODULE FLL_CP_M
CONTAINS
   
   FUNCTION FLL_CP(PWHAT,PWHERE,FPAR) RESULT(PNEW)
!
!  FUNCTIONS COPIES PHAT TO PWHERE. 
!  IF PWHERE IS NULL, PWHAT WILL 
!
!
!  INPUT PARAMETERS:
!                      
       USE FLL_TYPE_M
       USE FLL_MV_M
       USE FLL_DUPLICATE_M
       USE FLL_CAT_M

       IMPLICIT NONE
       TYPE(DNODE), POINTER  :: PWHAT,PWHERE
       TYPE(FUNC_DATA_SET) :: FPAR
       TYPE(DNODE), POINTER  :: PNEW
       
       NULLIFY(PNEW)
       
       IF(.NOT.ASSOCIATED(PWHAT))THEN
          WRITE(*,*)' Cp - SOURCE IS NULL NODE'
          WRITE(FPAR%MESG,'(A,A)')' Mv, Cp  - null node '
          FPAR%SUCCESS = .FALSE.
          RETURN
       END IF

       IF(.NOT.ASSOCIATED(PWHERE))THEN
!
! IF NOT SPECIFIED WHERE TO COPY
! JUST DUPLICATE NODE
!
         PNEW => FLL_DUPLICATE(PWHAT, FPAR)
       ELSE
!
!  DUPLICATE NODE AND MOVE IT 
!  TO PWHERE
!
         PNEW => FLL_CP_R(PWHAT,PWHERE,'C',FPAR) 
       END IF
 
       RETURN
   END FUNCTION FLL_CP  


   FUNCTION  FLL_CP_R(PWHAT,PWHERE,MODE,FPAR) RESULT(PSOURCETMP)
   
    USE FLL_TYPE_M
    USE FLL_RM_M
    USE FLL_STICH_M
    USE FLL_DUPLICATE_M
    
    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   TYPE(DNODE), POINTER  :: PWHAT,PWHERE
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER :: MODE    ! 'C' - COPY,   'M' - MOVE
!
!   LOCAL TYPES
!
   TYPE(DNODE), POINTER  :: PTNEXT, PTPREV,PTPAR,&
     PLAST, PSOURCETMP,PCHILD,PNEW
!   
!   BODY OF SUBROUTINE
!   
   PSOURCETMP => NULL()
   
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PWHAT))THEN
      WRITE(*,*)' Cp - SOURCE IS NULL NODE'
      WRITE(FPAR%MESG,'(A,A)')' Mv, Cp  - null node '
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   IF(.NOT.ASSOCIATED(PWHERE))THEN
      WRITE(FPAR%MESG,'(A,A)')' Mv, Cp  - null node '
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
!
   PTNEXT    => PWHERE%PNEXT
   PTPREV    => PWHERE%PPREV
   PTPAR     => PWHERE%PPAR
   
   IF(MODE == 'C')THEN 
      PNEW => FLL_DUPLICATE(PWHAT, FPAR)
      PSOURCETMP => PNEW
!
!  IN CASE NODE IS NOT COPIED ANYWAY, IT IS ONLY DUPLICATED, RETURN
!
      IF(.NOT.ASSOCIATED(PWHERE)) RETURN
   ELSE
      PSOURCETMP => PWHAT
   END IF
!
!  GET TYPE OF TARGET NODE
!  DISTINGUISH BETWEEN DIR, OTHER TYPES
!
   SELECT CASE(PWHERE%LTYPE)
      
   CASE('N','DIR')
!
!   STICH THE LIST WHERE THE SOURCE WILL BE TAKEN FROM 
!
     IF(MODE == 'M') CALL FLL_STICH(PWHAT,FPAR)
     
     IF(.NOT.ASSOCIATED(PWHERE%PCHILD))THEN
!
!  DIR IS EMPTY
!       
       PWHERE%PCHILD    => PSOURCETMP
       PSOURCETMP%PPAR  => PWHERE
       PSOURCETMP%PPREV => NULL()
       PSOURCETMP%PNEXT => NULL()
       PWHERE%NDIM     = 1

     ELSE
!
!   DIR HAS ALREADY CHILDREN, ADD TO THE END
!
       PCHILD => PWHERE%PCHILD
       DO WHILE(ASSOCIATED(PCHILD))
         PLAST   => PCHILD
         PCHILD => PCHILD%PNEXT
       END DO

       PLAST%PNEXT => PSOURCETMP
       PSOURCETMP%PPREV => PLAST
       PSOURCETMP%PNEXT => NULL()
       PSOURCETMP%PPAR  => PWHERE
       PWHERE%NDIM     = PWHERE%NDIM + 1 

     END IF
   
   CASE DEFAULT
!
!   DATA TYPES OF NODES, THE PWEHRE NODE WILL BE OVERWRITTEN
!   DELETE TARGET NODE
!
     CALL FLL_RM(PWHERE, FPAR)
!
!   STICH THE LIST WHERE THE SOURCE WILL BE TAKEN FROM 
!
     IF(MODE == 'M') CALL FLL_STICH(PSOURCETMP,FPAR)
!
!  ADD A NEW NODE
!
     IF(.NOT.ASSOCIATED(PTPREV))THEN
!
!  PWHERE WAS THE FIRST NODE IN THE LIST
!
       PTPAR%PCHILD => PSOURCETMP
       PSOURCETMP%PPAR   => PTPAR
       PSOURCETMP%PPREV => NULL()
       PTPAR%NDIM     = PTPAR%NDIM + 1
       
       IF(ASSOCIATED(PTNEXT))THEN
         PSOURCETMP%PNEXT => PTNEXT
       ELSE
         PSOURCETMP%PNEXT => NULL()
       END IF
       
     ELSE
!
!  NODE NOT THE FIRST IN THE LINE
!     
       PTPREV%PNEXT => PSOURCETMP
       PSOURCETMP%PPREV => PTPREV
       
       IF(ASSOCIATED(PTNEXT))THEN
         PSOURCETMP%PNEXT => PTNEXT
       ELSE
         PSOURCETMP%PNEXT => NULL()
       END IF
       PTPAR%NDIM     = PTPAR%NDIM + 1

     END IF
     
   END SELECT

   FPAR%SUCCESS = .TRUE.

   RETURN
   END FUNCTION  FLL_CP_R

END MODULE FLL_CP_M

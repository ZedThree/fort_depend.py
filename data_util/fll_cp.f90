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
!     Subroutine FLL_CP
!
!
MODULE FLL_CP_M
!
! Description: Contains function fll_cp
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
   FUNCTION FLL_CP(PWHAT,PWHERE,FPAR,ERRMSG) RESULT(PNEW)
!
! Description: Module copies PWHAT pointer to PWHERE pointer
!              If PWHERE pointer == NULL, PWHAT is a duplicate 
!              of PWHAT with PWHAT%Ppar == NULL
!
!              if PWHERE is DIR on N type, PWHAT is added
!              to it as a new sub-data set
!
!              if PWHERE is a data type of nodes
!              PWHAT overwrites it
!
! External Modules used
!                     
       USE FLL_TYPE_M
       USE FLL_MV_M
       USE FLL_DUPLICATE_M
       USE FLL_CAT_M
       USE FLL_OUT_M
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PWHAT        In         pointer which is to be copied
! PWHERE       In         pointer which is to be copied
! PNEW         Out        pointer to new copy of PWHAT
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
       IMPLICIT NONE
       TYPE(DNODE), POINTER  :: PWHAT,PWHERE
       TYPE(FUNC_DATA_SET) :: FPAR
       TYPE(DNODE), POINTER  :: PNEW
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
!  initialize PNEW pointer and check that PWHAT is not NULL
!
       NULLIFY(PNEW)
       
       IF(.NOT.ASSOCIATED(PWHAT))THEN
          WRITE(*,*)' Cp - SOURCE IS NULL NODE'
          WRITE(FPAR%MESG,'(A,A)')' Cp  - Pwhat null node '
          CALL FLL_OUT(LOC_ERRMSG,FPAR)
          FPAR%SUCCESS = .FALSE.
          RETURN
       END IF

       IF(.NOT.ASSOCIATED(PWHERE))THEN
!
! IF NOT SPECIFIED WHERE TO COPY
! JUST DUPLICATE NODE
!
         PNEW => FLL_DUPLICATE(PWHAT, FPAR,LOC_ERRMSG)
       ELSE
!
!  OTHERWISE DUPLICATE NODE AND MOVE IT 
!  TO PWHERE
!
         IF(.NOT.ASSOCIATED(PWHAT,PWHERE))&
            PNEW => FLL_CP_R(PWHAT,PWHERE,'C',FPAR,LOC_ERRMSG) 
       END IF
 
       RETURN
   END FUNCTION FLL_CP  


   FUNCTION  FLL_CP_R(PWHAT,PWHERE,MODE,FPAR,LOC_ERRMSG) RESULT(PSOURCETMP)
!
! Description: Module copies PWHAT pointer to PWHERE pointer
!              If PWHERE pointer == NULL, PWHAT is a duplicate 
!              of PWHAT with PWHAT%Ppar == NULL
!
! External Modules used
!   
    USE FLL_TYPE_M
    USE FLL_RM_M
    USE FLL_STICH_M
    USE FLL_DUPLICATE_M
    USE FLL_OUT_M
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PWHAT        In         pointer which is to be copied
! PWHERE       In         pointer which is to be copied
! MODE         In         if C - copy mode, if M - move mode
! PSOURCETMP   Out        pointer to new copy of PWHAT
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!    
    IMPLICIT NONE

   TYPE(DNODE), POINTER  :: PWHAT,PWHERE
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER :: MODE    ! 'C' - COPY,   'M' - MOVE
   CHARACTER(*) :: LOC_ERRMSG
!
!   LOCAL TYPES
!
   TYPE(DNODE), POINTER  :: PTNEXT, PTPREV,PTPAR,&
     PLAST, PSOURCETMP,PCHILD,PNEW
!   
!   BODY OF SUBROUTINE
!   
   PSOURCETMP => NULL()
!
!  check that node is not null
!
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PWHAT))THEN
      WRITE(*,*)' Cp - SOURCE IS NULL NODE'
      WRITE(FPAR%MESG,'(A,A)')' Cp  - Pwhat null node'
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
!
!  check that pwhere is associated
!
   IF(.NOT.ASSOCIATED(PWHERE))THEN
      WRITE(FPAR%MESG,'(A,A)')' Cp  - Pwhere null node'
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
!
   PTNEXT    => PWHERE%PNEXT
   PTPREV    => PWHERE%PPREV
   PTPAR     => PWHERE%PPAR
   
   IF(MODE == 'C')THEN 
      PNEW => FLL_DUPLICATE(PWHAT, FPAR,LOC_ERRMSG)
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
     IF(MODE == 'M') CALL FLL_STICH(PWHAT,FPAR,LOC_ERRMSG)
     
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
     CALL FLL_RM(PWHERE, FPAR,LOC_ERRMSG)
!
!   STICH THE LIST WHERE THE SOURCE WILL BE TAKEN FROM 
!
     IF(MODE == 'M') CALL FLL_STICH(PSOURCETMP,FPAR,LOC_ERRMSG)
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

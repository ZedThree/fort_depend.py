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
MODULE FLL_MV_M
!
! Description: Contains function fll_mv
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
   FUNCTION FLL_MV(PWHAT,PWHERE,FPAR,ERRMSG) RESULT(OK)
!
! Description: Module moves PWHAT pointer to PWHERE pointer
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
       
       IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PWHAT        In         pointer which is to be copied
! PWHERE       In         pointer which is to be copied
! FPAR         In/Out     structure containing function specific data
! OK           Out        if .TRUE. succes, of .FALSE. fail
!
! Arguments declaration
!
       TYPE(DNODE), POINTER  :: PWHAT,PWHERE
       TYPE(FUNC_DATA_SET)   :: FPAR
       LOGICAL OK
       CHARACTER(*), OPTIONAL :: ERRMSG
!
! Local declarations
!
       TYPE(DNODE), POINTER  :: PSOURCETMP
       CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
       IF(.NOT.PRESENT(ERRMSG))THEN
         LOC_ERRMSG='ALL'
       ELSE
         LOC_ERRMSG = ERRMSG
       END IF
       
       IF(.NOT.ASSOCIATED(PWHAT,PWHERE))THEN
    
         PSOURCETMP => FLL_MVCP(PWHAT,PWHERE,'M',FPAR,LOC_ERRMSG) 
         OK = FPAR%SUCCESS

       END IF
        
       RETURN
   END FUNCTION FLL_MV

   FUNCTION  FLL_MVCP(PWHAT,PWHERE,MODE,FPAR,LOC_ERRMSG) RESULT(PSOURCETMP)
!
! Description: Module moves or copies PWHAT pointer to PWHERE pointer
!              depending on MODE values (C or M)
!              If PWHERE pointer == NULL, PWHAT is a duplicate 
!              of PWHAT with PWHAT%Ppar == NULL
!
! External Modules used
! 
    USE FLL_TYPE_M
    USE FLL_RM_M
    USE FLL_STICH_M
    USE FLL_OUT_M
   
    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   TYPE(DNODE), POINTER  :: PWHAT,PWHERE
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER :: MODE    ! 'C' - COPY,   'M' - MOVE
   CHARACTER(*) :: LOC_ERRMSG
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
   TYPE(DNODE), POINTER  :: PTNEXT, PTPREV,PTPAR,&
     PLAST, PSOURCETMP,PCHILD,PWHERPAR,PWHERENEXT
   LOGICAL :: OK
!   
!   BODY OF SUBROUTINE
!   
   PSOURCETMP => NULL()
   
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PWHAT))THEN
      WRITE(FPAR%MESG,'(A,A)')' Mv  - Pwhat null node'
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   IF(.NOT.ASSOCIATED(PWHERE))THEN
      WRITE(FPAR%MESG,'(A,A)')' Mv  - Pwhere null node'
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
!
   PTNEXT    => PWHERE%PNEXT
   PTPREV    => PWHERE%PPREV
   PTPAR     => PWHERE%PPAR
   
   PSOURCETMP => PWHAT
!
!  GET TYPE OF TARGET NODE
!  DISTINGUISH BETWEEN DIR, OTHER TYPES
!
   SELECT CASE( TRIM(PWHERE%LTYPE))
      
   CASE('N','DIR')
!
!   STICH THE LIST WHERE THE SOURCE WILL BE TAKEN FROM 
!
     CALL FLL_STICH(PWHAT,FPAR,ERRMSG=LOC_ERRMSG)
     
     IF(.NOT.ASSOCIATED(PWHERE%PCHILD))THEN
!
!  DIR IS EMPTY
!       
       PWHERE%PCHILD    => PSOURCETMP
       PWHERE%NDIM     = 1

       PSOURCETMP%PPAR  => PWHERE
       PSOURCETMP%PPREV => NULL()
       PSOURCETMP%PNEXT => NULL()

     ELSE
!
!   DIR HAS ALREADY CHILDREN, ADD TO THE END
!
       PCHILD => PWHERE%PCHILD
       DO WHILE(ASSOCIATED(PCHILD))
         PLAST   => PCHILD
         PCHILD => PCHILD%PNEXT
       END DO
       
       PLAST%PNEXT     => PSOURCETMP
       PWHERE%NDIM     = PWHERE%NDIM + 1 

       PSOURCETMP%PPREV => PLAST
       PSOURCETMP%PNEXT => NULL()
       PSOURCETMP%PPAR  => PWHERE

     END IF
   
   CASE DEFAULT
!
!   DATA TYPES OF NODES, THE PWEHRE NODE WILL BE OVERWRITTEN
!   DELETE TARGET NODE
!
     PWHERPAR => PWHERE%PPAR
     PWHERENEXT => PWHERE%PNEXT

     CALL FLL_RM(PWHERE, FPAR,ERRMSG=LOC_ERRMSG) 
!
!  IF LIST MOVED TO ITS DIRECT PREDECESSOR OR SUCCESSOR
!  LEAVE
!
     IF(ASSOCIATED(PWHERPAR,PWHAT))RETURN
     IF(ASSOCIATED(PWHERENEXT,PWHAT))RETURN
!
!   STICH THE LIST WHERE THE SOURCE WILL BE TAKEN FROM 
!
     CALL FLL_STICH(PSOURCETMP,FPAR,ERRMSG=LOC_ERRMSG)
!
!  ADD A NEW NODE
!
     IF(.NOT.ASSOCIATED(PTPREV))THEN
!
!  PWHERE WAS THE FIRST NODE IN THE LIST
!
       PTPAR%PCHILD => PSOURCETMP
       IF(.NOT.ASSOCIATED(PWHERPAR,PTPAR))THEN
          PTPAR%NDIM   =  PTPAR%NDIM + 1
       END IF

       PSOURCETMP%PPAR  => PTPAR
       PSOURCETMP%PPREV => NULL()
       
       IF(ASSOCIATED(PTNEXT))THEN
         IF(.NOT.ASSOCIATED(PWHAT, PTNEXT))THEN
!
!  if pnext and pwhat are not the same
!  associate pnext from pwhere%next
!  otherwise leave pwhat%pnext as it is
!  the list is just move one step up
!
           PSOURCETMP%PNEXT => PTNEXT
         ELSE 
           PTPAR%NDIM   =  PTPAR%NDIM - 1
         END IF
       ELSE
         PSOURCETMP%PNEXT => NULL()
       END IF
       
     ELSE
!
!  NODE NOT THE FIRST IN THE LINE
!     
       IF(.NOT.ASSOCIATED(PWHERPAR,PTPAR))THEN
          PTPAR%NDIM   =  PTPAR%NDIM + 1
       END IF
       
       PTPREV%PNEXT => PSOURCETMP
          IF(.NOT.ASSOCIATED(PWHAT, PTPREV))THEN
              PSOURCETMP%PPREV => PTPREV
          END IF
       PSOURCETMP%PPAR  => PWHERE
      
       IF(ASSOCIATED(PTNEXT))THEN
        IF(.NOT.ASSOCIATED(PWHAT, PTNEXT))THEN
!
!  if pnext and pwhat are not the same
!  associate pnext from pwhere%next
!  otherwise leave pwhat%pnext as it is
!  the list is just move one step up
!
           PSOURCETMP%PNEXT => PTNEXT
         END IF
       ELSE
         PSOURCETMP%PNEXT => NULL()
       END IF

     END IF
     
   END SELECT

   FPAR%SUCCESS = .TRUE.

   RETURN
   END FUNCTION  FLL_MVCP

END MODULE FLL_MV_M

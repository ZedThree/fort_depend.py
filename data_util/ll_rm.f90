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
!     Subroutine LL_RM
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: removes node and all its children
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
MODULE LL_RM_M
CONTAINS

   SUBROUTINE LL_RM(PNODE,FPAR)
   
    USE LL_TYPE_M
    USE LL_STICH_M
    IMPLICIT NONE
!
!   SUBROUTINE REMOVES NODE
!
   TYPE(DNODE), POINTER  :: PNODE,PCHILD
   TYPE(FUNC_DATA_SET) :: FPAR
!
!   LOCAL TYPES
!
   INTEGER :: ISTAT
!   
!   BODY OF SUBROUTINE
!   
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' RM - null node '
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   PCHILD => PNODE%PCHILD
!
! IF NODE HAS CHILDREN, REMOVE ALL OF THEM
!
   IF(ASSOCIATED(PCHILD))CALL LL_RM_RECURSIVE_NODE(PCHILD,FPAR)

   CALL LL_DEALLOC_DATA(PNODE,FPAR)
!
!  STICH AND SUBSTRACT FROM PARENT 
!
   CALL LL_STICH(PNODE,FPAR)
!
!  NULLIFY NODE
!
   DEALLOCATE(PNODE, STAT=ISTAT)
   IF(ISTAT /= 0)STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:88 '
   NULLIFY(PNODE)

   FPAR%SUCCESS = .TRUE.

   RETURN
   END SUBROUTINE LL_RM
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE LL_RM_RECURSIVE_NODE(PNODE,FPAR)
  
     USE LL_TYPE_M
     IMPLICIT NONE
!
!   SUBROUTINE REMOVES NODE
! 
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
    
    TYPE(DNODE), POINTER  :: PCURR, PNEXT
    INTEGER :: ISTAT
!
!  IF NODE HAS CHILDREN, DELET THEM FIRST
!
    PCURR => PNODE
!
!  IF CHILDREN, DELETE THEM FIRST
!
    DO WHILE(ASSOCIATED(PCURR))
       PNEXT => PCURR%PNEXT
       IF(ASSOCIATED(PCURR%PCHILD))THEN
         CALL LL_RM_RECURSIVE_NODE(PCURR%PCHILD,FPAR)
       END IF

       IF(TRIM(PCURR%LTYPE) /= 'LINK')THEN

         CALL  LL_DEALLOC_DATA(PCURR,FPAR)
         
         IF(ASSOCIATED(PCURR%PLINK))THEN
          PNODE%PLINK%PCHILD => NULL();
         END IF
         
         DEALLOCATE(PCURR, STAT=ISTAT)
           IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> ll_rm ERR:132 '
         NULLIFY(PCURR)
         FPAR%SUCCESS = .TRUE.
       ELSE
!
!  SPCIAL TREATMENT FOR LINKS
!
         PCURR%PCHILD%PLINK => NULL()
         PCURR%PCHILD => NULL()
         DEALLOCATE(PCURR, STAT=ISTAT)
         IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> ll_rm ERR:142 '
         NULLIFY(PCURR)
         FPAR%SUCCESS = .TRUE.
       END IF
       
       PCURR => PNEXT

    END DO
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE LL_RM_RECURSIVE_NODE
!
!  FREE MEMORY FOR NODE
!
  SUBROUTINE LL_DEALLOC_DATA(PNODE,FPAR)
    USE LL_TYPE_M
    IMPLICIT NONE
!
!   SUBROUTINE REMOVES NODE
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   
   INTEGER :: ISTAT
!
!   1D ARRAYS
!
           IF(ASSOCIATED(PNODE%R1))THEN
             DEALLOCATE(PNODE%R1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:174 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE.
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%D1))THEN
             DEALLOCATE(PNODE%D1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:184 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%I1))THEN
             DEALLOCATE(PNODE%I1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:194 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%L1))THEN
             DEALLOCATE(PNODE%L1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:204 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
!
!  2D ARRAYS
!
           IF(ASSOCIATED(PNODE%R2))THEN
             DEALLOCATE(PNODE%R2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:216 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%D2))THEN
             DEALLOCATE(PNODE%D2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:226 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%I2))THEN
             DEALLOCATE(PNODE%I2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:236 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%L2))THEN
             DEALLOCATE(PNODE%L2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> ll_rm ERR:246 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
  
  END SUBROUTINE LL_DEALLOC_DATA
 

END MODULE LL_RM_M

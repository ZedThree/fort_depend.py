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
MODULE FLL_RM_M
!
! Description: Contains function fll_rm
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
   SUBROUTINE FLL_RM(PNODE,FPAR,ACTION)
!
! Description: function removes PNODE
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
    USE FLL_TYPE_M
    USE FLL_STICH_M
    USE FLL_OUT_M
    
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be removed
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE,PCHILD
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER(*), OPTIONAL :: ACTION
!
! Local declarations
!
   INTEGER :: ISTAT
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
   IF(.NOT.PRESENT(ACTION))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ACTION
   END IF
!   
!   BODY OF SUBROUTINE
!   
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' RM - null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   PCHILD => PNODE%PCHILD
!
!  STICH AND SUBSTRACT FROM PARENT 
!
   CALL FLL_STICH(PNODE,FPAR,LOC_ERRMSG)
!
! IF NODE HAS CHILDREN, REMOVE ALL OF THEM
!
   IF(ASSOCIATED(PCHILD))CALL FLL_RM_RECURSIVE_NODE(PCHILD,FPAR,LOC_ERRMSG)

   CALL FLL_DEALLOC_DATA(PNODE,FPAR,LOC_ERRMSG)
!
!  NULLIFY NODE
!
   DEALLOCATE(PNODE, STAT=ISTAT)
   IF(ISTAT /= 0)STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:103 '
   NULLIFY(PNODE)

   FPAR%SUCCESS = .TRUE.

   RETURN
   END SUBROUTINE FLL_RM
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE FLL_RM_RECURSIVE_NODE(PNODE,FPAR,LOC_ERRMSG)
!
! Description: recursive function removes PNODE
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
  
     USE FLL_TYPE_M
     IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be removed
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET)   :: FPAR
    CHARACTER(*) :: LOC_ERRMSG
!
!  Local declarations
!
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
         CALL FLL_RM_RECURSIVE_NODE(PCURR%PCHILD,FPAR,LOC_ERRMSG)
       END IF

       IF(TRIM(PCURR%LTYPE) /= 'LINK')THEN

         CALL  FLL_DEALLOC_DATA(PCURR,FPAR,LOC_ERRMSG)
         
         IF(ASSOCIATED(PCURR%PLINK))THEN
          PNODE%PLINK%PCHILD => NULL();
         END IF
         
         DEALLOCATE(PCURR, STAT=ISTAT)
           IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:169 '
         NULLIFY(PCURR)
         FPAR%SUCCESS = .TRUE.
       ELSE
!
!  SPCIAL TREATMENT FOR LINKS
!
         PCURR%PCHILD%PLINK => NULL()
         PCURR%PCHILD => NULL()
         DEALLOCATE(PCURR, STAT=ISTAT)
         IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> fll_rm ERR:179 '
         NULLIFY(PCURR)
         FPAR%SUCCESS = .TRUE.
       END IF
       
       PCURR => PNEXT

    END DO
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE FLL_RM_RECURSIVE_NODE
!
!  FREE MEMORY FOR NODE
!
  SUBROUTINE FLL_DEALLOC_DATA(PNODE,FPAR,LOC_ERRMSG)
!
! Description: function deallocates data from associated with PNODE
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
    USE FLL_TYPE_M
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be removed
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER(*) :: LOC_ERRMSG
!
! local declarations
!   
   INTEGER :: ISTAT
!
!   1D ARRAYS
!
           IF(ASSOCIATED(PNODE%R1))THEN
             DEALLOCATE(PNODE%R1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:233 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE.
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%D1))THEN
             DEALLOCATE(PNODE%D1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:243 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%I1))THEN
             DEALLOCATE(PNODE%I1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:253 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%L1))THEN
             DEALLOCATE(PNODE%L1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:263 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
           
          IF(ASSOCIATED(PNODE%S1))THEN
             DEALLOCATE(PNODE%S1, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:273 '
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
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:285 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%D2))THEN
             DEALLOCATE(PNODE%D2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:295 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%I2))THEN
             DEALLOCATE(PNODE%I2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:305 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF

           IF(ASSOCIATED(PNODE%L2))THEN
             DEALLOCATE(PNODE%L2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:315 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
           
            IF(ASSOCIATED(PNODE%S2))THEN
             DEALLOCATE(PNODE%S2, STAT=ISTAT)
             IF(ISTAT /= 0)THEN
                STOP' ERROR DEALLOCATING MEMORY ==> fll_rm ERR:325 '
                FPAR%SUCCESS = .FALSE.
             END IF
             FPAR%SUCCESS = .TRUE. 
             RETURN
           END IF
  
  END SUBROUTINE FLL_DEALLOC_DATA

END MODULE FLL_RM_M

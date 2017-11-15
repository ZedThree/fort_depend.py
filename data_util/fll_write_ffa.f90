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
MODULE FLL_WRITE_FFA_M
!
! Description: Contains functions writing FFA native format file, ASCII and BINARY
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
  FUNCTION FLL_WRITE_FFA(PNODE,FILE,IOUNIT,FMT,FPAR,ERRMSG) RESULT(OK)
!
! Description: main function opening, writing and closing file
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
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! FILE         In         Name of file
! PNODE        Out        Node to a first node in list from a file
! IOUNIT       In         Number of unit
! FMT          In         Format - a,A ASCII, b,B - Binary, * not specified
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   CHARACTER(*) :: FILE
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER :: FMT
   LOGICAL OK
   CHARACTER(32) :: FFVERSION='FFA-format-v2'
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! Local declarations
!
   CHARACTER :: FMT_LOC
   INTEGER :: ISTAT
   INTEGER(LINT) :: POS
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
!   DETERMINE RORMAT'
!
   SELECT CASE(FMT)
   CASE('A','a')     ! ASCII FORMAT
     FMT_LOC = 'A'
   CASE('B','b')     ! BINARY FORMAT
      FMT_LOC = 'B'
    CASE('U','u','*')! UNKNOWN - UNSPECIFIED FORMAT
      FMT_LOC = 'U'
    CASE DEFAULT
      WRITE(FPAR%MESG,'(A,A)')' Write  - unknown format',TRIM(FMT)
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      OK = .FALSE.
      RETURN
    END SELECT
!
!   OPEN THE FILE
!
   SELECT CASE(FMT_LOC)
    CASE('B')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='UNFORMATTED',&
           ACCESS='STREAM',CONVERT='big_endian', IOSTAT=ISTAT)
       WRITE(IOUNIT)FFVERSION
    CASE('A')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='FORMATTED',&
           IOSTAT=ISTAT,ACTION='WRITE')
    END SELECT

    IF(ISTAT/=0) THEN
      WRITE(FPAR%MESG,'(A,A)')' Write  - error opening file ',TRIM(FILE)
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      OK = .FALSE.
      RETURN
    END IF
!
!   WRITE  LINKED LIST
!
    CALL FLL_WRITE_FFA_LIST(PNODE,IOUNIT,FMT_LOC,FPAR,LOC_ERRMSG)
    
    CLOSE(IOUNIT)
    IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A,A)')' Read  - error reading file ',TRIM(FILE)
       CALL FLL_OUT(LOC_ERRMSG,FPAR)
       FPAR%SUCCESS = .FALSE.
       OK = .FALSE.
       RETURN
    END IF
    
    OK = .TRUE.
    RETURN
  
  END FUNCTION FLL_WRITE_FFA



   SUBROUTINE FLL_WRITE_FFA_LIST(PNODE,IOUNIT,FMT,FPAR,LOC_ERRMSG)
!
! Description: Function writes a list
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
    USE FLL_OUT_M
    
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        Out        Pointer to node
! IOUNIT       In         Number of unit
! FMT          In         Format - a,A ASCII, b,B - Binary
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER :: FMT   
   CHARACTER(*) :: LOC_ERRMSG
!
!  Local declarations
!
   TYPE(DNODE), POINTER  :: PCHILD
   INTEGER(LINT) :: POS
!   
!   BODY OF SUBROUTINE
!
   POS = 0
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' Write - null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   IF(FMT == 'A')THEN
     CALL FLL_SAVE_NODE_A_FFA(PNODE, IOUNIT, FPAR)
   ELSE
     CALL FLL_SAVE_NODE_B_FFA(PNODE, IOUNIT, POS, FPAR)
   END IF     

   PCHILD => PNODE%PCHILD
!
! IF NODE HAS CHILDREN PRINT THEM TOO
!
   IF(ASSOCIATED(PCHILD))CALL FLL_WRITE_FFA_RECURSIVE_NODE(PCHILD,IOUNIT,POS,FMT,FPAR)

   FPAR%SUCCESS = .TRUE.

   RETURN
   END SUBROUTINE FLL_WRITE_FFA_LIST
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE FLL_WRITE_FFA_RECURSIVE_NODE(PNODE,IOUNIT,POS,FMT,FPAR)
!
! Description: Function writes a node
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
! PNODE        Out        Pointer to node
! IOUNIT       In         Number of unit
! FMT          In         Format - a,A ASCII, b,B - Binary
! POS          In/Out     position in binary file
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
! 
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER(LINT) :: POS 
    INTEGER :: IOUNIT
    CHARACTER :: FMT
!
! Local declarations
!   
    TYPE(DNODE), POINTER  :: PCURR, PNEXT, PCHILD

!
!  IF NODE HAS CHILDREN
!
    PCURR => PNODE
!
!  IF CHILDREN, PRINT THEM TOO
!
    DO WHILE(ASSOCIATED(PCURR))
 
       IF(FMT == 'A')THEN
         CALL FLL_SAVE_NODE_A_FFA(PCURR, IOUNIT, FPAR)
       ELSE
         CALL FLL_SAVE_NODE_B_FFA(PCURR, IOUNIT, POS, FPAR)
       END IF  

       PNEXT  => PCURR%PNEXT
       PCHILD => PCURR%PCHILD
       IF(ASSOCIATED(PCHILD))THEN
         CALL FLL_WRITE_FFA_RECURSIVE_NODE(PCHILD,IOUNIT,POS,FMT,FPAR)
       END IF
       
       PCURR => PNEXT
    END DO
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE FLL_WRITE_FFA_RECURSIVE_NODE
!
!  FREE MEMORY FOR NODE
!
  SUBROUTINE FLL_SAVE_NODE_A_FFA(PNODE, IOUNIT, FPAR)
!
! Description: Writes node and its data to an ASCII file
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
! PNODE        Out        Pointer to node
! IOUNIT       In         Number of unit
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
!
! Local declarations
!
   INTEGER(LINT) :: I,J,NDIM,NSIZE
   LOGICAL :: SAVED
   CHARACTER(LEN=TYPE_LENGTH) :: LTYPE
   
   SAVED = .FALSE.
!
!   1D ARRAYS
!
     LTYPE = PNODE%LTYPE
     IF(TRIM(PNODE%LTYPE) == 'DIR' .OR.TRIM(PNODE%LTYPE) == 'N')THEN
        IF(PNODE%NLINK > 0)THEN 
          WRITE(IOUNIT, *)TRIM(PNODE%LNAME)," ",TRIM(PNODE%FTYPE),' 1 1 ',PNODE%NDIM
           LTYPE = PNODE%FTYPE
        ELSE
          WRITE(IOUNIT, *)TRIM(PNODE%LNAME),' N  0  0 ',PNODE%NDIM
          RETURN
        END IF
     ELSE 
        IF(TRIM(LTYPE) == 'S') THEN
          IF(TRIM(PNODE%FTYPE) == 'L')THEN
             LTYPE = 'L'
          ELSE
             LTYPE ='S'
          END IF
        ELSE IF(TRIM(LTYPE) == 'L') THEN
          LTYPE ='I'
        END IF

        IF(PNODE%FTYPE /= '') LTYPE = PNODE%FTYPE
        
        IF( (PNODE%NSIZE * PNODE%NDIM > 1) )THEN
          SELECT CASE(LTYPE)
            CASE('D','R','I','J')
             LTYPE(2:2) = 'F'
          END SELECT
        END IF
        WRITE(IOUNIT, *)TRIM(PNODE%LNAME),' ', TRIM(LTYPE),' ', PNODE%NSIZE, ' ',PNODE%NDIM,' ',0
     END IF
!
!  1 D ARRAYS
!
        IF(ASSOCIATED(PNODE%R1))THEN
          NDIM = SIZE(PNODE%R1, DIM =1, KIND = LINT)
          WRITE(IOUNIT, *)(PNODE%R1(I), I = 1,NDIM)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%D1))THEN
          NDIM = SIZE(PNODE%D1, DIM =1, KIND = LINT)
          WRITE(IOUNIT, *)(PNODE%D1(I), I = 1,NDIM)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%I1))THEN
          NDIM = SIZE(PNODE%I1, DIM =1, KIND = LINT)
          WRITE(IOUNIT, *)(PNODE%I1(I), I = 1,NDIM)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%L1))THEN
          NDIM = SIZE(PNODE%L1, DIM =1, KIND = LINT)
          WRITE(IOUNIT, *)(PNODE%L1(I), I = 1,NDIM)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%S1))THEN
          NDIM = SIZE(PNODE%S1, DIM =1, KIND = LINT)
          DO I = 1,NDIM
            WRITE(IOUNIT, *)"'",TRIM(PNODE%S1(I)),"'"
          END DO
          SAVED = .TRUE.
!
!  2D ARRAYS
!
       ELSE IF(ASSOCIATED(PNODE%R2))THEN
          NDIM  = SIZE(PNODE%R2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%R2, DIM =2, KIND = LINT)
          DO J=1,NSIZE
            WRITE(IOUNIT, *)(PNODE%R2(I,J), I = 1,NDIM)
          END DO
        SAVED = .TRUE.
      ELSE IF(ASSOCIATED(PNODE%D2))THEN
          NDIM  = SIZE(PNODE%D2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%D2, DIM =2, KIND = LINT)
          DO J=1,NSIZE
            WRITE(IOUNIT, *)(PNODE%D2(I,J), I = 1,NDIM)
          END DO
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%I2))THEN
          NDIM  = SIZE(PNODE%I2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%I2, DIM =2, KIND = LINT)
          DO J=1,NSIZE
            WRITE(IOUNIT, *)(PNODE%I2(I,J), I = 1,NDIM)
          END DO
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%L2))THEN
          NDIM  = SIZE(PNODE%L2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%L2, DIM =2, KIND = LINT)
          DO J=1,NSIZE
            WRITE(IOUNIT, *)(PNODE%L2(I,J), I = 1,NDIM)
          END DO
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%S2))THEN
          NDIM  = SIZE(PNODE%S2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%S2, DIM =2, KIND = LINT)
          DO J=1,NSIZE
            WRITE(IOUNIT, *)("'",TRIM(PNODE%S2(I,J)),"' ", I = 1,NDIM)
          END DO
          SAVED = .TRUE.
      END IF
!
!  CHECK IF NODE IS CONSTANT
!
      IF(.NOT.SAVED)THEN
         IF(PNODE%NSIZE*PNODE%NDIM /= 0 .OR. PNODE%NLINK >0)THEN
          SELECT CASE(LTYPE)
           CASE('R')
              WRITE(IOUNIT,*)PNODE%R0
           CASE('D')
              WRITE(IOUNIT,*)PNODE%D0
           CASE('I')
              WRITE(IOUNIT,*)PNODE%I0
           CASE('J')
              WRITE(IOUNIT,*)PNODE%L0
           CASE('S','L')
               WRITE(IOUNIT,*)"'",PNODE%S0,"'"

           CASE DEFAULT 
         
           END SELECT
        END IF
       END IF


     RETURN
  END SUBROUTINE FLL_SAVE_NODE_A_FFA
  
  SUBROUTINE FLL_SAVE_NODE_B_FFA(PNODE, IOUNIT, POS, FPAR)
!
! Description: Writes node and its data to a binary file
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
! PNODE        Out        Pointer to node
! IOUNIT       In         Number of unit
! POS          In/Out     position in binary file
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
! 
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   INTEGER(LINT) :: POS
!
! Local declarations
!
   INTEGER(LINT) :: I,J,NDIM,NSIZE
   LOGICAL :: SAVED
   
   CHARACTER(LEN=TYPE_LENGTH) :: NTYPE ='N', LTYPE
   CHARACTER(LEN=SSTRING_LENGTH) :: SHTEXT
   
   SAVED = .FALSE.
   LTYPE = PNODE%LTYPE

   IF(TRIM(PNODE%LTYPE) == 'DIR' .OR.TRIM(PNODE%LTYPE) == 'N')THEN
      IF(PNODE%NLINK > 0)THEN 
         WRITE(IOUNIT)PNODE%LNAME,PNODE%FTYPE,1_LINT,1_LINT,PNODE%NDIM
          LTYPE = PNODE%FTYPE
      ELSE
         WRITE(IOUNIT)PNODE%LNAME,NTYPE,0_LINT,0_LINT,PNODE%NDIM
         RETURN
      END IF
   ELSE 
      IF(TRIM(LTYPE) == 'S') THEN
        IF(TRIM(PNODE%FTYPE) == 'L')THEN
           LTYPE = 'L'
        ELSE
           LTYPE ='S'
        END IF
      ELSE IF(TRIM(LTYPE) == 'L') THEN
        LTYPE ='J'
       END IF
       IF(PNODE%FTYPE /= '') LTYPE = PNODE%FTYPE
       
       IF( (PNODE%NSIZE * PNODE%NDIM > 1) )THEN
          SELECT CASE(LTYPE)
            CASE('D','R','I','J')
             LTYPE(2:2) = 'F'
          END SELECT
        END IF

        WRITE(IOUNIT)PNODE%LNAME,LTYPE,PNODE%NSIZE, PNODE%NDIM,0_LINT
     END IF
!
!  1 D ARRAYS
!
        IF(ASSOCIATED(PNODE%R1))THEN
          NDIM = SIZE(PNODE%R1, DIM =1, KIND = LINT)
          WRITE(IOUNIT)NDIM,(PNODE%R1(I), I = 1,NDIM)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%D1))THEN
          NDIM = SIZE(PNODE%D1, DIM =1, KIND = LINT)
          WRITE(IOUNIT)NDIM,(PNODE%D1(I), I = 1,NDIM)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%I1))THEN
          NDIM = SIZE(PNODE%I1, DIM =1, KIND = LINT)
          WRITE(IOUNIT)NDIM,(PNODE%I1(I), I = 1,NDIM)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%L1))THEN
          NDIM = SIZE(PNODE%L1, DIM =1, KIND = LINT)
          WRITE(IOUNIT)NDIM,(PNODE%L1(I), I = 1,NDIM)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%S1))THEN
          NDIM = SIZE(PNODE%S1, DIM =1, KIND = LINT)
          IF(PNODE%FTYPE == 'L')THEN
            WRITE(IOUNIT)NDIM*LSTRING_LENGTH,(PNODE%S1(I),I=1,NDIM)
          ELSE
            WRITE(IOUNIT)NDIM*SSTRING_LENGTH
            DO I = 1,NDIM
              SHTEXT = PNODE%S1(I)
              WRITE(IOUNIT)SHTEXT
            END DO
          END IF
          SAVED = .TRUE.
!
!  2D ARRAYS
!
       ELSE IF(ASSOCIATED(PNODE%R2))THEN
          NDIM  = SIZE(PNODE%R2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%R2, DIM =2, KIND = LINT)
          WRITE(IOUNIT)NDIM*NSIZE
          DO J=1,NSIZE
            WRITE(IOUNIT)(PNODE%R2(I,J), I = 1,NDIM)
          END DO
        SAVED = .TRUE.
      ELSE IF(ASSOCIATED(PNODE%D2))THEN
          NDIM  = SIZE(PNODE%D2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%D2, DIM =2, KIND = LINT)
          WRITE(IOUNIT)NDIM*NSIZE
          DO J=1,NSIZE
            WRITE(IOUNIT)(PNODE%D2(I,J), I = 1,NDIM)
          END DO
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%I2))THEN
          NDIM  = SIZE(PNODE%I2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%I2, DIM =2, KIND = LINT)
          WRITE(IOUNIT)NDIM*NSIZE
          DO J=1,NSIZE
            WRITE(IOUNIT)(PNODE%I2(I,J), I = 1,NDIM)
          END DO
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%L2))THEN
          NDIM  = SIZE(PNODE%L2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%L2, DIM =2, KIND = LINT)
          WRITE(IOUNIT)NDIM*NSIZE
          DO J=1,NSIZE
            WRITE(IOUNIT)(PNODE%L2(I,J), I = 1,NDIM)
          END DO
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%S2))THEN
          NDIM  = SIZE(PNODE%S2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%S2, DIM =2, KIND = LINT)

          IF(PNODE%FTYPE == 'L')THEN
             WRITE(IOUNIT)NDIM*NSIZE*LSTRING_LENGTH,((PNODE%S2(I,J), I = 1,NDIM),J=1,NSIZE)
         ELSE   
           WRITE(IOUNIT)NDIM*NSIZE*SSTRING_LENGTH
           DO J=1,NSIZE
              DO I=1,NDIM
                SHTEXT = PNODE%S2(I,J)
                WRITE(IOUNIT)SHTEXT
              END DO
            END DO
         ENDIF 
         SAVED = .TRUE.
      END IF
!
!  CHECK IF NODE IS CONSTANT
!
      IF(.NOT.SAVED)THEN
         IF(PNODE%NSIZE*PNODE%NDIM /= 0 .OR. PNODE%NLINK >0)THEN
          SELECT CASE(LTYPE)
           CASE('R')
              WRITE(IOUNIT)1_LINT,PNODE%R0
           CASE('D')
              WRITE(IOUNIT)1_LINT,PNODE%D0
           CASE('I')
              WRITE(IOUNIT)1_LINT,PNODE%I0
           CASE('J')
              WRITE(IOUNIT)1_LINT,PNODE%L0
           CASE('L')
               WRITE(IOUNIT)1_LINT*LSTRING_LENGTH,PNODE%S0
           CASE('S')
               WRITE(IOUNIT)1_LINT*SSTRING_LENGTH,PNODE%S0(1:SSTRING_LENGTH)
           END SELECT
        END IF
       END IF


     RETURN
  END SUBROUTINE FLL_SAVE_NODE_B_FFA

END MODULE FLL_WRITE_FFA_M

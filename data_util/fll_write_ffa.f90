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
!     Subroutine FLL_WRITE_FFA
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: prints node
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
MODULE FLL_WRITE_FFA_M
CONTAINS


  FUNCTION FLL_WRITE_FFA(PNODE,FILE,IOUNIT,FMT,FPAR) RESULT(OK)
   
    USE FLL_TYPE_M
    
    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   CHARACTER(*) :: FILE
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER :: FMT
   LOGICAL OK
   CHARACTER(32) :: FFVERSION='FFA-format-v2'
!
!   LOCAL TYPES
!
   CHARACTER :: FMT_LOC
   INTEGER :: ISTAT
   INTEGER(LINT) :: POS
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
           ACCESS='STREAM',IOSTAT=ISTAT)
       WRITE(IOUNIT)FFVERSION
    CASE('A')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='FORMATTED',&
           IOSTAT=ISTAT,ACTION='WRITE')
    END SELECT

    IF(ISTAT/=0) THEN
      WRITE(FPAR%MESG,'(A,A)')' Write  - error opening file ',TRIM(FILE)
      FPAR%SUCCESS = .FALSE.
      OK = .FALSE.
      RETURN
    END IF
!
!   WRITE  LINKED LIST
!
    CALL FLL_WRITE_FFA_LIST(PNODE,IOUNIT,FMT_LOC,FPAR)
    
    CLOSE(IOUNIT)
    IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A,A)')' Read  - error reading file ',TRIM(FILE)
       FPAR%SUCCESS = .FALSE.
       OK = .FALSE.
       RETURN
    END IF
    
    OK = .TRUE.
    RETURN
  
  END FUNCTION FLL_WRITE_FFA



   SUBROUTINE FLL_WRITE_FFA_LIST(PNODE,IOUNIT,FMT,FPAR)
   
    USE FLL_TYPE_M
    IMPLICIT NONE
!
!   SUBROUTINE WRITES LIST
!
   TYPE(DNODE), POINTER  :: PNODE,PCHILD
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER :: FMT   
   INTEGER(LINT) :: POS
!
!   LOCAL TYPES
!
!   
!   BODY OF SUBROUTINE
!
   POS = 0
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' CAT - null node '
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   IF(FMT == 'A')THEN
     CALL FLL_SAVE_NODE_A(PNODE, IOUNIT, FPAR)
   ELSE
     CALL FLL_SAVE_NODE_B(PNODE, IOUNIT, POS, FPAR)
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
  
     USE FLL_TYPE_M
     IMPLICIT NONE
!
!   SUBROUTINE REMOVES NODE
! 
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER(LINT) :: POS 
   
    TYPE(DNODE), POINTER  :: PCURR, PNEXT, PCHILD
    INTEGER :: IOUNIT
    CHARACTER :: FMT
!
!  IF NODE HAS CHILDREN
!
    PCURR => PNODE
!
!  IF CHILDREN, PRINT THEM TOO
!
    DO WHILE(ASSOCIATED(PCURR))
 
       IF(FMT == 'A')THEN
         CALL FLL_SAVE_NODE_A(PCURR, IOUNIT, FPAR)
       ELSE
         CALL FLL_SAVE_NODE_B(PCURR, IOUNIT, POS, FPAR)
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
  SUBROUTINE FLL_SAVE_NODE_A(PNODE, IOUNIT, FPAR)
    USE FLL_TYPE_M
    IMPLICIT NONE
!
!   SUBROUTINE SAVES NODE
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   
   INTEGER :: IOUNIT
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
          WRITE(IOUNIT, *)TRIM(PNODE%LNAME),",",TRIM(PNODE%FTYPE),' ,1, 1,',PNODE%NDIM
           LTYPE = PNODE%FTYPE
        ELSE
          WRITE(IOUNIT, *)TRIM(PNODE%LNAME),',N, 0, 0,',PNODE%NDIM
          RETURN
        END IF
     ELSE 
        IF(TRIM(LTYPE) == 'S') THEN
          IF(LTYPE == 'S')THEN
            LTYPE = 'S'
          ELSE
            LTYPE ='L'
          END IF
        ELSE IF(TRIM(LTYPE) == 'L') THEN
          LTYPE ='J'
        END IF
        LTYPE = PNODE%FTYPE

        WRITE(IOUNIT, *)TRIM(PNODE%LNAME),',', TRIM(LTYPE),',', PNODE%NSIZE, ',',PNODE%NDIM,',',0
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
               WRITE(IOUNIT,*)"'",PNODE%S,"'"

           CASE DEFAULT 
         
           END SELECT
        END IF
       END IF


     RETURN
  END SUBROUTINE FLL_SAVE_NODE_A
  
  SUBROUTINE FLL_SAVE_NODE_B(PNODE, IOUNIT, POS, FPAR)
    
    USE FLL_TYPE_M
    IMPLICIT NONE
!
!   SUBROUTINE SAVES NODE
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   
   INTEGER :: IOUNIT
   INTEGER(LINT) :: I,J,NDIM,NSIZE,POS
   LOGICAL :: SAVED
   
   CHARACTER(LEN=TYPE_LENGTH) :: NTYPE ='N', LTYPE
   CHARACTER(LEN=NAME_LENGTH) :: SHTEXT
   
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
        IF(LTYPE == 'S')THEN
           LTYPE = 'S'
        ELSE
           LTYPE ='L'
        END IF
      ELSE IF(TRIM(LTYPE) == 'L') THEN
        LTYPE ='J'
       END IF
       LTYPE = PNODE%FTYPE

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
            WRITE(IOUNIT)NDIM*STRING_LENGHT,(PNODE%S1(I),I=1,NDIM)
          ELSE
            WRITE(IOUNIT)NDIM*NAME_LENGTH
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
             WRITE(IOUNIT)NDIM*NSIZE*STRING_LENGHT,((PNODE%S2(I,J), I = 1,NDIM),J=1,NSIZE)
         ELSE   
           WRITE(IOUNIT)NDIM*NSIZE*NAME_LENGTH
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
               WRITE(IOUNIT)1_LINT*STRING_LENGHT,PNODE%S
           CASE('S')
               WRITE(IOUNIT)1_LINT*NAME_LENGTH,PNODE%S(1:NAME_LENGTH)
           END SELECT
        END IF
       END IF


     RETURN
  END SUBROUTINE FLL_SAVE_NODE_B

END MODULE FLL_WRITE_FFA_M

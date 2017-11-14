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
MODULE FLL_WRITE_M
!
! Description: Contains functions writing FLL native format file, ASCII and BINARY
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
  FUNCTION FLL_WRITE(PNODE,FILE,IOUNIT,FMT,FPAR, ERRMSG) RESULT(OK)
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
   CHARACTER(LEN=*), OPTIONAL :: ERRMSG
!
! local declarations
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
      WRITE(FPAR%MESG,'(A,A)')' Write unknown format',TRIM(FMT)
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
           ACCESS='STREAM',CONVERT='big_endian',IOSTAT=ISTAT)
    CASE('A')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='FORMATTED',&
           IOSTAT=ISTAT,ACTION='WRITE')
    END SELECT

    IF(ISTAT/=0) THEN
      WRITE(FPAR%MESG,'(A,A)')' Write error opening file ',TRIM(FILE)
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      OK = .FALSE.
      RETURN
    END IF
!
!   WRITE  LINKED LIST
!
    CALL FLL_WRITE_LIST(PNODE,IOUNIT,FMT_LOC,FPAR,LOC_ERRMSG)
    
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
  
  END FUNCTION FLL_WRITE



   SUBROUTINE FLL_WRITE_LIST(PNODE,IOUNIT,FMT,FPAR,LOC_ERRMSG)
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
   TYPE(DNODE), POINTER  :: PNODE,PCHILD
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER :: FMT 
   CHARACTER(LEN=*) :: LOC_ERRMSG
!
! Local declarations
!   
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
     CALL FLL_SAVE_NODE_A(PNODE, IOUNIT, FPAR)
   ELSE
     CALL FLL_SAVE_NODE_B(PNODE, IOUNIT, POS, FPAR)
   END IF     

   PCHILD => PNODE%PCHILD
!
! IF NODE HAS CHILDREN PRINT THEM TOO
!
   IF(ASSOCIATED(PCHILD))CALL FLL_WRITE_RECURSIVE_NODE(PCHILD,IOUNIT,POS,FMT,FPAR,LOC_ERRMSG)

   FPAR%SUCCESS = .TRUE.

   RETURN
   END SUBROUTINE FLL_WRITE_LIST
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE FLL_WRITE_RECURSIVE_NODE(PNODE,IOUNIT,POS,FMT,FPAR,LOC_ERRMSG)
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
    INTEGER :: IOUNIT
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER(LINT) :: POS
    CHARACTER :: FMT   
    CHARACTER(LEN=*) :: LOC_ERRMSG 
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
         CALL FLL_SAVE_NODE_A(PCURR, IOUNIT, FPAR)
       ELSE
         CALL FLL_SAVE_NODE_B(PCURR, IOUNIT, POS, FPAR)
       END IF  

       PNEXT  => PCURR%PNEXT
       PCHILD => PCURR%PCHILD
       IF(ASSOCIATED(PCHILD))THEN
         CALL FLL_WRITE_RECURSIVE_NODE(PCHILD,IOUNIT,POS,FMT,FPAR,LOC_ERRMSG)
       END IF
       
       PCURR => PNEXT
    END DO
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE FLL_WRITE_RECURSIVE_NODE
!
!  FREE MEMORY FOR NODE
!
  SUBROUTINE FLL_SAVE_NODE_A(PNODE, IOUNIT, FPAR)
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
!  Local declarations
!
   INTEGER(LINT) :: I,J,NDIM,NSIZE
   LOGICAL :: SAVED
   
   SAVED = .FALSE.
!
!   1D ARRAYS
!
     IF(TRIM(PNODE%LTYPE) == 'DIR' .OR.TRIM(PNODE%LTYPE) == 'N')THEN
        WRITE(IOUNIT, *)TRIM(PNODE%LNAME),'  ', TRIM(PNODE%LTYPE),'  ',  PNODE%NDIM
     ELSE 
        WRITE(IOUNIT, *)TRIM(PNODE%LNAME),'  ', TRIM(PNODE%LTYPE),'  ', PNODE%NDIM, '  ',PNODE%NSIZE
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
          WRITE(IOUNIT, *)((PNODE%R2(I,J), J = 1,NSIZE), I=1,NDIM)
           SAVED = .TRUE.
      ELSE IF(ASSOCIATED(PNODE%D2))THEN
          NDIM  = SIZE(PNODE%D2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%D2, DIM =2, KIND = LINT)
          WRITE(IOUNIT, *)((PNODE%D2(I,J), J = 1,NSIZE), I=1,NDIM)
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%I2))THEN
          NDIM  = SIZE(PNODE%I2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%I2, DIM =2, KIND = LINT)
          WRITE(IOUNIT, *)((PNODE%I2(I,J), J = 1,NSIZE), I=1,NDIM)
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%L2))THEN
          NDIM  = SIZE(PNODE%L2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%L2, DIM =2, KIND = LINT)
          WRITE(IOUNIT, *)((PNODE%L2(I,J), J = 1,NSIZE), I=1,NDIM)
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%S2))THEN
          NDIM  = SIZE(PNODE%S2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%S2, DIM =2, KIND = LINT)
          DO I = 1,NDIM
            WRITE(IOUNIT, *)("'",TRIM(PNODE%S2(I,J)),"' ", J = 1,NSIZE)
          END DO
          SAVED = .TRUE.
      END IF
!
!  CHECK IF NODE IS CONSTANT
!
      IF(.NOT.SAVED)THEN
        SELECT CASE(PNODE%LTYPE)
         CASE('R')
          WRITE(IOUNIT, *)PNODE%R0
         CASE('D')
          WRITE(IOUNIT, *)PNODE%D0
         CASE('I')
          WRITE(IOUNIT, *)PNODE%I0
         CASE('L')
          WRITE(IOUNIT, *)PNODE%L0
        CASE('S')
          WRITE(IOUNIT,*)"'",TRIM(PNODE%S0),"'"

         CASE DEFAULT 
         
         END SELECT
       END IF


     RETURN
  END SUBROUTINE FLL_SAVE_NODE_A
  
  SUBROUTINE FLL_SAVE_NODE_B(PNODE, IOUNIT, POS, FPAR)
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
   
   SAVED = .FALSE.
!
!  Header
!
        WRITE(IOUNIT)PNODE%LNAME,PNODE%LTYPE, PNODE%NDIM,PNODE%NSIZE
        IF(TRIM(PNODE%LNAME) == 'DIR' .OR. TRIM(PNODE%LNAME) == 'N')RETURN 
!
!  1 D ARRAYS
!
        IF(ASSOCIATED(PNODE%R1))THEN
          WRITE(IOUNIT)PNODE%R1(:)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%D1))THEN
          WRITE(IOUNIT)PNODE%D1(:)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%I1))THEN
          WRITE(IOUNIT)PNODE%I1(:)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%L1))THEN
          WRITE(IOUNIT)PNODE%L1(:)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%S1))THEN
          NDIM = SIZE(PNODE%S1, DIM =1, KIND = LINT)
          DO I = 1,NDIM
            WRITE(IOUNIT)PNODE%S1(I)
          END DO          
          SAVED = .TRUE.
!
!  2D ARRAYS
!
       ELSE IF(ASSOCIATED(PNODE%R2))THEN
!          NDIM  = SIZE(PNODE%R2, DIM =1, KIND = LINT)
!          NSIZE = SIZE(PNODE%R2, DIM =2, KIND = LINT)
!          WRITE(IOUNIT)((PNODE%R2(I,J), J = 1,NSIZE), I=1,NDIM)
           WRITE(IOUNIT)PNODE%R2(:,:)
           SAVED = .TRUE.
      ELSE IF(ASSOCIATED(PNODE%D2))THEN
!          NDIM  = SIZE(PNODE%D2, DIM =1, KIND = LINT)
!          NSIZE = SIZE(PNODE%D2, DIM =2, KIND = LINT)
!          WRITE(IOUNIT)((PNODE%D2(I,J), J = 1,NSIZE), I=1,NDIM)
           WRITE(IOUNIT)PNODE%D2(:,:)
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%I2))THEN
!          NDIM  = SIZE(PNODE%I2, DIM =1, KIND = LINT)
!          NSIZE = SIZE(PNODE%I2, DIM =2, KIND = LINT)
!          WRITE(IOUNIT)((PNODE%I2(I,J), J = 1,NSIZE), I=1,NDIM)
           WRITE(IOUNIT)PNODE%I2(:,:)
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%L2))THEN
!          NDIM  = SIZE(PNODE%L2, DIM =1, KIND = LINT)
!          NSIZE = SIZE(PNODE%L2, DIM =2, KIND = LINT)
!          WRITE(IOUNIT)((PNODE%L2(I,J), J = 1,NSIZE), I=1,NDIM)
           WRITE(IOUNIT)PNODE%L2(:,:)
           SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%S2))THEN
          NDIM  = SIZE(PNODE%S2, DIM =1, KIND = LINT)
          NSIZE = SIZE(PNODE%S2, DIM =2, KIND = LINT)
          DO J=1,NSIZE
            WRITE(IOUNIT)(PNODE%S2(I,J), I = 1,NDIM)
          END DO           
          SAVED = .TRUE.
      END IF
!
!  CHECK IF NODE IS CONSTANT
!
      IF(.NOT.SAVED)THEN
        SELECT CASE(PNODE%LTYPE)
         CASE('R')
            WRITE(IOUNIT)PNODE%R0
         CASE('D')
            WRITE(IOUNIT)PNODE%D0
         CASE('I')
            WRITE(IOUNIT)PNODE%I0
         CASE('L')
            WRITE(IOUNIT)PNODE%L0
         CASE('S')
            WRITE(IOUNIT)PNODE%S0

         CASE DEFAULT 
         
         END SELECT
       END IF


     RETURN
  END SUBROUTINE FLL_SAVE_NODE_B

END MODULE FLL_WRITE_M

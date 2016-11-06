!
!     Copyright (C) 2016  Adam Jirasek
! 
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU Lesser General Public License as published by
!     the Rree Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
! 
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or RITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU Lesser General Public License for more details.
! 
!     You should have received a copy of the GNU Lesser General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.
!     
!     contact: libm3l@gmail.com
! 
!

!
!     Subroutine FLL_READ
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: reads a file
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
MODULE FLL_READ_M
!
! Description: Contains functions reading FLL native format file, ASCII and BINARY
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

  FUNCTION FLL_READ(FILE,IOUNIT,FMT,FPAR) RESULT(PNODE)
!
! Description: main function opening, reading and closing file
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
!
! Local declarations
!
   LOGICAL :: OK
   CHARACTER :: FMT_LOC
   INTEGER :: ISTAT
   INTEGER(LINT) :: POS

   
   INQUIRE (FILE=TRIM(FILE), EXIST=OK)
   IF(.NOT.OK) THEN
      WRITE(FPAR%MESG,'(A,A)')' Read  - file does not exist ',TRIM(FILE)
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
   END IF
!
!   DETERMINE RORMAT
!
   SELECT CASE(FMT)
   CASE('A','a')
     FMT_LOC = 'A'
   CASE('B','b')
      FMT_LOC = 'B'
    CASE('U','u','*')
      FMT_LOC = 'U'
    CASE DEFAULT
      WRITE(FPAR%MESG,'(A,A)')' Read  - unknown format',TRIM(FMT)
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
    END SELECT
!
!   OPEN THE FILE
!
   SELECT CASE(FMT_LOC)
    CASE('B')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='UNFORMATTED',&
           ACCESS='STREAM',IOSTAT=ISTAT)
    CASE('A')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='FORMATTED',&
           IOSTAT=ISTAT, ACTION = 'READ')
    END SELECT

    IF(ISTAT/=0) THEN
      WRITE(FPAR%MESG,'(A,A)')' Read  - error opening file ',TRIM(FILE)
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
    END IF
!
!   READ INITIAL NODE
!
    PNODE => READ_NODE(IOUNIT,FMT_LOC,POS,FPAR)
    
    CLOSE(IOUNIT)
    IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A,A)')' Read  - error reading file ',TRIM(FILE)
       CALL FLL_OUT('ALL',FPAR)
       FPAR%SUCCESS = .FALSE.
    END IF
    
    RETURN
  
  END FUNCTION FLL_READ
!
!  READS NODE
!
  RECURSIVE FUNCTION READ_NODE(IOUNIT,FMT,POS,FPAR) RESULT(PNODE)
!
! Description: Function reads a node
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
    USE FLL_MK_M
    USE FLL_MV_M
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
! POS          In/Out     Position in bindary file
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
    INTEGER(LINT),    INTENT(OUT)  :: POS
    TYPE(DNODE), POINTER :: PNODE
    CHARACTER :: FMT
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER :: IOUNIT
!
! Local declarations
!
    TYPE(DNODE), POINTER :: PNEW
    TYPE(FUNC_DATA_SET) :: FPAR_H
    CHARACTER(LEN=NAME_LENGTH)   :: NAME
    CHARACTER(LEN=TYPE_LENGTH)     :: LTYPE
    INTEGER(LINT) :: NDIM, NSIZE,NNODES
    LOGICAL :: OK
!
!  READ HEADER
!
    CALL READ_HEADER(IOUNIT,FMT,POS,NAME,LTYPE,NDIM,NSIZE,FPAR_H)
    
    IF(.NOT.FPAR_H%SUCCESS)THEN
      WRITE(FPAR%MESG,'(A)')' Read  - error reading header '
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
    END IF
    
    IF(TRIM(LTYPE) == 'DIR' .OR. TRIM(LTYPE) == 'N')THEN
       PNODE => FLL_MK(NAME,LTYPE,0_LINT,0_LINT,FPAR_H)
    ELSE
      PNODE => FLL_MK(NAME,LTYPE,NDIM,NSIZE,FPAR_H)
    END IF

    IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' Read  - error allocating node '
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
    END IF
    

    IF(TRIM(LTYPE) == 'DIR' .OR. TRIM(LTYPE) == 'N')THEN
      DO NNODES = 1,NDIM
        PNEW => READ_NODE(IOUNIT,FMT,POS,FPAR)
        IF(.NOT.ASSOCIATED(PNEW))STOP ' ERROR READING NODE'
!
!   ATTACH TO PNODE
!
        OK = FLL_MV(PNEW,PNODE,FPAR)
        IF(.NOT.OK) STOP' ERROR MV'

      END DO
    ELSE
!
!  READ DATA 
!
      SELECT CASE(FMT)
      CASE('A')
         CALL READ_DATA_ASCII(IOUNIT,PNODE,PNODE%LTYPE,PNODE%NDIM,PNODE%NSIZE,FPAR_H)
      CASE('B')
        CALL READ_DATA_BIN(IOUNIT,PNODE,PNODE%LTYPE,PNODE%NDIM,PNODE%NSIZE,FPAR_H)
      END SELECT
    
    END IF
    
    RETURN
    
  END FUNCTION READ_NODE
!
!  READ HEADER OR EACH NODE
!
   SUBROUTINE READ_HEADER(IOUNIT,FMT,POS,NAME,LTYPE,NDIM,NSIZE,FPAR)
!
! Description: Reads header of each node
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
    USE FLL_FUNC_PRT_M
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
! NAME         In         name of node
! LTYPE        In         type of node
! NDIM         In         1st dimension of array in the node
! NSIZE        In         2nd dimension of array in the node
! POS          In/Out     Position in bindary file
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!    
    INTEGER(LINT) :: POS
    CHARACTER :: FMT
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER :: IOUNIT   
    CHARACTER(*)     :: LTYPE
    CHARACTER(*)     :: NAME
    INTEGER(LINT) :: NDIM, NSIZE
!
! Local declarations
!
    INTEGER(LINT) :: ISTART,IIND
    CHARACTER*255 :: TEXT_LINE,TRIM_LINE
    INTEGER :: IOSTAT
    LOGICAL :: OK
!
!  body of function
!   
    SELECT CASE(FMT)
    CASE('A')
      TEXT_LINE(1:1) = '*'
      DO WHILE(TEXT_LINE(1:1)=='*') ! IGNORE COMMENT TEXT_LINE
        READ(IOUNIT,'(A256)',IOSTAT=IOSTAT) TEXT_LINE
        OK = TEST_IOSTAT(IOSTAT,FPAR)
        IF(.NOT.OK) RETURN
      END DO
      
      TRIM_LINE = TRIM(TEXT_LINE)
!
!  GET NAME
!
      IIND = 1
      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO 
      ISTART = IIND
      
      DO WHILE(TRIM_LINE(IIND:IIND) /= ' ')
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      NAME = TRIM_LINE(ISTART:IIND)

      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND
!
!  GET TYPE
!
      DO WHILE(TRIM_LINE(IIND:IIND) /= ' ')
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      LTYPE = TRIM_LINE(ISTART:IIND) 

      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND
!
!  GET NDIM
!
       DO WHILE(TRIM_LINE(IIND:IIND) /= ' ')
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      READ(TRIM_LINE(ISTART:IIND),'(I30)',IOSTAT=IOSTAT) NDIM     
      
      IF(TRIM(LTYPE) == 'DIR' .OR. TRIM(LTYPE) == 'N')THEN
        NSIZE = 0
        RETURN
      END IF      
      
      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND     
!
!  GET NSIZE
!
       DO WHILE(TRIM_LINE(IIND:IIND) /= ' ')
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      READ(TRIM_LINE(ISTART:IIND),'(I30)',IOSTAT=IOSTAT) NSIZE     
      
      FPAR%SUCCESS = .TRUE.
      RETURN
      
    CASE('B')
       NSIZE = 0
       READ(IOUNIT,IOSTAT=IOSTAT)NAME,LTYPE,NDIM,nsize
      FPAR%SUCCESS = .TRUE.
      RETURN
    END SELECT
    
    WRITE(FPAR%MESG,'(A)')' Read  - reading header error '
    CALL FLL_OUT('ALL',FPAR)
    FPAR%SUCCESS = .FALSE.
    RETURN
    
    WRITE(FPAR%MESG,'(A)')' Read  - reading header error, reached end of file '
    CALL FLL_OUT('ALL',FPAR)
    FPAR%SUCCESS = .FALSE.
    RETURN
   
   END SUBROUTINE READ_HEADER
!
!  READ DATA 
!
    SUBROUTINE READ_DATA_ASCII(IOUNIT,PNODE,LTYPE,NDIM,NSIZE,FPAR)
!
! Description: Reads data contatined in node Pnode - ASCII file
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
    USE FLL_FUNC_PRT_M
   
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         Pointer to node
! IOUNIT       In         Number of unit
! NAME         In         name of node
! LTYPE        In         type of node
! NDIM         In         1st dimension of array in the node
! NSIZE        In         2nd dimension of array in the node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
    TYPE(DNODE), POINTER :: PNODE
    INTEGER :: IOUNIT
    INTEGER(LINT) :: NDIM,NSIZE
    CHARACTER(LEN=TYPE_LENGTH) :: LTYPE
    TYPE(FUNC_DATA_SET) :: FPAR
!
! Local declarations
!
    INTEGER(LINT) :: I,J
    INTEGER :: IOSTAT
    LOGICAL :: OK
!
!  BODY
!
    SELECT CASE(LTYPE)
     CASE('R')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%R1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)PNODE%R0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%R1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%R2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF

     CASE('D')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%D1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)PNODE%D0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%D1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%D2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
       
     CASE('I')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%I1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)PNODE%I0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%I1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%I2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
       
     CASE('L')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%L1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)PNODE%L0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%L1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%L2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
       

      CASE('S')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%S1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)PNODE%S
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%S1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%S2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF

      CASE('C')

      CASE('N','DIR')
         RETURN
         
      CASE DEFAULT
            WRITE(*,*)' WRONG TYPE'
     
     END SELECT
     
      OK = TEST_IOSTAT(IOSTAT,FPAR)
     
   RETURN

   END SUBROUTINE READ_DATA_ASCII
!
!  READ DATA 
!
    SUBROUTINE READ_DATA_BIN(IOUNIT,PNODE,LTYPE,NDIM,NSIZE,FPAR)
!
! Description: Function reads data contained in Pnode, bindary file
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
    USE FLL_FUNC_PRT_M
   
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         Pointer to node
! IOUNIT       In         Number of unit
! LTYPE        In         type of node
! NDIM         In         1st dimension of array in the node
! NSIZE        In         2nd dimension of array in the node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!    
    TYPE(DNODE), POINTER :: PNODE
    INTEGER :: IOUNIT
    INTEGER(LINT) :: NDIM,NSIZE
    CHARACTER(*) :: LTYPE
    TYPE(FUNC_DATA_SET) :: FPAR
!
!  Local declarations
!
    INTEGER(LINT) :: I,J
    INTEGER :: IOSTAT
    LOGICAL :: OK
!
!  BODY
!
    SELECT CASE(LTYPE)
     CASE('R')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%R1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)PNODE%R0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%R1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)((PNODE%R2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF

     CASE('D')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%D1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)PNODE%D0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%D1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)((PNODE%D2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
       
     CASE('I')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%I1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)PNODE%I0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%I1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)((PNODE%I2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
       
     CASE('L')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%L1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)PNODE%L0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%L1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)((PNODE%L2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
       
      CASE('S')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%S1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)PNODE%S
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)(PNODE%S1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)((PNODE%S2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF

      CASE('C')

      CASE('N','DIR')
         RETURN
         
      CASE DEFAULT
            WRITE(*,*)' WRONG TYPE'
     
     END SELECT
     
      OK = TEST_IOSTAT(IOSTAT,FPAR)
     
   RETURN

   END SUBROUTINE READ_DATA_BIN
  
END MODULE FLL_READ_M

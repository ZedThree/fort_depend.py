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
MODULE FLL_READ_FFA_M
!
! Description: Contains functions reading FFA native format file, ASCII and BINARY
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
  FUNCTION FLL_READ_FFA(FILE,IOUNIT,FMT,FPAR,SCAN) RESULT(PNODE)
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
! SCAN         In         Optional parameter - scan only file
!
! Arguments declaration
!
   CHARACTER(*) :: FILE
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER :: FMT
   CHARACTER, OPTIONAL :: SCAN
!
! Local declarations
!
   LOGICAL :: OK
   CHARACTER :: FMT_LOC
   INTEGER :: ISTAT
   INTEGER(LINT) :: POS
   CHARACTER :: SCAN_LOC

   
   INQUIRE (FILE=TRIM(FILE), EXIST=OK)
   IF(.NOT.OK) THEN
      WRITE(FPAR%MESG,'(A,A)')' Read-ffa -  file does not exist ',TRIM(FILE)
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
   END IF

   POS = 1
!
!   DETERMINE RORMAT'
!
   SELECT CASE(FMT)
   CASE('A','a')
     FMT_LOC = 'A'
   CASE('B','b')
      FMT_LOC = 'B'
    CASE('U','u','*')
      FMT_LOC = 'U'
    CASE DEFAULT
      WRITE(FPAR%MESG,'(A,A)')' Read-ffa -  unknown format',TRIM(FMT)
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
    END SELECT

    IF(PRESENT(SCAN))THEN
      SCAN_LOC = SCAN
    ELSE
      SCAN_LOC = 'N'
    END IF
!
!   OPEN THE FILE
!
   SELECT CASE(FMT_LOC)
    CASE('B')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='UNFORMATTED',&
           ACCESS='STREAM',CONVERT='big_endian',IOSTAT=ISTAT)
    CASE('A')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='FORMATTED',&
           IOSTAT=ISTAT, ACTION = 'READ')
    END SELECT

    IF(ISTAT/=0) THEN
      WRITE(FPAR%MESG,'(A,A)')' Read-ffa -  error opening file ',TRIM(FILE)
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
    END IF
!
!   READ INITIAL NODE
!
    PNODE => READ_NODE_FFA(IOUNIT,FMT_LOC,POS,SCAN_LOC,FPAR)
    
    CLOSE(IOUNIT)
    IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A,A)')' Read-ffa  - error opening file ',TRIM(FILE)
       CALL FLL_OUT('ALL',FPAR)
       FPAR%SUCCESS = .FALSE.
    END IF
    
    RETURN
  
  END FUNCTION FLL_READ_FFA
!
!  READS NODE
!
  RECURSIVE FUNCTION READ_NODE_FFA(IOUNIT,FMT,POS,SCAN,FPAR) RESULT(PNODE)
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
! POS          In/Out     Position in binary file
! FPAR         In/Out     structure containing function specific data
! SCAN         In         scan only
!
! Arguments declaration
!    
    INTEGER(LINT),    INTENT(OUT)  :: POS
    TYPE(DNODE), POINTER :: PNODE
    CHARACTER :: FMT
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER :: IOUNIT
    CHARACTER :: SCAN
!
! Local declarations
!
    TYPE(DNODE), POINTER :: PNEW
    TYPE(FUNC_DATA_SET) :: FPAR_H
    CHARACTER(LEN=SSTRING_LENGTH)  :: NAME
    CHARACTER(LEN=TYPE_LENGTH)     :: LTYPE,FTYPE
    INTEGER(LINT) :: NDIM, NSIZE,NNODES,NDIMO,NSIZEO
    LOGICAL :: OK,EXTRALINE
    INTEGER(LINT) :: NSUB
!
!  READ HEADER
!
    CALL READ_HEADER_FFA(IOUNIT,FMT,POS,NAME,LTYPE,FTYPE,NDIM,NSIZE,NSUB,NDIMO,NSIZEO,EXTRALINE,FPAR_H)
    
    IF(.NOT.FPAR_H%SUCCESS)THEN
      WRITE(FPAR%MESG,'(A)')' Read-ffa -  error reading header '
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
      WRITE(FPAR%MESG,'(A)')' Read-ffa -  error allocating node '
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      STOP
      RETURN
    END IF
    
    PNODE%FTYPE = FTYPE

    IF(TRIM(LTYPE) == 'DIR' .OR. TRIM(LTYPE) == 'N')THEN
  
      IF(EXTRALINE)THEN
       PNODE%NLINK = NSUB 
       SELECT CASE(FMT)
         CASE('A')
           CALL READ_DATA_FFA_ASCII(IOUNIT,PNODE,PNODE%FTYPE,NDIMO,NSIZEO,FPAR_H)
         CASE('B')
           CALL READ_DATA_FFA_BIN(IOUNIT,PNODE,PNODE%FTYPE,NDIMO,NSIZEO,POS,FPAR_H)
        END SELECT
      END IF

      DO NNODES = 1,NDIM
        PNEW => READ_NODE_FFA(IOUNIT,FMT,POS,SCAN,FPAR)
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
         CALL READ_DATA_FFA_ASCII(IOUNIT,PNODE,PNODE%FTYPE,PNODE%NDIM,PNODE%NSIZE,FPAR_H)
      CASE('B')
        IF(SCAN /= 'Y')THEN
        CALL READ_DATA_FFA_BIN(IOUNIT,PNODE,PNODE%FTYPE,PNODE%NDIM,PNODE%NSIZE,POS,FPAR_H)
        ELSE
          IF(PNODE%NSIZE * PNODE%NDIM == 1)THEN
            CALL READ_DATA_FFA_BIN(IOUNIT,PNODE,PNODE%FTYPE,PNODE%NDIM,PNODE%NSIZE,POS,FPAR_H)         
          ELSE
            POS = POS + GET_NEW_FFA_POS(PNODE,PNODE%LTYPE,PNODE%NDIM,PNODE%NSIZE,FPAR_H)
          END IF
        END IF

      END SELECT
    
    END IF
    
    RETURN
    
  END FUNCTION READ_NODE_FFA
!
!  READ HEADER OR EACH NODE
!
   SUBROUTINE READ_HEADER_FFA(IOUNIT,FMT,POS,NAME,LTYPE,FTYPE,NDIM,NSIZE,NLINK,NDIMO,NSIZEO,EXTRALINE,FPAR)
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
! NSIZE        In         2nd dimension of array in the node
! NLINK        In         = NSUB in FFA format
! NDIM0        In         original value of NDIM from FFA format
! NSIZE0       In         original value of NSIZE from FFA format
! EXTRALINE    In/Out     If node node N but has NSUB > 0 read extra line
! FPAR         In/Out     structure containing function specific data
! POS          In         position in file
!
! Arguments declaration
!     
    CHARACTER :: FMT
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER :: IOUNIT
    LOGICAL :: EXTRALINE
    INTEGER(LINT) :: NDIM, NSIZE,NLINK,NDIMO,NSIZEO,POS
    CHARACTER(*)  :: LTYPE
    CHARACTER(*)  :: NAME
!
! Local declarations
!
    CHARACTER(*)  :: FTYPE
    INTEGER(LINT) :: ISTART,IIND,I
    CHARACTER*255 :: TEXT_LINE,TRIM_LINE
    CHARACTER*32  :: VER
    INTEGER :: IOSTAT
    LOGICAL :: OK

    EXTRALINE = .FALSE.
    
    SELECT CASE(FMT)
    CASE('A')
      TEXT_LINE(1:1) = '*'
      DO WHILE(TEXT_LINE(1:1)=='*') ! IGNORE COMMENT TEXT_LINE
        READ(IOUNIT,'(A256)',IOSTAT=IOSTAT) TEXT_LINE
        OK = TEST_IOSTAT(IOSTAT,FPAR)
        IF(.NOT.OK) RETURN
      END DO
            
      TRIM_LINE = TRIM(TEXT_LINE)
      
      DO I=1,255
        IF(TRIM_LINE(I:I) == ',') TRIM_LINE(I:I) =' '
      END DO
!
!  GET NAME
!
      IIND = 1
      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO 
      ISTART = IIND
      
      DO WHILE(TRIM_LINE(IIND:IIND) /= ' ' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      NAME = TRIM(TRIM_LINE(ISTART:IIND))

      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND
!
!  GET TYPE
!
      DO WHILE(TRIM_LINE(IIND:IIND) /= ' ' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      LTYPE = TRIM(TRIM_LINE(ISTART:IIND))
      LTYPE(2:) = ' '
      FTYPE = LTYPE

      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND
!
!  GET NSIZE
!
       DO WHILE(TRIM_LINE(IIND:IIND) /= ' ' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO

      READ(TRIM_LINE(ISTART:IIND-1),'(I30)',IOSTAT=IOSTAT) NSIZE 
      
      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND   
!
!  GET NDIM
!
       DO WHILE(TRIM_LINE(IIND:IIND) /= ' ' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      READ(TRIM_LINE(ISTART:IIND-1),'(I30)',IOSTAT=IOSTAT) NDIM
      IIND = IIND + 1     
       
      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND

      NSIZEO = NSIZE
      NDIMO = NDIM
!
!  GET NLINK
!
      DO WHILE(TRIM_LINE(IIND:IIND) /= ' ' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO

      READ(TRIM_LINE(ISTART:IIND-1),'(I30)',IOSTAT=IOSTAT) NLINK   
!
!  SWTICH FORMAT SPECIFIC DATA TO CURRENT DATA
!
      IF(TRIM(LTYPE) == 'DIR' .OR. TRIM(LTYPE) == 'N')THEN
        NDIM   = NLINK
      ELSE
      
         IF(TRIM(LTYPE) == 'L')THEN 
            LTYPE ='S'
         ELSE IF(TRIM(LTYPE) == 'J')THEN 
            LTYPE ='L'
         END IF
      
         IF(NLINK > 0  )THEN
           EXTRALINE = .TRUE.
           LTYPE = 'DIR'
           NDIM   = NLINK
          END IF
      END IF
      
      FPAR%SUCCESS = .TRUE.
      RETURN
!
!  binary
!
    CASE('B')
       NSIZE = 0
       READ(IOUNIT,IOSTAT=IOSTAT, POS=POS)NAME,LTYPE,NSIZE,NDIM,NLINK

       IF(POS == 1)THEN

         IF(TRIM(NAME) == 'FFA-format-v2')THEN 
!
!  DISREGARD, THIS IS JUST INFO ABOUT VERSION
!  CONSIDER HAVING VERSION ONLY AT THE BEGINING OF THE FILE
!
            REWIND(IOUNIT)
            READ(IOUNIT,IOSTAT=IOSTAT)VER
            READ(IOUNIT,IOSTAT=IOSTAT)NAME,LTYPE,NSIZE,NDIM,NLINK
         ELSE
     	    WRITE(FPAR%MESG,'(A)')' Read-ffa - error reading ffa header '
    	    CALL FLL_OUT('ALL',FPAR)
    	    FPAR%SUCCESS = .FALSE.
    	    RETURN

         END IF
       
       END IF
       INQUIRE(UNIT = IOUNIT, POS=POS)
      
       LTYPE(2:) = ' '
       FTYPE = LTYPE
       NSIZEO = NSIZE
       NDIMO = NDIM
       
       IF(TRIM(LTYPE) == 'DIR' .OR. TRIM(LTYPE) == 'N')THEN
         NDIM   = NLINK
       ELSE
      
         IF(TRIM(LTYPE(1:1)) == 'L')THEN 
           LTYPE ='S'
         ELSE IF(TRIM(LTYPE(1:1)) == 'J')THEN 
           LTYPE ='L'
         END IF
      
         IF(NLINK > 0  )THEN
           EXTRALINE = .TRUE.
           LTYPE = 'DIR'
           NDIM   = NLINK
         END IF
      END IF

      FPAR%SUCCESS = .TRUE.
      
      RETURN
    END SELECT
    
    WRITE(FPAR%MESG,'(A)')' Read-ffa -  reading header error '
    CALL FLL_OUT('ALL',FPAR)
    FPAR%SUCCESS = .FALSE.
    RETURN
    
    WRITE(FPAR%MESG,'(A)')' Read-ffa -  reading header error, reached end of file '
    CALL FLL_OUT('ALL',FPAR)
    FPAR%SUCCESS = .FALSE.
    RETURN
   
   END SUBROUTINE READ_HEADER_FFA
!
!  READ DATA 
!
    SUBROUTINE READ_DATA_FFA_ASCII(IOUNIT,PNODE,LTYPE,NDIM,NSIZE,FPAR)
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
    IF(NDIM*NSIZE == 0)RETURN
    SELECT CASE(LTYPE(1:1))
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
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%R2(I,J),I=1,NDIM),J=1,NSIZE)
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
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%D2(I,J),I=1,NDIM),J=1,NSIZE)
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
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%I2(I,J),I=1,NDIM),J=1,NSIZE)
         END IF
       END IF
       
       
     CASE('J')
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
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%L2(I,J),I=1,NDIM),J=1,NSIZE)
         END IF
       END IF
       
       
      CASE('S','L')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%S1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)PNODE%S0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,*,IOSTAT=IOSTAT)(PNODE%S1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,*,IOSTAT=IOSTAT)((PNODE%S2(I,J),I=1,NDIM),J=1,NSIZE)
         END IF
       END IF

      CASE('C')

      CASE('N','DIR')
         RETURN
         
      CASE DEFAULT
            WRITE(*,*)' WRONG TYPE ', pnode%lname, pnode%ltype
     
     END SELECT
     
      OK = TEST_IOSTAT(IOSTAT,FPAR)
     
   RETURN

   END SUBROUTINE READ_DATA_FFA_ASCII



    SUBROUTINE READ_DATA_FFA_BIN(IOUNIT,PNODE,LTYPE,NDIM,NSIZE,POS,FPAR)
!
! Description: Function reads data contained in Pnode, binary file
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
! POS          In         Position in file
!
! Arguments declaration
!    
    TYPE(DNODE), POINTER :: PNODE
    INTEGER :: IOUNIT
    INTEGER(LINT) :: NDIM,NSIZE,NINTEG,POS
    CHARACTER(*) :: LTYPE
    TYPE(FUNC_DATA_SET) :: FPAR
!
!  Local declarations
!
    CHARACTER(LEN=SSTRING_LENGTH) :: T
    INTEGER(LINT) :: I,J
    INTEGER :: IOSTAT
    LOGICAL :: OK
!
!  BODY
!
    IF(NDIM*NSIZE == 0)RETURN
    SELECT CASE(LTYPE(1:1))
     CASE('R')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%R1(I),I=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,PNODE%R0
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%R1(I),I=1,NDIM)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,((PNODE%R2(I,J),I=1,NDIM),J=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       END IF

     CASE('D')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%D1(I),I=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,PNODE%D0
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%D1(I),I=1,NDIM)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,((PNODE%D2(I,J),I=1,NDIM),J=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       END IF
       
       
     CASE('I')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%I1(I),I=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,PNODE%I0
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%I1(I),I=1,NDIM)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,((PNODE%I2(I,J),I=1,NDIM),J=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       END IF
       
       
     CASE('J')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%L1(I),I=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,PNODE%L0
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%L1(I),I=1,NDIM)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,((PNODE%L2(I,J),I=1,NDIM),J=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       END IF
       
      CASE('S')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG
           DO I=1,NSIZE
             READ(IOUNIT,IOSTAT=IOSTAT)T
             PNODE%S1(I) = ' '
             PNODE%S1(I) = T
           END DO
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,T
           INQUIRE(UNIT = IOUNIT, POS=POS)
           PNODE%S0 = T
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG
           DO I=1,NDIM
             READ(IOUNIT,IOSTAT=IOSTAT)T
             PNODE%S1(I) = ' '
             PNODE%S1(I) = T
           END DO
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG
           DO J=1,NSIZE
             DO I=1,NDIM
              READ(IOUNIT,IOSTAT=IOSTAT)T
              PNODE%S2(I,J) = ' '
              PNODE%S2(I,J) = T
             END DO
           END DO
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       END IF

      CASE('L')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%S1(I),I=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,PNODE%S0
           INQUIRE(UNIT = IOUNIT, POS=POS)
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,(PNODE%S1(I),I=1,NDIM)
           INQUIRE(UNIT = IOUNIT, POS=POS)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT,POS=POS)NINTEG,((PNODE%S2(I,J),I=1,NDIM),J=1,NSIZE)
           INQUIRE(UNIT = IOUNIT, POS=POS)
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

   END SUBROUTINE READ_DATA_FFA_BIN


   FUNCTION GET_NEW_FFA_POS(PNODE,LTYPE, NDIM, NSIZE, FPAR) RESULT (POS)
!
! Description: Function gets new position for reading bindary file without allocating arrays
!              used for scanning files
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
! Name      In/Out     Function
! PNODE     In         Pointer to node
! LTYPE     In         type of node
! NDIM      In         1st dimension of array in the node
! NSIZE     In         2nd dimension of array in the node
! POS       In         Position in file
! FPAR      In/Out     structure containing function specific data
!
! Arguments declaration
!    
    TYPE(DNODE), POINTER :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER(LINT) :: NDIM, NSIZE, POS
    CHARACTER(LEN=*) :: LTYPE
!
! Local declaration
!
    INTEGER :: LENGTH
!
!  BODY
!
    SELECT CASE(LTYPE)
     CASE('R', 'I')
       LENGTH = 4

     CASE('D', 'L')
       LENGTH = 8
       
       
      CASE('S')
       LENGTH = LSTRING_LENGTH

      CASE('C')

      CASE('N','DIR')
         RETURN
         
      CASE DEFAULT
            WRITE(*,*)' WRONG TYPE'
     
     END SELECT

   POS = (NDIM * NSIZE ) *LENGTH + 8
     
   RETURN

   END FUNCTION GET_NEW_FFA_POS
  
END MODULE FLL_READ_FFA_M

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
!     Subroutine FLL_READ_FFA
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: reads a ffa file
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
MODULE FLL_READ_FFA_M
CONTAINS

  FUNCTION FLL_READ_FFA(FILE,IOUNIT,FMT,FPAR) RESULT(PNODE)
   
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
!
!   LOCAL TYPES
!
   LOGICAL :: OK
   CHARACTER :: FMT_LOC
   INTEGER :: ISTAT
   INTEGER(LINT) :: POS

   
   INQUIRE (FILE=TRIM(FILE), EXIST=OK)
   IF(.NOT.OK) THEN
      WRITE(FPAR%MESG,'(A,A)')' Read  - file does not exist ',TRIM(FILE)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
   END IF
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
      WRITE(FPAR%MESG,'(A,A)')' Read  - unknown format',TRIM(FMT)
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
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
    END IF
!
!   READ INITIAL NODE
!
    PNODE => READ_NODE_FFA(IOUNIT,FMT_LOC,POS,FPAR)
    
    CLOSE(IOUNIT)
    IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A,A)')' Read  - error reading file ',TRIM(FILE)
       FPAR%SUCCESS = .FALSE.
    END IF
    
    RETURN
  
  END FUNCTION FLL_READ_FFA
!
!  READS NODE
!
  RECURSIVE FUNCTION READ_NODE_FFA(IOUNIT,FMT,POS,FPAR) RESULT(PNODE)
  
    USE FLL_TYPE_M
    USE FLL_MK_M
    USE FLL_MV_M
    
    IMPLICIT NONE  
    
    INTEGER(LINT),    INTENT(OUT)  :: POS
    TYPE(DNODE), POINTER :: PNODE,PNEW
    CHARACTER :: FMT
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER :: IOUNIT
    INTEGER(LINT) :: NSUB
    CHARACTER(LEN=NAME_LENGTH) :: T

    TYPE(FUNC_DATA_SET) :: FPAR_H
    CHARACTER(LEN=NAME_LENGTH)   :: NAME
    CHARACTER(LEN=TYPE_LENGTH)     :: LTYPE,FTYPE
    INTEGER(LINT) :: NDIM, NSIZE,NNODES
    LOGICAL :: OK,EXTRALINE
!
!  READ HEADER
!
    CALL READ_HEADER_FFA(IOUNIT,FMT,POS,NAME,LTYPE,FTYPE,NDIM,NSIZE,NSUB,EXTRALINE,FPAR_H)
    
    IF(.NOT.FPAR_H%SUCCESS)THEN
      WRITE(FPAR%MESG,'(A)')' Read  - error reading header '
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
!           CALL READ_DATA_FFA_ASCII(IOUNIT,PNODE,PNODE%FTYPE,PNODE%NDIM,PNODE%NSIZE,FPAR_H)
           CALL READ_DATA_FFA_ASCII(IOUNIT,PNODE,PNODE%FTYPE,1_LINT,1_LINT,FPAR_H)
         CASE('B')
!           CALL READ_DATA_FFA_BIN(IOUNIT,PNODE,PNODE%FTYPE,PNODE%NDIM,PNODE%NSIZE,FPAR_H)
           CALL READ_DATA_FFA_BIN(IOUNIT,PNODE,PNODE%FTYPE,1_LINT,1_LINT,FPAR_H)
        END SELECT
      END IF

      DO NNODES = 1,NDIM
        PNEW => READ_NODE_FFA(IOUNIT,FMT,POS,FPAR)
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
        CALL READ_DATA_FFA_BIN(IOUNIT,PNODE,PNODE%FTYPE,PNODE%NDIM,PNODE%NSIZE,FPAR_H)
      END SELECT
    
    END IF
    
    RETURN
    
  END FUNCTION READ_NODE_FFA
!
!  READ HEADER OR EACH NODE
!
   SUBROUTINE READ_HEADER_FFA(IOUNIT,FMT,POS,NAME,LTYPE,FTYPE,NDIM,NSIZE,NLINK,EXTRALINE,FPAR)
   
    USE FLL_TYPE_M    
    USE FLL_FUNC_PRT_M
    
    IMPLICIT NONE  
    
    INTEGER(LINT) :: POS
    CHARACTER :: FMT
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER :: IOUNIT
    
    CHARACTER(*)     :: LTYPE,FTYPE
    CHARACTER(*)     :: NAME
    INTEGER(LINT) :: NDIM, NSIZE,ISTART,IIND,NLINK,TMPINT
    CHARACTER*255 :: TEXT_LINE,TRIM_LINE
    CHARACTER*32  :: VER
    INTEGER :: IOSTAT
    LOGICAL :: OK,EXTRALINE

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
!
!  GET NAME
!
      IIND = 1
      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO 
      ISTART = IIND
      
      DO WHILE(TRIM_LINE(IIND:IIND) /= ',' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      NAME = TRIM(TRIM_LINE(ISTART:IIND-1))
      IIND = IIND + 1

      DO WHILE(TRIM_LINE(IIND:IIND) == ' ')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND
!
!  GET TYPE
!
      DO WHILE(TRIM_LINE(IIND:IIND) /= ',' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      LTYPE = TRIM(TRIM_LINE(ISTART:IIND-1))
      FTYPE = TRIM(TRIM_LINE(ISTART:IIND-1))
      IIND = IIND + 1

      DO WHILE(TRIM_LINE(IIND:IIND) == ' ' .OR. TRIM_LINE(IIND:IIND) == ',')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND
!
!  GET NSIZE
!
       DO WHILE(TRIM_LINE(IIND:IIND) /= ',' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO

      READ(TRIM_LINE(ISTART:IIND-1),'(I30)',IOSTAT=IOSTAT) NSIZE 
      IIND = IIND + 1         
      
      DO WHILE(TRIM_LINE(IIND:IIND) == ' ' .OR. TRIM_LINE(IIND:IIND) == ',')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND   
!
!  GET NDIM
!
       DO WHILE(TRIM_LINE(IIND:IIND) /= ',' )
         IIND =IIND + 1
         IF(IIND > 254)RETURN
      END DO
      READ(TRIM_LINE(ISTART:IIND-1),'(I30)',IOSTAT=IOSTAT) NDIM
      IIND = IIND + 1     
       
      DO WHILE(TRIM_LINE(IIND:IIND) == ' ' .OR. TRIM_LINE(IIND:IIND) == ',')
        IIND =IIND + 1
        IF(IIND > 254)RETURN
      END DO       
      ISTART = IIND
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
      
    CASE('B')
       NSIZE = 0
       READ(IOUNIT,IOSTAT=IOSTAT)NAME,LTYPE,NDIM,NSIZE,NLINK
       IF(TRIM(NAME) == 'FFA-format-v2')THEN 
!
!  DISREGARD, THIS IS JUST INFO ABOUT VERSION
!  CONSIDER HAVING VERSION ONLY AT THE BEGINING OF THE FILE
!
          REWIND(IOUNIT)
          READ(IOUNIT,IOSTAT=IOSTAT)VER
          READ(IOUNIT,IOSTAT=IOSTAT)NAME,LTYPE,NDIM,NSIZE,NLINK
       END IF
       FTYPE = LTYPE

write(*,*)' Header ',NAME,LTYPE,NDIM,NSIZE,NLINK 

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
    END SELECT
    
    WRITE(FPAR%MESG,'(A)')' Read  - reading header error '
    FPAR%SUCCESS = .FALSE.
    RETURN
    
    WRITE(FPAR%MESG,'(A)')' Read  - reading header error, reached end of file '
    FPAR%SUCCESS = .FALSE.
    RETURN
   
   END SUBROUTINE READ_HEADER_FFA
!
!  READ DATA 
!
    SUBROUTINE READ_DATA_FFA_ASCII(IOUNIT,PNODE,LTYPE,NDIM,NSIZE,FPAR)
    
    USE FLL_TYPE_M
    USE FLL_FUNC_PRT_M
   
    IMPLICIT NONE
    
    TYPE(DNODE), POINTER :: PNODE
    INTEGER :: IOUNIT
    INTEGER(LINT) :: NDIM,NSIZE
    CHARACTER(LEN=TYPE_LENGTH) :: LTYPE
    TYPE(FUNC_DATA_SET) :: FPAR
!
!   LOCAL DECLARATION
!
    INTEGER(LINT) :: I,J
    INTEGER :: IOSTAT
    LOGICAL :: OK
!
!  BODY
!
    IF(NDIM*NSIZE == 0)RETURN
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
           READ(IOUNIT,*,IOSTAT=IOSTAT)PNODE%S
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
   
   
   
   
!
!  READ DATA 
!
    SUBROUTINE READ_DATA_FFA_BIN(IOUNIT,PNODE,LTYPE,NDIM,NSIZE,FPAR)
    
    USE FLL_TYPE_M
    USE FLL_FUNC_PRT_M
   
    IMPLICIT NONE
    
    TYPE(DNODE), POINTER :: PNODE
    INTEGER :: IOUNIT
    INTEGER(LINT) :: NDIM,NSIZE,NINTEG
    CHARACTER(*) :: LTYPE
    CHARACTER(LEN=NAME_LENGTH) :: T
    TYPE(FUNC_DATA_SET) :: FPAR
!
!   LOCAL DECLARATION
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
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%R1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,PNODE%R0
           write(*,*)PNODE%R0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%R1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,((PNODE%R2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF

     CASE('D')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%D1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,PNODE%D0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%D1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,((PNODE%D2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
       
     CASE('I')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%I1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,PNODE%I0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%I1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,((PNODE%I2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
       
     CASE('J')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%L1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,PNODE%L0
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%L1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,((PNODE%L2(I,J),J=1,NSIZE),I=1,NDIM)
         END IF
       END IF
       
      CASE('S')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG
           DO I=1,NSIZE
             READ(IOUNIT,IOSTAT=IOSTAT)T
             PNODE%S1(I) = T
           END DO
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,PNODE%T
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG
           DO I=1,NDIM
             READ(IOUNIT,IOSTAT=IOSTAT)T
             PNODE%S1(I) = T
           END DO
         ELSE
	   READ(IOUNIT,IOSTAT=IOSTAT)NINTEG
           DO J=1,NSIZE
             DO I=1,NDIM
              READ(IOUNIT,IOSTAT=IOSTAT)T
              PNODE%S2(I,J) = T
             END DO
           END DO
         END IF
       END IF

      CASE('L')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%S1(I),I=1,NSIZE)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,PNODE%S
         END IF
       ELSE
         IF(NSIZE == 1)THEN
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,(PNODE%S1(I),I=1,NDIM)
         ELSE
           READ(IOUNIT,IOSTAT=IOSTAT)NINTEG,((PNODE%S2(I,J),I=1,NDIM),J=1,NSIZE)
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
  
END MODULE FLL_READ_FFA_M

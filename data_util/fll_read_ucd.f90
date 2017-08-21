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
!     Subroutine FLL_READ_UCD
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: reads a UCD file
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
MODULE FLL_READ_UCD_M
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

  FUNCTION FLL_READ_UCD(FILE,IOUNIT,FMT,FPAR) RESULT(PNODE)
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

   write(*,*)' here in ther'

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
   write(*,*)' here in ther1'

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
   write(*,*)' here in ther2'

   SELECT CASE(FMT_LOC)
    CASE('B')
      OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='UNFORMATTED',&
           ACCESS='STREAM',CONVERT='big_endian',IOSTAT=ISTAT)
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
   write(*,*)' here in ther3'
    POS = 1
    PNODE => READ_NODE_UCD(IOUNIT,FMT_LOC,FPAR)
    
    CLOSE(IOUNIT)
    IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A,A)')' Read  - error reading file ',TRIM(FILE)
       CALL FLL_OUT('ALL',FPAR)
       FPAR%SUCCESS = .FALSE.
    END IF
    
    RETURN
  
  END FUNCTION FLL_READ_UCD




  FUNCTION READ_NODE_UCD(IOUNIT,FMT,FPAR) RESULT(PNODE)
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
    USE FLL_MKDIR_M
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
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
    TYPE(DNODE), POINTER :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER :: IOUNIT
    CHARACTER :: FMT
!
!  Local declarations
!
    TYPE(DNODE), POINTER :: PREG,PTMP,PBND,PBELEM
    CHARACTER(LEN=NAME_LENGTH) :: TEXT
    CHARACTER(LEN=6) :: ETYPE
    INTEGER(LINT) :: NPTS,NELEM,A,B,C,I,N3,N4
    REAL(RDOUBLE), POINTER :: COO(:,:)
    INTEGER(LINT), ALLOCATABLE :: I4(:,:),I3(:,:)
    INTEGER :: ISTAT
    LOGICAL :: OK
    CHARACTER(LEN = 200)TEXTLONG
!
!   disregard all lines starting with #  - 3 lines
!
    READ(IOUNIT, '(A)')TEXT
    READ(IOUNIT, '(A)')TEXT
    READ(IOUNIT, '(A)')TEXT
!
!  read mesh dimensions
!
    READ(IOUNIT,*)NPTS,NELEM,A,B,C
!
!  allocate memory for coordinates
!
    PNODE => FLL_MKDIR('unstr_grid_data', FPAR)
    PREG  => FLL_MKDIR('region', FPAR)
    OK = FLL_MV(PREG,PNODE,FPAR)
    PBND  => FLL_MKDIR('boundary', FPAR)
    OK = FLL_MV(PBND,PREG,FPAR)

    PTMP => FLL_MK('coordinates','D',NPTS,3_LINT,FPAR)
    OK = FLL_MV(PTMP,PREG,FPAR)    
    COO => PTMP%D2

    DO I=1,NPTS
     READ(IOUNIT,*)A,COO(I,:)
    END DO
!
!   add boundary name
!
    PTMP => FLL_MK('boundary_name','S',1_LINT,1_LINT,FPAR)
    PTMP%S0 = 'wall'
!
!   read elements
!
    N3 = 0
    N4 = 0
!
    ALLOCATE(I3(NELEM,3),I4(NELEM,4), STAT=ISTAT)
     IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY ==> fll_read_ucd ERR:260 '


    DO I=1,NELEM
      READ(IOUNIT,'(A)')TEXTLONG
      READ(TEXTLONG,*)A,B,ETYPE
      SELECT CASE(ETYPE)
      CASE('tri')
        N3 = N3 + 1
        READ(TEXTLONG, *)A,B,ETYPE,I3(N3,:)
      CASE('quad')
        N4 = N4 + 1
        READ(TEXTLONG, *)A,B,ETYPE,I4(N4,:)
      END SELECT

    END DO

       WRITE(*,*)N3,N4,N3+N4
!
!  add it to the structure
!   
    IF(N3> 0)THEN

      PBELEM  => FLL_MKDIR('belem_group', FPAR)
      OK = FLL_MV(PBELEM,PBND,FPAR)
      PTMP => FLL_MK('bound_elem_type','S',1_LINT,1_LINT,FPAR)
      OK = FLL_MV(PTMP,PBELEM,FPAR)
      PTMP%S0 = 'tria3'
      PTMP => FLL_MK('bound_elem_nodes','L',N3,3_LINT,FPAR)       
      PTMP%L2 = I3(1:N3,:)
      OK = FLL_MV(PTMP,PBELEM,FPAR)
    END IF

    IF(N4> 0)THEN

      PBELEM  => FLL_MKDIR('belem_group', FPAR)
      OK = FLL_MV(PBELEM,PBND,FPAR)
      PTMP => FLL_MK('bound_elem_type','S',1_LINT,1_LINT,FPAR)
      OK = FLL_MV(PTMP,PBELEM,FPAR)
      PTMP%S0 = 'quad4'
      PTMP => FLL_MK('bound_elem_nodes','L',N4,4_LINT,FPAR)       
      PTMP%L2 = I4(1:N4,:)
      OK = FLL_MV(PTMP,PBELEM,FPAR)

    END IF


    DEALLOCATE(I3,I4, STAT=ISTAT)
     IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> fll_read_ucd ERR:277 '

  END FUNCTION READ_NODE_UCD

  
END MODULE FLL_READ_UCD_M

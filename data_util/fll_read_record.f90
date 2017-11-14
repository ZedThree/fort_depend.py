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
!     Subroutine FLL_READ_RECORD
!
!     Date: 2017-06-16
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
MODULE FLL_READ_RECORD_M
!
! Description: Contains functions reading record from FLL native format binary file
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

  FUNCTION FLL_READ_RECORD(FILE,IOUNIT,PLIST,NAME,LTYPE,DATADIM,NUMBER,RECURSE,FPAR,ERRMSG) RESULT(PNODE)
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
    USE FLL_RM_M
    USE FLL_READ_M
    USE FLL_OUT_M
    USE FLL_MK_M
    USE FLL_MV_M
    USE FLL_LOCATE_M
    USE FLL_CAT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! FILE         In         Name of file
! PNODE        Out        Node to a first node in list from a file
! IOUNIT       In         Number of unit
! PLIST        In         List of items in file, can be obtained by
!                         FLL_READ with SCAN = 'Y' optional parameter
! NAME         In         name of node
! NUMBER       In         position of node in list
! LTYPE        In         type of node  - can be *
! DATADIM      In         dimensions of data the node should contain
!                         can be 0 - scalar, 1 -1D array, 2 -2D array 
!                         if any other number specified (preferrable -1) - do not care about dimensions
! RECURSE      In         search recursively
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   CHARACTER(*) :: FILE
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER(*) :: NAME
   CHARACTER(*) :: LTYPE
   INTEGER(LINT) :: NUMBER,DATADIM
   LOGICAL :: RECURSE
   CHARACTER(LEN=*), OPTIONAL :: ERRMSG
!
! Local declarations
!
   TYPE(DNODE), POINTER  :: PLIST,PTMP
   LOGICAL :: OK
   CHARACTER :: FMT_LOC
   INTEGER :: ISTAT,PLISTOK
   INTEGER(LINT) :: POS
   CHARACTER :: SCAN_LOC
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   PLISTOK = 0

   IF(.NOT.ASSOCIATED(PLIST))THEN
    PLISTOK = 1
!
!  IF LIST OF ITEMS NOT PROVIDED THEN
!
     INQUIRE (FILE=TRIM(FILE), EXIST=OK)
     IF(.NOT.OK) THEN
        WRITE(FPAR%MESG,'(A,A)')' Read  - file does not exist ',TRIM(FILE)
        CALL FLL_OUT(LOC_ERRMSG,FPAR)
        FPAR%SUCCESS = .FALSE.
        PNODE => NULL()
        RETURN
     END IF
!
!  USE ONLY BINARY FORMAT
!
     PLIST => FLL_READ(FILE,IOUNIT,'B',FPAR,SCAN = 'Y',ERRMSG=LOC_ERRMSG)
     CALL FLL_CAT(PLIST, 6, .TRUE.,FPAR,ERRMSG=LOC_ERRMSG)
!
   END IF

   PTMP => FLL_LOCATE(PLIST,NAME,LTYPE,DATADIM,NUMBER,.TRUE.,FPAR,ERRMSG=LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PTMP))THEN
     CALL FLL_RM(PLIST, FPAR,ERRMSG=LOC_ERRMSG)
     PNODE => NULL()
     RETURN
   END IF

   POS = PTMP%FP
!
!   CLEAN MEMORY
!
   IF(PLISTOK == 0)THEN
!
!  LIST WAS CREATED IN THIS FUNCTION, REMOVE IT
!
     CALL FLL_RM(PLIST, FPAR,ERRMSG=LOC_ERRMSG)
   END IF
!
!  OPEN FILE
!
   INQUIRE (FILE=TRIM(FILE), EXIST=OK)
   IF(.NOT.OK) THEN
      WRITE(FPAR%MESG,'(A,A)')' Read  - file does not exist ',TRIM(FILE)
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
   END IF
!
!   DETERMINE RORMAT
!
   FMT_LOC = 'B'
   SCAN_LOC = 'N'
!
!   OPEN THE FILE
!
   OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='UNFORMATTED',&
      ACCESS='STREAM',IOSTAT=ISTAT)

   IF(ISTAT/=0) THEN
     WRITE(FPAR%MESG,'(A,A)')' Read_record  - error opening file ',TRIM(FILE)
     CALL FLL_OUT(LOC_ERRMSG,FPAR)
     FPAR%SUCCESS = .FALSE.
     PNODE => NULL()
     RETURN
   END IF
!
!   READ INITIAL NODE
!
   PLIST => READ_NODE(IOUNIT,FMT_LOC,POS,SCAN_LOC,FPAR)
   IF(.NOT.ASSOCIATED(PLIST)) THEN
     WRITE(FPAR%MESG,'(A,A)')' Read_record  - error reading specified data set ',TRIM(FILE)
     CALL FLL_OUT(LOC_ERRMSG,FPAR)
     FPAR%SUCCESS = .FALSE.
     PNODE => NULL()
     RETURN
   END IF
!
!  MAKE A HEAD NODE
!
   PNODE => FLL_MK('Record','DIR',0_LINT,0_LINT,FPAR)
   OK = FLL_MV(PLIST, PNODE, FPAR)
!
!  CLOSE FILE
!    
    CLOSE(IOUNIT)
    IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A,A)')' Read  - error reading file ',TRIM(FILE)
       CALL FLL_OUT(LOC_ERRMSG,FPAR)
       FPAR%SUCCESS = .FALSE.
    END IF
    
    RETURN
  
  END FUNCTION FLL_READ_RECORD
  
END MODULE FLL_READ_RECORD_M

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
!     Subroutine FLL_SCAN_FILE
!
!
MODULE FLL_SCAN_FILE_M
!
! Description: Contains function fll_scan_file
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

   FUNCTION FLL_SCAN_FILE(FILENAME,IOUNIT,FMT,FPAR,ERRMSG) RESULT(PNODE)
!
! Description: Scan file, returns fll list with data names in file and their
!       position in file in bytes
!
! External Modules used
!     
    USE FLL_TYPE_M
    USE FLL_OUT_M
    USE FLL_READ_M
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        Out        pointer to fll list with data in names in file
! IOUNIR       In         pointer which is to be copied
! FILENAME     In         Name of file
! FMT          In         Format of file
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER(LEN=FILE_NAME_LENGTH) :: FILENAME
   CHARACTER :: FMT
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! Local types
!
   LOGICAL :: OK
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  local action
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   INQUIRE (FILE=TRIM(FILENAME), EXIST=OK)
   IF(.NOT.OK) THEN
      WRITE(FPAR%MESG,'(A,A)')' Read  - file does not exist ',TRIM(FILENAME)
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      PNODE => NULL()
      RETURN
   END IF
!
!  GET FILE STRUCTURE
!
   PNODE => FLL_READ(FILENAME,IOUNIT,FMT,FPAR,SCAN = 'Y', ERRMSG=LOC_ERRMSG)

   RETURN 

   END FUNCTION FLL_SCAN_FILE
  
END MODULE FLL_SCAN_FILE_M

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
MODULE FLL_FUNC_PRT_M
!
! Description: Contains prototypes of some functions
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

   FUNCTION ERR_MSG(NAME,NTYPE) RESULT(MESG)
!
! Description: writes to a err message string
!
!
! Description: Writes some standard err message
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
! External Modules used
!      
      USE FLL_TYPE_M
      IMPLICIT NONE   
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! NAME         In         name of node
! NTYPE        In         type of node
!
! Arguments declaration
!
      CHARACTER(LEN=NAME_LENGTH) :: NAME
      CHARACTER(LEN=TYPE_LENGTH) :: NTYPE
      
      CHARACTER(LEN=ERR_MSG_LENGTH)  :: MESG
      
      WRITE(MESG,'(A,A,A,A,A)')' Array ', TRIM(NAME),' TYPE ',TRIM(NTYPE),' not associated'
      RETURN
    END FUNCTION ERR_MSG
    
    FUNCTION TEST_IOSTAT(IOSTAT, FPAR,ERRMSG) RESULT(OK)
!
! Description: tests IOSTAT return parameter from R/W functions
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
    INTEGER :: IOSTAT
    TYPE(FUNC_DATA_SET) :: FPAR
    LOGICAL :: OK
    CHARACTER(*), OPTIONAL :: ERRMSG

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
    OK = .FALSE.
    IF (IOSTAT > 0)  THEN
      WRITE(FPAR%MESG,'(A)')' Read  - error readig node data'
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
   ELSE IF (IOSTAT < 0) THEN
      WRITE(FPAR%MESG,'(A)')' Read  - EOF reached'
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
   ELSE
      FPAR%SUCCESS = .TRUE.
      OK =.TRUE.
   END IF
!    
    RETURN
! 
   END FUNCTION TEST_IOSTAT

END MODULE FLL_FUNC_PRT_M

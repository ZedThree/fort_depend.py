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
!     Subroutine FLL_FUNC_PRT_M
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: Finds node in a chain
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
MODULE FLL_FUNC_PRT_M
CONTAINS

   FUNCTION ERR_MSG(NAME,NTYPE) RESULT(MESG)
    
      USE FLL_TYPE_M
      IMPLICIT NONE   
      CHARACTER(LEN=NAME_LENGTH) :: NAME
      CHARACTER(LEN=TYPE_LENGTH) :: NTYPE
      
      CHARACTER(LEN=ERR_MSG_LENGTH)  :: MESG
      
      WRITE(MESG,'(A,A,A,A,A)')' Array ', TRIM(NAME),' TYPE ',TRIM(NTYPE),' not associated'
      RETURN
    END FUNCTION ERR_MSG
    
    FUNCTION TEST_IOSTAT(IOSTAT, FPAR) RESULT(OK)
!     
    USE FLL_TYPE_M
    IMPLICIT NONE
    INTEGER :: IOSTAT
    TYPE(FUNC_DATA_SET) :: FPAR
    LOGICAL :: OK
!     
    OK = .FALSE.
    IF (IOSTAT > 0)  THEN
      WRITE(FPAR%MESG,'(A)')' Read  - error readig node data'
      FPAR%SUCCESS = .FALSE.
   ELSE IF (IOSTAT < 0) THEN
      WRITE(FPAR%MESG,'(A)')' Read  - EOF reached'
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

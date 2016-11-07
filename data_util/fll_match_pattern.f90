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
MODULE FLL_MATCH_PATTERN_M
!
! Description: Contains function fll_match_pattern
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
   
   FUNCTION FLL_MATCH_PATTERN(PNODE, NAME, TYPE, DIM, FPAR) RESULT(OK)
!
! Description: matches pattern
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
! PNODE        In         pointer where find node
! NAME           In         name of node
! LTYPE         In         type of node  - can be *
!  DIM              In         dimensions of data the node should contain
!                         can be 0 - scalar), 1 -1D array, 2 -2D array 
!                         any other number (prefer -1) - do not care about dimensions
! OK               Out       return value
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(LEN=*) :: NAME
   CHARACTER(LEN=*) :: TYPE
   INTEGER(LINT) :: DIM
   
   LOGICAL OK
!
!  Local declarations
!
   INTEGER(LINT) :: NLOCD
   
   OK = .FALSE.
   IF(TRIM(PNODE%LNAME) == TRIM(NAME) .OR. TRIM(NAME) == '*')THEN
        OK = .TRUE.
   ELSE
     OK = .FALSE.
     RETURN
   END IF
     
   IF(TRIM(PNODE%LTYPE) == TRIM(TYPE) .OR. TRIM(TYPE) == '*')THEN
     OK = .TRUE.
   ELSE
     OK = .FALSE.
     RETURN
   END IF
   
   IF(DIM < 0 .OR. DIM > 2)THEN 
     OK = .TRUE.
     RETURN
   END IF
   
   IF(PNODE%NDIM > 1 .AND. PNODE%NSIZE > 1) THEN
     NLOCD = 2
   ELSE IF (PNODE%NDIM > 1 .OR. PNODE%NSIZE > 1) THEN
     NLOCD = 1
   ELSE
     NLOCD = 0
   END IF
 
   IF(NLOCD == DIM )THEN
     OK = .TRUE.
   ELSE
     OK = .FALSE.
     RETURN
   END IF
   
  RETURN
   
  END FUNCTION FLL_MATCH_PATTERN


END MODULE FLL_MATCH_PATTERN_M

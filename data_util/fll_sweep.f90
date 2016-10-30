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
!     Subroutine FLL_SWEEP
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: sweep through list
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
MODULE FLL_SWEEP_M
CONTAINS

   FUNCTION FLL_SWEEP(PNODE,NAME,LTYPE,RECURSE,FPAR) RESULT(PFIND)
   
    USE FLL_TYPE_M
    USE FLL_LOCATE_M

    IMPLICIT NONE
!
!   FUNCTION FIND ALL NODES WITH SPECIFIED NAME OR TYPE 
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE,PFIND
   CHARACTER(*) :: NAME
   CHARACTER(*) :: LTYPE
   INTEGER(LINT) :: NUMBER
   LOGICAL :: RECURSE
!
!   LOCAL TYPES
!
   CHARACTER(LEN=TYPE_LENGTH) :: TLTYPE
   TYPE(DNODE), POINTER  :: PCURR, PCHLD
   INTEGER(LINT) :: LOCNUM,I
!   
!   BODY OF FUNCTION
!
   NULLIFY(PFIND)
   I = 1
   DO WHILE(LTYPE(I:I) == ' ')
     I = I + 1
   END DO

   TLTYPE = TRIM(LTYPE(I:))
   
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A,A)')'Locate - Null node: ',TRIM(NAME)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF

!   DO WHILE(PNODE)   

   PCURR => PNODE%PCHILD
   IF(.NOT.ASSOCIATED(PNODE%PCHILD))THEN
     WRITE(*,*)' NODE NOT DIR NODE'
     RETURN 
   END IF
!   END DO


   RETURN
   END FUNCTION FLL_SWEEP


END MODULE FLL_SWEEP_M

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
MODULE FLL_SWEEP_M
!
! Description: Contains function fll_sweep and match_pattern
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
   RECURSIVE FUNCTION  FLL_SWEEP(PNODE,PFIND,NAME,LTYPE,DIM,FPAR) RESULT(OK)
!
! Description: Function sweep through list return each node   -------------  NOT FINISHED YET
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
    USE FLL_MATCH_PATTERN_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE          In         pointer where find node
! NAME             In         name of node
! NUMBER        In         position of node in list
! LTYPE            In         type of node  - can be *
! DIM                 In         dimensions of data the node should contain
!                                    can be 0 - scalar), 1 -1D array, 2 -2D array 
!                                    any other number (prefer -1) - do not care about dimensions
! PFIND          Out        return pointer to located node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE,PFIND
   CHARACTER(*) :: NAME
   CHARACTER(*) :: LTYPE
   INTEGER(LINT) :: DIM
   LOGICAL OK
!
!   LOCAL TYPES
!
   CHARACTER(LEN=TYPE_LENGTH) :: TLTYPE
   INTEGER(LINT) :: I
   TYPE(DNODE), POINTER :: PNEXT
!   
!   BODY OF FUNCTION
!      
   OK =.FALSE.

   I = 1
   DO WHILE(LTYPE(I:I) == ' ')
     I = I + 1
   END DO

   TLTYPE = TRIM(LTYPE(I:))
   
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A,A)')'Sweep - Null node: ',TRIM(NAME)
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
!
!  start loop
!
   IF(.NOT.ASSOCIATED(PFIND))THEN
     IF(ASSOCIATED(PNODE%PCHILD))THEN
       PFIND => PNODE%PCHILD
     ELSE
       PFIND => PNODE
     END IF
   ELSE 
     PFIND => PFIND%PNEXT
   END IF
          
   DO WHILE(ASSOCIATED(PFIND))
   
     IF(FLL_MATCH_PATTERN(PFIND,NAME,LTYPE,DIM,FPAR))THEN
       OK =.TRUE.
       RETURN
     END IF
     
     PFIND => PFIND%PNEXT
     
   END DO

   RETURN
   END FUNCTION FLL_SWEEP


END MODULE FLL_SWEEP_M

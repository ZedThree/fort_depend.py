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
MODULE FLL_GETNBYTES_M
!
! Description: Counts byte length of the list
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
   RECURSIVE FUNCTION FLL_GETNBYTES(PNODE,FPAR, ERRMSG) RESULT(BYTES)
!
! Description: Get size of linked list in bytes
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
! PNODE        In         node to duplicate
! BYTES        Out       size of list
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER(LINT) :: BYTES
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! Local declarations
!
   TYPE(DNODE), POINTER :: PCHILD,PNEXT
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
!   BODY OF SUBROUTINE
!
!  check the node is not null
!
   BYTES = 0
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' GETNBYTES - null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   PCHILD => PNODE%PCHILD

   BYTES = FLL_GETDATALENGTH(PNODE,FPAR)
!
! IF NODE HAS CHILDREN, GETNBYTES ALL OF THEM
!
   DO WHILE (ASSOCIATED(PCHILD))

     BYTES = BYTES + FLL_GETNBYTES(PCHILD,FPAR)

     IF(.NOT.FPAR%SUCCESS)THEN
       WRITE(FPAR%MESG,'(A)')' GETNBYTES - error duplicting children nodes '
       CALL FLL_OUT(LOC_ERRMSG,FPAR)
       FPAR%SUCCESS = .FALSE.
       RETURN
     END IF

     PCHILD => PCHILD%PNEXT

   END DO

   FPAR%SUCCESS = .TRUE.
   RETURN

   END FUNCTION FLL_GETNBYTES
!
  FUNCTION FLL_GETDATALENGTH(PNODE,FPAR) RESULT(BYTES)
!
! Description: duplicated data of the node
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
! PNODE      In         pointer data which is to be duplicated
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET)   :: FPAR
   INTEGER(LINT) :: BYTES
!
! Local declarations
!
   INTEGER(LINT) :: NDIM, NSIZE, NNDIM, NNSIZE
!
!  IF DIR NODE, RETURN
!
   BYTES = NAME_LENGTH + TYPE_LENGTH + 8 + 8
   
   IF(TRIM(PNODE%LTYPE) == 'DIR' .OR. TRIM(PNODE%LTYPE) == 'N') RETURN
!
!   1D ARRAYS
!
   IF(ASSOCIATED(PNODE%R1))THEN
     NDIM  = SIZE(PNODE%R1, DIM = 1, KIND = LINT)
     BYTES = BYTES + 4*NDIM
     RETURN
   END IF
!
   IF(ASSOCIATED(PNODE%D1))THEN
     NDIM  = SIZE(PNODE%D1, DIM = 1, KIND = LINT)
     BYTES = BYTES + 8*NDIM
     RETURN
   END IF
   IF(ASSOCIATED(PNODE%I1))THEN
     NDIM  = SIZE(PNODE%I1, DIM = 1, KIND = LINT)
     BYTES = BYTES + 4*NDIM
     RETURN
   END IF
   IF(ASSOCIATED(PNODE%L1))THEN
     NDIM  = SIZE(PNODE%L1, DIM = 1, KIND = LINT)
     BYTES = BYTES + 8*NDIM
     RETURN
   END IF
   IF(ASSOCIATED(PNODE%S1))THEN
     NDIM  = SIZE(PNODE%S1, DIM = 1, KIND = LINT)
     BYTES = BYTES + LSTRING_LENGTH*NDIM
     RETURN
   END IF
!
!  2D ARRAYS
!
     IF(ASSOCIATED(PNODE%R2))THEN
       NDIM    = SIZE(PNODE%R2, DIM = 1, KIND = LINT)
       NSIZE   = SIZE(PNODE%R2, DIM = 2, KIND = LINT)
       BYTES = BYTES + 4*NDIM*NSIZE
       RETURN  
    END IF
   
     IF(ASSOCIATED(PNODE%D2))THEN
       NDIM    = SIZE(PNODE%D2, DIM = 1, KIND = LINT)
       NSIZE   = SIZE(PNODE%D2, DIM = 2, KIND = LINT)
       BYTES = BYTES + 8*NDIM*NSIZE
       RETURN  
    END IF
    
    IF(ASSOCIATED(PNODE%I2))THEN
       NDIM    = SIZE(PNODE%I2, DIM = 1, KIND = LINT)
       NSIZE   = SIZE(PNODE%I2, DIM = 2, KIND = LINT)
       BYTES = BYTES + 4*NDIM*NSIZE
       RETURN  
    END IF
    
         IF(ASSOCIATED(PNODE%L2))THEN
       NDIM    = SIZE(PNODE%L2, DIM = 1, KIND = LINT)
       NSIZE   = SIZE(PNODE%L2, DIM = 2, KIND = LINT)
       BYTES = BYTES + 8*NDIM*NSIZE
       RETURN  
    END IF
    
    IF(ASSOCIATED(PNODE%S2))THEN
       NDIM    = SIZE(PNODE%S2, DIM = 1, KIND = LINT)
       NSIZE   = SIZE(PNODE%S2, DIM = 2, KIND = LINT)
       BYTES = BYTES + LSTRING_LENGTH*NDIM*NSIZE
       RETURN  
    END IF
!
!  SCALARS AND STATICALLY DEFINED ARRAYS
!
    SELECT CASE(PNODE%LTYPE)
    CASE('R')
      BYTES = BYTES + 4
    CASE('D')
      BYTES = BYTES + 8
    CASE('I')
      BYTES = BYTES + 4
    CASE('L')
      BYTES = BYTES + 8
    CASE('S')
      BYTES = BYTES + LSTRING_LENGTH
    END SELECT
  
    RETURN
    
  END FUNCTION FLL_GETDATALENGTH
 

END MODULE FLL_GETNBYTES_M

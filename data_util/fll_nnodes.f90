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
MODULE FLL_NNODES_M
!
! Description: Contains function fll_nnodes
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
   RECURSIVE FUNCTION FLL_NNODES(PNODE,NAME,LTYPE,DATADIM,RECURSE,FPAR) RESULT(NUMBER)
!
! Description: function returns number of nodes specified by 
!              name, type and dimensions of data
!              if data dimensions not specified as 0,1 or 2, 
!              do not care about data dimensions
!
! External Modules used
!     
    USE FLL_TYPE_M
    USE FLL_FUNC_PRT_M
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer where to search
! NAME         In         name of pointer
! LTYPE        Out        type of pointer
! DATADIM      Out        dimensions of data set of pointer
! RECURSE      Out        serach recursively
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   CHARACTER(*) :: LTYPE
   INTEGER(LINT) :: NUMBER,DATADIM
   LOGICAL :: RECURSE
!
! local declarations
!
   CHARACTER(LEN=TYPE_LENGTH) :: TLTYPE
   TYPE(DNODE), POINTER  :: PCURR, PCHLD, PFIND
   INTEGER(LINT) :: I,NDIM,NSIZE
!   
!   BODY OF FUNCTION
!
   NUMBER = 0
   NULLIFY(PFIND)
   I = 1
   DO WHILE(LTYPE(I:I) == ' ')
     I = I + 1
   END DO

   TLTYPE = TRIM(LTYPE(I:))
   
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A,A)')'Locate - Null node: ',TRIM(NAME)
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   

   PCURR => PNODE%PCHILD
   IF(.NOT.ASSOCIATED(PNODE%PCHILD))THEN
     WRITE(*,*)' NODE NOT DIR NODE'
     RETURN 
   END IF

   DO WHILE(ASSOCIATED(PCURR))
!
!  IF RECURSIVE DO NOT CONSIDER NUMBERING AND RETURN THE FIRST FOUND NODE
!
     IF(RECURSE .AND. ASSOCIATED(PCURR%PCHILD))THEN
       PCHLD => PCURR%PCHILD
       NUMBER = NUMBER + FLL_NNODES(PCHLD,NAME,LTYPE,DATADIM,RECURSE,FPAR)
       IF(ASSOCIATED(PFIND))THEN
         FPAR%SUCCESS = .TRUE.
         RETURN 
       END IF
     END IF
!
!   LOOK FOR NODE
!
      IF(   TRIM(PCURR%LNAME) == TRIM(NAME)  .AND.  &
          (TRIM(TLTYPE) == TRIM(PCURR%LTYPE)   .OR.  TLTYPE(1:1) == '*' ) )THEN

           NDIM  = PCURR%NDIM
           NSIZE = PCURR%NSIZE

          SELECT CASE(DATADIM)
          CASE(0)
            IF(NDIM == 1 .AND. NSIZE == 1)NUMBER = NUMBER + 1
          CASE(1)
            IF(NDIM > 1 .OR. NSIZE > 1)NUMBER = NUMBER + 1
          CASE(2)
            IF(NDIM > 1 .AND. NSIZE > 1)NUMBER = NUMBER + 1
          CASE DEFAULT 
            NUMBER = NUMBER + 1
          END SELECT

     END IF
     PCURR => PCURR%PNEXT
   
   END DO

   RETURN
   END FUNCTION FLL_NNODES


END MODULE FLL_NNODES_M

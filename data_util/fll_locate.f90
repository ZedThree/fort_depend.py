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
!     Subroutine FLL_LOCATE
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
MODULE FLL_LOCATE_M
CONTAINS

   RECURSIVE FUNCTION FLL_LOCATE(PNODE,NAME,NUMBER,LTYPE,RECURSE,FPAR) RESULT(PFIND)
   
    USE FLL_TYPE_M
    USE FLL_FUNC_PRT_M

    IMPLICIT NONE
!
!   FUNCTION FIND NODES WITH SPECIFIED NAME 
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
   

   PCURR => PNODE%PCHILD
   IF(.NOT.ASSOCIATED(PNODE%PCHILD))THEN
     WRITE(*,*)' NODE NOT DIR NODE'
     RETURN 
   END IF

   LOCNUM = 1
   DO WHILE(ASSOCIATED(PCURR))
!
!  IF RECURSIVE DO NOT CONSIDER NUMBERING AND RETURN THE FIRST FOUND NODE
!
     IF(RECURSE .AND. ASSOCIATED(PCURR%PCHILD))THEN
       PCHLD => PCURR%PCHILD
       PFIND   => FLL_LOCATE(PCHLD,NAME,1_LINT,LTYPE,RECURSE,FPAR)
       IF(ASSOCIATED(PFIND))THEN
         FPAR%SUCCESS = .TRUE.
         RETURN 
       END IF
     END IF
!
!   LOOK FOR NODE
!
      IF( (TRIM(PCURR%LNAME) == TRIM(NAME) .OR.TRIM(PCURR%LNAME) == '*')  .AND.  &
          (TRIM(TLTYPE) == TRIM(PCURR%LTYPE)   .OR.  TLTYPE(1:1) == '*' ) )THEN

        IF(LOCNUM == NUMBER)THEN
          PFIND => PCURR
          RETURN
        ELSE
          LOCNUM = LOCNUM + 1
        END IF 

     END IF
     PCURR => PCURR%PNEXT
   
   END DO
!
!  END OF LINKED LIST
!
   PFIND => NULL()
   FPAR%SUCCESS = .FALSE.
   WRITE(FPAR%MESG,'(A,A)')' Locate -  node not found: ',TRIM(NAME)

   RETURN
   END FUNCTION FLL_LOCATE


END MODULE FLL_LOCATE_M

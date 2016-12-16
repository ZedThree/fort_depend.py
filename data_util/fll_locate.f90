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
MODULE FLL_LOCATE_M
!
! Description: locate node
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

   RECURSIVE FUNCTION FLL_LOCATE(PNODE,NAME,LTYPE,DATADIM,NUMBER,RECURSE,FPAR) RESULT(PFIND)
!
! Description: function finds node identified by name, type, position in list, dimensions of data it contains
!                       search can be done recursively
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
! PNODE        In         pointer where find node
! NAME         In         name of node
! NUMBER       In         position of node in list
! LTYPE        In         type of node  - can be *
! DATADIM      In         dimensions of data the node should contain
!                         can be 0 - scalar, 1 -1D array, 2 -2D array 
!                         if any other number specified (preferrable -1) - do not care about dimensions
! RECURSE      In         search recursively
! PFIND        Out        return pointer to located node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE,PFIND
   CHARACTER(*) :: NAME
   CHARACTER(*) :: LTYPE
   INTEGER(LINT) :: NUMBER,DATADIM
   LOGICAL :: RECURSE
!
! Local declarations
!
   CHARACTER(LEN=TYPE_LENGTH) :: TLTYPE
   TYPE(DNODE), POINTER  :: PCURR, PCHLD
   INTEGER(LINT) :: LOCNUM,NDIM, NSIZE
!   
!  remove empty spaces
!
   NULLIFY(PFIND)

   TLTYPE = ADJUSTL(TRIM(LTYPE(1:)))
   
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

   LOCNUM = 1
   DO WHILE(ASSOCIATED(PCURR))
!
!  IF RECURSIVE DO NOT CONSIDER NUMBERING AND RETURN THE FIRST FOUND NODE
!
     IF(RECURSE .AND. ASSOCIATED(PCURR%PCHILD))THEN
       PCHLD => PCURR%PCHILD
       PFIND   => FLL_LOCATE(PCHLD,NAME,LTYPE,DATADIM,1_LINT,RECURSE,FPAR)
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

        NDIM  = PCURR%NDIM
        NSIZE = PCURR%NSIZE

        IF(LOCNUM == NUMBER)THEN
          SELECT CASE(DATADIM)
          CASE(0)
            IF(NDIM == 1 .AND. NSIZE == 1)THEN
              PFIND => PCURR
              FPAR%SUCCESS = .TRUE.
              RETURN
            ELSE
             LOCNUM = LOCNUM -1
            END IF 
          CASE(1)
            IF(NDIM > 1 .OR. NSIZE > 1)THEN
              PFIND => PCURR
              FPAR%SUCCESS = .TRUE.
              RETURN
            ELSE
             LOCNUM = LOCNUM -1
            END IF
          CASE(2)
            IF(NDIM > 1 .AND. NSIZE > 1)THEN
              PFIND => PCURR
              FPAR%SUCCESS = .TRUE.
              RETURN
            ELSE
             LOCNUM = LOCNUM -1
            END IF
          CASE DEFAULT 
          PFIND => PCURR
          RETURN
          END SELECT
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
   CALL FLL_OUT('ALL',FPAR)

   RETURN
   END FUNCTION FLL_LOCATE


END MODULE FLL_LOCATE_M

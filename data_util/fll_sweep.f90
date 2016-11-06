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
! Description: Contains function fll_sweek
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
   FUNCTION FLL_SWEEP(PNODE,NAME,LTYPE,RECURSE,FPAR) RESULT(PFIND)
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
    USE FLL_LOCATE_M
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Description: Module copies PWHAT pointer to PWHERE pointer
!              If PWHERE pointer == NULL, PWHAT is a duplicate 
!              of PWHAT with PWHAT%Ppar == NULL
!
!              if PWHERE is DIR on N type, PWHAT is added
!              to it as a new sub-data set
!
!              if PWHERE is a data type of nodes
!              PWHAT overwrites it
!
! External Modules used
! 
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE,PFIND
   CHARACTER(*) :: NAME
   CHARACTER(*) :: LTYPE
   LOGICAL :: RECURSE
!
!   LOCAL TYPES
!
   CHARACTER(LEN=TYPE_LENGTH) :: TLTYPE
   TYPE(DNODE), POINTER  :: PCURR, PCHLD
   INTEGER(LINT) :: I
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
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
!
!   DO WHILE(PNODE)   
!
   PCURR => PNODE%PCHILD
   IF(.NOT.ASSOCIATED(PNODE%PCHILD))THEN
     WRITE(*,*)' NODE NOT DIR NODE'
     RETURN 
   END IF
!   END DO


   RETURN
   END FUNCTION FLL_SWEEP


END MODULE FLL_SWEEP_M

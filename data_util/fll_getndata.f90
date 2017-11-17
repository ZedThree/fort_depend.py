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
MODULE FLL_GETNDATA_M
!
! Description: returns data of the node
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
  FUNCTION FLL_GETNDATA_R0(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(R0)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! R0           Out        returns value of real scalar
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   REAL(RSINGLE) :: R0
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF
!
!  check for null nodes
!
   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_R0 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR) 
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'R',0_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_R0 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN
   END IF

   R0 = PFIND%R0
   RETURN

  END FUNCTION FLL_GETNDATA_R0



  FUNCTION FLL_GETNDATA_R1(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(R1)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! R1           Out        returns pointer to real array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   REAL(RSINGLE), POINTER :: R1(:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_R1 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'R',1_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_R1 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      R1=>NULL()
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN
   END IF

   R1 => PFIND%R1
   RETURN

  END FUNCTION FLL_GETNDATA_R1

  FUNCTION FLL_GETNDATA_R2(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(R2)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! R2           Out        returns pointer to real array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   REAL(RSINGLE), POINTER :: R2(:,:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_R2 - Null node '
      
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'R',2_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_R2 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      R2=>NULL()
      RETURN
   END IF

   R2 => PFIND%R2
   RETURN

  END FUNCTION FLL_GETNDATA_R2
!
!  DOUBLE
!
  FUNCTION FLL_GETNDATA_D0(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(R0)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! R1           Out        returns double number
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   REAL(RDOUBLE) :: R0
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_D0 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'D',0_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_D0 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN
   END IF

   R0 = PFIND%D0
   RETURN

  END FUNCTION FLL_GETNDATA_D0



  FUNCTION FLL_GETNDATA_D1(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(R1)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! R1           Out        returns pointer to double array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   REAL(RDOUBLE), POINTER :: R1(:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_D1 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'D',1_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_D1 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      R1=>NULL()
      RETURN
   END IF

   R1 => PFIND%D1
   
   RETURN

  END FUNCTION FLL_GETNDATA_D1

  FUNCTION FLL_GETNDATA_D2(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(R2)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! R1           Out        returns pointer to double array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   REAL(RDOUBLE), POINTER :: R2(:,:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_D2 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'D',2_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_D2 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      R2=>NULL()
      RETURN
   END IF

   R2 => PFIND%D2
   RETURN

  END FUNCTION FLL_GETNDATA_D2
!
!  INTEGER
!
  FUNCTION FLL_GETNDATA_I0(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(I0)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! I0           Out        returns integer value
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   INTEGER(SINT) :: I0
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_I0 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'I',0_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_I0 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN
   END IF

   I0 = PFIND%I0
   RETURN

  END FUNCTION FLL_GETNDATA_I0



  FUNCTION FLL_GETNDATA_I1(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(I1)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! I1           Out        returns pointer to integer array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   INTEGER(SINT), POINTER :: I1(:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_I1 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'I',1_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_I1 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      I1=>NULL()
      RETURN
   END IF

   I1 => PFIND%I1
   RETURN

  END FUNCTION FLL_GETNDATA_I1

  FUNCTION FLL_GETNDATA_I2(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(I2)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! I2           Out        returns pointer to integer array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   INTEGER(SINT), POINTER :: I2(:,:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_I2 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'I',2_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_I2 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      I2=>NULL()
      RETURN
   END IF

   I2 => PFIND%I2
   RETURN

  END FUNCTION FLL_GETNDATA_I2
!
!  LONG INTEGER
!
  FUNCTION FLL_GETNDATA_L0(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(I0)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! I0           Out        returns long integer value
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   INTEGER(LINT) :: I0
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_L0 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'L',0_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_L0 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN
   END IF

   I0 = PFIND%L0

   RETURN

  END FUNCTION FLL_GETNDATA_L0



  FUNCTION FLL_GETNDATA_L1(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(I1)
   
    USE FLL_TYPE_M
    USE FLL_LOCATE_M
    USE FLL_OUT_M
   
    IMPLICIT NONE
!
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! I1           Out        returns pointer to long integer array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   INTEGER(LINT), POINTER :: I1(:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_L1 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'L',1_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_L1 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      I1=>NULL()
      RETURN
   END IF

   I1 => PFIND%L1
   RETURN

  END FUNCTION FLL_GETNDATA_L1

  FUNCTION FLL_GETNDATA_L2(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(I2)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! I2           Out        returns pointer to long integer array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   INTEGER(LINT), POINTER :: I2(:,:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_L2 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'L',2_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_L2 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      I2=>NULL()
      RETURN
   END IF

   I2 => PFIND%L2
   RETURN

  END FUNCTION FLL_GETNDATA_L2
  
  FUNCTION FLL_GETNDATA_S0(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(STRING)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! STRING       Out        returns string
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   CHARACTER(LEN=LSTRING_LENGTH) :: STRING
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_S0 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'S',0_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_S0 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN
   END IF

   STRING = PFIND%S0
   RETURN

  END FUNCTION FLL_GETNDATA_S0

FUNCTION FLL_GETNDATA_S1(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(STRING)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! STRING       Out        returns pointer to string array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   CHARACTER(LEN=LSTRING_LENGTH), POINTER :: STRING(:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_S1 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'S',1_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_S1 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      STRING=>NULL()
      RETURN
   END IF

   STRING = PNODE%S0
   RETURN

  END FUNCTION FLL_GETNDATA_S1

  FUNCTION FLL_GETNDATA_S2(PNODE,NAME,NUMBER,FPAR,ERRMSG) RESULT(STRING)
!
! Description: returns single real data of the node
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
! Declarations
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer to data set
! NAME         In         name of pointer
! NUMBER       Out        number of pointer if more pointers of the same type are present
! FPAR         In/Out     structure containing function specific data
! STRING       Out        returns pointer to string array
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   TYPE(DNODE), POINTER  :: PNODE
   CHARACTER(*) :: NAME
   INTEGER(LINT) :: NUMBER
   CHARACTER(LEN=LSTRING_LENGTH), POINTER :: STRING(:,:)
   CHARACTER(*), OPTIONAL :: ERRMSG
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PFIND
   CHARACTER(LEN=10) :: LOC_ERRMSG
!   
!  SET LOCAL ERRMSG
!
   IF(.NOT.PRESENT(ERRMSG))THEN
     LOC_ERRMSG='ALL'
   ELSE
     LOC_ERRMSG = ERRMSG
   END IF

   IF(.NOT.ASSOCIATED(PNODE))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A)')'FLL_GETNDATA_S2 - Null node '
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      RETURN      
   END IF

   PFIND => FLL_LOCATE(PNODE,NAME,'S',2_LINT,NUMBER,.FALSE.,FPAR,LOC_ERRMSG)

   IF(.NOT.ASSOCIATED(PFIND))THEN
      FPAR%SUCCESS = .FALSE.
      WRITE(FPAR%MESG,'(A,A,A,A)')'FLL_GETNDATA_S2 - Node ',&
            TRIM(PNODE%LNAME),' does not contain specified data ',NAME
      CALL FLL_OUT(LOC_ERRMSG,FPAR)
      STRING=>NULL()
      RETURN
   END IF

   STRING = PNODE%S0
   RETURN

  END FUNCTION FLL_GETNDATA_S2

END MODULE FLL_GETNDATA_M

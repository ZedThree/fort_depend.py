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
!
MODULE FLL_MPI_CP_M
!
! Description: Sends FLL list from one node to another
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
   FUNCTION FLL_MPI_CP(PNODE,COMMUNICATOR,SENDPART,RECPART,FPAR) RESULT(PNEW)
!
! Description: Sends FLL subset to a specified process in comunicator
!              if process ID == sending proceess ID, do not create
!              data set, just return the pointer on existing data set
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
    USE FLL_MK_M
    USE FLL_MV_M
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         node to duplicate
! PNEW         Out        duplicate node
! COMMUNICATOR In         MPI communicatior
! SENDPART     In         Sending partition
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE,PNEW
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: COMMUNICATOR,SENDPART
!
!  Local declarations
!
   TYPE(DNODE), POINTER :: PCHILD
   INTEGER :: RANK, IERR
!
!  check the node is not null
!
   FPAR%SUCCESS = .FALSE.
!
!  If not sending partition, nullify pointer
!  owherwise check that sending partition does not send NULL pointer and 
!  associate returning pointer with sending
!
   CALL MPI_Comm_rank ( COMMUNICATOR, RANK, IERR )

   DIFFPART: IF(SENDPART /= RANK)THEN
     PNEW => NULL()

     PNEW => BROADCAST_NODE_RECEIVE(COMMUNICATOR, SENDPART, FPAR)
!
!  If dir has childrenm loop over them
!  the number of children in this routine is stored in 
!  NLINK, not ndim, ndim is set to 0 and then incremented automatically
!  when adding children
!
     IF(PNEW%NLINK > 0)THEN
!
!  Node has children
!
       CALL FLL_RECEIVE_RECURSIVE(PNEW,COMMUNICATOR,SENDPART,FPAR)

     END IF

     RETURN 

   ELSE
!
!  Sending partition
!
     IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A)')' DUPLICATE - null node '
       CALL FLL_OUT('ALL',FPAR)
       FPAR%SUCCESS = .FALSE.
       RETURN
     END IF

     PNEW => PNODE

     CALL BROADCAST_NODE_SEND(PNODE, COMMUNICATOR, SENDPART, FPAR)
  
     PCHILD => PNODE%PCHILD
!
! If node has children, duplicate them too
!
     IF(ASSOCIATED(PCHILD))THEN

       CALL FLL_SEND_RECURSIVE(PCHILD,COMMUNICATOR,SENDPART,FPAR)

     END IF

   END IF DIFFPART

   FPAR%SUCCESS = .TRUE.

   RETURN

   END FUNCTION FLL_MPI_CP
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE FLL_SEND_RECURSIVE(PNODE,COMMUNICATOR,SENDPART,FPAR)
!
! Description: makes recursive duplicate of PNODE
!
! External Modules used
!
     USE FLL_TYPE_M
     USE FLL_MK_M
     USE FLL_MV_M
     USE FLL_OUT_M

     IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be duplicated
! SENDPART     In         sending partition rank
! COMMUNICATOR In         Commuticator
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
! 
    TYPE(DNODE), POINTER :: PNODE,PDUPL
    TYPE(FUNC_DATA_SET)  :: FPAR
    INTEGER              :: SENDPART,COMMUNICATOR
!
! Local declarations
!
    TYPE(DNODE), POINTER  :: PCURR, PNEXT,PNEW,PCHILD
    LOGICAL :: OK
!
    PCURR => PNODE
    PCHILD => PNODE%PCHILD
!  NODE IS DIR
!  LOOP OVER CHILDREN
!
    DO WHILE(ASSOCIATED(PCURR))

       PNEXT => PCURR%PNEXT
       PCHILD=> PCURR%PCHILD

       CALL BROADCAST_NODE_SEND(PCURR, COMMUNICATOR, SENDPART, FPAR)
!
!  NODE HAS CHILDREN
!
       DO WHILE(ASSOCIATED(PCHILD))
          
          CALL FLL_SEND_RECURSIVE(PCHILD,COMMUNICATOR,SENDPART,FPAR)
          PCHILD => PCHILD%PNEXT
         
       END DO
!
!  ADD TO PDUPL LIST
!
       PCURR => PNEXT

    END DO
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE FLL_SEND_RECURSIVE


  SUBROUTINE FLL_RECEIVE_RECURSIVE(PNODE,COMMUNICATOR,SENDPART,FPAR)
!
! Description: makes recursive duplicate of PNODE
!
! External Modules used
!
     USE FLL_TYPE_M
     USE FLL_MK_M
     USE FLL_MV_M
     USE FLL_OUT_M

     IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer which is to be duplicated
! PNEW         In         recevied pointer
! SENDPART     In         sending partition rank
! COMMUNICATOR In         Commuticator
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
! 
    TYPE(DNODE), POINTER :: PNODE,PNEW
    TYPE(FUNC_DATA_SET)  :: FPAR
    INTEGER              :: SENDPART,COMMUNICATOR
    INTEGER(LINT) :: I
!
! Local declarations
!
    TYPE(DNODE), POINTER  :: PCURR,PNEXT,PCHILD
    LOGICAL :: OK
    
    PCURR => PNODE
    PCHILD => PNODE%PCHILD
    
    DO I = 1, PCURR%NLINK

     PNEW => BROADCAST_NODE_RECEIVE(COMMUNICATOR,SENDPART,FPAR)
     OK = FLL_MV(PNEW,PCURR, FPAR)
     
     IF(PNEW%NLINK >0) CALL FLL_RECEIVE_RECURSIVE(PNEW,COMMUNICATOR,SENDPART,FPAR)
     
   END DO
!

  END SUBROUTINE FLL_RECEIVE_RECURSIVE
!
!
  SUBROUTINE BROADCAST_NODE_SEND(PNODE,COMMUNICATOR,SENDPART,FPAR)
!
! Description: Boradcast - send the node
!
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used
! 
    USE MPI
    USE FLL_TYPE_M
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer data which is to be duplicated
! COMMUNICATOR In         communicator
! SENDPART     In         sending partition
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER :: PNODE,PNEW
   TYPE(FUNC_DATA_SET)  :: FPAR
   INTEGER :: SENDPART, COMMUNICATOR
!
! Local declarations
!
   INTEGER(LINT) :: NDIM, NSIZE, CODE, IARR(3)
   INTEGER :: IERR
!
! Prepare header of the node
!
   CODE  = GET_NCODE(PNODE)
   NDIM  = PNODE%NDIM
   NSIZE = PNODE%NSIZE

   IARR(1) = CODE
   IARR(2) = NDIM
   IARR(3) = NSIZE
!
!  Send node header
!
   CALL MPI_BCAST(IARR, 3, MPI_INTEGER8, SENDPART,COMMUNICATOR, IERR)
   CALL MPI_BCAST(PNODE%LNAME, NAME_LENGTH, MPI_CHARACTER, SENDPART,COMMUNICATOR, IERR)
!   IF(IERR /= 0)THEN
!       WRITE(FPAR%MESG,'(A)')' GET_NCODE- null node '
!       CALL FLL_OUT('ALL',FPAR)
!       FPAR%SUCCESS = .FALSE.
!       CODE = -1
!       RETURN
!   END IF

   IF(CODE == 0) RETURN
   IF(NDIM*NSIZE > 1) THEN
!
!   1D ARRAYS
!
     IF(ASSOCIATED(PNODE%R1))THEN
       CALL MPI_BCAST(PNODE%R1, NDIM*NSIZE, MPI_REAL, SENDPART,COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D1))THEN
       CALL MPI_BCAST(PNODE%D1, NDIM*NSIZE, MPI_DOUBLE, SENDPART,COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I1))THEN
       CALL MPI_BCAST(PNODE%I1, NDIM*NSIZE, MPI_INTEGER, SENDPART,COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L1))THEN
       CALL MPI_BCAST(PNODE%L1, NDIM*NSIZE, MPI_INTEGER8, SENDPART,COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%S1))THEN
       CALL MPI_BCAST(PNODE%S1, NDIM*NSIZE*NAME_LENGTH, MPI_CHARACTER, SENDPART,COMMUNICATOR, IERR)
       RETURN     
     END IF
!
! 2D ARRAYS
!
     IF(ASSOCIATED(PNODE%R2))THEN
       CALL MPI_BCAST(PNODE%R2, NDIM*NSIZE, MPI_REAL, SENDPART,COMMUNICATOR, IERR)
      RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D2))THEN
      CALL MPI_BCAST(PNODE%D2, NDIM*NSIZE, MPI_DOUBLE, SENDPART,COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I2))THEN
       CALL MPI_BCAST(PNODE%I2, NDIM*NSIZE, MPI_INTEGER, SENDPART,COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L2))THEN
       CALL MPI_BCAST(PNODE%L2, NDIM*NSIZE, MPI_INTEGER8,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%S2))THEN
       CALL MPI_BCAST(PNODE%S2, NDIM*NSIZE*NAME_LENGTH, MPI_CHARACTER,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
!
!  SCALARS AND STATICALLY DEFINED ARRAYS
!
   ELSE
     SELECT CASE(CODE)
      CASE(1)
       CALL MPI_BCAST(PNODE%R0, 1, MPI_REAL, SENDPART,COMMUNICATOR, IERR)
      CASE(2)
       CALL MPI_BCAST(PNODE%D0, 1, MPI_DOUBLE, SENDPART,COMMUNICATOR, IERR)
      CASE(3)
       CALL MPI_BCAST(PNODE%I0, 1, MPI_INTEGER, SENDPART,COMMUNICATOR, IERR)
      CASE(4)
       CALL MPI_BCAST(PNODE%L0, 1, MPI_INTEGER8, SENDPART,COMMUNICATOR, IERR)
      CASE(5)
       CALL MPI_BCAST(PNODE%S0, NAME_LENGTH, MPI_CHARACTER, SENDPART,COMMUNICATOR, IERR)
     END SELECT
   END IF  

  END SUBROUTINE BROADCAST_NODE_SEND


  FUNCTION BROADCAST_NODE_RECEIVE(COMMUNICATOR,SENDPART,FPAR) RESULT(PNODE)
!
! Description: Boradcast - receive the node
!
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
! External Modules used
! 
    USE MPI
    USE FLL_TYPE_M
    USE FLL_OUT_M
    USE FLL_MK_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         pointer data which is to be duplicated
! COMMUNICATOR In         communicator
! SENDPART     In         sending partition
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER :: PNODE
   TYPE(FUNC_DATA_SET)  :: FPAR
   INTEGER :: COMMUNICATOR, SENDPART
!
! Local declarations
!
   INTEGER(LINT) :: IARR(3), NDIM, NSIZE
   INTEGER :: IERR
   CHARACTER(LEN=TYPE_LENGTH) :: TYPE
   CHARACTER(LEN=NAME_LENGTH) :: NAME
!
! Prepare header of the node
!
   CALL MPI_BCAST(IARR, 3, MPI_INTEGER8, SENDPART, COMMUNICATOR, IERR)
   CALL MPI_BCAST(NAME, NAME_LENGTH, MPI_CHARACTER, SENDPART,COMMUNICATOR, IERR)

   TYPE = GET_NTYPE(INT(IARR(1), KIND=SINT))

   IF(IARR(1) == 0)THEN
!
!  IF DIR, STORE THE NDIM DIMENSION IN NLINK AND 
!  KEEP NIDM = 0
!  UPON ADDING MODES TO DIR, NDIM IS INCREMENTED AUTOMATICALLY
!
     PNODE => FLL_MK(NAME,TYPE,0_LINT,0_LINT ,FPAR)
     PNODE%LNAME = NAME
     PNODE%NLINK = IARR(2)
     IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A)')' BROADCAST_NODE_RECEIVE - error allocating PNEW '
       FPAR%SUCCESS = .FALSE.
       PNODE => NULL()
       RETURN
     END IF
     RETURN

   ELSE
   
     PNODE => FLL_MK(NAME,TYPE,IARR(2),IARR(3) ,FPAR)
     IF(.NOT.ASSOCIATED(PNODE))THEN
       WRITE(FPAR%MESG,'(A)')' BROADCAST_NODE_RECEIVE - error allocating PNEW '
       FPAR%SUCCESS = .FALSE.
       PNODE => NULL()
       RETURN
     END IF
   END IF

   NDIM = IARR(2)
   NSIZE = IARR(3)
   IF(NDIM*NSIZE > 1 )THEN
!
!   1D ARRAYS
!
     IF(ASSOCIATED(PNODE%R1))THEN
       CALL MPI_BCAST(PNODE%R1, NDIM*NSIZE, MPI_REAL,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D1))THEN
       CALL MPI_BCAST(PNODE%D1, NDIM*NSIZE, MPI_DOUBLE,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I1))THEN
       CALL MPI_BCAST(PNODE%I1, NDIM*NSIZE, MPI_INTEGER,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L1))THEN
       CALL MPI_BCAST(PNODE%L1, NDIM*NSIZE, MPI_INTEGER8,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%S1))THEN
       CALL MPI_BCAST(PNODE%S1, NDIM*NSIZE*NAME_LENGTH, MPI_CHARACTER,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
!
! 2D ARRAYS
!
     IF(ASSOCIATED(PNODE%R2))THEN
       CALL MPI_BCAST(PNODE%R2, NDIM*NSIZE, MPI_REAL,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%D2))THEN
       CALL MPI_BCAST(PNODE%D2, NDIM*NSIZE, MPI_DOUBLE,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%I2))THEN
       CALL MPI_BCAST(PNODE%I2, NDIM*NSIZE, MPI_INTEGER,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%L2))THEN
       CALL MPI_BCAST(PNODE%L2, NDIM*NSIZE, MPI_INTEGER8,SENDPART, COMMUNICATOR, IERR)
       RETURN     
     END IF
     IF(ASSOCIATED(PNODE%S2))THEN
       CALL MPI_BCAST(PNODE%S2, NDIM*NSIZE*NAME_LENGTH, MPI_CHARACTER, SENDPART,COMMUNICATOR, IERR)
       RETURN     
     END IF
!
!  SCALARS AND STATICALLY DEFINED ARRAYS
!
   ELSE
     SELECT CASE(INT(IARR(1), KIND=SINT))
      CASE(1)
       CALL MPI_BCAST(PNODE%R0, 1, MPI_REAL, SENDPART,COMMUNICATOR, IERR)
      CASE(2)
       CALL MPI_BCAST(PNODE%D0, 1, MPI_DOUBLE, SENDPART,COMMUNICATOR, IERR)
      CASE(3)
       CALL MPI_BCAST(PNODE%I0, 1, MPI_INTEGER, SENDPART,COMMUNICATOR, IERR)
      CASE(4)
       CALL MPI_BCAST(PNODE%L0, 1, MPI_INTEGER8, SENDPART,COMMUNICATOR, IERR)
      CASE(5)
       PNODE%S0 = ''
       CALL MPI_BCAST(PNODE%S0, NAME_LENGTH, MPI_CHARACTER, SENDPART,COMMUNICATOR, IERR)
     END SELECT
   END IF

   RETURN
  
  END FUNCTION BROADCAST_NODE_RECEIVE





  FUNCTION GET_NCODE(PNODE) RESULT(CODE)
!
! Description: gives back code for node
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
! PNODE        In         pointer data which is to be duplicated
! CODE         Out        return code
!
! Arguments declaration
!
   TYPE(DNODE), POINTER :: PNODE
   INTEGER :: CODE
!
! Local declaration
! 
   IF(.NOT.ASSOCIATED(PNODE))THEN
    CODE = -1
    RETURN
   END IF

   SELECT CASE(TRIM(PNODE%LTYPE))

   CASE('DIR','N')
     CODE = 0
   CASE('R')
     CODE = 1
   CASE('D')
     CODE = 2
   CASE('I')
     CODE = 3
   CASE('L')
     CODE = 4
   CASE('S')
     CODE = 5
   CASE('C')
     CODE = 6
   CASE DEFAULT
     CODE = -1
   END SELECT


  END FUNCTION GET_NCODE



  FUNCTION GET_NTYPE(CODE) RESULT(TYPE)
!
! Description: gives back code for node
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
! PNODE        In         pointer data which is to be duplicated
! TYPE         Out        type of node
!
! Arguments declaration
!
   INTEGER :: CODE
   CHARACTER(LEN=TYPE_LENGTH):: TYPE
!
! Local declaration
!

    SELECT CASE(CODE)

    CASE(0)
      TYPE = 'DIR'
    CASE(1)
      TYPE = 'R'
    CASE(2)
      TYPE = 'D'
    CASE(3)
      TYPE = 'I'
    CASE(4)
      TYPE = 'L'
    CASE(5)
      TYPE = 'S'
    CASE(6)
      TYPE = 'C'
    CASE DEFAULT
      TYPE = '0'
  
    END SELECT


  END FUNCTION GET_NTYPE

 

END MODULE FLL_MPI_CP_M

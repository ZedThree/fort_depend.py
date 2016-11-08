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
MODULE FLL_DUPLICATE_M
!
! Description: Contains duplicates node
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
   FUNCTION FLL_DUPLICATE(PNODE,FPAR) RESULT(PNEW)
!
! Description: Contains duplicates node to PNEW mode
!              the parent, previous and next pointers of PNEW node are NULL
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
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE,PNEW
   TYPE(FUNC_DATA_SET) :: FPAR
!
! Local declarations
!
   TYPE(DNODE), POINTER :: PCHILD
!   
!   BODY OF SUBROUTINE
!   
!
!  check the node is not null
!
   PNEW => NULL()
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' DUPLICATE - null node '
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   PCHILD => PNODE%PCHILD
!
! IF NODE HAS CHILDREN, DUPLICATE ALL OF THEM
!
   IF(ASSOCIATED(PCHILD))THEN
     PNEW => FLL_MK(PNODE%LNAME,'DIR',0_LINT,0_LINT,FPAR)
     IF(.NOT.ASSOCIATED(PNEW))THEN
       WRITE(FPAR%MESG,'(A)')' DUPLICATE - error allocating PNEW '
       FPAR%SUCCESS = .FALSE.
       PNEW => NULL()
       RETURN
     END IF

     CALL FLL_DUPLICATE_RECURSIVE_NODE(PCHILD,PNEW,FPAR)
      IF(.NOT.FPAR%SUCCESS)THEN
        WRITE(FPAR%MESG,'(A)')' DUPLICATE - error duplicting children nodes '
        CALL FLL_OUT('ALL',FPAR)
        FPAR%SUCCESS = .FALSE.
        PNEW => NULL()
        RETURN
      END IF

   ELSE
!
!  NODE IS A FILE NODE
!
    PNEW => FLL_MK(PNODE%LNAME,PNODE%LTYPE,PNODE%NDIM,PNODE%NSIZE,FPAR)
    IF(.NOT.ASSOCIATED(PNEW))THEN
      WRITE(FPAR%MESG,'(A)')' DUPLICATE - error allocating PNEW '
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNEW => NULL()
      RETURN
    END IF
    CALL FLL_COPY_NODE_ARRAYS(PNODE, PNEW,FPAR)

   END IF

   FPAR%SUCCESS = .TRUE.
   RETURN

   END FUNCTION FLL_DUPLICATE
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE FLL_DUPLICATE_RECURSIVE_NODE(PNODE,PDUPL,FPAR)
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
! PDUPL        Out        duplicate of PNODE
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
! 
    TYPE(DNODE), POINTER  :: PNODE,PDUPL
    TYPE(FUNC_DATA_SET) :: FPAR
!
! Local declarations
!
    TYPE(DNODE), POINTER  :: PCURR, PNEXT,PNEW,PCHILD
    LOGICAL :: OK
!
    PCURR => PNODE
    PCHILD => PNODE%PCHILD
!
!  LOOP OVER CHILDREN
!
    DO WHILE(ASSOCIATED(PCURR))

     PNEXT => PCURR%PNEXT
     PCHILD=> PCURR%PCHILD

     PNEW => FLL_MK(PCURR%LNAME,PCURR%LTYPE,PCURR%NDIM,PCURR%NSIZE,FPAR)
     IF(.NOT.ASSOCIATED(PNEW))THEN
      WRITE(FPAR%MESG,'(A)')' DUPLICATE - error allocating PNEW '
      CALL FLL_OUT('ALL',FPAR)
      FPAR%SUCCESS = .FALSE.
      PNEW => NULL()
      RETURN
     END IF

     IF(.NOT.ASSOCIATED(PCHILD))THEN
       CALL FLL_COPY_NODE_ARRAYS(PCURR, PNEW, FPAR)
       OK = FLL_MV(PNEW, PDUPL, FPAR)
     ELSE
!
!  NODE HAS CHILDREN
!
       DO WHILE(ASSOCIATED(PCHILD))
        
         CALL FLL_DUPLICATE_RECURSIVE_NODE(PCHILD,PNEW, FPAR)
         IF(.NOT.FPAR%SUCCESS) STOP'DUPLICATE - Error duplicating nodes'
         PCHILD => PCHILD%PNEXT
      
       END DO

     END IF
!
!  ADD TO PDUPL LIST
!
     OK = FLL_MV(PNEW,PDUPL,FPAR)
     PCURR => PNEXT

    END DO
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE FLL_DUPLICATE_RECURSIVE_NODE
!
! 
!
  SUBROUTINE FLL_COPY_NODE_ARRAYS(PNODE,PNEW,FPAR)
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
! PNODE        In         pointer data which is to be duplicated
! PNEW         Out        duplicate of PNODE data 
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE,PNEW
   TYPE(FUNC_DATA_SET) :: FPAR
!
! Local declarations
!
   INTEGER(LINT) :: NDIM, NSIZE, NNDIM, NNSIZE
!
!  check node types
!
   IF(TRIM(PNODE%LTYPE) /= TRIM(PNEW%LTYPE))THEN
     WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
     FPAR%SUCCESS = .FALSE.
     CALL FLL_OUT('ALL',FPAR)
     RETURN
   END IF
!
!  COPY JUST NAME AND TYPE
!  THE REST IS GOING TO BE AUTOMATIC
!
   PNEW%LNAME = PNODE%LNAME
   PNEW%LTYPE = PNODE%LTYPE
!
!  IF DIR NODE, RETURN
!
   IF(TRIM(PNODE%LTYPE) == 'DIR' .OR. TRIM(PNODE%LTYPE) == 'N') RETURN
!
!   1D ARRAYS
!
   IF(ASSOCIATED(PNODE%R1))THEN
     NDIM   = SIZE(PNODE%R1, DIM = 1, KIND = LINT)

     IF(ASSOCIATED(PNEW%R1))THEN
       NNDIM  = SIZE(PNEW%R1, DIM = 1, KIND = LINT)

       IF(NDIM /= NNDIM)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%R1 = PNODE%R1
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - R1 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF     

   END IF
!
   IF(ASSOCIATED(PNODE%D1))THEN
     NDIM = SIZE(PNODE%D1, DIM = 1, KIND = LINT)

     IF(ASSOCIATED(PNEW%D1))THEN
       NNDIM = SIZE(PNEW%D1, DIM = 1, KIND = LINT)

       IF(NDIM /= NNDIM)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%D1 = PNODE%D1
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - D1 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF   

   END IF
!
   IF(ASSOCIATED(PNODE%I1))THEN

     NDIM = SIZE(PNODE%I1, DIM = 1, KIND = LINT)

     IF(ASSOCIATED(PNEW%I1))THEN
       NNDIM = SIZE(PNEW%I1, DIM = 1, KIND = LINT)

       IF(NDIM /= NNDIM)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF
       PNEW%I1(1:NDIM) = PNODE%I1(1:NDIM)
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - I1 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF  

   END IF
!
   IF(ASSOCIATED(PNODE%L1))THEN
     NDIM   = SIZE(PNODE%L1, DIM = 1, KIND = LINT)

     IF(ASSOCIATED(PNEW%L1))THEN
       NNDIM  = SIZE(PNEW%L1, DIM = 1, KIND = LINT)

       IF(NDIM /= NNDIM)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%L1 = PNODE%L1
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - L1 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF   

   END IF 

   IF(ASSOCIATED(PNODE%S1))THEN
     NDIM = SIZE(PNODE%S1, DIM = 1, KIND = LINT)

     IF(ASSOCIATED(PNEW%S1))THEN
       NNDIM = SIZE(PNEW%S1, DIM = 1, KIND = LINT)

       IF(NDIM /= NNDIM)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%S1 = PNODE%S1
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - S1 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF 
   END IF      
!
!  2D ARRAYS
!
     IF(ASSOCIATED(PNODE%R2))THEN
     NDIM    = SIZE(PNODE%R2, DIM = 1, KIND = LINT)
     NSIZE   = SIZE(PNODE%R2, DIM = 2, KIND = LINT)

     IF(ASSOCIATED(PNEW%R2))THEN
        NNDIM    = SIZE(PNODE%R2, DIM = 1, KIND = LINT)
        NNSIZE   = SIZE(PNODE%R2, DIM = 2, KIND = LINT)
        
       IF(NDIM /= NNDIM .OR. NSIZE /= NNSIZE)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%R2 = PNODE%R2
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - R2 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF     
   END IF
   
   IF(ASSOCIATED(PNODE%D2))THEN
     NDIM    = SIZE(PNODE%D2, DIM = 1, KIND = LINT)
     NSIZE   = SIZE(PNODE%D2, DIM = 2, KIND = LINT)

     IF(ASSOCIATED(PNEW%D2))THEN
        NNDIM    = SIZE(PNODE%D2, DIM = 1, KIND = LINT)
        NNSIZE   = SIZE(PNODE%D2, DIM = 2, KIND = LINT)
        
       IF(NDIM /= NNDIM .OR. NSIZE /= NNSIZE)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%D2 = PNODE%D2
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - D2 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF     
   END IF
   
   IF(ASSOCIATED(PNODE%I2))THEN
     NDIM    = SIZE(PNODE%I2, DIM = 1, KIND = LINT)
     NSIZE   = SIZE(PNODE%I2, DIM = 2, KIND = LINT)

     IF(ASSOCIATED(PNEW%I2))THEN
        NNDIM    = SIZE(PNODE%I2, DIM = 1, KIND = LINT)
        NNSIZE   = SIZE(PNODE%I2, DIM = 2, KIND = LINT)
        
       IF(NDIM /= NNDIM .OR. NSIZE /= NNSIZE)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%I2 = PNODE%I2
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - I2 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF     
   END IF
   
   IF(ASSOCIATED(PNODE%L2))THEN
     NDIM    = SIZE(PNODE%L2, DIM = 1, KIND = LINT)
     NSIZE   = SIZE(PNODE%L2, DIM = 2, KIND = LINT)

     IF(ASSOCIATED(PNEW%L2))THEN
        NNDIM    = SIZE(PNODE%L2, DIM = 1, KIND = LINT)
        NNSIZE   = SIZE(PNODE%L2, DIM = 2, KIND = LINT)
        
       IF(NDIM /= NNDIM .OR. NSIZE /= NNSIZE)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%L2 = PNODE%L2
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - L2 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF     
   END IF

   IF(ASSOCIATED(PNODE%S2))THEN
     NDIM    = SIZE(PNODE%S2, DIM = 1, KIND = LINT)
     NSIZE   = SIZE(PNODE%S2, DIM = 2, KIND = LINT)

     IF(ASSOCIATED(PNEW%L2))THEN
        NNDIM    = SIZE(PNODE%S2, DIM = 1, KIND = LINT)
        NNSIZE   = SIZE(PNODE%S2, DIM = 2, KIND = LINT)
        
       IF(NDIM /= NNDIM .OR. NSIZE /= NNSIZE)THEN
         WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - nodes do not have same dimensions', TRIM(PNODE%LNAME),' ', TRIM(PNEW%LNAME)
         FPAR%SUCCESS = .FALSE.
         CALL FLL_OUT('ALL',FPAR)
         RETURN
       END IF

       PNEW%S2 = PNODE%S2
     ELSE
       WRITE(FPAR%MESG,'(A,A,A,A)')' DUPLICATE - S2 array not allocated ',TRIM(PNEW%LNAME)
       FPAR%SUCCESS = .FALSE.
       CALL FLL_OUT('ALL',FPAR)
       RETURN 
     END IF     
   END IF
!
!  SCALARS AND STATICALLY DEFINED ARRAYS
!
  PNEW%R0 = PNODE%R0
  PNEW%D0 = PNODE%D0
  PNEW%I0 = PNODE%I0
  PNEW%L0 = PNODE%L0
  PNEW%S0 = PNODE%S0
  
  END SUBROUTINE FLL_COPY_NODE_ARRAYS
 

END MODULE FLL_DUPLICATE_M

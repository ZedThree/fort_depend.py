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
!     Subroutine FLL_CAT
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: prints linked list 
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
MODULE FLL_CAT_M
CONTAINS

   SUBROUTINE FLL_CAT(PNODE,IOUNIT,PARENT,FPAR)
   
    USE FLL_TYPE_M
    IMPLICIT NONE
!
!   SUBROUTINE REMOVES NODE
!
   TYPE(DNODE), POINTER  :: PNODE,PCHILD
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   INTEGER(LINT) :: POS
   LOGICAL :: PARENT
!
!   LOCAL TYPES
!
!   
!   BODY OF SUBROUTINE
!
   POS = 1
   FPAR%SUCCESS = .FALSE.
   IF(.NOT.ASSOCIATED(PNODE))THEN
      WRITE(FPAR%MESG,'(A)')' CAT - null node '
      FPAR%SUCCESS = .FALSE.
      RETURN
   END IF
   
   PCHILD => PNODE%PCHILD
   IF(PARENT) THEN
      IF(ASSOCIATED(PNODE%PPAR))WRITE(*,*)' ===> Node has a parent, its name is ', PNODE%PPAR%LNAME
   END IF
   CALL FLL_PRINT(PNODE, IOUNIT, POS, FPAR)
!
! IF NODE HAS CHILDREN PRINT THEM TOO
!
   IF(ASSOCIATED(PCHILD))CALL FLL_CAT_RECURSIVE_NODE(PCHILD,IOUNIT,POS,FPAR)

   FPAR%SUCCESS = .TRUE.

   RETURN
   END SUBROUTINE FLL_CAT
!
!  DELETE CHID WITH ALL ITS CHILDREN
!
  RECURSIVE SUBROUTINE FLL_CAT_RECURSIVE_NODE(PNODE,IOUNIT,POS,FPAR)
  
     USE FLL_TYPE_M
     IMPLICIT NONE
!
!   SUBROUTINE REMOVES NODE
! 
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
    INTEGER(LINT) :: POS 
   
    TYPE(DNODE), POINTER  :: PCURR, PNEXT, PCHILD
    INTEGER :: IOUNIT
!
!  IF NODE HAS CHILDREN
!
    POS = POS + 1
    PCURR => PNODE
!
!  IF CHILDREN, PRINT THEM TOO
!
    DO WHILE(ASSOCIATED(PCURR))

       CALL FLL_PRINT(PCURR, IOUNIT, POS, FPAR)

       PNEXT  => PCURR%PNEXT
       PCHILD => PCURR%PCHILD
       IF(ASSOCIATED(PCHILD))THEN
         CALL FLL_CAT_RECURSIVE_NODE(PCHILD,IOUNIT,POS,FPAR)
       END IF
       
       PCURR => PNEXT
    END DO
    
    POS = POS - 1 
    
    FPAR%SUCCESS = .TRUE.
    RETURN

  END SUBROUTINE FLL_CAT_RECURSIVE_NODE
!
!  FREE MEMORY FOR NODE
!
  SUBROUTINE FLL_PRINT(PNODE, IOUNIT, POS, FPAR)
    USE FLL_TYPE_M
    IMPLICIT NONE
!
!   SUBROUTINE REMOVES NODE
!
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   
   INTEGER :: IOUNIT
   INTEGER(LINT) :: POS,I,J,NDIM,NSIZE
   LOGICAL :: SAVED
   CHARACTER*2048 :: TEXT
   CHARACTER(2*POS) SPACE
   
   SAVED = .FALSE.
   
   NDIM = 3
   NSIZE = 3
   SPACE(:) = ' '
!
!   HEADERS
!
     IF(TRIM(PNODE%LTYPE) == 'DIR' .OR.TRIM(PNODE%LTYPE) == 'N')THEN
        WRITE(TEXT,*)"-",TRIM(PNODE%LTYPE),"-",SPACE,TRIM(PNODE%LNAME),'  ',   PNODE%NDIM
         WRITE(IOUNIT, *)TRIM(TEXT)
     ELSE 
        WRITE(TEXT,*)"-",TRIM(PNODE%LTYPE),"-  ",SPACE,TRIM(PNODE%LNAME),'  ',   PNODE%NDIM
         WRITE(IOUNIT, *)TRIM(TEXT)
     END IF
!
!  1 D ARRAYS
!
        IF(ASSOCIATED(PNODE%R1))THEN
          WRITE(TEXT,*)"     ",SPACE,(PNODE%R1(I), I = 1,MAX(NDIM,3_LINT))
         WRITE(IOUNIT, *)TRIM(TEXT)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%D1))THEN
          WRITE(TEXT,*)"     ",SPACE,(PNODE%D1(I), I = 1,MAX(NDIM,3_LINT))
          WRITE(IOUNIT, *)TRIM(TEXT)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%I1))THEN
          WRITE(TEXT,*)"     ",SPACE,(PNODE%I1(I), I = 1,MAX(NDIM,3_LINT))
          WRITE(IOUNIT, *)TRIM(TEXT)
          SAVED = .TRUE.
        ELSE IF(ASSOCIATED(PNODE%L1))THEN
          WRITE(TEXT,*)"     ",SPACE,(PNODE%L1(I), I = 1,MAX(NDIM,3_LINT))
          WRITE(IOUNIT, *)TRIM(TEXT)
          SAVED = .TRUE.
!
!  2D ARRAYS
!
       ELSE IF(ASSOCIATED(PNODE%R2))THEN
          WRITE(TEXT,*)"     ",SPACE,((PNODE%R2(I,J), J = 1,MAX(NSIZE,2_LINT)), I=1,MAX(NDIM,2_LINT))
          WRITE(IOUNIT, *)TRIM(TEXT)
          SAVED = .TRUE.
      ELSE IF(ASSOCIATED(PNODE%D2))THEN
          WRITE(TEXT,*)"     ",SPACE,((PNODE%D2(I,J), J = 1,MAX(NSIZE,2_LINT)), I=1,MAX(NDIM,2_LINT))
          WRITE(IOUNIT, *)TRIM(TEXT)
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%I2))THEN
          WRITE(TEXT,*)"     ",SPACE,((PNODE%I2(I,J), J = 1,MAX(NSIZE,2_LINT)), I=1,MAX(NDIM,2_LINT))
          WRITE(IOUNIT, *)TRIM(TEXT)
          SAVED = .TRUE.
       ELSE IF(ASSOCIATED(PNODE%L2))THEN
          WRITE(TEXT,*)"     ",SPACE,((PNODE%L2(I,J), J = 1,MAX(NSIZE,2_LINT)), I=1,MAX(NDIM,2_LINT))
          WRITE(IOUNIT, *)TRIM(TEXT)
          SAVED = .TRUE.
      END IF
!
!  CHECK IF NODE IS CONSTANT
!
      IF(.NOT.SAVED)THEN
        SELECT CASE(PNODE%LTYPE)
         CASE('R')
           WRITE(TEXT,*)"     ",SPACE,PNODE%R0
           WRITE(IOUNIT, *)TRIM(TEXT)
         CASE('D')
           WRITE(TEXT,*)"     ",SPACE,PNODE%D0
           WRITE(IOUNIT, *)TRIM(TEXT)         
        CASE('I')
           WRITE(TEXT,*)"     ",SPACE,PNODE%I0
           WRITE(IOUNIT, *)TRIM(TEXT)  
         CASE('L')
           WRITE(TEXT,*)"     ",SPACE,PNODE%L0
           WRITE(IOUNIT, *)TRIM(TEXT)  
        CASE('S')
           WRITE(TEXT,*)"     ",SPACE,PNODE%S
           WRITE(IOUNIT, *)TRIM(TEXT) 

         CASE DEFAULT 
         
         END SELECT
       END IF
       
     RETURN

   END SUBROUTINE FLL_PRINT
 

END MODULE FLL_CAT_M

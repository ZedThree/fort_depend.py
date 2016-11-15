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
!     Sample program
!
!     Date: 2016-10-10
! 
! 
!
!
!     Description: duplicate
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
    PROGRAM  FLL_TEST

    USE FLL_MODS_M
    USE FLL_TEST_SUBR_N
    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE
   TYPE(DNODE), POINTER  :: PNODE,PNEW,PNEW1,PNODE1,PTMP,PNEW2,PFIND
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT,I
   CHARACTER :: FMT
!
!   LOCAL TYPES
!
   LOGICAL :: OK
   CHARACTER :: FMT_LOC
   INTEGER :: ISTAT
   INTEGER(LINT) :: POS,NNODES
   REAL(RDOUBLE), POINTER :: PRESS(:),HUMID(:)

!
!  read TEST file and store it in PNODE
!  file is an ASCII file
!
!    PNODE => FLL_READ('TEST',8,'A',FPAR)
    PNODE => FLL_READ('PartitionedFile',8,'B',FPAR)
!
!  print node on the screen
!
   CALL FLL_CAT(PNODE, 6, .TRUE.,FPAR)
   WRITE(*,*)
   WRITE(*,*)'------------------------------------------------------1'
   stop
!
!  read TEST1 ASCII file and store it in PNODE1
!
   PNODE1 => FLL_READ('TEST1',8,'A',FPAR)
!
!  print PNODE1 on screen
!
  CALL FLL_CAT(PNODE1, 6, .TRUE.,FPAR)
   WRITE(*,*)
   WRITE(*,*)'------------------------------------------------------2'
!
!  save  PNODE1 as binary and ascii file
!
   OK =  FLL_WRITE(PNODE1,'File_2.txt',8,'B',FPAR)
   OK =  FLL_WRITE(PNODE1,'File_1.txt',8,'A',FPAR)

   WRITE(*,*)'------------------------------------------------------3'
!
!  MAKE A NEW NODE CALLED new_node AND MOVE ENTIRE PNODE INTO IT
!  THE NODE IS GOING TO CONTAIN OTHER NODESM, IT IS A DIR TYPE OF NODE
!  PRINT IT ON THE SCREEN
!
   PNEW1 => FLL_MK("new_node","DIR",0_LINT, 0_LINT,FPAR)
!
!  create a data node and add it to a PNEW1
!  the node will be called Humidity, will contain doubles, array of 10
!
   PTMP => FLL_MK('Humidity','D', 10_LINT, 1_LINT, FPAR)
   OK = FLL_MV(PTMP, PNEW1, FPAR)
!
!  ACCESS DATA OF NODE Humidity
!
!  can be done as
!  1. HUMID => 	PTMP%D1
!  2. Locating the node in PNEW1

   HUMID => FLL_GETNDATA_D1(PNEW1,'Humidity',1_LINT,FPAR)

   IF(.NOT.ASSOCIATED(HUMID))THEN
      STOP' DID NOT FIND HUMID'
   ELSE
!
!   with this array you can work as with a normal array
!
!  find dimensions
!  1. NDIM = PTMP%NDIM
!  2. SIZE FUNCTION
!
     WRITE(*,*)
     WRITE(*,*)'SIZE OF ARRAY IS ',SIZE(HUMID)
     WRITE(*,*)
   END IF
!
!  fill it with some data
!
   DO I=1,10
     HUMID(I) = 3.1415926*I
   END DO
   CALL FLL_CAT(PNEW1, 6, .TRUE.,FPAR)

! !
! !  add PNODE to PNEW1
! !  now the node contains Humidity and entire PNODE
! !
   OK = FLL_MV(PNODE, PNEW1, FPAR)
   
     WRITE(*,*)'------------------------------------------------------4-1'
    CALL FLL_CAT(pnode, 6, .TRUE.,FPAR)	
     WRITE(*,*)'------------------------------------------------------4-2'

   
   CALL FLL_CAT(PNEW1, 6, .TRUE.,FPAR)	
   WRITE(*,*)'------------------------------------------------------4'
! 
   CALL FLL_TEST_SUBR(PNODE,PNODE1)

   WRITE(*,*)'---BACK FROM SUBROUTINE --------------8'
   CALL FLL_CAT(PNODE, 6, .TRUE.,FPAR)	
   WRITE(*,*)'---BACK FROM SUBROUTINE --------------9'

!
   PFIND => NULL()
!
! SWEEP IN THE FIRST SUBDIR TEST_Subdir AND PRINT ALL ITEMS WHICH ARE CONTAINED THERE
!
   PTMP => FLL_LOCATE(PNODE,'TEST_Subdir','*',-1_LINT,1_LINT,.FALSE. ,FPAR)
   DO WHILE(FLL_SWEEP(PTMP, PFIND,'*', '*', -1_LINT,  FPAR))
      WRITE(*,*)'Name of node is ',PFIND%LNAME
   END DO
   
   CALL FLL_RM(PNODE1,FPAR)
   CALL FLL_RM(PNODE,FPAR)
   
   CALL FLL_CAT(PNEW1, 6, .FALSE.,FPAR)	
   WRITE(*,*)'---BACK FROM SUBROUTINE --------------9'
   
   CALL FLL_RM(PNEW1,FPAR)

  
END PROGRAM

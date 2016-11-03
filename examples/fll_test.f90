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
    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE
   TYPE(DNODE), POINTER  :: PNODE,PNEW,PNEW1,PNODE1,PTMP,PNEW2
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
   PNODE => FLL_READ('TEST',8,'A',FPAR)
!
!  print node on the screen
!
   CALL FLL_CAT(PNODE, 6, .TRUE.,FPAR)
   WRITE(*,*)
   WRITE(*,*)'------------------------------------------------------1'
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
   CALL FLL_CAT(PNEW1, 6, .TRUE.,FPAR)
!
!  ACCESS DATA OF NODE Humidity
!
!  can be done as
!  1. HUMID => 	PTMP%D1
!  2. Locating the node in PNEW1

   HUMID => FLL_GETNDATA_D1(PNEW1,'Humidity',1_LINT,'D',FPAR)

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
!
!  add PNODE to PNEW1
!  now the node contains Humidity and entire PNODE
!
   OK = FLL_MV(PNODE, PNEW1, FPAR)
   CALL FLL_CAT(PNEW1, 6, .TRUE.,FPAR)	
   WRITE(*,*)'------------------------------------------------------4'
!
!  SOME ADDITIONAL OPERATIONS
!

!
!  COPY PNODE, BECASE THE TARGET IN COPY IS NULL
!  THE PNEW IS GOING TO BE A DUPLICATE OF PNODE
!  AND IS GOING TO BE A NEW NODE
!
   PNEW => FLL_CP(PNODE, NULL(), FPAR)
   CALL FLL_CAT(PNEW, 6, .TRUE.,FPAR)
   WRITE(*,*)'------------------------------------------------------5'
!
!  find number 1st subdir IN PNODE1
! 
   NNODES =  FLL_NNODES(PNODE1,'TEST1_Subdir','*',-1_lint,.false.,FPAR)
   WRITE(*,*)
   WRITE(*,*)' number of TEST1_Subdir subsets is ', NNODES
   WRITE(*,*)
!
!   find the first TEST1_Subdir in PNODE1 and print it on the screen
!
   PTMP => FLL_LOCATE(PNODE1,'TEST1_Subdir',1_lint,'*',-1_LINT,.false.,FPAR)
   CALL FLL_CAT(PTMP, 6, .TRUE.,FPAR)
   WRITE(*,*)'------------------------------------------------------7'
!
!  find the values of pressure subset #1 and print the data on screen
!
    PRESS => FLL_GETNDATA_D1(PTMP,'pressure',1_LINT,'D',FPAR)
    write(*,*)' Values of pressure are ',PRESS
!
!  CLEANUP ALL MEMORY
!
   WRITE(*,*)' REMOVE ===========================' 
!
!  WE HAVE TO REMOVE PNODE1 AND PNEW WHICH WAS CREATED ON LINE 153
!  PNODE WAS DELETED AS PART OF PNEW1 WHERE IT WAS MOVED

!  PNEW2 IS NOT A NEW NODE, IT POINTS TO A COPY SO DOES NOT NEED TO BE FREED
!
   CALL FLL_RM(PNEW,FPAR)
   CALL FLL_RM(PNODE1,FPAR)
   CALL FLL_RM(PNODE,FPAR)

  
END PROGRAM

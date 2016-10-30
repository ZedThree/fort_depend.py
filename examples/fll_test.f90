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
   TYPE(DNODE), POINTER  :: PNODE,PNEW,PNODE2,PNEW1,PNODE1,PTMP,PNEW2
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   CHARACTER :: FMT
!
!   LOCAL TYPES
!
   LOGICAL :: OK
   CHARACTER :: FMT_LOC
   INTEGER :: ISTAT
   INTEGER(LINT) :: POS,NNODES
   REAL(RDOUBLE), POINTER :: PRESS(:)
!
!  read TEST file and store it in PNODE
!
   PNODE => FLL_READ('TEST',8,'A',FPAR)
!
! print Pnode on screen
!  
   CALL FLL_CAT(PNODE, 6, .TRUE.,FPAR)
   WRITE(*,*)'------------------------------------------------------1'
!
!  read TEST1 file and store it in PNODE1
!
   PNODE1 => FLL_READ('TEST1',8,'A',FPAR)
!
!  print PNODE1 on screen
!
  CALL FLL_CAT(PNODE1, 6, .TRUE.,FPAR)
   WRITE(*,*)'------------------------------------------------------2'
!
!  save  PNODE1 as binary and ascii file
!
   OK =  FLL_WRITE(PNODE1,'File_2.txt',8,'B',FPAR)
   OK =  FLL_WRITE(PNODE1,'File_1.txt',8,'A',FPAR)
!
!   read bindary file and store it in PNODE2
!   then print it on screed and then delete it
   PNODE2 => FLL_READ('File_2.txt',8,'B',FPAR)
   CALL FLL_CAT(PNODE2, 6, .TRUE.,FPAR)
   CALL FLL_RM(PNODE2,FPAR) 
   WRITE(*,*)'------------------------------------------------------3'
!
!  MAKE A NEW NODE AND MOVE ENTIRE PNODE INTO IT
!  PRINT IT ON THE SCREEN
!
    PNEW1 => FLL_MK("newnone","DIR",0_LINT, 0_LINT,FPAR)
    OK = FLL_MV(PNODE, PNEW1, FPAR)
   CALL FLL_CAT(PNEW1, 6, .TRUE.,FPAR)	
   WRITE(*,*)'------------------------------------------------------4'
!
!  COPY PNODE, BECASE THE TARGET IN COPY IS NULL
!  THE PNEW IS GOING TO BE A DUPLICATE OF PNODE
!  AND IS GOING TO BE A NEW NODE
!
   PNEW => FLL_CP(PNODE, NULL(), FPAR)
   CALL FLL_CAT(PNEW, 6, .TRUE.,FPAR)
   WRITE(*,*)'------------------------------------------------------5'
!
!  REMOVE PNEW1
!
   CALL FLL_RM(PNEW1,FPAR) 
!
!   COPY PNODE TO PNEW
!   ON RETURN, THE FLL_CP WILL GIVE BACK POINTER OF PNEW IN PNODE LIST
!
   PNEW2 => FLL_CP(PNEW, PNODE1, FPAR)
   CALL FLL_CAT(PNODE1, 6, .TRUE.,FPAR)
   WRITE(*,*)'------------------------------------------------------6'
!
!  find number 1st subdir IN PNODE1
! 
   NNODES =  FLL_NNODES(PNODE1,'TEST1_Subdir','*',.false.,FPAR)
   write(*,*)' number of TEST1_Subdir subsets is ', NNODES
!
!   find the first TEST1_Subdir in PNODE1 and print it on the screen
!
   PTMP => FLL_LOCATE(PNODE1,'TEST1_Subdir',1_lint,'*',.false.,FPAR)
   CALL FLL_CAT(PTMP, 6, .TRUE.,FPAR)
   WRITE(*,*)'------------------------------------------------------7'
!
!  find the values of pressure subset #1 and print them on screen
!
    PRESS => FLL_GETNDATA_D1(PTMP,'pressure',1_LINT,'D',FPAR)
    write(*,*)' Values of pressure are ',PRESS
!
!  CLEANUP ALL MEMORY
!
   WRITE(*,*)' REMOVE ===========================' 
!
!  WE HAVE TO REMOVE PNODE1 AND PNEW WHICH WAS CREATED ON LINE 107
!  PNODE WAS DELETED AS PART OF PNEW1 WHERE IT WAS MOVED
!  PNODE2 WAS DELETED RIGHT AFTER BEING RETURN FROM READ
!  PNEW2 IS NOT A NEW NODE, IT POINTS TO A COPY SO DOES NOT NEED TO BE FREED
!
   CALL FLL_RM(PNEW,FPAR)
   CALL FLL_RM(PNODE1,FPAR)

  
END PROGRAM

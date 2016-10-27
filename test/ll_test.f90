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
    PROGRAM  LL_TEST

    USE LL_MODS_M
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
   INTEGER(LINT) :: POS
!
!  read TEST file and store it in PNODE
!
   PNODE => LL_READ('TEST',8,'A',FPAR)
!
! print Pnode on screen
!  
   CALL LL_CAT(PNODE, 6, FPAR)
   WRITE(*,*)'------------------------------------------------------1'
!
!  read TEST1 file and store it in PNODE1
!
   PNODE1 => LL_READ('TEST1',8,'A',FPAR)
!
!  print PNODE1 on screen
!
  CALL LL_CAT(PNODE1, 6, FPAR)
   WRITE(*,*)'------------------------------------------------------2'
!
!  save  PNODE1 as binary and ascii file
!
   OK =  LL_WRITE(PNODE1,'File_2.txt',8,'B',FPAR)
   OK =  LL_WRITE(PNODE1,'File_1.txt',8,'A',FPAR)
!
!   read bindary file and store it in PNODE2
!   then print it on screed and then delete it
   PNODE2 => LL_READ('File_2.txt',8,'B',FPAR)
   CALL LL_CAT(PNODE2, 6, FPAR)
   CALL LL_RM(PNODE2,FPAR) 
   WRITE(*,*)'------------------------------------------------------3'
!
!  MAKE A NEW NODE AND MOVE ENTIRE PNODE INTO IT
!  PRINT IT ON THE SCREEN
!
    PNEW1 => LL_MK("newnone","DIR",0_LINT, 0_LINT,FPAR)
    OK = LL_MV(PNODE, PNEW1, FPAR)
   CALL LL_CAT(PNEW1, 6, FPAR)	
   WRITE(*,*)'------------------------------------------------------4'
!
!  COPY PNODE, BECASE THE TARGET IN COPY IS NULL
!  THE PNEW IS GOING TO BE A DUPLICATE OF PNODE
!  AND IS GOING TO BE A NEW NODE
!
   PNEW => LL_CP(PNODE, NULL(), FPAR)
   CALL LL_CAT(PNEW, 6, FPAR)
   WRITE(*,*)'------------------------------------------------------5'
!
!  REMOVE PNEW1
!
   CALL LL_RM(PNEW1,FPAR) 
!
!   COPY PNODE TO PNEW
!   ON RETURN, THE LL_CP WILL GIVE BACK POINTER OF PNEW IN PNODE LIST
!
   PNEW2 => LL_CP(PNEW, PNODE1, FPAR)
   CALL LL_CAT(PNODE1, 6, FPAR)
   WRITE(*,*)'------------------------------------------------------6'
!
!  LOCATE 1st subdir IN PNODE1 AND PRINT IT ON THE SCREEN
!
   PTMP => LL_LOCATE(PNODE1,'subdir',1_lint,'*',.false.,FPAR)
   CALL LL_CAT(PTMP, 6, FPAR)
   WRITE(*,*)'------------------------------------------------------7'
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
   CALL LL_RM(PNEW,FPAR)
   CALL LL_RM(PNODE1,FPAR)

  
END PROGRAM

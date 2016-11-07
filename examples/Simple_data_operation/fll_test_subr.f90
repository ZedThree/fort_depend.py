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
MODULE FLL_TEST_SUBR_N
CONTAINS

    SUBROUTINE  FLL_TEST_SUBR(PNODE,PNODE1)

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
   PTMP => FLL_LOCATE(PNODE1,'TEST1_Subdir','*',-1_LINT,1_LINT,.false.,FPAR)
   CALL FLL_CAT(PTMP, 6, .TRUE.,FPAR)
   WRITE(*,*)'------------------------------------------------------7'
!
!  find the values of pressure subset #1 and print the data on screen
!
    PRESS => FLL_GETNDATA_D1(PTMP,'pressure','D',1_LINT,FPAR)
    write(*,*)' Values of pressure are ',PRESS
!
!  PNEW WAS CREATED HERE, IT DOES NOT BELONG TO ANY NODE (WAS NOT MOVED TO ANY NODE)
!  HAS TO BE REMOVED HERE
!
   CALL FLL_RM(PNEW,FPAR)
	

  END SUBROUTINE  FLL_TEST_SUBR  
END MODULE FLL_TEST_SUBR_N

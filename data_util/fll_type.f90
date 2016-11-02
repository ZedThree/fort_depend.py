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
!     Subroutine FLL_TYPE_M
!
!     Date: 2016-10-10
! 
!     Description: Definition of the data set type for linked list
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
MODULE FLL_TYPE_M
!#ifdef f2003
!use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
!                                          stdout=>output_unit, &
!                                          stderr=>error_unit
!#else
!#define stdin  5
!#define stdout 6
!#define stderr 0
!#endif
!
!     MODULE SPECIFIC VARIABLES
!
  IMPLICIT NONE
  INTEGER, PARAMETER  :: RSINGLE = SELECTED_REAL_KIND(P=6,R=37)
  INTEGER, PARAMETER  :: RDOUBLE = KIND(0.D0)
  INTEGER, PARAMETER  :: SINT = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER  :: LINT = SELECTED_INT_KIND(16)

  INTEGER,  PARAMETER :: NAME_LENGTH = 16
  INTEGER,  PARAMETER :: TYPE_LENGTH = 4
  INTEGER,  PARAMETER :: ERR_MSG_LENGTH = 256
  INTEGER,  PARAMETER :: ERR_PATH_LENGTH = 1024
  INTEGER,  PARAMETER :: FILE_NAME_LENGTH = 1024
  INTEGER,  PARAMETER :: STRING_LENGHT = 72
!
! DEFINITION OF THE DATA SET OF THE NODE IN LINKED LIST
!
  TYPE DNODE
    CHARACTER(LEN=NAME_LENGTH)   :: LNAME = '' ! name the list
    CHARACTER(LEN=TYPE_LENGTH)     :: LTYPE = ''  ! type of the list
    CHARACTER(LEN=TYPE_LENGTH)     :: FTYPE = ''  ! type of the list
    INTEGER(LINT) :: NDIM  = 0, NSIZE = 0, NLINK = 0
    INTEGER(LINT) :: POS  = 0, LENGTH = 0 

    TYPE (DNODE), POINTER :: &
         PPAR     =>NULL(),&     ! Pointer to parent list
         PCHILD  =>NULL(),&     ! Pointer to first child list
         PNEXT   =>NULL(),&     ! Pointer to next list
         PPREV   =>NULL(),&     ! Pointer to previous list
         PLINK     =>NULL()        ! Pointer to link target
         
    REAL(RSINGLE)   , POINTER, CONTIGUOUS :: R1(:) =>NULL(), R2(:,:) =>NULL()
    REAL(RDOUBLE)   , POINTER, CONTIGUOUS :: D1(:) =>NULL(), D2(:,:) =>NULL()
    INTEGER(SINT)   , POINTER, CONTIGUOUS :: I1(:) =>NULL(), I2(:,:) =>NULL()
    INTEGER(LINT)   , POINTER, CONTIGUOUS :: L1(:) =>NULL(), L2(:,:) =>NULL()
    CHARACTER(LEN=STRING_LENGHT), POINTER, CONTIGUOUS  :: S1(:)=>NULL()
    CHARACTER(LEN=STRING_LENGHT), POINTER, CONTIGUOUS  :: S2(:,:)=>NULL()
    
    REAL(RSINGLE) :: R0
    REAL(RDOUBLE) :: D0
    INTEGER(SINT)  :: I0
    INTEGER(LINT)  :: L0
    CHARACTER(LEN=STRING_LENGHT) :: S
    CHARACTER :: C


    
  END TYPE DNODE
  
  TYPE FUNC_DATA_SET
      LOGICAL :: SUCCESS, ERRMSG
      CHARACTER(LEN=ERR_MSG_LENGTH)  :: MESG
      CHARACTER(LEN=ERR_PATH_LENGTH) :: ERRPATH
  END TYPE FUNC_DATA_SET

END MODULE FLL_TYPE_M

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
!
MODULE FLL_TYPE_M
!
! Description: efinition of the data set type for linked list and other related data
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

!
!  define std I/O desriptors

#ifdef f2008
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                          stdout=>output_unit, &
                                          stderr=>error_unit
#endif

!
! Declarations
!
! Arguments description
! Name                    Function
! PWHAT                   pointer which is to be copied
! PWHERE                  pointer which is to be copied
! PNEW                    pointer to new copy of PWHAT
! FPAR                   structure containing function specific data
!
!  RSINGLE    ! real kind
!  RDOUBLE    ! double kind
!  SINT       ! integer kind
!  LINT       ! long integer kind
!
!  NAME_LENGTH           length of string for names
!  TYPE_LENGTH           length of string for type
!  ERR_MSG_LENGTH        length of string for error message
!  ERR_PATH_LENGTH       length of string for path of in err message
!  FILE_NAME_LENGTH      length of string for file names
!  LSTRING_LENGTH         length of long string 

!  MAX_SUB               maximum number of subsets in each node
!
! Arguments declaration
!
  IMPLICIT NONE
#ifndef f2008  
  INTEGER, PARAMETER ::  STDIN =5
  INTEGER, PARAMETER ::  STDOUT=6
  INTEGER, PARAMETER ::  STDERR=0
#endif
  INTEGER, PARAMETER ::  IOSTATFILE=55
  INTEGER, PARAMETER ::  IOLOGFILE=99
  
  INTEGER, PARAMETER  :: RSINGLE = SELECTED_REAL_KIND(P=6,R=37)
  INTEGER, PARAMETER  :: RDOUBLE = KIND(0.D0)
  INTEGER, PARAMETER  :: SINT = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER  :: LINT = SELECTED_INT_KIND(16)

  INTEGER,  PARAMETER :: NAME_LENGTH = 16
  INTEGER,  PARAMETER :: TYPE_LENGTH = 4
  INTEGER,  PARAMETER :: ERR_MSG_LENGTH = 256
  INTEGER,  PARAMETER :: ERR_PATH_LENGTH = 1024
  INTEGER,  PARAMETER :: FILE_NAME_LENGTH = 1024
  INTEGER,  PARAMETER :: LSTRING_LENGTH = 72
  INTEGER,  PARAMETER :: SSTRING_LENGTH = NAME_LENGTH

  INTEGER, PARAMETER :: MAX_SUB = 32000
!
! DEFINITION OF THE DATA SET OF THE NODE IN LINKED LIST
!
  TYPE DNODE
    CHARACTER(LEN=NAME_LENGTH)     :: LNAME = ''  ! name the list
    CHARACTER(LEN=TYPE_LENGTH)     :: LTYPE = ''  ! type of the list
    CHARACTER(LEN=TYPE_LENGTH)     :: FTYPE = ''  ! type of the list
    INTEGER(LINT) :: NDIM = 0, NSIZE = 0, NLINK = 0
    INTEGER(LINT) :: FP   = 0                     ! byte position in file when reading data set

    TYPE (DNODE), POINTER :: &
         PPAR    =>NULL(),&     ! Pointer to parent list
         PCHILD  =>NULL(),&      ! Pointer to first child list
         PNEXT   =>NULL(),&      ! Pointer to next list
         PPREV   =>NULL(),&      ! Pointer to previous list
         PLINK   =>NULL()      ! Pointer to link target

#ifdef f2008         
    REAL(RSINGLE)   , POINTER, CONTIGUOUS :: R1(:) =>NULL(), R2(:,:) => NULL()  ! real arrays
    REAL(RDOUBLE)   , POINTER, CONTIGUOUS :: D1(:) =>NULL(), D2(:,:) => NULL()  ! double arrays
    INTEGER(SINT)   , POINTER, CONTIGUOUS :: I1(:) =>NULL(), I2(:,:) => NULL()  ! integer arrays
    INTEGER(LINT)   , POINTER, CONTIGUOUS :: L1(:) =>NULL(), L2(:,:) => NULL()  ! long integer arrays
    CHARACTER(LEN=LSTRING_LENGTH), POINTER, CONTIGUOUS  :: S1(:)  => NULL()     ! 1D array of strings
    CHARACTER(LEN=LSTRING_LENGTH), POINTER, CONTIGUOUS  :: S2(:,:)=> NULL()     ! 2D array of strings
#else
    REAL(RSINGLE)   , POINTER :: R1(:) =>NULL(), R2(:,:) => NULL()
    REAL(RDOUBLE)   , POINTER :: D1(:) =>NULL(), D2(:,:) => NULL()
    INTEGER(SINT)   , POINTER :: I1(:) =>NULL(), I2(:,:) => NULL()
    INTEGER(LINT)   , POINTER :: L1(:) =>NULL(), L2(:,:) => NULL()
    CHARACTER(LEN=LSTRING_LENGTH), POINTER :: S1(:)  => NULL()
    CHARACTER(LEN=LSTRING_LENGTH), POINTER :: S2(:,:)=> NULL()
#endif
    
    REAL(RSINGLE)   :: R0                                     ! real 
    REAL(RDOUBLE)   :: D0                                     ! double
    INTEGER(SINT)   :: I0                                     ! integer
    INTEGER(LINT)   :: L0                                     ! long integer
    CHARACTER(LEN=LSTRING_LENGTH) :: S0   ! string
    CHARACTER       :: C                                      ! character
    
  END TYPE DNODE
!
! type used for diagnostic data set passed to subroutines and functions
!
  TYPE FUNC_DATA_SET
      LOGICAL :: SUCCESS                                                          ! if .TRUE.=success, .FALSE. = fail
      CHARACTER(LEN=ERR_MSG_LENGTH)  :: MESG         ! error message
      CHARACTER(LEN=ERR_PATH_LENGTH) :: ERRPATH  ! path 
  END TYPE FUNC_DATA_SET

END MODULE FLL_TYPE_M

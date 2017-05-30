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
!     Description: creates mpi struct defining how many files will be 
!                  saved and which partitions will be saving to each 
!                  file
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
MODULE READ_INPUT_M
CONTAINS

  SUBROUTINE  READ_INPUT(NAME_OF_FILE,NFILES,NPROC)

  USE FLL_MODS_M
  IMPLICIT NONE
!
!   Local declarations
!
  INTEGER(LINT) :: NFILES, NPROC
  CHARACTER(LEN=*) :: NAME_OF_FILE
!
!  create node for name of the I/O file and add it to the main structure
!
  WRITE(*,*)' Name of file without suffix'
  READ(*,*)NAME_OF_FILE

  WRITE(*,*)' How many files'
  READ(*,*)NFILES

  WRITE(*,*)' How many processors'
  READ(*,*)NPROC

  RETURN
  END SUBROUTINE  READ_INPUT  
END MODULE READ_INPUT_M

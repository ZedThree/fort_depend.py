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
MODULE CREATE_MPI_STRUCT_M
CONTAINS

  SUBROUTINE  CREATE_MPI_STRUCT(PNODE)

  USE FLL_MODS_M
  IMPLICIT NONE
!   
!
  TYPE(DNODE), POINTER  :: PNODE,PTMP
  TYPE(FUNC_DATA_SET) :: FPAR
!
!  MAKE STRUCTURE
!
  PNODE => FLL_MKDIR('MPI-IO',FPAR)
!
  WRITE(*,*)' NAME OF OUTPUT FILE WITHOUT SUFFIX'
  READ(*,*)NAME_OF_FILE
!
!  create node for name of the I/O file and add it to the main structure
!
  PTMP  => FLL_MK('name-of-file','S', 1_LINT, 1_LINT, FPAR)
  PTMP%S0 = TRIM(NAME_OF_FILE)
  IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'
!
!  create node with number of files which are to be saved
!
  WRITE(*,*)' HOW MANY FILES DO YOU WANT TO SAVE'
  READ(*,*)NFILES
  PTMP  => FLL_MK('N-files','L', 1_LINT, 1_LINT, FPAR)
  PTMP%L0 = NFILES
  IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'

  
  DO I=1,NFILES

    

  END DO
   

  CALL FLL_CAT(PNODE, FPAR)

	

  END SUBROUTINE  CREATE_MPI_STRUCT  
END MODULE CREATE_MPI_STRUCT_M

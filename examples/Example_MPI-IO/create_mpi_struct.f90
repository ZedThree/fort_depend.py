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

  SUBROUTINE  CREATE_MPI_STRUCT(PNODE,NAME_OF_FILE,NFILES,NPROC)

  USE FLL_MODS_M
  IMPLICIT NONE

  TYPE(DNODE), POINTER  :: PNODE
!
!   Local declarations
!
  TYPE(DNODE), POINTER  :: PTMP
  TYPE(FUNC_DATA_SET) :: FPAR

  INTEGER(LINT) :: NFILES, I,J,NPROC, COUNT
  CHARACTER(LEN=*) :: NAME_OF_FILE
!
!  MAKE STRUCTURE
!
  PNODE => FLL_MKDIR('MPI-IO',FPAR)
!
!  create node for name of the I/O file and add it to the main structure
!
  PTMP  => FLL_MK('name-of-file','S', 1_LINT, 1_LINT, FPAR)
  PTMP%S0 = TRIM(NAME_OF_FILE)
  IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'
!
!  create node with number of files which are to be saved
!
  PTMP    => FLL_MK('N-files','L', 1_LINT, 1_LINT, FPAR)
  PTMP%L0 = NFILES
  IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'
!
!  create node with number processors the job will run at
!
  PTMP    => FLL_MK('N-proc','L', 1_LINT, 1_LINT, FPAR)
  PTMP%L0 = NPROC
  IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'
!
!  group processes 
!  there will be NFILES group
!  each group would have max NPROC/NFILES partitions and the increment 
!  between them is NFILES
!
!  in this way, each file should be associated to one partition from each node
!
  DO J=1,NFILES
!
! Make group and add it to the main structure
!
     PTMP => FLL_MK('group','L',NPROC/NFILES,1_LINT,FPAR)
     IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'
 
     COUNT = 1
     DO I=J,NPROC,NFILES
!
!  fill partition numbers
!
       PTMP%L1(COUNT) = I
       COUNT = COUNT + 1

     END DO

  END DO
!
!  print node on the screen and save to files
!
  CALL FLL_CAT(PNODE,6,.false., FPAR)
  IF(.NOT.FLL_WRITE(PNODE,"io.str", 9, 'A', FPAR))STOP'Error writing file'

  RETURN
  END SUBROUTINE  CREATE_MPI_STRUCT  
END MODULE CREATE_MPI_STRUCT_M

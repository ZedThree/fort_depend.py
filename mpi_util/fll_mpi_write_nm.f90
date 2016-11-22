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
MODULE FLL_MPI_WRITE_NM_M
!
! Description: contains subroutine writing file in paralell mode from N processors to M files
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
CONTAINS

  FUNCTION FLL_MPI_WRITE_NM(PNODE,PMPI,FPAR) RESULT(OK)
!
! Description: contains subroutine writing file in paralell mode from N processors to M files
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
  USE MPI
  USE FLL_MODS_M
  USE FLL_MPI_WRITE_M
  IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! FILE         In         Name of file
! PNODE        In         Node to be written
! IOUNIT       In         Number of unit
! FILE_TAB     In         Specifies which partition saves to which file
! FPAR         In/Out     structure containing function specific data
! OK           Out        Success or fail
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE,PMPI
   TYPE(FUNC_DATA_SET) :: FPAR
   LOGICAL OK
!
! Local declarations
!
   TYPE(DNODE), POINTER  :: PSUBPROC,PIOSTR,PIO
   INTEGER :: COMM,IOUNIT,WORLD_RANK,IERR,LOC_RANK
   INTEGER(LINT) :: I
   CHARACTER(LEN=NAME_LENGTH) :: NAME_OF_FILE

   OK = .FALSE.
!
!  get processor rank in world group
!
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, WORLD_RANK, IERR )
!
!  find MPI_prc_str -> Subprocs -> IO-NM_struct
!
   PSUBPROC => FLL_LOCATE(PMPI,'Subprocs','*',-1_LINT,1_LINT,.FALSE.,FPAR)
   PIOSTR   => FLL_LOCATE(PSUBPROC,'IO-NM_struct','*',-1_LINT,1_LINT,.FALSE.,FPAR)
!
!  loop over IO subsets
!
   DO I=1,PIOSTR%NDIM

        PIO   => FLL_LOCATE(PIOSTR,'IO','*',-1_LINT,I,.FALSE.,FPAR)
!
!  find: communicator
!        name of file to save into
!        descriptor for the file
!        local process rank, for each group of processes it will start with 0
!
        COMM = FLL_GETNDATA_I0(PIO,'communicator',1_LINT,FPAR)
        IF(COMM == MPI_COMM_NULL)CYCLE
        
        NAME_OF_FILE = FLL_GETNDATA_S0(PIO,'name-of-file',1_LINT,FPAR)
        IOUNIT = FLL_GETNDATA_I0(PIO,'io-descrpt', 1_LINT, FPAR)
        LOC_RANK = FLL_GETNDATA_I0(PIO,'loc_prc_rank', 1_LINT, FPAR)
!
!  Print some info
!
        WRITE(*,*)' Partition ',WORLD_RANK,' saving to :',trim(NAME_OF_FILE)
!
!  save file, ROOT_RANK is always 0, use local rank
! 
        OK = FLL_MPI_WRITE(PNODE,NAME_OF_FILE,IOUNIT,0, LOC_RANK, COMM, 'A', FPAR)

   END DO
  
  END FUNCTION FLL_MPI_WRITE_NM

END MODULE FLL_MPI_WRITE_NM_M

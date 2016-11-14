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
MODULE FLL_MPI_WRITE_M
!
! Description: contains subroutine writing file in paralell mode
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

  FUNCTION FLL_MPI_WRITE(PNODE,FILE,IOUNIT,ROOT_RANK, RANK, COMMUNICATOR, OPTION, FPAR) RESULT(OK)
!
! Description: contains subroutine writing file in paralell mode
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
  USE FLL_MPI_SUM_M
  IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! FILE         In         Name of file
! PNODE        Out        Node to a first node in list from a file
! IOUNIT       In         Number of unit
! OPTION       In         Type of write
! COMMUNICATOR In         communicator
! FPAR         In/Out     structure containing function specific data
! OK           Out        Success or fail
! ROOT_RANK    In         Rank of the root process 
! Rank         In         Rank of the process 
!
! Arguments declaration
!
   CHARACTER(*) :: FILE
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT,RANK, ROOT_RANK, COMMUNICATOR
   CHARACTER :: OPTION
   LOGICAL OK
!
! local declarations
!
   INTEGER :: ISTAT,IERR,NPROC
   INTEGER(LINT), ALLOCATABLE :: POS(:),DISPL(:)
   INTEGER(LINT) :: POS1,I,PART_NUM
!
!   DETERMINE RORMAT'
!
   OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='UNFORMATTED',&
      ACCESS='STREAM',IOSTAT=ISTAT)

   CALL MPI_BARRIER(COMMUNICATOR, IERR)

   IF(ISTAT/=0) THEN
    WRITE(FPAR%MESG,'(A,A)')' Write error opening file ',TRIM(FILE)
    CALL FLL_OUT('ALL',FPAR)
    FPAR%SUCCESS = .FALSE.
    OK = .FALSE.
    RETURN
   END IF
!
!  Get number of processors
!
   CALL MPI_Comm_size (COMMUNICATOR, NPROC, IERR )
!
!  Get length of each data set
!
   ALLOCATE(POS(NPROC+1), DISPL(NPROC+1), STAT = ISTAT)
    IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY'
   POS = 0

   POS(RANK+2) = FLL_GETNBYTES(PNODE,FPAR)
   IF(RANK == ROOT_RANK) POS(1) = 36 + 36 + (NPROC+1)*8
   CALL FLL_MPI_SUM(COMMUNICATOR, NPROC+1_LINT,L1=POS)

   WRITE(*,*)' --------   Size of data is ',POS
!
!  ... and distribute to all partitions
!
   CALL FLL_MPI_SUM(COMMUNICATOR, 1_LINT*NPROC,L1=POS)
!
!  Calculate displacement
!
   PART_NUM = FLL_GETNDATA_L0(PNODE, 'part_number',1_LINT, FPAR)

   DISPL = 0

   DO I=1,PART_NUM+1
     DISPL(I) = DISPL(I) + POS(I)
   END DO
   CALL FLL_MPI_SUM(COMMUNICATOR, NPROC+1_LINT,L1=DISPL)
!
!  Position in file with empty write statement
!
   IF(RANK == ROOT_RANK)THEN
    WRITE(IOUNIT,POS=0)
    POS1 = FLL_PART_FILE_HEADER(IOUNIT, NPROC, DISPL, FPAR) 
   ELSE
     WRITE(IOUNIT,POS=DISPL(RANK+2))
   END IF
!
!  Write linked list
! 
   CALL FLL_WRITE_LIST(PNODE,IOUNIT,'B',FPAR)
!
!  MPI_Barrier does not need to be here, 
!  just for testing purposes 
!
   CLOSE(IOUNIT)
   IF(.NOT.ASSOCIATED(PNODE))THEN
     WRITE(FPAR%MESG,'(A,A)')' Read  - error reading file ',TRIM(FILE)
     CALL FLL_OUT('ALL',FPAR)
     FPAR%SUCCESS = .FALSE.
     OK = .FALSE.
     RETURN
   END IF

   DEALLOCATE(POS, DISPL, STAT = ISTAT)
    IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY'
    
   OK = .TRUE.
   RETURN
  
  END FUNCTION FLL_MPI_WRITE


  FUNCTION FLL_PART_FILE_HEADER(IOUNIT, NPROC, DISPL, FPAR) RESULT(POS)
!
! Description: contains subroutine writing file in paralell mode
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
  USE FLL_MODS_M
  IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! NPROC        In         Number of processes
! DISPL        In         Length of each partition record
! IOUNIT       In         Number of unit
! FPAR         In/Out     structure containing function specific data
! OK           Out        Success or fail
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT,NPROC
   INTEGER(LINT) :: DISPL(:),POS
!
! Local declarations
!
   TYPE(DNODE), POINTER :: PTMP

   PTMP => FLL_MKDIR('partitioned_file', FPAR)
   PTMP%NDIM = NPROC + 1   ! Number of partitioned solutions and displacement vector
   CALL FLL_SAVE_NODE_B(PTMP, IOUNIT, 0_LINT, FPAR)
   POS = FLL_GETNBYTES(PTMP,FPAR)
   CALL FLL_RM(PTMP,FPAR)

   PTMP  => FLL_MK('displacements','L', NPROC+1_LINT, 1_LINT, FPAR)
!
! THE DATA CAN BE ACCESSE DIRECTLY THROUGH PTMP%D(:)
!
   POS = POS + FLL_GETNBYTES(PTMP,FPAR)
   DISPL(1) = POS

   PTMP%L1 = DISPL
   CALL FLL_SAVE_NODE_B(PTMP, IOUNIT, 0_LINT, FPAR)
   CALL FLL_RM(PTMP,FPAR)

   RETURN 

  END FUNCTION FLL_PART_FILE_HEADER
END MODULE FLL_MPI_WRITE_M

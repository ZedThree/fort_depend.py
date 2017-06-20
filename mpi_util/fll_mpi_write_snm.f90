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
MODULE FLL_MPI_WRITE_SNM_M
!
! Description: contains subroutine writing file in paralell mode from N processors to M files
!              in serial mode
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       22/11/16                         Initial implementation
!
!
! External Modules used
!
CONTAINS

  FUNCTION FLL_MPI_WRITE_SNM(PNODE,PMPI,FPAR) RESULT(OK)
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
  USE FLL_MPI_CP_M
  IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! FILE         In         Name of file
! PNODE        In         Node to be written
! IOUNIT       In         Number of unit
! FILE_TAB     In         Specifies which process saves to which file
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
   TYPE(DNODE), POINTER  :: PSUBPROC,PIOSTR,PIO,PDISPL,PMAIN,PTMP
   INTEGER :: COMM,IOUNIT,WORLD_RANK,IERR,ISTAT,LOCRANK,TMPINT
   INTEGER(LINT) :: I,J
   CHARACTER(LEN=NAME_LENGTH) :: NAME_OF_FILE
   INTEGER(LINT), POINTER :: PROC_NUM(:)

   INTEGER(LINT), ALLOCATABLE :: POS(:),DISPL(:)
   INTEGER(LINT) :: POS1,NPROC

   OK = .FALSE.
!
!  get processor rank in world group
!
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, WORLD_RANK, IERR )
!
!  find MPI_prc_str -> Subprocs -> IO-SNM_struct
!
   PSUBPROC => FLL_LOCATE(PMPI,'Subprocs','*',-1_LINT,1_LINT,.FALSE.,FPAR)
   PIOSTR   => FLL_LOCATE(PSUBPROC,'IO-SNM_struct','*',-1_LINT,1_LINT,.FALSE.,FPAR)
!
!  loop over IO subsets
!
   DO I=1,PIOSTR%NDIM

        PIO  => FLL_LOCATE(PIOSTR,'IO','*',-1_LINT,I,.FALSE.,FPAR)
!
!  find: communicator
!        name of file to save into
!        descriptor for the file
!        local process rank, for each group of processes it will start with 0
!
        COMM = FLL_GETNDATA_I0(PIO,'communicator',1_LINT,FPAR)
        IF(COMM == MPI_COMM_NULL)CYCLE

        CALL MPI_Comm_rank ( COMM, LOCRANK, IERR )

        NAME_OF_FILE = FLL_GETNDATA_S0(PIO,'name-of-file',1_LINT,FPAR)
        IOUNIT = FLL_GETNDATA_I0(PIO,'io-descrpt', 1_LINT, FPAR)
        PROC_NUM => FLL_GETNDATA_L1(PIO,'proc', 1_LINT, FPAR)
        NPROC = SIZE(PROC_NUM, DIM = 1, KIND = LINT)
!
!  if first process in group get data from other processes and
!  save the file
!
        IF(LOCRANK == 0)THEN
!
!  Create header for the file 
!
          PMAIN => FLL_MKDIR('partitioned_file', FPAR)
          PMAIN%NDIM = NPROC + 1   ! Number of processed solutions and displacement vector
          PDISPL  => FLL_MK('displacements','L', NPROC+1_LINT, 1_LINT, FPAR)
          OK = FLL_MV(PDISPL, PMAIN, FPAR)
!
!  ADD ROOT PARTITION SOLUTION
!
          PTMP => FLL_CP(PNODE, PMAIN, FPAR)

          ALLOCATE(POS(NPROC+1), DISPL(NPROC+1), STAT = ISTAT)
          IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY ==> fll_mpi_write_snm ERR:131 '
!
!  get length of each data subset of actual data
!
          POS = 0
!
!  the first subset is a header
!  header = 16 + 4 + 8 + 8 (name, type, ndim, nsize)
!
!  header + long int array 
!
          POS(1) = 36 + 36 + (NPROC+1)*8
          POS(2) = FLL_GETNBYTES(PNODE,FPAR)
!
!  get solutions from associated processes and add it to the main tree
!
          DO J=2,NPROC
            TMPINT = J-1
            PTMP => FLL_MPI_CP(PNODE,COMM,TMPINT,0,FPAR)
            OK = FLL_MV(PTMP, PMAIN,FPAR)
            POS(J+1) = FLL_GETNBYTES(PTMP,FPAR)
          END DO
!
!  Calculate displacement, ie where each subset starts
!
!  the first subset will start always at position 1
!  ie. beginning of the file
!
          DISPL(1) = 1

          DO J=2,NPROC+1
            DISPL(J) = DISPL(J-1) + POS(J-1)
          END DO

          PDISPL%L1 = DISPL

!          WRITE(*,*)' Partition ',WORLD_RANK,' saving to :',trim(NAME_OF_FILE)
          IF(.NOT.FLL_WRITE(PMAIN,TRIM(NAME_OF_FILE), IOUNIT, 'B', FPAR))STOP'Error writing file'
!          WRITE(*,*)' Partition ',WORLD_RANK,' done saving to :',trim(NAME_OF_FILE)
!
!  Clean memory
!
          CALL FLL_RM(PMAIN, FPAR)
          RETURN

        ELSE
!
!  Other then group root process, copy data set to root processes
!
          PTMP => FLL_MPI_CP(PNODE,COMM,LOCRANK,0,FPAR)

          RETURN

        END IF 

   END DO
  
  END FUNCTION FLL_MPI_WRITE_SNM

END MODULE FLL_MPI_WRITE_SNM_M

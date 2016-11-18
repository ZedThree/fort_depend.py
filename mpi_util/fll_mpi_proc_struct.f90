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
!     Description: creates global structure for MPI saving process and subprocesses structure 
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
MODULE FLL_MPI_PROC_STRUCT_M
!
! Description: Contains functions prepairing MPI structures
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

  FUNCTION  FLL_MPI_PROC_STRUCT(FPAR) RESULT(PNODE)
!
! Description: Creates structure with header for MPI process definition
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
  IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         Pointer to MPI structure
!
! Arguments declaration
!
  TYPE(DNODE), POINTER  :: PNODE
!
!   Local declarations
!
  TYPE(DNODE), POINTER  :: PTMP,PSUBPROC
  TYPE(FUNC_DATA_SET) :: FPAR

  INTEGER :: WORLD_GROUP_ID, IERR, NPROC

  LOGICAL :: OK

!
!  MAKE STRUCTURE
!
  PNODE => FLL_MKDIR('MPI_prc_str',FPAR)

  CALL MPI_Comm_group ( MPI_COMM_WORLD, WORLD_GROUP_ID, IERR )
  CALL  MPI_Comm_size ( MPI_COMM_WORLD, NPROC, IERR )

  PTMP => FLL_MK('World_comm', 'I', 1_LINT, 1_LINT, FPAR)
  PTMP%I0 = MPI_COMM_WORLD
  OK = FLL_MV(PTMP, PNODE, FPAR)

  PTMP => FLL_MK('World_group', 'I', 1_LINT, 1_LINT, FPAR)
  PTMP%I0 = WORLD_GROUP_ID
  OK = FLL_MV(PTMP, PNODE, FPAR)

  PTMP => FLL_MK('Nproc', 'I', 1_LINT, 1_LINT, FPAR)
  PTMP%I0 = NPROC
  OK = FLL_MV(PTMP, PNODE, FPAR)


  PSUBPROC  => FLL_MKDIR('Subprocs',FPAR)
  IF(.NOT.FLL_MV(PSUBPROC, PNODE, FPAR))THEN
    WRITE(FPAR%MESG,'(A)')' FLL_MPI_PROC_STRUCT: Error moving Subprocs'
    CALL FLL_OUT('ALL',FPAR)
    FPAR%SUCCESS = .FALSE.
    RETURN
   END IF


  PTMP  => FLL_MKDIR('IO_struct',FPAR)
  IF(.NOT.FLL_MV(PTMP, PSUBPROC, FPAR))THEN
    WRITE(FPAR%MESG,'(A)')' FLL_MPI_PROC_STRUCT: Error moving IO_struct'
    CALL FLL_OUT('ALL',FPAR)
    FPAR%SUCCESS = .FALSE.
    RETURN
   END IF

  FPAR%SUCCESS = .TRUE.
  RETURN
  END FUNCTION  FLL_MPI_PROC_STRUCT  



  SUBROUTINE  FLL_IO_STRUCT(PNODE,NAME_OF_FILE,EXTENSION,NFILES,FPAR)
!
! Description: Contains function prepiring MPI I/O structure
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
    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         Pointer to MPI structure
! NAME_OF_FILE In         Name of output file
! EXTENSION    In         File extension (suffix)
! NFILES       In         Number of files
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
    TYPE(DNODE), POINTER  :: PNODE
    TYPE(FUNC_DATA_SET) :: FPAR
    CHARACTER(LEN=*) :: NAME_OF_FILE,EXTENSION
    INTEGER(LINT) :: NFILES
!
!   Local declarations
!
    TYPE(DNODE), POINTER  :: PTMP,PSUBPROC,PIOSTR,PDIR
    INTEGER(LINT) :: I,J, COUNT,NSTEP
    INTEGER :: IERR,NPROC,WORLD_GROUP_ID,GROUP_ID, COMM_ID
    CHARACTER(LEN=NAME_LENGTH) :: FILENAME
    CHARACTER(LEN=5) :: STR
    INTEGER, ALLOCATABLE :: EVEN_RANK(:)
!
!  MAKE STRUCTURE
!
    PSUBPROC => FLL_LOCATE(PNODE,'Subprocs','*',-1_LINT,1_LINT,.FALSE.,FPAR)
    PIOSTR   => FLL_LOCATE(PSUBPROC,'IO_struct','*',-1_LINT,1_LINT,.FALSE.,FPAR)

    CALL MPI_Comm_size(  MPI_COMM_WORLD, NPROC, IERR )
    CALL MPI_Comm_group( MPI_COMM_WORLD, WORLD_GROUP_ID, IERR )
!
!  group processes 
!  there will be NFILES group
!  each group would have max NPROC/NFILES partitions and the increment 
!  between them is NFILES
!
!  in this way, each file should be associated to one partition from each node
!
    NSTEP = NPROC/NFILES

    ALLOCATE(EVEN_RANK(NSTEP), STAT = IERR)
     IF(IERR /= 0)STOP' ERROR ALLOCATING MEMORY'

    DO J=1,NFILES
!
!  create node for name of the I/O file and add it to the main structure
!
      PDIR => FLL_MKDIR('IO', FPAR)
      IF(.NOT.FLL_MV(PDIR, PIOSTR, FPAR))STOP' ERROR MOVING NODE'

      WRITE(STR,'(I5)')J
      WRITE(FILENAME,*)ADJUSTL(TRIM(NAME_OF_FILE))//"_",TRIM(ADJUSTL(STR))//".",ADJUSTL(TRIM(EXTENSION))

      PTMP  => FLL_MK('name-of-file','S', 1_LINT, 1_LINT, FPAR)
      PTMP%S0 = TRIM(FILENAME)
      IF(.NOT.FLL_MV(PTMP, PDIR, FPAR))STOP' ERROR MOVIN NODE'
!
!  create node with number processors the job will run at
!
      PTMP    => FLL_MK('proc','L', NSTEP, 1_LINT, FPAR)
      IF(.NOT.FLL_MV(PTMP, PDIR, FPAR))STOP' ERROR MOVING NODE'
!
!  group processes 
!  there will be NFILES group
!  each group would have max NPROC/NFILES partitions and the increment 
!  between them is NFILES
!
!  in this way, each file should be associated to one partition from each node
!
      COUNT = 1
      DO I=J,NPROC,NSTEP
!
!  fill partition numbers
!
         PTMP%L1(COUNT)   = I
!
!  Partition is always +1 larger than rank of the process
!
         EVEN_RANK(COUNT) = I-1
         COUNT = COUNT + 1

      END DO

      CALL MPI_Group_incl(WORLD_GROUP_ID, INT(NSTEP, KIND = SINT), EVEN_RANK, GROUP_ID, IERR )
      CALL MPI_Comm_create(MPI_COMM_WORLD, GROUP_ID, COMM_ID, IERR )

      PTMP    => FLL_MK('communicator','I', 1_LINT, 1_LINT, FPAR)
      PTMP%I0 = COMM_ID
      IF(.NOT.FLL_MV(PTMP, PDIR, FPAR))STOP' ERROR MOVING NODE'

      CALL MPI_GROUP_FREE(GROUP_ID, IERR)
!
!  print node on the screen and save to files
!
    END DO

    DEALLOCATE(EVEN_RANK, STAT = IERR)
     IF(IERR /= 0)STOP' ERROR ALLOCATING MEMORY'

    RETURN

  END SUBROUTINE  FLL_IO_STRUCT  



END MODULE FLL_MPI_PROC_STRUCT_M

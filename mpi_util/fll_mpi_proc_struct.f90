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
  CALL  MPI_Comm_size ( MPI_COMM_WORLD, NPROC, ierr )

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
!
!  print node on the screen and save to files
!
  CALL FLL_CAT(PNODE,6,.false., FPAR)

  FPAR%SUCCESS = .TRUE.
  RETURN
  END FUNCTION  FLL_MPI_PROC_STRUCT  
END MODULE FLL_MPI_PROC_STRUCT_M

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
!
MODULE FLL_MPI_MV_M
!
! Description: Moves FLL list from one node to another
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
   FUNCTION FLL_MPI_MV(PNODE,COMMUNICATOR,SENDPART,RECPART,FPAR) RESULT(PNEW)
!
! Description: Sends FLL subset to a specified process in comunicator
!              if process ID == sending proceess ID, do not create
!              data set, just return the pointer on existing data set
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
    USE FLL_TYPE_M
    USE FLL_MPI_CP_M
    USE FLL_RM_M
    USE FLL_OUT_M

    IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! PNODE        In         node to duplicate
! PNEW         Out        duplicate node
! COMMUNICATOR In         MPI communicatior
! SENDPART     In         Sending partition
! RECPART      In         Receiving partition
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
   TYPE(DNODE), POINTER  :: PNODE,PNEW
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: COMMUNICATOR,SENDPART,RECPART
!
!  Local declarations
!
   TYPE(DNODE), POINTER :: PCHILD
   INTEGER :: RANK, IERR
!
!  if not in group, return
!
   IF(COMMUNICATOR == MPI_COMM_NULL)RETURN
!
!  check the node is not null
!
   FPAR%SUCCESS = .FALSE.
!
!  If not sending partition, nullify pointer
!  owherwise check that sending partition does not send NULL pointer and 
!  associate returning pointer with sending
!
   CALL MPI_Comm_rank ( COMMUNICATOR, RANK, IERR )
   PNEW => NULL()
   PNEW => FLL_MPI_CP(PNODE,COMMUNICATOR,SENDPART,RECPART,FPAR)

   DIFFPART: IF(SENDPART == RANK)THEN
     CALL FLL_RM(PNODE,FPAR)
     PNEW => NULL()
   END IF DIFFPART

   FPAR%SUCCESS = .TRUE.

   RETURN

   END FUNCTION FLL_MPI_MV

END MODULE FLL_MPI_MV_M

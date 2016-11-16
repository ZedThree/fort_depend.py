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
MODULE FLL_MPI_READ_M
!
! Description: contains subroutine reading file in paralell mode
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

  FUNCTION FLL_MPI_READ(FILE,IOUNIT,ROOT_RANK, RANK, COMMUNICATOR, OPTION, FPAR) RESULT(PNODE)
!
! Description: contains subroutine readig file in paralell mode
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
!
!
!  structure of the MPI file is as follows
!
!  the main directory is a partitioned_file
!     followed by displacement which is a byte position of each partiton in the file 
!     each data on patition is in subset partition
!  
!   this is an example of a file with four partitions
!
! -DIR-   5\           partitioned_file
!   -L-     5x1            displacements                                1                  113             40000301
!   -DIR-   4\              partition
!   -L-     1x1               part_number                                   1
!   -D-     1000000x1               pressure     
!   -D-     1000000x1               density     
!   -D-     1000000x3               velocity        
!   -DIR-   4\              partition
!   -L-     1x1               part_number                                   2
!   -D-     1100000x1               pressure     
!   -D-     1100000x1               density     
!   -D-     1100000x3               velocity     
!   -DIR-   4\              partition
!   -L-     1x1               part_number                                   3
!   -D-     1200000x1               pressure   
!   -D-     1200000x1               density   
!   -D-     1200000x3               velocity    
!   -DIR-   4\              partition
!   -L-     1x1               part_number                                   4
!   -D-     1300000x1               pressure  
!   -D-     1300000x1               density   
!   -D-     1300000x3               velocity    


!
! External Modules used
!
  USE MPI
  USE FLL_MODS_M
  USE FLL_MPI_CP_ALL_M
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
!
! local declarations
!
   TYPE(DNODE), POINTER  :: PTMP, PTMP1
   INTEGER :: ISTAT,IERR
   INTEGER(LINT), POINTER :: DISPL(:)
   INTEGER(LINT) :: POS
!
!  if not in group, return
!
   IF(COMMUNICATOR == MPI_COMM_NULL)RETURN
!
!   use always binary fomat
!
   OPEN(UNIT=IOUNIT,STATUS='UNKNOWN',FILE=TRIM(FILE),FORM='UNFORMATTED',&
      ACCESS='STREAM',IOSTAT=ISTAT)

   IF(ISTAT/=0) THEN
    WRITE(FPAR%MESG,'(A,A)')' Write error opening file ',TRIM(FILE)
    CALL FLL_OUT('ALL',FPAR)
    FPAR%SUCCESS = .FALSE.
    RETURN
   END IF
!
!  Position in file with empty write statement
!
   IF(RANK == ROOT_RANK)THEN
!    INQUIRE(UNIT=IOUNIT,POS=1_LINT)
    PTMP => FLL_RPART_FILE_HEADER(IOUNIT, FPAR) 
   END IF
  
   PTMP1 => FLL_MPI_CP_ALL(PTMP,COMMUNICATOR,ROOT_RANK,FPAR)
   IF(RANK /= ROOT_RANK) PTMP => PTMP1
   
   DISPL => PTMP%L1
!
!  Read linked list
! 
   POS = DISPL(RANK+2)
   PNODE => READ_NODE(IOUNIT,'B',POS,FPAR)
   
   if(rank ==1)CALL FLL_CAT(PNODE,6,.false., FPAR)
!
!  MPI_Barrier does not need to be here, 
!  just for testing purposes 
!
   CLOSE(IOUNIT)
   IF(.NOT.ASSOCIATED(PNODE))THEN
     WRITE(FPAR%MESG,'(A,A)')' Read  - error reading file ',TRIM(FILE)
     CALL FLL_OUT('ALL',FPAR)
     FPAR%SUCCESS = .FALSE.
     RETURN
   END IF

  CALL FLL_RM(PTMP, FPAR)
    
    
   FPAR%SUCCESS = .TRUE.
   RETURN
  
  END FUNCTION FLL_MPI_READ


  FUNCTION FLL_RPART_FILE_HEADER(IOUNIT, FPAR) RESULT(PNEW)
!
! Description: contains subroutine reading file header
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
! IOUNIT       In         Number of unit
! FPAR         In/Out     structure containing function specific data
! PNEW       Returns array of displacements
!
! Arguments declaration
!
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IOUNIT
   TYPE(DNODE), POINTER :: PNEW
!
! Local declarations
!   
   CHARACTER(LEN=NAME_LENGTH) :: NAME
   CHARACTER(LEN=TYPE_LENGTH) :: TYPE
   INTEGER(LINT) :: NDIM,NSIZE,I

   READ(IOUNIT)NAME,TYPE,NDIM,NSIZE
   READ(IOUNIT)NAME,TYPE,NDIM,NSIZE
   PNEW => FLL_MK(NAME,TYPE,NDIM,NSIZE,FPAR)
   READ(IOUNIT)(PNEW%L1(I), I=1,NDIM)

   RETURN 

  END FUNCTION FLL_RPART_FILE_HEADER
END MODULE FLL_MPI_READ_M

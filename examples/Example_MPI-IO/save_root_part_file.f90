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
!     Description: Saves individual files
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
MODULE SAVE_ROOT_PART_FILE_M
CONTAINS

  SUBROUTINE SAVE_ROOT_PART_FILE(PNODE,FPAR)

  USE MPI
  USE FLL_MODS_M
  IMPLICIT NONE

  TYPE(DNODE), POINTER  :: PNODE,PMAIN,PTMP
!
!   Local declarations
!
   CHARACTER(LEN=NAME_LENGTH) :: FILENAME
   CHARACTER(LEN=6) :: STR
   LOGICAL :: OK
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: RANK, NPROC,IERR,WORLD_RANK,I

   REAL :: FINISH,START
   REAL :: CPUE,CPUS
   INTEGER :: VALS(8)
   
   CALL MPI_Comm_size ( MPI_COMM_WORLD, NPROC, IERR )
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, WORLD_RANK, IERR )

   FILENAME = 'Commsofile.bsol'
  
   PMAIN => FLL_MKDIR('partitioned_file',FPAR)

   DO I=1,NPROC
      PTMP => FLL_CP(PNODE, PMAIN, FPAR)
   END DO

   IF(WORLD_RANK == 0)THEN 
!     CALL CPU_TIME(START)
     CALL DATE_AND_TIME(VALUES=VALS)
     CPUS = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001

     OK =  FLL_WRITE(PMAIN,ADJUSTL(TRIM(FILENAME)),10,'B',FPAR)

!     CALL CPU_TIME(FINISH)
!     WRITE(*,*)' SAVING INDIVIDUAL FILES TIME IS ', FINISH-START
     CALL DATE_AND_TIME(VALUES=VALS)
     CPUE = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
     WRITE(*,*)'Time writing parallel to single file :',CPUE-CPUS
   END IF

   CALL FLL_RM(PMAIN, FPAR)

  END SUBROUTINE SAVE_ROOT_PART_FILE
END MODULE SAVE_ROOT_PART_FILE_M

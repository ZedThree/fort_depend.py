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
!     Description: test of MPI I-O operations
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
PROGRAM  EXAMPLE_MPI_IO

    USE MPI
    USE FLL_MODS_M
    USE FLL_MPI_MODS_M
    USE READ_INPUT_M
    USE CREATE_DATA_SET_M
    USE SAVE_INDIVID_FILES_M
    USE SAVE_ROOT_PART_FILE_M

    IMPLICIT NONE
!
!   SUBROUTINE MOVES NODE
!
   TYPE(DNODE), POINTER  :: FLL_MPI_STRUCT, PDATA_SET, PNEW,PTMP,PMPI
   TYPE(FUNC_DATA_SET) :: FPAR
   INTEGER :: IERR,WORLD_RANK,world_group_id,NPROC,ISTAT

  INTEGER(LINT) :: NFILES,BYTESN,NSIZE,I,J
  integer :: EVEN_COMM_ID,EVEN_P,EVEN_GROUP_ID,ODD_GROUP_ID,ODD_COMM_ID
  CHARACTER(LEN=FILE_NAME_LENGTH) :: NAME_OF_FILE
  LOGICAL :: OK

  INTEGER, ALLOCATABLE :: EVEN_RANK(:),ODD_RANK(:)
  INTEGER :: E_RANK,O_RANK,COMM
  REAL :: START, FINISH

  TYPE(DNODE), POINTER ::PIOSTR,PSUBPROC,PIO

  REAL :: CPUE,CPUS,CPUAVE,CPTIMERMS(100),BYTESIZE,BYTESPEED
  INTEGER :: VALS(8)

  INTEGER :: NCYC
  NCYC = 20
!
!  Initialize MPI
!
   CALL MPI_INIT(IERR)   
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, WORLD_RANK, IERR )
   CALL MPI_Comm_size ( MPI_COMM_WORLD, NPROC, ierr )

   FLL_MPI_STRUCT => NULL()
!
!   create sample data se
!
   NSIZE = 1000000   !+ 10000*SIN(2*3.145926*world_rank/nproc)
!   write(*,*)nsize

   BYTESIZE = NSIZE*8*NPROC*5

   CALL CREATE_DATA_SET(PDATA_SET,NSIZE, WORLD_RANK)
!   IF(WORLD_RANK == 0) OK = FLL_WRITE_FFA(PDATA_SET,'test.bcs',10,'B',FPAR)
!   PDATA_SET => FLL_READ_FFA('test.bcase',8,'B',FPAR)
   CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

   BYTESN = FLL_GETNBYTES(PDATA_SET,FPAR)
   IF(WORLD_RANK == 0) WRITE(*,*)' Partitions created data set size of ', WORLD_RANK,BYTESN
!
!  save to one file, all partitions at the same time
! 
       IF(WORLD_RANK == 0)THEN
         WRITE(*,*)'=============================================='
         write(*,*)' Saving to single file'
         WRITE(*,*)'=============================================='
       END IF
   CPUAVE = 0
   DO J=1,NCYC
     CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

     IF(WORLD_RANK == 0)THEN
       CALL DATE_AND_TIME(VALUES=VALS)
       CPUS = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
     END IF
   
     OK = FLL_MPI_WRITE(PDATA_SET,'PartitionedFile',10,0, world_rank, MPI_COMM_WORLD, 'A', FPAR)
   
     CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
     IF(WORLD_RANK == 0)THEN 
        CALL DATE_AND_TIME(VALUES=VALS)
        CPUE = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
!        WRITE(*,*)' Time writing paralell to single file  (N-1 model):',CPUE-CPUS
        CPTIMERMS(J) = CPUE-CPUS
        CPUAVE = CPUAVE + CPUE-CPUS
     END IF

     END DO

     IF(WORLD_RANK == 0)THEN
        CPUAVE = CPUAVE/FLOAT(NCYC)
        DO J=1,NCYC
          CPTIMERMS(J) = (CPTIMERMS(J)-CPUAVE)**2
        END DO
        write(*,*)' >>>>>>>>>>>>>  average time of saving (N-1 model) is ',CPUAVE, SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        BYTESPEED = BYTESIZE/CPUAVE/1048576
        write(*,*)' >>>>>>>>>>>>>  average speed of saving (N-1 model) is ',&
            BYTESPEED, BYTESPEED/CPUAVE*SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        write(*,*)
     END IF 




   NFILES = 1


   DO I=1,NPROC

       NFILES = NFILES*2
       IF(NFILES > NPROC-1)EXIT

       IF(WORLD_RANK == 0)THEN
         WRITE(*,*)'=============================================='
         write(*,*)' Saving to ',NFILES,' files'
         WRITE(*,*)'=============================================='
       END IF
!
! initiate MPI structure with all info
!
   	PMPI => FLL_MPI_PROC_STRUCT(FPAR)
!
!  define how to save files for N-M saving model
!
   	CALL  FLL_NMIO_STRUCT(PMPI,'ada','bmpi',NFILES, FPAR)
   	CALL  FLL_SNMIO_STRUCT(PMPI,'ceda','bmpi',NFILES, 'I', FPAR)
!
!  print the strucute on the screen and save into ASCII file
!
  	IF(WORLD_RANK == 0)THEN
!    	  CALL FLL_CAT(PMPI,6,.FALSE., FPAR)
!    	  IF(.NOT.FLL_WRITE(PMPI,"io.str", 9, 'A', FPAR))STOP'Error writing file'
  	END IF

        CPUAVE = 0
        DO J=1,NCYC
!
!  save to 2 separate files in S-N-M mode
!
   	IF(WORLD_RANK == 0)THEN
    	 CALL DATE_AND_TIME(VALUES=VALS)
    	 CPUS = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
   	END IF

   	OK = FLL_MPI_WRITE_SNM(PDATA_SET,PMPI,FPAR)

   	CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
   	IF(WORLD_RANK == 0)THEN 
      	 CALL DATE_AND_TIME(VALUES=VALS)
      	 CPUE = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
!      	 WRITE(*,*)"Time writing serial to files (S-N-M model):",CPUE-CPUS
         CPTIMERMS(J) = CPUE-CPUS
         CPUAVE = CPUAVE + CPUE-CPUS
   	END IF

   	CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

        END DO

     IF(WORLD_RANK == 0)THEN
        CPUAVE = CPUAVE/FLOAT(NCYC)
        DO J=1,NCYC
          CPTIMERMS(J) = (CPTIMERMS(J)-CPUAVE)**2
        END DO
        write(*,*)' >>>>>>>>>>>>>  average time of saving (S-N-M model) is ',CPUAVE, SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        BYTESPEED = BYTESIZE/CPUAVE/1048576
        write(*,*)' >>>>>>>>>>>>>  average speed of saving (S-N-M model) is ',&
            BYTESPEED, BYTESPEED/CPUAVE*SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        write(*,*)
     END IF       

!
!  save to 2 separate files all partitions at the same time
!
        CPUAVE = 0
        DO J=1,NCYC

   	IF(WORLD_RANK == 0)THEN
    	 CALL DATE_AND_TIME(VALUES=VALS)
    	 CPUS = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
   	 END IF

   	 OK = FLL_MPI_WRITE_NM(PDATA_SET,PMPI,FPAR)

   	 CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
   	  IF(WORLD_RANK == 0)THEN 
      	  CALL DATE_AND_TIME(VALUES=VALS)
          CPUE = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
!          WRITE(*,*)"Time writing paralell to partitions file (N-M model):",CPUE-CPUS
          CPTIMERMS(J) = CPUE-CPUS
          CPUAVE = CPUAVE + CPUE-CPUS
   	END IF

        END DO

     IF(WORLD_RANK == 0)THEN
        CPUAVE = CPUAVE/FLOAT(NCYC)
        DO J=1,NCYC
          CPTIMERMS(J) = (CPTIMERMS(J)-CPUAVE)**2
        END DO
        write(*,*)' >>>>>>>>>>>>>  average time of saving (N-M model) is ',CPUAVE, SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        BYTESPEED = BYTESIZE/CPUAVE/1048576
        write(*,*)' >>>>>>>>>>>>>  average speed of saving (N-M model) is ',&
            BYTESPEED, BYTESPEED/CPUAVE*SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        write(*,*)
     END IF

      CALL FLL_RM(PMPI,FPAR)

    END DO




       IF(WORLD_RANK == 0)THEN
         WRITE(*,*)'=============================================='
         write(*,*)' Saving to individual files'
         WRITE(*,*)'=============================================='
       END IF
!
!  write to individual files
!
   CPUAVE = 0

   DO J=1,NCYC

     CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
     IF(WORLD_RANK == 0)THEN
      CALL DATE_AND_TIME(VALUES=VALS)
      CPUS = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
     END IF

     CALL SAVE_INDIVID_FILES(PDATA_SET,WORLD_RANK,FPAR)

     CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)
     IF(WORLD_RANK == 0)THEN 
        CALL DATE_AND_TIME(VALUES=VALS)
        CPUE = REAL(VALS(5)*3600+VALS(6)*60+VALS(7))+REAL(VALS(8))*0.001
!        WRITE(*,*)' Time writing to individual files (N-N model):',CPUE-CPUS
        CPTIMERMS(J) = CPUE-CPUS
        CPUAVE = CPUAVE + CPUE-CPUS
     END IF
     CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)

     END DO

     IF(WORLD_RANK == 0)THEN
        CPUAVE = CPUAVE/FLOAT(NCYC)
        DO J=1,NCYC
          CPTIMERMS(J) = (CPTIMERMS(J)-CPUAVE)**2
        END DO
        write(*,*)' >>>>>>>>>>>>>  average time of saving (N-N model) is ',CPUAVE, SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        BYTESPEED = BYTESIZE/CPUAVE/1048576
        write(*,*)' >>>>>>>>>>>>>  average speed of saving (N-N model) is ',&
            BYTESPEED, BYTESPEED/CPUAVE*SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        write(*,*)
     END IF

!
!  save entire data set from too partiton: 1-1 model
!
       IF(WORLD_RANK == 0)THEN
         WRITE(*,*)'=============================================='
         write(*,*)' Saving to concentanated file'
         WRITE(*,*)'=============================================='
       END IF

     DO J=1,NCYC
       IF(WORLD_RANK == 0)CALL SAVE_ROOT_PART_FILE(PDATA_SET,J,CPTIMERMS,FPAR)
     END DO

     IF(WORLD_RANK == 0)THEN
        CPUAVE = SUM(CPTIMERMS(1:NCYC))/FLOAT(NCYC)
        DO J=1,NCYC
          CPTIMERMS(J) = (CPTIMERMS(J)-CPUAVE)**2
        END DO
        write(*,*)' >>>>>>>>>>>>>  average time of saving (1-1 model) is ',CPUAVE, SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        BYTESPEED = BYTESIZE/CPUAVE/1048576
        write(*,*)' >>>>>>>>>>>>>  average speed of saving (1-1 model) is ',&
            BYTESPEED, BYTESPEED/CPUAVE*SQRT(SUM(CPTIMERMS(1:NCYC)))/FLOAT(NCYC)
        write(*,*)
     END IF
!
!  Copy FLL_MPI_STRUCT date set which now exists on root partition only
!  to all other partitions
!  Upon return, the function will return pointer to newly allocated data
!  for all other partition then root partition
!  On root partition, the PNEW pointer is pointing on FLL_MPI_STRUCT
!
!   PNEW => FLL_MPI_CP_ALL(FLL_MPI_STRUCT,MPI_COMM_WORLD,0,FPAR)
!   IF(WORLD_RANK /= 0)THEN
!
!  make FLL_MPI_STRUCT point to PNEW so that we cane use the same names for all partitions
!  
!     FLL_MPI_STRUCT => PNEW
!   END IF
  
!   PTMP => FLL_MPI_READ('PartitionedFile',10,0, world_rank, MPI_COMM_WORLD, 'A', FPAR)
!   BYTESN = FLL_GETNBYTES(PTMP,FPAR)
!   WRITE(*,*)' Partition all-reads data set size of ', WORLD_RANK,BYTESN
!   CALL FLL_RM(PTMP,FPAR)
!
!  CLEAN MEMORY
!
   IF(WORLD_RANK==0)write(*,*)' Releasing memory'
   CALL FLL_RM(FLL_MPI_STRUCT,FPAR)
   CALL FLL_RM(PDATA_SET,FPAR)

  
   CALL MPI_FINALIZE(IERR)
  
END PROGRAM

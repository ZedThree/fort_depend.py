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
!     Description: calculates forces from bedg file and bout file
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
 PROGRAM  FLL_FORCES

    USE FLL_MODS_M
    IMPLICIT NONE
!
! Description: conversion utility
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/08/17                         Initial implementation
!
!
! External Modules used
!
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE,OUTFILE,BEDGFILE
   TYPE(DNODE), POINTER  :: PBEDGE,PBOUT,PNEW,PGRID,PREG,PFX,PFY,PFZ,&
     PTIME,PFREE,PTMP
   TYPE(FUNC_DATA_SET) :: FPAR
   REAL(RDOUBLE), POINTER :: NORMS(:,:),PRESS(:)
   INTEGER(LINT) :: I,NNODES
   LOGICAL :: OK
!
!  read a file and save it
!
    WRITE(*,*)' Name of bedg file '
    READ(*,'(A1024)')BEDGFILE
    WRITE(*,*)' Name of bout file '   
    READ(*,'(A1024)')FILE
    WRITE(*,*)' Name of output file '   
    READ(*,'(A1024)')OUTFILE    
!
!  read bedg file
!
     WRITE(*,*)' Reading bedg file'
     PBEDGE => FLL_READ_FFA(BEDGFILE,8,'B',FPAR)
!
!  read bout file
!
     WRITE(*,*)' Reading bout file'
     PBOUT => FLL_READ_FFA(FILE,8,'B',FPAR)
!
!
!  find normals in bedge file
!
    PREG   => FLL_LOCATE(PBEDGE,'region','*',-1_LINT,1_LINT,.FALSE. ,FPAR)
    PGRID  => FLL_LOCATE(PREG,'grid','*',-1_LINT,1_LINT,.FALSE. ,FPAR)
    NORMS => FLL_GETNDATA_D2(PGRID, 'edge_surfaces', 1_LINT, FPAR)
!
!  find pressures in bout file
!
    PREG   => FLL_LOCATE(PBOUT,'region','*',-1_LINT,1_LINT,.FALSE. ,FPAR)
    PFREE   => FLL_LOCATE(PBOUT,'free_stream_data','*',-1_LINT,1_LINT,.FALSE. ,FPAR)
!
!  look for time, if not found, steady solution
!
    PTIME   => NULL()
    PTIME   => FLL_LOCATE(PREG,'time','*',-1_LINT,1_LINT,.FALSE. ,FPAR)
    IF(.NOT.ASSOCIATED(PTIME)) PTIME => PREG
   
    PRESS => FLL_GETNDATA_D1(PTIME, 'pressure', 1_LINT, FPAR)
!
!  make new solution pointer
!
    PNEW => FLL_MKDIR('solution', FPAR)
    PTMP  => FLL_CP(PFREE,PNEW, FPAR)
!
!  make region and add it
!
    PREG => FLL_MKDIR('region', FPAR)
    OK = FLL_MV(PREG, PNEW, FPAR)
!
!  make three arrays for forces and add it to region
!
    NNODES = SIZE(PRESS, DIM =1, KIND = LINT)
    PFX    => FLL_MK('FX','D', NNODES, 1_LINT, FPAR)
    OK = FLL_MV(PFX, PREG, FPAR)
    PFY    => FLL_MK('FY','D', NNODES, 1_LINT, FPAR)
    OK = FLL_MV(PFY, PREG, FPAR)
    PFZ    => FLL_MK('FZ','D', NNODES, 1_LINT, FPAR)
    OK = FLL_MV(PFZ, PREG, FPAR)
!
!  calculate forces
!
    DO I=1,NNODES
       PFX%D1(I) = PRESS(I) * NORMS(I,1)
       PFY%D1(I) = PRESS(I) * NORMS(I,2)
       PFZ%D1(I) = PRESS(I) * NORMS(I,3)
    END DO
!
!  write solution
!
    OK = FLL_WRITE_FFA(PNEW, OUTFILE,9,'B',FPAR)
!   
!  cleanup memory
!
   CALL FLL_RM(PBEDGE,FPAR)
   CALL FLL_RM(PBOUT,FPAR)
   CALL FLL_RM(PNEW,FPAR)

  
END PROGRAM 

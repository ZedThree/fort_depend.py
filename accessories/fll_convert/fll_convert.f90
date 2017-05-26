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
!     Description: prints file on screen
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
 PROGRAM  FLL_CONVERT

    USE FLL_MODS_M
    IMPLICIT NONE
!
! Description: conversion utility
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
   CHARACTER(LEN=FILE_NAME_LENGTH) FILE,OUTFILE
   TYPE(DNODE), POINTER  :: PNODE
   TYPE(FUNC_DATA_SET) :: FPAR
   CHARACTER :: FMT, SCAN
   CHARACTER(LEN=3) :: EFMT,OFMT
   LOGICAL :: OK
!
!  read a file and save it
!
    READ(*,'(A1024)')FILE
    READ(*,*)FMT 
    READ(*,'(A3)')EFMT 
    READ(*,'(A1024)')OUTFILE
    READ(*,'(A3)')OFMT 

    SELECT CASE(EFMT)
     CASE('fll')
      PNODE => FLL_READ(FILE,8,FMT,FPAR)
     CASE('ffa')
      PNODE => FLL_READ_FFA(FILE,8,FMT,FPAR)
    END SELECT

    SELECT CASE(OFMT)
     CASE('fll')
      OK = FLL_WRITE(PNODE,OUTFILE,9,FMT,FPAR)
     CASE('ffa')
      OK = FLL_WRITE_FFA(PNODE, OUTFILE,9,FMT,FPAR)
    END SELECT    
    
    
   CALL FLL_RM(PNODE,FPAR)

  
END PROGRAM 

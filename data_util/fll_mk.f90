!
!     Copyright (C) 2016  Adam Jirasek
! 
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU Lesser General Public License as published by
!     the Rree Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
! 
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or RITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU Lesser General Public License for more details.
! 
!     You should have received a copy of the GNU Lesser General Public License
!     along with this program.  If not, see <http://www.gnu.org/licenses/>.
!     
!     contact: libm3l@gmail.com
! 
!

!
!     Subroutine FLL_MK
!
!     Date: 2016-10-19
! 
! 
!
!
!     Description: make nodes
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
MODULE FLL_MK_M
CONTAINS

   FUNCTION FLL_MK(NAME,LTYPE,NDIM,NSIZE,FPAR) RESULT(PNEW)
   
       USE FLL_TYPE_M
       
       IMPLICIT NONE
       TYPE(FUNC_DATA_SET)   :: FPAR
       TYPE(DNODE), POINTER  :: PNEW
       CHARACTER(*)  :: NAME
       CHARACTER(*)  :: LTYPE
       INTEGER(LINT) :: NDIM, NSIZE
       
       INTEGER :: ISTAT
       
       PNEW => NULL()
!
! Body
!
       IF(LEN_TRIM(LTYPE)<1.OR.LEN_TRIM(LTYPE)>TYPE_LENGTH) THEN
         WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
         RETURN
      END IF

      IF(LEN_TRIM(NAME)>NAME_LENGTH) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong name: ',TRIM(NAME)
        RETURN
      END IF

      IF(.NOT.ANY(LTYPE(1:1)==(/'C','S','I','L','R','D','N'/))) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
        RETURN
      END IF

      ALLOCATE(PNEW, STAT = ISTAT)
      IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:82 '
      PNEW%LNAME  = TRIM(NAME)
      PNEW%LTYPE = LTYPE
      PNEW%NDIM = 0
      PNEW%NSIZE = 0

      IF(TRIM(LTYPE) == 'DIR' .OR. TRIM(LTYPE) == 'N')THEN
        PNEW%NDIM = 0
        PNEW%NSIZE = 0
        RETURN
      ELSE
        PNEW%NDIM = NDIM
        PNEW%NSIZE = NSIZE
      END IF

      PNEW%PPAR => NULL()
      PNEW%PCHILD => NULL()
      PNEW%PPREV => NULL()
      PNEW%PNEXT => NULL()
      PNEW%PLINK => NULL()

      IF(NDIM < 1 .OR. NSIZE < 1)THEN
        WRITE(FPAR%MESG,'(A,A,I5,I5)')' Wrong dimensions for node ',TRIM(NAME), NDIM, NSIZE
!        WRITE(*,*)' MK WRONG DIMENSIONS ',TRIM(NAME), NDIM, NSIZE
!        STOP
         RETURN
      END IF
!
!  ALLOCATE ARRAYS
!
     SELECT CASE(LTYPE)
     CASE('R')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%R1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:106 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%R1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:111 '
         ELSE
            ALLOCATE(PNEW%R2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:114 '
         END IF
       END IF
       
     CASE('D')
            IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%D1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:122 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%D1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:127 '
         ELSE
            ALLOCATE(PNEW%D2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:130 '
         END IF
       END IF
       
       
     CASE('I')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%I1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:139 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%I1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:144 '
         ELSE
            ALLOCATE(PNEW%I2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:147 '
         END IF
       END IF
       
       
     CASE('L')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%L1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:156 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%L1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:161 '
         ELSE
            ALLOCATE(PNEW%L2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:164 '
         END IF
       END IF     
       
       
     CASE('S')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%S1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:156 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%S1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:161 '
         ELSE
            ALLOCATE(PNEW%S2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> ll_mk ERR:164 '
         END IF
       END IF
     
     CASE('C')

     CASE('N','DIR')
        RETURN
     
     
     END SELECT

    RETURN
   END FUNCTION FLL_MK
   
   
   
   
END MODULE FLL_MK_M

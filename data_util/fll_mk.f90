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
MODULE FLL_MK_M
!
! Description: creates node
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       10/10/16                         Initial implementation
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
   FUNCTION FLL_MK(NAME,LTYPE,NDIM,NSIZE,FPAR,ACTION) RESULT(PNEW)
!
! Description: function creates node specified by name, type and dimensions
!
! External Modules used
! 
       USE FLL_TYPE_M
       USE FLL_OUT_M
       
       IMPLICIT NONE
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! NAME         In         name of node
! LTYPE        In         type of node  - can be *
! NDIM, NSIZE  In         node dimensions
! PNEW         Out        return pointer to newly created node
! FPAR         In/Out     structure containing function specific data
!
! Arguments declaration
!
       TYPE(FUNC_DATA_SET)   :: FPAR
       TYPE(DNODE), POINTER  :: PNEW
       CHARACTER(*)  :: NAME
       CHARACTER(*)  :: LTYPE
       INTEGER(LINT) :: NDIM, NSIZE
       CHARACTER(*), OPTIONAL :: ACTION
!
! Local declarations
!       
       INTEGER :: ISTAT
       CHARACTER(LEN=10) :: LOC_ACT
!   
!  local action
!
       IF(.NOT.PRESENT(ACTION))THEN
         LOC_ACT='ALL'
       ELSE
         LOC_ACT = ACTION
       END IF
       
       PNEW => NULL()
!
! Body
!
       IF(LEN_TRIM(LTYPE)<1.OR.LEN_TRIM(LTYPE)>TYPE_LENGTH) THEN
         WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
         CALL FLL_OUT(LOC_ACT,FPAR)
         RETURN
      END IF

      IF(LEN_TRIM(NAME)>NAME_LENGTH) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong name: ',TRIM(NAME)
        CALL FLL_OUT(LOC_ACT,FPAR)
        RETURN
      END IF

      IF(.NOT.ANY(LTYPE(1:1)==(/'C','S','I','L','R','D','N'/))) THEN
        WRITE(FPAR%MESG,'(A,A)')' Wrong type: ',TRIM(LTYPE)
        CALL FLL_OUT(LOC_ACT,FPAR)
        RETURN
      END IF

      ALLOCATE(PNEW, STAT = ISTAT)
      IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:95 '
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
        CALL FLL_OUT(LOC_ACT,FPAR)
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
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:129 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%R1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:134 '
         ELSE
            ALLOCATE(PNEW%R2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:137 '
         END IF
       END IF
       
     CASE('D')
            IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%D1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:145 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%D1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:150 '
         ELSE
            ALLOCATE(PNEW%D2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:153 '
         END IF
       END IF
       
       
     CASE('I')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%I1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:162 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%I1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:167 '
         ELSE
            ALLOCATE(PNEW%I2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:170 '
         END IF
       END IF
       
       
     CASE('L')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%L1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:179 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%L1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:184 '
         ELSE
            ALLOCATE(PNEW%L2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:187 '
         END IF
       END IF     
       
       
     CASE('S')
       IF(NDIM == 1)THEN
         IF(NSIZE > 1)THEN
           ALLOCATE(PNEW%S1(NSIZE), STAT=ISTAT)
           IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:196 '
         END IF
       ELSE
         IF(NSIZE == 1)THEN
            ALLOCATE(PNEW%S1(NDIM), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:201 '
         ELSE
            ALLOCATE(PNEW%S2(NDIM,NSIZE), STAT=ISTAT)
            IF(ISTAT /= 0)STOP' ERROR ALLOCATING MEMORY ==> fll_mk ERR:204 '
         END IF
       END IF
     
     CASE('C')

     CASE('N','DIR')
        RETURN
     
     
     END SELECT

    RETURN
   END FUNCTION FLL_MK
   
END MODULE FLL_MK_M

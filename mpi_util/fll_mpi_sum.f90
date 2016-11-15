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
!     Subroutine FLL_MPI_SUM
!
!     Date: 2016-10-10
! 
! 
!
MODULE FLL_MPI_SUM_M
!
! Description: Counts byte length of the list
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
!
!
CONTAINS
  SUBROUTINE FLL_MPI_SUM(COMMUNICATOR, NDIM ,R,D,I,L,R1,D1,I1,L1)
!
! Description: performs sum operation in MPI_ALLREDUCE
!
! 
! History:
! Version   Date       Patch number  CLA     Comment
! -------   --------   --------      ---     -------
! 1.1       11/11/16                         Initial implementation
!
!
! External Modules used
!
    USE MPI
    USE FLL_MODS_M
!
! Declarations
!
! Arguments description
! Name         In/Out     Function
! CIMMUNICATOR In         Communicator
! NDIM, NSIZE  In         dimensions of arrays, if 1 variable is scalar
! R,R#         In         real scalar or array
! D,D#         In         double scalar or array
! I,I#         In         integer scalar or array
! L,L#         In         long integer scalar or array
!
! Arguments declaration
!
    INTEGER(LINT) :: NDIM
    INTEGER :: COMMUNICATOR
    REAL(RSINGLE), OPTIONAL :: R,R1(:)
    REAL(RDOUBLE), OPTIONAL :: D,D1(:)

    INTEGER(SINT), OPTIONAL :: I,I1(:)
    INTEGER(LINT), OPTIONAL :: L,L1(:)
!
! Local declaration
!
    REAL(RSINGLE), ALLOCATABLE :: TR1(:)
    REAL(RDOUBLE), ALLOCATABLE :: TD1(:)

    INTEGER(SINT), ALLOCATABLE :: TI1(:)
    INTEGER(LINT), ALLOCATABLE :: TL1(:)

    REAL(RSINGLE) :: TR
    REAL(RDOUBLE) :: TD

    INTEGER(SINT) :: TI
    INTEGER(LINT) :: TL

    INTEGER :: ISTAT,IERR
!
! Body of subroutine
!
!
!  vector/matrix
!
    VECSCAL: IF(NDIM > 1)THEN

      IF(PRESENT(R1))THEN
       ALLOCATE(TR1(NDIM), STAT = ISTAT)
       IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY ==> fll_mpi_sum ERR:106 '
       TR1 = R1
       CALL MPI_ALLREDUCE(TR1,R1,NDIM,MPI_REAL,MPI_SUM,COMMUNICATOR,IERR)
       DEALLOCATE(TR1, STAT = ISTAT)
       IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> fll_mpi_sum ERR:110 '
       RETURN 
      END IF

      IF(PRESENT(D1))THEN
       ALLOCATE(TD1(NDIM), STAT = ISTAT)
       IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY ==> fll_mpi_sum ERR:116 '
       TD1 = D1
       CALL MPI_ALLREDUCE(TD1,D1,NDIM,MPI_DOUBLE,MPI_SUM,COMMUNICATOR,IERR)
       DEALLOCATE(TD1, STAT = ISTAT)
       IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> fll_mpi_sum ERR:120 '
       RETURN 
      END IF

      IF(PRESENT(I1))THEN
       ALLOCATE(TI1(NDIM), STAT = ISTAT)
       IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY ==> fll_mpi_sum ERR:126 '
       TI1 = I1
       CALL MPI_ALLREDUCE(TI1,I1,NDIM,MPI_INTEGER,MPI_SUM,COMMUNICATOR,IERR)
       DEALLOCATE(TI1, STAT = ISTAT)
       IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> fll_mpi_sum ERR:130 '
       RETURN 
      END IF

      IF(PRESENT(L1))THEN
       ALLOCATE(TL1(NDIM), STAT = ISTAT)
       IF(ISTAT /= 0)STOP'ERROR ALLOCATING MEMORY ==> fll_mpi_sum ERR:136 '
       TL1 = L1
       CALL MPI_ALLREDUCE(TL1,L1,NDIM,MPI_INTEGER8,MPI_SUM,COMMUNICATOR,IERR)
       DEALLOCATE(TL1, STAT = ISTAT)
       IF(ISTAT /= 0)STOP'ERROR DEALLOCATING MEMORY ==> fll_mpi_sum ERR:140 '
       RETURN 
      END IF
!
!  Scalars
!
    ELSE

      IF(PRESENT(R))THEN
       TR = R
       CALL MPI_ALLREDUCE(TR,R,1,MPI_REAL,MPI_SUM,COMMUNICATOR,IERR)
       RETURN 
      END IF

      IF(PRESENT(D))THEN
       TD = D
       CALL MPI_ALLREDUCE(TD,D,1,MPI_DOUBLE,MPI_SUM,COMMUNICATOR,IERR)
       RETURN 
      END IF

      IF(PRESENT(I))THEN
       TI = I
       CALL MPI_ALLREDUCE(TI,I,1,MPI_INTEGER,MPI_SUM,COMMUNICATOR,IERR)
       RETURN 
      END IF

      IF(PRESENT(L))THEN
       TL = L
       CALL MPI_ALLREDUCE(TL,L,1,MPI_INTEGER8,MPI_SUM,COMMUNICATOR,IERR)
       RETURN 
      END IF


    END IF VECSCAL

!
    RETURN

  END SUBROUTINE FLL_MPI_SUM

END MODULE FLL_MPI_SUM_M

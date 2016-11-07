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
!     Description: creates mpi struct defining how many files will be 
!                  saved and which partitions will be saving to each 
!                  file
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
MODULE CREATE_DATA_SET_M
CONTAINS

  SUBROUTINE  CREATE_DATA_SET(PNODE,NNODES,RANK)

  USE FLL_MODS_M
  IMPLICIT NONE

  TYPE(DNODE), POINTER  :: PNODE
!
!   Local declarations
!
  TYPE(DNODE), POINTER :: PTMP
  TYPE(FUNC_DATA_SET)  :: FPAR

  INTEGER(LINT)    :: NNODES
  INTEGER          :: RANK

  REAL(RDOUBLE), POINTER :: PRESS(:), DENS(:), VELOC(:,:)
!
!  MAKE STRUCTURE
!
  PNODE => FLL_MKDIR('Main_Data_Set',FPAR)
!
!  create 1-D double array of pressure fill it up with 101323 value and attach to PNODE
!
  PTMP  => FLL_MK('pressure','D', NNODES, 1_LINT, FPAR)
  IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'
!
! THE DATA CAN BE ACCESSE DIRECTLY THROUGH PTMP%D(:)
!
  PRESS => FLL_GETNDATA_D1(PNODE,'pressure',1_LINT,FPAR)
  PRESS = 101232.
!
!  create 1-D double array of desnity fill it up with 1.225 value and attach to PNODE
!
  PTMP  => FLL_MK('density','D',NNODES, 1_LINT, FPAR)
  IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'
!
! THE DATA CAN BE ACCESSE DIRECTLY THROUGH PTMP%D(:)
!
  DENS => FLL_GETNDATA_D1(PNODE,'density',1_LINT,FPAR)
  DENS = 1.225
!
!  create 2-D double array of velocity fill it up with 100, 0, 0 values and attach to PNODE
!
  PTMP  => FLL_MK('velocity', 'D',NNODES, 3_LINT, FPAR)
  IF(.NOT.FLL_MV(PTMP, PNODE, FPAR))STOP' ERROR MOVING NODE'
!
! THE DATA CAN BE ACCESSE DIRECTLY THROUGH PTMP%D(:)
!
  VELOC => FLL_GETNDATA_D2(PNODE,'velocity',1_LINT,FPAR)
  VELOC(:,1) = 100.
  VELOC(:,2) = 0.
  VELOC(:,3) = 0.
!
! print data set on screen, only mpi root
!
  IF(RANK == 0)CALL FLL_CAT(PNODE,6,.false., FPAR)

  RETURN
  END SUBROUTINE  CREATE_DATA_SET  
END MODULE CREATE_DATA_SET_M

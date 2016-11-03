!
! Header for svn
!
MODULE Convection
!
! Description:
!
! 
! History:
! Version   Date       Comment
! -------   --------   -------
! 1.1       24/03/16  Update of ...
!
!

! External Modules used
  USE FFAType
  USE FFAData

! Declarations
  IMPLICIT NONE
  PRIVATE
  PUBLIC ConvectionMean

CONTAINS

  SUBROUTINE ConvectionMean(pglob,preg,nnodes)

! Description: Computes convective mean flow
!              Fluxes computed and added to residuals
!

! Declarations
    IMPLICIT NONE

! Arguments description
! Name         In/Out     Function
! pglob        In/Out     Pointer to all data
! preg         In/Out     Pointer to region data
! nnodes       In         Number of nodes

! Arguments declaration
    TYPE(DATA_SET),     POINTER      :: pglob
    TYPE(DATA_SET),     POINTER      :: pdom
    INTEGER,            INTENT(IN)   :: nnodes

! Local variables declaration
    INTEGER ndim,ncol,iupwin,irot,nsc,nedges
    REAL :: gom1

!
! Global data
!
    iupwin = Get_Child_I0(pglob,'IUPWIN')

!...

  END SUBROUTINE ConvectionMean
  
  SUBROUTINE ConvectionTurbulence(pglob,preg,nnodes)
!...
  
END MODULE Convection

END MODULE GCONEU_M

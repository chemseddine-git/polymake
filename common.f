      MODULE COMMON
      IMPLICIT NONE
      CHARACTER (LEN=2),ALLOCATABLE :: LABELMONO(:),LABELPOLY(:)
      CHARACTER,DIMENSION(3) :: LABELH2O*2
      INTEGER                :: NMONOATOMS,NMMUNITS,NPOLYATOMS,NH2O,
     $                          I,J,K,L,NX,NY,NZ,RANK,
     $                          POLYATOM,MONOATOM
      INTEGER :: NSAFEH2O = 0
      INTEGER, ALLOCATABLE   :: MMTYPEMONO(:),MMTYPEPOLY(:)
      INTEGER,DIMENSION(3)   :: MMTYPEH2O,COORDCOM
      REAL,DIMENSION(:),ALLOCATABLE :: SAFEH2O
      REAL,DIMENSION(:,:,:),ALLOCATABLE :: COORDH2OBOX
      REAL,DIMENSION(1,3)    :: TRANS
      REAL                   :: XBOX,A,B,C,TOTMASS,MASS
      REAL, ALLOCATABLE      :: COORDMONO(:,:),COORDPOLY(:,:)
      REAL,DIMENSION(3)      :: D
      REAL,DIMENSION(3,3)    :: COORDH2O
      REAL,PARAMETER :: MMH2O = 2*1.00794 + 15.9994   !g/mol
      REAL,PARAMETER :: RHOH2O = 1000000              !g/m3
      REAL,PARAMETER :: NA = 6.02214129E23            !mol-1
      REAL :: VDWR = 1.63                             !Angstorm!!!!!!!!!!!!!!!!!si je ne mets pas de valeur erreur bizarre
      REAL,PARAMETER :: OVDWR = 1.52                  !Angstorm
      REAL,PARAMETER :: HVDWR = 1.2                   !Angstorm
 
      END MODULE COMMON

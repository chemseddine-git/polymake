      SUBROUTINE READM
C
C
C     Purpose: Read the monomer xyz file
C
C
C     ******************************************************************
C
C
C
C     ******************************************************************
C
C     List of local variables:
C
C
C     ------------------------------------------------------------------
C
      USE COMMON
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
      READ(11,*)NMONOATOMS
      READ(11,*)
C
      ALLOCATE (LABELMONO(NMONOATOMS))
      ALLOCATE (COORDMONO(NMONOATOMS,3))
      ALLOCATE (MMTYPEMONO(NMONOATOMS))
C     
      DO MONOATOM=1,NMONOATOMS
        READ(11,*) LABELMONO(MONOATOM),
     $             COORDMONO(MONOATOM,1),
     $             COORDMONO(MONOATOM,2),
     $             COORDMONO(MONOATOM,3),
     $             MMTYPEMONO(MONOATOM)
      END DO

C
C
C     *** End of READM subroutine ***
C
      END

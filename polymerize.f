      SUBROUTINE POLYMERIZE
C
C
C     Purpose: Polymerize the ose unit
C
C
C     ******************************************************************
C
C
C
C     ******************************************************************
C
      USE COMMON
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
C     *** Number of units ? ***
C
      WRITE(*,*)"Quel est le nombre d unités MM ?"
      READ(5,*) NMMUNITS
C
      NPOLYATOMS = NMMUNITS*(NMONOATOMS-3)+3
C
      ALLOCATE (COORDPOLY(NPOLYATOMS,3))
      ALLOCATE (LABELPOLY(NPOLYATOMS))
      ALLOCATE (MMTYPEPOLY(NPOLYATOMS))
C
      COORDPOLY(:,:)=0.0
      LABELPOLY(:)='XX'
      MMTYPEPOLY(:)=0.0
C
C     *** Calculate the tanslation vector ***
C
      DO I=1,3
        TRANS(1,I) = COORDMONO(42,I)-COORDMONO(2,I)                
      END DO      
C
      open (34,access='sequential')
C
C     *** Write the first H ***
C
      COORDPOLY(1,:)=COORDMONO(1,:)
      MMTYPEPOLY(1)=MMTYPEMONO(1)
      LABELPOLY(1)=LABELMONO(1)
C
C     *** Write all the monomers ***
C
      POLYATOM=2
      DO I=1,NMMUNITS
        DO MONOATOM=2,NMONOATOMS-2
            COORDPOLY(POLYATOM,:)=COORDMONO(MONOATOM,:)
     $                           +TRANS(1,:)*(I-1)
            LABELPOLY(POLYATOM)=LABELMONO(MONOATOM)
            MMTYPEPOLY(POLYATOM)=MMTYPEMONO(MONOATOM)
            POLYATOM=POLYATOM+1
        END DO
      END DO
C
C     *** Correct the glycosidic bridge atoms parameters ***
C
      I=1
      DO WHILE (I.LT.NMMUNITS)
        MMTYPEPOLY (2+I*42)      = MMTYPEMONO(13) 
        MMTYPEPOLY (17+(I-1)*42) = MMTYPEMONO(8)
        MMTYPEPOLY (39+(I-1)*42) = MMTYPEMONO(36)
        MMTYPEPOLY (5+I*42)      = MMTYPEMONO(14)
        MMTYPEPOLY (34+I*42)     = MMTYPEMONO(31)
      I = I+1
      END DO
C
C     *** Write the last OH ***
C
      COORDPOLY(NPOLYATOMS-1,:)=COORDMONO(NMONOATOMS-1,:)
     $                         +(TRANS(1,:)*(NMMUNITS-1))
      COORDPOLY(NPOLYATOMS,:)=COORDMONO(NMONOATOMS,:)
     $                         +(TRANS(1,:)*(NMMUNITS-1))
      LABELPOLY(NPOLYATOMS-1)=LABELMONO(NMONOATOMS-1)
      LABELPOLY(NPOLYATOMS)=LABELMONO(NMONOATOMS)   
      MMTYPEPOLY(NPOLYATOMS-1)=MMTYPEMONO(NMONOATOMS-1)
      MMTYPEPOLY(NPOLYATOMS)=MMTYPEMONO(NMONOATOMS) 
C
C     *** End of SUBROUTINE polymerize ***
C
      END

        SUBROUTINE SOLVATE
C
        USE COMMON
        IMPLICIT NONE
C
C	*** INITIALISATION DES VARIABLES ***
C
        PRINT*, '[water box] Quelles sont les dimensions de la boite,'
        PRINT*, 'Selon l axe X ?'               
        READ*, A
        PRINT*, 'Selon l axe Y ?'               
        READ*, B
        PRINT*, 'Selon l axe Z ?'               
        READ*, C
C
        COORDH2OBOX = 0.0
C
        LABELH2O(1) = 'O '
        LABELH2O(2) = 'H '
        LABELH2O(3) = 'H '
C
        MMTYPEH2O(1) = -63
        MMTYPEH2O(2) = -64
        MMTYPEH2O(3) = -64
C
        COORDH2O (1,:) = (/0.0, 0.0, 0.0/)
        COORDH2O (2,:) = (/0.757, 0.586, 0.000/)
        COORDH2O (3,:) = (/-0.751, 0.586, 0.000/)
C
C       *** Paramètre de la maille d eau elementaire ***
C       ***  distribution uniforme (liquide) --> maille cubique ***
C
        XBOX = ((MMH2O/RHOH2O/NA)*1E30)**(1./3.)
C
C       *** Nombre de repetition de la maille elementaire selon ***
C       *** les trois axes                                      ***
C
        NX = (A/XBOX)
        NY = (B/XBOX)
        NZ = (C/XBOX)
C
        NH2O = NX*NY*NZ
C
        ALLOCATE(COORDH2OBOX(NH2O,3,3))
C
C	*** GENERATION D UNE BOITE D EAU ***
C
        DO I=1,NX
          DO J=1,NY
             DO K=1,NZ
               RANK = RANK + 1
               DO L=1,3
                 COORDH2OBOX(RANK,L,1) = COORDH2O(L,1) + (I-1)*XBOX
                 COORDH2OBOX(RANK,L,2) = COORDH2O(L,2) + (J-1)*XBOX
                 COORDH2OBOX(RANK,L,3) = COORDH2O(L,3) + (K-1)*XBOX
               END DO
             END DO
          END DO
        END DO
C
C       *** DETERMINATION DU CENTRE DE MASSE DE LA MOLECULE ***
C
        TOTMASS = 0.0
        MASS = 0.0
        COORDCOM =0.0
C
C       *** DEPLACEMENT DE LA MOLECULE AU CENTRE DE LA BOITE D'EAU ***
C
        DO I=1,NPOLYATOMS
          MASS = F(LABELPOLY(I))
          COORDCOM = COORDCOM + MASS*COORDPOLY(I,:)
          TOTMASS = TOTMASS + MASS
        END DO
C
        COORDCOM = COORDCOM / TOTMASS
C
        DO I=1,NPOLYATOMS
        COORDPOLY (I,:) = COORDPOLY (I,:)
     $                      - COORDCOM (:)
     $                      + (/XBOX*(NX-1)/2,
     $                          XBOX*(NY-1)/2,
     $                          XBOX*(NZ-1)/2/)
        END DO
C
C	*** DETECTION H2O TROP PROCHES***	
C
        ALLOCATE (SAFEH2O(NH2O))
        SAFEH2O(:) = 0

        DO I=1,NH2O
          J = 1
          L = 1
          DO WHILE ((J.LE.NPOLYATOMS).AND.(L.EQ.1))
            DO K=1,3
              D(K) = DIST(COORDH2OBOX(I,K,:),COORDPOLY(J,:))
            END DO
            VDWR = VDWRAD(LABELPOLY(J))
            IF(D(1).LT.(VDWR+OVDWR)
     $      .OR.D(2).LT.(VDWR+HVDWR)
     $      .OR.D(3).LT.(VDWR+HVDWR))THEN
            L=0
            END IF
            J = J + 1
          END DO
          IF (J.EQ.(NPOLYATOMS+1)) THEN          
             NSAFEH2O = NSAFEH2O + 1
             SAFEH2O(I) = 1
          END IF
         END DO
C
        CONTAINS
C
        REAL FUNCTION DIST(X,Y)
          IMPLICIT NONE
          INTEGER :: N
          REAL, DIMENSION(3) :: X
          REAL, DIMENSION(3) :: Y
          DIST = 0.0
          DO N=1,3
            DIST = DIST + (X(N)-Y(N))**2
          END DO
          DIST = SQRT(DIST)
        END FUNCTION DIST
C
         REAL FUNCTION F(ATOM)
           IMPLICIT none
           CHARACTER (LEN=2), Intent(In) :: ATOM
           IF (ATOM=='C') THEN
              F = 12.011000
           ELSEIF (ATOM=='H') THEN
              F = 1.007940 
           ELSEIF (ATOM=='O') THEN
              F = 15.9994
           ELSEIF (ATOM=='Na') THEN
              F = 22.989768
           ELSE
              F = 1
           END IF
         END FUNCTION F
C
         REAL FUNCTION VDWRAD(ATOM)
           IMPLICIT none
           CHARACTER (LEN=2), Intent(In) :: ATOM
           IF (ATOM=='Pd') THEN
              VDWRAD = 1.602
           ELSEIF (ATOM=='C') THEN
              VDWRAD = 0.77
           ELSEIF (ATOM=='H') THEN
              VDWRAD = 0.32
           ELSEIF (ATOM=='Na') THEN
              VDWRAD = 1.54
           ELSEIF (ATOM=='Co') THEN
              VDWRAD = 1.16
           ELSEIF (ATOM=='O') THEN
              VDWRAD = 0.73
           ELSE
              VDWRAD = 1
!!!!!!!!!!!WRITE(44,*) MOLECULE(J), ' n existe pas'	!output bizarre
           END IF
         END FUNCTION VDWRAD
C
        END SUBROUTINE









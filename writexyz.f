      SUBROUTINE WRITEXYZ
C
C
C     Purpose: Program for making polyM in water
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
      WRITE(91,"(I6)") NPOLYATOMS+NSAFEH2O*3
      WRITE(91,*)
C
      DO I=1,NPOLYATOMS
          WRITE(91,*) LABELPOLY(I),'  ',
     $                COORDPOLY(I,1),'  ',
     $                COORDPOLY(I,2),'  ',
     $                COORDPOLY(I,3),'  ',
     $                MMTYPEPOLY(I)
      END DO
C
      DO I=1,NH2O
        IF (SAFEH2O(I) == 1) THEN
          DO J=1,3
            WRITE(91,1000) LABELH2O(J),
     $                  COORDH2OBOX(I,J,1),
     $                  COORDH2OBOX(I,J,2),
     $                  COORDH2OBOX(I,J,3),
     $                  MMTYPEH2O(J)
          END DO
        END IF
      END DO
C
 1000 FORMAT (A2,2X,3(F10.6),2X,I4) 
C
C
      END

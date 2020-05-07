      SUBROUTINE IO
C
C
C     Purpose: Opening of all the I/O files
C
C
C     ******************************************************************
C
C
C
C     ******************************************************************
C
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
C
C     *** Ouverture I/O ***
C
      OPEN(11,file="input.xyz",form="formatted",access="sequential")
      OPEN(91,file="molecule.xyz",form="formatted",access="sequential")
C
C     *** End of SUBROUTINE IO ***
C
      END

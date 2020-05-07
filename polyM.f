      PROGRAM polyM
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
C       INCLUDE 'common.h'
C
C     *** Open I/O Files ***
C
      CALL IO
C
C     *** Read the M xyz  ***
C
      CALL READM
C
C     *** Polymerize ! ***
C
      CALL POLYMERIZE
C
C     *** Put in water box ***
C
      CALL SOLVATE
C
C     *** Write xyz file without water ***
C
      CALL WRITEXYZ
C
C     *** End of PROGRAM polyM ***
C
      END

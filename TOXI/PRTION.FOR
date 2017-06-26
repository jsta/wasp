      SUBROUTINE PRTION (PIXC, NUX)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:14.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      INCLUDE 'ENVIRON.CMN'
C
      INCLUDE 'PHYSCHM.CMN'
C
      INCLUDE 'OPTION.CMN'
C
      INCLUDE 'CONST.INC'
C
      REAL XARG
      REAL NUX
      DIMENSION PIXC (5,3), NUX (5)
C
C          COMPUTE CLASSICAL PARTITION COEFFICIENTS FOR PARENT COMPOUND
C
C          CALCULATE PARTITION COEFFICIENTS FOR SORPTION TO SOLIDS
C
C          FOR SEDIMENT SEGMENTS (POROSITY <= 0.99) SET
C          PARTITION COEFFICIENT EQUAL TO CLASSICAL PARTITION
C          COEFFICIENT, SINCE NO COLLISION INDUCED DESORPTION.
C
      IF (PORE .GT. 0.990) GO TO 1010
            DO 1000 J = 1, 3
               PART(J, 1) = SFOC (J)*PIXC(1,J)
 1000       CONTINUE
      DO 1020 J = 1, 3
         DO 1020 ION = 2, 5
            PART (J, ION) = PIXC (ION,J)
 1020 CONTINUE
      GO TO 1030
 1010 CONTINUE
C
C          loop thru solids types
      DO 1060 J = 1, 3
         IF (C (J + 1, ISEG) .GT. 0.0) THEN
            PART (J, 1) = PIXC (1,J)*SFOC(J)/
     1         (1 + SOLIDS*PIXC (1,J)*SFOC(J)/NUX (1))
         END IF
C
 1060    CONTINUE

      DO 1040 J = 1, 3
CCSC
         XARG = ABS(C(J+1,ISEG))
C         IF (C (J + 1, ISEG) .EQ. 0.0) GO TO 1040
         IF (XARG .LT. R0MIN) GO TO 1040
C
C             loop thru species
           DO 1050 ION = 2, 5
               IF (SPFLG (ION - 1) .EQ. 0) GO TO 1050
            PART (J, ION) = PIXC (ION,J)/
     1         (1 + SOLIDS*PIXC (ION,J)/NUX (ION))
C
 1050    CONTINUE
 1040 CONTINUE                         
C
 1030 CONTINUE
C
C
      RETURN
      END

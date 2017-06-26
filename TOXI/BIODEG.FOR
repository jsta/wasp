      SUBROUTINE BIODEG (ICHM, Q10DIS, Q10DOC, Q10PAR, KBIO20)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:15.
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
      INCLUDE 'KNETIC.CMN'
C
      INCLUDE 'CONC.CMN'
C
      INCLUDE 'CONST.INC'
C
      REAL K1, K2, KP, KBIO20 (5,3)
      REAL XARG
C
      DIMENSION Q10DIS (5), Q10DOC (5), Q10PAR (5)
C
      BIOW (ICHM) = 0.0
      BIOS (ICHM) = 0.0
C
C      BIODEGRADATION :  SECOND ORDER KINETICS ASSUMED
C
C      loop thru the molecular & 4 ionic species
      IF (INITB .LE. 1) THEN
         DO 1000 ION = 1, 5
CCSC
C            IF (Q10DIS (ION) .EQ. 0.) Q10DIS (ION) = 1.
C            IF (Q10DOC (ION) .EQ. 0.) Q10DOC (ION) = 1.
C            IF (Q10PAR (ION) .EQ. 0.) Q10PAR (ION) = 1.
            XARG=ABS(Q10DIS(ION))
            IF (XARG .LT. R0MIN) Q10DIS (ION) = 1.0
            XARG=ABS(Q10DOC(ION))
            IF (XARG .LT. R0MIN) Q10DOC (ION) = 1.0
            XARG=ABS(Q10PAR(ION))
            IF (XARG .LT. R0MIN) Q10PAR (ION) = 1.0
1000    CONTINUE
      END IF
      DO 1010 ION = 1, 5
         IF (ION .GT. 1 )THEN
            IF (SPFLG (ION - 1) .EQ. 0) GO TO 1010
         endif
C           COMPUTE TEMPERATURE CORRECTION FOR DISSOLVED AND SORBED
C           CHEMICAL BIODEGRADATION
C
         XX = (TMP - 20.)/10.
         TDIS = Q10DIS (ION)**XX
         TDOC = Q10DOC (ION)**XX
         TPAR = Q10PAR (ION)**XX
C
C     COMPUTE SEPERATE BIODEGRADATION RATES FOR DISSOLVED, DOC SORBED AN
C     SOLIDS SORBED CHEMICAL
C
C         A)  DEGRADATION IN WATER
C              1) DISSOLVED CHEMICAL
         K1 = KBIO20 (ION, 1)*TDIS
C              2) DOC SORBED CHEMICAL
         K2 = KBIO20 (ION, 2)*TDOC
         IF (DISTOX (ICHM, ION) .LT. 1.0E-30) DISTOX (ICHM, ION) = 0.00
         BIOW (ICHM) = BIOW (ICHM) + (K1*DISTOX (ICHM, ION) +
     1      K2*DOCTOX (ICHM, ION))*SBAC
C
C         B)  DEGRADATION ON SOLIDS
         KP = KBIO20 (ION, 3)*TPAR
C
         DO 1020 J = 1, 3
            IF (C (J + 1, ISEG) .GT. 0.) THEN
               BIOS (ICHM) = BIOS (ICHM)
     1            + KP*SBAC*PARTOX (ICHM, J, ION)
            END IF
 1020    CONTINUE
C
 1010 CONTINUE
      RETURN
      END

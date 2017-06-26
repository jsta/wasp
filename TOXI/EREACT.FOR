      SUBROUTINE EREACT (ICHM, EEX, KE20)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:18.
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
      REAL KE20 (5, 3), KOX (3)
      REAL XARG
      DIMENSION EEX (5)
C
      EXRN (ICHM) = 0.0
C
C     Second-Order Kinetics Assumed
C
C       Compute Temperature Correction Using Arrhenius Expression
C
      XX = 1000.*(TK - TRE (ICHM))/(R*TK*TRE (ICHM))
C
C     begin loop through the molecular & four ionic species
      DO 1000 ION = 1, 5
         IF (ION .GT. 1 .AND. SPFLG (ION - 1) .EQ. 0) GO TO 1000
C
         TH = EXP (EEX (ION)*XX)
         DO 1010 J = 1, 3
            KOX (J) = KE20 (ION, J)*TH
 1010    CONTINUE
C
C       COMPUTE THE OXIDATION RATE FOR DISSOLVED & DOC SORBED CHEMICAL
C
         EXRN (ICHM) = EXRN (ICHM) + (KOX (1)*DISTOX (ICHM, ION) +
     1                 KOX (2) * DOCTOX (ICHM, ION)) * SOXRAD
C
C       COMPUTE THE OXIDATION RATE FOR SOLIDS SORBED CHEMICAL
C
         DO 1020 J = 1, 3
CCSC
            XARG = ABS(C(J+1,ISEG))
C            IF (C (J + 1, ISEG) .EQ. 0.0) GO TO 1020
            IF (XARG .LT. R0MIN) GO TO 1020
            CP = PARTOX (ICHM, J, ION)
            EXRN (ICHM) = EXRN (ICHM) + KOX (3)*CP*SOXRAD
 1020    CONTINUE
C
 1000 CONTINUE
      RETURN
      END

      SUBROUTINE HYDROL (ICHM, EHOH, EHN, EHH, KH20)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:16.
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
      DIMENSION EHOH (5), EHN (5), EHH (5), TH (3)
      REAL XARG
      REAL KH20 (5, 3, 3), KHY (3, 3)
C
      ALKH (ICHM) = 0.0
      NEUTH (ICHM) = 0.0
      ACIDH (ICHM) = 0.0
C
C      HYDROLYSIS :  ALKALINE, NEUTRAL, AND ACID HYDROLYSIS CONSIDERED
C
C
C        COMPUTE TEMPERATURE CORRECTION FOR ALKALINE, NEUTRAL AND ACID
C        HYDROLYSIS REACTIONS USING ARRHENIUS EXPRESSION:
C
C            k(T) = k(TR) * exp[1000*(E)*(T - TR)/(R*T*TR)]
C
C                       E = activation energy (kcal/mol)
C                       R = 1.99 cal/mol-degree K
C                       T = temperature (degrees K)
C                      TR = reference temp (degrees K;default = 293)
C
C
      XX = 1000.*(TK - TRH (ICHM))/(R*TK*TRH (ICHM))
C
C     begin loop for the molecular & four ionic species
      DO 1000 ION = 1, 5
         IF (ION .GT. 1 .AND. SPFLG (ION - 1) .EQ. 0) GO TO 1000
C
C         NOTE: Indexing for TH and K loop as follows:
C                 1 = alkaline hydrolysis
C                 2 = neutral hydrolysis
C                 3 = acid hydrolysis
C
         TH (1) = EXP (EHOH (ION)*XX)
         TH (2) = EXP (EHN (ION)*XX)
         TH (3) = EXP (EHH (ION)*XX)
C
C     LOOP FOR HYDROLYSIS TYPE (i.e., alkaline, neutral and acid)
C
         DO 1010 K = 1, 3
C
C          COMPUTE HYDROLYSIS RATES FOR DISSOLVED AND SORBED CHEMICAL
C             J = 1              = DISSOLVED CHEMICAL
C             J = 2              = DOC SORBED CHEMICAL
C             J = 3              = SOLIDS SORBED CHEMICAL
C
            DO 1020 J = 1, 3
               KHY (J, K) = KH20 (ION, J, K)*TH (K)
 1020       CONTINUE
C
 1010    CONTINUE
C         DEBUG
         IF (DISTOX (ICHM, ION) .LT. 1.0E-27) DISTOX (ICHM, ION) = 0.0
         ALKH (ICHM) = ALKH (ICHM) + (KHY (1, 1)*DISTOX (ICHM, ION) +
     1      KHY (2, 1)*DOCTOX (ICHM, ION))*OH
         NEUTH (ICHM) = NEUTH (ICHM) + (KHY (1, 2)*DISTOX (ICHM, ION) +
     1      KHY (2, 2)*DOCTOX (ICHM, ION))
         ACIDH (ICHM) = ACIDH (ICHM) + (KHY (1, 3)*DISTOX (ICHM, ION) +
     1      KHY (2, 3)*DOCTOX (ICHM, ION))*H
         DO 1030 J = 1, 3
CCSC
            XARG = ABS(C(J+1,ISEG))
C            IF (C (J + 1, ISEG) .EQ. 0.0) GO TO 1030
            IF (XARG .LT. R0MIN) GO TO 1030
            CP = PARTOX (ICHM, J, ION)
            ALKH (ICHM) = ALKH (ICHM) + KHY (3, 1)*CP*OH
            NEUTH (ICHM) = NEUTH (ICHM) + KHY (3, 2)*CP
            ACIDH (ICHM) = ACIDH (ICHM) + KHY (3, 3)*CP*H
 1030    CONTINUE
C
C
 1000 CONTINUE
      RETURN
      END

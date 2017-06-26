      SUBROUTINE CHEM3
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:13.
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
      INCLUDE 'PHOTOL.CMN'
C
      INCLUDE 'KNETIC.CMN'
C
      INCLUDE 'CONC.CMN'
C
      INCLUDE 'CHMLOC.CMN'
C
      INCLUDE 'PARAM.EQU'
C
      INCLUDE 'GLOBAL.EQU'
C
      INCLUDE 'CHEM3.EQU'
C
      INCLUDE 'CONST.INC'
      REAL XARG
C
      K = 3
C
      DO 1000 ION = 1, 4
         SPFLG (ION) = INT (SFLG (ION))
 1000 CONTINUE
      IV = INT (XV)
      IPHOTO = INT (XPHOTO)
C
C
C         INITIALIZE VARIABLES
C
      DO 1010 ION = 1, 5
         KA (ION) = 0.0
         DO 1010 JJ = 1, 3
            PART (JJ, ION) = 0.0
 1010 CONTINUE
C
C
C          1) MOLECULAR DIFFUSIVITY (METERS**2/S) @ 25 degrees C
C
C             WATER:
      DIFFW = 22.E-09/(MOLWT**0.6666)
C
C             AIR:
      DIFFA = 1.9E-04/(MOLWT**0.6666)
C
C          2) OCTANOL-WATER PARTITION COEFFICIENT
C
      KOW = 10**LKOW
C
C          3) ORGANIC CARBON PARTITION COEFFICIENT
C
CCSC
           XARG = ABS(LKOC)
C          IF (LKOC .EQ. 0.) THEN
          IF (XARG .LT. R0MIN) THEN
             KOC = 0.0
          ELSE
             KOC = 10**LKOC
          END IF
C
C
C      C) SET PARENT COMPOUND FOR THIS CHEMICAL
C
C      L = INT (PARENT)
C
C      D) PRODUCTION FROM PARENT COMPOUND
C
C      IF (L .GT. 0) THEN
C         PROD = BIOW (L)*YIELD (1) + BIOS (L)*YIELD
C     1      (2) + ALKH (L)*YIELD (3)
C     2      + NEUTH (L)*YIELD (4) + ACIDH (L)*YIELD (5)
C     3      + FOTO (L)*YIELD (6) + OX (L)*YIELD (7)
C      ELSE
C         PROD = 0.0
C      END IF
C
C
C
C     E) DETERMINE SOLIDS-WATER PARTITION COEFFICIENT OF THE CHEMICAL
      CALL PRTION (PIXC, NUX)
C
C
C      F) COMPUTE AIR-WATER PARTITION COEFFICIENT OF THE CHEMICAL (H/RT)
      KAW = HENRY/(8.206E-05*(TMP + 273.15))
C
C
C     G) COMPUTE EQUILIBRIUM & DISTRIBUTION CONSTANTS FOR IONIZATION
C          1 = singly charged cation
C          2 = doubly charged cation
C          3 = singly charged anion
C          4 = doubly charged anion
C
      DO 1020 ION = 1, 4
         IF (SPFLG (ION) .GT. 0) THEN
            LOG10KA = -PKA (ION) + (1000.*EPKA (ION)*(TK-TRI(3))/(2.303*
     1R*TK*TRI(3)))
            KA (ION) = 10**LOG10KA
         END IF
 1020 CONTINUE
C
C           Distribution constants are the ratios of ionic form
C           to molecular form.
C
      DISTRIB (1) = KA (1)/OH
      DISTRIB (2) = KA (1)*KA (2)/(OH*OH)
      DISTRIB (3) = KA (3)/H
      DISTRIB (4) = KA (3)*KA (4)/(H*H)
C
C
C     H) COMPUTE FRACTION OF TOTAL CHEMICAL FOR EACH SPECIE AND PHASE
      CALL FRCION (3, DISTRIB, PIDOC)
C
C     I) DETERMINE KINETIC COMPONENTS OF DERIVATIVE
C
      IF (TOTKG (ISEG, 3) .GT. 0.) THEN
         CD (6, ISEG) = - TOTKG (ISEG, 3)*C (6, ISEG)*BVOL (ISEG)
      ELSE
         IF (TIME .GE. RXNTIM) THEN
C
C          1) BIODEGRADATION
C
            IF (ITYPE (ISEG) .LE. 2) THEN
CCSC
               XARG = ABS(KBW)
C               IF (KBW .EQ. 0.) THEN
               IF (XARG .LT. R0MIN) THEN
                  CALL BIODEG (3, Q10DIS, Q10DOC, Q10PAR, KBIO20)
               ELSE
                  BIOW (3) = KBW*C (6, ISEG)
               END IF
            ELSE
CCSC
               XARG = ABS(KBS)
C               IF (KBS .EQ. 0.) THEN
               IF (XARG .LT. R0MIN) THEN
                  CALL BIODEG (3, Q10DIS, Q10DOC, Q10PAR, KBIO20)
               ELSE
                  BIOS (3) = KBS*C (6, ISEG)
               END IF
            END IF
C
C          2) HYDROLYSIS
C
CCSC
            XARG = ABS(KH(3))
C            IF (KH (3) .EQ. 0.) THEN
            IF (XARG .LT. R0MIN) THEN
               CALL HYDROL (3, EHOH, EHN, EHH, KH20)
            ELSE
               ALKH (3) = KHOH*C (6, ISEG)
               NEUTH (3) = KHN*C (6, ISEG)
               ACIDH (3) = KHH*C (6, ISEG)
            END IF
C
C          3) PHOTOLYSIS
C
CCSC
            XARG = ABS(KF)
C            IF (KF .EQ. 0.) THEN
            IF (XARG .LT. R0MIN) THEN
               IF (IPHOTO .NE. 0) THEN
                  CALL BEER
                  CALL PHOTO (3, RFLATG, KDPG, QUANTG)
               END IF
            ELSE
               FOTO (3) = KF*C (6, ISEG)
            END IF
C
C          4) VOLATILIZATION
C
            VLT = 0.0
            IF (ITYPE (ISEG) .EQ. 1) THEN
CCSC
               XARG = ABS(KV)
C               IF (KV .EQ. 0.) THEN
               IF (XARG .LT. R0MIN) THEN
                  IF (IV .GT. 0) CALL VOLAT (MOLWT, ATMOS3, KLT, VLT, 3,
     1                                       KVOG)
               ELSE
                  VLT = KV*C (6, ISEG)
               END IF
            END IF
C
C          5) OXIDATION
CCSC
            XARG = ABS(KO)
C            IF (KO .EQ. 0.) THEN
            IF (XARG .LT. R0MIN) THEN
               CALL OXID (3, EOX, KOX20)
            ELSE
               OX (3) = KO*C (6, ISEG)
            END IF
C
C
C          6) User Defined Extra Reaction
C
CCSC
            XARG = ABS(KE)
C            IF (KE .EQ. 0.) THEN
            IF (XARG .LT. R0MIN) THEN
               CALL EREACT(3,EEX,KE20)
            ELSE
               EXRN(3) = KE * C(6,ISEG)
            ENDIF
C
C
C     J) KINETIC DERIVATIVE
C
         END IF
         CD (6, ISEG) = - (BIOW (3) + BIOS (3) + ALKH (3) + NEUTH (3)
     1      + ACIDH (3) + FOTO (3) + VLT + OX (3) + EXRN(3))
C
C
C     K) MULTIPLY BY VOLUME TO BE CONSISTANT WITH TRANSPORT CALCULATION
C
         CD (6, ISEG) = CD (6, ISEG)*BVOL (ISEG)
C
C           Save Effective Rate Constants
C
         IF (C (6, ISEG) .GT. 0.0) THEN
            RATE (3, 1) = (BIOW (3) + BIOS (3))/C (6, ISEG)
            RATE (3, 2) = (ALKH (3) + NEUTH (3) + ACIDH (3))/C (6, ISEG)
            RATE (3, 3) = FOTO (3)/C (6, ISEG)
            RATE (3, 4) = VLT/C (6, ISEG)
            RATE (3, 5) = OX (3)/C (6, ISEG)
            RATE (3, 6) = EXRN(3)/C (6, ISEG)
         END IF
      END IF
      RETURN
      END

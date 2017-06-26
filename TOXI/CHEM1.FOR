      SUBROUTINE CHEM1
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:03.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
C***********************************************************************
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
      INCLUDE 'CHEM1.EQU'
C
      INCLUDE 'CONST.INC'
C
      REAL XARG
C
C     INTEGER THE VARIOUS OPTION FLAGS:
C
      DO 1000 ION = 1, 4
         SPFLG (ION) = INT (SFLG (ION))
 1000 CONTINUE
      IV = INT (XV)
      IPHOTO = INT (XPHOTO)
C
C     UNIVERSAL GAS CONSTANT  (cal/deg-mol)
C
      R = 1.9872
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
      KOW = 10.00**LKOW
C
C          3) ORGANIC CARBON PARTITION COEFFICIENT
C
CCSC
           XARG=ABS(LKOC)
C          IF (LKOC .EQ. 0.) THEN
          IF (XARG .LT. R0MIN) THEN
             KOC = 0.0
          ELSE
             KOC = 10**LKOC
          END IF
C
C     INITIALIZE VARIABLES
C
      DO 1010 ION = 1, 5
         KA (ION) = 0.0
         DO 1010 J = 1, 3
            PART (J, ION) = 0.0
 1010 CONTINUE
      DO 1020 ICHM = 1, 3
         BIOS (ICHM) = 0.0
         BIOW (ICHM) = 0.0
         ALKH (ICHM) = 0.0
         NEUTH (ICHM) = 0.0
         ACIDH (ICHM) = 0.0
         FOTO (ICHM) = 0.0
 1020 CONTINUE
C
C
C     A) DETERMINE SOLIDS-WATER PARTITION COEFFICIENT OF THE CHEMICAL
C
      CALL PRTION (PIXC, NUX)
C
C
C     B) COMPUTE AIR-WATER PARTITION COEFFICIENT OF THE CHEMICAL (H/RT)
      KAW = HENRY/(8.206E-05*(TMP + 273.15))
C
C
C     C) COMPUTE EQUILIBRIUM & DISTRIBUTION CONSTANTS FOR IONIZATION
C          1 = singly charged cation
C          2 = doubly charged cation
C          3 = singly charged anion
C          4 = doubly charged anion
C
      DO 1030 ION = 1, 4
         IF (SPFLG (ION) .GT. 0) THEN
            LOG10KA = -PKA (ION) + (1000.*EPKA (ION)*(TK-TRI(1))/(2.303*
     1R*TK*TRI(1)))
            KA (ION) = 10**LOG10KA
         END IF
 1030 CONTINUE
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
C     D) COMPUTE FRACTION OF TOTAL CHEMICAL FOR EACH SPECIE AND PHASE
C
      CALL FRCION (1, DISTRIB, PIDOC)
C
      IF (TOTKG (ISEG, 1) .GT. 0.) THEN
         CD (1, ISEG) = - TOTKG (ISEG, 1)*C (1, ISEG)*BVOL (ISEG)
      ELSE
         IF (TIME .GE. RXNTIM) THEN
C
C     E) DETERMINE KINETIC COMPONENTS OF DERIVATIVE
C
C          1) BIODEGRADATION
C
            IF (ITYPE (ISEG) .LE. 2) THEN
CCSC
               XARG=ABS(KBW)
C               IF (KBW .EQ. 0.) THEN
               IF (XARG .LT. R0MIN) THEN
                  CALL BIODEG (1, Q10DIS, Q10DOC, Q10PAR, KBIO20)
               ELSE
                  BIOW (1) = KBW*C (1, ISEG)
               END IF
            ELSE
CCSC
               XARG=ABS(KBS)
C               IF (KBS .EQ. 0.) THEN
               IF (XARG .LT. R0MIN) THEN
                  CALL BIODEG (1, Q10DIS, Q10DOC, Q10PAR, KBIO20)
               ELSE
                  BIOS (1) = KBS*C (1, ISEG)
               END IF
            END IF
C
C          2) HYDROLYSIS
C
CCSC
            XARG=ABS(KH(1))
C            IF (KH (1) .EQ. 0.) THEN
            IF (XARG .LT. R0MIN) THEN
               CALL HYDROL (1, EHOH, EHN, EHH, KH20)
            ELSE
               ALKH (1) = KHOH*C (1, ISEG)
               NEUTH (1) = KHN*C (1, ISEG)
               ACIDH (1) = KHH*C (1, ISEG)
            END IF
C
C          3) PHOTOLYSIS
C
CCSC
            XARG=ABS(KF)
C            IF (KF .EQ. 0.) THEN
            IF (XARG .LT. R0MIN) THEN
               IF (IPHOTO .NE. 0) THEN
                  CALL BEER
                  CALL PHOTO (1, RFLATG, KDPG, QUANTG)
               END IF
            ELSE
               FOTO (1) = KF*C (1, ISEG)
            END IF
C
C          4) VOLATILIZATION
C
            VLT = 0.0
            IF (ITYPE (ISEG) .EQ. 1) THEN
CCSC
               XARG=ABS(KV)
C               IF (KV .EQ. 0.) THEN
               IF (XARG .LT. R0MIN) THEN
                  IF (IV .GT. 0) CALL VOLAT (MOLWT, ATMOS1, KLT, VLT, 1,
     1                                       KVOG)
               ELSE
                  VLT = KV*C (1, ISEG)
               END IF
            END IF
C
C          5) OXIDATION
C
CCSC
            XARG=ABS(KO)
C            IF (KO .EQ. 0.) THEN
            IF (XARG .LT. R0MIN) THEN
               CALL OXID (1, EOX, KOX20)
            ELSE
               OX (1) = KO*C (1, ISEG)
            END IF
C
C          6) User Defined Extra Reaction
C
CCSC
            XARG=ABS(KE)
C            IF (KE .EQ. 0.) THEN
            IF (XARG .LT. R0MIN) THEN
                CALL EREACT(1,EEX,KE20)
            ELSE
              EXRN(1) = KE * C(1,ISEG)
            ENDIF
C
C
C     F) KINETIC DERIVATIVE
C
         END IF
         CD (1, ISEG) = - (BIOW (1) + BIOS (1) + ALKH (1) + NEUTH (1)
     1      + ACIDH (1) + FOTO (1) + VLT + OX (1) + EXRN(1))
C
C
C     G) MULTIPLY BY VOLUME TO BE CONSISTANT WITH TRANSPORT CALCULATION
C
         CD (1, ISEG) = CD (1, ISEG)*BVOL (ISEG)
C
C           Save Effective Rate Constants
C
         IF (C (1, ISEG) .GT. 0.0) THEN
            RATE (1, 1) = (BIOW (1) + BIOS (1))/C (1, ISEG)
            RATE (1, 2) = (ALKH (1) + NEUTH (1) + ACIDH (1))/C (1, ISEG)
            RATE (1, 3) = FOTO (1)/C (1, ISEG)
            RATE (1, 4) = VLT/C (1, ISEG)
            RATE (1, 5) = OX (1)/C (1, ISEG)
            RATE (1, 6) = EXRN(1)/C (1, ISEG)
         END IF
      END IF
C
C
      RETURN
      END

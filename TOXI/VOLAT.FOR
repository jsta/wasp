      SUBROUTINE VOLAT (MOLWT, ATMOS, KLT, VLT, ICHM,KVOG)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:17.
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
      INCLUDE 'CONC.CMN'
C
C*-*-
      INCLUDE 'SSCOM.CMN'
C
      INCLUDE 'CONST.INC'
C*-*-
C
      REAL XARG
      REAL MOLWT, KLT, KVOG, KL, KLA
C
C*-*-
C
C     SET VISCOUS SUBLAYER AND DRAG COEFFICIENTS FOR VOLATILIZATION
C
      XLAM2 = 4.
      CDRAG = 0.0011
C
      STP20 = TMP - 20.
C
C      VOLATILIZATION
C
      VLT = 0.0
      IF (IV .EQ. 1) THEN
         KL = SREAER
      ELSE
         IF (IWB .EQ. 0) THEN
C
C         Stream, River or Estuary
C
            IF (IV .EQ. 2) THEN
CCSC
               XARG = ABS(KVOG)
C               IF (KVOG .EQ. 0.)THEN
               IF (XARG .LT. R0MIN) THEN
                  KVOG = SQRT(32./MOLWT)
               ENDIF
               XKL = SREAER * KVOG
            ELSE
C
C        CHOOSE KL EQUATION BASED ON DEPTH & VELOCITY:
               XX = SQRT (32./MOLWT)
C           Owens:
               IF (SDEPTH .LT. 0.61) THEN
                  XKL = XX*5.349*VEL**.67/SDEPTH**.85
               ELSE
C              O'connor-dobbins:
                  IF (VEL .LT. 0.518 .OR. SDEPTH .GT.
     1                  13.584*VEL**2.9135) THEN
                     XKL = SQRT (DIFFW*VEL/SDEPTH)*86400.
                  ELSE
C              Churchill:
                     XKL = XX*5.049*VEL**.969/SDEPTH**.673
                  END IF
               END IF
            END IF
            XKGH = 100.*KAW
C
            KL = (1./(1./XKL + 1./XKGH))*KLT**STP20
C
C              Reservoir, Lake or Pond
C
C     COMPUTE DENSITY (G/ML) AND VISCOSITY (M**2/S) OF AIR AND WATER
C
         ELSE
            DENA = 0.001293/(1. + 0.00367*ATMP)
            DENW = 1. - 8.8E-05*TMP
C            XNUA = (1.32 + 0.009*ATMP)*1.E-05
            XNUA = (1.32 + 0.009*ATMP)*10.0
            XNUW = (10.**(1301./(998.333 + 8.1855*
     1         STP20 + 0.00585*STP20**2) -
     2         3.30233)/DENW)*1.E-04
C
C     COMPUTE SCHMIDT NUMBERS FOR AIR AND WATER
C
            SCA = XNUA/DIFFA
            SCW = XNUW/DIFFW
C
C     COMPUTE AIR LAYER AND WATER LAYER TRANSFER RATES
C          USE O'CONNOR or MACKAY AS PER USER OPTION
C
            IF (IV .EQ. 2 .OR. IV .EQ. 4) THEN
C
C     O'CONNOR:
C        note: 0.905 = (Von Karmon constant)**0.333
C
               USTAR = SQRT (CDRAG)*SWIND
               IF (IV .EQ. 2) THEN
                  XKL = SREAER*SQRT (32./MOLWT)/86400.
               ELSE
                  XKL = USTAR*SQRT (DENA/DENW)*(.
     1               905/XLAM2)*(1/SCW)**.666 + 1.0E-9
               END IF
               XKG = USTAR*(.905/XLAM2)*(1/SCA)**.666 + 1.0E-9
            ELSE
C
C     MACKAY:
C
               USTAR = .01*SWIND*SQRT (6.1 + 0.63*SWIND)
               IF (IV .EQ. 3) THEN
                  XKL = SREAER*SQRT (32./MOLWT)/86400.
               ELSE
                  IF (USTAR .GT. 0.3) XKL = USTAR*.
     1               00341*(1/SCW)**.5 + 1.E-06
                  IF (USTAR .LE. 0.3) XKL = USTAR**2.2*.
     1               0144*(1/SCW)**.5 + 1.E-06
               END IF
               XKG = USTAR*.0462*(1/SCA)**.666 + 1.E-03
            END IF
C
            XKGH = XKG*KAW
C
            KL = (1./(1./XKL + 1./XKGH))*KLT**STP20*86400.
C
         END IF
      END IF
      KLA = KL/SDEPTH
      VLT = KLA*(DISTOX (ICHM, 1) - ATMOS/KAW)
C
      RETURN
      END

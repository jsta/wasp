      SUBROUTINE PHYTO
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:57.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG,XDIFF
C
C        *-*-*-*-*-*  System 4 - Phytoplankton  *-*-*-*-*-*
C
      IF (SEDSEG) THEN
C
         GP1 = 0.0
         GPP = 0.0
         RESP = 0.0
         DP1 = KPZDC*KPZDT**STP20
         DPP = DP1*PHYT
         TCHLA = PHYT/CCHL1
C
C                      Growth Rate
C
      ELSE
         IF (SYSBY (4) .EQ. 1) THEN
            GP1 = K1C
            GPP = GP1*PHYT
            PNH3G1 = 0.
         ELSE
            GITMX1 = K1C*K1T**STP20
            GITMAX = GITMX1
            IKE = KEFN (ISEG)
C
C               Compute growth rate reduction due to light conditions
C               using either Dick Smith's or Di Toro's formulation
C
CCSC
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:17:51.
C               XDIFF = LGHTSW - 1.0
C               XARG = ABS(XDIFF)
               IF (LGHTSW .LE. 1.0) call ditoro
C               IF (XARG .LT. R0MIN) CALL DITORO
               IF (LGHTSW .GE. 2.0) call smith
C
             GIT1 = RLIGHT*GITMAX
C
C                   Compute ammonia preference
C
            PNH3G1 = 0.0
C
C
            IF (NH3 .GE. 1.0E-5)
     1         PNH3G1 = NH3*NO3/((KMNG1 + NH3)*(KMNG1 + NO3))
     2         + NH3*KMNG1/((NH3 + NO3)*(KMNG1 + NO3))
C
C              Compute Michaelis Limitations
C
            CN = NH3 + NO3
            XEMP1 = CN/(KMNG1 + CN)
            DOPO4 = OPO4*F (2, 3, ISEG)
            XEMP2 = DOPO4/(KMPG1 + DOPO4)
C
C       Compute Growth Rate Reduction due to Nutrient Limitation
C
CCSC
            XARG = ABS(NUTLIM)
C            IF (NUTLIM .EQ. 0.) RNUTR = AMIN1 (XEMP1, XEMP2)
            IF (XARG .LT. R0MIN) RNUTR = AMIN1 (XEMP1, XEMP2)
CCSC
            XDIFF = NUTLIM - 1.0
            XARG = ABS(XDIFF)
C            IF (NUTLIM .EQ. 1.) RNUTR = XEMP1*XEMP2
            IF (XARG .LT. R0MIN) RNUTR = XEMP1*XEMP2
            GP1 = RNUTR*GIT1
            GPP = GP1*PHYT
         END IF
C
C                     Respiration Rate
C
         RESP = K1RC*K1RT**STP20
C
C       ALGAL RESPIRATION + DEATH
C
         DP1 = RESP + K1D + K1G*ZOO*ZOOSG(ISEG)
         DPP = DP1*PHYT
         RESP = RESP*PHYT
C         IF (PHTY .GT. 1.0E-6)THEN
            XEMPRC = PHYT/(KMPHYT + PHYT)
C         ELSE
C            XEMPRC = 1.0E-6/(KMPHYT + 1.0E-6)
C         ENDIF
C
      END IF
C
      CD (4, ISEG) = (GP1 - DP1)*PHYT*VOL
      RETURN
      END

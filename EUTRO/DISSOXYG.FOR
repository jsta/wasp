      SUBROUTINE DISSOXYG
C
C     Last Revised:  Date: Monday, 26 August 1991.  Time: 10:37:53.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG1, XARG2
      REAL K2WIND, K2HYDRA
C
C              *-*-*-*-*  SYSTEM 6 - OXYGEN  *-*-*-*-*
C
C                          Sources
C
      K20 = 0.0
      IF ( .NOT. SEDSEG) GO TO 1000
      SR190 = 0.0
      SR19PA = 0.0
      SR19PB = 0.0
      SK19P = 0.0
      GO TO 1010
C
C                         Reaeration
C
 1000 CONTINUE
      IF (ITYPE (ISEG) .EQ. 1 .AND. XICECVR .GT. 0.0) THEN
CCSC
         XARG1 = ABS(K2)
         XARG2 = ABS(rearsg(iseg))
C         IF (K2 .EQ. 0. .AND. REARSG(ISEG) .EQ. 0.0) THEN
         IF (XARG1.LT.R0MIN.AND.XARG2.LT.R0MIN) THEN
            K2WSAVE = 0.
            K2HSAVE = 0.
            CALL KAWIND (WIND, STP, AIRTMP, H, WTYPE, KAINIT, K2WIND)
            CALL KAHYDRA (K2HYDRA)
            K2WSAVE = K2WIND
            K2HSAVE = K2HYDRA
            IF (K2WIND .GT. K2HYDRA) THEN
               KA = K2WIND * XICECVR
            ELSE
               KA = K2HYDRA * XICECVR
            END IF
         ELSE
            IF (K2 .GT. 0)THEN
                KA = ((K2*1.028**STP20)* XICECVR)
            ELSE
                KA = REARSG(ISEG) * REAR
            ENDIF
         ENDIF
      ELSE
         KA = 0.0
      END IF
C
C       Calculate oxygen saturation level for current water
C       temperature; DOSAT is expressed as mg oxygen per liter
C
      SALIN (ISEG) = SAL (ISEG)*SALFN
      CL = (SAL (ISEG)*SALFN)/1.80655
      TK = STP + 273.
      RLNCS = - 139.34411 + (1.575701E05/TK) - (6.642308E07/TK**2) +
     1   (1.243800E10/TK**3) - (8.621949E11/TK**4) -
     2   (CL*(3.1929E-02 - (19.428/TK) + (3.8673E03/TK**2)))
C
      CS = EXP (RLNCS)
C
      SR190 = KA*(CS - DO)
C
C                 Evolution by phytoplankton
C          growth of phytoplankton using CO2 and NH3
C
      SR19PA = PNH3G1*GP1*PHYT*32./12.
C
C       Growth of phytoplankton using CO2 and NO3 (2NO3 = 2NH3 + 302)
C
      SR19PB = (1. - PNH3G1)*GP1*PHYT*32.*(1./12. + 1.5*NCRB/14.)
C
      SR19P = SR19PA + SR19PB
C
C      NOTE: SR19P = GPP*(32/12 + (1.5*NCRB/14)*(1-PNH3G1))
C
C                             Sinks
C                       Algal Respiration
C
      SK19P = OCRB*RESP
C
C        Nitrification (NH3-N + 2O2 = NO3-N + H2O + H)
C
 1010 CONTINUE
      SK1913 = 64./14.*SK1314
C
C                      Oxidation of CBOD
C
      SK1918 = SK180
C
C             Sediment Oxygen Demand (1-D Networks)
C
      SK19S = SOD1D (ISEG)*sodta(iseg)**stp20/H
C
C=======================================================================
C                     Formulate Derivative
C
      CD (6, ISEG) = (SR190 + SR19PA + SR19PB - SK19P
     1   - SK1913 - SK1918 - SK19S)*VOL
C
C
      RETURN
      END

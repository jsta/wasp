      SUBROUTINE SOLFCT (TIME1, GLOLIT)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:22.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C  Function:
C       Computation of irradiance as function of solar zenith angle.
C       Completed 26 November 1983 by L.A. Burns.
C       Revised 04-SEP-1985 (LAB) to hardwire result of EXP(-(>87.4))
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
C       Local variables:
      REAL TIME1, GLOLIT, BEAM, XTES
      REAL XMU, XMU2, XMUI (3), ZENANG, SLAMTH, PHI
C       BEAM is solar beam intensity.
C       GLOLIT is global light intensity (beam + skylight)
C       XMU are generalized cosine functions specific to air, aerosol,
C       and ozone.
C       ZENANG is solar zenith angle.
C       SLAMTH and PHI are skylight computational variables:
C        INCLUDE 'solar.cmn'
      REAL AERDEP (46), AIREFL, BREW, CLCD, EFF, EMMHI, G3T3, GT1GT2
     1   , HLAM (46), ONEFF, OZDEP (46), RAYDEP
     2   (46), REFIND (46), REFRAT
     3   , SLSD, TTEE
      INTEGER LAMBDA
      COMMON /SUNINT/ AERDEP, AIREFL, BREW, CLCD, EFF, EMMHI, G3T3
     1   , GT1GT2, HLAM, ONEFF, OZDEP, RAYDEP, REFRAT
     2   , SLSD, TTEE, LAMBDA
C
C       AERDEP is aerosol (turbidity) optical depth.
C       BREW is reflection computed according to Brewster's law (when
C       sum of the angles of incidence and reflection is 90 degrees).
C       CLCD is cos(latitude) * cos(solar declination)
C       EFF, ONEFF, G3T3, GT1GT2, TTEE are computational
C       elements for skylight.
C       EMMHI is product of vertical beam intensity (HIBEAM) and EMM
C       (the latter the "M" function), for skylight computations.
C       HLAM is solar input at the top of the atmosphere at the date to
C       be processed--i.e., solar constant/(square of the radius vector)
C       LAMBDA counts through wavelength intervals in WLAMG computations
C       OZDEP is the ozone optical depth, i.e., the product of the
C       amount of ozone in the atmosphere OZONEG (in cm NTP), the ozone
C       absorption coefficients (K3), and the elevation correction NHI(3
C       RAYDEP is Rayleigh optical depth corrected for ground station
C       elevation: RAYFAC * NHI(1); the latter the pressure correction
C       REFRAT is reflection for normal incidence of light beam.
C       SLSD is sin(latitude) * sin(solar declination)
C       REFIND is refractive index of water at 20 C, from Jerlov's book
C       p 23, computed by linear interpolation between pairs of values.
      DATA REFIND/1.3662, 1.3652, 1.3643, 1.3634, 1.3625, 1.3615, 1.3606
     1   , 1.3597, 1.3588, 1.3578, 1.3569, 1.3565, 1.
     2   3561, 1.3557, 1.3553, 1.3548
     3   , 1.3544, 1.3539, 1.3528, 1.3511, 1.3495, 1.
     4   3479, 1.3467, 1.3456, 1.3444
     5   , 1.3433, 1.3424, 1.3415, 1.3407, 1.3399, 1.
     6   3393, 1.3387, 1.3381, 1.3375
     7   , 1.3369, 1.3364, 1.3355, 1.3346, 1.3336, 1.
     8   3327, 1.3322, 1.3316, 1.3310
     9   , 1.3303, 1.3293, 1.3283/
C       End of include file "SOLAR"
      DATA XTES/87.4/
C       Convert time (sec) to local hour angle and compute cosine of
C       zenith angle (XMU):
      XMU = SLSD + CLCD*COS (TIME1/13751.)
      IF (XMU .LT. 0.0) XMU = 0.0
C       Compute fraction of beam reflected:
C       a. zenith angle is angle of incidence:
      ZENANG = ACOS (XMU)
C       b. Apply Snell's law to get angle of refraction:
      REFANG = ASIN (SIN (ZENANG)/REFIND (LAMBDA))
C       Special cases arise for normal incidence (case c2) and when
C       the angles of incidence and refraction sum to 90 degrees:
      IF (ZENANG .LT. 0.0017) GO TO 1000
      TEMANG = ABS (1.571 - (ZENANG + REFANG))
      IF (TEMANG .LT. 0.0017) GO TO 1010
C       Case 1. Compute trig functions for Fresnel's law:
      S1 = SIN (ZENANG - REFANG)
      S9 = SIN (ZENANG + REFANG)
      T1 = TAN (ZENANG - REFANG)
      T2 = TAN (ZENANG + REFANG)
C       Compute fraction of beam reflected via Fresnel's law:
      REFLEC = (ABS (((S1*S1)/(S9*S9)) + ((T1*T1)/(T2*T2))))/2.
      GO TO 1020
 1000 CONTINUE
C       Case 2. Normal incidence of beam (REFRAT is computed in SOLAR):
      REFLEC = REFRAT
      GO TO 1020
 1010 CONTINUE
C       Case 3. i + j is 90 degrees, use Brewster's law:
      REFLEC = BREW
 1020 CONTINUE
      XMU2 = XMU*XMU
      XMUI (1) = SQRT ((XMU2 + 1.8E-03)/1.0018)
      XMUI (2) = SQRT ((XMU2 + 3.0E-04)/1.0003)
      XMUI (3) = SQRT ((XMU2 + 7.4E-03)/1.0074)
C
      EXPARG = RAYDEP (LAMBDA)/XMUI (1) +
     1   AERDEP (LAMBDA)/XMUI (2) + OZDEP (LAMBDA)/XMUI (3)
      IF (EXPARG .LE. XTES) THEN
         BEAM = XMU*HLAM (LAMBDA)*EXP ( - EXPARG)
      ELSE
         BEAM = 0.0
      END IF
C       Compute beam irradiance just below water surface:
      BEAM = BEAM*(1.0 - REFLEC)
C       Compute skylight functions:
      PHI = SQRT ((1. + TTEE)/(XMU2 + TTEE)) - 1.0
CD      WRITE(5,*) 'PHI = ', PHI, 'G3T3 = ',G3T3,'GT1GT2 = ', GT1GT2
      EXPARG = ABS (G3T3*PHI)
      EXPAR2 = ABS (GT1GT2*PHI)
      IF (EXPAR2 .GT. XTES) THEN
         SLAMTH = 0.0
      ELSE IF (EXPARG .GT. XTES) THEN
         SLAMTH = EFF*EXP (GT1GT2*PHI)
      ELSE
         SLAMTH = (EFF + ONEFF*EXP (G3T3*PHI))*EXP (GT1GT2*PHI)
      END IF
C       Add sky radiation to beam:
      SKYLIT = (0.934/(1.0 - 0.066*AIREFL))*EMMHI*SLAMTH
      GLOLIT = BEAM + SKYLIT
      RETURN
C       End of Subroutine SOLFCT
      END

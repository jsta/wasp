      SUBROUTINE SOLAR (ILITE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:21.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C  Function:
C       Computes solar irradiance just below the water surface.
C       Completed 26 November 1983 (L.A. Burns).
C       Revised 05-SEP-1985 (LAB) to hardwire result of EXP(-(>87.4))
C       and enter data for SUNIN.
C       Revised 15-Oct-1986 by John P. Connolly for use with WASP.
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      INCLUDE 'PHOTOL.CMN'
C
C
      INCLUDE 'GLOBAL.EQU'
C
       REAL LATG
C
C       Allocate storage for integration:
      DOUBLE PRECISION DINTPT
      REAL GLOLIT, TIMER, TNIGHT, XINT (11), YINT (11)
C
C     Counters, pointers, and intermediate computational variables:
C
      REAL CENLAM (46), S1, S9, YHI
C       CENLAM is the center wavelength of each of the 46 wavelength
C       intervals (in nanometers):
C
      REAL ABFRAC (46), TAU2, TAU4, FA3, FA4, F1, F2, FB3, FB4
C       ABFRAC is fraction of total aerosol extinction coefficient that
C       is due to light absorption rather than scattering. ABFRAC was
C       computed via regression on wavelength using data in GCS.
C       TAU2 and TAU4 are aerosol optical depths due to scattering and
C       absorption, respectively.
C       AIREFL is air reflectivity function.
C       Air mass data for computing aerosol extinction coefficients as
C       function of wavelength and relative humidity (from Green and
C       Schippnick Copenhagen report)--entry 1 is Rural, entry 2 is
C       Urban, entry #3 is Maritime, and #4 is Tropospheric air mass.
C
      REAL ELAM0 (4), KAER (4), PAER (4), LAMA0 (4), CAPEL (4)
C       Results of relative humidity computations:
      REAL LAMAR, ELAM0R
C
C       Aerosol extinction coefficients:
      REAL EXTAER (46)
      REAL K3 (46), RAYFAC (46), NHI (3), RHUML
C       K3 are (Naperian) absorption coefficients for atmospheric ozone.
C       RAYFAC is Rayleigh optical depth (molecular scattering in the
C       atmosphere) at sea level pressure of 1013 mb.
C       The NHI are elevation corrections for ground station elevation
C       above sea level, based on the report of Green, Cross and Smith
C       (GCS, Photochem. Photobiol 31:59-65 (1980)). NHI(1) applies to
C       air (Rayleigh scattering), NHI(2) to aerosols, and NHI(3) to
C       ozone.  Currently NHI(2) is ignored, as it appears to refer to
C       elevation above the ground rather than station elevation.
C
C       RHUML is local value of relative humidity (RHUMG(%)/100).
C
C
      REAL HIBEAM, EMM, SUNIN (46), XTES, EXPARG
C       HIBEAM is irradiance at bottom of atmosphere for overhead sun.
C       EMM is "M" function
C       SUNIN is irradiance (photons/cm2/sec/N nm) at the top of the
C       atmosphere at mean solar distance (1 astronomical unit, 1 AU).
C       XTES (loaded as 87.4 below) prevents exponential underflows
C       on machines good to 1.E-38 by hardwiring such results as 0.0.
C       EXPARG and EXPAR2 are temporary variables (arguments to EXP).
C
      REAL ECCEN (13),  LATL, DECLIN (13)
C       ECCEN is the eccentricity correction factor to correct for
C       variation in the earth-sun distance.  These values correspond to
C       days of characteristic declination (the day of the month giving
C       mean monthly irradiance on a horizontal surface):
C
C       RVECSQ is square of radius vector
C
C       LATL is latitude converted to radians for FORTRAN trig funct.
C
C       RADVEC is the radius vector of the earth -- the distance from
C       the center of the earth to the center of the sun expressed in
C       terms of the length of the semimajor axis of the earth's orbit.
C       The 12 values used here are those at the middle day of each
C       month.  DECLIN is the declination of the sun at mid-month --
C       declination is the angular distance of the sun north (+) or
C       south (-) of the celestial equator -- here as radians N and S
C       for use with FORTRAN standard cosine function. Data for RADVEC
C       and DECLIN from List 1966 (Smithsonian Meteorological Tables)
C       page 495 -- ephemeris of the sun, from 1950 nautical almanac.
C
      INTEGER I, I2, II, N2, NFIRST, NLAST
C       I is a general loop counter.
C       I2 points at airmass characteristics.
C       NFIRST and NLAST set the size of the computational loop.
      INTEGER NBLOC
C       III and IV are I/O counters, VRAIL is name length function.
C
      REAL ACCUM1 (52)
C       ACCUM1 accumulates WLAML values for computation of annual ave.
C
      LOGICAL MEAN
C       MEAN signals that mean values are computed.
C
C        INCLUDE 'SOLAR.CMN'
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
C
      DATA CENLAM/280., 282.5, 285., 287.5, 290.,
     1   292.5, 295., 297.5, 300.,
     2   302.5, 305., 307.5, 310., 312.5, 315., 317.5,
     3   320., 323.1, 330., 340.,
     4   350., 360., 370., 380., 390., 400., 410., 420.,
     5   430., 440., 450., 460.,
     6   470., 480., 490., 503.75, 525., 550., 575.,
     7   600., 625., 650., 675.,
     8   706.25, 750., 800./
C
C       ABFRAC is fraction of total aerosol extinction coefficient that
C       is due to light absorption rather than scattering. ABFRAC was
C       computed via regression on wavelength using data in GCS.
      DATA ABFRAC/.152, .151, .150, .149, .148, .147, .147, .146,
     1   .145, .144, .143, .142, .141, .140, .140, .139, .138, .137,
     2   .134, .131, .128, .125, .122, .119, .116, .114, .111, .108,
     3   .106, .103, .101, .0982, .0959, .0936, .0913, .0883, .0839,
     4   .0789, .0743, .0699, .0658, .0619, .0583, .0540, .0486, .0430/
C
C       Air mass data for computing aerosol extinction coefficients as
C       function of wavelength and relative humidity (from Green and
C       Schippnick Copenhagen report)--entry 1 is Rural, entry 2 is
C       Urban, entry #3 is Maritime, and #4 is Tropospheric air mass.
      DATA ELAM0/0.255, 0.288, 0.106, 0.081/
      DATA KAER/1.962, 2.758, 3.393, 2.034/
      DATA PAER/0.345, 0.471, 0.435, 0.328/
      DATA LAMA0/0.439, 0.510, 0.734, 0.412/
      DATA CAPEL/0.122, 0.0827, 1.049, 0.102/
C
C       K3 are (Naperian) absorption coefficients for atmospheric ozone:
      DATA K3/116.8, 92.40, 71.26, 53.80, 39.92, 29.
     1   23, 21.18, 15.23, 10.89
     2   , 7.776, 5.507, 3.902, 2.760, 1.951, 1.378, .
     3   9725, .6862, .4451, .1697
     4   , 4.193E-02, 1.036E-02, 2.557E-03, 6.315E-04,
     5   1.6E-04, 0., 0., 0., 0.
     6   , 3.E-03, 3.E-03, 3.E-03, 7.E-03, 1.15E-02, 1.
     7   5E-02, 2.E-02, 2.99E-02
     8   , 5.30E-02, 8.29E-02, 0.120, 0.124, 9.44E -
     9   02, 6.45E-02, 3.91E-02
     A   , 2.07E-02, 9.21E-03, 9.E-03/
C       RAYFAC is Rayleigh optical depth (molecular scattering in the
C       atmosphere) at sea level pressure of 1013 mb:
      DATA RAYFAC/1.5469, 1.4923, 1.4412, 1.3917, 1.3443, 1.2990, 1.2555
     1   , 1.2138, 1.1739, 1.1355, 1.0988, 1.0635, 1.
     2   0296, .99702, .96574, .93568
     3   , 0.90678, .87248, .80176, .71152, .63362, .
     4   56610, .50734, .45600, .41100
     5   , 0.37142, .33649, .30557, .27812, .25369, .
     6   23187, .21236, .19485, .17912
     7   , 0.16494, .14765, .12516, .10391, .086982, .
     8   073366, .062314, .053266
     9   , 0.045802, .038218, .030051, .023214/
C
C       SUNIN is irradiance (photons/cm2/sec/N nm) at the top of the
C       atmosphere at mean solar distance (1 astronomical unit, 1 AU).
C       For 280-305 nm, computed from equations of Green and Schippnick
C       (conf on uv light in marine ecosystems, Copenhagen, July 1980)
C       For 305-800 nm: Leighton's tabulation (p.7) of Johnson's review.
C       DATA SUNIN /4.51E13, 7.78E13, 8.39E13, 1.18E14, 1.77E14, 1.92E14
C    -,    2.00E14, 1.93E14, 1.72E14, 2.00E14, 2.50E14, 2.78E14, 2.98E14
C    -,    3.11E14, 3.25E14, 3.34E14, 3.42E14, 5.70E14, 1.91E15, 1.90E15
C    -,    2.07E15, 2.10E15, 2.48E15, 2.36E15, 2.20E15, 3.10E15, 4.01E15
C    -,    4.06E15, 3.86E15, 4.50E15, 5.00E15, 5.00E15, 5.14E15, 5.22E15
C    -,    4.91E15, 4.98E15, 5.21E15, 5.40E15, 5.46E15, 5.47E15, 5.38E15
C    -,    5.30E15, 5.20E15, 5.08E15, 4.80E15, 4.55E15/
C       As of 05-SEP-1985, the above are replaced by values computed
C       from Frolich and Wehrli's table of extraterrestrial irradiance
C       given at page 380 of: Iqbal,M. 1983.  An Introduction to Solar
C       Radiation.  Academic Press, New York. 390 pp.
      DATA SUNIN/5.74E13, 1.06E14, 8.09E13, 1.39E14, 1.96E14, 2.14E14
     1   , 2.12E14, 1.97E14, 2.00E14, 2.05E14, 2.10E14, 2.31E14, 2.36E14
     2   , 2.65E14, 2.82E14, 2.88E14, 3.02E14, 4.17E14, 1.61E15, 1.56E15
     3   , 1.70E15, 1.80E15, 2.08E15, 2.10E15, 2.07E15, 2.99E15, 3.51E15
     4   , 3.67E15, 3.48E15, 4.06E15, 4.57E15, 4.75E15, 4.71E15, 4.89E15
     5   , 4.69E15, 4.91E15, 4.97E15, 5.20E15, 5.33E15, 5.30E15, 5.29E15
     6   , 5.18E15, 5.08E15, 5.00E15, 4.81E15, 4.57E15/
C
C       RADVEC is the radius vector of the earth -- the distance from
C       the center of the earth to the center of the sun expressed in
C       terms of the length of the semimajor axis of the earth's orbit.
C       The 12 values used here are those at the middle day of each
C       month.  DECLIN is the declination of the sun at mid-month --
C       declination is the angular distance of the sun north (+) or
C       south (-) of the celestial equator -- here as radians N and S
C       for use with FORTRAN standard cosine function. Data for RADVEC
C       and DECLIN from List 1966 (Smithsonian Meteorological Tables)
C       page 495 -- ephemeris of the sun, from 1950 nautical almanac.
C      DATA RADVEC/0.98365, 0.98756, 0.99452, 1.00333, 1.01095
C     1   , 1.01583, 1.01649, 1.01281, 1.00565, 0.99717, 0.98915, 0.98426
C     2   , 1.00/
C       Declination in degrees (for reference):
C       DATA DECLIN / -21.26, -13.28, -2.44, 9.48, 18.67, 23.27, 21.65
C    -, 14.30, 3.33, -8.22, -18.28, -23.33 / and, for 13 (mean) : 0.0
C       DATA DECLIN / -0.3710, -0.2317, -4.258E-02, 0.1654, 0.3258
C     -,  0.4061, 0.3778, 0.2495, 5.811E-02, -0.1434, -0.3190, -0.4071
C     -,  0.00/
C
C       Declination for days of the month which give mean values for
C       the entire month (from Iqbal 1983:62)
C       In degrees:
C       DATA DECLIN / -20.84, -13.32, -2.40, 9.46, 18.78, 23.04, 21.11
C    -, 13.28, 1.97, -9.84, -19.02, -23.12, 0.00/
      DATA DECLIN/ - 0.3637, - 0.2325, - 4.189E-02, 0.1651, 0.3278
     1   , 0.4021, 0.3684, 0.2318, 3.438E-02, - 0.
     2   1717, - 0.3320, - 0.4035
     3   , 0.00/
C
C       ECCEN is the eccentricity correction factor to correct for
C       variation in the earth-sun distance.  These values correspond to
C       days of characteristic declination (the day of the month giving
C       mean monthly irradiance on a horizontal surface):
      DATA ECCEN/1.0340, 1.0260, 1.0114, 0.9932, 0.9780, 0.9694
     1   , 0.9674, 0.9754, 0.9902, 1.0082, 1.024, 1.0326, 1.00/
C
      DATA XTES/87.4/
C
C
      IF (ILITE .EQ. 1) THEN
         MEAN = .TRUE.
         NFIRST = 1
         NLAST = 12
         NDAT = 13
      ELSE
         MEAN = .FALSE.
         NFIRST = NDAT
         NLAST = NDAT
      END IF
C       Zero accumulator:
      DO 1000 I = 1, 52
         ACCUM1 (I) = 0.0
 1000 CONTINUE
C
C       Start loop:
      DO 1020 NBLOC = NFIRST, NLAST
         DO 1030 LAMBDA = 1, 46
C
C       Computation of irradiance at the water surface:
C       Check for bad value of elevation:
            IF (TIME .GT. 0.0) GO TO 1040
            IF (ELEVG .LE. 5930. .AND. ELEVG .GE. - 100.) GO TO 1040
C       Inappropriate value for elevation--set to sea level, report
C       problem, and continue:
            WRITE (OUT, 6000) ELEVG
 6000   FORMAT(/1X,39HThe elevation given for this location (,1PG12.6,
     1 8H meters)/1X,40Hhas been reset to sea level.  ELEVG must,
     2 16H be in the range/1X, 16H -100 to 5930 m./
     3 1X,58H(The crater lake on  Lincancabur in the Andes, at 5930 m.,,
     4 7H is the/1X,39Hhighest body of water in the world (The,
     5 26H Sciences 24(1):27, 1984).)
            ELEVG = 0.0
 1040       CONTINUE
C       Convert elevation (m) to km:
            YHI = ELEVG/1000.
C       Load pointer (I2) for parameters for computing aerosol
C       properties according to air mass type: (R)ural, (U)rban,
C       (M)aritime, or (T)ropospheric:
            I2 = INT (AIRTYG (NBLOC))
C       If a faulty value was entered for the airmass type, report the
C       error, default to "rural," and continue ...
            IF (I2 .GT. 0) GO TO 1050
            WRITE (OUT, 6010) AIRTYG (NBLOC)
 6010   FORMAT(' Warning: Air mass type of "', A1, '" is not',
     1' appropriate.'/' AIRTY has been defaulted to "R" (Rural).')
            I2 = 1
            AIRTYG (NBLOC) = 1
 1050       CONTINUE
C
C       Check on relative humidity:
            TEMP = RHUMG (NBLOC)
            IF (RHUMG (NBLOC) .LE. 99.0 .AND. RHUMG
     1         (NBLOC) .GE. 0.) GO TO 1060
C       Data base only goes to 99% R.H.:
            IF (RHUMG (NBLOC) .GT. 99.0) RHUMG (NBLOC) = 99.0
C       Relative humidity negative, default to 50% ...
            IF (RHUMG (NBLOC) .LT. 0.0) RHUMG (NBLOC) = 50.0
C       Report change to user:
            WRITE (OUT, 6020) NBLOC, TEMP, RHUMG (NBLOC)
 6020 FORMAT(5X,26H relative humidity (RHUMG(,I2,2H)),/1X,
     1 15Hwas entered as , 1PG11.4,/25H It has been defaulted to,
     2 0PF5.1,2H%.)
 1060       CONTINUE
C       Convert relative humidity (%) to fraction:
            RHUML = RHUMG (NBLOC)/100.
C
C       Compute aerosol extinction coefficients (function of relative
C       humidity and wavelength):
C
            LAMAR = LAMA0 (I2)*(1. + ((CAPEL (I2)*RHUML)/(
     1         (1. - RHUML)**PAER (I2))))
            ELAM0R = ELAM0 (I2)
            IF (RHUML .GT. 0.) THEN
               EXPARG = (1./RHUML)**3
               IF (EXPARG .LE. XTES) ELAM0R = ELAM0 (I2)*
     1            (1. + ((KAER (I2)*(EXP ( - EXPARG)))/(
     2            (1. - RHUML)**PAER (I2))))
            END IF
C
            EXPARG = ((CENLAM (LAMBDA)/1000.) - .3)/LAMAR
            IF (EXPARG .GT. XTES) THEN
               EXTAER (LAMBDA) = 0.0
            ELSE
               EXTAER (LAMBDA) = ELAM0R*EXP ( - EXPARG)
            END IF
C
C       Compute correction factors for ground station elevation:
C
C         a. Compute effect on Rayleigh scattering (air pressure):
            NHI (1) = 1.437/(0.437 + EXP (YHI/6.35))
C         b. Compute ground station elevation effect on aerosol optical
C            depth: (This is GCS "average aerosols")
C            NHI(2) = (.8208/(EXP(YHI/.952)-.145))
C    1                + (4.0202724E-02/(1.+EXP((YHI-16.33)/3.09)))
            NHI (2) = 1.000
C         c. Compute ground station elevation corrector for ozone:
            NHI (3) = (0.13065/(2.35 + EXP (YHI/2.66)))
     1         + (0.970902341/(1.0 + EXP ((YHI - 22.51)/4.92)))
C
C       Compute square of radius vector:
C            RVECSQ = RADVEC (NBLOC)*RADVEC (NBLOC)
C
C       Convert latitude to radians, after data check:
            IF (ABS (LATG) .LE. 66.5) GO TO 1070
C       Latitude has bad value, default to 40 N and go on:
            WRITE (OUT, 6030) LATG
 6030   FORMAT(' System latitude was entered as ',1PG9.3,' degrees.'
     1/' (The method used by EXAMS to compute daylength'
     2/' is not appropriate for polar latitudes.)'
     3/' LATG has been defaulted to +40.0 degrees.')
            LATG = 40.0
 1070       CONTINUE
            LATL = LATG*0.01745
C
C       Compute constants in equation of time:
            SLSD = SIN (LATL)*SIN (DECLIN (NBLOC))
            CLCD = COS (LATL)*COS (DECLIN (NBLOC))
C
C       From date and location, compute time (in sec: 13751 s/radian)
C       from noon to nightfall:
            TNIGHT = 13751.*ACOS ( - (TAN (LATL)*TAN (DECLIN (NBLOC))))
C
C       Compute beam reflection for normal incidence and Brewster's Law:
C          a. normal incidence:
            S1 = REFIND (LAMBDA) - 1.
            S9 = REFIND (LAMBDA) + 1.
            REFRAT = (S1*S1)/(S9*S9)
C          b. Brewster's Law (when (i+j) = 90 degrees):
            S1 = REFIND (LAMBDA)*REFIND (LAMBDA)
            S9 = (S1 - 1.)/(S1 + 1.)
            BREW = S9*S9/2.
C
C       Compute optical depths:
C          Atmosphere--Rayleigh scattering:
            RAYDEP (LAMBDA) = RAYFAC (LAMBDA)*NHI (1)
C          Aerosol optical depth: product of extinction coefficient and
C          "equivalent aerosol layer thickness (km)" (Green & Schippnick
            AERDEP (LAMBDA) = EXTAER (LAMBDA)*NHI (2)*ATURBG (NBLOC)
C          Ozone optical depth:
            OZDEP (LAMBDA) = NHI (3)*K3 (LAMBDA)*OZONEG (NBLOC)
C
C       Compute sunlight at top of atmosphere:
C         HLAM(LAMBDA) = SUNIN(LAMBDA) / RVECSQ
            HLAM (LAMBDA) = SUNIN (LAMBDA)*ECCEN (NBLOC)
C
C       Compute solar beam for overhead sun:
            EXPARG = RAYDEP (LAMBDA) + AERDEP (LAMBDA) + OZDEP (LAMBDA)
            IF (EXPARG .LE. XTES) THEN
               HIBEAM = HLAM (LAMBDA)*EXP ( - EXPARG)
            ELSE
               HIBEAM = 0.0
            END IF
C
C       Compute "M" function for skylight:
            TAU4 = ABFRAC (LAMBDA)*AERDEP (LAMBDA)
            TAU2 = (1. - ABFRAC (LAMBDA))*AERDEP (LAMBDA)
            FA3 = 1./(1. + (.2864*(OZDEP (LAMBDA)**.
     1         8244))*(OZONEG (NBLOC)**
     2         0.4166))
            FA4 = 1./(1. + 2.662*TAU4)
            F1 = 0.8041*(RAYDEP (LAMBDA)**1.389)*FA3
            F2 = 1.437*(TAU2**1.12)
            EMM = FA4*(F1 + F2*(1. + F1))
C       Combine "M" function and vertical beam for transfer to
C       integrator:
            EMMHI = EMM*HIBEAM
C
C       Compute reflectivity function AIREFL:
            FB3 = 1./(1. + (.2797*(OZDEP (LAMBDA)**.8404))*
     1         (OZONEG (NBLOC)**0.1728))
            FB4 = 1./(1. + 3.70*TAU4)
            F1 = 0.4424*(RAYDEP (LAMBDA)**.5626)*FB3
            F2 = 0.100*(TAU2**0.878)
            AIREFL = FB4*(F1 + F2)
C
C       Prepare computational sections of skylight function for
C       transfer to integrator:
            TTEE = 0.0266 - 3.31E-03*YHI
            G3T3 = - OZDEP (LAMBDA)
            GT1GT2 = - (0.5346*RAYDEP (LAMBDA) + 0.6077*TAU2)
            EFF = 1./(1. + 84.37*((OZDEP (LAMBDA) + TAU4)**.6776))
            ONEFF = 1. - EFF
C
C       Call integration routine to compute irradiance delivered between
C       local noon and nightfall (method is 11-point quadrature):
            TIMER = 0.0
            TINCRL = TNIGHT/5.0
            CALL SOLFCT (TIMER, GLOLIT)
            XINT (6) = TNIGHT
            YINT (6) = GLOLIT
            DO 1080 N2 = 1, 5
               TIMER = TIMER + TINCRL
               CALL SOLFCT (TIMER, GLOLIT)
               XINT (6 + N2) = TIMER + TNIGHT
               XINT (6 - N2) = TNIGHT - TIMER
               YINT (6 + N2) = GLOLIT
               YINT (6 - N2) = GLOLIT
 1080       CONTINUE
C
C       Convert results of integration to daily means.
            WLAML (LAMBDA) = SNGL (DINTPT (11, XINT, YINT)/86400.)
C       End of LAMBDA loop:
 1030    CONTINUE
C
C
C       In this code sector the light field at the water surface
C       is adjusted for effects of cloudiness and for the wavelength
C       intervals (e.g. for 750 nm, the initial data is per 10 nm, but
C       the total wavelength interval is 50 nm, thus the 5* correction)
C
C       All the computations incorporate a linear numerical conversion
C       factor with several components; this factor is pre-computed at
C       this point and the local values for overall light intensity are
C       pre-multiplied by the factor.
C
         FACTOR = 3.3034E-16*(1.0 - 0.056*CLOUDG (NDAT))
C
C       FACTOR = 2.3026*1000.*3600.*24.*(1.0 - 0.056*CLOUDG)/6.022E+23
C                ln 10. ml/L  sec/hr                    Avogadro's #
C       Natural log of 10. = 2.3026 (conversion of decadic molar
C       extinction coefficients for chemical to Naperian basis). Factor
C       of 1000.0 corrects volume units (light is /sq.cm., chemical's
C       absorption is /cm, outcome is on a cc. volumetric basis,
C       multiplied by 1000. cc/liter). Division by Avogadro's number
C       (6.022E+23) converts photons to "molar" basis.
C       Cloudiness correction is conventional Buttner method.
         DO 1090 LAMKNT = 1, 35
            WLAML (LAMKNT) = WLAML (LAMKNT)*FACTOR
 1090    CONTINUE
         WLAML (36) = WLAML (36)*1.75*FACTOR
         DO 1100 LAMKNT = 37, 43
            WLAML (LAMKNT) = WLAML (LAMKNT)*2.5*FACTOR
 1100    CONTINUE
         WLAML (44) = WLAML (44)*3.75*FACTOR
         WLAML (45) = WLAML (45)*5.0*FACTOR
         WLAML (46) = WLAML (46)*5.0*FACTOR
C
C
         IF ( .NOT. MEAN) GO TO 1020
C       Load accumulator:
         DO 1110 II = 1, 46
            ACCUM1 (II) = ACCUM1 (II) + WLAML (II)
 1110    CONTINUE
C       End of NBLOC loop:
 1020 CONTINUE
C
C       Compute mean values if appropriate:
      IF ( .NOT. MEAN) GO TO 1120
      DO 1130 I = 1, 46
         WLAML (I) = ACCUM1 (I)/12.
 1130 CONTINUE
 1120 CONTINUE
      RETURN
C       End of Subroutine SOLAR
      END

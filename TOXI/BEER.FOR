      SUBROUTINE BEER         
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:20.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C  Function:
C       Computes average irradiances via Beer-Lambert Law.
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      INCLUDE 'OPTION.CMN'
C                          
      INCLUDE 'ENVIRON.CMN'
C
      INCLUDE 'CONST.INC'
C
      DIMENSION BOTLIT (46), XKE (46), TOPLIT (40, 46)
C
      REAL WATETA (46), PIGETA (46), DOCETA (46), SEDETA (46)
      REAL XARG
C
      INCLUDE 'PARAM.EQU'
C
      INCLUDE 'PHOTOL.CMN'
C
      INCLUDE 'GLOBAL.EQU'
C
C       Absorption coefficients for pure water (data from Smith and
C       Baker 1981 Applied Optics 20:177-184).
      DATA WATETA/.288, .268, .249, .231, .215, .194,
     1   .174, .157, .141, .133
     2   , .126, .119, .105, .0994, .0952, .0903, .0844, .
     3   0793, .0678, .0561, .0463
     4   , .0379, .0300, .0220, .0191, .0171, .0162, .
     5   0153, .0144, .0145, .0145
     6   , .01566, .0156, .0176, .0196, .0295, .0492, .
     7   0638, .0940, .244, .314, .349
     8   , .440, .768, 2.47, 2.07/
C
C       Specific absorption coefficients for chlorophyll pigments:
C       (/meter /(mg pigment/liter)), data from Smith and Baker (1978)
C       and (for first 20 values) from Baker and Smith 1982.
      DATA PIGETA/145., 138., 132., 126., 120., 115.,
     1   109., 106., 101., 95.,
     2   90., 85., 80., 78., 75., 72., 70., 68., 64., 59.,
     3   55., 55., 51., 46., 42., 41., 39., 38., 35., 32.,
     4   31., 28., 26., 24., 22.,
     5   19., 14., 10., 8., 6., 5., 8., 13., 3., 2., 0./
C
C       Specific absorption coefficients for "dissolved" organic carbon:
C       (Data of R.G. Zepp for Aucilla River water), (/m/(mg/L))
      DATA DOCETA/7.90, 7.65, 7.41, 7.18, 6.95, 6.73, 6.52, 6.30, 6.12,
     1   5.94, 5.76, 5.57, 5.39, 5.22, 5.06, 4.90, 4.74,
     2   4.56, 4.17, 3.64, 3.15,
     3   2.74, 2.34, 2.00, 1.64, 1.39, 1.19, 1.02, .870, .
     4   753, .654, .573, .504,
     5   .444, .396, .357, .282, .228, .188, .158, 6*0./
C
C       Specific absorption coefficients for suspended sediments
C       (/m/(mg/L)): Datum from Miller and Zepp 1979 (Water Research) -
C       average of 5 natural sediments, determined at 331 nm - assumed
C       flat spectral response.
      DATA SEDETA/46*0.34/
C
C       Datum for forcing exponential underflows to non-error zero.
C       Exponential light extinction produces underflows when the
C       exponent (that is, the diffuse attenuation coefficient times the
C       depth) is greater than 87 (on machines with range to 1.E-38)
C       In such cases, the light at the bottom of the segment is simply
C       set to zero.
      DATA XTES/87./
C
C       I is the number of the segment being processed.
C       ION is counter on ionic species
C
C
C     INITIALIZE VARIABLES
C
      DO 1010 LAMKNT = 1, 46
         AVELIT (LAMKNT) = 0.0
         BOTLIT (LAMKNT) = 0.0
 1010 CONTINUE
C
      IF (ITYPE (ISEG) .GE. 3 ) RETURN
C
C     BRANCH DEPENDING ON PHOTOLYSIS OPTION
C
      GO TO ( 1020, 1030), IPHOTO
C
C
C
C     OPTION 1: PHOTOLYSIS RATE CALCULATED FROM MOLAR ABSORPTIVITIES
C               AND QUANTUM YIELD OF THE CHEMICAL
C
C
C       A) Compute total diffuse attenuation coefficients:
C
C
 1020 CONTINUE
      DO 1040 LAMKNT = 1, 46
         XKE (LAMKNT) = SDFAC*(WATETA (LAMKNT)
     1      + PIGETA (LAMKNT)*CHL
     2      + DOCETA (LAMKNT)*SDOC
     3      + SEDETA (LAMKNT)*SOLIDS)
 1040 CONTINUE
C
C       B) Compute average and bottom light intensities for the segment.
C
C           If surface segment then set TOPLIT = surface light (WLAML)
      IF (ITYPE (ISEG) .EQ. 1) THEN
         DO 1050 LAMKNT = 1, 46
            TOPLIT (ISEG, LAMKNT) = WLAML (LAMKNT)
C                If there is ice cover, load zero:
            IF (TMP .LE. 0.0) TOPLIT (ISEG, LAMKNT) = 0.0
 1050    CONTINUE
      END IF
C
C           Compute light at bottom of segment and average light level:
      DO 1060 LAMKNT = 1, 46
         XXX = XKE (LAMKNT)*SDEPTH
         IF (XXX .GT. XTES) THEN
            BOTLIT (LAMKNT) = 0.0
         ELSE
            BOTLIT (LAMKNT) = TOPLIT (ISEG, LAMKNT)*EXP ( - XXX)
         END IF
         AVELIT (LAMKNT) = (TOPLIT (ISEG, LAMKNT) - BOTLIT (LAMKNT))
     1      /XXX
 1060 CONTINUE
C
C     C) Set toplight for segment below current segment equal to light
C        at bottom of this segment.
C
      IF (IBOTSG (ISEG) .EQ. 0) GO TO 1070
      DO 1080 LAMKNT = 1, 46
         TOPLIT (IBOTSG (ISEG), LAMKNT) = BOTLIT (LAMKNT)
 1080 CONTINUE
 1070 CONTINUE
C
C
      GO TO 1090
C
C
C     OPTIONS 2 : SURFACE 1ST-ORDER PHOTOLYSIS RATES INPUTTED BY USER
C
C       loop for molecular and 4 ionic species
 1030 CONTINUE
      DO 1100 ION = 1, 5
C
C       A) Compute attenuation coefficient for this segment
C          at the wavelength of maximum light absorption for this
C          compound.  The index of this wavelength is given by ILM(ION).
C
CCSC
C         IF (XKE2 (ISEG) .EQ. 0.0) THEN
         XARG=ABS(XKE2(ISEG))
         IF (XARG .LT. R0MIN) THEN
            CMPET = SEDETA (ILM (ION))*SOLIDS + WATETA (ILM (ION)) +
     1         PIGETA (ILM (ION))*CHL + DOCETA (ILM (ION))*SDOC
         ELSE
            CMPET = XKE2 (ISEG)
         END IF
C
C       B) If this is a surface segment (ITYPE(ISEG) = 1):
C           Set nominal level of surface light to 1.0, unless temperatur
C           zero or less (ice cover), in which case set light to zero:
         IF (ITYPE (ISEG) .EQ. 1) THEN
            XIZERO = 1.0
            IF (TMP .LE. 0.0) XIZERO = 0.0
         ELSE
C           This is a subsurface segment.  Set surface light to the
C           light at the bottom of the overlying segment.
            XIZERO = TOPLIT (ISEG, ION)
         END IF
C
C       C) Compute average light level (as fraction of incident light)
C          from the composite zenith light extinction coefficient:
C
         XXX = CMPET*SDEPTH*SDFAC
         IF (XXX .GT. XTES) THEN
            BOTLIT (ION) = 0.0
         ELSE
            BOTLIT (ION) = XIZERO*EXP ( - XXX)
         END IF
         AVELIT (ION) = (XIZERO - BOTLIT (ION))/XXX
C
C       D) Set toplight for segment below current segment equal to light
C          at bottom of this segment.
C
         IF (IBOTSG (ISEG) .EQ. 0) GO TO 1110
         TOPLIT (IBOTSG (ISEG), ION) = BOTLIT (ION)
 1110    CONTINUE
C
 1100 CONTINUE
C
 1090 CONTINUE
      RETURN
C       End of Subroutine BEER
      END

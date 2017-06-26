      SUBROUTINE ENV
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:26.
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
      EQUIVALENCE (TIME, T)
C
      INCLUDE 'PARAM.EQU'
C
      INCLUDE 'PHOTOL.CMN'
C
      INCLUDE 'GLOBAL.EQU'
C
      INCLUDE 'SOLID.EQU'
C
      INCLUDE 'CONST.INC'
C
      REAL XARG
C
C       SEGMENT PHYSICAL-CHEMICAL CHARACTERISTICS
C
C         Temperature
C
      IFUN = TMPFN (ISEG)
      IF (IFUN .GT. 0 .AND. IFUN .LT. 4) THEN
          TMP = TEMP (ISEG)*TEMPN (IFUN)
       ELSE
          TMP = TEMP (ISEG)
      END IF
C
         TK = TMP + 273.15
C
C         pH and resulting hydrogen ion & hydroxide ion concentrations
C
      IF (ITYPE (ISEG) .LE. 2) PHN = PHNW
      IF (ITYPE (ISEG) .GE. 3) PHN = PHNS
      PHS = PH (ISEG)*PHN
      H = 1./(10**PHS)
      OH = 1.E-14/H
C
C         Porosity & Solids Concentration
C
      SOLIDS = 0.0
      DO 1000 J = 2, 4
         SLDJ = C (J, ISEG) * 1.0E-06
         SOLIDS = SOLIDS + SLDJ
 1000 CONTINUE
        PORE = FRW (ISEG)
C
C         Dissolved organic carbon, oxidant concentration &
C         fraction organic carbon of the solids
C
      SDOC = DOC (ISEG)
      SOXRAD = OXRAD (ISEG)
      DO 1010 J = 1, 3
         SFOC (J) = FOC (ISEG, J)
 1010 CONTINUE
C
C         Bacterial Concentrations
C
      IF (ITYPE (ISEG) .LE. 2) SBAC= BACNW * BAC(ISEG)
      IF (ITYPE (ISEG) .GE. 3) SBAC= BACNS * BAC(ISEG)
C
C         Phytoplankton chlorophyll Concentration
C
      CHL = CHPHL (ISEG)*CHLN
C
C         Velocity of the water, Reaeration rate, Depth & Wind velocity
C
      IFUN = VELFN (ISEG)
      VEL = VELOCG (ISEG)
      IF (IFUN .GT. 0 .AND. IFUN .LE. 4) VEL = VEL + VELN (IFUN)
      SREAER = REAER (ISEG)*REARN
      SDEPTH = DEPTHG (ISEG)
      SWIND = WVEL (ISEG)*WINDN
C
C         Ratio of optical path to vertical depth
C
C        The distribution factor (DFACG) is the optical path in the
C        compartment as a ratio to the depth.  It is difficult to
C        compute, but a probable best value is 1.19 (Hutchinson, Treatis
C        Limnology).  However, in the presence of a large concentration
C        of scattering particles, it may approach or reach 2.0.  In orde
C        to ensure that an improper value is not loaded and used in
C        computations, the input DFACG is checked and set to 1.19 if the
C        input is invalid.
CCSC
      XARG = ABS(T)
      IF (XARG.LT.R0MIN.AND.DFACG.LT.1..OR.DFACG.GT.2.) DFACG = 1.19
C      IF (T.EQ.0.0.AND.DFACG.LT.1..OR.DFACG.GT.2.) DFACG = 1.19
C
      SDFAC = DFACG
C
C         Segment type & segment below
C
C
      RETURN
      END

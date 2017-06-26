       SUBROUTINE WASPB
C
C                            Version 1.0
C                             Of WASP4
C                      Toxic Chemical Kinetics
C
C                           Written By:
C
C  John P. Connolly        Robert B. Ambrose   Tim A. Wool
C  Env. Eng. & Science     US E.P.A.           AScI Corporation
C  Manhattan College       Env. Research Lab.  Env. Research Lab.
C  Riverdale, NY  10471    Athens, GA  30613   Athens, GA  30613
C
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:31:59.
C---------------------------------------------------------------------
C
C      This WASPB contains the kinetics describing the fate of a toxic
C      chemical in an aquatic environment.  It models total
C      chemical and up to 3 classes of solids.  The distribution
C      of chemical between the dissolved and various particulate
C      phases is computed from the total chemical assuming
C      instantaneous equilibrium linear partitioning.
C
C      Total chemical includes dissolved chemical, chemical adsorbed
C      to dissolved organic carbon and chemical adsorbed to the solids
C      includes in the model, i.e.,
C
C      Total Chemical = Dissolved + Reversibly Sorbed to Solids
C                       + Reversibly sorbed to Dissoved Oranic Carbon
C
C      Up to 3 chemicals can be modelled.  These chemicals can be
C      independent of each other or the latter two can be products of
C      chemical 1 or of each other.
C
C
C      SYSTEM 1 is total chemical
C      SYSTEMS 2 through 4 are SOLIDS
C      SYSTEMS 5 through 6 are Chemical 2 and Chemical 3

C
C----------------------------------------------------------------------
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
      INCLUDE 'PARAM.EQU'
C
      INCLUDE 'GLOBAL.EQU'
C
C***********************************************************************
C
      EQUIVALENCE (TIME, T)
C
      CHARACTER*1 PROP(4)
      REAL LATG
      DIMENSION DAYMO (12)
      DATA DAYMO/31., 28., 31., 30., 31., 30., 31.,
     1   31., 30., 31., 30., 31./
      DATA PROP /'|','/','-','\'/
C
      XKFLUX = 0.0
C
      ILITE = INT (XLITE)
      IF (INITB .EQ. 0) CALL TOXINT (ILITE)
      IWB = INT (WTYPE)
      CALL SETRA (RATE, 3, 6, 0.0)
C
C     Evaluate straight line functions
C
      IF (TIME .GE. NTF) CALL WASP8 (MFUNC, BFUNC,
     1   NFUNC, 4,itIME, NTF, 73)
C
      TEMPN (1) = MFUNC (1)*(TIME - NFUNT (1)) + BFUNC (1)
      TEMPN (2) = MFUNC (2)*(TIME - NFUNT (2)) + BFUNC (2)
      TEMPN (3) = MFUNC (3)*(TIME - NFUNT (3)) + BFUNC (3)
      TEMPN (4) = MFUNC (4)*(TIME - NFUNT (4)) + BFUNC (4)
      VELN (1)  = MFUNC (5)*(TIME - NFUNT (5)) + BFUNC (5)
      VELN (2)  = MFUNC (6)*(TIME - NFUNT (6)) + BFUNC (6)
      VELN (3)  = MFUNC (7)*(TIME - NFUNT (7)) + BFUNC (7)
      VELN (4)  = MFUNC (8)*(TIME - NFUNT (8)) + BFUNC (8)
      WINDN     = MFUNC (9)*(TIME - NFUNT (9)) + BFUNC (9)
      PHNW      = MFUNC (10)*(TIME - NFUNT (10)) + BFUNC (10)
      PHNS      = MFUNC (11)*(TIME - NFUNT (12)) + BFUNC (11)
      REARN     = MFUNC (12)*(TIME - NFUNT (12)) + BFUNC (12)
      AIRTMPN   = MFUNC (13)*(TIME - NFUNT (13)) + BFUNC (13)
      CHLN      = MFUNC (14)*(TIME - NFUNT (14)) + BFUNC (14)
      PHTON     = MFUNC (15)*(TIME - NFUNT (15)) + BFUNC (15)
      BACNW     = MFUNC (16)*(TIME - NFUNT (16)) + BFUNC (16)
      BACNS     = MFUNC (17)*(TIME - NFUNT (17)) + BFUNC (17)
C
C
C ==============  Check to see What Light Regime is Used ==============
C
      IF (ILITE .EQ. 3) THEN
C
C        COMPUTE DAY OF THE YEAR (T9)
C
         RTIM = TIME + T0
         N9 = RTIM/365.
         T9 = RTIM - N9*365.
C
C        CHECK FOR NEXT MONTH
C
         IF (T9 .LT. DT .OR. T9 .GE. DAYR) THEN
            IF (NDAT .LT. 12) THEN
               NDAT = NDAT + 1
               DAYR = DAYR + DAYMO (NDAT)
            ELSE
               NDAT = 1
               DAYR = DAYMO (1)
            END IF
C
C        CALL SOLAR TO COMPUTE LIGHT INTENSITY
C
            CALL SOLAR (ILITE)
         END IF
      END IF
C
C
C     COMPUTE CURRENT AIR TEMPERATURE IN DEGREES C
C
      ATMP = AIRTMP*AIRTMPN
C
      CALL COLOR ('yellow','BLACK')
C
      DO 1000 ISEG = 1, NOSEG
C
         IF (IPROP .GT. 3)IPROP=0
         IPROP=IPROP + 1
         if (mflag .lt.2)CALL OUTSXY (72,5,PROP(IPROP))
         CALL ENV
C
         CALL CHEM1
C
C         COMPUTE MASS LOST DUE TO DEGRADATION
C
C         IF (IDUMP (1, 1) .EQ. 0 .AND. IDUMP (1, 2) .NE. 0) THEN
         IF (JMASS .NE. 0)THEN
            XKFLUX = (XKFLUX + CD (JMASS, ISEG)/1000.)
         END IF
C
         CALL SOLID
C
C
         IF (NOSYS .GE. 5) CALL CHEM2
         IF (NOSYS .EQ. 6) CALL CHEM3
C
         CALL RXPROD
C
         IF (IDISK .EQ. 1) CALL TOXIDU
C
 1000 CONTINUE
C
      RETURN
      END

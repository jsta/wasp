      SUBROUTINE WAS13
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:05.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C            INTERMEDIATE PRINT, MASS CHECK
C
      INCLUDE 'WASP.CMN'
C
      REAL*8 EXMAS
      INTEGER IREAD, IREAD2
      SAVE IREAD
      DATA IREAD, IREAD2/0, 0/
C-----------------------------------------------------------------------
C
      IDISK = 1
      IREC = IREC + 1
      IF (IREC .EQ. 1) THEN
C
C=======================================================================
C                     Initialize Mass and Tables
C=======================================================================
C
         IPRNT = 1
C        TPRINT = PRINT(IPRNT)
C        TPRINT = 0.0
         TPRINT = TZERO
         IF (JMASS .GT. 0) THEN
            AOMASS = 0.
            AIMASS = 0.
            ROMASS = 0.
            RIMASS = 0.
            XLMASS = 0.
            XKMASS = 0.
            XMASS0 = 0.
            XBMASS = 0.
            DO 1000 J = 1, NOSEG
               XMASS0 = XMASS0 + C (JMASS, J)*BVOL (J)/1000.
 1000       CONTINUE
            WRITE (IMASS, 6000) JMASS, XMASS0
 6000       FORMAT(1X//48X,'Mass Balance for Constituent',I3/
     1      48X,'Initial Mass is',E13.4,' KG'/
     2      11X,'   ***** Accumulated Mass In (KG) *****',
     3      3X,11(1H*),' Accumulated Mass Out (KG) ',11(1H*),
     4      '   Resident       Excess'/5X,
     5      'Time     Advection    Dispersion    Loading     Advection',
     6      '    Dispersion     Buried      Kinetic     Mass (KG)',
     7      '    Mass (KG)'/,132('~'))
         END IF
         IF (MFLAG .LT. 2)CALL WSCREEN
         IF (MFLAG .LT. 2)CALL WLABELS
      END IF
C=======================================================================
C                   Add to Intermediate Table
C=======================================================================
C
      IF (TIME .GE. TPRNT (IPRNT)) IPRNT = IPRNT + 1
      TPRINT = TPRINT + PRINT (IPRNT)
      PT = TIME + 0.000001*TIME
C
C=======================================================================
C          Perform Mass Check on Designated Constituent
C          (NOTE: XKMASS and XBMASS Calculated in user WASPB)
C=======================================================================
C
      IF (JMASS .GT. 0) THEN
         XMASS = 0.
         DO 1010 J = 1, NOSEG
            XMASS = XMASS + C (JMASS, J)*BVOL (J)/1000.
 1010    CONTINUE
         EXMAS = XMASS0 + AIMASS + RIMASS + XLMASS
     1      - (abs(AOMASS) + abs(ROMASS) + abs(XBMASS) + abs(XKMASS))
     2      - XMASS
         WRITE (IMASS, 6010) PT, AIMASS, RIMASS, XLMASS, AOMASS, ROMASS,
     1      XBMASS, XKMASS, XMASS, EXMAS
 6010    FORMAT(1X,F10.3,9E13.4)
      END IF
C
C=======================================================================
C                       Check Stability
C=======================================================================
C
      IF (TIME .LE. TZERO) RETURN
      DO 1020 ISYS = 1, NOSYS
         DO 1030 ISEG = 1, NOSEG
            IF (C (ISYS, ISEG) .GT. CMAX (ISYS)) GO TO 1040
 1030    CONTINUE
 1020 CONTINUE
C
C=======================================================================
C             Calling the TRN File to Store Flow Related Data
C=======================================================================
C
      IF (TFLAG .EQ. 0)CALL TRNOUT
C
C
C=======================================================================
C                 Write Inter File and Screen
C=======================================================================
      IF (MFLAG .LE. 1) THEN
          IF (WQ.AND. MFLAG .LT.2)CALL WUPDATE
      END IF
C
      RETURN
C
C=======================================================================
C                   Unstable: Terminate Simulation
C=======================================================================
C
 1040 CONTINUE
C
      CALL GETMES (21, 1)
C
      CKMAX = CMAX (ISYS)
      CALL WMESS (2, CKMAX)
      CALL WERR(25,1,0)
      CALL WEXIT('Simulation Aborted due to instability',1)
      ISKPLT = 1
      TEND = 0.0
      RETURN
      END

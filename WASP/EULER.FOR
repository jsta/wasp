      SUBROUTINE EULER
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:41.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
C
      REAL*4 XARG
      REAL*8 MASS, MDER, VOLOLD, VOLNEW
      CHARACTER*1 PROP(4)
C
      COMMON /W19/ ISKPLT
C
      INTEGER HDEPVEL
      COMMON /DYNHYD/ ICYCLE, itcyc, ITOTCYC, HDEPVEL, NSEG, NFLOW
      EQUIVALENCE (TIME, T)
C
      DATA PROP /'|','/','-','\'/
C======================================================================
C    Controls computation and output, Version 2:mass derivatives,
C    corrected for time variable volumes: in this version,
C    mass is constituent mass,  mder is the mass derivative,
C    BVOl(ISEG) is the OLD VOLUME, and MVOL(ISEG) is the NEW VOLUME
C======================================================================
C
      ITRAK = 1
      T = 0.0
      NDEVL = 0.0
      DTS2 = DT
      TMARKS = TZERO + TDINTS
CCSC
C      IF (TZERO .EQ. 0.) GO TO 1000
      XARG=ABS(TZERO)
      IF (XARG .LT. R0MIN) GO TO 1000
C
      CALL TINIT
      TIME = TZERO
      TNEXT = TZERO
 1000 CONTINUE
C
      DAY = TZERO
      NEWDAY = 1
      CALL WAS13
C======================================================================
C                      Evaluate Derivatives
C======================================================================
C
      CALL DERIV
C
C======================================================================
C                         Update Volumes
C======================================================================
C
      IDISK = 0
C
C======================================================================
C              Check for FATAL Input ERROR Condition
C======================================================================
C
      IF (INPERR .GT. 0) CALL WERR (10, I, J)
C
C======================================================================
C                  Integrate using Euler Scheme
C         for Mass Derivatives, then Find NEW Concentrations
C======================================================================
C
 1020 CONTINUE
      CALL COLOR ('BLUE','BLACK')
      DO 1030 ISYS = 1, NOSYS
         IF (SYSBY (ISYS) .GT. 0) GO TO 1030
         DO 1040 ISEG = 1, NOSEG
            IF (IPROP .GT. 3)IPROP=0
            IPROP=IPROP + 1
            IF (MFLAG .LT. 2)CALL OUTSXY (72,5,PROP(IPROP))
            VOLOLD = BVOL (ISEG)
            VOLNEW = MVOL (ISEG)
            MASS = C (ISYS, ISEG)*VOLOLD
            MDER = CD (ISYS, ISEG)
            C (ISYS, ISEG) = (MASS + DT*MDER)/VOLNEW
            CD (ISYS, ISEG) = 0.0
            IF (NEGSLN .EQ. 1) GO TO 1050
C
C======================================================================
C           Protect Against Underflows or Negative Solutions
C======================================================================
C
            IF (C (ISYS, ISEG) .LT. 1.0E-25) C (ISYS,
     1         ISEG) = 1.0E-25
 1050       CONTINUE
 1040    CONTINUE
 1030 CONTINUE
C
C======================================================================
C                         Update Volumes
C======================================================================
C
      IF (IVOPT .LT. 10)THEN
         DO 1060 ISEG = 1, NOSEG
            BVOL (ISEG) = MVOL (ISEG)
 1060    CONTINUE
      ENDIF
C
C======================================================================
C                         Increment Time
C======================================================================
      T = T + DT
C
      LDAY = DAY
      NDAY = T
      NEWDAY = NDAY - LDAY
      IF (NEWDAY .GE. 1) DAY = FLOAT (NDAY)
C
      IF (TIME .GE. TPRINT) CALL WAS13
C        EVALUATE DERIVATIVES
C
      IF (T .LT. TEND - .00001) CALL DERIV
C
C
      IF (IDISK .NE. 1) GO TO 1070
      IDISK = 0
 1070 CONTINUE
C
C======================================================================
C            Check to See if Simulation Finished
C======================================================================
C
      IF (T .LT. TEND - .00001) GO TO 1020
C
C          Check for New Time Step, New Simulation Segment
C
      ITRAK = ITRAK + 1
      IF (TEND .GT. 0.) CALL WAS14 (ITRAK)
      DTS2 = DT/2.0
      IF (ITRAK .GT. 0) GO TO 1020
C
C======================================================================
C                 Simulation Finished ... WRAP UP
C======================================================================
      IREC = IREC + 1
      T = T + DTS2
      IDISK = 1
      DO 1090 ISEG = 1, NOSEG
C         DVOL (IREC, ISEG) = BVOL (ISEG)
 1090 CONTINUE
C
      CALL WASPB
C
C======================================================================
C        Storing Final Conditions in Simulation Restart File
C======================================================================
C
      IF (ICFL .GE. 1) THEN
         OPEN (UNIT = RESTRT, STATUS = 'UNKNOWN', ACCESS = 'SEQUENTIAL',
     1      FILE = 'RESTART.OUT')
         WRITE (RESTRT, 6000) (ISEG, IBOTSG (ISEG),
     1       ITYPE (ISEG), MVOL (ISEG),
     2      VMULT (ISEG), VEXP (ISEG), DMULT (ISEG), DXP (ISEG),
     3      ISEG = 1, NOSEG)
C
         DO 1100 ISYS = 1, NOSYS
            DENS = DSED (ISYS)/1.E+06
            WRITE (RESTRT, 6010) CHEML (ISYS), IFIELD (ISYS), DENS,
     1                           cmax(isys)
            WRITE (RESTRT, 6020) (C (ISYS, ISEG), F (2, ISYS, ISEG),
     1         ISEG = 1, NOSEG)
 6000   FORMAT(3I10,E10.3,4F10.4)
 6010   FORMAT(A40,I5,F5.2,e10.3)
 6020   FORMAT(3(5X,E10.3,E10.3))
 1100    CONTINUE
      END IF
      CALL WAS13
      IF (MFLAG .LT. 2 .AND. .NOT. WISP)
     1 CALL PROMPT('SIMULATION COMPLETED--PRESS RETURN TO EXIT',0)
C      CALL WEXIT('COMPLETED',1)
      RETURN
      END

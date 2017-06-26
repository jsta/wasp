      SUBROUTINE WASP9
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:15.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      CHARACTER*5 ANAME (SG)
C
C***********************************************************************
C          J:INITIAL CONDITIONS BY SYSTEM
C***********************************************************************
C
      WRITE (OUT, 6000)
C
C***********************************************************************
C              READ INITIAL CONDITIONS FOR EACH SYSTEM
C***********************************************************************
C
      DO 1000 ISYS = 1, NOSYS
         READ (ICRD, 5000) CHEML (ISYS), IFIELD (ISYS), DSED (ISYS),
     1      CMAX (ISYS)
         IF (DSED (ISYS) .LT. 1.) DSED (ISYS) = 1.0
     1
C***********************************************************************
C     IFIELD POINTS TO FIELD THAT TRANSPORTS SYSTEM ISYS.
C     CHECK IF IFIELD IS GREATER THAN THE MAXIMUM ALLOWED:
C***********************************************************************
C
         IF (IFIELD (ISYS) .GT. MNF) THEN
            WRITE (OUT, 6010) ISYS, MNF
            CALL WERR(23,1,0)
            CALL WEXIT('Dimension Error See the Output File',1)
         END IF
C**********************************************************************
C     Check to see if user neglected to enter and IFIELD for each
C     System.  If not set default to Water IFIELD=1 and warn user.
C**********************************************************************
C
C###########  Prompt User to Set Ifield for System  Default = 3
         IF (IFIELD (ISYS) .LT. 3 .OR. IFIELD (ISYS) .GT. 5) THEN
            IFIELD(ISYS)=3
         END IF
C
C        INITIALIZE F -ARRAY AS SPECIFIED BY IFIELD:
C
         DO 1010 ISEG = 1, NOSEG
            F (IFIELD (ISYS), ISYS, ISEG) = 1.0
 1010    CONTINUE
C
C        READ IN INITIAL CONCENTRATIONS AND DISSOLVED FRACTIONS:
C
         READ (ICRD, 5010, ERR = 1020) (ANAME (I), C
     1      (ISYS, I), F (2, ISYS, I),
     2      I = 1, NOSEG)
C
C       If Initial Conditions are close to Zero set to 1.0e-24
C
         DO 1030 I = 1, NOSEG
            IF (C (ISYS, I) .LT. 1.00E-24) C (ISYS, I) = 1.00E-24
 1030    CONTINUE
C
         WRITE (OUT, 6020) ISYS, IFIELD (ISYS), DSED (ISYS)
         WRITE (OUT, 6030) (ANAME (I), C (ISYS, I), F (2,
     1      ISYS, I), I = 1, NOSEG)
C
C***********************************************************************
C              Compute Initial Particulate Fractions
C***********************************************************************
         DO 1040 ISEG = 1, NOSEG
            F (IFIELD (ISYS), ISYS, ISEG) = 1.0 - F (2, ISYS, ISEG)
 1040    CONTINUE
C
C***********************************************************************
C           Convert Densities From g/cm3 to g/m3:
C***********************************************************************
C
         DSED (ISYS) = DSED (ISYS)*1.E+06
 1000 CONTINUE
C
C              COMPUTE INITIAL SEGMENT POROSITIES:
C
      DO 1050 ISEG = 1, NOSEG
         SUM = 0.0
         DO 1060 I = 1, NOSYS
            IF (DSED (I) .GT. 0.) SUM = SUM + C (I, ISEG)/DSED (I)
 1060    CONTINUE
         FRW (ISEG) = 1.0 - SUM
 1050 CONTINUE
C
C              PRINT STABILITY CRITERIA
C
      WRITE (OUT, 6040) (I, CMAX (I), I = 1, NOSYS)
      RETURN
 1020 CONTINUE
      CALL WERR(24,1,0)
      CALL WEXIT('  Error Reading Card Group J  ',1)
C
C***********************************************************************
C          FORMAT STATEMENTS
C***********************************************************************
C
 6000 FORMAT(///28X,'Initial Conditions'/,28X,18('~'),/)
 5000 FORMAT(A40,I5,F5.0,F10.0)
 5010 FORMAT(3(A5,2F10.0))
 6020 FORMAT(//20X,'Initial Conditions for System',I2,
     1        /4X,'Particulate Fraction of System is Transported',
     2       ' by Solids Field',I5,
     3        /24X,'Density of System = ',E10.3/)
 6030 FORMAT(3X,A5,2E11.4,2X,A5,2E11.4,2X,A5,2E11.4)
 6040 FORMAT(///15X,'Stability Criteria for Numerical Integration'//
     1 20X,'System   Maximum Concentration'/ (20X,I3,5X,F15.2))
 6010 FORMAT(/10X,'The Field Number for System ',I5,2X,'is Greater',
     1      ' Than',/10X,'the Maximum of ',I5,2X,
     2      '** SIMULATION ABORTED **')
      END

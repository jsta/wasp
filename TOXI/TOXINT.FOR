      SUBROUTINE TOXINT (ILITE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:24.
C
C  Correction History:
C                - Added routine to assign absorption spectrum
C                - Added Partitioning defaults if none
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      INCLUDE 'ENVIRON.CMN'
C
      INCLUDE 'PHOTOL.CMN'
C
      INCLUDE 'KNETIC.CMN'
C
      INCLUDE 'OPTION.CMN'
C
      INCLUDE 'PARAM.EQU'
C
      INCLUDE 'SOLID.EQU'
      INCLUDE 'GLOBAL.EQU'
C
      INCLUDE 'CONST.INC'
C
C      INCLUDE 'CHMLOC.CMN'
C
      DIMENSION DAYMO (12)
      REAL XARG
      REAL LATG
      DATA DAYMO/31., 28., 31., 30., 31., 30., 31.,
     1   31., 30., 31., 30., 31./
C
      INITB = 1
      IF (NOSYS .LT. 6) SYSBY (6) = 1
      IF (NOSYS .LT. 5) SYSBY (5) = 1
      IF (NOSYS .LT. 4) SYSBY (4) = 1
      IF (NOSYS .EQ. 6) NCHEM = 3
      IF (NOSYS .EQ. 5) NCHEM = 2
      IF (NOSYS .LE. 4) NCHEM = 1
C
CCSC
      XARG = ABS(CONST(102))
C      IF (CONST (102) .EQ. 0.) CONST (102) = ALOG10(0.6)
      IF (XARG .LT. R0MIN) CONST (102) = ALOG10(0.6)
CCSC
      XARG = ABS(CONST(103))
C      IF (CONST (103) .EQ. 0.) CONST (103) = 1.
      IF (XARG .LT. R0MIN) CONST (103) = 1.
CCSC
      XARG = ABS(CONST(702))
C      IF (CONST (702) .EQ. 0.) CONST (702) = ALOG10(0.6)
      IF (XARG .LT. R0MIN) CONST (702) = ALOG10(0.6)
CCSC
      XARG = ABS(CONST (703))
C      IF (CONST (703) .EQ. 0.) CONST (703) = 1.
      IF (XARG .LT. R0MIN) CONST (703) = 1.
CCSC
      XARG = ABS(CONST (1302))
C      IF (CONST (1302) .EQ. 0.) CONST (1302) = ALOG10(0.6)
      IF (XARG .LT. R0MIN) CONST (1302) = ALOG10(0.6)
CCSC
      XARG = ABS(CONST (1303))
C      IF (CONST (1303) .EQ. 0.) CONST (1303) = 1.
      IF (XARG .LT. R0MIN) CONST (1303) = 1.
      IF (NCHEM .GE. 1) CALL CHM1INT (ILITE)
      IF (NCHEM .GE. 2) CALL CHM2INT (ILITE)
      IF (NCHEM .GE. 3) CALL CHM3INT (ILITE)

C
C     SET KOW AND MOLECULAR WEIGHT FOR DUMP FILE
C
C
CCSC
      XARG = ABS(CONST(81))
C      IF(CONST(81) .EQ. 0. .AND. NOSYS .GE. 1)THEN
      IF (XARG .LT. R0MIN .AND. NOSYS .GE. 1)THEN
         CONST(81) = 1.0
         WRITE(OUT,2222)
 2222    FORMAT(15X,'molecular wt. for chemical 1 was 0, set to 1 ')
      ENDIF
CCSC
      XARG = ABS(CONST(681))
C      IF(CONST(681) .EQ. 0..AND. NOSYS .GE. 5)THEN
      IF (XARG .LT. R0MIN .AND. NOSYS .GE. 5)THEN
         CONST(681) = 1.0
         WRITE(OUT,2223)
 2223    FORMAT(15X,'Molecular Wt. for Chemical 2 was 0, set to 1 ')
      ENDIF
CCSC
      XARG = ABS(CONST(1281))
C      IF(CONST(1281) .EQ. 0..AND. NOSYS .GE. 6)THEN
      IF (XARG .LT. R0MIN .AND. NOSYS .GE. 6)THEN
         CONST(1281) = 1.0
         WRITE(OUT,2224)
 2224    FORMAT(15X,'Molecular Wt. for Chemical 2 was 0, set to 1 ')
      ENDIF
C
      TMOLWT (1) = CONST (81)
      TMOLWT (2) = CONST (681)
      TMOLWT (3) = CONST (1281)
      TKOW (1) = CONST (84)
      TKOW (2) = CONST (684)
      TKOW (3) = CONST (1284)
C
C ------------- Default Constants to Simple Model ----------------
C
C Default the Solids-Dependent Partitioning Parameter to a Large Number
C for no Influence.
CC           Default NUX Values for CHEM 1-3
C
      DO 1000 I = 1, 5
         I1 = 105 + I
         I2 = 705 + I
         I3 = 1305 + I
CCSC
         XARG = ABS(CONST (I1))
C         IF (CONST (I1) .EQ. 0) CONST (I1) = 1.0E20
        IF (XARG .LT. R0MIN) CONST (I1) = 1.0E20
CCSC
         XARG = ABS(CONST (I2))
C         IF (CONST (I2) .EQ. 0) CONST (I2) = 1.0E20
         IF (XARG .LT. R0MIN) CONST (I2) = 1.0E20
CCSC
         XARG = ABS(CONST (I3))
C         IF (CONST (I3) .EQ. 0) CONST (I3) = 1.0E20
         IF (XARG .LT. R0MIN) CONST (I3) = 1.0E20
 1000 CONTINUE
C
C           Default A0,A1,RHO1,RHO2,RHO3 for CHEM 1-3
C                                                               
C        IF (CONST   (71) .EQ. 0.) CONST   (71) = 2.65
C        IF (CONST   (72) .EQ. 0.) CONST   (72) = 2.65
C        IF (CONST   (73) .EQ. 0.) CONST   (73) = 2.65
C        IF (CONST    (2) .EQ. 0.) CONST    (2) = 1.
C
C----------------------------------------------------------------------
C     Set Decay Rates if Only Chemical Half Life is Given
C
C       CHEMICAL - 1
C
CCSC
      XARG = ABS(CONST (141))
C      IF (CONST (141) .EQ. 0. .AND. CONST (143) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (143) .GT. 0.
     1   ) CONST (141) = 0.693/
     2   CONST (143)
C
CCSC
      XARG = ABS(CONST (142))
C      IF (CONST (142) .EQ. 0. .AND. CONST (144) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (144) .GT. 0.
     1   ) CONST (142) = 0.693/
     2   CONST (144)
C
CCSC
      XARG = ABS(CONST (181))
C      IF (CONST (181) .EQ. 0. .AND. CONST (252) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (252) .GT. 0.
     1   ) CONST (181) = 0.693/
     2   CONST (252)
C
CCSC
      XARG = ABS(CONST (182))
C      IF (CONST (182) .EQ. 0. .AND. CONST (253) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (253) .GT. 0.
     1   ) CONST (182) = 0.693/
     2   CONST (253)
C
CCSC
      XARG = ABS(CONST (183))
C      IF (CONST (183) .EQ. 0. .AND. CONST (254) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (254) .GT. 0.
     1   ) CONST (183) = 0.693/
     2   CONST (254)
C
CCSC
      XARG = ABS(CONST (256))
C      IF (CONST (256) .EQ. 0. .AND. CONST (257) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (257) .GT. 0.
     1   ) CONST (256) = 0.693/
     2   CONST (257)
C
CCSC
      XARG = ABS(CONST (287))
C      IF (CONST (287) .EQ. 0. .AND. CONST (289) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (289) .GT. 0.
     1   ) CONST (287) = 0.693/
     2   CONST (289)
C
CCSC
      XARG = ABS(CONST (571))
C      IF (CONST (571) .EQ. 0. .AND. CONST (572) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (572) .GT. 0.
     1   ) CONST (571) = 0.693/
     2   CONST (572)
C
C       CHEMICAL - 2
C
CCSC
      XARG = ABS(CONST (741))
C      IF (CONST (741) .EQ. 0. .AND. CONST (743) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (743) .GT. 0.
     1   ) CONST (741) = 0.693/
     2   CONST (743)
C
CCSC
      XARG = ABS(CONST (742))
C      IF (CONST (742) .EQ. 0. .AND. CONST (744) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (744) .GT. 0.
     1   ) CONST (742) = 0.693/
     2   CONST (744)
C
CCSC
      XARG = ABS(CONST (781))
C      IF (CONST (781) .EQ. 0. .AND. CONST (852) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (852) .GT. 0.
     1   ) CONST (781) = 0.693/
     2   CONST (852)
C
CCSC
      XARG = ABS(CONST (782))
C      IF (CONST (782) .EQ. 0. .AND. CONST (853) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (853) .GT. 0.
     1   ) CONST (782) = 0.693/
     2   CONST (853)
C
CCSC
      XARG = ABS(CONST (783))
C      IF (CONST (783) .EQ. 0. .AND. CONST (854) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (854) .GT. 0.
     1   ) CONST (783) = 0.693/
     2   CONST (854)
C
CCSC
      XARG = ABS(CONST (856))
C      IF (CONST (856) .EQ. 0. .AND. CONST (857) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (857) .GT. 0.
     1   ) CONST (856) = 0.693/
     2   CONST (857)
C
CCSC
      XARG = ABS(CONST (887))
C      IF (CONST (887) .EQ. 0. .AND. CONST (889) .GT. 0.
      IF (XARG .LT. R0MIN .AND. CONST (889) .GT. 0.
     1   ) CONST (887) = 0.693/
     2   CONST (889)
C
CCSC
      XARG = ABS(CONST (1171))
C      IF (CONST (1171) .EQ. 0. .AND. CONST (1172) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1172) .GT. 0.)
     1   CONST (1171) = 0.693/
     2   CONST (1172)
C
C
C
C       CHEMICAL - 3
C
CCSC
      XARG = ABS(CONST (1341))
C      IF (CONST (1341) .EQ. 0. .AND. CONST (1343) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1343) .GT. 0.)
     1   CONST (1341) = 0.693/
     2   CONST (1343)
C
CCSC
      XARG = ABS(CONST (1342))
C      IF (CONST (1342) .EQ. 0. .AND. CONST (1344) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1344) .GT. 0.)
     1   CONST (1342) = 0.693/
     2   CONST (1344)
C
CCSC
      XARG = ABS(CONST (1381))
C      IF (CONST (1381) .EQ. 0. .AND. CONST (1452) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1452) .GT. 0.)
     1   CONST (1381) = 0.693/
     2   CONST (1452)
C
CCSC
      XARG = ABS(CONST (1382))
C      IF (CONST (1382) .EQ. 0. .AND. CONST (1453) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1453) .GT. 0.)
     1   CONST (1382) = 0.693/
     2   CONST (1453)
C
CCSC
      XARG = ABS(CONST (1383))
C      IF (CONST (1383) .EQ. 0. .AND. CONST (1454) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1454) .GT. 0.)
     1   CONST (1383) = 0.693/
     2   CONST (1454)
C
CCSC
      XARG = ABS(CONST (1456))
C      IF (CONST (1456) .EQ. 0. .AND. CONST (1457) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1457) .GT. 0.)
     1   CONST (1456) = 0.693/
     2   CONST (1457)
C
CCSC
      XARG = ABS(CONST (1487))
C      IF (CONST (1487) .EQ. 0. .AND. CONST (1489) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1489) .GT. 0.)
     1   CONST (1487) = 0.693/
     2   CONST (1489)
C
CCSC
      XARG = ABS(CONST (1771))
C      IF (CONST (1771) .EQ. 0. .AND. CONST (1772) .GT. 0.)
      IF (XARG .LT. R0MIN .AND. CONST (1772) .GT. 0.)
     1   CONST (1771) = 0.693/
     2   CONST (1772)
C
C     DETERMINE LIGHT AT THE WATER SURFACE FOR PHOTOLYSIS
C
      IF (ILITE .EQ. 0) THEN
         NDAT=13
         CLOUDG(13)=1.0
      ELSE
C
C           DETERMINE MONTH AT TIME ZERO
C
         NTF = .9*TZERO
         DAYR = 0.0
         DO 1010 K = 1, 12
            DAYR = DAYR + DAYMO (K)
            IF (T0 .LT. DAYR) GO TO 1020
 1010    CONTINUE
C
C           Error in defining t0.  Write message to user and abort run.
C
         WRITE (OUT, 6000) T0
 6000          FORMAT(//,5X,'JUIAN DATE AT START INCORRECTLY SPECIFIED'
     1         ,' AS',F5.0,/,5X,'  MUST BE BETWEEN 0 AND 365',/,
     2         'EXECUTION OF MODEL TERMINATED',//)
         CALL WEXIT
C
 1020    CONTINUE
         NDAT = K
C
C           Read in data needed for light intensity calculation
C
C         IF (ILITE .EQ. 2 .OR. ILITE .EQ. 4) THEN
C            MOI = NDAT
C            MOF = NDAT
C         ELSE
C            MOI = 1
C            MOF = 12
C         END IF
C
C
         IF (ILITE .LT. 3) CALL SOLAR (ILITE, ELEVG, LATG)
      END IF
      IF (IPHOTO .GT. 1) CALL LMDAMA (LAMAXG)
      DO 1040 ISEG = 1, NOSEG
         DO 1050 K = 1, NCHEM
CCSC
            XARG = ABS(PH(ISEG))
C            IF(PH(ISEG) .EQ. 0.)  PH(ISEG) = 7.0
            IF (XARG .LT. R0MIN) PH(ISEG) = 7.0
CCSC
            XARG = ABS(FOC(ISEG,K))
C            IF (FOC (ISEG, K) .EQ. 0.) FOC (ISEG, K) = 0.01
            IF (XARG .LT. R0MIN) FOC (ISEG, K) = 0.01
 1050    CONTINUE
 1040 CONTINUE
C
C       Initialize Yield Array
      Y (1, 1, 2) = CONST (176)
      Y (1, 2, 2) = CONST (178)
      Y (1, 3, 2) = CONST (246)
      Y (1, 4, 2) = CONST (248)
      Y (1, 5, 2) = CONST (250)
      Y (1, 6, 2) = CONST (281)
      Y (1, 7, 2) = CONST (566)
      Y (1, 8, 2) = CONST (596)
C
      Y (1, 1, 3) = CONST (177)
      Y (1, 2, 3) = CONST (179)
      Y (1, 3, 3) = CONST (247)
      Y (1, 4, 3) = CONST (249)
      Y (1, 5, 3) = CONST (251)
      Y (1, 6, 3) = CONST (282)
      Y (1, 7, 3) = CONST (567)
      Y (1, 8, 3) = CONST (597)
C
      Y (2, 1, 1) = CONST (776)
      Y (2, 2, 1) = CONST (778)
      Y (2, 3, 1) = CONST (846)
      Y (2, 4, 1) = CONST (848)
      Y (2, 5, 1) = CONST (850)
      Y (2, 6, 1) = CONST (881)
      Y (2, 7, 1) = CONST (1166)
      Y (2, 8, 1) = CONST (1196)
C
      Y (2, 1, 3) = CONST (777)
      Y (2, 2, 3) = CONST (779)
      Y (2, 3, 3) = CONST (847)
      Y (2, 4, 3) = CONST (849)
      Y (2, 5, 3) = CONST (851)
      Y (2, 6, 3) = CONST (882)
      Y (2, 7, 3) = CONST (1167)
      Y (2, 8, 3) = CONST (1197)
C
      Y (3, 1, 1) = CONST (1376)
      Y (3, 2, 1) = CONST (1378)
      Y (3, 3, 1) = CONST (1446)
      Y (3, 4, 1) = CONST (1448)
      Y (3, 5, 1) = CONST (1450)
      Y (3, 6, 1) = CONST (1481)
      Y (3, 7, 1) = CONST (1766)
      Y (3, 8, 1) = CONST (1796)
C
      Y (3, 1, 2) = CONST (1377)
      Y (3, 2, 2) = CONST (1379)
      Y (3, 3, 2) = CONST (1447)
      Y (3, 4, 2) = CONST (1449)
      Y (3, 5, 2) = CONST (1451)
      Y (3, 6, 2) = CONST (1482)
      Y (3, 7, 2) = CONST (1767)
      Y (3, 8, 2) = CONST (1797)
      RETURN
      END

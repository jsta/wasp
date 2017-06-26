      SUBROUTINE WASP3
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:12.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C        SUBROUTINE TO READ VOLUMES
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG
C
      INTEGER SEGJUN, SEACHN
      CHARACTER*80 DUMMY
C
      COMMON /SWFLW/ SCALQ, SCALV, NJ, NC, JUNSEG (SG), SEGJUN (SG),
     1   NSEA, JSEA (5), SIGN (5), SEACHN (5), NBC, NUPS
C
C***********************************************************************
C                READ VOLUME OPTION AND NO. VOLUMES
C***********************************************************************
C
      IF (MFLAG .EQ. 0) CALL GETMES (8, 0)
C
      READ (IN, 5000, ERR = 1000) IVOPT, IBEDV, TDINTS
 5000 FORMAT(2I5,F10.0)
      WRITE (OUT, 6000)
 6000 FORMAT(////33X,'VOLUMES'/,33X,7('~'),/)
C
C***********************************************************************
C   READ IN BED VOLUME OPTIONS:  IBEDV = 0, BED SEDIMENT CONCENTRATION
C                                      = 1, CONSTANT CONCENTRATION,
C                                          VARIABLE VOLUMES.
C                               TDINTS = TIME STEP FOR BED CALCULATIONS
C***********************************************************************
C
      WRITE (OUT, 6010) IBEDV, TDINTS
 6010 FORMAT(10X,'Bed Volume Option = ',I5,2X,'Bed Time Step = ',E10.3/)
C
C***********************************************************************
C     READ IN TYPES OF SEGMENTS (ITYPE): 1 = SFC WATER, 2 = LOWER WATER
C                                        3 = UPPER BED, 4 = LOWER BED
C***********************************************************************
C
      IF (ICFL .GE. 2) THEN
          REWIND ICRD
      END IF
C
      IF (ICRD .EQ. IN) THEN
         READ (IN, 5010, ERR = 1000) SCALV, CONVV
 5010    FORMAT(2E10.3)
      ELSE
         SCALV = 1.0
         CONVV = 1.0
         DO 10 K = 1, NOSEG + 1
            READ (IN, 5011, ERR = 1000) DUMMY
 5011       FORMAT(A80)
   10    CONTINUE
      END IF
CCSC
      XARG = ABS(CONVV)
C      IF (CONVV .EQ. 0.) CONVV = 1.0
      IF (XARG .LT. R0MIN) CONVV = 1.0
      WRITE (OUT, 6020) SCALV, CONVV
 6020 FORMAT(10X,'Scale Factor =',E12.4,1X,
     1           'Conversion Factor =',E12.4,//)
      SCALV = SCALV*CONVV
C
C***********************************************************************
C          OPTION 1 - CONSTANT VOLUMES
C***********************************************************************
C
      DO 1010 J = 1, NOSEG
         READ (ICRD, 5020, ERR = 1000) ISEG, NSEG,
     1       ITY, V, A1, B1, C1, D1
 5020    FORMAT(3I10,E10.3,4F10.5)
         IBOTSG (ISEG) = NSEG
         ITYPE (ISEG) = ITY
         BVOL (ISEG) = V
CCSC
         XARG = ABS(C1)
C         IF (C1 .EQ. 0.0) GO TO 1010
         IF (XARG .LT. R0MIN) GO TO 1010
         VMULT (ISEG) = A1
         VEXP (ISEG) = B1
         DMULT (ISEG) = C1
         DXP (ISEG) = D1
         TOPSEG (ISEG) = 0
 1010 CONTINUE
C
      DO 1020 ISEG = 1, NOSEG
         NSEG = IBOTSG (ISEG)
         IF (NSEG .GT. 0) TOPSEG (NSEG) = ISEG
 1020 CONTINUE
C
      WRITE (OUT, 6030)
 6030 FORMAT(2X,'Seg #',3X,'BOTSG',3X,'Type',7X,'Volume',6X,'V mult',
     1       4X,'V exp',3X,'D mult',4X,'D exp')
      WRITE (OUT, 6040)
 6040 FORMAT(1X,73('~'),/)
      DO 1030 ISEG = 1, NOSEG
         WRITE (OUT, 6050) ISEG, IBOTSG (ISEG),
     1      ITYPE (ISEG), BVOL (ISEG),
     2      VMULT (ISEG), VEXP (ISEG), DMULT (ISEG), DXP (ISEG)
 6050    FORMAT(1X,I5,3X,I5,2X,I5,1X,F15.3,4(3X,F6.3))
 1030 CONTINUE
C
      CALL SCALP (BVOL, SCALV, NOSEG)
C
      DO 1040 I = 1, NOSEG
         MVOL (I) = BVOL (I)
         B0VOL (I) = BVOL (I)
 1040 CONTINUE
C
      WRITE (OUT, 6060)
 6060 FORMAT('1')
C
      RETURN
C
 1000 CONTINUE
      CALL WERR(15,1,0)
      CALL WEXIT('Error Reading Card Group C',1)
      END

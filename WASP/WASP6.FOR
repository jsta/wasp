      SUBROUTINE WASP6
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:13.
C
C  Correction History:
C
C----------------------------------------------------------------------
C        SUBROUTINE TO READ FORCING FUNCTIONS
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
C
      REAL XARG
C
      REAL WKT (MB), T (MB)
C
      REAL SCALW (16)
C
C
      WRITE (OUT, 6000)
 6000 FORMAT(35X,'Waste Loads',/,35X,11('~'),/)
C
C                           SYSTEM LOOP
C
      IF (MFLAG .EQ. 0) CALL GETMES (16, 0)
C
      DO 1000 ISYS = 1, NOSYS
C
         READ (IN, 5000, ERR = 1010) NOWK (ISYS)
 5000    FORMAT(I10)
         IWKOP (ISYS) = 3
         NO = NOWK (ISYS)
         WRITE (OUT, 6010) ISYS
 6010    FORMAT(//27X,'Forcing Functions For System',I3/,27X,29('~')/)
         WRITE (OUT, 6020) IWKOP (ISYS), NOWK (ISYS)
 6020    FORMAT(21X,'Wk Option',I3,' Used',5X,'No.of WK''S Read',I5/)
         IF (NO .GT. WK) GO TO 1020
         IF (NO .EQ. 0) GO TO 1030
         READ (IN, 5010, ERR = 1010) SCALW (ISYS), CONVW
 5010    FORMAT(2E10.3)
CCSC
         XARG = ABS(CONVW)
C         IF (CONVW .EQ. 0.) CONVW = 1.0
         IF (XARG .LT. R0MIN) CONVW = 1.0
         WRITE (OUT, 6030) SCALW (ISYS), CONVW
 6030    FORMAT(11X,'Scale Factor =',E12.4,2X,
     1              'Conversion Factor =',E12.4//)
         SCALW (ISYS) = SCALW (ISYS)*CONVW
C
         WRITE (OUT, 6040)
 6040    FORMAT(9X,'Segment',2X,'No.Brk.',2(6X,'WK(T)',12X,'T',5X)/,9X,
     1         71('~'))
         WSCAL = SCALW (ISYS)
C
         DO 1040 I = 1, NO
            II = I + I + 2*WK*(ISYS - 1)
            IM = II - 1
            READ (IN, 5020, ERR = 1010) IWK (ISYS, I), NOBRK
            IF (NOBRK .GT. MB) CALL BRKERR (NOBRK)
            READ (IN, 5030, ERR = 1010) (WKT (J), T (J), J = 1, NOBRK)
 5020       FORMAT(2I5)
 5030       FORMAT(8F10.0)
            WRITE (OUT, 6050) IWK (ISYS, I), NOBRK, (WKT
     1         (J), T (J), J = 1, NOBRK)
 6050       FORMAT(12X,I3,4X,I3,5X,E14.7,F10.2,5X,E14.7,F10.2,/,
     1             1(27X,E14.7,F10.2,5X,E14.7,F10.2))
C
            IF (IWK (ISYS, I) .GT. SG) CALL WERR (6, I, IWK (ISYS, I))
            NBRK50 (ISYS, I) = NOBRK
            DO 1050 J = 1, NOBRK
               FILE50 (J, IM) = T (J)
 1050       CONTINUE
C
            DO 1060 J = 1, NOBRK
               WKT (J) = WKT (J)*WSCAL
               FILE50 (J, II) = WKT (J)
 1060       CONTINUE
 1040    CONTINUE
         GO TO 1000
C
C***********************************************************************
C           OPTION 0 - NO FF'S
C***********************************************************************
C
 1030    CONTINUE
         WRITE (OUT, 6060) ISYS
 6060    FORMAT(///24X,'No Forcing Function for System',I3)
         WRITE (OUT, '(''1'')')
C
 1000 CONTINUE
      RETURN
C
 1020 CONTINUE
      WRITE (OUT, 6070) ISYS, NO, WK
 6070 FORMAT(///10X,'The Number of Loads Specified ',
     1     'for System',I3,' is ',I4/10X,
     2     'The Maximum Dimensioned for This Version of WASP ',
     3     'is ',I3/10X,'Respecify Loads or Redimension ',
     4     'Parameter "WK" in the Common Block and Recompile.')
C
      CALL WERR(8,1,0)
      CALL WEXIT('Dimension Error See the Output File',1)
      RETURN
C
 1010 CONTINUE
      CALL WEXIT('  Error Reading Card Group F  ',1)
      END

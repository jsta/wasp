      SUBROUTINE EUTROINT
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:53.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG
      CHARACTER STRING*12,STROUT*60,STRING2*5,STRING1*10,CONSTRING*20
C
C                             Initialize Values
C
      PI = 3.14159
      DUMMY = 0.
      CHLA2 = 0.
      GP1 = 0.
      DP1 = 0.
      GP2 = 0.
      DP2 = 0.
      GZ1 = 0.
      DZ1 = 0.
      GZ2 = 0.
      DZ2 = 0.
      CFOREA = 1.0
C
      DO 1000 J = 1, MXSEG
         RLGHTS (J, 1) = 0.0
         RLGHTS (J, 2) = 1.0
 1000 CONTINUE
C
      INITB = 1
      MXDMP = 4
C
CCSC
       XARG = ABS(nfunt(7))
C      IF (nfunt(7) .EQ. 0.) THEN
      IF (XARG .LT. R0MIN) THEN
            WIND = 0.3
      END IF
C             Check to see if Michalis Menton constants are Zero
C             and readjust values to prevent floating zero divide
C
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:01:20.
      XARG = ABS(NCRB)
      IF (XARG .LT. R0MIN) NCRB = 0.25
      XARG = ABS(PCRB)
      IF (XARG .LT. R0MIN) PCRB = 0.025
      XARG = ABS(LGHTSW)
      IF (XARG .LT. R0MIN) LGHTSW = 1.0
CCSC
      XARG = ABS(KBOD)
C      IF (KBOD .EQ. 0.0) KBOD = 1.00E-20
      IF (XARG .LT. R0MIN) KBOD = 1.00E-20
CCSC
      XARG = ABS(KNO3)
C      IF (KNO3 .EQ. 0.0) KNO3 = 1.00E-20
      IF (XARG .LT. R0MIN) KNO3 = 1.00E-20
CCSC
      XARG = ABS(KPO4)
C      IF (KPO4 .EQ. 0.0) KPO4 = 1.00E-20
      IF (XARG .LT. R0MIN) KPO4 = 1.00E-20
CCSC
      XARG = ABS(KNIT)
C      IF (KNIT .EQ. 0.0) KNIT = 1.00E-20
      IF (XARG .LT. R0MIN) KNIT = 1.00E-20
CCSC
      XARG = ABS(KMNG1)
C      IF (KMNG1 .EQ. 0.0) KMNG1 = 1.00E-20
      IF (XARG .LT. R0MIN) KMNG1 = 1.00E-20
CCSC
      XARG = ABS(KMPG1)
C      IF (KMPG1 .EQ. 0.0) KMPG1 = 1.00E-20
      IF (XARG .LT. R0MIN) KMPG1 = 1.00E-20
CCSC
      XARG = ABS(KMPHYT)
C      IF (KMPHYT .EQ. 0.0) KMPHYT = 1.00E-20
      IF (XARG .LT. R0MIN) KMPHYT = 1.00E-20
C
CCSC
      XARG = ABS(OCRB)
C      IF (OCRB .EQ. 0.) OCRB = 32./12.
      IF (XARG .LT. R0MIN) OCRB = 32./12.
CCSC
      XARG = ABS(IS1)
C      IF (IS1 .EQ. 0.) IS1 = 300.
      IF (XARG .LT. R0MIN) IS1 = 300.
CCSC
      XARG = ABS(CCHL)
C      IF (CCHL .EQ. 0.) CCHL = 30.
      IF (XARG .LT. R0MIN) CCHL = 30.
      CCHL1 = CCHL
CCSC
      XARG = ABS(FON)
C      IF (FON .EQ. 0.) FON = 1.0
      IF (XARG .LT. R0MIN) FON = 1.0
CCSC
      XARG = ABS(FOP)
C      IF (FOP .EQ. 0.) FOP = 1.0
      IF (XARG .LT. R0MIN) FOP = 1.0
CCSC
      XARG = ABS(PHIMAX)
C      IF (PHIMAX .EQ. 0.) PHIMAX = 720.
      IF (XARG .LT. R0MIN) PHIMAX = 720.
CCSC
      XARG = ABS(XKC)
C      IF (XKC .EQ. 0.) XKC = 0.017
      IF (XARG .LT. R0MIN) XKC = 0.017
C
C  Check for Zero Temperature Correction Factors and readjust to 1.0
C
CCSC
      XARG = ABS(K1320T)
C      IF (K1320T .EQ. 0.) K1320T = 1.0
      IF (XARG .LT. R0MIN) K1320T = 1.0
CCSC
      XARG = ABS(K140T)
C      IF (K140T .EQ. 0.) K140T = 1.0
      IF (XARG .LT. R0MIN) K140T = 1.0
CCSC
      XARG = ABS(K1T)
C      IF (K1T .EQ. 0.) K1T = 1.0
      IF (XARG .LT. R0MIN) K1T = 1.0
CCSC
      XARG = ABS(K1RT)
C      IF (K1RT .EQ. 0.) K1RT = 1.0
      IF (XARG .LT. R0MIN) K1RT = 1.0
CCSC
      XARG = ABS(KDT)
C      IF (KDT .EQ. 0.) KDT = 1.0
      IF (XARG .LT. R0MIN) KDT = 1.0
CCSC
      XARG = ABS(K1013T)
C      IF (K1013T .EQ. 0.) K1013T = 1.0
      IF (XARG .LT. R0MIN) K1013T = 1.0
CCSC
      XARG = ABS(KONDT)
C      IF (KONDT .EQ. 0.) KONDT = 1.0
      IF (XARG .LT. R0MIN) KONDT = 1.0
CCSC
      XARG = ABS(K58T)
C      IF (K58T .EQ. 0.) K58T = 1.0
      IF (XARG .LT. R0MIN) K58T = 1.0
CCSC
      XARG = ABS(KOPDT)
C      IF (KOPDT .EQ. 0.) KOPDT = 1.0
      IF (XARG .LT. R0MIN) KOPDT = 1.0
CCSC
      XARG = ABS(KPZDT)
C      IF (KPZDT .EQ. 0.) KPZDT = 1.0
      IF (XARG .LT. R0MIN) KPZDT = 1.0
CCSC
      XARG = ABS(KDST)
C      IF (KDST .EQ. 0.) KDST = 1.0
      IF (XARG .LT. R0MIN) KDST = 1.0
C
C
C        Initialize internal clock for Dick Smith Light Formulation
C
C             Convert IC'S for Phyt from ug/L to mg C/L
C             Set all SOD Thetas to 1.0 if equal 0
C
      DO 1010 ISEG = 1, NOSEG
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:02:09.
         CCHLX(ISEG)=CCHL
         IF (C (4, ISEG) .LT. 1.0e-24) C (4, ISEG) = 1.0e-24
         C (4, ISEG) = C (4, ISEG)*CCHL/1000.
CCSC
         XARG = ABS(SODTA(ISEG))
C         IF (SODTA (ISEG) .EQ. 0.) SODTA (ISEG) = 1.0
         IF (XARG .LT. R0MIN) SODTA (ISEG) = 1.0
 1010 CONTINUE
C
C        Convert BC's for Phyt from ug/L to mg C/L
C
      NO = NOBC (4)
      DO 1020 I = 1, NO
         II = I + I + 2*bc*(4 - 1)
         NOBRK = NBRK30 (4, I)
         DO 1030 J = 1, NOBRK
            FILE30 (J, II) = FILE30 (J, II)*CCHL/1000.
 1030    CONTINUE
 1020 CONTINUE
 1040 CONTINUE
C
C       For 1-D Networks, Convert Observed Bed-Water Flux
C               IF (AIRTMP .EQ. 0.) CALL GETMES (34, 0)
CCSC
               XARG = ABS(AIRTMP)
C               IF (AIRTMP .EQ. 0.) AIRTMP = 15.
               IF (XARG .LT. R0MIN) AIRTMP = 15.
C
      DO 1050 ISEG = 1, NOSEG
        IF(DEPTHG(ISEG) .LE. 0.)THEN
           CALL REASTR(DEPTHG(ISEG),STRING1,'(F10.2)')
           CALL INTSTR(ISEG,STRING2,'(I5)')
           CONSTRING=' For Segment '//STRING2
      STROUT='Depth has to be > then 0. Depth unknown for seg'//STRING2
           CALL WEXIT(STROUT,1)
         ELSE
         SA = BVOL (ISEG)/DEPTHG (ISEG)
         FNH4 (ISEG) = FNH4 (ISEG)*SA*0.001
         FPO4 (ISEG) = FPO4 (ISEG)*SA*0.001
         ENDIF
 1050 CONTINUE
      RETURN
      END

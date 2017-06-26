      SUBROUTINE READIN
C
C======================================================================
C
C     Last Revised:  Date: FRIDAY, March 11, 1990.  Time: 22:30:00
C     The original READIN and INI subroutines were combined==> READIN
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C*****************   READ PROGRAM DESCRIPTION CARDS   ******************
c
      CALL GETMES (3, 0)
      READ (IN, 5000) ALPHA (1)
      READ (IN, 5000) ALPHA (2)
C
C****************   PRINT PROGRAM/OUTPUT CONTROL DATA   ****************
C
C    Print out banner, with program description and input file descripti
C    from the description cards read into ALPHA(1) and ALPHA(2).
C
      CALL PRTBAN
C
C*********************   READ PROGRAM CONTROL DATA   *******************
C
      CALL GETMES (4, 0)
      READ (IN, 5000) HEADER
      READ (IN, 5010) NJ, NC, NCYC, DELT, ICRD, ZDAY, ZHR, ZMIN, EDAY,
     1   EHR, EMIN
      TZERO = 24.*(ZDAY - 1.0) + ZHR + ZMIN/60.
      TZ = 3600.*TZERO
      TEND = 24.*(EDAY - 1.0) + EHR + EMIN/60.
      IF (NCYC .EQ. 0) NCYC = 3600.*(TEND - TZERO)/DELT
      IF (TEND .EQ. 0) TEND = FLOAT (NCYC)*DELT/3600. + TZERO
cia
      nzero = tz/delt
      if(nzero .le. 0) NZERO = 1
C
C*********************  READ OUTPUT CONTROL DATA   *********************
C
      CALL GETMES (5, 0)
      READ (IN, 5000) HEADER
      READ (IN, 5020) FPRINT, PINTVL, NOPRT
      IPRINT = FPRINT*3600./DELT
      INTRVL = PINTVL*3600./DELT
      READ (IN, 5030) (JPRT (I), I = 1, NOPRT)
      if (noprt .lt. 6) then
         intseg = noprt
      else
         intseg = 6
      endif
      do 111 k=1,intseg
         isegout(k)=jprt(k)
  111 continue
      CALL GETMES (7, 0)
C
C    Print out Program Control Data
C
      CALL PRTPCD
C
C    Print out Output Control Data
C
      CALL PRTOCD
C
C******************  READ HYDRAULIC SUMMARY DATA  **********************
C
      CALL GETMES (6, 0)
      READ (IN, 5000) HEADER
      READ (IN, 5040) SUMRY, TDAY, THR, TMIN, DTDUMP, NODYN, INTSCR
      IF (NODYN .GT. ND) WRITE (OUT, 6000) NODYN, ND
 6000   FORMAT ('Error! Number of Hydrodynamic Time Steps (',I4,
     1    ') Exceeds Model Dimensions (',I4,').')
      TTAPE = 24.*(TDAY - 1) + THR + TMIN/60.
C
cia   TTAPE = TTAPE*3600.
cia
      ITAPE = 3600.*(TTAPE - TZERO)/DELT
      ttape = ttape*3600.
      IF (ITAPE .LT. 0) ITAPE = 0
      NRSTRT = NZERO - 1
      ITDUMP = 3600.*DTDUMP/DELT
      IF (ITDUMP .LT. NODYN) ITDUMP = NODYN
 1000 CONTINUE
      XK = FLOAT (ITDUMP)/FLOAT (NODYN)
      IK = XK
      XKP = IK
      IF (XKP .NE. XK) THEN
         ITDUMP = ITDUMP + 1
         GO TO 1000
      END IF
 1010 CONTINUE
      XK = FLOAT (NCYC)/FLOAT (NODYN)
      IK = XK
      XKP = IK
      IF (XKP .NE. XK) THEN
         NCYC = NCYC + 1
         GO TO 1010
      END IF
      IF (INTSCR .EQ. 0) INTSCR = 1
      IF (INTSCR .GT. NODYN) INTSCR = NODYN
 1020 CONTINUE
      XK = FLOAT (NODYN)/FLOAT (INTSCR)
      IK = XK
      XKP = IK
      IF (XKP .NE. XK) THEN
         INTSCR = INTSCR + 1
         GO TO 1020
      END IF
      NOSCR = IK
C
 1030 CONTINUE
      XK = FLOAT (ITAPE)/FLOAT (NODYN)
      IK = XK
      XKP = IK
      IF (XKP .NE. XK) THEN
         ITAPE = ITAPE + 1
         GO TO 1030
      END IF
c
      IF (ICRD .EQ. 0) ICRD = 5
      IF (ICRD .NE. 5) THEN
C         OPEN (UNIT = ICRD, FILE = 'RESTART.INP', STATUS =
C     1      'OLD', FORM = 'FORMATTED')
C ICRD uses a file named from the concatenation of the input file name
C and the extension .RST.  BASE is in the common block FILE.
         OPEN (UNIT = ICRD, FILE = BASE//'.RST',
     1      STATUS = 'OLD', FORM = 'FORMATTED')
         REWIND ICRD
         READ (ICRD, 5050) ALPHA (2), TRSTRT, NRSTRT
         TZERO = TRSTRT
         NZERO = NRSTRT + 1
         ITAPE = NRSTRT
         IPRINT = FPRINT*3600./DELT
         INTRVL = PINTVL*3600./DELT
         IF (IPRINT .LT. NRSTRT) IPRINT = NRSTRT + INTRVL
      END IF
      IDUMP = ITDUMP + ITAPE
      if (idump .gt. ncyc) idump = ncyc
C
C    Print out Transient-Flow Tape Information
      CALL PRTTFT
C************************   READ JUNCTION DATA   ***********************
C
      CALL GETMES (8, 0)
      READ (IN, 5000) HEADER
C  HB - Print summaries with
C      WRITE(OUT,6090)
      IF (NJ .GT. JU) THEN
         WRITE (out, 6010)
 6010    FORMAT('ERROR. NUMBER OF JUNCTIONS EXCEEDS MODEL CAPACITY')
         STOP
      END IF
      DO 1040 J = 1, NJ
         READ (ICRD, 5060) JJ, Y (J), SURF (J), BELEV (J),
     1      (NCHAN (J, K), K = 1, CJ)
         IF (JJ .NE. J) STOP
 1040 CONTINUE
C
C***********************   READ CHANNEL DATA   *************************
C
      CALL GETMES (9, 0)
      READ (IN, 5000) HEADER
      IF (NC .GT. CH) THEN
         WRITE (out, 6020)
 6020    FORMAT('ERROR. NUMBER OF CHANNELS EXCEEDS MODEL CAPACITY')
         STOP
      END IF
      DO 1050 N = 1, NC
         READ (ICRD, 5070) NN, CLEN (N), B (N), R (N),
     1      CDIR (N), CN (N), V (N),
     2      (NJUNC (N, K), K = 1, 2)
 1050 CONTINUE
C
C*****  PRINT OUT SUMMARIES OF JUNCTION DATA, AND THE CHANNEL DATA  ****
C
      CALL PRTSUM
C
C********************   READ CONSTANT FLOW INPUTS   ********************
C
      CALL GETMES (10, 0)
      READ (IN, 5000) HEADER
      READ (IN, 5030) NCFLOW
      IF (NCFLOW .GT. CF) THEN
         WRITE (OUT, 6030)
 6030    FORMAT('ERROR.  NUMBER OF CONSTANT FLOWS EXCEEDS MODEL',
     1           'CAPACITY')
         STOP
      END IF
      IF (NCFLOW .GT. 0) THEN
         WRITE (OUT, 6040)
         DO 1060 I = 1, NCFLOW
            READ (IN, 5080) JRCF (I), CFLOW (I)
 1060    CONTINUE
      DO 2040 I = 1, NCFLOW
         WRITE (OUT, 7040) JRCF (I), CFLOW (I)
         J = JRCF (I)
         CQIN (J) = CFLOW (I)
 2040 CONTINUE
C
      END IF
C
C*******************   READ TRANSIENT FLOW INPUTS   ********************
C
      CALL GETMES (11, 0)
      READ (IN, 5000) HEADER
      READ (IN, 5030) NVFLOW
      IF (NVFLOW .GT. VF) THEN
         WRITE (MESS, 6050)
 6050    FORMAT('ERROR. NUMBER OF VAR FLOWS EXCEEDS MODEL CAPACITY')
         STOP
      END IF
      IF (NVFLOW .GT. 0) THEN
         DO 1070 I = 1, NVFLOW
            READ (IN, 5090) JRVF (I), NINCR (I)
            IF (NINCR (I) .GT. MQ) THEN
               WRITE (MESS, 6060)
 6060       FORMAT('ERROR. NUMBER OF DATA POINTS EXCEEDS',
     1             ' MODEL CAPACITY')
               STOP
            END IF
            NX = NINCR (I)
            READ (IN, 5100) (DAY (K), HR (K), MIN (K),
     1         VFLOW (I, K), K = 1, NX)
c 1070    CONTINUE july 90
c      END IF
c process............................................
c      DO 2050 I = 1, NVFLOW
c90
         NQ (I) = 0
         DO 2130 J = 1, NJ
            VQ (I, J) = 0.0
 2130    CONTINUE
         qrep(i) = 0
c90
         DO 2060 K = 1, Nincr(i)
            QTIME = 86400.*(DAY (K) - 1.0) +
     1         3600.*HR (K) + 60.*MIN (K)
            QCYC (I, K) = (QTIME - TZ)/DELT
            IDAY (K) = DAY (K)
            IHR (K) = HR (K)
            IMIN (K) = MIN (K)
 2060 CONTINUE
         WRITE (OUT, 7050) JRVF (I)
         WRITE (OUT, 7060)
         WRITE (OUT, 7070) (IDAY (K), IHR (K), IMIN (K),
     1      VFLOW (I, K), K = 1, NX)
c 2050 CONTINUE
 1070    CONTINUE
      END IF
C
C*************   READ SEAWARD BOUNDARY TIDAL CONDITIONS   **************
c
       CALL GETMES (12, 0)
       CALL SEAWRD
C
C*********************   READ WIND CONDITIONS  *************************
c
        CALL GETMES (13, 0)
        IREADW = 0
        CALL WIND
C
C90************ READ IN EVAPORATION DATA ********************************
c
       IRE = 0
       CALL EVAPORAT
C
C************ READ CHANGE IN SURFACE AREA PER CHANGE IN HEAD ***********
C
       READ (ICRD, 5000) HEADER
       READ (ICRD, 5030) IJ
       DO 1080 J = 1, IJ
         READ (ICRD, 5110) JJ, SAN (JJ)
 1080 CONTINUE
C
C *************** READ CHANGE IN WIDTH PER CHANGE IN HEAD **************
C
      READ (ICRD, 5000) HEADER
      READ (ICRD, 5030) IC
      DO 1100 N = 1, IC
         READ (ICRD, 5110) ICC, SLOPE (ICC)
 1100 CONTINUE
C90
C**********************   INITIALIZE CONSTANTS   ***********************
      CALL GETMES (14, 0)
	iscr = 0
      DO 2000 J = 1, NJ
         IF (ABS (SAN (J)) .LT. .0001) SAN (J) = 0.
 2000 CONTINUE
      DO 2010 N = 1, NC
         IF (ABS (SLOPE (N)) .LT. .0001) SLOPE (N) = 0.
 2010 CONTINUE
      IF (ICRD .NE. 5) THEN
         NCYC = NCYC + NRSTRT
         WRITE (OUT, 7030) ALPHA (2), NRSTRT, NZERO, NCYC
      END IF
CAdded 7/18/91 YTMP never initialized TAW
      ytmp=0.
      DO 2020 J = 1, NJ
           YTMP = YTMP + Y (J)
         BTMP = ABS (BTMP) + ABS (BELEV (J))
         YT (J) = Y (J)
 2020 CONTINUE
      DO 2030 N = 1, NC
cia
         R(N) = (Y (NJUNC (N, 1)) + Y (NJUNC (N, 2)))/2.0
     *    - (BELEV (NJUNC (N, 1)) + BELEV (NJUNC (N, 2)))/2.0
          area (n) = b(n)* R(N)
cia
 2030 CONTINUE
c
      TZERO = TZERO*3600.
      TEND = TEND*3600.
      LTAPE = NCYC
      DT = DELT
      DT2 = DT/2.
      G = 9.807
      T = TZERO
      DO 2070 N = 1, NC
C         AK (N) = G*(CN (N)**2./2.208196)
         AK (N) = G*(CN (N)**2.)
         NL = NJUNC (N, 1)
         NH = NJUNC (N, 2)
         IF (NL .LT. NH) GO TO 2070
         NJUNC (N, 1) = NH
         NJUNC (N, 2) = NL
 2070 CONTINUE
C
C*******************   INITIALIZE JUNCTION VOLUMES   *******************
C
      DO 2080 J = 1, NJ
        VOL (J) = SURF (J)*( Y(J) - BELEV(J) )
        if (vol(j) .le. 0.0) then
           write(out, 6666)
           stop
         end if
 2080 CONTINUE
C
C ******************  FORMAT BLOCKS  ***********************************
 5000 FORMAT (A80)
 5110 FORMAT (I5,F10.0)
 5040 FORMAT (I5,F5.0,1X,2F2.0,F5.0,11I5)
 5050 FORMAT (1X,20A4/1X,20A4/F10.5,I5)
 5010 FORMAT (3I5,F5.0,I5,2(F5.0,1X,2F2.0))
 5020 FORMAT (2F10.0,I5)
 5030 FORMAT (16I5)
 5080 FORMAT (I10,F10.0)
 5100 FORMAT (4(F5.0,1X,2F2.0,F10.0))
 5090 FORMAT (I10,I10)
 5060 FORMAT (I5,3F10.0, 7I5)
 5070 FORMAT (I5,6F10.0,2I5)
 6040 FORMAT (1H1,//1X,130(1H*),//56X,'CONSTANT FLOW INPUTS',//1X,130(1H
     1*),//43X,'(WITHDRAWALS ARE POSITIVE - INPUTS ARE NEGATIVE)',///52X
     2,'JUNCTION',16X,'FLOW',/45X,45(1H=),/ )
 6666 format(2x, ' ERROR!:Initial Water Level is not Higher ',
     *                     'Than Bottom')
 7030 FORMAT (1H0/55X,'THIS IS A RESTART RUN'/33X,'INITIAL ',
     1  'CONDITIONS SET EQUAL TO FINAL CONDITIONS FROM RUN TITLED'/
     2  25X,20A4/25X,20A4//
     3  42X,'INITIAL CONDITIONS REFLECT HYDRAULIC CYCLE',I5/
     4  40X,'THIS SIMULATION RUNS FROM CYCLE',I5,' TO CYCLE',I5)
 7040 FORMAT (54X,I3,15X,F10.3,/)
 7050 FORMAT (60X,'JUNCTION # ',I3,///)
 7060 FORMAT (1X,5('DAY    TIME      FLOW     '),/,
     1        1X,5('===    =====     =====    '),/)
 7070 FORMAT (1H ,5(I3,4X,I2,':',I2,F10.1,4X),/)
C************ END *****************************************************
      return
      end

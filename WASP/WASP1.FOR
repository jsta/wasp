      SUBROUTINE WASP1
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:10.
C
C  Correction History:
C  - Removed all references to LISTC & LISTG (TAW)
C
C---------------------------------------------------------------------
C
C        INITILIZATION ROUTINE
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG
C
      REAL DTS (MB), T (MB)
      CHARACTER HEADS*80, TYPE*80, OUTFIL*12, CONT1*1,
     1   SIMTYPE*13, HEADER*80, TRNSFIL*12, DMPFIL*12, MASSFIL*12,
     2   HOLDIT*1, ANS*1, DATE*30,WISIM*10,TSEG(SG)*10,FILES(30)*25,
     3   MSGFILE*12,MESSFIL*30,RSTIN*12
C 
      DATA CONT1/'$'/
C
C     Current date and version of WASP change with every upgrade
C
C      DATA DATE/'Date: Wenesday 19 May 1993'/
C              /'123456789012345678901234567890'/
      DATA DATE/' Date: Wed., 1 September 1993 '/
C
      WISIM = '5.10'
C
      INPERR = 0
      CALL SHINIT (' ')
      IF (INFOHW(20).EQ.0) CALL IIDISP(1)
      WQ=.TRUE.
      INPERR = 0
      ICOUNT = 0
      IPROP=0
C
C======================================================================
C       -*-*  INQUIRE FOR INPUT-OUTPUT FILENAMES  *-*-
C======================================================================
C
      OPEN (UNIT = CTL, STATUS = 'OLD', ACCESS = 'SEQUENTIAL',
     1   FILE = 'WASP.CTL',IOSTAT=ISTAT)
      IF (ISTAT .NE. 0)THEN
         OPEN(UNIT=19,STATUS='OLD',ACCESS='SEQUENTIAL',
     1        FORM='FORMATTED',FILE='MESSFILE.DAT', IOSTAT = ISTAT)
      IF (ISTAT .NE. 0)THEN
        CALL WEXIT ('Unable to Find WASP Message File MESSFILE.DAT',0)
      END IF
         IF (SY .EQ. 6) THEN
             CALL GETMES (1, 1)
          ELSE
             CALL GETMES (2, 1)
         ENDIF
C
         CALL GETMES(41,1)
         CALL ATTRIB (' ')
         CALL CLSCRN
         CALL SCBACK ('BLUE', 'WHITE')
         CALL COLOR ('WHITE', 'BLUE')
         CALL HIGHLT ('BLUE', 'BLACK')
         N = 30
         CALL OSDIRL (' ','*.INP',FILES, N)
C
         IF ( N .EQ. 0)THEN
            CALL ATTRIB(' ')
            CALL COLOR('W','RED')
            CALL WNOPEN(0,21,70,2)
            CALL WNOUST('No WASP Input file(s) were found!!  WASP'
     1      //' assumes the input filenames have the extension *.INP.'
     2      //'  Please correct and re-execute.')
            CALL WEXIT('Error Finding Input File',1)
         ELSE
            CALL ATTRIB('B')
            CALL COLOR('CYAN','BLUE')
            CALL WNOPEN(0,21,70,3)
            CALL WNOUST('Please select the input file to execute.  '
     1      //'WASP assumes an input filename extension of #.INP and  '
     2      //'must be located in the WASP5 root directory.')
            CALL COLOR ('WHITE', 'BLUE')
            IOPT = MNSCRL (FILES, N, 0, 0, 'Input File Name', 10, 2, 1)
            INFIL=FILES(IOPT)
            CALL WNCLOS(0)
         ENDIF
C
      ELSE
         WISP=.TRUE.
         READ(1,8010)WSPATH
         READ(1,8000)INFIL
 8000    FORMAT(A12)
 8010    FORMAT(A30)
         CLOSE(UNIT=CTL)
         ICHR=LENACT(WSPATH)
         MESSFIL=WSPATH(1:ICHR)//'MESSFILE.DAT'
         OPEN(UNIT=19,STATUS='OLD',ACCESS='SEQUENTIAL',
     1        FORM='FORMATTED',FILE=MESSFIL, IOSTAT = ISTAT)
         IF (SY .EQ. 6) CALL GETMES (1, 1)
         IF (SY .EQ. 8) CALL GETMES (2, 1)
      ENDIF
C
C
C determine output file name from INFIL
C
      DO 1010 I = 1, 12
         IF (INFIL (I:I) .EQ. '.') GO TO 1020
         OUTFIL (I:I) = INFIL (I:I)
         DMPFIL (I:I) = INFIL (I:I)
         TRNSFIL (I:I) = INFIL (I:I)
         MASSFIL (I:I) = INFIL (I:I)
         RSTIN (I:I) = INFIL (I:I)
         ERRFIL (I:I) = INFIL (I:I)
         GO TO 1010
 1020    CONTINUE
         OUTFIL (I:I + 3) = '.OUT'
         IF(SY .EQ. 6)DMPFIL (I:I + 3) = '.TDF'
         IF(SY .EQ. 8)DMPFIL (I:I + 3) = '.EDF'
         TRNSFIL (I:I + 3) = '.TRN'
         MASSFIL (I:I + 3) = '.MSB'
         RSTIN (I:I + 3) = '.RST'
         ERRFIL (I:I + 3) = '.ERR'
         GO TO 1030
 1010 CONTINUE
 1030 CONTINUE
C
C======================================================================
C     -*-* Open All Input-Output Units used by WASP4 *-*-
C======================================================================
C
C                        User Supplied Input Dataset
C
      OPEN (UNIT = IN, STATUS = 'OLD', ACCESS = 'SEQUENTIAL',
     1   FILE = INFIL)
C
C                        WASP Simulation Result File
C
      OPEN (UNIT = IDMP, STATUS = 'UNKNOWN', FILE = DMPFIL)
C
C                           WASP Output File
C
      OPEN (UNIT = OUT, STATUS = 'UNKNOWN', ACCESS = 'SEQUENTIAL',
     1   FILE = OUTFIL)
C
C======================================================================
      READ (IN, 5060, ERR = 1060) HEADS
      READ (IN, 5060, ERR = 1030) TYPE
 5060 FORMAT(A80)
      READ (IN, 5060, ERR = 1060) HEADER
C
      READ (IN, 5070, ERR = 1060) NOSEG, NOSYS, ICFL, MFLAG, JMASS,
     1   NEGSLN, INTYP, ADFAC, ZDAY, ZHR, ZMIN, tflag
 5070 FORMAT(7I5,2F5.0,F3.0,F2.0,i5)
      READ (IN,5075,ERR=1060) (ISEGOUT(I), I = 1, 6)
 5075 FORMAT(6(I5))
C
C
C======================================================================
C
C  Open the Mass Balance Table and Transport Table Only if the User
C  requested the files be created.
C
C======================================================================
C
C
C                             Transport Table
C
      IF (TFLAG .EQ. 0)OPEN (UNIT = ITRNS, STATUS = 'UNKNOWN', 
     1                       ACCESS = 'SEQUENTIAL',FILE = TRNSFIL)
C
C                           Mass Balance Table
C
      IF(JMASS .GT. 0)OPEN (UNIT = IMASS, STATUS = 'UNKNOWN', 
     1                      ACCESS = 'SEQUENTIAL',FILE = MASSFIL)
C
C======================================================================
C
      IF (NOSEG .EQ. 0)THEN
         CALL GETMES (40,1)
         STOP
      ENDIF
C
      CALL ATTRIB ('B')
      CALL COLOR ('WHITE','BLUE')
      IF (ISEGOUT(3).EQ.0.OR.ISEGOUT(4).EQ.0.OR.ISEGOUT(5).EQ.0
     1    .OR. ISEGOUT (6) .EQ. 0)THEN
         IF(NOSEG .GT. 6)THEN
            CALL WNOPEN(0,21,70,3)
            CALL WNOUST('Please select the input file to execute.  '
     1      //'WASP assumes an input filename extension of #.INP and  '
     2      //'must be located in the WASP5 root directory.')
            IF(ISEGOUT(1) .EQ. 0)THEN
               DO 10 I =1,NOSEG
                  CALL INTSTR(I,TSEG(I),'(I10)')
 10            CONTINUE
C
               IMOVE=1
C
               DO 20 I = 1, 6
                 IOPT = MNSCRL(TSEG,NOSEG, 0, 0,'Segments',5, 2,IMOVE)
                 ISEGOUT(I)=IOPT
                 TSEG(IOPT)(1:1)='*'
                 IMOVE=IOPT+1
 20            CONTINUE
            ENDIF
C
            CALL WNCLOS(0)
            CALL CLSCRN
            CALL ATTRIB(' ')
         ELSE
            CALL CLSCRN
            CALL ATTRIB(' ')
C
            DO 25 I=1,NOSEG
               ISEGOUT(I)=I
 25         CONTINUE
C
         ENDIF
      ENDIF
C
      IF (MFLAG .EQ. 0) CALL GETMES (5, 0)
C
      IF (NOSYS .GT. SY) THEN
         CALL GETMES (6, 1)
         WRITE (OUT, 6080) SY, NOSYS
 6080    FORMAT(//,7X,'Program was Compiled for ',I2,' sytems ',
     1          'you are trying to run',/,7X,' a simulation with ',
     2           I2,' systems.  Simulation ABORTED need to recompile',
     3           /,' change "SY" in the WASP Common Block to the',
     4           'number of systems needed')
         CALL WERR(27,1,0)
         CALL WEXIT('Incorrect Dataset',1)
      END IF
C
      IF (ICFL .EQ. 0) ICRD = IN
      IF (ICFL .EQ. 1) ICRD = IN
      IF (ICFL .EQ. 2) THEN
         ICRD = 8
         OPEN (UNIT = ICRD, FILE = RSTIN, STATUS = 'OLD',
     1      FORM = 'FORMATTED')
      END IF
C
      READ (IN, 5070, ERR = 1060) NOBRK
      IF (NOBRK .GT. MB) CALL BRKERR (NOBRK)
      READ (IN, 5080, ERR = 1060) (DTS (I), T (I), I = 1, NOBRK)
 5080 FORMAT(8F10.0)
      FTIME = T(NOBRK)
      IF (INTYP .GE. 1) DTMAX = DTS (1)
      READ (IN, 5070) NPRINT
      READ (IN, 5080) (PRINT (I), TPRNT (I), I = 1, NPRINT)
      READ (IN, 5090, ERR = 1060) (SYSBY (I), I = 1, NOSYS)
 5090 FORMAT(16I5)
C
C***********************************************************************
C          Set Maximum Values for Output Arrays
C***********************************************************************
C
      MXSYS = NOSYS
      MXSEG = NOSEG
C
C          INITIALIZE VALUES
C
      IF (MFLAG .EQ. 0) CALL GETMES (23, 0)
C
CCSC
      XARG = ABS(ZDAY)
C      IF (ZDAY .EQ. 0.0) THEN
      IF (XARG .LT. R0MIN) THEN
         TZERO = 0.
      ELSE
         TZERO = (ZDAY - 1.0) + (ZHR/24.) + (ZMIN/1440.)
      END IF
C
      INITB = 0
      IREC = 0
      ITIMV = 1
      DRTIME = 0.0
      DWKTIM = 0.0
      NVOLT = 0.0
      DT = 0.0
      TEND = 0.0
      DO 1070 I = 1, NOSYS
         NTW (I) = 0.0
         NTB (I) = 0.0
 1070 CONTINUE
      NTF = TZERO
      DO 1080 I = 1, TF
         MFUNC (I) = 0.0
         BFUNC (I) = 1.0
         NFUNT (I) = 0.0
         ITIMF (I) = 1
 1080 CONTINUE
C
      CALL SETRA (C, SY, SG, 0.0)
C
      CALL SETXA (CD, SY, SG, 0.0D0)
C
      CALL SETRA (PARAM, SG, PR, 0.0)
C
      CALL SETRA (CONST, CX, 1, 0.0)
C
      CALL SETRA (BBC, SY, BC, 0.0)
C
      CALL SETRA (BWK, SY, WK, 0.0)
C
      CALL SETRA (MBC, SY, BC, 0.0)
C
      CALL SETRA (MWK, SY, WK, 0.0)
C
      CALL SETRA (NBCT, SY, BC, TZERO)
C
      CALL SETRA (NWKT, SY, WK, TZERO)
C
      CALL SETIA (ITIMB, SY, BC, 1)
C
      CALL SETIA (ITIMW, SY, WK, 1)
C
C***********************************************************************
C          INITIALIZE TRANSPORT FIELD ARRAY (F-ARRAY)
C***********************************************************************
C
      CALL SETRB (F, MNF, SY, SG, 0.0)
C
      DO 1090 ISYS = 1, NOSYS
         DO 1090 ISEG = 1, SG
            F (1, ISYS, ISEG) = 1.0
 1090 CONTINUE
C
C**********************************************************************
C             Initialize Continuity Matrix to Zero.
C**********************************************************************
C
      CALL SETRB (BR, MNF, MNI, S2, 0.0)
C
      CALL SETRB (BQ, MNF, MNI, S2, 0.0)
C
C***********************************************************************
C          Initialize Transport Field Time Function Counters
C***********************************************************************
C
      CALL SETRA (TNQ, MNF, MNI, TZERO)
C
      CALL SETRA (TNR, MNF, MNI, TZERO)
C
      CALL SETIA (ITIMQ, MNF, MNI, 1)
C
      CALL SETIA (ITIMR, MNF, MNI, 1)
C
C***********************************************************************
C             Write Introduction Page to WASP Output File
C***********************************************************************
C
      WRITE (OUT, 6090)
 6090 FORMAT( 1X,13(/),5X,70('*'),//,11X,'  WASP  - -',
     1' WATER QUALITY ANALYSIS SIMULATION PROGRAM',//,5X,70('*'))
      WRITE (OUT, 6100)
 6100 FORMAT(1X,/,
     1 /,20X,'Developed and supported by:',
     2 /,20X,'U.S. Environmental Protection Agency',
     3 /,20x,'Center for Exposure Assessment Modeling (CEAM)',
     4 /,20X,'Athens Environmental Research Laboratory',
     5 /,20X,'960 College Station Road',
     6 /,20X,'Athens, Georgia  30605-2720',
     7 /,20X,'Voice: 706/546-3130 / BBS: 706/546-3402',//)
      WRITE (OUT, 6110) WISIM, DATE
 6110 FORMAT(31X,'Version ',A10,/,19X,'Compiled on: ',a30,//)
      WRITE (OUT, 6120) SY, SG, MB, PR, CX, BC, WK, MP, TF
 6120 FORMAT (23X,'Maximum Parameters for this Model'//,
     1 13X,'Systems: ',i4,11x,'Segments: ',
     2 i4,5x,'Break Points: ',i4,/,
     3 10x,'Parameters: ',i4,10x,'Constants: ',i4,
     4 4x,'Boundary Cond: ',i4,/,
     5 8x,' Waste Loads: ',i4,5x,'Print Interval: ',i4,
     6 4x,'Time Function: ',i4,/,
     7 13x,//,5X,70('*'),/)
      WRITE (OUT, 6130) NOSEG, NOSYS
 6130 FORMAT(//,5X,'No. Segments ---> ',I5,7X,
     1' No. Systems ---> ',I5//)
C
      WRITE (OUT, 6150) NOSYS, (SYSBY (I), I = 1, NOSYS)
 6150 FORMAT(12X,' System Bypass Options for System  1 TO',I3,' are',
     1  20I2)
      WRITE (OUT, 6160)
 6160 FORMAT(1X,/,5X,70('*'))
C
      WRITE (OUT, 6170) INTYP, ADFAC
 6170 FORMAT('1',1H ////,
     1       29X,'Simulation Time Steps'/
     2       30X,'Option',I2,' Selected'/
     3       28X,'Advection Factor =',F6.3/)
      IF (NEGSLN .EQ. 1) WRITE (OUT, 6180)
 6180 FORMAT(5X,'User has chosen to permit negative solutions'/)
C
C           VARIABLE TIME STEPS
C
      WRITE (OUT, 6190)
      WRITE (OUT, 6200) (DTS (I), T (I), I = 1, NOBRK)
 6190 FORMAT(3X,3(5X,'DT',9X,'T',5X)/,3X,3(3X,6('~'),6X,7('~'))/)
 6200 FORMAT(3X,3(F10.5,F12.5))
      WRITE (OUT, 6210)
 6210 FORMAT(/////)
      NBRK75 (1) = NOBRK
      DO 1100 I = 1, NOBRK
         II = 2*I - 1
         III = II + 1
         FILE75 (II, 1) = DTS (I)
         FILE75 (III, 1) = T (I)
 1100 CONTINUE
      DT = DTS (1)
      TEND = T (1) 
C
C              READ IN PRINTER CONTROL PARAMETERS
C
      WRITE (OUT, 6220)
      WRITE (OUT, 6230) (PRINT (I), TPRNT (I), I = 1, NPRINT)
 6220 FORMAT(30X,'Print Intervals'//
     13(3X,'   PRINT  ',2X,'  ELAPSED ')/
     23(3X,' INTERVAL ',2X,'   TIME   ')/
     33(3X,'~~~~~~~~~~',2X,'~~~~~~~~~~')/)
 6230 FORMAT(3(2X,F10.3,2X,F10.2))
      WRITE (OUT, 6240)
 6240 FORMAT('1')
      CALL COLOR ('WH','BLACK')
      RETURN
C
 1060 CONTINUE
      CALL WERR(28,1,0)
      CALL WEXIT ('Error Reading Card Group A',1)
      END

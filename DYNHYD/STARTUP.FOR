      SUBROUTINE STARTUP
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
      INTEGER I, J
      CHARACTER*12 FILES(30)
      WISP = .FALSE.
      mespath='dynmes.dat'
C      initialize filenames
      DMPCT = 0
C
      OPEN (UNIT = ctl, STATUS = 'OLD', ACCESS = 'SEQUENTIAL',
     1   FILE = 'wasp.ctl',iostat=istat)
      if (istat .NE. 0)then
      CALL GETMES (1, 1)
         n = 30
         call OSDIRl (' ','*.INP',FILES, n)
      call clscrn
      CALL SCBACK ('blue', 'white')
      CALL COLOR ('white', 'blue')
      CALL HIGHLT ('blue', 'yellow')
         if ( n .eq. 0)then
            CALL WNOPEN(0,21,70,2)
            CALL WNOUST('No WASP Input file(s) were found!!  WASP'
     1      //' assumes the input filenames have the extension *.INP.'
     2      //'  Please correct and re-execute.')
            call wexit('Error Finding Input File',1)
        else
            CALL WNOPEN(0,21,70,3)
            CALL WNOUST('Please select the input file to execute.  '
     1      //'WASP assumes an input filename extension of #.INP and  '
     2      //'must be located in the WASP4 root directory.')
c         iopt = mnscrl (files, n, 0, 0, 'Input File Name', 10, 1, 1)
         iopt = mnscrl (files, n, 0, 0, 'Input File', 10, 1, 1)
         infil = files (iopt)
        endif
      else
          wisp=.true.
          CALL GETMES (1, 1)
          read(ctl,2010)mespath
 2010     format(a32)
          read(ctl,2000)infil
 2000     format(a12)
CCSC
          CLOSE(UNIT=CTL)
CCSC
C          OPEN (UNIT=CTL,STATUS='OLD',ACCESS='SEQUENTIAL',
C     1          FORM='FORMATTED',FILE='DYNMESFL.CTL',IOSTAT=ISTAT)
C          read(CTL,2010)mespath
C          close(unit=ctl)
CCSC
      endif
        CALL WNCLOS(0)
      DO 1111 I = 1, 12
         IF (INFIL (I:I) .EQ. '.') GO TO 1121
         OUTFIL (I:I) = INFIL (I:I)
         DMPFIL (I:I) = INFIL (I:I)
         HYDFIL (I:I) = INFIL (I:I)
         restFIL (I:I) = INFIL (I:I)
         GO TO 1111
 1121    CONTINUE
         OUTFIL (I:I + 3) = '.OUT'
         DMPFIL (I:I + 3) = '.DDF'
         HYDFIL (I:I + 3) = '.HYD'
        restFIL (I:I + 3) = '.rst'
         GO TO 1131
 1111 CONTINUE
 1131 CONTINUE
C INITIALIZATION........
         DO 1001 J = 1, NJ
            CQIN (J) = 0.0
            VQIN (J) = 0.0
1001     CONTINUE
C
C***********************************************************************
C                              OPEN FILES
C***********************************************************************
C
      OPEN (UNIT = SCR, FILE = 'SCRATCH.TMP',STATUS='UNKNOWN',
     1   FORM = 'FORMATTED')
C
      OPEN (UNIT = OUT, FILE = outfil, STATUS = 'UNKNOWN',
     1   FORM = 'FORMATTED')
      OPEN (UNIT = IDMP, FILE= dmpfil, STATUS = 'UNKNOWN')
      OPEN (UNIT = IN, FILE= infil, STATUS = 'UNKNOWN')
C
      RETURN
      END

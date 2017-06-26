      SUBROUTINE PRTPCD
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C     PRINT PROGRAM CONTROL DATA
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
      WRITE (OUT, 6000) NJ, NC, NCYC, DELT, TZERO, TEND
 6000 FORMAT (///60X,'PROGRAM CONTROL',/45X,50(1H=),//50X,'NUMBER OF JUN
     1CTIONS (NJ)    = ',I5,//50X,'NUMBER OF CHANNELS (NC)     = ',I5,//
     250X,'LENGTH OF SIMULATION (NCYC) = ',3X,I5,' CYCLES',//50X,
     3'HYDRAULIC TIME STEP (DELT)  = ',3X,F5.1,' SEC',//50X,'STARTING ',
     4'TIME (TZERO)       = ',F7.2,' HOURS'//50X,'ENDING TIME (TEND)
     5= ',F6.2,' HOURS')
      RETURN
      END

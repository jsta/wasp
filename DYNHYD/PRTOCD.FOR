      SUBROUTINE PRTOCD
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C     PRINT OUTPUT CONTROL DATA
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
      WRITE (OUT, 6000) IPRINT, INTRVL
 6000 FORMAT (///60X,'OUTPUT CONTROL',/45X,50(1H=),//50X,'PRINTOUT BEGIN
     1S AT CYCLE (IPRINT)   = ',I5,//50X,'INTERVAL BETWEEN PRINTOUTS (IN
     2TRVL) = ',I5)
      RETURN
      END

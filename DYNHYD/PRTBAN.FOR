      SUBROUTINE PRTBAN
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C                PRINT BANNER TO OUTPUT FILE
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
      INTEGER I
      WRITE (OUT, 6000) (ALPHA (I), I = 1, 2)
 6000 FORMAT (1H1,/1X,130(1H*),//58X,'PROGRAM DYNHYD5',/
     1 53X,'Developed and Supported by',/
     2 50X,'U.S. Environmental Protection Agency',/
     3 45X,'Environmental Research Laboratory',/
     4 59X,'Athens, Georgia  30605',//
     5 50X,'Based on DYNHYD2, as developed by'/
     6 43X,'U.S. EPA Region 3 - Central Regional Laboratory',/
     7  1X,130(1H*),///30X,A80,/30X,A80)
      RETURN
      END

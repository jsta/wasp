      SUBROUTINE CLRSCR
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:09.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
CC
C
      CHARACTER*4 STRING
C
      DATA STRING /'[2J'/
C
      WRITE (6,6000) STRING
 6000 FORMAT (1X,A4)
C
      RETURN
      END

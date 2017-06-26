      SUBROUTINE WEXIT(MESSAGE,ICOL)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:18.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
      INCLUDE 'WASP.CMN'
C
      CHARACTER*(*) MESSAGE
C
C-----------------------------------------------------------------------
      WISP=.FALSE.
      IF (MFLAG .LT. 2)CALL PROMPT(MESSAGE,ICOL)
      CALL CURSOR('ON')
      CALL SHQUIT (' ')
      STOP
      END

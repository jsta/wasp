      SUBROUTINE WEXIT(message,icol)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:18.
C
C  Correction History:
C
C----------------------------------------------------------------------
      INCLUDE 'DYNHYD.CMN'
      character*(*) message
C      wisp=.false.
      call prompt(message,icol)
      call cursor('ON')
      STOP
      END

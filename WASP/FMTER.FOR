      SUBROUTINE FMTER (CGROUP)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:42.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      CHARACTER*30 CGROUP
C
C-----------------------------------------------------------------------
      WRITE (OUT, 6000)
 6000 FORMAT(////10X,'DATA CARD ERROR ... WASP TERMINATED'/////)
      WRITE (OUT, 6010) CGROUP
 6010 FORMAT(12(/),17X,'========================================',/,
     1            22X,A30,/,
     2            17X,'========================================')
      CALL CLSCRN
      WRITE (MESS, 6010) CGROUP
      CALL WEXIT('User input error.',1)
      RETURN
      END

      SUBROUTINE SCALP (BVECT, SCALE, NO)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:25:44.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
      REAL BVECT (SG)
      DO 1000 I = 1, NO
         BVECT (I) = BVECT (I)*SCALE
 1000 CONTINUE
      RETURN
      END

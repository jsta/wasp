      SUBROUTINE WAS14 (ITRAK)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:06.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      REAL STEP (MB), END (MB)
C
C-----------------------------------------------------------------------
C          SUBROUTINE HANDLES VARIABLE TIME STEPS
C
      NOBRK = NBRK75 (1)
      DO 1000 I = 1, NOBRK
         II = 2*I - 1
         III = II + 1
         STEP (I) = FILE75 (II, 1)
         END (I) = FILE75 (III, 1)
 1000 CONTINUE
      IF (ITRAK .GT. NOBRK) GO TO 1010
      DT = STEP (ITRAK)
      TEND = END (ITRAK)
      RETURN
 1010 CONTINUE
      ITRAK = 0
      RETURN
      END

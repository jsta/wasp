      DOUBLE PRECISION FUNCTION DSPLY (AVAR, N, DX, CVAR)
C
      INCLUDE 'WASP.CMN'
C
      DOUBLE PRECISION AVAR (1), CVAR (1), DX
      INTEGER I, N, NX
      IF (N .LT. 1 .OR. N .GT. 100) GO TO 1000
      DSPLY = AVAR (N + 1)
      DO 1010 I = 1, N
         NX = N - I + 1
         DSPLY = DSPLY*(DX - CVAR (NX)) + AVAR (NX)
 1010 CONTINUE
      GO TO 1020
C
 1000 CONTINUE
      WRITE (OUT, 6000)
 6000   FORMAT(1X,'Error in Subroutine DSPLY.')
      DSPLY = 0.0D+00
 1020 CONTINUE
      RETURN
      END

      SUBROUTINE SCAL3D (B, NR, NC, NS, SCALE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:25:42.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      REAL B (NR, NC, NS)
C
C  CREATED 3/6/86
C  SUBROUTINE MULTIPLIES 3-D ARRAY BY SCALE
CX*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      DO 1000 I = 1, NR
         DO 1010 J = 1, NC
            DO 1020 K = 1, NS
               B (I, J, K) = B (I, J, K)*SCALE
 1020       CONTINUE
 1010    CONTINUE
 1000 CONTINUE
      RETURN
      END

      SUBROUTINE SETRB (A, NR, NC, NS, VALUE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:25:49.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      REAL A (NR, NC, NS)
C
C  CREATED 3/5/86
C  SUBROUTINE INITIALIZES A 3-D ARRAY TO VALUE.
CX*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      DO 1000 I = 1, NR
         DO 1000 J = 1, NC
            DO 1000 K = 1, NS
               A (I, J, K) = VALUE
 1000 CONTINUE
      RETURN
      END

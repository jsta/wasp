      SUBROUTINE SETIA (IARAY, NROW, NCOL, IVALUE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:25:45.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INTEGER IARAY (NROW, NCOL)
C
      DO 1000 I = 1, NROW
         DO 1000 J = 1, NCOL
            IARAY (I, J) = IVALUE
 1000 CONTINUE
      RETURN
      END

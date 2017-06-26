      SUBROUTINE SETCA (CARAY, NROW, NCOL, CVALUE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:25:44.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      CHARACTER*1 CARAY (NROW, NCOL), CVALUE
      DO 1000 I = 1, NROW
         DO 1000 J = 1, NCOL
            CARAY (I, J) = CVALUE
 1000 CONTINUE
      RETURN
      END

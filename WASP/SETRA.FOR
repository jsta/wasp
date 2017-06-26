      SUBROUTINE SETRA (ARRAY, NROW, NCOL, VALUE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:25:48.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      REAL ARRAY (NROW, NCOL)
      DO 1000 IROW = 1, NROW
         DO 1000 JCOL = 1, NCOL
            ARRAY (IROW, JCOL) = VALUE
 1000 CONTINUE
      RETURN
      END

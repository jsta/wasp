      SUBROUTINE DIVDIF (DC, DY, NP, DVD)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:23.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      DOUBLE PRECISION DC (1), DY (1), DVD (1), DIVS
      INTEGER J, N, NP, KP, KYCL
      IF (NP .LT. 2 .OR. NP .GT. 60) GO TO 1000
      DO 1010 J = 1, NP
         DVD (J) = DY (J)
 1010 CONTINUE
      N = NP - 1
      DO 1020 KYCL = 1, N
         KP = KYCL + 1
         DO 1030 J = KP, NP
            DIVS = DC (J) - DC (KYCL)
            IF (DABS (DIVS) .LT. 1.0D-14) GO TO 1040
            DVD (J) = (DVD (J) - DVD (KYCL))/DIVS
 1030    CONTINUE
 1020 CONTINUE
      GO TO 1050
 1000 CONTINUE
      WRITE (OUT, 6000) NP
      GO TO 1050
 1040 CONTINUE
      WRITE (OUT, 6010)
 6000   FORMAT(1X, I4, 21H POINTS INV IN DIVDIF)
 6010   FORMAT(1X, 14HTWO ARGS EQUAL)
 1050 CONTINUE
      RETURN
C       End of DIVDIF
      END

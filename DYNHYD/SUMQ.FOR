      SUBROUTINE SUMQ (YQ, YSECQ, VSQ, TQ, J)
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
c
      SUM = QIN (J)
      DO 1000 K = 1, CJ
         IF (NCHAN (J, K) .EQ. 0) GO TO 1010
         N = NCHAN (J, K)
         IF (J .EQ. NJUNC (N, 1)) SUM = SUM + Q(N)
         IF (J .EQ. NJUNC (N, 2)) SUM = SUM - Q(N)
 1000 CONTINUE
 1010 CONTINUE
      DVOL = - TQ*SUM
c      IF (SAN (J) .EQ. 0.) THEN
         DH = (DVOL/SURF (J))
c      ELSE
c         ROOT = YSECQ*SAN (J) + SURF (J)
c         DH = ( - ROOT + SQRT (ROOT**2. + 2.*SAN (J)*DVOL))/SAN (J)
c      END IF
      DH = DH + REVAP*TQ
c
      YQ = YSECQ + DH
c
      VSQ = surf(j) * (yq -belev(j))
      RETURN
      END

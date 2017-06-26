      SUBROUTINE REPORT
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
cia      LTAPE = ICYC
C4?      IDUMP  = IDUMP + ITDUMP
C********************   HYDRAULIC DATA PRINTOUTS   *********************
C
      WRITE (OUT, 6000) ICYC, TIME
      DO 1000 I = 1, NOPRT
         J = JPRT (I)
         DO 1010 K = 1, CJ
            N = NCHAN (J, K)
            IF (N .EQ. 0) GO TO 1000
            IF (J .EQ. NJUNC (N, 1)) THEN
               VOUT = V (N)
               FLOUT = Q (N)
               FMOM = MOM (N)
               FFRIC = FRIC (N)
               FGRAV = GRAV (N)
               FWIN = WIN (N)
            ELSE
               VOUT = - V (N)
               FLOUT = - Q (N)
               FMOM = - MOM (N)
               FFRIC = - FRIC (N)
               FGRAV = - GRAV (N)
               FWIN = - WIN (N)
            END IF
            IF (K .EQ. 1) WRITE (OUT, 6010) J, Y (J), N, VOUT, FLOUT,
     1         FMOM, FFRIC, FGRAV, FWIN
            IF (K .GT. 1) WRITE (OUT, 6020) N, VOUT, FLOUT, FMOM,
     1         FFRIC, FGRAV, FWIN
 1010    CONTINUE
 1000 CONTINUE
cia    LTAPE = ICYC
 6000 FORMAT (1H1,/5X,113(1H*),//40X,'HYDRAULIC CONDITIONS AT CYCLE ',
     1I5,6X,'TIME = ',F7.2,' HOURS',//5X,113(1H*),
     2//87X,'ACCELERATION TERMS'/69X,43(1H=),
     3/10X,'JUNCTION      HEAD         CHANNEL     VELOCITY     FLOW',
     4'     MOMENTUM      FRICTION     GRAVITY         WIND',
     5/5X,113(1H=))
 6010 FORMAT ( /12X,I3,7X,F7.3,9X,I3,7X,F7.3,2X,F9.1,3X,E10.4,3X,E10.4,
     13X,E10.4,3X,E10.4)
 6020 FORMAT (38X,I3,7X,F7.3,2X,F9.1,3X,E10.4,3X,E10.4,3X,E10.4,3X,
     1         E10.4)
      RETURN
      END

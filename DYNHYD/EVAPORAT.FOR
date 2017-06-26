      SUBROUTINE EVAPORAT
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C     CALCULATE THE EVAPORATION OR PRECIPITATION
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C
      IF (IRE .EQ. 0) THEN
         IRE = 1
         REVAP = 0.0
         READ (IN, 5000) HEADER
         READ (IN, 5010) NOEVA, SCALE, CONVE
         IF (NOEVA .GT. MQ) THEN
            WRITE (OUT, 6000) MQ
            STOP
         END IF
         IF (NOEVA .GT. 0) THEN
            READ (IN, 5020) (DAY (K), HR (K), MIN (K),
     1          EVAP (K), K = 1, NOEVA)
            IF (NOEVA .EQ. 1) THEN
               NOEVA = 2
               DAY (2) = 365.
               HR (2) = 0.
               MIN (2) = 0.
               EVAP (2) = EVAP (1)
            END IF
            WRITE (OUT, 6010)
            DO 1000 K = 1, NOEVA
               IDY = DAY (K)
               IH = HR (K)
               IM = MIN (K)
               EVAP (K) = EVAP (K)*SCALE*CONVE
               WRITE (OUT, 6020) IDY, IH, IM, EVAP (K)
 1000       CONTINUE
C
C CONVERT TIME TO SECONDS
C
            DO 1010 K = 1, NOEVA
               ETIM (K) = 86400.*(DAY (K) - 1.) + 3600.*
     1            HR (K) + 60.*MIN (K)
               IF (ETIM (K) .LE. TZ) IEV = K
 1010       CONTINUE
C
            DETEV = EVAP (IEV + 1) - EVAP (IEV)
            DETT = ETIM (IEV + 1) - ETIM (IEV)
            ESLOP = DETEV/DETT
            EDTIM = ETIM (NOEVA) - ETIM (1)
            EREPT = 0.0
         END IF
      END IF
C
C LINEAR INTERPLATE THE PRECIPITATION OR EVAPORATION DATA...............
C
      IF (NOEVA .GT. 0) THEN
         TNEXT = EREPT*EDTIM + ETIM (IEV + 1)
         IF (T .GT. TNEXT) THEN
            IF (IEV + 1 .LE. NOEVA) THEN
               IEV = IEV + 1
               DETEV = EVAP (IEV + 1) - EVAP (IEV)
               DETT = ETIM (IEV + 1) - ETIM (IEV)
               ESLOP = DETEV/DETT
            ELSE
               IEV = 1
               EREPT = EREPT + 1.0
               DETEV = EVAP (2) - EVAP (1)
               DETT = ETIM (2) - ETIM (1)
               ESLOP = DETEV/DETT
            END IF
         END IF
C
         TTT = T - (EREPT*EDTIM + ETIM (IEV))
         REVAP = EVAP (IEV) + ESLOP*TTT
      END IF
C
C FORMAT BLOCK.......................................................
 5000       FORMAT(20A4)
 5010       FORMAT(I5,2E10.4)
 6000       FORMAT(5X,'ERROR, DATA POINTS OF PRECIPITATION OR
     1                 EVAPORATION EXCEEDS',I5)
 5020   FORMAT(4(F5.0,1X,2F2.0, F10.0))
 6010   FORMAT(1H1//1X,130(1H*)//55X,'SUMMARY OF PRECIPITION'
     1  //1X,130(1H*)///40X,'  DAY ',10X,'TIME ',10X,'PRECPITATION(m/s)'
     2  //40X,57(1H=)///)
 6020   FORMAT(40X,I5,10X,I2,':',I2,10X,E15.4)
      RETURN
      END

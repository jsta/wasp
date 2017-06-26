      SUBROUTINE SEAWRD
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C     PERIOD = LENGTH OF THE TIDAL PERIOD, IN HOURS
C     A1(J,I)  = VALUE OF THE I-TH TIDAL COEFFICIENT (I=1,7) FOR
C                SEAWARD BOUNDARY J (J=1,NSEA)
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C
      NK = 0
C
C***********************************************************************
C
      READ (IN, 5000) HEADER
      READ (IN, 5010) NSEA
      WRITE (OUT, 6000)
      IF (NSEA .EQ. 0) THEN
C
C       DOWNSTREAM FLOW BOUNDARY
C
         WRITE (OUT, 6010)
C
      ELSE
C
C       DOWNSTREAM HEAD BOUNDARIES
C
         IF (NSEA .GT. SB) THEN
            WRITE (OUT, 6020)
 6020    FORMAT('ERROR. NUMBER OF SEAWARD BOUNDARIES',
     1          ' EXCEEDS MODEL CAPACITY')
            STOP
         END IF
cia boundary
         DO 1000 J = 1, NSEA
            READ (IN, 5010) SEAOPT, juns(j), NDATA, MAXIT,
     1         MAXRES, TSHIFT, PSHIFT, YSCALE
            IF (SEAOPT .EQ. 2) THEN
C              CALCULATE COEFFICIENTS FOR AVERAGE TIDES:
               CALL REGAN (J)
               PERIOD (J) = PERIOD (J)*3600.0
               TSTART (J) = TSTART (J)*3600.0
               W (J) = W (J)/3600.0
            END IF
            IF (SEAOPT .EQ. 1) THEN
C                 GIVE COEFFICIENTS FOR AVERAGE TIDE:
               READ (IN, 5020) PERIOD (J), TSTART (J)
               NK = 7
               IF (PERIOD (J) .EQ. 0.0) PERIOD (J) = 24.0
               NTIDES = 0.9999*(TEND - TZERO)/PERIOD (J) + 1
               WRITE (OUT, 6030) NSEA, PERIOD (J), TSTART (J), NTIDES
               PERIOD (J) = PERIOD (J)*3600.0
               TSTART (J) = TSTART (J)*3600.0
               W (J) = 2.*3.1416/PERIOD (J)
               READ (IN, 5030) (A1 (J, I), I = 1, NK)
cia boundary
             WRITE(OUT, 6040)juns(j), A1 (J, 1), (A1 (J, I), I = 1, NK)
            END IF
            IF (SEAOPT .EQ. 3) THEN
C              FIT SINUSOIDAL CURVE TO VARIABLE TIDE DATA:
               NTS = NDATA
               IF (NTS .GT. TC2) THEN
                  WRITE (OUT, 6050)
 6050          FORMAT ('ERROR.  TOO MANY TIDAL CYCLES REQUESTED')
                  STOP
               END IF
               READ (IN, 5040) (DAY (L), HR (L), MIN (L),
     1            BHEAD (J, L), L = 1, NTS)
               WRITE (OUT, 6060)
               DO 1010 L = 1, NTS
                  PDAY = DAY (L)
                  PHR = HR (L)
                  PMIN = MIN (L)
                  BHEAD (J, L) = BHEAD (J, L)*YSCALE
                  IF (PMIN .GE. 10) THEN
                     WRITE (OUT, 6070) PDAY, PHR, PMIN, BHEAD (J, L)
                  ELSE
                     WRITE (OUT, 6080) PDAY, PHR, PMIN, BHEAD (J, L)
                  END IF
 1010          CONTINUE
               NHCYC (J) = 0
               DO 1020 L = 1, NTS
                  BTIME (J, L) = 86400.*(DAY (L) - 1.0) + 3600.*HR (L)
     1               + 60.*MIN (L)
                  IF (BTIME (J, L) .LE. TZ) NHCYC (J) = L
 1020          CONTINUE
               L = NHCYC (J)
               PERIOD (J) = 2.0*(BTIME (J, L + 1) - BTIME (J, L))
               RANGE (J) = BHEAD (J, L + 1) - BHEAD (J, L)
               DTIME (J) = BTIME (J, NTS) - BTIME (J, 1)
               TREP (J) = 0.0
               IF (NHCYC (J) .EQ. 0) THEN
                  WRITE (OUT, 6090)
                  STOP
               END IF
            END IF
            IF (SEAOPT .EQ. 4) THEN
C  Tidal level vs. Time With Linear Interpolatation and repead from the
cia   the begining when simulation exceeds the available dada.....
               NTS = NDATA
               IF (NTS .GT. TC2) THEN
                  WRITE (OUT, 6250)
 6250          FORMAT ('ERROR.  TOO MANY TIDAL POINTS REQUESTED')
                  STOP
               END IF
               READ (IN, 5040) (DAY (L), HR (L), MIN (L),
     1            BHEAD (J, L), L = 1, NTS)
               WRITE (OUT, 6060)
               DO 2010 L = 1, NTS
                  PDAY = DAY (L)
                  PHR = HR (L)
                  PMIN = MIN (L)
                  BHEAD (J, L) = BHEAD (J, L)*YSCALE
                  IF (PMIN .GE. 10) THEN
                     WRITE (OUT, 6070) PDAY, PHR, PMIN, BHEAD (J, L)
                  ELSE
                     WRITE (OUT, 6080) PDAY, PHR, PMIN, BHEAD (J, L)
                  END IF
 2010          CONTINUE
               NHCYC (J) = 0
               DO 2020 L = 1, NTS
                  BTIME (J, L) = 86400.*(DAY (L) - 1.0) + 3600.*HR (L)
     1               + 60.*MIN (L)
 2020          CONTINUE
               if ( btime(j, 1) .gt. tz )then
                  write(out, 4321)
4321              format(5x, ' error, simulation is earlier than the',
     -                 ' available tidal data.')
                  stop
               end if
               L = NHCYC (J)
               DTIME (J) = BTIME (J, NTS) - BTIME (J, 1)
               TREP (J) = 0.0
            END IF
 1000    CONTINUE
      END IF
Cia
      NS = NK/2
      NINL = NSEA + 1
C
C***********************************************************************
C                           FORMAT STATEMENTS
C***********************************************************************
C
 5000 FORMAT (20A4)
 5020 FORMAT (2F10.0,I5)
 5010 FORMAT (4I5,9F5.0)
 5030 FORMAT (8F10.0)
 5040 FORMAT (4(F5.0,1X,2F2.0,F10.0))
 6000 FORMAT(1H1//1X,130(1H*),//54X,'DOWNSTREAM BOUNDARY CONDITIONS',
     1//1X,130(1H*))
 6010 FORMAT (1H ,/////48X,33(1H*),//48X,'NO DOWNSTREAM
     1HEADS ARE SPECIFIED',
     2//41X,'THE DOWNSTREAM BOUNDARY IS CONTROLLED BY OUTFLOWS',
     3//48X,33(1H*))
 6030 FORMAT (1H ,////41X,'DOWNSTREAM HEADS ARE SPECIFIED AT ',I3,
     1' BOUNDARIES',//41X,'THE (TIDAL) PERIOD IS ',F7.2,' HOURS',
     2 //41X,'BEGINNING AT HOUR',F5.2,//41X,
     3'THIS SIMULATION INCLUDES',I3,' (TIDAL) PERIODS',//41X,47(1H=))
 6040 FORMAT (1H ,/////51X,'AT DOWNSTREAM JUNCTION',I3,//51X,'THE MEAN',
     1' HEAD IS',F9.6,' Meters',//41X,47(1H=)/////44X,
     2'AVERAGE INTRATIDAL'
     3,' VARIABILITY IS GIVEN BY:',//54X,'HEAD = ',F9.6,//60X,'+ ',F9.6,
     4' * SIN(WT)',//60X,'+ ',F9.6,' * SIN(2WT)',//60X,'+ ',F9.6,
     5' * SIN(3WT)',//60X,'+ ',F9.6,' * COS(WT)',//60X,'+ ',F9.6,
     6' * COS(2WT)',//60X,'+ ',F9.6,' * COS(3WT)',//41X,47(1H=))
 8000 FORMAT (1H ,////15X,'THIS INTERTIDAL VARIABILITY IS REPEATED',
     1I3,'AND',I3,'/',I3,'TIMES DURING THIS',I3,'TIDAL CYCLE
     2  SIMULATION')
 6090 FORMAT(1H0//24X,'TZERO IS LESS THAN THE FIRST TIDAL CYCLE'/
     120X,'SPECIFY TIDAL HEIGHTS PRIOR TO TZERO AND TRY AGAIN')
 6060 FORMAT (////49X,41(1H=),//,50X,'SUMMARY OF VARIABLE BOUNDARY CONDI
     1TIONS',//49X,41(1H=),///50X,'DAY',12X,'TIME',
     212X,'HEAD(FT)'//50X,3(1H=),12X,4(1H=),12X,8(1H=))
 6070 FORMAT (48X,I4,12X,I2,':',I2,8X,F10.2)
 6080 FORMAT (48X,I4,12X,I2,':0',I1,8X,F10.2)
      RETURN
      END

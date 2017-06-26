      SUBROUTINE WIND
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C     THIS PROGRAM READS WIND DATA (SPEED, DIRECTION) AND
C       CALCULATES THE WIND COMPONNENT FOR EACH TIME
C     STEP.
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C---- LOCAL VARIABLES
      INTEGER IW
      REAL WSLOPA, WSLOPS
      REAL TREPW
      REAL DTIMW
      IF (IREADW .EQ. 0) THEN
         IREADW = 1
         READ (IN, 5000) HEADER
         READ (IN, 5010) NOBSW
         IF (NOBSW .GT. MQ) THEN
            WRITE (OUT, 6000)
 6000    FORMAT('ERROR. NUMBER OF WIND DATA POINTS',
     1          ' EXCEEDS MODEL CAPACITY')
            STOP
         END IF
         IF (NOBSW .GT. 0) THEN
            READ (IN, 5020) (DAY (K), HR (K), MIN (K)
     1         , WINDS (K), WDIR (K),
     2         K = 1, NOBSW)
            IF (NOBSW .EQ. 1) THEN
               NOBSW = 2
               DAY (2) = 3650.
               HR (2) = 0.
               MIN (2) = 0.
               WINDS (2) = WINDS (1)
               WDIR (2) = WDIR (1)
            END IF
            WRITE (OUT, 6010)
            DO 1000 K = 1, NOBSW
C ------------ convert real DAY, HR, and MIN to integer for output.
               pDAY = DAY (K)
               pHR = HR (K)
               pMIN = MIN (K)
               WRITE (OUT, 6020) pDAY, pHR, pMIN, WINDS (K), WDIR (K)
 1000       CONTINUE
            DO 1010 K = 1, NOBSW
C ------------ convert time to seconds
               WTIM (K) = 86400.*(DAY (K) - 1) +
     1            3600.*HR (K) + 60.*MIN (K)
C ------------ matching wind cycle to initial time
               IF (WTIM (K) .LE. TZ) IW = K
 1010       CONTINUE
            DO 1020 N = 1, NC
               CDIR (N) = CDIR (N)*3.1416/180.
 1020       CONTINUE
            WDELTS = WINDS (IW + 1) - WINDS (IW)
            WDELTA = WDIR (IW + 1) - WDIR (IW)
            WDELTT = WTIM (IW + 1) - WTIM (IW)
            WSLOPS = WDELTS/WDELTT
            WSLOPA = WDELTA/WDELTT
            TREPW = 0.0
            DTIMW = WTIM (NOBSW) - WTIM (1)
         ELSE
C -------- condition of no wind forces
            DO 1030 N = 1, NC
               FW (N) = 0.
 1030       CONTINUE
         END IF
      ELSE
C*****************************************************************
C         LINEAR INTERPOLATION TO UPDATE WIND
C*****************************************************************
         IF (NOBSW .GT. 0) THEN
            TNEXTW = TREPW*DTIMW + WTIM (IW + 1)
            IF (T .GE. TNEXTW) THEN
               IF (IW + 1 .LE. NOBSW) THEN
                  IW = IW + 1
                  WDELTS = WINDS (IW + 1) - WINDS (IW)
                  WDELTA = WDIR (IW + 1) - WDIR (IW)
                  WDELTT = WTIM (IW + 1) - WTIM (IW)
                  WSLOPS = WDELTS/WDELTT
                  WSLOPA = WDELTA/WDELTT
               ELSE
C -------------- starting wind cycle over
                  IW = 1
                  TREPW = TREPW + 1.0
                  WDELTS = WINDS (2) - WINDS (1)
                  WDELTA = WDIR (2) - WDIR (1)
                  WDELTT = WTIM (2) - WTIM (1)
                  WSLOPS = WDELTS/WDELTT
                  WSLOPA = WDELTA/WDELTT
               END IF
            END IF
            TTIMW = T - (TREPW*DTIMW + WTIM (IW))
C --------- linear equations for calculating wind speed and direction
            WINDL = WINDS (IW) + WSLOPS*(TTIMW)
            WINDA = WDIR (IW) + WSLOPA*(TTIMW)
            WINDA = WINDA*3.1416/180.
C    *****CALCULATING THE WIND ACCELERATIONAL FORCE***********
            DO 1040 N = 1, NC
               WRSQ = WINDL**2 + V (N)**2 -
     1            (2.*WINDL*V (N)*COS (CDIR (N) - WINDA))
               IF (V (N) .EQ. 0.0) V (N) = 1E-06
               WINDX = WINDL*SIN (WINDA) - V (N)*SIN (CDIR (N))
               WINDYL = WINDL*COS (WINDA) - V (N)*COS (CDIR (N))
               WANGL = ATAN2 (WINDX, WINDYL)
               FW (N) = (0.0026/R (N))*(1.165E-03)*
     1            WRSQ*COS (WANGL - CDIR (N))
 1040       CONTINUE
         END IF
      END IF
C****************************************************************
C                     FORMAT STATEMENTS
C****************************************************************
C
 5000 FORMAT (20A4)
 5010 FORMAT (I5)
 5020 FORMAT (4(F5.0,1X,2F2.0,F5.0,F5.0))
 6010 FORMAT (1H1,//1X,130(1H*),//,55X,'SUMMARY OF WIND CONDITIONS',
     1        //1X,130(1H*),///40X,'DAY',10X,'TIME',8X,'WIND SPEED',
     2        8X,'WIND DIRECTION'//40X,57(1H=)///)
 6020 FORMAT (40X,I4,8X,I2,':',I2,9X,F5.0,15X,F5.0)
      RETURN
      END

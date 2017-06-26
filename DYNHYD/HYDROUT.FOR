      SUBROUTINE HYDROUT
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C        SUBROUTINE SUMRY SUMMARIZES AND SAVES A RECORD OF THE
C        HYDRAULIC CONDITIONS FOR THE SIMULATION.  HYDRAULIC
C        PARAMETERS ARE SAVED WITH A FREQUENCY DEPENDENT ON
C        THE LENGTHS OF THE HYDRAULIC TIME STEP AND THE TIME
C        STEP USED IN THE WATER QUALITY MODEL ACCESSING THE
C        STORED HYDRAULIC DATA.  THE PARAMETERS STORED FOR
C        USE BY THE QUALITY MODEL ARE ...
C             - JUNCTION VOLUMES
C             - CHANNEL FLOWS, VELOCITIES, AND DEPTHS
C
C***********************************************************************
C                        CONTROL OPTIONS
C***********************************************************************
C
C        NODYN = NUMBER OF HYDRAULIC CYCLES PER QUALITY CYCLE
C        NOPTA = PARAMETER SPECIFYING THE TRAPEZOIDAL RULE
C             IN SUBROUTINE MEAN IS TO BE USED TO COMPUTE
C             INTER-TIDAL CYCLE PARAMETERS
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C--- Local variables
      DIMENSION IC(100)
      REAL AVGFLO, AVGVEL, AVGDEP
      INTEGER K1, K2
      INTEGER NOPTA
C  arbitrarily chose NOPTA=2 (TRAPEZOID METHOD in MEAN)
      DATA NOPTA/2/
C
C =====================================================================
C
      IF (INIT .EQ. 0) THEN
C
C            OPEN the Hydrodynamic Interface File
C
         OPEN (UNIT = SUMY, FILE = hydfil, STATUS = 'UNKNOWN',
     1      FORM = 'FORMATTED')
C
         NREAD = NOSCR + 1
         DELTQ = DELT * FLOAT(NODYN)
C
C      Read the Dynhyd Junction to WASP Segment Mapping
C
C INITIALIZE JUNSEG
         DO 500 I=1,NJ
            JUNSEG(I) = 999
500      CONTINUE
C
         CALL GETMES (19, 0)
         READ (IN, 5000) ALPHA

C READ IN JUNCTION/WASP4 SEGMENT MAP
C
           READ (IN, 5010) HDEPVEL, NPAIR
           IF (NPAIR .GT. 0) THEN
              DO 600 I=1,NPAIR
                 READ (IN, 5010) K,JUNSEG (K)
600           CONTINUE
           ENDIF
5010       FORMAT(2I5)
CC
C COUNT NUMBER OF SEGMENTS AND CHECK FOR CONTINUITY
         NSEG = 0
        DO 1000 I = 1, NJ
         IF (JUNSEG (I).NE.0.AND.JUNSEG(I).NE.999)THEN
            NSEG = NSEG + 1
               DO 900 KK=1,CJ
                 MM = NCHAN(I,KK)
                 IF(MM.GT.0)THEN
                   K1 = JUNSEG(NJUNC (MM, 1))
                   K2 = JUNSEG (NJUNC (MM, 2))
                   IF(K1.EQ.999.OR.K2.EQ.999)WRITE(OUT,800)MM
                 END IF
900           CONTINUE
        END IF
1000   CONTINUE
800    FORMAT(//2X,'WARNING!!:WASP4 CONTINUITY WILL NOT BE MAINTAINED',
     * /2X,'CHANNEL ',I5,' IS NOT INLCUDED IN WASP4 MAPING')

C
C COUNT NUMBER OF CHANNELS AND MAP
          NC1 = 0
          DO 700 I=1,NC
            K1 = JUNSEG (NJUNC (I, 1))
            K2 = JUNSEG (NJUNC (I, 2))
            IF(K1.NE.999.AND.K2.NE.999)THEN
               K = K1 + K2
                IF(K.GT.0) THEN
                  NC1 = NC1 + 1
                  IC(NC1) = I
                END IF
            END IF
700       CONTINUE
C
C Write the Initial Header Information for Hydrodynamic Interface File
C This is only written once.
C
         WRITE (SUMY, 6000) NSEG, NC1, DELTQ, TTAPE, TEND, HDEPVEL
 6000    FORMAT(2(I5),3(F20.5),I5)
C
         DO 1010 I = 1, NC1
            K = IC(I)
            K1 = JUNSEG (NJUNC (K, 1))
            K2 = JUNSEG (NJUNC (K, 2))
            WRITE (SUMY, 6010) K1, K2
1010    CONTINUE
6010      FORMAT(2(I5))
C
C
      END IF
C
C ========== End Hydrodynamic Interface File Initialization ===========
C
C
      CALL GETMES (17, 0)
      REWIND SCR
      READ (SCR, 5020) CYCLE
 5020 FORMAT(I10)
      READ (SCR, 5030) (VOL (J), J = 1, NJ)
 5030 FORMAT(E13.3)
      READ (SCR, 5040) (V (N), Q (N), R (N), N = 1, NC)
 5040 FORMAT (3(E13.3))
      DO 1020 N = 1, NC
         VEL (N, 1) = V (N)
         FLO (N, 1) = Q (N)
         DEP (N, 1) = R (N)
         VSAVE (N) = V (N)
         QSAVE (N) = Q (N)
         RSAVE (N) = R (N)
 1020 CONTINUE
C
C STORE INITIAL VOLUMES IN PVOL(J)
C
      DO 1030 J = 1, NJ
         PVOL (J) = VOL (J)
 1030 CONTINUE
C
C ================= Store Initial Data on First Pass ==================
C
C
      IF (INIT .EQ. 0 .AND. HDEPVEL .EQ. 0) THEN
         init=1
         CALL AVGSG
         DO 1040 I = 1, Nj
            DO 1050 J = 1, NJ
               IF (JUNSEG (J) .EQ. I) THEN
                  ddpptt = depthg(j)
                  WRITE (SUMY, 6020) VOL (J), FLOWG (J), ddpptt,
     1               VELOCG (J)
               END IF
 1050       CONTINUE
 1040    CONTINUE
      END IF
C
C =====================================================================
C
C                  Continue Processing Scratch File
C =====================================================================
 1060 CONTINUE
      DO 1070 K = 2, NREAD
         READ (SCR, 5020) CYCLE
         READ (SCR, 5030) (VOL (J), J = 1, NJ)
         READ (SCR, 5040) (V (N), Q (N), R (N), N = 1, NC)
         DO 1080 N = 1, NC
            FLO (N, K) = Q (N)
            VEL (N, K) = V (N)
            DEP (N, K) = R (N)
 1080    CONTINUE
 1070 CONTINUE
C
      NODATA = NREAD
C
C ============= Compute Avg. Flows, Velocities, and Depths ============
C
      DO 1090 N = 1, NC
         CALL MEAN (FLO, N, NC, NODATA, NOPTA, AVGFLO)
         CALL MEAN (VEL, N, NC, NODATA, NOPTA, AVGVEL)
         CALL MEAN (DEP, N, NC, NODATA, NOPTA, AVGDEP)
         QSAVE (N) = AVGFLO
         VSAVE (N) = AVGVEL
         RSAVE (N) = AVGDEP
 1090 CONTINUE
C
      CALL AVGSG
C
C INITIAL DATA FOR HDEPVEL=1
C
      IF (INIT .EQ. 0 .AND. HDEPVEL .NE. 0) THEN
         INIT = 1
         DO 1100 I = 1, NSEG
            DO 1110 J = 1, NJ
               IF (JUNSEG (J) .EQ. I) THEN
                  ddpptt = depthg(j)
                  WRITE (SUMY, 6030) FLOWG (J), ddpptt, VELOCG (J)
               END IF
 1110       CONTINUE
 1100    CONTINUE
C
C INITIAL VOLUMES
         DO 1120 I = 1, NSEG
            DO 1130 J = 1, NJ
               IF (JUNSEG (J) .EQ. I) THEN
                  WRITE (SUMY, 6040) PVOL (J)
               END IF
 1130       CONTINUE
 1120    CONTINUE
      END IF
 6030   FORMAT(3F20.5)
C
C
C*************   STORE AVERAGED CHANNEL FLOWS  *************************
C
      WRITE (SUMY, 6040) (QSAVE (IC(I)), I = 1, NC1)
 6040   FORMAT(F20.5)
C
C*************   STORE AVG SEGMENT VOL, DEPTH, VELOC AND FLOWS *********
C
C
      DO 1140 I = 1, NSEG
         DO 1150 J = 1, NJ
            IF (JUNSEG (J) .EQ. I) THEN
               IF (HDEPVEL .EQ. 0) THEN
                  ddpptt = depthg(j)
                  WRITE (SUMY, 6020) VOL (J), FLOWG (J), ddpptt,
     1               VELOCG (J)
               ELSE
                  WRITE (SUMY, 6040) VOL (J)
               END IF
            END IF
 1150    CONTINUE
 1140 CONTINUE
C
C******************   RE-INITIALIZE SUMMARY ARRAYS     *****************
C
      DO 1160 N = 1, NC
         FLO (N, 1) = Q (N)
         VEL (N, 1) = V (N)
         DEP (N, 1) = R (N)
 1160 CONTINUE
C
      IF (CYCLE .LT. LTAPE) GO TO 1060
      CALL WSCREEN
      CALL WLABELS
C
C******************   PRINT HYDRAULIC SUMMARY DATA   *******************
C
      WRITE (OUT, 6050) ITAPE, LTAPE, DELT, NODYN, DELTQ
C
C***********************************************************************
C                            FORMAT STATEMENTS
C***********************************************************************
C
 6020 FORMAT(4F20.5)
 6050 FORMAT (//40X,'HYDRAULIC SUMMARY BEGINS AT CYCLE',I6,//40X,'HYDRAU
     1LIC SUMMARY  ENDS  AT CYCLE',I6//40X,'LENGTH OF A HYDRAULIC TIME S
     2TEP =',F8.2,'  SECONDS'//40X,'NUMBER OF HYDRAULIC TIME STEPS'/40X,
     3'PER QUALITY TIME STEP (NODYN)      =',I8//40X,'LENGTH OF A QUALIT
     4,Y TIME STEP   =',F8.2,' HOURS')
 5000 FORMAT (20A4)
      RETURN
      END

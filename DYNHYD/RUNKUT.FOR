      SUBROUTINE RUNKUT
C======================================================================
C     Last Revised:  Date: Monday, 2 November 1987.  Time: 07:29:01.
C
C        THIS SUBROUTINE SOLVES THE EQUATIONS OF CONTINUITY
C        AND MOMENTUM USING A MODIFIED RUNGE-KUTTA TECHNIQUE
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C
C
C
C
C **************************************************************
C * STEP I:  COMPUTE CHANNEL VELOCITY AND FLOW FOR 1/2 TIME    *
C *          STEP  USING INFORMATION FROM PREVIOUS TIME STEP   *
C **************************************************************
C
C          ###########################################
C          #  A) SOLVE MOMENTUM EQUATION IN CHANNELS #
C          ###########################################
C
      DO 1000 N = 1, NC
C
C 1ST:   DETERMINE UPSTREAM (NL) AND DOWNSTREAM (NH) JUNCTONS
C        ----------------------------------------------------
C
         NL = NJUNC (N, 1)
         NH = NJUNC (N, 2)
C
C 2ND:  COMPUTE THE FRICTION COEFICIENT
C       -------------------------------
         KT = AK (N)/R(N)**(4./3.)
C
C
C 3RD:   COMPUTE: 1) DYT AS THE AVERAGE OF THE CHANGE IN WATER SURFACE
C                   ELEVATIONS  OF THE JUNCTIONS AT THE BEGINING AND END
C                   OF THE TIME STEP
C                 2) DYX AS THE AVERAGE CHANGE IN THE WATER SURFACE
C                    OF THE JUNCTIONS AT THE BEGINNING OF THE TIME
C                    STEP
C                 3) DR AS THE THE AVERAGE CHANGE IN THE HYDRAULIC
C                    RADIUS OF THE JUNCTIONS AT THE BEGINNING OF THE TIME
C                    STEP (COMPUTED ASSUMING R = DEPTH)

C        -------------------------------------------------------------------
C
         dyt = (Y (NH) - YT (NH) + Y (NL) - YT (NL))/2.
         dyx = (y (NH) - y (NL))
         dr = (y(nh) - belev(nh))- (y(nl) - belev(nl))
C
C
C 4TH:   COMPUTE DERIVATIVES
C        -------------------
C        (NOTE: Y = HEAD, R = HYDRAULIC RADIUS COMPUTED ASSUMING
C         R = DEPTH FOR A WIDE RECTANGULAR CHANNEL)
C
         DYDT = dyt/DT2
         DYDX = dyx/CLEN (N)
         drdx = dr/clen (n)
         DVDX = - (DYDT + V (N)*drdx)/R (N)
         DVDT1 = - V (N)*DVDX
         DVDT2 = - KT*V (N)*ABS (V (N))
         DVDT3 = - G*DYDX
         DVDT4 = FW (N)
         DVDT = DT2*(DVDT1 + DVDT2 + DVDT3 + DVDT4)
C
C 5TH:  COMPUTE VELOCITY AND FLOW
C       -------------------------
C
         VT (N) = V (N) + DVDT
         Q (N) = VT (N)*AREA (N)
C
 1000 CONTINUE


C
C          ##############################################
C          #  B) SOLVE CONTINUITY EQUATION AT JUNCTIONS #
C          #     USING NEW FLOWS AT THE 1/2 TIME STEP   #
C          ##############################################
C
C
C 1ST:   COMPUTE SEAWARD BOUNDARY ELEVATIONS (IF SPEFICIED, NSEA>0):
C        -----------------------------------------------------------
C               a) IF SEAOPT < 3 THEN EITHER THE TIDE COEFFICIENTS
C                  WERE SPECIFIED (SEAOPT=1) OR COMPUTED BY REGAN
C                  (SEAOPT = 2)
C                  ------------------------------------------------
C               b) IF SEAOPT = 3 THEN THE ELEVATION IS INTERPOLATED
C                  BETWEEN HIGH AND LOW TIDE ELEVATIONS WITH A
C                  SINUSOIDAL CURVE
C                  ------------------------------------------------
C
        IF (NSEA .GT. 0) THEN
         DO 1010 J = 1, NSEA
            JJ = JUNS(J)
            IF (SEAOPT .LT. 3) THEN
               YT (jj) = A1 (J, 1)
               NS = 3
               DO 1020 I = 1, NS
                  F = FLOAT (I)
                  K = I + 1
                  YT (JJ) = YT (JJ) + A1 (J, K)*
     1               SIN (F*W (J)*(T2 - TSTART (J)))
     2               + A1 (J, NS + K)*
     3               COS (F*W (J)*(T2 - TSTART (J)))
 1020          CONTINUE
            ELSE
               L = NHCYC (J)
               TNEXTC = TREP (J)*DTIME (J) + BTIME (J, L + 1)
               IF (T2 .GE. TNEXTC) THEN
                  IF (L + 1 .LE. NDATA) THEN
                     NHCYC (J) = L + 1
                     L = NHCYC (J)
                     RANGE (J) = BHEAD (J, L + 1) - BHEAD (J, L)
                     PERIOD (J) = 2.*(BTIME (J, L + 1) - BTIME (J, L))
                  ELSE
                     TREP (J) = TREP (J) + 1.0
                     NHCYC (J) = 1
                     RANGE (J) = BHEAD (J, 2) - BHEAD (J, 1)
                     PERIOD (J) = 2.*(BTIME (J, 2) - BTIME (J, 1))
                     NHCYC (J) = 1
                     L = 1
                  END IF
               END IF
                TTIME (J) = T2 - (BTIME (J, L) + DTIME (J)*TREP (J))
                ARG = 2.*3.1416*TTIME (J)/PERIOD (J) + 3.1416
                YT (JJ) = BHEAD (J, L) + 0.5*RANGE (J)*(1. + COS (ARG))
            END IF
 1010    CONTINUE
        END IF
C
C     2ND: COMPUTE JUNCTION HEAD DUE TO FLOW BOUNDARY CONDITIONS AND
C          AT INTERNAL JUNCTIONS
C          ---------------------------------------------------------
C
          DO 1030 J = 1, NJ

C              a)  BYPASS COMPUTATION IF SEAWARD BOUNDARY
C                  (HEAD SPECIFIED)
C                  --------------------------------------
C
        IF (NSEA .GT. 0)THEN
            DO 8008 K = 1, NSEA
               IF(J.EQ. JUNS(K))GOTO 1030
8008        CONTINUE
        END IF
C
C              b) ADD FLOWS INTO AND OUT OF THE JUNCTION
C                 AND SOLVE FOR SURFACE ELEVATION.  CORRECT
C                 ELEVATION FOR EVAPORATION
C                 -----------------------------------------
C
         SUMQ = QIN (J)
         DO 1040 K = 1, CJ
            IF (NCHAN (J, K) .EQ. 0) GO TO 1050
            N = NCHAN (J, K)
            IF (J .EQ. NJUNC (N, 1)) SUMQ = SUMQ + Q (N)
            IF (J .EQ. NJUNC (N, 2)) SUMQ = SUMQ - Q (N)
 1040    CONTINUE
 1050    CONTINUE
         YT (J) = Y (J) - (DT2*SUMQ/SURF (J))
         YT (J) = YT (J) + REVAP*DT2

 1030 CONTINUE
C


C
C **************************************************************
C * STEP II: COMPUTE CHANNEL VELOCITY AND FLOW FOR FULL TIME   *
C *          STEP  USING INFORMATION FROM PREVIOUS TIME STEP   *
C **************************************************************
C
C          ###########################################
C          #  A) SOLVE MOMENTUM EQUATION IN CHANNELS #
C          ###########################################
C
C
      DO 1070 N = 1, NC

C
C 1ST:   DETERMINE UPSTREAM (NL) AND DOWNSTREAM (NH) JUNCTONS
C        ----------------------------------------------------
C
         NL = NJUNC (N, 1)
         NH = NJUNC (N, 2)
C
C
C 2ND:  COMPUTE HYDRAULIC RADIUS, THE FRICTION COEFFICIENT,
C       AND THE AREA AT THE 1/2 TIME STEP
C       --------------------------------------------------
C
         R(N) = ( YT(NH) + YT(NL) - BELEV(NH) - BELEV(NL) )/2.0
         KT = AK (N)/(R (N)**(4./3.))
         AREAT (N) = B (N)*R (N)
C
C
C 3RD:   COMPUTE: 1) DYT AS THE AVERAGE OF THE CHANGE IN WATER SURFACE
C                   ELEVATIONS  OF THE JUNCTIONS AT THE BEGINING AND END
C                   OF THE TIME STEP
C                 2) DYX AS THE AVERAGE CHANGE IN THE WATER SURFACE
C                    OF THE JUNCTIONS AT THE BEGINNING OF THE TIME
C                    STEP
C                 3) DR AS THE THE AVERAGE CHANGE IN THE HYDRAULIC
C                    RADIUS OF THE JUNCTIONS AT THE BEGINNING OF THE TIME
C                    STEP (COMPUTED ASSUMING R = DEPTH)

C        -------------------------------------------------------------------
C
         dyt = (YT (NH) - Y (NH) + YT (NL) - Y (NL))/2.
         dyx = (yt (NH) - yt (NL))
         dr = (yt(nh) - belev(nh))- (yt(nl) - belev(nl))
C
C
C 4TH:   COMPUTE DERIVATIVES
C        -------------------
C        (NOTE: Y = HEAD, R = HYDRAULIC RADIUS COMPUTED ASSUMING
C         R = DEPTH FOR A WIDE RECTANGULAR CHANNEL)
C
         DYDT = dyt/DT2
         DYDX = dyx/CLEN (N)
         drdx = dr/clen (n)
         DVDX = - (DYDT + V (N)*drdx)/R (N)
         DVDT1 = - VT (N)*DVDX
         DVDT2 = - KT*VT (N)*ABS (VT (N))
         DVDT3 = - G*DYDX
         DVDT4 = FW (N)
         DVDT = DT*(DVDT1 + DVDT2 + DVDT3 + DVDT4)
C
C 5TH:  COMPUTE VELOCITY AND FLOW AT FULL TIME STEP
C       -------------------------------------------
C
         V (N) = V (N) + DVDT
         Q (N) = V (N)*AREAT (N)
C
C 6TH:   SUM TERMS IN MOMENTUM EQUATION FOR OUTPUT (CONVECTIVE
C         INERTIA=MOM, FRICTIONAL RESISTANCE=FRIC, GRAVITATIAL
C         ACCELERATION=GRAV AND WIND STRESS=WIN
C         ----------------------------------------------------
C
         MOM (N) = DVDT1
         FRIC (N) = DVDT2
         GRAV (N) = DVDT3
         WIN (N) = DVDT4

 1070 CONTINUE
C
C
C          ##############################################
C          #  B) SOLVE CONTINUITY EQUATION AT JUNCTIONS #
C          ##############################################
C
C
C
C
C    1ST:COMPUTE SEAWARD BOUNDARY ELEVATIONS (IF SPEFICIED, NSEA>0)
C        AT THE FULL TIME STEP:
C        ----------------------------------------------------------
C               a) IF SEAOPT < 3 THEN EITHER THE TIDE COEFFICIENTS
C                  WERE SPECIFIED (SEAOPT=1) OR COMPUTED BY REGAN
C                  (SEAOPT = 2)
C                  ------------------------------------------------
C               b) IF SEAOPT = 3 THEN THE ELEVATION IS INTERPOLATED
C                  BETWEEN HIGH AND LOW TIDE ELEVATIONS WITH A
C                  SINUSOIDAL CURVE
C                  ------------------------------------------------
C
      IF (NSEA .GT. 0) THEN
         DO 1080 J = 1, NSEA
	    JJ = JUNS(J)
            IF (SEAOPT .LT. 3) THEN
               Y (JJ) = A1 (J, 1)
               NS = 3
               DO 1090 I = 1, NS
                  F = FLOAT (I)
                  K = I + 1
                  Y (JJ) = Y (JJ) + A1 (J, K)*SIN (F*
     1               W (J)*(T - TSTART (J)))
     2               + A1 (J, NS + K)*COS (F*W (J)*(T - TSTART (J)))
 1090          CONTINUE
            ELSE
               L = NHCYC (J)
               TNEXTC = TREP (J)*DTIME (J) + BTIME (J, L + 1)
               IF (T .GE. TNEXTC) THEN
                  IF (L + 1 .LT. NDATA) THEN
                     NHCYC (J) = L + 1
                     L = NHCYC (J)
                     RANGE (J) = BHEAD (J, L + 1) - BHEAD (J, L)
                     PERIOD (J) = 2.*(BTIME (J, L + 1) - BTIME (J, L))
                  ELSE
                     TREP (J) = TREP (J) + 1.0
                     NHCYC (J) = 1
                     RANGE (J) = BHEAD (J, 2) - BHEAD (J, 1)
                     PERIOD (J) = 2.*(BTIME (J, 2) - BTIME (J, 1))
                     NHCYC (J) = 1
                     L = 1
                  END IF
               END IF
               TTIME (J) = T - (BTIME (J, L) + DTIME (J)*TREP (J))
               ARG = 2.*3.1416*TTIME (J)/PERIOD (J) + 3.1416
               Y (JJ) = BHEAD (J, L) + 0.5*RANGE (J)*(1. + COS (ARG))
            END IF
 1080    CONTINUE
      END IF
C
C     2ND: COMPUTE JUNCTION HEAD DUE TO FLOW BOUNDARY CONDITIONS AND
C          AT INTERNAL JUNCTIONS
C          ---------------------------------------------------------
C
       DO 1100 J = 1, NJ

C              a)  BYPASS COMPUTATION IF SEAWARD BOUNDARY
C                  (HEAD SPECIFIED)
C                  --------------------------------------
C
         IF (NSEA .GT. 0)THEN
	    DO 8088 K =1, NSEA
              IF( J .EQ. JUNS(K)) GOTO 1100
8088      CONTINUE
         END IF

C              b) ADD FLOWS INTO AND OUT OF THE JUNCTION
C                 AND SOLVE FOR SURFACE ELEVATION.  CORRECT
C                 ELEVATION FOR EVAPORATION
C                 -----------------------------------------
C
         SUMQ = QIN (J)
         DO 1110 K = 1, CJ
            IF (NCHAN (J, K) .EQ. 0) GO TO 1120
            N = NCHAN (J, K)
            IF (J .EQ. NJUNC (N, 1)) SUMQ = SUMQ + Q (N)
            IF (J .EQ. NJUNC (N, 2)) SUMQ = SUMQ - Q (N)
 1110    CONTINUE
 1120    CONTINUE
         Y (J) = Y (J) - (DT*SUMQ/SURF (J))
         Y (J) = Y (J) + REVAP*DT

 1100 CONTINUE
C
C
C     3RD: COMPUTE JUNCTION VOLUME FOR FULL TIME STEP
C          ---------------------------------------------------------
C
      DO 1130 J = 1, NJ
         VOL(J) = SURF(J) * (Y(J) -BELEV(J))
 1130 CONTINUE
C
C **************************************************************
C * STEP III:COMPUTE CHANNEL CROSS-SECTIONAL AREA AND          *
C *          HYDRAULIC RADIUS FOR FULL TIME STEP               *
C **************************************************************
C
      DO 1140 N = 1, NC
         NL = NJUNC (N, 1)
         NH = NJUNC (N, 2)
         R(N) = (Y (NH) + Y (NL) - BELEV (NH) - BELEV (NL) )/2.0
         AREA(N) = B (N)*R (N)
 1140 CONTINUE
C
C **************************************************************
C * STEP IV:   CHECK FOR STABILITY                             *
C **************************************************************
C
C
      DO 1150 N = 1, NC
         IF (abs(V (N)) .GE. 10.0) THEN
          WRITE (OUT, 6000) N, ICYC
          STOP
         END IF
1150  CONTINUE
C
C***********************************************************************
C                           FORMAT STATEMENTS
C***********************************************************************
C
 6000 FORMAT (1H1///10X,'VELOCITY IN CHANNEL ',I3,' EXCEEDS 10 M/S DURIN
     1G CYCLE ',I6,' .  EXECUTION TERMINATED.')
      RETURN
      END

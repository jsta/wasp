      SUBROUTINE REGAN (JUN)
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C        THIS PROGRAM PERFORMS A LEAST SQUARES FIT OF AN EQUATION OF THE
C     FORM
C           Y(T) = A1 + A2*SIN(WT) + A3*SIN(2WT) + A4*SIN(3WT)
C                     + A5*COS(WT) + A6*COS(2WT) + A7*COS(3WT)
C     TO OBSERVED DATA BY SOLVING THE NORMAL EQUATIONS.
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
      INTEGER NK
C**************************  READ TIDAL INPUT DATA  ********************
      READ (IN, 5000) PERIOD (JUN), TSTART (JUN)
      READ (IN, 5010) (DAY (I), HR (I), MIN (I),
     1   BHEAD (JUN, I), I = 1, NDATA)
      DO 1000 L = 1, NDATA
         RT (L) = (DAY (L) - 1.)*24. + HR (L) + MIN (L)/60.0
         YLOC (L) = BHEAD (JUN, L)
 1000 CONTINUE
      W (JUN) = 2.*3.1416/PERIOD (JUN)
C      NK = 7
C      NCOEFF = NK
      NCOEFF = 7
      IF (YSCALE .EQ. 0.0) YSCALE = 1.0
      WRITE (OUT, 6000) NDATA, NCOEFF, PERIOD (JUN),
     1   W (JUN), MAXIT, MAXRES,
     2   TSHIFT, PSHIFT, YSCALE
      WRITE (OUT, 6010)
C****************************  PRINT INPUT DATA  ***********************
      DO 1010 I = 1, NDATA
         WRITE (OUT, 6020) I, RT (I), YLOC (I)
         RT (I) = RT (I) + TSHIFT
         YLOC (I) = YLOC (I)*YSCALE
 1010 CONTINUE
C*******************************  INITIALIZE  **************************
      DO 1020 K = 1, NCOEFF
         DO 1030 J = 1, NCOEFF
            A (J) = 0.
            SXY (J) = 0.
            SXX (K, J) = 0.
 1030    CONTINUE
 1020 CONTINUE
C************************* SET UP NORMAL EQUATIONS *********************
      NC2 = NCOEFF/2 + 1
      DO 1040 I = 1, NDATA
         DO 1050 J = 1, NCOEFF
            FJ1 = FLOAT (J - 1)
            FJ2 = FLOAT (J - NC2)
            IF (J .LE. NC2) X (J) = SIN (FJ1*W (JUN)*RT (I) + PSHIFT)
            IF (J .EQ. 1) X (J) = 1.
            IF (J .GT. NC2) X (J) = COS (FJ2*W (JUN)*RT (I) + PSHIFT)
            SXY (J) = SXY (J) + (X (J)*YLOC (I))
 1050    CONTINUE
         DO 1060 J = 1, NCOEFF
            DO 1070 K = 1, NCOEFF
               SXX (K, J) = SXX (K, J) + (X (K)*X (J))
 1070       CONTINUE
 1060    CONTINUE
 1040 CONTINUE
C*************************  PRINT NORMAL COEFFICIENTS  *****************
      WRITE (OUT, 6030)
      DO 1080 J = 1, NCOEFF
         WRITE (OUT, 6040) J, SXY (J), (SXX (K, J), K = 1, NCOEFF)
 1080 CONTINUE
C**************************  SOLVE NORMAL EQUATIONS  *******************
      IT = 0
 1090 CONTINUE
      IT = IT + 1
      RESID = 0.
      DO 1100 K = 1, NCOEFF
         SUM = 0.
         DO 1110 J = 1, NCOEFF
            IF (J .EQ. K) GO TO 1110
            SUM = SUM - (A (J)*SXX (K, J))
 1110    CONTINUE
         SUM = (SUM + SXY (K))/SXX (K, K)
         DEL = ABS (SUM - A (K))
         IF (DEL .GT. RESID) RESID = DEL
         A (K) = SUM
 1100 CONTINUE
      IF (IT .GE. MAXIT) GO TO 1120
      IF (RESID .GT. MAXRES) GO TO 1090
 1120 CONTINUE
      WRITE (OUT, 6050) IT, RESID, (A (K), K = 1, NCOEFF)
C***************  PRINT OBSERVED, PREDICTED, AND RESIDUAL DATA  ********
      WRITE (OUT, 6060)
      TRES = 0.
      YMINL = 0.0
      YMAXL = 0.0
      DO 1130 I = 1, NDATA
         IF (YLOC (I) .GT. YMAXL) YMAXL = YLOC (I)
         IF (YLOC (I) .LT. YMINL) YMINL = YLOC (I)
         PRED = 0.
         DO 1140 J = 2, NCOEFF
            FJ1 = FLOAT (J - 1)
            FJ2 = FLOAT (J - NC2)
            IF (J .LE. NC2) PRED = PRED + A (J)*
     1         SIN (FJ1*W (JUN)*RT (I) + PSHIFT)
            IF (J .GT. NC2) PRED = PRED + A (J)*
     1         COS (FJ2*W (JUN)*RT (I) + PSHIFT)
 1140    CONTINUE
         PRED = PRED + A (1)
         DIFF = PRED - YLOC (I)
         TRES = TRES + ABS (DIFF)
         WRITE (OUT, 6070) I, RT (I), YLOC (I), PRED, DIFF
 1130 CONTINUE
      DO 1150 K = 1, NCOEFF
         A1 (JUN, K) = A (K)
 1150 CONTINUE
      WRITE (OUT, 6080) TRES
C****************************  FORMAT STATEMENTS  **********************
 5000 FORMAT (2F10.0)
 5010 FORMAT (4(F5.0,1X,2F2.0,F10.0))
 6000 FORMAT (1H ///,48X,'LEAST SQUARES CURVE FITTING',
     1///// ,38X,'NUMBER OF DATA POINTS',
     210X,'NUMBER OF COEFFICIENTS',/45X,'(NDATA)',24X,'(NCOEFF)'
     3//,47X,I3,28X,I3,///// ,38X,'TIDAL PERIOD (HOURS)',11X,'OMEGA (2*P
     4I/PERIOD)',/44X,'(PERIOD)',23X,'(W)',//,47X,F5.2 ,23X,F7.4,///// ,
     538X,'MAXIMUM NUMBER OF',14X,'MAXIMUM RESIDUAL',/38X,'ITERATIONS AL
     6LOWED',17X,'ALLOWED',//,44X,I4,26X,F6.4,/////,41X,'TIME SHIFT',
     718X,'PHASE ANGLE SHIFT',/42X,'(TSHIFT)',23X,'(PSHIFT)',//,43X,
     8F5.2,26X,F5.2/////57X,'SCALE FACTOR',/59X,'(YSCALE)',//56X,F10.4)
 6010 FORMAT (1H1//1X,30(1H*),'   SUMMARY OF INPUT DATA   ',30(1H*),// ,
     120X,'OBSERVATION NO.',12X,'TIME',12X,'VALUE',/ ,15X,60(1H-)/   )
 6020 FORMAT (1H ,23X,I3,18X,F6.2,10X,F7.3)
 6030 FORMAT (1H1/// ,75X,13(1H-),/6X,'!   -----------   !',50X,'SIGMA X
     1X(K,J)',/6X,'!   SIGMA XY(J)   !',50X,13(1H-),/3X,'J  !   --------
     2---   !',/6X,'!',17X,'! K =     1',14X,'2',14X,'3',14X,'4',14X,'5'
     3,14X,'6',14X,'7',/6X,'!',17(1H-),'!',105(1H-),/6X,'!',17X,'!' )
 6040 FORMAT (1H ,1X,I2,2X,'!',4X,F10.6,3X,'!',7(5X,F10.6),/6X,'!',17X,
     1'!' )
 6050 FORMAT (////50X,30(1H*),/61X,'SOLUTION',/50X,30(1H*),/// ,43X,'NUM
     1BER OF ITERATIONS',10X,'MAXIMUM RESIDUAL',//,51X,I3,22X,F9.6,////,
     235X,'THE CURVE WHICH BEST FITS THE OBSERVED DATA IS GIVEN BY    '
     3///,20X,'Y(T)  =',F12.6,'  +',F12.6,' SIN(WT)  +',F12.6,
     4' SIN(2WT)  +',F12.6,' SIN(3WT)',//,41X,'+',F12.6,' COS(WT)  +',
     5F12.6,' COS(2WT)  +',F12.6,' COS(3WT)')
 6060 FORMAT (1H1// ,1X,30(1H*),'   SUMMARY OF OUTPUT DATA   ',30(1H*)//
     1,4X,'OBSERVATION',10X,'TIME',10X,'OBSERVED',10X,'PREDICTED',10X,'R
     2ESIDUAL',/2X ,86(1H-)// )
 6070 FORMAT (1H ,7X,I3,14X,F5.2,11X,F6.3,11X,F7.4,13X,F7.4)
 6080 FORMAT (1H ,//5X,'TOTAL RESIDUAL = ',F10.5 )
      RETURN
      END

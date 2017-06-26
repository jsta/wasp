      SUBROUTINE DYNHYD
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C********************   INCREMENT TRANSIENT FLOWS   ********************
C
      IF (NVFLOW .GT. 0) THEN
         DO 1000 I = 1, NVFLOW
           K = NQ (I)
           IF (ICYC.EQ.1 .OR. ICYC.EQ.QCYC(I, K + 1)) THEN
             NQ (I) = NQ (I) + 1
c zhu Apr. 90......
             K = NQ (I)
             QCYC (I, K) = QREP(i)*QCYC (I, NINCR (I)) + QCYC (I, K)
             QCYC (I, K+1) = QREP(i)*QCYC (I, NINCR (I)) + QCYC(I,K+1)
           END IF
C
Czhu repeat flow cycle..QREP is declared as an array recently...
C
           IF (NQ (I) .EQ. NINCR (I)) THEN
             QREP(i) = QREP(i) + 1.0
             NQ (I) = 1
c zhu Apr. 90......
             K = NQ (I)
             QCYC (I, K) = QREP(i)*QCYC (I, NINCR (I)) + QCYC (I, K)
             QCYC (I, K+1) = QREP(i)*QCYC (I, NINCR (I)) + QCYC(I,K+1)
          END IF
CC
            Q1 = VFLOW (I, K + 1) - VFLOW (I, K)
            Q2 = FLOAT (QCYC (I, K + 1) - QCYC (I, K))
            Q3 = FLOAT (ICYC - QCYC (I, K))
            J = JRVF (I)
            VQIN (J) = 0.0
            VQ (I, J) = VFLOW (I, K) + (Q1*Q3/Q2)
 1000    CONTINUE
C
C***************  SUM VARIABLE FLOWS INTO EACH JUNCTION  ***************
C
         DO 1010 I = 1, NVFLOW
            DO 1020 J = 1, NJ
               VQIN (J) = VQIN (J) + VQ (I, J)
 1020       CONTINUE
 1010    CONTINUE
      END IF
C
C**********************   SUM TOTAL INFLOWS   **************************
C
      DO 1030 J = 1, NJ
         QIN (J) = CQIN (J) + VQIN (J)
 1030 CONTINUE
C******************  COMPUTE WIND COMPONNENT  **************************
C
      CALL WIND
C
C***************** COMPUTE EVAPORATION COMPONNENT **********************
C
      CALL EVAPORAT
C
C****************   COMPUTE FLOWS, VELOCITIES, HEADS   *****************
C
      CALL  RUNKUT
      RETURN
      END

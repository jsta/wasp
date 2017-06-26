      DOUBLE PRECISION FUNCTION DINTRP (NN, DX, DY, DXX, MMM)
C
C      IMPLICIT DOUBLE PRECISION (D)
      INCLUDE 'WASP.CMN'
C
      DIMENSION DX (1), DY (1), DVD (20)
      REAL*8 DZ, DEPS, DX, DY, DVD, DXX, DW, D5, DL
      DATA DZ, DEPS, D5/00.D00, 1.0D-12, 0.5D00/
C       Check for valid parameters
      DINTRP = DZ
      M = MMM
      N = NN
      IF (DXX .LT. (DX (1) - DEPS)) GO TO 1000
      IF (M .LT. 2 .OR. M .GT. 20 .OR. NN .LT. 2) GO TO 1010
      L = 1
      IF (NN .GT. M) GO TO 1020
      M = NN
      GO TO 1030
C       N=0 => Sequential search (MAX N = 1000 or whatever)
C       N<15=> Sequential search up to ABS(N)
C       ELSE=> Binary search with N ELE'S in table
 1020 CONTINUE
      IF (NN .NE. 0) GO TO 1040
      N = 1000
      GO TO 1050
 1040 CONTINUE
      IF (NN .GT. 15) GO TO 1060
C       Sequential search rtn
 1050 CONTINUE
      L = 1
 1070 CONTINUE
      IF (DABS (DXX - DX (L)) .LT. DEPS) GO TO 1080
      IF (DXX .LT. DX (L)) GO TO 1090
      L = L + 1
      IF (L .LE. N) GO TO 1070
      GO TO 1000
 1090 CONTINUE
      L = L - 1
      GO TO 1100
C       Binary search rtn
 1060 CONTINUE
      IF (DXX .GT. (DX (N) + DEPS)) GO TO 1000
      L = N
      IF (DABS (DXX - DX (N)) .LT. DEPS) GO TO 1080
      IF (M .NE. N) GO TO 1110
      L = 1
      GO TO 1030
 1110 CONTINUE
      DW = DBLE (N)
      DW = DW*D5
      DL = DW
      L = N/2
 1120 CONTINUE
      DW = D5*DW
      IF (DABS (DXX - DX (L)) .LT. DEPS) GO TO 1080
      IF (DW .LT. D5) GO TO 1130
      IF (DXX .GT. DX (L)) GO TO 1140
      DL = DL - DW
      L = IDINT (DL)
      GO TO 1120
 1140 CONTINUE
      DL = DL + DW
      L = IDINT (DL)
      GO TO 1120
 1130 CONTINUE
      IF (DX (L) .LT. DXX) GO TO 1100
      L = L - 1
      IF (L .LE. 0) GO TO 1000
      GO TO 1130
C       Calculate beginning point for input to DIVDIF
 1100 CONTINUE
      L = L - M/2 + 1
      IF (L .LE. 0) L = 1
      IF (L .GT. (N - M + 1)) L = N - M + 1
 1030 CONTINUE
      IF (M .EQ. 2) GO TO 1150
C     TYPE *,2HL=,L
      MM = M - 1
      CALL DIVDIF (DX (L), DY (L), M, DVD)
      DINTRP = DSPLY (DVD, MM, DXX, DX (L))
      GO TO 1160
C       Linear interpolation
 1150 CONTINUE
      DINTRP = (DXX - DX (L))/(DX (L + 1) - DX (L))*(DY (L +
     1   1) - DY (L)) + DY (L)
C       TYPE *,DXX,DX(L),DY(L),DINTRP
C       TYPE *,2HL=,L
      GO TO 1160
C       Direct hit
 1080 CONTINUE
      DINTRP = DY (L)
      GO TO 1160
 1010 CONTINUE
      WRITE (OUT, 6000)
 6000   FORMAT(/' Invalid argument in DINTRP.')
      GO TO 1160
 1000 CONTINUE
      WRITE (OUT, 6010)
 6010   FORMAT(/' DXX outside table.')
 1160 CONTINUE
      RETURN
C       End of DINTRP
      END

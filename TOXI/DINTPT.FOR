      DOUBLE PRECISION FUNCTION DINTPT (N, X, Y)
C       DINTPT calculates an approximation for an integral
C       on the basis of points supplied by using cubic
C       interpolation and Gaussian quadrature.  Input parameters:
C       N  =  Number of points input (must have  N > 2  and  N < 12).
C       X  =  Single precision array of abscissas.
C       Y  =  Single precision array of ordinates.
C
C      IMPLICIT DOUBLE PRECISION (D)
      INCLUDE 'WASP.CMN'
C
      REAL*8 DX, DY, DC, DW, DSUM, DBLE, DALF, DBET, DGC, DXX
      DIMENSION DX (11), DY (11), DC (7), DW (7)
      REAL X (1), Y (1)
      INTEGER N, NGPTS, NIP
C
C       NGPTS is number of gaussian points to be used.
C       DC = Gaussian points, DW = Gaussian weights
C
      DATA NGPTS/7/
      DATA DC/ - .949107912342759D0, - .
     1   741531185599394D0, - .405845151377397D
     2   0, 0.D0, .405845151377397D0, .
     3   741531185599394D0, .949107912342759D0/
      DATA DW/.129484966168870D0, .
     1   279705391489277D0, .381830050505119D0,
     2   .417959183673469D0, .381830050505119D0, .279705391489277D0,
     3   .129484966168870D0/
C
      DSUM = 0.0D+00
      IF (N .LT. 3 .OR. N .GT. 11) GO TO 1000
C
      DO 1010 I = 1, N
         DX (I) = DBLE (X (I))
         DY (I) = DBLE (Y (I))
 1010 CONTINUE
      NIP = 4
      IF (N .EQ. 3) NIP = 3
C
      DALF = 0.5D+00*(DX (1) + DX (N))
      DBET = 0.5D+00*(DX (N) - DX (1))
      DO 1020 I = 1, NGPTS
         DXX = DBET*DC (I) + DALF
         DGC = DBET*DINTRP (N, DX, DY, DXX, NIP)
         DSUM = DSUM + DW (I)*DGC
 1020 CONTINUE
      GO TO 1030
 1000 CONTINUE
      WRITE (OUT, 6000) N
 6000   FORMAT(1X,'invalid input in dintpt',i5)
 1030 CONTINUE
      DINTPT = DSUM
      RETURN
C       End of DINTPT
      END

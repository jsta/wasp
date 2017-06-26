      SUBROUTINE MEAN (XX, N, NROW, NCOL, NOPT, AVG)
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C     SUBROUTINE MEAN COMPUTES THE AVERAGE OF THE ELEMENTS WITHIN
C     THE ROW OF A MATRIX.   THREE AVERAGING OPTIONS ARE AVAILABLE
C
C     XX     = THE MATRIX WHOSE ROW ELEMENTS ARE AVERAGED
C     N     = NUMBER OF THE ROW WHOSE ELEMENTS ARE AVERAGED
C             (CORRESPONDS TO A CHANNEL OR JUNCTION NUMBER)
C     NROW  = NUMBER OF ROWS IN MATRIX XX (CORRESPONDS TO THE
C             NUMBER OF CHANNELS OR JUNCTIONS)
C     NCOL  = NUMBER OF COLUMNS IN MATRIX XX (CORRESPONDS TO THE
C             NUMBER OF DATA POINTS. NCOL MUST BE ODD IF NOPT=1)
C           = 1  SIMPSON'S METHOD IS USED FOR COMPUTATIONS
C     NOPT  = 2  TRAPEZOIDAL METHOD IS USED FOR COMPUTATIONS
C           = 3   STRAIGHT METHOD IS USED FOR COMPUTATIONS
C     AVG   = AVERAGE OF THE NCOL ELEMENTS IN ROW N OF XX
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C--- Dummy arguments
      INTEGER N, NROW, NCOL, NOPT
C      REAL XX (NROW, NCOL)
      REAL XX( CH, NR)
C       ********* Jan.,1990********
      REAL AVG
C --- Local variables
      REAL YSUM
C
C***********************************************************************
C                   TRANSFORMATION OF ROW ELEMENTS
C***********************************************************************
C
      GO TO ( 1000, 1010, 1020), NOPT
C
C********************   SIMPSON'S TRANSFORMATIONS   ********************
C
 1000 CONTINUE
      YL (1) = XX (N, 1)
      DO 1030 I = 2, NCOL, 2
         YL (I) = 4.*XX (N, I)
 1030 CONTINUE
      DO 1040 I = 3, NCOL, 2
         YL (I) = 2.*XX (N, I)
 1040 CONTINUE
      YL (NCOL) = XX (N, NCOL)
      GO TO 1050
C
C*******************   TRAPEZOIDAL TRANSFORMATIONS   *******************
C
 1010 CONTINUE
      YL (1) = .5*XX (N, 1)
      DO 1060 I = 2, NCOL
         YL (I) = XX (N, I)
 1060 CONTINUE
      YL (NCOL) = .5*XX (N, NCOL)
      GO TO 1050
C
C********************   STRAIGHT TRANSFORMATIONS   *********************
C
 1020 CONTINUE
      DO 1070 I = 1, NCOL
         YL (I) = XX (N, I)
 1070 CONTINUE
C
C***********************************************************************
C                  SUMMATION OF TRANSFORMED ELEMENTS
C***********************************************************************
C
 1050 CONTINUE
      YSUM = 0.0
      DO 1080 I = 1, NCOL
         YSUM = YSUM + YL (I)
 1080 CONTINUE
C
C***********************************************************************
C                      COMPUTATION OF AVERAGE
C***********************************************************************
C
      GO TO ( 1090, 1100, 1110), NOPT
C
C***********************   SIMPSON'S FORMULA   *************************
C
 1090 CONTINUE
      AVG = YSUM/(3.*FLOAT (NCOL - 1))
      RETURN
C
C**********************   TRAPEZOIDAL FORMULA   ************************
C
 1100 CONTINUE
      AVG = YSUM/FLOAT (NCOL - 1)
      RETURN
C
C***********************   STRAIGHT FORMULA   **************************
C
 1110 CONTINUE
      AVG = YSUM/FLOAT (NCOL)
      RETURN
      END

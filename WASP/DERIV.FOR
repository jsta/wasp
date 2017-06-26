      SUBROUTINE DERIV
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:40.
C
C
C  Correction History:  Added block 'sumq', 9/6/87 rba
C     added qsum ,9/9/87, rba
C
C----------------------------------------------------------------------
C
C     flow convention : (i,j) = (to, from)
C
      INCLUDE 'WASP.CMN'
C
C          READ NEW FLOWS AND VOLUMES
C
C  UPDATE FLOWS AND EXCHANGES FOR CURRENT TIME STEP.
C  IF IQOPT IS 2 OR 3, FIELD 1 FLOWS WILL BE UPDATED IN
C  HYDRAULIC SUBROUTINES (I.E. QINT(1,1)=1.0).
C
      CALL WAS8B
C
      IF (IQOPT .LE. 2) CALL QCALC
      IF (IQOPT .GE. 3) CALL HYDROIN
C
C          CALCULATE DERIVATIVES
C
C     CALCULATE KINETIC DERIVATIVES
C
      CALL WASPB
C
C               Calculate Transport and Load Derivatives
C
      CALL WAS12
C
      RETURN
      END

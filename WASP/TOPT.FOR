      SUBROUTINE TOPT
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:25:52.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C           THIS SUBROUTINE REDUCES NUMERICAL DISPERSION BY MAXIMIZING
C           THE TIME STEP FOR THE EULER ONE STEP INTEGRATION SUBJECT TO
C           FLOW AND DISPERSION STABILITY CONSTRAINTS.
C
C
      INCLUDE 'WASP.CMN'
C
      REAL MASS
C
      TRT = DTMAX
      DT = DTMAX
C          SET TIME STEP
C
      DO 1000 ISYS = 1, NOSYS
         DO 1010 ISEG = 1, NOSEG
            MASS = BVOL (ISEG)*C (ISYS, ISEG)
            IF (SUMM (ISYS, ISEG) .GT. 0. .AND. MASS .GT. 0.)
     1         TRT = MASS/SUMM (ISYS, ISEG)
C
            IF (TRT .LT. DT) DT = TRT
 1010    CONTINUE
 1000 CONTINUE
      IF (DT .LT. 0.01E-04 .AND. IQOPT .LT. 3) DT = 0.01E-04
C
      RETURN
      END

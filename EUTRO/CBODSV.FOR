      SUBROUTINE CBODSV
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:57.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
C                *-*-*-*-* SYSTEM 5 - CBOD  *-*-*-*-*
C                             Sources
C
      IF ( .NOT. SEDSEG) GO TO 1000
      SR18P = OCRB*DPP
      GO TO 1010
C
C                        Phytoplankton 'DEATH'
C
 1000 CONTINUE
      DEATH = K1D*PHYT
      SR18P = OCRB*DEATH
C
C                              Sinks
C                            Oxidation
C
 1010 CONTINUE
      IF ( .NOT. SEDSEG) THEN
         IF (DO .GT. 1.0E-15) THEN
            SK180 = (KDC*KDT**STP20)*((CBOD*DO)/(KBOD + DO))
         ELSE
            SK180 = 0.0
         END IF
      ELSE
         IF (CBOD .GT. 1.0E-15) THEN
            SK180 = (KDSC*KDST**STP20)*CBOD
         ELSE
            SK180 = 0.0
         END IF
      END IF
C
C                         Denitrification
C
      IF (CBOD .GT. 0.)TEMPBOD= SK180/CBOD
      SK18D = (5./4.)*(32./14.)*SK14D
C
C                      Formulate Derivative
C
      CD (5, ISEG) = (SR18P - SK180 - SK18D)*VOL
C
      RETURN
      END

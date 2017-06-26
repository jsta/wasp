      SUBROUTINE AMMONIA
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:55.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
C       *-*-*-*-*  SYSTEM 1 - AMMONIA (NH3-N)  *-*-*-*-*
C
C                        Sources
C               Mineralization of organic nitrogen
C
      SR13ON = SK1013
C
C                  Phytoplankton Death
C
      SR13P = NCRB*DPP*(1.0 - FON)
C
C                        Sinks
C                    Algal Uptake
C
      SK13P1 = PNH3G1*NCRB*GP1*PHYT
C
C                      Nitrification
C
      IF (DO .GT. 1.0E-10) THEN
         SK1314 = (K1320C*K1320T**STP20)*NH3*DO/(KNIT + DO)
      END IF
      IF (STP .LT. 7.) SK1314 = 0.0
C
C                   Formulate Derivative
C
      CD (1, ISEG) = (SR13P + SR13ON - SK13P1 - SK1314)*VOL
C
      RETURN
      END

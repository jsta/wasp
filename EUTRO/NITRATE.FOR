      SUBROUTINE NITRATE
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:56.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
C    *-*-*-*-* SYSTEM 2 - Nitrate (+Nitrite)  (NO3-N+NO2-N)  *-*-*-*-*
C
C                         Sources
C                      Nitrification
C
      SR1413 = SK1314
C
C                          Sinks
C                      Algal Uptake
C
      SK14P1 = (1. - PNH3G1)*NCRB*GP1*PHYT
C
C                     Denitrification
C
      SK14D = (K140C*K140T**STP20)*NO3
      IF (DO .GT. 0) SK14D = SK14D*KNO3/(KNO3 + DO)
C
      IF (SK14D .LT. 1.00E-24) SK14D = 1.00E-24
C
C                   Formulate Derivative
C
      CD (2, ISEG) = (SR1413 - SK14P1 - SK14D)*VOL
C
      RETURN
      END

      SUBROUTINE ORGANICN
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:59.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
C
C             *-*-*-*-*  SYSTEM 7 Organic Nitrogen      *-*-*-*-*
C
C                          Sources
C             Phytoplankton Respiration and 'DEATH'
C
      SR10P = NCRB*DPP*FON
C
C                         Sinks
C         Mineralization of Dissolved Organic Nitrogen
C
      IF ( .NOT. SEDSEG) SK1013 = (K1013C*K1013T**STP20)*ON*XEMPRC
      IF (SEDSEG) SK1013 = (KONDC*KONDT**STP20)*ON
C
C                   Formulate Derivative
C
      CD (7, ISEG) = (SR10P - SK1013)*VOL
C
      RETURN
      END

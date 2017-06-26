      SUBROUTINE ORGANOP
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:59.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
C        *-*-*-*-*-*  SYSTEM 8 Organic Phosphorous  *-*-*-*-*-*
C
C                            Sources
C                 Phytoplankton respiration and 'death'
C
      SR5P = PCRB*DPP*FOP
C
C                             Sinks
C
C           Mineralization of dissolved organic phosphorus and
C           Phytoplankton respiration and 'death'
C
      IF ( .NOT. SEDSEG) SK58 = (K58C*K58T**STP20)*OP*XEMPRC
      IF (SEDSEG) SK58 = (KOPDC*KOPDT**STP20)*OP
C
C                        Formulate Derivative
C
      CD (8, ISEG) = (SR5P - SK58)*VOL
C
      RETURN
      END

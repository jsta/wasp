      SUBROUTINE INORGANP
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:56.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
C   *-*-*-*-*  SYSTEM 3 - Dissolved Inorganic Phosphorous  *-*-*-*-*
C
C                            Sources
C               Mineralization of Organic Phosphorous
C
      SR8OP = SK58
C
C                         Phytoplankton Death
C
      SR8P = PCRB*DPP*(1. - FOP)
C
C                            Sinks
C                        Algal uptake
C
      SK8P = PCRB*GPP
C
C                   Formulate Derivative
C
      CD (3, ISEG) = (SR8P + SR8OP - SK8P)*VOL
C
      RETURN
      END

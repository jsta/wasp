      SUBROUTINE BENTFLUX
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:54.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
      DO 1000 ISEG = 1, NOSEG
         IBOT = IBOTSG (ISEG)
C
      IF (SEDSEG) GOTO 1000
C              Benthic ammonium and phosphate fluxes
C
         FLUX = FNH4 (ISEG)*TFNH4
         CD (1, ISEG) = CD (1, ISEG) + FLUX
         FLUX = FPO4 (ISEG)*TFPO4
         CD (3, ISEG) = CD (3, ISEG) + FLUX
C
 1000 CONTINUE
C
      RETURN
      END

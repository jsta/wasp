      SUBROUTINE KAHYDRA (K2HYDRA)
C
C     Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:58.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
      REAL K2HYDRA
C
C
C                      Calculate Oxygen Reaeration
C
      CFOREA = 1.0
      AVDEPE = H
      AVVELE = ABS(VEL)
C
C
C         Calculate reaeration coefficient for free-flowing reach
C         Calculate reaeration coefficient as a power function of
C         average hydraulic depth and velocity; determine exponents
C         to depth and velocity terms and assign value to REAK
C
      IF (AVDEPE .GT. 0.61) GO TO 1000
C
C          Use Owen's formulation for reaeration
C
      REAK = 5.349
      EXPREV = 0.67
      EXPRED = - 1.85
      GO TO 1010
C
 1000 CONTINUE
C
C       Calculate transition depth; transition depth determines
C       which method of calculation is used given the current
C       velocity
C
      IF (AVVELE .GE. 0.518) GO TO 1020
      TRANDP = 0.0
      GO TO 1030
 1020 CONTINUE
      TRANDP = 4.411*(AVVELE**2.9135)
 1030 CONTINUE
C
      DIF = AVDEPE - TRANDP
      IF (DIF .GT. 0.0) GO TO 1040
C
C                 Use Churchill's formulation for reaeration
C
      REAK = 5.049
      EXPREV = .969
      EXPRED = - 1.673
      GO TO 1050
C
 1040 CONTINUE
C
C                 Use O'Connor-Dobbins formulation for reaeration
C
      REAK = 3.93
      EXPREV = 0.5
      EXPRED = - 1.5
C
 1050 CONTINUE
C
 1010 CONTINUE
C
C                               Calculate reaeration coefficient
C
      K20 = REAK*(AVVELE**EXPREV)*(AVDEPE**EXPRED)
      IF (K20 .GT. 24.) K20 = 24.
      K2HYDRA = K20*1.028**STP20
 1060 CONTINUE
      RETURN
      END

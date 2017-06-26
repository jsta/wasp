      SUBROUTINE KAWIND (WS, TW, TA, depth, WTYPE, KAINIT, RK)
C*******************************************************************
C*
C*     SUBROUTINE REAERK CALCULATES:
C*
C*              RK = REAERATION COEFFICIENT (RK) (M/DAY)*
C*
C*     GIVEN:
C*              WS = Wind Speed (WS) (m/s)
C*              TA = Temperature of the Air  (Degrees C)
C*              TW = Water Temperature (Degrees C)
C*
C* Using the method presented in:
C*
C*           Jour. of Env Eng, Vol. 109, NO.3,PP.731-752,
C*           June 1983, Author: D.J.O'Connor, TITLE: "Wind Effects on
C*           Gas- Liquid Transfer Coefficients"
C*
C*=====================================================================
C*
C* THIS SUBROUTINE WAS WRITTEN BY:
C*     SANDRA BIRD
C*     USAE WATERWAYS EXPERIMENT STATION (WES-ES-Q)
C*     VICKSBURG, MISSISSIPPI
C* AND MODIFIED BY JAMES L. MARTIN
C*
C*
C*=====================================================================
C*
C*   Parameters used in the model include:
C*
C*        Transitional Shear Velocity - UT(cm/sec)
C*        Critical Shear Velocity - UC (cm/sec)
C*        Vonkarman's Constant (KA)
C*        Equilibrium Roughness - ZE (cm)
C*        1/LAM Is a Reynold's Number
C*        GAM is a a Nondimensional Coefficient Dependent on
C*        Water Body Size (WTYPE).
C*        LAM, GAM, UT, UC and ZE are Dependent on Water Body
C*        Size (See O'Conners Paper for Table of Values).
C*
C*       UT       UC      ZE    LAM     GAM
C*       10.0     11.    .35    3.0     5.          Large Scale
C*       10.0     11.    .25    3.0     6.5         Intermediate
C*        9.      22.    .25   10.     10.          Small Scale
C*
CC******************************************************************
C*
      INCLUDE 'CONST.INC'
C*
C*  Declarations:
C*
      REAL XARG1,XARG2,XWDIFF
      Character*1 cont1
      REAL*4 KA, LAM, KA3, TMPAIR
      COMMON /KAHOLD/ ut, uc, ze, lam, gam, TMPAIR
      data cont1/'$'/
C*
C*   Determine Water Body Type, if WTYPE=0., then default is large
C*   Water Body:
C*
CCSC
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:13:32.
CRBA      XARG1 = ABS(WTYPE)
      XWDIFF= WTYPE - 3.0
      XARG2 = ABS(XWDIFF)
C      IF (WTYPE .EQ. 0. .OR. WTYPE .EQ. 3.) then
CRBA      IF (XARG1.LT.R0MIN.OR.XARG2.LT.R0MIN) THEN
      IF (XARG2.LT.R0MIN) THEN
         UT = 10.0
         UC = 11.0
         ZE = 0.35
         LAM = 3.0
         GAM = 5.0
      ELSE
CCSC
         XWDIFF = WTYPE - 1.0
         XARG1 = ABS(XWDIFF)
C         IF (WTYPE .EQ. 1.) THEN
         IF (XARG1 .LT. R0MIN) THEN
            UT = 9.0
            UC = 22.0
            ZE = 0.25
            LAM = 10.0
            GAM = 10.0
         ELSE
            UT = 10.0
            UC = 11.0
            ZE = 0.25
            LAM = 3.0
            GAM = 6.5
         END IF
      END IF
C*
C CALCULATE DIFFUSIVITY OF OXYGEN IN WATER (DIFF) (CM**2/SEC), VISCOSIT
C OF WATER (VW) (CM**2/SEC),VISCOSITY OF AIR (VA) (CM**2/SEC),DENSITY
C OF WATER (PW) (G/CM**3), DENSITY OF AIR (PA) (G/CM**3)
      DIFF = 4.58E-07*TW + 1.2E-05
C  NOTE: IF OTHER CHEMICALS WERE USED, THEN THEIR DIFFUSIVITIES
C  MAY VARY. FOR EXAMPLE FOR TCDD:   (JLM)
C      DIFF=4.83E-6
C
      VW = 0.0164 - .00024514*TW
      VA = .133 + .0009*TA
      PA = .00129 - .0000040*TA
      PW = 1.00
      WS = WS*100.
      RK = 1.
C USE NEWTON RAPHSON METHOD TO CALCULATE THE SQUARE ROOT OF THE DRAG
C COEFFICIENT
      N = 0
C PARAMETERS USED IN THE MODEL INCLUDE TRANSITIONAL SHEAR VELOCITY - UT(
C CRITICAL SHEAR VELOCITY - UC (CM/SEC); VONKARMAN'S CONSTANT (KA);
C EQUILIBRIUM ROUGHNESS - ZE (CM); 1/LAM IS A REYNOLD'S NUMBER; GAM IS
C NONDIMENSIONAL COEFFICIENT DEPENDENT ON WATER BODY SIZE.  LAM, GAM, UT
C UC AND ZE ARE DEPENDENT ON WATER BODY SIZE
      KA = 0.4
      KA3 = KA**.3333
      WH = 1000.
C MAKE INITIAL GUESS FOR SQUARE ROOT OF THE DRAG COEFFICIENT
      SRCD = 0.04
 1000 CONTINUE
      N = N + 1
C CALCULATE VALUE OF FUNCTION(F2) AND DERIVATIVE OF FUNCTION(FP)
      EF = EXP ( - SRCD*WS/UT)
      F1 = LOG ((WH/ZE) + (WH*LAM/VA)*SRCD*WS*EF)
      F2 = F1 - KA/SRCD
      FP1 = 1./((WH/ZE) + (LAM*WH/VA)*SRCD*WS*EF)
      FP2 = ((WH*LAM)/(VA*UT))*SRCD*WS**2*EF
      FP3 = (WH*LAM/VA)*WS*EF
      FP4 = FP1*(FP2 + FP3) + (KA/(SRCD**2))
C CALCULATE A NEW GUESS FOR SQUARE ROOT OF DRAG AND COMPARE TO
C PREVIOUS GUESS AND LOOP BACK THROUGH N-R WITH NEW GUESS IF
C APPROPRIATE
      SRCD2 = SRCD - F2/FP4
      ERR = ABS (SRCD - SRCD2)
      IF (ERR .GT. 0.0005 .AND. N .LT. 8) THEN
         SRCD = SRCD2
         GO TO 1000
      END IF
      IF (ERR .GT. 0.005 .AND. N .GE. 8) GO TO 1010
      CD = SRCD**2
      US = SRCD*WS
      Z0 = 1./((1./ZE) + LAM*US*EXP ( - US/UT)/VA)
      WS = WS/100.
      IF (WS .LT. 6.0) GO TO 1020
      IF (WS .GE. 6.0 .AND. WS .LE. 20.0) GO TO 1030
      IF (WS .GT. 20.0) GO TO 1040
C CALC 1050S VALUES FOR WINDSPEEDS LESS THAN 6.0 M/SEC
 1020 CONTINUE
      RK1 = (DIFF/VW)**0.666667*SRCD*(PA/PW)**0.5
      RK = RK1*KA3*WS/GAM
      RK = RK*3600.*24.
      GO TO 1050
C CALC 1050S VALUES FOR WINDSPEED GREATER THAN 20 M/S
 1040 CONTINUE
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:15:47.
C      RK = (DIFF*PA*VA*US/(0.1*PW*VW))**0.5
      RK = (DIFF*PA*VA*US/(KA*ZE*PW*VW))**0.5
      RK = RK*3600.*24./100.
      GO TO 1050
C CALC 1050S VALUES FOR WINDSPEED BETWEEN 6 AND 20 M/S
 1030 CONTINUE
      GAMU = GAM*US*EXP ( - US/UC + 1.)/UC
      RK1 = (DIFF/VW)**.6667*KA3*(PA/PW)**0.5*US/GAMU
      RK2 = (DIFF*US*PA*VA/(KA*Z0*PW*VW))**0.5
      RK3 = (1./RK1) + (1./RK2)
      RK = 1./RK3
      RK = RK*3600.*24./100.
      GO TO 1050
 1050 continue
      GO TO 1060
 1010 CONTINUE
      WRITE (6, 6000)
 6000 FORMAT(5X,'SOLUTION DID NOT CONVERGE')
 1060 CONTINUE
      rk = rk/depth
      RETURN
      END

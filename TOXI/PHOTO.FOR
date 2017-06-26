      SUBROUTINE PHOTO (ICHM, RFLATG, KDPG, QUANTG)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:19.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C                      
      INCLUDE 'ENVIRON.CMN'
C
      INCLUDE 'PHYSCHM.CMN'
C
      INCLUDE 'OPTION.CMN'
C
      INCLUDE 'PHOTOL.CMN'
C
      INCLUDE 'KNETIC.CMN'
C
      INCLUDE 'CONC.CMN'
C
      INCLUDE 'GLOBAL.EQU'
C
      INCLUDE 'CONST.INC'
C
C       THIS PHOTOLYSIS CODE IS BASED ON THE PHOTOLYSIS CALCULATION
C       OF EXAMS II.
C
      REAL  XARG
      REAL  LATG, KDPL, KDPG(5)
      DIMENSION QUANTG (5, 3)
      EQUIVALENCE (CONST (4), LATG)
C
      FOTO (ICHM) = 0.0
C
C       Loop thru the molecular & 4 ionic species
      DO 1000 ION = 1, 5
C         Skip non-existent species:
         IF (ION .GT. 1 .AND. SPFLG (ION - 1) .EQ. 0) GO TO 1000
C
C     BRANCH TO PROPER PROGRAM SECTOR FOR THE PHOTOLYSIS OPTION USED
C
         GO TO ( 1010, 1020), IPHOTO
C
C     OPTION 1: PHOTOLYSIS RATES WILL BE CALCULATED FROM MOLAR
C               ABORPTIVITIES AND QUANTUM YIELD OF THE CHEMICAL
C
C
C        A) Compute rate of light absorption by the chemical
C
 1010    CONTINUE
         DO 1030 LAMKNT = 1, 46
            KDPL = KDPL + SDFAC*AVELIT (LAMKNT)
     1         *ABSORG (ICHM, LAMKNT, ION)
 1030    CONTINUE
C
C
         GO TO 1040
C
C     OPTION 2: SURFACE 1ST-ORDER PHOTOLYSIS RATES INPUTTED BY USER
C
C
C        A) Adjust rate constant for time variability, cloudiness and
C           any difference in latitude between location of water body
C           the site of photolysis measurement.
C
 1020    CONTINUE
         KDPL = 0.0
         IF (KDPG (ION) .GT. 0.) THEN
            KDPL = KDPG (ION)*PHTON*AVELIT (ION)*(1.
     1         - 0.056*CLOUDG (NDAT))
     2         *(191696.65 + 87054.63*COS (0.0349*LATG))/(191696.65
     3         + 87054.63*COS (0.0349*RFLATG ))
         END IF
C            Latter 2 lines in this equation are latitude adjustments ba
C            on total irradiance data from Smithsonian meteorological ta
C
C
C     Compute 1st-order photolysis rate constants
C
 1040    CONTINUE
         FOTO (ICHM) = FOTO (ICHM) + KDPL*(QUANTG (ION,1)*
     1   DISTOX (ICHM, ION)
     2      + QUANTG (ION, 3)*DOCTOX (ICHM, ION))
         DO 1050 J = 1, 3
CCSC
            XARG = ABS(C(J+1,ISEG))
C            IF (C (J + 1, ISEG) .EQ. 0.0) GO TO 1050
            IF (XARG .LT. R0MIN) GO TO 1050
            CP = PARTOX (ICHM, J, ION)
            FOTO (ICHM) = FOTO (ICHM) + KDPL*QUANTG (ION, 2)*CP
 1050    CONTINUE
C
C
 1000 CONTINUE
C
C
C
C
C       Call singlet oxygen transformations:
C
C***        IF(SINGSW) CALL SINGO2(K2,J)
C       SINGO2 should be called at least once,
C        to calculate [singlet oxygen] for report in ouput tables;
C***        IF (K2.EQ.KCHEM .AND. SINCAL.LT.J .AND. .NOT.SINGSW)
C***     -     CALL SINGO2(K2,J)
C
C***        IF (PHOXSW .OR. K2.EQ.KCHEM) CALL PHOX(K2,LIGHTL)
C
      RETURN
C       End of Subroutine SUNLYS
      END

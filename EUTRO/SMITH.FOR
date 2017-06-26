      SUBROUTINE SMITH
C*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C          Dick Smith variable carbon/chlorophyll ratio
C*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
C            IAV = IAV/1440.
C
C               (Average solar radiation during daylight hours)
C
C/cm2 day*[0.43 visible/total*10,000 cm2/m2*E/52,000]=E visible/m2-day
C             0.083  is FU (E/M2 - Ly or E/10 Kcal)
C
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:05:36.
      CCHL1 = CCHLX(ISEG)
      IS1 = IS1X(ISEG)
      IAVBOT=IAVBOTX(ISEG)
C      CCHL1 = 0.3*0.083*PHIMX*XKC*IAV/(GITMX1*2.718)
C      IF (CCHL1 .LT. 20.) CCHL1 = 20.
      TCHLA = PHYT/CCHL1
      RLIGHT = RLGHTS (ISEG, 1)
C
      IF (NEWDAY .GE. 1) THEN
C
C           Dick Smith formulation integrated every day
C
         KESHD = XKC*1000.*TCHLA
         SKE = KESG (ISEG)
         IF (IKE .GT. 0 .AND. IKE .LE. 5) SKE = SKE*KE (IKE)
         SKE = SKE + KESHD
         TEMP1 = SKE*H
         TEMP2 = 0.083*PHIMX*XKC/(GITMAX*CCHL1*2.718)
         TEMP3 = EXP ( - TEMP1)
         RLGHTS (ITO, 2) = RLGHTS (ISEG, 2)*TEMP3
         IMAX = PI*ITOT*RLGHTS (ISEG, 2)/(2.*FDAY)
         SUM = 0.0
         DO 1000 I = 1, 25
            DTDAY = FLOAT (I - 1)/24.
            IF (DTDAY .GT. FDAY) GO TO 1010
            I0 = IMAX*SIN (PI*DTDAY/FDAY)
            SUM = SUM + 2.7183/TEMP1*
     1         (EXP ( - TEMP2*I0*TEMP3) - EXP ( - TEMP2*I0))
 1000    CONTINUE
 1010    CONTINUE
         RLIGHT = SUM/24.
         RLGHTS (ISEG, 1) = RLIGHT
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:06:22.
C        Adapt carbon to chlorophyll ratio:
         IAVSG=IAV*(1.0-TEMP3)/TEMP1
         CCHLX(ISEG)=0.3*0.083*PHIMX*XKC*IAVSG/(GITMX1*2.718)
         IF(CCHLX(ISEG).LT.20.0) CCHLX(ISEG)=20.0
         IS1X(ISEG)=1/TEMP2
         IAVBOTX(ISEG)=IAV*TEMP3
      END IF
      RETURN
      END

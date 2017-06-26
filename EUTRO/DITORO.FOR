      SUBROUTINE DITORO
C*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C        Di Toro et al Light Formulation
C*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
      CCHL1 = CCHL
      TCHLA = PHYT/CCHL1
      KESHD = (0.0088*1000.*TCHLA + 0.054*(1000.*TCHLA)**0.6667)
      SKE = KESG (ISEG)
      IF (IKE .GT. 0 .AND. IKE .LE. 5) SKE = SKE*KE (IKE)
      SKE = SKE + KESHD
      TEMP1 = SKE*H
C
C         Get average solar radiation during daylight hours
C
      TEMP2 = IAV/IS1
      TEMP3 = EXP ( - TEMP1)
      IAVBOT=IAV * TEMP3
      RLGHTS (ITO, 2) = RLGHTS (ISEG, 2)*TEMP3
      RLIGHT = 2.718*FDAY/TEMP1*(EXP ( - TEMP2*
     1   TEMP3) - EXP ( - TEMP2))
      RLGHTS (ISEG, 1) = RLIGHT
C
C
      RETURN
      END

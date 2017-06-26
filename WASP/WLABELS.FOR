      SUBROUTINE WLABELS
C
      INCLUDE 'WASP.CMN'
C
      CHARACTER*15 TVOUT(7)
C
      CHARACTER*15 EVOUT(9)
C
      CHARACTER SCOLOR(9)*6
C
      DATA SCOLOR /'CYAN','GREEN','CYAN','GREEN','CYAN','GREEN',
     1             'CYAN','GREEN','CYAN'/
C
      DATA TVOUT /'Total Chem 1','Total Solids 1','Total Solids 2',
     1            'Total Solids 3','Total Chem 2','Total Chem 3',
     2            'Segment Flow'/
C
      DATA EVOUT /'Ammonia NH3 ','Nitrate NO3   ','Inorganic PO4 ',
     1            'Phytoplankton ','BOD         ','Dissolved O2',
     2            'Organic N     ','Organic P   ','Segment Flow'/
C
C-----------------------------------------------------------------------
C
      CALL COLOR ('YELLOW','BLACK')
      IF(SY.EQ.6)THEN
         CALL OUTCEN (1,'TOXI5 -- Intermediate Simulation Results')
         ISPACE=2
      ELSE
         CALL OUTCEN (1,'EUTRO5 -- Intermediate Simulation Results')
         ISPACE=1
      ENDIF
      L =12
      IF(NOSEG .LT. 6)THEN
         NPSEG=NOSEG
      ELSE
         NPSEG=6
      ENDIF
      CALL COLOR ('YELLOW','BLACK')
      DO 100 I=1,NPSEG
         K=ISEGOUT(I)
         L = L + 10
         CALL OUTIXY (L,7,K,5)
  100 CONTINUE
      J =9
      RSPACE = 0.
      DO 200 K = 1,SY+1
         IF (RSPACE .EQ. 1.0)RSPACE=0.
         IF(SY .EQ. 8)RSPACE=RSPACE + 0.5
         CALL COLOR(SCOLOR(K),'BLACK')
         IF (SY.EQ.6)CALL OUTSXY(4,J,TVOUT(K))
         IF (SY.EQ.8)CALL OUTSXY(4,J,EVOUT(K))
         J = J + ISPACE + RSPACE
  200 CONTINUE
      RETURN
      END

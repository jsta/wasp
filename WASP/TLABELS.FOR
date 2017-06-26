      SUBROUTINE TLABELS
C
      INCLUDE 'WASP.CMN'
C
      CHARACTER*15 TVOUT(7)
C
      CHARACTER SCOLOR(9)*6
C
      DATA SCOLOR /'CYAN','GREEN','CYAN','GREEN','CYAN','GREEN',
     1             'CYAN','GREEN','CYAN'/
C
      DATA TVOUT /'Max DT.','Velocity','Depth',
     1            'Volume','Flow In','Flow Out','Dispersion'/
C
C
C-----------------------------------------------------------------------
C
      CALL OUTCEN (1,'TRANSPORT -- Intermediate Simulation Results')
      ISPACE=2
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
      J = 9
      RSPACE = 0.
      DO 200 K = 1,7
         CALL COLOR(SCOLOR(K),'BLACK')
         CALL OUTSXY(4,J,TVOUT(K))
         J = J + ISPACE
  200 CONTINUE
      RETURN
      END

      SUBROUTINE WSCREEN
C
      INCLUDE 'WASP.CMN'
C
C-----------------------------------------------------------------------
C
      CALL CLSCRN
      CALL CURSOR ('OFF')
      CALL ATTRIB('BOLD')
      CALL COLOR ('YELLOW','BLACK')
      IHEAD = (INFOSC(3)-23)/2
      ILEFT = (INFOSC(2)-78)/2
      CALL FRAME2(1+ILEFT,1+IHEAD,79+ILEFT,21+IHEAD,' ',
     1     '                               ')
      CALL HSPLIT(1+ILEFT,4,79+ILEFT)
      CALL HSPLIT(1+ILEFT,6,79+ILEFT)
      CALL HSPLIT(1+ILEFT,8,79+ILEFT)
      CALL VSPLIT(20,6,22)
      CALL VSPLIT(30,6,22)
      CALL VSPLIT(40,6,22)
      CALL VSPLIT(50,6,22)
      CALL VSPLIT(60,6,22)
      CALL VSPLIT(70,6,22)
      IF (INFOAT(7).GT.1) THEN
         CALL WNACTN('C')
      ELSE
         CALL WNACTN(' ')
      ENDIF
C
C  TEXT ATTRIBUTES
C
      CALL WNOPEN(2+ILEFT,3,75,1)
      CALL COLOR ('CYAN','BLACK')
      CALL WNOUSX(1,1,'Simulation Time         Days')
      CALL WNOUSX(30,1,'Percent Complete:    ')
      CALL COLOR ('cyan','black')
      CALL WNOUSX (57,1,'Real Time:        ')
      CALL WNCLOS(0)
      CALL ATTRIB ('B')
      CALL COLOR ('RED','BLACK')
      CALL WNOPEN(2+ILEFT,5,75,1)
      CALL WNOUSX(2,2,' Time Step:                            Segments
     1       Iterating:')
      CALL COLOR('green','black')
      CALL OUTSXY(4,9,'State Variables')
C
      IF (WQ)THEN
         CALL WLABELS
      ELSE
         CALL TLABELS
      ENDIF
      CALL RSCREEN
      RETURN
      END

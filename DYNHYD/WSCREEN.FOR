      subroutine wscreen
      call clscrn
      CALL CURSOR ('OFF')
      call attrib('bold')
      call color ('yellow','black')
      IHEAD = (INFOSC(3)-22)/2
      ILEFT = (INFOSC(2)-78)/2
      CALL FRAME2(1+ILEFT,1+IHEAD,79+ILEFT,23+IHEAD,' ',
     1     '                               ')
      CALL HSPLIT(1+ILEFT,6,79+ILEFT)
      CALL HSPLIT(1+ILEFT,8,79+ILEFT)
      CALL HSPLIT(1+ileft,10,79+ILEFT)
      CALL VSPLIT(20,8,24)
      CALL VSPLIT(30,8,24)
      CALL VSPLIT(40,8,24)
      CALL VSPLIT(50,8,24)
      CALL VSPLIT(60,8,24)
      CALL VSPLIT(70,8,24)
      IF (INFOAT(7).GT.1) THEN
          CALL WNACTN('C')
      ELSE
          CALL WNACTN(' ')
      ENDIF
C
C  text attributes
C
      CALL WNOPEN(2+ILEFT,5,75,1)
      call color ('cyan','black')
      CALL WNOUsx(1,1,'Simulation Time        days')
      call wnousx(30,1,'Percent Complete:    ')
      call color ('cyan','black')
      call wnousx (57,1,'Real Time:        ')
      CALL WNCLOS(0)
      call attrib ('B')
      call color ('red','black')
      CALL WNOPEN(2+ILEFT,7,75,1)
      call wnousx(2,1,'                              Junctions/Channels
     1       Iterating:')
      call color('green','black')
      call outsxy(4,9,'State Variables')
C  pause
C
      call wlabels
      call wupdate
      return
      end

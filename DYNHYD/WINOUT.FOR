      SUBROUTINE winout (TEXT, ilineS,ISTOP)
      INCLUDE 'DYNHYD.CMN'
      CHARACTER*50 TEXT(24)
      INTEGER ILINES, BORDER
      CALL ATTRIB('B')
      CALL SCBACK ('BLUE','WHITE')
      CALL COLOR ('cyan', 'black')
      IF (INFOAT(7).GT.1) THEN
          CALL WNACTN('FC')
      ELSE
          CALL WNACTN(' ')
      ENDIF
C
C  text attributes
      CALL CURSOR('OFF')
      itop = 12 - (ilineS/2)
      CALL WNCLOS(1)
c      CALL CLSCRN
      CALL WNOPEN(15,itop,50,ilineS)
      do 100 i =1,ilineS
         call wnouce(i,text(i))
  100 continue
      CALL WNCLOS(0)
      IF (ISTOP .EQ. 1 .AND. .NOT. WISP) THEN
          CALL OUTMES('Press any key to continue.')
          call gpause('a')
      endif
      call attrib(' ')
      return
      end

      SUBROUTINE WINOUT (TEXT, ILINES,ISTOP)
C=======================================================================
      INCLUDE 'WASP.CMN'
C
      CHARACTER*50 TEXT(24)
C
      INTEGER ILINES, BORDER
C
C=======================================================================
C
      CALL ATTRIB('B')
C
      CALL SCBACK ('BLUE','WHITE')
C
      CALL COLOR ('YELLOW', 'BLACK')
C
      IF (INFOAT(7).GT.1) THEN
         CALL WNACTN('FC')
      ELSE
         CALL WNACTN(' ')
      ENDIF
C
      CALL CURSOR('OFF')
C
      ITOP = 12 - (ILINES/2)
C
      CALL WNCLOS(1)
C
      CALL WNOPEN(15,ITOP,50,ILINES)
C
      DO 100 I =1,ILINES
         CALL WNOUCE(I,TEXT(I))
  100 CONTINUE
C
      CALL WNCLOS(0)
C
      IF (ISTOP .EQ. 1 .AND. .NOT. WISP )THEN
          CALL OUTMES('Press any key to continue.')
          CALL GPAUSE('A')
      ENDIF
C
      CALL ATTRIB(' ')
C
      RETURN
      END

      SUBROUTINE PROMPT(MESSAGE,ICOL)
C
      CHARACTER MESSAGE*(*)
C
C-----------------------------------------------------------------------
      CALL REVERS('ON')
C
      IF (ICOL .EQ. 0 .OR. ICOL .EQ. 3)THEN
         CALL COLOR ('WHITE','BLUE')
      ELSE
         CALL ATTRIB('F')
         CALL COLOR ('WHITE','RED')
      ENDIF
C
      CALL OUTMES(MESSAGE)
      IF(ICOL .NE. 3)CALL BLEEP (' ')
      CALL REVERS(' ')
      IF (ICOL .LE. 1)CALL INKEY(KEY)
      IF (ICOL .LE. 1)CALL ATTRIB('BOLD')
      CALL COLOR('BLACK','BLACK')
      RETURN
      END

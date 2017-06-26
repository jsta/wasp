      SUBROUTINE PRTSCR (ILINES, TEXT, BORDER, istop)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:44.
C
C
C  Correction History:  Added option of printing with or without an
C                       asterisk border.
C
C
C----------------------------------------------------------------------
C
      CHARACTER*50 TEXT (24), TMPTXT, JUNK
      CHARACTER TXTOUT*54
      INTEGER ILINES, BORDER
c
      LSKIP = (24 - ILINES)/2
      CALL CLRSCR
      DO 1000 K = 1, LSKIP - 1
         WRITE (6, 6000)
 6000      FORMAT(1X,' ')
 1000 CONTINUE
C
C If BORDER=0 then do not print asterisks and left-justify the text.
C Otherwise, center text and print a border.
C
      if (border .ne. 0) then
         WRITE (6, 6010)
 6010   FORMAT(12X,54('Û'))
c
         DO 1010 I = 1, ILINES
            TXTOUT (1:1) = 'Û'
            TXTOUT (2:53) = ' '
            TXTOUT (54:54) = 'Û'
            TMPTXT = TEXT (I)
c
            DO 1020 J = 50, 1, - 1
               IF (TMPTXT (J:J) .NE. ' ') GO TO 1030
 1020       CONTINUE
c
            ISPACE = 0
            GO TO 1040
c
 1030       CONTINUE
            ISPACE = 51 - J
c
 1040       CONTINUE
            IBUFF = 3 + (ISPACE/2)
            TXTOUT (IBUFF:53) = TMPTXT
            WRITE (6, 6020) TXTOUT
 6020     FORMAT(12X,A54)
 1010    CONTINUE
c
         WRITE (6, 6010)
c
      else
         DO 1050 I = 1, ILINES
            write (6, 6020) TEXT (I)
 1050    continue
      end if
c
      DO 1060 I = 1, LSKIP - 1
         WRITE (6, 6030)
 6030     FORMAT(1X,' ')
 1060 CONTINUE
c
      if (istop .eq. 1) then
         Write (6, 6040)
 6040   Format(15x,'==========> PRESS ENTER TO CONTINUE <============')
         READ (5, 5000) JUNK
 5000   FORMAT(A1)
      END IF
      RETURN
      END

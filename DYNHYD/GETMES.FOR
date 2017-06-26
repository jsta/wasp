      SUBROUTINE GETMES (MESSNO,ISTOP)
      include 'dynhyd.cmn'
      CHARACTER*50 TEXT(24)
      character*50  TXTMP
C
      OPEN(UNIT=19,STATUS='OLD',ACCESS='SEQUENTIAL',FORM='FORMATTED',
     1     FILE=mespath, IOSTAT = ISTAT)
C
      if (istat .gt. 0)then
         write(6,6000)
 6000    format (15(/),15x,'DYNHYD program cannot find the runtime',
     1   /,15x,'message file "DYNMES.DAT".  This file must be',
     2   'in the directory the program is executed from.')
         stop
      endif
C  Read message file.  Since the message numbers are in increasing order,
C  close the file after message number is found, otherwise continue reading.
C
      ILINES = 0
 1000 CONTINUE
      READ (19, 5000, END = 1010) MESTMP, TXTMP
 5000   FORMAT(I5,1X,A50)
      IF (MESSNO .GT. MESTMP) THEN
        GO TO 1000
      ELSE IF (MESSNO .LT. MESTMP) THEN
        GO TO 1010
      ELSE
        ILINES = ILINES + 1
        TEXT (ILINES) = TXTMP
        GO TO 1000
      ENDIF
C
 1010 CONTINUE
      call winout(text,ilineS,ISTOP)
      CLOSE(UNIT=19)
      RETURN
      END

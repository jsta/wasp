      SUBROUTINE KEYCHECK
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:10.
C
C  Correction History:
C
C
C---------------------------------------------------------------------
      INCLUDE 'WASP.CMN'
C
      CHARACTER TSEG(SG)*10
C
C-----------------------------------------------------------------------
      CALL INCHRI(ICHR)
      IF (ICHR .EQ. 83 .OR. ICHR .EQ. 115)CALL BLEEP('OFF')
      IF (ICHR .EQ. 27)THEN
         CALL COLOR ('WHITE', 'BLUE')
         CALL HIGHLT ('BLUE', 'WHITE')
         CALL HIGHLT ('CYAN', 'BLACK')
         IOPT = MNTWO ('Yes', 'No', 0, 0,
     1      'Quit Simulation: ', 1, 1)
         IF (IOPT .EQ. 1) THEN
            CALL WERR(30,1,0)
            CALL WEXIT('User Aborted Simulation',1)
         ELSE
            CALL COLOR ('BLACK','BLACK')
            CALL CLSCRN
            IF(MFLAG.LT.2)CALL WSCREEN
            IF(MFLAG.LT.2)CALL RSCREEN
            IF (WQ)THEN
               IF(MFLAG.LT.2)CALL WLABELS
            ELSE
               IF(MFLAG.LT.2)CALL TLABELS
            ENDIF
         ENDIF
      ENDIF
C
      IF (ICHR .EQ. 84 .OR. ICHR .EQ. 116)THEN
         CALL CLSCRN
         CALL WNCLOS(0)
         WQ=.FALSE.
         IF(MFLAG.LT.2)CALL WSCREEN
      ENDIF
C
      IF (ICHR .EQ. 87 .OR. ICHR .EQ. 119)THEN
         WQ=.TRUE.
         CALL WNCLOS(0)
         CALL CLSCRN
         IF(MFLAG.LT.2)CALL WSCREEN
      ENDIF
C
      IF (ICHR .EQ. 67 .OR. ICHR .EQ. 99)THEN
         IF(NOSEG .GT. 6)THEN
         DO 10 I =1,NOSEG
             CALL INTSTR(I,TSEG(I),'(I10)')
 10      CONTINUE
         IMOVE=1
         DO 20 I = 1, 6
             IOPT = MNSCRL(TSEG,NOSEG, 0, 0,'Segments',5, 2,IMOVE)
             ISEGOUT(I)=IOPT
             TSEG(IOPT)(1:1)='*'
             IMOVE=IOPT+1
 20      CONTINUE
            CALL WNCLOS(0)
            CALL CLSCRN
            CALL ATTRIB(' ')
         ELSE
            DO 25 I=1,NOSEG
               ISEGOUT(I)=I
 25         CONTINUE
         ENDIF
C
         IF (WQ)THEN
            IF(MFLAG .LT. 2)CALL WSCREEN
            IF(MFLAG .LT. 2)CALL WLABELS
            IF(MFLAG .LT. 2)CALL WUPDATE
         ELSE
            IF(MFLAG .LT. 2)CALL WSCREEN
            IF(MFLAG .LT. 2)CALL TLABELS
         ENDIF
      ENDIF
      RETURN
      END

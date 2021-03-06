      SUBROUTINE WUPDATE
C
      INCLUDE 'WASP.CMN'
C
      REAL SYSDUMP(SY+1)
C
      CHARACTER SCOLOR(9)*6
C
      DATA SCOLOR /'CYAN','GREEN','CYAN','GREEN','CYAN','GREEN',
     1             'CYAN','GREEN','CYAN'/
C-----------------------------------------------------------------------
      PERTIME = (((TIME - TZERO)/(FTIME - TZERO))*100.)
      IF(SY.EQ.6)ISPACE=2
      IF(SY.EQ.8)ISPACE=1
      CALL COLOR ('GREEN','BLACK')
      CALL OUTRXY(18,3,TIME,'(F8.2)')
      CALL OUTRXY(50,3,PERTIME,'(F6.2)')
      CALL OSTIME (IHOUR,IMINUTE,ISEC)
      CALL OUTIXY (70,3,IHOUR,2)
      CALL OUTSXY (72,3,':')
      CALL OUTIXY (73,3,IMINUTE,2)
      CALL OUTSXY (75,3,':')
      CALL OUTIXY (76,3,ISEC,2)
C
      IF(NOSEG .LT. 6)THEN
         NPSEG=NOSEG
      ELSE
         NPSEG=6
      ENDIF
C
      ILINE=9
      RSPACE = 0.
      DO 100 I =1,SY+1
         IF (RSPACE .EQ. 1.0)RSPACE=0.
         IF(SY .EQ. 8)RSPACE=RSPACE + 0.5
         ICOL=11
         CALL COLOR(SCOLOR(I),'BLACK')
         DO 200 J = 1, NPSEG
            ICOL=ICOL + 10
            IF (I .LE. SY)THEN
               SYSDUMP(J)=C(I,ISEGOUT(J))
               IF(SYSDUMP(J) .LT. 1.0E-15)SYSDUMP(J)=0.00
               CALL OUTRXY(ICOL,ILINE,SYSDUMP(J),'(E9.3)')
            ELSE
               SYSDUMP(J)=QSEG(ISEGOUT(J))
               IF(SYSDUMP(J) .LT. 1.0E-15)SYSDUMP(J)=0.00
               CALL OUTRXY(ICOL,ILINE,SYSDUMP(J),'(E9.3)')
            ENDIF
  200    CONTINUE
         ILINE=ILINE+ISPACE+RSPACE
  100 CONTINUE
      CALL RSCREEN
      RETURN
      END
                                                                
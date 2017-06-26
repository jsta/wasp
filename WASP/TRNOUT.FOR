      SUBROUTINE TRNOUT
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:09.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      DIMENSION DRTOT (SG), AXL (SG), DQIN (SG), DQOUT (SG)
C
      CHARACTER*30 TNAME (18)
C
      REAL SYSDUMP(7,SG)
C
      CHARACTER SCOLOR(9)*6
C
      DATA SCOLOR /'cyan','GREEN','cyan','GREEN','cyan','GREEN',
     1             'cyan','GREEN','cyan'/
C
      DATA TNAME/'Time Step      (days)','Maximum Calc DT(days)',
     *           'Velocity       (m/sec)','Depth          (m)',
     *           'Volume         (m3)','Average Flow   (cms)',
     *           'Flow into Seg  (cms)','Flow out of Seg(cms)',
     *           'Tot. Dispersion(m2/day)',
     *           'Retention Time (days)','Exchange Time  (days)',
     *           'Total Ret. Time(days)','Ave. Dispersion(m3/sec)',
     *           'Numerical Disp.(m3/sec)','Sediment Porosity',
     *           'DUMMY2','DUMMY3','DUMMY4'/
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C                   Initialize the Dump File
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
      IF (ITRNIT .EQ. 0) THEN
         TRNUM = 1
         WRITE (ITRNS, 6000) TRNUM
 6000    FORMAT (I5)
         WRITE (ITRNS, 6010)
 6010    FORMAT(1X)
         WRITE (ITRNS, 6020) TIME, TEND, NOSEG
 6020    FORMAT (1X, F10.2,F10.2, /, I5)
         WRITE (ITRNS, 6030) (TNAME (J), J = 1, 6 + (12*TRNUM))
 6030    FORMAT (A30)
         ITRNIT=1
      END IF
CC=======================================================================
C                 Dump Transport Values to Disk
C=======================================================================
C
      NF = 1
      DO 80 ISEG = 1, NOSEG
         DQIN(ISEG) = 0.0
         DQOUT(ISEG) = 0.0
         DRTOT(ISEG) = 0.0
         AXL(ISEG) = 0.0
   80 CONTINUE
      DO 1050 NI = 1, NINQ (NF)
         NOQ = NOQS (NF, NI)
         DO 1060 NQ = 1, NOQ
            J = JQ (NF, NI, NQ)
            I = IQ (NF, NI, NQ)
            QQ = BQ (NF, NI, NQ) * QINT(NF,NI)/86400.
            IF (QQ .GE. 0.) THEN
               IF (I .GT. 0) DQIN (I) = DQIN (I) + QQ
               IF (J .GT. 0) DQOUT (J) = DQOUT (J) + QQ
            ELSE
               IF (J .GT. 0) DQIN (J) = DQIN (J) - QQ
               IF (I .GT. 0) DQOUT (I) = DQOUT (I) - QQ
            END IF
 1060    CONTINUE
 1050 CONTINUE
      FAC = 1.0 - 2.0*ADFAC
      NF = 1
      IF (NTEX (NF) .EQ. 0) GO TO 1070
      DO 1080 NI = 1, NTEX (NF)
         NOR = NORS (NF, NI)
         DO 1090 NR = 1, NOR
            AL = BR (NF, NI, NR)
            J = JR (NF, NI, NR)
            I = IR (NF, NI, NR)
            RR = BR (NF, NI, NR) * BRINT(NF,NI)
            IF (I .GT. 0) THEN
               DRTOT (I) = DRTOT (I) + RR
               AXL (I) = AXL (I) + AL
            END IF
            IF (J .GT. 0) THEN
               DRTOT (J) = DRTOT (J) + RR
               AXL (J) = AXL (J) + AL
            END IF
 1090    CONTINUE
 1080 CONTINUE
 1070 CONTINUE
      DO 1100 ISEG = 1, NOSEG
         DEBAR = 0.
         DENUM = 0.
         DQBAR = (DQIN (ISEG) + DQOUT (ISEG))/2.0
         IF (DQBAR + DRTOT (ISEG) .GT. 0) THEN
            DTMX = BVOL (ISEG)/((DQBAR*86400.) + DRTOT (ISEG))
         ELSE
            DTMX = 999999.
         END IF
         DELTAQ = DQIN (ISEG) - DQOUT (ISEG)
         TCONSQ = DQBAR * 86400./BVOL (ISEG)
         IF (DQBAR .GT. 0.)THEN
              HRESTIME=BVOL(ISEG)/DQBAR
         ELSE
              HRESTIME=999999.99
         ENDIF
         IF (DRTOT(ISEG) .GT. 0.)THEN
              DRESTIME=BVOL(ISEG)/DRTOT(ISEG)
         ELSE
              DRESTIME=999999.99
         ENDIF
         TCONSR = DRTOT (ISEG)*86400./BVOL (ISEG)
         IF (AXL (ISEG) .GT. 0) THEN
            DEBAR = DRTOT (ISEG)/AXL (ISEG)
            DRNUM = 0.5*DQBAR*(FAC - TCONSQ*DT)
            DENUM = DRNUM/AXL (ISEG)
         END IF
         SYSDUMP(1,ISEG)=DTMX
         SYSDUMP(2,ISEG)=VELOCG(ISEG)
         SYSDUMP(3,ISEG)=DEPTHG(ISEG)
         SYSDUMP(4,ISEG)=BVOL(ISEG)
         SYSDUMP(5,ISEG)=DQIN(ISEG)
         SYSDUMP(6,ISEG)=DQOUT(ISEG)
         SYSDUMP(7,ISEG)=DRTOT(ISEG)
         DUMMY1=0.
         DUMMY2=0.
         DUMMY3=0.
         DUMMY4=0.
         DUMMY5=0.
         DUMMY6=0.
         IF(DT .GT. DTMX .AND. INTYP .EQ. 0)THEN
            CALL WEXIT('WASP has calculated a smaller time step than th
     1e one provided',1)
         ENDIF
         WRITE (ITRNS, 6040)ISEG,TIME,DT,DTMX,VELOCG(ISEG),DEPTHG(ISEG),
     1   BVOL(ISEG),DQBAR, Dqin(iseg),dqout(iseg),DRTOT(ISEG),hrestime,
     2   drestime,dtmx,DEBAR,DENUM,frw(iseg),dummy2,dummy3,dummy4
 6040    FORMAT(1X,I5,F12.4,3X,/,6(E11.3),/,6(E11.3))
 1100 CONTINUE
C
      IF (.NOT. WQ. AND. MFLAG .LT. 2)THEN
         PERTIME = (((TIME - TZERO)/(FTIME - TZERO))*100.)
         ISPACE=2
         CALL COLOR ('GREEN','BLACK')
         CALL OUTRXY(18,3,TIME,'(F8.2)')
         CALL OUTRXY(50,3,PERTIME,'(F6.2)')
         CALL OSTIME (IHOUR,IMINUTE,ISEC)
         CALL OUTIXY (70,3,IHOUR,2)
         CALL OUTSXY (72,3,':')
         CALL OUTIXY (73,3,IMINUTE,2)
         CALL OUTSXY (75,3,':')
         CALL OUTIXY (76,3,ISEC,2)
            IF(NOSEG .LT. 6)THEN
               NPSEG=NOSEG
            ELSE
               NPSEG=6
            ENDIF
         ILINE=9
         RSPACE = 0.
      DO 100 I =1,7
         IF (RSPACE .EQ. 1.0)RSPACE=0.0
         ICOL=11
         CALL COLOR(SCOLOR(I),'BLACK')
         DO 200 J = 1, NPSEG
           ICOL=ICOL + 10
           IF(SYSDUMP(I,ISEGOUT(J)) .LT. 1.0E-15)
     1               SYSDUMP(I,ISEGOUT(J))=0.00
           CALL OUTRXY(ICOL,ILINE,SYSDUMP(I,ISEGOUT(J)),'(E9.3)')
  200    CONTINUE
         ILINE=ILINE+ISPACE
  100 CONTINUE
      ENDIF
      RETURN
      END

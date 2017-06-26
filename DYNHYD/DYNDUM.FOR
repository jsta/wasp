      SUBROUTINE DYNDUM
C======================================================================
c
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C write results into dump file *.dmp
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C
      DATA DNAME/'Channel Flow   (cms)' ,
     * 'Channel Vel.   (m/sec)       ',
     * 'Upstream Head  (m)           ',
     * 'Downstream Head(m)                                        ',
     * 'Mannings Coeff              ',
     * 'Upstrem Depth  (m)           ',
     * 'Downstr. Depth (m)           ',
     * 'UpStream Flow  (cms)         ',
     * 'Downstream Flow(cms)         ',
     * 'UpStream Q Dir (degrees)     ',
     * 'Downstr. Q Dir (degrees)     ',
     * 'UpStr. Velocity(m/sec)       ',
     * 'Downstr Vel.   (m/sec)       ',
     * 'Channel Momentum             ',
     * 'Channel Friction             ',
     * 'Channel Gravity              ',
     * 'Wind on Channel              ',
     * 'dummy                        '/
      IF (TINIT .EQ. 0) THEN
         NCHEM = 1
         ihyd = 1
         WRITE (IDMP, 6000) NCHEM, ihyd
C
         BEGTI = TZERO/86400.
         ENDTI = TEND/86400.
         WRITE (IDMP, 6010) BEGTI, ENDTI, NC
C
         WRITE (IDMP, 6020) (DNAME (I), I = 1, 6 + (12*NCHEM))
         TINIT = 1
      END IF
C     CONVERTING TIME (HRS) TO TIME (DAYS)
      DUTIM = TIME/24.
C
      DO 1020 N = 1, NC
         QSAVE (N) = Q (N)
         VSAVE (N) = V (N)
         RSAVE (N) = R (N)
 1020 CONTINUE
      CALL AVGSG
cc
C      ENDTI = TEND/86400.
      ENDTI = TEND/86400.
      DUTIME = T/86400.
      begti=tzero/86400.
      pertime = ((dUtimE-begti)/(endti-begti)) * 100.
      call color ('green','black')
      call outrxy(19,5,DUtime,'(f6.2)')
      call outrxy(50,5,pertime,'(f6.2)')
      call ostime (ihour,iminute,isec)
      call outixy (70,5,ihour,2)
      call outsxy (72,5,':')
      call outixy (73,5,iminute,2)
      call outsxy (75,5,':')
      call outixy (76,5,isec,2)
C
      DO 1000 I = 1, NC
         J1 = NJUNC (I, 1)
         J2 = NJUNC (I, 2)
         dg1 = y(j1) - belev(j1)
         dg2 = y(j2) - belev (j2)
         do 3 k=1,6
         if (i .eq. isegout(k))then
            sysdump(1,k)=q(i)
            sysdump(2,k)=v(i)
            sysdump(3,k)=y(j1)
            sysdump(4,k)=y(j2)
            sysdump(5,k)=cn(i)
            sysdump(6,k)=mom(i)
            sysdump(7,k)=win(i)
          endif
   3     continue
       call wupdate
       dum15=0.
       WRITE (IDMP, 6030) I, DUTIM, Q(I), V(I), Y(J1), Y(J2),
     1           CN(I),
     2           DG1, DG2, FLOWG(J1), FLOWG(J2),
     3           QDIR(J1), QDIR(J2),velocg(j1),velocg(j2),
     4           mom(i), fric(i), grav(i), win(i),dum15
c     4           dum15, dum16, dum17, dum18,dum15
c     2           dum9,dum10,dum11, dum12, dum13, dum14,
 1000 CONTINUE
c      call wscreen
C ******************************************************************
C              FORMAT STATEMENTS
C ******************************************************************
 6000   FORMAT(I5, 35x, I5,/)
 6010   FORMAT(1X,F7.2,3X,F7.2,/,I5)
 6020   FORMAT(A30)
 6030   FORMAT(1X,I5,F10.4,/,6E11.3,/,(6(E11.3)))
C ******************************************************************
      call keycheck
      RETURN
      END

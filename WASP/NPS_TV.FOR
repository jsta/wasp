      SUBROUTINE  NPS_TV(NPSFIL)
C======================================================================
C  Last Revised:  Date: Thursday, 28 March 1993
C
C  Correction History:
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      CHARACTER*12 NPSFIL
C
C======================== NPS Initialization ==========================
      IF (NPSINIT .EQ. 0)THEN
           OPEN(UNIT=AUX,STATUS='OLD',FORM='FORMATTED',FILE=NPSFIL)
           READ(AUX,1000)NPSTYP,NUMLOAD,INTOPT,NUMSYS
 1000      FORMAT(A15,I5,I5,I5)
           IF(INTOPT .EQ. 0)THEN
              INTERDES='LINEAR'
           ELSE
              INTERDES='STEP'
           ENDIF
           READ(AUX,1050)(NPS_SEG(I), I = 1, NUMLOAD)
 1050      FORMAT(I5)
           DO 10 I = 1, NUMLOAD
             NP_SEG(I)=NPS_SEG(I)
   10      CONTINUE
           READ(AUX,1100)(NPS_SYS(I), I = 1, NUMSYS)
 1100      FORMAT(8(I5))
           DO 20 I = 1, NUMSYS
             LDSYS(I)=NPS_SYS(I)
   20      CONTINUE
           DO 100 I = 1, NUMSYS
             READ(AUX,1200)NPSNAME(I)
 1200        FORMAT(A15)
  100      CONTINUE
           WRITE(OUT,6000)
 6000      FORMAT(1X,75('*'))
           WRITE(OUT,6100)
 6100      FORMAT(1x,/,28x,'Non Point Source Linkage',/)
           WRITE(OUT,6000)
           WRITE(OUT,6200)NPSTYP,NUMLOAD
 6200      FORMAT(1x,'     Linkage Type: ',a15,5x,'Number of Segments',
     +         ' Linked: ',i5)
           WRITE(OUT,6250)NUMSYS,INTERDES
 6250      FORMAT(1x,'     Number of Systems: ',i5,10x,'Interpolation',
     +         ' Option: ',a10,//)
           WRITE(OUT,6300)
 6300      FORMAT(27x,'Segment(s) Receiving Loads')
           WRITE(OUT,6400)
 6400      FORMAT(27X,'==========================')
           DO 200 I = 1, NUMLOAD
              WRITE(OUT,6500)NPS_SEG(I)
 6500         FORMAT(37X,I5)
  200      CONTINUE
           WRITE(OUT,6600)
 6600      FORMAT(//,34X,'NPS SYSTEMS')
           WRITE(OUT,6700)
 6700      FORMAT(34X,'===========',/)
           DO 300 I = 1, NUMSYS
             WRITE(OUT,6800)NPSNAME(I)
 6800        FORMAT(32X,A15)
  300      CONTINUE
           WRITE(OUT,6000)
           RETURN
      ENDIF
C======================== End Initialization ==========================
C========================  Begin Main NPS Linkage ===================
      DO 500 I = 1, NUMSYS
         READ(AUX,1400)(NP_LOAD(LDSYS(I),NP_SEG(K)),K=1,NUMLOAD)
 1400    FORMAT(15X,20(F10.0))
  500 CONTINUE
C
      CALL NPS_CHECK
      READ(AUX,1300,ERR=9998,END=9999)NPTIME
 1300 FORMAT(F10.0)
C
      RETURN
 9998 CALL PROMPT(' Error encountered in NPS file; Loads set to ZERO
     + <ENTER> to continue',0)
      NPTIME=1.0E20
      RETURN 
 9999 CALL PROMPT(' End of NPS File Encountered; Loads set to ZERO
     + <ENTER> to continue',0)
      NPTIME=1.0E20
      RETURN 
      END

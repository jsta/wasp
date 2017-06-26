      SUBROUTINE WASPB
C
C     Last Revised:  Date: Monday, 26 August 1991.  Time: 10:37:42.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
      INCLUDE 'CONST.INC'
C
      CHARACTER*1 PROP(4)
      REAL XARG
      DATA PROP /'|','/','-','\'/
C              Initialize Internal Variables
C
      IF (INITB .EQ. 0) THEN
         CALL EUTRODMP
         CALL EUTROINT
         KAINIT = 0
      END IF
C
      XKFLUX = 0.0
C
C            Evaluate piecewise linear time functions
C            ----------------------------------------
C
      IF (TIME .GE. NTF) CALL WASP8 (MFUNC, BFUNC,
     1   NFUNC, 4, ITIME, NTF, 73)
      TEMP (1) = MFUNC (1)*(TIME - NFUNT (1)) + BFUNC (1)
      TEMP (2) = MFUNC (2)*(TIME - NFUNT (2)) + BFUNC (2)
      TEMP (3) = MFUNC (3)*(TIME - NFUNT (3)) + BFUNC (3)
      TEMP (4) = MFUNC (4)*(TIME - NFUNT (4)) + BFUNC (4)
      ITOT     = MFUNC (5)*(TIME - NFUNT (5)) + BFUNC (5)
      FDAY     = MFUNC (6)*(TIME - NFUNT (6)) + BFUNC (6)
      WIND     = MFUNC (7)*(TIME - NFUNT (7)) + BFUNC (7)
      KE (1)   = MFUNC (8)*(TIME - NFUNT (8)) + BFUNC (8)
      KE (2)   = MFUNC (9)*(TIME - NFUNT (9)) + BFUNC (9)
      KE (3)   = MFUNC (10)*(TIME - NFUNT (10)) + BFUNC (10)
      KE (4)   = MFUNC (11)*(TIME - NFUNT (11)) + BFUNC (11)
      KE (5)   = MFUNC (12)*(TIME - NFUNT (12)) + BFUNC (12)
      TFNH4    = MFUNC (13)*(TIME - NFUNT (13)) + BFUNC (13)
      TFPO4    = MFUNC (14)*(TIME - NFUNT (14)) + BFUNC (14)
      VELN (1) = MFUNC (15)*(TIME - NFUNT (15)) + BFUNC (15)
      VELN (2) = MFUNC (16)*(TIME - NFUNT (16)) + BFUNC (16)
      VELN (3) = MFUNC (17)*(TIME - NFUNT (17)) + BFUNC (17)
      VELN (4) = MFUNC (18)*(TIME - NFUNT (18)) + BFUNC (18)
      ZOO      = MFUNC (19)*(TIME - NFUNT (19)) + BFUNC (19)
      SALFN    = MFUNC (20)*(TIME - NFUNT (20)) + BFUNC (20)
      AIRTMP   = MFUNC (21)*(TIME - NFUNT (21)) + BFUNC (21)
      xicecvr  = MFUNC (22)*(TIME - NFUNT (22)) + BFUNC (22)
      rear     = MFUNC (23)*(TIME - NFUNT (23)) + BFUNC (23)
C
CCSC
      XARG = ABS(zoo)
C      if (zoo .eq. 0.)zoo = 1.
      IF (XARG .LT. R0MIN) zoo = 1.
      IDFREC = MXSEG*(IREC - 1) + 1
C
      CALL SETIA (IDFRC, MXSYS, 1, IDFREC)
C
C          Set up light flags for Dick Smith formulation
C
C
C                  S E G M E N T   L O O P
C                 -------------------------
C
      call color ('yellow','black')
      DO 1000 ISEG = 1, NOSEG
C
C             Compute current SEGMENT conditions
C
        IF (IPROP .GT. 3)IPROP=0
         IPROP=IPROP + 1
         IF(MFLAG .LT. 2)CALL OUTSXY (72,5,PROP(IPROP))
         NH3 = C (1, ISEG)
         NO3 = C (2, ISEG)
         OPO4 = C (3, ISEG)
         PHYT = C (4, ISEG)
         CBOD = C (5, ISEG)
         DO = C (6, ISEG)
         ON = C (7, ISEG)
         OP = C (8, ISEG)
         VOL = BVOL (ISEG)
         H = DEPTHG (ISEG)
         SA = VOL/H
         IFUN = VELFN (ISEG)
         VEL = VELOCG (ISEG)
C
         IF (IFUN .GT. 0 .AND. IFUN .LE. 4) VEL = VEL + VELN (IFUN)
         IFUN = TMPFN (ISEG)
         STP = TMPSG (ISEG)
C
         IF (IFUN .GT. 0 .AND. IFUN .LE. 4) STP = STP*TEMP (IFUN)
         STP20 = STP - 20
C
C                 Set up sediment SEGMENT flag
C
         SEDSEG = .FALSE.
         IF (ITYPE (ISEG) .GE. 3.) SEDSEG = .TRUE.
         ITO = IBOTSG (ISEG)
C
C        Calculate Segment Specific Reaeration Values based on
C        time function REAR and segment Parameter REARSG
C
CCSC
      XARG = ABS(rear)
C      if (rear .eq. 0.)rear=1.
      IF (XARG .LT. R0MIN) rear=1.
C
C     Set Segment Specific Zoo Plankton to 1. if not entered by the
C     User
C
CCSC
      XARG = ABS(zoosg(iseg))
C      if (zoosg(iseg) .eq. 0.)zoosg(iseg)=1.0
      IF (XARG .LT. R0MIN) zoosg(iseg)=1.0
C
C
CCSC
      XARG = ABS(itotlim(iseg))
C      if(itotlim(iseg) .eq. 0.)itotlim(iseg)=1.
      IF (XARG .LT. R0MIN) itotlim(iseg)=1.
      itotmp = itot * itotlim(iseg)
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:03:03.
C      IAV = ITOTmp*RLGHTS (ISEG, 2)/FDAY
      IAV = 0.9*ITOTmp*RLGHTS (ISEG, 2)/FDAY
C
C                        Compute derivatives
C              For PHYT, OP, OPO4, ON, NH3, NO3, CBOD, DO
C
         CALL PHYTO
C
         CALL ORGANOP
C
         CALL INORGANP
C
         CALL ORGANICN
C
         CALL AMMONIA
C
         CALL NITRATE
C
         CALL CBODSV
C
         CALL DISSOXYG
C
C                Check if time to dump to disk
C
         IF (IDISK .NE. 0) CALL EUTRODMP
C
 1000 CONTINUE
C
C             End SEGMENT loop
C
C       Exchange of dissolved phases between water column and sediment
C
      CALL BENTFLUX
C
      DO 1010 ISEG = 1, NOSEG
         IF (JMASS .NE. 0) THEN
            XKFLUX = (XKFLUX + CD (JMASS, ISEG)/1000.)
         END IF
 1010 CONTINUE
      RETURN
      END

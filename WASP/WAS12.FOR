      SUBROUTINE WAS12
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:05.
C
C
C  Correction History: Added qsum array, 9/9/87, rba
C
C
C----------------------------------------------------------------------
C        WAS12 COMPUTES THE PORTION OF THE DERIVATIVE DUE TO TRANSPORT
C        AND LOADS, AND ADJUSTS VOLUMES FOR CONTINUITY (IF IVOPT=2)
C
C
      REAL LOAD
C
      INCLUDE 'WASP.CMN'
C
      EQUIVALENCE (TIME, T)
C
      CHARACTER DUMMY*12
C
C-----------------------------------------------------------------------
C        FLOW AND EXCHANGE CONVENTION
C          (I,J) --- (TO,FROM)
C
C          IF I=0 THEN DOWNSTREAM B.C.
C          IF J=0 THEN UPSTREAM B.C.
C
C        TRANSPORT DUE TO FLOW
C
C***********************************************************************
C                    Update Boundary Conditions
C***********************************************************************
C
      AIFLUX = 0.
      AOFLUX = 0.
      RIFLUX = 0.
      ROFLUX = 0.
      XLFLUX = 0.
      DO 1000 ISYS = 1, NOSYS
         IF (SYSBY (ISYS) .EQ. 0 .AND. NOBC (ISYS) .GT. 0) THEN
            IF (TIME .GE. NTB (ISYS)) CALL WAS8A (30)
         END IF
         DO 1010 ISEG = 1, NOSEG
CRBA--Date: Tuesday, 1 June 1993.  Time: 09:36:01.
C            SUMM (ISYS, ISEG) = - CD (ISYS, ISEG)
            SUMM (ISYS, ISEG) = - CD (ISYS, ISEG)*5.0
 1010    CONTINUE
 1000 CONTINUE
C
C***********************************************************************
C        Compute Advective Derivative and Adjust Segment Volumes
C                        Loop Through Systems
C***********************************************************************
C
C   FIRST DO SURFACE WATER FLOW FIELD
C
      IF (IQOPT .LE. 2) THEN
         IF (IQOPT .EQ. 1) CALL QSURF1
         IF (IQOPT .EQ. 2) CALL QSURF2
      ELSE
         CALL QHYDRO
      END IF
      IF (NINQ (2) .GT. 0) CALL QPORE
      IF (NFIELD .GE. 3) CALL QSED
      IF (NINQ (6) .GT. 0) CALL QEVAP
C
C***********************************************************************
C        TRANSPORT DUE TO EXCHANGE
C***********************************************************************
C
C  LOOP THROUGH SYSTEMS
C
      DO 1020 ISYS = 1, NOSYS
         IF (RBY (ISYS) .NE. 0) GO TO 1020
C
C   LOOP THROUGH FIELDS
C
         DO 1030 NF = 1, NRFLD
C
C    LOOP THROUGH TIME FUNCTIONS
C
            IF (NTEX (NF) .EQ. 0) GO TO 1030
            DO 1040 NT = 1, NTEX (NF)
C
C     SEGMENT LOOP
C
               DO 1050 K = 1, NORS (NF, NT)
                  I = IR (NF, NT, K)
                  J = JR (NF, NT, K)
                  AVPOR = 0.0
                  BNUM = 0.0
                  BLKR = BR (NF, NT, K)*BRINT (NF, NT)
                  CALL WA12A (I, J, NOBC (ISYS), CONC1, CONC2,
     1               ADVECT, IERR)
                  IF (IERR .GT. 0) GO TO 1060
                  IF (I .GT. 0) THEN
                     CONC1 = CONC1*F (NF, ISYS, I)
                     IF (NF .EQ. 2) THEN
                        CONC1 = CONC1/FRW (I)
                        AVPOR = FRW (I)
                        BNUM = 1.0
                     END IF
                  END IF
                  IF (J .GT. 0) THEN
                     CONC2 = CONC2*F (NF, ISYS, J)
                     IF (NF .EQ. 2) THEN
                        CONC2 = CONC2/FRW (J)
                        AVPOR = AVPOR + FRW (J)
                        BNUM = BNUM + 1.0
                     END IF
                  END IF
C
C     CORRECT BLKR FOR POROSITY IF PORE WATER EXCHANGE
C
                  IF (BNUM .GT. 0.0) THEN
                     AVPOR = AVPOR/BNUM
                     BLKR = BLKR*AVPOR*AVPOR
                  END IF
C
C      TRANSPORT SYSTEM ISYS:
C
                  RTERM = BLKR*(CONC2 - CONC1)
                  IF (I .GT. 0) THEN
                     CD (ISYS, I) = CD (ISYS, I) + RTERM
                     IF (RTERM .LT. 0.) SUMM (ISYS, I) =
     1                                  SUMM (ISYS, I) - RTERM
                  ELSE
                     IF (ISYS .EQ. JMASS)
     1                  ROFLUX = ROFLUX + RTERM/1000.
                  END IF
                  IF (J .GT. 0) THEN
                     CD (ISYS, J) = CD (ISYS, J) - RTERM
                     IF (RTERM .GT. 0.) SUMM (ISYS, J) =
     1                                  SUMM (ISYS, J) + RTERM
                  ELSE
                     IF (ISYS .EQ. JMASS)
     1                  RIFLUX = RIFLUX + RTERM/1000.
                  END IF
C
 1050          CONTINUE
 1040       CONTINUE
 1030    CONTINUE
 1020 CONTINUE
C
C        ADD IN FORCING FUNCTIONS
C
      DO 1070 ISYS = 1, NOSYS
         IF (SYSBY (ISYS) .EQ. 0 .AND. NOWK (ISYS) .GT. 0) THEN
            NWK = NOWK (ISYS)
            IF (TIME .GE. NTW (ISYS)) CALL WAS8A (50)
            DO 1080 J = 1, NWK
               DWKTIM = TIME - NWKT (ISYS, J)
               I = IWK (ISYS, J)
               IF (I .LE. 0) GO TO 1090
               LOAD = (MWK (ISYS, J)*DWKTIM + BWK (ISYS, J))*1000.
               CD (ISYS, I) = CD (ISYS, I) + LOAD
               IF (ISYS .EQ. JMASS) XLFLUX =
     1            XLFLUX + LOAD/1000.
 1080       CONTINUE
         END IF
 1070 CONTINUE
C
C          ADD IN NPS LOADS
C
      IF (LOPT .EQ. 0) GO TO 1100
      IF(NPSINIT .EQ. 0)THEN
  333     READ(AUX,1305)NPTIME
 1305     FORMAT(F10.0)
          IF(NPTIME - 1.0 .LT. TIME)THEN
          DO 501 I = 1, NUMSYS
             READ(AUX,1405)(NP_LOAD(LDSYS(I),NP_SEG(K)),K=1,NUMLOAD)
 1405        FORMAT(15X,20(F10.0))
  501     CONTINUE
             GOTO 333
          ELSE
             NPSINIT=1
          ENDIF
        ENDIF
C      IF (NEWDAY .GE. 1 .AND. NPSINIT .NE. 3 .AND. NPTIME .GE. TIME)THEN
C      IF (NEWDAY .GE. 1 .AND. NPSINIT .NE. 3 .AND. NPTIME .LE. TIME)THEN
      IF (NEWDAY .GE. 1 .AND. NPTIME .LE. TIME)THEN
         CALL NPS_TV(DUMMY)
         DO 1110 J = 1, numload
            I = NP_SEG(J)
            DO 1120 ISYS = 1, numsys
               LOAD = NP_LOAD (LDSYS(ISYS), I)*1000./DT
               WRITE(OUT,8888)J,ISYS,LOAD
 8888          FORMAT(1x,'Load Num ',I5,5x,'Sys ',I5,5x,'Load ',f10.2)
               CD (LDSYS(ISYS), I) = CD (LDSYS(ISYS), I) + LOAD
               IF (ISYS .EQ. JMASS) XLFLUX = XLFLUX + LOAD/1000.
 1120       CONTINUE
 1110    CONTINUE
      ENDIF
 1100 CONTINUE
C
C        BED VOLUME COMPUTATIONS
C
      CALL BEDSED
C
C  Time Optimization
      IF (INTYP .EQ. 1) CALL TOPT
      CALL COLOR ('YELLOW','BLACK')
      IF (MFLAG .LT. 2)CALL OUTRXY(18,5,DT,'(F8.4)')
C
C  Accumulate Fluxes for current time step
C
      AIMASS = AIMASS + AIFLUX*DT
      AOMASS = AOMASS + AOFLUX*DT
      RIMASS = RIMASS + RIFLUX*DT
      ROMASS = ROMASS + ROFLUX*DT
      XLMASS = XLMASS + XLFLUX*DT
      XKMASS = XKMASS + XKFLUX*DT
C      XBMASS = XBMASS + XBFLUX * DT
      RETURN
C
 1090 CONTINUE
      IERR = 3
C
 1060 CONTINUE
      CALL WERR (IERR, I, J)
      CALL WEXIT
      END

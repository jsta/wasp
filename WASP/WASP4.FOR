      SUBROUTINE WASP4
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:12.
C
C
C  Correction History:
C                                -- Added Evaporation
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG
C
      INTEGER SEGJUN, SEACHN
      CHARACTER HYDROFIL*12,FILES(5)*15
C
      COMMON /SWFLW/ SCALQ, SCALV, NJ, NC, JUNSEG (SG), SEGJUN (SG),
     1   NSEA, JSEA (5), SIGN (5), SEACHN (5), NBC, NUPS
C
C=======================================================================
      IF (MFLAG .EQ. 0) CALL GETMES (10,0)
C
      READ (IN, 5000, ERR = 1000) IQOPT, NFIELD, HYDROFIL
 5000 FORMAT(2I5,A12)
C
      WRITE (OUT, 6000) IQOPT
 6000 FORMAT(////37X,'FLOWS'/,37X,5('~')//,29X,'Flow Option',I3,' Used')
C
      WRITE (OUT, 6010) NFIELD
 6010 FORMAT(26X,'Number of Flow Fields = ',I3)
C
C***********************************************************************
C              IF NFIELD = 0, Skip Flow Computations
C***********************************************************************
C
      IF (NFIELD .EQ. 0) THEN
         NFIELD = 1
         NINQ (1) = 0
         GO TO 1010
      END IF
C
C***********************************************************************
C        Check if NFIELD is greater than the maximum allowed
C        check for sediment transport fields if IBEDV=1
C***********************************************************************
C
      IF (NFIELD .GT. MNF) THEN
         WRITE (OUT, 6020) MNF
 6020    FORMAT(/20X,'The Number of Flow Fields is Greater than ',
     1         /20X,'the Maximum of ',I5,2X,'Set in the Common Block',
     2         /20X,'** EXECUTION TERMINATED')
         CALL WERR(16,1,0)
      END IF
C
      IF (NFIELD .LT. 3 .AND. IBEDV .GE. 1) THEN
         WRITE (OUT, 6030)
 6030    FORMAT(/10X,'** Solids Transport Fields are Required for **',
     1        /10X,'Variable Bed Volume Option ',
     2        /10X,'Specify Field 3 Flows or Change Bed Option IBEDV')
         CALL WERR(17,1,0)
         CALL WEXIT('Dimension Error See the Output File',1)
      END IF
C
      NDISS = 2
C
C                       LOOP THROUGH FIELDS
C
C
C       ***************************************************************
C       *   FIELD 1 = Total Water Transport, BQ is continuity array,  *
C       *             QT contains flows for time function             *
C       *   FIELD 2 = Dissolved transport only                        *
C       *   FIELDS 3 or greater are solids fields, BQ is array of     *
C       *             areas, QT contains settling and scour velocities*
C       ***************************************************************
C
      DO 1020 NF = 1, NFIELD
C
C***********************************************************************
C          IF IQOPT =3 Field 1 flows are input from a
C          link-node hydrodynamic model
C***********************************************************************
C
      IF (IQOPT .GE. 3 .AND. NF .EQ. 1) THEN
         IF (HYDROFIL .EQ. '           ')THEN
            N = 30
            CALL OSDIRL (' ','*.HYD',FILES, N)
            IF ( N .EQ. 0)THEN
               CALL ATTRIB(' ')
               CALL COLOR('W','RED')
               CALL WNOPEN(0,20,70,3)
               CALL WNOUST('No Hydrodynamic linkage file exists in the '
     1   //'WASP5 directory!!  You need to run the hydrodyamic program'
     2   //' first and copy the linkage file to the WASP directory.')
              CALL WERR(18,1,0)
              CALL WEXIT('Error Finding Hydrodynamic Linkage File',1)
            ENDIF
C
            CALL ATTRIB('BOLD')
            CALL COLOR('CYAN','BLUE')
            CALL WNOPEN(0,21,70,3)
            CALL WNOUST('Please select the hydrodynamic linkage file '
     1 //'to use.  NOTE:  The hydrodynamic file must be copied from '
     2 //'the DYNHYD directory to the WASP directory.')
            IOPT = MNSCRL (FILES, N, 0, 0, 'Hydro-Link File', 10, 1, 1)
            HYDROFIL = FILES (IOPT)
         ENDIF
C
         DO 1111 ISEG = 1, NOSEG
            IF (QSEG (ISEG) .GT. 0.0 .AND. NOQ .NE. 0) THEN
               QSEG (ISEG) = QSEG (ISEG)/86400.
               VELOCG (ISEG) = VMULT (ISEG)*QSEG (ISEG)**VEXP (ISEG)
               DEPTHG (ISEG) = DMULT (ISEG)*QSEG (ISEG)**DXP (ISEG)
            ELSE
               DEPTHG (ISEG) = DMULT (ISEG)
               VELOCG (ISEG) = VMULT (ISEG)
            END IF
C
 1111    CONTINUE
C
         CALL WAS4A (HYDROFIL)
         NINQ (1) = 1.0
         GO TO 1030
      END IF
C
      READ (IN, 5010, ERR = 1000) NINQ (NF), SCALQ, CONVQ
      WRITE (OUT, 6040) NF, NINQ (NF), SCALQ, CONVQ
 5010 FORMAT(I5,2F10.0)
 6040 FORMAT(//,23X,' Field',I3,1X,'has',1X,I3,1X,'Inflows',6X,/,
     1       23X,' SCALQ =',E10.3,2X,'CONVQ =',E10.3,/)
C
      NINQX = NINQ (NF)
C
C***********************************************************************
C        If no Inflows for field NF, skip to next flow field
C***********************************************************************
C
      IF (NINQX .EQ. 0) GO TO 1030
C
C           Check if number of inflows is greater than the maximum
C
      IF (NINQX .GT. MNI) THEN
         WRITE (OUT, 6050) MNI
 6050    FORMAT(///20X,'The Number of Inflows is Greater Than',
     1            /20X,'The Maximum of ',I5,2X,'Set in the Common ',
     2            'Block',/20X,'**** EXECUTION TERMINATED ****')
C
         CALL WERR(19,1,0)
         CALL WEXIT('Dimension Error See the Output File',1)
C
      END IF
C
CCSC
      XARG = ABS(CONVQ)
C      IF (CONVQ .EQ. 0.0) CONVQ = 1.0
      IF (XARG .LT. R0MIN) CONVQ = 1.0
      SCALQ = SCALQ*CONVQ
C
C                       LOOP THROUGH INFLOWS
C
      DO 1040 NI = 1, NINQX
         WRITE (OUT, 6060) NI
 6060    FORMAT(//31X,'Inflow Number',I3/,31X,16('~'))
         READ (IN, 5020, ERR = 1000) NOQS (NF, NI)
 5020    FORMAT(I5)
C
C***********************************************************************
C              Read in continuity array
C***********************************************************************
C
         NOQ = NOQS (NF, NI)
         IF (NOQ .GT. S2) GO TO 1050
         READ (IN, 5030, ERR = 1000) (BQ (NF, NI,
     1         NQ), JQ (NF, NI, NQ),
     2         IQ (NF, NI, NQ), NQ = 1, NOQ)
 5030    FORMAT(4(F10.0,2I5))
         IF (NF .LE. NDISS) WRITE (OUT, 6070)
         IF (NF .GT. NDISS) THEN
            IF (NF .LE. 5) WRITE (OUT, 6080)
            IF (NF .EQ. 6) WRITE (OUT, 6090)
         END IF
 6070    FORMAT(5X,'Continuity Array:'//,3(8X,'Flow    From   To'),
     1            /,80('~'),/)
 6080    FORMAT(5X,'Solids Transport:'//,3(8X,'Area    From   To'),
     1            /,80('~'),/)
 6090    FORMAT(5X,'Evap. or Precip.:'//,3(8X,'Area    From   To'),
     1            /,80('~'),/)
         WRITE (OUT, 6100) (BQ (NF, NI, NQ), JQ (NF,
     1         NI, NQ), IQ (NF, NI, NQ),
     2         NQ = 1, NOQ)
 6100    FORMAT(3(5x,E10.3,2I5))
C
C***********************************************************************
C        Check for illegal solids transport flows to lower beds
C        (variable bed volume option IBEdV=1)
C***********************************************************************
C
         DO 1060 NQ = 1, NOQ
            IF (IBEDV .GE. 1 .AND. NF .NE. 2) THEN
               I1 = IQ (NF, NI, NQ)
               J1 = JQ (NF, NI, NQ)
               IF (I1 .EQ. 0 .OR. J1 .EQ. 0) GOTO 1060
               IF (ITYPE (I1) .EQ. 4 .OR. ITYPE (J1) .EQ. 4) THEN
                  WRITE (OUT, 6110) NF, NI, I1, J1
 6110             FORMAT(///20X,'Flow For Field',I3,2X,'Inflow',I3,
     1                   2X,'Between Segments',I3,2X,'and',I3,2X,
     2                   /20X,'is Not Allowed When IBEDV =1',
     3                   10X,'** RUN DISCONTINUED **')
                  CALL WEXIT('Error with Bed Option and Flows',1)
               END IF
            END IF
 1060    CONTINUE
C
C***********************************************************************
C                 Read in time functions.
C***********************************************************************
C
         READ (IN, 5040, ERR = 1000) NBRKQ (NF, NI)
 5040    FORMAT(I5)
         NOBRK = NBRKQ (NF, NI)
         IF (NOBRK .GT. MB) CALL BRKERR (NOBRK)
         WRITE (OUT, 6120) NOBRK
 6120    FORMAT(//5X,'Number of Breaks in Time Function = ',I5,///)
         READ (IN, 5050, ERR = 1000) (QT (NF, NI, NB),
     1         TQ (NF, NI, NB), NB = 1, NOBRK)
 5050    FORMAT(4(2F10.0))
         IF (NF .LE. NDISS) WRITE (OUT, 6130)
         IF (NF .GT. NDISS) WRITE (OUT, 6140)
 6130    FORMAT(3(8X,'Flow      Time   '),/,1X,78('~'),/)
 6140    FORMAT(3(6X,'Velocity    Time   '),/1X,78('~'),/)
         WRITE (OUT, 6150) (QT (NF, NI, NB), TQ (NF,
     1         NI, NB), NB = 1, NOBRK)
 6150    FORMAT(3(5x,2(E10.3)))
C
C***********************************************************************
C     Multiply flows by scale and conversion factor (cms to m**3/day)
C***********************************************************************
C
         DO 1070 NQ = 1, NOQ
            IF (IQ (NF, NI, NQ) .GT. SG .OR.
     1            JQ (NF, NI, NQ) .GT. SG)
     2      CALL WERR (4, IQ (NF, NI, NQ), JQ (NF, NI, NQ))
            BQ (NF, NI, NQ) = BQ (NF, NI, NQ)*SCALQ*86400.
 1070    CONTINUE
C
 1040 CONTINUE
C
 1030 CONTINUE
 1020 CONTINUE
C
C***********************************************************************
C        Mass balance check for advective flows (FIELD 1)
C***********************************************************************
C
      IF (IQOPT .LT. 2 .AND. NINQ (1) .GT. 0) THEN
         DO 1080 ISEG = 1, NOSEG
            DO 1090 NI = 1, NINQ (1)
               QSUMX = 0.0
               DO 1100 NQ = 1, NOQS (1, NI)
                  IF (IQ (1, NI, NQ) .EQ. ISEG) QSUMX =
     1               QSUMX + BQ (1, NI, NQ)
                  IF (JQ (1, NI, NQ) .EQ. ISEG) QSUMX =
     1               QSUMX - BQ (1, NI, NQ)
 1100          CONTINUE
               IF (QSUMX .GT. 1.0E-06)then
                  WRITE (OUT, 6160) ISEG, NI
                 CALL PROMPT('Flows Do Not Balance, Check Continuity',0)
               ENDIF
 1090       CONTINUE
 1080    CONTINUE
      END IF
 6160 FORMAT(//10X,40('*'),/10X,'Advective Flows in Segment',I5,
     1       /10X,'Do Not Balance For Inflow',I5,
     2       /10X,'Check Continuity Matrix',/10X,40('*')//)
C
C           SEGMENT LOOP
C
      NF=1
C
      DO 1920 NI=1,NINQ(NF)
         DO 1930 NQ = 1, NOQ
            Q = BQ (NF, NI, NQ)*QINT (NF, NI)
            IF (Q .GE. 0.) THEN
               I = IQ (NF, NI, NQ)
               J = JQ (NF, NI, NQ)
            ELSE
               J = IQ (NF, NI, NQ)
               I = JQ (NF, NI, NQ)
               Q = - Q
            END IF
C
C       Sum Segment Flows
C
            IF (J .GT. 0) QSEG (J) = QSEG (J) + 0.5*Q/86400.
            IF (I .GT. 0) QSEG (I) = QSEG (I) + 0.5*Q/86400.
 1930    CONTINUE
 1920 CONTINUE
C
      DO 1110 ISEG = 1, NOSEG
         IF (QSEG (ISEG) .GT. 0.0 .AND. NOQ .NE. 0) THEN
            QSEG (ISEG) = QSEG (ISEG)/86400.
            VELOCG (ISEG) = VMULT (ISEG)*QSEG (ISEG)**VEXP (ISEG)
            DEPTHG (ISEG) = DMULT (ISEG)*QSEG (ISEG)**DXP (ISEG)
         ELSE
            DEPTHG (ISEG) = DMULT (ISEG)
            VELOCG (ISEG) = VMULT (ISEG)
         END IF
 1110 CONTINUE
C
C***********************************************************************
C                        BYPASS OPTIONS
C***********************************************************************
C
      READ (IN, 5060, ERR = 1000) (QBY (I), I = 1, NOSYS)
 5060 FORMAT(16I5)
      DO 1120 ISYS = 1, NOSYS
         QBY (ISYS) = QBY (ISYS) + SYSBY (ISYS)
 1120 CONTINUE
      WRITE (OUT, 6170) NOSYS, (QBY (I), I = 1, NOSYS)
 6170 FORMAT(//10X,'Q Bypass Options for Systems 1 to',I3,' are',20I3/)
      WRITE(OUT,6181)
 6181 FORMAT('1')
      RETURN
C
C***********************************************************************
C          OPTION 0 - NO FLOWS
C***********************************************************************
C
 1010 CONTINUE
      DO 1130 ISYS = 1, NOSYS
         QBY (ISYS) = 1
 1130 CONTINUE
      IQOPT = 1
      WRITE (OUT, 6180)
 6180 FORMAT(///25X,'NO FLOWS'//)
      RETURN
C
 1050 CONTINUE
      WRITE (OUT, 6190) NOQ, S2
 6190 FORMAT(///10X,'The Number of Flows ',
     1  'Specified is ',I5/10X,
     2  'The Maximum Dimensioned for This Version of WASP',
     3  ' is ',I3/10X,'Respecify Flows or Redimension Parameter "S2"',
     4  'in the Common',/,10X,'Block and Recompile.')
         CALL WERR(20,1,0)
         CALL WEXIT('Dimension Error See the Output File',1)
 1000 CONTINUE
      CALL WERR(21,1,0)
      CALL WEXIT('Error Reading Card Group D',1)
      END

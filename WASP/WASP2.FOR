      SUBROUTINE WASP2
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:11.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C            Subroutine to read exchange coefficients
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG
C
      WRITE (OUT, 6000)
 6000 FORMAT(/////,29X,'Exchange Coefficients',/,
     1       29X,22('~'))
C
      IF (MFLAG .EQ. 0) CALL GETMES (7, 0)
C
      READ (IN, 5000) NRFLD
 5000 FORMAT(I5)
C
C***********************************************************************
C              If no FIELDS (NRFLD = 0) skip exchanges
C***********************************************************************
C
      IF (NRFLD .EQ. 0) THEN
         NRFLD = 1
         NTEX (1) = 0
         GO TO 1000
      END IF
C
      WRITE (OUT, 6010) NRFLD
 6010 FORMAT(///,25X,'Number of Exchange Fields = ',I3//)
C
C***********************************************************************
C              Check if NRFLD is greater than the maximum allowed
C***********************************************************************
C
      IF (NRFLD .GT. MNF) THEN
         WRITE (OUT, 6020) MNF
 6020   FORMAT(///20X,'*** ERROR *** The Number of Exchange Fields is',
     1         'Greater than',/20X,'the Maximum of',I5,2X,'Set in the',
     2         ' Common Block',/20X,'** EXECUTION TERMINATED **')
         CALL WERR(38,1,0)
         CALL WEXIT('Dimension Error See the Output File',1)
      END IF
C
C                       LOOP THROUGH FIELDS
C
      DO 1010 NF = 1, NRFLD
         READ (IN, 5010, ERR = 1020) NTEX (NF), SCALR, CONVR
 5010   FORMAT(I5,2F10.0)
CCSC
         XARG = ABS(CONVR)
C         IF (CONVR .EQ. 0.0) CONVR = 1.0
         IF (XARG .LT. R0MIN) CONVR = 1.0
         WRITE (OUT, 6030) NF, NTEX (NF), SCALR, CONVR
 6030   FORMAT(/,23X,'  FIELD',I3,1X,'has',I3,2X,'Time Functions',2X,//,
     1         21X,'  SCALR =',E10.3,2X,'CONVR =',E10.3,2X,///)
C
C***********************************************************************
C           If no time functions, skip to next exchange field
C***********************************************************************
C
         IF (NTEX (NF) .EQ. 0) GO TO 1010
         IF (NTEX (NF) .GT. MNI) THEN
            WRITE (OUT, 6040) MNI
 6040     FORMAT(///20X,'*** ERROR ***  The Number of Time Functions ',
     1           'is Greater than',/20X,'the Maximum of',I5,2X,
     2           'Set in the Common Block',/20X,
     3           '** EXECUTION TERMINATED **')
         CALL WERR(37,1,0)
C
         CALL WEXIT('Dimension Error See the Output File',1)
C
         END IF
C
C                    LOOP THROUGH TIME FUNCTIONS
C
         DO 1030 NT = 1, NTEX (NF)
            READ (IN, 5020, ERR = 1020) NORS (NF, NT)
 5020     FORMAT(I5)
            NOR = NORS (NF, NT)
            WRITE (OUT, 6050) NT, NOR
 6050     FORMAT(//23X,'Time Function',I3,1X,'has',I3,1X,'Exchanges',//)
            IF (NOR .GT. S2) GO TO 1040
            WRITE (OUT, 6060)
 6060     FORMAT(28X,'A',9X,'EL',5X,'From',2X,'To',/,24X,34('~'),/)
            DO 1050 NR = 1, NOR
               READ (IN, 5030, ERR = 1020) A, EL, JR (NF,
     1            NT, NR), IR (NF, NT, NR)
 5030       FORMAT(2(2F10.0,2I5))
               I = IR (NF, NT, NR)
               J = JR (NF, NT, NR)
C               IF (I .GT. S2 .OR. J .GT. S2) CALL WERR (2, I, J)
C
C***********************************************************************
C        Store Areas and Characteristic Lengths in BR Array:
C        Multiply by 86400 for Conversion of Disperions to M**2/day
C***********************************************************************
C
               BR (NF, NT, NR) = 86400.*A/EL
               BR (NF, NT, NR) = BR (NF, NT, NR)*SCALR*CONVR
               WRITE (OUT, 6070) A, EL, J, I
 6070       FORMAT(23X,2E10.3,2I5)
 1050       CONTINUE
C
C***********************************************************************
C        READ IN TIME FUNCTIONS FOR DISPERSION COEFFICIENTS
C***********************************************************************
C
            READ (IN, 5040, ERR = 1020) NBRKR (NF, NT)
 5040     FORMAT(I5)
            NOBRK = NBRKR (NF, NT)
            WRITE (OUT, 6080) NOBRK
 6080     FORMAT(//,19X,'Number of Breaks in Time Function = ',I3,//)
            IF (NOBRK .GT. MB) CALL BRKERR (NOBRK)
            READ (IN, 5050, ERR = 1020) (RT (NF, NT, NB),
     1         TR (NF, NT, NB), NB = 1, NOBRK)
 5050     FORMAT(4(2F10.0))
            WRITE (OUT, 6090)
 6090     FORMAT(10X,3('Dispersion    Time',4X),/,7X,70('~'),/)
            WRITE (OUT, 6100) (RT (NF, NT, NB), TR (NF,
     1         NT, NB), NB = 1, NOBRK)
 6100     FORMAT(8X,3(1X,E10.3,1X,E10.3))
 1030    CONTINUE
 1010 CONTINUE
C
C***********************************************************************
C                  EXCHANGE BYPASS OPTION
C***********************************************************************
C
      READ (IN, 5060, ERR = 1020) (RBY (I), I = 1, NOSYS)
 5060 FORMAT(16I5)
      DO 1060 ISYS = 1, NOSYS
         RBY (ISYS) = RBY (ISYS) + SYSBY (ISYS)
 1060 CONTINUE
      WRITE (OUT, 6110) NOSYS, (RBY (I), I = 1, NOSYS)
 6110 FORMAT(//15X,'R Bypass Options for Systems 1 to',I3,' are',20I3//)
      WRITE (OUT, 6120)
 6120 FORMAT('1')
      RETURN
C
C***********************************************************************
C                  OPTION 0 - NO EXCHANGES
C***********************************************************************
C
 1000 CONTINUE
      DO 1070 ISYS = 1, NOSYS
         RBY (ISYS) = 1
 1070 CONTINUE
      WRITE (OUT, 6130)
 6130 FORMAT(///5X,'*** No Exchange Coefficents ***',//)
      WRITE (OUT, 6140)
      RETURN
C
 1040 CONTINUE
      WRITE (OUT, 6140) NOR, S2
 6140 FORMAT(///1X,/5X,'The Number of Exchanges ',
     1  'Specified is ',I5/5X,
     2  'The maximum dimension for this version of WASP',
     3  ' is ',I3/5X,'Respecify exchanges or redimension',
     4  ' parameter "S2"',/,5X,'in the COMMON block and recompile.')
         CALL WERR(36,1,0)
         CALL WEXIT('Dimension error-see the output file',1)
C
 1020 CONTINUE
      CALL WEXIT ('Error Reading Card Group B',1)
      END

      SUBROUTINE TINIT
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:25:51.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C        TINIT IS USED TO SET UP INDICES FOR PROPER M's and b's(slopes
C        and intercepts) for time other than time equal to zero
C
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG
C
      REAL T (MB)
      CHARACTER VAR (4)*4
      DATA VAR/'  R ', ' VOL', '  Q ', 'F(T)'/
C
C-----------------------------------------------------------------------
CCSC
      XARG = ABS(TZERO)
C      IF (TZERO .EQ. 0.0) RETURN
      IF (XARG .LT. R0MIN) RETURN
C
C          EXCHANGES,VOLUMES,FLOWS,TIME FUNCTIONS
C
C  EXCHANGES
C
C  LOOP THROUGH FIELDS
C
      DO 1000 NF = 1, NRFLD
C
C   LOOP THROUGH TIME FUNCTIONS
C
         IF (NTEX (NF) .EQ. 0) GO TO 1000
         DO 1010 NT = 1, NTEX (NF)
            DO 1020 J = 1, NBRKR (NF, NT)
               IF (TR (NF, NT, J) .GT. TZERO) THEN
                  ITIMR (NF, NT) = J - 1
                  TNR (NF, NT) = TR (NF, NT, J)
                  GO TO 1010
               END IF
 1020       CONTINUE
C
C  ERROR MESSAGE IF TZERO IS GREATER THAN AVAILABLE DATA
C
            WRITE (OUT, 6000) VAR (1), TZERO
            CALL WEXIT ('Beginning Simulation Time Greater then user spe
     1cified Time Functions',1)
 1010    CONTINUE
 1000 CONTINUE
C
C  FLOWS
C
C  LOOP THROUGH FIELDS
C
      DO 1030 NF = 1, NFIELD
         IF (IQOPT .GE. 2 .AND. NF .EQ. 1) GO TO 1030
         NINQX = NINQ (NF)
         IF (NINQX .EQ. 0) GO TO 1030
C
C   LOOP THROUGH INFLOWS
C
         DO 1040 NI = 1, NINQX
            NOBRK = NBRKQ (NF, NI)
C
C    LOOP THROUGH TIME FUNCTION BREAK POINTS
C
            DO 1050 J = 1, NOBRK
               IF (TQ (NF, NI, J) .GT. TZERO) THEN
                  ITIMQ (NF, NI) = J - 1
                  TNQ (NF, NI) = TQ (NF, NI, J)
                  GO TO 1040
               END IF
 1050       CONTINUE
C
C    ERROR MESSAGE IF TZERO IS GREATER THAN AVAILABLE DATA
C
            WRITE (OUT, 6000) VAR (3), TZERO
            CALL WERR(22,1,0)
            CALL WEXIT ('Beginning Simulation Time Greater then user spe
     1cified Time Functions',1)
C
 1040    CONTINUE
 1030 CONTINUE
 6000     FORMAT(///15X,'Not possible To Set Up Broken Line Function I',
     1    'ntercept for ',A4,' Since'/15X,'Requested Time(',F7.0,') Gr',
     2    'eater Than Maximum Time Specified for Function')
C
C          KINETIC TIME FUNCTIONS
C
      IF (NFUNC .EQ. 0) GO TO 1060
      DO 1070 I = 1, TF
         II = I + I
         IM = II - 1
         NOBRK = NBRK73 (I)
         IF (NOBRK .EQ. 0) GOTO 1070
         DO 1080 J = 2, NOBRK
            T (J) = FILE73 (J, IM)
            IF (T (J) .GE. TZERO) GO TO 1090
 1080    CONTINUE
         WRITE (OUT, 6010) I, TZERO
 6010       FORMAT(///15X,'Not Possible to Set Up Broken Line Function',
     1      'Intercept for Kinetic Time Function',I3,' Since'/15X,
     2      'Requested Time (',F5.0,') Greater Than Maximum Time ',
     3      'Specified for Function')
            CALL WEXIT ('Beginning Simulation Time Greater then user spe
     1cified Time Functions',1)
 1090    CONTINUE
         ITIMF (I) = J - 1
 1070 CONTINUE
 1060 CONTINUE
C
C          BOUNDARY CONDITIONS
C
      DO 1100 ISYS = 1, NOSYS
         IF (SYSBY(ISYS) .EQ. 0 .AND. NOBC (ISYS) .GT. 0) THEN
          NO = NOBC(ISYS)
          DO 1110 I = 1, NO
             II = I + I + 2*BC*(ISYS - 1)
             IM = II - 1
             NOBRK = NBRK30 (ISYS,I)
             DO 1120 J = 2, NOBRK
                T (J) = FILE30 (J, IM)
                IF (T (J) .GE. TZERO) GO TO 1130
 1120        CONTINUE
             WRITE (OUT, 6020) ISYS, TZERO
 6020        FORMAT(///15X,
     +            'Not Possible to Set Up Broken Line Function',
     +    ' Intercept for System',I3,' B.C. Since'/15X,'Requested ',
     +    'Time (',F5.0,') Greater Than Maximum Time Specified for ',
     +    'Function')
C
            CALL WEXIT ('Beginning Simulation Time Greater then user spe
     1cified Time Functions',1)
 1130       CONTINUE
            ITIMB (ISYS, I) = J - 1
 1110    CONTINUE
         ENDIF
 1100 CONTINUE
C
C          LOADS
C
      DO 1140 ISYS = 1, NOSYS
         IF (SYSBY (ISYS) .EQ. 0 .AND. NOWK (ISYS) .GT. 0) THEN
         NO = NOWK(ISYS)
         DO 1150 I = 1, NO
            II = I + I + 2*WK*(ISYS - 1)
            IM = II - 1
            NOBRK = NBRK50 (ISYS,I)
            DO 1160 J = 2, NOBRK
               T (J) = FILE50 (J, IM)
               IF (T (J) .GE. TZERO) GO TO 1170
 1160       CONTINUE
            WRITE (OUT, 6030) ISYS, TZERO
 6030       FORMAT(///15X,'Not Possible to Set Up Broken Line Function',
     1      ' Intercept for System',I3,' Loads Since'/15X,'Requested ',
     2      'Time (',F5.0,') Greater Than Maximum Time Specified for ',
     3      'Function')
            CALL WEXIT ('Beginning Simulation Time Greater then user spe
     1cified Time Functions',1)
 1170       CONTINUE
            ITIMW (ISYS, I) = J - 1
 1150    CONTINUE
      ENDIF
 1140 CONTINUE
      RETURN
      END

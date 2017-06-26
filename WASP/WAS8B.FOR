      SUBROUTINE WAS8B
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:10.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
C  SUBROUTINE RETURNS MATRIX QINT(NF,NI) OF UPDATED FLOWS
C  AND MATRIX BRINT(NF,NI) OF EXCHANGES FOR ALL FIELDS.
C  IF IQOPT IS 3 OR GREATER, QINT(1,1) = 1.0; FIELD 1 FLOWS
C  ARE ALREADY UPDATED IN THE HYDRODYNAMIC MODEL SUBROUTINES.
C    ITIMQ=COUNTER ARRAY OF # OF TIMES SLOPE HAS BEEN UPDATED.
C    TNQ=ARRAY OF TIMES AT WHICH NEXT BREAK POINT OCCURS.
C
C  FLOWS
C
C             LOOP THROUGH FIELDS
C
      DO 1000 NF = 1, NFIELD
         NINQX = NINQ (NF)
         IF (NINQX .EQ. 0) GO TO 1000
C
C             LOOP THROUGH INFLOWS
C
         DO 1010 NI = 1, NINQX
            IF (IQOPT .GE. 3 .AND. NF .EQ. 1) THEN
               QINT (1, 1) = 1.0
            ELSE
               IF (TIME .GT. TNQ (NF, NI)) ITIMQ (NF,
     1            NI) = ITIMQ (NF, NI) + 1
               IF (ITIMQ (NF, NI) .GE. NBRKQ (NF, NI)) THEN
                  NOBRK = NBRKQ (NF, NI)
                  TNEXT = TQ (NF, NI, NOBRK) - TQ (NF, NI, 1)
                  DO 1020 J = 1, NOBRK
                     TQ (NF, NI, J) = TQ (NF, NI, J) + TNEXT
 1020             CONTINUE
                  ITIMQ (NF, NI) = 1
               END IF
               IT = ITIMQ (NF, NI)
               TNQ (NF, NI) = TQ (NF, NI, IT + 1)
               SLOPE = (QT (NF, NI, IT + 1) - QT (NF, NI, IT)
     1            )/(TQ (NF, NI, IT + 1)
     2            - TQ (NF, NI, IT))
               DELT = TNQ (NF, NI) - TIME
               QINT (NF, NI) = QT (NF, NI, IT + 1) - SLOPE*DELT
            END IF
 1010    CONTINUE
 1000 CONTINUE
C
C***********************************************************************
C          EXCHANGES; RETURN BRINT(NF,NT)
C          LOOP THROUGH FIELDS
C***********************************************************************
C
      DO 1030 NF = 1, NRFLD
C   TIME FUNCTION LOOP
         IF (NTEX (NF) .EQ. 0) GO TO 1030
         DO 1040 NT = 1, NTEX (NF)
            IF (TIME .GT. TNR (NF, NT)) ITIMR (NF,
     1         NT) = ITIMR (NF, NT) + 1
C    IF TIME FUNCTION IS TO REPEAT, REINITIALIZE TIME COUNTERS
            IF (ITIMR (NF, NT) .GE. NBRKR (NF, NT)) THEN
               NOBRK = NBRKR (NF, NT)
               TNEXT = TR (NF, NT, NOBRK) - TR (NF, NT, 1)
               DO 1050 J = 1, NOBRK
                  TR (NF, NT, J) = TR (NF, NT, J) + TNEXT
 1050          CONTINUE
               ITIMR (NF, NT) = 1
            END IF
            IT = ITIMR (NF, NT)
            TNR (NF, NT) = TR (NF, NT, IT + 1)
            SLOPE = (RT (NF, NT, IT + 1) - RT (NF, NT, IT))/
     1         (TR (NF, NT, IT + 1) -
     2         TR (NF, NT, IT))
            DELT = TNR (NF, NT) - TIME
            BRINT (NF, NT) = RT (NF, NT, IT + 1) - SLOPE*DELT
 1040    CONTINUE
 1030 CONTINUE
      RETURN
      END

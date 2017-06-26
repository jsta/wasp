      SUBROUTINE QCALC
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:09.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
      INCLUDE 'WASP.CMN'
C
C       Surface water flow field
C
      CALL SETRA (QSEG, NOSEG, 1, 0.0)
      DO 1000 JJ = 1, NOSEG + 1
         J = JJ - 1
         QSUM (0, J) = 0.0
         DO 1010 II = 1, NOSEG + 1
            I = II - 1
            QSUM (I, J) = 0.0
 1010    CONTINUE
 1000 CONTINUE
C
      NF = 1
      NINQX = NINQ (NF)
      IF (NINQX .GT. 0) THEN
C
C       LOOP THROUGH INFLOWS
C
         DO 1020 NI = 1, NINQX
            NOQ = NOQS (NF, NI)
C
C           SEGMENT LOOP
C
            DO 1030 NQ = 1, NOQ
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
               QSUM (I, J) = QSUM (I, J) + Q
 1030       CONTINUE
 1020    CONTINUE
C
C       Combine opposite internal flows in QSUM matrix by subtraction
C
         DO 1040 J = 1, NOSEG - 1
            DO 1050 I = J + 1, NOSEG
               QSUM (I, J) = QSUM (I, J) - QSUM (J, I)
 1050       CONTINUE
 1040    CONTINUE
      END IF
      DO 1060 ISEG = 1, NOSEG
         IF (QSEG (ISEG) .GT. 0.0) THEN
            VELOCG (ISEG) = VMULT (ISEG)*QSEG (ISEG)**VEXP (ISEG)
            DEPTHG (ISEG) = DMULT (ISEG)*QSEG (ISEG)**DXP (ISEG)
         END IF
 1060 CONTINUE
      RETURN
      END

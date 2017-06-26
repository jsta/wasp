      SUBROUTINE QSURF1
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:09.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      REAL LOAD
      CHARACTER CONSTRING*65,STRING1*5, STRING2*5, TMPSTR*25
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG
      EQUIVALENCE (TIME, T)
C
C   SUM USER-SPECIFIED SURFACE WATER FLOWS
C
C        Internal Flows (stored in half of QSUM array where I > J)
C
      DO 1000 JJ = 1, NOSEG - 1
         I1 = JJ + 1
         DO 1010 II = I1, NOSEG
            Q = QSUM (II, JJ)
CCSC
            XARG = ABS(Q)
C            IF (Q .NE. 0.) THEN
            IF (XARG .GT. R0MIN) THEN
               IF (Q .GT. 0.) THEN
                  I = II
                  J = JJ
               ELSE
                  J = II
                  I = JJ
                  Q = - Q
               END IF
C
               IF(IVOPT .LT. 10)MVOL (I) = MVOL (I) + Q*DT
               IF(IVOPT .LT. 10)MVOL (J) = MVOL (J) - Q*DT
C
               DO 1020 ISYS = 1, NOSYS
                  IF (QBY (ISYS) .EQ. 0) THEN
                     CALL WA12A (I, J, NOBC (ISYS),
     1                  CONC1, CONC2, ADVECT, IERR)
                     IF (IERR .GT. 0) GO TO 1030
                     CSTAR = CONC2 + (CONC1 - CONC2)*ADVECT
                     QM = Q*CSTAR
                     CD (ISYS, J) = CD (ISYS, J) - QM
                     SUMM (ISYS, J) = SUMM (ISYS, J) + QM
                     CD (ISYS, I) = CD (ISYS, I) + QM
                  END IF
 1020          CONTINUE
            END IF
 1010    CONTINUE
 1000 CONTINUE
C
C        Inflows and Outflows (inflows stored as QSUM(ISEG,0)
C                             (outflows stored as QSUM(0,ISEG)
      DO 1040 ISEG = 1, NOSEG
C          Pass 1 checks nominal inflows; pass 2 checks nominal outflows
         I = ISEG
         J = 0
         SIGN = + 1.0
         DO 1050 K = 1, 2
            Q = QSUM (I, J)
CCSC
            XARG = ABS(Q)
C            IF (Q .NE. 0.) THEN
            IF (XARG .GT. R0MIN) THEN
               IF(IVOPT .LT. 10)MVOL (ISEG) = MVOL (ISEG) + Q*DT*SIGN
               DO 1060 ISYS = 1, NOSYS
                  IF (QBY (ISYS) .EQ. 0) THEN
                     CALL WA12A (I, J, NOBC (ISYS),
     1                  CONC1, CONC2, ADVECT, IERR)
                     IF (IERR .GT. 0) GO TO 1030
                     IF (Q .GT. 0.) THEN
                        CSTAR = CONC2
                     ELSE
                        CSTAR = CONC1
                     END IF
                     QM = Q*CSTAR
                     CD (ISYS, ISEG) = CD (ISYS, ISEG) + QM*SIGN
                     IF (J .GT. 0) SUMM (ISYS, J) = SUMM (ISYS, J) + QM
                     IF (ISYS .EQ. JMASS) THEN
                        IF (Q .GT. 0) THEN
                           IF (I .GT. 0) AIFLUX = AIFLUX + QM/1000.
                           IF (J .GT. 0) AOFLUX = AOFLUX + QM/1000.
                        ELSE
                           IF (J .GT. 0) AOFLUX = AOFLUX - QM/1000.
                           IF (I .GT. 0) AIFLUX = AIFLUX - QM/1000.
                        END IF
                     END IF
                  END IF
 1060          CONTINUE
            END IF
            J = ISEG
            I = 0
            SIGN = - 1.0
 1050    CONTINUE
 1040 CONTINUE
C
      RETURN
C
C
 1030 CONTINUE
      CALL WERR (7, I, J)
      CALL INTSTR(I,STRING1,'(I5)')
      CALL INTSTR(J,STRING2,'(I5)')
      TMPSTR=STRING1//' For Segment'//STRING2
      CONSTRING='No Boundary Conditions Specified for Sys'//TMPSTR
      CALL WEXIT (CONSTRING,1)
      END

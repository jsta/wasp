      SUBROUTINE Qhydro
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:09.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
      CHARACTER CONSTRING*65,STRING1*5, STRING2*5, TMPSTR*25
C
      NF = 1
      NINQX = NINQ (NF)
      IF (NINQX .GT. 0) THEN
C
C                             LOOP THROUGH INFLOWS
C
         DO 1000 NI = 1, NINQX
            NOQ = NOQS (NF, NI)
C
C                                SEGMENT LOOP
C
            DO 1010 NQ = 1, NOQ
               Q = BQ (NF, NI, NQ)
               IF (Q .GE. 0.) THEN
                  I = IQ (NF, NI, NQ)
                  J = JQ (NF, NI, NQ)
               ELSE
                  J = IQ (NF, NI, NQ)
                  I = JQ (NF, NI, NQ)
                  Q = - Q
               END IF
C
C                              ADJUST WATER VOLUMES
C
C                  IF (I .GT. 0) MVOL (I) = MVOL (I) + Q*DT
C                  IF (J .GT. 0) MVOL (J) = MVOL (J) - Q*DT
C
C     ADVECT SYSTEM(ISYS)
C
               DO 1020 ISYS = 1, NOSYS
                  IF (QBY (ISYS) .EQ. 0) THEN
                     CALL WA12A (I, J, NOBC (ISYS),
     1                  CONC1, CONC2, ADVECT, IERR)
                     IF (IERR .GT. 0) GO TO 1030
                     IF (NF .GE. 2) ADVECT = 0.0
                     CSTAR = CONC2 + (CONC1 - CONC2)*ADVECT
                     IF (J .GT. 0) THEN
                        CD (ISYS, J) = CD (ISYS, J) - Q*CSTAR
                        SUMM (ISYS, J) = SUMM (ISYS, J) + Q*CSTAR
                     END IF
                     IF (I .GT. 0) CD (ISYS, I) =
     1                  CD (ISYS, I) + Q*CSTAR
C
C                                 MASS BALANCE
C
                     IF (ISYS .EQ. JMASS) THEN
                        IF (I .EQ. 0) AOFLUX =
     1                     AOFLUX - Q*CSTAR/1000.
                        IF (J .EQ. 0) AIFLUX =
     1                     AIFLUX + Q*CSTAR/1000.
                     END IF
                  END IF
 1020          CONTINUE
 1010       CONTINUE
 1000    CONTINUE
      END IF
      RETURN
C
 1030 CONTINUE
      CALL WERR (6, I, J)
      CALL INTSTR(I,STRING1,'(I5)')
      CALL INTSTR(J,STRING2,'(I5)')
      TMPSTR=STRING1//' For Segment'//STRING2
      CONSTRING='No Boundary Conditions Specified for Sys'//TMPSTR
      CALL WEXIT (CONSTRING,1)
      END

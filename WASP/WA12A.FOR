      SUBROUTINE WA12A (I, J, NBC, CONC1, CONC2, ADVECT, IERR)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:04.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      EQUIVALENCE (TIME, T)
C
C=======================================================================
      IERR = 0
      II = 1
      IF (I) 1000, 1010, 1020
 1020 CONTINUE
      IF (J) 1000, 1030, 1040
C
C        DOWNSTREAM BOUNDARY CONDITION
C
 1010 CONTINUE
      IF (NBC .EQ. 0) GO TO 1050
      DO 1060 JJ = 1, NBC
         IF (J .EQ. IBC (ISYS, JJ)) GO TO 1070
 1060 CONTINUE
 1050 CONTINUE
      IERR = 7
      GO TO 1080
C
 1070 CONTINUE
      CONC1 = BBC (ISYS, JJ) + MBC (ISYS, JJ)*(T - NBCT (ISYS, JJ))
      CONC2 = C (ISYS, J)
      ADVECT = 0.0
      GO TO 1080
C
C***********************************************************************
C        UPSTREAM BOUNDARY CONDITION
C***********************************************************************
C
 1030 CONTINUE
      IF (NBC .EQ. 0) GO TO 1090
      DO 1100 JJ = 1, NBC
         IF (I .EQ. IBC (ISYS, JJ)) GO TO 1110
 1100 CONTINUE
 1090 CONTINUE
      IERR = 7
      GO TO 1080
C
 1110 CONTINUE
      CONC1 = C (ISYS, I)
      CONC2 = BBC (ISYS, JJ) + MBC (ISYS, JJ)*(T - NBCT (ISYS, JJ))
      ADVECT = 0.0
      GO TO 1080
C
C***********************************************************************
C          INTERNAL TRANSFERS
C***********************************************************************
C
 1040 CONTINUE
      CONC1 = C (ISYS, I)
      CONC2 = C (ISYS, J)
      ADVECT = ADFAC
 1080 CONTINUE
      RETURN
C
C***********************************************************************
C          ERROR CONDITION
C***********************************************************************
C
 1000 CONTINUE
      IERR = 1
      RETURN
      END

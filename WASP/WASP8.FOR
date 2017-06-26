      SUBROUTINE WASP8 (SLOPE, B, NO, IOPT, ITIME, NT, IFILE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:15.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
C
      INCLUDE 'WASP.CMN'
C
      REAL NT, SLOPE (TF), B (TF), VAL (MB), T (MB)
C
      INTEGER NFIL (4)
C
      DATA NFIL/70, 72, 73, 75/
C
C***********************************************************************
C      EVALUATE TIME VARIABLE FUNCTIONS
C***********************************************************************
C
      DO 1000 I = 1, 4
         IF (NFIL (I) .EQ. IFILE) GO TO 1010
 1000 CONTINUE
      RETURN
 1010 CONTINUE
      GO TO ( 1020, 1030, 1040, 1050), I
C
C***********************************************************************
C                 UPDATE DISPERSIVE EXCHANGES
C***********************************************************************
C
 1020 CONTINUE
      RETURN
C
C***********************************************************************
C                  UPDATE ADVECTIVE FLOWS
C***********************************************************************
C
 1030 CONTINUE
      RETURN
C***********************************************************************
C          UPDATE KINETIC TIME FUNCTIONS
C***********************************************************************
 1040 CONTINUE
      NT = 1.E+06
      DO 1060 I = 1, TF
         IF (NBRK73 (I) .GT. 0) THEN
            IF (NFUNT (I) .LT. NT) NT = NFUNT (I)
            IF (TIME .LT. NFUNT (I)) GO TO 1060
            II = I + I
            IM = II - 1
            ITIME = ITIMF (I)
            NOBRK = NBRK73 (I)
            DO 1070 J = 1, NOBRK
               T (J) = FILE73 (J, IM)
 1070       CONTINUE
            IF (ITIME .LT. NOBRK) GO TO 1080
            TNEXT = T (NOBRK) - T (1)
            DO 1090 J = 1, NOBRK
               T (J) = T (J) + TNEXT
               FILE73 (J, IM) = T (J)
 1090       CONTINUE
            ITIME = 1
 1080       CONTINUE
            DO 1100 J = 1, NOBRK
               VAL (J) = FILE73 (J, II)
 1100       CONTINUE
            B (I) = VAL (ITIME + 1)
            SLOPE (I) = (VAL (ITIME) - VAL (ITIME + 1))/
     1         (T (ITIME) - T (ITIME + 1))
            NFUNT (I) = T (ITIME + 1)
            ITIMF (I) = ITIME + 1
            IF (NFUNT (I) .LT. NT) NT = NFUNT (I)
         END IF
 1060 CONTINUE
      RETURN
C***********************************************************************
C                           UPDATE TIME STEPS
C***********************************************************************
 1050 CONTINUE
      NOBRK = NBRK75 (1)
      DO 1110 J = 1, NOBRK
         T (J) = FILE75 (J, 1)
 1110 CONTINUE
      IF (ITIME .LT. NOBRK) GO TO 1120
      TNEXT = T (NOBRK) - T (1)
      DO 1130 J = 1, NOBRK
         T (J) = T (J) + TNEXT
         FILE75 (J, 1) = T (J)
 1130 CONTINUE
      ITIME = 1
 1120 CONTINUE
      DO 1140 I = 1, NO
         II = I + 1
         DO 1150 J = 1, NOBRK
            VAL (J) = FILE75 (J, II)
 1150    CONTINUE
         B (I) = VAL (ITIME + 1)
         SLOPE (I) = (VAL (ITIME) - VAL (ITIME + 1))/(T
     1      (ITIME) - T (ITIME + 1))
 1140 CONTINUE
      NT = T (ITIME + 1)
      RETURN
      END

      SUBROUTINE WAS8A (IFILE)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:09.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      REAL NT, VAL (MB), T (MB)
C
      IF (IFILE .EQ. 50) GO TO 1000
C
C***********************************************************************
C                  UPDATE BOUNDARY CONDITIONS
C***********************************************************************
C
C
      NO = NOBC (ISYS)
      NT = 1.E+06
      DO 1010 I = 1, NO
         IF (NBCT (ISYS, I) .LT. NT) NT = NBCT (ISYS, I)
         IF (TIME .LT. NBCT (ISYS, I)) GO TO 1010
         II = I + I + 2*BC*(ISYS - 1)
         IM = II - 1
         ITIME = ITIMB (ISYS, I)
         NOBRK = NBRK30 (ISYS, I)
         DO 1020 J = 1, NOBRK
            T (J) = FILE30 (J, IM)
 1020    CONTINUE
         IF (ITIME .LT. NOBRK) GO TO 1030
         TNEXT = T (NOBRK) - T (1)
         DO 1040 J = 1, NOBRK
            T (J) = T (J) + TNEXT
            FILE30 (J, IM) = T (J)
 1040    CONTINUE
         ITIME = 1
 1030    CONTINUE
         DO 1050 J = 1, NOBRK
            VAL (J) = FILE30 (J, II)
 1050    CONTINUE
         BBC (ISYS, I) = VAL (ITIME + 1)
         MBC (ISYS, I) = (VAL (ITIME) - VAL (ITIME + 1))/
     1      (T (ITIME) - T (ITIME + 1))
         NBCT (ISYS, I) = T (ITIME + 1)
         ITIMB (ISYS, I) = ITIME + 1
         IF (NBCT (ISYS, I) .LT. NT) NT = NBCT (ISYS, I)
 1010 CONTINUE
      NTB (ISYS) = NT
      RETURN
C
C***********************************************************************
C          UPDATE FORCING FUNCTIONS
C***********************************************************************
C
 1000 CONTINUE
C
      NO = NOWK (ISYS)
      NT = 1.E+06
      DO 1060 I = 1, NO
         IF (NWKT (ISYS, I) .LT. NT) NT = NWKT (ISYS, I)
         IF (TIME .LT. NWKT (ISYS, I)) GO TO 1060
         II = I + I + 2*WK*(ISYS - 1)
         IM = II - 1
         ITIME = ITIMW (ISYS, I)
         NOBRK = NBRK50 (ISYS, I)
         DO 1070 J = 1, NOBRK
            T (J) = FILE50 (J, IM)
 1070    CONTINUE
         IF (ITIME .LT. NOBRK) GO TO 1080
         TNEXT = T (NOBRK) - T (1)
         DO 1090 J = 1, NOBRK
            T (J) = T (J) + TNEXT
            FILE50 (J, IM) = T (J)
 1090    CONTINUE
         ITIME = 1
 1080    CONTINUE
         DO 1100 J = 1, NOBRK
            VAL (J) = FILE50 (J, II)
 1100    CONTINUE
         BWK (ISYS, I) = VAL (ITIME + 1)
         MWK (ISYS, I) = (VAL (ITIME) - VAL (ITIME + 1))/
     1      (T (ITIME) - T (ITIME + 1))
         NWKT (ISYS, I) = T (ITIME + 1)
         ITIMW (ISYS, I) = ITIME + 1
         IF (NWKT (ISYS, I) .LT. NT) NT = NWKT (ISYS, I)
 1060 CONTINUE
      NTW (ISYS) = NT
      RETURN
      END

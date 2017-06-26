      SUBROUTINE WASP7
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:14.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      REAL VALT (MB), T (MB), PSCAL (PR)
      CHARACTER*5 ANAME (cx), TNAME (20), PNAME (SG, PR)
      CHARACTER*10 CHNAME, FLDNAME, HEADER*80, ERRMSG*30
      DIMENSION TCONST (20), ISC (20)
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C             Reading Card Group G:  Segment Parameters
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
      IF (MFLAG .EQ. 0) CALL GETMES (18, 0)
      WRITE (OUT, 6000)
 6000 FORMAT(34X,'Segment Parameters',
     1       /,34X,19('~')//)
C*-*
C*-*-*-* Read in Segment Parameter Scale factors and Values  *-*-*-*
C*-*
      WRITE (OUT, 6010)
 6010 FORMAT(1X,4('PARM',5X,'SCALE',7X),/1X,79('~'))
      ERRMSG = '  Error Reading Card Group G  '
      IERR=1
      READ (IN, 5000, ERR = 1000) NOPAM
 5000 FORMAT(I10)
      IF (NOPAM .GT. 0) THEN
         NLINES = (NOPAM - 1)/4 + 1
         DO 1010 K = 1, NLINES
            READ (IN, 5010, ERR = 1000) (TNAME (I), ISC (I),
     1         TCONST (I), I = 1, 4)
 5010       FORMAT (4(A5,I5,E10.0))
            DO 1020 I = 1, 4
               IC = ISC (I)
               IF (IC .GT. 0) THEN
                  PSCAL (IC) = TCONST (I)
               END IF
 1020       CONTINUE
            WRITE (OUT, 6020) (TNAME (I), TCONST (I), I = 1, 4)
 6020       FORMAT(4(1X,A5,2X,E10.3,2X))
 1010    CONTINUE
C
         WRITE (OUT, 6030)
 6030    FORMAT(1X,/)
         DO 1030 ISEG = 1, NOSEG
            READ (IN, 5020, ERR = 1000) ISG
 5020       FORMAT (I10)
            WRITE (OUT, 6040) ISEG
 6040       FORMAT (1X,'Segment # ',I3)
            DO 1040 K = 1, NLINES
               READ (IN, 5010, ERR = 1000) (TNAME (I)
     1            , ISC (I), TCONST (I),
     2            I = 1, 4)
               DO 1050 I = 1, 4
                  IC = ISC (I)
                  IF (IC .GT. 0) THEN
                     PARAM (ISG, IC) = TCONST (I)
                     PNAME (ISG, IC) = TNAME (I)
                  END IF
 1050          CONTINUE
               WRITE (OUT, 6050) (TNAME (I), ISC (I),
     1            TCONST (I), I = 1, 4)
 6050          FORMAT(4(1X,A5,I4,E10.3))
 1040       CONTINUE
            WRITE (OUT, 6030)
C
C         ADJUST PARAMETER BY SCALE FACTOR
C
            DO 1060 I = 1, PR
               PARAM (ISEG, I) = PARAM (ISEG, I)*PSCAL (I)
 1060       CONTINUE
 1030    CONTINUE
      ELSE
         WRITE (OUT, 6060)
 6060    FORMAT(1X,/,1X,'No Parameters for this Model Run')
      END IF
C
      WRITE (OUT, '(''1'')')
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C                     Reading Card Group H:
C            Global, Environmental, Chemical Constants
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
      WRITE (OUT, 6070)
 6070 FORMAT(23X,'Environmental and Chemical Constants',/,23X,36('~'))
      IF (MFLAG .EQ. 0) CALL GETMES (24, 0)
      IERR=2
      ERRMSG = '   Error Reading Card Group H '
      READ (IN, 5030, ERR = 1000) HEADER
 5030 FORMAT(A80)
      DO 1070 ISY = 1, NOSYS + 1
         READ (IN, 5040, ERR = 1000) CHNAME, NFLD
 5040    FORMAT(A10,I10)
C
         IF (NFLD .GT. 0) THEN
            WRITE (OUT, 6080) CHNAME, ISY
 6080       FORMAT(1X,//20X,A10,' Constants for System ',I2)
            DO 1080 NN = 1, NFLD
               READ (IN, 5050, ERR = 1000) FLDNAME, NCNS
 5050          FORMAT(A10,I10)
               WRITE (OUT, 6090) ISY, FLDNAME, NCNS
 6090          FORMAT (1X,//5X,' Number of Constants for System ',
     1              I2,';  Group ',A10,' is ',I2,//)
               IF (NCNS .EQ. 0) GO TO 1080
               WRITE (OUT, 6100)
 6100          FORMAT(1X,2(1X,'Constant',5X,'Const. #',5X,'K Value',3X),
     1             /,1X,78('~'))
               NLINES = (NCNS - 1)/2 + 1
               DO 1090 K = 1, NLINES
                  READ (IN, 5060, ERR = 1000) (TNAME (I), ISC (I),
     1               TCONST (I), I = 1, 2)
 5060             FORMAT(2(5X,A5,5X,I5,E10.0))
                  DO 1100 I = 1, 2
                     IC = ISC (I)
                     IF (IC .GT. 0) THEN
                        CONST (IC) = TCONST (I)
                        ANAME (IC) = TNAME (I)
                     END IF
 1100             CONTINUE
                  WRITE (OUT, 6110) (TNAME (I), ISC (I),
     1               TCONST (I), I = 1, 2)
 6110             FORMAT(1X,2(1X,A5,5X,I5,7X,E10.3,5X))
 1090          CONTINUE
 1080       CONTINUE
         ELSE
            WRITE (OUT, 6120) ISY, CHNAME
 6120       FORMAT(1X,/,20X,'No Constants Enter for System ',I2,
     1                ':',2X,A10)
         END IF
C
 1070 CONTINUE
      WRITE (OUT, '(''1'')')
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C                     I:  Time Functions
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
      IF (MFLAG .EQ. 0) CALL GETMES (25, 0)
      IERR=3
      ERRMSG = '  Error Reading Card Group I  '
      READ (IN, 5070, ERR = 1000) NFUNC
 5070 FORMAT(I10)
      IF (NFUNC .LE. 0) GO TO 1110
      WRITE (OUT, 6130)
 6130 FORMAT(25X,'Environmental Time Functions',/,25X,27('~'))
      WRITE (OUT, 6140)
 6140 FORMAT(//25X,'Piecewise Linear Functions'/6X,3(7X,'VAL(T)',6X,
     1  'T',2X)/)
      DO 1120 I = 1, NFUNC
         READ (IN, 5080, ERR = 1000) ANAME (1), nobrk, IFUNC
         IF (NOBRK .GT. MB) CALL BRKERR (NOBRK)
         II = IFUNC + IFUNC
         IM = II - 1
         IF (NOBRK .GT. 0) READ (IN, 5090, ERR = 1000) (VALT (J), T (J),
     1      J = 1, NOBRK)
 5080    FORMAT(A5,2I5)
 5090    FORMAT(8F10.0)
         IF (NOBRK .GT. 0) WRITE (OUT, 6150)
     1      ANAME (1), (VALT (J), T (J),
     2      J = 1, NOBRK)
 6150    FORMAT(1X,A5,3(E15.5,F7.2)/(6X,3(E15.5,F7.2)))
         NBRK73 (IFUNC) = NOBRK
         DO 1130 J = 1, NOBRK
            FILE73 (J, IM) = T (J)
            FILE73 (J, II) = VALT (J)
 1130    CONTINUE
 1120 CONTINUE
 1110 CONTINUE
      IF (NFUNC .LE. 0) WRITE (OUT, 6160)
 6160 FORMAT(1X,/////,25X,'No Time Functions have been Entered',
     1       25X,'for this Simulation')
      WRITE (OUT, '(''1'')')
      RETURN
 1000 CONTINUE
      IF(IERR .EQ. 1)CALL WERR(12,1,0)
      IF(IERR .EQ. 2)CALL WERR(13,1,0)
      IF(IERR .EQ. 3)CALL WERR(14,1,0)
      CALL WEXIT (ERRMSG,1)
      END

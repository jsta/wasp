      SUBROUTINE WASP5
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:13.
C
C  Correction History:
C
C----------------------------------------------------------------------
C        SUBROUTINE TO READ BOUNDARY CONCENTRATIONS
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
C
      REAL XARG
C
      REAL BCT (MB), T (MB)
C
      REAL SCALB (16)
C
C
       WRITE(OUT,6005)
 6005  FORMAT(///,32X,'Boundary Conditions',/,32X,19('~'))
C
C                           SYSTEM LOOP
C
      IF(MFLAG.EQ.0)CALL GETMES (15,0)
C
      DO 1000 ISYS = 1, NOSYS
         READ (IN, 5000, ERR = 1010) NOBC (ISYS)
 5000    FORMAT(I10)
         NO = NOBC(ISYS)
         WRITE (OUT, 6010) ISYS
 6010    FORMAT(//26X,'Boundary Concentrations for System',I3/)
         WRITE (OUT, 6020) IBCOP (ISYS), NOBC (ISYS)
 6020    FORMAT(22X,'BC Option',I3,' Used',5X,'No.Of BC''S Read',I5/)
C
         IBCOP(ISYS)=3
         IF (NO .GT. BC) GO TO 1020
         IF (NO .EQ. 0) GO TO 1030
C
         READ (IN, 5010, ERR = 1010) SCALB (ISYS), CONVB
 5010    FORMAT(2E10.3)
CCSC
         XARG = ABS(CONVB)
C         IF (CONVB .EQ. 0.) CONVB = 1.0
         IF (XARG .LT. R0MIN) CONVB = 1.0
         WRITE (OUT, 6030) SCALB (ISYS), CONVB
 6030    FORMAT(14X,'Scale Factor =',E12.4,2X,
     1              'Conversion Factor =',E12.4,////)
         SCALB (ISYS) = SCALB (ISYS)*CONVB
C
         WRITE (OUT, 6080)
 6080    FORMAT(9X,'Segment',2X,'No.Brk.',2(6X,'BC(T)',11X,'T',5X)/,9X,
     1         70('~'))
         BSCAL = SCALB (ISYS)
         DO 1210 I = 1, NO
            II = I + I + 2*BC*(ISYS - 1)
            IM = II - 1
            READ (IN, 5050, ERR = 1010) IBC (ISYS, I), NOBRK
            IF (NOBRK .GT. MB) CALL BRKERR (NOBRK)
            READ (IN, 5060, ERR = 1010) (BCT (J), T (J), J = 1, NOBRK)
 5050       FORMAT(2I5)
 5060       FORMAT(8F10.0)
            WRITE (OUT, 6090) IBC (ISYS, I), NOBRK, (BCT
     1         (J), T (J), J = 1, NOBRK)
 6090       FORMAT(12X,I3,4X,I3,5X,E14.7,F10.2,2X,E14.7,F10.2,/,
     1             1(27X,E14.7,F10.2,2X,E14.7,F10.2))
C 6090      FORMAT(1X,2I9,2X,3(E14.4,F7.2)/(24X,E14.4,F7.2,E14.4,F7.2,
C     1            E14.4,F7.2,E14.4,F7.2) )
            IF (IBC (ISYS, I) .GT. SG) CALL WERR (5, I, IBC (ISYS, I))
            NBRK30 (ISYS,I) = NOBRK
C
            DO 1240 J = 1, NOBRK
               FILE30 (J, IM) = T (J)
 1240       CONTINUE
            DO 1250 J = 1, NOBRK
               BCT (J) = BCT (J)*BSCAL
               FILE30 (J, II) = BCT (J)
 1250       CONTINUE
 1210    CONTINUE
         GO TO 1000
C
C***********************************************************************
C          OPTION 0 - NO BC FOR THIS SYSTEM
C***********************************************************************
C
 1030    CONTINUE
C
         WRITE (OUT, 6100) ISYS
 6100    FORMAT(///22X,'No Boundary Conditions for System',I3//)
C
         WRITE(OUT,6105)
 6105    FORMAT('1')
 1000 CONTINUE
C
      RETURN
C
 1020 CONTINUE
      WRITE (OUT, 6110) ISYS, NO, BC
 6110 FORMAT(///1X,/10X,'The Number of Boundaries Specified ',
     1       'For System',I3,' is ',I4/10X,
     2       'The Maximum Dimensioned for This Version of WASP ',
     3       'is ',I3/10X,'Respecify Boundaries or Redimension ')
      WRITE (OUT, 6120)
 6120 FORMAT(1X,80('*'))
      CALL WERR(9,1,0)
      CALL WEXIT('Dimension Error See the Output File',1)
 1010 CONTINUE
      CALL WERR(34,1,0)
      CALL WEXIT('Error Reading Card Group E',1)
      END

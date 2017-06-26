      SUBROUTINE CHM2INT(ILITE)
      INCLUDE 'WASP.CMN'
      INCLUDE 'CHMLOC.CMN'
      INCLUDE 'PARAM.EQU'
      INCLUDE 'CHEM2.EQU'
      INCLUDE 'PHOTOL.CMN'
      INCLUDE 'KNETIC.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG,XARG1,XARG2
      CHARACTER STRING*12,STROUT*60
C----------------------------------------------------------------------
      KH (2) = KHOH + KHN + KHH
      IF (TREFH .GT. 0.0) THEN
         TRH (2) = TREFH
         IF (TRH (2) .LT. 50.) TRH (2) = TRH (2) + 273.0
      ELSE
         TRH (2) = 293.0
      END IF
C
C-----------------------------------------------------------------------
C                     *** IONIZATION ***
C-----------------------------------------------------------------------
C
      IF (TREFI .GT. 0.0)THEN
         TRI(2) = TREFI
         IF (TRI(2) .LT. 50.)TRI(2)=TRI(2) + 273.
      ELSE
         TRI(2)=293.0
      ENDIF
C
C----------------------------------------------------------------------
C                         **** Sorption ***
C----------------------------------------------------------------------
C
C If LKOC is not Specified, Calculate from LKOW
C
C   
C    If the Neutral Hydrophobic Partition Coefficient to Solid I is
C    Specified, use it Directly by Setting FOC (I) = 1.
C   
            IF(LKOW .LT. 0. .OR. LKOW .GT. 10)THEN
                CALL REASTR(LKOW,STRING,'(F10.2)')
            STROUT='LKOW OUT OF RANGE (0 - 10)  YOU SPECIFIED '//STRING
                CALL WEXIT(STROUT,1)
            ELSE
CCSC
            XARG1 = ABS(LKOC)
            XARG2 = ABS(LKOW)
C            IF(LKOC.EQ.0.0.AND.LKOW.NE.0.0)LKOC=A0 +A1*LKOW
            IF (XARG1.LT.R0MIN.AND.XARG2.GT.R0MIN)LKOC=A0 +A1*LKOW
            endif
            DO 1000 I=1,3
CCSC
                XARG = ABS(PIXC(1,I))
C                IF(PIXC(1,I).EQ.0.)THEN
                IF (XARG .LT. R0MIN) THEN
CCSC
                    XARG = ABS(LKOC)
C                    IF(LKOC.NE.0.0)PIXC(1,I)=10**LKOC
                    IF (XARG .GT. R0MIN)PIXC(1,I)=10**LKOC
                ELSE
                    DO 1010 ISEG=1,NOSEG
                       FOC(ISEG,I)= 1.0
1010                CONTINUE
                END IF
1000        CONTINUE
C
C ---------  VOLATILIZATION
CCSC
      XARG1 = ABS(HENRY)
      XARG2 = ABS(SOLG)
C      IF (HENRY .EQ. 0. .AND. SOLG .NE. 0.) HENRY = VAPRG/SOLG
      IF (XARG1.LT.R0MIN.AND.XARG2.GT.R0MIN) HENRY = VAPRG/SOLG
      XARG1=ABS(HENRY)
      IF (XARG1 .LT. R0MIN) HENRY=R0MIN
      IF (XV .GE. 1.0)THEN
         DO 3000 I = 1, NOSEG
            IF (ITYPE(I) .EQ. 1.)THEN
               IF (DEPTHG(I) .LE. 0.)THEN
                  CALL WEXIT('Volatilization Option Used without Specify
     .ing Segment Depths',1)
               END IF
            END IF
3000      CONTINUE
      ENDIF
C
CCSC
      XARG = ABS(KLT)
C      IF (KLT .EQ. 0.) KLT = 1.
      IF (XARG .LT. R0MIN) KLT = 1.
C
C ---------  OXIDATION
C
      IF (TREFO .GT. 0.0) THEN
         TRO (2) = TREFO
         IF (TRO (2) .LT. 50.) TRO (2) = TRO (2) + 273.0
      ELSE
         TRO (2) = 293.0
      END IF
C
C --------- Photolysis
C
         IF (ILITE .LT. 4) THEN
            DO 1030 LAMKNT = 1, 46
               ABSORG (2, LAMKNT, 1) = ABS21 (LAMKNT)
               ABSORG (2, LAMKNT, 2) = ABS22 (LAMKNT)
               ABSORG (2, LAMKNT, 3) = ABS23 (LAMKNT)
               ABSORG (2, LAMKNT, 4) = ABS24 (LAMKNT)
               ABSORG (2, LAMKNT, 5) = ABS25 (LAMKNT)
 1030       CONTINUE
         END IF
C
C ---------  Extra Reaction
C
        IF (TREFE .GT. 0.0)THEN
           TRE (2) = TREFE
           IF (TRE (2) .LT. 50.)TRE (2) = TRE (2) + 273.
        ELSE
           TRE (2) = 293.
        ENDIF
        RETURN
      END

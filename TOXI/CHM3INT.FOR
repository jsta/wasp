      SUBROUTINE CHM3INT(ILITE)
      INCLUDE 'WASP.CMN'
      INCLUDE 'CHMLOC.CMN'
      INCLUDE 'PARAM.EQU'
      INCLUDE 'CHEM3.EQU'
      INCLUDE 'PHOTOL.CMN'
      INCLUDE 'KNETIC.CMN'
      INCLUDE 'CONST.INC'
      REAL XARG,XARG1,XARG2
      CHARACTER STRING*12,STROUT*60
C----------------------------------------------------------------------
      KH (3) = KHOH + KHN + KHH
      IF (TREFH .GT. 0.0) THEN
         TRH (3) = TREFH
         IF (TRH (3) .LT. 50.) TRH (3) = TRH (3) + 273.0
      ELSE
         TRH (3) = 293.0
      END IF
C
C-----------------------------------------------------------------------
C                     *** IONIZATION ***
C-----------------------------------------------------------------------
C
      IF (TREFI .GT. 0.0)THEN
         TRI(3) = TREFI
         IF (TRI(3) .LT. 50.)TRI(3)=TRI(3) + 273.
      ELSE
         TRI(3)=293.0
      ENDIF
C
C----------------------------------------------------------------------
C                        **** Sorption ***
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
                    IF (XARG.GT.R0MIN)PIXC(1,I)=10**LKOC
                ELSE
                    DO 1010 ISEG=1,NOSEG
                       FOC(ISEG,I)= 1.0
1010                CONTINUE
                END IF
1000        CONTINUE
C
C ---------  VOLATILIZATION
C
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
CCSC
      XARG = ABS(KLT)
C      IF (KLT .EQ. 0.) KLT = 1.
      IF (XARG .LT. R0MIN) KLT = 1.
C
C ---------  OXIDATION
C
      IF (TREFO .GT. 0.0) THEN
         TRO (3) = TREFO
         IF (TRO (3) .LT. 50.) TRO (3) = TRO (3) + 273.0
      ELSE
         TRO (3) = 293.0
      END IF
C
C --------- Photolysis
C
         IF (ILITE .LT. 4) THEN
            DO 1030 LAMKNT = 1, 46
               ABSORG (3, LAMKNT, 1) = ABS31 (LAMKNT)
               ABSORG (3, LAMKNT, 2) = ABS32 (LAMKNT)
               ABSORG (3, LAMKNT, 3) = ABS33 (LAMKNT)
               ABSORG (3, LAMKNT, 4) = ABS34 (LAMKNT)
               ABSORG (3, LAMKNT, 5) = ABS35 (LAMKNT)
 1030       CONTINUE
         END IF
C
C ---------  Extra Reaction
C
        IF (TREFE .GT. 0.0)THEN
           TRE (3) = TREFE
           IF (TRE (3) .LT. 50.)TRE (3) = TRE (3) + 273.
        ELSE
           TRE (3) = 293.
        ENDIF
        RETURN
      END

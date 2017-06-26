      SUBROUTINE   INIT
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'CONST.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          R2PREC,D2PREC,TI
      REAL             R1,R2,R3,R4,  TR
      DOUBLE PRECISION D1,D2,D3,D4
C
C     + + + FUNCTIONS + + +
      REAL             RNOP
      DOUBLE PRECISION DNOP

C     + + + INTRINSICS + + +
      INTRINSIC  INT,LOG10,DBLE
C
C     + + + EQUIVALENCE STATEMENTS + + +
      EQUIVALENCE (TR,TI)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' UNKNOWN DOUBLE PRECISION FORMAT, using default double pre
     .cision values!')
 2010 FORMAT(' UNKNOWN MACHINE TYPE, using default precision values!')
C
C     + + + STATEMENT FUNCTION DEFINITIONS + + +
C     No OPperation, used to keep Ryan/McFarland optimization honest
      RNOP(R1) = R1
      DNOP(D1) = D1
C
C     + + + END SPECIFICATIONS + + +
C
C     Calculate machine dependent numeric constants
C
C     Determine the number of decimal digits of REAL precision number and
C     the smallest REAL greater than 1.0.
C     First find the number of significant binary digits, then convert
C     it to the number of significant decimal digits.  Any machine used
C     today is going to have more than 7 binary digits of precision 
C     (actually, we're cheating, because 1 is added to R2PREC after
C     it is tested.  This usually results in 7 decimal digits of
C     precision, which is usually the case, whereas strictly
C     speaking only 6 decimal digits are guaranteed, and 6 is
C     usually the result if R2PREC is initialized to 6).
      R2PREC = 7
      R1 = 1.0
      R2 = 0.0078125
 100  CONTINUE
      R3 = R1 + R2
C     this 'nop' keeps Ryan/McFarland optimization honest.  Without it,
C     the precision of an 80 bit floating point register is computed
C     (instead of a 4-byte real) when R/M optimization is turned on.
C     You can comment out the call to NOP if you observe that by
C     doing so DECCHR doesn't provide extra digits of precision.
      R4 = RNOP(R3)
      IF (R1 .LT. R3) THEN
         RP1MIN = R3
         R2PREC = R2PREC + 1
         R2 = R2 / 2.0
         GO TO 100
      END IF
C
      RPREC = INT(LOG10(2.0**R2PREC))
C
C     Determine the number of decimal digits of the typical DOUBLE precision
C     number and the smallest DOUBLE greater than 1.0D0.  
      D2PREC = 7
      D1 = 1.0D0
      D2 = 0.0078125D0
 200  CONTINUE
      D3 = D1 + D2
C     this 'nop' keeps Ryan/McFarland optimization honest.  Without it,
C     the precision of an 80 bit floating point register is computed
C     (instead of a 4-byte real) when R/M optimization is turned on.
C     You can comment out the call to NOP if you observe that by
C     doing so DECCHR doesn't provide extra digits of precision.
      D4 = DNOP(D3)
      IF (D1 .LT. D3) THEN
         DP1MIN = D3
         D2PREC = D2PREC + 1
         D2 = D2 / 2.0D0
         GO TO 200
      END IF
C
      DPREC = INT(LOG10(2.0D0**D2PREC))
C
      TR = 1.0
      IF (TI .EQ. 1065353216) THEN
C       this should be the case for the Sun or Ryan/McFarland
        R1   = 1.0E-19
C        R0MIN = 1.17549435E-19 * R1
        R0MIN = 1.1754945E-19 * R1
        R1   = 1.0E+19
        R0MAX = 3.40282347E+19 * R1
        D1   = 1.0D-28
        D0MIN = (2.22507385850720219D-28 * D1**10)
        D1   = 1.0D+28
        D0MAX = (1.7976931348623157D0 * D1**11)
      ELSE IF (TI .EQ. 16512) THEN
C       this should be the case for the VAX
        R1   = 1.0E-20
        R0MIN = 2.9387359E-19 * R1
        R1   = 1.0E+18
        R0MAX = 1.7014117E+20 * R1
        IF (DPREC .EQ. 17) THEN
C          this should be the case for the default /NOG_FLOAT compiler option
           D1   = 1.0D-20
           D0MIN = 2.938735877055719D-19 * D1
           D1   = 1.0E+19
           D0MAX = 1.7014118346046923D+19 * D1
        ELSE IF (DPREC .EQ. 16) THEN
C          this should be the case for the /G_FLOAT option
           D3 = 1.0D-21
           D4 = 1.0D-20
           D0MIN = (5.562684646268008D-20)*(D4**5)*(D3**9)
           D3 = 1.0D+21
           D4 = 1.0D+20
           D0MAX = (8.988465674311578D+21) * (D3**6) * (D4**8) 
        ELSE 
C          assume D_FLOAT real*8 type
           WRITE(*,2000)
           D1   = 1.0D-20
           D0MIN = 2.938735877055719D-19 * D1
           D1   = 1.0E+19
           D0MAX = 1.7014118346046923D+19 * D1
        END IF
      ELSE
        WRITE(*,2010)
        R1    = 1.0E-19
        R0MIN = 1.17549435E-19 * R1
        R1    = 1.0E+18
        R0MAX = 1.7014117E+20 * R1
        D1    = 1.0D-20
        D0MIN = 2.938735877055719D-19 * D1
        D1    = 1.0E+19
        D0MAX = 1.7014118346046923D+19 * D1
      ENDIF
C
      RETURN
      END

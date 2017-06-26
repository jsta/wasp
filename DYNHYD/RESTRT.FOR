      SUBROUTINE RESTRT
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
      INCLUDE 'DYNHYD.CMN'
C
      TRSTRT = T/3600.
      NRSTRT = NCYC
      WRITE (RSTR, 6000) (ALPHA (I), I = 1, 2), TRSTRT, NRSTRT
      DO 1000 J = 1, NJ
         WRITE (RSTR, 6010) J, Y (J), SURF (J), BELEV (J),
     1      (NCHAN (J, K), K = 1, CJ)
 1000 CONTINUE
      DO 1010 N = 1, NC
         CDIR (N) = CDIR (N)*180./3.1416
         WRITE (RSTR, 6020) N, CLEN (N), B (N), R (N), CDIR (N),
     1      CN (N), V (N), (NJUNC (N, K), K = 1, 2)
 1010 CONTINUE
C
C                 Print Restart Data
C                    Junctions
C
      WRITE (OUT, 6030)
      WRITE (OUT, 6040)
      DO 1020 J = 1, NJ
         WRITE (OUT, 6050) J, Y (J), SURF (J), QIN (J),
     1      (NCHAN (J, K), K = 1, CJ)
 1020 CONTINUE
C
C                       Channels
C
      WRITE (OUT, 6060)
      WRITE (OUT, 6070) (N, CLEN (N), B (N), AREA (N), CN (N), V (N),
     1   R (N), (NJUNC (N, K), K = 1, 2), N = 1, NC)
C
C********************  FORMAT STATEMENTS  *********************
C
 6010 FORMAT (I5,F10.4,F10.0,F10.2,10I5)
 6020 FORMAT (I5,2F10.0,F10.2,F10.0,F10.3,F10.5,2I5)
 6030 FORMAT (1H1///,78(1H*)//,31X,
     1       ' Junction Data for Restart Deck'//78(1H*)//)
 6040 FORMAT (' Junction   Initial Head   Surface Area   Input-Output
     1     Channels Entering Junction'//)
 6050 FORMAT(' ',I6,F15.4,F17.0,F11.2,I12,9I6)
 6060 FORMAT (1H1///,78(1H*)//,31X,
     1        ' Channel Data for Restart Deck'//78(1H*)//)
 6070 FORMAT ( 97H Channel   Length   Width     Area    Manning   Veloci
     1ty   Hyd Radius           Junctions at Ends//
     2(1H ,I5,F11.0,F8.0,F10.0,F9.3,F10.5,F13.2,I18,I6))
 6000 FORMAT (A80,/A80/F10.5,I5)
      RETURN
      END

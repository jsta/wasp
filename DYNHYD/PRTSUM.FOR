      SUBROUTINE PRTSUM
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C    Print out Summary of Junction Data
      WRITE (OUT, 6000)
      DO 1000 J = 1, NJ
         WRITE (OUT, 6010) J, Y (J), SURF (J), belev(j),
     1      (NCHAN (J, K), K = 1, CJ)
 1000 CONTINUE
C    Print out Summary of Channel Data
      WRITE (OUT, 6020)
      DO 1010 N = 1, NC
         DH = (Y (NJUNC (N, 1)) + Y (NJUNC (N, 2)))/2
     ,    - (belev (NJUNC (N, 1)) + belev (NJUNC (N, 2)))/2
cba
c         AREA (N) = (B (N) - (R (N) + DH)*SLOPE (N))*(R (N) + DH)
          area (n) = b(n)* dh
cba
         WRITE (OUT, 6030) N, CLEN (N), B (N), R (N),
     1      AREA (N), CDIR (N), CN (N),
     2      V (N), (NJUNC (N, K), K = 1, 2)
 1010 CONTINUE
C ****** Format Statements ******
 6000 FORMAT (1H1,//1X,130(1H*),//53X,'SUMMARY OF JUNCTION DATA',//1X,13
     10(1H*),///25X,'JUNC  INI HEAD  SURFACE AREA  belev
     2          CHANNELS ENTERING JUNCTION',/20X,90(1H=),/)
 6010 FORMAT (20X,I3,9X,F6.2,3X,F10.0,3X,f6.2,3x, 7I3 )
 6020 FORMAT (1H1,//1X,130(1H*),//53X,'SUMMARY OF CHANNEL DATA',//1X,130
     1(1H*),///10X,'CHANNEL       LENGTH     WIDTH      DEPTH     C.S. A
     2REA     DIRECTION      ROUGHNESS        VELOCITY   JUNCTIONS AT',
     3' ENDS',/5X,120(1H=),/ )
 6030 FORMAT (12X,I3,8X,F7.0,3X,F7.0,6X,F4.1,5X,F9.0,8X,F4.0,8X,E10.2,
     17X,F8.5,5X,I3,5X,I3 )
      RETURN
      END

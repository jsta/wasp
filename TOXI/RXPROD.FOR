      SUBROUTINE RXPROD
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:13.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      INCLUDE 'KNETIC.CMN'
C
C
      DO 1000 ICHM = 1, 3
         DO 1010 IPROD = 1, 3
            IF (IPROD .EQ. 1) JPROD = 1
            IF (IPROD .EQ. 2) JPROD = 5
            IF (IPROD .EQ. 3) JPROD = 6
            CD (JPROD, ISEG) = CD (JPROD, ISEG) +
     1         (BIOW (ICHM)*Y (ICHM, 1, IPROD) +
     2         BIOS (ICHM)*Y (ICHM, 2, IPROD) +
     3         ALKH (ICHM)*Y (ICHM, 3, IPROD) +
     4         NEUTH (ICHM)*Y (ICHM, 4, IPROD) +
     5         ACIDH (ICHM)*Y (ICHM, 5, IPROD) +
     6         OX (ICHM)*Y (ICHM, 6, IPROD) +
     7         FOTO (ICHM)*Y (ICHM, 7, IPROD) +
     8         EXRN (ICHM) * Y (ICHM, 8, IPROD) +
     9         RATE9 (ICHM)*Y (ICHM, 9, IPROD) +
     A         RATE10 (ICHM)*Y (ICHM, 10, IPROD))*BVOL (ISEG)
 1010    CONTINUE
 1000 CONTINUE
      RETURN
      END

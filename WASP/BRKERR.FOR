      SUBROUTINE BRKERR (NOBRK)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:24:09.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
C-----------------------------------------------------------------------
      WRITE (OUT, 6000) NOBRK, MB
 6000 FORMAT(///1X,/,20X,'The Number of Breaks ',
     1  'Specified is ',I5/20X,
     2  'The Maximum Dimensioned for this Version of WASP',
     3  ' is ',I3/20X,'Respecify Breaks or Redimension',
     4  ' Parameter "MB" in the Common Block and Recompile.')
      WRITE (MESS, 6010)
 6010 FORMAT(///5X,'The Number of Breaks Specified is ',I5/5X,
     1  'the Maximum Dimensioned for this Version of WASP',
     2  ' IS ',I3/5X,'Respecify Breaks or Redimension',
     3  ' Parameter "MB" in the COMMON BLOCK and RECOMPILE.')
      CALL WERR(29,1,0)
      CALL WEXIT('Maximum Number of Breaks Exceeded in Time Function',
     11)
      END

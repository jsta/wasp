      PROGRAM WASP4V
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:16.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
C*-*-
      COMMON /W19/ ISKPLT
      COMMON /SWFLW/ SCALQ, SCALV, NJ, NC, JUNSEG (SG), SEGJUN (SG),
     1   NSEA, JSEA (5), SIGN (5), SEACHN (5), NBC, NUPS
C
C***********************************************************************
C                 CALLING SEQUENCE FOR WASP SUBROUTINES
C***********************************************************************
      ISKPLT = 0
      ICOLOR = 0
C
      CALL INIT
C
      CALL WASP1
C
      CALL WASP2
C
      CALL WASP3
C
      CALL WASP4
C
      CALL WASP5
C
      CALL WASP6
C
      CALL WAS6A
C
      CALL WASP7
C
      CALL WASP9
C
      CALL EULER
C
      CALL WNCLOS(1)
C
      CALL CLSCRN
C
      CLOSE (UNIT = IN)
      CLOSE (UNIT = OUT)
      CLOSE (UNIT = 15)
      CLOSE (UNIT = 16)
      CLOSE (UNIT = 20)
C
      STOP
      END

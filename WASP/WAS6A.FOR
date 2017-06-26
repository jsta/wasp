      SUBROUTINE WAS6A
C======================================================================
C  Last Revised:  Date: Monday, 4 March 1991.  Time: 09:04:38.
C
C
C  Correction History:
C
C----------------------------------------------------------------------
      INCLUDE 'WASP.CMN'
C
      CHARACTER*12 NPSFIL,files(30)
      DIMENSION KPRT (SY)
C=======================================================================
      NPSINIT=0
C
      READ (IN, 5000) LOPT, NPSFIL
 5000 FORMAT(I10,10x,a12)
C
C  If Non-Point Source Loading Option is 0 Return 
C
      IF (LOPT .EQ. 0) RETURN
C
      CALL GETMES (17, 1)
C
      IF (NPSFIL .EQ. '           ')THEN
         N = 30
         CALL OSDIRL (' ','*.NPS',FILES, N)
         IF ( N .EQ. 0)THEN
            CALL ATTRIB(' ')
            CALL COLOR('W','RED')
            CALL WNOPEN(0,21,70,2)
            CALL WNOUST('No NPS file(s) were found!!  WASP'
     1      //' assumes the NPS filenames have the extension *.NPS'
     2      //'  Please correct and re-execute.')
            CALL WERR(40,1,0)
            CALL WEXIT('Error Finding NPS File',1)
         ELSE
            CALL ATTRIB('B')
            CALL COLOR('CYAN','BLUE')
            CALL WNOPEN(0,21,70,3)
            CALL WNOUST('Please select the input file to execute.  '
     1      //'WASP assumes an NPS filename extension of #.NPS and  '
     2      //'must be located in the default directory.')
            CALL COLOR ('CYAN', 'BLUE')
            IOPT = MNSCRL (FILES, N, 0, 0, 'Non Point Source Files',
     1                     10, 2, 1)
            NPSFIL=FILES(IOPT)
         ENDIF
      END IF
C
C***********************************************************************
      CALL NPS_TV(NPSFIL)
C***********************************************************************
      RETURN
      END

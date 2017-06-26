      SUBROUTINE WERR (IMES, IS, JS)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:17.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      CHARACTER*40 ERRORTXT
C
C-----------------------------------------------------------------------
      CALL WSPERROR(IMES,1)
C
      GO TO ( 1000, 1010, 1020, 1030, 1040, 1050,
     1    1060, 1020, 1020, 1070), IMES
C
 1000 CONTINUE
      WRITE (OUT, 6000) IS, JS
 6000 FORMAT(1X,' Check Exchanges for Improper Segment Number ',
     1' ---   (',I5,',',I5,')')

      RETURN
C
 1010 CONTINUE
      WRITE (OUT, 6000) IS, JS
      RETURN
C
 1030 CONTINUE
      WRITE (OUT, 6010) IS, JS
 6010 FORMAT(1X,' Check Flows for Improper Segment Number ',
     1' ---   (',I5,',',I5,')')
      RETURN
C
 1040 CONTINUE
      WRITE (OUT, 6020) ISYS, IS, JS
 6020 FORMAT(1X,' Check System ',I2,' Boundary No. ',I2,' for ',
     1 'Improper Segment Number ---  (',I5,')')
      RETURN
C
 1050 CONTINUE
      WRITE (OUT, 6030) ISYS, IS, JS
 6030 FORMAT(1X,' Check System ',I2,' Load No. ',I2,' for ',
     1 'Improper Segment Number ---  (',I5,')')
      RETURN
C
 1060 CONTINUE
      WRITE (OUT, 6040) ISYS, IS, JS
 6040 FORMAT(1X,'System ',I2,' Missing Boundary Condition   ---'
     1 ,'   (', I5  ,',',I5,')')
      GO TO 1070
C
 1020 CONTINUE
      RETURN
C
 1070 CONTINUE
      WRITE (OUT, 6050)
 6050 FORMAT(///1X,/,20X,'Simulation ABORTED Due to Errors in ',
     1  'Input Data',/,20X,'Check Output Listing for Specific Messages'/
     2  1X)
      CALL WEXIT('Simulated Halted Due to Errors',1)
      END

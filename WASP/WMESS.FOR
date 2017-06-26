      SUBROUTINE WMESS (I, DATA)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:26:19.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C        THIS ROUTINE IS USED TO INFORM USER CONCERNING CHANGES IN
C        INTEGRATION PROCEDURE AND/OR STABILITY DURING THE SIMULATION
C
      INCLUDE 'WASP.CMN'
C
C-----------------------------------------------------------------------
C
      IF (I .EQ. 1) THEN
         WRITE (OUT, 6000) TIME, DATA, DT
 6000    FORMAT( /9X,'Integration Interval Doubled at Time',F7.2,
     1   ' From',F7.4,' to',F7.4)
      ELSE
         IF (I .EQ. 2) THEN
            WRITE (OUT, 6010) TIME, ISYS, ISEG, C (ISYS, ISEG),
     1         DATA, CD (ISYS, ISEG)
 6010       FORMAT(/5X,'Stability Criteria Violated at Time',F9.3,
     1      ' in System',I4,' Segment',I4 / 4X,'C =',E15.6,
     2      ' Exceeded CMAX =',E15.6,5X,'Derivative at Previous Step =',
     3      E15.6// 5X,'Computation Discontinued, Dumps Until Last ',
     4      'Printout will Follow')
         ELSE
            IF (I .EQ. 3) THEN
               WRITE (OUT, 6020) TIME, DATA, C (ISYS, ISEG), CD
     1            (ISYS, ISEG), ISYS, ISEG, DT
 6020          FORMAT( /2X,'Change in Step Size Needed at Time', F7.2,
     1                /,2X, ' CONCK =',E12.4,' CONC =',E12.4,' DER =',
     2                E12.4,' SYS =',I3,' SEG =',I3,' DT =',F6.4)
            ELSE
               IF (I .EQ. 4) THEN
                  WRITE (OUT, 6030)
 6030             FORMAT(//5X,'Minimum Step Size Reached, However,',
     1                  ' Computation will Continue'//)
               ELSE
                  IF (I .EQ. 5) THEN
                     WRITE (OUT, 6040) DATA, TIME, ISYS, ISEG,
     1                  C (ISYS, ISEG)
 6040                FORMAT(/5X,'Negative Conc (',E11.4,') Computed ',
     1               'at Time = ',F7.2,' in System',I3,' Segment',I4,
     2               /23X,'Half Concentration Adjustment Made ',
     3              '(Conc = ',E11.4,')')
                  END IF
               END IF
            END IF
         END IF
      END IF
      RETURN
      END

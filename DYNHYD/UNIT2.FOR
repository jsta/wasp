      SUBROUTINE UNIT2
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C**********************   STORE DATA ON UNIT 2    **********************
	if( icyc .eq. 1  .or.  icyc .eq. itape)then
         WRITE (SCR, 6030) ICYC
         WRITE (SCR, 6040) (VOL (J), J = 1, NJ)
         WRITE (SCR, 6050) (V (N), Q (N), R (N), N = 1, NC)
	 if(icyc .gt. 1) iscr = -1
	end if
cia
      ISCR = ISCR + 1
      IF (ISCR .GE. INTSCR) THEN
         ISCR = 0
         WRITE (SCR, 6030) ICYC
 6030    FORMAT (I10)
         WRITE (SCR, 6040) (VOL (J), J = 1, NJ)
 6040    FORMAT (E13.3)
         WRITE (SCR, 6050) (V (N), Q (N), R (N), N = 1, NC)
 6050    FORMAT(3(E13.3))
      END IF
      IF (ICYC .GE. IDUMP) THEN
         LTAPE = ICYC
         IDUMP = IDUMP + ITDUMP
         IF (IDUMP .GT. NCYC) IDUMP = NCYC
cia
         write(scr, 9876)
9876     format(10x, ' The end of this round of scratch file process')
cia
         CALL hydrout
         REWIND SCR
         WRITE (SCR, 6030) ICYC
         WRITE (SCR, 6040) (VOL (J), J = 1, NJ)
         WRITE (SCR, 6050) (V (N), Q (N), R (N), N = 1, NC)
         ITAPE = ICYC
      END IF
      RETURN
      END

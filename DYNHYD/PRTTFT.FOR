      SUBROUTINE PRTTFT
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
C     PRINT TRANSIENT-FLOW TAPE INFORMATION
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
      WRITE (OUT, 6000) ITAPE, SUMRY
 6000 FORMAT (///50X,'A TRANSIENT-FLOW TAPE WILL BE',/50X,'CREATED BEGIN
     1NING AT CYCLE ',I5,/60X,'(SUMRY = ',I1,')')
      RETURN
      END

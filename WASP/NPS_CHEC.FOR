      SUBROUTINE NPS_CHECK
      INCLUDE 'WASP.CMN'

      WRITE (OUT,9000)NPTIME
 9000 FORMAT (1x,'Time: ',f10.4)
      DO 500 I = 1, NUMSYS
         WRITE(OUT,9100)LDSYS(I), (NP_LOAD(LDSYS(I),NP_SEG(K)),
     +             K=1,NUMLOAD)
 9100    FORMAT(1X,'Sys: ',I5,5x,'Load: ',8(f10.2))
  500 CONTINUE
      RETURN
      END

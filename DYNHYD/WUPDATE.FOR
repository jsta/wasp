      subroutine wupdate
      include 'DYNHYD.cmn'
      CHARACTER SCOLOR(9)*6
      DATA SCOLOR /'cyan','GREEN','cyan','GREEN','cyan','GREEN',
     1             'cyan','GREEN','cyan'/
      ISPACE=2
      if(nj .lt. 6)then
         npseg=nj
      else
         npseg=6
      endif
      iline=11
      rspace = 0.
      do 100 i =1,7
         icol=11
         CALL COLOR(SCOLOR(I),'BLACK')
        do 200 j = 1, npseg
          icol=icol + 10
          call outrxy(icol,iline,sysdump(i,j),'(e9.3)')
  200   continue
       iline=iline+ISPACE
  100 continue
      return
      end

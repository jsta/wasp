      subroutine wlabels
      include 'DYNHYD.cmn'
      character*15 Tvout(7)
      CHARACTER SCOLOR(9)*6
      DATA SCOLOR /'cyan','GREEN','cyan','GREEN','cyan','GREEN',
     1             'cyan','GREEN','cyan'/
      data Tvout /'Flow (cms)   CH','Vel. (m/sec) CH',
     1            'Up Head (m)  JU','Low Head (m) JU',
     2            'Mannings     CH','Momentum     CH',
     2            'Wind (m)     CH'/
        call outcen (3,'DYNHYD5 -- Intermediate Simulation Results')
      ISPACE=2
      l =12
      if(nj .lt. 6)then
         npseg=nj
      else
         npseg=6
      endif
      call color ('yellow','black')
      do 100 i=1,npseg
        k=isegout(i)
        l = l + 10
        call outixy (l,9,k,5)
  100 continue
      j = 11
      rspace = 0.
      do 200 k = 1,7
       CALL COLOR(SCOLOR(K),'BLACK')
        call outsxy(4,j,Tvout(k))
        j = j + ISPACE
  200 continue
      call rscreen
      return
      end

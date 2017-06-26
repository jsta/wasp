      subroutine keycheck
      include 'DYNHYD.cmn'
      character tseg(JU)*10
      call inchri(ichr)
      if (ichr .eq. 27)then
         CALL SCBACK ('blue', 'white', 1)
         CALL COLOR ('white', 'blue')
         CALL HIGHLT ('blue', 'white')
         call wnactn('f')
         CALL WNOPEN(0,9,45,1)
         CALL WNOUST(' You have elected to ABORT this simulation.  ')
         CALL HIGHLT ('cyan', 'black')
         IOPT = MNTWO ('Yes', 'No', 0, 0,
     1      'Quit Simulation: ', 1, 1)
         if (iopt .eq. 1) then
            CALL WNCLOS(0)
            Call wexit('User Aborted Simulation',1)
         endif
      endif
C=====================================================================
      if (ichr .eq. 67 .or. ichr .eq. 99)then
         if(NC .gt. 6)then
            do 10 i =1,nc
               call Intstr(I,tseg(I),'(I10)')
 10         continue
            imove=1
            do 20 i = 1, 6
               iopt = mnscrl(tseg,nc, 0, 0,'Segments',5, 2,imove)
               isegout(i)=iopt
               tseg(iopt)(1:1)='*'
               imove=iopt+1
 20         continue
            CALL WNCLOS(0)
            call clscrn
            call attrib(' ')
         else
             do 25 i=1,6
                isegout(i)=i
 25          continue
         endif
         call wscreen
         call wlabels
c         call wupdate
      endif
      return
      end

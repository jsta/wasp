       subroutine rscreen
       CALL COLOR('W','blue')
       CALL WNOPEN(0,25,80,1)
       CALL WNOUST(' ESC - Abort  C - Change Junction/Channels'
     1 //' JU=Junction Calc. CH=Channel Calc.')
       return
       end

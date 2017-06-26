      SUBROUTINE PROMPT(message,icol)
       character message*(*)
      CALL REVERS('ON')
      if (icol .eq. 0)then
         call color ('white','blue')
      else
         call attrib('F')
         call color ('white','red')
      endif
      CALL OUTMES(message)
      call bleep (' ')
      CALL REVERS(' ')
      CALL INKEY(KEY)
      call attrib(' ')
      RETURN
      END

      SUBROUTINE JHEAD (YHEAD, THEAD, J, jj)
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
c
C       CALCULATE HEAD OF SEAWADR BOUNDARIES
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
c
      IF (SEAOPT .LT. 3) THEN
         YHEAD = A1 (J, 1)
c feb.,90
         ns = 3
         DO 1000 I = 1, NS
            F = FLOAT (I)
            K = I + 1
            YHEAD = YHEAD + A1 (J, K)*
     1         SIN (F*W (J)*(THEAD - TSTART (J)))
     2         + A1 (J, NS + K)*
     3         COS (F*W (J)*(THEAD - TSTART (J)))
c90            SURF (jj) = YHEAD*SAN (J) + SURF (jj)
 1000    CONTINUE
      ELSE if(seaopt .eq. 3)then
         L = NHCYC (J)
         TNEXTC = TREP (J)*DTIME (J) + BTIME (J, L + 1)
         IF (THEAD .GE. TNEXTC) THEN
            IF (L + 1 .LE. NDATA) THEN
               NHCYC (J) = L + 1
               L = NHCYC (J)
               RANGE (J) = BHEAD (J, L + 1) - BHEAD (J, L)
               PERIOD (J) = 2.*(BTIME (J, L + 1) - BTIME (J, L))
            ELSE
               TREP (J) = TREP (J) + 1.0
               NHCYC (J) = 1
               RANGE (J) = BHEAD (J, 2) - BHEAD (J, 1)
               PERIOD (J) = 2.*(BTIME (J, 2) - BTIME (J, 1))
               NHCYC (J) = 1
               L = 1
            END IF
         END IF
         TTIME (J) = THEAD - (BTIME (J, L) + DTIME (J)*TREP (J))
         ARG = 2.*3.1416*TTIME (J)/PERIOD (J) + 3.1416
         YHEAD = BHEAD (J, L) + 0.5*RANGE (J)*(1. + COS (ARG))
c90         SURF (jj) = YHEAD*SAN (J) + SURF (jj)
cia....linear interpolation for opt = 4 ............
      else if (seaopt .eq. 4) then
8810     tcy = thead - trep(j)*dtime(j)
         if( tcy .gt. (dtime(j)+btime(j, 1)) ) then
	    trep(j) = trep(j) + 1.0
	    goto 8810
         else
            do 8820 k = 1, tc2
               if ( btime(j, k) .lt. tcy  .and.  btime(j, k+1) .ge.
     -              tcy ) then
                  part1 = (tcy-btime(j,k))/(btime(j,k+1)-btime(j,k))
                  part2 = 1.0 - part1
                  yhead = part1*bhead(j,k+1) + part2*bhead(j,k)
                  goto 8830
               end if
8820        continue
         end if
8830        continue
c90         SURF (jj) = YHEAD*SAN (J) + SURF (jj)
      END IF
      RETURN
      END

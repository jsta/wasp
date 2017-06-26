      SUBROUTINE AVGSG
C
C======================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
C
      DO 1000 J = 1, NJ
         depv = 0.0
         flowg(j)=0.0
         VOLG = 0.0
         VELG = 0.0
         QNORTH = 0.0
         QEAST = 0.0
         DO 1010 K = 1, CJ
            N = NCHAN (J, K)
            IF (N .GT. 0) THEN
               CHVOL = ((B (N) - RSAVE (N)*SLOPE (N))
     #                   *RSAVE (N))*CLEN (N)
               depv = depv + rsave(n)*chvol
               VELG = VELG + ABS (VSAVE (N))*CHVOL
               VOLG = VOLG + CHVOL
               flowg(j)=flowg(j)+qsave(n)
c               QNORTH = QNORTH + QSAVE (N)*COS (CDIR (N))
c               QEAST = QEAST + QSAVE (N)*SIN (CDIR (N))
            END IF
 1010    CONTINUE
         VELOCG (J) = VELG/VOLG
         DEPTHG (J) = depv/volg
c         FLOWG (J) = SQRT (QNORTH**2 + QEAST**2)
         FLOWG (J) = flowg(j)/K
         IF (QEAST .EQ. 0.0 .AND. QNORTH .EQ. 0.0) QNORTH = 1
         QDIR (J) = ATAN2 (QEAST, QNORTH)
         QDIR (J) = QDIR (J)*180.0/3.14159
 1000 CONTINUE
      RETURN
      end

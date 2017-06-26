      SUBROUTINE HYDROIN
C======================================================================
C  LAST REVISED: Thursday, 11 January 1990.  Time: 21:47:04.
C
C  CORRECTION HISTORY:
C  Corrected BQ converion from cms to cmd -- TAW
C  Updated the Runtime Dignostics
C----------------------------------------------------------------------
C    This subroutine is used to link the wasp4 modeling system to an
C    array of hydrodynamic models.  This replaces the DHYDX subroutines
C    in the old WASP structure.
C======================================================================
C
      INCLUDE 'WASP.CMN'
C
      CHARACTER*80 HEADER
C
      INTEGER HDEPVEL
C
      COMMON /DYNHYD/ ICYCLE, ITCYC, ITOTCYC, HDEPVEL, NSEG, NFLOW
C
C======================================================================
C
C     Check for the Beginning of the Simulation.  Need to perform
C     initial read of the Hydrodynamic Interface File (*.HYD)
C
C======================================================================
C
C
      IF (ICYCLE .EQ. 0) THEN
C
C       Start on the First Cycle in the Hydrodynamic File
C
         ICYCLE = 1
         IF (HDEPVEL .EQ. 0) THEN
            READ (HYDRO, 5000) (BVOL (J), QSEG (J), DEPTHG (J),
     1            VELOCG (J), J = 1, NSEG)
C
            READ (HYDRO, 5010) (BQ (1, 1, I), I = 1, NFLOW)
C
C======================================================================
C    Convert the Hydrodynamic Flows from cubic meters per second
C    to the internal units used by WASP cubic meters per day.
C======================================================================
C
            DO 1000 K = 1, NFLOW
               BQ (1, 1, K) = (BQ (1, 1, K)*86400.)
 1000       CONTINUE
            READ (HYDRO, 5000) (MVOL (J), QSEG (J), DEPTHG (J),
     1            VELOCG (J), J = 1, NSEG)
C
            GO TO 1010
         ELSE
            READ (HYDRO, 5010) (BVOL (J), J = 1, NSEG)
            READ (HYDRO, 5010) (BQ (1, 1, J), J = 1, NFLOW)
C
C======================================================================
C    Convert the Hydrodynamic Flows from cubic meters per second
C    to the internal units used by WASP cubic meters per day.
C======================================================================
C
            DO 1020 K = 1, NFLOW
               BQ (1, 1, K) = (BQ (1, 1, K)*86400.)
 1020       CONTINUE
            READ (HYDRO, 5010) (MVOL (J), J = 1, NSEG)
            GO TO 1010
         END IF
      END IF
C
C======================================================================
C
C             Check and REWIND HYDRO File
C
C======================================================================
C
      IF (ICYCLE .GT. ITCYC) THEN
         ITCYC = ITCYC + ITOTCYC
         IF (MFLAG .LE. 1) THEN
            IF (MFLAG .LT. 2)
     1CALL PROMPT('Reset Hydrodynamic File to Beginning of file',3)
              CALL ATTRIB('BOLD')
         END IF
C
         WRITE (OUT, 6000) TIME
 6000    FORMAT(5X,'Repeating HYDRO File at Time ',F7.2,//)
         REWIND HYDRO
         IF (HDEPVEL .EQ. 0) THEN
            KK = 1 + NFLOW
         ELSE
            KK = 1 + NFLOW + NSEG
         END IF
C
         DO 1030 K = 1, KK
            READ (HYDRO, 5020) HEADER
 5020       FORMAT(A80)
 1030    CONTINUE
C
C======================================================================
C
C  Check Volume Difference After Rewind HYDRO File.  This is done to
C  insure that there is not a significant volume differential between
C  the end of the Hydrodynamic interface file and the beginning.
C  If the volumes are not within 0.05 the simulation will be terminated
C  to prevent radical changes in concentration.
C
C======================================================================
C
         IF (HDEPVEL .EQ. 0) THEN
            READ (HYDRO, 5000) (MVOL (J), QSEG (J), DEPTHG (J),
     1            VELOCG (J), J = 1, NSEG)
         ELSE
            READ (HYDRO, 5010) (MVOL (J), J = 1, NSEG)
         END IF
C
         IF(IHINFO .EQ. 0)WRITE(OUT, 6009)
 6009       FORMAT(14X,'Water Volume Analysis Table for Hydrodynamic',
     +         ' Linkage',
     1         //,4X,'Seg. #       New Volume           Old Volume  ',
     +         '  % Diff.    Cumulative Error',/,4X,
     +         '======================================================',
     +         '===========================')

         DO 1040 I = 1, NSEG
            CRIT = ABS (MVOL (I) - BVOL (I))/BVOL (I)
            IF(IHINFO .EQ. 0)WRITE (OUT, 6011)I, MVOL(I),BVOL(I),
     +                                        CRIT,CRIT*IREPET
 6011       FORMAT(2X,I5,5X,F15.2,5X,F15.2,5X,F7.4,F15.2)
            IF (CRIT .GT. 0.05) THEN
               WRITE (OUT, 6010) I, CRIT
               CALL GETMES (27, 1)
 6010          FORMAT(9X,'Rewind failure, last VOLUME is not equal to',
     1                /,11X,'the first VOLUME in Hydro file. SEG # ',
     2                I4,' CRIT= ',F8.4)
               STOP
            END IF
 1040    CONTINUE
         IF(IHINFO .EQ.0)WRITE (OUT,6015)
 6015    FORMAT(//,2x,'+ Positive Error Indicated Loss of Mass',
     +         '- Negative Indicates Introduction of Mass')
         IHINFO=1
      END IF
C======================================================================
C
C         Continue with Reading the Hydrodynamic File
C
C======================================================================
      IF (HDEPVEL .EQ. 0) THEN
         READ (HYDRO, 5010) (BQ (1, 1, I), I = 1, NFLOW)
C
C======================================================================
C    Convert the Hydrodynamic Flows from cubic meters per second
C    to the internal units used by WASP cubic meters per day.
C======================================================================
C
         DO 1050 K = 1, NFLOW
            BQ (1, 1, K) = (BQ (1, 1, K)*86400.)
 1050    CONTINUE
C
         READ (HYDRO, 5000) (MVOL (I), QSEG (I), DEPTHG (I),
     1      VELOCG (I), I = 1, NSEG)
 5000     FORMAT (4F20.5)
C
      ELSE
         READ (HYDRO, 5010) (BQ (1, 1, I), I = 1, NFLOW)
C
C======================================================================
C    Convert the Hydrodynamic Flows from cubic meters per second
C    to the internal units used by WASP cubic meters per day.
C======================================================================
C
         DO 1060 K = 1, NFLOW
            BQ (1, 1, K) = (BQ (1, 1, K)*86400.)
 1060    CONTINUE
         READ (HYDRO, 5010) (MVOL (J), J = 1, NSEG)
 5010    FORMAT (F20.5)
C
      END IF
C
 1010 CONTINUE
C
C======================================================================
C  Write Hydrodynamic Information to the Output file.
C======================================================================
C
      IF (IQOPT .GE. 10) THEN
         WRITE (OUT, 6020) TIME + DT, ICYCLE
 6020    FORMAT(9X,'WASP Flows from Hydrodynamic Interface File'/,
     1           2X,'   at Time ',F8.2,', Hyd. Cycle',I10/)
         WRITE (OUT, 6030)
 6030    FORMAT(10X,3('     Flow   From  To ')/
     1           10X,3('   (cm/day)  Seg  Seg')/
     2           10X,3('   -------- ---- ----')/)
         WRITE (OUT, 6040) (BQ (1, 1, I), JQ (1, 1, I), IQ
     1      (1, 1, I), I = 1, NFLOW)
 6040    FORMAT(40(10X,3(E11.3,2I5)/))
      END IF
C
C======================================================================
C      Well, if you found this, you found an undocumented feature
C      of the Hydrodynamic Interface Routine, set Iqopt .gt. 10
C      and a DEBUG file will be created with the following information
C======================================================================
      IF (IQOPT .GE. 10) THEN
         WRITE (HDBG, 6020) TIME + DT, ICYCLE
         WRITE (HDBG, 6050)
 6050    FORMAT(2X,'Volume    Velocity     Depth'/
     1      1X,'---------  ---------  ---------')
         WRITE (HDBG, 6060) (MVOL (I), VELOCG (I),
     1          DEPTHG (I), I = 1, NSEG)
 6060    FORMAT( 3(E11.3))
      END IF
      ICYCLE = ICYCLE + 1
      RETURN
      END

      SUBROUTINE WAS4A (HYDROFIL)
C======================================================================
C  LAST REVISED: Date: Thursday, 1 February 1990.  Time: 16:26:07.
C
C  Correction History:
C
C
C------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      INTEGER HDEPVEL
      CHARACTER*12 HYDROFIL
C
      COMMON /DYNHYD/ ICYCLE, ITCYC, ITOTCYC, HDEPVEL, NSEG, NFLOW
C
      IF (MFLAG .EQ. 0) CALL GETMES (9, 0)
C
C======================================================================
C              Open the Hydrodynamic Interface File
C======================================================================
C
      OPEN (UNIT = HYDRO, FILE = hydrofil, STATUS = 'OLD')
      IF(IQOPT.GE.10)OPEN(UNIT=HDBG, FILE='HYDBG.
     1   OUT', STATUS = 'UNKNOWN')
C
      ICYCLE = 0
C
      READ (HYDRO, 5000) NSEG, NFLOW, HDT, BEGTIME, ENDTIME, HDEPVEL
 5000 FORMAT(2(I5),3(F20.5), I5)
C
C======================================================================
C      Calculate the Number of Hydrodynamic Cycles in the Hydrodynamic
C      File.
C======================================================================
C
      ITOTCYC = ((ENDTIME - BEGTIME)/HDT)
      ITCYC = ITOTCYC
C
C======================================================================
C     Convert the WASP Starting Times to Seconds for comparison to
C     the starting and ending times of the Hydrodynamic file.
C======================================================================
C
      WSTART = TZERO*86400.
      WEND = TEND*86400.
      NOQS (1, 1) = NFLOW
C
C======================================================================
C     Adjust the Simulation Start time if different
C======================================================================
C
      IF (WSTART .LT. BEGTIME) THEN
         CALL COLOR ('BLACK','BLACK')
         CALL SCBACK('BLACK','BLACK')
         CALL CLSCRN
         IF (MFLAG .LT. 2)
     1   CALL PROMPT ('WASP and DYNHYD Time Set Equal <Hit a Key>',0)
         TZERO = BEGTIME/86400.
      END IF
C
      RLENGTH=(WEND-TZERO)/86400.
      HDAY=RLENGTH
C
      IF (ENDTIME .LT. WEND) THEN
         IF (MFLAG .LT. 2)
     1   CALL PROMPT ('WASP Longer then HYD File <Hit a Key>',0)
         HDAY = ITOTCYC*HDT/86400.
         WRITE (OUT, 6000)
 6000    FORMAT(1x,//,20x,'Hydrodynamic File Summary Information',/,
     1          5x,70('~'))
         WRITE (OUT, 6010) ITOTCYC, HDAY
 6010    FORMAT(1x,/,15x,'The Water Quality Simulation is Longer Than ',
     1         'the',/,15x,'Hydrodynamic Simulation.  The Hydrodynamic',
     2         ' Simulation'/,15x,'will be Repeated After', I6,
     3         ' Cycles or Every ',F7.2,' Days',//)
      END IF
C
      HREPET=RLENGTH/HDAY
      IREPET=HREPET
      READ (HYDRO, 5010) (JQ (1, 1, I), IQ (1, 1, I), I = 1, NFLOW)
 5010 FORMAT(2(I5))
C
      IF (HDEPVEL .EQ. 1) THEN
         IF (MFLAG .LT. 2)
     1   CALL PROMPT ('Invariant Depths and Velocities will be used',0)
         CALL CLSCRN
         WRITE (OUT, 6020) HDEPVEL
 6020    FORMAT (1X,/,15x,'A Time INVARIANT Segment Velocity and ',
     1           'Depth will be used.',/,15x,'HDEPVEL = ',I5,//)
         READ (HYDRO, 5020) (QSEG (I), DEPTHG (I),
     1       VELOCG (I), I = 1, NSEG)
 5020    FORMAT (3F20.5)
      END IF
C
C======================================================================
C     Calculate the Water Qualtity Time Step Based on Information
C     From the Hydrodynamic Model
C======================================================================
C
      DT = HDT/86400.
C
C======================================================================
C     Turn TOPT Off
C======================================================================
C
      INTYP=0
C======================================================================
C    Write out the calculated Hydrodynamic Linkage Information
C    that is expected during Simulation
C======================================================================
C
      WRITE (OUT, 6030) NSEG, NFLOW, HDT, DT, TZERO, TEND, ITOTCYC
 6030 FORMAT(17x,'Number Hydrodynamic Segments = ',I5,/,
     1       17x,'Number of Flows = ',I5,/,
     2       17x,'Hydrodynamic Time Step = ',F7.2,/,
     3       17x,'Water Quality Model Time Step =',F5.2,/,
     4       17x,'Simulation Starting on ',F7.2,' days',/,
     5       17x,'           Ending on ',F7.2,' days',//,
     6       17x,'Total Number of Hydrodynamic Cycles = ',I5)
      WRITE (OUT, 6040)
 6040 FORMAT(1X,//,5X,70('^'))
C
      RETURN
      END

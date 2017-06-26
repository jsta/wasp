      PROGRAM DYN5
C
C                        Developed and Supported by
C                    Environmental Protection Agency
C                Center for Exposure Assessment Modeling
C                Athens Environmental Research Laboratory
C                             Athens, Georgia
C
C     DYNHYD5 describes the unsteady hydrodynamics of an estuarine
C     system by solving the equations of continuity and momentum.
C     hydraulic inflows or withdrawals can be steady or transient.
C     Variable tidal boundaries can be specified. If desired,
C     a record of volumes, flows, velocities, and depths can be
c     stored on magnetic tape or disk.
C
C*********************************************************************
C                            CONTROL OPTIONS
C*********************************************************************
C
C     ALPHA   = An alphanumeric field describing the important
C               features of the simulation
C
C     NJ      = Number of Junctions (NODES)
C     NC      = Number of Channels (LINKS)
C     NCYC    = Length of the simulation, in cycles
C     DELT    = Length of a hydraulic cycle, in seconds
C     TZERO   = Starting time of the simulation, in hours
C
C     IPRINT  = First cycle for printed output
C     INTRVL  = Interval, in cycles, between subsequent outputs
C     NOPRT   = Number of junctions for which printed
c               output is given
C     JPRT(I) = Junction number of the i-th junction
c               for which printed output is given
c
c     SUMRY   = 0  No record of hydraulic conditions is saved
c                  on magnetic tape or disk, or
c     sumry   = 1  A Record of the last dynamic steady-state
c                  tidal cycle is saved, or
C
C     ITAPE   = First cycle at which hydraulic data is saved
c*********************************************************************
C
C=====================================================================
C
C     Last Revised:  Date: Thursday, 11 January 1990.  Time: 08:30:00
C
C======================================================================
C
      INCLUDE 'DYNHYD.CMN'
      CHARACTER*1 PROP(4)
      DATA PROP /'|','/','-','\'/
C***********************************************************************
C                INPUT DATA AND INITIALIZE VARIABLES
C***********************************************************************
C
      CALL SHINIT(' ')
      IPROP=0
      CALL STARTUP
      INIT = 0
      CALL READIN
C
C***********************************************************************
C                        MAIN COMPUTATIONAL LOOP
C***********************************************************************
      CALL GETMES (15, 0)
      CALL WSCREEN
      CALL WUPDATE
      DO 1000 ICYC = 1, NCYC
         T2 = T + DT2
         T = T + DT
         CALL DYNHYD
         IF (IPROP .GT. 3)IPROP=0
          IPROP=IPROP + 1
          CALL OUTSXY (72,7,PROP(IPROP))
        IF (ICYC .GE. ITAPE .AND. (SUMRY .GE. 1))CALL UNIT2
         IF (ICYC .GE. IPRINT ) THEN
            IPRINT = IPRINT + INTRVL
            TIME = T/3600.
            CALL DYNDUM
         END IF
 1000 CONTINUE
      WRITE (OUT, 6000) LTAPE
 6000 FORMAT (///20X,'END OF ',I5,' CYCLE SIMULATION')
C
C***********************************************************************
C                        CLOSE SIMULATION
C***********************************************************************
C
C  RSTR file name is created from the concatenation of base of the input
C  file name and the extension .RST.  BASE is in common block FILE.
      OPEN (UNIT = RSTR, FILE = restfil, STATUS = 'UNKNOWN',
     1   FORM = 'FORMATTED')
      CALL RESTRT
      CLOSE (ICRD)
      CLOSE (IDMP)
      CLOSE (IN)
      CLOSE (OUT)
      IF(SUMRY .GE. 1) then
        CLOSE (SCR)
        CLOSE (SUMY)
      END IF
      CALL GETMES (20,1)
      call bleep (' ')
      call cursor('ON')
      CALL REVERS('ON')
      CALL BOLD('ON')
      call color ('white','blue')
      IF (WISP) THEN
         CALL OUTMES('Returning to WISP program...wait.')
      ELSE
         CALL OUTMES('Returning to DOS command prompt...wait.')
      ENDIF
      STOP
      END

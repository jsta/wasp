      SUBROUTINE LMDAMA (LAMAXG)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:20.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'PHOTOL.CMN'
C
      INCLUDE 'OPTION.CMN'
C
      REAL LAMAXG (5)
C
C     This routine determines the wavelength index for the user
C     specified wavelength of maximum light absorption for each
C     chemical and species.
C
C       loop for molecular and 4 ionic species
      DO 1000 ION = 1, 5
C
C       Skip nonexistent ions:
         IF (ION .GT. 1 .AND. SPFLG (ION - 1) .EQ. 0) GO TO 1000
C
C       A) Check to see that LAMAXG is in relevant spectral range,
C          else use 300:
         IF (LAMAXG (ION) .GE. 278.75 .AND.
     1         LAMAXG (ION) .LE. 825.) THEN
            GO TO 1010
         ELSE
            ILM (ION) = 9
            GO TO 1000
         END IF
 1010    CONTINUE
C
C       B) Locate proper line on table of absorption coefficients:
C
         IF (LAMAXG (ION) .GE. 321.25) GO TO 1020
C             First seventeen lines of table: set parameters for mapping
C             LAMAXG onto the table - "IBOT" is index of first line of t
C             section of the table, "XLBOT" is bottom of waveband, "XINT
C             bandwidth.
         IBOT = 1
         XLBOT = 278.75
         XINT = 2.5
         GO TO 1030
C
 1020    CONTINUE
         IF (LAMAXG (ION) .GT. 325.) GO TO 1040
         ILM (ION) = 18
         GO TO 1000
C
 1040    CONTINUE
         IF (LAMAXG (ION) .GE. 495.) GO TO 1050
         IBOT = 19
         XLBOT = 325.
         XINT = 10.
         GO TO 1030
C
 1050    CONTINUE
         IF (LAMAXG (ION) .GT. 512.5) GO TO 1060
         ILM (ION) = 36
         GO TO 1000
C
 1060    CONTINUE
         IF (LAMAXG (ION) .GE. 687.5) GO TO 1070
         IBOT = 37
         XLBOT = 512.5
         XINT = 25.
         GO TO 1030
C
 1070    CONTINUE
         IF (LAMAXG (ION) .GE. 725.) GO TO 1080
         ILM (ION) = 44
         GO TO 1000
C
 1080    CONTINUE
         IF (LAMAXG (ION) .LE. 775.) ILM (ION) = 45
         IF (LAMAXG (ION) .GT. 775.) ILM (ION) = 46
         GO TO 1000
C
C       Map LAMAXG onto table index:      
 1030    CONTINUE
         ILM (ION) = IBOT + INT ((LAMAXG (ION) - XLBOT)/XINT)
C
 1000 CONTINUE
C
      RETURN
      END

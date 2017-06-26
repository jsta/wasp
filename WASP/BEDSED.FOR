      SUBROUTINE BEDSED
C======================================================================
C  Last Revised:  Date: Friday, 23 August 1991.  Time: 10:40:44.
C
C
C  Correction History:
C
C
C----------------------------------------------------------------------
C     BEDSED Computes Bed Volume Changes (Sedimentation,Erosion) and
C     Porosity for Every Sedimentation Time Step.
C     For Variable Bed Volumes, Compute Change in Volume and Erosion
C     of Upper Bed Layers:
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'CONST.INC'
C
      CHARACTER MESSAGE*40,TEMP*3,TMPTIME*8
C
      REAL*4 XARG
C
      XBFLUX = 0.0
      IF (IBEDV .GE. 1) THEN
         DO 1000 ISEG = 1, NOSEG
C
C=======================================================================
C    Compute Volume Change in Upper Bed (ITYPE = 3) due to solids transport
C    (solids system identified by having a density greater than 1.5).
C    The change in volume is computed from the sum of the mass derivatives
C    (units of M/T) for the 3 possible solids types divided by the total
C     solids concentration (M/L^3) so that the TOTAL solids concentration
C     remains constant.
C=======================================================================
C
            IF (ITYPE (ISEG) .EQ. 3) THEN
               TOTALCD=0.
               TOTALC=0.
             DO 1010 ISYS = 1, NOSYS
               IF(DSED(ISYS) .GT. 1.5E06)THEN
                  TOTALCD=TOTALCD + CD (ISYS,ISEG)
                  TOTALC=TOTALC + C(ISYS,ISEG)
               ENDIF
 1010        CONTINUE
             MVOL (ISEG) = MVOL (ISEG) + DT * TOTALCD/TOTALC
C
C=======================================================================
C        If MVOL (the new volume) is Less Than 0  the Top Bed Segment Erodes
C        and Lower Bed Segments are Renumbered.
C=======================================================================
C
               IF (MVOL (ISEG) .LE. 0.0) THEN
                  J = ISEG
 1020             CONTINUE
                  N = IBOTSG (J)
                  IF (N .EQ. 0) THEN
                     DO 1030 ISYS = 1, NOSYS
                        CD (ISYS, J) = 0.0
                        IF (ISYS .EQ.1 .OR. ISYS .GE. 5)C(ISYS,J)=0.0
 1030                CONTINUE
                     MVOL (J) = B0VOL (J)
                     BVOL (J) = B0VOL (J)
                  ELSE
                     B0VOL (J) = B0VOL (N)
                     BVOL (J) = BVOL (N)
                     MVOL (J) = MVOL (N)
                     FRW (J) = FRW (N)
                     DO 1040 ISYS = 1, NOSYS
                        C (ISYS, J) = C (ISYS, N)
                        CD (ISYS, J) = CD (ISYS, N)
 1040                CONTINUE
                     J = N
                     GO TO 1020
               END IF
            END IF
         END IF
 1000    CONTINUE
      END IF
C
C=======================================================================
C  Sedimentation Time Step -- Porosity is Recomputed, and Compaction
C  of Bed Segments Occurs:
C=======================================================================
C
      IF (TIME .GE. TMARKS) THEN
         TMARKS = TMARKS + TDINTS
C
C=======================================================================
C    For Constant Volume Option, Recompute Porosity:
C=======================================================================
C
         IF (IBEDV .EQ. 0) THEN
            DO 1050 ISEG = 1, NOSEG
               SUM = 0.0
               DO 1060 I = 1, NOSYS
                  IF (F (2, I, NOSEG) .GE. 0.0) SUM =
     1               SUM + C (I, ISEG)/DSED (I)
 1060          CONTINUE
               FRW (ISEG) = 1. - SUM
 1050       CONTINUE
C
C=======================================================================
C    For Variable Volume Option, Compaction Occurs.
C      1st Compute Pore Volume Squeezed out and Modify Volumes
C      of Water (SEG M) and Surface Bed (Seg J).  Segment N
C      is the Lower Bed Segment (Type 4) Below Segment J.
C      DELVOL Represents the Portion of Upper Bed That will
C      be Compressed.
C=======================================================================
         ELSE
            DO 1070 J = 1, NOSEG
               M = TOPSEG (J)
               N = IBOTSG (J)
               IF(N .EQ. 0)GOTO 1070
                  DELVOL = BVOL (J) - B0VOL (J)
               IF (ITYPE (J) .EQ. 3 .AND. DELVOL .GT. 0.0) THEN
C
C=======================================================================
C     Second, reset the old and new volumes to their original values.
C     The PORVOL is the pore-water volume from the surface sediment
C     layer which is squeezed into the water column following
C     compaction.  The volume of the water column is incremented by this
C     amount.
C=======================================================================
C
                  MVOL (J) = B0VOL (J)
                  BVOL (J) = B0VOL (J)
                    IF (ITYPE (N) .EQ. 4) THEN
                        PORVOL = (DELVOL*(FRW (J) -
     1                  FRW (N))/(1. - FRW (N)))
                        MVOL (M) = MVOL (M) + PORVOL
                     ELSE
                     PORVOL = 0.0
                     END IF
C
C=======================================================================
C      Compute the chemical mass associated with the pore water volume and
C      increment the mass derivative by this amount.  Then for solids,
C      increment the first subsurface layer by the amount of solids settled
C      during the current time step and set the mass derivative equal to zero.
C
C      Chemical to Water Segment:
C=======================================================================
C
                  DO 1080 ISYS = 1, NOSYS
                     PORMAS = PORVOL*C (ISYS, J)*F (2, ISYS, J)/FRW (J)
                     CD(ISYS,M) =CD(ISYS,M) + PORMAS/DT
                       IF(DSED(ISYS) .GT. 1.5E06)THEN
                         C(ISYS,N) =C(ISYS,N)+CD(ISYS,J)*DT/BVOL(N)
                         CD(ISYS,J)=0.0
                       END IF
C=======================================================================
C        Bury the solids and chemical.  TMASS is the Mass of solids or
C        chemical buried into next Lower Bed while TMPM is the mass buried to
C        the next lower segment.  Note that PORMAS and PORVOL were squeezed
C        to the overlaying water segment.  XBFLUX is the Mass Buried Out of the
C        Network, computed only for the mass balance table.
C=======================================================================
C
                     TMASS = C (ISYS, J)*DELVOL - PORMAS
                     ISEG = J
 1090                CONTINUE
C
                     NSEG = IBOTSG (ISEG)
                     IF (NSEG .EQ. 0) THEN
                        IF (ISYS .EQ. JMASS)
     1                     XBMASS = XBMASS - TMASS/1000.
                     ELSE
                        TMPM = C (ISYS, NSEG)*(DELVOL - PORVOL)
                        C (ISYS, NSEG) = C (ISYS, NSEG) + (TMASS - TMPM)
     1                     /BVOL (NSEG)
                        TMASS = TMPM
                        ISEG = NSEG
                        GO TO 1090
                     END IF
 1080             CONTINUE
C
C----------------------------------------------------------------------
C               Adjust Porosity of Lower Bed Segments
C----------------------------------------------------------------------
C
                  ISEG = J
 1100             CONTINUE
                  NSEG = IBOTSG (ISEG)
                  IF (NSEG .GT. 0) THEN
                     SUM = 0.0
                     DO 1110 I = 1, NOSYS
CCSC
C                        IF (F (2, I, NSEG) .EQ. 0.0) THEN
                        XARG=ABS(F(2,I,NSEG))
                        IF (XARG .LT. R0MIN) THEN
                           SUM = SUM + C (I, NSEG)/DSED (I)
                        END IF
 1110                CONTINUE
C                     FRW (NSEG) = 1.0 - SUM
                     ISEG = NSEG
                     GO TO 1100
                  END IF
               END IF
 1070       CONTINUE
         END IF
      END IF
      RETURN
      END

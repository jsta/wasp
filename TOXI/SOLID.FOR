      SUBROUTINE SOLID
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:25.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
C     A) NO KINETICS FOR SOLIDS SYSTEMS
C
C     B) DEFINE F ARRAY FOR PROPER TRANSPORT
      DO 1010 J = 2, 4
         F (1, J, ISEG) = 1.0
         F (2, J, ISEG) = 0.0
         F (J + 1, J, ISEG) = 1.0
 1010 CONTINUE
C
C     C) KINETIC DERIVATIVE
C
      CD (2, ISEG) = 0.0
      CD (3, ISEG) = 0.0
      CD (4, ISEG) = 0.0
C
      RETURN
      END

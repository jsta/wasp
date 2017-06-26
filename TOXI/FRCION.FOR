      SUBROUTINE FRCION (ICHM, DISTRIB, PIDOC)
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:15.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C  Function:
C                       This routine computes the fraction of total chem
C           dissolved, doc sorbed and solids sorbed molecular and
C           ionic species JS = # of the system for the chemical being
C           considered.
C----------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      INCLUDE 'ENVIRON.CMN'
C
      INCLUDE 'PHYSCHM.CMN'
C
      INCLUDE 'CONC.CMN'
C
C
      DIMENSION FRAC (25), DISTRIB (4), PIDOC (4), DISFCT (25)
C
C     I. COMPUTE DISTRIBUTION FACTORS FOR EACH PHASE AND SPECIE
C         Distribution factor is the ratio of component concentration
C         to dissolved parent compound concentration
C
C          A) SORBED TO SOLIDS:
C
      IF (ICHM .EQ. 1) JS = 1
      IF (ICHM .EQ. 2) JS = 5
      IF (ICHM .EQ. 3) JS = 6
      DO 1000 J = 1, 3
         JJ = 5*(J - 1)
           SLD = C(J+1,ISEG)*1.0E-06
C
C              1) parent compound

         DISFCT (1 + JJ) = PART (J, 1)*SLD
C
C              2) singly charged cation
         DISFCT (2 + JJ) = PART (J, 2)*SLD*DISTRIB (1)
C
C              3) doubly charged cation
         DISFCT (3 + JJ) = PART (J, 3)*SLD*DISTRIB (2)
C
C              4) singly charged anion
         DISFCT (4 + JJ) = PART (J, 4)*SLD*DISTRIB (3)
C
C              5) doubly charged anion
         DISFCT (5 + JJ) = PART (J, 5)*SLD*DISTRIB (4)
C
 1000 CONTINUE
C
C          B) SORBED TO DOC:
C
      DOCKGL = SDOC*1.E-06
C
C              1) parent compound
      DISFCT (16) = KOC*DOCKGL
C
C              2) singly charged cation
      DISFCT (17) = PIDOC (1)*DOCKGL*DISTRIB (1)
C
C              3) doubly charged cation
      DISFCT (18) = PIDOC (2)*DOCKGL*DISTRIB (2)
C
C              4) singly charged anion
      DISFCT (19) = PIDOC (3)*DOCKGL*DISTRIB (3)
C
C              5) doubly charged anion
      DISFCT (20) = PIDOC (4)*DOCKGL*DISTRIB (4)
C
C          C) DISSOLVED:
C
C              1) parent compound
      DISFCT (21) = 1. * PORE
C
C              2) singly charged cation
      DISFCT (22) = DISTRIB (1) * PORE
C
C              3) doubly charged cation
      DISFCT (23) = DISTRIB (2) * PORE
C
C              4) singly charged anion
      DISFCT (24) = DISTRIB (3) * PORE
C
C              5) doubly charged anion
      DISFCT (25) = DISTRIB (4) * PORE
C
C
C     II. COMPUTE FRACTION OF TOTAL CHEMICAL FOR EACH COMPONENT
C
C          A) SUM DISTRIBUTION FACTORS
C
      DENOM = 0.0
      DO 1010 K = 1, 25
C
C             For doc sorbed & dissolved components multiply by
C             porosity to convert water volume to total volume
C
         DENOM = DENOM + DISFCT (K)
 1010 CONTINUE
C
C          B) COMPUTE FRACTIONS
C
      DO 1020 K = 1, 25
         FRAC (K) = DISFCT (K)/DENOM
         IF (FRAC (K) .LT. 1.00E-10) FRAC (K) = 0.0
 1020 CONTINUE
C
C          C) COMPUTE COMPONENT CONCENTRATIONS
C
C              1) solids sorbed components
      k=0
      DO 1030 J = 1, 3
         DO 1040 ION = 1, 5
            K = K + 1
            PARTOX (ICHM, J, ION) = FRAC (K)*C (JS, ISEG)
            IF (PARTOX (ICHM, J, ION) .LT. 1.0E-30)
     1         PARTOX (ICHM, J, ION) = 0.0
 1040    CONTINUE
 1030 CONTINUE
C
C              2) doc sorbed components
      DO 1050 K = 16, 20
         DOCTOX (ICHM, K - 15) = FRAC (K)*C (JS, ISEG)
         IF (DOCTOX (ICHM, K - 15) .LT. 1.0E-30)
     1      DOCTOX (ICHM, K - 15) = 0.0
 1050 CONTINUE
C
C              3) dissolved components
      DO 1060 K = 21, 25
         DISTOX (ICHM, K - 20) = FRAC (K)*C (JS, ISEG)
         IF (DISTOX (ICHM, K - 20) .LT. 1.0E-30)
     1      DISTOX (ICHM, K - 20) = 0.0
 1060 CONTINUE
C
C
C
C          D) EQUATE FRACTIONS TO F ARRAY FOR PROPER TRANSPORT
C              NOTE: The transport (exchange & flow) fields must
C                    be as follows:
C                      field #1 applies to total chemical
C                      field #2 applies to dissolved & doc sorbed chemic
C                      field #3 to field N applies to particulate chemic
C
      F (1, JS, ISEG) = 1.0
      F (2, JS, ISEG) = 0.0
      DO 1070 K = 16, 25
         F (2, JS, ISEG) = F (2, JS, ISEG) + FRAC (K)
 1070 CONTINUE
      K=0
      DO 1080 J = 1, 3
         F (J + 2, JS, ISEG) = 0.0
         DO 1090 ION = 1, 5
            K = K + 1
            F (J + 2, JS, ISEG) = F (J + 2, JS, ISEG) + FRAC (K)
 1090    CONTINUE
 1080 CONTINUE
C
      RETURN
      END

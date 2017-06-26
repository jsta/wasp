      SUBROUTINE TOXIDU
C======================================================================
C  Last Revised:  Date: Thursday, 1 February 1990.  Time: 16:32:24.
C
C  Correction History:
C
C
C
C---------------------------------------------------------------------
C
      INCLUDE 'WASP.CMN'
C
      INCLUDE 'CONC.CMN'
C
      INCLUDE 'KNETIC.CMN'
C
      INCLUDE 'ENVIRON.CMN'
C
      DIMENSION DVAR (60), stype(sg)
      CHARACTER*30 DNAME (42)
      DATA DNAME/'Total Solids   mg/l','Solids Type 1  mg/l',
     *'Solids Type 2  mg/l','Solids Type 3  mg/l',
     *'Seg Temperaturec','Segment Type','Tot. Chemical 1ug/l',
     *'Dissolved 1    ug/l','DOC Sorbed 1   ug/l',
     *'Total Sorbed 1 ug/l','Total Sorbed 1 ug/kg',
     *'Total Ionic 1  ug/l','Biodeg Decay 1 day-1',
     *'Hydro Decay 1  day-1','Photo Decay 1  day-1',
     *'Volat Decay 1  day-1','Oxid Decay 1   day-1',
     *'User Decay 1   day-1','Tot. Chemical 2ug/l',
     *'Dissolved 2    ug/l','DOC Sorbed 2   ug/l',
     *'Total Sorbed 2 ug/l','Total Sorbed 2 ug/kg',
     *'Total Ionic 2  ug/l','Biodeg Decay 2 day-1',
     *'Hydro. Decay 2 day-1','Photolysis 2   day-1',
     *'Volat Decay 2  day-1','Oxid Decay 2   day-1',
     *'User Decay 2   day-1','Tot. Chemical 3ug/l',
     *'Dissolved 3    ug/l','DOC Sorbed 3   ug/l',
     *'Total Sorbed 3 ug/l','Total Sorbed 3 ug/kg',
     *'Total Ionic 3  ug/l','Biodeg Decay 3 day-1',
     *'Hydro Decay 3  day-1','Photo Decay 3  day-1','Volat Decay 3  day-
     *1','Oxid Decay 3   day-1','User Decay 3   day-1'/
C                                     
C
      IF (IFIRST .EQ. 0) THEN
         WRITE (IDMP, 6000) NCHEM
 6000   FORMAT(I5)
         DO 1000 ICHM = 1, NCHEM
            IF (ICHM .EQ. 1) JSYS = 1
            IF (ICHM .EQ. 2) JSYS = 5
            IF (ICHM .EQ. 3) JSYS = 6
            WRITE (IDMP, 6010) CHEML (JSYS), TKOW (ICHM), TMOLWT (ICHM)
 6010       FORMAT(A40,E10.2,F10.2)
 1000    CONTINUE
         WRITE (IDMP, 6020) TIME, TEND, NOSEG
 6020   FORMAT(1X,F10.2,F10.2,/,I5)
C
         WRITE (IDMP, 6030) (DNAME (J), J = 1, 6 + (12*NCHEM))
 6030 FORMAT(A30)
         IFIRST = 1
      END IF
C
C
      TOTSOL = C (2, ISEG) + C (3, ISEG) + C (4, ISEG)
C
      DO 1010 ICHM = 1, NCHEM
         IF (ICHM .EQ. 1) JSYS = 1
         IF (ICHM .EQ. 2) JSYS = 5
         IF (ICHM .EQ. 3) JSYS = 6
         TOTDIS(ICHM)=0.
         TOTDOC(ICHM)=0.
         TOTPAR(ICHM)=0.
         DO 1020 ION = 1, 5
C           Dissolved:
            TOTDIS (ICHM) = TOTDIS (ICHM) + DISTOX (ICHM, ION)
C           DOC sorbed:
            TOTDOC (ICHM) = TOTDOC (ICHM) + DOCTOX (ICHM, ION)
C           Solids sorbed:
            DO 1030 JSORB = 1, 3
C#               IF (C (JSORB + 1, ISEG) .EQ. 0.0) GO TO 1030
               TOTPAR (ICHM) = TOTPAR (ICHM) + PARTOX (ICHM, JSORB, ION)
C#               TOTPAR1 (ICHM) = TOTPAR1 (ICHM) +
C#     1            PARTOX (ICHM, JSORB, ION)*
C#     2            1.E09/C (JSORB + 1, ISEG)
 1030       CONTINUE
 1020    CONTINUE
C
         IF (TOTSOL .GT. 0.)TOTPAR1(ICHM)=TOTPAR(ICHM) * 1.0E06/TOTSOL
C           Ionic:
         DISION (ICHM) = TOTDIS (ICHM) - DISTOX (ICHM, 1)
         DOCION (ICHM) = TOTDOC (ICHM) - DOCTOX (ICHM, 1)
         PARION (ICHM) = TOTPAR (ICHM) - (PARTOX (ICHM, 1, 1) +
     1      PARTOX (ICHM, 2, 1) + PARTOX (ICHM, 3, 1))
         TOTION (ICHM) = DISION (ICHM) + DOCION (ICHM) + PARION (ICHM)
         IF (SDOC .GT. 0.) TOTDOC1 (ICHM) = TOTDOC (ICHM)*1.E06/SDOC
C        Convert chemical concentrations from mg/l to ug/l
         TOTCHEM (ICHM) = C (JSYS, ISEG)*1000.
         TOTDIS (ICHM) = TOTDIS (ICHM)*1000./PORE
         TOTDOC (ICHM) = TOTDOC (ICHM)*1000./PORE
         TOTPAR (ICHM) = TOTPAR (ICHM)*1000.
         TOTPAR1(ICHM) = TOTPAR1(ICHM)*1000.
         TOTION (ICHM) = TOTION (ICHM)*1000.
C
 1010 CONTINUE
      DVAR (1) = TOTSOL
      DVAR (2) = C (2, ISEG)
      DVAR (3) = C (3, ISEG)
      DVAR (4) = C (4, ISEG)
      DVAR (5) = 0.00
      DVAR (6) = 0.00
      DO 1040 K = 1, NCHEM
         DVAR (7 + (K - 1)*12) = TOTCHEM (K)
         DVAR (8 + (K - 1)*12) = TOTDIS (K)
         DVAR (9 + (K - 1)*12) = TOTDOC (K)
         DVAR (10 + (K - 1)*12) = TOTPAR (K)
         DVAR (11 + (K - 1)*12) = TOTPAR1 (K)
         DVAR (12 + (K - 1)*12) = TOTION (K)
         DVAR (13 + (K - 1)*12) = RATE (K, 1)
         DVAR (14 + (K - 1)*12) = RATE (K, 2)
         DVAR (15 + (K - 1)*12) = RATE (K, 3)
         DVAR (16 + (K - 1)*12) = RATE (K, 4)
         DVAR (17 + (K - 1)*12) = RATE (K, 5)
         DVAR (18 + (K - 1)*12) = RATE (K, 6)
 1040 CONTINUE
      STYPE(ISEG)=ITYPE(ISEG)
      WRITE (IDMP, 6040) ISEG, TIME, DVAR (1), DVAR
     1   (2), DVAR (3), DVAR (4),
     2   TMP, STYPE (ISEG), (DVAR (J), J = 7, 6 + (12*NCHEM))
C 6040 FORMAT(1X,I5,F10.2,3X,/,6(E11.3),/,6(E11.3))
 6040 FORMAT(1X,I5,F10.4,3X,/,6(E11.3),/,6(E11.3))
      DO 1050 L = 1, NCHEM
         TOTDIS (L) = 0.00
         TOTDOC (L) = 0.00
         TOTPAR (L) = 0.00
         TOTPAR1 (L) = 0.00
 1050 CONTINUE
      if (iqopt .ge. 3) call rscreen
      call keycheck
      RETURN
      END

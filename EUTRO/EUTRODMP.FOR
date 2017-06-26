      SUBROUTINE EUTRODMP
C
C     Last Revised:  Date: Monday, 16 August 1993.  Time: 14:15:54.
C
C*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*X*
C
C
      INCLUDE 'WASP.CMN'
      INCLUDE 'EUTRO.CMN'
C
      DIMENSION STYPE (SG)
      CHARACTER*25 DNAME (42)
      DATA DNAME/
     *'Segment Depth  m',         'Temperature    C',
     *'Wind Speed     m/sec',     'Water Velocity m/sec',
     *'DO             mg/L',      'DO Minimum     mg/L',
     *'DO Maximum     mg/L',      'DO Saturation  mg/L',
     *'Percent DO Sat %',         'Effective KA   day-1',
     *'Wind KA        day-1',     'Hydraulic KA   day-1',
     *'Sed. O2 Demand g/m2/day',  'CBOD           mg/L',
     *'BOD5           mg/L',      'Ultimate BOD   mg/L',
     *'BOD Decay Rate day-1',     'Phytoplankton Cmg/L',
     *'Total Chloro a ug/L',      'Phyt Growth Ratday-1',
     *'Phyt Death Rateday-1',     'Phyt DO Prod.  mg/L/day',
     *'Phyt DO Consumpmg/L/day',  'Carbon::Chl-a  mg/mg',
     *'Phyt Light Lim. ',         'Phyt Nutr Limit ',
     *'Nitrogen Limit  ',         'Phosphors Limit ',
     *'Tot Daily Lightlang./day', 'Sat. Light Int.lang./day',
     *'Light Top Seg  lang./day', 'Light Bot Seg  lang./day',
     *'Ammonia N      mg/L',      'Nitrate N      mg/L',
     *'NH3 + NO3 N    mg/L',      'Tot Org N      mg/L',
     *'Total Inorg N  mg/L',      'Total Nitrogen mg/L',
     *'Avail Inorg P  mg/L',      'Tot Inorganic Pmg/L',
     *'Tot Organic P  mg/L',      'NonLiving Org Pmg/L'/
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C                   Initialize the Dump File
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
      IF (IFIRST .EQ. 0) THEN
         NCHEM = 3
         WRITE (IDMP, 6000) NCHEM
 6000    FORMAT (I5)
         WRITE (IDMP, 6010)
 6010    FORMAT(2(/))
         WRITE (IDMP, 6020) TIME, TEND, NOSEG
 6020    FORMAT (1X, F10.2,F10.2, /, I5)
         WRITE (IDMP, 6030) (DNAME (J), J = 1, 6 + (12*NCHEM))
 6030    FORMAT (A25)
         IF(TFLAG .EQ. 0)CALL TRNOUT
         IFIRST = 1
         RETURN
      END IF
C
C        Compute contributions to total nutrients from algal cells
C
      IF (IQOPT .GE. 3)CALL RSCREEN
      TOP = FOP*PCRB*PHYT + OP
      TIP = (1.0 - FOP)*PCRB*PHYT + OPO4
      TON = FON*NCRB*PHYT + ON
      TIN = (1.0 - FON)*NCRB*PHYT + NH3 + NO3
      TN = TIN + TON
      TP = TIP + TOP
C
C        Convert Chla to ug/L for output
C
      TCHLAX = 1000.*TCHLA
C
C        Compute daily D.O. swing and other variables
C
      DELO2 = (0.67 - 0.736*(FDAY - 0.33))*GPP*OCRB
      DOMAX = DO + DELO2/2.
      DOMIN = DO - DELO2/2.
      DODEF = CS - DO
      PERSAT = (DO/CS)*100.
C      
C        Compute BOD variables
C
      UBOD = CBOD + 4.57 * NH3 
     1   +   4.57 * ON * (1. - EXP ( - 30.*K1013C))
     2   +   OCRB * PHYT * (1. - EXP ( - 30.*DP1))
      BOD5 = CBOD * (1. - EXP ( - 5.*KDC))  
     1   +   4.57 * NH3 * (1.- EXP ( - 5.*K1320C))
     2   +   OCRB * PHYT * (1. - EXP ( - 5.*DP1))
      SOD = SOD1D (ISEG) * SODTA(ISEG)**STP20
C
C        Dump Time
C
      IF (ISEG .LE. 1) PTIME = 1.0001*TIME
C
      STYPE (ISEG) = ITYPE (ISEG)
C      
      WRITE (IDMP, 6040) ISEG, TIME, DEPTHG (ISEG),
     1   STP, WIND, VEL, DO,DOMIN,DOMAX,CS,PERSAT,
     2   KA,K2WSAVE,K2HSAVE,SOD1D(ISEG),CBOD,BOD5,UBOD,TEMPBOD,PHYT,
     3   TCHLAX,GP1,DP1,SR19P,SK19P,CCHL1,RLGHTS (ISEG, 1),
     4   RNUTR,XEMP1,XEMP2,ITOTMP,IS1,IAV,IAVBOT,NH3,NO3,
     5   CN,TON,TIN,TN,OPO4,TIP,TOP,OP
 6040 FORMAT(1X,I5,F12.4,3X,/,6(E11.3),/,6(E11.3))
C      
      CALL KEYCHECK
      RETURN
      END

Sat 04/22/2017 
10:21 AM
;Model Desc: Interoccasion Variability
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB run# example7r
$INPUT C SET ID  TIME  AMT RATE EVID MDV CMT DV OCC
$ABBR REPLACE ETA(OCC_CL)=ETA(3,4,5)
$DATA example7r.csv IGNORE=C

$SUBROUTINES ADVAN1 TRANS2

$PK
MU_1=THETA(1)
MU_2=THETA(2)
V=DEXP(MU_1+ETA(1))
S1=V
VC=V
CL=DEXP(MU_2+ETA(2))*EXP(ETA(OCC_CL))

$ERROR
IPRED=F
Y = F+F*EPS(1)

;Initial Thetas
$THETA
 2.0  ;[MU_1]
 2.0  ;[MU_2]

;Initial omegas
$OMEGA BLOCK(2)
 .3 ;[p]
 -.01  ;[f]
 .3 ;[p]
$OMEGA BLOCK(1)
 .1  ;[p]
$OMEGA BLOCK(1) SAME(2)

$SIGMA
 0.1 ;[p]

$PRIOR NWPRI
; Degrees of freedom for Prior Omega blocks
$OMEGAPD (2.0 FIXED) (1.0 FIXED)
; Prior Omegas
$OMEGAP BLOCK(2)
 .14 FIX
 0.0 .125
$OMEGAP BLOCK(1) .0164 FIX
$OMEGAP BLOCK(1) SAME(2)

$EST METHOD=ITS INTERACTION FILE=example7r.ext   NITER=10000 
     PRINT=5 NOABORT SIGL=8 CTYPE=3 CITER=10
     NOPRIOR=1 CALPHA=0.05 NSIG=2

$EST METHOD=SAEM INTERACTION NBURN=30000 NITER=500 SIGL=8 
     ISAMPLE=2 PRINT=10 SEED=1556678 CTYPE=3
     CITER=10 CALPHA=0.05 NOPRIOR=1

$EST METHOD=IMP  INTERACTION EONLY=1 NITER=4 ISAMPLE=3000 
     PRINT=1 SIGL=10 NOPRIOR=1 MAPITER=0

$EST METHOD=BAYES INTERACTION FILE=example7r.txt NBURN=10000 
     NITER=10000 PRINT=100 CTYPE=3 CITER=10 
     CALPHA=0.05 NOPRIOR=0

$EST METHOD=COND INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 PRINT=5 
     NOABORT NOPRIOR=1
     FILE=example7r.ext

$COV MATRIX=R PRINT=E UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  3) THERE MAY BE AN ERROR IN THE ABBREVIATED CODE. THE FOLLOWING
 ONE OR MORE RANDOM VARIABLES ARE DEFINED WITH "IF" STATEMENTS THAT DO NOT
 PROVIDE DEFINITIONS FOR BOTH THE "THEN" AND "ELSE" CASES. IF ALL
 CONDITIONS FAIL, THE VALUES OF THESE VARIABLES WILL BE ZERO.
  
   ETA(OCC_CL)

  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       22 APR 2017
Days until program expires :4785
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 beta 2 (nm74b2)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 run# example7r
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     4500
 NO. OF DATA ITEMS IN DATA SET:  11
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:  10
 MDV DATA ITEM IS DATA ITEM NO.:  8
0INDICES PASSED TO SUBROUTINE PRED:
   7   4   5   6   0   0   9   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID TIME AMT RATE EVID MDV CMT DV OCC
0FORMAT FOR DATA:
 (E2.0,E3.0,E4.0,E5.0,E4.0,4E2.0,E11.0,E2.0)

 TOT. NO. OF OBS RECS:     3750
 TOT. NO. OF INDIVIDUALS:      250
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  0  0  2
  0  0  0  2
  0  0  0  0  2
  0  0  0  0  0  3
  0  0  0  0  0  3  3
  0  0  0  0  0  0  0  4
  0  0  0  0  0  0  0  0  4
  0  0  0  0  0  0  0  0  0  4
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.2000E+01     0.1000E+07
 -0.1000E+07     0.2000E+01     0.1000E+07
  0.2000E+01     0.2000E+01     0.2000E+01
  0.1000E+01     0.1000E+01     0.1000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.3000E+00
                 -0.1000E-01   0.3000E+00
        2                                                                                   NO
                  0.1000E+00
        3                                                                                  YES
                  0.1400E+00
                  0.0000E+00   0.1250E+00
        4                                                                                  YES
                  0.1640E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
 GRADIENT METHOD USED:     NOSLOW
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 TURN OFF Cholesky Transposition of R Matrix (CHOLROFF): NO
 KNUTHSUMOFF:                                -1
 RESUME COV ANALYSIS (RESUME):               NO
 SIR SAMPLE SIZE (SIRSAMPLE):              -1
 NON-LINEARLY TRANSFORM THETAS DURING COV (THBND): 1
 PRECONDTIONING CYCLES (PRECOND):        0
 PRECONDTIONING TYPES (PRECONDS):        TOS
 FORCED PRECONDTIONING CYCLES (PFCOND):0
 PRECONDTIONING TYPE (PRETYPE):        0
 FORCED POS. DEFINITE SETTING: (FPOSDEF):0
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 beta 2 (nm74b2)

 ONE COMPARTMENT MODEL (ADVAN1)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   2
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1

 TRANSLATOR WILL CONVERT PARAMETERS
 CLEARANCE (CL) AND VOLUME (V) TO K (TRANS2)
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         NO         YES        YES        YES
    2         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            3           *           *           *           *
    2            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      7
   TIME DATA ITEM IS DATA ITEM NO.:          4
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   5
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     6
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    9

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 
 
 #TBLN:      1
 #METH: Iterative Two Stage (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            1224
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      8
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     8
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example7r.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                ITERATIVE TWO STAGE (ITS)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          5
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 ITERATIONS (NITER):                        10000
 ANEAL SETTING (CONSTRAIN):                 1

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -5991.99597493248
 iteration            5 OBJ=  -14742.8328223556
 iteration           10 OBJ=  -17336.5624279366
 iteration           15 OBJ=  -19538.9622853427
 iteration           20 OBJ=  -19599.6299717943
 iteration           25 OBJ=  -19599.6298731143
 iteration           30 OBJ=  -19599.6298724690
 iteration           35 OBJ=  -19599.6298725053
 iteration           40 OBJ=  -19599.6298724719
 iteration           45 OBJ=  -19599.6298724781
 iteration           50 OBJ=  -19599.6298724991
 iteration           55 OBJ=  -19599.6298724821
 Convergence achieved
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.0237E-10 -1.5354E-10 -6.6951E-03  2.6073E-03  4.0878E-03
 SE:             2.3552E-02  2.1839E-02  6.6358E-03  6.6176E-03  6.8553E-03
 N:                     250         250         250         250         250
 
 P VAL.:         1.0000E+00  1.0000E+00  3.1301E-01  6.9359E-01  5.5098E-01
 
 ETASHRINKSD(%)  1.3846E-01  2.1757E+00  1.8793E+01  1.9015E+01  1.6106E+01
 ETASHRINKVR(%)  2.7672E-01  4.3041E+00  3.4054E+01  3.4415E+01  2.9619E+01
 EBVSHRINKSD(%)  1.3845E-01  2.1757E+00  1.7839E+01  1.7936E+01  1.7858E+01
 EBVSHRINKVR(%)  2.7671E-01  4.3041E+00  3.2496E+01  3.2655E+01  3.2526E+01
 EPSSHRINKSD(%)  1.4063E+01
 EPSSHRINKVR(%)  2.6148E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         3750
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    6892.03899903504     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -19599.6298724821     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -12707.5908734470     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1250
  
 #TERE:
 Elapsed estimation  time in seconds:    10.80
 Elapsed covariance  time in seconds:     0.55
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -19599.630       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.68E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.40E-01
 
 ETA2
+       -8.07E-02  1.25E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.68E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.68E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.68E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.50E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        3.74E-01
 
 ETA2
+       -6.11E-01  3.54E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.29E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.29E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.29E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.40E-02  2.33E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.26E-02
 
 ETA2
+        1.11E-02  1.31E-02
 
 ETA3
+        0.00E+00  0.00E+00  1.14E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.14E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.14E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        6.46E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.69E-02
 
 ETA2
+        4.73E-02  1.86E-02
 
 ETA3
+       ......... .........  4.42E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        6.46E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.75E-04
 
 TH 2
+       -3.35E-04  5.43E-04
 
 OM11
+        2.41E-05 -6.18E-06  1.59E-04
 
 OM12
+       -9.86E-07 -4.25E-06 -1.03E-04  1.23E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -8.12E-07 -8.05E-06  7.46E-05 -1.08E-04  0.00E+00  0.00E+00  0.00E+00  1.72E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        2.87E-06 -4.34E-06  1.41E-06 -6.91E-07  0.00E+00  0.00E+00  0.00E+00  1.24E-06  0.00E+00  0.00E+00  0.00E+00  1.31E-06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -1.60E-08  2.56E-08  1.90E-08 -1.93E-08  0.00E+00  0.00E+00  0.00E+00 -2.56E-08  0.00E+00  0.00E+00  0.00E+00 -3.80E-09
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.17E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.40E-02
 
 TH 2
+       -6.00E-01  2.33E-02
 
 OM11
+        7.96E-02 -2.10E-02  1.26E-02
 
 OM12
+       -3.70E-03 -1.64E-02 -7.37E-01  1.11E-02
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.58E-03 -2.63E-02  4.50E-01 -7.39E-01  0.00E+00  0.00E+00  0.00E+00  1.31E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.04E-01 -1.63E-01  9.74E-02 -5.44E-02  0.00E+00  0.00E+00  0.00E+00  8.23E-02  0.00E+00  0.00E+00  0.00E+00  1.14E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -1.03E-02  1.70E-02  2.33E-02 -2.69E-02  0.00E+00  0.00E+00  0.00E+00 -3.02E-02  0.00E+00  0.00E+00  0.00E+00 -5.13E-02
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.46E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.75E+03
 
 TH 2
+        1.69E+03  2.95E+03
 
 OM11
+       -6.12E+02 -1.28E+02  1.47E+04
 
 OM12
+       -2.64E+02  3.95E+02  1.49E+04  3.32E+04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.93E+02  4.05E+02  3.01E+03  1.44E+04  0.00E+00  0.00E+00  0.00E+00  1.36E+04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -2.25E+01  2.00E+03 -3.28E+03 -3.25E+03  0.00E+00  0.00E+00  0.00E+00 -2.40E+03  0.00E+00  0.00E+00  0.00E+00  8.86E+04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        2.81E+03 -1.24E+03  1.02E+04  1.62E+05  0.00E+00  0.00E+00  0.00E+00  1.28E+05  0.00E+00  0.00E+00  0.00E+00  2.14E+05
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.42E+08
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.54E-01  3.84E-01  5.57E-01  9.15E-01  1.03E+00  1.65E+00  2.31E+00
 
1
 
 
 #TBLN:      2
 #METH: Stochastic Approximation Expectation-Maximization (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            1224
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      8
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     8
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example7r.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                STOCHASTIC APPROXIMATION EXPECTATION MAXIMIZATION (SAEM)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          10
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 BURN-IN ITERATIONS (NBURN):                30000
 ITERATIONS (NITER):                        500
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       1556678
 MC SAMPLES PER SUBJECT (ISAMPLE):          2
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 EXPECTATION ONLY (EONLY):                  0
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0
 SAMPLES FOR MASS/IMP/POST. MATRIX SEARCH (ISAMPLE_M1B): 2
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2
 PWR. WT. MASS/IMP/POST MATRIX ACCUM. FOR ETAS (IKAPPA): 1.00000000000000
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration       -30000 SAEMOBJ=  -29264.6112122335
 iteration       -29990 SAEMOBJ=  -28923.4946309655
 iteration       -29980 SAEMOBJ=  -28828.2797976965
 iteration       -29970 SAEMOBJ=  -28824.0348476018
 iteration       -29960 SAEMOBJ=  -28729.8528482576
 iteration       -29950 SAEMOBJ=  -28718.4598169028
 iteration       -29940 SAEMOBJ=  -28721.9008230306
 iteration       -29930 SAEMOBJ=  -28686.2234736700
 iteration       -29920 SAEMOBJ=  -28697.1943412565
 iteration       -29910 SAEMOBJ=  -28650.5651525371
 iteration       -29900 SAEMOBJ=  -28608.7974680073
 iteration       -29890 SAEMOBJ=  -28671.8519131938
 iteration       -29880 SAEMOBJ=  -28598.6660627475
 iteration       -29870 SAEMOBJ=  -28617.2879509446
 iteration       -29860 SAEMOBJ=  -28645.1250588058
 iteration       -29850 SAEMOBJ=  -28599.6128626962
 iteration       -29840 SAEMOBJ=  -28594.4226668342
 iteration       -29830 SAEMOBJ=  -28591.2506429709
 iteration       -29820 SAEMOBJ=  -28586.5705837910
 iteration       -29810 SAEMOBJ=  -28554.3353283839
 iteration       -29800 SAEMOBJ=  -28560.4780937060
 iteration       -29790 SAEMOBJ=  -28578.4672566683
 iteration       -29780 SAEMOBJ=  -28593.3888013621
 iteration       -29770 SAEMOBJ=  -28554.7017697135
 iteration       -29760 SAEMOBJ=  -28619.8285744068
 iteration       -29750 SAEMOBJ=  -28579.9088328346
 iteration       -29740 SAEMOBJ=  -28578.6140355160
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -28567.7001531093
 iteration           10 SAEMOBJ=  -28685.0231574882
 iteration           20 SAEMOBJ=  -28690.4210514432
 iteration           30 SAEMOBJ=  -28691.7588030626
 iteration           40 SAEMOBJ=  -28692.2186750902
 iteration           50 SAEMOBJ=  -28694.6370804773
 iteration           60 SAEMOBJ=  -28698.4149782867
 iteration           70 SAEMOBJ=  -28698.7603659230
 iteration           80 SAEMOBJ=  -28698.1922874752
 iteration           90 SAEMOBJ=  -28698.8927221324
 iteration          100 SAEMOBJ=  -28698.1539745996
 iteration          110 SAEMOBJ=  -28698.2803869877
 iteration          120 SAEMOBJ=  -28697.5041859077
 iteration          130 SAEMOBJ=  -28697.3174198367
 iteration          140 SAEMOBJ=  -28696.9868018647
 iteration          150 SAEMOBJ=  -28695.7805222056
 iteration          160 SAEMOBJ=  -28695.1464511287
 iteration          170 SAEMOBJ=  -28694.4672219041
 iteration          180 SAEMOBJ=  -28693.9629132220
 iteration          190 SAEMOBJ=  -28693.6138024196
 iteration          200 SAEMOBJ=  -28692.5455670260
 iteration          210 SAEMOBJ=  -28691.3638916543
 iteration          220 SAEMOBJ=  -28691.0973874002
 iteration          230 SAEMOBJ=  -28690.9954239157
 iteration          240 SAEMOBJ=  -28689.9932166937
 iteration          250 SAEMOBJ=  -28689.5925242190
 iteration          260 SAEMOBJ=  -28689.4464940135
 iteration          270 SAEMOBJ=  -28688.7214343426
 iteration          280 SAEMOBJ=  -28688.7476686181
 iteration          290 SAEMOBJ=  -28688.7265644357
 iteration          300 SAEMOBJ=  -28688.6338153498
 iteration          310 SAEMOBJ=  -28687.8702478927
 iteration          320 SAEMOBJ=  -28687.8667710374
 iteration          330 SAEMOBJ=  -28687.5636967993
 iteration          340 SAEMOBJ=  -28687.6343445053
 iteration          350 SAEMOBJ=  -28687.4003239745
 iteration          360 SAEMOBJ=  -28687.2606040379
 iteration          370 SAEMOBJ=  -28687.0771231605
 iteration          380 SAEMOBJ=  -28686.7884690193
 iteration          390 SAEMOBJ=  -28686.2875406863
 iteration          400 SAEMOBJ=  -28685.7997608970
 iteration          410 SAEMOBJ=  -28685.6092789481
 iteration          420 SAEMOBJ=  -28685.2394001883
 iteration          430 SAEMOBJ=  -28684.5644763938
 iteration          440 SAEMOBJ=  -28684.2176271923
 iteration          450 SAEMOBJ=  -28684.3015123890
 iteration          460 SAEMOBJ=  -28683.9360015494
 iteration          470 SAEMOBJ=  -28683.7327117954
 iteration          480 SAEMOBJ=  -28683.3290857807
 iteration          490 SAEMOBJ=  -28683.0846705736
 iteration          500 SAEMOBJ=  -28682.9831498863
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.2334E-06 -6.5093E-06 -6.9733E-03  2.6271E-03  4.1369E-03
 SE:             2.3574E-02  2.1955E-02  6.6241E-03  6.6095E-03  6.8601E-03
 N:                     250         250         250         250         250
 
 P VAL.:         9.9996E-01  9.9976E-01  2.9247E-01  6.9102E-01  5.4649E-01
 
 ETASHRINKSD(%)  1.3632E-01  1.8226E+00  1.6773E+01  1.6957E+01  1.3808E+01
 ETASHRINKVR(%)  2.7245E-01  3.6120E+00  3.0733E+01  3.1038E+01  2.5709E+01
 EBVSHRINKSD(%)  1.3618E-01  1.8229E+00  1.5699E+01  1.5820E+01  1.5718E+01
 EBVSHRINKVR(%)  2.7217E-01  3.6126E+00  2.8933E+01  2.9137E+01  2.8965E+01
 EPSSHRINKSD(%)  1.4000E+01
 EPSSHRINKVR(%)  2.6040E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         3750
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    6892.03899903504     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -28682.9831498863     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -21790.9441508512     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1250
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2297.34633301168     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -28682.9831498863     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -26385.6368168746     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    91.57
 Elapsed covariance  time in seconds:     0.37
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -28682.983       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.68E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.40E-01
 
 ETA2
+       -8.13E-02  1.26E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.59E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.59E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.59E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.50E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        3.74E-01
 
 ETA2
+       -6.14E-01  3.54E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.26E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.26E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.26E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.39E-02  2.31E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.27E-02
 
 ETA2
+        1.11E-02  1.31E-02
 
 ETA3
+        0.00E+00  0.00E+00  1.03E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.03E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.03E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        6.42E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.70E-02
 
 ETA2
+        4.69E-02  1.85E-02
 
 ETA3
+       ......... .........  4.10E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        6.42E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.72E-04
 
 TH 2
+       -3.33E-04  5.34E-04
 
 OM11
+        2.30E-05 -5.60E-06  1.61E-04
 
 OM12
+       -7.00E-07 -4.11E-06 -1.05E-04  1.24E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.29E-06 -6.72E-06  7.66E-05 -1.09E-04  0.00E+00  0.00E+00  0.00E+00  1.73E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.67E-06 -2.99E-06  1.59E-06 -9.73E-07  0.00E+00  0.00E+00  0.00E+00  1.49E-06  0.00E+00  0.00E+00  0.00E+00  1.07E-06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -1.86E-08  2.74E-08  1.95E-08 -1.97E-08  0.00E+00  0.00E+00  0.00E+00 -2.41E-08  0.00E+00  0.00E+00  0.00E+00 -3.55E-09
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.12E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.39E-02
 
 TH 2
+       -6.02E-01  2.31E-02
 
 OM11
+        7.59E-02 -1.91E-02  1.27E-02
 
 OM12
+       -2.63E-03 -1.60E-02 -7.43E-01  1.11E-02
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -7.29E-03 -2.21E-02  4.60E-01 -7.45E-01  0.00E+00  0.00E+00  0.00E+00  1.31E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        6.74E-02 -1.25E-01  1.21E-01 -8.45E-02  0.00E+00  0.00E+00  0.00E+00  1.10E-01  0.00E+00  0.00E+00  0.00E+00  1.03E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -1.21E-02  1.85E-02  2.40E-02 -2.75E-02  0.00E+00  0.00E+00  0.00E+00 -2.85E-02  0.00E+00  0.00E+00  0.00E+00 -5.36E-02
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.42E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.77E+03
 
 TH 2
+        1.73E+03  2.98E+03
 
 OM11
+       -6.11E+02 -1.36E+02  1.48E+04
 
 OM12
+       -2.54E+02  3.80E+02  1.52E+04  3.38E+04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.08E+02  3.90E+02  3.08E+03  1.47E+04  0.00E+00  0.00E+00  0.00E+00  1.38E+04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.00E+02  1.88E+03 -3.98E+03 -3.44E+03  0.00E+00  0.00E+00  0.00E+00 -3.11E+03  0.00E+00  0.00E+00  0.00E+00  1.08E+05
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        4.70E+03 -2.46E+03  7.84E+03  1.63E+05  0.00E+00  0.00E+00  0.00E+00  1.27E+05  0.00E+00  0.00E+00  0.00E+00  2.54E+05
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.45E+08
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.50E-01  3.83E-01  5.48E-01  9.22E-01  1.03E+00  1.63E+00  2.34E+00
 
1
 
 
 #TBLN:      3
 #METH: Objective Function Evaluation by Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            1224
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      10
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     10
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example7r.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          1
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 ITERATIONS (NITER):                        4
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       1556678
 MC SAMPLES PER SUBJECT (ISAMPLE):          3000
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 EXPECTATION ONLY (EONLY):                  1
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     0.100000000000000       ,10.0000000000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 LONG TAIL SAMPLE ACCEPT. RATE (IACCEPTL):   0.00000000000000
 T-DIST. PROPOSAL DENSITY (DF):             0
 NO. ITERATIONS FOR MAP (MAPITER):          0
 INTERVAL ITER. FOR MAP (MAPINTER):         0
 MAP COVARIANCE/MODE SETTING (MAPCOV):      1
 Gradient Quick Value (GRDQ):               0.00000000000000

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -19600.3722764073 eff.=    3354. Smpl.=    3000. Fit.= 0.98085
 iteration            1 OBJ=  -19600.4217773312 eff.=    1102. Smpl.=    3000. Fit.= 0.92488
 iteration            2 OBJ=  -19600.4567322614 eff.=    1190. Smpl.=    3000. Fit.= 0.93000
 iteration            3 OBJ=  -19600.5166626185 eff.=    1202. Smpl.=    3000. Fit.= 0.93053
 iteration            4 OBJ=  -19601.0924668718 eff.=    1203. Smpl.=    3000. Fit.= 0.93045
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         5.5626E-06 -8.8798E-05 -6.9333E-03  2.7318E-03  4.1650E-03
 SE:             2.3578E-02  2.1932E-02  6.6311E-03  6.6165E-03  6.8461E-03
 N:                     250         250         250         250         250
 
 P VAL.:         9.9981E-01  9.9677E-01  2.9576E-01  6.7970E-01  5.4294E-01
 
 ETASHRINKSD(%)  1.1638E-01  1.9243E+00  1.6686E+01  1.6869E+01  1.3984E+01
 ETASHRINKVR(%)  2.3262E-01  3.8115E+00  3.0587E+01  3.0892E+01  2.6012E+01
 EBVSHRINKSD(%)  1.3821E-01  2.0686E+00  1.7949E+01  1.8055E+01  1.7996E+01
 EBVSHRINKVR(%)  2.7623E-01  4.0944E+00  3.2676E+01  3.2849E+01  3.2753E+01
 EPSSHRINKSD(%)  1.4014E+01
 EPSSHRINKVR(%)  2.6065E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         3750
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    6892.03899903504     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -19601.0924668718     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -12709.0534678367     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1250
  
 #TERE:
 Elapsed estimation  time in seconds:    34.57
 Elapsed covariance  time in seconds:     9.95
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -19601.092       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.68E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.40E-01
 
 ETA2
+       -8.13E-02  1.26E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.59E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.59E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.59E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.50E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        3.74E-01
 
 ETA2
+       -6.14E-01  3.54E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.26E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.26E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.26E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.37E-02  2.29E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.27E-02
 
 ETA2
+        1.00E-02  1.17E-02
 
 ETA3
+        0.00E+00  0.00E+00  9.75E-04
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  9.75E-04
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.75E-04
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        6.72E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.70E-02
 
 ETA2
+        4.10E-02  1.65E-02
 
 ETA3
+       ......... .........  3.87E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        6.73E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.61E-04
 
 TH 2
+       -3.24E-04  5.24E-04
 
 OM11
+        2.71E-07 -2.64E-07  1.61E-04
 
 OM12
+       -1.71E-07  8.14E-08 -9.27E-05  1.01E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.59E-07  9.34E-08  5.36E-05 -8.51E-05  0.00E+00  0.00E+00  0.00E+00  1.36E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.96E-08 -2.59E-08  3.17E-07 -2.78E-07  0.00E+00  0.00E+00  0.00E+00 -3.25E-08  0.00E+00  0.00E+00  0.00E+00  9.50E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        4.63E-09  3.34E-09  3.32E-09 -3.02E-09  0.00E+00  0.00E+00  0.00E+00  3.63E-09  0.00E+00  0.00E+00  0.00E+00  2.23E-10
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.52E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.37E-02
 
 TH 2
+       -5.98E-01  2.29E-02
 
 OM11
+        9.04E-04 -9.11E-04  1.27E-02
 
 OM12
+       -7.18E-04  3.54E-04 -7.29E-01  1.00E-02
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.73E-04  3.49E-04  3.62E-01 -7.27E-01  0.00E+00  0.00E+00  0.00E+00  1.17E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.72E-03 -1.16E-03  2.57E-02 -2.85E-02  0.00E+00  0.00E+00  0.00E+00 -2.86E-03  0.00E+00  0.00E+00  0.00E+00  9.75E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        2.91E-03  2.17E-03  3.90E-03 -4.48E-03  0.00E+00  0.00E+00  0.00E+00  4.62E-03  0.00E+00  0.00E+00  0.00E+00  3.40E-03
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.72E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.78E+03
 
 TH 2
+        1.72E+03  2.97E+03
 
 OM11
+       -1.72E+00  2.67E+00  1.52E+04
 
 OM12
+       -3.50E+00 -2.82E+00  1.90E+04  4.49E+04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -5.82E+00 -6.73E+00  5.88E+03  2.05E+04  0.00E+00  0.00E+00  0.00E+00  1.78E+04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -2.29E+01  2.77E+00  2.28E+02  2.49E+03  0.00E+00  0.00E+00  0.00E+00  1.55E+03  0.00E+00  0.00E+00  0.00E+00  1.17E+05
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -4.11E+03 -3.96E+03 -3.25E+03 -8.62E+02  0.00E+00  0.00E+00  0.00E+00 -5.15E+03  0.00E+00  0.00E+00  0.00E+00 -1.70E+04
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.21E+08
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.36E-01  4.02E-01  6.37E-01  9.97E-01  1.00E+00  1.60E+00  2.23E+00
 
1
 
 
 #TBLN:      4
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            1224
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      10
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     10
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example7r.txt
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                MCMC BAYESIAN (BAYES)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 KEEP ITERATIONS (THIN):            1
 CONVERGENCE INTERVAL (CINTERVAL):          100
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 BURN-IN ITERATIONS (NBURN):                10000
 ITERATIONS (NITER):                        10000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       1556678
 MC SAMPLES PER SUBJECT (ISAMPLE):          1
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0
 SAMPLES FOR MASS/IMP/POST. MATRIX SEARCH (ISAMPLE_M1B): 2
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2
 PWR. WT. MASS/IMP/POST MATRIX ACCUM. FOR ETAS (IKAPPA): 1.00000000000000
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS
 SAMPLED THETAS AND SIGMAS:
 PROPOSAL DENSITY SCALING RANGE
              (PSCALE_MIN, PSCALE_MAX):   1.000000000000000E-02   ,1000.00000000000
 SAMPLE ACCEPTANCE RATE (PACCEPT):                       0.500000000000000
 SAMPLES FOR GLOBAL SEARCH KERNEL (PSAMPLE_M1):          1
 SAMPLES FOR LOCAL SEARCH KERNEL (PSAMPLE_M2):           -1
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (PSAMPLE_M3):       1
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS
 SAMPLED OMEGAS:
 SAMPLE ACCEPTANCE RATE (OACCEPT):                       0.500000000000000
 SAMPLES FOR GLOBAL SEARCH KERNEL (OSAMPLE_M1):          -1
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           6
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):6
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2
 THETAS THAT ARE GIBBS SAMPLED:
   1   2
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration       -10000 MCMCOBJ=   -28384.1233322318     
 iteration        -9900 MCMCOBJ=   -28205.6314867147     
 iteration        -9800 MCMCOBJ=   -28151.5384738058     
 iteration        -9700 MCMCOBJ=   -28295.0751896391     
 iteration        -9600 MCMCOBJ=   -28160.0898649784     
 iteration        -9500 MCMCOBJ=   -28189.6073977108     
 iteration        -9400 MCMCOBJ=   -28211.7227949386     
 iteration        -9300 MCMCOBJ=   -28268.2365358603     
 iteration        -9200 MCMCOBJ=   -28321.7343110547     
 iteration        -9100 MCMCOBJ=   -28215.0849213444     
 iteration        -9000 MCMCOBJ=   -28274.7646583437     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -28318.6103999121     
 iteration          100 MCMCOBJ=   -28237.4436805277     
 iteration          200 MCMCOBJ=   -28309.1402379001     
 iteration          300 MCMCOBJ=   -28148.1886896230     
 iteration          400 MCMCOBJ=   -28238.0132263463     
 iteration          500 MCMCOBJ=   -28208.6561250086     
 iteration          600 MCMCOBJ=   -28153.7500935494     
 iteration          700 MCMCOBJ=   -28244.2285939775     
 iteration          800 MCMCOBJ=   -28246.4600696468     
 iteration          900 MCMCOBJ=   -28121.2337116148     
 iteration         1000 MCMCOBJ=   -28305.2831558175     
 iteration         1100 MCMCOBJ=   -28230.7593048979     
 iteration         1200 MCMCOBJ=   -28250.9634649923     
 iteration         1300 MCMCOBJ=   -28247.0961256874     
 iteration         1400 MCMCOBJ=   -28268.4681656316     
 iteration         1500 MCMCOBJ=   -28258.9549348754     
 iteration         1600 MCMCOBJ=   -28297.5267366363     
 iteration         1700 MCMCOBJ=   -28287.6223222907     
 iteration         1800 MCMCOBJ=   -28240.8018574552     
 iteration         1900 MCMCOBJ=   -28301.7703348713     
 iteration         2000 MCMCOBJ=   -28182.1411210160     
 iteration         2100 MCMCOBJ=   -28148.5513652674     
 iteration         2200 MCMCOBJ=   -28193.9160207747     
 iteration         2300 MCMCOBJ=   -28136.3181436180     
 iteration         2400 MCMCOBJ=   -28117.8864917160     
 iteration         2500 MCMCOBJ=   -28341.6590229439     
 iteration         2600 MCMCOBJ=   -28218.9858825703     
 iteration         2700 MCMCOBJ=   -28253.8330716981     
 iteration         2800 MCMCOBJ=   -28249.2735290406     
 iteration         2900 MCMCOBJ=   -28202.7674951077     
 iteration         3000 MCMCOBJ=   -28327.4928707314     
 iteration         3100 MCMCOBJ=   -28307.7501137284     
 iteration         3200 MCMCOBJ=   -28239.5115900384     
 iteration         3300 MCMCOBJ=   -28248.8083670136     
 iteration         3400 MCMCOBJ=   -28228.3705064734     
 iteration         3500 MCMCOBJ=   -28244.0354292888     
 iteration         3600 MCMCOBJ=   -28089.9144714402     
 iteration         3700 MCMCOBJ=   -28271.3763067312     
 iteration         3800 MCMCOBJ=   -28229.9965050121     
 iteration         3900 MCMCOBJ=   -28187.5834783346     
 iteration         4000 MCMCOBJ=   -28217.0425256220     
 iteration         4100 MCMCOBJ=   -28173.1909592222     
 iteration         4200 MCMCOBJ=   -28264.7424381143     
 iteration         4300 MCMCOBJ=   -28294.2428026895     
 iteration         4400 MCMCOBJ=   -28229.0315785207     
 iteration         4500 MCMCOBJ=   -28308.7186803717     
 iteration         4600 MCMCOBJ=   -28183.1374201238     
 iteration         4700 MCMCOBJ=   -28275.7741287995     
 iteration         4800 MCMCOBJ=   -28181.3271931588     
 iteration         4900 MCMCOBJ=   -28196.9907251916     
 iteration         5000 MCMCOBJ=   -28200.2627395629     
 iteration         5100 MCMCOBJ=   -28201.4787946433     
 iteration         5200 MCMCOBJ=   -28202.7303830235     
 iteration         5300 MCMCOBJ=   -28299.4732822216     
 iteration         5400 MCMCOBJ=   -28306.1329928569     
 iteration         5500 MCMCOBJ=   -28291.0922981658     
 iteration         5600 MCMCOBJ=   -28236.9632062437     
 iteration         5700 MCMCOBJ=   -28170.0086614061     
 iteration         5800 MCMCOBJ=   -28214.2691212746     
 iteration         5900 MCMCOBJ=   -28212.8932196717     
 iteration         6000 MCMCOBJ=   -28272.0516431814     
 iteration         6100 MCMCOBJ=   -28210.4555264528     
 iteration         6200 MCMCOBJ=   -28278.9246366502     
 iteration         6300 MCMCOBJ=   -28229.7684269851     
 iteration         6400 MCMCOBJ=   -28231.9796105027     
 iteration         6500 MCMCOBJ=   -28151.8330890157     
 iteration         6600 MCMCOBJ=   -28237.1626211793     
 iteration         6700 MCMCOBJ=   -28203.1285262007     
 iteration         6800 MCMCOBJ=   -28213.8161745434     
 iteration         6900 MCMCOBJ=   -28308.7110535122     
 iteration         7000 MCMCOBJ=   -28359.7838413405     
 iteration         7100 MCMCOBJ=   -28243.4744948103     
 iteration         7200 MCMCOBJ=   -28208.1863937094     
 iteration         7300 MCMCOBJ=   -28164.4768195280     
 iteration         7400 MCMCOBJ=   -28159.0496004969     
 iteration         7500 MCMCOBJ=   -28209.2442868037     
 iteration         7600 MCMCOBJ=   -28168.7208301848     
 iteration         7700 MCMCOBJ=   -28247.4682995960     
 iteration         7800 MCMCOBJ=   -28127.9964183278     
 iteration         7900 MCMCOBJ=   -28251.0933257919     
 iteration         8000 MCMCOBJ=   -28303.3699044724     
 iteration         8100 MCMCOBJ=   -28242.3974001644     
 iteration         8200 MCMCOBJ=   -28195.5514376806     
 iteration         8300 MCMCOBJ=   -28313.4652488813     
 iteration         8400 MCMCOBJ=   -28231.6860269476     
 iteration         8500 MCMCOBJ=   -28214.4619161062     
 iteration         8600 MCMCOBJ=   -28311.8319934151     
 iteration         8700 MCMCOBJ=   -28181.0002993725     
 iteration         8800 MCMCOBJ=   -28197.6073767406     
 iteration         8900 MCMCOBJ=   -28216.9138802970     
 iteration         9000 MCMCOBJ=   -28209.5930896986     
 iteration         9100 MCMCOBJ=   -28159.1153305120     
 iteration         9200 MCMCOBJ=   -28141.2839701095     
 iteration         9300 MCMCOBJ=   -28319.8019228567     
 iteration         9400 MCMCOBJ=   -28143.1681199061     
 iteration         9500 MCMCOBJ=   -28215.1931004777     
 iteration         9600 MCMCOBJ=   -28189.4885528490     
 iteration         9700 MCMCOBJ=   -28240.6483868862     
 iteration         9800 MCMCOBJ=   -28212.6632179766     
 iteration         9900 MCMCOBJ=   -28222.0360536232     
 iteration        10000 MCMCOBJ=   -28231.0481266237     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         3750
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    6892.03899903504     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -28224.3380333645     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -21332.2990343295     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1250
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2297.34633301168     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -28224.3380333645     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -25926.9917003529     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    16.3289195783656     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -28224.3380333645     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -28208.0091137862     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   605.61
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -28224.338       **************************************************
 #OBJS:********************************************       59.333 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.68E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.42E-01
 
 ETA2
+       -8.17E-02  1.27E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.71E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.71E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.71E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.51E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        3.76E-01
 
 ETA2
+       -6.07E-01  3.56E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.31E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.31E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.31E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.35E-02  2.27E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.29E-02
 
 ETA2
+        1.02E-02  1.19E-02
 
 ETA3
+        0.00E+00  0.00E+00  1.10E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.10E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.10E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        6.78E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.70E-02
 
 ETA2
+        4.13E-02  1.66E-02
 
 ETA3
+        0.00E+00  0.00E+00  4.20E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  4.20E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.20E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        6.77E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.55E-04
 
 TH 2
+       -3.09E-04  5.17E-04
 
 OM11
+       -2.55E-06  4.30E-06  1.66E-04
 
 OM12
+        1.06E-06 -2.09E-06 -9.62E-05  1.04E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -6.03E-07  1.42E-06  5.61E-05 -8.77E-05  0.00E+00  0.00E+00  0.00E+00  1.42E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.31E-07 -5.95E-09  2.42E-07 -2.69E-07  0.00E+00  0.00E+00  0.00E+00 -2.14E-07  0.00E+00  0.00E+00  0.00E+00  1.21E-06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -1.58E-08  8.34E-09  3.78E-09 -8.36E-10  0.00E+00  0.00E+00  0.00E+00 -4.82E-09  0.00E+00  0.00E+00  0.00E+00 -1.15E-09
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.60E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.35E-02
 
 TH 2
+       -5.77E-01  2.27E-02
 
 OM11
+       -8.40E-03  1.47E-02  1.29E-02
 
 OM12
+        4.40E-03 -9.01E-03 -7.32E-01  1.02E-02
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.15E-03  5.25E-03  3.64E-01 -7.21E-01  0.00E+00  0.00E+00  0.00E+00  1.19E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.28E-02 -2.38E-04  1.71E-02 -2.40E-02  0.00E+00  0.00E+00  0.00E+00 -1.63E-02  0.00E+00  0.00E+00  0.00E+00  1.10E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -9.89E-03  5.41E-03  4.33E-03 -1.21E-03  0.00E+00  0.00E+00  0.00E+00 -5.96E-03  0.00E+00  0.00E+00  0.00E+00 -1.54E-02
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.78E-05
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           EIGENVALUES OF COR MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.39E-01  4.23E-01  6.34E-01  9.86E-01  1.02E+00  1.58E+00  2.23E+00
 
1
 
 
 #TBLN:      5
 #METH: First Order Conditional Estimation with Interaction (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            9999
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      10
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     10
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example7r.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 ADDITIONAL CONVERGENCE TEST (CTYPE=4)?:    NO
 EM OR BAYESIAN METHOD USED:                 NONE

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -19599.4027649381        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.8942E+00  3.6811E+00  1.4178E-01 -8.1663E-02  1.2726E-01  1.7065E-02  2.5050E-03
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   1.2519E+04  1.6050E+04  9.8013E+00  1.2426E+01  1.2105E+01  2.0568E+01  2.3545E+00
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -19599.6323137492        NO. OF FUNC. EVALS.: 102
 CUMULATIVE NO. OF FUNC. EVALS.:      107
 NPARAMETR:  3.8942E+00  3.6816E+00  1.3985E-01 -8.0852E-02  1.2483E-01  1.6718E-02  2.5052E-03
 PARAMETER:  1.0000E-01  1.0001E-01  9.3127E-02 -9.9690E-02  8.6488E-02  8.9732E-02  1.0004E-01
 GRADIENT:  -1.0320E+04  1.2013E+04  2.1214E+00  2.9602E+00  3.2843E-02  5.6743E-02  2.3888E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -19599.6347662623        NO. OF FUNC. EVALS.:  96
 CUMULATIVE NO. OF FUNC. EVALS.:      203             RESET HESSIAN, TYPE I
 NPARAMETR:  3.8945E+00  3.6820E+00  1.3934E-01 -8.0676E-02  1.2479E-01  1.6717E-02  2.5034E-03
 PARAMETER:  1.0001E-01  1.0002E-01  9.1307E-02 -9.9654E-02  8.6447E-02  8.9704E-02  9.9666E-02
 GRADIENT:   1.2707E+04  1.6291E+04 -7.8825E-02 -1.0335E+00 -1.5011E-03 -7.5963E-03 -1.7161E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -19599.6348750046        NO. OF FUNC. EVALS.:  79
 CUMULATIVE NO. OF FUNC. EVALS.:      282
 NPARAMETR:  3.8944E+00  3.6819E+00  1.3933E-01 -8.0650E-02  1.2476E-01  1.6717E-02  2.5036E-03
 PARAMETER:  1.0001E-01  1.0002E-01  9.1288E-02 -9.9623E-02  8.6448E-02  8.9704E-02  9.9712E-02
 GRADIENT:  -1.0243E+04  1.2118E+04 -1.7642E-02 -2.2550E-01 -1.3333E-03 -5.0468E-03 -1.2136E+00
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -19599.6349358620        NO. OF FUNC. EVALS.:  96
 CUMULATIVE NO. OF FUNC. EVALS.:      378             RESET HESSIAN, TYPE I
 NPARAMETR:  3.8944E+00  3.6819E+00  1.3933E-01 -8.0643E-02  1.2475E-01  1.6717E-02  2.5044E-03
 PARAMETER:  1.0001E-01  1.0002E-01  9.1287E-02 -9.9615E-02  8.6449E-02  8.9707E-02  9.9866E-02
 GRADIENT:   1.2680E+04  1.6265E+04  5.0458E-03  1.0382E-02  7.6012E-04  3.3721E-03  4.9235E-01
 
0ITERATION NO.:   22    OBJECTIVE VALUE:  -19599.6349358620        NO. OF FUNC. EVALS.:  32
 CUMULATIVE NO. OF FUNC. EVALS.:      410
 NPARAMETR:  3.8944E+00  3.6819E+00  1.3933E-01 -8.0643E-02  1.2475E-01  1.6717E-02  2.5044E-03
 PARAMETER:  1.0001E-01  1.0002E-01  9.1287E-02 -9.9615E-02  8.6449E-02  8.9707E-02  9.9866E-02
 GRADIENT:  -1.9413E+00 -2.1213E+00  4.8406E-03  1.0696E-02  5.6985E-04  3.3520E-03  3.5681E-01
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.
 REGARD THE RESULTS OF THE ESTIMATION STEP CAREFULLY, AND ACCEPT THEM ONLY
 AFTER CHECKING THAT THE COVARIANCE STEP PRODUCES REASONABLE OUTPUT.
 NO. OF FUNCTION EVALUATIONS USED:      410
 NO. OF SIG. DIGITS IN FINAL EST.:  3.2

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -5.7327E-04  7.3265E-05 -6.7124E-03  2.5884E-03  4.0686E-03
 SE:             2.3551E-02  2.1838E-02  6.6359E-03  6.6176E-03  6.8554E-03
 N:                     250         250         250         250         250
 
 P VAL.:         9.8058E-01  9.9732E-01  3.1177E-01  6.9570E-01  5.5285E-01
 
 ETASHRINKSD(%)  3.8945E-02  2.0443E+00  1.8686E+01  1.8911E+01  1.5997E+01
 ETASHRINKVR(%)  7.7875E-02  4.0467E+00  3.3881E+01  3.4245E+01  2.9435E+01
 EBVSHRINKSD(%)  1.3875E-01  2.1761E+00  1.7839E+01  1.7937E+01  1.7858E+01
 EBVSHRINKVR(%)  2.7732E-01  4.3049E+00  3.2496E+01  3.2656E+01  3.2527E+01
 EPSSHRINKSD(%)  1.4070E+01
 EPSSHRINKVR(%)  2.6161E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         3750
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    6892.03899903504     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -19599.6349358620     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -12707.5959368270     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1250
  
 #TERE:
 Elapsed estimation  time in seconds:    39.98
 Elapsed covariance  time in seconds:     5.03
 Elapsed postprocess time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -19599.635       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.68E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.39E-01
 
 ETA2
+       -8.06E-02  1.25E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.67E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.67E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.67E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.50E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        3.73E-01
 
 ETA2
+       -6.12E-01  3.53E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.29E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.29E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.29E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.36E-02  2.29E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.25E-02
 
 ETA2
+        9.95E-03  1.17E-02
 
 ETA3
+       ......... .........  1.08E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        6.76E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5     
 
 ETA1
+        1.67E-02
 
 ETA2
+        4.12E-02  1.65E-02
 
 ETA3
+       ......... .........  4.16E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        6.75E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.59E-04
 
 TH 2
+       -3.22E-04  5.22E-04
 
 OM11
+        1.55E-07 -2.44E-07  1.56E-04
 
 OM12
+       -2.02E-07  1.23E-07 -9.00E-05  9.89E-05
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.31E-07 -6.86E-09  5.19E-05 -8.41E-05 ......... ......... .........  1.37E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        2.88E-08 -8.88E-09  1.60E-08 -1.31E-08 ......... ......... ......... -3.76E-07 ......... ......... .........  1.16E-06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        4.40E-09  4.93E-09 -8.57E-10 -2.08E-10 ......... ......... ......... -5.95E-10 ......... ......... ......... -3.51E-10
         ......... ......... ......... ......... .........  4.57E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.36E-02
 
 TH 2
+       -5.96E-01  2.29E-02
 
 OM11
+        5.24E-04 -8.53E-04  1.25E-02
 
 OM12
+       -8.60E-04  5.41E-04 -7.24E-01  9.95E-03
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        8.37E-04 -2.57E-05  3.55E-01 -7.24E-01 ......... ......... .........  1.17E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.13E-03 -3.61E-04  1.19E-03 -1.22E-03 ......... ......... ......... -2.99E-02 ......... ......... .........  1.08E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        2.75E-03  3.19E-03 -1.01E-03 -3.09E-04 ......... ......... ......... -7.54E-04 ......... ......... ......... -4.83E-03
         ......... ......... ......... ......... .........  6.76E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.77E+03
 
 TH 2
+        1.71E+03  2.97E+03
 
 OM11
+        2.54E+00  4.35E+00  1.54E+04
 
 OM12
+        1.98E+00 -1.38E-01  1.90E+04  4.46E+04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.54E+00 -4.57E+00  5.84E+03  2.03E+04 ......... ......... .........  1.76E+04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -5.89E+01 -2.28E+01  1.90E+03  6.84E+03 ......... ......... .........  5.88E+03 ......... ......... .........  8.67E+05
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -4.52E+03 -4.85E+03  4.65E+03  8.76E+03 ......... ......... .........  4.78E+03 ......... ......... .........  6.81E+04
         ......... ......... ......... ......... .........  2.19E+08
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.38E-01  4.04E-01  6.44E-01  9.96E-01  1.01E+00  1.60E+00  2.22E+00
 
 Elapsed finaloutput time in seconds:     0.34
 #CPUT: Total CPU Time in Seconds,      762.767
Stop Time: 
Sat 04/22/2017 
10:35 AM

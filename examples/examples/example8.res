Sat 04/22/2017 
11:28 AM
;Model Desc: Two compartment Model, Using ADVAN3, TRANS4
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# Example 8 (from samp5l)
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT 
       CLX V1X QX V2X SDIX SDSX
$DATA example8.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4


$PK
include nonmem_reserved_general
; Request extra information for Bayesian analysis.  
; An extra call will then be made for accepted samples
BAYES_EXTRA_REQUEST=1
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
MU_4=THETA(4)
CL=DEXP(MU_1+ETA(1))
V1=DEXP(MU_2+ETA(2))
Q=DEXP(MU_3+ETA(3))
V2=DEXP(MU_4+ETA(4))
S1=V1
; When Bayes_extra=1, then this particular set of individual 
; parameters were "accepted" So you may record them if you wish
  IF(BAYES_EXTRA==1 .AND. ITER_REPORT>=0 .AND. TIME==0.0) THEN
"  WRITE(51,98) ITER_REPORT,ID,CL,V1,Q,V2
" 98 FORMAT(I12,1X,F14.0,4(1X,1PG12.5))
ENDIF

$ERROR
include nonmem_reserved_general
BAYES_EXTRA_REQUEST=1
Y = F + F*EPS(1)
IF(BAYES_EXTRA==1 .AND. ITER_REPORT>=0 ) THEN
" WRITE(52,97) ITER_REPORT,ID,TIME,F
" 97 FORMAT(I12,1X,F14.0,2(1X,1PG12.5))
ENDIF

; Initial values of THETA
$THETA 
(2.0) ;[LN(CL)]
(2.0) ;[LN(V1)]
(2.0) ;[LN(Q)]
(2.0) ;[LN(V2)]
;INITIAL values of OMEGA
$OMEGA BLOCK(4)
0.15   ;[P]
0.01  ;[F]
0.15   ;[P]
0.01  ;[F]
0.01  ;[F]
0.15   ;[P]
0.01  ;[F]
0.01  ;[F]
0.01  ;[F]
0.15   ;[P]
;Initial value of SIGMA
$SIGMA 
(0.6 )   ;[P]


$PRIOR NWPRI
; Prior information to the Thetas.
$THETAP (2.0 FIX)x4
$THETAPV BLOCK(4) FIX VALUES(10000.0,0.0)

; Prior information to the OMEGAS.
$OMEGAP BLOCK(4)
0.2 FIX 
0.0  0.2 
0.0  0.0 0.2
0.0  0.0 0.0 0.2
$OMEGAPD (4 FIX)

$EST METHOD=BAYES INTERACTION FILE=example8.ext NBURN=10000 
     NITER=1000 PRINT=100 NOPRIOR=0 CTYPE=3 CINTERVAL=100
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.
  
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
 RUN# Example 8 (from samp5l)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      600
 NO. OF DATA ITEMS IN DATA SET:  17
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   5   7   8   0   0  11   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID JID TIME CONC DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
0FORMAT FOR DATA:
 (2E2.0,3E4.0,E11.0,E4.0,4E2.0,2E7.0,E8.0,E7.0,E2.0,E5.0)

 TOT. NO. OF OBS RECS:      500
 TOT. NO. OF INDIVIDUALS:      100
0LENGTH OF THETA:   9
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
  1  1  1  1
  0  0  0  0  2
  0  0  0  0  2  2
  0  0  0  0  2  2  2
  0  0  0  0  2  2  2  2
  0  0  0  0  0  0  0  0  3
  0  0  0  0  0  0  0  0  3  3
  0  0  0  0  0  0  0  0  3  3  3
  0  0  0  0  0  0  0  0  3  3  3  3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.2000E+01     0.1000E+07
 -0.1000E+07     0.2000E+01     0.1000E+07
 -0.1000E+07     0.2000E+01     0.1000E+07
 -0.1000E+07     0.2000E+01     0.1000E+07
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.4000E+01     0.4000E+01     0.4000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.1500E+00
                  0.1000E-01   0.1500E+00
                  0.1000E-01   0.1000E-01   0.1500E+00
                  0.1000E-01   0.1000E-01   0.1000E-01   0.1500E+00
        2                                                                                  YES
                  0.1000E+05
                  0.0000E+00   0.1000E+05
                  0.0000E+00   0.0000E+00   0.1000E+05
                  0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+05
        3                                                                                  YES
                  0.2000E+00
                  0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.6000E+00
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 beta 2 (nm74b2)

 TWO COMPARTMENT MODEL (ADVAN3)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   4
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   BASIC PK PARAMETER NO.  1: ELIMINATION RATE (K)
   BASIC PK PARAMETER NO.  2: CENTRAL-TO-PERIPH. RATE (K12)
   BASIC PK PARAMETER NO.  3: PERIPH.-TO-CENTRAL RATE (K21)
 TRANSLATOR WILL CONVERT PARAMETERS
 CL, V1, Q, V2 TO K, K12, K21 (TRANS4)
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         NO         YES        YES        YES
    2         PERIPH.      ON         NO         YES        NO         NO
    3         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            5           *           *           *           *
    2            *           *           *           *           *
    3            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      9
   TIME DATA ITEM IS DATA ITEM NO.:          5
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   7
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     8
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:   11

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 
 
 #TBLN:      1
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            2400
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      100
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     100
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example8.ext
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
 ITERATIONS (NITER):                        1000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
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
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           10
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):10
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
   1   2   3   4
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3   4
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration       -10000 MCMCOBJ=    103327118.625202     
 iteration        -9900 MCMCOBJ=   -2351.39209850422     
 iteration        -9800 MCMCOBJ=   -2310.92808347893     
 iteration        -9700 MCMCOBJ=   -2334.28857676149     
 iteration        -9600 MCMCOBJ=   -2326.79585109319     
 iteration        -9500 MCMCOBJ=   -2250.68260367568     
 iteration        -9400 MCMCOBJ=   -2329.91054478491     
 iteration        -9300 MCMCOBJ=   -2279.36178834818     
 iteration        -9200 MCMCOBJ=   -2260.28356548992     
 iteration        -9100 MCMCOBJ=   -2333.88108611920     
 iteration        -9000 MCMCOBJ=   -2261.10282047449     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -2317.28396620577     
 iteration          100 MCMCOBJ=   -2294.64639441381     
 iteration          200 MCMCOBJ=   -2411.47215437863     
 iteration          300 MCMCOBJ=   -2272.77518578007     
 iteration          400 MCMCOBJ=   -2312.83927951359     
 iteration          500 MCMCOBJ=   -2344.57432935686     
 iteration          600 MCMCOBJ=   -2338.81158780034     
 iteration          700 MCMCOBJ=   -2274.21855894737     
 iteration          800 MCMCOBJ=   -2277.53179181359     
 iteration          900 MCMCOBJ=   -2300.87983149971     
 iteration         1000 MCMCOBJ=   -2322.05175839863     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):          500
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    918.938533204673     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.32435960879     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1387.38582640412     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.32435960879     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1571.17353304505     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    66.6250661892040     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.32435960879     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -2239.69929341959     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    24.21
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -2306.324       **************************************************
 #OBJS:********************************************       44.459 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.64E+00  1.56E+00  7.50E-01  2.35E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.76E-01
 
 ETA2
+       -5.15E-03  1.46E-01
 
 ETA3
+        8.99E-03 -7.88E-03  1.92E-01
 
 ETA4
+       -1.95E-02  1.05E-02  2.43E-02  1.64E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        6.00E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.18E-01
 
 ETA2
+       -3.59E-02  3.79E-01
 
 ETA3
+        4.04E-02 -5.15E-02  4.33E-01
 
 ETA4
+       -1.20E-01  6.24E-02  1.13E-01  4.03E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.45E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.52E-02  5.08E-02  7.09E-02  5.43E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.81E-02
 
 ETA2
+        2.27E-02  3.37E-02
 
 ETA3
+        2.86E-02  3.39E-02  6.18E-02
 
 ETA4
+        2.33E-02  2.57E-02  3.52E-02  3.84E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.31E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        3.31E-02
 
 ETA2
+        1.41E-01  4.33E-02
 
 ETA3
+        1.52E-01  1.97E-01  7.02E-02
 
 ETA4
+        1.37E-01  1.60E-01  1.83E-01  4.70E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.48E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.04E-03
 
 TH 2
+        1.45E-04  2.58E-03
 
 TH 3
+        5.36E-04  8.66E-05  5.03E-03
 
 TH 4
+        2.15E-04  3.52E-04  1.87E-03  2.94E-03
 
 OM11
+       -2.77E-05  1.01E-04  1.57E-04  6.84E-05  7.89E-04
 
 OM12
+        2.58E-05  1.82E-04  2.20E-04  7.74E-05  5.57E-05  5.15E-04
 
 OM13
+        8.46E-05  1.39E-04  2.11E-04  1.51E-04  1.93E-04  9.91E-05  8.19E-04
 
 OM14
+        9.72E-05  2.81E-05  3.26E-04  1.18E-04  4.82E-05  1.12E-04  3.08E-04  5.45E-04
 
 OM22
+        3.72E-05 -4.49E-05 -1.57E-04 -2.73E-05 -3.32E-05  7.37E-05 -5.50E-05 -1.74E-05  1.13E-03
 
 OM23
+       -5.87E-05  2.11E-04  5.15E-04  1.28E-04  4.61E-05  1.79E-04  4.64E-05  8.24E-05 -2.44E-05  1.15E-03
 
 OM24
+       -2.93E-05  8.10E-05  1.37E-04  8.48E-05 -3.59E-05  5.01E-05  8.30E-06  7.90E-05  1.22E-04  3.06E-04  6.63E-04
 
 OM33
+        2.20E-04 -1.71E-04  4.08E-04  1.62E-04  8.65E-05  2.50E-04  5.26E-04  3.32E-04 -1.34E-04  7.89E-05  7.83E-06  3.82E-03
 
 OM34
+        1.05E-04 -9.41E-05  1.53E-04  2.91E-05  2.07E-05  1.18E-04  1.78E-04  1.90E-04  2.48E-06  1.52E-04  1.20E-04  1.30E-03
          1.24E-03
 
 OM44
+        1.01E-05  3.82E-05 -9.07E-05  9.31E-05 -5.22E-05  8.03E-05  3.03E-05  5.18E-05  1.00E-04  7.81E-05  2.73E-04  4.81E-04
          6.92E-04  1.48E-03
 
 SG11
+        7.47E-06  3.66E-05  3.64E-05  3.35E-05 -6.80E-06 -1.39E-05 -1.66E-05 -1.69E-05 -3.72E-05 -8.90E-06 -1.86E-05 -1.21E-04
         -7.63E-05 -7.93E-05  5.34E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        4.52E-02
 
 TH 2
+        6.32E-02  5.08E-02
 
 TH 3
+        1.67E-01  2.40E-02  7.09E-02
 
 TH 4
+        8.79E-02  1.28E-01  4.86E-01  5.43E-02
 
 OM11
+       -2.19E-02  7.06E-02  7.86E-02  4.49E-02  2.81E-02
 
 OM12
+        2.51E-02  1.58E-01  1.37E-01  6.29E-02  8.74E-02  2.27E-02
 
 OM13
+        6.54E-02  9.56E-02  1.04E-01  9.69E-02  2.40E-01  1.53E-01  2.86E-02
 
 OM14
+        9.22E-02  2.37E-02  1.97E-01  9.31E-02  7.36E-02  2.11E-01  4.62E-01  2.33E-02
 
 OM22
+        2.44E-02 -2.62E-02 -6.57E-02 -1.49E-02 -3.51E-02  9.64E-02 -5.70E-02 -2.21E-02  3.37E-02
 
 OM23
+       -3.84E-02  1.23E-01  2.15E-01  6.96E-02  4.85E-02  2.33E-01  4.79E-02  1.04E-01 -2.14E-02  3.39E-02
 
 OM24
+       -2.52E-02  6.19E-02  7.49E-02  6.07E-02 -4.97E-02  8.57E-02  1.13E-02  1.31E-01  1.41E-01  3.51E-01  2.57E-02
 
 OM33
+        7.89E-02 -5.44E-02  9.30E-02  4.82E-02  4.98E-02  1.78E-01  2.97E-01  2.30E-01 -6.42E-02  3.77E-02  4.92E-03  6.18E-02
 
 OM34
+        6.61E-02 -5.26E-02  6.13E-02  1.52E-02  2.10E-02  1.48E-01  1.77E-01  2.31E-01  2.09E-03  1.27E-01  1.33E-01  5.95E-01
          3.52E-02
 
 OM44
+        5.80E-03  1.96E-02 -3.33E-02  4.46E-02 -4.84E-02  9.20E-02  2.76E-02  5.78E-02  7.76E-02  6.00E-02  2.76E-01  2.02E-01
          5.11E-01  3.84E-02
 
 SG11
+        2.26E-02  9.86E-02  7.02E-02  8.46E-02 -3.31E-02 -8.37E-02 -7.96E-02 -9.92E-02 -1.51E-01 -3.60E-02 -9.88E-02 -2.68E-01
         -2.97E-01 -2.82E-01  7.31E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        5.17E+02
 
 TH 2
+       -3.58E+01  4.25E+02
 
 TH 3
+       -5.82E+01  3.15E+01  2.94E+02
 
 TH 4
+        3.56E+00 -5.48E+01 -1.75E+02  4.62E+02
 
 OM11
+        3.85E+01 -3.49E+01 -3.50E+01  7.87E-01  1.38E+03
 
 OM12
+        1.56E+01 -1.43E+02 -5.46E+01  2.25E+01 -8.56E+01  2.28E+03
 
 OM13
+       -1.59E+01 -7.49E+01  2.68E+01 -4.56E+01 -3.43E+02 -2.85E+01  1.75E+03
 
 OM14
+       -4.98E+01  3.33E+01 -1.10E+02  1.86E+01  9.63E+01 -3.14E+02 -8.76E+02  2.57E+03
 
 OM22
+       -3.52E+01  2.38E+01  3.10E+01 -1.91E+01  1.94E+01 -1.90E+02  3.67E+01  3.96E+01  9.55E+02
 
 OM23
+        5.54E+01 -6.38E+01 -1.04E+02  3.75E+01 -3.34E+01 -2.97E+02  6.72E+00  3.49E+01  7.94E+01  1.12E+03
 
 OM24
+        2.77E+01 -2.37E+01 -5.28E+00 -2.10E+01  8.66E+01  8.94E+01  5.29E+01 -2.69E+02 -1.90E+02 -4.98E+02  1.95E+03
 
 OM33
+       -1.54E+01  2.08E+01 -1.07E+01 -1.54E+01  2.51E+01 -1.15E+02 -1.79E+02  1.43E+01  5.97E+01  4.58E+01  3.43E+01  4.61E+02
 
 OM34
+       -3.49E+01  4.32E+01 -1.21E+01  4.32E+01 -2.96E+01  2.56E+01  6.03E+01 -2.60E+02 -3.36E+00 -1.61E+02  5.61E+01 -4.76E+02
          1.73E+03
 
 OM44
+        6.34E+00 -3.39E+01  4.03E+01 -6.31E+01  5.94E+01 -7.31E+01  9.17E+00  1.18E+02 -2.92E+00  9.99E+01 -3.44E+02  9.68E+01
         -6.38E+02  1.06E+03
 
 SG11
+       -1.03E+02 -2.71E+02 -1.28E+02 -2.17E+02  2.59E+02  1.10E+02  2.22E+01  2.95E+02  6.96E+02  1.05E+02  1.03E+02  4.98E+02
          3.32E+02  8.36E+02  2.27E+04
 
 Elapsed postprocess time in seconds:     0.00
 Elapsed finaloutput time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,       21.825
Stop Time: 
Sat 04/22/2017 
11:28 AM

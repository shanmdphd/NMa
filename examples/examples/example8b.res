Sat 04/22/2017 
11:31 AM
;Model Desc: Two compartment Model, Using ADVAN3, TRANS4
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# Example 8b (from samp5l)
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
$DATA example8.csv IGNORE=C
$abbr DECLARE INTEGER FIRST_WRITE INTEGER FIRST_WRITE2

$SUBROUTINES ADVAN3 TRANS4


$PK
include nonmem_reserved_general
; Request extra information for Bayesian analysis.  An extra call will then be made
; for accepted samples
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
; When Bayes_extra=1, then this particular set of individual parameters were "accepted"
; So you may record them if you wish
IF(BAYES_EXTRA==1 .AND. ITER_REPORT>=0 .AND. TIME==0.0) THEN
IF(FIRST_WRITE==0) THEN
" OPEN(unit=53,FILE='C:\NONMEM\WORKA_'//TRIM(TFI(PNM_NODE_NUMBER)))
FIRST_WRITE=1
ENDIF
" WRITE(53,'(I12,1X,F14.0,5(1X,1PG12.5))') ITER_REPORT,ID,CL,V1,Q,V2,OBJI(NIREC,1)
ENDIF

$ERROR
include nonmem_reserved_general
BAYES_EXTRA_REQUEST=1
Y = F + F*EPS(1)
IF(BAYES_EXTRA==1 .AND. ITER_REPORT>=0 ) THEN
IF(FIRST_WRITE2==0) THEN
"OPEN(UNIT=54,FILE='C:\NONMEM\WORKB_'//TRIM(TFI(PNM_NODE_NUMBER)))
FIRST_WRITE2=1
ENDIF
" WRITE(54,'(I12,1X,F14.0,2(1X,1PG12.5))') ITER_REPORT,ID,TIME,F
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
; Prior information to the THETAS.
$THETAP (2.0 FIX) (2.0 FIX) (2.0 FIX) (2.0 FIX)
$THETAPV BLOCK(4)
10000 FIX 
0.00 10000
0.00  0.00 10000
0.00  0.00 0.0 10000

; Prior information to the OMEGAS.
$OMEGAP BLOCK(4)
0.2 FIX 
0.0  0.2 
0.0  0.0 0.2
0.0  0.0 0.0 0.2
$OMEGAPD (4 FIX)

$EST METHOD=BAYES INTERACTION FILE=example8b.ext NBURN=10000 NITER=1000 PRINT=100 NOPRIOR=0
     CTYPE=3 CINTERVAL=100
  
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
 RUN# Example 8b (from samp5l)
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
 
 #PARA: PARAFILE=mpiwini8.pnm, PROTOCOL=MPI, NODES= 3
 
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
 RAW OUTPUT FILE (FILE): example8b.ext
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
 iteration       -10000 MCMCOBJ=    92048792.7847798     
 iteration        -9900 MCMCOBJ=   -2323.31168357922     
 iteration        -9800 MCMCOBJ=   -2343.73989924781     
 iteration        -9700 MCMCOBJ=   -2273.68967848306     
 iteration        -9600 MCMCOBJ=   -2299.12775876797     
 iteration        -9500 MCMCOBJ=   -2332.67302559446     
 iteration        -9400 MCMCOBJ=   -2335.46057619876     
 iteration        -9300 MCMCOBJ=   -2368.04992664009     
 iteration        -9200 MCMCOBJ=   -2278.53476206423     
 iteration        -9100 MCMCOBJ=   -2249.99869272604     
 iteration        -9000 MCMCOBJ=   -2226.40541263845     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -2185.77027379304     
 iteration          100 MCMCOBJ=   -2351.07545870292     
 iteration          200 MCMCOBJ=   -2266.80936058760     
 iteration          300 MCMCOBJ=   -2250.79054691251     
 iteration          400 MCMCOBJ=   -2331.69435615366     
 iteration          500 MCMCOBJ=   -2336.64457327169     
 iteration          600 MCMCOBJ=   -2298.22969594683     
 iteration          700 MCMCOBJ=   -2344.43931327803     
 iteration          800 MCMCOBJ=   -2365.61417444760     
 iteration          900 MCMCOBJ=   -2277.16196638036     
 iteration         1000 MCMCOBJ=   -2380.53343731649     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):          500
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    918.938533204673     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.86079571278     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1387.92226250811     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.86079571278     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1571.70996914904     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    66.6250661892040     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.86079571278     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -2240.23572952358     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    22.80
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -2306.861       **************************************************
 #OBJS:********************************************       42.431 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.63E+00  1.56E+00  7.41E-01  2.34E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.76E-01
 
 ETA2
+       -2.24E-03  1.55E-01
 
 ETA3
+        8.44E-03  2.26E-04  1.87E-01
 
 ETA4
+       -2.24E-02  1.57E-02  1.80E-02  1.56E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        6.05E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.18E-01
 
 ETA2
+       -1.78E-02  3.91E-01
 
 ETA3
+        3.78E-02  1.46E-03  4.27E-01
 
 ETA4
+       -1.43E-01  9.60E-02  8.08E-02  3.93E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.46E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.33E-02  5.34E-02  6.47E-02  5.33E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.87E-02
 
 ETA2
+        2.40E-02  3.60E-02
 
 ETA3
+        2.95E-02  3.46E-02  5.74E-02
 
 ETA4
+        2.47E-02  2.62E-02  3.50E-02  3.65E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.33E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        3.38E-02
 
 ETA2
+        1.44E-01  4.57E-02
 
 ETA3
+        1.60E-01  2.00E-01  6.69E-02
 
 ETA4
+        1.50E-01  1.63E-01  1.94E-01  4.55E-02
 


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
+        1.87E-03
 
 TH 2
+        2.99E-04  2.85E-03
 
 TH 3
+        3.83E-04  2.67E-04  4.19E-03
 
 TH 4
+        8.55E-05  4.91E-04  1.44E-03  2.84E-03
 
 OM11
+        3.52E-05  1.40E-04  7.30E-05  5.47E-05  8.23E-04
 
 OM12
+        4.69E-05  1.09E-04  1.95E-04  1.57E-04  1.63E-04  5.75E-04
 
 OM13
+        9.08E-05  8.74E-05  1.80E-04  1.81E-04  2.47E-04  9.38E-05  8.68E-04
 
 OM14
+        8.79E-05  5.94E-05  2.36E-04  1.59E-04  1.13E-04  1.43E-04  3.42E-04  6.11E-04
 
 OM22
+        3.15E-05 -2.73E-04 -2.23E-04 -1.01E-04 -4.07E-05  4.66E-05 -5.64E-05  2.75E-05  1.30E-03
 
 OM23
+       -9.05E-06  2.11E-04  5.50E-04  1.17E-04  2.64E-05  9.82E-05  5.55E-05  3.62E-05 -1.00E-04  1.19E-03
 
 OM24
+        2.12E-05 -1.04E-04  1.81E-04  1.27E-04 -5.46E-05 -3.81E-05 -2.29E-05  3.36E-05  2.51E-04  2.11E-04  6.87E-04
 
 OM33
+        1.39E-04 -3.23E-04  4.13E-04  1.16E-04  1.30E-04  1.38E-05  4.52E-04  1.87E-04 -4.72E-05  1.06E-04  9.92E-05  3.30E-03
 
 OM34
+        1.40E-04 -9.39E-05  3.17E-04  7.55E-06  1.10E-04  2.03E-05  1.24E-04  2.00E-04  7.45E-05  1.72E-04  1.13E-04  1.11E-03
          1.22E-03
 
 OM44
+        1.55E-04  8.16E-05  3.90E-05  5.13E-05  9.86E-05 -4.87E-07  7.46E-05  8.70E-05  2.17E-04  1.11E-05  1.85E-04  2.60E-04
          6.34E-04  1.33E-03
 
 SG11
+       -2.52E-05  3.87E-05 -1.17E-05  3.75E-07 -3.01E-05 -1.75E-05 -4.41E-05 -4.59E-05 -7.83E-05  6.73E-06 -1.85E-05 -8.08E-05
         -7.20E-05 -7.59E-05  5.37E-05
 
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
+        4.33E-02
 
 TH 2
+        1.30E-01  5.34E-02
 
 TH 3
+        1.37E-01  7.73E-02  6.47E-02
 
 TH 4
+        3.71E-02  1.72E-01  4.18E-01  5.33E-02
 
 OM11
+        2.84E-02  9.12E-02  3.93E-02  3.57E-02  2.87E-02
 
 OM12
+        4.52E-02  8.48E-02  1.26E-01  1.22E-01  2.37E-01  2.40E-02
 
 OM13
+        7.12E-02  5.56E-02  9.45E-02  1.15E-01  2.93E-01  1.33E-01  2.95E-02
 
 OM14
+        8.23E-02  4.50E-02  1.48E-01  1.21E-01  1.60E-01  2.42E-01  4.70E-01  2.47E-02
 
 OM22
+        2.02E-02 -1.42E-01 -9.56E-02 -5.28E-02 -3.94E-02  5.40E-02 -5.31E-02  3.09E-02  3.60E-02
 
 OM23
+       -6.06E-03  1.14E-01  2.46E-01  6.33E-02  2.66E-02  1.18E-01  5.45E-02  4.23E-02 -8.05E-02  3.46E-02
 
 OM24
+        1.87E-02 -7.43E-02  1.07E-01  9.08E-02 -7.27E-02 -6.06E-02 -2.97E-02  5.19E-02  2.66E-01  2.33E-01  2.62E-02
 
 OM33
+        5.60E-02 -1.05E-01  1.11E-01  3.78E-02  7.88E-02  1.00E-02  2.67E-01  1.32E-01 -2.28E-02  5.33E-02  6.59E-02  5.74E-02
 
 OM34
+        9.24E-02 -5.03E-02  1.40E-01  4.05E-03  1.10E-01  2.43E-02  1.21E-01  2.31E-01  5.91E-02  1.42E-01  1.23E-01  5.54E-01
          3.50E-02
 
 OM44
+        9.84E-02  4.19E-02  1.65E-02  2.64E-02  9.43E-02 -5.57E-04  6.94E-02  9.66E-02  1.65E-01  8.84E-03  1.93E-01  1.24E-01
          4.97E-01  3.65E-02
 
 SG11
+       -7.95E-02  9.89E-02 -2.47E-02  9.60E-04 -1.43E-01 -9.98E-02 -2.04E-01 -2.53E-01 -2.96E-01  2.66E-02 -9.64E-02 -1.92E-01
         -2.81E-01 -2.84E-01  7.33E-03
 
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
+        5.65E+02
 
 TH 2
+       -6.46E+01  3.96E+02
 
 TH 3
+       -5.69E+01  1.16E+01  3.24E+02
 
 TH 4
+        2.55E+01 -6.57E+01 -1.53E+02  4.51E+02
 
 OM11
+        1.58E+01 -4.92E+01  2.63E+00  1.54E+01  1.43E+03
 
 OM12
+       -1.86E+01 -2.96E+01 -3.96E+01 -6.85E+01 -3.46E+02  2.03E+03
 
 OM13
+       -2.75E+01 -1.62E+01  1.28E+01 -4.94E+01 -3.70E+02  8.40E+01  1.75E+03
 
 OM14
+       -1.97E+01 -2.52E+01 -5.12E+01 -3.05E+01  7.52E+01 -4.31E+02 -8.92E+02  2.41E+03
 
 OM22
+       -1.41E+01  5.64E+01  4.68E+01  1.32E+01  5.88E+01 -1.43E+02  6.93E+01  1.77E+01  9.66E+02
 
 OM23
+        4.56E+01 -7.73E+01 -1.13E+02  5.44E+01  1.83E+01 -1.85E+02 -9.02E+01  1.05E+02  1.05E+02  1.01E+03
 
 OM24
+       -7.21E+00  6.88E+01 -3.24E+01 -7.97E+01  7.54E+01  2.23E+02  1.11E+02 -1.50E+02 -3.49E+02 -3.63E+02  1.81E+03
 
 OM33
+       -7.88E+00  3.78E+01 -6.06E+00 -1.58E+01  2.98E+01 -7.03E+00 -2.62E+02  1.58E+02  4.16E+01  4.85E+01 -5.92E+01  5.09E+02
 
 OM34
+       -2.00E+01  2.99E+01 -6.36E+01  6.47E+01 -9.40E+01  9.15E+01  3.20E+02 -4.54E+02  9.81E+00 -2.10E+02  8.86E+01 -5.39E+02
          1.80E+03
 
 OM44
+       -3.76E+01 -6.31E+01  3.51E+01 -3.06E+01 -4.50E+01  2.13E+01 -1.03E+02  1.55E+02 -6.28E+01  1.05E+02 -2.30E+02  1.73E+02
         -7.19E+02  1.16E+03
 
 SG11
+        1.42E+02 -2.62E+02  1.83E+01 -5.28E+01  4.55E+02  2.09E+02  5.38E+02  1.04E+03  1.28E+03 -1.43E+02 -1.15E+02  2.24E+02
          4.58E+02  8.13E+02  2.45E+04
 
 Elapsed postprocess time in seconds:     0.00
 Elapsed finaloutput time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,       51.434
Stop Time: 
Sat 04/22/2017 
11:31 AM

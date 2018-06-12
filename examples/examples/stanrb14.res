Mon 09/05/2016 
08:25 PM
$PROBLEM Stan model
$DATA full2.csv IGNORE=@
$INPUT DV TIME MDV EVID AMT CMT DRUG SARM ID

$THETAI
THETA(4:5)=LOG(EXP(THETAI(4:5))+1.0)
THETA(9:10)=LOG(EXP(THETAI(9:10))+1.0)

$THETAR
THETAR(4:5)=LOG(EXP(THETA(4:5))-1.0)
THETAR(9:10)=LOG(EXP(THETA(9:10))-1.0)

$PRED
include nonmem_reserved_general
MUFIRSTREC=1
OBJQUICK=2
ALPHAC=50.0
; MU REFERENCE NEEDED TO do Gibbs sampling
LALPHA0=THETA(1)
LALPHAS=THETA(2)
LKAPPA=THETA(3)
LEMAX1=THETA(4)
LEMAX2=THETA(5)
STIM=0.0
IF(SARM==2) STIM=LEMAX1
IF(SARM==3) STIM=LEMAX2
MU_1=LALPHA0
MU_2=(LKAPPA+LALPHAS)/2.0+STIM
MU_3=(LKAPPA-LALPHAS)/2.0
G0=ALPHAC*EXP(MU_1+ETA(1))
KIN=EXP(MU_2+ETA(2))
KOUT=EXP(MU_3+ETA(3))
ALPHA=KIN/KOUT
F=ALPHA+(G0-ALPHA)*EXP(-KOUT*TIME)
Y=F+EPS(1)

$THETA 0.01 4.0 -5.0 -1.1 -1.6

$OMEGA BLOCK(3) VALUES(0.5,0.001)

$SIGMA 25.0

$PRIOR NWPRI

$THETAP (0.0 FIXED) (3.738 FIXED) (-4.451 FIXED) (-1.386 FIXED) (-1.386 FIXED)

$THETAPV BLOCK(5)
0.0086529 FIXED
0.0 0.042795
0.0 0.0 0.125065
0.0 0.0 0.0  0.67427
0.0 0.0 0.0  0.0   0.67427

$OMEGAP BLOCK(3)
0.01 FIX
0.0  0.16
0.0  0.0 0.04

$OMEGAPD (3 FIX)

$SIGMAP BLOCK(1)
16.0 FIX

$SIGMAPD (2 FIX)

$EST METHOD=BAYES NBURN=500 NITER=0 PRINT=50 MASSRESET=1
file=stanrb14_bayes.ext KAPPA=0.75
$EST METHOD=NUTS NBURN=1000 NITER=2000 PRINT=20 file=stanrb14.ext
     OLKJDF=3.0 MASSRESET=0 KAPPA=1.0
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  121) INTERACTION IS IMPLIED WITH EM/BAYES ESTIMATION METHODS

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.

 (MU_WARNING 6) THETA(003): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.

 (MU_WARNING 6) THETA(002): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        5 SEP 2016
Days until program expires :5017
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha12 (nm74a12)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 Stan model
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     9900
 NO. OF DATA ITEMS IN DATA SET:   9
 ID DATA ITEM IS DATA ITEM NO.:   9
 DEP VARIABLE IS DATA ITEM NO.:   1
 MDV DATA ITEM IS DATA ITEM NO.:  3
0LABELS FOR DATA ITEMS:
 DV TIME MDV EVID AMT CMT DRUG SARM ID
0FORMAT FOR DATA:
 (9E7.0)

 TOT. NO. OF OBS RECS:     9000
 TOT. NO. OF INDIVIDUALS:    900
0LENGTH OF THETA:  12
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
  0  0  0  2
  0  0  0  2  2
  0  0  0  2  2  2
  0  0  0  2  2  2  2
  0  0  0  2  2  2  2  2
  0  0  0  0  0  0  0  0  3
  0  0  0  0  0  0  0  0  3  3
  0  0  0  0  0  0  0  0  3  3  3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS BLOCK FORM:
  1
  0  2
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.1000E-01     0.1000E+07
 -0.1000E+07     0.4000E+01     0.1000E+07
 -0.1000E+07    -0.5000E+01     0.1000E+07
 -0.1000E+07    -0.1100E+01     0.1000E+07
 -0.1000E+07    -0.1600E+01     0.1000E+07
  0.0000E+00     0.0000E+00     0.0000E+00
  0.3738E+01     0.3738E+01     0.3738E+01
 -0.4451E+01    -0.4451E+01    -0.4451E+01
 -0.1386E+01    -0.1386E+01    -0.1386E+01
 -0.1386E+01    -0.1386E+01    -0.1386E+01
  0.3000E+01     0.3000E+01     0.3000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.5000E+00
                  0.1000E-02   0.5000E+00
                  0.1000E-02   0.1000E-02   0.5000E+00
        2                                                                                  YES
                  0.8653E-02
                  0.0000E+00   0.4279E-01
                  0.0000E+00   0.0000E+00   0.1251E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.6743E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.6743E+00
        3                                                                                  YES
                  0.1000E-01
                  0.0000E+00   0.1600E+00
                  0.0000E+00   0.0000E+00   0.4000E-01
0INITIAL ESTIMATE OF SIGMA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.2500E+02
        2                                                                                  YES
                  0.1600E+02
0
 THETAI SUBROUTINE USER-SUPPLIED
 THETAR SUBROUTINE USER-SUPPLIED
 PRIOR SUBROUTINE USER-SUPPLIED
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
 NO. OF FUNCT. EVALS. ALLOWED:            3024
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
 RAW OUTPUT FILE (FILE): stanrb14_bayes.ext
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
 CONVERGENCE TYPE (CTYPE):                  0
 BURN-IN ITERATIONS (NBURN):                500
 ITERATIONS (NITER):                        0
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
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      1
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
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           -1
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):0
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
   1   2   3   4   5
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3   4   5
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration         -500 MCMCOBJ=    170022.704591851     
 iteration         -450 MCMCOBJ=    28524.8673475298     
 iteration         -400 MCMCOBJ=    27987.9090420068     
 iteration         -350 MCMCOBJ=    27944.3144116227     
 iteration         -300 MCMCOBJ=    27740.5210337188     
 iteration         -250 MCMCOBJ=    27340.9887951215     
 iteration         -200 MCMCOBJ=    27395.7360833738     
 iteration         -150 MCMCOBJ=    26913.5787311905     
 iteration         -100 MCMCOBJ=    27202.9285437297     
 iteration          -50 MCMCOBJ=    27612.6893076103     
 Sampling Mode
 iteration            0 MCMCOBJ=    27407.3922058125     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS NOT PERFORMED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27407.3922058125     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43948.2858034966     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27407.3922058125     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32369.6602851177     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    16.0970839384867     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27407.3922058125     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27423.4892897510     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    27.93
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27407.392       **************************************************
 #OBJS:********************************************        0.000 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.02E-02  3.70E+00 -4.97E+00 -1.12E+00 -1.16E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.03E-02
 
 ETA2
+        2.49E-02  1.57E-01
 
 ETA3
+       -2.27E-03 -3.56E-02  5.21E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.58E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.01E-01
 
 ETA2
+        6.21E-01  3.96E-01
 
 ETA3
+       -9.81E-02 -3.94E-01  2.28E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.97E+00
 
1
 
 
 #TBLN:      2
 #METH: NUTS Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            3024
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
 RAW OUTPUT FILE (FILE): stanrb14.ext
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
 CONVERGENCE TYPE (CTYPE):                  0
 BURN-IN ITERATIONS (NBURN):                1000
 ITERATIONS (NITER):                        2000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
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
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           -1
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):0
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      0
 MASS MATRIX ACCUMULATION ITERATIONS (MADAPT):          -1
 MASS MATRIX BLOCKING TYPE:                              B
 MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (NUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (KAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (NUTS_DELTA):                   0.800000000000000
 NUTS GAMMA SETTING (NUTS_GAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 3.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000
 NUTS WARMUP METHOD (NUTS_TEST):       0
 NUTS MAXIMAL DEPTH SEARCH (NUTS_MAXDEPTH):       10
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       7.500000000000000E-02
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): 2.500000000000000E-02
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 5.000000000000000E-02
 INITIAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPITER): 1
 INTERVAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPINTER):0
 ETA PARAMETERIZATION (NUTS_EPARAM):0
 OMEGA PARAMETERIZATION (NUTS_OPARAM):1
 SIGMA PARAMETERIZATION (NUTS_SPARAM):1
 NUTS REGULARIZING METHOD (NUTS_REG): 0.00000000000000

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3   4   5
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3   4   5
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -1000 MCMCOBJ=    28068.8413774990     
 iteration         -980 MCMCOBJ=    27524.0759706134     
 iteration         -960 MCMCOBJ=    27212.0149841708     
 iteration         -940 MCMCOBJ=    27004.9225405248     
 iteration         -920 MCMCOBJ=    26622.6271851988     
 iteration         -900 MCMCOBJ=    26559.7020610187     
 iteration         -880 MCMCOBJ=    26302.6479446963     
 iteration         -860 MCMCOBJ=    26173.1359503672     
 iteration         -840 MCMCOBJ=    26548.7726235500     
 iteration         -820 MCMCOBJ=    26772.6453300258     
 iteration         -800 MCMCOBJ=    26443.1425427856     
 iteration         -780 MCMCOBJ=    26104.8520882345     
 iteration         -760 MCMCOBJ=    25849.0938702507     
 iteration         -740 MCMCOBJ=    26570.0931884523     
 iteration         -720 MCMCOBJ=    26904.8463997607     
 iteration         -700 MCMCOBJ=    27443.4798523471     
 iteration         -680 MCMCOBJ=    27019.0658046270     
 iteration         -660 MCMCOBJ=    27082.8856290471     
 iteration         -640 MCMCOBJ=    26815.7936307196     
 iteration         -620 MCMCOBJ=    27057.9688985048     
 iteration         -600 MCMCOBJ=    27227.3035165925     
 iteration         -580 MCMCOBJ=    26870.5819307034     
 iteration         -560 MCMCOBJ=    27282.9228964719     
 iteration         -540 MCMCOBJ=    27311.9640514841     
 iteration         -520 MCMCOBJ=    27077.2898548536     
 iteration         -500 MCMCOBJ=    26981.1015688622     
 iteration         -480 MCMCOBJ=    26299.8709978692     
 iteration         -460 MCMCOBJ=    26847.1272595026     
 iteration         -440 MCMCOBJ=    26587.7143602765     
 iteration         -420 MCMCOBJ=    26671.4469267920     
 iteration         -400 MCMCOBJ=    26790.8717147994     
 iteration         -380 MCMCOBJ=    27168.6015410037     
 iteration         -360 MCMCOBJ=    27215.3127157971     
 iteration         -340 MCMCOBJ=    27178.6379827982     
 iteration         -320 MCMCOBJ=    26950.3825789917     
 iteration         -300 MCMCOBJ=    27486.1797230553     
 iteration         -280 MCMCOBJ=    27177.5591536781     
 iteration         -260 MCMCOBJ=    26711.0346959790     
 iteration         -240 MCMCOBJ=    26801.3677056742     
 iteration         -220 MCMCOBJ=    27106.6715143558     
 iteration         -200 MCMCOBJ=    27091.8762602073     
 iteration         -180 MCMCOBJ=    26925.4099705971     
 iteration         -160 MCMCOBJ=    27302.8578382669     
 iteration         -140 MCMCOBJ=    27643.1899641132     
 iteration         -120 MCMCOBJ=    27760.9290177040     
 iteration         -100 MCMCOBJ=    27190.5041507454     
 iteration          -80 MCMCOBJ=    27407.6986997463     
 iteration          -60 MCMCOBJ=    27575.4510962587     
 iteration          -40 MCMCOBJ=    27732.4496383559     
 iteration          -20 MCMCOBJ=    27666.9298314999     
 Sampling Mode
 iteration            0 MCMCOBJ=    27567.1273095581     
 iteration           20 MCMCOBJ=    26942.6092831808     
 iteration           40 MCMCOBJ=    27041.1188409865     
 iteration           60 MCMCOBJ=    27126.0994663487     
 iteration           80 MCMCOBJ=    27120.5994061845     
 iteration          100 MCMCOBJ=    27191.3830067302     
 iteration          120 MCMCOBJ=    26905.5825435377     
 iteration          140 MCMCOBJ=    27519.0077130427     
 iteration          160 MCMCOBJ=    27351.0533509243     
 iteration          180 MCMCOBJ=    27266.1661358116     
 iteration          200 MCMCOBJ=    27879.1611055795     
 iteration          220 MCMCOBJ=    27338.0774738909     
 iteration          240 MCMCOBJ=    27426.4222643879     
 iteration          260 MCMCOBJ=    27717.2330144953     
 iteration          280 MCMCOBJ=    27198.7430465315     
 iteration          300 MCMCOBJ=    27211.2594691567     
 iteration          320 MCMCOBJ=    27337.5190246322     
 iteration          340 MCMCOBJ=    27147.1050104447     
 iteration          360 MCMCOBJ=    27100.3599140127     
 iteration          380 MCMCOBJ=    27151.8663066235     
 iteration          400 MCMCOBJ=    27090.1863496181     
 iteration          420 MCMCOBJ=    27019.3530046116     
 iteration          440 MCMCOBJ=    26973.5142920489     
 iteration          460 MCMCOBJ=    27145.5267664456     
 iteration          480 MCMCOBJ=    27297.9817537521     
 iteration          500 MCMCOBJ=    26938.3884789221     
 iteration          520 MCMCOBJ=    27180.0391222260     
 iteration          540 MCMCOBJ=    26772.1521657490     
 iteration          560 MCMCOBJ=    26945.5833819664     
 iteration          580 MCMCOBJ=    26856.0394074552     
 iteration          600 MCMCOBJ=    26817.9990838324     
 iteration          620 MCMCOBJ=    26472.3749914803     
 iteration          640 MCMCOBJ=    26634.0734811750     
 iteration          660 MCMCOBJ=    27137.0017184405     
 iteration          680 MCMCOBJ=    27726.3068727283     
 iteration          700 MCMCOBJ=    27679.4771717320     
 iteration          720 MCMCOBJ=    27406.9188034942     
 iteration          740 MCMCOBJ=    27141.9583446445     
 iteration          760 MCMCOBJ=    26998.8060537299     
 iteration          780 MCMCOBJ=    26811.9837140650     
 iteration          800 MCMCOBJ=    27248.9585849346     
 iteration          820 MCMCOBJ=    27186.6342891637     
 iteration          840 MCMCOBJ=    26961.2752344780     
 iteration          860 MCMCOBJ=    26848.3504118739     
 iteration          880 MCMCOBJ=    27001.4325969495     
 iteration          900 MCMCOBJ=    27317.4886609330     
 iteration          920 MCMCOBJ=    27597.8580029363     
 iteration          940 MCMCOBJ=    27120.5051569086     
 iteration          960 MCMCOBJ=    26817.0077348153     
 iteration          980 MCMCOBJ=    26824.1637349273     
 iteration         1000 MCMCOBJ=    26718.9363121644     
 iteration         1020 MCMCOBJ=    26505.0972575317     
 iteration         1040 MCMCOBJ=    26912.4958675119     
 iteration         1060 MCMCOBJ=    27280.7652692117     
 iteration         1080 MCMCOBJ=    27357.5030804514     
 iteration         1100 MCMCOBJ=    27289.1104215403     
 iteration         1120 MCMCOBJ=    27158.3374148062     
 iteration         1140 MCMCOBJ=    26925.9212267670     
 iteration         1160 MCMCOBJ=    26524.4675219481     
 iteration         1180 MCMCOBJ=    26562.9097043264     
 iteration         1200 MCMCOBJ=    26551.3939177709     
 iteration         1220 MCMCOBJ=    26363.0662565184     
 iteration         1240 MCMCOBJ=    26656.7974830240     
 iteration         1260 MCMCOBJ=    26648.4902172864     
 iteration         1280 MCMCOBJ=    26898.5611200865     
 iteration         1300 MCMCOBJ=    26953.3099034915     
 iteration         1320 MCMCOBJ=    26319.6647435275     
 iteration         1340 MCMCOBJ=    26424.1355956379     
 iteration         1360 MCMCOBJ=    26591.8979099751     
 iteration         1380 MCMCOBJ=    26457.9556182959     
 iteration         1400 MCMCOBJ=    26611.9803948154     
 iteration         1420 MCMCOBJ=    26788.1297151716     
 iteration         1440 MCMCOBJ=    27020.6217980721     
 iteration         1460 MCMCOBJ=    27345.6585599379     
 iteration         1480 MCMCOBJ=    27530.9150574356     
 iteration         1500 MCMCOBJ=    27392.4203936462     
 iteration         1520 MCMCOBJ=    26966.4193514481     
 iteration         1540 MCMCOBJ=    26867.9267581236     
 iteration         1560 MCMCOBJ=    26381.4002365100     
 iteration         1580 MCMCOBJ=    26459.4288647815     
 iteration         1600 MCMCOBJ=    26379.2407587670     
 iteration         1620 MCMCOBJ=    27150.7121523245     
 iteration         1640 MCMCOBJ=    27285.8096928032     
 iteration         1660 MCMCOBJ=    27656.8599710502     
 iteration         1680 MCMCOBJ=    27572.7343995445     
 iteration         1700 MCMCOBJ=    27337.5812920682     
 iteration         1720 MCMCOBJ=    27909.2563280773     
 iteration         1740 MCMCOBJ=    27482.1097360640     
 iteration         1760 MCMCOBJ=    27810.7163509198     
 iteration         1780 MCMCOBJ=    27483.1426716584     
 iteration         1800 MCMCOBJ=    27160.3910928325     
 iteration         1820 MCMCOBJ=    27077.6731041780     
 iteration         1840 MCMCOBJ=    27349.6184694294     
 iteration         1860 MCMCOBJ=    27456.9650093786     
 iteration         1880 MCMCOBJ=    27454.1059544392     
 iteration         1900 MCMCOBJ=    27343.9256288876     
 iteration         1920 MCMCOBJ=    26875.5437598321     
 iteration         1940 MCMCOBJ=    26870.5438439858     
 iteration         1960 MCMCOBJ=    26982.3781857306     
 iteration         1980 MCMCOBJ=    27036.1308246947     
 iteration         2000 MCMCOBJ=    26745.5445897941     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27069.2661416961     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43610.1597393802     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27069.2661416961     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32031.5342210013     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -7.99194303480276     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27069.2661416961     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27061.2741986613     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   693.98
 Elapsed covariance  time in seconds:     0.01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27069.266       **************************************************
 #OBJS:********************************************      360.683 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.00E-02  3.67E+00 -4.99E+00 -9.15E-01 -1.17E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.92E-03
 
 ETA2
+        2.52E-02  1.61E-01
 
 ETA3
+       -3.14E-04 -3.95E-02  5.00E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.56E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.95E-02
 
 ETA2
+        6.31E-01  4.00E-01
 
 ETA3
+       -1.51E-02 -4.51E-01  2.23E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.96E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.02E-03  2.81E-02  4.16E-02  1.30E-01  1.62E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.65E-04
 
 ETA2
+        2.51E-03  1.34E-02
 
 ETA3
+        1.80E-03  7.22E-03  7.96E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.64E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.33E-03
 
 ETA2
+        4.54E-02  1.67E-02
 
 ETA3
+        8.19E-02  1.10E-01  1.77E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.33E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.62E-05
 
 TH 2
+        3.47E-05  7.89E-04
 
 TH 3
+        3.35E-05  5.74E-04  1.73E-03
 
 TH 4
+       -1.81E-05 -2.32E-03 -2.19E-03  1.68E-02
 
 TH 5
+       -2.47E-05 -2.87E-03 -2.65E-03  9.59E-03  2.62E-02
 
 OM11
+       -3.08E-07 -3.04E-08 -2.30E-06 -1.75E-06 -2.94E-06  4.43E-07
 
 OM12
+       -4.91E-07 -1.76E-06 -2.41E-05 -1.18E-06  2.23E-06  7.95E-07  6.29E-06
 
 OM13
+       -3.09E-07 -5.34E-06 -8.27E-06  1.58E-05  2.28E-05 -4.84E-08  1.96E-06  3.25E-06
 
 OM22
+       -4.00E-06 -4.90E-06 -1.69E-04 -9.65E-05 -7.13E-05  1.61E-06  1.99E-05  5.95E-06  1.80E-04
 
 OM23
+       -1.10E-06 -1.27E-05 -5.31E-05  5.35E-05  5.44E-05 -1.43E-07  3.71E-06  5.98E-06  3.96E-05  5.21E-05
 
 OM33
+        2.75E-06 -1.32E-05  8.25E-07  9.99E-05  8.06E-05 -2.63E-07  1.05E-06  2.08E-06  1.47E-05  2.98E-05  6.33E-05
 
 SG11
+        3.32E-05  1.18E-04 -5.15E-05  4.06E-04  6.86E-04 -8.31E-06  2.58E-05 -8.18E-07 -1.66E-04 -2.15E-04 -2.38E-04  6.95E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.02E-03
 
 TH 2
+        3.07E-01  2.81E-02
 
 TH 3
+        2.00E-01  4.91E-01  4.16E-02
 
 TH 4
+       -3.47E-02 -6.38E-01 -4.06E-01  1.30E-01
 
 TH 5
+       -3.79E-02 -6.32E-01 -3.94E-01  4.58E-01  1.62E-01
 
 OM11
+       -1.15E-01 -1.63E-03 -8.31E-02 -2.03E-02 -2.74E-02  6.65E-04
 
 OM12
+       -4.86E-02 -2.50E-02 -2.31E-01 -3.63E-03  5.49E-03  4.77E-01  2.51E-03
 
 OM13
+       -4.26E-02 -1.05E-01 -1.10E-01  6.78E-02  7.83E-02 -4.03E-02  4.33E-01  1.80E-03
 
 OM22
+       -7.41E-02 -1.30E-02 -3.02E-01 -5.55E-02 -3.29E-02  1.80E-01  5.91E-01  2.46E-01  1.34E-02
 
 OM23
+       -3.78E-02 -6.29E-02 -1.77E-01  5.72E-02  4.66E-02 -2.98E-02  2.05E-01  4.59E-01  4.09E-01  7.22E-03
 
 OM33
+        8.60E-02 -5.91E-02  2.49E-03  9.69E-02  6.26E-02 -4.97E-02  5.25E-02  1.45E-01  1.38E-01  5.20E-01  7.96E-03
 
 SG11
+        3.13E-02  1.60E-02 -4.69E-03  1.19E-02  1.61E-02 -4.74E-02  3.89E-02 -1.72E-03 -4.69E-02 -1.13E-01 -1.13E-01  2.64E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.76E+04
 
 TH 2
+       -6.34E+03  3.57E+03
 
 TH 3
+       -7.92E+02 -3.31E+02  9.43E+02
 
 TH 4
+       -6.09E+02  3.05E+02  6.11E+01  1.11E+02
 
 TH 5
+       -4.67E+02  2.39E+02  3.98E+01 -8.75E-01  6.87E+01
 
 OM11
+        5.86E+04 -1.53E+02 -1.12E+03 -9.42E+01  1.07E+02  3.42E+06
 
 OM12
+       -9.86E+03 -1.75E+03  1.74E+03 -7.83E+01 -2.76E+01 -6.60E+05  4.31E+05
 
 OM13
+        4.84E+03  3.25E+03 -2.03E+03 -2.13E+01 -1.08E+02  4.22E+05 -2.47E+05  5.46E+05
 
 OM22
+        4.28E+02  1.23E+02  6.85E+02  1.35E+02  6.89E+01  3.31E+04 -3.84E+04  1.73E+04  1.13E+04
 
 OM23
+        2.81E+03 -8.14E+02  7.28E+02 -2.78E+01 -6.72E+00 -2.24E+04  2.95E+04 -6.73E+04 -7.72E+03  4.05E+04
 
 OM33
+       -4.36E+03  4.89E+02 -6.57E+02 -1.05E+02 -2.78E+01  1.42E+04 -7.53E+03  1.64E+04  9.99E+02 -1.58E+04  2.33E+04
 
 SG11
+       -1.31E+01 -7.31E+00  1.73E+00 -9.27E-01 -7.56E-01  6.88E+02 -2.58E+02  2.94E+01  2.36E+01  3.93E+01  3.95E+01  1.49E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,      709.914
Stop Time: 
Mon 09/05/2016 
08:37 PM

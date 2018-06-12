Mon 09/05/2016 
08:13 PM
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

$SIGMAPD (1 FIX)

$EST METHOD=BAYES NBURN=500 NITER=0 PRINT=50 MASSRESET=1
file=stanrb13_bayes.ext KAPPA=0.75
$EST METHOD=NUTS NBURN=1000 NITER=2000 PRINT=20 file=stanrb13.ext
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
  0.1000E+01     0.1000E+01     0.1000E+01
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
 RAW OUTPUT FILE (FILE): stanrb13_bayes.ext
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
 iteration         -500 MCMCOBJ=    170018.845716026     
 iteration         -450 MCMCOBJ=    28521.0942398481     
 iteration         -400 MCMCOBJ=    27984.1357817329     
 iteration         -350 MCMCOBJ=    27940.5417007697     
 iteration         -300 MCMCOBJ=    27736.7473188563     
 iteration         -250 MCMCOBJ=    27337.2160288419     
 iteration         -200 MCMCOBJ=    27391.9631032986     
 iteration         -150 MCMCOBJ=    26909.8059128017     
 iteration         -100 MCMCOBJ=    27199.1553084459     
 iteration          -50 MCMCOBJ=    27608.9162213522     
 Sampling Mode
 iteration            0 MCMCOBJ=    27403.6191975157     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS NOT PERFORMED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27403.6191975157     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43944.5127951998     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27403.6191975157     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32365.8872768210     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    20.7075497271359     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27403.6191975157     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27424.3267472429     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    28.34
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27403.619       **************************************************
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
 RAW OUTPUT FILE (FILE): stanrb13.ext
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
 iteration        -1000 MCMCOBJ=    28065.0683646668     
 iteration         -980 MCMCOBJ=    27520.3228670552     
 iteration         -960 MCMCOBJ=    27154.3359837860     
 iteration         -940 MCMCOBJ=    26756.6952287115     
 iteration         -920 MCMCOBJ=    27197.2297581766     
 iteration         -900 MCMCOBJ=    26961.3155284387     
 iteration         -880 MCMCOBJ=    27228.6501481054     
 iteration         -860 MCMCOBJ=    27469.6827511916     
 iteration         -840 MCMCOBJ=    27289.7746266829     
 iteration         -820 MCMCOBJ=    27403.5892843516     
 iteration         -800 MCMCOBJ=    27402.4658423234     
 iteration         -780 MCMCOBJ=    27508.8714064172     
 iteration         -760 MCMCOBJ=    27493.4095639876     
 iteration         -740 MCMCOBJ=    27639.4046683160     
 iteration         -720 MCMCOBJ=    27346.5492775023     
 iteration         -700 MCMCOBJ=    27160.2524265203     
 iteration         -680 MCMCOBJ=    27272.7919205643     
 iteration         -660 MCMCOBJ=    27758.8332827419     
 iteration         -640 MCMCOBJ=    27198.8176174356     
 iteration         -620 MCMCOBJ=    27574.2232532055     
 iteration         -600 MCMCOBJ=    27306.8541745486     
 iteration         -580 MCMCOBJ=    27167.0722434370     
 iteration         -560 MCMCOBJ=    27309.9877001952     
 iteration         -540 MCMCOBJ=    27604.4433229875     
 iteration         -520 MCMCOBJ=    27426.4987076344     
 iteration         -500 MCMCOBJ=    27655.4926970842     
 iteration         -480 MCMCOBJ=    27389.0892295893     
 iteration         -460 MCMCOBJ=    27555.8846990868     
 iteration         -440 MCMCOBJ=    27415.6724566034     
 iteration         -420 MCMCOBJ=    27088.7189531060     
 iteration         -400 MCMCOBJ=    27298.2019523075     
 iteration         -380 MCMCOBJ=    27368.4902752016     
 iteration         -360 MCMCOBJ=    26812.9798484782     
 iteration         -340 MCMCOBJ=    27002.5055568833     
 iteration         -320 MCMCOBJ=    27193.5814200479     
 iteration         -300 MCMCOBJ=    27286.8665481305     
 iteration         -280 MCMCOBJ=    27435.4176084329     
 iteration         -260 MCMCOBJ=    26925.3591032182     
 iteration         -240 MCMCOBJ=    26764.5006456330     
 iteration         -220 MCMCOBJ=    26603.3438484483     
 iteration         -200 MCMCOBJ=    26888.1760266094     
 iteration         -180 MCMCOBJ=    26581.8099037769     
 iteration         -160 MCMCOBJ=    26766.0484997726     
 iteration         -140 MCMCOBJ=    26745.2440044391     
 iteration         -120 MCMCOBJ=    26868.6564191603     
 iteration         -100 MCMCOBJ=    26983.4206236039     
 iteration          -80 MCMCOBJ=    26753.2743217609     
 iteration          -60 MCMCOBJ=    26504.7470533297     
 iteration          -40 MCMCOBJ=    26988.5518700816     
 iteration          -20 MCMCOBJ=    26895.8104138684     
 Sampling Mode
 iteration            0 MCMCOBJ=    26354.0634468906     
 iteration           20 MCMCOBJ=    26745.4991848012     
 iteration           40 MCMCOBJ=    27410.4090305311     
 iteration           60 MCMCOBJ=    27437.4912720891     
 iteration           80 MCMCOBJ=    27363.9248616036     
 iteration          100 MCMCOBJ=    27419.6168790934     
 iteration          120 MCMCOBJ=    27086.8319978029     
 iteration          140 MCMCOBJ=    26893.1487735477     
 iteration          160 MCMCOBJ=    26839.5998729183     
 iteration          180 MCMCOBJ=    26807.0308238356     
 iteration          200 MCMCOBJ=    27171.6038456127     
 iteration          220 MCMCOBJ=    27002.2675491680     
 iteration          240 MCMCOBJ=    27218.7334886302     
 iteration          260 MCMCOBJ=    27281.2757124512     
 iteration          280 MCMCOBJ=    27433.1706367164     
 iteration          300 MCMCOBJ=    27245.8946020175     
 iteration          320 MCMCOBJ=    27347.6012249736     
 iteration          340 MCMCOBJ=    26963.7215414957     
 iteration          360 MCMCOBJ=    27188.4712993299     
 iteration          380 MCMCOBJ=    27299.9867415422     
 iteration          400 MCMCOBJ=    27435.3714646974     
 iteration          420 MCMCOBJ=    27414.8040440582     
 iteration          440 MCMCOBJ=    27189.2004847221     
 iteration          460 MCMCOBJ=    27512.2682335213     
 iteration          480 MCMCOBJ=    27163.6754104724     
 iteration          500 MCMCOBJ=    27131.6729014437     
 iteration          520 MCMCOBJ=    27015.4405326677     
 iteration          540 MCMCOBJ=    27008.3000435802     
 iteration          560 MCMCOBJ=    26949.5170879278     
 iteration          580 MCMCOBJ=    27036.5509256676     
 iteration          600 MCMCOBJ=    27057.9948159220     
 iteration          620 MCMCOBJ=    26675.8948321743     
 iteration          640 MCMCOBJ=    26932.3069501112     
 iteration          660 MCMCOBJ=    26810.5386585670     
 iteration          680 MCMCOBJ=    27664.8367996746     
 iteration          700 MCMCOBJ=    27855.4516715889     
 iteration          720 MCMCOBJ=    27711.2918996485     
 iteration          740 MCMCOBJ=    27706.5793415967     
 iteration          760 MCMCOBJ=    27397.3393784706     
 iteration          780 MCMCOBJ=    27167.6584638315     
 iteration          800 MCMCOBJ=    27557.3307687790     
 iteration          820 MCMCOBJ=    27169.2577126302     
 iteration          840 MCMCOBJ=    27452.1910615770     
 iteration          860 MCMCOBJ=    27184.3709402072     
 iteration          880 MCMCOBJ=    27074.0990693894     
 iteration          900 MCMCOBJ=    27211.0890487483     
 iteration          920 MCMCOBJ=    27417.3171719557     
 iteration          940 MCMCOBJ=    27063.2260716087     
 iteration          960 MCMCOBJ=    27535.2282750360     
 iteration          980 MCMCOBJ=    27413.9380266006     
 iteration         1000 MCMCOBJ=    27133.7999142871     
 iteration         1020 MCMCOBJ=    26947.7373868121     
 iteration         1040 MCMCOBJ=    26808.6779813368     
 iteration         1060 MCMCOBJ=    27563.1374194390     
 iteration         1080 MCMCOBJ=    27577.5135839384     
 iteration         1100 MCMCOBJ=    27313.5697603947     
 iteration         1120 MCMCOBJ=    27130.3231506027     
 iteration         1140 MCMCOBJ=    27016.0574099535     
 iteration         1160 MCMCOBJ=    26541.2661500162     
 iteration         1180 MCMCOBJ=    26310.6018023222     
 iteration         1200 MCMCOBJ=    26352.5119764361     
 iteration         1220 MCMCOBJ=    26093.2429598144     
 iteration         1240 MCMCOBJ=    26255.0835664134     
 iteration         1260 MCMCOBJ=    26433.4079978753     
 iteration         1280 MCMCOBJ=    27154.9556483750     
 iteration         1300 MCMCOBJ=    27295.9494653957     
 iteration         1320 MCMCOBJ=    27109.6270669699     
 iteration         1340 MCMCOBJ=    27172.9148823572     
 iteration         1360 MCMCOBJ=    27003.1413730535     
 iteration         1380 MCMCOBJ=    27304.2513993436     
 iteration         1400 MCMCOBJ=    27243.9947523757     
 iteration         1420 MCMCOBJ=    27492.0998825425     
 iteration         1440 MCMCOBJ=    27205.1888184913     
 iteration         1460 MCMCOBJ=    27364.6599497057     
 iteration         1480 MCMCOBJ=    27273.3130052208     
 iteration         1500 MCMCOBJ=    27359.2302913870     
 iteration         1520 MCMCOBJ=    27350.9806762710     
 iteration         1540 MCMCOBJ=    27593.2555989264     
 iteration         1560 MCMCOBJ=    27479.2559624904     
 iteration         1580 MCMCOBJ=    27582.9394963444     
 iteration         1600 MCMCOBJ=    27520.1406618179     
 iteration         1620 MCMCOBJ=    27167.6997890809     
 iteration         1640 MCMCOBJ=    27160.3212253581     
 iteration         1660 MCMCOBJ=    27274.6491214676     
 iteration         1680 MCMCOBJ=    27045.4740424902     
 iteration         1700 MCMCOBJ=    27356.0558701290     
 iteration         1720 MCMCOBJ=    27190.3712846462     
 iteration         1740 MCMCOBJ=    27258.4489666692     
 iteration         1760 MCMCOBJ=    27223.6350265978     
 iteration         1780 MCMCOBJ=    27550.2903567794     
 iteration         1800 MCMCOBJ=    27242.3138238873     
 iteration         1820 MCMCOBJ=    27128.4734218682     
 iteration         1840 MCMCOBJ=    27388.1792644318     
 iteration         1860 MCMCOBJ=    27190.0657629994     
 iteration         1880 MCMCOBJ=    27087.0867256638     
 iteration         1900 MCMCOBJ=    27114.3707488866     
 iteration         1920 MCMCOBJ=    27239.0867162850     
 iteration         1940 MCMCOBJ=    27259.5453950284     
 iteration         1960 MCMCOBJ=    27292.3833168840     
 iteration         1980 MCMCOBJ=    27478.7469244312     
 iteration         2000 MCMCOBJ=    26947.8090668274     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27194.4331219008     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43735.3267195849     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27194.4331219008     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32156.7012012060     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -3.38147724615355     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27194.4331219008     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27191.0516446546     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   645.91
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27194.433       **************************************************
 #OBJS:********************************************      326.191 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         9.94E-03  3.67E+00 -4.99E+00 -9.20E-01 -1.18E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.91E-03
 
 ETA2
+        2.48E-02  1.61E-01
 
 ETA3
+       -6.33E-04 -3.83E-02  5.18E-02
 


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
+        6.20E-01  4.01E-01
 
 ETA3
+       -2.85E-02 -4.27E-01  2.27E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.95E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.11E-03  2.71E-02  4.09E-02  1.32E-01  1.63E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.54E-04
 
 ETA2
+        2.45E-03  1.26E-02
 
 ETA3
+        1.82E-03  6.68E-03  7.63E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.60E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.28E-03
 
 ETA2
+        4.65E-02  1.56E-02
 
 ETA3
+        8.10E-02  9.79E-02  1.66E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.29E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.69E-05
 
 TH 2
+        2.78E-05  7.32E-04
 
 TH 3
+        3.56E-05  5.95E-04  1.68E-03
 
 TH 4
+        6.91E-06 -2.38E-03 -2.49E-03  1.74E-02
 
 TH 5
+       -2.09E-05 -2.95E-03 -3.07E-03  1.07E-02  2.64E-02
 
 OM11
+       -1.46E-07  5.01E-07 -2.90E-06 -7.80E-08 -4.70E-07  4.28E-07
 
 OM12
+       -2.56E-08  3.06E-06 -2.29E-05 -1.88E-05 -4.95E-06  6.88E-07  5.98E-06
 
 OM13
+        7.10E-08  3.69E-07 -3.11E-06 -1.12E-05 -2.66E-06 -8.34E-08  1.88E-06  3.32E-06
 
 OM22
+       -4.74E-06 -3.20E-06 -1.38E-04 -9.10E-05 -4.15E-05  1.08E-06  1.69E-05  5.71E-06  1.58E-04
 
 OM23
+        7.29E-07 -1.48E-06 -2.52E-05  3.03E-06  8.26E-06 -1.64E-07  2.93E-06  5.58E-06  3.15E-05  4.46E-05
 
 OM33
+        4.44E-06 -5.70E-06  8.76E-06  8.21E-05  7.07E-05 -1.57E-07  7.01E-07  1.22E-06  5.90E-06  2.46E-05  5.83E-05
 
 SG11
+        1.89E-05  1.08E-04  4.59E-04 -4.94E-04  2.81E-04 -1.51E-05 -4.13E-06 -1.35E-05 -1.45E-04 -1.16E-04 -1.65E-04  6.77E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.11E-03
 
 TH 2
+        2.50E-01  2.71E-02
 
 TH 3
+        2.11E-01  5.37E-01  4.09E-02
 
 TH 4
+        1.27E-02 -6.67E-01 -4.61E-01  1.32E-01
 
 TH 5
+       -3.12E-02 -6.71E-01 -4.61E-01  4.99E-01  1.63E-01
 
 OM11
+       -5.45E-02  2.83E-02 -1.08E-01 -9.03E-04 -4.42E-03  6.54E-04
 
 OM12
+       -2.55E-03  4.63E-02 -2.28E-01 -5.81E-02 -1.24E-02  4.30E-01  2.45E-03
 
 OM13
+        9.49E-03  7.49E-03 -4.18E-02 -4.64E-02 -8.99E-03 -7.01E-02  4.23E-01  1.82E-03
 
 OM22
+       -9.17E-02 -9.41E-03 -2.69E-01 -5.49E-02 -2.03E-02  1.32E-01  5.51E-01  2.50E-01  1.26E-02
 
 OM23
+        2.66E-02 -8.18E-03 -9.24E-02  3.44E-03  7.61E-03 -3.75E-02  1.80E-01  4.59E-01  3.75E-01  6.68E-03
 
 OM33
+        1.42E-01 -2.76E-02  2.80E-02  8.15E-02  5.70E-02 -3.15E-02  3.76E-02  8.80E-02  6.15E-02  4.83E-01  7.63E-03
 
 SG11
+        1.76E-02  1.54E-02  4.31E-02 -1.44E-02  6.65E-03 -8.85E-02 -6.49E-03 -2.84E-02 -4.44E-02 -6.66E-02 -8.31E-02  2.60E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.34E+04
 
 TH 2
+       -5.88E+03  4.13E+03
 
 TH 3
+       -1.33E+03 -3.59E+02  1.07E+03
 
 TH 4
+       -7.36E+02  3.46E+02  7.92E+01  1.18E+02
 
 TH 5
+       -4.43E+02  2.74E+02  5.40E+01  1.16E-01  7.46E+01
 
 OM11
+        3.97E+04 -4.50E+03 -5.65E+02 -8.32E+02 -2.07E+02  3.33E+06
 
 OM12
+       -1.42E+04 -2.82E+03  3.60E+03  2.98E+02 -1.44E+01 -5.94E+05  4.10E+05
 
 OM13
+        2.96E+03  2.56E+03 -2.42E+03  1.02E+02 -3.37E+01  4.12E+05 -2.36E+05  5.34E+05
 
 OM22
+        1.71E+03  1.86E+02  5.56E+02  9.15E+01  5.97E+01  3.26E+04 -3.50E+04  1.53E+04  1.16E+04
 
 OM23
+       -1.51E+02 -4.54E+02  5.79E+02  2.36E+01  2.42E+01 -3.21E+04  3.39E+04 -7.45E+04 -8.61E+03  4.54E+04
 
 OM33
+       -4.30E+03  2.22E+02 -5.83E+02 -1.16E+02 -5.70E+01  1.82E+04 -1.31E+04  2.34E+04  2.31E+03 -1.74E+04  2.48E+04
 
 SG11
+       -4.04E+00 -1.65E+00 -5.60E+00 -2.20E-01 -1.01E+00  8.41E+02 -2.17E+02  1.58E+02  1.97E+01 -6.37E+00  4.78E+01  1.52E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,      661.538
Stop Time: 
Mon 09/05/2016 
08:25 PM

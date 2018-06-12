Mon 09/05/2016 
07:59 PM
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

$EST METHOD=ITS NITER=0 file=stanrb12_its.ext
$EST METHOD=NUTS NBURN=1000 NITER=2000 PRINT=20 file=stanrb12.ext
  
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
 #METH: Iterative Two Stage
 
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
 RAW OUTPUT FILE (FILE): stanrb12_its.ext
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
 CONVERGENCE TYPE (CTYPE):                  0
 ITERATIONS (NITER):                        0
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
   1   2   3   4   5
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   43739.3487327051
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.1121E-03 -1.0864E-01  1.6909E-01
 SE:             3.9030E-03  1.1687E-02  9.1065E-03
 N:                     900         900         900
 
 P VAL.:         7.7570E-01  1.4853E-20  6.2288E-77
 
 ETAshrink(%):   8.3432E+01  5.0389E+01  6.1343E+01
 EBVshrink(%):   6.8202E-01  2.0376E+01  1.9329E+01
 EPSshrink(%):   3.1003E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    43739.3487327051     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       60280.2423303892     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    16.0970839384867     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    43739.3487327051     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43755.4458166436     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:     0.27
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    43739.349       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.00E-02  4.00E+00 -5.00E+00 -1.10E+00 -1.60E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        5.00E-01
 
 ETA2
+        1.00E-03  5.00E-01
 
 ETA3
+        1.00E-03  1.00E-03  5.00E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.50E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        7.07E-01
 
 ETA2
+        2.00E-03  7.07E-01
 
 ETA3
+        2.00E-03  2.00E-03  7.07E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        5.00E+00
 
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
 RAW OUTPUT FILE (FILE): stanrb12.ext
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
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1
 MASS MATRIX ACCUMULATION ITERATIONS (MADAPT):          -1
 MASS MATRIX BLOCKING TYPE:                              B
 MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (NUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (KAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (NUTS_DELTA):                   0.800000000000000
 NUTS GAMMA SETTING (NUTS_GAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 0.00000000000000
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
 iteration        -1000 MCMCOBJ=    31176.9903931158     
 iteration         -980 MCMCOBJ=    26958.0759049003     
 iteration         -960 MCMCOBJ=    27578.4163076490     
 iteration         -940 MCMCOBJ=    27284.5140450139     
 iteration         -920 MCMCOBJ=    26997.4206485723     
 iteration         -900 MCMCOBJ=    26742.9723611516     
 iteration         -880 MCMCOBJ=    27073.0497620556     
 iteration         -860 MCMCOBJ=    27416.6365119667     
 iteration         -840 MCMCOBJ=    27391.9446283624     
 iteration         -820 MCMCOBJ=    27352.6034046376     
 iteration         -800 MCMCOBJ=    27322.3998635993     
 iteration         -780 MCMCOBJ=    26816.5416520101     
 iteration         -760 MCMCOBJ=    27031.2792927886     
 iteration         -740 MCMCOBJ=    27505.3253904835     
 iteration         -720 MCMCOBJ=    27363.0566466794     
 iteration         -700 MCMCOBJ=    27443.1116255181     
 iteration         -680 MCMCOBJ=    27845.1566334766     
 iteration         -660 MCMCOBJ=    27497.9196962298     
 iteration         -640 MCMCOBJ=    27345.0702792945     
 iteration         -620 MCMCOBJ=    27339.5646158932     
 iteration         -600 MCMCOBJ=    27511.1282596779     
 iteration         -580 MCMCOBJ=    27606.1293388161     
 iteration         -560 MCMCOBJ=    27891.9309393133     
 iteration         -540 MCMCOBJ=    27600.5691938160     
 iteration         -520 MCMCOBJ=    27472.5672391532     
 iteration         -500 MCMCOBJ=    27312.0404304129     
 iteration         -480 MCMCOBJ=    27382.9852415554     
 iteration         -460 MCMCOBJ=    26861.6694094592     
 iteration         -440 MCMCOBJ=    26859.9575822381     
 iteration         -420 MCMCOBJ=    26892.7854491753     
 iteration         -400 MCMCOBJ=    27185.8926154186     
 iteration         -380 MCMCOBJ=    27074.9029281405     
 iteration         -360 MCMCOBJ=    27237.1959298903     
 iteration         -340 MCMCOBJ=    27570.0364682280     
 iteration         -320 MCMCOBJ=    27748.6069064259     
 iteration         -300 MCMCOBJ=    27215.5872629510     
 iteration         -280 MCMCOBJ=    27308.9462356586     
 iteration         -260 MCMCOBJ=    26731.5202256159     
 iteration         -240 MCMCOBJ=    27226.6047826385     
 iteration         -220 MCMCOBJ=    27193.5377625010     
 iteration         -200 MCMCOBJ=    27510.7130556985     
 iteration         -180 MCMCOBJ=    27159.6410936913     
 iteration         -160 MCMCOBJ=    27736.3534068348     
 iteration         -140 MCMCOBJ=    27773.2277688843     
 iteration         -120 MCMCOBJ=    27370.5929889190     
 iteration         -100 MCMCOBJ=    27551.8808386234     
 iteration          -80 MCMCOBJ=    27390.2545140154     
 iteration          -60 MCMCOBJ=    27361.4658828095     
 iteration          -40 MCMCOBJ=    26940.1951694198     
 iteration          -20 MCMCOBJ=    26854.4929339982     
 Sampling Mode
 iteration            0 MCMCOBJ=    26916.2215308693     
 iteration           20 MCMCOBJ=    26735.8386555101     
 iteration           40 MCMCOBJ=    27146.6051633518     
 iteration           60 MCMCOBJ=    27539.0527689285     
 iteration           80 MCMCOBJ=    27791.5456012816     
 iteration          100 MCMCOBJ=    27633.9820802980     
 iteration          120 MCMCOBJ=    27370.4397555942     
 iteration          140 MCMCOBJ=    27203.2951303192     
 iteration          160 MCMCOBJ=    26934.8641112946     
 iteration          180 MCMCOBJ=    26999.5016715652     
 iteration          200 MCMCOBJ=    26947.6397805260     
 iteration          220 MCMCOBJ=    26710.2425076461     
 iteration          240 MCMCOBJ=    26873.8891434427     
 iteration          260 MCMCOBJ=    26540.7590015339     
 iteration          280 MCMCOBJ=    27041.4886395917     
 iteration          300 MCMCOBJ=    27129.0590499810     
 iteration          320 MCMCOBJ=    27001.0243283184     
 iteration          340 MCMCOBJ=    26968.3614482249     
 iteration          360 MCMCOBJ=    27107.9219148202     
 iteration          380 MCMCOBJ=    26954.1940185847     
 iteration          400 MCMCOBJ=    26985.9246581276     
 iteration          420 MCMCOBJ=    26891.4518017472     
 iteration          440 MCMCOBJ=    27181.5111822713     
 iteration          460 MCMCOBJ=    27342.1466599455     
 iteration          480 MCMCOBJ=    27443.5937477090     
 iteration          500 MCMCOBJ=    27689.7420516269     
 iteration          520 MCMCOBJ=    27605.6381973141     
 iteration          540 MCMCOBJ=    27616.8143464055     
 iteration          560 MCMCOBJ=    27547.3553057220     
 iteration          580 MCMCOBJ=    27602.9718324554     
 iteration          600 MCMCOBJ=    27017.1795526558     
 iteration          620 MCMCOBJ=    27107.7334533678     
 iteration          640 MCMCOBJ=    27480.5126203264     
 iteration          660 MCMCOBJ=    27480.2206306548     
 iteration          680 MCMCOBJ=    27072.7865117180     
 iteration          700 MCMCOBJ=    26862.7945095643     
 iteration          720 MCMCOBJ=    27245.1623293310     
 iteration          740 MCMCOBJ=    27519.2939584455     
 iteration          760 MCMCOBJ=    27393.2606086583     
 iteration          780 MCMCOBJ=    27390.7944443302     
 iteration          800 MCMCOBJ=    27351.6189859713     
 iteration          820 MCMCOBJ=    27342.6830925819     
 iteration          840 MCMCOBJ=    27390.7856591028     
 iteration          860 MCMCOBJ=    27387.4147462869     
 iteration          880 MCMCOBJ=    26949.9371014090     
 iteration          900 MCMCOBJ=    27527.0201619514     
 iteration          920 MCMCOBJ=    27556.3206709324     
 iteration          940 MCMCOBJ=    27124.8273946893     
 iteration          960 MCMCOBJ=    27164.6010101883     
 iteration          980 MCMCOBJ=    27214.4740691645     
 iteration         1000 MCMCOBJ=    27262.8914359131     
 iteration         1020 MCMCOBJ=    26855.8922845724     
 iteration         1040 MCMCOBJ=    27163.0571864972     
 iteration         1060 MCMCOBJ=    27384.5329483441     
 iteration         1080 MCMCOBJ=    27385.4514602846     
 iteration         1100 MCMCOBJ=    27145.2119403274     
 iteration         1120 MCMCOBJ=    27349.7243197468     
 iteration         1140 MCMCOBJ=    27336.3385644585     
 iteration         1160 MCMCOBJ=    27116.4575746534     
 iteration         1180 MCMCOBJ=    27365.2619109526     
 iteration         1200 MCMCOBJ=    27563.3877734176     
 iteration         1220 MCMCOBJ=    27497.2319042751     
 iteration         1240 MCMCOBJ=    27556.2863725082     
 iteration         1260 MCMCOBJ=    27066.0577600667     
 iteration         1280 MCMCOBJ=    27533.8965503361     
 iteration         1300 MCMCOBJ=    27565.9367018522     
 iteration         1320 MCMCOBJ=    27107.6190044825     
 iteration         1340 MCMCOBJ=    27112.9012930877     
 iteration         1360 MCMCOBJ=    27370.5842479844     
 iteration         1380 MCMCOBJ=    27366.3616206000     
 iteration         1400 MCMCOBJ=    27517.0974578626     
 iteration         1420 MCMCOBJ=    27442.2705916642     
 iteration         1440 MCMCOBJ=    27396.4928602235     
 iteration         1460 MCMCOBJ=    27025.0698498580     
 iteration         1480 MCMCOBJ=    27117.0447331893     
 iteration         1500 MCMCOBJ=    27367.2715050123     
 iteration         1520 MCMCOBJ=    27675.1429486544     
 iteration         1540 MCMCOBJ=    27329.9888718005     
 iteration         1560 MCMCOBJ=    27134.8155975599     
 iteration         1580 MCMCOBJ=    27404.6916975751     
 iteration         1600 MCMCOBJ=    27146.6909758573     
 iteration         1620 MCMCOBJ=    27475.6418000248     
 iteration         1640 MCMCOBJ=    27599.3553632535     
 iteration         1660 MCMCOBJ=    27179.0925109925     
 iteration         1680 MCMCOBJ=    27548.2651300782     
 iteration         1700 MCMCOBJ=    27297.9966671558     
 iteration         1720 MCMCOBJ=    27633.1831612035     
 iteration         1740 MCMCOBJ=    27498.7040001852     
 iteration         1760 MCMCOBJ=    27395.7373836391     
 iteration         1780 MCMCOBJ=    27289.0906768915     
 iteration         1800 MCMCOBJ=    27488.4108963002     
 iteration         1820 MCMCOBJ=    27211.2937569636     
 iteration         1840 MCMCOBJ=    26931.7120249362     
 iteration         1860 MCMCOBJ=    27049.0030745011     
 iteration         1880 MCMCOBJ=    27426.7245443415     
 iteration         1900 MCMCOBJ=    27258.6322176641     
 iteration         1920 MCMCOBJ=    27554.6436299237     
 iteration         1940 MCMCOBJ=    27350.3935081626     
 iteration         1960 MCMCOBJ=    27352.1951994544     
 iteration         1980 MCMCOBJ=    27236.8816660646     
 iteration         2000 MCMCOBJ=    27151.6967136838     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27273.3830094964     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43814.2766071805     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27273.3830094964     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32235.6510888017     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    16.0970839384867     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27273.3830094964     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27289.4800934349     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   872.38
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27273.383       **************************************************
 #OBJS:********************************************      247.941 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         9.78E-03  3.67E+00 -5.00E+00 -9.10E-01 -1.16E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.98E-03
 
 ETA2
+        2.49E-02  1.65E-01
 
 ETA3
+       -8.19E-04 -3.73E-02  5.25E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.56E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.98E-02
 
 ETA2
+        6.13E-01  4.05E-01
 
 ETA3
+       -3.68E-02 -4.08E-01  2.29E-01
 


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
 
         3.88E-03  2.87E-02  4.43E-02  1.32E-01  1.66E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.50E-04
 
 ETA2
+        2.53E-03  1.34E-02
 
 ETA3
+        1.87E-03  6.78E-03  7.00E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.63E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.25E-03
 
 ETA2
+        4.43E-02  1.65E-02
 
 ETA3
+        8.20E-02  9.37E-02  1.52E-02
 


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
+        1.50E-05
 
 TH 2
+        2.58E-05  8.25E-04
 
 TH 3
+        2.28E-05  6.84E-04  1.96E-03
 
 TH 4
+        9.67E-06 -2.54E-03 -2.65E-03  1.73E-02
 
 TH 5
+        6.04E-06 -3.20E-03 -3.53E-03  1.09E-02  2.74E-02
 
 OM11
+       -7.59E-08  8.86E-07 -1.70E-06 -3.32E-06 -3.55E-06  4.23E-07
 
 OM12
+        5.35E-08  3.01E-06 -2.22E-05 -2.00E-05 -1.24E-05  7.44E-07  6.42E-06
 
 OM13
+       -3.36E-08  8.82E-08 -4.38E-06  4.17E-06  6.17E-06 -6.57E-08  2.16E-06  3.48E-06
 
 OM22
+       -4.08E-06 -7.44E-07 -1.55E-04 -7.86E-05 -9.06E-05  1.56E-06  2.18E-05  7.65E-06  1.79E-04
 
 OM23
+       -7.09E-07 -1.05E-05 -3.86E-05  7.44E-05  7.25E-05 -1.55E-07  4.39E-06  6.94E-06  2.98E-05  4.60E-05
 
 OM33
+        1.37E-06 -1.04E-05  1.31E-05  6.59E-05  8.07E-05 -1.57E-07  1.62E-06  1.98E-06  7.02E-06  2.12E-05  4.89E-05
 
 SG11
+        8.91E-06 -2.62E-04  1.79E-04  1.89E-04  1.15E-03 -1.57E-05 -1.64E-05 -1.21E-05 -1.78E-04 -1.28E-04 -1.31E-04  6.92E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        3.88E-03
 
 TH 2
+        2.31E-01  2.87E-02
 
 TH 3
+        1.33E-01  5.37E-01  4.43E-02
 
 TH 4
+        1.89E-02 -6.72E-01 -4.54E-01  1.32E-01
 
 TH 5
+        9.42E-03 -6.73E-01 -4.81E-01  5.00E-01  1.66E-01
 
 OM11
+       -3.01E-02  4.74E-02 -5.91E-02 -3.88E-02 -3.29E-02  6.50E-04
 
 OM12
+        5.45E-03  4.13E-02 -1.98E-01 -6.01E-02 -2.96E-02  4.51E-01  2.53E-03
 
 OM13
+       -4.64E-03  1.65E-03 -5.30E-02  1.70E-02  2.00E-02 -5.42E-02  4.56E-01  1.87E-03
 
 OM22
+       -7.86E-02 -1.93E-03 -2.61E-01 -4.46E-02 -4.09E-02  1.79E-01  6.44E-01  3.06E-01  1.34E-02
 
 OM23
+       -2.70E-02 -5.38E-02 -1.29E-01  8.33E-02  6.46E-02 -3.52E-02  2.56E-01  5.49E-01  3.29E-01  6.78E-03
 
 OM33
+        5.07E-02 -5.15E-02  4.24E-02  7.15E-02  6.97E-02 -3.45E-02  9.12E-02  1.51E-01  7.50E-02  4.47E-01  7.00E-03
 
 SG11
+        8.74E-03 -3.47E-02  1.54E-02  5.47E-03  2.64E-02 -9.17E-02 -2.46E-02 -2.47E-02 -5.05E-02 -7.16E-02 -7.09E-02  2.63E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        8.02E+04
 
 TH 2
+       -6.32E+03  3.74E+03
 
 TH 3
+       -6.36E+02 -3.24E+02  8.99E+02
 
 TH 4
+       -7.33E+02  3.38E+02  5.96E+01  1.17E+02
 
 TH 5
+       -5.42E+02  2.61E+02  5.86E+01  1.47E+00  7.47E+01
 
 OM11
+        3.83E+04 -4.43E+03 -2.11E+03 -8.31E+02 -3.46E+02  3.51E+06
 
 OM12
+       -1.71E+04 -6.39E+02  2.60E+03  4.67E+02  1.00E+02 -6.66E+05  4.47E+05
 
 OM13
+        6.74E+03 -2.87E+02 -2.76E+03 -3.26E+02 -2.59E+02  4.35E+05 -2.40E+05  5.62E+05
 
 OM22
+        1.89E+03  9.91E+01  5.55E+02  7.07E+01  8.44E+01  3.39E+04 -3.99E+04  1.13E+04  1.11E+04
 
 OM23
+        1.80E+03 -6.46E+02  6.53E+02 -9.19E+01 -1.44E+01 -2.05E+04  2.37E+04 -7.84E+04 -5.25E+03  4.24E+04
 
 OM33
+       -2.21E+03  4.79E+02 -8.23E+02 -5.77E+01 -7.12E+01  2.20E+04 -1.34E+04  2.05E+04  1.29E+03 -1.55E+04  2.73E+04
 
 SG11
+       -1.18E+01  9.31E+00 -3.87E+00  6.43E-01 -3.79E-01  7.96E+02 -1.81E+02  7.34E+01  1.86E+01  1.90E+01  3.73E+01  1.48E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,      860.579
Stop Time: 
Mon 09/05/2016 
08:13 PM

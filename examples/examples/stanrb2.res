Wed 03/09/2016 
09:42 PM
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

$OMEGAPD (4 FIX)

$SIGMAP BLOCK(1)
16.0 FIX

$SIGMAPD (2 FIX)

$EST METHOD=ITS NITER=0 file=stanrb2_its.ext
$EST METHOD=NUTS NBURN=500 NITER=2000 PRINT=20 file=stanrb2.ext
     WISHTYPE=1 OLKJDF=6.0 NUTSREG=1.0
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  121) INTERACTION IS IMPLIED WITH EM/BAYES ESTIMATION METHODS

 (MU_WARNING 6) THETA(003): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.

 (MU_WARNING 6) THETA(002): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        9 MAR 2016
Days until program expires :5193
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha9 (nm74a9)
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
  0.4000E+01     0.4000E+01     0.4000E+01
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
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): stanrb2_its.ext
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

 iteration            0 OBJ=   43737.6892825437
 
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
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    43737.6892825437     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       60278.5828802278     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    19.9404013334959     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    43737.6892825437     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43757.6296838772     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:     0.34
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    43737.689       **************************************************
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
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): stanrb2.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):1
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
 NO U-TURN BAYES SAMPLING TYPE:                          TSOI
 THE FOLLOWING PERTAIN TO THETAS/OMEGAS/SIGMAS/ETAS
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1
 MASS MATRIX ACCUMULATION ITERATIONS (PMADAPT):          -1
 MASS MATRIX BLOCKING TYPE:                              B
 POP. MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (PNUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (PKAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (PDELTA):                   0.800000000000000
 NUTS GAMMA SETTING (PGAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 6.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000
 STAN WARMUP METHOD (NUTSTEST):       NO
 STAN STAGE I WARMUP ITERATIONS (NUTSINIT):       0.150000000000000
 STAN STAGE II base WARMUP ITERATIONS (NUTSBASE): 5.000000000000000E-02
 STAN STAGE III FINAL ITERATIONS (NUTSTERM): 0.100000000000000
 INITIAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (STEPITER): 1
 INTERVAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (STEPINTER):0
 MASS MATRIX REPARAMETERIZATION (NUTS_REPARAM):0
 OBJECTIVE FUNCTION TYPE (NUTS_OBJTYPE):0
 NUTS CHOLESKY BOUNDARY (NUTSCHOLBND): 0.00000000000000
 NUTS REGULARIZING METHOD (NUTSREG): 1.00000000000000

 
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
 iteration         -500 MCMCOBJ=    28664.2635239218     
 iteration         -480 MCMCOBJ=    27708.8458246718     
 iteration         -460 MCMCOBJ=    27459.7372599304     
 iteration         -440 MCMCOBJ=    27544.3996349738     
 iteration         -420 MCMCOBJ=    27743.4937073206     
 iteration         -400 MCMCOBJ=    27609.2627692933     
 iteration         -380 MCMCOBJ=    27555.0656596796     
 iteration         -360 MCMCOBJ=    27313.6722628967     
 iteration         -340 MCMCOBJ=    28217.4440111997     
 iteration         -320 MCMCOBJ=    27599.4665866159     
 iteration         -300 MCMCOBJ=    27681.7758586211     
 iteration         -280 MCMCOBJ=    27474.0909401315     
 iteration         -260 MCMCOBJ=    27798.3081614794     
 iteration         -240 MCMCOBJ=    27373.6229526734     
 iteration         -220 MCMCOBJ=    27502.7234653343     
 iteration         -200 MCMCOBJ=    27274.3737133273     
 iteration         -180 MCMCOBJ=    27609.3694213808     
 iteration         -160 MCMCOBJ=    27384.6778740559     
 iteration         -140 MCMCOBJ=    27261.8935433771     
 iteration         -120 MCMCOBJ=    27579.1299550881     
 iteration         -100 MCMCOBJ=    27273.0640754587     
 iteration          -80 MCMCOBJ=    27072.7876304394     
 iteration          -60 MCMCOBJ=    27117.8597764778     
 iteration          -40 MCMCOBJ=    27116.2725970751     
 iteration          -20 MCMCOBJ=    27638.4180567848     
 Sampling Mode
 iteration            0 MCMCOBJ=    27549.5200535985     
 iteration           20 MCMCOBJ=    27393.8439625786     
 iteration           40 MCMCOBJ=    27116.6404704174     
 iteration           60 MCMCOBJ=    27165.6558208509     
 iteration           80 MCMCOBJ=    26845.5223761352     
 iteration          100 MCMCOBJ=    27420.4240586799     
 iteration          120 MCMCOBJ=    27633.9005638320     
 iteration          140 MCMCOBJ=    27442.5984215213     
 iteration          160 MCMCOBJ=    27693.4857488750     
 iteration          180 MCMCOBJ=    27631.0319722462     
 iteration          200 MCMCOBJ=    27309.4861578435     
 iteration          220 MCMCOBJ=    27023.8003781595     
 iteration          240 MCMCOBJ=    27416.2550454333     
 iteration          260 MCMCOBJ=    26899.8743340611     
 iteration          280 MCMCOBJ=    27097.8538815996     
 iteration          300 MCMCOBJ=    26699.0756207220     
 iteration          320 MCMCOBJ=    26766.4192158374     
 iteration          340 MCMCOBJ=    26636.2334797525     
 iteration          360 MCMCOBJ=    26503.4857763752     
 iteration          380 MCMCOBJ=    26477.0288417741     
 iteration          400 MCMCOBJ=    26905.6276602110     
 iteration          420 MCMCOBJ=    27431.9467848114     
 iteration          440 MCMCOBJ=    27044.8892826152     
 iteration          460 MCMCOBJ=    27502.5820350470     
 iteration          480 MCMCOBJ=    27158.6561288746     
 iteration          500 MCMCOBJ=    27475.4890098280     
 iteration          520 MCMCOBJ=    27193.5718140607     
 iteration          540 MCMCOBJ=    27538.9365325640     
 iteration          560 MCMCOBJ=    27858.0116556514     
 iteration          580 MCMCOBJ=    27638.8440727915     
 iteration          600 MCMCOBJ=    27206.2179873988     
 iteration          620 MCMCOBJ=    27470.0990978353     
 iteration          640 MCMCOBJ=    27395.7183677154     
 iteration          660 MCMCOBJ=    27288.3488088057     
 iteration          680 MCMCOBJ=    27110.2745361138     
 iteration          700 MCMCOBJ=    27438.5585542504     
 iteration          720 MCMCOBJ=    27343.4916737944     
 iteration          740 MCMCOBJ=    27291.7284814487     
 iteration          760 MCMCOBJ=    27284.0687041097     
 iteration          780 MCMCOBJ=    27143.1725937549     
 iteration          800 MCMCOBJ=    27151.2365672859     
 iteration          820 MCMCOBJ=    26920.7735438892     
 iteration          840 MCMCOBJ=    27004.2433179857     
 iteration          860 MCMCOBJ=    26826.7218841986     
 iteration          880 MCMCOBJ=    26670.1917878805     
 iteration          900 MCMCOBJ=    26821.9358262623     
 iteration          920 MCMCOBJ=    26693.6806696272     
 iteration          940 MCMCOBJ=    26533.4879378908     
 iteration          960 MCMCOBJ=    26475.9954552295     
 iteration          980 MCMCOBJ=    26677.9690390559     
 iteration         1000 MCMCOBJ=    26758.0930776137     
 iteration         1020 MCMCOBJ=    26971.9688574464     
 iteration         1040 MCMCOBJ=    26874.6401317553     
 iteration         1060 MCMCOBJ=    27642.7777244918     
 iteration         1080 MCMCOBJ=    27233.5971558914     
 iteration         1100 MCMCOBJ=    27116.4419501948     
 iteration         1120 MCMCOBJ=    27196.0544276295     
 iteration         1140 MCMCOBJ=    27128.8725710703     
 iteration         1160 MCMCOBJ=    27158.3942661133     
 iteration         1180 MCMCOBJ=    27295.2862831582     
 iteration         1200 MCMCOBJ=    27208.4629357636     
 iteration         1220 MCMCOBJ=    27107.3089301882     
 iteration         1240 MCMCOBJ=    27007.0714435877     
 iteration         1260 MCMCOBJ=    27124.6347070475     
 iteration         1280 MCMCOBJ=    27231.7954285067     
 iteration         1300 MCMCOBJ=    27094.7894349347     
 iteration         1320 MCMCOBJ=    27293.9676434552     
 iteration         1340 MCMCOBJ=    27218.3250718391     
 iteration         1360 MCMCOBJ=    26900.9569537924     
 iteration         1380 MCMCOBJ=    27307.2875854961     
 iteration         1400 MCMCOBJ=    27642.1699914449     
 iteration         1420 MCMCOBJ=    27676.1384812518     
 iteration         1440 MCMCOBJ=    28088.3226226751     
 iteration         1460 MCMCOBJ=    27532.5379893297     
 iteration         1480 MCMCOBJ=    27642.1095765981     
 iteration         1500 MCMCOBJ=    27498.8834609576     
 iteration         1520 MCMCOBJ=    27416.5561243688     
 iteration         1540 MCMCOBJ=    27461.3152762640     
 iteration         1560 MCMCOBJ=    27404.7801293173     
 iteration         1580 MCMCOBJ=    26857.7488921780     
 iteration         1600 MCMCOBJ=    27036.0128761976     
 iteration         1620 MCMCOBJ=    27099.5832893774     
 iteration         1640 MCMCOBJ=    27002.4666861702     
 iteration         1660 MCMCOBJ=    26813.5014775137     
 iteration         1680 MCMCOBJ=    26644.9715026050     
 iteration         1700 MCMCOBJ=    26855.6653642661     
 iteration         1720 MCMCOBJ=    26600.0588345780     
 iteration         1740 MCMCOBJ=    26993.3482626079     
 iteration         1760 MCMCOBJ=    27505.4380032873     
 iteration         1780 MCMCOBJ=    27266.8062242213     
 iteration         1800 MCMCOBJ=    27349.1186955770     
 iteration         1820 MCMCOBJ=    27365.8900905674     
 iteration         1840 MCMCOBJ=    27115.0043491765     
 iteration         1860 MCMCOBJ=    26939.7297413955     
 iteration         1880 MCMCOBJ=    26974.7283701868     
 iteration         1900 MCMCOBJ=    27369.6288249989     
 iteration         1920 MCMCOBJ=    27518.0841632874     
 iteration         1940 MCMCOBJ=    27157.3680597461     
 iteration         1960 MCMCOBJ=    26983.3872971399     
 iteration         1980 MCMCOBJ=    27005.5497612080     
 iteration         2000 MCMCOBJ=    26448.9861410383     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27168.2384036620     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43709.1320013461     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27168.2384036620     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32130.5064829672     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -10.0300439664667     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27168.2384036620     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27158.2083596955     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  1842.64
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27168.238       **************************************************
 #OBJS:********************************************      334.173 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         9.99E-03  3.67E+00 -5.00E+00 -9.05E-01 -1.16E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.89E-03
 
 ETA2
+        2.47E-02  1.60E-01
 
 ETA3
+       -5.72E-04 -3.87E-02  5.09E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.56E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.94E-02
 
 ETA2
+        6.21E-01  4.00E-01
 
 ETA3
+       -2.60E-02 -4.37E-01  2.25E-01
 


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
 
         3.91E-03  2.75E-02  4.19E-02  1.24E-01  1.56E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.45E-04
 
 ETA2
+        2.57E-03  1.40E-02
 
 ETA3
+        1.83E-03  7.13E-03  7.22E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.61E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.24E-03
 
 ETA2
+        4.92E-02  1.75E-02
 
 ETA3
+        8.18E-02  1.04E-01  1.59E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.30E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.53E-05
 
 TH 2
+        2.76E-05  7.57E-04
 
 TH 3
+        3.01E-05  5.52E-04  1.75E-03
 
 TH 4
+       -3.82E-06 -2.12E-03 -2.04E-03  1.54E-02
 
 TH 5
+        1.42E-06 -2.72E-03 -2.64E-03  8.73E-03  2.42E-02
 
 OM11
+       -1.42E-07  2.37E-07 -2.47E-06  7.55E-07 -4.36E-07  4.17E-07
 
 OM12
+       -9.32E-08 -1.11E-06 -3.22E-05  4.63E-06  1.61E-05  7.21E-07  6.58E-06
 
 OM13
+        1.88E-07 -2.23E-06 -1.11E-05  4.14E-06  8.55E-06 -7.17E-08  2.38E-06  3.36E-06
 
 OM22
+       -5.37E-06  1.35E-06 -2.23E-04 -2.84E-05 -1.45E-04  1.53E-06  1.98E-05  6.88E-06  1.97E-04
 
 OM23
+       -9.30E-07 -3.30E-06 -5.92E-05  1.43E-05 -4.45E-05 -2.91E-08  3.58E-06  5.51E-06  4.57E-05  5.08E-05
 
 OM33
+        2.46E-06  9.64E-06  1.15E-05  1.37E-06 -8.88E-06 -1.10E-07  3.54E-07  9.90E-07  1.13E-05  2.38E-05  5.21E-05
 
 SG11
+        2.87E-06 -4.09E-06  3.33E-04 -1.18E-03  1.40E-03 -9.21E-06  1.68E-05 -4.41E-06 -3.05E-04 -1.26E-04 -1.32E-04  6.80E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        3.91E-03
 
 TH 2
+        2.56E-01  2.75E-02
 
 TH 3
+        1.84E-01  4.79E-01  4.19E-02
 
 TH 4
+       -7.89E-03 -6.23E-01 -3.93E-01  1.24E-01
 
 TH 5
+        2.33E-03 -6.35E-01 -4.05E-01  4.53E-01  1.56E-01
 
 OM11
+       -5.62E-02  1.34E-02 -9.15E-02  9.44E-03 -4.34E-03  6.45E-04
 
 OM12
+       -9.29E-03 -1.58E-02 -2.99E-01  1.45E-02  4.04E-02  4.35E-01  2.57E-03
 
 OM13
+        2.63E-02 -4.43E-02 -1.45E-01  1.82E-02  3.00E-02 -6.06E-02  5.05E-01  1.83E-03
 
 OM22
+       -9.78E-02  3.48E-03 -3.80E-01 -1.63E-02 -6.66E-02  1.69E-01  5.50E-01  2.67E-01  1.40E-02
 
 OM23
+       -3.33E-02 -1.68E-02 -1.98E-01  1.62E-02 -4.01E-02 -6.32E-03  1.96E-01  4.22E-01  4.56E-01  7.13E-03
 
 OM33
+        8.70E-02  4.85E-02  3.81E-02  1.53E-03 -7.90E-03 -2.35E-02  1.91E-02  7.48E-02  1.12E-01  4.63E-01  7.22E-03
 
 SG11
+        2.82E-03 -5.70E-04  3.05E-02 -3.66E-02  3.45E-02 -5.47E-02  2.51E-02 -9.22E-03 -8.32E-02 -6.77E-02 -7.01E-02  2.61E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.89E+04
 
 TH 2
+       -5.71E+03  3.52E+03
 
 TH 3
+       -9.39E+02 -3.30E+02  1.02E+03
 
 TH 4
+       -5.98E+02  2.99E+02  5.61E+01  1.15E+02
 
 TH 5
+       -5.15E+02  2.51E+02  5.87E+01 -1.65E+00  7.76E+01
 
 OM11
+        3.40E+04 -1.59E+03 -2.30E+03 -5.68E+02  1.40E+02  3.55E+06
 
 OM12
+       -9.12E+03 -2.06E+03  2.62E+03  8.45E+01 -2.82E+02 -6.59E+05  4.26E+05
 
 OM13
+       -6.10E+03  2.33E+03 -1.63E+03  1.07E+02  2.64E+01  5.41E+05 -2.99E+05  5.88E+05
 
 OM22
+        1.44E+03 -1.86E+02  8.64E+02  5.88E+01  1.15E+02  2.84E+04 -3.35E+04  1.87E+04  1.07E+04
 
 OM23
+        1.21E+03  7.84E+01  6.68E+02 -5.90E-01  9.81E+01 -4.35E+04  3.83E+04 -7.04E+04 -8.75E+03  4.08E+04
 
 OM33
+       -3.11E+03 -2.99E+02 -6.01E+02 -5.44E+01 -9.30E+01  1.65E+04 -9.42E+03  2.08E+04  1.51E+03 -1.60E+04  2.63E+04
 
 SG11
+        1.02E+01  1.29E+00 -2.31E+00  1.87E+00 -1.27E+00  7.54E+02 -3.17E+02  1.89E+02  4.23E+01 -1.99E+01  3.81E+01  1.52E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     1832.091
Stop Time: 
Wed 03/09/2016 
10:13 PM

Mon 09/05/2016 
09:46 PM
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

$EST METHOD=ITS NITER=0 file=stanrb19_its.ext
$EST METHOD=NUTS NBURN=1000 NITER=2000 PRINT=20 file=stanrb19.ext
     NUTS_EPARAM=2 OLKJDF=1 NUTS_MASS=BD
  
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
 RAW OUTPUT FILE (FILE): stanrb19_its.ext
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
 RAW OUTPUT FILE (FILE): stanrb19.ext
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
 MASS MATRIX BLOCKING TYPE:                              BD
 MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (NUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (KAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (NUTS_DELTA):                   0.800000000000000
 NUTS GAMMA SETTING (NUTS_GAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 1.00000000000000
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
 ETA PARAMETERIZATION (NUTS_EPARAM):2
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
 iteration        -1000 MCMCOBJ=    34129.0587852261     
 iteration         -980 MCMCOBJ=    36183.8597799850     
 iteration         -960 MCMCOBJ=    36200.6670047161     
 iteration         -940 MCMCOBJ=    36365.9339753772     
 iteration         -920 MCMCOBJ=    36271.6984986791     
 iteration         -900 MCMCOBJ=    36271.8601091526     
 iteration         -880 MCMCOBJ=    36313.5419574539     
 iteration         -860 MCMCOBJ=    36620.3842684164     
 iteration         -840 MCMCOBJ=    36556.0473337754     
 iteration         -820 MCMCOBJ=    36577.5564991101     
 iteration         -800 MCMCOBJ=    36504.8394522009     
 iteration         -780 MCMCOBJ=    36436.8031724572     
 iteration         -760 MCMCOBJ=    36677.5205113393     
 iteration         -740 MCMCOBJ=    36338.9492555161     
 iteration         -720 MCMCOBJ=    36462.6762627319     
 iteration         -700 MCMCOBJ=    36491.3488572153     
 iteration         -680 MCMCOBJ=    36503.2853927451     
 iteration         -660 MCMCOBJ=    36397.1042144853     
 iteration         -640 MCMCOBJ=    36439.2102213116     
 iteration         -620 MCMCOBJ=    36509.1289623083     
 iteration         -600 MCMCOBJ=    36626.8563764942     
 iteration         -580 MCMCOBJ=    36468.1147175718     
 iteration         -560 MCMCOBJ=    36368.5005589921     
 iteration         -540 MCMCOBJ=    36393.8951447124     
 iteration         -520 MCMCOBJ=    36225.7228187963     
 iteration         -500 MCMCOBJ=    36397.1451155428     
 iteration         -480 MCMCOBJ=    36665.7907399313     
 iteration         -460 MCMCOBJ=    36376.7501514718     
 iteration         -440 MCMCOBJ=    36427.4710045127     
 iteration         -420 MCMCOBJ=    36605.8794510074     
 iteration         -400 MCMCOBJ=    36558.4386754092     
 iteration         -380 MCMCOBJ=    36450.8405821609     
 iteration         -360 MCMCOBJ=    36509.5173959164     
 iteration         -340 MCMCOBJ=    36299.4496711511     
 iteration         -320 MCMCOBJ=    36580.1631424634     
 iteration         -300 MCMCOBJ=    36694.1341669949     
 iteration         -280 MCMCOBJ=    36435.1120626923     
 iteration         -260 MCMCOBJ=    36408.1266078491     
 iteration         -240 MCMCOBJ=    36333.8164515655     
 iteration         -220 MCMCOBJ=    36350.4503021709     
 iteration         -200 MCMCOBJ=    36482.6276549904     
 iteration         -180 MCMCOBJ=    36513.4790131698     
 iteration         -160 MCMCOBJ=    36501.1760990250     
 iteration         -140 MCMCOBJ=    36674.5006766998     
 iteration         -120 MCMCOBJ=    36322.1845775987     
 iteration         -100 MCMCOBJ=    36596.1623104856     
 iteration          -80 MCMCOBJ=    36533.7559480457     
 iteration          -60 MCMCOBJ=    36482.8593813386     
 iteration          -40 MCMCOBJ=    36535.5546285140     
 iteration          -20 MCMCOBJ=    36634.2457233060     
 Sampling Mode
 iteration            0 MCMCOBJ=    36402.6021461803     
 iteration           20 MCMCOBJ=    36553.6484855249     
 iteration           40 MCMCOBJ=    36541.6891779096     
 iteration           60 MCMCOBJ=    36388.7602459803     
 iteration           80 MCMCOBJ=    36423.1262541754     
 iteration          100 MCMCOBJ=    36675.1759995595     
 iteration          120 MCMCOBJ=    36406.8360924100     
 iteration          140 MCMCOBJ=    36391.3446088597     
 iteration          160 MCMCOBJ=    36410.3749743589     
 iteration          180 MCMCOBJ=    36529.7331119788     
 iteration          200 MCMCOBJ=    36509.6083723143     
 iteration          220 MCMCOBJ=    36568.4510860226     
 iteration          240 MCMCOBJ=    36427.9563852273     
 iteration          260 MCMCOBJ=    36724.1413560165     
 iteration          280 MCMCOBJ=    36530.3392212178     
 iteration          300 MCMCOBJ=    36494.1421339050     
 iteration          320 MCMCOBJ=    36326.0500749436     
 iteration          340 MCMCOBJ=    36594.4484110334     
 iteration          360 MCMCOBJ=    36577.8749731881     
 iteration          380 MCMCOBJ=    36578.9803699673     
 iteration          400 MCMCOBJ=    36301.9848533966     
 iteration          420 MCMCOBJ=    36389.7699529379     
 iteration          440 MCMCOBJ=    36380.6270363316     
 iteration          460 MCMCOBJ=    36452.9790905661     
 iteration          480 MCMCOBJ=    36501.1453610740     
 iteration          500 MCMCOBJ=    36473.6824237954     
 iteration          520 MCMCOBJ=    36476.3938832589     
 iteration          540 MCMCOBJ=    36567.9865462281     
 iteration          560 MCMCOBJ=    36384.3258483125     
 iteration          580 MCMCOBJ=    36448.4323127937     
 iteration          600 MCMCOBJ=    36621.7561763366     
 iteration          620 MCMCOBJ=    36590.7399032300     
 iteration          640 MCMCOBJ=    36389.8048946908     
 iteration          660 MCMCOBJ=    36362.2034755074     
 iteration          680 MCMCOBJ=    36691.9891391303     
 iteration          700 MCMCOBJ=    36530.7678923590     
 iteration          720 MCMCOBJ=    36579.6949393658     
 iteration          740 MCMCOBJ=    36408.8063830575     
 iteration          760 MCMCOBJ=    36420.9830322817     
 iteration          780 MCMCOBJ=    36462.0839817301     
 iteration          800 MCMCOBJ=    36596.8985647362     
 iteration          820 MCMCOBJ=    36487.9549975002     
 iteration          840 MCMCOBJ=    36611.9085203518     
 iteration          860 MCMCOBJ=    36561.6359617482     
 iteration          880 MCMCOBJ=    36294.5961509265     
 iteration          900 MCMCOBJ=    36551.2412700851     
 iteration          920 MCMCOBJ=    36604.7627170591     
 iteration          940 MCMCOBJ=    36238.8892325787     
 iteration          960 MCMCOBJ=    36265.5043286677     
 iteration          980 MCMCOBJ=    36634.0851749992     
 iteration         1000 MCMCOBJ=    36559.3503765505     
 iteration         1020 MCMCOBJ=    36766.5434614578     
 iteration         1040 MCMCOBJ=    36517.8994686725     
 iteration         1060 MCMCOBJ=    36379.6124611679     
 iteration         1080 MCMCOBJ=    36368.7132115901     
 iteration         1100 MCMCOBJ=    36441.6545006367     
 iteration         1120 MCMCOBJ=    36568.3268102973     
 iteration         1140 MCMCOBJ=    36453.1942621782     
 iteration         1160 MCMCOBJ=    36554.2978129466     
 iteration         1180 MCMCOBJ=    36430.6483789574     
 iteration         1200 MCMCOBJ=    36580.7045394389     
 iteration         1220 MCMCOBJ=    36458.8776170441     
 iteration         1240 MCMCOBJ=    36565.1474420471     
 iteration         1260 MCMCOBJ=    36449.3908955311     
 iteration         1280 MCMCOBJ=    36431.3106148483     
 iteration         1300 MCMCOBJ=    36434.8309958207     
 iteration         1320 MCMCOBJ=    36521.9424957946     
 iteration         1340 MCMCOBJ=    36568.9403300630     
 iteration         1360 MCMCOBJ=    36503.2957758707     
 iteration         1380 MCMCOBJ=    36488.9843024837     
 iteration         1400 MCMCOBJ=    36496.4973476910     
 iteration         1420 MCMCOBJ=    36230.9392835732     
 iteration         1440 MCMCOBJ=    36411.3684440389     
 iteration         1460 MCMCOBJ=    36604.4019961885     
 iteration         1480 MCMCOBJ=    36552.0008689729     
 iteration         1500 MCMCOBJ=    36652.7492623979     
 iteration         1520 MCMCOBJ=    36646.6802786684     
 iteration         1540 MCMCOBJ=    36405.2714618030     
 iteration         1560 MCMCOBJ=    36495.6409855817     
 iteration         1580 MCMCOBJ=    36502.9055738989     
 iteration         1600 MCMCOBJ=    36431.9402907465     
 iteration         1620 MCMCOBJ=    36435.2590885746     
 iteration         1640 MCMCOBJ=    36609.4286114401     
 iteration         1660 MCMCOBJ=    36561.5059856354     
 iteration         1680 MCMCOBJ=    36435.9119913200     
 iteration         1700 MCMCOBJ=    36549.6847980706     
 iteration         1720 MCMCOBJ=    36468.3176918328     
 iteration         1740 MCMCOBJ=    36330.1047131092     
 iteration         1760 MCMCOBJ=    36520.2328416319     
 iteration         1780 MCMCOBJ=    36367.8427095232     
 iteration         1800 MCMCOBJ=    36298.9325236787     
 iteration         1820 MCMCOBJ=    36574.7339807293     
 iteration         1840 MCMCOBJ=    36487.6288072596     
 iteration         1860 MCMCOBJ=    36811.3093253104     
 iteration         1880 MCMCOBJ=    36462.4039181043     
 iteration         1900 MCMCOBJ=    36372.8211810135     
 iteration         1920 MCMCOBJ=    36439.0278849880     
 iteration         1940 MCMCOBJ=    36354.5179276421     
 iteration         1960 MCMCOBJ=    36641.9530845964     
 iteration         1980 MCMCOBJ=    36483.2302306189     
 iteration         2000 MCMCOBJ=    36419.3010578609     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    36480.3350854722     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       53021.2286831563     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    36480.3350854722     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       41442.6031647774     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -4.85471119897507     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    36480.3350854722     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       36475.4803742732     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  5729.58
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    36480.335       **************************************************
 #OBJS:********************************************      105.006 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.00E-02  3.67E+00 -4.99E+00 -9.16E-01 -1.17E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.94E-03
 
 ETA2
+        2.59E-02  1.57E-01
 
 ETA3
+        1.28E-04 -4.38E-02  4.56E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.57E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.97E-02
 
 ETA2
+        6.55E-01  3.96E-01
 
 ETA3
+        5.01E-03 -5.30E-01  2.13E-01
 


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
 
         4.06E-03  2.81E-02  4.40E-02  1.27E-01  1.60E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.48E-04
 
 ETA2
+        2.57E-03  1.31E-02
 
 ETA3
+        1.84E-03  7.33E-03  7.50E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.79E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.24E-03
 
 ETA2
+        4.78E-02  1.65E-02
 
 ETA3
+        8.61E-02  1.22E-01  1.75E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.52E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.65E-05
 
 TH 2
+        2.92E-05  7.92E-04
 
 TH 3
+        3.70E-05  6.38E-04  1.93E-03
 
 TH 4
+        1.77E-05 -2.33E-03 -2.38E-03  1.61E-02
 
 TH 5
+       -1.59E-05 -3.05E-03 -3.00E-03  9.85E-03  2.56E-02
 
 OM11
+       -2.99E-07 -3.88E-07 -2.59E-06  1.82E-06 -7.47E-07  4.20E-07
 
 OM12
+       -6.18E-07  2.63E-06 -2.75E-05 -2.92E-06 -2.34E-05  7.55E-07  6.61E-06
 
 OM13
+        2.66E-08  5.32E-07 -5.41E-06 -3.65E-06 -8.38E-06 -8.39E-09  2.09E-06  3.38E-06
 
 OM22
+       -6.68E-06  2.09E-05 -1.79E-04 -1.39E-04 -1.86E-04  1.52E-06  1.97E-05  4.86E-06  1.71E-04
 
 OM23
+       -1.25E-06  6.37E-07 -4.53E-05  1.45E-05 -2.14E-05 -2.10E-07  2.48E-06  5.84E-06  3.42E-05  5.37E-05
 
 OM33
+        2.44E-06 -2.36E-06 -2.68E-06  7.87E-05  4.60E-05 -2.56E-07  5.01E-07  2.03E-06  1.27E-05  3.05E-05  5.62E-05
 
 SG11
+       -2.02E-05  3.36E-04  2.11E-04 -1.98E-03 -2.65E-03 -1.77E-05 -8.46E-07 -1.31E-06 -4.95E-05 -1.39E-04 -2.13E-04  7.79E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.06E-03
 
 TH 2
+        2.55E-01  2.81E-02
 
 TH 3
+        2.07E-01  5.16E-01  4.40E-02
 
 TH 4
+        3.44E-02 -6.52E-01 -4.26E-01  1.27E-01
 
 TH 5
+       -2.45E-02 -6.77E-01 -4.26E-01  4.85E-01  1.60E-01
 
 OM11
+       -1.14E-01 -2.13E-02 -9.08E-02  2.21E-02 -7.20E-03  6.48E-04
 
 OM12
+       -5.92E-02  3.63E-02 -2.44E-01 -8.93E-03 -5.70E-02  4.53E-01  2.57E-03
 
 OM13
+        3.57E-03  1.03E-02 -6.69E-02 -1.56E-02 -2.85E-02 -7.04E-03  4.43E-01  1.84E-03
 
 OM22
+       -1.26E-01  5.67E-02 -3.11E-01 -8.38E-02 -8.89E-02  1.79E-01  5.87E-01  2.02E-01  1.31E-02
 
 OM23
+       -4.20E-02  3.09E-03 -1.41E-01  1.55E-02 -1.83E-02 -4.41E-02  1.32E-01  4.34E-01  3.56E-01  7.33E-03
 
 OM33
+        8.02E-02 -1.12E-02 -8.13E-03  8.27E-02  3.83E-02 -5.26E-02  2.60E-02  1.47E-01  1.30E-01  5.55E-01  7.50E-03
 
 SG11
+       -1.78E-02  4.28E-02  1.72E-02 -5.58E-02 -5.94E-02 -9.79E-02 -1.18E-03 -2.55E-03 -1.35E-02 -6.81E-02 -1.02E-01  2.79E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.67E+04
 
 TH 2
+       -6.37E+03  3.90E+03
 
 TH 3
+       -9.53E+02 -3.65E+02  9.23E+02
 
 TH 4
+       -8.23E+02  3.43E+02  6.44E+01  1.23E+02
 
 TH 5
+       -4.89E+02  2.82E+02  4.81E+01  1.64E+00  7.83E+01
 
 OM11
+        5.17E+04  1.38E+03 -2.61E+03 -3.04E+02 -6.20E+01  3.40E+06
 
 OM12
+       -6.68E+03 -2.85E+03  2.37E+03 -3.93E+02  1.08E+02 -5.94E+05  4.19E+05
 
 OM13
+       -4.64E+03  2.88E+03 -1.65E+03  3.69E+02  2.69E+01  3.63E+05 -2.61E+05  5.36E+05
 
 OM22
+        1.66E+03 -1.78E+02  7.87E+02  1.51E+02  6.61E+01  3.12E+04 -4.05E+04  2.26E+04  1.22E+04
 
 OM23
+        3.17E+03 -5.26E+02  6.41E+02 -3.75E+01  4.95E+01 -2.68E+04  3.92E+04 -7.09E+04 -8.14E+03  4.06E+04
 
 OM33
+       -3.65E+03 -3.43E+01 -5.53E+02 -1.42E+02 -7.06E+01  1.63E+04 -8.81E+03  1.78E+04  1.05E+03 -1.82E+04  2.75E+04
 
 SG11
+        2.07E+01  2.32E-02  1.63E+00  8.88E-01  1.16E+00  7.93E+02 -1.17E+02  2.62E+01  8.29E+00  1.29E+01  4.19E+01  1.32E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     5714.597
Stop Time: 
Mon 09/05/2016 
11:21 PM

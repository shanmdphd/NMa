Thu 09/08/2016 
05:31 PM
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

$EST METHOD=ITS NITER=0 file=stanrb10_its.ext
$EST METHOD=NUTS NBURN=1000 NITER=2000 PRINT=20 file=stanrb10.ext
     OLKJDF=3.0
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  121) INTERACTION IS IMPLIED WITH EM/BAYES ESTIMATION METHODS

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.

 (MU_WARNING 6) THETA(003): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.

 (MU_WARNING 6) THETA(002): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        8 SEP 2016
Days until program expires :5014
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
 RAW OUTPUT FILE (FILE): stanrb10_its.ext
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
 Elapsed estimation  time in seconds:     0.22
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
 RAW OUTPUT FILE (FILE): stanrb10.ext
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
 iteration        -1000 MCMCOBJ=    31189.1663890637     
 iteration         -980 MCMCOBJ=    27230.6363615397     
 iteration         -960 MCMCOBJ=    26764.0273064844     
 iteration         -940 MCMCOBJ=    26944.9265645645     
 iteration         -920 MCMCOBJ=    26873.6284961972     
 iteration         -900 MCMCOBJ=    26745.9939461171     
 iteration         -880 MCMCOBJ=    26974.5899407456     
 iteration         -860 MCMCOBJ=    27116.9380548246     
 iteration         -840 MCMCOBJ=    27339.1144188912     
 iteration         -820 MCMCOBJ=    27492.6733278289     
 iteration         -800 MCMCOBJ=    27178.6010694481     
 iteration         -780 MCMCOBJ=    26966.1459512079     
 iteration         -760 MCMCOBJ=    26932.7407171101     
 iteration         -740 MCMCOBJ=    26672.2052186863     
 iteration         -720 MCMCOBJ=    26703.0256524740     
 iteration         -700 MCMCOBJ=    26667.4551744736     
 iteration         -680 MCMCOBJ=    27180.6605361173     
 iteration         -660 MCMCOBJ=    26601.9209975320     
 iteration         -640 MCMCOBJ=    26038.9501961386     
 iteration         -620 MCMCOBJ=    26378.7792489311     
 iteration         -600 MCMCOBJ=    26145.9810432549     
 iteration         -580 MCMCOBJ=    26242.4527655033     
 iteration         -560 MCMCOBJ=    26539.8126145660     
 iteration         -540 MCMCOBJ=    26329.0174488351     
 iteration         -520 MCMCOBJ=    26477.3955195589     
 iteration         -500 MCMCOBJ=    26886.9456103602     
 iteration         -480 MCMCOBJ=    27001.8311163968     
 iteration         -460 MCMCOBJ=    26492.9258953087     
 iteration         -440 MCMCOBJ=    27060.1598564822     
 iteration         -420 MCMCOBJ=    26970.0564633708     
 iteration         -400 MCMCOBJ=    27122.5603823046     
 iteration         -380 MCMCOBJ=    27331.5059711780     
 iteration         -360 MCMCOBJ=    27566.0350946435     
 iteration         -340 MCMCOBJ=    27394.7595326466     
 iteration         -320 MCMCOBJ=    27303.6136196184     
 iteration         -300 MCMCOBJ=    27127.3795811184     
 iteration         -280 MCMCOBJ=    27333.2315243013     
 iteration         -260 MCMCOBJ=    26905.6243259916     
 iteration         -240 MCMCOBJ=    27510.9185005469     
 iteration         -220 MCMCOBJ=    27471.3229081067     
 iteration         -200 MCMCOBJ=    27467.5237759646     
 iteration         -180 MCMCOBJ=    27259.3732878638     
 iteration         -160 MCMCOBJ=    27728.6194762863     
 iteration         -140 MCMCOBJ=    27426.1810437998     
 iteration         -120 MCMCOBJ=    27319.0515380987     
 iteration         -100 MCMCOBJ=    27134.6240036691     
 iteration          -80 MCMCOBJ=    26716.0678870431     
 iteration          -60 MCMCOBJ=    26646.3005911931     
 iteration          -40 MCMCOBJ=    26839.8529396765     
 iteration          -20 MCMCOBJ=    26851.8915259749     
 Sampling Mode
 iteration            0 MCMCOBJ=    26799.6003542769     
 iteration           20 MCMCOBJ=    26735.6325115267     
 iteration           40 MCMCOBJ=    26856.2614283411     
 iteration           60 MCMCOBJ=    26803.0410760703     
 iteration           80 MCMCOBJ=    26930.0737939405     
 iteration          100 MCMCOBJ=    26838.7804240057     
 iteration          120 MCMCOBJ=    26890.7916632221     
 iteration          140 MCMCOBJ=    27446.2489280511     
 iteration          160 MCMCOBJ=    27575.4965883840     
 iteration          180 MCMCOBJ=    27192.9874570553     
 iteration          200 MCMCOBJ=    26862.6122774749     
 iteration          220 MCMCOBJ=    27428.0736490555     
 iteration          240 MCMCOBJ=    27187.6161178873     
 iteration          260 MCMCOBJ=    27249.3817702505     
 iteration          280 MCMCOBJ=    27614.5071084846     
 iteration          300 MCMCOBJ=    27638.8084634214     
 iteration          320 MCMCOBJ=    27185.8623698190     
 iteration          340 MCMCOBJ=    27260.7817134677     
 iteration          360 MCMCOBJ=    27165.8092814126     
 iteration          380 MCMCOBJ=    27405.0532139148     
 iteration          400 MCMCOBJ=    27643.5304150776     
 iteration          420 MCMCOBJ=    27122.2531015788     
 iteration          440 MCMCOBJ=    27030.0200911158     
 iteration          460 MCMCOBJ=    27081.1167679202     
 iteration          480 MCMCOBJ=    26992.8940365165     
 iteration          500 MCMCOBJ=    26889.5282040129     
 iteration          520 MCMCOBJ=    26887.0937412913     
 iteration          540 MCMCOBJ=    27061.7609914356     
 iteration          560 MCMCOBJ=    27144.7858656434     
 iteration          580 MCMCOBJ=    27214.4387187088     
 iteration          600 MCMCOBJ=    27125.2604222273     
 iteration          620 MCMCOBJ=    27356.0847673137     
 iteration          640 MCMCOBJ=    27218.8807994281     
 iteration          660 MCMCOBJ=    27355.9974696739     
 iteration          680 MCMCOBJ=    27258.5273028100     
 iteration          700 MCMCOBJ=    27614.6515248902     
 iteration          720 MCMCOBJ=    27226.2577659963     
 iteration          740 MCMCOBJ=    27328.7053194787     
 iteration          760 MCMCOBJ=    27123.8594301102     
 iteration          780 MCMCOBJ=    27500.3392455157     
 iteration          800 MCMCOBJ=    27427.9427951287     
 iteration          820 MCMCOBJ=    27524.0170268553     
 iteration          840 MCMCOBJ=    27320.9959547925     
 iteration          860 MCMCOBJ=    27234.3742249545     
 iteration          880 MCMCOBJ=    27085.6047024198     
 iteration          900 MCMCOBJ=    27266.1578473227     
 iteration          920 MCMCOBJ=    27416.0339832314     
 iteration          940 MCMCOBJ=    27650.9381135291     
 iteration          960 MCMCOBJ=    27195.3972435092     
 iteration          980 MCMCOBJ=    27373.2429665094     
 iteration         1000 MCMCOBJ=    27276.7545943083     
 iteration         1020 MCMCOBJ=    27619.8005936587     
 iteration         1040 MCMCOBJ=    27717.7117866297     
 iteration         1060 MCMCOBJ=    27359.9692209583     
 iteration         1080 MCMCOBJ=    27029.0062135505     
 iteration         1100 MCMCOBJ=    26905.4256174347     
 iteration         1120 MCMCOBJ=    26832.9829732441     
 iteration         1140 MCMCOBJ=    26378.1922355391     
 iteration         1160 MCMCOBJ=    26516.0756660480     
 iteration         1180 MCMCOBJ=    26724.6725274410     
 iteration         1200 MCMCOBJ=    27475.5813358245     
 iteration         1220 MCMCOBJ=    27439.7566360100     
 iteration         1240 MCMCOBJ=    27225.1592511972     
 iteration         1260 MCMCOBJ=    27568.6836952590     
 iteration         1280 MCMCOBJ=    27513.3530221880     
 iteration         1300 MCMCOBJ=    27456.5471358840     
 iteration         1320 MCMCOBJ=    27542.2965928311     
 iteration         1340 MCMCOBJ=    26953.7959004125     
 iteration         1360 MCMCOBJ=    26907.0613784517     
 iteration         1380 MCMCOBJ=    27251.3801140488     
 iteration         1400 MCMCOBJ=    27139.5783756765     
 iteration         1420 MCMCOBJ=    27599.8095818035     
 iteration         1440 MCMCOBJ=    27393.4057786561     
 iteration         1460 MCMCOBJ=    27317.0575184546     
 iteration         1480 MCMCOBJ=    27430.8529032139     
 iteration         1500 MCMCOBJ=    27222.2027935165     
 iteration         1520 MCMCOBJ=    27393.0374445229     
 iteration         1540 MCMCOBJ=    27082.0488063070     
 iteration         1560 MCMCOBJ=    27452.5565782018     
 iteration         1580 MCMCOBJ=    27267.4121425847     
 iteration         1600 MCMCOBJ=    26974.7018004066     
 iteration         1620 MCMCOBJ=    27100.8859406840     
 iteration         1640 MCMCOBJ=    27233.5066655648     
 iteration         1660 MCMCOBJ=    27499.6063433701     
 iteration         1680 MCMCOBJ=    27440.2025596409     
 iteration         1700 MCMCOBJ=    27344.7914824352     
 iteration         1720 MCMCOBJ=    27510.2744999631     
 iteration         1740 MCMCOBJ=    27513.6996343017     
 iteration         1760 MCMCOBJ=    27430.0612455232     
 iteration         1780 MCMCOBJ=    27112.7315867201     
 iteration         1800 MCMCOBJ=    27189.2636929718     
 iteration         1820 MCMCOBJ=    27175.1298338787     
 iteration         1840 MCMCOBJ=    26689.1007253776     
 iteration         1860 MCMCOBJ=    26878.6653321043     
 iteration         1880 MCMCOBJ=    26839.0732605093     
 iteration         1900 MCMCOBJ=    26921.9410968376     
 iteration         1920 MCMCOBJ=    27180.4922854689     
 iteration         1940 MCMCOBJ=    26543.4593186162     
 iteration         1960 MCMCOBJ=    26322.3769034329     
 iteration         1980 MCMCOBJ=    26308.7297091517     
 iteration         2000 MCMCOBJ=    26072.1034640042     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27181.8152979181     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43722.7088956022     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27181.8152979181     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32144.0833772234     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -7.99194303480276     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27181.8152979181     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27173.8233548833     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   904.69
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27181.815       **************************************************
 #OBJS:********************************************      318.582 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.00E-02  3.67E+00 -4.99E+00 -9.11E-01 -1.17E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.94E-03
 
 ETA2
+        2.52E-02  1.62E-01
 
 ETA3
+       -1.68E-04 -3.77E-02  5.19E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.56E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.96E-02
 
 ETA2
+        6.28E-01  4.02E-01
 
 ETA3
+       -8.23E-03 -4.19E-01  2.27E-01
 


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
 
         4.11E-03  2.85E-02  4.23E-02  1.33E-01  1.62E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.77E-04
 
 ETA2
+        2.53E-03  1.34E-02
 
 ETA3
+        1.81E-03  7.04E-03  7.36E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.57E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.39E-03
 
 ETA2
+        4.36E-02  1.66E-02
 
 ETA3
+        7.96E-02  1.02E-01  1.63E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.25E-02
 
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
+        2.98E-05  8.12E-04
 
 TH 3
+        2.89E-05  6.38E-04  1.79E-03
 
 TH 4
+        1.06E-05 -2.46E-03 -2.71E-03  1.78E-02
 
 TH 5
+        6.40E-06 -3.00E-03 -3.29E-03  1.10E-02  2.63E-02
 
 OM11
+       -1.13E-07 -1.00E-08 -2.75E-06 -4.09E-06  2.22E-06  4.58E-07
 
 OM12
+       -5.79E-07 -1.93E-07 -2.62E-05 -1.92E-05 -6.67E-06  8.23E-07  6.42E-06
 
 OM13
+       -1.24E-07 -2.22E-07 -5.96E-06 -8.08E-06 -7.73E-06 -5.73E-09  2.02E-06  3.28E-06
 
 OM22
+       -7.18E-06 -3.71E-06 -1.37E-04 -1.06E-04 -1.48E-04  1.67E-06  2.18E-05  8.24E-06  1.79E-04
 
 OM23
+       -1.74E-06  8.55E-06 -1.76E-05 -3.57E-05 -9.11E-05 -1.06E-07  4.30E-06  6.76E-06  3.76E-05  4.95E-05
 
 OM33
+        1.63E-06  7.33E-06  1.71E-05 -2.91E-05 -2.33E-05 -2.26E-07  1.45E-06  1.87E-06  1.08E-05  2.46E-05  5.41E-05
 
 SG11
+       -2.87E-05  1.03E-04  3.54E-04 -8.56E-04 -4.84E-04 -1.91E-05 -8.96E-06 -6.46E-06 -1.95E-04 -1.47E-04 -8.43E-05  6.59E-02
 
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
+        2.55E-01  2.85E-02
 
 TH 3
+        1.66E-01  5.30E-01  4.23E-02
 
 TH 4
+        1.93E-02 -6.46E-01 -4.80E-01  1.33E-01
 
 TH 5
+        9.62E-03 -6.48E-01 -4.81E-01  5.07E-01  1.62E-01
 
 OM11
+       -4.08E-02 -5.20E-04 -9.61E-02 -4.53E-02  2.02E-02  6.77E-04
 
 OM12
+       -5.56E-02 -2.68E-03 -2.44E-01 -5.68E-02 -1.62E-02  4.80E-01  2.53E-03
 
 OM13
+       -1.67E-02 -4.31E-03 -7.78E-02 -3.34E-02 -2.63E-02 -4.67E-03  4.39E-01  1.81E-03
 
 OM22
+       -1.31E-01 -9.72E-03 -2.43E-01 -5.91E-02 -6.82E-02  1.84E-01  6.44E-01  3.40E-01  1.34E-02
 
 OM23
+       -6.02E-02  4.26E-02 -5.91E-02 -3.80E-02 -7.99E-02 -2.23E-02  2.41E-01  5.30E-01  3.99E-01  7.04E-03
 
 OM33
+        5.41E-02  3.50E-02  5.51E-02 -2.96E-02 -1.96E-02 -4.54E-02  7.76E-02  1.40E-01  1.09E-01  4.74E-01  7.36E-03
 
 SG11
+       -2.72E-02  1.41E-02  3.26E-02 -2.50E-02 -1.16E-02 -1.10E-01 -1.38E-02 -1.39E-02 -5.68E-02 -8.16E-02 -4.46E-02  2.57E-01
 
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
+       -5.91E+03  3.40E+03
 
 TH 3
+       -9.69E+02 -2.53E+02  9.93E+02
 
 TH 4
+       -6.84E+02  2.89E+02  8.11E+01  1.11E+02
 
 TH 5
+       -5.10E+02  2.35E+02  6.59E+01 -3.08E+00  7.52E+01
 
 OM11
+        1.70E+04  3.74E+02 -1.68E+03  6.21E+02 -6.00E+02  3.25E+06
 
 OM12
+       -7.70E+03 -1.47E+03  3.26E+03  2.56E+02  8.78E+01 -6.34E+05  4.40E+05
 
 OM13
+       -8.48E+03  2.66E+03 -1.48E+03  2.44E+02 -8.59E+01  3.34E+05 -2.12E+05  5.50E+05
 
 OM22
+        1.82E+03  2.04E+02  4.61E+02  7.42E+01  8.28E+01  3.45E+04 -4.08E+04  1.07E+04  1.17E+04
 
 OM23
+        4.06E+03 -7.96E+02  3.33E+02 -9.47E+01  6.81E+01 -1.84E+04  2.71E+04 -7.56E+04 -7.04E+03  4.19E+04
 
 OM33
+       -3.26E+03  3.23E+02 -4.69E+02  3.08E+01 -5.62E+01  2.21E+04 -1.21E+04  2.07E+04  1.63E+03 -1.59E+04  2.53E+04
 
 SG11
+        4.71E+01 -1.66E+00 -2.50E+00  5.24E-01 -2.77E-01  9.96E+02 -2.35E+02  1.36E+01  2.60E+01  4.40E+01  8.96E+00  1.57E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,      893.855
Stop Time: 
Thu 09/08/2016 
05:46 PM

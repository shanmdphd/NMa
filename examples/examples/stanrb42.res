Tue 10/18/2016 
11:41 AM
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
BAYES_EXTRA_REQUEST=1
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
PHI1=ETA(1)+MU_1
PHI2=ETA(2)+MU_2
PHI3=ETA(3)+MU_3
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

$OMEGAPD (3.0 FIX)

$SIGMAP BLOCK(1)
16.0 FIX

$SIGMAPD (2 FIX)

$EST METHOD=NUTS AUTO=1 PRINT=20
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  121) INTERACTION IS IMPLIED WITH EM/BAYES ESTIMATION METHODS

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.

 (MU_WARNING 6) THETA(003): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.

 (MU_WARNING 6) THETA(002): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.

 (MU_WARNING 19) ETA(001): HAS ALREADY BEEN MU_ ASSOCIATED, CANNOT BE USED AGAIN.

 (MU_WARNING 22) MU_001: HAS ALREADY BEEN MU_ ASSOCIATED, CANNOT BE USED AGAIN.

 (MU_WARNING 19) ETA(002): HAS ALREADY BEEN MU_ ASSOCIATED, CANNOT BE USED AGAIN.

 (MU_WARNING 22) MU_002: HAS ALREADY BEEN MU_ ASSOCIATED, CANNOT BE USED AGAIN.

 (MU_WARNING 19) ETA(003): HAS ALREADY BEEN MU_ ASSOCIATED, CANNOT BE USED AGAIN.

 (MU_WARNING 22) MU_003: HAS ALREADY BEEN MU_ ASSOCIATED, CANNOT BE USED AGAIN.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       18 OCT 2016
Days until program expires :4974
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
 RAW OUTPUT FILE (FILE): stanrb42.ext
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
 AUTOMATIC SETTING FEATURE (AUTO):          ON
 CONVERGENCE TYPE (CTYPE):                  0
 KEEP ITERATIONS (ALIAS):            1
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
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1
 MASS MATRIX ACCUMULATION ITERATIONS (MADAPT):          -1
 MASS MATRIX BLOCKING TYPE (NUTS_MASS):                 B
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
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       75.0000000000000
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): 25.0000000000000
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 50.0000000000000
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
 iteration         -500 MCMCOBJ=    82174.3160301781     
 iteration         -480 MCMCOBJ=    26442.1592358022     
 iteration         -460 MCMCOBJ=    27302.8629675819     
 iteration         -440 MCMCOBJ=    27290.2427710136     
 iteration         -420 MCMCOBJ=    26750.0477601483     
 iteration         -400 MCMCOBJ=    27168.5402501277     
 iteration         -380 MCMCOBJ=    26934.4740961675     
 iteration         -360 MCMCOBJ=    26804.0240714350     
 iteration         -340 MCMCOBJ=    27102.6335561908     
 iteration         -320 MCMCOBJ=    27257.3272475955     
 iteration         -300 MCMCOBJ=    27166.6371769784     
 iteration         -280 MCMCOBJ=    27204.5054461283     
 iteration         -260 MCMCOBJ=    27375.1306693504     
 iteration         -240 MCMCOBJ=    27374.8737937023     
 iteration         -220 MCMCOBJ=    27261.5161010209     
 iteration         -200 MCMCOBJ=    27539.3049491056     
 iteration         -180 MCMCOBJ=    27541.4263645110     
 iteration         -160 MCMCOBJ=    27197.2350654752     
 iteration         -140 MCMCOBJ=    27678.6884781616     
 iteration         -120 MCMCOBJ=    27881.2103354345     
 iteration         -100 MCMCOBJ=    27974.4630698493     
 iteration          -80 MCMCOBJ=    27531.3780133985     
 iteration          -60 MCMCOBJ=    27677.0554597680     
 iteration          -40 MCMCOBJ=    27233.8457039009     
 iteration          -20 MCMCOBJ=    27206.9162766731     
 Sampling Mode
 iteration            0 MCMCOBJ=    27137.0045718034     
 iteration           20 MCMCOBJ=    27390.6666869035     
 iteration           40 MCMCOBJ=    27354.8663049547     
 iteration           60 MCMCOBJ=    27510.9861784939     
 iteration           80 MCMCOBJ=    27440.4178749048     
 iteration          100 MCMCOBJ=    26959.4478789197     
 iteration          120 MCMCOBJ=    27136.6378010949     
 iteration          140 MCMCOBJ=    27265.1744399173     
 iteration          160 MCMCOBJ=    27556.6648707738     
 iteration          180 MCMCOBJ=    27736.1785007905     
 iteration          200 MCMCOBJ=    27128.9943461077     
 iteration          220 MCMCOBJ=    27012.0243427974     
 iteration          240 MCMCOBJ=    27165.3815185162     
 iteration          260 MCMCOBJ=    27223.4216121870     
 iteration          280 MCMCOBJ=    27732.1151277848     
 iteration          300 MCMCOBJ=    27082.1771574232     
 iteration          320 MCMCOBJ=    27100.4058281679     
 iteration          340 MCMCOBJ=    27211.4859934352     
 iteration          360 MCMCOBJ=    27180.0817653445     
 iteration          380 MCMCOBJ=    26810.4882864270     
 iteration          400 MCMCOBJ=    27204.7574377918     
 iteration          420 MCMCOBJ=    27252.1217479991     
 iteration          440 MCMCOBJ=    26861.4248445489     
 iteration          460 MCMCOBJ=    27145.1235878406     
 iteration          480 MCMCOBJ=    26918.5164357163     
 iteration          500 MCMCOBJ=    26983.6582193113     
 iteration          520 MCMCOBJ=    26780.4348478796     
 iteration          540 MCMCOBJ=    27562.0267794806     
 iteration          560 MCMCOBJ=    27191.8500356065     
 iteration          580 MCMCOBJ=    27267.5182417188     
 iteration          600 MCMCOBJ=    27316.3916607983     
 iteration          620 MCMCOBJ=    27213.7766179662     
 iteration          640 MCMCOBJ=    27272.6721394412     
 iteration          660 MCMCOBJ=    27210.3275908837     
 iteration          680 MCMCOBJ=    26766.0240427079     
 iteration          700 MCMCOBJ=    26656.0454328455     
 iteration          720 MCMCOBJ=    26684.9069366209     
 iteration          740 MCMCOBJ=    26917.3885010902     
 iteration          760 MCMCOBJ=    27312.0697275631     
 iteration          780 MCMCOBJ=    27108.0066053204     
 iteration          800 MCMCOBJ=    27351.4697438412     
 iteration          820 MCMCOBJ=    27626.4257460851     
 iteration          840 MCMCOBJ=    27059.4910410740     
 iteration          860 MCMCOBJ=    27100.6709666023     
 iteration          880 MCMCOBJ=    27098.3054654453     
 iteration          900 MCMCOBJ=    26881.6240686252     
 iteration          920 MCMCOBJ=    26776.7127149197     
 iteration          940 MCMCOBJ=    26895.5728258219     
 iteration          960 MCMCOBJ=    26720.0789583751     
 iteration          980 MCMCOBJ=    27142.8400665510     
 iteration         1000 MCMCOBJ=    26883.8592415564     
 iteration         1020 MCMCOBJ=    27113.4232280230     
 iteration         1040 MCMCOBJ=    27596.6272797451     
 iteration         1060 MCMCOBJ=    27192.8164720181     
 iteration         1080 MCMCOBJ=    26995.0656134525     
 iteration         1100 MCMCOBJ=    27101.6466973158     
 iteration         1120 MCMCOBJ=    27168.9868853999     
 iteration         1140 MCMCOBJ=    27024.4868109262     
 iteration         1160 MCMCOBJ=    26713.4282467003     
 iteration         1180 MCMCOBJ=    26793.8282703458     
 iteration         1200 MCMCOBJ=    26743.8873106654     
 iteration         1220 MCMCOBJ=    26650.4166420967     
 iteration         1240 MCMCOBJ=    26781.6374066427     
 iteration         1260 MCMCOBJ=    27088.5267332832     
 iteration         1280 MCMCOBJ=    27024.4297169586     
 iteration         1300 MCMCOBJ=    27016.7180932440     
 iteration         1320 MCMCOBJ=    27007.8355824392     
 iteration         1340 MCMCOBJ=    26890.0940632563     
 iteration         1360 MCMCOBJ=    27239.6069586037     
 iteration         1380 MCMCOBJ=    27385.9993133070     
 iteration         1400 MCMCOBJ=    27000.1824504844     
 iteration         1420 MCMCOBJ=    26986.0195682431     
 iteration         1440 MCMCOBJ=    27090.0160117422     
 iteration         1460 MCMCOBJ=    27180.8698611660     
 iteration         1480 MCMCOBJ=    27150.6238461413     
 iteration         1500 MCMCOBJ=    27235.6579498957     
 iteration         1520 MCMCOBJ=    27367.4544185251     
 iteration         1540 MCMCOBJ=    27313.2204606473     
 iteration         1560 MCMCOBJ=    27642.8694989411     
 iteration         1580 MCMCOBJ=    27599.6327495794     
 iteration         1600 MCMCOBJ=    27125.7148501492     
 iteration         1620 MCMCOBJ=    27027.3701899892     
 iteration         1640 MCMCOBJ=    26858.0698739641     
 iteration         1660 MCMCOBJ=    27128.6599615457     
 iteration         1680 MCMCOBJ=    26767.1569870622     
 iteration         1700 MCMCOBJ=    27019.6663474825     
 iteration         1720 MCMCOBJ=    26899.8939794252     
 iteration         1740 MCMCOBJ=    27264.0493363472     
 iteration         1760 MCMCOBJ=    27914.1007906702     
 iteration         1780 MCMCOBJ=    27909.7910322577     
 iteration         1800 MCMCOBJ=    27437.8681364781     
 iteration         1820 MCMCOBJ=    27095.4815754027     
 iteration         1840 MCMCOBJ=    27470.8095022469     
 iteration         1860 MCMCOBJ=    27273.9484562236     
 iteration         1880 MCMCOBJ=    27209.7491018154     
 iteration         1900 MCMCOBJ=    27082.0016092519     
 iteration         1920 MCMCOBJ=    27587.4959390212     
 iteration         1940 MCMCOBJ=    26922.5503079219     
 iteration         1960 MCMCOBJ=    27185.9883931611     
 iteration         1980 MCMCOBJ=    27131.4934558087     
 iteration         2000 MCMCOBJ=    27614.9106553572     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27142.9092844012     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43683.8028820853     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27142.9092844012     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32105.1773637064     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    16.0970839384867     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27142.9092844012     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27159.0063683396     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   726.77
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27142.909       **************************************************
 #OBJS:********************************************      268.539 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.01E-02  3.67E+00 -4.99E+00 -9.16E-01 -1.17E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.99E-03
 
 ETA2
+        2.47E-02  1.61E-01
 
 ETA3
+       -1.01E-03 -4.00E-02  5.02E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.56E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.99E-02
 
 ETA2
+        6.16E-01  4.01E-01
 
 ETA3
+       -4.59E-02 -4.53E-01  2.23E-01
 


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
 
         4.09E-03  2.92E-02  4.47E-02  1.29E-01  1.68E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.55E-04
 
 ETA2
+        2.62E-03  1.41E-02
 
 ETA3
+        1.96E-03  6.86E-03  7.13E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.70E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.27E-03
 
 ETA2
+        4.52E-02  1.74E-02
 
 ETA3
+        8.72E-02  9.85E-02  1.57E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.42E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.68E-05
 
 TH 2
+        3.48E-05  8.53E-04
 
 TH 3
+        3.70E-05  7.17E-04  2.00E-03
 
 TH 4
+       -1.96E-05 -2.53E-03 -2.86E-03  1.67E-02
 
 TH 5
+       -2.68E-05 -3.31E-03 -3.48E-03  1.12E-02  2.82E-02
 
 OM11
+       -1.42E-07 -2.01E-07 -1.83E-06 -2.05E-06 -2.08E-08  4.29E-07
 
 OM12
+       -8.06E-07 -2.87E-06 -3.78E-05 -2.26E-06 -1.23E-05  7.82E-07  6.89E-06
 
 OM13
+       -3.18E-07  1.23E-07 -1.52E-05 -5.40E-06 -6.61E-06 -3.16E-08  2.40E-06  3.84E-06
 
 OM22
+       -7.50E-06 -1.18E-05 -2.41E-04 -2.24E-05 -8.56E-05  1.59E-06  2.46E-05  9.86E-06  1.98E-04
 
 OM23
+       -2.32E-06 -1.45E-06 -6.95E-05  7.19E-07 -4.54E-05 -9.86E-08  5.11E-06  7.41E-06  4.17E-05  4.71E-05
 
 OM33
+        3.70E-06  1.29E-06 -1.45E-05  5.90E-05  1.22E-05 -4.31E-08  1.73E-06  1.53E-06  1.06E-05  2.09E-05  5.09E-05
 
 SG11
+        3.04E-05  9.87E-05  4.53E-04 -9.58E-04  5.15E-04 -8.96E-06  7.98E-06 -6.31E-07 -1.08E-04 -4.50E-05 -1.09E-04  7.30E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.09E-03
 
 TH 2
+        2.91E-01  2.92E-02
 
 TH 3
+        2.02E-01  5.49E-01  4.47E-02
 
 TH 4
+       -3.70E-02 -6.71E-01 -4.94E-01  1.29E-01
 
 TH 5
+       -3.91E-02 -6.75E-01 -4.64E-01  5.15E-01  1.68E-01
 
 OM11
+       -5.29E-02 -1.05E-02 -6.24E-02 -2.42E-02 -1.89E-04  6.55E-04
 
 OM12
+       -7.50E-02 -3.75E-02 -3.22E-01 -6.68E-03 -2.80E-02  4.55E-01  2.62E-03
 
 OM13
+       -3.96E-02  2.14E-03 -1.73E-01 -2.13E-02 -2.01E-02 -2.47E-02  4.68E-01  1.96E-03
 
 OM22
+       -1.30E-01 -2.87E-02 -3.82E-01 -1.23E-02 -3.63E-02  1.72E-01  6.65E-01  3.58E-01  1.41E-02
 
 OM23
+       -8.26E-02 -7.24E-03 -2.26E-01  8.10E-04 -3.94E-02 -2.19E-02  2.83E-01  5.51E-01  4.32E-01  6.86E-03
 
 OM33
+        1.27E-01  6.19E-03 -4.55E-02  6.40E-02  1.02E-02 -9.23E-03  9.25E-02  1.09E-01  1.05E-01  4.26E-01  7.13E-03
 
 SG11
+        2.75E-02  1.25E-02  3.75E-02 -2.74E-02  1.14E-02 -5.06E-02  1.13E-02 -1.19E-03 -2.85E-02 -2.43E-02 -5.68E-02  2.70E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.52E+04
 
 TH 2
+       -6.13E+03  3.60E+03
 
 TH 3
+       -7.64E+02 -3.15E+02  1.02E+03
 
 TH 4
+       -6.23E+02  3.16E+02  8.84E+01  1.24E+02
 
 TH 5
+       -4.84E+02  2.53E+02  5.84E+01 -1.09E+00  7.29E+01
 
 OM11
+        2.38E+04 -1.45E+03 -4.23E+03  1.48E+02 -8.72E+02  3.41E+06
 
 OM12
+       -7.68E+03  1.38E+03  3.12E+03  2.84E+02  5.05E+02 -6.48E+05  4.37E+05
 
 OM13
+       -4.46E+03 -7.11E+02 -1.23E+03  1.86E+01 -3.23E+02  3.68E+05 -2.18E+05  5.05E+05
 
 OM22
+        1.52E+03 -3.51E+02  7.80E+02  7.19E+01  3.60E+01  3.45E+04 -4.04E+04  1.25E+04  1.16E+04
 
 OM23
+        5.14E+03 -1.17E+02  8.15E+02  6.88E+01  1.18E+02 -2.28E+04  3.17E+04 -7.81E+04 -7.27E+03  4.51E+04
 
 OM33
+       -6.75E+03 -6.58E+01 -3.41E+02 -1.32E+02 -3.73E+01  1.45E+04 -1.29E+04  2.23E+04  1.67E+03 -1.60E+04  2.63E+04
 
 SG11
+       -2.41E+01  9.71E-01 -4.56E+00  8.52E-01 -1.13E+00  5.78E+02 -2.07E+02  9.01E+01  1.97E+01 -2.09E+01  3.90E+01  1.39E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,      716.450
Stop Time: 
Tue 10/18/2016 
11:54 AM

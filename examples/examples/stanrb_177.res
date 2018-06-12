Tue 02/23/2016 
05:40 PM
$PROBLEM Stan model
$DATA full3.csv IGNORE=@
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
0.011 FIX
0.0  0.161
0.0  0.0 0.041

$OMEGAPD (4 FIX)

$SIGMAP BLOCK(1)
16.0 FIX

$SIGMAPD (2 FIX)

$EST METHOD=ITS NITER=0 file=stanrb_177_its.ext
$EST METHOD=NUTS NBURN=500 NITER=2000 PRINT=20 file=stanrb_177.ext
     WISHTYPE=1 OLKJDF=6.0
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  121) INTERACTION IS IMPLIED WITH EM/BAYES ESTIMATION METHODS

 (MU_WARNING 6) THETA(003): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.

 (MU_WARNING 6) THETA(002): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       23 FEB 2016
Days until program expires :5209
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
 (E11.0,6E10.0/2E10.0)

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
                  0.1100E-01
                  0.0000E+00   0.1610E+00
                  0.0000E+00   0.0000E+00   0.4100E-01
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
 RAW OUTPUT FILE (FILE): stanrb_177_its.ext
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

 iteration            0 OBJ=   43819.4442691685
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.1334E-02 -1.0260E-01  1.6265E-01
 SE:             3.9766E-03  1.2304E-02  8.9866E-03
 N:                     900         900         900
 
 P VAL.:         4.3687E-03  7.5881E-17  3.4896E-73
 
 ETAshrink(%):   8.3119E+01  4.7771E+01  6.1852E+01
 EBVshrink(%):   6.9891E-01  2.0245E+01  1.9313E+01
 EPSshrink(%):   3.0548E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    43819.4442691685     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       60360.3378668526     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    19.4354679649146     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    43819.4442691685     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43838.8797371334     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:     0.31
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    43819.444       **************************************************
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
 RAW OUTPUT FILE (FILE): stanrb_177.ext
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
 SAMPLES FOR LOCAL SEARCH KERNEL3(OSAMPLE_M2):           0
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
 INITIAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (SETPITER): 1
 INTERVAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (SETPINTER):0

 
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
 iteration         -500 MCMCOBJ=    28952.1417114403     
 iteration         -480 MCMCOBJ=    27957.8029214935     
 iteration         -460 MCMCOBJ=    27535.3731867431     
 iteration         -440 MCMCOBJ=    27265.3633799680     
 iteration         -420 MCMCOBJ=    27317.2986789543     
 iteration         -400 MCMCOBJ=    27290.5126901656     
 iteration         -380 MCMCOBJ=    27337.0232553621     
 iteration         -360 MCMCOBJ=    27107.7683509126     
 iteration         -340 MCMCOBJ=    27117.9977167180     
 iteration         -320 MCMCOBJ=    26897.5524873640     
 iteration         -300 MCMCOBJ=    27229.1695205811     
 iteration         -280 MCMCOBJ=    26789.6331080301     
 iteration         -260 MCMCOBJ=    27029.4881560252     
 iteration         -240 MCMCOBJ=    26873.3280435949     
 iteration         -220 MCMCOBJ=    27173.7456378499     
 iteration         -200 MCMCOBJ=    27333.0725064624     
 iteration         -180 MCMCOBJ=    27324.6660067141     
 iteration         -160 MCMCOBJ=    26972.6763242598     
 iteration         -140 MCMCOBJ=    26995.0075932260     
 iteration         -120 MCMCOBJ=    26821.7047564828     
 iteration         -100 MCMCOBJ=    26806.4388371450     
 iteration          -80 MCMCOBJ=    27069.1160003159     
 iteration          -60 MCMCOBJ=    26858.4855522974     
 iteration          -40 MCMCOBJ=    27053.5009074820     
 iteration          -20 MCMCOBJ=    27284.8070393345     
 Sampling Mode
 iteration            0 MCMCOBJ=    27272.5065547213     
 iteration           20 MCMCOBJ=    27165.8748124604     
 iteration           40 MCMCOBJ=    27234.6092517664     
 iteration           60 MCMCOBJ=    27446.1506700012     
 iteration           80 MCMCOBJ=    27107.1584687193     
 iteration          100 MCMCOBJ=    27231.1684647366     
 iteration          120 MCMCOBJ=    26987.9525244952     
 iteration          140 MCMCOBJ=    26816.5536319198     
 iteration          160 MCMCOBJ=    27034.8643959899     
 iteration          180 MCMCOBJ=    26731.2744553994     
 iteration          200 MCMCOBJ=    26853.4610903761     
 iteration          220 MCMCOBJ=    26364.4546895700     
 iteration          240 MCMCOBJ=    26609.2845361751     
 iteration          260 MCMCOBJ=    27015.7405016849     
 iteration          280 MCMCOBJ=    27236.8987935083     
 iteration          300 MCMCOBJ=    27345.3686511363     
 iteration          320 MCMCOBJ=    27238.5006835116     
 iteration          340 MCMCOBJ=    27174.3982677161     
 iteration          360 MCMCOBJ=    26959.7523053333     
 iteration          380 MCMCOBJ=    26598.7063943663     
 iteration          400 MCMCOBJ=    26427.3889533600     
 iteration          420 MCMCOBJ=    26783.3727427278     
 iteration          440 MCMCOBJ=    26857.9935255476     
 iteration          460 MCMCOBJ=    26698.5153006480     
 iteration          480 MCMCOBJ=    26295.2525217114     
 iteration          500 MCMCOBJ=    26602.5531567964     
 iteration          520 MCMCOBJ=    26217.2831386236     
 iteration          540 MCMCOBJ=    25966.6548078767     
 iteration          560 MCMCOBJ=    26320.5555043221     
 iteration          580 MCMCOBJ=    26495.5577111711     
 iteration          600 MCMCOBJ=    26781.5270483284     
 iteration          620 MCMCOBJ=    26725.5349769556     
 iteration          640 MCMCOBJ=    27020.8983993392     
 iteration          660 MCMCOBJ=    26931.1193487249     
 iteration          680 MCMCOBJ=    27229.8363229714     
 iteration          700 MCMCOBJ=    27109.5770452583     
 iteration          720 MCMCOBJ=    26973.3861913124     
 iteration          740 MCMCOBJ=    26874.1002514509     
 iteration          760 MCMCOBJ=    26984.6737740535     
 iteration          780 MCMCOBJ=    27054.0139630378     
 iteration          800 MCMCOBJ=    27461.1760907399     
 iteration          820 MCMCOBJ=    27262.3017802500     
 iteration          840 MCMCOBJ=    26687.2779150100     
 iteration          860 MCMCOBJ=    27314.3509782796     
 iteration          880 MCMCOBJ=    27048.7184033633     
 iteration          900 MCMCOBJ=    26911.7391109173     
 iteration          920 MCMCOBJ=    26707.5502708901     
 iteration          940 MCMCOBJ=    26764.1266950227     
 iteration          960 MCMCOBJ=    26504.0373546587     
 iteration          980 MCMCOBJ=    26257.5677407967     
 iteration         1000 MCMCOBJ=    26136.8443487251     
 iteration         1020 MCMCOBJ=    25910.8222354179     
 iteration         1040 MCMCOBJ=    25920.6558293297     
 iteration         1060 MCMCOBJ=    26054.6175375153     
 iteration         1080 MCMCOBJ=    25932.2130716987     
 iteration         1100 MCMCOBJ=    25777.7432606362     
 iteration         1120 MCMCOBJ=    25945.3695375964     
 iteration         1140 MCMCOBJ=    26032.6430476498     
 iteration         1160 MCMCOBJ=    26469.5298457208     
 iteration         1180 MCMCOBJ=    26772.5604708556     
 iteration         1200 MCMCOBJ=    26319.5836800237     
 iteration         1220 MCMCOBJ=    26394.7962044770     
 iteration         1240 MCMCOBJ=    26596.5271281552     
 iteration         1260 MCMCOBJ=    26564.2958803029     
 iteration         1280 MCMCOBJ=    26643.8758132703     
 iteration         1300 MCMCOBJ=    26670.2214416085     
 iteration         1320 MCMCOBJ=    26359.0497474527     
 iteration         1340 MCMCOBJ=    26248.8884586326     
 iteration         1360 MCMCOBJ=    26294.6371735293     
 iteration         1380 MCMCOBJ=    26515.7024759183     
 iteration         1400 MCMCOBJ=    26816.3581567993     
 iteration         1420 MCMCOBJ=    26708.8691064275     
 iteration         1440 MCMCOBJ=    26832.3344139738     
 iteration         1460 MCMCOBJ=    27143.6689025530     
 iteration         1480 MCMCOBJ=    26990.6576257941     
 iteration         1500 MCMCOBJ=    27182.4209216671     
 iteration         1520 MCMCOBJ=    27112.7741354798     
 iteration         1540 MCMCOBJ=    27259.6003448170     
 iteration         1560 MCMCOBJ=    27404.4762748380     
 iteration         1580 MCMCOBJ=    26863.1240212856     
 iteration         1600 MCMCOBJ=    26839.4451814880     
 iteration         1620 MCMCOBJ=    26652.8153913029     
 iteration         1640 MCMCOBJ=    26509.1377691591     
 iteration         1660 MCMCOBJ=    26406.7898386058     
 iteration         1680 MCMCOBJ=    26646.4146838927     
 iteration         1700 MCMCOBJ=    26398.5598026310     
 iteration         1720 MCMCOBJ=    26635.5443144498     
 iteration         1740 MCMCOBJ=    26765.2319647862     
 iteration         1760 MCMCOBJ=    27405.3228629035     
 iteration         1780 MCMCOBJ=    27457.3576563965     
 iteration         1800 MCMCOBJ=    27589.6271882275     
 iteration         1820 MCMCOBJ=    27156.7427817026     
 iteration         1840 MCMCOBJ=    26958.3064197601     
 iteration         1860 MCMCOBJ=    27071.3796457666     
 iteration         1880 MCMCOBJ=    27222.8750912706     
 iteration         1900 MCMCOBJ=    27266.7083514588     
 iteration         1920 MCMCOBJ=    27584.4473134239     
 iteration         1940 MCMCOBJ=    27539.3892949695     
 iteration         1960 MCMCOBJ=    27234.7493762858     
 iteration         1980 MCMCOBJ=    26966.5181862341     
 iteration         2000 MCMCOBJ=    26949.2918936819     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26805.9586774319     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43346.8522751160     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26805.9586774319     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       31768.2267567371     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -10.0300439664667     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26805.9586774319     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       26795.9286334654     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  1494.29
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    26805.959       **************************************************
 #OBJS:********************************************      426.386 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
        -2.49E-03  3.68E+00 -5.02E+00 -9.76E-01 -1.11E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.03E-02
 
 ETA2
+        2.70E-02  1.77E-01
 
 ETA3
+        7.52E-04 -4.52E-02  4.17E-02
 


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
+        6.33E-01  4.21E-01
 
 ETA3
+        3.55E-02 -5.33E-01  2.04E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.98E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.14E-03  2.98E-02  4.44E-02  1.53E-01  1.63E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.72E-04
 
 ETA2
+        2.55E-03  1.34E-02
 
 ETA3
+        1.65E-03  5.81E-03  6.18E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.72E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.31E-03
 
 ETA2
+        3.97E-02  1.58E-02
 
 ETA3
+        7.83E-02  9.40E-02  1.52E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.43E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.72E-05
 
 TH 2
+        3.55E-05  8.90E-04
 
 TH 3
+        4.63E-05  6.98E-04  1.97E-03
 
 TH 4
+       -1.26E-05 -2.91E-03 -2.69E-03  2.34E-02
 
 TH 5
+       -3.77E-05 -3.20E-03 -3.24E-03  1.22E-02  2.64E-02
 
 OM11
+       -1.21E-07  1.08E-06 -2.54E-06 -9.33E-06 -1.45E-06  4.52E-07
 
 OM12
+       -7.99E-07  2.37E-06 -4.01E-05 -3.66E-05 -1.18E-05  8.96E-07  6.50E-06
 
 OM13
+       -3.89E-08  1.84E-07 -1.78E-05 -4.69E-06  6.12E-06  1.27E-07  1.86E-06  2.72E-06
 
 OM22
+       -7.99E-06  1.05E-05 -2.37E-04 -2.25E-04 -1.26E-04  2.07E-06  2.28E-05  6.76E-06  1.79E-04
 
 OM23
+       -9.40E-07  8.64E-06 -6.81E-05 -4.64E-05 -4.23E-05  3.31E-07  5.36E-06  5.14E-06  2.73E-05  3.38E-05
 
 OM33
+        3.38E-06  7.76E-06  2.68E-06  2.28E-05 -1.67E-05  3.32E-09  1.51E-06  1.72E-06 -8.93E-07  1.16E-05  3.81E-05
 
 SG11
+        2.42E-05 -7.19E-04 -7.67E-05  1.26E-03  2.76E-03 -1.96E-05 -4.95E-05 -1.65E-05 -4.78E-04 -6.16E-05 -9.11E-05  7.41E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.14E-03
 
 TH 2
+        2.88E-01  2.98E-02
 
 TH 3
+        2.52E-01  5.27E-01  4.44E-02
 
 TH 4
+       -1.99E-02 -6.39E-01 -3.95E-01  1.53E-01
 
 TH 5
+       -5.60E-02 -6.60E-01 -4.49E-01  4.91E-01  1.63E-01
 
 OM11
+       -4.33E-02  5.40E-02 -8.51E-02 -9.07E-02 -1.33E-02  6.72E-04
 
 OM12
+       -7.57E-02  3.12E-02 -3.54E-01 -9.39E-02 -2.85E-02  5.23E-01  2.55E-03
 
 OM13
+       -5.70E-03  3.75E-03 -2.43E-01 -1.86E-02  2.28E-02  1.14E-01  4.43E-01  1.65E-03
 
 OM22
+       -1.44E-01  2.62E-02 -3.99E-01 -1.10E-01 -5.80E-02  2.30E-01  6.69E-01  3.06E-01  1.34E-02
 
 OM23
+       -3.90E-02  4.98E-02 -2.64E-01 -5.22E-02 -4.48E-02  8.48E-02  3.62E-01  5.36E-01  3.51E-01  5.81E-03
 
 OM33
+        1.32E-01  4.21E-02  9.79E-03  2.41E-02 -1.66E-02  7.99E-04  9.58E-02  1.69E-01 -1.08E-02  3.22E-01  6.18E-03
 
 SG11
+        2.14E-02 -8.85E-02 -6.35E-03  3.03E-02  6.25E-02 -1.07E-01 -7.13E-02 -3.68E-02 -1.31E-01 -3.89E-02 -5.42E-02  2.72E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.25E+04
 
 TH 2
+       -5.16E+03  3.25E+03
 
 TH 3
+       -1.15E+03 -4.29E+02  1.08E+03
 
 TH 4
+       -4.94E+02  2.29E+02  5.37E+01  8.04E+01
 
 TH 5
+       -4.23E+02  2.23E+02  6.15E+01 -3.06E+00  7.38E+01
 
 OM11
+        1.81E+04 -2.07E+03 -3.51E+03  4.55E+02 -8.97E+02  3.32E+06
 
 OM12
+       -6.42E+03 -7.57E+02  3.18E+03  2.02E+02  2.18E+02 -6.54E+05  4.52E+05
 
 OM13
+       -7.51E+03 -3.42E+02  6.69E+02  3.29E+01 -2.68E+02  1.66E+05 -1.54E+05  5.88E+05
 
 OM22
+        1.47E+03 -2.28E+02  9.50E+02  1.00E+02  7.57E+01  3.62E+04 -4.08E+04  7.13E+03  1.23E+04
 
 OM23
+        2.36E+03 -8.81E+02  1.21E+03  4.85E+01  9.81E+01  5.54E+03  1.88E+03 -7.20E+04 -3.45E+03  5.00E+04
 
 OM33
+       -5.38E+03  1.36E+02 -3.86E+02 -7.29E+01 -2.38E+00  1.74E+04 -1.24E+04  2.32E+03  2.58E+03 -1.22E+04  3.10E+04
 
 SG11
+       -4.71E+01  1.74E+01  2.13E+00  2.05E+00  7.61E-02  7.37E+02 -1.96E+02  7.08E+01  5.75E+01 -2.17E+01  4.55E+01  1.41E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     1483.070
Stop Time: 
Tue 02/23/2016 
06:05 PM

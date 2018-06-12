Tue 02/23/2016 
06:16 PM
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

$EST METHOD=BAYES NBURN=1000 NITER=0 PRINT=50 MASSRESET=1 file=stanrb_178_beys.ext
     OLKJDF=6 OSAMPLE_M1=1 PKAPPA=0.75
$EST METHOD=NUTS NBURN=500 NITER=2000 PRINT=20 file=stanrb_178.ext
     WISHTYPE=1 OLKJDF=6.0 MASSRESET=0 PKAPPA=1.0
  
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
 #METH: MCMC Bayesian Analysis
 
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
 RAW OUTPUT FILE (FILE): stanrb_178_beys.ext
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
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      1
 PWR. WT. MASS/IMP/POST MATRIX ACCUM. FOR ETAS (IKAPPA): 1.00000000000000
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
 SAMPLES FOR GLOBAL SEARCH KERNEL (OSAMPLE_M1):          1
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           -1
 SAMPLES FOR LOCAL SEARCH KERNEL3(OSAMPLE_M2):           0
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 6.00000000000000
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
 
 OMEGAS ARE METROPOLIS-HASTINGS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -1000 MCMCOBJ=    159178.805317800     
 iteration         -950 MCMCOBJ=    29211.0683931776     
 iteration         -900 MCMCOBJ=    28012.3115499091     
 iteration         -850 MCMCOBJ=    27740.6310440286     
 iteration         -800 MCMCOBJ=    27563.7418802238     
 iteration         -750 MCMCOBJ=    27371.6477241122     
 iteration         -700 MCMCOBJ=    27256.4011006495     
 iteration         -650 MCMCOBJ=    27556.8634580408     
 iteration         -600 MCMCOBJ=    27227.0723131471     
 iteration         -550 MCMCOBJ=    27072.4565488688     
 iteration         -500 MCMCOBJ=    27523.7038372069     
 iteration         -450 MCMCOBJ=    27174.7186671550     
 iteration         -400 MCMCOBJ=    27440.3970090468     
 iteration         -350 MCMCOBJ=    27281.7455860834     
 iteration         -300 MCMCOBJ=    27486.4854312963     
 iteration         -250 MCMCOBJ=    27328.8869200244     
 iteration         -200 MCMCOBJ=    27309.2206055054     
 iteration         -150 MCMCOBJ=    27244.3639491136     
 iteration         -100 MCMCOBJ=    27279.0613631050     
 iteration          -50 MCMCOBJ=    26984.7724169001     
 Sampling Mode
 iteration            0 MCMCOBJ=    26856.3400836193     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS NOT PERFORMED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26856.3400836193     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43397.2336813034     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26856.3400836193     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       31818.6081629245     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -10.0300439664667     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26856.3400836193     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       26846.3100396528     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    94.90
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    26856.340       **************************************************
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
 
         3.43E-03  3.70E+00 -5.10E+00 -1.05E+00 -1.18E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.93E-03
 
 ETA2
+        2.82E-02  1.90E-01
 
 ETA3
+        2.21E-04 -4.91E-02  4.11E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.60E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.96E-02
 
 ETA2
+        6.49E-01  4.36E-01
 
 ETA3
+        1.10E-02 -5.55E-01  2.03E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        4.00E+00
 
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
 RAW OUTPUT FILE (FILE): stanrb_178.ext
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
 SAMPLES FOR GLOBAL SEARCH KERNEL (OSAMPLE_M1):          1
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           -1
 SAMPLES FOR LOCAL SEARCH KERNEL3(OSAMPLE_M2):           0
 NO U-TURN BAYES SAMPLING TYPE:                          TSOI
 THE FOLLOWING PERTAIN TO THETAS/OMEGAS/SIGMAS/ETAS
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      0
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
 
 OMEGAS ARE METROPOLIS-HASTINGS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration         -500 MCMCOBJ=    26867.4241482264     
 iteration         -480 MCMCOBJ=    27116.9082409697     
 iteration         -460 MCMCOBJ=    26897.2856354634     
 iteration         -440 MCMCOBJ=    26440.1875195891     
 iteration         -420 MCMCOBJ=    26607.2492939261     
 iteration         -400 MCMCOBJ=    26909.3448950028     
 iteration         -380 MCMCOBJ=    26549.3814457311     
 iteration         -360 MCMCOBJ=    26471.3435887224     
 iteration         -340 MCMCOBJ=    26606.6542847949     
 iteration         -320 MCMCOBJ=    25951.5885511478     
 iteration         -300 MCMCOBJ=    25813.3244782465     
 iteration         -280 MCMCOBJ=    25484.2243108197     
 iteration         -260 MCMCOBJ=    25548.7255370179     
 iteration         -240 MCMCOBJ=    26427.6206968496     
 iteration         -220 MCMCOBJ=    26424.1687451351     
 iteration         -200 MCMCOBJ=    26974.6292950452     
 iteration         -180 MCMCOBJ=    26752.0469806891     
 iteration         -160 MCMCOBJ=    26603.9642577988     
 iteration         -140 MCMCOBJ=    26659.2709083195     
 iteration         -120 MCMCOBJ=    26559.7367111819     
 iteration         -100 MCMCOBJ=    26609.3201432330     
 iteration          -80 MCMCOBJ=    26272.7628175571     
 iteration          -60 MCMCOBJ=    26681.7927840747     
 iteration          -40 MCMCOBJ=    26460.2622035899     
 iteration          -20 MCMCOBJ=    26338.2017821644     
 Sampling Mode
 iteration            0 MCMCOBJ=    26526.1604129032     
 iteration           20 MCMCOBJ=    26700.6188079119     
 iteration           40 MCMCOBJ=    26363.2171844391     
 iteration           60 MCMCOBJ=    26332.8519392419     
 iteration           80 MCMCOBJ=    26884.6715306914     
 iteration          100 MCMCOBJ=    26808.4112694387     
 iteration          120 MCMCOBJ=    26995.9798701702     
 iteration          140 MCMCOBJ=    26582.8056791580     
 iteration          160 MCMCOBJ=    26838.7084816898     
 iteration          180 MCMCOBJ=    26619.4137570578     
 iteration          200 MCMCOBJ=    26264.5779088645     
 iteration          220 MCMCOBJ=    26656.5373389875     
 iteration          240 MCMCOBJ=    26334.8987183992     
 iteration          260 MCMCOBJ=    26139.2006397384     
 iteration          280 MCMCOBJ=    26125.7645192668     
 iteration          300 MCMCOBJ=    26453.7142939610     
 iteration          320 MCMCOBJ=    26457.9915235676     
 iteration          340 MCMCOBJ=    26286.2774233208     
 iteration          360 MCMCOBJ=    26243.6543630510     
 iteration          380 MCMCOBJ=    26627.5903457259     
 iteration          400 MCMCOBJ=    26784.6700320358     
 iteration          420 MCMCOBJ=    26881.4412990640     
 iteration          440 MCMCOBJ=    26816.2193266811     
 iteration          460 MCMCOBJ=    27030.4679982103     
 iteration          480 MCMCOBJ=    26405.7655170155     
 iteration          500 MCMCOBJ=    26595.8917709465     
 iteration          520 MCMCOBJ=    26298.1185184351     
 iteration          540 MCMCOBJ=    26580.7270280213     
 iteration          560 MCMCOBJ=    26744.8064761866     
 iteration          580 MCMCOBJ=    26559.1544465087     
 iteration          600 MCMCOBJ=    26166.3043101579     
 iteration          620 MCMCOBJ=    26591.2480130851     
 iteration          640 MCMCOBJ=    26597.4547376565     
 iteration          660 MCMCOBJ=    26814.7031827800     
 iteration          680 MCMCOBJ=    26565.1465982920     
 iteration          700 MCMCOBJ=    26550.9342849584     
 iteration          720 MCMCOBJ=    26349.5794388279     
 iteration          740 MCMCOBJ=    26560.4172915529     
 iteration          760 MCMCOBJ=    26176.0804973788     
 iteration          780 MCMCOBJ=    26159.3258853748     
 iteration          800 MCMCOBJ=    26190.5304916203     
 iteration          820 MCMCOBJ=    26515.1078000713     
 iteration          840 MCMCOBJ=    26842.0174126438     
 iteration          860 MCMCOBJ=    26939.1976007147     
 iteration          880 MCMCOBJ=    26968.0121594843     
 iteration          900 MCMCOBJ=    27451.5035397779     
 iteration          920 MCMCOBJ=    27561.5428215398     
 iteration          940 MCMCOBJ=    27442.6282262503     
 iteration          960 MCMCOBJ=    26876.3804426364     
 iteration          980 MCMCOBJ=    26662.9601853756     
 iteration         1000 MCMCOBJ=    26264.0736101636     
 iteration         1020 MCMCOBJ=    27062.5656486405     
 iteration         1040 MCMCOBJ=    26726.6505540860     
 iteration         1060 MCMCOBJ=    26446.2950563582     
 iteration         1080 MCMCOBJ=    26644.6491946475     
 iteration         1100 MCMCOBJ=    26559.6692780123     
 iteration         1120 MCMCOBJ=    26388.7902795040     
 iteration         1140 MCMCOBJ=    26064.9013566657     
 iteration         1160 MCMCOBJ=    26265.0744733345     
 iteration         1180 MCMCOBJ=    26927.5107995237     
 iteration         1200 MCMCOBJ=    26609.2583647395     
 iteration         1220 MCMCOBJ=    26792.7354630961     
 iteration         1240 MCMCOBJ=    26895.5142281867     
 iteration         1260 MCMCOBJ=    26339.4603596737     
 iteration         1280 MCMCOBJ=    26499.4197040522     
 iteration         1300 MCMCOBJ=    26751.4421385384     
 iteration         1320 MCMCOBJ=    26870.9819807765     
 iteration         1340 MCMCOBJ=    27248.0451282067     
 iteration         1360 MCMCOBJ=    26872.8150437203     
 iteration         1380 MCMCOBJ=    27102.0412508862     
 iteration         1400 MCMCOBJ=    27581.7346095544     
 iteration         1420 MCMCOBJ=    27468.7429786373     
 iteration         1440 MCMCOBJ=    27550.9960899182     
 iteration         1460 MCMCOBJ=    27507.2548968516     
 iteration         1480 MCMCOBJ=    27814.4453218141     
 iteration         1500 MCMCOBJ=    27675.6856382165     
 iteration         1520 MCMCOBJ=    27384.1564857869     
 iteration         1540 MCMCOBJ=    27617.8021442311     
 iteration         1560 MCMCOBJ=    27194.6688574197     
 iteration         1580 MCMCOBJ=    27144.9654768694     
 iteration         1600 MCMCOBJ=    26621.4754443562     
 iteration         1620 MCMCOBJ=    26910.3874407688     
 iteration         1640 MCMCOBJ=    26926.3250031771     
 iteration         1660 MCMCOBJ=    27195.1601519658     
 iteration         1680 MCMCOBJ=    27019.4244694326     
 iteration         1700 MCMCOBJ=    26950.8467689539     
 iteration         1720 MCMCOBJ=    27092.8655175718     
 iteration         1740 MCMCOBJ=    27147.3725752741     
 iteration         1760 MCMCOBJ=    27047.3687883338     
 iteration         1780 MCMCOBJ=    26603.8589256758     
 iteration         1800 MCMCOBJ=    26259.1777452799     
 iteration         1820 MCMCOBJ=    26113.4767916874     
 iteration         1840 MCMCOBJ=    25924.4307487153     
 iteration         1860 MCMCOBJ=    26083.5227228940     
 iteration         1880 MCMCOBJ=    26259.3890129210     
 iteration         1900 MCMCOBJ=    26315.9683856917     
 iteration         1920 MCMCOBJ=    26581.7578773454     
 iteration         1940 MCMCOBJ=    26906.1676414152     
 iteration         1960 MCMCOBJ=    26988.1606250480     
 iteration         1980 MCMCOBJ=    27163.6305690629     
 iteration         2000 MCMCOBJ=    26672.3607025877     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26746.1666709967     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43287.0602686808     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26746.1666709967     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       31708.4347503019     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -10.0300439664667     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    26746.1666709967     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       26736.1366270302     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  2127.02
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    26746.167       **************************************************
 #OBJS:********************************************      403.723 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
        -2.58E-03  3.68E+00 -5.02E+00 -9.87E-01 -1.12E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.02E-02
 
 ETA2
+        2.69E-02  1.76E-01
 
 ETA3
+        6.74E-04 -4.58E-02  4.10E-02
 


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
+        6.32E-01  4.20E-01
 
 ETA3
+        3.22E-02 -5.47E-01  2.02E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.97E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.00E-03  2.93E-02  4.49E-02  1.48E-01  1.66E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.85E-04
 
 ETA2
+        2.56E-03  1.37E-02
 
 ETA3
+        1.69E-03  5.79E-03  5.81E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.76E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.38E-03
 
 ETA2
+        4.15E-02  1.62E-02
 
 ETA3
+        8.22E-02  9.27E-02  1.43E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.47E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.60E-05
 
 TH 2
+        2.78E-05  8.60E-04
 
 TH 3
+        2.97E-05  6.95E-04  2.01E-03
 
 TH 4
+        9.44E-06 -2.84E-03 -2.86E-03  2.19E-02
 
 TH 5
+        3.18E-05 -3.32E-03 -3.38E-03  1.28E-02  2.77E-02
 
 OM11
+       -2.24E-07  1.02E-07 -2.38E-06 -7.81E-07 -2.47E-06  4.69E-07
 
 OM12
+       -2.75E-07  5.78E-06 -3.21E-05 -3.62E-05 -4.76E-05  8.47E-07  6.56E-06
 
 OM13
+        9.07E-10  6.33E-07 -9.11E-06 -1.58E-05 -1.59E-05  1.48E-08  1.71E-06  2.85E-06
 
 OM22
+       -3.58E-06  1.61E-05 -2.34E-04 -1.71E-04 -1.55E-04  1.74E-06  2.27E-05  5.73E-06  1.86E-04
 
 OM23
+       -1.08E-06  9.83E-06 -3.53E-05 -7.55E-05 -4.42E-05  4.25E-08  3.55E-06  5.13E-06  2.16E-05  3.35E-05
 
 OM33
+        2.39E-06 -6.36E-06  1.90E-05  2.77E-05  7.91E-05 -1.97E-07  6.59E-07  1.48E-06  1.05E-06  9.65E-06  3.37E-05
 
 SG11
+        5.52E-05  4.87E-04  4.23E-04  5.48E-04 -4.53E-04 -1.64E-05 -4.28E-06 -2.90E-06 -1.06E-04 -4.88E-05 -8.84E-05  7.62E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.00E-03
 
 TH 2
+        2.37E-01  2.93E-02
 
 TH 3
+        1.66E-01  5.28E-01  4.49E-02
 
 TH 4
+        1.60E-02 -6.56E-01 -4.31E-01  1.48E-01
 
 TH 5
+        4.77E-02 -6.80E-01 -4.53E-01  5.20E-01  1.66E-01
 
 OM11
+       -8.17E-02  5.08E-03 -7.73E-02 -7.71E-03 -2.17E-02  6.85E-04
 
 OM12
+       -2.69E-02  7.69E-02 -2.79E-01 -9.56E-02 -1.12E-01  4.83E-01  2.56E-03
 
 OM13
+        1.34E-04  1.28E-02 -1.20E-01 -6.32E-02 -5.67E-02  1.28E-02  3.95E-01  1.69E-03
 
 OM22
+       -6.55E-02  4.03E-02 -3.83E-01 -8.45E-02 -6.81E-02  1.87E-01  6.50E-01  2.49E-01  1.37E-02
 
 OM23
+       -4.67E-02  5.79E-02 -1.36E-01 -8.81E-02 -4.58E-02  1.07E-02  2.40E-01  5.25E-01  2.74E-01  5.79E-03
 
 OM33
+        1.03E-01 -3.73E-02  7.30E-02  3.22E-02  8.18E-02 -4.96E-02  4.43E-02  1.51E-01  1.32E-02  2.87E-01  5.81E-03
 
 SG11
+        5.00E-02  6.01E-02  3.41E-02  1.34E-02 -9.85E-03 -8.67E-02 -6.06E-03 -6.23E-03 -2.83E-02 -3.05E-02 -5.51E-02  2.76E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.88E+04
 
 TH 2
+       -6.46E+03  3.62E+03
 
 TH 3
+       -9.25E+02 -4.04E+02  1.03E+03
 
 TH 4
+       -5.78E+02  2.58E+02  6.16E+01  8.99E+01
 
 TH 5
+       -7.07E+02  2.67E+02  6.47E+01 -1.16E+00  7.89E+01
 
 OM11
+        4.56E+04 -6.93E+02 -4.66E+03 -7.63E+02 -8.72E+02  3.15E+06
 
 OM12
+       -1.10E+04 -1.43E+03  3.08E+03  2.47E+02  5.11E+02 -6.18E+05  4.24E+05
 
 OM13
+       -9.58E+03  4.25E+03 -7.44E+02  2.13E+02  2.96E+02  2.78E+05 -1.84E+05  5.78E+05
 
 OM22
+        4.68E+02 -3.01E+02  1.02E+03  8.64E+01  4.83E+01  3.30E+04 -3.78E+04  1.00E+04  1.12E+04
 
 OM23
+        4.84E+03 -1.29E+03  8.56E+02  7.29E+01 -2.07E+01 -1.47E+04  1.55E+04 -7.82E+04 -3.43E+03  4.73E+04
 
 OM33
+       -4.66E+03  7.04E+02 -1.13E+03 -6.41E+01 -1.47E+02  2.53E+04 -1.02E+04  3.00E+03  3.15E+02 -1.14E+04  3.47E+04
 
 SG11
+       -3.55E+00 -1.70E+01 -2.72E+00 -2.28E+00 -1.34E+00  7.16E+02 -1.69E+02  2.30E+01  1.48E+01  6.32E+00  4.31E+01  1.35E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     2205.293
Stop Time: 
Tue 02/23/2016 
06:53 PM

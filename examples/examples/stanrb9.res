Thu 09/08/2016 
05:09 PM
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

$EST METHOD=BAYES NBURN=2000 NITER=2000 PRINT=50 MASSRESET=1
file=stanrb9_bayes.ext KAPPA=0.75
$EST METHOD=NUTS NBURN=500 NITER=2000 PRINT=20 file=stanrb9.ext
     OLKJDF=3.0 MASSRESET=0 KAPPA=1.0 MADAPT=250
  
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
 RAW OUTPUT FILE (FILE): stanrb9_bayes.ext
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
 BURN-IN ITERATIONS (NBURN):                2000
 ITERATIONS (NITER):                        2000
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
 iteration        -2000 MCMCOBJ=    170022.704591851     
 iteration        -1950 MCMCOBJ=    28524.8673475298     
 iteration        -1900 MCMCOBJ=    27987.9090420068     
 iteration        -1850 MCMCOBJ=    27944.3144116227     
 iteration        -1800 MCMCOBJ=    27740.5210337188     
 iteration        -1750 MCMCOBJ=    27340.9887951215     
 iteration        -1700 MCMCOBJ=    27395.7360833738     
 iteration        -1650 MCMCOBJ=    26913.5787311905     
 iteration        -1600 MCMCOBJ=    27202.9285437297     
 iteration        -1550 MCMCOBJ=    27612.6893076103     
 iteration        -1500 MCMCOBJ=    27407.3922058125     
 iteration        -1450 MCMCOBJ=    27798.0648212460     
 iteration        -1400 MCMCOBJ=    27473.2051728955     
 iteration        -1350 MCMCOBJ=    27118.4376686414     
 iteration        -1300 MCMCOBJ=    27382.3128956934     
 iteration        -1250 MCMCOBJ=    26854.9306325209     
 iteration        -1200 MCMCOBJ=    27137.1454199631     
 iteration        -1150 MCMCOBJ=    27098.6494038447     
 iteration        -1100 MCMCOBJ=    27238.3411091399     
 iteration        -1050 MCMCOBJ=    26911.2375704196     
 iteration        -1000 MCMCOBJ=    27082.2315072138     
 iteration         -950 MCMCOBJ=    27121.6332375378     
 iteration         -900 MCMCOBJ=    27062.8745435377     
 iteration         -850 MCMCOBJ=    26932.1978900747     
 iteration         -800 MCMCOBJ=    26568.6048957043     
 iteration         -750 MCMCOBJ=    27003.2374151800     
 iteration         -700 MCMCOBJ=    26937.8401116780     
 iteration         -650 MCMCOBJ=    26782.5795844871     
 iteration         -600 MCMCOBJ=    26680.0277810166     
 iteration         -550 MCMCOBJ=    27068.0691397711     
 iteration         -500 MCMCOBJ=    27477.0418963468     
 iteration         -450 MCMCOBJ=    26973.5382611421     
 iteration         -400 MCMCOBJ=    26877.5636304542     
 iteration         -350 MCMCOBJ=    27023.3247974210     
 iteration         -300 MCMCOBJ=    26892.1207384610     
 iteration         -250 MCMCOBJ=    26923.5515157702     
 iteration         -200 MCMCOBJ=    27115.9657372490     
 iteration         -150 MCMCOBJ=    27240.3088892076     
 iteration         -100 MCMCOBJ=    26863.4834983955     
 iteration          -50 MCMCOBJ=    27375.1661717316     
 Sampling Mode
 iteration            0 MCMCOBJ=    27205.0965914860     
 iteration           50 MCMCOBJ=    27217.3799859342     
 iteration          100 MCMCOBJ=    27101.5691009573     
 iteration          150 MCMCOBJ=    26562.6939737864     
 iteration          200 MCMCOBJ=    26711.3629684869     
 iteration          250 MCMCOBJ=    27321.8775356030     
 iteration          300 MCMCOBJ=    27020.5544195097     
 iteration          350 MCMCOBJ=    26984.6246138654     
 iteration          400 MCMCOBJ=    27282.3260957183     
 iteration          450 MCMCOBJ=    27201.2249026557     
 iteration          500 MCMCOBJ=    26836.9916310219     
 iteration          550 MCMCOBJ=    26799.5316059692     
 iteration          600 MCMCOBJ=    26858.0596310296     
 iteration          650 MCMCOBJ=    27086.8580958628     
 iteration          700 MCMCOBJ=    27164.9268671869     
 iteration          750 MCMCOBJ=    27185.2775991081     
 iteration          800 MCMCOBJ=    27289.7649489430     
 iteration          850 MCMCOBJ=    27356.5759293215     
 iteration          900 MCMCOBJ=    27407.8873551576     
 iteration          950 MCMCOBJ=    27058.6234619846     
 iteration         1000 MCMCOBJ=    27024.6159517194     
 iteration         1050 MCMCOBJ=    26835.0864011288     
 iteration         1100 MCMCOBJ=    26719.2271430842     
 iteration         1150 MCMCOBJ=    26934.0715299799     
 iteration         1200 MCMCOBJ=    27168.7924746911     
 iteration         1250 MCMCOBJ=    26910.0829321649     
 iteration         1300 MCMCOBJ=    27383.3546599500     
 iteration         1350 MCMCOBJ=    27093.0298404098     
 iteration         1400 MCMCOBJ=    27167.7582638822     
 iteration         1450 MCMCOBJ=    27178.6877830146     
 iteration         1500 MCMCOBJ=    26773.9919467079     
 iteration         1550 MCMCOBJ=    27232.7990602209     
 iteration         1600 MCMCOBJ=    26803.6933217608     
 iteration         1650 MCMCOBJ=    27196.0227938632     
 iteration         1700 MCMCOBJ=    27299.7507779714     
 iteration         1750 MCMCOBJ=    27223.8578136122     
 iteration         1800 MCMCOBJ=    27406.6553420653     
 iteration         1850 MCMCOBJ=    27567.7955333348     
 iteration         1900 MCMCOBJ=    27290.7654492855     
 iteration         1950 MCMCOBJ=    27267.6268693385     
 iteration         2000 MCMCOBJ=    27347.1018519686     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27095.8443384039     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43636.7379360880     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27095.8443384039     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32058.1124177091     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    16.0970839384867     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27095.8443384039     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27111.9414223424     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   213.25
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27095.844       **************************************************
 #OBJS:********************************************      250.590 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         9.84E-03  3.67E+00 -4.99E+00 -9.14E-01 -1.17E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.99E-03
 
 ETA2
+        2.51E-02  1.62E-01
 
 ETA3
+       -5.79E-04 -4.06E-02  4.81E-02
 


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
+        6.24E-01  4.02E-01
 
 ETA3
+       -2.65E-02 -4.65E-01  2.19E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.95E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.02E-03  2.83E-02  4.02E-02  1.28E-01  1.66E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.71E-04
 
 ETA2
+        2.51E-03  1.29E-02
 
 ETA3
+        1.80E-03  5.92E-03  6.61E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.71E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.35E-03
 
 ETA2
+        4.50E-02  1.61E-02
 
 ETA3
+        8.15E-02  8.32E-02  1.50E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.42E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.61E-05
 
 TH 2
+        2.65E-05  7.98E-04
 
 TH 3
+        2.68E-05  6.55E-04  1.61E-03
 
 TH 4
+        1.00E-05 -2.35E-03 -2.56E-03  1.64E-02
 
 TH 5
+        1.91E-05 -3.10E-03 -3.34E-03  1.01E-02  2.74E-02
 
 OM11
+       -1.63E-07 -5.72E-07 -2.90E-06  2.63E-06  6.68E-07  4.50E-07
 
 OM12
+       -2.39E-07  4.57E-09 -1.88E-05 -1.36E-05 -2.32E-06  7.85E-07  6.30E-06
 
 OM13
+       -2.42E-08  2.09E-06 -2.00E-06 -1.93E-05 -6.05E-06 -1.09E-07  1.84E-06  3.24E-06
 
 OM22
+       -8.96E-06  1.60E-05 -8.59E-05 -1.90E-04 -1.85E-04  1.32E-06  1.95E-05  7.42E-06  1.68E-04
 
 OM23
+       -1.86E-06  4.26E-06 -2.78E-06 -2.17E-05 -4.07E-05 -3.28E-07  3.69E-06  6.35E-06  2.33E-05  3.50E-05
 
 OM33
+        4.54E-06  1.34E-06  2.96E-05  5.48E-05  6.13E-06 -1.67E-07  4.83E-07  4.97E-07 -1.58E-05  7.53E-06  4.37E-05
 
 SG11
+        5.83E-05 -1.09E-04 -4.14E-04  1.00E-03  1.52E-03 -7.97E-06  3.64E-05 -8.29E-06  6.32E-05 -4.94E-05 -8.73E-05  7.34E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.02E-03
 
 TH 2
+        2.34E-01  2.83E-02
 
 TH 3
+        1.66E-01  5.77E-01  4.02E-02
 
 TH 4
+        1.95E-02 -6.49E-01 -4.99E-01  1.28E-01
 
 TH 5
+        2.87E-02 -6.62E-01 -5.02E-01  4.77E-01  1.66E-01
 
 OM11
+       -6.05E-02 -3.02E-02 -1.08E-01  3.07E-02  6.02E-03  6.71E-04
 
 OM12
+       -2.37E-02  6.44E-05 -1.87E-01 -4.24E-02 -5.57E-03  4.66E-01  2.51E-03
 
 OM13
+       -3.35E-03  4.11E-02 -2.76E-02 -8.39E-02 -2.03E-02 -9.02E-02  4.07E-01  1.80E-03
 
 OM22
+       -1.72E-01  4.36E-02 -1.65E-01 -1.14E-01 -8.61E-02  1.52E-01  6.01E-01  3.19E-01  1.29E-02
 
 OM23
+       -7.83E-02  2.55E-02 -1.17E-02 -2.87E-02 -4.16E-02 -8.26E-02  2.48E-01  5.96E-01  3.05E-01  5.92E-03
 
 OM33
+        1.71E-01  7.18E-03  1.12E-01  6.47E-02  5.60E-03 -3.77E-02  2.91E-02  4.18E-02 -1.85E-01  1.93E-01  6.61E-03
 
 SG11
+        5.35E-02 -1.42E-02 -3.81E-02  2.89E-02  3.38E-02 -4.39E-02  5.34E-02 -1.70E-02  1.80E-02 -3.08E-02 -4.87E-02  2.71E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.97E+04
 
 TH 2
+       -6.19E+03  3.69E+03
 
 TH 3
+       -9.55E+02 -3.85E+02  1.13E+03
 
 TH 4
+       -6.83E+02  3.10E+02  9.13E+01  1.21E+02
 
 TH 5
+       -5.84E+02  2.58E+02  6.43E+01  2.63E+00  7.33E+01
 
 OM11
+        3.98E+04 -2.27E+03 -2.16E+02 -8.12E+02  1.39E+02  3.41E+06
 
 OM12
+       -1.82E+04  3.26E+02  2.52E+03  3.26E+02  2.74E+01 -6.59E+05  4.18E+05
 
 OM13
+       -1.04E+04  6.67E+02 -5.02E+02  4.94E+02 -1.59E+02  3.95E+05 -1.94E+05  5.98E+05
 
 OM22
+        3.75E+03 -2.01E+02  4.26E+02  7.71E+01  6.42E+01  3.60E+04 -3.83E+04  6.96E+03  1.16E+04
 
 OM23
+        6.70E+03 -4.36E+02 -1.06E+02 -1.49E+02  9.37E+00  2.96E+03  1.42E+04 -9.28E+04 -5.29E+03  4.99E+04
 
 OM33
+       -5.92E+03  3.45E+02 -6.28E+02 -1.12E+02  1.85E+01  2.69E+04 -2.20E+04  1.65E+04  4.86E+03 -1.00E+04  2.78E+04
 
 SG11
+       -5.00E+01 -1.39E+00  1.49E+00 -6.78E-01 -3.84E-01  7.16E+02 -2.60E+02  1.60E+02  1.26E+01  4.30E+00  4.06E+01  1.39E+01
 
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
 RAW OUTPUT FILE (FILE): stanrb9.ext
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
 MASS MATRIX ACCUMULATION ITERATIONS (MADAPT):          250
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
 iteration         -500 MCMCOBJ=    29062.5984561532     
 iteration         -480 MCMCOBJ=    27916.3625176194     
 iteration         -460 MCMCOBJ=    27498.7861387603     
 iteration         -440 MCMCOBJ=    27204.2238447550     
 iteration         -420 MCMCOBJ=    26737.0087396048     
 iteration         -400 MCMCOBJ=    26402.4582915717     
 iteration         -380 MCMCOBJ=    26761.3081442791     
 iteration         -360 MCMCOBJ=    26770.2538208055     
 iteration         -340 MCMCOBJ=    27000.1677452027     
 iteration         -320 MCMCOBJ=    26648.0155818298     
 iteration         -300 MCMCOBJ=    26750.5251678552     
 iteration         -280 MCMCOBJ=    26958.4426521771     
 iteration         -260 MCMCOBJ=    27119.8122175082     
 iteration         -240 MCMCOBJ=    27708.7607468834     
 iteration         -220 MCMCOBJ=    27267.3372439575     
 iteration         -200 MCMCOBJ=    26970.9940285149     
 iteration         -180 MCMCOBJ=    26800.3348144604     
 iteration         -160 MCMCOBJ=    26813.3768165808     
 iteration         -140 MCMCOBJ=    26403.6362114462     
 iteration         -120 MCMCOBJ=    26517.5692093689     
 iteration         -100 MCMCOBJ=    26698.5714984153     
 iteration          -80 MCMCOBJ=    27071.1286571934     
 iteration          -60 MCMCOBJ=    27273.4399974824     
 iteration          -40 MCMCOBJ=    27112.4262387449     
 iteration          -20 MCMCOBJ=    27055.1065673681     
 Sampling Mode
 iteration            0 MCMCOBJ=    27356.7309990005     
 iteration           20 MCMCOBJ=    27038.1888081999     
 iteration           40 MCMCOBJ=    27221.1141141723     
 iteration           60 MCMCOBJ=    26866.0951967084     
 iteration           80 MCMCOBJ=    27328.3975362786     
 iteration          100 MCMCOBJ=    27094.9135023946     
 iteration          120 MCMCOBJ=    27083.3679519262     
 iteration          140 MCMCOBJ=    27045.5526885287     
 iteration          160 MCMCOBJ=    26963.9521035329     
 iteration          180 MCMCOBJ=    27290.9584130164     
 iteration          200 MCMCOBJ=    27085.6502971336     
 iteration          220 MCMCOBJ=    26689.3362105974     
 iteration          240 MCMCOBJ=    26880.1962487740     
 iteration          260 MCMCOBJ=    27268.3291511500     
 iteration          280 MCMCOBJ=    27230.8204405921     
 iteration          300 MCMCOBJ=    27369.2999173914     
 iteration          320 MCMCOBJ=    27173.4272825922     
 iteration          340 MCMCOBJ=    27511.9751974781     
 iteration          360 MCMCOBJ=    27481.0919156169     
 iteration          380 MCMCOBJ=    27691.1122319412     
 iteration          400 MCMCOBJ=    27326.1236426711     
 iteration          420 MCMCOBJ=    27398.7510858885     
 iteration          440 MCMCOBJ=    27306.5390109119     
 iteration          460 MCMCOBJ=    27494.6647156909     
 iteration          480 MCMCOBJ=    27510.2720683570     
 iteration          500 MCMCOBJ=    27154.3495658010     
 iteration          520 MCMCOBJ=    27387.5226251510     
 iteration          540 MCMCOBJ=    26801.5154548882     
 iteration          560 MCMCOBJ=    26822.1921479876     
 iteration          580 MCMCOBJ=    26994.7906658753     
 iteration          600 MCMCOBJ=    27247.5993399001     
 iteration          620 MCMCOBJ=    27332.9011405076     
 iteration          640 MCMCOBJ=    27429.9058273622     
 iteration          660 MCMCOBJ=    27237.6519992042     
 iteration          680 MCMCOBJ=    27380.7560141268     
 iteration          700 MCMCOBJ=    27311.4224648932     
 iteration          720 MCMCOBJ=    27271.3345857613     
 iteration          740 MCMCOBJ=    27738.1595900799     
 iteration          760 MCMCOBJ=    27120.7616317293     
 iteration          780 MCMCOBJ=    26826.6974615882     
 iteration          800 MCMCOBJ=    26943.9387721685     
 iteration          820 MCMCOBJ=    26889.1542909000     
 iteration          840 MCMCOBJ=    26515.1965671653     
 iteration          860 MCMCOBJ=    26348.5450945531     
 iteration          880 MCMCOBJ=    26721.3927971027     
 iteration          900 MCMCOBJ=    27021.2791577732     
 iteration          920 MCMCOBJ=    27413.2052645237     
 iteration          940 MCMCOBJ=    27293.3687996584     
 iteration          960 MCMCOBJ=    27529.2314946242     
 iteration          980 MCMCOBJ=    27105.0504774649     
 iteration         1000 MCMCOBJ=    26987.9124579404     
 iteration         1020 MCMCOBJ=    26859.9708436185     
 iteration         1040 MCMCOBJ=    26781.1489921925     
 iteration         1060 MCMCOBJ=    27217.4556897324     
 iteration         1080 MCMCOBJ=    27049.4997107606     
 iteration         1100 MCMCOBJ=    26686.8303379385     
 iteration         1120 MCMCOBJ=    26477.4670670870     
 iteration         1140 MCMCOBJ=    26663.1643995095     
 iteration         1160 MCMCOBJ=    26197.3767879844     
 iteration         1180 MCMCOBJ=    26704.1132315807     
 iteration         1200 MCMCOBJ=    26992.8430873266     
 iteration         1220 MCMCOBJ=    27082.9979876055     
 iteration         1240 MCMCOBJ=    27266.3605301867     
 iteration         1260 MCMCOBJ=    27054.6841393876     
 iteration         1280 MCMCOBJ=    27055.4965575105     
 iteration         1300 MCMCOBJ=    27204.5720253189     
 iteration         1320 MCMCOBJ=    26863.5801489396     
 iteration         1340 MCMCOBJ=    26773.2718883884     
 iteration         1360 MCMCOBJ=    26906.9531200932     
 iteration         1380 MCMCOBJ=    27166.0431790699     
 iteration         1400 MCMCOBJ=    27392.7374389277     
 iteration         1420 MCMCOBJ=    27157.9293158523     
 iteration         1440 MCMCOBJ=    27117.7562025515     
 iteration         1460 MCMCOBJ=    26796.9761169715     
 iteration         1480 MCMCOBJ=    27097.1436087420     
 iteration         1500 MCMCOBJ=    27445.8408824474     
 iteration         1520 MCMCOBJ=    27499.6935185505     
 iteration         1540 MCMCOBJ=    27399.8192647336     
 iteration         1560 MCMCOBJ=    27402.2086832601     
 iteration         1580 MCMCOBJ=    27504.3116026678     
 iteration         1600 MCMCOBJ=    27230.1327668895     
 iteration         1620 MCMCOBJ=    26846.3042976848     
 iteration         1640 MCMCOBJ=    26844.7780288143     
 iteration         1660 MCMCOBJ=    26484.1592191624     
 iteration         1680 MCMCOBJ=    26595.9422192253     
 iteration         1700 MCMCOBJ=    26587.9703292196     
 iteration         1720 MCMCOBJ=    26612.8789860689     
 iteration         1740 MCMCOBJ=    26984.2077933152     
 iteration         1760 MCMCOBJ=    26923.1802697085     
 iteration         1780 MCMCOBJ=    27227.9707319603     
 iteration         1800 MCMCOBJ=    27645.4559915083     
 iteration         1820 MCMCOBJ=    27360.4874993803     
 iteration         1840 MCMCOBJ=    27755.8399774784     
 iteration         1860 MCMCOBJ=    27659.9059910013     
 iteration         1880 MCMCOBJ=    27352.9397850900     
 iteration         1900 MCMCOBJ=    27530.6469579544     
 iteration         1920 MCMCOBJ=    27697.1902208939     
 iteration         1940 MCMCOBJ=    27576.2346071326     
 iteration         1960 MCMCOBJ=    27308.2085467642     
 iteration         1980 MCMCOBJ=    27134.8842464406     
 iteration         2000 MCMCOBJ=    27224.3156745353     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27140.8439555809     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       43681.7375532650     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27140.8439555809     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       32103.1120348861     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -7.99194303480276     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    27140.8439555809     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       27132.8520125461     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   763.48
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    27140.844       **************************************************
 #OBJS:********************************************      330.753 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.01E-02  3.67E+00 -4.99E+00 -9.13E-01 -1.17E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.92E-03
 
 ETA2
+        2.54E-02  1.63E-01
 
 ETA3
+       -7.40E-05 -3.81E-02  5.11E-02
 


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
+        6.33E-01  4.03E-01
 
 ETA3
+       -3.80E-03 -4.26E-01  2.26E-01
 


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
 
         3.87E-03  2.84E-02  4.65E-02  1.34E-01  1.62E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.58E-04
 
 ETA2
+        2.61E-03  1.41E-02
 
 ETA3
+        1.92E-03  7.28E-03  7.29E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.70E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.30E-03
 
 ETA2
+        4.63E-02  1.76E-02
 
 ETA3
+        8.53E-02  1.05E-01  1.61E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        3.41E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        1.49E-05
 
 TH 2
+        3.28E-05  8.06E-04
 
 TH 3
+        4.25E-05  7.12E-04  2.17E-03
 
 TH 4
+       -1.33E-05 -2.51E-03 -2.92E-03  1.80E-02
 
 TH 5
+       -1.78E-06 -3.02E-03 -3.51E-03  1.14E-02  2.63E-02
 
 OM11
+       -2.02E-07  3.01E-08 -3.35E-06  1.91E-06 -1.58E-06  4.33E-07
 
 OM12
+       -8.49E-07 -1.91E-06 -3.75E-05 -5.43E-06  1.09E-05  8.14E-07  6.79E-06
 
 OM13
+       -2.64E-07  2.32E-07 -1.05E-05 -9.50E-06  4.53E-06 -2.48E-08  2.34E-06  3.68E-06
 
 OM22
+       -6.52E-06 -1.78E-05 -2.43E-04 -7.67E-05 -5.02E-05  1.65E-06  2.23E-05  8.16E-06  2.00E-04
 
 OM23
+       -1.04E-07 -3.08E-06 -6.77E-05  1.49E-06  1.51E-05 -4.75E-08  4.83E-06  7.30E-06  4.74E-05  5.29E-05
 
 OM33
+        2.86E-06 -8.91E-06 -1.29E-06  4.05E-05  7.33E-05 -1.75E-07  7.71E-07  1.38E-06  1.31E-05  2.38E-05  5.32E-05
 
 SG11
+        1.97E-05  3.18E-04  5.79E-04 -1.09E-03  3.21E-04 -1.71E-05  8.39E-06 -1.79E-05 -2.03E-04 -2.17E-04 -1.58E-04  7.29E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        3.87E-03
 
 TH 2
+        2.99E-01  2.84E-02
 
 TH 3
+        2.36E-01  5.38E-01  4.65E-02
 
 TH 4
+       -2.57E-02 -6.58E-01 -4.68E-01  1.34E-01
 
 TH 5
+       -2.84E-03 -6.57E-01 -4.64E-01  5.23E-01  1.62E-01
 
 OM11
+       -7.94E-02  1.61E-03 -1.09E-01  2.17E-02 -1.48E-02  6.58E-04
 
 OM12
+       -8.42E-02 -2.58E-02 -3.09E-01 -1.55E-02  2.58E-02  4.75E-01  2.61E-03
 
 OM13
+       -3.56E-02  4.26E-03 -1.18E-01 -3.69E-02  1.45E-02 -1.96E-02  4.69E-01  1.92E-03
 
 OM22
+       -1.19E-01 -4.44E-02 -3.69E-01 -4.05E-02 -2.19E-02  1.77E-01  6.05E-01  3.01E-01  1.41E-02
 
 OM23
+       -3.71E-03 -1.49E-02 -2.00E-01  1.52E-03  1.28E-02 -9.93E-03  2.55E-01  5.23E-01  4.61E-01  7.28E-03
 
 OM33
+        1.02E-01 -4.30E-02 -3.79E-03  4.15E-02  6.20E-02 -3.65E-02  4.06E-02  9.88E-02  1.27E-01  4.48E-01  7.29E-03
 
 SG11
+        1.89E-02  4.15E-02  4.60E-02 -3.03E-02  7.34E-03 -9.62E-02  1.19E-02 -3.45E-02 -5.32E-02 -1.11E-01 -8.01E-02  2.70E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        8.72E+04
 
 TH 2
+       -7.13E+03  3.67E+03
 
 TH 3
+       -1.41E+03 -2.69E+02  9.23E+02
 
 TH 4
+       -7.23E+02  3.00E+02  7.77E+01  1.13E+02
 
 TH 5
+       -6.72E+02  2.55E+02  6.01E+01 -3.90E+00  7.74E+01
 
 OM11
+        4.20E+04 -4.27E+03 -2.42E+03 -1.24E+03  1.63E+02  3.52E+06
 
 OM12
+       -7.72E+03 -1.54E+03  3.00E+03  2.55E+02 -1.23E+02 -6.72E+05  4.27E+05
 
 OM13
+        8.99E+03  4.48E+02 -2.19E+03  4.81E+01 -2.07E+02  4.31E+05 -2.53E+05  5.40E+05
 
 OM22
+        7.38E+02  2.51E+02  7.20E+02  1.26E+02  1.07E+02  3.34E+04 -3.63E+04  1.77E+04  1.10E+04
 
 OM23
+       -1.11E+03 -6.97E+02  7.89E+02 -2.21E+01  3.37E+01 -3.15E+04  3.46E+04 -7.90E+04 -8.48E+03  4.22E+04
 
 OM33
+       -4.08E+03  6.30E+02 -6.22E+02 -2.00E+01 -6.16E+01  1.68E+04 -9.01E+03  2.22E+04  1.09E+03 -1.53E+04  2.55E+04
 
 SG11
+        1.37E+01 -9.28E+00 -3.30E+00 -8.43E-02 -1.54E+00  1.05E+03 -2.97E+02  1.39E+02  1.85E+01  3.48E+01  2.63E+01  1.43E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,      953.306
Stop Time: 
Thu 09/08/2016 
05:26 PM

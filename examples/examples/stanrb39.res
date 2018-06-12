Tue 10/04/2016 
09:13 AM
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

$EST METHOD=NUTS AUTO=2 PRINT=20 OLKJDF=1
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  121) INTERACTION IS IMPLIED WITH EM/BAYES ESTIMATION METHODS

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.

 (MU_WARNING 6) THETA(003): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.

 (MU_WARNING 6) THETA(002): SHOULD BE ASSOCIATED WITH ONLY ONE MU_.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        4 OCT 2016
Days until program expires :4988
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
 RAW OUTPUT FILE (FILE): stanrb39.ext
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
 AUTOMATIC SETTING FEATURE (AUTO):          OLD
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
 MASS MATRIX BLOCKING TYPE (NUTS_MASS):                 BD
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
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       75.0000000000000
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): 25.0000000000000
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 50.0000000000000
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
 iteration         -500 MCMCOBJ=    73531.1146014491     
 iteration         -480 MCMCOBJ=    36082.8188779613     
 iteration         -460 MCMCOBJ=    36438.1245062945     
 iteration         -440 MCMCOBJ=    36466.6790978879     
 iteration         -420 MCMCOBJ=    36463.0883872713     
 iteration         -400 MCMCOBJ=    36406.8375513422     
 iteration         -380 MCMCOBJ=    36496.4635496358     
 iteration         -360 MCMCOBJ=    36379.1632371041     
 iteration         -340 MCMCOBJ=    36465.0081229564     
 iteration         -320 MCMCOBJ=    36509.0906277286     
 iteration         -300 MCMCOBJ=    36391.8370018374     
 iteration         -280 MCMCOBJ=    36533.2844251791     
 iteration         -260 MCMCOBJ=    36650.8225633811     
 iteration         -240 MCMCOBJ=    36517.7208821365     
 iteration         -220 MCMCOBJ=    36366.3993275997     
 iteration         -200 MCMCOBJ=    36557.4286234115     
 iteration         -180 MCMCOBJ=    36394.4150370508     
 iteration         -160 MCMCOBJ=    36483.3700579149     
 iteration         -140 MCMCOBJ=    36398.9567280583     
 iteration         -120 MCMCOBJ=    36522.2007714640     
 iteration         -100 MCMCOBJ=    36594.9666852854     
 iteration          -80 MCMCOBJ=    36738.5958318389     
 iteration          -60 MCMCOBJ=    36367.7633591335     
 iteration          -40 MCMCOBJ=    36416.3419530430     
 iteration          -20 MCMCOBJ=    36449.9717278395     
 Sampling Mode
 iteration            0 MCMCOBJ=    36600.6412411406     
 iteration           20 MCMCOBJ=    36481.4903667801     
 iteration           40 MCMCOBJ=    36535.2345787651     
 iteration           60 MCMCOBJ=    36458.1501051782     
 iteration           80 MCMCOBJ=    36401.6374871869     
 iteration          100 MCMCOBJ=    36321.1446769956     
 iteration          120 MCMCOBJ=    36398.7900577989     
 iteration          140 MCMCOBJ=    36351.5328107939     
 iteration          160 MCMCOBJ=    36420.1175929352     
 iteration          180 MCMCOBJ=    36645.8192446251     
 iteration          200 MCMCOBJ=    36415.8702060790     
 iteration          220 MCMCOBJ=    36295.5684874057     
 iteration          240 MCMCOBJ=    36446.3465274656     
 iteration          260 MCMCOBJ=    36459.1355171989     
 iteration          280 MCMCOBJ=    36694.3276381545     
 iteration          300 MCMCOBJ=    36509.6408959996     
 iteration          320 MCMCOBJ=    36419.8782786507     
 iteration          340 MCMCOBJ=    36503.9581011766     
 iteration          360 MCMCOBJ=    36249.8084196011     
 iteration          380 MCMCOBJ=    36441.8356509229     
 iteration          400 MCMCOBJ=    36301.3974440012     
 iteration          420 MCMCOBJ=    36290.2289140004     
 iteration          440 MCMCOBJ=    36445.2773734545     
 iteration          460 MCMCOBJ=    36320.5686704048     
 iteration          480 MCMCOBJ=    36628.4775179452     
 iteration          500 MCMCOBJ=    36518.0007703570     
 iteration          520 MCMCOBJ=    36468.2934581757     
 iteration          540 MCMCOBJ=    36403.4594697022     
 iteration          560 MCMCOBJ=    36639.5658375978     
 iteration          580 MCMCOBJ=    36518.3122488825     
 iteration          600 MCMCOBJ=    36391.3230627930     
 iteration          620 MCMCOBJ=    36577.9325191316     
 iteration          640 MCMCOBJ=    36452.9295328289     
 iteration          660 MCMCOBJ=    36590.8565006416     
 iteration          680 MCMCOBJ=    36461.1524340221     
 iteration          700 MCMCOBJ=    36370.5151671608     
 iteration          720 MCMCOBJ=    36489.9739023071     
 iteration          740 MCMCOBJ=    36516.9844848097     
 iteration          760 MCMCOBJ=    36438.8706759195     
 iteration          780 MCMCOBJ=    36628.6865431271     
 iteration          800 MCMCOBJ=    36551.9041461602     
 iteration          820 MCMCOBJ=    36344.5164493683     
 iteration          840 MCMCOBJ=    36562.9773430517     
 iteration          860 MCMCOBJ=    36486.8801704185     
 iteration          880 MCMCOBJ=    36356.0826841457     
 iteration          900 MCMCOBJ=    36387.6812479852     
 iteration          920 MCMCOBJ=    36435.4236299923     
 iteration          940 MCMCOBJ=    36431.4449488078     
 iteration          960 MCMCOBJ=    36328.6822815257     
 iteration          980 MCMCOBJ=    36648.9193814818     
 iteration         1000 MCMCOBJ=    36524.2573137479     
 iteration         1020 MCMCOBJ=    36597.4329715567     
 iteration         1040 MCMCOBJ=    36646.8629714469     
 iteration         1060 MCMCOBJ=    36522.4607393156     
 iteration         1080 MCMCOBJ=    36410.6788520760     
 iteration         1100 MCMCOBJ=    36556.1290268969     
 iteration         1120 MCMCOBJ=    36600.3009102818     
 iteration         1140 MCMCOBJ=    36595.3409658743     
 iteration         1160 MCMCOBJ=    36224.6046735136     
 iteration         1180 MCMCOBJ=    36350.4225900571     
 iteration         1200 MCMCOBJ=    36673.3343389067     
 iteration         1220 MCMCOBJ=    36284.7428750628     
 iteration         1240 MCMCOBJ=    36567.8869488512     
 iteration         1260 MCMCOBJ=    36433.0451638119     
 iteration         1280 MCMCOBJ=    36485.2600950050     
 iteration         1300 MCMCOBJ=    36471.7909078393     
 iteration         1320 MCMCOBJ=    36595.2423176727     
 iteration         1340 MCMCOBJ=    36579.9634156710     
 iteration         1360 MCMCOBJ=    36339.7748500885     
 iteration         1380 MCMCOBJ=    36442.1994360787     
 iteration         1400 MCMCOBJ=    36442.1597266912     
 iteration         1420 MCMCOBJ=    36351.6053568300     
 iteration         1440 MCMCOBJ=    36457.6185864085     
 iteration         1460 MCMCOBJ=    36330.6277752433     
 iteration         1480 MCMCOBJ=    36631.7486903024     
 iteration         1500 MCMCOBJ=    36423.2285800274     
 iteration         1520 MCMCOBJ=    36516.3801160706     
 iteration         1540 MCMCOBJ=    36534.9796461204     
 iteration         1560 MCMCOBJ=    36346.5718201429     
 iteration         1580 MCMCOBJ=    36491.1651455772     
 iteration         1600 MCMCOBJ=    36391.6829579816     
 iteration         1620 MCMCOBJ=    36432.1416689756     
 iteration         1640 MCMCOBJ=    36465.1135128671     
 iteration         1660 MCMCOBJ=    36366.1207683012     
 iteration         1680 MCMCOBJ=    36578.4436025094     
 iteration         1700 MCMCOBJ=    36349.7404392342     
 iteration         1720 MCMCOBJ=    36437.3216045076     
 iteration         1740 MCMCOBJ=    36326.3265219578     
 iteration         1760 MCMCOBJ=    36568.9130328375     
 iteration         1780 MCMCOBJ=    36526.5256601696     
 iteration         1800 MCMCOBJ=    36635.0380730641     
 iteration         1820 MCMCOBJ=    36423.1657637225     
 iteration         1840 MCMCOBJ=    36580.7869582263     
 iteration         1860 MCMCOBJ=    36336.7383397831     
 iteration         1880 MCMCOBJ=    36358.3003931634     
 iteration         1900 MCMCOBJ=    36503.7218810399     
 iteration         1920 MCMCOBJ=    36471.3475021381     
 iteration         1940 MCMCOBJ=    36447.0422583768     
 iteration         1960 MCMCOBJ=    36390.6550923278     
 iteration         1980 MCMCOBJ=    36550.7163799006     
 iteration         2000 MCMCOBJ=    36328.3001776541     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         9000
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    16540.8935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    36473.3282159730     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       53014.2218136571     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          2700
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4962.26807930523     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    36473.3282159730     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       41435.5962952782     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -4.85471119897507     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    36473.3282159730     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       36468.4735047740     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  4682.28
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    36473.328       **************************************************
 #OBJS:********************************************      102.755 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         9.95E-03  3.67E+00 -4.99E+00 -9.21E-01 -1.17E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.97E-03
 
 ETA2
+        2.59E-02  1.59E-01
 
 ETA3
+        7.96E-05 -4.27E-02  4.67E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.57E+01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.98E-02
 
 ETA2
+        6.51E-01  3.98E-01
 
 ETA3
+        2.84E-03 -5.09E-01  2.15E-01
 


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
 
         4.09E-03  2.75E-02  4.45E-02  1.31E-01  1.61E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        6.67E-04
 
 ETA2
+        2.60E-03  1.37E-02
 
 ETA3
+        1.97E-03  7.60E-03  7.89E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.63E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.34E-03
 
 ETA2
+        4.76E-02  1.71E-02
 
 ETA3
+        9.02E-02  1.25E-01  1.82E-02
 


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
+        1.67E-05
 
 TH 2
+        2.64E-05  7.57E-04
 
 TH 3
+        3.65E-05  6.56E-04  1.98E-03
 
 TH 4
+        1.16E-05 -2.33E-03 -2.71E-03  1.71E-02
 
 TH 5
+        1.40E-05 -2.89E-03 -3.12E-03  1.04E-02  2.58E-02
 
 OM11
+       -1.70E-07 -5.72E-07 -2.25E-06 -4.11E-07  4.65E-06  4.45E-07
 
 OM12
+       -3.35E-07 -3.48E-06 -2.81E-05 -1.88E-05  1.33E-05  7.82E-07  6.77E-06
 
 OM13
+       -2.04E-07 -2.23E-06 -6.46E-06 -6.07E-06  1.72E-06 -3.69E-08  2.57E-06  3.89E-06
 
 OM22
+       -5.96E-06 -2.44E-05 -2.13E-04 -4.48E-05 -4.56E-05  1.45E-06  2.11E-05  7.31E-06  1.88E-04
 
 OM23
+       -1.51E-06 -7.22E-06 -5.71E-05  2.69E-05 -3.33E-05 -1.77E-07  3.86E-06  6.55E-06  4.47E-05  5.78E-05
 
 OM33
+        2.55E-06 -5.60E-06 -7.12E-06  9.49E-05  1.37E-05 -1.74E-07  1.47E-07  1.80E-06  1.53E-05  3.36E-05  6.22E-05
 
 SG11
+        6.85E-05  9.33E-05  4.46E-04 -5.14E-04 -6.56E-04 -1.93E-05 -1.58E-05 -1.05E-05 -2.32E-04 -2.45E-04 -2.05E-04  6.93E-02
 
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
+        2.35E-01  2.75E-02
 
 TH 3
+        2.01E-01  5.36E-01  4.45E-02
 
 TH 4
+        2.17E-02 -6.49E-01 -4.66E-01  1.31E-01
 
 TH 5
+        2.13E-02 -6.54E-01 -4.36E-01  4.93E-01  1.61E-01
 
 OM11
+       -6.24E-02 -3.12E-02 -7.60E-02 -4.72E-03  4.34E-02  6.67E-04
 
 OM12
+       -3.15E-02 -4.86E-02 -2.42E-01 -5.53E-02  3.17E-02  4.51E-01  2.60E-03
 
 OM13
+       -2.53E-02 -4.11E-02 -7.36E-02 -2.36E-02  5.44E-03 -2.80E-02  5.00E-01  1.97E-03
 
 OM22
+       -1.06E-01 -6.47E-02 -3.49E-01 -2.50E-02 -2.07E-02  1.59E-01  5.92E-01  2.70E-01  1.37E-02
 
 OM23
+       -4.85E-02 -3.45E-02 -1.69E-01  2.71E-02 -2.73E-02 -3.50E-02  1.95E-01  4.37E-01  4.29E-01  7.60E-03
 
 OM33
+        7.89E-02 -2.58E-02 -2.03E-02  9.21E-02  1.08E-02 -3.31E-02  7.18E-03  1.16E-01  1.41E-01  5.60E-01  7.89E-03
 
 SG11
+        6.37E-02  1.29E-02  3.81E-02 -1.50E-02 -1.55E-02 -1.10E-01 -2.30E-02 -2.02E-02 -6.43E-02 -1.23E-01 -9.88E-02  2.63E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.43E+04
 
 TH 2
+       -5.73E+03  3.73E+03
 
 TH 3
+       -1.32E+03 -2.86E+02  9.43E+02
 
 TH 4
+       -7.04E+02  3.11E+02  8.69E+01  1.18E+02
 
 TH 5
+       -5.55E+02  2.62E+02  4.97E+01 -1.54E+00  7.54E+01
 
 OM11
+        4.32E+04 -1.34E+03 -3.56E+03 -8.27E+02 -5.17E+02  3.37E+06
 
 OM12
+       -1.70E+04  1.19E+01  2.51E+03  6.99E+02 -8.52E+01 -6.61E+05  4.43E+05
 
 OM13
+        6.00E+03  1.99E+03 -2.54E+03 -1.61E+02 -5.14E+01  4.37E+05 -2.75E+05  5.04E+05
 
 OM22
+        9.73E+02  1.25E+02  7.47E+02  7.94E+01  9.40E+01  3.42E+04 -3.94E+04  1.94E+04  1.15E+04
 
 OM23
+        1.78E+03 -3.21E+02  6.65E+02  5.21E+01  6.13E+01 -2.28E+04  3.32E+04 -6.44E+04 -8.25E+03  3.99E+04
 
 OM33
+       -3.92E+03  1.09E+02 -4.94E+02 -1.58E+02 -1.52E+01  4.20E+03 -3.78E+03  1.73E+04  1.21E+03 -1.77E+04  2.53E+04
 
 SG11
+       -6.07E+01  6.81E+00 -6.72E-01  4.65E-01  8.94E-01  8.69E+02 -1.45E+02  2.94E+01  1.20E+01  4.79E+01  2.47E+01  1.50E+01
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     4667.971
Stop Time: 
Tue 10/04/2016 
10:31 AM

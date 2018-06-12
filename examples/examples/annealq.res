Mon 09/30/2013 
02:11 PM
$PROB Emax model with hill=3
$INPUT ID DOSE DV
$DATA anneal.dat IGNORE=@
$PRED
 
 MU_1 = THETA(1)
 EMAX = EXP(MU_1+ETA(1))
 MU_2 = THETA(2)
 ED50 = EXP(MU_2+ETA(2))
 MU_3 = THETA(4)
 E0   = EXP(MU_3+ETA(3))

 MU_4=THETA(3)
 HILL = EXP(MU_4+ETA(4))

 IPRED = E0+EMAX*DOSE**HILL/(ED50**HILL+DOSE**HILL)
 Y     = IPRED + EPS(1)

$THETA  4.1 ; 1. Emax
$THETA  6.9 ; 2. ED50
$THETA  0.001 ; 3. Hill
$THETA  2.3 ; 4. E0

$OMEGA BLOCK(2) 0.1
                 0.01 0.1
$OMEGA 0.1
$OMEGA 0.0 FIXED

$ANNEAL 4:0.8

$SIGMA 1
$ESTIMATION METH=SAEM INTER NBURN=1000 NITER=500 ISAMPLE=5 IACCEPT=0.3 CINTERVAL=25 CTYPE=0 NOABORT PRINT=50 CONSTRAIN=5 SIGL=8
$ESTIMATION METH=IMP INTER PRINT=1 NITER=0 ISAMPLE=10000 EONLY=1 CONSTRAIN=0 MAPITER=0 DF=4
$COV MATRIX=R UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  97) A RANDOM QUANTITY IS RAISED TO A POWER. IF THE RESULT AFFECTS
 THE VALUE OF THE OBJECTIVE FUNCTION, THE USER SHOULD ENSURE THAT THE
 RANDOM QUANTITY IS NEVER 0 WHEN THE POWER IS < 1.
             
 (WARNING  99) A RANDOM QUANTITY IS USED AS A POWER. IF THE RESULT AFFECTS
 THE VALUE OF THE OBJECTIVE FUNCTION, THE USER SHOULD ENSURE THAT THE
 QUANTITY RAISED TO THE POWER IS NOT 0.
             
 (WARNING  13) WITH USER-WRITTEN PRED OR $PRED, NM-TRAN CANNOT APPEND THE
 MDV DATA ITEM.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       30 SEP 2013
Days until program expires :6087
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3beta7.0(P)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 Emax model with hill=3
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      200
 NO. OF DATA ITEMS IN DATA SET:   3
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
0LABELS FOR DATA ITEMS:
 ID DOSE DV
0FORMAT FOR DATA:
 (3E9.0)

 TOT. NO. OF OBS RECS:      200
 TOT. NO. OF INDIVIDUALS:    100
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  0  0  2
  0  0  0  3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.4100E+01  0.6900E+01  0.1000E-02  0.2300E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.1000E+00
                  0.1000E-01   0.1000E+00
        2                                                                                   NO
                  0.1000E+00
        3                                                                                  YES
                  0.0000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+01
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
1
 
 
 #TBLN:      1
 #METH: Stochastic Approximation Expectation-Maximization
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            528
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               OFF
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 EM OR BAYESIAN METHOD USED:              STOCHASTIC APPROXIMATION EXPECTATION MAXIMIZATION (SAEM)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                0           
 BURN-IN ITERATIONS (NBURN):              1000        
 ITERATIONS (NITER):                      500         
 ANEAL SETTING (CONSTRAIN):               5           
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        5           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.300000000000000       
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2           
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0           
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2           
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2           
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration        -1000 SAEMOBJ=   12074.1144263090
 iteration         -950 SAEMOBJ=   229.515503945385
 iteration         -900 SAEMOBJ=   182.574267528486
 iteration         -850 SAEMOBJ=   146.034825665128
 iteration         -800 SAEMOBJ=   127.181123609867
 iteration         -750 SAEMOBJ=   176.067894386368
 iteration         -700 SAEMOBJ=   217.659433777873
 iteration         -650 SAEMOBJ=   150.665457872826
 iteration         -600 SAEMOBJ=  -52.0231522792427
 iteration         -550 SAEMOBJ=  -117.512332798961
 iteration         -500 SAEMOBJ=  -217.901769325766
 iteration         -450 SAEMOBJ=  -284.014244086000
 iteration         -400 SAEMOBJ=  -369.191222058163
 iteration         -350 SAEMOBJ=  -412.250344942986
 iteration         -300 SAEMOBJ=   355.552216760430
 iteration         -250 SAEMOBJ=   399.755730675312
 iteration         -200 SAEMOBJ=   413.477072158996
 iteration         -150 SAEMOBJ=   372.037201287620
 iteration         -100 SAEMOBJ=   418.325773877641
 iteration          -50 SAEMOBJ=   404.704721917129
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=   374.143440543729
 iteration           50 SAEMOBJ=   346.839578755704
 iteration          100 SAEMOBJ=   344.816481841031
 iteration          150 SAEMOBJ=   344.968755399370
 iteration          200 SAEMOBJ=   344.305676680711
 iteration          250 SAEMOBJ=   344.272751933910
 iteration          300 SAEMOBJ=   344.670394697640
 iteration          350 SAEMOBJ=   344.559344764700
 iteration          400 SAEMOBJ=   344.710341605930
 iteration          450 SAEMOBJ=   345.074493410458
 iteration          500 SAEMOBJ=   344.977117395390
 
 #TERM:
 STOCHASTIC PORTION WAS NOT TESTED FOR CONVERGENCE
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         4.7997E-05  3.9722E-05 -1.1346E-05  0.0000E+00
 SE:             4.9743E-02  5.9804E-02  3.6998E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         9.9923E-01  9.9947E-01  9.9976E-01  1.0000E+00
 
 ETAshrink(%):   4.8924E+01  4.1838E+01  2.4613E+01  0.0000E+00
 EBVshrink(%):   4.8926E+01  4.1840E+01  2.4618E+01  0.0000E+00
 EPSshrink(%):   1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:    34.68
 Elapsed covariance time in seconds:     0.11
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      344.977       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.59E+00  6.35E+00  1.01E+00  1.53E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.58E-01
 
 ETA2
+        8.27E-01  1.07E+00
 
 ETA3
+        0.00E+00  0.00E+00  2.43E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.60E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.79E-01
 
 ETA2
+        8.18E-01  1.03E+00
 
 ETA3
+        0.00E+00  0.00E+00  4.93E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.61E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.09E-01  3.26E-01  2.01E-01  7.84E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.42E-01
 
 ETA2
+        5.85E-01  5.58E-01
 
 ETA3
+        0.00E+00  0.00E+00  7.30E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.65E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.77E-01
 
 ETA2
+        1.57E-01  2.70E-01
 
 ETA3
+       ......... .........  7.40E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.06E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.67E-01
 
 TH 2
+        1.21E-01  1.06E-01
 
 TH 3
+       -6.40E-02 -5.09E-02  4.06E-02
 
 TH 4
+       -6.24E-03 -2.42E-03  5.77E-03  6.15E-03
 
 OM11
+        1.36E-01  6.85E-02 -5.67E-02 -6.53E-03  2.94E-01
 
 OM12
+        1.69E-01  8.99E-02 -7.11E-02 -9.00E-03  3.08E-01  3.43E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.66E-01  9.43E-02 -7.53E-02 -1.01E-02  2.57E-01  3.08E-01  0.00E+00  0.00E+00  3.11E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.45E-04 -3.77E-04  7.10E-05 -8.35E-04  1.62E-03  1.20E-03  0.00E+00  0.00E+00  1.29E-04  0.00E+00  0.00E+00  5.33E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        2.02E-02  2.23E-02 -1.81E-02 -2.74E-03  7.50E-03  8.33E-03  0.00E+00  0.00E+00  9.93E-03  0.00E+00  0.00E+00 -1.19E-02
          0.00E+00  0.00E+00  4.42E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        4.09E-01
 
 TH 2
+        9.10E-01  3.26E-01
 
 TH 3
+       -7.77E-01 -7.74E-01  2.01E-01
 
 TH 4
+       -1.95E-01 -9.47E-02  3.65E-01  7.84E-02
 
 OM11
+        6.12E-01  3.87E-01 -5.19E-01 -1.54E-01  5.42E-01
 
 OM12
+        7.06E-01  4.71E-01 -6.03E-01 -1.96E-01  9.71E-01  5.85E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        7.27E-01  5.18E-01 -6.70E-01 -2.31E-01  8.49E-01  9.44E-01  0.00E+00  0.00E+00  5.58E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.15E-02 -1.58E-02  4.83E-03 -1.46E-01  4.09E-02  2.80E-02  0.00E+00  0.00E+00  3.16E-03  0.00E+00  0.00E+00  7.30E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        7.42E-02  1.03E-01 -1.35E-01 -5.25E-02  2.08E-02  2.14E-02  0.00E+00  0.00E+00  2.68E-02  0.00E+00  0.00E+00 -2.44E-01
          0.00E+00  0.00E+00  6.65E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.19E+02
 
 TH 2
+       -1.09E+02  1.28E+02
 
 TH 3
+       -1.24E+01  5.14E+01  1.07E+02
 
 TH 4
+        3.40E+01 -5.93E+01 -7.00E+01  2.31E+02
 
 OM11
+        1.00E+02 -6.50E+01  2.34E+01 -1.01E+01  3.30E+02
 
 OM12
+       -1.69E+02  1.16E+02 -3.52E+01  4.05E+00 -4.95E+02  7.72E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.19E+01 -3.13E+01  3.02E+01 -5.32E+00  1.89E+02 -3.10E+02  0.00E+00  0.00E+00  1.43E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -3.84E+00  3.82E-01 -4.83E+00  3.57E+01 -9.41E+00  7.84E+00  0.00E+00  0.00E+00  1.65E+00  0.00E+00  0.00E+00  2.07E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -2.94E-02 -1.47E-01  1.38E+00  1.18E+00 -1.18E+00  1.46E+00  0.00E+00  0.00E+00 -1.24E-01  0.00E+00  0.00E+00  5.71E+00
          0.00E+00  0.00E+00  2.48E+00
 
1
 
 
 #TBLN:      2
 #METH: Objective Function Evaluation by Importance Sampling
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            528
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               OFF
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 EM OR BAYESIAN METHOD USED:              IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                0           
 ITERATIONS (NITER):                      0           
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        10000       
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                YES
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-01   ,10.00000000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.300000000000000       
 T-DIST. PROPOSAL DENSITY (DF):           4           
 NO. ITERATIONS FOR MAP (MAPITER):        0           
 INTERVAL ITER. FOR MAP (MAPINTER):       0           
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   858.993886803605 eff.=    4081. Smpl.=   10000. Fit.= 0.91972
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.9541E-03  5.7423E-03  9.3930E-05  0.0000E+00
 SE:             4.8318E-02  5.8771E-02  3.6805E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         9.6774E-01  9.2217E-01  9.9796E-01  1.0000E+00
 
 ETAshrink(%):   5.0388E+01  4.2842E+01  2.5007E+01  0.0000E+00
 EBVshrink(%):   4.8581E+01  4.1799E+01  2.4817E+01  0.0000E+00
 EPSshrink(%):   1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:     0.00
 Elapsed covariance time in seconds:    63.01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      858.994       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.59E+00  6.35E+00  1.01E+00  1.53E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.58E-01
 
 ETA2
+        8.27E-01  1.07E+00
 
 ETA3
+        0.00E+00  0.00E+00  2.43E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.60E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.79E-01
 
 ETA2
+        8.18E-01  1.03E+00
 
 ETA3
+        0.00E+00  0.00E+00  4.93E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.61E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.63E-01  3.07E-01  1.94E-01  7.61E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.50E-01
 
 ETA2
+        4.83E-01  5.00E-01
 
 ETA3
+        0.00E+00  0.00E+00  6.75E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        7.52E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.30E-01
 
 ETA2
+        1.23E-01  2.42E-01
 
 ETA3
+       ......... .........  6.84E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.33E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.32E-01
 
 TH 2
+        1.03E-01  9.45E-02
 
 TH 3
+       -5.33E-02 -4.56E-02  3.77E-02
 
 TH 4
+       -2.30E-03 -4.09E-04  3.78E-03  5.79E-03
 
 OM11
+        1.03E-01  6.65E-02 -5.00E-02 -3.09E-03  2.03E-01
 
 OM12
+        1.29E-01  8.41E-02 -6.00E-02 -4.82E-03  2.04E-01  2.33E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.35E-01  9.03E-02 -6.44E-02 -6.34E-03  1.79E-01  2.28E-01  0.00E+00  0.00E+00  2.50E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -4.09E-03 -2.98E-03  1.93E-03 -1.23E-03 -2.86E-03 -4.92E-03  0.00E+00  0.00E+00 -6.69E-03  0.00E+00  0.00E+00  4.55E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        8.71E-02  7.84E-02 -6.47E-02  1.94E-03  8.92E-02  9.56E-02  0.00E+00  0.00E+00  9.33E-02  0.00E+00  0.00E+00 -1.89E-02
          0.00E+00  0.00E+00  5.66E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        3.63E-01
 
 TH 2
+        9.27E-01  3.07E-01
 
 TH 3
+       -7.55E-01 -7.63E-01  1.94E-01
 
 TH 4
+       -8.32E-02 -1.75E-02  2.56E-01  7.61E-02
 
 OM11
+        6.27E-01  4.80E-01 -5.72E-01 -9.02E-02  4.50E-01
 
 OM12
+        7.38E-01  5.67E-01 -6.40E-01 -1.31E-01  9.37E-01  4.83E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        7.43E-01  5.87E-01 -6.62E-01 -1.67E-01  7.92E-01  9.45E-01  0.00E+00  0.00E+00  5.00E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.67E-01 -1.44E-01  1.47E-01 -2.40E-01 -9.42E-02 -1.51E-01  0.00E+00  0.00E+00 -1.98E-01  0.00E+00  0.00E+00  6.75E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        3.19E-01  3.39E-01 -4.43E-01  3.39E-02  2.63E-01  2.63E-01  0.00E+00  0.00E+00  2.48E-01  0.00E+00  0.00E+00 -3.72E-01
          0.00E+00  0.00E+00  7.52E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.22E+02
 
 TH 2
+       -1.11E+02  1.30E+02
 
 TH 3
+       -9.50E+00  4.36E+01  1.02E+02
 
 TH 4
+        2.58E+01 -4.80E+01 -5.59E+01  2.32E+02
 
 OM11
+        4.55E+01 -3.08E+01  1.98E+01  1.46E-01  1.44E+02
 
 OM12
+       -1.00E+02  7.47E+01 -2.78E+01 -1.83E+01 -2.46E+02  4.67E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.18E+01 -2.38E+01  2.37E+01  1.39E+01  1.13E+02 -2.31E+02  0.00E+00  0.00E+00  1.32E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.71E+01 -2.11E+01 -8.12E-01  7.37E+01  2.67E+00 -2.80E+01  0.00E+00  0.00E+00  2.80E+01  0.00E+00  0.00E+00  2.93E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        5.39E-01 -3.99E-01  4.95E+00 -1.28E+00 -2.61E-01 -9.08E-01  0.00E+00  0.00E+00  1.39E+00  0.00E+00  0.00E+00  9.41E+00
          0.00E+00  0.00E+00  2.59E+00
 
 #CPUT: Total CPU Time in Seconds,       97.391
Stop Time: 
Mon 09/30/2013 
02:13 PM

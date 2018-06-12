Mon 09/30/2013 
02:04 PM
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

$ANNEAL 4:0.3

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
 iteration        -1000 SAEMOBJ=   13672.6521567091
 iteration         -950 SAEMOBJ=   282.603212946243
 iteration         -900 SAEMOBJ=   230.934337263766
 iteration         -850 SAEMOBJ=   177.173897203015
 iteration         -800 SAEMOBJ=   112.023172128142
 iteration         -750 SAEMOBJ=   41.8661310244185
 iteration         -700 SAEMOBJ=   33.6796435328774
 iteration         -650 SAEMOBJ=  -38.9256507730716
 iteration         -600 SAEMOBJ=  -190.062101242507
 iteration         -550 SAEMOBJ=  -204.089741511646
 iteration         -500 SAEMOBJ=  -288.741879823454
 iteration         -450 SAEMOBJ=  -322.857318307953
 iteration         -400 SAEMOBJ=  -366.460391060469
 iteration         -350 SAEMOBJ=  -414.794240488443
 iteration         -300 SAEMOBJ=   354.497304092107
 iteration         -250 SAEMOBJ=   361.584686096535
 iteration         -200 SAEMOBJ=   410.214421333314
 iteration         -150 SAEMOBJ=   365.132373245924
 iteration         -100 SAEMOBJ=   420.014437247495
 iteration          -50 SAEMOBJ=   348.772021356788
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=   355.045650130072
 iteration           50 SAEMOBJ=   349.077330059038
 iteration          100 SAEMOBJ=   347.127705893398
 iteration          150 SAEMOBJ=   347.603869977962
 iteration          200 SAEMOBJ=   347.478897219325
 iteration          250 SAEMOBJ=   346.763716211723
 iteration          300 SAEMOBJ=   346.574037663290
 iteration          350 SAEMOBJ=   346.254003046627
 iteration          400 SAEMOBJ=   346.121595408334
 iteration          450 SAEMOBJ=   346.180984049110
 iteration          500 SAEMOBJ=   345.928588338208
 
 #TERM:
 STOCHASTIC PORTION WAS NOT TESTED FOR CONVERGENCE
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         2.2451E-06  3.1539E-05  1.5308E-05  0.0000E+00
 SE:             4.8310E-02  5.7846E-02  3.7072E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         9.9996E-01  9.9956E-01  9.9967E-01  1.0000E+00
 
 ETAshrink(%):   4.8999E+01  4.1857E+01  2.4474E+01  0.0000E+00
 EBVshrink(%):   4.8989E+01  4.1853E+01  2.4472E+01  0.0000E+00
 EPSshrink(%):   1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:    33.32
 Elapsed covariance time in seconds:     0.09
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      345.929       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.55E+00  6.34E+00  1.02E+00  1.53E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.06E-01
 
 ETA2
+        7.57E-01  1.00E+00
 
 ETA3
+        0.00E+00  0.00E+00  2.43E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.63E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.52E-01
 
 ETA2
+        7.96E-01  1.00E+00
 
 ETA3
+        0.00E+00  0.00E+00  4.93E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.62E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.86E-01  2.98E-01  1.75E-01  7.95E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.15E-01
 
 ETA2
+        5.91E-01  5.64E-01
 
 ETA3
+        0.00E+00  0.00E+00  7.24E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.81E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.70E-01
 
 ETA2
+        1.94E-01  2.82E-01
 
 ETA3
+       ......... .........  7.33E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.10E-01
 
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
+        1.49E-01
 
 TH 2
+        1.02E-01  8.88E-02
 
 TH 3
+       -4.91E-02 -3.80E-02  3.05E-02
 
 TH 4
+       -5.25E-03 -1.43E-03  5.28E-03  6.33E-03
 
 OM11
+        1.21E-01  5.51E-02 -4.19E-02 -5.66E-03  2.65E-01
 
 OM12
+        1.66E-01  8.32E-02 -6.11E-02 -9.21E-03  2.93E-01  3.49E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.62E-01  8.61E-02 -6.55E-02 -1.06E-02  2.45E-01  3.15E-01  0.00E+00  0.00E+00  3.18E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        8.36E-04  6.78E-06 -2.69E-04 -1.01E-03  1.27E-03  1.46E-03  0.00E+00  0.00E+00  7.49E-04  0.00E+00  0.00E+00  5.24E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -3.46E-03  8.63E-03 -7.42E-03 -5.89E-04 -2.69E-02 -3.06E-02  0.00E+00  0.00E+00 -2.55E-02  0.00E+00  0.00E+00 -1.15E-02
          0.00E+00  0.00E+00  4.64E-01
 
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
+        3.86E-01
 
 TH 2
+        8.92E-01  2.98E-01
 
 TH 3
+       -7.30E-01 -7.29E-01  1.75E-01
 
 TH 4
+       -1.71E-01 -6.04E-02  3.80E-01  7.95E-02
 
 OM11
+        6.11E-01  3.59E-01 -4.65E-01 -1.38E-01  5.15E-01
 
 OM12
+        7.29E-01  4.73E-01 -5.93E-01 -1.96E-01  9.65E-01  5.91E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        7.45E-01  5.12E-01 -6.65E-01 -2.37E-01  8.42E-01  9.47E-01  0.00E+00  0.00E+00  5.64E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.00E-02  3.14E-04 -2.13E-02 -1.75E-01  3.42E-02  3.41E-02  0.00E+00  0.00E+00  1.83E-02  0.00E+00  0.00E+00  7.24E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -1.32E-02  4.25E-02 -6.24E-02 -1.09E-02 -7.66E-02 -7.61E-02  0.00E+00  0.00E+00 -6.62E-02  0.00E+00  0.00E+00 -2.33E-01
          0.00E+00  0.00E+00  6.81E-01
 
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
+        1.18E+02
 
 TH 2
+       -1.07E+02  1.28E+02
 
 TH 3
+       -3.74E+01  7.08E+01  1.31E+02
 
 TH 4
+        3.64E+01 -6.30E+01 -8.03E+01  2.32E+02
 
 OM11
+        9.24E+01 -5.57E+01 -3.56E+01 -7.24E-01  3.07E+02
 
 OM12
+       -1.51E+02  9.65E+01  4.92E+01 -8.63E+00 -4.46E+02  6.80E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.13E+01 -2.05E+01  2.91E+00 -1.27E+00  1.67E+02 -2.70E+02  0.00E+00  0.00E+00  1.28E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.15E-02 -2.28E+00 -4.70E+00  3.91E+01  6.37E+00 -1.34E+01  0.00E+00  0.00E+00  9.27E+00  0.00E+00  0.00E+00  2.11E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -4.26E-02 -1.61E-01  1.63E+00  7.40E-01 -1.18E+00  1.71E+00  0.00E+00  0.00E+00 -1.86E-01  0.00E+00  0.00E+00  5.23E+00
          0.00E+00  0.00E+00  2.35E+00
 
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

 iteration            0 OBJ=   858.639044402553 eff.=    4133. Smpl.=   10000. Fit.= 0.91752
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         7.3916E-03  5.9826E-03 -2.4153E-03  0.0000E+00
 SE:             4.7309E-02  5.6894E-02  3.6979E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         8.7584E-01  9.1625E-01  9.4792E-01  1.0000E+00
 
 ETAshrink(%):   5.0056E+01  4.2814E+01  2.4664E+01  0.0000E+00
 EBVshrink(%):   4.9005E+01  4.1928E+01  2.4501E+01  0.0000E+00
 EPSshrink(%):   1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:     0.02
 Elapsed covariance time in seconds:    57.45
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      858.639       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.55E+00  6.34E+00  1.02E+00  1.53E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.06E-01
 
 ETA2
+        7.57E-01  1.00E+00
 
 ETA3
+        0.00E+00  0.00E+00  2.43E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.63E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.52E-01
 
 ETA2
+        7.96E-01  1.00E+00
 
 ETA3
+        0.00E+00  0.00E+00  4.93E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.62E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.66E-01  2.16E-01  8.88E-02  7.38E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.76E-01
 
 ETA2
+        3.90E-01  3.91E-01
 
 ETA3
+        0.00E+00  0.00E+00  6.67E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        7.03E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.98E-01
 
 ETA2
+        1.24E-01  1.95E-01
 
 ETA3
+       ......... .........  6.76E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.17E-01
 
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
+        7.06E-02
 
 TH 2
+        4.87E-02  4.66E-02
 
 TH 3
+       -1.01E-02 -8.50E-03  7.89E-03
 
 TH 4
+        1.59E-03  2.93E-03  8.91E-04  5.44E-03
 
 OM11
+        4.67E-02  1.70E-02 -9.85E-03  6.00E-04  1.41E-01
 
 OM12
+        6.27E-02  2.45E-02 -1.13E-02 -3.20E-04  1.34E-01  1.52E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.23E-02  2.62E-02 -1.16E-02 -1.38E-03  1.04E-01  1.39E-01  0.00E+00  0.00E+00  1.53E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.57E-03 -9.43E-04  1.82E-04 -1.41E-03 -2.92E-04 -1.76E-03  0.00E+00  0.00E+00 -3.27E-03  0.00E+00  0.00E+00  4.45E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        1.93E-03  7.81E-03 -8.75E-03  8.01E-03  6.75E-03 -4.28E-03  0.00E+00  0.00E+00 -1.33E-02  0.00E+00  0.00E+00 -1.68E-02
          0.00E+00  0.00E+00  4.94E-01
 
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
+        2.66E-01
 
 TH 2
+        8.48E-01  2.16E-01
 
 TH 3
+       -4.28E-01 -4.43E-01  8.88E-02
 
 TH 4
+        8.11E-02  1.84E-01  1.36E-01  7.38E-02
 
 OM11
+        4.67E-01  2.09E-01 -2.95E-01  2.16E-02  3.76E-01
 
 OM12
+        6.06E-01  2.91E-01 -3.25E-01 -1.11E-02  9.13E-01  3.90E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.00E-01  3.11E-01 -3.35E-01 -4.79E-02  7.06E-01  9.15E-01  0.00E+00  0.00E+00  3.91E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -8.86E-02 -6.55E-02  3.07E-02 -2.86E-01 -1.16E-02 -6.78E-02  0.00E+00  0.00E+00 -1.25E-01  0.00E+00  0.00E+00  6.67E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        1.03E-02  5.14E-02 -1.40E-01  1.54E-01  2.55E-02 -1.56E-02  0.00E+00  0.00E+00 -4.85E-02  0.00E+00  0.00E+00 -3.59E-01
          0.00E+00  0.00E+00  7.03E-01
 
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
+        1.20E+02
 
 TH 2
+       -1.09E+02  1.30E+02
 
 TH 3
+       -7.67E+00  4.02E+01  1.89E+02
 
 TH 4
+        2.47E+01 -4.71E+01 -5.42E+01  2.34E+02
 
 OM11
+        4.54E+01 -3.12E+01  2.30E+01 -1.73E+00  1.38E+02
 
 OM12
+       -1.01E+02  7.66E+01 -2.98E+01 -1.56E+01 -2.29E+02  4.29E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.08E+01 -2.44E+01  2.21E+01  1.28E+01  1.04E+02 -2.11E+02  0.00E+00  0.00E+00  1.23E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.53E+01 -1.83E+01  7.59E-01  7.28E+01  1.56E+00 -2.62E+01  0.00E+00  0.00E+00  2.76E+01  0.00E+00  0.00E+00  2.96E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        5.73E-01 -3.44E-01  3.67E+00 -1.39E+00 -2.60E-01 -8.36E-01  0.00E+00  0.00E+00  1.45E+00  0.00E+00  0.00E+00  9.64E+00
          0.00E+00  0.00E+00  2.48E+00
 
 #CPUT: Total CPU Time in Seconds,       90.543
Stop Time: 
Mon 09/30/2013 
02:05 PM

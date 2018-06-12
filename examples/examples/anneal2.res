Mon 09/30/2013 
02:05 PM
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
$THETA  (-3.0,0.001,3.0) ; 3. Hill
$THETA  2.3 ; 4. E0

$OMEGA BLOCK(2) 0.1
                 0.01 0.1
$OMEGA 0.1
$OMEGA 0.0 FIXED

$SIGMA 1
$EST METHOD=CHAIN ISAMPLE=1 ISAMPEND=30 NSAMPLE=30 FILE=anneal2.chn
$ESTIMATION METH=SAEM INTER NBURN=4000 NITER=200 ISAMPLE=5 IACCEPT=0.3 CINTERVAL=25 CTYPE=3 NOABORT PRINT=100
$ESTIMATION METH=IMP INTER PRINT=1 NITER=0 ISAMPLE=10000 EONLY=1 MAPITER=0
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
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.4100E+01     0.1000E+07
 -0.1000E+07     0.6900E+01     0.1000E+07
 -0.3000E+01     0.1000E-02     0.3000E+01
 -0.1000E+07     0.2300E+01     0.1000E+07
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
 #METH: Chain Method Processing
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 
 
 CHAIN FILE SEARCH:
 NUMBER,      OBECTIVE FUNCTION
           1   1147.58083747608     
           2   1620.71372677829     
           3   10564.4920013860     
           4   54275.5944108467     
           5   11346.8403986852     
           6   38914.9523055394     
           7   15925.4295133109     
           8   24535.0462589829     
           9   4102.25776567382     
          10   14300.6759785775     
          11   982.180016083324     
          12   16733.8214335501     
          13   50766.9179627348     
          14   3221.31150034071     
          15   12038.9642602445     
          16   1744.74682791811     
          17   13638.1867177559     
          18   9038.70553023336     
          19   6841.41392522806     
          20   7338.44800332074     
          21   34712.8737704224     
          22   3694.59198993551     
          23   24162.8031407898     
          24   14606.4143676267     
          25   2037.09365572350     
          26   3359.28785700180     
          27   31180.5387920285     
          28   3084.98305346328     
          29   20471.7769597231     
          30   20328.5330813020     
 FROM SAMPLE 11 OF CHAIN FILE anneal2.chn
 NEW INITIAL ESTIMATES OF THETA
  0.2933E+01
  0.5596E+01
  0.2035E+01
  0.2077E+01
 NEW INITIAL ESTIMATES OF OMEGA
  0.1870E+01
  0.7805E-01  0.1521E+00
  0.0000E+00  0.0000E+00  0.2382E+00
  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 NEW INITIAL ESTIMATES OF SIGMA
  0.1639E+01
 WITH INITIAL OBJECTIVE FUNCTION VALUE    982.180016083324     
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    100         
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   100         
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
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        25          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              4000        
 ITERATIONS (NITER):                      200         
 ANEAL SETTING (CONSTRAIN):               1           
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
   1   2   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration        -4000 SAEMOBJ=   1872.25059781933
 iteration        -3900 SAEMOBJ=   305.514922215038
 iteration        -3800 SAEMOBJ=   317.424055479484
 iteration        -3700 SAEMOBJ=   328.640490748111
 iteration        -3600 SAEMOBJ=   352.087586507871
 Convergence achieved
 iteration        -3575 SAEMOBJ=   303.320961811198
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=   306.973599931870
 iteration          100 SAEMOBJ=   299.358588087798
 iteration          200 SAEMOBJ=   301.449430480862
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         5.4780E-05 -3.4015E-05  1.6397E-05  0.0000E+00
 SE:             4.5676E-02  5.0969E-02  3.8495E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         9.9904E-01  9.9947E-01  9.9966E-01  1.0000E+00
 
 ETAshrink(%):   4.4907E+01  4.0660E+01  2.2977E+01  0.0000E+00
 EBVshrink(%):   4.4948E+01  4.0690E+01  2.2978E+01  0.0000E+00
 EPSshrink(%):   1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:    19.31
 Elapsed covariance time in seconds:     0.08
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      301.449       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.36E+00  6.16E+00  1.24E+00  1.55E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        6.94E-01
 
 ETA2
+        5.29E-01  7.45E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.52E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.47E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        8.33E-01
 
 ETA2
+        7.35E-01  8.63E-01
 
 ETA3
+        0.00E+00  0.00E+00  5.02E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.57E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.71E-01  2.27E-01  2.40E-01  7.77E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.04E-01
 
 ETA2
+        3.49E-01  3.40E-01
 
 ETA3
+        0.00E+00  0.00E+00  7.40E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.19E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.83E-01
 
 ETA2
+        1.97E-01  1.97E-01
 
 ETA3
+       ......... .........  7.36E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.97E-01
 
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
+        7.35E-02
 
 TH 2
+        5.04E-02  5.16E-02
 
 TH 3
+       -4.41E-02 -3.82E-02  5.74E-02
 
 TH 4
+       -3.98E-03 -1.07E-03  6.84E-03  6.03E-03
 
 OM11
+        3.26E-02  4.98E-03 -1.19E-02 -8.65E-05  9.26E-02
 
 OM12
+        5.87E-02  2.13E-02 -3.57E-02 -3.18E-03  9.77E-02  1.22E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.11E-02  2.87E-02 -5.00E-02 -5.45E-03  6.65E-02  1.03E-01  0.00E+00  0.00E+00  1.16E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.08E-03 -1.90E-03  2.09E-03 -5.08E-04  4.49E-05  1.15E-04  0.00E+00  0.00E+00 -1.04E-03  0.00E+00  0.00E+00  5.47E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        3.72E-03  1.06E-02 -1.01E-02 -1.26E-03 -8.47E-03 -8.66E-03  0.00E+00  0.00E+00 -8.42E-04  0.00E+00  0.00E+00 -1.08E-02
          0.00E+00  0.00E+00  3.84E-01
 
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
+        2.71E-01
 
 TH 2
+        8.18E-01  2.27E-01
 
 TH 3
+       -6.79E-01 -7.01E-01  2.40E-01
 
 TH 4
+       -1.89E-01 -6.06E-02  3.67E-01  7.77E-02
 
 OM11
+        3.95E-01  7.21E-02 -1.63E-01 -3.66E-03  3.04E-01
 
 OM12
+        6.21E-01  2.68E-01 -4.27E-01 -1.17E-01  9.19E-01  3.49E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.64E-01  3.72E-01 -6.14E-01 -2.06E-01  6.43E-01  8.71E-01  0.00E+00  0.00E+00  3.40E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -5.40E-02 -1.13E-01  1.18E-01 -8.85E-02  1.99E-03  4.46E-03  0.00E+00  0.00E+00 -4.14E-02  0.00E+00  0.00E+00  7.40E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        2.21E-02  7.52E-02 -6.81E-02 -2.63E-02 -4.49E-02 -4.01E-02  0.00E+00  0.00E+00 -4.00E-03  0.00E+00  0.00E+00 -2.35E-01
          0.00E+00  0.00E+00  6.19E-01
 
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
+        1.24E+02
 
 TH 2
+       -1.02E+02  1.30E+02
 
 TH 3
+       -2.57E+01  5.40E+01  6.96E+01
 
 TH 4
+        4.31E+01 -6.63E+01 -5.50E+01  2.33E+02
 
 OM11
+        1.01E+02 -5.74E+01 -4.42E+01  1.17E+01  3.64E+02
 
 OM12
+       -1.59E+02  9.54E+01  5.15E+01 -2.84E+01 -4.84E+02  6.89E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.50E+01 -1.03E+01  6.98E+00 -1.78E-01  1.67E+02 -2.57E+02  0.00E+00  0.00E+00  1.30E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.24E+01 -7.00E+00 -1.72E+01  3.10E+01  5.40E+01 -7.55E+01  0.00E+00  0.00E+00  2.75E+01  0.00E+00  0.00E+00  2.09E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        1.52E-01 -7.10E-01  1.32E-01  1.22E+00 -1.55E+00  2.35E+00  0.00E+00  0.00E+00 -9.29E-01  0.00E+00  0.00E+00  5.14E+00
          0.00E+00  0.00E+00  2.79E+00
 
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    100         
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   100         
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
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        25          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      0           
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        10000       
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                YES
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-01   ,10.00000000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.300000000000000       
 T-DIST. PROPOSAL DENSITY (DF):           0           
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

 iteration            0 OBJ=   860.368314001496 eff.=    5155. Smpl.=   10000. Fit.= 0.89433
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.5528E-02 -1.0228E-02 -3.9510E-03  0.0000E+00
 SE:             4.5336E-02  5.0474E-02  3.7674E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         7.3197E-01  8.3941E-01  9.1648E-01  1.0000E+00
 
 ETAshrink(%):   4.5317E+01  4.1237E+01  2.4620E+01  0.0000E+00
 EBVshrink(%):   4.3187E+01  4.0670E+01  2.2368E+01  0.0000E+00
 EPSshrink(%):   1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:     0.01
 Elapsed covariance time in seconds:    58.27
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      860.368       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.36E+00  6.16E+00  1.24E+00  1.55E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        6.94E-01
 
 ETA2
+        5.29E-01  7.45E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.52E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.47E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        8.33E-01
 
 ETA2
+        7.35E-01  8.63E-01
 
 ETA3
+        0.00E+00  0.00E+00  5.02E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.57E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.27E-01  1.92E-01  1.65E-01  7.21E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.56E-01
 
 ETA2
+        2.88E-01  3.19E-01
 
 ETA3
+        0.00E+00  0.00E+00  6.41E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.08E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.54E-01
 
 ETA2
+        1.46E-01  1.85E-01
 
 ETA3
+       ......... .........  6.38E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.93E-01
 
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
+        5.15E-02
 
 TH 2
+        3.59E-02  3.70E-02
 
 TH 3
+       -2.40E-02 -1.86E-02  2.72E-02
 
 TH 4
+       -1.67E-03  5.34E-04  3.01E-03  5.19E-03
 
 OM11
+        2.29E-02  6.43E-03 -1.69E-02 -2.04E-03  6.58E-02
 
 OM12
+        3.95E-02  1.55E-02 -2.45E-02 -3.35E-03  6.48E-02  8.29E-02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.76E-02  2.31E-02 -3.08E-02 -4.35E-03  5.37E-02  8.37E-02  0.00E+00  0.00E+00  1.02E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.62E-03 -6.95E-04  5.38E-04 -9.01E-04 -8.47E-04 -2.16E-03  0.00E+00  0.00E+00 -3.59E-03  0.00E+00  0.00E+00  4.10E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        9.66E-03  9.12E-03 -1.28E-02  3.29E-03  6.86E-03  6.35E-03  0.00E+00  0.00E+00  8.61E-03  0.00E+00  0.00E+00 -1.20E-02
          0.00E+00  0.00E+00  3.70E-01
 
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
+        2.27E-01
 
 TH 2
+        8.22E-01  1.92E-01
 
 TH 3
+       -6.41E-01 -5.85E-01  1.65E-01
 
 TH 4
+       -1.02E-01  3.85E-02  2.53E-01  7.21E-02
 
 OM11
+        3.94E-01  1.30E-01 -3.99E-01 -1.11E-01  2.56E-01
 
 OM12
+        6.05E-01  2.80E-01 -5.15E-01 -1.61E-01  8.78E-01  2.88E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.57E-01  3.77E-01 -5.85E-01 -1.89E-01  6.56E-01  9.11E-01  0.00E+00  0.00E+00  3.19E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.11E-01 -5.64E-02  5.09E-02 -1.95E-01 -5.15E-02 -1.17E-01  0.00E+00  0.00E+00 -1.76E-01  0.00E+00  0.00E+00  6.41E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        7.00E-02  7.79E-02 -1.27E-01  7.50E-02  4.40E-02  3.62E-02  0.00E+00  0.00E+00  4.44E-02  0.00E+00  0.00E+00 -3.07E-01
          0.00E+00  0.00E+00  6.08E-01
 
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
+        1.30E+02
 
 TH 2
+       -1.04E+02  1.29E+02
 
 TH 3
+        6.85E+00  2.74E+01  8.49E+01
 
 TH 4
+        2.55E+01 -4.70E+01 -3.72E+01  2.39E+02
 
 OM11
+        4.44E+01 -2.34E+01  2.63E+01  4.66E-01  1.62E+02
 
 OM12
+       -9.68E+01  6.99E+01 -3.68E+01 -1.82E+01 -2.47E+02  4.59E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.29E+01 -2.04E+01  3.11E+01  1.47E+01  1.11E+02 -2.31E+02  0.00E+00  0.00E+00  1.46E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.66E+01 -1.89E+01  7.39E+00  5.97E+01  8.09E+00 -3.49E+01  0.00E+00  0.00E+00  3.56E+01  0.00E+00  0.00E+00  3.00E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        2.37E-02 -1.42E-02  2.06E+00 -1.01E+00 -7.64E-01  6.55E-01  0.00E+00  0.00E+00  4.98E-01  0.00E+00  0.00E+00  9.08E+00
          0.00E+00  0.00E+00  3.07E+00
 
 #CPUT: Total CPU Time in Seconds,       72.416
Stop Time: 
Mon 09/30/2013 
02:07 PM

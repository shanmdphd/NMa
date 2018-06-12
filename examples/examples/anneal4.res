Mon 09/30/2013 
02:07 PM
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

$ANNEAL 4:0.3

$SIGMA 1
;$EST METHOD=CHAIN ISAMPLE=1 ISAMPEND=30 NSAMPLE=30 FILE=anneal4.chn
$ESTIMATION METH=IMPMAP INTER NITER=100 NBURN=60 ISAMPLE=1000 DF=4 CTYPE=3 NOABORT PRINT=1 CONSTRAIN=5 SIGL=8
$ESTIMATION METH=IMP INTER NITER=20 ISAMPLE=1000 DF=4 CONSTRAIN=0 SIGL=8 MAPITER=0
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
 #METH: Importance Sampling assisted by MAP Estimation
 
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
 EM OR BAYESIAN METHOD USED:              IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION (IMPMAP)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      100         
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        1000        
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-01   ,10.00000000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 T-DIST. PROPOSAL DENSITY (DF):           4           
 NO. ITERATIONS FOR MAP (MAPITER):        1           
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
   1   2   3   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   3613.34693708080 eff.=     671. Smpl.=    1000. Fit.= 0.95766
 iteration            1 OBJ=   1127.87901871800 eff.=     609. Smpl.=    1000. Fit.= 0.90018
 iteration            2 OBJ=   990.519681555970 eff.=     574. Smpl.=    1000. Fit.= 0.89777
 iteration            3 OBJ=   957.983038687826 eff.=     489. Smpl.=    1000. Fit.= 0.90108
 iteration            4 OBJ=   939.679681382778 eff.=     450. Smpl.=    1000. Fit.= 0.90578
 iteration            5 OBJ=   930.075311786688 eff.=     413. Smpl.=    1000. Fit.= 0.90703
 iteration            6 OBJ=   912.960450942792 eff.=     461. Smpl.=    1000. Fit.= 0.91158
 iteration            7 OBJ=   905.722930462562 eff.=     436. Smpl.=    1000. Fit.= 0.91452
 iteration            8 OBJ=   899.886796864687 eff.=     432. Smpl.=    1000. Fit.= 0.91648
 iteration            9 OBJ=   889.336445367773 eff.=     475. Smpl.=    1000. Fit.= 0.91913
 iteration           10 OBJ=   890.088832680724 eff.=     430. Smpl.=    1000. Fit.= 0.91778
 iteration           11 OBJ=   887.344957742445 eff.=     428. Smpl.=    1000. Fit.= 0.92056
 iteration           12 OBJ=   881.538743813456 eff.=     454. Smpl.=    1000. Fit.= 0.92216
 iteration           13 OBJ=   882.765202439557 eff.=     408. Smpl.=    1000. Fit.= 0.92328
 iteration           14 OBJ=   881.074717753479 eff.=     405. Smpl.=    1000. Fit.= 0.92178
 iteration           15 OBJ=   878.437822503384 eff.=     429. Smpl.=    1000. Fit.= 0.92391
 iteration           16 OBJ=   881.056204532669 eff.=     400. Smpl.=    1000. Fit.= 0.92440
 iteration           17 OBJ=   880.137038443466 eff.=     408. Smpl.=    1000. Fit.= 0.92394
 iteration           18 OBJ=   876.919423482703 eff.=     413. Smpl.=    1000. Fit.= 0.92590
 iteration           19 OBJ=   879.815944530330 eff.=     400. Smpl.=    1000. Fit.= 0.92580
 iteration           20 OBJ=   878.755975750877 eff.=     407. Smpl.=    1000. Fit.= 0.92658
 iteration           21 OBJ=   876.408217091381 eff.=     426. Smpl.=    1000. Fit.= 0.92737
 iteration           22 OBJ=   872.466738916278 eff.=     428. Smpl.=    1000. Fit.= 0.92725
 iteration           23 OBJ=   875.784125309449 eff.=     417. Smpl.=    1000. Fit.= 0.92680
 iteration           24 OBJ=   877.503973900988 eff.=     410. Smpl.=    1000. Fit.= 0.92892
 iteration           25 OBJ=   875.792571853034 eff.=     414. Smpl.=    1000. Fit.= 0.93007
 iteration           26 OBJ=   882.505052932048 eff.=     391. Smpl.=    1000. Fit.= 0.92878
 iteration           27 OBJ=   879.150092126366 eff.=     410. Smpl.=    1000. Fit.= 0.93124
 iteration           28 OBJ=   874.920403272419 eff.=     417. Smpl.=    1000. Fit.= 0.93095
 iteration           29 OBJ=   878.174619064662 eff.=     395. Smpl.=    1000. Fit.= 0.93108
 iteration           30 OBJ=   877.032053156801 eff.=     407. Smpl.=    1000. Fit.= 0.93238
 iteration           31 OBJ=   876.300333789435 eff.=     408. Smpl.=    1000. Fit.= 0.93231
 iteration           32 OBJ=   876.305633347983 eff.=     410. Smpl.=    1000. Fit.= 0.93192
 iteration           33 OBJ=   874.080148760989 eff.=     416. Smpl.=    1000. Fit.= 0.93260
 iteration           34 OBJ=   878.102639025589 eff.=     400. Smpl.=    1000. Fit.= 0.93161
 iteration           35 OBJ=   876.212082036242 eff.=     412. Smpl.=    1000. Fit.= 0.93281
 iteration           36 OBJ=   878.619824217956 eff.=     401. Smpl.=    1000. Fit.= 0.93261
 iteration           37 OBJ=   878.344197789394 eff.=     405. Smpl.=    1000. Fit.= 0.93318
 iteration           38 OBJ=   875.082082579504 eff.=     415. Smpl.=    1000. Fit.= 0.93405
 iteration           39 OBJ=   876.456817715152 eff.=     403. Smpl.=    1000. Fit.= 0.93315
 iteration           40 OBJ=   880.212525292266 eff.=     395. Smpl.=    1000. Fit.= 0.93281
 iteration           41 OBJ=   879.203381896660 eff.=     405. Smpl.=    1000. Fit.= 0.93429
 iteration           42 OBJ=   877.344183821569 eff.=     411. Smpl.=    1000. Fit.= 0.93473
 iteration           43 OBJ=   876.270486939149 eff.=     411. Smpl.=    1000. Fit.= 0.93397
 iteration           44 OBJ=   870.112451495399 eff.=     451. Smpl.=    1000. Fit.= 0.93451
 iteration           45 OBJ=   877.228535679415 eff.=     394. Smpl.=    1000. Fit.= 0.93383
 iteration           46 OBJ=   876.715161012210 eff.=     405. Smpl.=    1000. Fit.= 0.93499
 iteration           47 OBJ=   878.607065299438 eff.=     400. Smpl.=    1000. Fit.= 0.93493
 iteration           48 OBJ=   875.782584552722 eff.=     410. Smpl.=    1000. Fit.= 0.93566
 iteration           49 OBJ=   878.884829422618 eff.=     396. Smpl.=    1000. Fit.= 0.93482
 iteration           50 OBJ=   879.389072428092 eff.=     400. Smpl.=    1000. Fit.= 0.93535
 iteration           51 OBJ=   876.042202967314 eff.=     411. Smpl.=    1000. Fit.= 0.93643
 iteration           52 OBJ=   880.000911598679 eff.=     395. Smpl.=    1000. Fit.= 0.93535
 iteration           53 OBJ=   874.252326920177 eff.=     420. Smpl.=    1000. Fit.= 0.93608
 iteration           54 OBJ=   876.850644506542 eff.=     401. Smpl.=    1000. Fit.= 0.93506
 iteration           55 OBJ=   877.587143326913 eff.=     403. Smpl.=    1000. Fit.= 0.93541
 iteration           56 OBJ=   878.024932860053 eff.=     403. Smpl.=    1000. Fit.= 0.93585
 iteration           57 OBJ=   875.505129857288 eff.=     408. Smpl.=    1000. Fit.= 0.93545
 iteration           58 OBJ=   876.931045503147 eff.=     400. Smpl.=    1000. Fit.= 0.93459
 iteration           59 OBJ=   879.527753629912 eff.=     396. Smpl.=    1000. Fit.= 0.93445
 iteration           60 OBJ=   880.539611985006 eff.=     502. Smpl.=    1000. Fit.= 0.91194
 iteration           61 OBJ=   876.082164733112 eff.=     409. Smpl.=    1000. Fit.= 0.89172
 iteration           62 OBJ=   865.980530301197 eff.=     459. Smpl.=    1000. Fit.= 0.88862
 iteration           63 OBJ=   876.317758395892 eff.=     391. Smpl.=    1000. Fit.= 0.88593
 iteration           64 OBJ=   875.702342000836 eff.=     403. Smpl.=    1000. Fit.= 0.88894
 iteration           65 OBJ=   874.486079844602 eff.=     405. Smpl.=    1000. Fit.= 0.88913
 iteration           66 OBJ=   872.178209675091 eff.=     407. Smpl.=    1000. Fit.= 0.88946
 iteration           67 OBJ=   869.439774767271 eff.=     416. Smpl.=    1000. Fit.= 0.88819
 iteration           68 OBJ=   867.648791469192 eff.=     413. Smpl.=    1000. Fit.= 0.88746
 iteration           69 OBJ=   864.152725772719 eff.=     426. Smpl.=    1000. Fit.= 0.88628
 iteration           70 OBJ=   868.848386118909 eff.=     401. Smpl.=    1000. Fit.= 0.88449
 iteration           71 OBJ=   872.261541208954 eff.=     400. Smpl.=    1000. Fit.= 0.88689
 iteration           72 OBJ=   871.902587259641 eff.=     405. Smpl.=    1000. Fit.= 0.88896
 iteration           73 OBJ=   869.626256593924 eff.=     411. Smpl.=    1000. Fit.= 0.88931
 iteration           74 OBJ=   872.409364679677 eff.=     397. Smpl.=    1000. Fit.= 0.88859
 iteration           75 OBJ=   870.002172475987 eff.=     408. Smpl.=    1000. Fit.= 0.89072
 iteration           76 OBJ=   866.510594523528 eff.=     408. Smpl.=    1000. Fit.= 0.89027
 iteration           77 OBJ=   868.981207380611 eff.=     397. Smpl.=    1000. Fit.= 0.88908
 iteration           78 OBJ=   868.324299565609 eff.=     402. Smpl.=    1000. Fit.= 0.89002
 iteration           79 OBJ=   866.252307654050 eff.=     412. Smpl.=    1000. Fit.= 0.89063
 iteration           80 OBJ=   861.327826197127 eff.=     451. Smpl.=    1000. Fit.= 0.88990
 iteration           81 OBJ=   864.761019549560 eff.=     409. Smpl.=    1000. Fit.= 0.88834
 iteration           82 OBJ=   865.306577358481 eff.=     410. Smpl.=    1000. Fit.= 0.88798
 iteration           83 OBJ=   864.896948021154 eff.=     407. Smpl.=    1000. Fit.= 0.88917
 iteration           84 OBJ=   866.529470038226 eff.=     401. Smpl.=    1000. Fit.= 0.88838
 iteration           85 OBJ=   860.651705222511 eff.=     433. Smpl.=    1000. Fit.= 0.88906
 iteration           86 OBJ=   867.374030171963 eff.=     394. Smpl.=    1000. Fit.= 0.88711
 iteration           87 OBJ=   868.422156898977 eff.=     401. Smpl.=    1000. Fit.= 0.88851
 iteration           88 OBJ=   868.980027221587 eff.=     400. Smpl.=    1000. Fit.= 0.88982
 iteration           89 OBJ=   868.577926288125 eff.=     403. Smpl.=    1000. Fit.= 0.89138
 iteration           90 OBJ=   866.418170868562 eff.=     409. Smpl.=    1000. Fit.= 0.89079
 iteration           91 OBJ=   865.028431496263 eff.=     407. Smpl.=    1000. Fit.= 0.89055
 iteration           92 OBJ=   870.113718525774 eff.=     393. Smpl.=    1000. Fit.= 0.89208
 iteration           93 OBJ=   868.062646069495 eff.=     406. Smpl.=    1000. Fit.= 0.89471
 iteration           94 OBJ=   867.834327596508 eff.=     402. Smpl.=    1000. Fit.= 0.89346
 iteration           95 OBJ=   867.350285905209 eff.=     403. Smpl.=    1000. Fit.= 0.89411
 iteration           96 OBJ=   864.814181143588 eff.=     408. Smpl.=    1000. Fit.= 0.89466
 iteration           97 OBJ=   865.132496241406 eff.=     402. Smpl.=    1000. Fit.= 0.89363
 iteration           98 OBJ=   867.804042862810 eff.=     401. Smpl.=    1000. Fit.= 0.89346
 iteration           99 OBJ=   863.230388969582 eff.=     416. Smpl.=    1000. Fit.= 0.89502
 iteration          100 OBJ=   865.516584938983 eff.=     400. Smpl.=    1000. Fit.= 0.89347
 
 #TERM:
 OPTIMIZATION WAS NOT COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         5.2553E-03 -5.1228E-03 -6.3123E-03  0.0000E+00
 SE:             5.1361E-02  3.0547E-02  3.8973E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         9.1850E-01  8.6682E-01  8.7133E-01  1.0000E+00
 
 ETAshrink(%):   3.2254E+01  4.2889E+01  2.2277E+01  0.0000E+00
 EBVshrink(%):   3.1854E+01  4.3349E+01  2.1447E+01  0.0000E+00
 EPSshrink(%):   3.3185E+01
 
 #TERE:
 Elapsed estimation time in seconds:    74.32

 Number of Negative Eigenvalues in Matrix=           1
 Most negative value=  -32.1914808951219
 Most positive value=   571.428264193753
 Forcing positive definiteness
 Root mean square deviation of matrix from original=   8.801194720079160E-002

 Elapsed covariance time in seconds:     6.01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      865.517       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.04E+00  6.03E+00  1.24E+00  1.55E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.81E-01
 
 ETA2
+        1.66E-01  2.89E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.54E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.99E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        7.62E-01
 
 ETA2
+        4.05E-01  5.38E-01
 
 ETA3
+        0.00E+00  0.00E+00  5.04E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.73E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.29E-01  1.07E-01  1.71E-01  7.43E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.59E-01
 
 ETA2
+        9.37E-02  1.09E-01
 
 ETA3
+        0.00E+00  0.00E+00  6.73E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        7.48E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.04E-01
 
 ETA2
+        1.75E-01  1.01E-01
 
 ETA3
+       ......... .........  6.67E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.16E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.67E-02
 
 TH 2
+        7.83E-03  1.15E-02
 
 TH 3
+        1.42E-03 -1.11E-03  2.91E-02
 
 TH 4
+       -3.28E-04  1.06E-03  3.66E-03  5.52E-03
 
 OM11
+       -5.95E-03 -5.51E-03  2.62E-03  1.23E-03  2.52E-02
 
 OM12
+        1.02E-03 -2.73E-03 -1.32E-03 -5.96E-04  6.85E-03  8.78E-03
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.22E-03  1.72E-03 -7.62E-04 -9.51E-04 -4.52E-03  4.89E-03  0.00E+00  0.00E+00  1.19E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.75E-04 -4.05E-04  5.50E-04 -1.65E-03 -7.01E-04 -4.18E-05  0.00E+00  0.00E+00 -1.21E-04  0.00E+00  0.00E+00  4.53E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -1.85E-02 -1.12E-02 -6.12E-03  8.58E-03  3.65E-02  4.92E-03  0.00E+00  0.00E+00 -2.31E-02  0.00E+00  0.00E+00 -1.53E-02
          0.00E+00  0.00E+00  5.59E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.29E-01
 
 TH 2
+        5.66E-01  1.07E-01
 
 TH 3
+        6.46E-02 -6.09E-02  1.71E-01
 
 TH 4
+       -3.42E-02  1.34E-01  2.89E-01  7.43E-02
 
 OM11
+       -2.90E-01 -3.24E-01  9.69E-02  1.05E-01  1.59E-01
 
 OM12
+        8.43E-02 -2.72E-01 -8.24E-02 -8.57E-02  4.60E-01  9.37E-02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.71E-01  1.48E-01 -4.10E-02 -1.18E-01 -2.61E-01  4.79E-01  0.00E+00  0.00E+00  1.09E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        2.01E-02 -5.63E-02  4.79E-02 -3.31E-01 -6.57E-02 -6.63E-03  0.00E+00  0.00E+00 -1.65E-02  0.00E+00  0.00E+00  6.73E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -1.91E-01 -1.40E-01 -4.80E-02  1.54E-01  3.08E-01  7.02E-02  0.00E+00  0.00E+00 -2.84E-01  0.00E+00  0.00E+00 -3.04E-01
          0.00E+00  0.00E+00  7.48E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.13E+02
 
 TH 2
+       -8.25E+01  1.68E+02
 
 TH 3
+       -1.20E+01  2.88E+01 -8.60E+00
 
 TH 4
+        2.18E+01 -4.41E+01 -4.18E+01  2.43E+02
 
 OM11
+        2.44E+01 -9.97E+00 -3.23E+01 -5.96E+00  8.44E+01
 
 OM12
+       -5.46E+01  9.06E+01  2.74E+01 -1.10E+01 -1.20E+02  3.49E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -5.62E+00 -3.31E+01 -8.18E+00  1.71E+01  7.38E+01 -1.84E+02  0.00E+00  0.00E+00  2.05E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        2.55E+00 -1.44E+00 -1.80E+01  8.46E+01  8.15E+00 -2.82E+01  0.00E+00  0.00E+00  3.76E+01  0.00E+00  0.00E+00  2.81E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        1.67E-01 -2.87E-01  4.13E+00 -5.37E-01  2.55E-02 -3.02E+00  0.00E+00  0.00E+00  4.93E+00  0.00E+00  0.00E+00  7.61E+00
          0.00E+00  0.00E+00  2.12E+00
 
1
 
 
 #TBLN:      2
 #METH: Importance Sampling
 
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
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      20          
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        1000        
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-01   ,10.00000000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
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

 iteration            0 OBJ=   863.980143272548 eff.=     693. Smpl.=    1000. Fit.= 0.92342
 iteration            1 OBJ=   862.813569812625 eff.=     351. Smpl.=    1000. Fit.= 0.90638
 iteration            2 OBJ=   861.605815564960 eff.=     420. Smpl.=    1000. Fit.= 0.91060
 iteration            3 OBJ=   861.658867070911 eff.=     373. Smpl.=    1000. Fit.= 0.90114
 iteration            4 OBJ=   862.683580534955 eff.=     402. Smpl.=    1000. Fit.= 0.90296
 iteration            5 OBJ=   859.961075707827 eff.=     455. Smpl.=    1000. Fit.= 0.90642
 iteration            6 OBJ=   864.331125707702 eff.=     403. Smpl.=    1000. Fit.= 0.90477
 iteration            7 OBJ=   862.910447437090 eff.=     480. Smpl.=    1000. Fit.= 0.90361
 iteration            8 OBJ=   860.856875996888 eff.=     386. Smpl.=    1000. Fit.= 0.90379
 iteration            9 OBJ=   862.270847366698 eff.=     407. Smpl.=    1000. Fit.= 0.90550
 iteration           10 OBJ=   862.074019018470 eff.=     434. Smpl.=    1000. Fit.= 0.90569
 iteration           11 OBJ=   863.580633037175 eff.=     377. Smpl.=    1000. Fit.= 0.90297
 iteration           12 OBJ=   862.441968894210 eff.=     425. Smpl.=    1000. Fit.= 0.90519
 iteration           13 OBJ=   863.149599444177 eff.=     391. Smpl.=    1000. Fit.= 0.90322
 iteration           14 OBJ=   861.663878432888 eff.=     429. Smpl.=    1000. Fit.= 0.90445
 iteration           15 OBJ=   861.805199645314 eff.=     383. Smpl.=    1000. Fit.= 0.90364
 iteration           16 OBJ=   863.885491560940 eff.=     397. Smpl.=    1000. Fit.= 0.90478
 iteration           17 OBJ=   862.767847619652 eff.=     420. Smpl.=    1000. Fit.= 0.90349
 iteration           18 OBJ=   862.221741538309 eff.=     399. Smpl.=    1000. Fit.= 0.90487
 iteration           19 OBJ=   859.407571207654 eff.=     417. Smpl.=    1000. Fit.= 0.90445
 iteration           20 OBJ=   863.913204634894 eff.=     413. Smpl.=    1000. Fit.= 0.90467
 
 #TERM:
 OPTIMIZATION WAS NOT COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -6.9404E-03 -1.0943E-02  4.3840E-05  0.0000E+00
 SE:             4.6224E-02  3.4187E-02  3.7501E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         8.8065E-01  7.4889E-01  9.9907E-01  1.0000E+00
 
 ETAshrink(%):   3.6845E+01  4.4463E+01  2.2814E+01  0.0000E+00
 EBVshrink(%):   3.5635E+01  4.3986E+01  2.3027E+01  0.0000E+00
 EPSshrink(%):   1.5888E+01
 
 #TERE:
 Elapsed estimation time in seconds:    24.97
 Elapsed covariance time in seconds:     5.90
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      863.913       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.08E+00  6.03E+00  1.39E+00  1.57E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.41E-01
 
 ETA2
+        2.08E-01  3.83E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.38E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.92E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        7.36E-01
 
 ETA2
+        4.57E-01  6.19E-01
 
 ETA3
+        0.00E+00  0.00E+00  4.88E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.71E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.66E-01  1.37E-01  9.89E-02  7.07E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.60E-01
 
 ETA2
+        1.42E-01  2.06E-01
 
 ETA3
+        0.00E+00  0.00E+00  6.58E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        8.30E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.08E-01
 
 ETA2
+        2.09E-01  1.66E-01
 
 ETA3
+       ......... .........  6.74E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.43E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.75E-02
 
 TH 2
+        1.52E-02  1.88E-02
 
 TH 3
+       -4.57E-03 -3.89E-03  9.79E-03
 
 TH 4
+       -7.63E-04  1.43E-03  8.33E-04  5.00E-03
 
 OM11
+       -8.07E-03 -8.23E-03 -1.56E-04  1.75E-04  2.55E-02
 
 OM12
+        1.06E-02  6.79E-04 -1.98E-03 -1.18E-03  7.17E-03  2.01E-02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.19E-02  1.01E-02 -4.07E-03 -1.57E-03 -7.63E-03  2.12E-02  0.00E+00  0.00E+00  4.23E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.40E-03 -1.19E-03  8.64E-04 -1.34E-03  5.36E-04 -7.28E-04  0.00E+00  0.00E+00 -2.40E-03  0.00E+00  0.00E+00  4.33E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -1.59E-02 -4.25E-04 -1.57E-02  9.38E-03  1.30E-02 -1.86E-02  0.00E+00  0.00E+00 -3.87E-02  0.00E+00  0.00E+00 -1.92E-02
          0.00E+00  0.00E+00  6.88E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.66E-01
 
 TH 2
+        6.67E-01  1.37E-01
 
 TH 3
+       -2.79E-01 -2.87E-01  9.89E-02
 
 TH 4
+       -6.50E-02  1.48E-01  1.19E-01  7.07E-02
 
 OM11
+       -3.05E-01 -3.76E-01 -9.88E-03  1.55E-02  1.60E-01
 
 OM12
+        4.52E-01  3.50E-02 -1.41E-01 -1.17E-01  3.17E-01  1.42E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.42E-01  3.59E-01 -2.00E-01 -1.08E-01 -2.32E-01  7.27E-01  0.00E+00  0.00E+00  2.06E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.28E-01 -1.32E-01  1.33E-01 -2.88E-01  5.10E-02 -7.82E-02  0.00E+00  0.00E+00 -1.78E-01  0.00E+00  0.00E+00  6.58E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -1.15E-01 -3.74E-03 -1.91E-01  1.60E-01  9.80E-02 -1.58E-01  0.00E+00  0.00E+00 -2.27E-01  0.00E+00  0.00E+00 -3.52E-01
          0.00E+00  0.00E+00  8.30E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.17E+02
 
 TH 2
+       -7.81E+01  1.32E+02
 
 TH 3
+        8.09E+00  2.30E+01  1.27E+02
 
 TH 4
+        2.36E+01 -4.89E+01 -3.46E+01  2.48E+02
 
 OM11
+        2.94E+01 -7.06E+00  1.52E+01 -4.84E+00  9.87E+01
 
 OM12
+       -6.55E+01  6.51E+01 -8.68E+00 -9.69E+00 -1.17E+02  2.70E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.41E+00 -2.58E+01  1.09E+01  1.32E+01  6.45E+01 -1.41E+02  0.00E+00  0.00E+00  1.19E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        8.07E+00 -1.30E+01 -9.32E+00  8.06E+01  6.25E+00 -2.62E+01  0.00E+00  0.00E+00  4.17E+01  0.00E+00  0.00E+00  3.16E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        3.38E-01 -4.53E-01  3.40E+00 -8.38E-01 -1.44E-01 -7.05E-01  0.00E+00  0.00E+00  2.87E+00  0.00E+00  0.00E+00  9.21E+00
          0.00E+00  0.00E+00  1.95E+00
 
 #CPUT: Total CPU Time in Seconds,      110.605
Stop Time: 
Mon 09/30/2013 
02:09 PM

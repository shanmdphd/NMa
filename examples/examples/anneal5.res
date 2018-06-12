Mon 09/30/2013 
02:09 PM
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
$EST METHOD=CHAIN ISAMPLE=1 ISAMPEND=30 NSAMPLE=30 FILE=anneal5.chn
$ESTIMATION METH=IMP INTER NITER=50 ISAMPLE=1000 DF=4 CTYPE=3 NOABORT PRINT=1 SIGL=8
$ESTIMATION METH=IMP EONLY=1 INTER NITER=0 ISAMPLE=10000 DF=4 SIGL=8 MAPITER=0
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
 FROM SAMPLE 11 OF CHAIN FILE anneal5.chn
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
 ITERATIONS (NITER):                      50          
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
   1   2   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   1001.39700189329 eff.=     780. Smpl.=    1000. Fit.= 0.93037
 iteration            1 OBJ=   887.490798851439 eff.=     528. Smpl.=    1000. Fit.= 0.90933
 iteration            2 OBJ=   875.502125456331 eff.=     332. Smpl.=    1000. Fit.= 0.90725
 iteration            3 OBJ=   871.032423345357 eff.=     329. Smpl.=    1000. Fit.= 0.90272
 iteration            4 OBJ=   869.038516551047 eff.=     406. Smpl.=    1000. Fit.= 0.91232
 iteration            5 OBJ=   867.268220381987 eff.=     438. Smpl.=    1000. Fit.= 0.91330
 iteration            6 OBJ=   866.534120178198 eff.=     398. Smpl.=    1000. Fit.= 0.91058
 iteration            7 OBJ=   866.928050380509 eff.=     419. Smpl.=    1000. Fit.= 0.91387
 iteration            8 OBJ=   865.736645985675 eff.=     396. Smpl.=    1000. Fit.= 0.91066
 iteration            9 OBJ=   864.967399301302 eff.=     404. Smpl.=    1000. Fit.= 0.91277
 iteration           10 OBJ=   865.995470994611 eff.=     401. Smpl.=    1000. Fit.= 0.91176
 iteration           11 OBJ=   865.083288890741 eff.=     405. Smpl.=    1000. Fit.= 0.91086
 iteration           12 OBJ=   865.751784680448 eff.=     422. Smpl.=    1000. Fit.= 0.91250
 iteration           13 OBJ=   866.082410089496 eff.=     396. Smpl.=    1000. Fit.= 0.91029
 iteration           14 OBJ=   864.489739594638 eff.=     423. Smpl.=    1000. Fit.= 0.91255
 iteration           15 OBJ=   866.092622783154 eff.=     399. Smpl.=    1000. Fit.= 0.91175
 iteration           16 OBJ=   865.944058375375 eff.=     416. Smpl.=    1000. Fit.= 0.91463
 iteration           17 OBJ=   864.681804841391 eff.=     417. Smpl.=    1000. Fit.= 0.91308
 iteration           18 OBJ=   865.437208035520 eff.=     360. Smpl.=    1000. Fit.= 0.90692
 iteration           19 OBJ=   861.558744428592 eff.=     460. Smpl.=    1000. Fit.= 0.91658
 iteration           20 OBJ=   865.458586010472 eff.=     390. Smpl.=    1000. Fit.= 0.91090
 iteration           21 OBJ=   864.855425715626 eff.=     386. Smpl.=    1000. Fit.= 0.90605
 iteration           22 OBJ=   864.718951226536 eff.=     409. Smpl.=    1000. Fit.= 0.90922
 iteration           23 OBJ=   863.197117691591 eff.=     423. Smpl.=    1000. Fit.= 0.91016
 iteration           24 OBJ=   865.087153177728 eff.=     387. Smpl.=    1000. Fit.= 0.90928
 iteration           25 OBJ=   865.108207749961 eff.=     404. Smpl.=    1000. Fit.= 0.90787
 iteration           26 OBJ=   864.471316020630 eff.=     405. Smpl.=    1000. Fit.= 0.90714
 iteration           27 OBJ=   864.764076383412 eff.=     421. Smpl.=    1000. Fit.= 0.91277
 iteration           28 OBJ=   864.073477600372 eff.=     395. Smpl.=    1000. Fit.= 0.90934
 iteration           29 OBJ=   863.718164467014 eff.=     394. Smpl.=    1000. Fit.= 0.90739
 iteration           30 OBJ=   862.703013462011 eff.=     434. Smpl.=    1000. Fit.= 0.91096
 iteration           31 OBJ=   865.203813049467 eff.=     392. Smpl.=    1000. Fit.= 0.90866
 iteration           32 OBJ=   863.548192163055 eff.=     418. Smpl.=    1000. Fit.= 0.90977
 iteration           33 OBJ=   864.593550509996 eff.=     409. Smpl.=    1000. Fit.= 0.90554
 iteration           34 OBJ=   863.829022144165 eff.=     433. Smpl.=    1000. Fit.= 0.91181
 iteration           35 OBJ=   863.901623333696 eff.=     401. Smpl.=    1000. Fit.= 0.90889
 iteration           36 OBJ=   864.232522798485 eff.=     410. Smpl.=    1000. Fit.= 0.91027
 iteration           37 OBJ=   863.899047757058 eff.=     392. Smpl.=    1000. Fit.= 0.90658
 iteration           38 OBJ=   863.028496031647 eff.=     403. Smpl.=    1000. Fit.= 0.90710
 iteration           39 OBJ=   862.324348422217 eff.=     433. Smpl.=    1000. Fit.= 0.91131
 iteration           40 OBJ=   864.069585909415 eff.=     399. Smpl.=    1000. Fit.= 0.90759
 iteration           41 OBJ=   861.217015930028 eff.=     398. Smpl.=    1000. Fit.= 0.90331
 iteration           42 OBJ=   864.948285447188 eff.=     431. Smpl.=    1000. Fit.= 0.91156
 iteration           43 OBJ=   863.801101151162 eff.=     420. Smpl.=    1000. Fit.= 0.90801
 iteration           44 OBJ=   862.154734258377 eff.=     389. Smpl.=    1000. Fit.= 0.90538
 iteration           45 OBJ=   862.308004264937 eff.=     427. Smpl.=    1000. Fit.= 0.91021
 iteration           46 OBJ=   863.047712599146 eff.=     357. Smpl.=    1000. Fit.= 0.90262
 iteration           47 OBJ=   864.653270926619 eff.=     435. Smpl.=    1000. Fit.= 0.90868
 iteration           48 OBJ=   861.896575900530 eff.=     423. Smpl.=    1000. Fit.= 0.90499
 iteration           49 OBJ=   861.489928266977 eff.=     390. Smpl.=    1000. Fit.= 0.90658
 iteration           50 OBJ=   860.888497444458 eff.=     408. Smpl.=    1000. Fit.= 0.90703
 
 #TERM:
 OPTIMIZATION WAS NOT COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.2114E-03 -1.4061E-03  9.8145E-04  0.0000E+00
 SE:             5.1015E-02  2.9053E-02  3.8618E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         9.6542E-01  9.6140E-01  9.7972E-01  1.0000E+00
 
 ETAshrink(%):   3.1315E+01  4.4290E+01  2.1838E+01  0.0000E+00
 EBVshrink(%):   3.2789E+01  4.7145E+01  2.1512E+01  0.0000E+00
 EPSshrink(%):   2.2268E+01
 
 #TERE:
 Elapsed estimation time in seconds:    60.03

 Number of Negative Eigenvalues in Matrix=           1
 Most negative value=  -72.8428948963318
 Most positive value=   491.471601982748
 Forcing positive definiteness
 Root mean square deviation of matrix from original=   0.216317952549111

 Elapsed covariance time in seconds:     5.82
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      860.888       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.00E+00  5.97E+00  1.47E+00  1.57E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.57E-01
 
 ETA2
+        1.57E-01  2.75E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.47E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.91E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        7.46E-01
 
 ETA2
+        4.02E-01  5.24E-01
 
 ETA3
+        0.00E+00  0.00E+00  4.97E-01
 
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
 
         1.21E-01  1.08E-01  1.18E-01  6.98E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.33E-01
 
 ETA2
+        1.08E-01  1.20E-01
 
 ETA3
+        0.00E+00  0.00E+00  6.30E-02
 
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
+        8.89E-02
 
 ETA2
+        2.09E-01  1.14E-01
 
 ETA3
+       ......... .........  6.35E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.20E-01
 
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
+        1.46E-02
 
 TH 2
+        5.93E-03  1.17E-02
 
 TH 3
+        1.64E-03 -6.14E-04  1.40E-02
 
 TH 4
+       -2.35E-04  1.49E-03  7.74E-04  4.87E-03
 
 OM11
+       -3.67E-03 -4.05E-03 -5.92E-04  5.48E-04  1.76E-02
 
 OM12
+        1.85E-03 -4.36E-03  1.30E-03 -5.97E-04  6.09E-03  1.16E-02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.02E-03 -1.13E-03  8.97E-04 -8.57E-04 -1.17E-03  8.02E-03  0.00E+00  0.00E+00  1.43E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        6.48E-05 -2.14E-04 -1.31E-04 -1.39E-03 -3.34E-04 -1.60E-05  0.00E+00  0.00E+00 -5.00E-04  0.00E+00  0.00E+00  3.97E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -8.85E-03  7.60E-04 -4.76E-03  8.23E-03  7.43E-03 -8.07E-03  0.00E+00  0.00E+00 -2.35E-02  0.00E+00  0.00E+00 -1.42E-02
          0.00E+00  0.00E+00  5.65E-01
 
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
+        1.21E-01
 
 TH 2
+        4.55E-01  1.08E-01
 
 TH 3
+        1.15E-01 -4.81E-02  1.18E-01
 
 TH 4
+       -2.79E-02  1.97E-01  9.39E-02  6.98E-02
 
 OM11
+       -2.29E-01 -2.82E-01 -3.78E-02  5.91E-02  1.33E-01
 
 OM12
+        1.42E-01 -3.74E-01  1.02E-01 -7.94E-02  4.26E-01  1.08E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.78E-01 -8.77E-02  6.34E-02 -1.03E-01 -7.35E-02  6.23E-01  0.00E+00  0.00E+00  1.20E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        8.51E-03 -3.14E-02 -1.76E-02 -3.16E-01 -4.00E-02 -2.37E-03  0.00E+00  0.00E+00 -6.62E-02  0.00E+00  0.00E+00  6.30E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -9.75E-02  9.36E-03 -5.36E-02  1.57E-01  7.44E-02 -9.98E-02  0.00E+00  0.00E+00 -2.61E-01  0.00E+00  0.00E+00 -2.99E-01
          0.00E+00  0.00E+00  7.52E-01
 
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
+        1.12E+02
 
 TH 2
+       -7.57E+01  1.52E+02
 
 TH 3
+        5.27E+00  3.40E+01 -5.73E+01
 
 TH 4
+        2.14E+01 -4.57E+01 -2.81E+01  2.50E+02
 
 OM11
+        2.26E+01 -9.78E+00  1.93E+01 -6.97E+00  9.68E+01
 
 OM12
+       -5.46E+01  8.17E+01 -1.54E+00 -1.18E+01 -9.85E+01  2.78E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -6.11E+00 -2.30E+01  2.96E+01  1.64E+01  5.36E+01 -1.50E+02  0.00E+00  0.00E+00  1.63E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.50E+00 -1.06E+01  1.79E+00  8.22E+01  1.34E+01 -3.44E+01  0.00E+00  0.00E+00  4.75E+01  0.00E+00  0.00E+00  3.16E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        3.31E-01 -3.79E-01  1.05E+00 -8.07E-01  5.04E-01 -2.62E+00  0.00E+00  0.00E+00  5.05E+00  0.00E+00  0.00E+00  8.11E+00
          0.00E+00  0.00E+00  2.16E+00
 
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
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      0           
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        10000       
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                YES
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

 iteration            0 OBJ=   861.831840612591 eff.=    5677. Smpl.=   10000. Fit.= 0.91956
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.8717E-03  2.9772E-03  3.1545E-03  0.0000E+00
 SE:             5.0303E-02  2.8450E-02  3.8569E-02  0.0000E+00
 N:                     100         100         100           0
 
 P VAL.:         9.7032E-01  9.1665E-01  9.3481E-01  1.0000E+00
 
 ETAshrink(%):   3.2274E+01  4.5447E+01  2.1936E+01  0.0000E+00
 EBVshrink(%):   3.2456E+01  4.7535E+01  2.2097E+01  0.0000E+00
 EPSshrink(%):   2.6047E+01
 
 #TERE:
 Elapsed estimation time in seconds:     0.00
 Elapsed covariance time in seconds:    59.27
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      861.832       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         3.00E+00  5.97E+00  1.47E+00  1.57E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.57E-01
 
 ETA2
+        1.57E-01  2.75E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.47E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.91E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        7.46E-01
 
 ETA2
+        4.02E-01  5.24E-01
 
 ETA3
+        0.00E+00  0.00E+00  4.97E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.71E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.42E-01  1.19E-01  1.36E-01  7.13E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.40E-01
 
 ETA2
+        1.12E-01  1.44E-01
 
 ETA3
+        0.00E+00  0.00E+00  6.64E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        7.91E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.39E-02
 
 ETA2
+        2.07E-01  1.37E-01
 
 ETA3
+       ......... .........  6.69E-02
 
 ETA4
+       ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.32E-01
 
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
+        2.03E-02
 
 TH 2
+        1.04E-02  1.42E-02
 
 TH 3
+       -6.94E-03 -6.11E-03  1.84E-02
 
 TH 4
+       -1.09E-03  7.96E-04  1.83E-03  5.09E-03
 
 OM11
+       -4.71E-03 -4.27E-03  8.68E-04  7.70E-04  1.96E-02
 
 OM12
+        5.25E-03 -5.06E-04 -3.64E-03 -8.93E-04  5.37E-03  1.25E-02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.07E-02  4.78E-03 -7.54E-03 -1.57E-03 -2.90E-03  1.08E-02  0.00E+00  0.00E+00  2.07E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.13E-03  5.68E-04 -1.91E-03 -1.65E-03 -6.62E-04  7.53E-04  0.00E+00  0.00E+00  8.63E-04  0.00E+00  0.00E+00  4.41E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -2.25E-02 -1.12E-02  1.46E-02  1.05E-02  1.05E-02 -1.67E-02  0.00E+00  0.00E+00 -3.73E-02  0.00E+00  0.00E+00 -2.02E-02
          0.00E+00  0.00E+00  6.26E-01
 
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
+        1.42E-01
 
 TH 2
+        6.15E-01  1.19E-01
 
 TH 3
+       -3.59E-01 -3.77E-01  1.36E-01
 
 TH 4
+       -1.07E-01  9.35E-02  1.89E-01  7.13E-02
 
 OM11
+       -2.36E-01 -2.55E-01  4.56E-02  7.70E-02  1.40E-01
 
 OM12
+        3.30E-01 -3.80E-02 -2.40E-01 -1.12E-01  3.43E-01  1.12E-01
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.21E-01  2.78E-01 -3.86E-01 -1.53E-01 -1.44E-01  6.75E-01  0.00E+00  0.00E+00  1.44E-01
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.19E-01  7.16E-02 -2.12E-01 -3.49E-01 -7.11E-02  1.02E-01  0.00E+00  0.00E+00  9.03E-02  0.00E+00  0.00E+00  6.64E-02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+       -1.99E-01 -1.19E-01  1.36E-01  1.87E-01  9.50E-02 -1.90E-01  0.00E+00  0.00E+00 -3.28E-01  0.00E+00  0.00E+00 -3.84E-01
          0.00E+00  0.00E+00  7.91E-01
 
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
+        1.13E+02
 
 TH 2
+       -7.56E+01  1.50E+02
 
 TH 3
+        7.93E-01  2.81E+01  7.57E+01
 
 TH 4
+        2.16E+01 -4.90E+01 -2.08E+01  2.46E+02
 
 OM11
+        2.23E+01 -8.53E+00  7.00E+00 -4.80E+00  8.83E+01
 
 OM12
+       -5.07E+01  7.02E+01  2.47E-01 -1.55E+01 -9.38E+01  2.74E+02
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -8.80E+00 -2.62E+01  2.08E+01  1.41E+01  5.41E+01 -1.49E+02  0.00E+00  0.00E+00  1.60E+02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.80E+00 -1.35E+01  2.25E+01  8.00E+01  1.68E+01 -4.07E+01  0.00E+00  0.00E+00  4.28E+01  0.00E+00  0.00E+00  3.10E+02
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 SG11
+        1.68E-01  1.66E-01  9.67E-01 -6.74E-01  3.40E-01 -1.60E+00  0.00E+00  0.00E+00  4.49E+00  0.00E+00  0.00E+00  9.19E+00
          0.00E+00  0.00E+00  2.11E+00
 
 #CPUT: Total CPU Time in Seconds,      124.972
Stop Time: 
Mon 09/30/2013 
02:11 PM

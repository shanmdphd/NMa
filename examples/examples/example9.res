Mon 09/30/2013 
04:01 PM
;Model Desc: Two compartment Model, Using ADVAN3, TRANS4
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# Example 9 (from samp5l)
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
$DATA example9.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4 OTHER=aneal.f90

$PK
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
MU_4=THETA(4)
CL=DEXP(MU_1+ETA(1))
V1=DEXP(MU_2+ETA(2))
Q=DEXP(MU_3+ETA(3))
V2=DEXP(MU_4+ETA(4))
S1=V1

$ERROR
Y = F + F*EPS(1)

; Initial values of THETA
$THETA 
(0.001, 2.0) ;[LN(CL)]
(0.001, 2.0) ;[LN(V1)]
(0.001, 2.0) ;[LN(Q)]
(0.001, 2.0) ;[LN(V2)]
;INITIAL values of OMEGA
$OMEGA BLOCK(4)
0.05   ;[P]
0.01  ;[F]
0.05   ;[P]
0.01  ;[F]
0.01  ;[F]
0.05   ;[P]
0.01  ;[F]
0.01  ;[F]
0.01  ;[F]
0.05   ;[P]
;Initial value of SIGMA
$SIGMA 
(0.6 )   ;[P]

$EST METHOD=SAEM INTERACTION FILE=example9.ext NBURN=5000 NITER=500 PRINT=10 NOABORT SIGL=6 
    CTYPE=3 CINTERVAL=100 CITER=10 CALPHA=0.05
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
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
 RUN# Example 9 (from samp5l)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      600
 NO. OF DATA ITEMS IN DATA SET:  17
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   5   7   8   0   0  11   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID JID TIME CONC DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
0FORMAT FOR DATA:
 (2E2.0,3E4.0,E11.0,E4.0,4E2.0,2E7.0,E8.0,E7.0,E2.0,E5.0)

 TOT. NO. OF OBS RECS:      500
 TOT. NO. OF INDIVIDUALS:    100
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
  1  1  1  1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.5000E-01
                  0.1000E-01   0.5000E-01
                  0.1000E-01   0.1000E-01   0.5000E-01
                  0.1000E-01   0.1000E-01   0.1000E-01   0.5000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.6000E+00
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(P)

 TWO COMPARTMENT MODEL (ADVAN3)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   4
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   BASIC PK PARAMETER NO.  1: ELIMINATION RATE (K)
   BASIC PK PARAMETER NO.  2: CENTRAL-TO-PERIPH. RATE (K12)
   BASIC PK PARAMETER NO.  3: PERIPH.-TO-CENTRAL RATE (K21)
 TRANSLATOR WILL CONVERT PARAMETERS
 CL, V1, Q, V2 TO K, K12, K21 (TRANS4)
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         NO         YES        YES        YES
    2         PERIPH.      ON         NO         YES        NO         NO
    3         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            5           *           *           *           *
    2            *           *           *           *           *
    3            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0ERROR IN LOG Y IS MODELED
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      9
   TIME DATA ITEM IS DATA ITEM NO.:          5
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   7
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     8
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:   11

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    6           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   6           
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
 CONVERGENCE INTERVAL (CINTERVAL):        100         
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              5000        
 ITERATIONS (NITER):                      500         
 ANEAL SETTING (CONSTRAIN):               1           
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        2           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
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
 iteration        -5000 SAEMOBJ=   405.675342113477
 iteration        -4990 SAEMOBJ=  -1433.70329739686
 iteration        -4980 SAEMOBJ=  -1989.59632740355
 iteration        -4970 SAEMOBJ=  -2225.75957162013
 iteration        -4960 SAEMOBJ=  -2352.10726513168
 iteration        -4950 SAEMOBJ=  -2343.13285074178
 iteration        -4940 SAEMOBJ=  -2380.18826397703
 iteration        -4930 SAEMOBJ=  -2361.43101078320
 iteration        -4920 SAEMOBJ=  -2336.14363857127
 iteration        -4910 SAEMOBJ=  -2414.16398415953
 iteration        -4900 SAEMOBJ=  -2379.52928758004
 iteration        -4890 SAEMOBJ=  -2413.63977059502
 iteration        -4880 SAEMOBJ=  -2412.99869268411
 iteration        -4870 SAEMOBJ=  -2449.37136254096
 iteration        -4860 SAEMOBJ=  -2471.31910901693
 iteration        -4850 SAEMOBJ=  -2443.21538728127
 iteration        -4840 SAEMOBJ=  -2444.09020728757
 iteration        -4830 SAEMOBJ=  -2420.53253468605
 iteration        -4820 SAEMOBJ=  -2431.67039875666
 iteration        -4810 SAEMOBJ=  -2443.43846355349
 iteration        -4800 SAEMOBJ=  -2393.56183913331
 iteration        -4790 SAEMOBJ=  -2432.32884202607
 iteration        -4780 SAEMOBJ=  -2438.24028009091
 iteration        -4770 SAEMOBJ=  -2368.95337571722
 iteration        -4760 SAEMOBJ=  -2416.99148252432
 iteration        -4750 SAEMOBJ=  -2399.32563122380
 iteration        -4740 SAEMOBJ=  -2386.88634493910
 iteration        -4730 SAEMOBJ=  -2385.45069251745
 iteration        -4720 SAEMOBJ=  -2387.12351727171
 iteration        -4710 SAEMOBJ=  -2483.96807283744
 iteration        -4700 SAEMOBJ=  -2385.02284222592
 iteration        -4690 SAEMOBJ=  -2458.48839637246
 iteration        -4680 SAEMOBJ=  -2444.07007192805
 iteration        -4670 SAEMOBJ=  -2455.07392509523
 iteration        -4660 SAEMOBJ=  -2437.65267623205
 iteration        -4650 SAEMOBJ=  -2414.14724211614
 iteration        -4640 SAEMOBJ=  -2457.18510998313
 iteration        -4630 SAEMOBJ=  -2436.51989517802
 iteration        -4620 SAEMOBJ=  -2447.91193896100
 iteration        -4610 SAEMOBJ=  -2429.44006123989
 iteration        -4600 SAEMOBJ=  -2444.96841966155
 iteration        -4590 SAEMOBJ=  -2412.82037998754
 iteration        -4580 SAEMOBJ=  -2399.31539520304
 iteration        -4570 SAEMOBJ=  -2441.09735885760
 iteration        -4560 SAEMOBJ=  -2477.05298502092
 iteration        -4550 SAEMOBJ=  -2479.29219274749
 iteration        -4540 SAEMOBJ=  -2438.65174235739
 iteration        -4530 SAEMOBJ=  -2442.46053144250
 iteration        -4520 SAEMOBJ=  -2435.01121980636
 iteration        -4510 SAEMOBJ=  -2415.30725984672
 iteration        -4500 SAEMOBJ=  -2418.87550847058
 iteration        -4490 SAEMOBJ=  -2466.72414709841
 iteration        -4480 SAEMOBJ=  -2442.01033160868
 iteration        -4470 SAEMOBJ=  -2441.40995037815
 iteration        -4460 SAEMOBJ=  -2453.61009088198
 iteration        -4450 SAEMOBJ=  -2419.73812132846
 iteration        -4440 SAEMOBJ=  -2454.39342173458
 iteration        -4430 SAEMOBJ=  -2479.64461900439
 iteration        -4420 SAEMOBJ=  -2479.06640890042
 iteration        -4410 SAEMOBJ=  -2430.78672723371
 iteration        -4400 SAEMOBJ=  -2406.67547583503
 iteration        -4390 SAEMOBJ=  -2422.23067168302
 iteration        -4380 SAEMOBJ=  -2388.24006797143
 iteration        -4370 SAEMOBJ=  -2425.86896442716
 iteration        -4360 SAEMOBJ=  -2421.40092445087
 iteration        -4350 SAEMOBJ=  -2390.78490616847
 iteration        -4340 SAEMOBJ=  -2414.94965272503
 iteration        -4330 SAEMOBJ=  -2408.25668280678
 iteration        -4320 SAEMOBJ=  -2462.64215201714
 iteration        -4310 SAEMOBJ=  -2446.11877623839
 iteration        -4300 SAEMOBJ=  -2454.84652542661
 iteration        -4290 SAEMOBJ=  -2427.15038122286
 iteration        -4280 SAEMOBJ=  -2416.93573028862
 iteration        -4270 SAEMOBJ=  -2464.63313979890
 iteration        -4260 SAEMOBJ=  -2417.79335696959
 iteration        -4250 SAEMOBJ=  -2410.77025500104
 iteration        -4240 SAEMOBJ=  -2426.09979429672
 iteration        -4230 SAEMOBJ=  -2441.53302988626
 iteration        -4220 SAEMOBJ=  -2437.86302964586
 iteration        -4210 SAEMOBJ=  -2429.75865738751
 iteration        -4200 SAEMOBJ=  -2411.36576019513
 iteration        -4190 SAEMOBJ=  -2418.10776148147
 iteration        -4180 SAEMOBJ=  -2498.48387002448
 iteration        -4170 SAEMOBJ=  -2435.20665516927
 iteration        -4160 SAEMOBJ=  -2492.51435690791
 iteration        -4150 SAEMOBJ=  -2466.71026910969
 iteration        -4140 SAEMOBJ=  -2469.71115466891
 iteration        -4130 SAEMOBJ=  -2468.00632055319
 iteration        -4120 SAEMOBJ=  -2454.27254612538
 iteration        -4110 SAEMOBJ=  -2454.64198918585
 iteration        -4100 SAEMOBJ=  -2443.63023500716
 iteration        -4090 SAEMOBJ=  -2417.30214521259
 iteration        -4080 SAEMOBJ=  -2428.36774694823
 iteration        -4070 SAEMOBJ=  -2420.39210870565
 iteration        -4060 SAEMOBJ=  -2416.97943285274
 iteration        -4050 SAEMOBJ=  -2419.71252016333
 iteration        -4040 SAEMOBJ=  -2442.38323273125
 iteration        -4030 SAEMOBJ=  -2418.85050155111
 iteration        -4020 SAEMOBJ=  -2440.31073810654
 iteration        -4010 SAEMOBJ=  -2417.29099891717
 iteration        -4000 SAEMOBJ=  -2462.64198437377
 iteration        -3990 SAEMOBJ=  -2405.73574237737
 iteration        -3980 SAEMOBJ=  -2395.60452140208
 iteration        -3970 SAEMOBJ=  -2483.03985927375
 iteration        -3960 SAEMOBJ=  -2464.22655047837
 iteration        -3950 SAEMOBJ=  -2424.03391336087
 iteration        -3940 SAEMOBJ=  -2458.93666780198
 iteration        -3930 SAEMOBJ=  -2444.82966679886
 iteration        -3920 SAEMOBJ=  -2440.32168946225
 iteration        -3910 SAEMOBJ=  -2402.27932440118
 iteration        -3900 SAEMOBJ=  -2399.61904056133
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -2413.10762733515
 iteration           10 SAEMOBJ=  -2441.12460131397
 iteration           20 SAEMOBJ=  -2452.15692886092
 iteration           30 SAEMOBJ=  -2463.02013830030
 iteration           40 SAEMOBJ=  -2471.95806786150
 iteration           50 SAEMOBJ=  -2476.85422972222
 iteration           60 SAEMOBJ=  -2480.32076433196
 iteration           70 SAEMOBJ=  -2481.42577737784
 iteration           80 SAEMOBJ=  -2481.75261660116
 iteration           90 SAEMOBJ=  -2482.69340583349
 iteration          100 SAEMOBJ=  -2483.25849427735
 iteration          110 SAEMOBJ=  -2482.99675066597
 iteration          120 SAEMOBJ=  -2483.80893406591
 iteration          130 SAEMOBJ=  -2483.47305989385
 iteration          140 SAEMOBJ=  -2483.47281761893
 iteration          150 SAEMOBJ=  -2483.75114149459
 iteration          160 SAEMOBJ=  -2484.58302799032
 iteration          170 SAEMOBJ=  -2484.91082515959
 iteration          180 SAEMOBJ=  -2484.82114377785
 iteration          190 SAEMOBJ=  -2484.21075784175
 iteration          200 SAEMOBJ=  -2484.40945737634
 iteration          210 SAEMOBJ=  -2484.43650817725
 iteration          220 SAEMOBJ=  -2484.23783841884
 iteration          230 SAEMOBJ=  -2484.14662460849
 iteration          240 SAEMOBJ=  -2484.29699305007
 iteration          250 SAEMOBJ=  -2484.47569455000
 iteration          260 SAEMOBJ=  -2484.88736851434
 iteration          270 SAEMOBJ=  -2485.34180141609
 iteration          280 SAEMOBJ=  -2486.00530615499
 iteration          290 SAEMOBJ=  -2485.52486911724
 iteration          300 SAEMOBJ=  -2485.12254847370
 iteration          310 SAEMOBJ=  -2484.94889001198
 iteration          320 SAEMOBJ=  -2485.06507748598
 iteration          330 SAEMOBJ=  -2484.99933635463
 iteration          340 SAEMOBJ=  -2484.96418624305
 iteration          350 SAEMOBJ=  -2485.00927472335
 iteration          360 SAEMOBJ=  -2484.85165577558
 iteration          370 SAEMOBJ=  -2484.77088535343
 iteration          380 SAEMOBJ=  -2484.60814495097
 iteration          390 SAEMOBJ=  -2484.66892383274
 iteration          400 SAEMOBJ=  -2484.75922997063
 iteration          410 SAEMOBJ=  -2484.70776735534
 iteration          420 SAEMOBJ=  -2484.64930300210
 iteration          430 SAEMOBJ=  -2484.70640942364
 iteration          440 SAEMOBJ=  -2484.87342245025
 iteration          450 SAEMOBJ=  -2484.72037207123
 iteration          460 SAEMOBJ=  -2484.55321490103
 iteration          470 SAEMOBJ=  -2484.32397384615
 iteration          480 SAEMOBJ=  -2484.03523324612
 iteration          490 SAEMOBJ=  -2483.87172378538
 iteration          500 SAEMOBJ=  -2483.84036257996
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.7863E-05 -3.3604E-05 -3.5729E-05 -4.7628E-05
 SE:             3.9788E-02  2.8993E-02  3.1819E-02  3.2817E-02
 N:                     100         100         100         100
 
 P VAL.:         9.9964E-01  9.9908E-01  9.9910E-01  9.9884E-01
 
 ETAshrink(%):   3.5132E+00  2.2552E+01  2.7147E+01  1.6501E+01
 EBVshrink(%):   3.5095E+00  2.2549E+01  2.7145E+01  1.6493E+01
 EPSshrink(%):   3.0488E+01
 
 #TERE:
 Elapsed estimation time in seconds:    52.87
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -2483.840       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.64E+00  1.56E+00  7.55E-01  2.36E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.72E-01
 
 ETA2
+        2.81E-03  1.42E-01
 
 ETA3
+        1.95E-02 -3.81E-03  1.93E-01
 
 ETA4
+       -1.31E-02  1.65E-02  3.00E-02  1.56E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.66E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.14E-01
 
 ETA2
+        1.80E-02  3.76E-01
 
 ETA3
+        1.07E-01 -2.31E-02  4.39E-01
 
 ETA4
+       -8.00E-02  1.11E-01  1.73E-01  3.95E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.38E-01
 
 #CPUT: Total CPU Time in Seconds,       49.031
Stop Time: 
Mon 09/30/2013 
04:02 PM

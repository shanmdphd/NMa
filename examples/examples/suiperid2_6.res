Sat 09/07/2013 
09:36 PM
$PROB RUN# Example 1 (from samp5l)
$INPUT ID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT SID CID
$DATA superid2.csv

$SUBROUTINES ADVAN1 TRANS2

$PK
MU_1=THETA(1)
MU_2=THETA(2)
CL=DEXP(MU_1+ETA(1)+ETA(3)+ETA(5))
V=DEXP(MU_2+ETA(2)+ETA(4)+ETA(6))
S1=V

$ERROR
Y = F + F*EPS(1)

; Initial values of THETA
$THETA 5.0 5.0
;$OMEGA 0.02 0.02 0.0 FIXED 0.0 FIXED 0.0 FIXED 0.0 FIXED
$OMEGA BLOCK(2)
0.1
0.00001 0.1

$OMEGA BLOCK(2)
0.3
0.00001 0.3

$OMEGA BLOCK(2)
1.0
0.00001 1.0

$SIGMA 
0.1

$LEVEL
SID=(3[1],4[2])
CID=(5[3],6[4])

;$EST METHOD=ITS INTERACTION PRINT=1 NSIG=3 NITER=12 SIGL=6 FNLETA=0
$EST METHOD=SAEM INTERACTION PRINT=1 NSIG=3 NBURN=30 NITER=100 CTYPE=3 CINTERVAL=50 SIGL=6
$EST METHOD=IMP INTERACTION PRINT=1 NSIG=3 NITER=10 CTYPE=3 ISAMPLE=300 SIGL=6 EONLY=1 MAPITER=0
$COV MATRIX=R UNCONDITIONAL
$TABLE  ID TIME CONC DOSE RATE EVID MDV CMT SID CID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
NOAPPEND ONEHEADER FILE=superid2_6.tab  NOPRINT
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 12) MU_001: SHOULD NOT BE ASSOCIATED WITH ETA(003)

 (MU_WARNING 11) MU_001: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_001: SHOULD NOT BE ASSOCIATED WITH ETA(005)

 (MU_WARNING 11) MU_001: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_002: SHOULD NOT BE ASSOCIATED WITH ETA(004)

 (MU_WARNING 11) MU_002: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_002: SHOULD NOT BE ASSOCIATED WITH ETA(006)

 (MU_WARNING 11) MU_002: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        7 SEP 2013
Days until program expires :6110
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3beta7.0(N)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 RUN# Example 1 (from samp5l)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:    20000
 NO. OF DATA ITEMS IN DATA SET:  10
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   2   4   5   0   0   8   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME CONC DOSE RATE EVID MDV CMT SID CID
0FORMAT FOR DATA:
 (7E10.0/3E10.0)

 TOT. NO. OF OBS RECS:    17500
 TOT. NO. OF INDIVIDUALS:   2500
0LENGTH OF THETA:   2
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  0  0  2
  0  0  2  2
  0  0  0  0  3
  0  0  0  0  3  3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.5000E+01  0.5000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.1000E+00
                  0.1000E-04   0.1000E+00
        2                                                                                   NO
                  0.3000E+00
                  0.1000E-04   0.3000E+00
        3                                                                                   NO
                  0.1000E+01
                  0.1000E-04   0.1000E+01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 RELATIVE TOLERANCE (TOL):                  -1
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY (ATOL): -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 RESUME COV ANALYSIS (RESUME):               NO
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE:            EIGENVALUE
0-- TABLE   1 --
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADERS:               ONE
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID TIME CONC DOSE RATE EVID MDV CMT SID CID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(N)

 ONE COMPARTMENT MODEL (ADVAN1)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   2
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1

 TRANSLATOR WILL CONVERT PARAMETERS
 CLEARANCE (CL) AND VOLUME (V) TO K (TRANS2)
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         NO         YES        YES        YES
    2         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            3           *           *           *           *
    2            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0ERROR IN LOG Y IS MODELED
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      6
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     5
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    8

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
 CONVERGENCE INTERVAL (CINTERVAL):        50          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              30          
 ITERATIONS (NITER):                      100         
 ANEAL SETTING (CONSTRAIN):               1           
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        2           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.00000000        
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
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration          -30 SAEMOBJ=  1.277296277318074E+021
 iteration          -29 SAEMOBJ=   44268.7829140358
 iteration          -28 SAEMOBJ=  -9215.47922286188
 iteration          -27 SAEMOBJ=  -26714.9110364948
 iteration          -26 SAEMOBJ=  -35888.3615352560
 iteration          -25 SAEMOBJ=  -47089.4939796318
 iteration          -24 SAEMOBJ=  -56336.7186026357
 iteration          -23 SAEMOBJ=  -64083.1586196213
 iteration          -22 SAEMOBJ=  -70069.2755233608
 iteration          -21 SAEMOBJ=  -73049.9970311690
 iteration          -20 SAEMOBJ=  -73671.8362631163
 iteration          -19 SAEMOBJ=  -73989.3321366611
 iteration          -18 SAEMOBJ=  -74102.2089850511
 iteration          -17 SAEMOBJ=  -74231.0075988849
 iteration          -16 SAEMOBJ=  -74229.2127446911
 iteration          -15 SAEMOBJ=  -74344.9278810969
 iteration          -14 SAEMOBJ=  -74458.0074000380
 iteration          -13 SAEMOBJ=  -74478.0618980878
 iteration          -12 SAEMOBJ=  -74588.3365413588
 iteration          -11 SAEMOBJ=  -74612.0299010234
 iteration          -10 SAEMOBJ=  -74474.4080946395
 iteration           -9 SAEMOBJ=  -74536.9526197547
 iteration           -8 SAEMOBJ=  -74591.0523414687
 iteration           -7 SAEMOBJ=  -74677.5611204274
 iteration           -6 SAEMOBJ=  -74656.7086866539
 iteration           -5 SAEMOBJ=  -74523.5516200615
 iteration           -4 SAEMOBJ=  -74663.7073663307
 iteration           -3 SAEMOBJ=  -74696.5710978782
 iteration           -2 SAEMOBJ=  -74743.6585248060
 iteration           -1 SAEMOBJ=  -74740.3539210559
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -74688.7098969092
 iteration            1 SAEMOBJ=  -74951.4098534893
 iteration            2 SAEMOBJ=  -75052.8844237046
 iteration            3 SAEMOBJ=  -75116.7243476654
 iteration            4 SAEMOBJ=  -75149.0059453049
 iteration            5 SAEMOBJ=  -75167.7070121540
 iteration            6 SAEMOBJ=  -75194.4773018798
 iteration            7 SAEMOBJ=  -75220.5566947979
 iteration            8 SAEMOBJ=  -75232.9100740798
 iteration            9 SAEMOBJ=  -75241.2679571907
 iteration           10 SAEMOBJ=  -75245.2547534515
 iteration           11 SAEMOBJ=  -75249.5951932100
 iteration           12 SAEMOBJ=  -75255.8716563292
 iteration           13 SAEMOBJ=  -75257.7392622204
 iteration           14 SAEMOBJ=  -75262.5732144485
 iteration           15 SAEMOBJ=  -75267.8679533164
 iteration           16 SAEMOBJ=  -75273.5023203850
 iteration           17 SAEMOBJ=  -75275.5560463121
 iteration           18 SAEMOBJ=  -75277.0185870405
 iteration           19 SAEMOBJ=  -75280.7356185033
 iteration           20 SAEMOBJ=  -75284.5228818347
 iteration           21 SAEMOBJ=  -75287.8347576872
 iteration           22 SAEMOBJ=  -75289.8185923006
 iteration           23 SAEMOBJ=  -75290.5103667816
 iteration           24 SAEMOBJ=  -75294.4657108247
 iteration           25 SAEMOBJ=  -75298.3224703965
 iteration           26 SAEMOBJ=  -75301.3564674912
 iteration           27 SAEMOBJ=  -75302.9109865241
 iteration           28 SAEMOBJ=  -75305.5272876526
 iteration           29 SAEMOBJ=  -75303.7541197338
 iteration           30 SAEMOBJ=  -75306.4831292635
 iteration           31 SAEMOBJ=  -75311.3924153205
 iteration           32 SAEMOBJ=  -75311.5504519095
 iteration           33 SAEMOBJ=  -75314.0061997050
 iteration           34 SAEMOBJ=  -75316.0138729970
 iteration           35 SAEMOBJ=  -75318.8209727692
 iteration           36 SAEMOBJ=  -75320.0483053272
 iteration           37 SAEMOBJ=  -75321.6967183791
 iteration           38 SAEMOBJ=  -75323.5873445898
 iteration           39 SAEMOBJ=  -75324.0459609132
 iteration           40 SAEMOBJ=  -75325.5886037040
 iteration           41 SAEMOBJ=  -75325.5868901029
 iteration           42 SAEMOBJ=  -75326.9581974553
 iteration           43 SAEMOBJ=  -75328.7756259046
 iteration           44 SAEMOBJ=  -75326.8225283771
 iteration           45 SAEMOBJ=  -75327.7071174511
 iteration           46 SAEMOBJ=  -75328.4915499502
 iteration           47 SAEMOBJ=  -75329.1344031238
 iteration           48 SAEMOBJ=  -75331.3139741055
 iteration           49 SAEMOBJ=  -75331.0353940902
 iteration           50 SAEMOBJ=  -75330.0866665458
 iteration           51 SAEMOBJ=  -75329.6577507988
 iteration           52 SAEMOBJ=  -75330.0087389459
 iteration           53 SAEMOBJ=  -75330.0928885285
 iteration           54 SAEMOBJ=  -75329.5733025055
 iteration           55 SAEMOBJ=  -75330.5702352722
 iteration           56 SAEMOBJ=  -75330.5639314586
 iteration           57 SAEMOBJ=  -75331.3114774988
 iteration           58 SAEMOBJ=  -75331.9090109463
 iteration           59 SAEMOBJ=  -75333.5847359620
 iteration           60 SAEMOBJ=  -75335.2073237725
 iteration           61 SAEMOBJ=  -75335.7876005407
 iteration           62 SAEMOBJ=  -75335.9077745099
 iteration           63 SAEMOBJ=  -75335.5102437157
 iteration           64 SAEMOBJ=  -75335.7805896980
 iteration           65 SAEMOBJ=  -75335.2684668384
 iteration           66 SAEMOBJ=  -75335.1817123177
 iteration           67 SAEMOBJ=  -75333.8413343059
 iteration           68 SAEMOBJ=  -75334.1493162806
 iteration           69 SAEMOBJ=  -75334.8951429514
 iteration           70 SAEMOBJ=  -75335.3916606637
 iteration           71 SAEMOBJ=  -75335.0708050750
 iteration           72 SAEMOBJ=  -75335.8684225422
 iteration           73 SAEMOBJ=  -75336.4335539670
 iteration           74 SAEMOBJ=  -75336.1887839421
 iteration           75 SAEMOBJ=  -75336.9085081512
 iteration           76 SAEMOBJ=  -75337.4458422762
 iteration           77 SAEMOBJ=  -75337.4534784403
 iteration           78 SAEMOBJ=  -75337.8635551564
 iteration           79 SAEMOBJ=  -75337.6035644935
 iteration           80 SAEMOBJ=  -75338.5347283516
 iteration           81 SAEMOBJ=  -75339.3322985271
 iteration           82 SAEMOBJ=  -75340.1708605738
 iteration           83 SAEMOBJ=  -75340.2251599756
 iteration           84 SAEMOBJ=  -75339.9454338434
 iteration           85 SAEMOBJ=  -75340.1539569309
 iteration           86 SAEMOBJ=  -75339.6739114700
 iteration           87 SAEMOBJ=  -75338.8795906558
 iteration           88 SAEMOBJ=  -75339.0619997397
 iteration           89 SAEMOBJ=  -75339.2128443896
 iteration           90 SAEMOBJ=  -75339.3315329277
 iteration           91 SAEMOBJ=  -75339.5354525471
 iteration           92 SAEMOBJ=  -75339.3729070429
 iteration           93 SAEMOBJ=  -75339.7285904131
 iteration           94 SAEMOBJ=  -75340.5174661712
 iteration           95 SAEMOBJ=  -75341.4734653345
 iteration           96 SAEMOBJ=  -75341.6120160624
 iteration           97 SAEMOBJ=  -75341.8284003589
 iteration           98 SAEMOBJ=  -75342.2465209132
 iteration           99 SAEMOBJ=  -75342.8176367465
 iteration          100 SAEMOBJ=  -75342.9738724941
 
 #TERM:
 STOCHASTIC PORTION WAS NOT TESTED FOR CONVERGENCE
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -3.8558E-06 -4.1008E-06  1.0014E-17  1.3406E-18  6.7413E-17  1.7151E-16
 SE:             1.6732E-03  1.7573E-03  9.9679E-03  1.0699E-02  6.2436E-02  5.5624E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         9.9816E-01  9.9814E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.0740E+01  1.0378E+01  1.0000E-10  1.8871E-03  1.0000E-10  3.7144E-03
 EBVshrink(%):   1.0743E+01  1.0371E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.1887E+01
 
 #TERE:
 Elapsed estimation time in seconds:   734.84
 Elapsed covariance time in seconds:     0.76
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -75342.974       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         1.99E+00  3.63E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.76E-03
 
 ETA2
+        2.61E-05  1.07E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.76E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.19E-03  3.18E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.02E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.82E-02  8.06E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.90E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.88E-02
 
 ETA2
+        2.56E-03  1.03E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.66E-01
 
 ETA4
+        0.00E+00  0.00E+00  4.02E-02  1.78E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.19E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.01E-01  2.84E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.95E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.35E-03  2.44E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        3.43E-04
 
 ETA2
+        2.57E-04  3.82E-04
 
 ETA3
+        0.00E+00  0.00E+00  2.33E-03
 
 ETA4
+        0.00E+00  0.00E+00  1.86E-03  2.71E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.28E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.46E-02  2.70E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.26E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.74E-03
 
 ETA2
+        2.52E-02  1.85E-03
 
 ETA3
+       ......... .........  7.00E-03
 
 ETA4
+       ......... .........  6.29E-02  7.61E-03
 
 ETA5
+       ......... ......... ......... .........  5.14E-02
 
 ETA6
+       ......... ......... ......... .........  2.44E-01  4.75E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.31E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        5.55E-06
 
 TH 2
+        2.93E-07  5.96E-06
 
 OM11
+       -1.49E-08  5.09E-08  1.18E-07
 
 OM12
+        6.51E-08  2.06E-08  7.51E-09  6.60E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.22E-09  1.27E-08 -1.25E-09  8.87E-11  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.46E-07
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+        6.24E-08  5.25E-08  1.75E-08  8.62E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.40E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.41E-06
 
 OM34
+        2.93E-10  3.51E-08 -7.75E-09 -1.94E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.86E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.71E-07  3.45E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        3.52E-08  1.80E-08  1.38E-08  4.26E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.11E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -9.75E-08 -4.20E-09  0.00E+00  0.00E+00  7.37E-06
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+        1.76E-06 -3.31E-07  1.80E-07  1.55E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.77E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.82E-06  2.48E-06  0.00E+00  0.00E+00  5.08E-06  0.00E+00  0.00E+00  1.07E-03
 
 OM56
+       -4.28E-07  4.13E-07 -5.36E-08  1.55E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.47E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.59E-06  1.74E-06  0.00E+00  0.00E+00 -1.32E-06  0.00E+00  0.00E+00 -5.33E-04  6.05E-04
 
 OM66
+        1.33E-06  9.63E-07  4.64E-08 -1.05E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.64E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.24E-07 -5.06E-06  0.00E+00  0.00E+00  1.35E-06  0.00E+00  0.00E+00  1.72E-04 -1.93E-04  7.28E-04
 
 SG11
+        1.52E-08  7.05E-09 -3.43E-09 -6.78E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.11E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.45E-09  1.56E-10  0.00E+00  0.00E+00 -1.42E-09  0.00E+00  0.00E+00 -1.09E-07  2.99E-08 -2.62E-09  1.58E-08
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.35E-03
 
 TH 2
+        5.09E-02  2.44E-03
 
 OM11
+       -1.85E-02  6.08E-02  3.43E-04
 
 OM12
+        1.08E-01  3.28E-02  8.52E-02  2.57E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.36E-03  1.37E-02 -9.53E-03  9.03E-04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.82E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+        1.14E-02  9.26E-03  2.20E-02  1.44E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.58E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.33E-03
 
 OM34
+        6.70E-05  7.73E-03 -1.21E-02 -4.06E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.39E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.59E-02  1.86E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        5.51E-03  2.71E-03  1.48E-02  6.11E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.07E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.54E-02 -8.32E-04  0.00E+00  0.00E+00  2.71E-03
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+        2.28E-02 -4.15E-03  1.60E-02  1.84E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.21E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.64E-02  4.08E-02  0.00E+00  0.00E+00  5.71E-02  0.00E+00  0.00E+00  3.28E-02
 
 OM56
+       -7.39E-03  6.88E-03 -6.34E-03  2.45E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.75E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.02E-02  3.80E-02  0.00E+00  0.00E+00 -1.98E-02  0.00E+00  0.00E+00 -6.62E-01  2.46E-02
 
 OM66
+        2.09E-02  1.46E-02  5.01E-03 -1.51E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.50E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -6.75E-03 -1.01E-01  0.00E+00  0.00E+00  1.84E-02  0.00E+00  0.00E+00  1.95E-01 -2.91E-01  2.70E-02
 
 SG11
+        5.14E-02  2.30E-02 -7.96E-02 -2.10E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -8.57E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  8.39E-03  6.70E-04  0.00E+00  0.00E+00 -4.16E-03  0.00E+00  0.00E+00 -2.64E-02  9.66E-03 -7.74E-04  1.26E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        1.84E+05
 
 TH 2
+       -8.38E+03  1.69E+05
 
 OM11
+        3.43E+04 -7.46E+04  8.65E+06
 
 OM12
+       -1.84E+05 -3.75E+04 -9.68E+05  1.55E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.19E+03 -1.88E+04  1.26E+05 -3.64E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.92E+06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+       -1.71E+03 -1.65E+03 -2.52E+04 -1.49E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.64E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.88E+05
 
 OM34
+       -1.14E+03 -2.59E+03  1.24E+04  9.34E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.48E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.11E+04  2.98E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -5.97E+02 -2.87E+02 -1.45E+04 -5.35E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  8.60E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.12E+03  1.08E+03  0.00E+00  0.00E+00  1.36E+05
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+       -3.89E+02  4.13E+01 -1.34E+03 -3.74E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.94E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.02E+02 -2.09E+03  0.00E+00  0.00E+00 -8.90E+02  0.00E+00  0.00E+00  1.68E+03
 
 OM56
+       -2.68E+02 -1.77E+02 -5.44E+02 -7.13E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.67E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  9.79E+02 -1.89E+03  0.00E+00  0.00E+00 -5.19E+02  0.00E+00  0.00E+00  1.48E+03  3.11E+03
 
 OM66
+       -3.43E+02 -2.97E+02 -2.97E+02  2.32E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.61E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  6.91E+02  2.08E+03  0.00E+00  0.00E+00 -1.63E+02  0.00E+00  0.00E+00 -1.80E+01  4.67E+02  1.52E+03
 
 SG11
+       -1.77E+05 -8.95E+04  1.87E+06  6.39E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.86E+06  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.52E+04 -1.15E+04  0.00E+00  0.00E+00  6.15E+03  0.00E+00  0.00E+00  9.54E+03  5.05E+03  8.02E+02  6.46E+07
 
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
 EM OR BAYESIAN METHOD USED:              IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        50          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      10          
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        300         
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                YES
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   0.100000000000000       ,10.0000000000000        
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
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
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -40959.5995764523 eff.=     307. Smpl.=     300. Fit.= 0.94636
 iteration            1 OBJ=  -41100.2627948795 eff.=     119. Smpl.=     300. Fit.= 0.78751
 iteration            2 OBJ=  -41163.6006241472 eff.=     120. Smpl.=     300. Fit.= 0.78804
 iteration            3 OBJ=  -41181.8763565212 eff.=     120. Smpl.=     300. Fit.= 0.78928
 iteration            4 OBJ=  -41191.7838978545 eff.=     120. Smpl.=     300. Fit.= 0.78902
 iteration            5 OBJ=  -41192.4470723409 eff.=     120. Smpl.=     300. Fit.= 0.78891
 iteration            6 OBJ=  -41191.4704527700 eff.=     120. Smpl.=     300. Fit.= 0.78930
 iteration            7 OBJ=  -41187.0396895826 eff.=     120. Smpl.=     300. Fit.= 0.78901
 iteration            8 OBJ=  -41190.5068610670 eff.=     120. Smpl.=     300. Fit.= 0.78924
 iteration            9 OBJ=  -41193.9592516872 eff.=     121. Smpl.=     300. Fit.= 0.79001
 iteration           10 OBJ=  -41189.2288416004 eff.=     120. Smpl.=     300. Fit.= 0.78963
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.3157E-04 -3.9461E-05 -1.1013E-17 -3.5805E-19 -1.2979E-16 -7.1817E-16
 SE:             1.6677E-03  1.7516E-03  9.9930E-03  1.0724E-02  6.2426E-02  5.5680E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         8.8956E-01  9.8203E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.1033E+01  1.0665E+01  1.0000E-10  1.0000E-10  1.5269E-02  1.0000E-10
 EBVshrink(%):   1.0953E+01  1.0567E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.1933E+01
 
 #TERE:
 Elapsed estimation time in seconds:   588.37
 Elapsed covariance time in seconds:    61.92
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41189.229       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         1.99E+00  3.63E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.76E-03
 
 ETA2
+        2.61E-05  1.07E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.76E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.19E-03  3.18E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.02E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.82E-02  8.06E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.90E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.88E-02
 
 ETA2
+        2.56E-03  1.03E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.66E-01
 
 ETA4
+        0.00E+00  0.00E+00  4.02E-02  1.78E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.19E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.01E-01  2.84E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.95E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.23E-03  2.32E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        3.46E-04
 
 ETA2
+        2.60E-04  3.81E-04
 
 ETA3
+        0.00E+00  0.00E+00  2.32E-03
 
 ETA4
+        0.00E+00  0.00E+00  1.85E-03  2.71E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.28E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.46E-02  2.69E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.24E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.75E-03
 
 ETA2
+        2.55E-02  1.84E-03
 
 ETA3
+       ......... .........  6.97E-03
 
 ETA4
+       ......... .........  6.25E-02  7.59E-03
 
 ETA5
+       ......... ......... ......... .........  5.14E-02
 
 ETA6
+       ......... ......... ......... .........  2.44E-01  4.74E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.23E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        4.97E-06
 
 TH 2
+        2.57E-07  5.37E-06
 
 OM11
+       -1.53E-08  1.82E-08  1.20E-07
 
 OM12
+        3.06E-08 -2.91E-09  7.72E-09  6.79E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.74E-08  3.03E-09  4.23E-10  7.83E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.45E-07
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+        6.14E-08  5.61E-08  1.85E-08  6.22E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.33E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.36E-06
 
 OM34
+        9.47E-09  3.01E-08 -7.62E-09 -2.02E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  7.91E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.83E-07  3.41E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        3.07E-08 -2.89E-09  1.60E-08  6.68E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -8.60E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.11E-07 -4.28E-08  0.00E+00  0.00E+00  7.33E-06
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+        2.03E-06 -1.44E-08  2.23E-07  1.71E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.47E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.78E-06  2.37E-06  0.00E+00  0.00E+00  4.95E-06  0.00E+00  0.00E+00  1.07E-03
 
 OM56
+       -5.54E-07  1.32E-07 -9.92E-08  1.35E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.34E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.58E-06  1.85E-06  0.00E+00  0.00E+00 -1.22E-06  0.00E+00  0.00E+00 -5.34E-04  6.06E-04
 
 OM66
+        1.14E-06  8.99E-07  5.70E-08 -1.26E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.24E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -5.45E-07 -5.25E-06  0.00E+00  0.00E+00  8.84E-07  0.00E+00  0.00E+00  1.73E-04 -1.94E-04  7.24E-04
 
 SG11
+        9.83E-09  1.17E-08 -2.67E-09 -9.30E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -3.32E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.24E-09  4.34E-10  0.00E+00  0.00E+00 -6.41E-10  0.00E+00  0.00E+00 -8.85E-08  2.05E-08  1.02E-08  1.53E-08
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.23E-03
 
 TH 2
+        4.97E-02  2.32E-03
 
 OM11
+       -1.98E-02  2.27E-02  3.46E-04
 
 OM12
+        5.27E-02 -4.82E-03  8.56E-02  2.60E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.05E-02  3.43E-03  3.21E-03  7.89E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.81E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+        1.19E-02  1.04E-02  2.31E-02  1.03E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.50E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.32E-03
 
 OM34
+        2.30E-03  7.04E-03 -1.19E-02 -4.20E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.12E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.97E-02  1.85E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        5.09E-03 -4.61E-04  1.71E-02  9.47E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -8.34E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.77E-02 -8.56E-03  0.00E+00  0.00E+00  2.71E-03
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+        2.78E-02 -1.90E-04  1.96E-02  2.01E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.98E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.62E-02  3.91E-02  0.00E+00  0.00E+00  5.57E-02  0.00E+00  0.00E+00  3.28E-02
 
 OM56
+       -1.01E-02  2.32E-03 -1.16E-02  2.11E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.56E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.04E-02  4.07E-02  0.00E+00  0.00E+00 -1.82E-02  0.00E+00  0.00E+00 -6.62E-01  2.46E-02
 
 OM66
+        1.90E-02  1.44E-02  6.11E-03 -1.80E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.14E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.75E-03 -1.06E-01  0.00E+00  0.00E+00  1.21E-02  0.00E+00  0.00E+00  1.96E-01 -2.93E-01  2.69E-02
 
 SG11
+        3.56E-02  4.09E-02 -6.23E-02 -2.88E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -7.03E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.13E-02  1.90E-03  0.00E+00  0.00E+00 -1.91E-03  0.00E+00  0.00E+00 -2.18E-02  6.73E-03  3.06E-03  1.24E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.03E+05
 
 TH 2
+       -9.55E+03  1.87E+05
 
 OM11
+        3.21E+04 -3.36E+04  8.45E+06
 
 OM12
+       -1.00E+05  1.47E+04 -9.57E+05  1.50E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.55E+04 -9.86E+03  5.65E+04 -8.13E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.99E+06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+       -1.96E+03 -1.99E+03 -2.70E+04 -1.02E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.65E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.90E+05
 
 OM34
+       -1.21E+03 -2.13E+03  1.03E+04  9.72E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.68E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.24E+04  3.03E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -5.39E+02  1.17E+02 -1.69E+04 -1.05E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  7.23E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.65E+03  2.82E+03  0.00E+00  0.00E+00  1.37E+05
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+       -4.89E+02 -3.16E+00 -1.45E+03 -3.70E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.79E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.09E+02 -2.12E+03  0.00E+00  0.00E+00 -8.98E+02  0.00E+00  0.00E+00  1.68E+03
 
 OM56
+       -3.18E+02 -1.52E+02 -1.03E+02 -6.53E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.75E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  9.91E+02 -1.93E+03  0.00E+00  0.00E+00 -5.20E+02  0.00E+00  0.00E+00  1.47E+03  3.11E+03
 
 OM66
+       -2.90E+02 -2.73E+02 -4.34E+02  2.19E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.12E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.52E+02  2.21E+03  0.00E+00  0.00E+00 -6.42E+01  0.00E+00  0.00E+00 -1.98E+01  4.71E+02  1.53E+03
 
 SG11
+       -1.20E+05 -1.43E+05  1.43E+06  6.07E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.47E+06  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.55E+04 -1.78E+04  0.00E+00  0.00E+00 -1.33E+03  0.00E+00  0.00E+00  8.35E+03  4.56E+03 -6.43E+02  6.60E+07
 
 #CPUT: Total CPU Time in Seconds,     1367.219
Stop Time: 
Sat 09/07/2013 
10:00 PM

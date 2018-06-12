Sat 09/07/2013 
10:00 PM
$PROB RUN# 
$INPUT C ID TIME DV AMT RATE EVID MDV CMT ROWNUM SID
$DATA superid3.csv IGNORE=C

$SUBROUTINES ADVAN2 TRANS2

$PK
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
KA=DEXP(MU_1+ETA(1)+ETA(4))
CL=DEXP(MU_2+ETA(2)+ETA(5))
V=DEXP(MU_3+ETA(3)+ETA(6))
S2=V

$ERROR
IPRE=F
Y = IPRE + IPRE*EPS(1)

; Initial values of THETA
$THETA 0.2 -4 -2
;INITIAL values of OMEGA
$OMEGA BLOCK(3)
0.1
0.001 0.1
0.001 0.001 0.1

$OMEGA BLOCK(3)
0.3
0.001 0.3
0.001 0.001 0.3

;Initial value of SIGMA
$SIGMA 
0.1     ;[P]

$LEVEL
SID=(4[1],5[2],6[3])


$EST METHOD=ITS INTERACTION PRINT=1 NSIG=2 NITER=500 SIGL=8 FNLETA=0 NOABORT CTYPE=3 MCETA=0
$EST METHOD=IMP INTERACTION PRINT=1 NSIG=2 NITER=500 SIGL=8 FNLETA=0 NOABORT CTYPE=3 MCETA=0 ISAMPLE=300 MAPITER=0
$EST METHOD=SAEM INTERACTION PRINT=10 NSIG=2 NITER=100 SIGL=8 FNLETA=0 NOABORT CTYPE=3 MCETA=0 ISAMPLE=2 CONSTRAIN=0
$EST METHOD=IMP EONLY=1 INTERACTION PRINT=1 NSIG=2 NITER=5 SIGL=8 FNLETA=0 NOABORT CTYPE=3 MCETA=0 ISAMPLE=300 MAPITER=0
$EST METHOD=BAYES INTERACTION PRINT=10 NSIG=2 NBURN=1000 NITER=500 SIGL=8 FNLETA=0 NOABORT CTYPE=3
$EST METHOD=1 INTERACTION PRINT=5 NSIG=2 NBURN=1000 NITER=500 SIGL=10 FNLETA=0 NOHABORT SLOW NONINFETA=1 MCETA=20
$COV MATRIX=R UNCONDITIONAL SIGL=10
$TABLE  ID TIME DV AMT RATE EVID MDV CMT SID KA CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 
NOAPPEND ONEHEADER FILE=superid3_1.tab  NOPRINT
$TABLE  ID SID KA CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 NOAPPEND ONEHEADER FILE=superid3_1.dat
        FIRSTONLY NOPRINT FORMAT=,1PE15.8
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 12) MU_001: SHOULD NOT BE ASSOCIATED WITH ETA(004)

 (MU_WARNING 11) MU_001: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_002: SHOULD NOT BE ASSOCIATED WITH ETA(005)

 (MU_WARNING 11) MU_002: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_003: SHOULD NOT BE ASSOCIATED WITH ETA(006)

 (MU_WARNING 11) MU_003: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.
  
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
 RUN#
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     8800
 NO. OF DATA ITEMS IN DATA SET:  11
 ID DATA ITEM IS DATA ITEM NO.:   2
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  8
0INDICES PASSED TO SUBROUTINE PRED:
   7   3   5   6   0   0   9   0   0   0   0
0LABELS FOR DATA ITEMS:
 C ID TIME DV AMT RATE EVID MDV CMT ROWNUM SID
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA CL V
0FORMAT FOR DATA:
 (7E10.0/4E10.0)

 TOT. NO. OF OBS RECS:     8000
 TOT. NO. OF INDIVIDUALS:    800
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
  0  0  0  2
  0  0  0  2  2
  0  0  0  2  2  2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.2000E+00 -0.4000E+01 -0.2000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.1000E+00
                  0.1000E-02   0.1000E+00
                  0.1000E-02   0.1000E-02   0.1000E+00
        2                                                                                   NO
                  0.3000E+00
                  0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.3000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                10
 RELATIVE TOLERANCE (TOL):                  -1
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY (ATOL): -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 RESUME COV ANALYSIS (RESUME):               NO
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           2
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
 ID TIME DV AMT RATE EVID MDV CMT SID KA CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADERS:               ONE
 FILE TO BE FORWARDED:   NO
 FORMAT:                ,1PE15.8
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID SID KA CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(N)

 ONE COMPARTMENT MODEL WITH FIRST-ORDER ABSORPTION (ADVAN2)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   3
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1
   ABSORPTION RATE (KA) IS BASIC PK PARAMETER NO.:  3

 TRANSLATOR WILL CONVERT PARAMETERS
 CLEARANCE (CL) AND VOLUME (V) TO K (TRANS2)
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         DEPOT        OFF        YES        YES        YES        NO
    2         CENTRAL      ON         NO         YES        NO         YES
    3         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            4           *           *           *           *
    3            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      7
   TIME DATA ITEM IS DATA ITEM NO.:          3
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   5
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     6
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    9

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 
 
 #TBLN:      1
 #METH: Iterative Two Stage
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            624
 NO. OF SIG. FIGURES REQUIRED:            2
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
 FINAL ETA RE-EVALUATION (FNLETA):        OFF
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 EM OR BAYESIAN METHOD USED:              ITERATIVE TWO STAGE (ITS)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      500         
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   11380.8005496164
 iteration            1 OBJ=  -3371.53177892496
 iteration            2 OBJ=  -6542.71405398850
 iteration            3 OBJ=  -8624.39302809895
 iteration            4 OBJ=  -10459.7649237779
 iteration            5 OBJ=  -12195.6066317707
 iteration            6 OBJ=  -13856.5917235061
 iteration            7 OBJ=  -15404.6942302480
 iteration            8 OBJ=  -16657.4704262491
 iteration            9 OBJ=  -17072.0926699669
 iteration           10 OBJ=  -17076.8951780335
 iteration           11 OBJ=  -17077.3295819753
 iteration           12 OBJ=  -17077.4606026358
 iteration           13 OBJ=  -17077.4774869389
 iteration           14 OBJ=  -17077.4549791622
 iteration           15 OBJ=  -17077.4233286520
 iteration           16 OBJ=  -17077.3937662334
 iteration           17 OBJ=  -17077.3696054890
 iteration           18 OBJ=  -17077.3510672396
 iteration           19 OBJ=  -17077.3373292030
 iteration           20 OBJ=  -17077.3273578339
 iteration           21 OBJ=  -17077.3202140485
 iteration           22 OBJ=  -17077.3151390211
 iteration           23 OBJ=  -17077.3115534821
 iteration           24 OBJ=  -17077.3090295308
 iteration           25 OBJ=  -17077.3072571645
 iteration           26 OBJ=  -17077.3060145627
 iteration           27 OBJ=  -17077.3051443357
 iteration           28 OBJ=  -17077.3045352339
 iteration           29 OBJ=  -17077.3041091340
 iteration           30 OBJ=  -17077.3038110980
 iteration           31 OBJ=  -17077.3036026536
 iteration           32 OBJ=  -17077.3034569986
 Convergence achieved
 iteration           32 OBJ=  -17077.3034569501
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         3.0346E-10 -5.2824E-07 -5.0781E-07 -2.7874E-16 -2.4980E-17  2.7894E-17
 SE:             2.2545E-03  3.3949E-03  3.3592E-03  4.2798E-02  3.9229E-02  5.5272E-02
 N:                     800         800         800          16          16          16
 
 P VAL.:         1.0000E+00  9.9988E-01  9.9988E-01  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   3.5865E+01  2.0427E+00  2.6911E+00  3.3154E-05  6.2985E-06  7.7697E-06
 EBVshrink(%):   3.5864E+01  2.0426E+00  2.6910E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.2308E+01
 
 #TERE:
 Elapsed estimation time in seconds:   158.18
 Elapsed covariance time in seconds:     3.06
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -17077.303       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.79E-01 -5.31E+00 -3.08E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.01E-02
 
 ETA2
+        1.49E-04  9.80E-03
 
 ETA3
+        5.39E-04  6.58E-04  9.73E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  3.13E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -5.61E-03  2.63E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  2.73E-02 -6.28E-03  5.21E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.00E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.00E-01
 
 ETA2
+        1.50E-02  9.90E-02
 
 ETA3
+        5.44E-02  6.74E-02  9.86E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.77E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -1.96E-01  1.62E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  6.76E-01 -1.70E-01  2.28E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.48E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         5.72E-03  3.67E-03  3.64E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.16E-04
 
 ETA2
+        5.44E-04  5.23E-04
 
 ETA3
+        6.90E-04  3.85E-04  5.27E-04
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.16E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  1.01E-02  1.53E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  1.64E-02  1.20E-02  2.90E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.56E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        4.56E-03
 
 ETA2
+        5.46E-02  2.64E-03
 
 ETA3
+        6.92E-02  3.89E-02  2.67E-03
 
 ETA4
+       ......... ......... .........  3.28E-02
 
 ETA5
+       ......... ......... .........  3.24E-01  4.73E-02
 
 ETA6
+       ......... ......... .........  1.79E-01  2.80E-01  6.34E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.08E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.27E-05
 
 TH 2
+        1.15E-06  1.35E-05
 
 TH 3
+        1.86E-06  1.44E-06  1.33E-05
 
 OM11
+       -3.52E-07  1.19E-07  2.04E-07  8.40E-07
 
 OM12
+        1.62E-07 -1.95E-08  2.84E-08  3.28E-08  2.96E-07
 
 OM13
+        7.16E-07  5.13E-08  5.66E-08  6.81E-08  5.92E-08  4.76E-07
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.80E-10  2.75E-07 -3.40E-08  6.75E-09  1.78E-08  1.29E-08  0.00E+00  0.00E+00  0.00E+00  2.74E-07
 
 OM23
+        4.28E-08 -3.89E-08  7.12E-08  1.21E-08  2.65E-08  2.78E-08  0.00E+00  0.00E+00  0.00E+00  4.04E-08  1.48E-07
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        8.28E-08  8.75E-08  5.78E-09  3.91E-08  1.31E-08  7.33E-08  0.00E+00  0.00E+00  0.00E+00  1.52E-08  2.58E-08  0.00E+00
          0.00E+00  0.00E+00  2.77E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        2.38E-06  6.64E-07  3.60E-07 -6.82E-07  1.70E-07  2.08E-07  0.00E+00  0.00E+00  0.00E+00  1.43E-07 -1.69E-07  0.00E+00
          0.00E+00  0.00E+00 -9.48E-08  0.00E+00  0.00E+00  0.00E+00  1.35E-04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        5.83E-07  5.74E-07  4.01E-07  2.93E-07 -8.03E-08  3.23E-07  0.00E+00  0.00E+00  0.00E+00  7.28E-08 -9.91E-08  0.00E+00
          0.00E+00  0.00E+00  1.50E-07  0.00E+00  0.00E+00  0.00E+00 -1.81E-05  1.03E-04
 
 OM46
+        1.66E-06  1.23E-06  7.45E-07 -4.77E-08  3.70E-08 -3.37E-08  0.00E+00  0.00E+00  0.00E+00  4.01E-07 -1.56E-07  0.00E+00
          0.00E+00  0.00E+00 -3.50E-07  0.00E+00  0.00E+00  0.00E+00  1.34E-04 -5.27E-05  2.68E-04
 
 OM55
+        3.58E-08 -3.59E-07  1.86E-08 -2.78E-07  1.49E-07 -7.20E-07  0.00E+00  0.00E+00  0.00E+00 -7.08E-08  2.43E-07  0.00E+00
          0.00E+00  0.00E+00  4.67E-08  0.00E+00  0.00E+00  0.00E+00  3.04E-05 -8.20E-05  2.86E-05  2.35E-04
 
 OM56
+        1.03E-06  3.53E-07  1.00E-07 -1.19E-07 -4.92E-08  8.43E-08  0.00E+00  0.00E+00  0.00E+00  8.08E-08 -1.75E-07  0.00E+00
          0.00E+00  0.00E+00 -1.33E-08  0.00E+00  0.00E+00  0.00E+00 -3.90E-05  9.46E-05 -1.03E-04 -7.81E-05  1.44E-04
 
 OM66
+       -3.55E-07  1.47E-06  6.13E-07  2.93E-07 -4.57E-07 -4.31E-07  0.00E+00  0.00E+00  0.00E+00  5.97E-07 -1.91E-07  0.00E+00
          0.00E+00  0.00E+00 -4.46E-07  0.00E+00  0.00E+00  0.00E+00  1.77E-04 -9.08E-05  4.26E-04  2.15E-05 -2.01E-04  8.40E-04
 
 SG11
+        3.80E-09 -5.28E-10  3.08E-09 -3.27E-09 -4.02E-10 -2.65E-09  0.00E+00  0.00E+00  0.00E+00 -1.59E-09 -1.41E-09  0.00E+00
          0.00E+00  0.00E+00 -2.14E-09  0.00E+00  0.00E+00  0.00E+00  3.11E-08 -5.35E-09  2.25E-08  4.79E-08  1.78E-09 -1.18E-08
         3.09E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        5.72E-03
 
 TH 2
+        5.46E-02  3.67E-03
 
 TH 3
+        8.93E-02  1.08E-01  3.64E-03
 
 OM11
+       -6.71E-02  3.53E-02  6.12E-02  9.16E-04
 
 OM12
+        5.21E-02 -9.75E-03  1.43E-02  6.58E-02  5.44E-04
 
 OM13
+        1.81E-01  2.02E-02  2.25E-02  1.08E-01  1.58E-01  6.90E-04
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.60E-04  1.43E-01 -1.79E-02  1.41E-02  6.26E-02  3.58E-02  0.00E+00  0.00E+00  0.00E+00  5.23E-04
 
 OM23
+        1.95E-02 -2.75E-02  5.07E-02  3.43E-02  1.27E-01  1.05E-01  0.00E+00  0.00E+00  0.00E+00  2.01E-01  3.85E-04
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        2.75E-02  4.53E-02  3.01E-03  8.10E-02  4.56E-02  2.02E-01  0.00E+00  0.00E+00  0.00E+00  5.52E-02  1.27E-01  0.00E+00
          0.00E+00  0.00E+00  5.27E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        3.58E-02  1.56E-02  8.50E-03 -6.41E-02  2.69E-02  2.59E-02  0.00E+00  0.00E+00  0.00E+00  2.36E-02 -3.79E-02  0.00E+00
          0.00E+00  0.00E+00 -1.55E-02  0.00E+00  0.00E+00  0.00E+00  1.16E-02
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        1.01E-02  1.54E-02  1.09E-02  3.16E-02 -1.46E-02  4.62E-02  0.00E+00  0.00E+00  0.00E+00  1.37E-02 -2.54E-02  0.00E+00
          0.00E+00  0.00E+00  2.81E-02  0.00E+00  0.00E+00  0.00E+00 -1.53E-01  1.01E-02
 
 OM46
+        1.77E-02  2.05E-02  1.25E-02 -3.18E-03  4.15E-03 -2.99E-03  0.00E+00  0.00E+00  0.00E+00  4.69E-02 -2.47E-02  0.00E+00
          0.00E+00  0.00E+00 -4.06E-02  0.00E+00  0.00E+00  0.00E+00  7.04E-01 -3.18E-01  1.64E-02
 
 OM55
+        4.08E-04 -6.37E-03  3.32E-04 -1.97E-02  1.79E-02 -6.80E-02  0.00E+00  0.00E+00  0.00E+00 -8.83E-03  4.12E-02  0.00E+00
          0.00E+00  0.00E+00  5.78E-03  0.00E+00  0.00E+00  0.00E+00  1.71E-01 -5.28E-01  1.14E-01  1.53E-02
 
 OM56
+        1.50E-02  8.03E-03  2.30E-03 -1.08E-02 -7.54E-03  1.02E-02  0.00E+00  0.00E+00  0.00E+00  1.29E-02 -3.79E-02  0.00E+00
          0.00E+00  0.00E+00 -2.11E-03  0.00E+00  0.00E+00  0.00E+00 -2.80E-01  7.79E-01 -5.26E-01 -4.25E-01  1.20E-02
 
 OM66
+       -2.14E-03  1.38E-02  5.80E-03  1.11E-02 -2.90E-02 -2.16E-02  0.00E+00  0.00E+00  0.00E+00  3.94E-02 -1.72E-02  0.00E+00
          0.00E+00  0.00E+00 -2.92E-02  0.00E+00  0.00E+00  0.00E+00  5.27E-01 -3.09E-01  8.97E-01  4.85E-02 -5.79E-01  2.90E-02
 
 SG11
+        1.19E-02 -2.59E-03  1.52E-02 -6.42E-02 -1.33E-02 -6.90E-02  0.00E+00  0.00E+00  0.00E+00 -5.45E-02 -6.61E-02  0.00E+00
          0.00E+00  0.00E+00 -7.32E-02  0.00E+00  0.00E+00  0.00E+00  4.81E-02 -9.49E-03  2.47E-02  5.61E-02  2.66E-03 -7.34E-03
         5.56E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.23E+04
 
 TH 2
+       -2.35E+03  7.76E+04
 
 TH 3
+       -4.27E+03 -8.41E+03  7.75E+04
 
 OM11
+        1.86E+04 -9.19E+03 -1.97E+04  1.25E+06
 
 OM12
+       -9.35E+03  1.01E+04 -1.40E+03 -1.13E+05  3.55E+06
 
 OM13
+       -4.99E+04 -7.09E+02  1.88E+03 -1.67E+05 -3.70E+05  2.38E+06
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.84E+03 -8.53E+04  2.50E+04  7.43E+03 -1.48E+05 -1.08E+04  0.00E+00  0.00E+00  0.00E+00  3.95E+06
 
 OM23
+       -1.15E+03  5.08E+04 -4.61E+04 -8.58E+03 -5.11E+05 -2.56E+05  0.00E+00  0.00E+00  0.00E+00 -1.08E+06  7.38E+06
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        1.07E+03 -2.30E+04  6.69E+03 -1.17E+05 -1.93E+03 -5.31E+05  0.00E+00  0.00E+00  0.00E+00 -8.86E+04 -5.16E+05  0.00E+00
          0.00E+00  0.00E+00  3.86E+06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+       -1.21E+02  6.33E+01  7.12E+01  1.32E+04 -3.83E+03 -7.70E+03  0.00E+00  0.00E+00  0.00E+00  3.79E+03  1.10E+04  0.00E+00
          0.00E+00  0.00E+00 -6.13E+03  0.00E+00  0.00E+00  0.00E+00  1.74E+04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        1.78E+02 -4.03E+02 -4.82E+02 -1.14E+04  3.12E+03 -8.75E+03  0.00E+00  0.00E+00  0.00E+00  3.79E+03 -9.48E+03  0.00E+00
          0.00E+00  0.00E+00 -1.45E+04  0.00E+00  0.00E+00  0.00E+00 -2.67E+03  3.02E+04
 
 OM46
+       -5.95E+02 -5.64E+02 -6.19E+02 -7.19E+03 -1.37E+04 -5.26E+03  0.00E+00  0.00E+00  0.00E+00 -1.13E+04  4.00E+03  0.00E+00
          0.00E+00  0.00E+00  1.44E+04  0.00E+00  0.00E+00  0.00E+00 -1.43E+04  4.19E+03  3.19E+04
 
 OM55
+       -1.88E+02 -5.78E+01 -6.97E+01 -1.70E+03  1.83E+02  9.79E+03  0.00E+00  0.00E+00  0.00E+00 -8.12E+02 -7.77E+03  0.00E+00
          0.00E+00  0.00E+00 -4.13E+03  0.00E+00  0.00E+00  0.00E+00 -1.80E+03  4.05E+03 -1.26E+02  6.49E+03
 
 OM56
+       -6.03E+02  2.55E+01  3.74E+01  7.00E+03  2.65E+03  1.34E+04  0.00E+00  0.00E+00  0.00E+00 -1.54E+04  1.88E+04  0.00E+00
          0.00E+00  0.00E+00  1.22E+04  0.00E+00  0.00E+00  0.00E+00 -1.60E+02 -1.98E+04 -2.29E+03  1.91E+03  2.71E+04
 
 OM66
+        1.87E+02  1.74E+02  1.95E+02  7.50E+02  1.05E+04  7.05E+03  0.00E+00  0.00E+00  0.00E+00 -1.37E+03  1.07E+03  0.00E+00
          0.00E+00  0.00E+00 -2.72E+03  0.00E+00  0.00E+00  0.00E+00  3.26E+03 -3.16E+03 -1.32E+04  1.18E+03  5.53E+03  8.18E+03
 
 SG11
+       -4.75E+04 -1.66E+04 -9.15E+04  1.00E+06 -9.36E+04  1.35E+06  0.00E+00  0.00E+00  0.00E+00  1.47E+06  2.18E+06  0.00E+00
          0.00E+00  0.00E+00  1.78E+06  0.00E+00  0.00E+00  0.00E+00 -2.53E+04 -4.61E+04 -1.36E+05 -7.08E+04 -1.09E+04  7.37E+04
         3.31E+08
 
1
 
 
 #TBLN:      2
 #METH: Importance Sampling
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            624
 NO. OF SIG. FIGURES REQUIRED:            2
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
 FINAL ETA RE-EVALUATION (FNLETA):        OFF
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
 ITERATIONS (NITER):                      500         
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        300         
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
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
   1   2   3
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -17081.1003716554 eff.=     301. Smpl.=     300. Fit.= 0.98953
 iteration            1 OBJ=  -17080.3224311101 eff.=     122. Smpl.=     300. Fit.= 0.85714
 iteration            2 OBJ=  -17083.6896061662 eff.=     120. Smpl.=     300. Fit.= 0.85420
 iteration            3 OBJ=  -17079.2003306038 eff.=     120. Smpl.=     300. Fit.= 0.85549
 iteration            4 OBJ=  -17080.9634360613 eff.=     121. Smpl.=     300. Fit.= 0.85529
 iteration            5 OBJ=  -17075.8084931533 eff.=     120. Smpl.=     300. Fit.= 0.85510
 iteration            6 OBJ=  -17079.7874286551 eff.=     120. Smpl.=     300. Fit.= 0.85564
 iteration            7 OBJ=  -17078.9639734533 eff.=     121. Smpl.=     300. Fit.= 0.85583
 iteration            8 OBJ=  -17079.1123472032 eff.=     121. Smpl.=     300. Fit.= 0.85551
 iteration            9 OBJ=  -17078.3904482328 eff.=     120. Smpl.=     300. Fit.= 0.85512
 iteration           10 OBJ=  -17080.3116110011 eff.=     120. Smpl.=     300. Fit.= 0.85541
 iteration           11 OBJ=  -17078.7896806515 eff.=     120. Smpl.=     300. Fit.= 0.85523
 Convergence achieved
 iteration           11 OBJ=  -17079.6812632801 eff.=     121. Smpl.=     300. Fit.= 0.85506
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         2.7163E-04 -6.1665E-05 -6.3243E-05 -6.1617E-17  1.3042E-16  7.1262E-17
 SE:             2.2622E-03  3.3936E-03  3.3582E-03  4.2839E-02  3.9230E-02  5.5249E-02
 N:                     800         800         800          16          16          16
 
 P VAL.:         9.0442E-01  9.8550E-01  9.8497E-01  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   3.5841E+01  2.0678E+00  2.7605E+00  1.0000E-10  1.0000E-10  3.5942E-02
 EBVshrink(%):   3.5952E+01  2.0481E+00  2.6983E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.2393E+01
 
 #TERE:
 Elapsed estimation time in seconds:   336.37
 Elapsed covariance time in seconds:    28.98
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -17079.681       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.80E-01 -5.31E+00 -3.08E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.01E-02
 
 ETA2
+        1.56E-04  9.80E-03
 
 ETA3
+        5.13E-04  6.52E-04  9.74E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  3.12E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -5.60E-03  2.63E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  2.73E-02 -6.32E-03  5.21E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.00E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.01E-01
 
 ETA2
+        1.57E-02  9.90E-02
 
 ETA3
+        5.16E-02  6.68E-02  9.87E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.77E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -1.96E-01  1.62E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  6.76E-01 -1.71E-01  2.28E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.48E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         5.57E-03  3.58E-03  3.60E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        8.87E-04
 
 ETA2
+        5.43E-04  5.15E-04
 
 ETA3
+        6.11E-04  3.67E-04  5.18E-04
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.16E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  1.01E-02  1.53E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  1.64E-02  1.20E-02  2.90E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.51E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        4.40E-03
 
 ETA2
+        5.43E-02  2.60E-03
 
 ETA3
+        6.09E-02  3.71E-02  2.62E-03
 
 ETA4
+       ......... ......... .........  3.28E-02
 
 ETA5
+       ......... ......... .........  3.24E-01  4.73E-02
 
 ETA6
+       ......... ......... .........  1.79E-01  2.80E-01  6.35E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.03E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.11E-05
 
 TH 2
+        1.15E-06  1.28E-05
 
 TH 3
+        1.80E-06  1.46E-06  1.29E-05
 
 OM11
+       -1.43E-08 -1.85E-09 -9.00E-10  7.86E-07
 
 OM12
+       -4.98E-08 -7.91E-09 -2.77E-09  5.66E-08  2.95E-07
 
 OM13
+        1.04E-07  6.37E-10 -5.57E-09  9.91E-08  4.15E-08  3.73E-07
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.07E-09  1.10E-08  4.05E-09  2.76E-09  2.36E-08  2.37E-09  0.00E+00  0.00E+00  0.00E+00  2.65E-07
 
 OM23
+       -4.90E-09 -6.51E-10  4.01E-10  4.77E-09  1.89E-08  1.32E-08  0.00E+00  0.00E+00  0.00E+00  2.91E-08  1.35E-07
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        2.30E-09 -4.52E-10  1.40E-09  5.68E-09  3.59E-09  3.48E-08  0.00E+00  0.00E+00  0.00E+00  2.05E-09  3.08E-08  0.00E+00
          0.00E+00  0.00E+00  2.68E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        1.75E-06  5.65E-07  6.32E-07 -5.72E-07  1.37E-07  4.86E-08  0.00E+00  0.00E+00  0.00E+00  1.61E-07 -1.71E-07  0.00E+00
          0.00E+00  0.00E+00 -1.18E-07  0.00E+00  0.00E+00  0.00E+00  1.35E-04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        5.11E-07  3.22E-07  3.82E-07  2.80E-07 -6.17E-08  2.77E-07  0.00E+00  0.00E+00  0.00E+00  6.14E-08 -9.79E-08  0.00E+00
          0.00E+00  0.00E+00  1.06E-07  0.00E+00  0.00E+00  0.00E+00 -1.77E-05  1.02E-04
 
 OM46
+        1.78E-06  8.60E-07  9.22E-07  1.21E-08  4.70E-08 -8.09E-08  0.00E+00  0.00E+00  0.00E+00  4.13E-07 -1.72E-07  0.00E+00
          0.00E+00  0.00E+00 -3.85E-07  0.00E+00  0.00E+00  0.00E+00  1.34E-04 -5.26E-05  2.68E-04
 
 OM55
+        7.11E-07 -3.99E-08 -8.99E-09 -2.95E-07  1.19E-07 -5.69E-07  0.00E+00  0.00E+00  0.00E+00 -6.45E-08  2.57E-07  0.00E+00
          0.00E+00  0.00E+00  1.48E-07  0.00E+00  0.00E+00  0.00E+00  3.02E-05 -8.13E-05  2.85E-05  2.35E-04
 
 OM56
+        1.02E-06  1.64E-07  2.21E-07 -1.03E-07 -2.61E-08  7.76E-08  0.00E+00  0.00E+00  0.00E+00  9.14E-08 -1.64E-07  0.00E+00
          0.00E+00  0.00E+00 -2.96E-08  0.00E+00  0.00E+00  0.00E+00 -3.90E-05  9.44E-05 -1.03E-04 -7.80E-05  1.44E-04
 
 OM66
+        7.63E-07  8.53E-07  8.57E-07  2.43E-07 -4.06E-07 -3.68E-07  0.00E+00  0.00E+00  0.00E+00  5.85E-07 -2.09E-07  0.00E+00
          0.00E+00  0.00E+00 -4.80E-07  0.00E+00  0.00E+00  0.00E+00  1.79E-04 -9.06E-05  4.26E-04  2.16E-05 -2.01E-04  8.40E-04
 
 SG11
+        1.87E-09  2.47E-09  2.78E-09 -2.49E-09 -6.34E-10 -9.37E-10  0.00E+00  0.00E+00  0.00E+00 -4.63E-10 -5.27E-10  0.00E+00
          0.00E+00  0.00E+00 -6.16E-10  0.00E+00  0.00E+00  0.00E+00  3.05E-08  3.75E-10  2.23E-08  4.28E-08  4.46E-09 -1.17E-08
         3.03E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        5.57E-03
 
 TH 2
+        5.77E-02  3.58E-03
 
 TH 3
+        8.99E-02  1.13E-01  3.60E-03
 
 OM11
+       -2.89E-03 -5.83E-04 -2.82E-04  8.87E-04
 
 OM12
+       -1.65E-02 -4.07E-03 -1.42E-03  1.18E-01  5.43E-04
 
 OM13
+        3.06E-02  2.91E-04 -2.54E-03  1.83E-01  1.25E-01  6.11E-04
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.12E-03  5.99E-03  2.19E-03  6.04E-03  8.44E-02  7.53E-03  0.00E+00  0.00E+00  0.00E+00  5.15E-04
 
 OM23
+       -2.39E-03 -4.95E-04  3.04E-04  1.46E-02  9.50E-02  5.88E-02  0.00E+00  0.00E+00  0.00E+00  1.54E-01  3.67E-04
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        7.97E-04 -2.44E-04  7.52E-04  1.24E-02  1.28E-02  1.10E-01  0.00E+00  0.00E+00  0.00E+00  7.68E-03  1.62E-01  0.00E+00
          0.00E+00  0.00E+00  5.18E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        2.71E-02  1.36E-02  1.52E-02 -5.56E-02  2.18E-02  6.87E-03  0.00E+00  0.00E+00  0.00E+00  2.70E-02 -4.00E-02  0.00E+00
          0.00E+00  0.00E+00 -1.96E-02  0.00E+00  0.00E+00  0.00E+00  1.16E-02
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        9.07E-03  8.88E-03  1.05E-02  3.13E-02 -1.12E-02  4.48E-02  0.00E+00  0.00E+00  0.00E+00  1.18E-02 -2.64E-02  0.00E+00
          0.00E+00  0.00E+00  2.02E-02  0.00E+00  0.00E+00  0.00E+00 -1.51E-01  1.01E-02
 
 OM46
+        1.95E-02  1.47E-02  1.57E-02  8.35E-04  5.28E-03 -8.09E-03  0.00E+00  0.00E+00  0.00E+00  4.90E-02 -2.85E-02  0.00E+00
          0.00E+00  0.00E+00 -4.54E-02  0.00E+00  0.00E+00  0.00E+00  7.06E-01 -3.17E-01  1.64E-02
 
 OM55
+        8.33E-03 -7.27E-04 -1.63E-04 -2.17E-02  1.43E-02 -6.09E-02  0.00E+00  0.00E+00  0.00E+00 -8.18E-03  4.56E-02  0.00E+00
          0.00E+00  0.00E+00  1.86E-02  0.00E+00  0.00E+00  0.00E+00  1.70E-01 -5.25E-01  1.14E-01  1.53E-02
 
 OM56
+        1.53E-02  3.82E-03  5.13E-03 -9.66E-03 -4.01E-03  1.06E-02  0.00E+00  0.00E+00  0.00E+00  1.48E-02 -3.72E-02  0.00E+00
          0.00E+00  0.00E+00 -4.77E-03  0.00E+00  0.00E+00  0.00E+00 -2.80E-01  7.79E-01 -5.25E-01 -4.25E-01  1.20E-02
 
 OM66
+        4.72E-03  8.22E-03  8.22E-03  9.46E-03 -2.58E-02 -2.08E-02  0.00E+00  0.00E+00  0.00E+00  3.92E-02 -1.96E-02  0.00E+00
          0.00E+00  0.00E+00 -3.20E-02  0.00E+00  0.00E+00  0.00E+00  5.31E-01 -3.09E-01  8.98E-01  4.86E-02 -5.79E-01  2.90E-02
 
 SG11
+        6.10E-03  1.25E-02  1.40E-02 -5.10E-02 -2.12E-02 -2.79E-02  0.00E+00  0.00E+00  0.00E+00 -1.63E-02 -2.61E-02  0.00E+00
          0.00E+00  0.00E+00 -2.16E-02  0.00E+00  0.00E+00  0.00E+00  4.77E-02  6.74E-04  2.47E-02  5.07E-02  6.75E-03 -7.33E-03
         5.51E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.26E+04
 
 TH 2
+       -2.40E+03  7.92E+04
 
 TH 3
+       -4.23E+03 -8.55E+03  7.90E+04
 
 OM11
+        1.01E+03 -8.61E+01 -3.68E+02  1.35E+06
 
 OM12
+        7.19E+03  1.97E+03 -2.79E+02 -2.17E+05  3.55E+06
 
 OM13
+       -1.05E+04  3.77E+02  2.79E+03 -3.32E+05 -3.20E+05  2.87E+06
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -3.96E+02 -2.84E+03 -1.22E+02  8.32E+03 -2.65E+05  2.43E+04  0.00E+00  0.00E+00  0.00E+00  3.91E+06
 
 OM23
+        2.21E+02 -5.32E+01 -1.21E+03  2.49E+04 -4.12E+05 -1.62E+05  0.00E+00  0.00E+00  0.00E+00 -8.44E+05  7.91E+06
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        3.32E+02 -2.45E+02 -1.02E+03  1.98E+04  4.49E+04 -3.39E+05  0.00E+00  0.00E+00  0.00E+00  4.86E+04 -8.51E+05  0.00E+00
          0.00E+00  0.00E+00  3.90E+06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+       -1.38E+02  4.87E+01  4.97E+01  1.32E+04 -3.93E+03 -5.74E+03  0.00E+00  0.00E+00  0.00E+00  3.71E+03  1.13E+04  0.00E+00
          0.00E+00  0.00E+00 -5.77E+03  0.00E+00  0.00E+00  0.00E+00  1.75E+04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        1.27E+02 -3.99E+02 -4.78E+02 -1.21E+04  3.11E+03 -9.81E+03  0.00E+00  0.00E+00  0.00E+00  3.82E+03 -9.54E+03  0.00E+00
          0.00E+00  0.00E+00 -1.47E+04  0.00E+00  0.00E+00  0.00E+00 -2.87E+03  3.04E+04
 
 OM46
+       -6.26E+02 -5.56E+02 -6.12E+02 -7.69E+03 -1.38E+04 -5.35E+03  0.00E+00  0.00E+00  0.00E+00 -1.13E+04  3.68E+03  0.00E+00
          0.00E+00  0.00E+00  1.42E+04  0.00E+00  0.00E+00  0.00E+00 -1.43E+04  4.50E+03  3.21E+04
 
 OM55
+       -2.18E+02 -5.47E+01 -7.14E+01 -1.72E+03  3.64E+02  8.85E+03  0.00E+00  0.00E+00  0.00E+00 -6.58E+02 -7.73E+03  0.00E+00
          0.00E+00  0.00E+00 -4.19E+03  0.00E+00  0.00E+00  0.00E+00 -1.81E+03  4.02E+03 -1.23E+02  6.49E+03
 
 OM56
+       -6.37E+02  2.76E+01  3.24E+01  7.64E+03  2.07E+03  1.33E+04  0.00E+00  0.00E+00  0.00E+00 -1.53E+04  1.88E+04  0.00E+00
          0.00E+00  0.00E+00  1.23E+04  0.00E+00  0.00E+00  0.00E+00 -2.00E+01 -1.99E+04 -2.59E+03  1.94E+03  2.72E+04
 
 OM66
+        1.90E+02  1.70E+02  1.90E+02  1.06E+03  1.04E+04  6.80E+03  0.00E+00  0.00E+00  0.00E+00 -1.29E+03  1.21E+03  0.00E+00
          0.00E+00  0.00E+00 -2.67E+03  0.00E+00  0.00E+00  0.00E+00  3.29E+03 -3.29E+03 -1.33E+04  1.18E+03  5.66E+03  8.24E+03
 
 SG11
+       -4.46E+03 -5.01E+04 -5.69E+04  9.10E+05  5.28E+05  4.38E+05  0.00E+00  0.00E+00  0.00E+00  4.93E+05  9.04E+05  0.00E+00
          0.00E+00  0.00E+00  5.59E+05  0.00E+00  0.00E+00  0.00E+00 -2.21E+04 -6.37E+04 -1.45E+05 -7.18E+04 -9.22E+03  7.63E+04
         3.34E+08
 
1
 
 
 #TBLN:      3
 #METH: Stochastic Approximation Expectation-Maximization
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            624
 NO. OF SIG. FIGURES REQUIRED:            2
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
 FINAL ETA RE-EVALUATION (FNLETA):        OFF
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
 CONVERGENCE INTERVAL (CINTERVAL):        10          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              1000        
 ITERATIONS (NITER):                      100         
 ANEAL SETTING (CONSTRAIN):               0           
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
   1   2   3
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration        -1000 SAEMOBJ=  -37217.8014280751
 iteration         -990 SAEMOBJ=  -36102.8373855624
 iteration         -980 SAEMOBJ=  -35925.3493598892
 iteration         -970 SAEMOBJ=  -35828.0861475209
 iteration         -960 SAEMOBJ=  -35816.8963251559
 iteration         -950 SAEMOBJ=  -35749.1037247343
 iteration         -940 SAEMOBJ=  -35673.1674823337
 iteration         -930 SAEMOBJ=  -35655.2917653864
 iteration         -920 SAEMOBJ=  -35714.0254958673
 iteration         -910 SAEMOBJ=  -35764.6025995903
 iteration         -900 SAEMOBJ=  -35697.7235401163
 iteration         -890 SAEMOBJ=  -35735.7777424962
 iteration         -880 SAEMOBJ=  -35807.8842425475
 iteration         -870 SAEMOBJ=  -35841.9308738793
 iteration         -860 SAEMOBJ=  -35809.8512055789
 iteration         -850 SAEMOBJ=  -35719.2029482770
 iteration         -840 SAEMOBJ=  -35833.5584406704
 iteration         -830 SAEMOBJ=  -35766.7449009443
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -35769.1170012858
 iteration           10 SAEMOBJ=  -35968.2950897214
 iteration           20 SAEMOBJ=  -35992.9654133284
 iteration           30 SAEMOBJ=  -36004.0080680283
 iteration           40 SAEMOBJ=  -36013.2638614167
 iteration           50 SAEMOBJ=  -36019.5333687371
 iteration           60 SAEMOBJ=  -36019.4705499337
 iteration           70 SAEMOBJ=  -36021.8484464755
 iteration           80 SAEMOBJ=  -36030.0542420599
 iteration           90 SAEMOBJ=  -36035.0552511597
 iteration          100 SAEMOBJ=  -36042.1383102471
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.5829E-05  3.1944E-06  4.5786E-06 -2.1419E-16  1.2528E-16 -1.1671E-16
 SE:             2.4989E-03  3.4066E-03  3.3682E-03  4.3097E-02  3.9288E-02  5.5352E-02
 N:                     800         800         800          16          16          16
 
 P VAL.:         9.9495E-01  9.9925E-01  9.9892E-01  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   3.0881E+01  1.8104E+00  2.3886E+00  1.3560E-02  1.0000E-10  4.0477E-04
 EBVshrink(%):   3.0847E+01  1.8036E+00  2.3811E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.1803E+01
 
 #TERE:
 Elapsed estimation time in seconds:   753.21
 Elapsed covariance time in seconds:     0.36
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -36042.138       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.79E-01 -5.31E+00 -3.08E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.07E-02
 
 ETA2
+        3.54E-04  9.83E-03
 
 ETA3
+        3.96E-04  6.65E-04  9.72E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  3.17E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -5.56E-03  2.63E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  2.75E-02 -6.22E-03  5.23E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.99E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.03E-01
 
 ETA2
+        3.46E-02  9.91E-02
 
 ETA3
+        3.89E-02  6.80E-02  9.86E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.78E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -1.92E-01  1.62E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  6.75E-01 -1.67E-01  2.29E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.47E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         5.45E-03  3.66E-03  3.62E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        8.94E-04
 
 ETA2
+        5.11E-04  5.20E-04
 
 ETA3
+        6.19E-04  3.79E-04  5.16E-04
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.18E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  1.03E-02  1.54E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  1.65E-02  1.20E-02  2.90E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.50E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        4.33E-03
 
 ETA2
+        4.98E-02  2.62E-03
 
 ETA3
+        6.05E-02  3.83E-02  2.62E-03
 
 ETA4
+       ......... ......... .........  3.30E-02
 
 ETA5
+       ......... ......... .........  3.27E-01  4.73E-02
 
 ETA6
+       ......... ......... .........  1.81E-01  2.81E-01  6.34E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.02E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        2.98E-05
 
 TH 2
+        1.25E-06  1.34E-05
 
 TH 3
+        1.37E-06  1.33E-06  1.31E-05
 
 OM11
+       -5.78E-07  4.84E-09  9.46E-08  7.99E-07
 
 OM12
+        4.46E-08  2.58E-08 -1.56E-08  2.27E-08  2.61E-07
 
 OM13
+        5.10E-07 -3.95E-09  2.59E-08  2.49E-08  2.95E-08  3.83E-07
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.96E-08  2.59E-07 -4.72E-08 -5.92E-09  2.03E-08  4.71E-09  0.00E+00  0.00E+00  0.00E+00  2.70E-07
 
 OM23
+        4.35E-09 -4.98E-08  6.57E-08  1.77E-09  1.52E-08  2.01E-08  0.00E+00  0.00E+00  0.00E+00  3.69E-08  1.44E-07
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        6.29E-08  7.96E-08  7.71E-09  3.10E-08  8.16E-09  5.84E-08  0.00E+00  0.00E+00  0.00E+00  1.17E-08  1.93E-08  0.00E+00
          0.00E+00  0.00E+00  2.66E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        7.49E-07  6.83E-07  3.13E-07 -5.12E-07  1.23E-07  2.39E-07  0.00E+00  0.00E+00  0.00E+00  1.47E-07 -1.72E-07  0.00E+00
          0.00E+00  0.00E+00 -8.14E-08  0.00E+00  0.00E+00  0.00E+00  1.38E-04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+       -3.48E-07  2.93E-07  1.55E-07  4.18E-07 -1.50E-08  4.20E-07  0.00E+00  0.00E+00  0.00E+00  6.79E-08 -1.05E-07  0.00E+00
          0.00E+00  0.00E+00  1.49E-07  0.00E+00  0.00E+00  0.00E+00 -1.63E-05  1.06E-04
 
 OM46
+       -8.82E-07  1.04E-06  5.38E-07  1.78E-07  2.78E-08  5.12E-09  0.00E+00  0.00E+00  0.00E+00  4.02E-07 -1.48E-07  0.00E+00
          0.00E+00  0.00E+00 -3.14E-07  0.00E+00  0.00E+00  0.00E+00  1.36E-04 -5.21E-05  2.73E-04
 
 OM55
+        3.13E-06  4.09E-08  2.63E-07 -6.42E-07  2.89E-07 -5.87E-07  0.00E+00  0.00E+00  0.00E+00 -3.43E-08  2.65E-07  0.00E+00
          0.00E+00  0.00E+00  4.60E-08  0.00E+00  0.00E+00  0.00E+00  3.00E-05 -8.38E-05  2.81E-05  2.36E-04
 
 OM56
+        1.22E-06  1.96E-07 -7.86E-08 -6.78E-08 -9.40E-10  2.56E-07  0.00E+00  0.00E+00  0.00E+00  8.58E-08 -1.67E-07  0.00E+00
          0.00E+00  0.00E+00  1.15E-08  0.00E+00  0.00E+00  0.00E+00 -3.84E-05  9.57E-05 -1.03E-04 -7.85E-05  1.44E-04
 
 OM66
+       -3.61E-06  9.11E-07  2.46E-07  9.39E-07 -3.40E-07 -9.60E-08  0.00E+00  0.00E+00  0.00E+00  5.82E-07 -1.57E-07  0.00E+00
          0.00E+00  0.00E+00 -3.37E-07  0.00E+00  0.00E+00  0.00E+00  1.80E-04 -8.88E-05  4.29E-04  2.08E-05 -2.00E-04  8.40E-04
 
 SG11
+        1.95E-09  2.19E-09  4.60E-09 -3.46E-09  2.16E-10 -2.21E-09  0.00E+00  0.00E+00  0.00E+00 -1.56E-09 -1.38E-09  0.00E+00
          0.00E+00  0.00E+00 -2.03E-09  0.00E+00  0.00E+00  0.00E+00  2.67E-08 -5.91E-09  1.31E-08  4.97E-08  5.53E-10 -3.12E-08
         3.02E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        5.45E-03
 
 TH 2
+        6.28E-02  3.66E-03
 
 TH 3
+        6.94E-02  1.00E-01  3.62E-03
 
 OM11
+       -1.19E-01  1.48E-03  2.92E-02  8.94E-04
 
 OM12
+        1.60E-02  1.38E-02 -8.40E-03  4.97E-02  5.11E-04
 
 OM13
+        1.51E-01 -1.75E-03  1.15E-02  4.50E-02  9.32E-02  6.19E-04
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.40E-02  1.36E-01 -2.51E-02 -1.27E-02  7.65E-02  1.46E-02  0.00E+00  0.00E+00  0.00E+00  5.20E-04
 
 OM23
+        2.11E-03 -3.59E-02  4.78E-02  5.21E-03  7.84E-02  8.57E-02  0.00E+00  0.00E+00  0.00E+00  1.87E-01  3.79E-04
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        2.24E-02  4.22E-02  4.13E-03  6.73E-02  3.10E-02  1.83E-01  0.00E+00  0.00E+00  0.00E+00  4.34E-02  9.88E-02  0.00E+00
          0.00E+00  0.00E+00  5.16E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        1.17E-02  1.59E-02  7.34E-03 -4.87E-02  2.05E-02  3.28E-02  0.00E+00  0.00E+00  0.00E+00  2.41E-02 -3.86E-02  0.00E+00
          0.00E+00  0.00E+00 -1.34E-02  0.00E+00  0.00E+00  0.00E+00  1.18E-02
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+       -6.21E-03  7.78E-03  4.15E-03  4.55E-02 -2.86E-03  6.61E-02  0.00E+00  0.00E+00  0.00E+00  1.27E-02 -2.68E-02  0.00E+00
          0.00E+00  0.00E+00  2.82E-02  0.00E+00  0.00E+00  0.00E+00 -1.35E-01  1.03E-02
 
 OM46
+       -9.80E-03  1.72E-02  8.99E-03  1.21E-02  3.29E-03  5.01E-04  0.00E+00  0.00E+00  0.00E+00  4.69E-02 -2.37E-02  0.00E+00
          0.00E+00  0.00E+00 -3.69E-02  0.00E+00  0.00E+00  0.00E+00  7.00E-01 -3.07E-01  1.65E-02
 
 OM55
+        3.73E-02  7.28E-04  4.72E-03 -4.68E-02  3.68E-02 -6.17E-02  0.00E+00  0.00E+00  0.00E+00 -4.30E-03  4.55E-02  0.00E+00
          0.00E+00  0.00E+00  5.80E-03  0.00E+00  0.00E+00  0.00E+00  1.66E-01 -5.30E-01  1.11E-01  1.54E-02
 
 OM56
+        1.87E-02  4.45E-03 -1.81E-03 -6.31E-03 -1.53E-04  3.45E-02  0.00E+00  0.00E+00  0.00E+00  1.37E-02 -3.67E-02  0.00E+00
          0.00E+00  0.00E+00  1.85E-03  0.00E+00  0.00E+00  0.00E+00 -2.72E-01  7.75E-01 -5.18E-01 -4.26E-01  1.20E-02
 
 OM66
+       -2.29E-02  8.60E-03  2.34E-03  3.62E-02 -2.30E-02 -5.36E-03  0.00E+00  0.00E+00  0.00E+00  3.86E-02 -1.43E-02  0.00E+00
          0.00E+00  0.00E+00 -2.26E-02  0.00E+00  0.00E+00  0.00E+00  5.29E-01 -2.98E-01  8.96E-01  4.66E-02 -5.73E-01  2.90E-02
 
 SG11
+        6.50E-03  1.09E-02  2.31E-02 -7.04E-02  7.71E-03 -6.51E-02  0.00E+00  0.00E+00  0.00E+00 -5.45E-02 -6.62E-02  0.00E+00
          0.00E+00  0.00E+00 -7.17E-02  0.00E+00  0.00E+00  0.00E+00  4.13E-02 -1.05E-02  1.45E-02  5.89E-02  8.38E-04 -1.96E-02
         5.50E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.54E+04
 
 TH 2
+       -2.90E+03  7.78E+04
 
 TH 3
+       -3.54E+03 -8.06E+03  7.78E+04
 
 OM11
+        2.68E+04 -1.77E+03 -1.23E+04  1.31E+06
 
 OM12
+       -2.15E+03 -3.89E+03  8.14E+03 -1.19E+05  3.93E+06
 
 OM13
+       -4.98E+04  7.46E+03  8.45E+02 -8.45E+04 -2.77E+05  2.84E+06
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.26E+03 -8.18E+04  2.70E+04  4.65E+04 -2.49E+05  4.72E+04  0.00E+00  0.00E+00  0.00E+00  3.98E+06
 
 OM23
+        7.43E+03  5.36E+04 -4.80E+04  3.61E+04 -3.16E+05 -3.10E+05  0.00E+00  0.00E+00  0.00E+00 -1.04E+06  7.48E+06
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+       -2.80E+02 -2.46E+04  3.16E+03 -1.27E+05 -1.73E+04 -5.60E+05  0.00E+00  0.00E+00  0.00E+00 -8.45E+04 -4.06E+05  0.00E+00
          0.00E+00  0.00E+00  3.97E+06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        7.89E+01  3.76E+01  6.23E+01  1.07E+04 -1.35E+03 -9.64E+03  0.00E+00  0.00E+00  0.00E+00  3.87E+03  1.14E+04  0.00E+00
          0.00E+00  0.00E+00 -5.17E+03  0.00E+00  0.00E+00  0.00E+00  1.65E+04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        5.29E+02 -3.50E+02 -4.29E+02 -1.17E+04 -3.40E+03 -9.69E+03  0.00E+00  0.00E+00  0.00E+00  2.96E+03 -8.94E+03  0.00E+00
          0.00E+00  0.00E+00 -1.24E+04  0.00E+00  0.00E+00  0.00E+00 -3.00E+03  2.93E+04
 
 OM46
+       -1.57E+02 -5.85E+02 -6.79E+02 -3.73E+03 -1.25E+04  4.47E+01  0.00E+00  0.00E+00  0.00E+00 -1.11E+04  3.04E+03  0.00E+00
          0.00E+00  0.00E+00  1.34E+04  0.00E+00  0.00E+00  0.00E+00 -1.32E+04  4.46E+03  3.02E+04
 
 OM55
+       -6.33E+02 -2.05E+01 -2.92E+01  7.14E+01 -4.65E+03  7.50E+03  0.00E+00  0.00E+00  0.00E+00 -8.95E+02 -8.37E+03  0.00E+00
          0.00E+00  0.00E+00 -3.94E+03  0.00E+00  0.00E+00  0.00E+00 -1.83E+03  4.08E+03 -2.55E+01  6.49E+03
 
 OM56
+       -9.20E+02  1.87E+02  2.17E+02  6.44E+03  3.09E+03  5.77E+03  0.00E+00  0.00E+00  0.00E+00 -1.46E+04  1.82E+04  0.00E+00
          0.00E+00  0.00E+00  1.09E+04  0.00E+00  0.00E+00  0.00E+00  5.41E+01 -1.93E+04 -2.55E+03  1.83E+03  2.66E+04
 
 OM66
+        3.89E+01  2.60E+02  2.97E+02 -1.53E+03  8.92E+03  2.03E+03  0.00E+00  0.00E+00  0.00E+00 -1.29E+03  1.43E+03  0.00E+00
          0.00E+00  0.00E+00 -2.63E+03  0.00E+00  0.00E+00  0.00E+00  2.93E+03 -3.22E+03 -1.27E+04  1.10E+03  5.54E+03  7.99E+03
 
 SG11
+       -6.95E+03 -6.84E+04 -1.25E+05  1.28E+06 -6.86E+05  1.50E+06  0.00E+00  0.00E+00  0.00E+00  1.67E+06  2.49E+06  0.00E+00
          0.00E+00  0.00E+00  1.90E+06  0.00E+00  0.00E+00  0.00E+00 -2.54E+04 -6.30E+04 -1.33E+05 -7.21E+04  1.46E+04  8.29E+04
         3.40E+08
 
1
 
 
 #TBLN:      4
 #METH: Objective Function Evaluation by Importance Sampling
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            624
 NO. OF SIG. FIGURES REQUIRED:            2
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
 FINAL ETA RE-EVALUATION (FNLETA):        OFF
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
 ITERATIONS (NITER):                      5           
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
   1   2   3
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -16473.9546622767 eff.=     379. Smpl.=     300. Fit.= 0.90980
 iteration            1 OBJ=  -16944.8814346056 eff.=     111. Smpl.=     300. Fit.= 0.84261
 iteration            2 OBJ=  -17074.6488238393 eff.=     113. Smpl.=     300. Fit.= 0.84613
 iteration            3 OBJ=  -17077.8005604589 eff.=     120. Smpl.=     300. Fit.= 0.85537
 iteration            4 OBJ=  -17081.4992757684 eff.=     120. Smpl.=     300. Fit.= 0.85481
 iteration            5 OBJ=  -17075.4086732197 eff.=     120. Smpl.=     300. Fit.= 0.85536
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         9.6190E-04  8.6274E-05  1.1744E-04  1.3989E-16 -1.2653E-16  2.5112E-16
 SE:             2.2796E-03  3.3965E-03  3.3547E-03  4.2836E-02  3.9254E-02  5.5274E-02
 N:                     800         800         800          16          16          16
 
 P VAL.:         6.7305E-01  9.7974E-01  9.7207E-01  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   3.6947E+01  2.1018E+00  2.7810E+00  6.1914E-01  8.2700E-02  1.4096E-01
 EBVshrink(%):   3.5643E+01  2.0453E+00  2.7050E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.2267E+01
 
 #TERE:
 Elapsed estimation time in seconds:   138.90
 Elapsed covariance time in seconds:    29.01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -17075.409       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.79E-01 -5.31E+00 -3.08E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.07E-02
 
 ETA2
+        3.54E-04  9.83E-03
 
 ETA3
+        3.96E-04  6.65E-04  9.72E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  3.17E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -5.56E-03  2.63E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  2.75E-02 -6.22E-03  5.23E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.99E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.03E-01
 
 ETA2
+        3.46E-02  9.91E-02
 
 ETA3
+        3.89E-02  6.80E-02  9.86E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.78E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -1.92E-01  1.62E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  6.75E-01 -1.67E-01  2.29E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.47E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         5.69E-03  3.59E-03  3.59E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.60E-04
 
 ETA2
+        5.68E-04  5.17E-04
 
 ETA3
+        6.36E-04  3.68E-04  5.17E-04
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.19E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  1.03E-02  1.54E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  1.66E-02  1.20E-02  2.91E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.48E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        4.65E-03
 
 ETA2
+        5.51E-02  2.61E-03
 
 ETA3
+        6.21E-02  3.71E-02  2.62E-03
 
 ETA4
+       ......... ......... .........  3.34E-02
 
 ETA5
+       ......... ......... .........  3.27E-01  4.74E-02
 
 ETA6
+       ......... ......... .........  1.80E-01  2.81E-01  6.37E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.01E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.23E-05
 
 TH 2
+        1.40E-06  1.29E-05
 
 TH 3
+        1.63E-06  1.47E-06  1.29E-05
 
 OM11
+       -1.11E-07 -4.71E-09 -2.96E-09  9.22E-07
 
 OM12
+       -5.14E-08 -2.92E-08 -5.32E-09  8.26E-08  3.22E-07
 
 OM13
+        1.44E-07 -2.30E-10 -3.00E-08  9.83E-08  4.57E-08  4.04E-07
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.14E-09  5.05E-09  3.26E-09  4.75E-09  3.34E-08  3.31E-09  0.00E+00  0.00E+00  0.00E+00  2.67E-07
 
 OM23
+       -6.30E-10 -4.28E-09 -2.97E-09  5.41E-09  1.70E-08  1.81E-08  0.00E+00  0.00E+00  0.00E+00  2.97E-08  1.35E-07
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        5.74E-09 -1.54E-09 -5.43E-09  3.88E-09  2.89E-09  3.02E-08  0.00E+00  0.00E+00  0.00E+00  2.05E-09  3.13E-08  0.00E+00
          0.00E+00  0.00E+00  2.67E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        1.53E-06  4.96E-07  5.45E-07 -6.17E-07  1.49E-07  8.71E-08  0.00E+00  0.00E+00  0.00E+00  1.65E-07 -1.71E-07  0.00E+00
          0.00E+00  0.00E+00 -1.02E-07  0.00E+00  0.00E+00  0.00E+00  1.42E-04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        3.01E-07  3.42E-07  3.68E-07  3.01E-07 -9.41E-08  2.71E-07  0.00E+00  0.00E+00  0.00E+00  5.28E-08 -1.04E-07  0.00E+00
          0.00E+00  0.00E+00  9.98E-08  0.00E+00  0.00E+00  0.00E+00 -1.67E-05  1.05E-04
 
 OM46
+        1.53E-06  7.92E-07  8.55E-07  2.10E-08  6.64E-08 -6.03E-08  0.00E+00  0.00E+00  0.00E+00  4.19E-07 -1.78E-07  0.00E+00
          0.00E+00  0.00E+00 -3.89E-07  0.00E+00  0.00E+00  0.00E+00  1.38E-04 -5.25E-05  2.74E-04
 
 OM55
+        6.70E-07 -6.28E-08  2.64E-08 -2.91E-07  1.72E-07 -6.50E-07  0.00E+00  0.00E+00  0.00E+00 -4.63E-08  2.58E-07  0.00E+00
          0.00E+00  0.00E+00  1.53E-07  0.00E+00  0.00E+00  0.00E+00  3.08E-05 -8.18E-05  2.85E-05  2.37E-04
 
 OM56
+        7.50E-07  1.84E-07  2.20E-07 -8.11E-08 -6.37E-08  7.36E-08  0.00E+00  0.00E+00  0.00E+00  8.54E-08 -1.67E-07  0.00E+00
          0.00E+00  0.00E+00 -3.05E-08  0.00E+00  0.00E+00  0.00E+00 -3.82E-05  9.57E-05 -1.03E-04 -7.75E-05  1.44E-04
 
 OM66
+        6.53E-07  8.09E-07  8.11E-07  2.47E-07 -4.07E-07 -3.49E-07  0.00E+00  0.00E+00  0.00E+00  5.81E-07 -2.21E-07  0.00E+00
          0.00E+00  0.00E+00 -4.94E-07  0.00E+00  0.00E+00  0.00E+00  1.80E-04 -9.08E-05  4.32E-04  2.05E-05 -2.01E-04  8.47E-04
 
 SG11
+        3.59E-10  2.46E-09  2.76E-09 -1.95E-09 -5.46E-10 -9.67E-10  0.00E+00  0.00E+00  0.00E+00 -2.41E-10 -4.87E-10  0.00E+00
          0.00E+00  0.00E+00 -4.36E-10  0.00E+00  0.00E+00  0.00E+00  3.82E-08 -3.41E-09  3.24E-08  5.23E-08 -2.49E-09  6.51E-09
         3.00E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        5.69E-03
 
 TH 2
+        6.85E-02  3.59E-03
 
 TH 3
+        7.99E-02  1.14E-01  3.59E-03
 
 OM11
+       -2.04E-02 -1.37E-03 -8.59E-04  9.60E-04
 
 OM12
+       -1.59E-02 -1.43E-02 -2.61E-03  1.52E-01  5.68E-04
 
 OM13
+        3.97E-02 -1.01E-04 -1.31E-02  1.61E-01  1.27E-01  6.36E-04
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.41E-03  2.72E-03  1.76E-03  9.57E-03  1.14E-01  1.01E-02  0.00E+00  0.00E+00  0.00E+00  5.17E-04
 
 OM23
+       -3.01E-04 -3.25E-03 -2.25E-03  1.53E-02  8.15E-02  7.76E-02  0.00E+00  0.00E+00  0.00E+00  1.56E-01  3.68E-04
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        1.95E-03 -8.28E-04 -2.92E-03  7.81E-03  9.84E-03  9.19E-02  0.00E+00  0.00E+00  0.00E+00  7.66E-03  1.65E-01  0.00E+00
          0.00E+00  0.00E+00  5.17E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        2.26E-02  1.16E-02  1.27E-02 -5.40E-02  2.20E-02  1.15E-02  0.00E+00  0.00E+00  0.00E+00  2.69E-02 -3.90E-02  0.00E+00
          0.00E+00  0.00E+00 -1.66E-02  0.00E+00  0.00E+00  0.00E+00  1.19E-02
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        5.17E-03  9.30E-03  9.98E-03  3.06E-02 -1.62E-02  4.16E-02  0.00E+00  0.00E+00  0.00E+00  9.95E-03 -2.75E-02  0.00E+00
          0.00E+00  0.00E+00  1.88E-02  0.00E+00  0.00E+00  0.00E+00 -1.37E-01  1.03E-02
 
 OM46
+        1.63E-02  1.33E-02  1.44E-02  1.32E-03  7.06E-03 -5.73E-03  0.00E+00  0.00E+00  0.00E+00  4.90E-02 -2.92E-02  0.00E+00
          0.00E+00  0.00E+00 -4.54E-02  0.00E+00  0.00E+00  0.00E+00  6.99E-01 -3.09E-01  1.66E-02
 
 OM55
+        7.66E-03 -1.14E-03  4.77E-04 -1.97E-02  1.97E-02 -6.64E-02  0.00E+00  0.00E+00  0.00E+00 -5.81E-03  4.56E-02  0.00E+00
          0.00E+00  0.00E+00  1.93E-02  0.00E+00  0.00E+00  0.00E+00  1.68E-01 -5.18E-01  1.12E-01  1.54E-02
 
 OM56
+        1.10E-02  4.26E-03  5.09E-03 -7.03E-03 -9.34E-03  9.63E-03  0.00E+00  0.00E+00  0.00E+00  1.37E-02 -3.77E-02  0.00E+00
          0.00E+00  0.00E+00 -4.90E-03  0.00E+00  0.00E+00  0.00E+00 -2.67E-01  7.77E-01 -5.18E-01 -4.19E-01  1.20E-02
 
 OM66
+        3.94E-03  7.75E-03  7.75E-03  8.83E-03 -2.46E-02 -1.89E-02  0.00E+00  0.00E+00  0.00E+00  3.86E-02 -2.07E-02  0.00E+00
          0.00E+00  0.00E+00 -3.28E-02  0.00E+00  0.00E+00  0.00E+00  5.20E-01 -3.04E-01  8.96E-01  4.58E-02 -5.74E-01  2.91E-02
 
 SG11
+        1.15E-03  1.25E-02  1.40E-02 -3.70E-02 -1.76E-02 -2.78E-02  0.00E+00  0.00E+00  0.00E+00 -8.51E-03 -2.42E-02  0.00E+00
          0.00E+00  0.00E+00 -1.54E-02  0.00E+00  0.00E+00  0.00E+00  5.86E-02 -6.06E-03  3.58E-02  6.20E-02 -3.79E-03  4.08E-03
         5.48E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.13E+04
 
 TH 2
+       -2.95E+03  7.91E+04
 
 TH 3
+       -3.62E+03 -8.60E+03  7.90E+04
 
 OM11
+        4.45E+03 -5.60E+02 -9.33E+02  1.15E+06
 
 OM12
+        5.84E+03  6.94E+03 -7.31E+02 -2.64E+05  3.29E+06
 
 OM13
+       -1.34E+04 -1.30E+02  7.52E+03 -2.51E+05 -2.90E+05  2.63E+06
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.46E+02 -1.89E+03 -2.02E+02  1.47E+04 -3.72E+05  3.69E+04  0.00E+00  0.00E+00  0.00E+00  3.90E+06
 
 OM23
+        2.94E+02  1.24E+03 -2.66E+02  2.63E+04 -2.88E+05 -2.66E+05  0.00E+00  0.00E+00  0.00E+00 -8.52E+05  7.90E+06
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        1.23E+02 -1.82E+02  4.65E+02  1.39E+04  3.63E+04 -2.57E+05  0.00E+00  0.00E+00  0.00E+00  5.01E+04 -8.63E+05  0.00E+00
          0.00E+00  0.00E+00  3.89E+06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+       -7.74E+01  7.38E+01  8.70E+01  1.16E+04 -3.67E+03 -6.19E+03  0.00E+00  0.00E+00  0.00E+00  3.71E+03  1.04E+04  0.00E+00
          0.00E+00  0.00E+00 -6.33E+03  0.00E+00  0.00E+00  0.00E+00  1.63E+04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        1.41E+02 -3.87E+02 -4.60E+02 -1.06E+04  3.01E+03 -7.89E+03  0.00E+00  0.00E+00  0.00E+00  3.98E+03 -8.69E+03  0.00E+00
          0.00E+00  0.00E+00 -1.42E+04  0.00E+00  0.00E+00  0.00E+00 -2.76E+03  2.91E+04
 
 OM46
+       -5.05E+02 -5.14E+02 -5.89E+02 -6.66E+03 -1.28E+04 -4.55E+03  0.00E+00  0.00E+00  0.00E+00 -1.07E+04  3.88E+03  0.00E+00
          0.00E+00  0.00E+00  1.41E+04  0.00E+00  0.00E+00  0.00E+00 -1.35E+04  4.15E+03  3.08E+04
 
 OM55
+       -1.86E+02 -5.25E+01 -7.71E+01 -1.68E+03 -1.84E+02  9.59E+03  0.00E+00  0.00E+00  0.00E+00 -6.78E+02 -7.83E+03  0.00E+00
          0.00E+00  0.00E+00 -3.90E+03  0.00E+00  0.00E+00  0.00E+00 -1.72E+03  3.84E+03 -1.78E+02  6.36E+03
 
 OM56
+       -5.02E+02  2.59E+01  1.66E+01  6.08E+03  3.17E+03  1.22E+04  0.00E+00  0.00E+00  0.00E+00 -1.53E+04  1.84E+04  0.00E+00
          0.00E+00  0.00E+00  1.22E+04  0.00E+00  0.00E+00  0.00E+00 -3.00E+01 -1.93E+04 -2.43E+03  1.88E+03  2.67E+04
 
 OM66
+        1.53E+02  1.52E+02  1.78E+02  7.20E+02  1.01E+04  6.23E+03  0.00E+00  0.00E+00  0.00E+00 -1.54E+03  1.30E+03  0.00E+00
          0.00E+00  0.00E+00 -2.45E+03  0.00E+00  0.00E+00  0.00E+00  3.16E+03 -3.10E+03 -1.29E+04  1.16E+03  5.48E+03  8.04E+03
 
 SG11
+        1.08E+04 -5.09E+04 -5.78E+04  5.69E+05  4.29E+05  4.98E+05  0.00E+00  0.00E+00  0.00E+00  2.14E+05  9.30E+05  0.00E+00
          0.00E+00  0.00E+00  3.58E+05  0.00E+00  0.00E+00  0.00E+00 -3.56E+04 -6.42E+04 -1.32E+05 -8.33E+04 -5.81E+03  6.65E+04
         3.37E+08
 
1
 
 
 #TBLN:      5
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            624
 NO. OF SIG. FIGURES REQUIRED:            2
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
 FINAL ETA RE-EVALUATION (FNLETA):        OFF
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 EM OR BAYESIAN METHOD USED:              MCMC BAYESIAN (BAYES)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        10          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              1000        
 ITERATIONS (NITER):                      500         
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        1           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.00000000        
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2           
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0           
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2           
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2           
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
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -1000 MCMCOBJ=   -37073.6031302235     
 iteration         -990 MCMCOBJ=   -35751.4942146457     
 iteration         -980 MCMCOBJ=   -35332.6944936807     
 iteration         -970 MCMCOBJ=   -35292.4418512129     
 iteration         -960 MCMCOBJ=   -35264.1698783583     
 iteration         -950 MCMCOBJ=   -35236.0194507077     
 iteration         -940 MCMCOBJ=   -35234.7934116013     
 iteration         -930 MCMCOBJ=   -35336.8624138618     
 iteration         -920 MCMCOBJ=   -35194.9362095637     
 iteration         -910 MCMCOBJ=   -35121.5136339066     
 iteration         -900 MCMCOBJ=   -35101.1986308304     
 iteration         -890 MCMCOBJ=   -35227.0479466417     
 iteration         -880 MCMCOBJ=   -35159.5532082608     
 iteration         -870 MCMCOBJ=   -35283.2581888694     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -35221.5949611685     
 iteration           10 MCMCOBJ=   -35200.3663360969     
 iteration           20 MCMCOBJ=   -35244.8832254667     
 iteration           30 MCMCOBJ=   -35306.2799791646     
 iteration           40 MCMCOBJ=   -35216.0281349424     
 iteration           50 MCMCOBJ=   -35242.2599283906     
 iteration           60 MCMCOBJ=   -35211.5366133730     
 iteration           70 MCMCOBJ=   -35099.2739511526     
 iteration           80 MCMCOBJ=   -35196.7543628653     
 iteration           90 MCMCOBJ=   -35249.2176219179     
 iteration          100 MCMCOBJ=   -35207.7422567931     
 iteration          110 MCMCOBJ=   -35159.2110505651     
 iteration          120 MCMCOBJ=   -35149.1070458709     
 iteration          130 MCMCOBJ=   -35172.3193765173     
 iteration          140 MCMCOBJ=   -34987.4707893201     
 iteration          150 MCMCOBJ=   -35189.7234115332     
 iteration          160 MCMCOBJ=   -35180.3544784369     
 iteration          170 MCMCOBJ=   -35244.8376914824     
 iteration          180 MCMCOBJ=   -35158.9779837750     
 iteration          190 MCMCOBJ=   -35156.0101786055     
 iteration          200 MCMCOBJ=   -35213.2093728146     
 iteration          210 MCMCOBJ=   -35156.5470897407     
 iteration          220 MCMCOBJ=   -35172.4735865854     
 iteration          230 MCMCOBJ=   -35177.8844829049     
 iteration          240 MCMCOBJ=   -35144.6268147935     
 iteration          250 MCMCOBJ=   -35085.9258207120     
 iteration          260 MCMCOBJ=   -35096.6220619864     
 iteration          270 MCMCOBJ=   -35082.3988160665     
 iteration          280 MCMCOBJ=   -35249.8260552709     
 iteration          290 MCMCOBJ=   -35255.9395709025     
 iteration          300 MCMCOBJ=   -35319.2311558046     
 iteration          310 MCMCOBJ=   -35162.6956333423     
 iteration          320 MCMCOBJ=   -35076.1961657318     
 iteration          330 MCMCOBJ=   -35219.3548743145     
 iteration          340 MCMCOBJ=   -35267.2111154140     
 iteration          350 MCMCOBJ=   -35335.6019101781     
 iteration          360 MCMCOBJ=   -35234.7421138403     
 iteration          370 MCMCOBJ=   -35144.5817338665     
 iteration          380 MCMCOBJ=   -35168.1877308544     
 iteration          390 MCMCOBJ=   -35050.6307331527     
 iteration          400 MCMCOBJ=   -35208.7676507006     
 iteration          410 MCMCOBJ=   -35143.6655307806     
 iteration          420 MCMCOBJ=   -35073.2807963462     
 iteration          430 MCMCOBJ=   -34951.8636080250     
 iteration          440 MCMCOBJ=   -35083.6228033911     
 iteration          450 MCMCOBJ=   -35186.5057979329     
 iteration          460 MCMCOBJ=   -35198.7370817183     
 iteration          470 MCMCOBJ=   -35298.3825848524     
 iteration          480 MCMCOBJ=   -35229.1478330798     
 iteration          490 MCMCOBJ=   -35355.1962539730     
 iteration          500 MCMCOBJ=   -35340.3150125341     
 
 #TERM:
 BURN-IN WAS COMPLETED
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation time in seconds:   843.09
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -35168.444       **************************************************
 #OBJS:********************************************       88.098 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.80E-01 -5.31E+00 -3.08E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.20E-02
 
 ETA2
+        5.44E-04  9.97E-03
 
 ETA3
+        1.03E-03  7.42E-04  9.81E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  4.38E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -7.36E-03  3.44E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  3.80E-02 -7.76E-03  7.04E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.01E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.09E-01
 
 ETA2
+        4.96E-02  9.98E-02
 
 ETA3
+        9.50E-02  7.49E-02  9.90E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  2.04E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -1.84E-01  1.81E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  6.72E-01 -1.50E-01  2.60E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.49E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         6.68E-03  3.57E-03  3.81E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        8.65E-04
 
 ETA2
+        5.63E-04  5.69E-04
 
 ETA3
+        6.45E-04  3.82E-04  5.25E-04
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  2.03E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  1.35E-02  1.73E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  2.05E-02  1.55E-02  3.03E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.59E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        3.93E-03
 
 ETA2
+        5.10E-02  2.84E-03
 
 ETA3
+        5.89E-02  3.80E-02  2.64E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  4.41E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  2.62E-01  4.07E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  1.56E-01  2.62E-01  5.34E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.09E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        4.46E-05
 
 TH 2
+        1.74E-06  1.28E-05
 
 TH 3
+        3.98E-06  9.82E-07  1.45E-05
 
 OM11
+       -2.81E-07 -1.04E-07 -8.19E-09  7.49E-07
 
 OM12
+        7.84E-07  9.18E-08  4.45E-08  4.94E-08  3.17E-07
 
 OM13
+       -8.99E-08  1.60E-07  9.17E-08  8.76E-08 -1.15E-08  4.16E-07
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -7.77E-08  5.74E-08 -7.82E-08 -5.89E-11  4.08E-08 -2.60E-08  0.00E+00  0.00E+00  0.00E+00  3.23E-07
 
 OM23
+       -5.75E-08  7.80E-08  6.06E-08 -4.30E-09  2.27E-08 -8.04E-09  0.00E+00  0.00E+00  0.00E+00  3.94E-08  1.46E-07
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        1.48E-07 -7.64E-08 -3.39E-08  2.30E-08 -3.30E-09  4.06E-08  0.00E+00  0.00E+00  0.00E+00  1.45E-08  3.30E-08  0.00E+00
          0.00E+00  0.00E+00  2.75E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+       -2.03E-06  2.26E-07 -6.02E-06 -1.72E-07  8.70E-07 -3.30E-07  0.00E+00  0.00E+00  0.00E+00 -1.63E-07  8.17E-07  0.00E+00
          0.00E+00  0.00E+00  4.66E-07  0.00E+00  0.00E+00  0.00E+00  4.11E-04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        2.66E-06 -1.08E-06  5.52E-07  7.30E-07 -1.42E-07  1.06E-06  0.00E+00  0.00E+00  0.00E+00 -2.27E-07 -3.18E-07  0.00E+00
          0.00E+00  0.00E+00  4.22E-07  0.00E+00  0.00E+00  0.00E+00 -6.81E-05  1.81E-04
 
 OM46
+        7.22E-06  2.12E-06 -2.18E-06 -3.88E-07  7.40E-07 -3.61E-07  0.00E+00  0.00E+00  0.00E+00 -2.59E-07  5.82E-07  0.00E+00
          0.00E+00  0.00E+00  9.92E-07  0.00E+00  0.00E+00  0.00E+00  3.28E-04 -6.08E-05  4.19E-04
 
 OM55
+       -6.83E-06  1.01E-06 -1.68E-06  4.42E-07 -2.90E-07  3.01E-07  0.00E+00  0.00E+00  0.00E+00  9.58E-07  6.78E-08  0.00E+00
          0.00E+00  0.00E+00 -4.56E-07  0.00E+00  0.00E+00  0.00E+00  5.67E-05 -6.35E-05  3.78E-05  2.99E-04
 
 OM56
+       -6.04E-07 -3.81E-07 -8.08E-07 -2.93E-08  4.97E-08  1.01E-06  0.00E+00  0.00E+00  0.00E+00 -4.99E-07 -4.90E-07  0.00E+00
          0.00E+00  0.00E+00  1.98E-07  0.00E+00  0.00E+00  0.00E+00 -7.25E-05  1.49E-04 -8.76E-05 -6.48E-05  2.40E-04
 
 OM66
+        1.67E-05  2.91E-06  2.72E-07 -2.38E-07  9.61E-07 -1.14E-06  0.00E+00  0.00E+00  0.00E+00  4.49E-07  6.61E-07  0.00E+00
          0.00E+00  0.00E+00  1.70E-06  0.00E+00  0.00E+00  0.00E+00  2.87E-04 -6.22E-05  4.78E-04  3.54E-05 -1.18E-04  9.16E-04
 
 SG11
+        1.43E-08 -1.53E-09  1.92E-08  1.98E-09  5.24E-10  1.58E-09  0.00E+00  0.00E+00  0.00E+00  1.91E-09  6.68E-10  0.00E+00
          0.00E+00  0.00E+00 -2.35E-09  0.00E+00  0.00E+00  0.00E+00  2.98E-08  3.27E-08  6.13E-08  1.85E-08  3.88E-08  9.95E-08
         3.13E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        6.68E-03
 
 TH 2
+        7.28E-02  3.57E-03
 
 TH 3
+        1.57E-01  7.22E-02  3.81E-03
 
 OM11
+       -4.86E-02 -3.35E-02 -2.49E-03  8.65E-04
 
 OM12
+        2.09E-01  4.57E-02  2.08E-02  1.02E-01  5.63E-04
 
 OM13
+       -2.08E-02  6.93E-02  3.73E-02  1.57E-01 -3.18E-02  6.45E-04
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.04E-02  2.83E-02 -3.61E-02 -1.20E-04  1.27E-01 -7.10E-02  0.00E+00  0.00E+00  0.00E+00  5.69E-04
 
 OM23
+       -2.25E-02  5.71E-02  4.17E-02 -1.30E-02  1.06E-01 -3.26E-02  0.00E+00  0.00E+00  0.00E+00  1.81E-01  3.82E-04
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        4.23E-02 -4.08E-02 -1.70E-02  5.08E-02 -1.12E-02  1.20E-01  0.00E+00  0.00E+00  0.00E+00  4.87E-02  1.65E-01  0.00E+00
          0.00E+00  0.00E+00  5.25E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+       -1.50E-02  3.12E-03 -7.80E-02 -9.81E-03  7.63E-02 -2.52E-02  0.00E+00  0.00E+00  0.00E+00 -1.42E-02  1.06E-01  0.00E+00
          0.00E+00  0.00E+00  4.38E-02  0.00E+00  0.00E+00  0.00E+00  2.03E-02
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        2.96E-02 -2.25E-02  1.08E-02  6.27E-02 -1.87E-02  1.22E-01  0.00E+00  0.00E+00  0.00E+00 -2.96E-02 -6.18E-02  0.00E+00
          0.00E+00  0.00E+00  5.98E-02  0.00E+00  0.00E+00  0.00E+00 -2.50E-01  1.35E-02
 
 OM46
+        5.28E-02  2.89E-02 -2.79E-02 -2.19E-02  6.43E-02 -2.73E-02  0.00E+00  0.00E+00  0.00E+00 -2.22E-02  7.44E-02  0.00E+00
          0.00E+00  0.00E+00  9.24E-02  0.00E+00  0.00E+00  0.00E+00  7.91E-01 -2.21E-01  2.05E-02
 
 OM55
+       -5.91E-02  1.64E-02 -2.55E-02  2.96E-02 -2.98E-02  2.70E-02  0.00E+00  0.00E+00  0.00E+00  9.75E-02  1.03E-02  0.00E+00
          0.00E+00  0.00E+00 -5.03E-02  0.00E+00  0.00E+00  0.00E+00  1.62E-01 -2.73E-01  1.07E-01  1.73E-02
 
 OM56
+       -5.84E-03 -6.88E-03 -1.37E-02 -2.18E-03  5.70E-03  1.01E-01  0.00E+00  0.00E+00  0.00E+00 -5.66E-02 -8.28E-02  0.00E+00
          0.00E+00  0.00E+00  2.44E-02  0.00E+00  0.00E+00  0.00E+00 -2.31E-01  7.16E-01 -2.76E-01 -2.42E-01  1.55E-02
 
 OM66
+        8.25E-02  2.69E-02  2.36E-03 -9.07E-03  5.65E-02 -5.83E-02  0.00E+00  0.00E+00  0.00E+00  2.61E-02  5.72E-02  0.00E+00
          0.00E+00  0.00E+00  1.07E-01  0.00E+00  0.00E+00  0.00E+00  4.69E-01 -1.53E-01  7.71E-01  6.76E-02 -2.51E-01  3.03E-02
 
 SG11
+        3.82E-02 -7.68E-03  8.99E-02  4.08E-02  1.67E-02  4.39E-02  0.00E+00  0.00E+00  0.00E+00  5.99E-02  3.13E-02  0.00E+00
          0.00E+00  0.00E+00 -8.02E-02  0.00E+00  0.00E+00  0.00E+00  2.63E-02  4.35E-02  5.35E-02  1.92E-02  4.47E-02  5.88E-02
         5.59E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        2.48E+04
 
 TH 2
+       -2.65E+03  8.06E+04
 
 TH 3
+       -6.09E+03 -4.33E+03  7.26E+04
 
 OM11
+        1.37E+04  1.28E+04  1.36E+02  1.42E+06
 
 OM12
+       -6.50E+04 -1.37E+04  3.26E+03 -2.91E+05  3.50E+06
 
 OM13
+        6.07E+03 -4.05E+04 -1.53E+04 -2.81E+05  8.03E+04  2.60E+06
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.16E+04 -1.45E+04  2.34E+04  3.26E+04 -4.28E+05  2.06E+05  0.00E+00  0.00E+00  0.00E+00  3.35E+06
 
 OM23
+        2.55E+04 -4.78E+04 -4.40E+04  9.23E+04 -4.65E+05  1.57E+05  0.00E+00  0.00E+00  0.00E+00 -7.82E+05  7.54E+06
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+       -1.84E+04  3.70E+04  1.32E+04 -1.05E+05  1.86E+05 -4.22E+05  0.00E+00  0.00E+00  0.00E+00 -1.75E+05 -9.51E+05  0.00E+00
          0.00E+00  0.00E+00  3.96E+06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        7.62E+02  8.55E+02  1.71E+03 -2.05E+03 -8.65E+03  1.77E+03  0.00E+00  0.00E+00  0.00E+00  4.22E+02 -2.01E+04  0.00E+00
          0.00E+00  0.00E+00  5.22E+03  0.00E+00  0.00E+00  0.00E+00  7.86E+03
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+       -7.45E+02  9.32E+02 -2.05E+02 -1.18E+04  1.01E+04 -1.19E+04  0.00E+00  0.00E+00  0.00E+00 -6.71E+03 -1.21E+03  0.00E+00
          0.00E+00  0.00E+00 -6.81E+03  0.00E+00  0.00E+00  0.00E+00  1.04E+03  1.21E+04
 
 OM46
+       -6.11E+02 -1.10E+03 -6.12E+02  5.03E+03  1.80E+03 -6.98E+03  0.00E+00  0.00E+00  0.00E+00  1.29E+04  1.08E+04  0.00E+00
          0.00E+00  0.00E+00 -9.57E+03  0.00E+00  0.00E+00  0.00E+00 -8.02E+03 -3.15E+02  1.43E+04
 
 OM55
+        3.12E+02 -2.18E+02  1.59E+02 -2.67E+03  5.45E+03 -6.56E+03  0.00E+00  0.00E+00  0.00E+00 -1.16E+04  4.44E+03  0.00E+00
          0.00E+00  0.00E+00  5.57E+03  0.00E+00  0.00E+00  0.00E+00 -5.50E+02  9.26E+02  3.22E+02  3.76E+03
 
 OM56
+        5.64E+02 -6.80E+02  7.55E+02  9.17E+03 -1.08E+04 -3.18E+03  0.00E+00  0.00E+00  0.00E+00  8.25E+03  1.37E+04  0.00E+00
          0.00E+00  0.00E+00 -3.22E+03  0.00E+00  0.00E+00  0.00E+00 -5.41E+02 -7.18E+03  7.95E+02  3.77E+02  9.15E+03
 
 OM66
+       -2.52E+02  5.09E+00 -1.75E+01 -1.26E+03 -1.40E+03  5.94E+03  0.00E+00  0.00E+00  0.00E+00 -5.76E+03 -4.64E+02  0.00E+00
          0.00E+00  0.00E+00 -5.28E+03  0.00E+00  0.00E+00  0.00E+00  1.73E+03 -2.98E+02 -4.87E+03 -5.40E+01  4.33E+02  3.16E+03
 
 SG11
+       -9.25E+04  1.53E+05 -4.18E+05 -8.92E+05  4.14E+05 -1.45E+06  0.00E+00  0.00E+00  0.00E+00 -2.30E+06 -1.97E+06  0.00E+00
          0.00E+00  0.00E+00  3.95E+06  0.00E+00  0.00E+00  0.00E+00  2.23E+04 -2.12E+04 -6.89E+04 -2.41E+04 -8.50E+04 -2.45E+04
         3.32E+08
 
1
 
 
 #TBLN:      6
 #METH: First Order Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            624
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  20          
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    10          
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   10          
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               OFF
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        OFF
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    ON 
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 ADDITIONAL CONVERGENCE TEST (CTYPE=4)?:  NO
 EM OR BAYESIAN METHOD USED:                NONE
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -17045.1644888743        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:       17
 NPARAMETR:  1.8046E-01 -5.3139E+00 -3.0828E+00  1.1965E-02  5.4380E-04  1.0314E-03  9.9674E-03  7.4225E-04  9.8136E-03  4.3757E-02
            -7.3584E-03  3.7990E-02  3.4427E-02 -7.7633E-03  7.0431E-02  3.0122E-03
 PARAMETER:  1.0000E-01 -1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
            -1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   4.8070E+01 -4.9111E+03 -1.9417E+03  5.6413E+01  4.4772E+00 -1.3007E+00  4.6157E+01  3.7391E+00  3.0614E+01  5.8684E+01
            -4.1104E+01  6.1411E+02  1.2799E+02  3.6946E-01  1.1451E+02  6.6474E+01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -17071.6535061100        NO. OF FUNC. EVALS.:  18
 CUMULATIVE NO. OF FUNC. EVALS.:      123
 NPARAMETR:  1.8468E-01 -5.3127E+00 -3.0810E+00  1.0377E-02  4.8165E-04  1.0296E-03  9.0332E-03  6.8255E-04  9.3638E-03  3.6332E-02
            -3.3553E-03  3.0189E-02  2.5201E-02 -3.9937E-03  5.5937E-02  2.9428E-03
 PARAMETER:  1.0234E-01 -9.9978E-02 -9.9943E-02  2.8816E-02  9.5105E-02  1.0718E-01  5.0800E-02  9.5887E-02  7.5691E-02  7.0202E-03
            -5.0041E-02  8.7209E-02 -4.3852E-02 -1.0127E-01  2.9416E-03  8.8356E-02
 GRADIENT:   3.5867E+02  1.3964E+02  1.6146E+03  1.7407E+00  7.1029E+00  8.0978E+00 -6.9191E+01  7.1605E+00 -2.2967E+01  2.3586E+01
             4.2926E+01  2.1615E+02 -9.2613E+00  3.4513E+00  4.0654E+01 -2.2828E+02
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -17077.4712485165        NO. OF FUNC. EVALS.:  19
 CUMULATIVE NO. OF FUNC. EVALS.:      217
 NPARAMETR:  1.8432E-01 -5.3127E+00 -3.0810E+00  1.0295E-02  2.7861E-04  5.7442E-04  9.7160E-03  6.7246E-04  9.6021E-03  3.2480E-02
            -4.0646E-03  2.8940E-02  2.4614E-02 -4.8295E-03  5.2053E-02  3.0013E-03
 PARAMETER:  1.0214E-01 -9.9978E-02 -9.9943E-02  2.4823E-02  5.5234E-02  6.0039E-02  8.8082E-02  9.5603E-02  9.2165E-02 -4.9015E-02
            -6.4114E-02  8.8419E-02 -5.9890E-02 -1.0310E-01 -7.7721E-02  9.8188E-02
 GRADIENT:   3.4435E+02  1.3173E+02  1.6574E+03 -7.5981E+00  2.5145E+00  1.3879E+00 -1.1408E+01  1.6806E+00 -7.3155E+00 -2.7249E+00
             1.5256E+01  1.6152E+02 -8.7124E+00  1.1228E+00  7.9603E+00  8.7725E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -17077.8165764878        NO. OF FUNC. EVALS.:  18
 CUMULATIVE NO. OF FUNC. EVALS.:      310
 NPARAMETR:  1.8265E-01 -5.3128E+00 -3.0814E+00  1.0214E-02  5.4089E-05  6.0036E-04  9.7022E-03  7.5246E-04  9.6766E-03  3.2193E-02
            -3.6182E-03  2.8021E-02  2.4923E-02 -5.6695E-03  5.1108E-02  3.0008E-03
 PARAMETER:  1.0121E-01 -9.9980E-02 -9.9955E-02  2.0919E-02  1.0765E-02  6.2996E-02  8.7745E-02  1.0908E-01  9.5191E-02 -5.3459E-02
            -5.7326E-02  8.5992E-02 -5.1444E-02 -2.1330E-01 -7.2919E-02  9.8099E-02
 GRADIENT:   2.1324E+02  2.1727E+01  1.1063E+03 -7.3028E+00 -3.4471E-01  1.4371E+00 -1.1229E+01  3.6772E+00 -4.5226E+00 -3.3124E+00
             6.5614E+00  9.3724E+01 -4.4844E+00 -4.1366E+00  5.6113E+00  6.4341E+00
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -17078.4161214117        NO. OF FUNC. EVALS.:  18
 CUMULATIVE NO. OF FUNC. EVALS.:      415            RESET HESSIAN, TYPE II
 NPARAMETR:  1.7989E-01 -5.3129E+00 -3.0821E+00  1.0459E-02  1.8928E-04  5.9748E-04  9.8136E-03  6.6952E-04  9.7354E-03  3.2028E-02
            -4.3803E-03  2.7480E-02  2.5085E-02 -5.1497E-03  4.9753E-02  2.9987E-03
 PARAMETER:  9.9685E-02 -9.9981E-02 -9.9978E-02  3.2763E-02  3.7227E-02  6.1955E-02  9.3295E-02  9.5365E-02  9.9015E-02 -5.6028E-02
            -6.9580E-02  8.4548E-02 -5.2066E-02 -1.1783E-01 -7.9809E-02  9.7756E-02
 GRADIENT:  -6.0878E-01  1.2013E+02 -4.0108E+00  1.3840E-01 -1.4050E-02 -1.3169E-02 -1.0125E-01  7.4833E-03 -3.9857E-01  2.4402E-01
             2.4382E-02 -3.6666E+00 -9.8811E-02  4.7101E-02 -5.0119E-01 -6.6506E-02
 
0ITERATION NO.:   22    OBJECTIVE VALUE:  -17078.4161214117        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      447
 NPARAMETR:  1.7989E-01 -5.3129E+00 -3.0821E+00  1.0459E-02  1.8928E-04  5.9748E-04  9.8136E-03  6.6952E-04  9.7354E-03  3.2028E-02
            -4.3803E-03  2.7480E-02  2.5085E-02 -5.1497E-03  4.9753E-02  2.9987E-03
 PARAMETER:  9.9685E-02 -9.9981E-02 -9.9978E-02  3.2763E-02  3.7227E-02  6.1955E-02  9.3295E-02  9.5365E-02  9.9015E-02 -5.6028E-02
            -6.9580E-02  8.4548E-02 -5.2066E-02 -1.1783E-01 -7.9809E-02  9.7756E-02
 GRADIENT:  -6.6740E-01 -2.3767E+01 -4.9329E+01  1.2685E-01 -1.5652E-04 -2.9038E-02 -8.0112E-02  1.9898E-02 -3.9600E-01  3.0040E-01
             6.4132E-02 -3.6807E+00 -5.6103E-02  2.2472E-02 -4.8314E-01 -4.5082E-02
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      447
 NO. OF SIG. DIGITS IN FINAL EST.:  2.2

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -3.2742E-04 -9.1717E-05 -7.1640E-05  2.9698E-17 -1.7621E-16 -1.2275E-16
 SE:             2.2699E-03  3.3953E-03  3.3596E-03  4.2896E-02  3.9228E-02  5.5279E-02
 N:                     800         800         800          16          16          16
 
 P VAL.:         8.8531E-01  9.7845E-01  9.8299E-01  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   3.6584E+01  2.0740E+00  2.7145E+00  9.7958E-01  1.0000E-10  1.0000E-10
 EBVshrink(%):   3.5641E+01  2.0449E+00  2.6950E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.2336E+01
 
 #TERE:
 Elapsed estimation time in seconds:  1338.74
 Elapsed covariance time in seconds:  1585.52
 SKIPPING EXECUTION OF SLOW FNLMOD/FNLETA BY USER REQUEST
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -17078.416       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.80E-01 -5.31E+00 -3.08E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.05E-02
 
 ETA2
+        1.89E-04  9.81E-03
 
 ETA3
+        5.97E-04  6.70E-04  9.74E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  3.20E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -4.38E-03  2.51E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  2.75E-02 -5.15E-03  4.98E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.00E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.02E-01
 
 ETA2
+        1.87E-02  9.91E-02
 
 ETA3
+        5.92E-02  6.85E-02  9.87E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.79E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00 -1.55E-01  1.58E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  6.88E-01 -1.46E-01  2.23E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.48E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         5.63E-03  3.58E-03  3.59E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.19E-04
 
 ETA2
+        5.54E-04  5.19E-04
 
 ETA3
+        6.17E-04  3.70E-04  5.20E-04
 
 ETA4
+       ......... ......... .........  1.11E-02
 
 ETA5
+       ......... ......... .........  7.13E-03  8.87E-03
 
 ETA6
+       ......... ......... .........  1.14E-02  8.73E-03  1.67E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.47E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        4.49E-03
 
 ETA2
+        5.46E-02  2.62E-03
 
 ETA3
+        6.05E-02  3.74E-02  2.64E-03
 
 ETA4
+       ......... ......... .........  3.10E-02
 
 ETA5
+       ......... ......... .........  2.42E-01  2.80E-02
 
 ETA6
+       ......... ......... .........  1.29E-01  2.38E-01  3.74E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.00E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.17E-05
 
 TH 2
+        1.17E-06  1.28E-05
 
 TH 3
+        1.89E-06  1.47E-06  1.29E-05
 
 OM11
+        7.66E-08  4.49E-09  5.66E-09  8.45E-07
 
 OM12
+       -6.14E-08  4.31E-09 -1.11E-09  5.85E-08  3.07E-07
 
 OM13
+        1.15E-07  3.40E-09  9.96E-09  1.11E-07  4.05E-08  3.81E-07
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        7.74E-10  3.14E-09  2.61E-09  3.16E-09  2.49E-08  2.81E-09 ......... ......... .........  2.69E-07
 
 OM23
+        1.28E-09  4.74E-10  4.58E-10  4.48E-09  2.18E-08  1.42E-08 ......... ......... .........  3.10E-08  1.37E-07
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        9.38E-09 -2.59E-10  7.33E-10  6.86E-09  4.99E-09  3.98E-08 ......... ......... .........  3.51E-09  3.03E-08 .........
         ......... .........  2.71E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        2.04E-07  7.39E-09  5.09E-08 -6.30E-07 -1.58E-07 -3.37E-07 ......... ......... ......... -2.94E-08 -4.63E-08 .........
         ......... ......... -4.66E-08 ......... ......... .........  1.23E-04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+       -3.26E-08 -1.62E-08 -1.57E-08 -1.76E-07 -1.62E-07 -9.61E-08 ......... ......... ......... -4.48E-08 -2.86E-08 .........
         ......... ......... -2.86E-08 ......... ......... ......... -1.63E-05  5.08E-05
 
 OM46
+        1.99E-07  2.86E-09  5.07E-08 -4.10E-07 -1.29E-07 -3.16E-07 ......... ......... ......... -3.21E-08 -6.14E-08 .........
         ......... ......... -6.03E-08 ......... ......... .........  9.81E-05 -1.66E-05  1.30E-04
 
 OM55
+       -2.59E-08  2.02E-09 -1.09E-08 -2.72E-08 -7.79E-08  8.85E-11 ......... ......... ......... -5.10E-08 -2.89E-08 .........
         ......... ......... -2.23E-08 ......... ......... .........  5.96E-06 -1.62E-05  6.32E-06  7.86E-05
 
 OM56
+       -2.27E-09 -5.65E-09 -2.12E-09 -8.86E-08 -6.25E-08 -1.71E-07 ......... ......... ......... -5.72E-08 -2.60E-08 .........
         ......... ......... -4.38E-08 ......... ......... ......... -1.29E-05  4.21E-05 -2.21E-05 -1.66E-05  7.62E-05
 
 OM66
+        1.51E-07 -9.05E-09  4.48E-08 -2.16E-07 -1.49E-07 -3.47E-07 ......... ......... ......... -5.72E-08 -1.07E-07 .........
         ......... ......... -9.51E-08 ......... ......... .........  7.91E-05 -1.67E-05  1.48E-04  6.09E-06 -3.32E-05  2.78E-04
 
 SG11
+       -4.31E-10  2.90E-09  2.86E-09 -1.90E-09 -6.31E-10 -7.45E-10 ......... ......... ......... -4.33E-10 -4.61E-10 .........
         ......... ......... -5.13E-10 ......... ......... .........  8.91E-09  3.66E-09  8.30E-09  1.72E-09  3.96E-09  7.17E-09
         3.00E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        5.63E-03
 
 TH 2
+        5.83E-02  3.58E-03
 
 TH 3
+        9.33E-02  1.14E-01  3.59E-03
 
 OM11
+        1.48E-02  1.36E-03  1.71E-03  9.19E-04
 
 OM12
+       -1.97E-02  2.17E-03 -5.55E-04  1.15E-01  5.54E-04
 
 OM13
+        3.32E-02  1.54E-03  4.49E-03  1.95E-01  1.18E-01  6.17E-04
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.65E-04  1.69E-03  1.40E-03  6.62E-03  8.66E-02  8.76E-03 ......... ......... .........  5.19E-04
 
 OM23
+        6.17E-04  3.57E-04  3.44E-04  1.32E-02  1.06E-01  6.20E-02 ......... ......... .........  1.61E-01  3.70E-04
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        3.20E-03 -1.39E-04  3.92E-04  1.43E-02  1.73E-02  1.24E-01 ......... ......... .........  1.30E-02  1.57E-01 .........
         ......... .........  5.20E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+        3.27E-03  1.86E-04  1.28E-03 -6.17E-02 -2.56E-02 -4.92E-02 ......... ......... ......... -5.10E-03 -1.13E-02 .........
         ......... ......... -8.06E-03 ......... ......... .........  1.11E-02
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+       -8.12E-04 -6.33E-04 -6.15E-04 -2.69E-02 -4.10E-02 -2.18E-02 ......... ......... ......... -1.21E-02 -1.08E-02 .........
         ......... ......... -7.71E-03 ......... ......... ......... -2.05E-01  7.13E-03
 
 OM46
+        3.10E-03  7.01E-05  1.24E-03 -3.91E-02 -2.04E-02 -4.50E-02 ......... ......... ......... -5.43E-03 -1.46E-02 .........
         ......... ......... -1.02E-02 ......... ......... .........  7.76E-01 -2.05E-01  1.14E-02
 
 OM55
+       -5.19E-04  6.36E-05 -3.41E-04 -3.34E-03 -1.59E-02  1.62E-05 ......... ......... ......... -1.11E-02 -8.80E-03 .........
         ......... ......... -4.84E-03 ......... ......... .........  6.05E-02 -2.57E-01  6.26E-02  8.87E-03
 
 OM56
+       -4.63E-05 -1.81E-04 -6.77E-05 -1.10E-02 -1.29E-02 -3.17E-02 ......... ......... ......... -1.26E-02 -8.06E-03 .........
         ......... ......... -9.65E-03 ......... ......... ......... -1.33E-01  6.77E-01 -2.22E-01 -2.15E-01  8.73E-03
 
 OM66
+        1.61E-03 -1.52E-04  7.47E-04 -1.41E-02 -1.61E-02 -3.38E-02 ......... ......... ......... -6.62E-03 -1.73E-02 .........
         ......... ......... -1.10E-02 ......... ......... .........  4.27E-01 -1.40E-01  7.80E-01  4.12E-02 -2.28E-01  1.67E-02
 
 SG11
+       -1.40E-03  1.48E-02  1.45E-02 -3.77E-02 -2.08E-02 -2.20E-02 ......... ......... ......... -1.52E-02 -2.28E-02 .........
         ......... ......... -1.80E-02 ......... ......... .........  1.47E-02  9.38E-03  1.33E-02  3.54E-03  8.30E-03  7.86E-03
         5.47E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 TH 1
+        3.20E+04
 
 TH 2
+       -2.43E+03  7.91E+04
 
 TH 3
+       -4.39E+03 -8.64E+03  7.91E+04
 
 OM11
+       -2.16E+03 -2.18E+02 -1.20E+02  1.25E+06
 
 OM12
+        8.20E+03 -1.69E+03 -4.24E+02 -1.88E+05  3.40E+06
 
 OM13
+       -9.87E+03  3.78E+02 -7.20E+02 -3.39E+05 -2.89E+05  2.81E+06
 
 OM14
+       ......... ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -6.03E+02 -7.78E+02 -6.99E+02  5.70E+03 -2.54E+05  2.67E+04 ......... ......... .........  3.83E+06
 
 OM23
+       -4.16E+02 -4.29E+01 -1.08E+02  2.27E+04 -4.48E+05 -1.50E+05 ......... ......... ......... -8.36E+05  7.76E+06
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM33
+        3.07E+02  5.32E+01 -6.48E+01  2.14E+04  4.03E+04 -3.79E+05 ......... ......... .........  4.62E+04 -8.23E+05 .........
         ......... .........  3.84E+06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... .........
 
 OM44
+       -4.49E+01  3.02E+00 -1.10E+01  7.30E+03  4.55E+03  3.63E+03 ......... ......... .........  4.93E+02  8.47E+02 .........
         ......... ......... -9.30E+01 ......... ......... .........  2.60E+04
 
1

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24  
             OM25      OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66  
            SG11  
 
 OM45
+        4.05E+01  2.56E+01  3.16E+01  7.37E+03  1.69E+04 -1.50E+03 ......... ......... .........  9.73E+02  1.97E+03 .........
         ......... .........  6.88E+02 ......... ......... .........  3.65E+03  3.86E+04
 
 OM46
+       -5.30E+01  2.73E+00 -1.28E+01 -1.07E+03 -1.87E+03  1.39E+03 ......... ......... ......... -4.36E+02 -9.11E+02 .........
         ......... .........  4.49E+01 ......... ......... ......... -2.82E+04 -4.99E+01  5.10E+04
 
 OM55
+        2.04E+01 -9.82E-01  1.49E+01  7.69E+02  4.91E+03  3.21E+02 ......... ......... .........  2.66E+03  2.09E+03 .........
         ......... .........  1.23E+03 ......... ......... ......... -5.34E+01  3.48E+03 -1.31E+02  1.37E+04
 
 OM56
+       -5.49E+01 -2.99E+00 -1.80E+01 -2.88E+03 -6.01E+03  8.20E+03 ......... ......... .........  2.80E+03  1.19E+03 .........
         ......... .........  1.35E+03 ......... ......... ......... -2.49E+03 -2.05E+04  1.76E+03  1.08E+03  2.55E+04
 
 OM66
+        9.64E+00  5.19E+00 -9.46E-01 -9.80E+02  9.75E+02  2.02E+03 ......... ......... .........  8.04E+02  2.55E+03 .........
         ......... .........  7.39E+02 ......... ......... .........  7.60E+03 -1.21E+03 -1.90E+04  1.29E+02  1.57E+03  1.17E+04
 
 SG11
+        9.22E+03 -6.92E+04 -6.81E+04  6.53E+05  4.03E+05  3.09E+05 ......... ......... .........  3.82E+05  8.03E+05 .........
         ......... .........  4.60E+05 ......... ......... ......... -1.17E+04 -2.15E+04 -1.50E+04 -1.08E+04 -1.07E+04  2.02E+03
         3.35E+08
 
 #CPUT: Total CPU Time in Seconds,     5151.812
Stop Time: 
Sat 09/07/2013 
11:27 PM

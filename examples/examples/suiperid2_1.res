Sat 09/07/2013 
07:00 PM
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
$THETA 5.0 5
$OMEGA BLOCK(2)
0.03
0.00001 0.03

;$OMEGA 0.03 0.03
$OMEGA BLOCK(2)
0.1
0.00001 0.1

$OMEGA BLOCK(2)
0.3
0.00001 0.3

$SIGMA 
0.1

$LEVEL
SID=(3[1],4[2])
CID=(5[3],6[4])

$EST METHOD=ITS INTERACTION PRINT=1 NSIG=2 NITER=500 SIGL=6 FNLETA=0 NOABORT MCETA=2 CTYPE=3
$COV MATRIX=R UNCONDITIONAL
$TABLE  ID TIME CONC DOSE RATE EVID MDV CMT SID CID CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 
NOAPPEND ONEHEADER FILE=superid2_1.tab  NOPRINT
$TABLE  ID SID CID CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 NOAPPEND ONEHEADER FILE=superid2_1.dat FIRSTONLY NOPRINT FORMAT=,1PE15.8
  
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
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 CL V
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
                  0.3000E-01
                  0.1000E-04   0.3000E-01
        2                                                                                   NO
                  0.1000E+00
                  0.1000E-04   0.1000E+00
        3                                                                                   NO
                  0.3000E+00
                  0.1000E-04   0.3000E+00
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
 ID TIME CONC DOSE RATE EVID MDV CMT SID CID CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
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
 ID SID CID CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
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
 #METH: Iterative Two Stage
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            528
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  2           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    6           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   6           
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
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   315017.478338237
 iteration            1 OBJ=  -2085.34255666506
 iteration            2 OBJ=  -20208.3263305444
 iteration            3 OBJ=  -30383.3401833088
 iteration            4 OBJ=  -37819.8783950506
 iteration            5 OBJ=  -40914.5914360600
 iteration            6 OBJ=  -41155.3275947754
 iteration            7 OBJ=  -41169.4780668011
 iteration            8 OBJ=  -41170.5291246145
 iteration            9 OBJ=  -41170.6073777654
 iteration           10 OBJ=  -41170.6065024467
 iteration           11 OBJ=  -41170.6005930555
 iteration           12 OBJ=  -41170.5966338529
 iteration           13 OBJ=  -41170.5945099780
 Convergence achieved
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.0723E-05 -1.1176E-05  3.8969E-18 -6.4559E-18 -4.0483E-16 -5.2234E-16
 SE:             1.6652E-03  1.7457E-03  9.8883E-03  1.0710E-02  6.1688E-02  5.5549E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         9.9007E-01  9.9489E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.0849E+01  1.0620E+01  1.5889E-05  7.8170E-06  1.1965E-05  2.1937E-06
 EBVshrink(%):   1.0847E+01  1.0619E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.1906E+01
 
 #TERE:
 Elapsed estimation time in seconds:    93.76
 Elapsed covariance time in seconds:     0.80
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41170.595       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.00E+00  3.63E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.69E-03
 
 ETA2
+        3.59E-05  1.06E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.72E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.75E-03  3.19E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.91E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.69E-02  8.04E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.96E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.84E-02
 
 ETA2
+        3.55E-03  1.03E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.65E-01
 
 ETA4
+        0.00E+00  0.00E+00  5.96E-02  1.78E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.15E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.89E-01  2.83E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.98E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.35E-03  2.44E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        3.40E-04
 
 ETA2
+        2.55E-04  3.79E-04
 
 ETA3
+        0.00E+00  0.00E+00  2.28E-03
 
 ETA4
+        0.00E+00  0.00E+00  1.84E-03  2.72E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.17E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.42E-02  2.69E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.28E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.73E-03
 
 ETA2
+        2.52E-02  1.84E-03
 
 ETA3
+       ......... .........  6.90E-03
 
 ETA4
+       ......... .........  6.28E-02  7.63E-03
 
 ETA5
+       ......... ......... ......... .........  5.04E-02
 
 ETA6
+       ......... ......... ......... .........  2.46E-01  4.74E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.39E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        5.52E-06
 
 TH 2
+        3.20E-07  5.95E-06
 
 OM11
+       -5.86E-09  5.77E-08  1.16E-07
 
 OM12
+        6.68E-08  2.46E-08  8.30E-09  6.52E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.75E-09  1.59E-08 -2.11E-09 -4.96E-12  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.44E-07
 
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
+        6.35E-08  6.71E-08  1.78E-08  6.76E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.29E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.18E-06
 
 OM34
+       -2.03E-09  2.06E-08 -7.03E-09 -1.97E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.11E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.35E-07  3.39E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        4.11E-08  7.23E-09  1.68E-08  6.55E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.11E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.03E-07  1.07E-07  0.00E+00  0.00E+00  7.42E-06
 
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
+        2.21E-06  6.70E-08  1.91E-07  5.54E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.53E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.48E-06  2.44E-06  0.00E+00  0.00E+00  4.92E-06  0.00E+00  0.00E+00  1.01E-03
 
 OM56
+       -5.61E-07  1.86E-07 -8.03E-08  1.12E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.60E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.33E-06  1.78E-06  0.00E+00  0.00E+00 -1.19E-06  0.00E+00  0.00E+00 -4.98E-04  5.85E-04
 
 OM66
+        1.21E-06  9.55E-07  6.73E-08 -6.48E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.09E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.27E-07 -5.26E-06  0.00E+00  0.00E+00  8.05E-07  0.00E+00  0.00E+00  1.62E-04 -1.79E-04  7.23E-04
 
 SG11
+        1.51E-08  2.86E-09 -1.88E-09 -2.63E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.19E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.82E-09 -5.48E-10  0.00E+00  0.00E+00 -2.58E-09  0.00E+00  0.00E+00 -1.24E-07  2.80E-08 -2.03E-08  1.63E-08
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.35E-03
 
 TH 2
+        5.58E-02  2.44E-03
 
 OM11
+       -7.32E-03  6.96E-02  3.40E-04
 
 OM12
+        1.11E-01  3.95E-02  9.56E-02  2.55E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.33E-03  1.72E-02 -1.64E-02 -5.12E-05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.79E-04
 
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
+        1.19E-02  1.21E-02  2.30E-02  1.16E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.49E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.28E-03
 
 OM34
+       -4.70E-04  4.59E-03 -1.12E-02 -4.18E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.59E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -5.61E-02  1.84E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        6.42E-03  1.09E-03  1.82E-02  9.42E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.07E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.66E-02  2.14E-02  0.00E+00  0.00E+00  2.72E-03
 
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
+        2.96E-02  8.66E-04  1.77E-02  6.84E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.11E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.59E-02  4.18E-02  0.00E+00  0.00E+00  5.70E-02  0.00E+00  0.00E+00  3.17E-02
 
 OM56
+       -9.86E-03  3.15E-03 -9.75E-03  1.82E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.74E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.86E-02  4.00E-02  0.00E+00  0.00E+00 -1.80E-02  0.00E+00  0.00E+00 -6.48E-01  2.42E-02
 
 OM66
+        1.92E-02  1.46E-02  7.36E-03 -9.44E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.01E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.19E-02 -1.06E-01  0.00E+00  0.00E+00  1.10E-02  0.00E+00  0.00E+00  1.89E-01 -2.74E-01  2.69E-02
 
 SG11
+        5.05E-02  9.20E-03 -4.34E-02 -8.08E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.53E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  6.28E-03 -2.33E-03  0.00E+00  0.00E+00 -7.42E-03  0.00E+00  0.00E+00 -3.07E-02  9.08E-03 -5.92E-03  1.28E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        1.85E+05
 
 TH 2
+       -9.20E+03  1.70E+05
 
 OM11
+        2.56E+04 -8.26E+04  8.79E+06
 
 OM12
+       -1.89E+05 -4.46E+04 -1.10E+06  1.57E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -9.23E+03 -2.10E+04  1.45E+05 -9.90E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.99E+06
 
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
+       -1.76E+03 -2.04E+03 -2.66E+04 -1.08E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.68E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.96E+05
 
 OM34
+       -9.03E+02 -1.89E+03  1.10E+04  9.77E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.91E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.44E+04  3.03E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -6.67E+02  7.34E+01 -1.82E+04 -1.05E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.21E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.16E+03 -3.44E+03  0.00E+00  0.00E+00  1.36E+05
 
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
+       -5.14E+02 -7.29E-01 -1.23E+03 -3.80E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.00E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.18E+02 -2.14E+03  0.00E+00  0.00E+00 -8.78E+02  0.00E+00  0.00E+00  1.74E+03
 
 OM56
+       -3.11E+02 -1.56E+02 -2.19E+01 -6.41E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.79E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.02E+03 -1.98E+03  0.00E+00  0.00E+00 -4.65E+02  0.00E+00  0.00E+00  1.47E+03  3.10E+03
 
 OM66
+       -2.97E+02 -2.72E+02 -3.97E+02  1.87E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.14E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.26E+02  2.21E+03  0.00E+00  0.00E+00 -8.41E+01  0.00E+00  0.00E+00 -3.94E+01  4.27E+02  1.52E+03
 
 SG11
+       -1.75E+05 -3.43E+04  1.00E+06  2.97E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.91E+05  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.63E+04 -6.84E+02  0.00E+00  0.00E+00  1.46E+04  0.00E+00  0.00E+00  1.13E+04  6.87E+03  1.70E+03  6.19E+07
 
 SKIPPING EXECUTION OF SLOW FNLMOD/FNLETA BY USER REQUEST
 #CPUT: Total CPU Time in Seconds,       94.719
Stop Time: 
Sat 09/07/2013 
07:02 PM

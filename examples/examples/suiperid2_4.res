Sat 09/07/2013 
07:36 PM
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

$EST METHOD=ITS INTERACTION PRINT=1 NSIG=3 NITER=12 SIGL=6 FNLETA=0 MCETA=3 NOABORT
$EST METHOD=1 INTERACTION PRINT=1 MAXEVAL=9999 NSIG=2 FNLETA=0 NOHABORT  SIGL=10 MCETA=10 SLOW
$COV MATRIX=R UNCONDITIONAL
$TABLE  ID TIME CONC DOSE RATE EVID MDV CMT SID CID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
NOAPPEND ONEHEADER FILE=superid2_4.tab  NOPRINT
  
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
 #METH: Iterative Two Stage
 
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
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  3           
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
 CONVERGENCE TYPE (CTYPE):                0           
 ITERATIONS (NITER):                      12          
 
 
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

 iteration            0 OBJ=   101575.529294619
 iteration            1 OBJ=  -12307.2709437705
 iteration            2 OBJ=  -20409.1345859530
 iteration            3 OBJ=  -28159.2447782346
 iteration            4 OBJ=  -34873.9533570576
 iteration            5 OBJ=  -39811.0190034239
 iteration            6 OBJ=  -41005.2216923696
 iteration            7 OBJ=  -41166.7958422211
 iteration            8 OBJ=  -41170.5748764954
 iteration            9 OBJ=  -41170.6549852522
 iteration           10 OBJ=  -41170.6305055248
 iteration           11 OBJ=  -41170.6114191167
 iteration           12 OBJ=  -41170.6014305743
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.3812E-05 -1.3080E-05  9.2149E-19 -3.8886E-18 -3.6806E-16  5.3443E-16
 SE:             1.6652E-03  1.7457E-03  9.8883E-03  1.0710E-02  6.1688E-02  5.5549E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         9.8859E-01  9.9402E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.0852E+01  1.0621E+01  4.3858E-05  2.0085E-05  2.7773E-05  1.0000E-10
 EBVshrink(%):   1.0846E+01  1.0619E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.1907E+01
 
 #TERE:
 Elapsed estimation time in seconds:    92.20
 Elapsed covariance time in seconds:     0.88
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41170.601       **************************************************
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
+        3.80E-05  1.06E-02
 
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
+        3.75E-03  1.03E-01
 
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
+        5.53E-06
 
 TH 2
+        3.22E-07  5.95E-06
 
 OM11
+       -5.79E-09  5.78E-08  1.16E-07
 
 OM12
+        6.69E-08  2.47E-08  8.35E-09  6.53E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.77E-09  1.60E-08 -2.11E-09  3.64E-11  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.44E-07
 
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
+       -2.04E-09  2.06E-08 -7.03E-09 -1.97E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.11E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.35E-07  3.39E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        4.11E-08  7.24E-09  1.69E-08  6.55E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.11E-08  0.00E+00  0.00E+00  0.00E+00
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
+        2.21E-06  6.73E-08  1.91E-07  5.54E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.53E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.48E-06  2.44E-06  0.00E+00  0.00E+00  4.92E-06  0.00E+00  0.00E+00  1.01E-03
 
 OM56
+       -5.61E-07  1.86E-07 -8.02E-08  1.12E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.60E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.33E-06  1.78E-06  0.00E+00  0.00E+00 -1.19E-06  0.00E+00  0.00E+00 -4.98E-04  5.85E-04
 
 OM66
+        1.22E-06  9.56E-07  6.73E-08 -6.49E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.09E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.28E-07 -5.26E-06  0.00E+00  0.00E+00  8.05E-07  0.00E+00  0.00E+00  1.62E-04 -1.79E-04  7.23E-04
 
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
+        5.61E-02  2.44E-03
 
 OM11
+       -7.24E-03  6.96E-02  3.40E-04
 
 OM12
+        1.11E-01  3.96E-02  9.60E-02  2.55E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.35E-03  1.73E-02 -1.63E-02  3.76E-04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.79E-04
 
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
+       -4.70E-04  4.59E-03 -1.12E-02 -4.18E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.58E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -5.61E-02  1.84E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        6.42E-03  1.09E-03  1.82E-02  9.41E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.07E-02  0.00E+00  0.00E+00  0.00E+00
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
+        2.96E-02  8.69E-04  1.77E-02  6.83E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.11E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.59E-02  4.18E-02  0.00E+00  0.00E+00  5.70E-02  0.00E+00  0.00E+00  3.17E-02
 
 OM56
+       -9.86E-03  3.15E-03 -9.74E-03  1.82E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.74E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.86E-02  4.00E-02  0.00E+00  0.00E+00 -1.80E-02  0.00E+00  0.00E+00 -6.48E-01  2.42E-02
 
 OM66
+        1.92E-02  1.46E-02  7.35E-03 -9.45E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.01E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.19E-02 -1.06E-01  0.00E+00  0.00E+00  1.10E-02  0.00E+00  0.00E+00  1.89E-01 -2.74E-01  2.69E-02
 
 SG11
+        5.05E-02  9.20E-03 -4.34E-02 -8.06E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.53E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  6.28E-03 -2.33E-03  0.00E+00  0.00E+00 -7.42E-03  0.00E+00  0.00E+00 -3.07E-02  9.07E-03 -5.92E-03  1.28E-04
 
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
+       -9.25E+03  1.70E+05
 
 OM11
+        2.56E+04 -8.26E+04  8.79E+06
 
 OM12
+       -1.89E+05 -4.46E+04 -1.11E+06  1.57E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -9.18E+03 -2.10E+04  1.45E+05 -1.45E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.99E+06
 
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
+       -9.03E+02 -1.89E+03  1.10E+04  9.77E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.92E+04  0.00E+00  0.00E+00  0.00E+00
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
+       -5.14E+02 -6.46E-01 -1.23E+03 -3.80E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.00E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.18E+02 -2.14E+03  0.00E+00  0.00E+00 -8.78E+02  0.00E+00  0.00E+00  1.74E+03
 
 OM56
+       -3.11E+02 -1.56E+02 -2.12E+01 -6.41E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.79E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.02E+03 -1.98E+03  0.00E+00  0.00E+00 -4.65E+02  0.00E+00  0.00E+00  1.47E+03  3.10E+03
 
 OM66
+       -2.97E+02 -2.72E+02 -3.97E+02  1.87E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.14E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.26E+02  2.21E+03  0.00E+00  0.00E+00 -8.41E+01  0.00E+00  0.00E+00 -3.94E+01  4.27E+02  1.52E+03
 
 SG11
+       -1.75E+05 -3.42E+04  1.00E+06  2.95E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.90E+05  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.63E+04 -6.91E+02  0.00E+00  0.00E+00  1.46E+04  0.00E+00  0.00E+00  1.13E+04  6.87E+03  1.70E+03  6.19E+07
 
1
 
 
 #TBLN:      2
 #METH: First Order Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            9999
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  10          
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
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -41170.6014762249        NO. OF FUNC. EVALS.:  13
 CUMULATIVE NO. OF FUNC. EVALS.:       13
 NPARAMETR:  2.0021E+00  3.6307E+00  9.6922E-03  3.7978E-05  1.0597E-02  2.7161E-02  1.7524E-03  3.1861E-02  9.9101E-02 -1.6877E-02
             8.0357E-02  9.9579E-03
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:   6.0730E+03 -1.3021E+04 -5.2081E+01 -6.3009E-01 -4.9948E+01  2.1797E+00 -9.2165E-01  1.0313E+01  6.1580E+00 -9.9515E+00
             2.2690E+01 -2.4894E+01
 
0ITERATION NO.:    1    OBJECTIVE VALUE:  -41171.0276496825        NO. OF FUNC. EVALS.:  19
 CUMULATIVE NO. OF FUNC. EVALS.:       32
 NPARAMETR:  2.0016E+00  3.6326E+00  9.6922E-03  3.7978E-05  1.0597E-02  2.7161E-02  1.7524E-03  3.1861E-02  9.9101E-02 -1.6877E-02
             8.0357E-02  9.9579E-03
 PARAMETER:  9.9975E-02  1.0005E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:   3.5545E+03  1.7793E+03 -5.4346E+01 -1.7679E+00 -5.1506E+01  8.8114E-01 -1.7020E+00  9.3263E+00  4.8695E+00 -1.0469E+01
             2.1695E+01 -2.9704E+01
 
0ITERATION NO.:    2    OBJECTIVE VALUE:  -41171.1030948601        NO. OF FUNC. EVALS.:  20
 CUMULATIVE NO. OF FUNC. EVALS.:       52
 NPARAMETR:  2.0008E+00  3.6324E+00  9.6922E-03  3.7978E-05  1.0597E-02  2.7161E-02  1.7524E-03  3.1861E-02  9.9101E-02 -1.6876E-02
             8.0357E-02  9.9579E-03
 PARAMETER:  9.9935E-02  1.0005E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:   4.3887E+01  1.4050E+02 -5.4122E+01 -1.3984E+00 -5.1388E+01  1.0686E+00 -1.3343E+00  9.4719E+00  4.7949E+00 -9.9599E+00
             2.1961E+01 -2.4306E+01
 
0ITERATION NO.:    3    OBJECTIVE VALUE:  -41171.5698004142        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:       68
 NPARAMETR:  2.0009E+00  3.6325E+00  9.8387E-03  3.8337E-05  1.0749E-02  2.7153E-02  1.7553E-03  3.1778E-02  9.8969E-02 -1.6633E-02
             7.9807E-02  1.0025E-02
 PARAMETER:  9.9940E-02  1.0005E-01  1.0750E-01  1.0019E-01  1.0712E-01  9.9850E-02  1.0018E-01  9.8685E-02  9.9334E-02 -9.8621E-02
             9.6953E-02  1.0335E-01
 GRADIENT:   1.6730E+02  3.7845E+02 -2.9492E+01 -1.3459E+00 -2.8893E+01  1.7395E+00 -7.1738E-01  7.1757E+00  5.5516E+00 -2.4893E+00
             1.6939E+01  1.5214E+02
 
0ITERATION NO.:    4    OBJECTIVE VALUE:  -41172.0964584955        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:       83
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0161E-02  3.9136E-05  1.1085E-02  2.7131E-02  1.7615E-03  3.1591E-02  9.8644E-02 -1.6142E-02
             7.8597E-02  9.9340E-03
 PARAMETER:  9.9934E-02  1.0005E-01  1.2363E-01  1.0064E-01  1.2250E-01  9.9443E-02  1.0057E-01  9.5714E-02  9.7693E-02 -9.5866E-02
             9.0048E-02  9.8799E-02
 GRADIENT:   3.6039E+02  6.8617E+02  1.6550E+01 -1.0283E+00  1.2504E+01  2.0236E+00  1.5796E-01  1.3534E+00  5.9959E+00  1.1904E+01
             4.8446E+00 -6.2182E+01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -41172.1295369830        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      100
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0152E-02  3.9265E-05  1.1095E-02  2.7093E-02  1.7607E-03  3.1517E-02  9.8234E-02 -1.6538E-02
             7.8261E-02  9.9344E-03
 PARAMETER:  9.9933E-02  1.0005E-01  1.2317E-01  1.0102E-01  1.2295E-01  9.8743E-02  1.0060E-01  9.4536E-02  9.5607E-02 -9.8423E-02
             8.6886E-02  9.8821E-02
 GRADIENT:   3.3413E+02  5.1664E+02  1.4845E+01 -8.9159E-01  1.3055E+01  1.3275E-01 -6.4003E-01 -1.4543E+00  4.4117E-01  1.8380E+00
            -9.8244E-01 -6.0862E+01
 
0ITERATION NO.:    6    OBJECTIVE VALUE:  -41172.1312868245        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      117
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0146E-02  3.9354E-05  1.1092E-02  2.7091E-02  1.7647E-03  3.1511E-02  9.8208E-02 -1.6435E-02
             7.8108E-02  9.9345E-03
 PARAMETER:  9.9933E-02  1.0005E-01  1.2290E-01  1.0128E-01  1.2281E-01  9.8706E-02  1.0083E-01  9.4422E-02  9.5474E-02 -9.7827E-02
             8.6093E-02  9.8826E-02
 GRADIENT:   3.2832E+02  5.0794E+02  1.3661E+01 -1.0672E+00  1.2045E+01 -3.4344E-01 -9.3574E-01 -2.6779E+00 -6.1720E-01  4.2358E+00
            -3.4077E+00 -6.1212E+01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:  -41172.1326077039        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      133
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0134E-02  4.0385E-05  1.1098E-02  2.7126E-02  1.8132E-03  3.1621E-02  9.8404E-02 -1.6472E-02
             7.7833E-02  9.9346E-03
 PARAMETER:  9.9934E-02  1.0005E-01  1.2228E-01  1.0400E-01  1.2310E-01  9.9361E-02  1.0354E-01  9.6073E-02  9.6475E-02 -9.7946E-02
             8.4225E-02  9.8830E-02
 GRADIENT:   3.4777E+02  6.2706E+02  1.2494E+01 -6.4922E-01  1.3076E+01  1.3660E+00  1.6527E-01 -2.1595E+00  3.0720E+00  6.0129E+00
            -4.8146E+00 -6.0661E+01
 
0ITERATION NO.:    8    OBJECTIVE VALUE:  -41172.1352667090        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      149
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0142E-02  4.1328E-05  1.1087E-02  2.7108E-02  1.8478E-03  3.1720E-02  9.8216E-02 -1.6420E-02
             7.7753E-02  9.9347E-03
 PARAMETER:  9.9934E-02  1.0005E-01  1.2266E-01  1.0638E-01  1.2261E-01  9.9021E-02  1.0555E-01  9.7570E-02  9.5516E-02 -9.7731E-02
             8.3767E-02  9.8835E-02
 GRADIENT:   3.6417E+02  7.1888E+02  1.2650E+01 -1.5298E+00  1.0933E+01 -3.7339E-01 -3.2336E-01 -2.1656E+00 -4.1375E-01  6.7809E+00
            -5.4428E+00 -6.1395E+01
 
0ITERATION NO.:    9    OBJECTIVE VALUE:  -41172.1352702049        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      166
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0142E-02  4.1440E-05  1.1087E-02  2.7107E-02  1.8502E-03  3.1722E-02  9.8201E-02 -1.6415E-02
             7.7753E-02  9.9347E-03
 PARAMETER:  9.9934E-02  1.0005E-01  1.2267E-01  1.0667E-01  1.2259E-01  9.9007E-02  1.0568E-01  9.7595E-02  9.5443E-02 -9.7709E-02
             8.3777E-02  9.8835E-02
 GRADIENT:   3.6608E+02  7.2793E+02  1.3031E+01 -9.8406E-01  1.1163E+01 -7.9708E-02 -1.0311E-01 -1.8104E+00 -4.5329E-01  7.1067E+00
            -4.9503E+00 -6.0965E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -41172.1352706023        NO. OF FUNC. EVALS.:  21
 CUMULATIVE NO. OF FUNC. EVALS.:      187
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0142E-02  4.1440E-05  1.1087E-02  2.7107E-02  1.8502E-03  3.1722E-02  9.8201E-02 -1.6415E-02
             7.7753E-02  9.9347E-03
 PARAMETER:  9.9934E-02  1.0005E-01  1.2267E-01  1.0667E-01  1.2259E-01  9.9006E-02  1.0569E-01  9.7595E-02  9.5443E-02 -9.7709E-02
             8.3777E-02  9.8835E-02
 GRADIENT:   3.6641E+02  7.2837E+02  1.3427E+01 -6.9762E-01  1.1668E+01  3.8942E-01  3.2620E-01 -1.4540E+00  2.5368E-01  7.9426E+00
            -4.3208E+00 -6.0409E+01
 
0ITERATION NO.:   11    OBJECTIVE VALUE:  -41172.1352746397        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      204
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0142E-02  4.1464E-05  1.1086E-02  2.7101E-02  1.8508E-03  3.1720E-02  9.8211E-02 -1.6417E-02
             7.7753E-02  9.9347E-03
 PARAMETER:  9.9934E-02  1.0005E-01  1.2269E-01  1.0673E-01  1.2256E-01  9.8903E-02  1.0573E-01  9.7569E-02  9.5492E-02 -9.7716E-02
             8.3774E-02  9.8836E-02
 GRADIENT:   3.6668E+02  7.2852E+02  1.3335E+01 -7.8459E-01  1.1488E+01  2.5004E-01  4.2651E-01 -1.4161E+00  1.8319E-01  7.7381E+00
            -4.6587E+00 -6.0508E+01
 
0ITERATION NO.:   12    OBJECTIVE VALUE:  -41172.1355234068        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      219
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0133E-02  4.1574E-05  1.1097E-02  2.7049E-02  1.8481E-03  3.1736E-02  9.8231E-02 -1.6402E-02
             7.7764E-02  9.9349E-03
 PARAMETER:  9.9934E-02  1.0005E-01  1.2226E-01  1.0706E-01  1.2302E-01  9.7947E-02  1.0568E-01  9.7814E-02  9.5595E-02 -9.7620E-02
             8.3886E-02  9.8842E-02
 GRADIENT:   3.6498E+02  7.2024E+02  1.1868E+01 -7.4850E-01  1.2742E+01 -4.4555E-01  4.4394E-02 -1.2114E+00 -5.1204E-01  7.6297E+00
            -4.3577E+00 -6.0224E+01
 
0ITERATION NO.:   13    OBJECTIVE VALUE:  -41172.1611943240        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      233
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0122E-02  4.2719E-05  1.1064E-02  2.6822E-02  1.8833E-03  3.1521E-02  9.8545E-02 -1.6471E-02
             7.8063E-02  9.9401E-03
 PARAMETER:  9.9934E-02  1.0005E-01  1.2172E-01  1.1007E-01  1.2156E-01  9.3728E-02  1.0815E-01  9.4318E-02  9.7191E-02 -9.7870E-02
             8.5781E-02  9.9106E-02
 GRADIENT:   3.2360E+02  6.3336E+02  1.0481E+01 -1.0528E+00  8.4802E+00 -2.1754E+00  3.0922E-01 -2.9001E+00  4.8034E-01  6.6344E+00
            -3.7589E+00 -4.8313E+01
 
0ITERATION NO.:   14    OBJECTIVE VALUE:  -41172.2193377176        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      248
 NPARAMETR:  2.0008E+00  3.6325E+00  1.0058E-02  4.1558E-05  1.1028E-02  2.7144E-02  1.9495E-03  3.1506E-02  9.8036E-02 -1.6585E-02
             7.8377E-02  9.9533E-03
 PARAMETER:  9.9933E-02  1.0005E-01  1.1850E-01  1.0742E-01  1.1993E-01  9.9700E-02  1.1128E-01  9.3947E-02  9.4599E-02 -9.8805E-02
             8.7508E-02  9.9768E-02
 GRADIENT:   1.4351E+02  3.2951E+02  1.0367E+00 -7.5471E-01  4.7464E+00 -1.8232E-01  8.3485E-01 -1.2868E+00 -2.8503E+00  4.7171E+00
            -3.5916E-01 -1.6295E+01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -41172.2232012310        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      262
 NPARAMETR:  2.0007E+00  3.6324E+00  1.0064E-02  4.3279E-05  1.0951E-02  2.6979E-02  1.7897E-03  3.1964E-02  9.8919E-02 -1.6610E-02
             7.8150E-02  9.9658E-03
 PARAMETER:  9.9931E-02  1.0005E-01  1.1883E-01  1.1183E-01  1.1643E-01  9.6635E-02  1.0247E-01  1.0153E-01  9.9084E-02 -9.8511E-02
             8.6120E-02  1.0040E-01
 GRADIENT:  -1.0096E+02 -1.5757E+02  3.2537E+00 -8.1522E-01 -4.9713E+00  7.8937E-01 -4.9331E-01  2.6943E+00  6.6145E+00  1.7660E+00
             1.1732E+00  1.4628E+01
 
0ITERATION NO.:   16    OBJECTIVE VALUE:  -41172.2373469267        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      278
 NPARAMETR:  2.0007E+00  3.6324E+00  1.0052E-02  4.2928E-05  1.0980E-02  2.7042E-02  1.8790E-03  3.1706E-02  9.8505E-02 -1.6660E-02
             7.8337E-02  9.9616E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1823E-01  1.1099E-01  1.1775E-01  9.7813E-02  1.0746E-01  9.7267E-02  9.6984E-02 -9.9015E-02
             8.7171E-02  1.0019E-01
 GRADIENT:  -1.8127E+01  1.9295E+00  2.4607E-01 -1.3708E+00 -1.7500E+00 -7.0992E-01 -5.2316E-01 -1.0173E-01  1.1091E+00  1.0925E+00
             4.0418E-02  3.3343E+00
 
0ITERATION NO.:   17    OBJECTIVE VALUE:  -41172.2378880980        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      292
 NPARAMETR:  2.0007E+00  3.6324E+00  1.0049E-02  4.3266E-05  1.0987E-02  2.7088E-02  1.9018E-03  3.1653E-02  9.8347E-02 -1.6707E-02
             7.8347E-02  9.9601E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1806E-01  1.1189E-01  1.1808E-01  9.8652E-02  1.0867E-01  9.6387E-02  9.6183E-02 -9.9373E-02
             8.7095E-02  1.0011E-01
 GRADIENT:   6.6133E+00  4.1291E+01 -4.0808E-01 -1.1565E+00 -8.6996E-01 -6.6691E-01 -5.8908E-01 -9.2414E-01 -2.4748E-01  2.6160E-01
            -3.8237E-01 -4.6012E-01
 
0ITERATION NO.:   18    OBJECTIVE VALUE:  -41172.2386288707        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      307
 NPARAMETR:  2.0007E+00  3.6324E+00  1.0046E-02  4.3857E-05  1.0991E-02  2.7103E-02  1.9048E-03  3.1667E-02  9.8272E-02 -1.6746E-02
             7.8307E-02  9.9595E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1795E-01  1.1342E-01  1.1824E-01  9.8932E-02  1.0881E-01  9.6598E-02  9.5801E-02 -9.9646E-02
             8.6731E-02  1.0008E-01
 GRADIENT:   9.2889E+00  2.6549E+01 -9.9653E-01 -1.2299E+00 -5.4157E-01 -7.4365E-01 -5.4459E-01 -7.7151E-01 -1.3061E+00 -7.3961E-01
            -6.3774E-01 -2.0285E+00
 
0ITERATION NO.:   19    OBJECTIVE VALUE:  -41172.2388895804        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      321
 NPARAMETR:  2.0007E+00  3.6324E+00  1.0046E-02  4.4496E-05  1.0992E-02  2.7092E-02  1.9028E-03  3.1674E-02  9.8258E-02 -1.6771E-02
             7.8266E-02  9.9592E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1791E-01  1.1508E-01  1.1828E-01  9.8733E-02  1.0872E-01  9.6723E-02  9.5732E-02 -9.9799E-02
             8.6400E-02  1.0007E-01
 GRADIENT:   1.0372E+01  1.9860E+01 -9.1941E-01 -1.0115E+00 -3.7209E-01 -8.1346E-01 -6.8344E-01 -6.9839E-01 -1.3306E+00 -1.2058E+00
            -1.1384E+00 -2.4613E+00
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -41172.2388899130        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      338
 NPARAMETR:  2.0007E+00  3.6324E+00  1.0046E-02  4.4497E-05  1.0992E-02  2.7092E-02  1.9028E-03  3.1674E-02  9.8258E-02 -1.6771E-02
             7.8266E-02  9.9592E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1791E-01  1.1508E-01  1.1828E-01  9.8732E-02  1.0872E-01  9.6723E-02  9.5732E-02 -9.9800E-02
             8.6400E-02  1.0007E-01
 GRADIENT:   1.1114E+01  2.0205E+01 -4.5498E-01 -4.8739E-01  2.2149E-01 -1.7956E-01 -1.1259E-01 -4.9768E-02 -6.8899E-01 -4.6029E-01
            -6.7331E-01 -1.8002E+00
 
0ITERATION NO.:   21    OBJECTIVE VALUE:  -41172.2388899130        NO. OF FUNC. EVALS.:  24
 CUMULATIVE NO. OF FUNC. EVALS.:      362
 NPARAMETR:  2.0007E+00  3.6324E+00  1.0046E-02  4.4497E-05  1.0992E-02  2.7092E-02  1.9028E-03  3.1674E-02  9.8258E-02 -1.6771E-02
             7.8266E-02  9.9592E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1791E-01  1.1508E-01  1.1828E-01  9.8732E-02  1.0872E-01  9.6723E-02  9.5732E-02 -9.9800E-02
             8.6400E-02  1.0007E-01
 GRADIENT:  -3.3198E+01 -1.1268E+02 -1.2354E+00 -3.6970E-01 -4.2809E-01 -9.2214E-01  6.2876E-01 -1.4163E+00 -1.1958E+00 -8.4162E-01
            -8.4153E-01 -2.6720E+00
 
0ITERATION NO.:   22    OBJECTIVE VALUE:  -41172.2417558602        NO. OF FUNC. EVALS.:  28
 CUMULATIVE NO. OF FUNC. EVALS.:      390
 NPARAMETR:  2.0008E+00  3.6324E+00  1.0051E-02  4.3806E-05  1.0993E-02  2.7135E-02  1.8722E-03  3.1781E-02  9.8294E-02 -1.6716E-02
             7.8187E-02  9.9597E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1817E-01  1.1327E-01  1.1833E-01  9.9532E-02  1.0689E-01  9.8480E-02  9.5912E-02 -9.9457E-02
             8.6008E-02  1.0009E-01
 GRADIENT:  -1.9048E+00 -1.1200E+01 -3.3175E-01 -3.7071E-01 -1.9579E-01 -2.1412E-01  5.7507E-01 -4.4822E-01 -1.0516E-01 -4.4488E-02
            -4.3675E-01 -1.3109E+00
 
0ITERATION NO.:   23    OBJECTIVE VALUE:  -41172.2423101732        NO. OF FUNC. EVALS.:  26
 CUMULATIVE NO. OF FUNC. EVALS.:      416
 NPARAMETR:  2.0008E+00  3.6324E+00  1.0052E-02  4.3660E-05  1.0994E-02  2.7131E-02  1.8528E-03  3.1835E-02  9.8340E-02 -1.6689E-02
             7.8158E-02  9.9601E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1824E-01  1.1288E-01  1.1837E-01  9.9451E-02  1.0579E-01  9.9376E-02  9.6148E-02 -9.9273E-02
             8.5883E-02  1.0011E-01
 GRADIENT:   3.6911E+00  6.9425E+00 -3.2037E-02 -3.6834E-01 -1.6725E-02 -7.4354E-02  5.3388E-01  1.0140E-01  4.7088E-01  2.3708E-01
            -9.5515E-02 -2.1518E-01
 
0ITERATION NO.:   24    OBJECTIVE VALUE:  -41172.2423101732        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      416
 NPARAMETR:  2.0008E+00  3.6324E+00  1.0052E-02  4.3660E-05  1.0994E-02  2.7131E-02  1.8528E-03  3.1835E-02  9.8340E-02 -1.6689E-02
             7.8158E-02  9.9601E-03
 PARAMETER:  9.9932E-02  1.0005E-01  1.1824E-01  1.1288E-01  1.1837E-01  9.9451E-02  1.0579E-01  9.9376E-02  9.6148E-02 -9.9273E-02
             8.5883E-02  1.0011E-01
 GRADIENT:   3.6911E+00  6.9425E+00 -3.2037E-02 -3.6834E-01 -1.6725E-02 -7.4354E-02  5.3388E-01  1.0140E-01  4.7088E-01  2.3708E-01
            -9.5515E-02 -2.1518E-01
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      416
 NO. OF SIG. DIGITS IN FINAL EST.:  2.3

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.0816E-03 -1.4311E-03 -2.1760E-18 -1.5987E-18 -5.6390E-16  6.3630E-16
 SE:             1.6763E-03  1.7588E-03  9.8879E-03  1.0706E-02  6.1688E-02  5.5532E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         5.1880E-01  4.1581E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.1879E+01  1.1593E+01  1.0000E-10  1.0000E-10  1.0000E-10  1.0000E-10
 EBVshrink(%):   1.0570E+01  1.0306E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.2069E+01
 
 #TERE:
 Elapsed estimation time in seconds:  2205.62
0R MATRIX ALGORITHMICALLY NON-POSITIVE-SEMIDEFINITE
 BUT NONSINGULAR
0COVARIANCE STEP ABORTED
 Elapsed covariance time in seconds:  1574.79
 SKIPPING EXECUTION OF SLOW FNLMOD/FNLETA BY USER REQUEST
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41172.242       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.00E+00  3.63E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.01E-02
 
 ETA2
+        4.37E-05  1.10E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.71E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.85E-03  3.18E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.83E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.67E-02  7.82E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.96E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.00E-01
 
 ETA2
+        4.15E-03  1.05E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.65E-01
 
 ETA4
+        0.00E+00  0.00E+00  6.30E-02  1.78E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.14E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.90E-01  2.80E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.98E-02
 
 #CPUT: Total CPU Time in Seconds,     3849.531
Stop Time: 
Sat 09/07/2013 
08:40 PM

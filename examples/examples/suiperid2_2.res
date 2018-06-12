Sat 09/07/2013 
07:02 PM
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
0.03
0.00001 0.03

$OMEGA BLOCK(2)
0.03
0.00001 0.03

$OMEGA BLOCK(2)
0.03
0.00001 0.03

$SIGMA 
0.05

$LEVEL
SID=(3[1],4[2])
CID=(5[3],6[4])

$EST METHOD=ITS INTERACTION PRINT=1 NSIG=3 NITER=4 SIGL=6 FNLETA=0 MCETA=3
$EST METHOD=IMP INTERACTION PRINT=1 NSIG=3 NITER=50 CTYPE=3 ISAMPLE=300 SIGL=6 FNLETA=0 NOABORT MCETA=3
$COV MATRIX=R UNCONDITIONAL
$TABLE  ID TIME CONC DOSE RATE EVID MDV CMT SID CID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
NOAPPEND ONEHEADER FILE=superid2_2.tab  NOPRINT
  
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
                  0.3000E-01
                  0.1000E-04   0.3000E-01
        2                                                                                   NO
                  0.3000E-01
                  0.1000E-04   0.3000E-01
        3                                                                                   NO
                  0.3000E-01
                  0.1000E-04   0.3000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.5000E-01
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
 ITERATIONS (NITER):                      4           
 
 
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

 iteration            0 OBJ=   451071.104563563
 iteration            1 OBJ=  -14995.1065534362
 iteration            2 OBJ=  -35873.4881807928
 iteration            3 OBJ=  -40830.1012968019
 iteration            4 OBJ=  -41154.7947787274
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -6.5764E-03 -6.2026E-03 -7.7605E-18  1.2682E-17  3.5356E-16  3.4475E-16
 SE:             1.6775E-03  1.7489E-03  9.8842E-03  1.0705E-02  6.1663E-02  5.5525E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         8.8416E-05  3.9048E-04  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.1962E+01  1.1368E+01  1.0000E-10  1.0000E-10  1.0000E-10  1.0000E-10
 EBVshrink(%):   1.0622E+01  1.0626E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.2457E+01
 
 #TERE:
 Elapsed estimation time in seconds:    40.00
 Elapsed covariance time in seconds:     0.81
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41154.795       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.01E+00  3.64E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.01E-02
 
 ETA2
+        6.83E-04  1.08E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.71E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.82E-03  3.18E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.90E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.67E-02  8.02E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.01E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.00E-01
 
 ETA2
+        6.54E-02  1.04E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.65E-01
 
 ETA4
+        0.00E+00  0.00E+00  6.20E-02  1.78E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.15E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.87E-01  2.83E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.46E-03  2.52E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        3.65E-04
 
 ETA2
+        2.71E-04  3.94E-04
 
 ETA3
+        0.00E+00  0.00E+00  2.27E-03
 
 ETA4
+        0.00E+00  0.00E+00  1.84E-03  2.72E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.17E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.41E-02  2.68E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.25E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.82E-03
 
 ETA2
+        2.55E-02  1.89E-03
 
 ETA3
+       ......... .........  6.89E-03
 
 ETA4
+       ......... .........  6.27E-02  7.62E-03
 
 ETA5
+       ......... ......... ......... .........  5.03E-02
 
 ETA6
+       ......... ......... ......... .........  2.46E-01  4.73E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.19E-04
 
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
+        6.07E-06
 
 TH 2
+        1.03E-06  6.33E-06
 
 OM11
+        1.03E-07  7.56E-08  1.33E-07
 
 OM12
+        1.28E-07  8.56E-08  2.24E-08  7.33E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.62E-08  1.22E-07 -3.45E-10  1.39E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.55E-07
 
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
+        8.61E-08  6.68E-08  1.93E-08  7.54E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.33E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.16E-06
 
 OM34
+       -1.63E-08  1.44E-08 -9.23E-09 -2.02E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.48E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.09E-07  3.38E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        5.13E-08  6.49E-09  1.81E-08  6.31E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.06E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.02E-07  1.35E-07  0.00E+00  0.00E+00  7.38E-06
 
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
+        2.22E-06 -4.42E-08  1.83E-07  3.42E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.62E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.47E-06  2.45E-06  0.00E+00  0.00E+00  4.91E-06  0.00E+00  0.00E+00  1.00E-03
 
 OM56
+       -4.29E-07  3.20E-07 -5.37E-08  1.23E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.47E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.30E-06  1.76E-06  0.00E+00  0.00E+00 -1.17E-06  0.00E+00  0.00E+00 -4.93E-04  5.82E-04
 
 OM66
+        1.20E-06  6.60E-07  4.79E-08 -9.03E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.42E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.55E-07 -5.23E-06  0.00E+00  0.00E+00  7.72E-07  0.00E+00  0.00E+00  1.59E-04 -1.75E-04  7.19E-04
 
 SG11
+        2.05E-08  7.94E-09 -1.53E-09  1.54E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.02E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.33E-09 -3.55E-10  0.00E+00  0.00E+00 -2.48E-09  0.00E+00  0.00E+00 -1.12E-07  2.48E-08 -1.07E-08  1.55E-08
 
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
+        2.46E-03
 
 TH 2
+        1.65E-01  2.52E-03
 
 OM11
+        1.14E-01  8.24E-02  3.65E-04
 
 OM12
+        1.91E-01  1.26E-01  2.27E-01  2.71E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.67E-02  1.23E-01 -2.40E-03  1.30E-01  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.94E-04
 
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
+        1.54E-02  1.17E-02  2.33E-02  1.23E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.49E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.27E-03
 
 OM34
+       -3.60E-03  3.10E-03 -1.38E-02 -4.05E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.31E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -5.00E-02  1.84E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        7.66E-03  9.49E-04  1.83E-02  8.58E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -9.87E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.65E-02  2.71E-02  0.00E+00  0.00E+00  2.72E-03
 
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
+        2.84E-02 -5.55E-04  1.58E-02  3.99E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.10E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.61E-02  4.22E-02  0.00E+00  0.00E+00  5.71E-02  0.00E+00  0.00E+00  3.17E-02
 
 OM56
+       -7.21E-03  5.27E-03 -6.10E-03  1.89E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.66E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.85E-02  3.98E-02  0.00E+00  0.00E+00 -1.78E-02  0.00E+00  0.00E+00 -6.45E-01  2.41E-02
 
 OM66
+        1.81E-02  9.78E-03  4.90E-03 -1.24E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.18E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.24E-02 -1.06E-01  0.00E+00  0.00E+00  1.06E-02  0.00E+00  0.00E+00  1.87E-01 -2.70E-01  2.68E-02
 
 SG11
+        6.68E-02  2.53E-02 -3.37E-02  4.57E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.12E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  8.25E-03 -1.55E-03  0.00E+00  0.00E+00 -7.33E-03  0.00E+00  0.00E+00 -2.85E-02  8.25E-03 -3.21E-03  1.25E-04
 
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
+        1.77E+05
 
 TH 2
+       -2.42E+04  1.67E+05
 
 OM11
+       -8.10E+04 -5.82E+04  8.02E+06
 
 OM12
+       -2.57E+05 -1.12E+05 -2.29E+06  1.52E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.87E+04 -1.20E+05  2.84E+05 -1.26E+06  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.68E+06
 
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
+       -1.63E+03 -1.90E+03 -2.35E+04 -8.69E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.88E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.96E+05
 
 OM34
+       -8.60E+02 -1.77E+03  6.63E+03  9.54E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.17E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.29E+04  3.04E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -5.43E+02  6.26E+01 -1.64E+04 -8.14E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.10E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.14E+03 -4.63E+03  0.00E+00  0.00E+00  1.36E+05
 
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
+       -4.79E+02  1.55E+01 -6.52E+02 -3.33E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.98E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.19E+02 -2.13E+03  0.00E+00  0.00E+00 -8.76E+02  0.00E+00  0.00E+00  1.74E+03
 
 OM56
+       -2.97E+02 -1.52E+02  3.05E+02 -6.14E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.93E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.02E+03 -1.99E+03  0.00E+00  0.00E+00 -4.59E+02  0.00E+00  0.00E+00  1.46E+03  3.09E+03
 
 OM66
+       -2.64E+02 -2.53E+02 -1.85E+02  1.76E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.17E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.21E+02  2.22E+03  0.00E+00  0.00E+00 -8.80E+01  0.00E+00  0.00E+00 -4.28E+01  4.19E+02  1.52E+03
 
 SG11
+       -2.27E+05 -7.32E+04  9.83E+05 -1.54E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.65E+05  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.17E+04 -7.55E+03  0.00E+00  0.00E+00  1.59E+04  0.00E+00  0.00E+00  1.11E+04  6.72E+03  9.86E+02  6.51E+07
 
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
 MC SAMPLES PER SUBJECT (ISAMPLE):        300         
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   0.100000000000000       ,10.0000000000000        
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 T-DIST. PROPOSAL DENSITY (DF):           0           
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
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -41160.7751213143 eff.=     300. Smpl.=     300. Fit.= 0.98006
 iteration            1 OBJ=  -41191.9487357055 eff.=     120. Smpl.=     300. Fit.= 0.78682
 iteration            2 OBJ=  -41191.6360500620 eff.=     121. Smpl.=     300. Fit.= 0.78916
 iteration            3 OBJ=  -41192.8942180833 eff.=     120. Smpl.=     300. Fit.= 0.78894
 iteration            4 OBJ=  -41192.1808527931 eff.=     120. Smpl.=     300. Fit.= 0.78903
 iteration            5 OBJ=  -41195.5804441181 eff.=     120. Smpl.=     300. Fit.= 0.78887
 iteration            6 OBJ=  -41191.2532492680 eff.=     120. Smpl.=     300. Fit.= 0.78901
 iteration            7 OBJ=  -41186.2389523547 eff.=     120. Smpl.=     300. Fit.= 0.78940
 iteration            8 OBJ=  -41192.6161825219 eff.=     120. Smpl.=     300. Fit.= 0.78931
 iteration            9 OBJ=  -41192.8160874184 eff.=     121. Smpl.=     300. Fit.= 0.78992
 iteration           10 OBJ=  -41189.8878670038 eff.=     120. Smpl.=     300. Fit.= 0.78947
 iteration           11 OBJ=  -41196.7103719057 eff.=     120. Smpl.=     300. Fit.= 0.78917
 iteration           12 OBJ=  -41194.1265513125 eff.=     120. Smpl.=     300. Fit.= 0.78941
 iteration           13 OBJ=  -41194.5217117611 eff.=     120. Smpl.=     300. Fit.= 0.78870
 iteration           14 OBJ=  -41195.2066271570 eff.=     120. Smpl.=     300. Fit.= 0.78931
 Convergence achieved
 iteration           14 OBJ=  -41195.5909372323 eff.=     121. Smpl.=     300. Fit.= 0.78952
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         3.3629E-05 -9.6122E-05  6.2506E-18 -9.5007E-18 -7.3814E-16 -4.3051E-16
 SE:             1.6658E-03  1.7470E-03  9.9873E-03  1.0730E-02  6.2433E-02  5.5654E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         9.8389E-01  9.5612E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.0977E+01  1.0653E+01  4.5606E-02  3.3912E-03  6.7855E-02  1.1924E-02
 EBVshrink(%):   1.0989E+01  1.0633E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.2068E+01
 
 #TERE:
 Elapsed estimation time in seconds:   935.37
 Elapsed covariance time in seconds:    61.92
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41195.591       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         1.99E+00  3.63E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.73E-03
 
 ETA2
+       -1.48E-05  1.06E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.77E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.29E-03  3.20E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.02E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.81E-02  8.07E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.94E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.86E-02
 
 ETA2
+       -1.46E-03  1.03E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.67E-01
 
 ETA4
+        0.00E+00  0.00E+00  4.34E-02  1.79E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.19E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.00E-01  2.84E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.97E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.23E-03  2.31E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        3.45E-04
 
 ETA2
+        2.59E-04  3.78E-04
 
 ETA3
+        0.00E+00  0.00E+00  2.34E-03
 
 ETA4
+        0.00E+00  0.00E+00  1.87E-03  2.73E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.28E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.47E-02  2.70E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.25E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.75E-03
 
 ETA2
+        2.54E-02  1.84E-03
 
 ETA3
+       ......... .........  7.02E-03
 
 ETA4
+       ......... .........  6.29E-02  7.64E-03
 
 ETA5
+       ......... ......... ......... .........  5.15E-02
 
 ETA6
+       ......... ......... ......... .........  2.45E-01  4.76E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.27E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        4.96E-06
 
 TH 2
+        2.38E-07  5.35E-06
 
 OM11
+       -2.12E-08  1.76E-08  1.19E-07
 
 OM12
+        3.08E-08 -5.51E-09  6.43E-09  6.68E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.82E-08  4.09E-09  4.33E-10  6.37E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.43E-07
 
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
+        6.21E-08  5.26E-08  1.85E-08  6.15E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.38E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.47E-06
 
 OM34
+        8.58E-09  2.72E-08 -6.89E-09 -2.03E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.93E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.56E-07  3.48E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        2.34E-08 -4.25E-09  1.77E-08  5.18E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -9.71E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.12E-07  8.06E-09  0.00E+00  0.00E+00  7.47E-06
 
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
+        1.93E-06 -9.61E-08  2.42E-07  3.34E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.61E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.84E-06  2.42E-06  0.00E+00  0.00E+00  5.15E-06  0.00E+00  0.00E+00  1.08E-03
 
 OM56
+       -5.08E-07  2.23E-07 -1.05E-07  1.25E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.59E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.63E-06  1.88E-06  0.00E+00  0.00E+00 -1.26E-06  0.00E+00  0.00E+00 -5.35E-04  6.09E-04
 
 OM66
+        1.13E-06  8.34E-07  5.09E-08 -1.06E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.14E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -5.39E-07 -5.40E-06  0.00E+00  0.00E+00  7.97E-07  0.00E+00  0.00E+00  1.72E-04 -1.93E-04  7.30E-04
 
 SG11
+        1.00E-08  1.18E-08 -2.98E-09 -9.21E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -3.63E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.41E-09 -1.81E-12  0.00E+00  0.00E+00 -1.93E-09  0.00E+00  0.00E+00 -1.14E-07  3.44E-08 -1.62E-08  1.56E-08
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.23E-03
 
 TH 2
+        4.63E-02  2.31E-03
 
 OM11
+       -2.76E-02  2.21E-02  3.45E-04
 
 OM12
+        5.35E-02 -9.21E-03  7.21E-02  2.59E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.16E-02  4.68E-03  3.32E-03  6.51E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.78E-04
 
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
+        1.19E-02  9.73E-03  2.30E-02  1.02E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.56E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.34E-03
 
 OM34
+        2.07E-03  6.30E-03 -1.07E-02 -4.22E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.41E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.16E-02  1.87E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        3.85E-03 -6.73E-04  1.87E-02  7.33E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -9.39E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.75E-02  1.58E-03  0.00E+00  0.00E+00  2.73E-03
 
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
+        2.64E-02 -1.27E-03  2.14E-02  3.93E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.10E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.61E-02  3.94E-02  0.00E+00  0.00E+00  5.73E-02  0.00E+00  0.00E+00  3.28E-02
 
 OM56
+       -9.25E-03  3.91E-03 -1.24E-02  1.96E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.85E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.03E-02  4.08E-02  0.00E+00  0.00E+00 -1.87E-02  0.00E+00  0.00E+00 -6.61E-01  2.47E-02
 
 OM66
+        1.87E-02  1.34E-02  5.46E-03 -1.52E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.05E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.53E-03 -1.07E-01  0.00E+00  0.00E+00  1.08E-02  0.00E+00  0.00E+00  1.94E-01 -2.89E-01  2.70E-02
 
 SG11
+        3.61E-02  4.08E-02 -6.92E-02 -2.85E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -7.67E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  8.25E-03 -7.75E-06  0.00E+00  0.00E+00 -5.66E-03  0.00E+00  0.00E+00 -2.77E-02  1.12E-02 -4.81E-03  1.25E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.04E+05
 
 TH 2
+       -9.00E+03  1.88E+05
 
 OM11
+        4.11E+04 -3.40E+04  8.51E+06
 
 OM12
+       -1.02E+05  2.16E+04 -8.15E+05  1.52E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.58E+04 -1.18E+04  4.85E+04 -6.81E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  7.08E+06
 
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
+       -2.01E+03 -1.87E+03 -2.68E+04 -1.04E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.69E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.86E+05
 
 OM34
+       -1.18E+03 -1.85E+03  1.02E+04  9.75E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.02E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.99E+04  2.96E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -3.94E+02  1.28E+02 -1.85E+04 -7.91E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  8.31E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.41E+03  7.39E+02  0.00E+00  0.00E+00  1.35E+05
 
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
+       -4.71E+02 -1.17E+01 -1.63E+03 -3.89E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.00E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -6.85E+02 -2.08E+03  0.00E+00  0.00E+00 -8.96E+02  0.00E+00  0.00E+00  1.67E+03
 
 OM56
+       -3.12E+02 -1.80E+02 -1.96E+02 -6.55E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.80E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  9.83E+02 -1.91E+03  0.00E+00  0.00E+00 -5.05E+02  0.00E+00  0.00E+00  1.46E+03  3.09E+03
 
 OM66
+       -2.91E+02 -2.65E+02 -2.66E+02  1.95E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.07E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.16E+02  2.19E+03  0.00E+00  0.00E+00 -5.55E+01  0.00E+00  0.00E+00 -2.09E+01  4.59E+02  1.52E+03
 
 SG11
+       -1.19E+05 -1.44E+05  1.58E+06  6.23E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.62E+06  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.38E+04 -6.44E+03  0.00E+00  0.00E+00  8.74E+03  0.00E+00  0.00E+00  9.39E+03  4.90E+03  1.69E+03  6.51E+07
 
 SKIPPING EXECUTION OF SLOW FNLMOD/FNLETA BY USER REQUEST
 #CPUT: Total CPU Time in Seconds,      994.016
Stop Time: 
Sat 09/07/2013 
07:19 PM

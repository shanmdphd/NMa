Sat 09/07/2013 
08:40 PM
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

$PRIOR NWPRI NTHETA=2 NETA=6 NTHP=0 NETP=6
$OMEGA BLOCK(2)
0.01 FIX
0.0 0.01

;$OMEGA 0.03 0.03
$OMEGA BLOCK(2)
0.03 FIX
0.0 0.03

$OMEGA BLOCK(2)
0.1 FIX
0.0 0.1

$THETA (2 FIX) (2 FIX) (2 FIX)

$EST METHOD=ITS INTERACTION PRINT=1 NSIG=3 NITER=12 SIGL=5 FNLETA=0 NOABORT SIGLO=8 MCETA=10 NOPRIOR=1
;$EST METHOD=IMP INTERACTION PRINT=1 NSIG=3 NITER=500 CTYPE=3 ISAMPLE=300 MCETA=3 FNLETA=0 NOABORT SIGL=5 SIGLO=8
;$EST METHOD=1 INTERACTION PRINT=1 MAXEVAL=9999 NSIG=3 FNLETA=0 SIGL=10
$EST METHOD=BAYES INTERACTION PRINT=10 NSIG=3 NBURN=1000 NITER=1000 CTYPE=3 FNLETA=0 NOABORT NOPRIOR=0
$COV MATRIX=R UNCONDITIONAL
$TABLE  ID TIME CONC DOSE RATE EVID MDV CMT SID CID CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 
NOAPPEND ONEHEADER FILE=superid2_5.tab  NOPRINT
  
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
0LENGTH OF THETA:   5
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  0  0  2
  0  0  2  2
  0  0  0  0  3
  0  0  0  0  3  3
  0  0  0  0  0  0  4
  0  0  0  0  0  0  4  4
  0  0  0  0  0  0  0  0  5
  0  0  0  0  0  0  0  0  5  5
  0  0  0  0  0  0  0  0  0  0  6
  0  0  0  0  0  0  0  0  0  0  6  6
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.5000E+01     0.1000E+07
 -0.1000E+07     0.5000E+01     0.1000E+07
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
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
        4                                                                                  YES
                  0.1000E-01
                  0.0000E+00   0.1000E-01
        5                                                                                  YES
                  0.3000E-01
                  0.0000E+00   0.3000E-01
        6                                                                                  YES
                  0.1000E+00
                  0.0000E+00   0.1000E+00
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
 ID TIME CONC DOSE RATE EVID MDV CMT SID CID CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
0
 PRIOR SUBROUTINE USER-SUPPLIED
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
 #METH: Iterative Two Stage (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            1680
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  10          
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   5           
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               ON 
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

 iteration            0 OBJ=   315017.478531624
 iteration            1 OBJ=  -4558.48088494222
 iteration            2 OBJ=  -30822.9653927707
 iteration            3 OBJ=  -40488.2608707429
 iteration            4 OBJ=  -41146.4274706013
 iteration            5 OBJ=  -41169.7387088252
 iteration            6 OBJ=  -41170.9142916071
 iteration            7 OBJ=  -41170.7817380935
 iteration            8 OBJ=  -41170.6632394651
 iteration            9 OBJ=  -41170.6038386367
 iteration           10 OBJ=  -41170.5768634905
 iteration           11 OBJ=  -41170.5649811441
 iteration           12 OBJ=  -41170.5598130110
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -6.3099E-05 -3.4199E-05  7.2164E-19 -8.6042E-18 -1.9398E-16  2.0335E-16
 SE:             1.6659E-03  1.7466E-03  9.8889E-03  1.0710E-02  6.1692E-02  5.5552E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         9.6979E-01  9.8438E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.0829E+01  1.0598E+01  4.3017E-05  4.1794E-05  3.5868E-05  3.2798E-05
 EBVshrink(%):   1.0827E+01  1.0596E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.1823E+01
 
 #TERE:
 Elapsed estimation time in seconds:   146.15
 Elapsed covariance time in seconds:     0.91
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41170.560       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
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
+        3.76E-05  1.06E-02
 
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
+        9.94E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.85E-02
 
 ETA2
+        3.71E-03  1.03E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.65E-01
 
 ETA4
+        0.00E+00  0.00E+00  5.96E-02  1.79E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.15E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.89E-01  2.83E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.97E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
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
+        1.27E-04
 
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
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        5.52E-06
 
 TH 2
+        3.21E-07  5.95E-06
 
 OM11
+       -5.25E-09  5.77E-08  1.16E-07
 
 OM12
+        6.69E-08  2.49E-08  8.30E-09  6.52E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.78E-09  1.62E-08 -2.10E-09  1.80E-12  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.44E-07
 
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
+        6.35E-08  6.69E-08  1.78E-08  6.76E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.29E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.18E-06
 
 OM34
+       -2.13E-09  2.05E-08 -7.02E-09 -1.97E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.11E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.35E-07  3.39E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        4.11E-08  7.25E-09  1.69E-08  6.55E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.11E-08  0.00E+00  0.00E+00  0.00E+00
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
+        2.21E-06  6.56E-08  1.91E-07  5.52E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.53E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.48E-06  2.44E-06  0.00E+00  0.00E+00  4.92E-06  0.00E+00  0.00E+00  1.01E-03
 
 OM56
+       -5.59E-07  1.87E-07 -8.03E-08  1.13E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.58E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.33E-06  1.78E-06  0.00E+00  0.00E+00 -1.19E-06  0.00E+00  0.00E+00 -4.98E-04  5.86E-04
 
 OM66
+        1.21E-06  9.52E-07  6.73E-08 -6.51E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.09E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.28E-07 -5.26E-06  0.00E+00  0.00E+00  8.04E-07  0.00E+00  0.00E+00  1.62E-04 -1.79E-04  7.23E-04
 
 SG11
+        1.51E-08  2.85E-09 -1.88E-09 -2.62E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.19E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.82E-09 -5.47E-10  0.00E+00  0.00E+00 -2.58E-09  0.00E+00  0.00E+00 -1.24E-07  2.80E-08 -2.03E-08  1.62E-08
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.35E-03
 
 TH 2
+        5.59E-02  2.44E-03
 
 OM11
+       -6.56E-03  6.95E-02  3.40E-04
 
 OM12
+        1.12E-01  4.00E-02  9.55E-02  2.55E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.36E-03  1.75E-02 -1.63E-02  1.85E-05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.79E-04
 
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
+       -4.92E-04  4.56E-03 -1.12E-02 -4.18E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.59E-02  0.00E+00  0.00E+00  0.00E+00
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
+        2.96E-02  8.47E-04  1.77E-02  6.81E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.10E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.59E-02  4.18E-02  0.00E+00  0.00E+00  5.70E-02  0.00E+00  0.00E+00  3.17E-02
 
 OM56
+       -9.83E-03  3.17E-03 -9.75E-03  1.82E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.73E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.86E-02  3.99E-02  0.00E+00  0.00E+00 -1.80E-02  0.00E+00  0.00E+00 -6.48E-01  2.42E-02
 
 OM66
+        1.92E-02  1.45E-02  7.35E-03 -9.48E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.01E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.19E-02 -1.06E-01  0.00E+00  0.00E+00  1.10E-02  0.00E+00  0.00E+00  1.89E-01 -2.74E-01  2.69E-02
 
 SG11
+        5.05E-02  9.18E-03 -4.34E-02 -8.05E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.53E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  6.28E-03 -2.33E-03  0.00E+00  0.00E+00 -7.42E-03  0.00E+00  0.00E+00 -3.07E-02  9.08E-03 -5.92E-03  1.27E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
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
+        2.46E+04 -8.24E+04  8.79E+06
 
 OM12
+       -1.89E+05 -4.54E+04 -1.10E+06  1.57E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -9.24E+03 -2.13E+04  1.44E+05 -1.03E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.99E+06
 
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
+       -1.75E+03 -2.03E+03 -2.66E+04 -1.08E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.68E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.96E+05
 
 OM34
+       -9.00E+02 -1.88E+03  1.10E+04  9.78E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.92E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.44E+04  3.02E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -6.65E+02  7.29E+01 -1.82E+04 -1.05E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.20E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.16E+03 -3.45E+03  0.00E+00  0.00E+00  1.36E+05
 
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
+       -5.13E+02 -5.51E-01 -1.23E+03 -3.80E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.99E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.18E+02 -2.14E+03  0.00E+00  0.00E+00 -8.78E+02  0.00E+00  0.00E+00  1.74E+03
 
 OM56
+       -3.10E+02 -1.56E+02 -2.19E+01 -6.42E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.79E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.02E+03 -1.98E+03  0.00E+00  0.00E+00 -4.65E+02  0.00E+00  0.00E+00  1.47E+03  3.10E+03
 
 OM66
+       -2.96E+02 -2.72E+02 -3.96E+02  1.87E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.14E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.26E+02  2.21E+03  0.00E+00  0.00E+00 -8.40E+01  0.00E+00  0.00E+00 -3.94E+01  4.27E+02  1.52E+03
 
 SG11
+       -1.75E+05 -3.43E+04  1.00E+06  2.96E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.91E+05  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.63E+04 -7.10E+02  0.00E+00  0.00E+00  1.46E+04  0.00E+00  0.00E+00  1.13E+04  6.87E+03  1.70E+03  6.21E+07
 
1
 
 
 #TBLN:      2
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            1680
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  10          
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   5           
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
 ITERATIONS (NITER):                      1000        
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
   1   2
 THETAS THAT ARE GIBBS SAMPLED:
   1   2
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -1000 MCMCOBJ=   -75087.6998896754     
 iteration         -990 MCMCOBJ=   -73486.2116452652     
 iteration         -980 MCMCOBJ=   -73475.5751807470     
 iteration         -970 MCMCOBJ=   -73576.3954523911     
 iteration         -960 MCMCOBJ=   -73331.7200766217     
 iteration         -950 MCMCOBJ=   -73464.9205094807     
 iteration         -940 MCMCOBJ=   -73393.4324354501     
 iteration         -930 MCMCOBJ=   -73334.0616480362     
 iteration         -920 MCMCOBJ=   -73391.0957170410     
 iteration         -910 MCMCOBJ=   -73313.6835563330     
 iteration         -900 MCMCOBJ=   -73226.6511072169     
 iteration         -890 MCMCOBJ=   -73377.7006436912     
 iteration         -880 MCMCOBJ=   -73308.7333922296     
 iteration         -870 MCMCOBJ=   -73567.5812247034     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -73410.5114216421     
 iteration           10 MCMCOBJ=   -73398.3330256855     
 iteration           20 MCMCOBJ=   -73154.8269479648     
 iteration           30 MCMCOBJ=   -73210.7828060229     
 iteration           40 MCMCOBJ=   -73621.2017247893     
 iteration           50 MCMCOBJ=   -73376.4392060030     
 iteration           60 MCMCOBJ=   -73353.1915535829     
 iteration           70 MCMCOBJ=   -73219.7361878425     
 iteration           80 MCMCOBJ=   -73067.5609710707     
 iteration           90 MCMCOBJ=   -73471.9925991141     
 iteration          100 MCMCOBJ=   -73480.6010685427     
 iteration          110 MCMCOBJ=   -73418.9965064015     
 iteration          120 MCMCOBJ=   -73342.2421795558     
 iteration          130 MCMCOBJ=   -73420.2071632322     
 iteration          140 MCMCOBJ=   -73325.8691775895     
 iteration          150 MCMCOBJ=   -73351.8104248683     
 iteration          160 MCMCOBJ=   -73418.7390393339     
 iteration          170 MCMCOBJ=   -73466.3346981580     
 iteration          180 MCMCOBJ=   -73350.8534588670     
 iteration          190 MCMCOBJ=   -73636.8952910239     
 iteration          200 MCMCOBJ=   -73321.6436615010     
 iteration          210 MCMCOBJ=   -73279.0676902936     
 iteration          220 MCMCOBJ=   -73365.2846132080     
 iteration          230 MCMCOBJ=   -73364.3854116300     
 iteration          240 MCMCOBJ=   -73539.9048115145     
 iteration          250 MCMCOBJ=   -73212.4770030099     
 iteration          260 MCMCOBJ=   -73213.9202556931     
 iteration          270 MCMCOBJ=   -73402.5100349339     
 iteration          280 MCMCOBJ=   -73487.7616762796     
 iteration          290 MCMCOBJ=   -73491.5923524141     
 iteration          300 MCMCOBJ=   -73261.3442485823     
 iteration          310 MCMCOBJ=   -73449.4300704168     
 iteration          320 MCMCOBJ=   -73249.7485502880     
 iteration          330 MCMCOBJ=   -73343.7863302171     
 iteration          340 MCMCOBJ=   -73306.7945092068     
 iteration          350 MCMCOBJ=   -73378.4547020937     
 iteration          360 MCMCOBJ=   -73519.6741062097     
 iteration          370 MCMCOBJ=   -73236.1720068292     
 iteration          380 MCMCOBJ=   -73469.2039674663     
 iteration          390 MCMCOBJ=   -73390.5746040517     
 iteration          400 MCMCOBJ=   -73314.1027509393     
 iteration          410 MCMCOBJ=   -73373.1587447926     
 iteration          420 MCMCOBJ=   -73729.0168154556     
 iteration          430 MCMCOBJ=   -73411.2782123667     
 iteration          440 MCMCOBJ=   -73286.2394727328     
 iteration          450 MCMCOBJ=   -73477.8771849496     
 iteration          460 MCMCOBJ=   -73419.7781614125     
 iteration          470 MCMCOBJ=   -73355.0262766575     
 iteration          480 MCMCOBJ=   -73441.0487073609     
 iteration          490 MCMCOBJ=   -73397.9851795777     
 iteration          500 MCMCOBJ=   -73322.9335061996     
 iteration          510 MCMCOBJ=   -73491.6705780540     
 iteration          520 MCMCOBJ=   -73352.6496816674     
 iteration          530 MCMCOBJ=   -73349.1436038522     
 iteration          540 MCMCOBJ=   -73362.6189486172     
 iteration          550 MCMCOBJ=   -73330.1424812345     
 iteration          560 MCMCOBJ=   -73457.7935650487     
 iteration          570 MCMCOBJ=   -73296.0002344205     
 iteration          580 MCMCOBJ=   -73439.2596313331     
 iteration          590 MCMCOBJ=   -73494.6945151148     
 iteration          600 MCMCOBJ=   -73389.7198270001     
 iteration          610 MCMCOBJ=   -73319.2460240089     
 iteration          620 MCMCOBJ=   -73462.5855574508     
 iteration          630 MCMCOBJ=   -73513.6142572356     
 iteration          640 MCMCOBJ=   -73613.2326328322     
 iteration          650 MCMCOBJ=   -73350.1091601330     
 iteration          660 MCMCOBJ=   -73416.8328808541     
 iteration          670 MCMCOBJ=   -73297.4980274052     
 iteration          680 MCMCOBJ=   -73495.5881572510     
 iteration          690 MCMCOBJ=   -73455.2611066663     
 iteration          700 MCMCOBJ=   -73224.3941144277     
 iteration          710 MCMCOBJ=   -73352.5003614101     
 iteration          720 MCMCOBJ=   -73407.6797359689     
 iteration          730 MCMCOBJ=   -73458.9339746128     
 iteration          740 MCMCOBJ=   -73540.5587051634     
 iteration          750 MCMCOBJ=   -73513.9637283213     
 iteration          760 MCMCOBJ=   -73422.2929678987     
 iteration          770 MCMCOBJ=   -73336.7003645395     
 iteration          780 MCMCOBJ=   -73303.7228012540     
 iteration          790 MCMCOBJ=   -73348.3706010812     
 iteration          800 MCMCOBJ=   -73593.9463748984     
 iteration          810 MCMCOBJ=   -73263.9890157669     
 iteration          820 MCMCOBJ=   -73407.4270929056     
 iteration          830 MCMCOBJ=   -73485.2979275204     
 iteration          840 MCMCOBJ=   -73420.1846734292     
 iteration          850 MCMCOBJ=   -73580.0990568309     
 iteration          860 MCMCOBJ=   -73280.1247599790     
 iteration          870 MCMCOBJ=   -73101.3368147208     
 iteration          880 MCMCOBJ=   -73245.2083043621     
 iteration          890 MCMCOBJ=   -73373.4973577701     
 iteration          900 MCMCOBJ=   -73246.5795783104     
 iteration          910 MCMCOBJ=   -73461.7886104407     
 iteration          920 MCMCOBJ=   -73389.2238907293     
 iteration          930 MCMCOBJ=   -73376.3302214098     
 iteration          940 MCMCOBJ=   -73360.1384301321     
 iteration          950 MCMCOBJ=   -73272.1435182646     
 iteration          960 MCMCOBJ=   -73606.0121009074     
 iteration          970 MCMCOBJ=   -73346.7357261846     
 iteration          980 MCMCOBJ=   -73310.1684046383     
 iteration          990 MCMCOBJ=   -73249.8262542717     
 iteration         1000 MCMCOBJ=   -73354.8860370646     
 
 #TERM:
 BURN-IN WAS COMPLETED
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation time in seconds:  3183.39
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -73394.787       **************************************************
 #OBJS:********************************************      116.891 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         1.99E+00  3.63E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.03E-02
 
 ETA2
+        1.08E-04  1.13E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.85E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.46E-03  3.27E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.14E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.86E-02  9.27E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.02E-01
 
 ETA2
+        1.00E-02  1.06E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.69E-01
 
 ETA4
+        0.00E+00  0.00E+00  4.77E-02  1.81E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.35E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.78E-01  3.01E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.00E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.18E-03  2.42E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        3.62E-04
 
 ETA2
+        2.75E-04  4.10E-04
 
 ETA3
+        0.00E+00  0.00E+00  2.61E-03
 
 ETA4
+        0.00E+00  0.00E+00  1.94E-03  2.89E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.25E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.12E-02  2.80E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.27E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.78E-03
 
 ETA2
+        2.55E-02  1.93E-03
 
 ETA3
+        0.00E+00  0.00E+00  7.68E-03
 
 ETA4
+        0.00E+00  0.00E+00  6.29E-02  8.00E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.60E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.85E-01  4.41E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.33E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        4.75E-06
 
 TH 2
+        3.43E-07  5.84E-06
 
 OM11
+       -3.35E-08  3.51E-08  1.31E-07
 
 OM12
+        2.96E-08  1.08E-08  8.46E-09  7.56E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.01E-08 -4.35E-08 -6.52E-09  4.24E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.68E-07
 
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
+        1.55E-07 -4.33E-08 -1.69E-09  8.76E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.13E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  6.80E-06
 
 OM34
+        5.87E-08  9.09E-08 -3.75E-08 -1.25E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.25E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.77E-07  3.75E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -2.21E-07 -3.17E-07 -2.30E-08  2.16E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.41E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.93E-08  5.11E-07  0.00E+00  0.00E+00  8.37E-06
 
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
+       -3.77E-06  1.89E-06 -8.80E-08 -1.77E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -9.39E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.31E-06  1.32E-06  0.00E+00  0.00E+00  1.77E-06  0.00E+00  0.00E+00  1.06E-03
 
 OM56
+        7.87E-07 -3.98E-06 -3.63E-07 -1.30E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.22E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.66E-07  2.15E-06  0.00E+00  0.00E+00  1.68E-08  0.00E+00  0.00E+00 -1.80E-04  4.48E-04
 
 OM66
+       -3.36E-07  4.54E-07  3.56E-07  2.59E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  8.35E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.79E-06 -6.38E-07  0.00E+00  0.00E+00 -6.10E-07  0.00E+00  0.00E+00  8.74E-05 -1.44E-04  7.85E-04
 
 SG11
+       -8.05E-09  7.22E-09 -3.08E-09 -9.95E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.43E-11  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.43E-08  1.65E-08  0.00E+00  0.00E+00  1.69E-08  0.00E+00  0.00E+00  9.21E-08  2.34E-08 -1.07E-07  1.61E-08
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.18E-03
 
 TH 2
+        6.51E-02  2.42E-03
 
 OM11
+       -4.24E-02  4.01E-02  3.62E-04
 
 OM12
+        4.94E-02  1.62E-02  8.49E-02  2.75E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.49E-02 -4.40E-02 -4.39E-02  3.76E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.10E-04
 
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
+        2.73E-02 -6.87E-03 -1.79E-03  1.22E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.99E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.61E-03
 
 OM34
+        1.39E-02  1.94E-02 -5.36E-02 -2.35E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.57E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.48E-02  1.94E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -3.50E-02 -4.54E-02 -2.20E-02  2.72E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.19E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -6.54E-03  9.13E-02  0.00E+00  0.00E+00  2.89E-03
 
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
+       -5.31E-02  2.41E-02 -7.47E-03 -1.98E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -7.05E-04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.73E-02  2.10E-02  0.00E+00  0.00E+00  1.88E-02  0.00E+00  0.00E+00  3.25E-02
 
 OM56
+        1.71E-02 -7.78E-02 -4.74E-02 -2.23E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.87E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.01E-03  5.24E-02  0.00E+00  0.00E+00  2.74E-04  0.00E+00  0.00E+00 -2.62E-01  2.12E-02
 
 OM66
+       -5.49E-03  6.71E-03  3.51E-02  3.36E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  7.27E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.82E-02 -1.18E-02  0.00E+00  0.00E+00 -7.52E-03  0.00E+00  0.00E+00  9.59E-02 -2.43E-01  2.80E-02
 
 SG11
+       -2.91E-02  2.36E-02 -6.70E-02 -2.85E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -8.53E-04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.35E-02  6.74E-02  0.00E+00  0.00E+00  4.62E-02  0.00E+00  0.00E+00  2.23E-02  8.73E-03 -3.01E-02  1.27E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.14E+05
 
 TH 2
+       -1.27E+04  1.74E+05
 
 OM11
+        6.94E+04 -4.59E+04  7.80E+06
 
 OM12
+       -9.12E+04 -1.95E+04 -8.67E+05  1.34E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.36E+04  3.66E+04  3.37E+05 -3.95E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.07E+06
 
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
+       -4.66E+03  2.34E+03 -6.65E+03 -2.19E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.40E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.49E+05
 
 OM34
+       -3.42E+03 -5.99E+03  6.50E+04  3.91E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.10E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.48E+04  2.74E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        5.47E+03  6.87E+03  1.70E+04 -4.55E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.34E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.21E+03 -1.65E+04  0.00E+00  0.00E+00  1.21E+05
 
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
+        7.16E+02 -1.05E+02  1.60E+03  2.64E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -6.23E+02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.70E+02 -5.74E+02  0.00E+00  0.00E+00 -1.60E+02  0.00E+00  0.00E+00  1.02E+03
 
 OM56
+       -2.39E+02  1.54E+03  4.64E+03  3.32E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -8.19E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  4.60E+02 -1.59E+03  0.00E+00  0.00E+00  8.79E+01  0.00E+00  0.00E+00  4.02E+02  2.57E+03
 
 OM66
+       -8.16E+01  1.71E+02 -2.65E+03 -3.32E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -7.83E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.26E+02 -1.64E+02  0.00E+00  0.00E+00  9.87E+01  0.00E+00  0.00E+00 -4.07E+01  4.30E+02  1.37E+03
 
 SG11
+        1.21E+05 -9.96E+04  1.39E+06  6.26E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.63E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.18E+05 -2.22E+05  0.00E+00  0.00E+00 -1.13E+05  0.00E+00  0.00E+00 -5.64E+03 -2.07E+03  7.15E+03  6.34E+07
 
 SKIPPING EXECUTION OF SLOW FNLMOD/FNLETA BY USER REQUEST
 #CPUT: Total CPU Time in Seconds,     3283.219
Stop Time: 
Sat 09/07/2013 
09:36 PM

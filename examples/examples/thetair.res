Mon 09/30/2013 
03:05 PM
; Used for comparing single versus parallel computing for FOCE method.
;$SIZES LVR=30
;$SIZES LTH=15
;$SIZES LIM1=100
$PROB RUN# Example 1 (from samp5l)
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
$DATA example1.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4

$THETAI
THETA=DLOG(THETAI)

$THETAR
THETAR=DEXP(THETA)


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
$THETA 7.389 7.389 7.389 7.389
;INITIAL values of OMEGA
$OMEGA BLOCK(4)
0.15   ;[P]
0.01  ;[F]
0.15   ;[P]
0.01  ;[F]
0.01  ;[F]
0.15   ;[P]
0.01  ;[F]
0.01  ;[F]
0.01  ;[F]
0.15   ;[P]
;Initial value of SIGMA
$SIGMA 
(0.6 )   ;[P]

$PRIOR NWPRI NTHETA=4, NETA=4, NTHP=4, NETP=4
; Prior information of THETAS
$THETA (7.389 FIX) (7.389  FIX) (7.389  FIX) (7.389  FIX)

; Variance to prior information of THETAS.  Because variances are very large, this
; means that the prior information to the THETAS is highly uninformative.
$OMEGA BLOCK(4)
545973 FIX 
0.00 545973
0.00  0.00 545973
0.00  0.00 0.0 545973

; Prior information to the OMEGAS.
$OMEGA BLOCK(4)
0.2 FIX 
0.0  0.2 
0.0  0.0 0.2
0.0  0.0 0.0 0.2
;Degrees of freedom to prior OMEGA matrix.  Because degrees of freedom is very low, equal to the
; the dimension of the prior OMEGA, this means that the prior information to the OMEGAS is
; highly uninformative
$THETA (4 FIX)


$EST METHOD=ITS INTERACTION NOABORT CTYPE=3 PRINT=5 NOPRIOR=1
$EST METHOD=BAYES INTERACTION NOABORT NBURN=200 NITER=500 CTYPE=3 PRINT=50 NOPRIOR=0
$EST METHOD=1 INTERACTION NSIG=3 SIGL=10 PRINT=1 NOABORT MAXEVAL=9999 NOPRIOR=1
$COV MATRIX=R PRINT=E UNCONDITIONAL
  
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
 RUN# Example 1 (from samp5l)
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
0LENGTH OF THETA:   9
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
  1  1  1  1
  0  0  0  0  2
  0  0  0  0  2  2
  0  0  0  0  2  2  2
  0  0  0  0  2  2  2  2
  0  0  0  0  0  0  0  0  3
  0  0  0  0  0  0  0  0  3  3
  0  0  0  0  0  0  0  0  3  3  3
  0  0  0  0  0  0  0  0  3  3  3  3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.7389E+01     0.1000E+07
 -0.1000E+07     0.7389E+01     0.1000E+07
 -0.1000E+07     0.7389E+01     0.1000E+07
 -0.1000E+07     0.7389E+01     0.1000E+07
  0.7389E+01     0.7389E+01     0.7389E+01
  0.7389E+01     0.7389E+01     0.7389E+01
  0.7389E+01     0.7389E+01     0.7389E+01
  0.7389E+01     0.7389E+01     0.7389E+01
  0.4000E+01     0.4000E+01     0.4000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.1500E+00
                  0.1000E-01   0.1500E+00
                  0.1000E-01   0.1000E-01   0.1500E+00
                  0.1000E-01   0.1000E-01   0.1000E-01   0.1500E+00
        2                                                                                  YES
                  0.5460E+06
                  0.0000E+00   0.5460E+06
                  0.0000E+00   0.0000E+00   0.5460E+06
                  0.0000E+00   0.0000E+00   0.0000E+00   0.5460E+06
        3                                                                                  YES
                  0.2000E+00
                  0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.6000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
0
 THETAI SUBROUTINE USER-SUPPLIED
 THETAR SUBROUTINE USER-SUPPLIED
 PRIOR SUBROUTINE USER-SUPPLIED
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
 #METH: Iterative Two Stage (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            2400
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
 NOPRIOR SETTING (NOPRIOR):               ON 
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
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
 CONVERGENCE INTERVAL (CINTERVAL):        5           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      50          
 
 
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

 iteration            0 OBJ=  -234.362195878046
 iteration            5 OBJ=  -1112.61771542417
 iteration           10 OBJ=  -1119.91899653010
 iteration           15 OBJ=  -1120.28770757949
 iteration           20 OBJ=  -1120.36902334021
 iteration           25 OBJ=  -1120.37117460523
 iteration           30 OBJ=  -1120.35776445867
 iteration           35 OBJ=  -1120.34406931994
 iteration           40 OBJ=  -1120.33332186545
 iteration           45 OBJ=  -1120.32565041105
 iteration           50 OBJ=  -1120.32043278566
 
 #TERM:
 OPTIMIZATION WAS NOT COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         6.8463E-06 -5.9096E-06  1.5471E-05  1.8212E-05
 SE:             3.9032E-02  2.9091E-02  3.4975E-02  3.4027E-02
 N:                     100         100         100         100
 
 P VAL.:         9.9986E-01  9.9984E-01  9.9965E-01  9.9957E-01
 
 ETAshrink(%):   3.3133E+00  1.9640E+01  2.2889E+01  1.4543E+01
 EBVshrink(%):   3.3170E+00  1.9649E+01  2.2931E+01  1.4563E+01
 EPSshrink(%):   3.1638E+01
 
 #TERE:
 Elapsed estimation time in seconds:     4.73
 Elapsed covariance time in seconds:     0.09
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1120.320       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         5.38E+00  4.90E+00  2.25E+00  1.07E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.65E-01
 
 ETA2
+        3.95E-03  1.32E-01
 
 ETA3
+        4.91E-03  1.74E-02  2.08E-01
 
 ETA4
+       -1.64E-02  1.23E-02  4.96E-02  1.60E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.55E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.06E-01
 
 ETA2
+        2.68E-02  3.64E-01
 
 ETA3
+        2.66E-02  1.05E-01  4.56E-01
 
 ETA4
+       -1.01E-01  8.42E-02  2.72E-01  4.00E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.36E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.48E-01  2.40E-01  1.46E-01  5.78E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.89E-02
 
 ETA2
+        2.31E-02  3.48E-02
 
 ETA3
+        3.23E-02  3.80E-02  6.47E-02
 
 ETA4
+        2.76E-02  2.73E-02  4.51E-02  4.04E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        7.77E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.56E-02
 
 ETA2
+        1.54E-01  4.78E-02
 
 ETA3
+        1.73E-01  2.25E-01  7.10E-02
 
 ETA4
+        1.75E-01  1.82E-01  1.93E-01  5.05E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.65E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        6.16E-02
 
 TH 2
+        9.59E-03  5.75E-02
 
 TH 3
+        4.33E-03  2.41E-03  2.15E-02
 
 TH 4
+       -1.81E-03  8.26E-03  3.87E-02  3.35E-01
 
 OM11
+       -1.31E-03  4.80E-04  1.78E-04 -8.70E-04  8.35E-04
 
 OM12
+        7.98E-05  8.67E-04  9.28E-05 -1.39E-03  3.09E-04  5.32E-04
 
 OM13
+       -8.77E-04  7.91E-06 -5.51E-04  2.49E-03  1.39E-04  2.40E-04  1.04E-03
 
 OM14
+       -9.51E-04 -1.04E-03  3.51E-04  1.94E-03  2.21E-04  1.82E-04  5.41E-04  7.62E-04
 
 OM22
+       -2.07E-04 -9.21E-05 -4.62E-04 -1.16E-03  1.39E-04  2.35E-04  8.49E-05  1.40E-04  1.21E-03
 
 OM23
+       -4.05E-04 -4.64E-04  1.47E-04  1.38E-03  1.88E-04  3.79E-05 -6.18E-05 -3.07E-05  2.82E-04  1.45E-03
 
 OM24
+       -1.39E-03 -5.09E-04  3.30E-04  1.71E-03  1.15E-04  8.57E-05  2.24E-05  9.11E-05  3.79E-04  5.81E-04  7.46E-04
 
 OM33
+       -1.74E-03 -1.61E-03 -3.83E-04  5.87E-03  2.24E-04  1.49E-04  8.01E-04  3.90E-04  5.49E-04  4.35E-04  4.07E-04  4.19E-03
 
 OM34
+       -1.07E-04 -4.47E-04  6.30E-04  3.06E-03  2.52E-04  1.18E-04  2.92E-04  2.22E-04  3.82E-04  3.69E-04  3.35E-04  2.19E-03
          2.03E-03
 
 OM44
+        1.71E-04 -8.02E-05  4.64E-04 -3.04E-03  2.71E-04  1.26E-04  6.72E-05  1.57E-04  3.27E-04  1.92E-04  2.29E-04  1.11E-03
          1.34E-03  1.64E-03
 
 SG11
+        2.45E-04  3.04E-04 -1.54E-05  2.74E-04 -6.03E-05 -4.30E-05 -5.85E-05 -7.99E-05 -8.09E-05  8.14E-06 -4.16E-05 -1.04E-04
         -1.13E-04 -1.20E-04  6.03E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.48E-01
 
 TH 2
+        1.61E-01  2.40E-01
 
 TH 3
+        1.19E-01  6.85E-02  1.46E-01
 
 TH 4
+       -1.26E-02  5.96E-02  4.56E-01  5.78E-01
 
 OM11
+       -1.83E-01  6.93E-02  4.20E-02 -5.20E-02  2.89E-02
 
 OM12
+        1.39E-02  1.57E-01  2.75E-02 -1.05E-01  4.64E-01  2.31E-02
 
 OM13
+       -1.10E-01  1.02E-03 -1.17E-01  1.33E-01  1.49E-01  3.23E-01  3.23E-02
 
 OM14
+       -1.39E-01 -1.57E-01  8.67E-02  1.22E-01  2.77E-01  2.86E-01  6.08E-01  2.76E-02
 
 OM22
+       -2.40E-02 -1.10E-02 -9.07E-02 -5.76E-02  1.38E-01  2.93E-01  7.56E-02  1.46E-01  3.48E-02
 
 OM23
+       -4.29E-02 -5.08E-02  2.63E-02  6.26E-02  1.71E-01  4.32E-02 -5.04E-02 -2.93E-02  2.13E-01  3.80E-02
 
 OM24
+       -2.05E-01 -7.77E-02  8.26E-02  1.08E-01  1.46E-01  1.36E-01  2.54E-02  1.21E-01  3.99E-01  5.59E-01  2.73E-02
 
 OM33
+       -1.08E-01 -1.04E-01 -4.04E-02  1.57E-01  1.20E-01  1.00E-01  3.84E-01  2.18E-01  2.44E-01  1.77E-01  2.30E-01  6.47E-02
 
 OM34
+       -9.56E-03 -4.14E-02  9.53E-02  1.17E-01  1.94E-01  1.13E-01  2.01E-01  1.78E-01  2.43E-01  2.15E-01  2.72E-01  7.51E-01
          4.51E-02
 
 OM44
+        1.70E-02 -8.27E-03  7.84E-02 -1.30E-01  2.32E-01  1.36E-01  5.15E-02  1.41E-01  2.33E-01  1.25E-01  2.07E-01  4.24E-01
          7.32E-01  4.04E-02
 
 SG11
+        1.27E-01  1.63E-01 -1.35E-02  6.11E-02 -2.69E-01 -2.40E-01 -2.34E-01 -3.73E-01 -2.99E-01  2.75E-02 -1.96E-01 -2.06E-01
         -3.24E-01 -3.83E-01  7.77E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.95E+01
 
 TH 2
+       -2.19E+00  2.06E+01
 
 TH 3
+       -4.57E+00 -1.04E+00  7.07E+01
 
 TH 4
+        3.30E-01 -9.44E-01 -9.02E+00  4.78E+00
 
 OM11
+        4.10E+01 -1.92E+01  5.77E-01  8.32E-01  1.82E+03
 
 OM12
+       -2.89E+01 -3.53E+01 -6.05E+01  2.15E+01 -9.71E+02  3.09E+03
 
 OM13
+        4.25E+00 -2.92E+01  9.47E+01 -1.42E+01  2.41E+02 -6.80E+02  2.12E+03
 
 OM14
+        5.98E+00  4.86E+01 -7.16E+01 -3.47E+00 -3.83E+02  1.01E+01 -1.33E+03  2.62E+03
 
 OM22
+       -1.19E+01 -7.12E+00  4.34E+01 -5.38E-01  1.16E+02 -4.48E+02  2.17E+02 -1.25E+02  1.18E+03
 
 OM23
+       -1.56E+01  1.01E+01  9.44E+00 -8.98E-02 -2.80E+02  1.13E+02  9.55E+00  1.20E+02 -2.37E+01  1.11E+03
 
 OM24
+        5.14E+01  2.03E+00 -2.98E+01 -7.98E+00  1.90E+02 -1.91E+02  1.41E+02 -1.40E+02 -4.75E+02 -8.76E+02  2.49E+03
 
 OM33
+        1.23E+01  1.47E+01  2.12E+01 -3.26E+00  1.26E+01  5.21E+01 -3.83E+02  1.07E+02 -1.26E+02 -2.45E+00 -2.10E+00  7.29E+02
 
 OM34
+       -1.97E+01 -6.36E+00 -1.47E+01 -1.68E+01 -5.54E+01  1.59E+01  1.26E+02  3.37E+01  7.10E+01 -1.30E+02 -7.85E+01 -8.78E+02
          2.36E+03
 
 OM44
+       -3.80E+00 -1.13E+01 -3.61E+01  2.67E+01 -1.28E+02  8.55E+01  1.27E+02 -9.16E+01 -6.05E+01  6.15E+01 -5.05E+01  2.01E+02
         -1.29E+03  1.70E+03
 
 SG11
+       -4.01E+01 -1.18E+02  1.05E+01 -1.29E+01  7.67E+02  3.07E+02  4.94E+02  1.31E+03  1.00E+03 -9.25E+02  7.60E+02 -4.50E+02
          7.16E+02  1.06E+03  2.53E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.28E-01  2.51E-01  3.18E-01  3.39E-01  4.51E-01  5.04E-01  6.13E-01  7.78E-01  9.48E-01  1.05E+00  1.37E+00  1.44E+00
          1.57E+00  1.75E+00  3.48E+00
 
1
 
 
 #TBLN:      2
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            2400
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
 EM OR BAYESIAN METHOD USED:              MCMC BAYESIAN (BAYES)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        50          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              200         
 ITERATIONS (NITER):                      500         
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        1           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2           
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0           
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2           
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2           
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS 
 SAMPLED THETAS AND SIGMAS: 
 PROPOSAL DENSITY SCALING RANGE 
              (PSCALE_MIN, PSCALE_MAX):   1.000000000000000E-02   ,1000.000000000000       
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
   1   2   3   4
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3   4
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration         -200 MCMCOBJ=   -2558.94076613630     
 iteration         -150 MCMCOBJ=   -2229.63409546415     
 iteration         -100 MCMCOBJ=   -2403.60157258356     
 iteration          -50 MCMCOBJ=   -2292.26393312381     
 Sampling Mode
 iteration            0 MCMCOBJ=   -2381.11588333206     
 iteration           50 MCMCOBJ=   -2351.44959109583     
 iteration          100 MCMCOBJ=   -2262.81198149537     
 iteration          150 MCMCOBJ=   -2327.72142804596     
 iteration          200 MCMCOBJ=   -2356.01445022275     
 iteration          250 MCMCOBJ=   -2311.84269161323     
 iteration          300 MCMCOBJ=   -2372.21622971362     
 iteration          350 MCMCOBJ=   -2320.09918992675     
 iteration          400 MCMCOBJ=   -2262.73048233276     
 iteration          450 MCMCOBJ=   -2322.41561695304     
 iteration          500 MCMCOBJ=   -2311.34505636761     
 
 #TERM:
 BURN-IN WAS NOT COMPLETED
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation time in seconds:    10.19
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -2316.002       **************************************************
 #OBJS:********************************************       41.666 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         5.12E+00  4.72E+00  2.10E+00  1.04E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.76E-01
 
 ETA2
+       -2.04E-03  1.49E-01
 
 ETA3
+        4.54E-03 -4.32E-03  1.77E-01
 
 ETA4
+       -2.23E-02  8.69E-03  3.21E-03  1.40E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.11E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.18E-01
 
 ETA2
+       -1.34E-02  3.83E-01
 
 ETA3
+        2.27E-02 -3.48E-02  4.17E-01
 
 ETA4
+       -1.47E-01  5.85E-02  1.09E-02  3.72E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.47E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.41E-01  2.75E-01  1.47E-01  5.55E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.94E-02
 
 ETA2
+        2.36E-02  3.92E-02
 
 ETA3
+        2.58E-02  3.00E-02  5.02E-02
 
 ETA4
+        2.01E-02  2.10E-02  2.31E-02  3.20E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        7.19E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.46E-02
 
 ETA2
+        1.44E-01  4.93E-02
 
 ETA3
+        1.40E-01  1.84E-01  5.80E-02
 
 ETA4
+        1.25E-01  1.44E-01  1.46E-01  4.25E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.46E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        5.79E-02
 
 TH 2
+        1.48E-02  7.58E-02
 
 TH 3
+        8.63E-03  1.29E-02  2.16E-02
 
 TH 4
+        1.56E-02  5.44E-02  4.15E-02  3.08E-01
 
 OM11
+        7.82E-04  1.05E-03  3.93E-04  2.41E-03  8.62E-04
 
 OM12
+        8.87E-04  1.22E-03  7.59E-04  1.97E-03  1.08E-04  5.58E-04
 
 OM13
+       -1.91E-04  6.93E-04 -5.92E-04 -3.80E-04  1.33E-04 -9.58E-06  6.67E-04
 
 OM14
+        8.94E-05  1.19E-03  1.50E-04 -9.42E-05  1.75E-05  6.32E-05  1.23E-04  4.02E-04
 
 OM22
+       -1.58E-03 -2.28E-03 -2.58E-03 -8.46E-03 -2.38E-04 -7.59E-05 -4.55E-05 -2.21E-05  1.54E-03
 
 OM23
+       -2.87E-04  4.45E-05  6.16E-04 -4.33E-04 -6.52E-05  2.37E-05  1.56E-05 -3.89E-05 -1.26E-04  9.01E-04
 
 OM24
+       -7.92E-04 -1.33E-03 -4.90E-04 -2.27E-03 -1.01E-04 -7.36E-05 -5.65E-05 -6.27E-05  2.22E-04  8.02E-05  4.41E-04
 
 OM33
+       -9.67E-04 -1.90E-03 -1.12E-03 -3.37E-03 -1.07E-04 -1.06E-04  1.52E-04 -1.99E-04 -2.14E-05  5.63E-04  2.25E-04  2.52E-03
 
 OM34
+       -3.46E-04 -2.99E-04 -2.41E-04 -2.74E-03 -1.04E-05 -1.83E-05  1.53E-05  3.88E-05  7.59E-05  9.70E-05  5.94E-05  2.79E-04
          5.35E-04
 
 OM44
+       -3.63E-04  7.37E-04 -5.58E-04 -1.87E-03  3.50E-05  9.50E-05  3.68E-05  2.58E-05  2.26E-04 -2.29E-04  1.29E-05 -2.67E-04
          2.33E-04  1.02E-03
 
 SG11
+        1.68E-04  1.95E-04  2.90E-04  1.21E-03  2.53E-05  2.18E-06 -6.83E-07  3.36E-06 -9.49E-05  1.10E-05 -1.87E-05 -5.74E-05
         -4.47E-05 -6.83E-05  5.17E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.41E-01
 
 TH 2
+        2.23E-01  2.75E-01
 
 TH 3
+        2.44E-01  3.20E-01  1.47E-01
 
 TH 4
+        1.17E-01  3.56E-01  5.09E-01  5.55E-01
 
 OM11
+        1.11E-01  1.30E-01  9.13E-02  1.48E-01  2.94E-02
 
 OM12
+        1.56E-01  1.87E-01  2.19E-01  1.50E-01  1.56E-01  2.36E-02
 
 OM13
+       -3.07E-02  9.75E-02 -1.56E-01 -2.65E-02  1.76E-01 -1.57E-02  2.58E-02
 
 OM14
+        1.85E-02  2.16E-01  5.10E-02 -8.47E-03  2.97E-02  1.33E-01  2.37E-01  2.01E-02
 
 OM22
+       -1.67E-01 -2.11E-01 -4.48E-01 -3.89E-01 -2.07E-01 -8.19E-02 -4.49E-02 -2.81E-02  3.92E-02
 
 OM23
+       -3.97E-02  5.39E-03  1.40E-01 -2.60E-02 -7.40E-02  3.34E-02  2.02E-02 -6.46E-02 -1.07E-01  3.00E-02
 
 OM24
+       -1.57E-01 -2.31E-01 -1.59E-01 -1.95E-01 -1.64E-01 -1.48E-01 -1.04E-01 -1.49E-01  2.70E-01  1.27E-01  2.10E-02
 
 OM33
+       -8.00E-02 -1.38E-01 -1.52E-01 -1.21E-01 -7.24E-02 -8.98E-02  1.17E-01 -1.98E-01 -1.09E-02  3.73E-01  2.13E-01  5.02E-02
 
 OM34
+       -6.22E-02 -4.70E-02 -7.11E-02 -2.13E-01 -1.53E-02 -3.36E-02  2.56E-02  8.37E-02  8.36E-02  1.40E-01  1.22E-01  2.40E-01
          2.31E-02
 
 OM44
+       -4.72E-02  8.36E-02 -1.19E-01 -1.05E-01  3.72E-02  1.26E-01  4.45E-02  4.03E-02  1.80E-01 -2.38E-01  1.93E-02 -1.66E-01
          3.15E-01  3.20E-02
 
 SG11
+        9.72E-02  9.88E-02  2.74E-01  3.04E-01  1.20E-01  1.28E-02 -3.68E-03  2.33E-02 -3.37E-01  5.11E-02 -1.24E-01 -1.59E-01
         -2.69E-01 -2.97E-01  7.19E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.96E+01
 
 TH 2
+       -2.90E+00  1.78E+01
 
 TH 3
+       -5.99E+00 -4.55E+00  8.30E+01
 
 TH 4
+        9.18E-01 -2.28E+00 -7.32E+00  5.27E+00
 
 OM11
+       -8.25E+00 -3.97E+00  1.05E+01 -3.70E+00  1.32E+03
 
 OM12
+       -1.89E+01 -7.02E+00 -5.28E+01 -4.12E+00 -2.17E+02  2.08E+03
 
 OM13
+        3.84E+00 -1.36E+01  7.02E+01 -1.57E+00 -2.71E+02  1.15E+02  1.81E+03
 
 OM14
+        1.21E+01 -4.10E+01 -1.51E+01  1.18E+01  1.34E+02 -3.10E+02 -6.08E+02  3.05E+03
 
 OM22
+        5.62E+00  9.52E-01  7.90E+01  1.02E+01  1.50E+02 -6.75E+01  6.79E+01 -4.45E+00  9.86E+02
 
 OM23
+        1.32E+01 -1.09E+01 -6.11E+01  9.72E+00  9.70E+01 -1.67E+02 -6.69E+01  8.96E+01  4.24E+01  1.47E+03
 
 OM24
+        1.56E+01  2.30E+01 -1.43E+01  3.82E+00  1.02E+02  2.01E+02  1.85E+02  1.66E+02 -3.30E+02 -1.69E+02  2.77E+03
 
 OM33
+       -2.06E-01  2.32E+00  3.41E+01 -1.95E+00  3.72E+01 -2.01E+01 -1.74E+02  2.88E+02  8.94E+01 -2.72E+02 -1.77E+02  5.89E+02
 
 OM34
+        1.49E+00  6.02E+00 -3.94E+01  1.52E+01 -9.43E+01  1.97E+02  1.21E+02 -4.57E+02  4.13E+01 -2.56E+02 -8.76E+01 -2.94E+02
          2.54E+03
 
 OM44
+        9.49E+00 -2.06E+01  1.72E+01  7.09E-01 -2.53E+01 -2.75E+02 -1.21E+02  2.07E+02 -6.39E+01  2.94E+02 -8.24E+01  1.97E+02
         -6.59E+02  1.39E+03
 
 SG11
+       -9.89E+00  1.88E+01 -7.84E+01 -4.59E+01 -3.62E+02  2.90E+02 -1.69E+02 -1.03E+02  9.61E+02 -3.66E+02 -1.65E+02  6.27E+02
          1.01E+03  1.23E+03  2.60E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           EIGENVALUES OF COR MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         3.61E-01  4.04E-01  4.97E-01  5.41E-01  6.15E-01  6.38E-01  7.91E-01  8.16E-01  8.79E-01  9.46E-01  1.03E+00  1.28E+00
          1.49E+00  1.79E+00  2.92E+00
 
1
 
 
 #TBLN:      3
 #METH: First Order Conditional Estimation with Interaction (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            9999
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    10          
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   10          
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               ON 
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -1115.11256332678        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  5.1160E+00  4.7168E+00  2.0975E+00  1.0392E+01  1.7578E-01 -2.0433E-03  4.5368E-03 -2.2287E-02  1.4894E-01 -4.3223E-03
             8.6878E-03  1.7715E-01  3.2128E-03  1.3994E-01  6.1079E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01 -1.0000E-01
             1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:  -7.2389E+02 -6.8114E+02 -1.3867E+02 -4.6093E+02  1.6251E+01  3.6263E-01 -4.2856E-01 -1.5023E-01  8.1411E+00 -5.8130E-01
            -1.8938E-01  1.3655E+01 -3.1420E+00  9.6968E+00  2.5001E+01
 
0ITERATION NO.:    1    OBJECTIVE VALUE:  -1117.61970553977        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:       18
 NPARAMETR:  5.3626E+00  4.9196E+00  2.1061E+00  1.0849E+01  1.7576E-01 -2.0432E-03  4.5366E-03 -2.2285E-02  1.4893E-01 -4.3221E-03
             8.6876E-03  1.7713E-01  3.2131E-03  1.3993E-01  6.1067E-02
 PARAMETER:  1.0288E-01  1.0271E-01  1.0055E-01  1.0184E-01  9.9935E-02 -1.0000E-01  1.0000E-01 -9.9999E-02  9.9968E-02 -9.9998E-02
             1.0000E-01  9.9946E-02  1.0001E-01  9.9961E-02  9.9900E-02
 GRADIENT:   4.6567E+01 -2.6378E+02 -2.5851E+02  4.8182E+02  1.5909E+01  2.6828E-01 -8.6596E-01 -8.1647E-01  9.6077E+00 -5.1651E-01
             1.5354E-01  1.1533E+01 -2.9385E+00  7.0103E+00  1.9121E+01
 
0ITERATION NO.:    2    OBJECTIVE VALUE:  -1117.62061707903        NO. OF FUNC. EVALS.:  12
 CUMULATIVE NO. OF FUNC. EVALS.:       30
 NPARAMETR:  5.3465E+00  5.0000E+00  2.1221E+00  1.0374E+01  1.7573E-01 -2.0431E-03  4.5365E-03 -2.2283E-02  1.4892E-01 -4.3218E-03
             8.6872E-03  1.7711E-01  3.2134E-03  1.3992E-01  6.1057E-02
 PARAMETER:  1.0270E-01  1.0376E-01  1.0158E-01  9.9927E-02  9.9872E-02 -1.0000E-01  1.0001E-01 -9.9996E-02  9.9929E-02 -9.9996E-02
             1.0000E-01  9.9900E-02  1.0002E-01  9.9934E-02  9.9825E-02
 GRADIENT:  -7.3361E+01  3.0286E+01 -1.0168E+02 -6.9641E+02  1.5908E+01  2.9273E-01 -5.7368E-01 -9.6018E-01  9.4917E+00 -7.8675E-01
             9.6555E-01  1.3343E+01 -3.2726E+00  7.0999E+00  2.0205E+01
 
0ITERATION NO.:    3    OBJECTIVE VALUE:  -1117.98763496313        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:       41
 NPARAMETR:  5.3555E+00  5.1276E+00  2.1593E+00  1.0547E+01  1.7566E-01 -2.0428E-03  4.5360E-03 -2.2276E-02  1.4888E-01 -4.3210E-03
             8.6857E-03  1.7706E-01  3.2144E-03  1.3990E-01  6.1027E-02
 PARAMETER:  1.0280E-01  1.0538E-01  1.0392E-01  1.0063E-01  9.9673E-02 -1.0001E-01  1.0001E-01 -9.9985E-02  9.9810E-02 -9.9988E-02
             9.9994E-02  9.9745E-02  1.0006E-01  9.9845E-02  9.9579E-02
 GRADIENT:  -1.0513E+02  3.5128E+02 -5.8959E+01 -4.9558E+02  1.4674E+01  1.7202E-01 -6.7292E-01 -2.3816E+00  9.5572E+00 -1.0350E+00
             1.1250E+00  1.2656E+01 -3.2014E+00  6.4343E+00  1.7727E+01
 
0ITERATION NO.:    4    OBJECTIVE VALUE:  -1118.44994404768        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:       52
 NPARAMETR:  5.4294E+00  5.0627E+00  2.2406E+00  1.0640E+01  1.7550E-01 -2.0420E-03  4.5349E-03 -2.2255E-02  1.4880E-01 -4.3187E-03
             8.6814E-03  1.7692E-01  3.2169E-03  1.3984E-01  6.0957E-02
 PARAMETER:  1.0364E-01  1.0456E-01  1.0891E-01  1.0101E-01  9.9199E-02 -1.0001E-01  1.0004E-01 -9.9938E-02  9.9514E-02 -9.9964E-02
             9.9974E-02  9.9371E-02  1.0016E-01  9.9637E-02  9.9006E-02
 GRADIENT:   8.6238E+01  1.7678E+02  6.8961E+01 -5.4057E+02  1.3952E+01  9.7074E-02 -4.0092E-01 -3.5090E+00  1.0791E+01 -1.4907E+00
             5.5901E-01  1.1213E+01 -3.1427E+00  6.8366E+00  1.6166E+01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -1118.48829084206        NO. OF FUNC. EVALS.:  12
 CUMULATIVE NO. OF FUNC. EVALS.:       64
 NPARAMETR:  5.3855E+00  5.0493E+00  2.2495E+00  1.0659E+01  1.7546E-01 -2.0418E-03  4.5346E-03 -2.2249E-02  1.4877E-01 -4.3181E-03
             8.6805E-03  1.7689E-01  3.2176E-03  1.3982E-01  6.0942E-02
 PARAMETER:  1.0314E-01  1.0439E-01  1.0944E-01  1.0108E-01  9.9093E-02 -1.0001E-01  1.0004E-01 -9.9919E-02  9.9439E-02 -9.9956E-02
             9.9971E-02  9.9290E-02  1.0018E-01  9.9587E-02  9.8883E-02
 GRADIENT:  -5.7336E+01  1.5298E+02  8.8531E+01 -5.2926E+02  1.3973E+01  1.1908E-01 -3.2219E-01 -3.8220E+00  1.0961E+01 -1.5359E+00
             4.6239E-01  1.1022E+01 -3.1070E+00  7.0211E+00  1.6640E+01
 
0ITERATION NO.:    6    OBJECTIVE VALUE:  -1119.26205689451        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       73
 NPARAMETR:  5.3654E+00  5.0278E+00  2.2384E+00  1.0608E+01  1.6420E-01 -1.9809E-03  4.4356E-03 -1.9753E-02  1.4136E-01 -4.0797E-03
             8.4241E-03  1.6850E-01  3.4437E-03  1.3498E-01  5.6417E-02
 PARAMETER:  1.0292E-01  1.0412E-01  1.0878E-01  1.0088E-01  6.5924E-02 -1.0030E-01  1.0116E-01 -9.1704E-02  7.3878E-02 -9.6795E-02
             9.9688E-02  7.4965E-02  1.0714E-01  8.3178E-02  6.0303E-02
 GRADIENT:  -5.5716E+01  1.6266E+02  9.0950E+01 -5.5081E+02  2.2071E+00  1.8011E-01 -1.5176E-01 -1.5487E+00  4.1847E+00 -1.6713E+00
             6.8231E-01  5.2570E+00 -2.9114E+00  9.7725E-01 -1.5231E+01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:  -1119.62756879234        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       82
 NPARAMETR:  5.3710E+00  5.0300E+00  2.2409E+00  1.0608E+01  1.5571E-01 -1.9430E-03  4.3866E-03 -1.7461E-02  1.3385E-01 -3.7184E-03
             8.0933E-03  1.5935E-01  3.8591E-03  1.3133E-01  5.9158E-02
 PARAMETER:  1.0298E-01  1.0414E-01  1.0893E-01  1.0088E-01  3.9397E-02 -1.0103E-01  1.0273E-01 -8.3243E-02  4.6570E-02 -9.0518E-02
             9.8563E-02  4.7051E-02  1.1949E-01  7.0712E-02  8.4025E-02
 GRADIENT:  -5.9105E+01  1.6862E+02  9.6971E+01 -5.8416E+02 -6.9144E+00  4.2563E-01  5.4575E-02  1.9370E+00  9.7000E-01 -1.6880E+00
             9.7600E-01  3.2217E+00 -2.0381E+00 -9.4987E-01 -2.5909E+00
 
0ITERATION NO.:    8    OBJECTIVE VALUE:  -1119.76089033664        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       91
 NPARAMETR:  5.3733E+00  5.0324E+00  2.2403E+00  1.0599E+01  1.6099E-01 -2.0232E-03  4.5159E-03 -1.9235E-02  1.2878E-01 -3.2590E-03
             7.7041E-03  1.5095E-01  4.2562E-03  1.3074E-01  5.9532E-02
 PARAMETER:  1.0301E-01  1.0417E-01  1.0889E-01  1.0084E-01  5.6071E-02 -1.0346E-01  1.0401E-01 -9.0184E-02  2.7259E-02 -8.0661E-02
             9.5215E-02  1.9982E-02  1.3392E-01  6.7091E-02  8.7177E-02
 GRADIENT:  -5.6349E+01  1.7281E+02  1.0257E+02 -6.0489E+02 -1.0865E+00  4.3085E-01  1.4110E-01 -6.0245E-01 -1.8015E+00 -1.7479E+00
             1.1991E+00  1.3322E-02 -1.4069E+00 -2.1234E+00 -3.3922E+00
 
0ITERATION NO.:    9    OBJECTIVE VALUE:  -1119.81282896099        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       99
 NPARAMETR:  5.3742E+00  5.0442E+00  2.2340E+00  1.0583E+01  1.6009E-01 -2.3387E-03  4.6446E-03 -1.7097E-02  1.3206E-01 -9.1965E-04
             5.4939E-03  1.4211E-01  6.3611E-03  1.4121E-01  5.9167E-02
 PARAMETER:  1.0302E-01  1.0433E-01  1.0851E-01  1.0078E-01  5.3255E-02 -1.1993E-01  1.0727E-01 -8.0385E-02  3.9808E-02 -2.1188E-02
             6.6077E-02 -9.9912E-03  1.9089E-01  1.0833E-01  8.4103E-02
 GRADIENT:  -4.8813E+01  1.5871E+02  1.0084E+02 -5.7160E+02 -2.3753E+00  3.1924E-01  9.5172E-02  1.0502E+00  4.5190E-01 -1.3758E+00
            -4.3653E-01 -3.1698E+00 -1.2326E+00  4.4487E+00 -4.1786E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -1120.10193508506        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      107
 NPARAMETR:  5.3755E+00  5.0420E+00  2.2373E+00  1.0598E+01  1.6046E-01 -3.0354E-03  4.9651E-03 -1.7308E-02  1.3542E-01  4.2185E-03
             2.6419E-03  1.4743E-01  1.0938E-02  1.3656E-01  5.8899E-02
 PARAMETER:  1.0303E-01  1.0430E-01  1.0871E-01  1.0084E-01  5.4411E-02 -1.5548E-01  1.1454E-01 -8.1281E-02  5.2270E-02  1.0594E-01
             2.8802E-02  7.8857E-03  3.1020E-01  8.9729E-02  8.1827E-02
 GRADIENT:  -4.3875E+01  1.3420E+02  9.0366E+01 -5.2697E+02 -1.8184E+00  1.4832E-01 -3.3443E-02  1.7668E+00  3.1695E+00 -6.8785E-01
            -2.0676E+00 -3.7107E+00 -4.9297E-01 -5.4501E-01 -3.7415E+00
 
0ITERATION NO.:   11    OBJECTIVE VALUE:  -1120.32879913083        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      115
 NPARAMETR:  5.3810E+00  5.0332E+00  2.2477E+00  1.0641E+01  1.5905E-01 -3.7691E-03  5.3826E-03 -2.0585E-02  1.2738E-01  9.4643E-03
             1.6730E-03  1.6265E-01  1.6244E-02  1.4281E-01  5.8436E-02
 PARAMETER:  1.0309E-01  1.0419E-01  1.0934E-01  1.0101E-01  5.0000E-02 -1.9392E-01  1.2473E-01 -9.7101E-02  2.1560E-02  2.4299E-01
             1.5209E-02  5.5200E-02  4.3735E-01  1.0667E-01  7.7886E-02
 GRADIENT:  -3.4592E+01  1.0942E+02  7.4656E+01 -4.5599E+02 -3.3689E+00  9.5759E-02  2.1991E-01 -3.4849E+00 -1.4315E+00 -1.3310E-01
            -2.9279E+00 -4.6032E-01 -7.0004E-01  3.1634E+00 -3.9879E+00
 
0ITERATION NO.:   12    OBJECTIVE VALUE:  -1120.40606086590        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      123
 NPARAMETR:  5.3827E+00  5.0297E+00  2.2704E+00  1.0715E+01  1.6446E-01 -4.4721E-03  5.8163E-03 -1.1537E-02  1.2244E-01  1.3487E-02
             4.4696E-03  1.7464E-01  2.1854E-02  1.4506E-01  5.7769E-02
 PARAMETER:  1.0311E-01  1.0414E-01  1.1070E-01  1.0131E-01  6.6706E-02 -2.2627E-01  1.3254E-01 -5.3518E-02  1.6316E-03  3.5262E-01
             5.4402E-02  8.8573E-02  5.4715E-01  1.1732E-01  7.2144E-02
 GRADIENT:  -3.3551E+01  8.9407E+01  6.5288E+01 -4.0642E+02  8.2700E-01 -1.9588E-01 -8.5751E-01  6.7998E+00 -4.1619E+00  8.9727E-02
            -2.7605E+00  1.2744E+00 -7.9663E-01  2.8570E+00 -4.2410E+00
 
0ITERATION NO.:   13    OBJECTIVE VALUE:  -1120.73681915103        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:      132
 NPARAMETR:  5.3855E+00  5.0129E+00  2.2767E+00  1.0766E+01  1.6332E-01 -5.3127E-03  6.9183E-03 -1.2258E-02  1.2738E-01  1.9635E-02
             1.4417E-02  1.8163E-01  3.0569E-02  1.5190E-01  5.7115E-02
 PARAMETER:  1.0314E-01  1.0392E-01  1.1107E-01  1.0151E-01  6.3245E-02 -2.6974E-01  1.5820E-01 -5.7062E-02  2.1211E-02  5.0328E-01
             1.7994E-01  1.0374E-01  7.1449E-01  1.2918E-01  6.6447E-02
 GRADIENT:  -2.9346E+01  5.5175E+01  5.4621E+01 -3.2916E+02 -2.5862E-01 -4.4803E-01 -6.2027E-01  3.6934E+00 -2.0239E+00  2.5401E-01
             1.5976E-03 -2.0753E-02 -2.1121E-01  2.9423E+00 -3.4036E+00
 
0ITERATION NO.:   14    OBJECTIVE VALUE:  -1120.95348669637        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      140
 NPARAMETR:  5.3923E+00  5.0136E+00  2.2667E+00  1.0846E+01  1.6546E-01 -2.8673E-03  1.4298E-02 -1.0979E-02  1.2942E-01  1.7825E-02
             1.4117E-02  1.8705E-01  3.2288E-02  1.5064E-01  5.7026E-02
 PARAMETER:  1.0322E-01  1.0393E-01  1.1048E-01  1.0183E-01  6.9759E-02 -1.4463E-01  3.2484E-01 -5.0775E-02  2.9653E-02  4.5414E-01
             1.7727E-01  1.1773E-01  7.6286E-01  1.2334E-01  6.5670E-02
 GRADIENT:  -1.5360E+01  2.5265E+01  1.9035E+01 -1.3458E+02  1.7770E-01 -2.5926E-01  2.4746E-01  1.6054E+00 -1.0673E+00  1.8972E-01
            -3.2384E-02  2.0031E-01 -2.2648E-01  1.2543E+00 -1.4193E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -1120.96307403375        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      148
 NPARAMETR:  5.3949E+00  5.0004E+00  2.2552E+00  1.0898E+01  1.6419E-01  9.7040E-04  8.4028E-03 -1.4550E-02  1.3272E-01  1.3374E-02
             1.3325E-02  1.8940E-01  3.6158E-02  1.5128E-01  5.6997E-02
 PARAMETER:  1.0325E-01  1.0376E-01  1.0979E-01  1.0203E-01  6.5902E-02  4.9139E-02  1.9164E-01 -6.7551E-02  4.2388E-02  3.3059E-01
             1.6855E-01  1.2943E-01  8.5676E-01  1.1921E-01  6.5416E-02
 GRADIENT:   3.7108E+00 -1.0333E+01 -1.6737E+01  5.6309E+01 -5.9581E-02  2.6382E-01 -6.0181E-01 -4.3517E-01  1.2685E-01 -1.4797E-01
             8.6784E-02  1.7433E-01  2.9188E-01 -4.8817E-01  9.0249E-01
 
0ITERATION NO.:   16    OBJECTIVE VALUE:  -1121.02033488363        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      158
 NPARAMETR:  5.3955E+00  5.0041E+00  2.2604E+00  1.0893E+01  1.6464E-01 -6.3816E-04  1.1184E-02 -1.3513E-02  1.3181E-01  1.5206E-02
             1.3608E-02  1.8634E-01  3.2954E-02  1.4940E-01  5.7192E-02
 PARAMETER:  1.0326E-01  1.0381E-01  1.1010E-01  1.0201E-01  6.7270E-02 -3.2271E-02  2.5472E-01 -6.2651E-02  3.8980E-02  3.7964E-01
             1.7095E-01  1.1917E-01  7.8641E-01  1.1684E-01  6.7127E-02
 GRADIENT:   2.0634E+00 -3.9760E+00 -4.0449E+00  1.5843E+01  2.6048E-02  5.6594E-02 -1.2354E-01 -1.1920E-01  7.5768E-02 -2.7520E-02
             3.6608E-02  7.2811E-03  5.0674E-02 -2.7710E-01  1.4875E-01
 
0ITERATION NO.:   17    OBJECTIVE VALUE:  -1121.02202820034        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      166
 NPARAMETR:  5.3953E+00  5.0064E+00  2.2614E+00  1.0889E+01  1.6479E-01 -1.1056E-03  1.2226E-02 -1.3000E-02  1.3155E-01  1.5291E-02
             1.3554E-02  1.8681E-01  3.3057E-02  1.4986E-01  5.7157E-02
 PARAMETER:  1.0326E-01  1.0384E-01  1.1016E-01  1.0199E-01  6.7734E-02 -5.5884E-02  2.7833E-01 -6.0242E-02  3.7988E-02  3.8310E-01
             1.6999E-01  1.1995E-01  7.8931E-01  1.1867E-01  6.6817E-02
 GRADIENT:  -9.3903E-01  1.2212E+00  4.1562E-01 -3.6318E+00 -1.2068E-02 -7.7238E-03  8.5374E-03  1.4402E-02 -1.9027E-02 -7.5204E-03
            -1.4107E-02  1.1874E-02  8.1371E-03  1.0476E-01  4.3251E-02
 
0ITERATION NO.:   18    OBJECTIVE VALUE:  -1121.02287477131        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      174
 NPARAMETR:  5.3958E+00  5.0055E+00  2.2627E+00  1.0893E+01  1.6479E-01 -9.0428E-04  1.1905E-02 -1.3184E-02  1.3152E-01  1.6022E-02
             1.3761E-02  1.8568E-01  3.2151E-02  1.4904E-01  5.7231E-02
 PARAMETER:  1.0326E-01  1.0383E-01  1.1023E-01  1.0201E-01  6.7710E-02 -4.5708E-02  2.7102E-01 -6.1096E-02  3.7882E-02  4.0095E-01
             1.7281E-01  1.1653E-01  7.6715E-01  1.1661E-01  6.7462E-02
 GRADIENT:   1.5037E+00 -1.9466E+00 -1.3311E-01  4.2907E+00  4.9053E-03  1.1825E-02 -1.4126E-03 -8.5670E-03  2.5266E-02  2.4437E-02
             1.7780E-02 -2.7389E-02 -2.3090E-02 -1.4021E-01 -9.9755E-02
 
0ITERATION NO.:   19    OBJECTIVE VALUE:  -1121.02288094975        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      182
 NPARAMETR:  5.3957E+00  5.0057E+00  2.2623E+00  1.0892E+01  1.6478E-01 -9.6167E-04  1.1973E-02 -1.3146E-02  1.3154E-01  1.5802E-02
             1.3702E-02  1.8597E-01  3.2392E-02  1.4924E-01  5.7212E-02
 PARAMETER:  1.0326E-01  1.0383E-01  1.1021E-01  1.0201E-01  6.7704E-02 -4.8609E-02  2.7257E-01 -6.0922E-02  3.7929E-02  3.9554E-01
             1.7201E-01  1.1745E-01  7.7313E-01  1.1711E-01  6.7297E-02
 GRADIENT:   8.9191E-01 -1.1666E+00 -9.8387E-02  2.6437E+00  2.9177E-03  6.8441E-03 -1.0095E-03 -5.0048E-03  1.4854E-02  1.4205E-02
             1.1022E-02 -1.6016E-02 -1.3640E-02 -8.6263E-02 -5.9039E-02
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -1121.02288094975        NO. OF FUNC. EVALS.:  19
 CUMULATIVE NO. OF FUNC. EVALS.:      201
 NPARAMETR:  5.3957E+00  5.0057E+00  2.2623E+00  1.0892E+01  1.6478E-01 -9.6167E-04  1.1973E-02 -1.3146E-02  1.3154E-01  1.5802E-02
             1.3702E-02  1.8597E-01  3.2392E-02  1.4924E-01  5.7212E-02
 PARAMETER:  1.0326E-01  1.0383E-01  1.1021E-01  1.0201E-01  6.7704E-02 -4.8609E-02  2.7257E-01 -6.0922E-02  3.7929E-02  3.9554E-01
             1.7201E-01  1.1745E-01  7.7313E-01  1.1711E-01  6.7297E-02
 GRADIENT:  -1.2970E+01 -2.3571E+00 -2.1160E+00 -8.5627E+01  2.9177E-03  6.8441E-03 -1.0095E-03 -5.0048E-03  1.4854E-02  1.4205E-02
             1.1022E-02 -1.6016E-02 -1.3640E-02 -8.6263E-02 -6.0041E-02
 
0ITERATION NO.:   21    OBJECTIVE VALUE:  -1121.02720194622        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      215
 NPARAMETR:  5.3983E+00  5.0074E+00  2.2661E+00  1.0915E+01  1.6494E-01 -8.4120E-04  1.2202E-02 -1.2934E-02  1.3147E-01  1.5957E-02
             1.3826E-02  1.8688E-01  3.2846E-02  1.4953E-01  5.7192E-02
 PARAMETER:  1.0329E-01  1.0385E-01  1.1044E-01  1.0210E-01  6.8168E-02 -4.2500E-02  2.7765E-01 -5.9913E-02  3.7668E-02  3.9933E-01
             1.7375E-01  1.1971E-01  7.8168E-01  1.1772E-01  6.7123E-02
 GRADIENT:  -8.1860E+00 -1.7024E+00 -1.5549E+00 -5.1407E+01  9.0860E-03  3.4072E-03 -1.4216E-03  1.7101E-03  9.1889E-03  1.2997E-02
             3.1576E-03  1.8092E-02 -7.5687E-03 -7.4377E-02  9.6828E-03
 
0ITERATION NO.:   22    OBJECTIVE VALUE:  -1121.02822418610        NO. OF FUNC. EVALS.:  13
 CUMULATIVE NO. OF FUNC. EVALS.:      228
 NPARAMETR:  5.4007E+00  5.0088E+00  2.2694E+00  1.0934E+01  1.6506E-01 -7.3119E-04  1.2386E-02 -1.2759E-02  1.3142E-01  1.6039E-02
             1.3929E-02  1.8744E-01  3.3170E-02  1.4981E-01  5.7171E-02
 PARAMETER:  1.0332E-01  1.0387E-01  1.1063E-01  1.0217E-01  6.8531E-02 -3.6928E-02  2.8173E-01 -5.9079E-02  3.7501E-02  4.0124E-01
             1.7520E-01  1.2114E-01  7.8790E-01  1.1839E-01  6.6937E-02
 GRADIENT:  -3.7816E+00 -9.2048E-01 -7.6171E-01 -2.3886E+01  4.0239E-03  1.9037E-03 -1.7535E-04  2.7441E-04  3.4257E-03  6.4200E-03
             1.0639E-03  3.8148E-03 -2.9348E-03 -3.3272E-02  4.8241E-03
 
0ITERATION NO.:   23    OBJECTIVE VALUE:  -1121.02822425115        NO. OF FUNC. EVALS.:  18
 CUMULATIVE NO. OF FUNC. EVALS.:      246
 NPARAMETR:  5.4007E+00  5.0088E+00  2.2694E+00  1.0934E+01  1.6506E-01 -7.3116E-04  1.2386E-02 -1.2759E-02  1.3142E-01  1.6039E-02
             1.3929E-02  1.8744E-01  3.3170E-02  1.4981E-01  5.7171E-02
 PARAMETER:  1.0332E-01  1.0387E-01  1.1063E-01  1.0217E-01  6.8531E-02 -3.6927E-02  2.8173E-01 -5.9078E-02  3.7501E-02  4.0124E-01
             1.7520E-01  1.2114E-01  7.8791E-01  1.1839E-01  6.6937E-02
 GRADIENT:  -3.7801E+00 -9.2016E-01 -7.6118E-01 -2.3880E+01  4.0206E-03  1.9028E-03 -1.7617E-04  2.7292E-04  3.4214E-03  6.4176E-03
             1.0636E-03  3.8071E-03 -2.9344E-03 -3.3260E-02  4.8313E-03
 
0ITERATION NO.:   24    OBJECTIVE VALUE:  -1121.02827223361        NO. OF FUNC. EVALS.:  45
 CUMULATIVE NO. OF FUNC. EVALS.:      291             RESET HESSIAN, TYPE I
 NPARAMETR:  5.4012E+00  5.0089E+00  2.2695E+00  1.0933E+01  1.6506E-01 -7.3123E-04  1.2386E-02 -1.2759E-02  1.3142E-01  1.5863E-02
             1.3929E-02  1.8740E-01  3.3155E-02  1.4982E-01  5.7170E-02
 PARAMETER:  1.0332E-01  1.0387E-01  1.1064E-01  1.0217E-01  6.8529E-02 -3.6930E-02  2.8174E-01 -5.9078E-02  3.7500E-02  3.9688E-01
             1.7520E-01  1.2113E-01  7.8800E-01  1.1841E-01  6.6936E-02
 GRADIENT:   1.1433E+01  6.3495E-01  2.0830E+00  6.1867E+01  5.0923E-03  3.4042E-03  1.8643E-04 -8.8071E-03 -8.9737E-03 -1.0703E-02
             2.0160E-02 -4.2407E-03 -4.5575E-03 -2.6586E-02  3.6071E-03
 
0ITERATION NO.:   25    OBJECTIVE VALUE:  -1121.02827224440        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:      300
 NPARAMETR:  5.4012E+00  5.0089E+00  2.2695E+00  1.0933E+01  1.6506E-01 -7.3123E-04  1.2386E-02 -1.2759E-02  1.3142E-01  1.5858E-02
             1.3929E-02  1.8740E-01  3.3155E-02  1.4982E-01  5.7170E-02
 PARAMETER:  1.0332E-01  1.0387E-01  1.1064E-01  1.0217E-01  6.8529E-02 -3.6931E-02  2.8174E-01 -5.9078E-02  3.7500E-02  3.9675E-01
             1.7520E-01  1.2113E-01  7.8800E-01  1.1841E-01  6.6936E-02
 GRADIENT:   1.1473E+01  6.4652E-01  2.1080E+00  6.1778E+01  5.1290E-03  3.4493E-03  1.9879E-04 -9.0707E-03 -9.3486E-03 -1.1213E-02
             2.0730E-02 -4.4728E-03 -4.6057E-03 -2.6374E-02  3.5468E-03
 
0ITERATION NO.:   26    OBJECTIVE VALUE:  -1121.02833882330        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:      309
 NPARAMETR:  5.4022E+00  5.0091E+00  2.2697E+00  1.0932E+01  1.6505E-01 -7.3143E-04  1.2386E-02 -1.2758E-02  1.3142E-01  1.5870E-02
             1.3924E-02  1.8740E-01  3.3165E-02  1.4984E-01  5.7170E-02
 PARAMETER:  1.0333E-01  1.0388E-01  1.1065E-01  1.0216E-01  6.8523E-02 -3.6941E-02  2.8174E-01 -5.9075E-02  3.7505E-02  3.9704E-01
             1.7514E-01  1.2113E-01  7.8825E-01  1.1847E-01  6.6934E-02
 GRADIENT:   1.4867E+01  8.9123E-01  2.5693E+00  5.8565E+01  5.0663E-04  2.9111E-03  7.4967E-04 -1.3369E-02 -7.4885E-03 -1.0910E-02
             1.8454E-02 -8.3841E-03 -4.3875E-03 -1.4591E-02 -4.7621E-03
 
0ITERATION NO.:   27    OBJECTIVE VALUE:  -1121.02834126797        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      317
 NPARAMETR:  5.4027E+00  5.0092E+00  2.2696E+00  1.0931E+01  1.6505E-01 -7.3157E-04  1.2385E-02 -1.2756E-02  1.3142E-01  1.5836E-02
             1.3921E-02  1.8739E-01  3.3171E-02  1.4985E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8523E-02 -3.6948E-02  2.8172E-01 -5.9066E-02  3.7506E-02  3.9620E-01
             1.7510E-01  1.2114E-01  7.8847E-01  1.1850E-01  6.6938E-02
 GRADIENT:   1.6371E+01  1.1032E+00  2.4861E+00  5.7867E+01  1.3678E-03  3.1700E-03 -2.6586E-04 -1.1323E-02 -1.1366E-02 -1.3318E-02
             2.1395E-02 -9.1146E-03 -4.1714E-03 -1.1306E-02 -2.0118E-03
 
0ITERATION NO.:   28    OBJECTIVE VALUE:  -1121.02834143753        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      325
 NPARAMETR:  5.4027E+00  5.0092E+00  2.2696E+00  1.0931E+01  1.6505E-01 -7.3163E-04  1.2386E-02 -1.2756E-02  1.3142E-01  1.5832E-02
             1.3920E-02  1.8739E-01  3.3174E-02  1.4985E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8523E-02 -3.6951E-02  2.8173E-01 -5.9064E-02  3.7509E-02  3.9608E-01
             1.7508E-01  1.2114E-01  7.8856E-01  1.1851E-01  6.6939E-02
 GRADIENT:   1.6656E+01  1.1648E+00  2.5396E+00  5.7526E+01  1.0614E-03  3.1673E-03 -2.4951E-04 -1.1678E-02 -1.1167E-02 -1.3728E-02
             2.1363E-02 -9.9788E-03 -3.9622E-03 -9.6179E-03 -8.7243E-04
 
0ITERATION NO.:   29    OBJECTIVE VALUE:  -1121.02834143837        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      335
 NPARAMETR:  5.4028E+00  5.0092E+00  2.2696E+00  1.0931E+01  1.6505E-01 -7.3163E-04  1.2386E-02 -1.2756E-02  1.3142E-01  1.5831E-02
             1.3920E-02  1.8739E-01  3.3174E-02  1.4985E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8523E-02 -3.6951E-02  2.8173E-01 -5.9064E-02  3.7509E-02  3.9607E-01
             1.7508E-01  1.2114E-01  7.8856E-01  1.1851E-01  6.6939E-02
 GRADIENT:   1.6665E+01  1.1666E+00  2.5401E+00  5.7518E+01  1.0608E-03  3.1682E-03 -2.5253E-04 -1.1672E-02 -1.1178E-02 -1.3743E-02
             2.1365E-02 -9.9970E-03 -3.9584E-03 -9.5605E-03 -8.3833E-04
 
0ITERATION NO.:   30    OBJECTIVE VALUE:  -1121.02834143837        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      351
 NPARAMETR:  5.4028E+00  5.0092E+00  2.2696E+00  1.0931E+01  1.6505E-01 -7.3163E-04  1.2386E-02 -1.2756E-02  1.3142E-01  1.5831E-02
             1.3920E-02  1.8739E-01  3.3174E-02  1.4985E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8523E-02 -3.6951E-02  2.8173E-01 -5.9064E-02  3.7509E-02  3.9607E-01
             1.7508E-01  1.2114E-01  7.8856E-01  1.1851E-01  6.6939E-02
 GRADIENT:   2.7801E+00 -2.4970E-02  5.1638E-01 -3.0998E+01  1.0608E-03  3.1682E-03 -2.5253E-04 -1.1672E-02 -1.1178E-02 -1.3743E-02
             2.1365E-02 -9.9970E-03 -3.9584E-03 -9.5605E-03 -1.8412E-03
 
0ITERATION NO.:   31    OBJECTIVE VALUE:  -1121.02834145390        NO. OF FUNC. EVALS.:  13
 CUMULATIVE NO. OF FUNC. EVALS.:      364
 NPARAMETR:  5.4024E+00  5.0091E+00  2.2696E+00  1.0932E+01  1.6505E-01 -7.3136E-04  1.2383E-02 -1.2757E-02  1.3142E-01  1.5853E-02
             1.3922E-02  1.8739E-01  3.3168E-02  1.4984E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8522E-02 -3.6937E-02  2.8167E-01 -5.9072E-02  3.7501E-02  3.9663E-01
             1.7512E-01  1.2111E-01  7.8837E-01  1.1850E-01  6.6945E-02
 GRADIENT:   1.7697E+00 -1.3353E-01  3.4885E-01 -2.9786E+01  1.3140E-03  3.1262E-03 -4.5177E-04 -1.0876E-02 -1.0297E-02 -1.1764E-02
             1.9832E-02 -9.4430E-03 -3.9565E-03 -1.1121E-02 -3.9625E-04
 
0ITERATION NO.:   32    OBJECTIVE VALUE:  -1121.02834149516        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      379
 NPARAMETR:  5.4024E+00  5.0091E+00  2.2696E+00  1.0932E+01  1.6505E-01 -7.3136E-04  1.2383E-02 -1.2757E-02  1.3142E-01  1.5854E-02
             1.3922E-02  1.8739E-01  3.3168E-02  1.4984E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8522E-02 -3.6937E-02  2.8166E-01 -5.9072E-02  3.7501E-02  3.9664E-01
             1.7512E-01  1.2111E-01  7.8836E-01  1.1850E-01  6.6945E-02
 GRADIENT:   1.7460E+00 -1.3523E-01  3.4566E-01 -2.9760E+01  1.3099E-03  3.1242E-03 -4.5438E-04 -1.0858E-02 -1.0261E-02 -1.1716E-02
             1.9776E-02 -9.4465E-03 -3.9514E-03 -1.1133E-02 -3.6860E-04
 
0ITERATION NO.:   33    OBJECTIVE VALUE:  -1121.02836136380        NO. OF FUNC. EVALS.:  44
 CUMULATIVE NO. OF FUNC. EVALS.:      423             RESET HESSIAN, TYPE I
 NPARAMETR:  5.4028E+00  5.0092E+00  2.2695E+00  1.0931E+01  1.6505E-01 -7.3444E-04  1.2385E-02 -1.2750E-02  1.3143E-01  1.6029E-02
             1.3884E-02  1.8745E-01  3.3209E-02  1.4985E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8524E-02 -3.7093E-02  2.8171E-01 -5.9037E-02  3.7550E-02  4.0099E-01
             1.7462E-01  1.2117E-01  7.8899E-01  1.1854E-01  6.6943E-02
 GRADIENT:   1.7149E+01  8.4062E-01  1.9976E+00  5.8040E+01 -1.0585E-03  9.3222E-04 -1.3654E-03  7.7222E-03  1.4229E-02  7.1618E-03
            -1.4286E-02 -4.9031E-03 -3.4264E-04 -6.6411E-03  8.5115E-03
 
0ITERATION NO.:   34    OBJECTIVE VALUE:  -1121.02836136380        NO. OF FUNC. EVALS.:  21
 CUMULATIVE NO. OF FUNC. EVALS.:      444
 NPARAMETR:  5.4028E+00  5.0092E+00  2.2695E+00  1.0931E+01  1.6505E-01 -7.3444E-04  1.2385E-02 -1.2750E-02  1.3143E-01  1.6029E-02
             1.3884E-02  1.8745E-01  3.3209E-02  1.4985E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8524E-02 -3.7093E-02  2.8171E-01 -5.9037E-02  3.7550E-02  4.0099E-01
             1.7462E-01  1.2117E-01  7.8899E-01  1.1854E-01  6.6943E-02
 GRADIENT:   3.2632E+00 -3.5127E-01 -2.5889E-02 -3.0464E+01 -1.0585E-03  9.3222E-04 -1.3654E-03  7.7222E-03  1.4229E-02  7.1618E-03
            -1.4286E-02 -4.9031E-03 -3.4264E-04 -6.6411E-03  7.5112E-03
 
0ITERATION NO.:   35    OBJECTIVE VALUE:  -1121.02836713140        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      460
 NPARAMETR:  5.4027E+00  5.0092E+00  2.2695E+00  1.0931E+01  1.6505E-01 -7.3345E-04  1.2384E-02 -1.2752E-02  1.3143E-01  1.5973E-02
             1.3896E-02  1.8743E-01  3.3196E-02  1.4985E-01  5.7171E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1064E-01  1.0216E-01  6.8523E-02 -3.7043E-02  2.8170E-01 -5.9048E-02  3.7534E-02  3.9960E-01
             1.7478E-01  1.2115E-01  7.8879E-01  1.1853E-01  6.6944E-02
 GRADIENT:   2.8066E+00 -2.8970E-01  8.0804E-02 -3.0134E+01 -3.7988E-04  1.6197E-03 -1.1038E-03  1.7379E-03  6.4868E-03  1.1607E-03
            -3.4935E-03 -6.4740E-03 -1.4830E-03 -8.1970E-03  4.7250E-03
 
0ITERATION NO.:   36    OBJECTIVE VALUE:  -1121.02836974988        NO. OF FUNC. EVALS.:  43
 CUMULATIVE NO. OF FUNC. EVALS.:      503             RESET HESSIAN, TYPE I
 NPARAMETR:  5.4028E+00  5.0093E+00  2.2696E+00  1.0931E+01  1.6506E-01 -7.3834E-04  1.2388E-02 -1.2754E-02  1.3142E-01  1.5957E-02
             1.3906E-02  1.8745E-01  3.3208E-02  1.4987E-01  5.7170E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1065E-01  1.0216E-01  6.8527E-02 -3.7290E-02  2.8178E-01 -5.9058E-02  3.7493E-02  3.9923E-01
             1.7490E-01  1.2121E-01  7.8905E-01  1.1856E-01  6.6936E-02
 GRADIENT:   1.6915E+01  1.2792E+00  2.4522E+00  5.6648E+01  3.3012E-04  1.1776E-03  1.1374E-05 -7.6110E-03 -2.2900E-03 -1.3883E-03
             3.0110E-03 -3.5753E-03 -1.8230E-03 -1.8168E-03  5.6418E-03
 
0ITERATION NO.:   37    OBJECTIVE VALUE:  -1121.02836974988        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:      514
 NPARAMETR:  5.4028E+00  5.0093E+00  2.2696E+00  1.0931E+01  1.6506E-01 -7.3834E-04  1.2388E-02 -1.2754E-02  1.3142E-01  1.5957E-02
             1.3906E-02  1.8745E-01  3.3208E-02  1.4987E-01  5.7170E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1065E-01  1.0216E-01  6.8527E-02 -3.7290E-02  2.8178E-01 -5.9058E-02  3.7493E-02  3.9923E-01
             1.7490E-01  1.2121E-01  7.8905E-01  1.1856E-01  6.6936E-02
 GRADIENT:   3.0288E+00  8.7473E-02  4.2857E-01 -3.1860E+01  3.3012E-04  1.1776E-03  1.1374E-05 -7.6110E-03 -2.2900E-03 -1.3883E-03
             3.0110E-03 -3.5753E-03 -1.8230E-03 -1.8168E-03  4.6409E-03
 
0ITERATION NO.:   38    OBJECTIVE VALUE:  -1121.02836974988        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      524
 NPARAMETR:  5.4028E+00  5.0093E+00  2.2696E+00  1.0931E+01  1.6506E-01 -7.3834E-04  1.2388E-02 -1.2754E-02  1.3142E-01  1.5957E-02
             1.3906E-02  1.8745E-01  3.3208E-02  1.4987E-01  5.7170E-02
 PARAMETER:  1.0334E-01  1.0388E-01  1.1065E-01  1.0216E-01  6.8527E-02 -3.7290E-02  2.8178E-01 -5.9058E-02  3.7493E-02  3.9923E-01
             1.7490E-01  1.2121E-01  7.8905E-01  1.1856E-01  6.6936E-02
 GRADIENT:   3.0288E+00  8.7473E-02  4.2857E-01 -3.1860E+01  3.3012E-04  1.1776E-03  1.1374E-05 -7.6110E-03 -2.2900E-03 -1.3883E-03
             3.0110E-03 -3.5753E-03 -1.8230E-03 -1.8168E-03  4.6409E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      524
 NO. OF SIG. DIGITS IN FINAL EST.:  3.1

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -3.2411E-07 -1.1914E-02 -3.5037E-03 -1.1570E-02
 SE:             3.9192E-02  2.8848E-02  3.2725E-02  3.3058E-02
 N:                     100         100         100         100
 
 P VAL.:         9.9999E-01  6.7961E-01  9.1474E-01  7.2634E-01
 
 ETAshrink(%):   3.0463E+00  2.0022E+01  2.4035E+01  1.4177E+01
 EBVshrink(%):   3.3162E+00  1.9863E+01  2.4454E+01  1.5064E+01
 EPSshrink(%):   3.0938E+01
 
 #TERE:
 Elapsed estimation time in seconds:    16.07
 Elapsed covariance time in seconds:     5.88
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1121.028       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         5.40E+00  5.01E+00  2.27E+00  1.09E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.65E-01
 
 ETA2
+       -7.38E-04  1.31E-01
 
 ETA3
+        1.24E-02  1.60E-02  1.87E-01
 
 ETA4
+       -1.28E-02  1.39E-02  3.32E-02  1.50E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.72E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.06E-01
 
 ETA2
+       -5.01E-03  3.63E-01
 
 ETA3
+        7.04E-02  1.02E-01  4.33E-01
 
 ETA4
+       -8.11E-02  9.91E-02  1.98E-01  3.87E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.39E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.34E-01  2.39E-01  1.60E-01  5.66E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.72E-02
 
 ETA2
+        2.15E-02  3.16E-02
 
 ETA3
+        3.07E-02  3.54E-02  7.90E-02
 
 ETA4
+        2.40E-02  2.36E-02  4.67E-02  4.02E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        8.40E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.34E-02
 
 ETA2
+        1.46E-01  4.36E-02
 
 ETA3
+        1.66E-01  2.27E-01  9.12E-02
 
 ETA4
+        1.58E-01  1.63E-01  2.26E-01  5.19E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.76E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        5.46E-02
 
 TH 2
+        6.66E-03  5.74E-02
 
 TH 3
+        7.10E-03  2.67E-03  2.56E-02
 
 TH 4
+        1.44E-02  1.79E-02  4.89E-02  3.20E-01
 
 OM11
+        4.01E-04  6.96E-04  4.63E-04  1.84E-03  7.38E-04
 
 OM12
+        4.31E-04  6.56E-04  5.99E-04  1.90E-03  1.42E-04  4.61E-04
 
 OM13
+        6.10E-04  9.46E-04  1.02E-04  2.40E-03  2.82E-04  1.66E-04  9.41E-04
 
 OM14
+        5.12E-04  6.68E-04  5.47E-04  2.39E-03  1.54E-04  1.51E-04  4.39E-04  5.78E-04
 
 OM22
+       -1.43E-05  1.94E-04 -5.51E-04 -7.40E-04  2.53E-05  1.02E-04  1.16E-04  7.52E-05  9.99E-04
 
 OM23
+        3.01E-04  4.76E-04  1.89E-03  2.71E-03  6.16E-05  1.70E-04 -5.54E-05  2.57E-05 -4.58E-05  1.25E-03
 
 OM24
+        1.69E-04 -6.99E-05  7.18E-04  2.04E-03  4.11E-05  6.11E-05  6.03E-05  1.06E-04  1.45E-04  3.37E-04  5.55E-04
 
 OM33
+        1.42E-03  1.28E-04  2.13E-03  7.86E-03  3.84E-04  3.80E-04  1.13E-03  7.13E-04  4.19E-04  1.88E-05  2.79E-04  6.24E-03
 
 OM34
+        8.69E-04  3.61E-04  8.33E-04  3.55E-03  2.18E-04  2.26E-04  6.09E-04  4.74E-04  2.82E-04 -1.83E-05  2.39E-04  3.04E-03
          2.18E-03
 
 OM44
+        5.86E-04  7.07E-04  3.18E-04  2.55E-03  1.53E-04  1.60E-04  3.79E-04  3.12E-04  2.36E-04 -5.19E-06  2.64E-04  1.63E-03
          1.37E-03  1.62E-03
 
 SG11
+        2.49E-05  4.29E-05 -2.89E-05 -2.30E-04 -3.97E-05 -4.53E-05 -8.94E-05 -6.75E-05 -8.12E-05  1.29E-05 -3.05E-05 -3.92E-04
         -2.33E-04 -1.66E-04  7.05E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        4.32E-02
 
 TH 2
+        1.19E-01  4.78E-02
 
 TH 3
+        1.90E-01  6.98E-02  7.05E-02
 
 TH 4
+        1.09E-01  1.32E-01  5.40E-01  5.18E-02
 
 OM11
+        6.33E-02  1.07E-01  1.06E-01  1.19E-01  2.72E-02
 
 OM12
+        8.59E-02  1.28E-01  1.74E-01  1.56E-01  2.44E-01  2.15E-02
 
 OM13
+        8.52E-02  1.29E-01  2.07E-02  1.38E-01  3.39E-01  2.51E-01  3.07E-02
 
 OM14
+        9.11E-02  1.16E-01  1.42E-01  1.76E-01  2.36E-01  2.92E-01  5.95E-01  2.40E-02
 
 OM22
+       -1.94E-03  2.57E-02 -1.09E-01 -4.14E-02  2.95E-02  1.50E-01  1.20E-01  9.90E-02  3.16E-02
 
 OM23
+        3.65E-02  5.62E-02  3.33E-01  1.36E-01  6.41E-02  2.23E-01 -5.11E-02  3.02E-02 -4.09E-02  3.54E-02
 
 OM24
+        3.06E-02 -1.24E-02  1.91E-01  1.53E-01  6.42E-02  1.21E-01  8.34E-02  1.87E-01  1.94E-01  4.04E-01  2.36E-02
 
 OM33
+        7.71E-02  6.78E-03  1.69E-01  1.76E-01  1.79E-01  2.24E-01  4.66E-01  3.75E-01  1.68E-01  6.74E-03  1.50E-01  7.90E-02
 
 OM34
+        7.97E-02  3.22E-02  1.12E-01  1.34E-01  1.72E-01  2.25E-01  4.25E-01  4.22E-01  1.91E-01 -1.11E-02  2.17E-01  8.23E-01
          4.67E-02
 
 OM44
+        6.24E-02  7.34E-02  4.95E-02  1.12E-01  1.40E-01  1.85E-01  3.07E-01  3.22E-01  1.85E-01 -3.65E-03  2.78E-01  5.14E-01
          7.29E-01  4.02E-02
 
 SG11
+        1.27E-02  2.13E-02 -2.15E-02 -4.83E-02 -1.74E-01 -2.51E-01 -3.47E-01 -3.34E-01 -3.06E-01  4.33E-02 -1.54E-01 -5.91E-01
         -5.94E-01 -4.91E-01  8.40E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        1.95E+01
 
 TH 2
+       -1.72E+00  1.88E+01
 
 TH 3
+       -5.55E+00  5.52E-01  6.59E+01
 
 TH 4
+        2.04E-01 -8.84E-01 -8.67E+00  4.63E+00
 
 OM11
+       -2.14E+00 -8.03E+00 -1.44E+01 -1.49E+00  1.60E+03
 
 OM12
+       -8.19E+00 -1.50E+01 -1.83E+01 -4.73E+00 -2.89E+02  2.75E+03
 
 OM13
+       -6.79E+00 -1.37E+01  5.25E+01 -6.20E+00 -4.17E+02 -9.48E+01  2.04E+03
 
 OM14
+       -3.10E+00 -8.83E+00 -3.89E+01 -2.49E+00  1.08E+01 -3.67E+02 -1.18E+03  3.03E+03
 
 OM22
+       -2.49E+00 -6.47E+00  3.03E+01  1.25E+00  4.94E+01 -2.05E+02 -1.88E+01  6.67E+01  1.18E+03
 
 OM23
+        4.97E+00 -8.20E+00 -6.79E+01  6.15E+00 -3.20E+01 -3.79E+02  7.86E+01  7.04E+01  1.02E+02  1.14E+03
 
 OM24
+        1.13E+00  1.76E+01 -1.25E+01 -7.60E+00  3.10E-01  1.93E+02  5.53E+01 -2.95E+02 -3.45E+02 -6.89E+02  2.54E+03
 
 OM33
+       -9.74E-01  5.13E+00 -2.17E+01 -3.04E+00  2.17E+01  1.61E+00 -2.57E+02  1.36E+02  1.07E+00 -2.32E+01  5.57E+01  6.09E+02
 
 OM34
+       -4.60E+00  1.07E+00  3.64E+00  4.50E+00  4.43E+00 -8.25E+00  1.11E+02 -3.96E+02  3.13E+00  5.43E+01 -6.15E+01 -8.41E+02
          2.44E+03
 
 OM44
+       -3.09E+00 -1.21E+01  1.48E+01 -4.14E+00 -1.40E+01 -3.15E+01 -3.14E+01  4.78E+01  4.12E+00  6.30E+01 -3.39E+02  2.25E+02
         -1.11E+03  1.49E+03
 
 SG11
+       -5.67E+01 -4.83E+01 -2.20E+01 -1.68E+01  3.57E+02  9.46E+02  2.61E+01  6.52E+02  1.18E+03 -2.89E+02  8.39E+00  9.60E+02
          5.20E+02  9.22E+02  2.63E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.17E-01  3.27E-01  3.85E-01  4.27E-01  4.49E-01  5.66E-01  7.48E-01  8.03E-01  8.59E-01  8.77E-01  1.00E+00  1.16E+00
          1.35E+00  1.92E+00  4.00E+00
 
 #CPUT: Total CPU Time in Seconds,       36.676
Stop Time: 
Mon 09/30/2013 
03:06 PM

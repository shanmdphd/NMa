Mon 09/09/2013 
08:21 AM
$PROB RUN# Example 1 (from samp5l)
$INPUT ID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT
$DATA tdist6.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4

$PK
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
MU_4=THETA(4)
NU=4.0
CL=EXP(MU_1+ETA(1))
V1=EXP(MU_2+ETA(2))
Q=EXP(MU_3+ETA(3))
V2=EXP(MU_4+ETA(4))
S1=V1

$ERROR
Y = F + F*EPS(1)

;$THETA 1.68338E+00  1.58811E+00  8.12694E-01  2.37435E+00  
$THETA 2 2 2 2
$OMEGA BLOCK(4)
0.3
0.001 0.3
0.001 0.001 0.3
0.001 0.001 0.001 0.3

$SIGMA 
0.3

$EST METHOD=ITS LAPLACE INTERACTION MAXEVAL=9999 PRINT=5 NOHABORT SIGL=8 CTYPE=3 NITER=200
$EST METHOD=IMP INTERACTION MAXEVAL=9999 PRINT=1 NOABORT ISAMPLE=3000 NITER=200 SIGL=8 DF=1
$EST METHOD=1 LAPLACE INTERACTION MAXEVAL=9999 PRINT=1 NOHABORT
$COV MATRIX=R UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  87) WITH "LAPLACIAN" AND "INTERACTION", "NUMERICAL" AND "SLOW"
 ARE ALSO REQUIRED ON $ESTIM RECORD, AND "SLOW" IS REQUIRED ON $COV
 RECORD. NM-TRAN HAS SUPPLIED THESE OPTIONS.
             
 (WARNING  87) WITH "LAPLACIAN" AND "INTERACTION", "NUMERICAL" AND "SLOW"
 ARE ALSO REQUIRED ON $ESTIM RECORD, AND "SLOW" IS REQUIRED ON $COV
 RECORD. NM-TRAN HAS SUPPLIED THESE OPTIONS.
             
 (WARNING  87) WITH "LAPLACIAN" AND "INTERACTION", "NUMERICAL" AND "SLOW"
 ARE ALSO REQUIRED ON $ESTIM RECORD, AND "SLOW" IS REQUIRED ON $COV
 RECORD. NM-TRAN HAS SUPPLIED THESE OPTIONS.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        9 SEP 2013
Days until program expires :6108
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
 NO. OF DATA RECS IN DATA SET:      600
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   2   4   5   0   0   8   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME CONC DOSE RATE EVID MDV CMT
0FORMAT FOR DATA:
 (7E10.0/E10.0)

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
   0.2000E+01  0.2000E+01  0.2000E+01  0.2000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.3000E+00
                  0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.3000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.3000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 SLOW GRADIENT METHOD USED:     YES
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(N)

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
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    YES 
 NUMERICAL 2ND DERIVATIVES:               YES 
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
 EM OR BAYESIAN METHOD USED:              ITERATIVE TWO STAGE (ITS)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        5           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      200         
 
 
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

 iteration            0 OBJ=  -190.967855728702
 iteration            5 OBJ=  -1634.04244100939
 iteration           10 OBJ=  -1639.48780471870
 iteration           15 OBJ=  -1639.73026437977
 iteration           20 OBJ=  -1639.77011575544
 iteration           25 OBJ=  -1639.77920415928
 iteration           30 OBJ=  -1639.78160017659
 iteration           35 OBJ=  -1639.78207351964
 iteration           40 OBJ=  -1639.78220922681
 iteration           45 OBJ=  -1639.78229171600
 iteration           50 OBJ=  -1639.78228825600
 iteration           55 OBJ=  -1639.78227185133
 Convergence achieved
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         6.8035E-08 -3.1435E-08  1.3001E-07  1.1983E-07
 SE:             2.3700E-02  2.9494E-02  1.9703E-02  2.7107E-02
 N:                     100         100         100         100
 
 P VAL.:         1.0000E+00  1.0000E+00  9.9999E-01  1.0000E+00
 
 ETAshrink(%):   2.1815E+00  6.1585E+00  1.6219E+01  6.1486E+00
 EBVshrink(%):   2.1815E+00  6.1585E+00  1.6219E+01  6.1487E+00
 EPSshrink(%):   4.0829E+01
 
 #TERE:
 Elapsed estimation time in seconds:    16.80
 Elapsed covariance time in seconds:     0.34
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1639.782       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.69E+00  1.58E+00  8.03E-01  2.38E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.93E-02
 
 ETA2
+        2.63E-02  9.98E-02
 
 ETA3
+        4.33E-03  3.51E-02  5.59E-02
 
 ETA4
+        2.20E-02 -9.28E-03  3.49E-02  8.43E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.46E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.44E-01
 
 ETA2
+        3.41E-01  3.16E-01
 
 ETA3
+        7.52E-02  4.70E-01  2.36E-01
 
 ETA4
+        3.12E-01 -1.01E-01  5.09E-01  2.90E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.73E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.79E-02  4.61E-02  3.53E-02  3.63E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        7.19E-03
 
 ETA2
+        1.30E-02  1.68E-02
 
 ETA3
+        9.91E-03  9.58E-03  1.50E-02
 
 ETA4
+        1.16E-02  1.26E-02  1.47E-02  1.76E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.42E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.48E-02
 
 ETA2
+        1.44E-01  2.66E-02
 
 ETA3
+        1.69E-01  1.20E-01  3.18E-02
 
 ETA4
+        1.33E-01  1.35E-01  1.41E-01  3.03E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        7.32E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        7.77E-04
 
 TH 2
+        4.11E-04  2.12E-03
 
 TH 3
+        1.13E-04  8.11E-04  1.25E-03
 
 TH 4
+        2.51E-04 -7.80E-05  7.28E-04  1.32E-03
 
 OM11
+        4.18E-06  1.08E-04  1.18E-05 -3.46E-05  5.16E-05
 
 OM12
+        7.53E-05  2.77E-04  1.06E-04  2.09E-06  5.52E-05  1.70E-04
 
 OM13
+       -2.65E-05  1.24E-04 -2.63E-07 -3.76E-05  4.07E-05  5.79E-05  9.83E-05
 
 OM14
+       -9.72E-05  7.78E-05  1.96E-05 -2.18E-05  4.02E-05  1.88E-05  7.98E-05  1.34E-04
 
 OM22
+        6.67E-05  2.87E-05 -1.18E-04 -1.38E-04  3.51E-05  1.23E-04  3.11E-05  8.60E-06  2.82E-04
 
 OM23
+       -1.13E-05 -1.84E-04 -9.40E-05 -2.02E-05 -7.02E-06 -2.77E-05  8.36E-06 -2.12E-06 -8.82E-06  9.18E-05
 
 OM24
+        1.53E-05  3.54E-05  7.74E-05  1.56E-04  1.18E-05  2.43E-05  1.92E-05  6.62E-06 -9.61E-05  2.74E-05  1.59E-04
 
 OM33
+       -7.11E-05 -2.17E-04 -3.50E-05  4.55E-05 -1.03E-05 -5.54E-05  1.24E-05  2.41E-05 -7.43E-05  5.89E-05  3.37E-05  2.26E-04
 
 OM34
+       -3.83E-05  2.43E-05  6.74E-05  7.39E-05  1.20E-05 -6.29E-06  4.58E-05  5.30E-05 -7.40E-05 -2.02E-05  6.74E-05  1.40E-04
          2.15E-04
 
 OM44
+       -1.05E-04  9.52E-05  7.29E-05 -1.25E-05  2.66E-05 -7.39E-06  7.15E-05  1.47E-04 -2.66E-05 -4.13E-05  3.92E-06  1.04E-04
          1.97E-04  3.09E-04
 
 SG11
+       -1.24E-06  1.37E-05  8.24E-06  1.37E-07  4.78E-07  8.30E-07 -1.38E-06 -4.08E-07 -2.91E-06 -2.18E-06 -1.35E-07 -9.11E-06
         -5.95E-06 -4.19E-06  2.03E-06
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.79E-02
 
 TH 2
+        3.20E-01  4.61E-02
 
 TH 3
+        1.15E-01  4.98E-01  3.53E-02
 
 TH 4
+        2.48E-01 -4.66E-02  5.68E-01  3.63E-02
 
 OM11
+        2.09E-02  3.26E-01  4.65E-02 -1.33E-01  7.19E-03
 
 OM12
+        2.07E-01  4.61E-01  2.31E-01  4.42E-03  5.90E-01  1.30E-02
 
 OM13
+       -9.57E-02  2.72E-01 -7.49E-04 -1.04E-01  5.71E-01  4.49E-01  9.91E-03
 
 OM14
+       -3.01E-01  1.46E-01  4.80E-02 -5.20E-02  4.84E-01  1.25E-01  6.95E-01  1.16E-02
 
 OM22
+        1.43E-01  3.71E-02 -1.99E-01 -2.26E-01  2.91E-01  5.63E-01  1.87E-01  4.43E-02  1.68E-02
 
 OM23
+       -4.21E-02 -4.16E-01 -2.78E-01 -5.80E-02 -1.02E-01 -2.22E-01  8.80E-02 -1.91E-02 -5.48E-02  9.58E-03
 
 OM24
+        4.34E-02  6.08E-02  1.74E-01  3.40E-01  1.31E-01  1.48E-01  1.53E-01  4.53E-02 -4.54E-01  2.27E-01  1.26E-02
 
 OM33
+       -1.70E-01 -3.13E-01 -6.59E-02  8.34E-02 -9.57E-02 -2.83E-01  8.35E-02  1.39E-01 -2.95E-01  4.09E-01  1.78E-01  1.50E-02
 
 OM34
+       -9.37E-02  3.60E-02  1.30E-01  1.39E-01  1.14E-01 -3.29E-02  3.15E-01  3.13E-01 -3.01E-01 -1.44E-01  3.65E-01  6.38E-01
          1.47E-02
 
 OM44
+       -2.15E-01  1.18E-01  1.17E-01 -1.97E-02  2.11E-01 -3.23E-02  4.10E-01  7.24E-01 -9.03E-02 -2.45E-01  1.77E-02  3.96E-01
          7.65E-01  1.76E-02
 
 SG11
+       -3.14E-02  2.09E-01  1.64E-01  2.64E-03  4.67E-02  4.48E-02 -9.81E-02 -2.47E-02 -1.22E-01 -1.60E-01 -7.51E-03 -4.26E-01
         -2.85E-01 -1.67E-01  1.42E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.49E+03
 
 TH 2
+       -1.05E+03  1.63E+03
 
 TH 3
+        9.93E+02 -1.30E+03  2.85E+03
 
 TH 4
+       -1.29E+03  1.10E+03 -1.90E+03  2.45E+03
 
 OM11
+       -1.83E+03 -9.50E+01  1.73E+02  2.24E+03  5.37E+04
 
 OM12
+       -1.32E+02 -1.61E+02 -3.34E+03  9.77E+02 -1.96E+04  4.01E+04
 
 OM13
+       -9.07E+01 -2.73E+03  2.98E+03  1.50E+02  2.04E+04 -3.50E+04  9.31E+04
 
 OM14
+        4.70E+03  7.42E+02  2.15E+02 -4.40E+03 -5.16E+04  3.79E+04 -1.07E+05  1.80E+05
 
 OM22
+       -1.37E+02  2.88E+02  2.07E+03 -9.20E+02  2.58E+03 -1.94E+04  1.13E+04 -9.93E+03  1.67E+04
 
 OM23
+       -2.58E+03  3.12E+03 -3.22E+03  3.04E+03 -6.45E+03  2.52E+04 -4.37E+04  3.78E+04 -1.32E+04  5.70E+04
 
 OM24
+        6.03E+02 -1.47E+03  2.88E+03 -2.08E+03  1.22E+04 -2.86E+04  4.31E+04 -5.64E+04  1.68E+04 -3.61E+04  4.55E+04
 
 OM33
+        8.18E+02 -6.75E+02  1.12E+03 -7.06E+02  8.67E+03 -1.59E+04  3.43E+04 -4.13E+04  8.98E+03 -3.37E+04  2.68E+04  3.32E+04
 
 OM34
+        9.10E+02  2.13E+03 -1.18E+03 -1.58E+03 -3.67E+04  3.57E+04 -9.85E+04  1.48E+05 -1.12E+04  5.80E+04 -6.63E+04 -5.62E+04
          1.63E+05
 
 OM44
+       -2.34E+03 -9.34E+02 -8.03E+02  3.24E+03  3.39E+04 -2.22E+04  7.34E+04 -1.31E+05  4.73E+03 -2.69E+04  4.48E+04  3.21E+04
         -1.22E+05  1.09E+05
 
 SG11
+        4.60E+03 -3.10E+03  9.75E+02 -1.64E+03 -3.57E+03 -2.87E+04  4.67E+04 -4.12E+04  3.09E+04 -3.73E+04  3.23E+04  4.92E+04
         -3.67E+04  2.44E+04  7.19E+05
 
1
 
 
 #TBLN:      2
 #METH: Importance Sampling
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    YES 
 NUMERICAL 2ND DERIVATIVES:               YES 
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
 ITERATIONS (NITER):                      200         
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        3000        
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   0.100000000000000       ,10.0000000000000        
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 T-DIST. PROPOSAL DENSITY (DF):           1           
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

 iteration            0 OBJ=  -1645.10777908267 eff.=    1249. Smpl.=    3000. Fit.= 0.97357
 iteration            1 OBJ=  -1645.09253093132 eff.=    1171. Smpl.=    3000. Fit.= 0.97441
 iteration            2 OBJ=  -1644.78665375079 eff.=    1173. Smpl.=    3000. Fit.= 0.97433
 iteration            3 OBJ=  -1645.63842091762 eff.=    1187. Smpl.=    3000. Fit.= 0.97388
 iteration            4 OBJ=  -1644.35463851795 eff.=    1198. Smpl.=    3000. Fit.= 0.97373
 iteration            5 OBJ=  -1645.68387287301 eff.=    1247. Smpl.=    3000. Fit.= 0.97284
 iteration            6 OBJ=  -1645.92863727522 eff.=    1187. Smpl.=    3000. Fit.= 0.97407
 iteration            7 OBJ=  -1645.63869540434 eff.=    1193. Smpl.=    3000. Fit.= 0.97392
 iteration            8 OBJ=  -1644.93510168633 eff.=    1198. Smpl.=    3000. Fit.= 0.97375
 iteration            9 OBJ=  -1645.58159618673 eff.=    1197. Smpl.=    3000. Fit.= 0.97369
 iteration           10 OBJ=  -1645.21001643063 eff.=    1197. Smpl.=    3000. Fit.= 0.97377
 iteration           11 OBJ=  -1645.13593825524 eff.=    1237. Smpl.=    3000. Fit.= 0.97310
 iteration           12 OBJ=  -1645.48623025281 eff.=    1168. Smpl.=    3000. Fit.= 0.97443
 iteration           13 OBJ=  -1645.32941906887 eff.=    1235. Smpl.=    3000. Fit.= 0.97302
 iteration           14 OBJ=  -1645.28453447073 eff.=    1156. Smpl.=    3000. Fit.= 0.97459
 iteration           15 OBJ=  -1644.76862717227 eff.=    1252. Smpl.=    3000. Fit.= 0.97264
 iteration           16 OBJ=  -1645.67795311912 eff.=    1166. Smpl.=    3000. Fit.= 0.97432
 iteration           17 OBJ=  -1645.22578555659 eff.=    1221. Smpl.=    3000. Fit.= 0.97334
 iteration           18 OBJ=  -1645.21384363281 eff.=    1217. Smpl.=    3000. Fit.= 0.97332
 iteration           19 OBJ=  -1645.41570529607 eff.=    1168. Smpl.=    3000. Fit.= 0.97423
 iteration           20 OBJ=  -1644.54478908356 eff.=    1201. Smpl.=    3000. Fit.= 0.97361
 iteration           21 OBJ=  -1645.56343465304 eff.=    1260. Smpl.=    3000. Fit.= 0.97267
 Convergence achieved
 iteration           21 OBJ=  -1645.14137474288 eff.=    1162. Smpl.=    3000. Fit.= 0.97455
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -9.5542E-06 -3.8230E-04 -7.2086E-05  1.6791E-04
 SE:             2.3597E-02  2.9609E-02  1.9291E-02  2.6780E-02
 N:                     100         100         100         100
 
 P VAL.:         9.9968E-01  9.8970E-01  9.9702E-01  9.9500E-01
 
 ETAshrink(%):   2.3909E+00  6.5637E+00  1.7124E+01  6.6714E+00
 EBVshrink(%):   2.3008E+00  6.3932E+00  1.7179E+01  6.5577E+00
 EPSshrink(%):   4.0880E+01
 
 #TERE:
 Elapsed estimation time in seconds:   195.40
 Elapsed covariance time in seconds:    10.09
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1645.141       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.68E+00  1.57E+00  7.87E-01  2.36E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.90E-02
 
 ETA2
+        2.68E-02  1.01E-01
 
 ETA3
+        3.60E-03  3.45E-02  5.47E-02
 
 ETA4
+        2.13E-02 -1.08E-02  3.38E-02  8.32E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.74E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.43E-01
 
 ETA2
+        3.46E-01  3.18E-01
 
 ETA3
+        6.34E-02  4.63E-01  2.34E-01
 
 ETA4
+        3.04E-01 -1.17E-01  5.01E-01  2.88E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.87E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.50E-02  3.43E-02  2.98E-02  3.17E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        9.05E-03
 
 ETA2
+        9.21E-03  1.81E-02
 
 ETA3
+        7.45E-03  1.13E-02  1.34E-02
 
 ETA4
+        8.52E-03  1.12E-02  1.11E-02  1.46E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.32E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.86E-02
 
 ETA2
+        9.74E-02  2.83E-02
 
 ETA3
+        1.29E-01  1.16E-01  2.87E-02
 
 ETA4
+        1.03E-01  1.20E-01  9.83E-02  2.54E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.69E-03
 
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
+        6.23E-04
 
 TH 2
+        3.07E-04  1.18E-03
 
 TH 3
+        9.86E-05  3.68E-04  8.88E-04
 
 TH 4
+        2.69E-04 -7.66E-05  5.59E-04  1.00E-03
 
 OM11
+        1.18E-06  1.17E-06  3.15E-06  2.31E-06  8.18E-05
 
 OM12
+        6.25E-07 -3.78E-06  2.65E-06  1.06E-06  4.01E-05  8.49E-05
 
 OM13
+        1.66E-06  1.46E-06 -2.51E-06  1.32E-06  1.39E-05  2.81E-05  5.55E-05
 
 OM14
+        1.72E-06  4.15E-06  3.42E-06  4.62E-06  3.61E-05  4.80E-06  3.80E-05  7.25E-05
 
 OM22
+       -7.22E-06 -1.41E-05 -4.52E-05 -3.44E-05  2.17E-05  7.99E-05  2.70E-05 -4.67E-06  3.26E-04
 
 OM23
+        2.00E-07 -1.41E-05  3.32E-05  1.47E-05  6.96E-06  2.60E-05  3.06E-05  1.66E-05  8.83E-05  1.29E-04
 
 OM24
+        3.43E-06 -4.70E-06  3.11E-05  2.13E-05  1.69E-05  2.81E-05  2.80E-05  3.20E-05 -3.11E-05  6.55E-05  1.25E-04
 
 OM33
+        2.29E-07 -1.67E-05  3.40E-05  1.29E-05  4.06E-06  1.01E-05  2.13E-05  1.48E-05  1.58E-05  8.36E-05  5.44E-05  1.81E-04
 
 OM34
+        1.25E-06  2.88E-06 -2.49E-06 -2.28E-06  7.04E-06  1.02E-05  3.14E-05  2.76E-05 -1.58E-05  1.86E-05  4.13E-05  1.07E-04
          1.23E-04
 
 OM44
+        2.93E-06  1.71E-05 -9.00E-06 -4.04E-06  1.68E-05 -3.65E-06  3.22E-05  5.59E-05 -9.23E-06 -7.47E-06 -9.41E-06  6.69E-05
          1.16E-04  2.14E-04
 
 SG11
+       -2.17E-08  9.08E-07  3.86E-10  3.59E-07 -7.00E-07 -5.97E-07 -1.54E-06 -1.28E-06 -2.23E-06 -8.24E-07 -9.46E-07 -4.77E-06
         -3.33E-06 -2.86E-06  1.75E-06
 
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
+        2.50E-02
 
 TH 2
+        3.58E-01  3.43E-02
 
 TH 3
+        1.33E-01  3.60E-01  2.98E-02
 
 TH 4
+        3.40E-01 -7.05E-02  5.92E-01  3.17E-02
 
 OM11
+        5.23E-03  3.78E-03  1.17E-02  8.06E-03  9.05E-03
 
 OM12
+        2.72E-03 -1.20E-02  9.65E-03  3.63E-03  4.82E-01  9.21E-03
 
 OM13
+        8.91E-03  5.72E-03 -1.13E-02  5.58E-03  2.06E-01  4.09E-01  7.45E-03
 
 OM14
+        8.07E-03  1.42E-02  1.35E-02  1.71E-02  4.69E-01  6.12E-02  5.99E-01  8.52E-03
 
 OM22
+       -1.60E-02 -2.27E-02 -8.39E-02 -6.00E-02  1.33E-01  4.80E-01  2.00E-01 -3.04E-02  1.81E-02
 
 OM23
+        7.06E-04 -3.64E-02  9.82E-02  4.09E-02  6.79E-02  2.49E-01  3.62E-01  1.72E-01  4.31E-01  1.13E-02
 
 OM24
+        1.23E-02 -1.23E-02  9.32E-02  6.01E-02  1.67E-01  2.73E-01  3.37E-01  3.36E-01 -1.54E-01  5.17E-01  1.12E-02
 
 OM33
+        6.82E-04 -3.63E-02  8.48E-02  3.02E-02  3.34E-02  8.18E-02  2.12E-01  1.29E-01  6.50E-02  5.49E-01  3.62E-01  1.34E-02
 
 OM34
+        4.53E-03  7.59E-03 -7.55E-03 -6.49E-03  7.02E-02  1.00E-01  3.80E-01  2.92E-01 -7.89E-02  1.48E-01  3.33E-01  7.18E-01
          1.11E-02
 
 OM44
+        8.01E-03  3.41E-02 -2.06E-02 -8.70E-03  1.27E-01 -2.70E-02  2.95E-01  4.49E-01 -3.49E-02 -4.50E-02 -5.75E-02  3.40E-01
          7.16E-01  1.46E-02
 
 SG11
+       -6.57E-04  2.00E-02  9.81E-06  8.57E-03 -5.86E-02 -4.90E-02 -1.56E-01 -1.14E-01 -9.37E-02 -5.50E-02 -6.40E-02 -2.69E-01
         -2.28E-01 -1.48E-01  1.32E-03
 
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
+        2.54E+03
 
 TH 2
+       -1.07E+03  1.62E+03
 
 TH 3
+        1.02E+03 -1.30E+03  2.84E+03
 
 TH 4
+       -1.32E+03  1.12E+03 -1.92E+03  2.49E+03
 
 OM11
+        8.87E+01 -1.84E+02  3.32E+02 -1.84E+02  3.02E+04
 
 OM12
+       -2.97E+02  4.76E+02 -9.94E+02  4.85E+02 -2.47E+04  4.72E+04
 
 OM13
+        2.11E+02 -3.99E+02  8.04E+02 -5.45E+02  2.28E+04 -3.82E+04  7.63E+04
 
 OM14
+       -1.23E+02  2.74E+02 -4.79E+02  3.40E+02 -3.00E+04  3.69E+04 -5.47E+04  7.10E+04
 
 OM22
+        1.66E+02 -2.52E+02  6.60E+02 -2.32E+02  4.70E+03 -1.43E+04  1.11E+04 -1.01E+04  1.07E+04
 
 OM23
+       -2.37E+02  3.69E+02 -8.84E+02  4.73E+02 -8.48E+03  2.43E+04 -3.79E+04  2.68E+04 -1.77E+04  5.74E+04
 
 OM24
+        1.11E+02 -1.79E+02  3.29E+02 -3.33E+02  1.17E+04 -2.77E+04  3.17E+04 -3.53E+04  1.56E+04 -4.03E+04  4.73E+04
 
 OM33
+       -2.70E+02  4.67E+02 -7.05E+02  2.93E+02  4.70E+03 -1.29E+04  2.82E+04 -1.81E+04  9.20E+03 -3.85E+04  2.51E+04  4.13E+04
 
 OM34
+        2.43E+02 -4.06E+02  8.29E+02 -1.14E+02 -1.08E+04  2.42E+04 -5.18E+04  4.36E+04 -1.37E+04  5.49E+04 -5.01E+04 -5.50E+04
          1.05E+05
 
 OM44
+        2.50E+01 -8.28E+01 -6.44E+01 -1.42E+02  6.64E+03 -1.17E+04  2.09E+04 -2.67E+04  5.77E+03 -1.93E+04  2.43E+04  1.76E+04
         -4.33E+04  2.66E+04
 
 SG11
+        8.19E+02 -1.03E+03  1.39E+03 -1.15E+03  1.33E+04 -3.07E+04  4.94E+04 -2.51E+04  2.18E+04 -5.84E+04  3.64E+04  5.26E+04
         -4.97E+04  1.79E+04  6.92E+05
 
1
 
 
 #TBLN:      3
 #METH: Laplacian Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    YES 
 NUMERICAL 2ND DERIVATIVES:               YES 
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -1640.28119784337        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:       16
 NPARAMETR:  1.6757E+00  1.5745E+00  7.8727E-01  2.3635E+00  5.9031E-02  2.6768E-02  3.6013E-03  2.1295E-02  1.0143E-01  3.4490E-02
            -1.0777E-02  5.4731E-02  3.3782E-02  8.3169E-02  9.7428E-03
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
            -1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   7.7606E+01  2.9608E+01  3.1133E+01  1.2763E+02  2.3108E+00  1.2528E+00  1.0454E-01  2.9373E-01  3.7000E+00 -2.4202E-01
             1.0879E+00  3.2638E+00 -3.5089E+00  3.2037E+00  6.2202E+00
 
0ITERATION NO.:    1    OBJECTIVE VALUE:  -1640.28119784337        NO. OF FUNC. EVALS.:  38
 CUMULATIVE NO. OF FUNC. EVALS.:       54
 NPARAMETR:  1.6757E+00  1.5745E+00  7.8727E-01  2.3635E+00  5.9031E-02  2.6768E-02  3.6013E-03  2.1295E-02  1.0143E-01  3.4490E-02
            -1.0777E-02  5.4731E-02  3.3782E-02  8.3169E-02  9.7428E-03
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
            -1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   6.1642E+00 -1.0566E+01  1.3498E+01 -1.1319E+01  2.2675E+00  1.0506E+00  8.7435E-02  1.3305E-01  3.6671E+00 -5.0673E-01
             9.9355E-01  3.2403E+00 -4.1358E+00  3.1406E+00  6.1810E+00
 
0ITERATION NO.:    2    OBJECTIVE VALUE:  -1640.33314612451        NO. OF FUNC. EVALS.:  24
 CUMULATIVE NO. OF FUNC. EVALS.:       78             RESET HESSIAN, TYPE I
 NPARAMETR:  1.6756E+00  1.5747E+00  7.8697E-01  2.3636E+00  5.8421E-02  2.6504E-02  3.5661E-03  2.1176E-02  9.9359E-02  3.4158E-02
            -1.0804E-02  5.3616E-02  3.3385E-02  8.3394E-02  9.4413E-03
 PARAMETER:  9.9996E-02  1.0001E-01  9.9962E-02  1.0000E-01  9.4803E-02  9.9531E-02  9.9539E-02  9.9959E-02  8.8910E-02  1.0014E-01
            -1.0100E-01  8.6293E-02  1.0061E-01  9.5071E-02  8.4285E-02
 GRADIENT:   4.1981E+01  6.9518E+01 -4.9564E+00  1.9538E+02 -1.6126E-01  2.7334E+00 -1.8851E-01  1.5026E+00 -1.2805E+00  8.9007E+00
            -5.9379E+00 -1.2738E+00  1.4788E+01  2.0801E+00 -4.6978E+00
 
0ITERATION NO.:    3    OBJECTIVE VALUE:  -1640.36146715145        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:       95
 NPARAMETR:  1.6754E+00  1.5743E+00  7.8697E-01  2.3631E+00  5.8381E-02  2.6374E-02  3.5755E-03  2.1138E-02  9.9288E-02  3.3833E-02
            -1.0496E-02  5.3410E-02  3.3356E-02  8.2603E-02  9.4844E-03
 PARAMETER:  9.9986E-02  9.9988E-02  9.9963E-02  9.9983E-02  9.4462E-02  9.9078E-02  9.9834E-02  9.9813E-02  8.9124E-02  9.9129E-02
            -9.9173E-02  8.6740E-02  9.9944E-02  9.3634E-02  8.6559E-02
 GRADIENT:   7.1487E+01  2.6168E+01  2.7558E+01  1.1258E+02  9.6700E-02  6.8524E-01  1.9808E-01  2.6687E-01 -4.3909E-02 -6.5588E-01
             4.6174E-01 -2.8387E-01  5.8876E+00  1.1521E+00 -2.9763E+00
 
0ITERATION NO.:    4    OBJECTIVE VALUE:  -1640.36999329086        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      112
 NPARAMETR:  1.6748E+00  1.5739E+00  7.8652E-01  2.3624E+00  5.8329E-02  2.6288E-02  3.5436E-03  2.1115E-02  9.9133E-02  3.3851E-02
            -1.0592E-02  5.3455E-02  3.3052E-02  8.2028E-02  9.5728E-03
 PARAMETER:  9.9950E-02  9.9965E-02  9.9905E-02  9.9954E-02  9.4011E-02  9.8798E-02  9.8988E-02  9.9747E-02  8.8623E-02  9.9289E-02
            -9.9531E-02  8.6871E-02  9.9288E-02  9.2023E-02  9.1197E-02
 GRADIENT:   4.5412E+01  2.7243E+01  2.1693E+01  9.6339E+01  5.5345E-02  1.5910E-01  2.3439E-01  6.7133E-01 -2.1638E-01  8.8439E-01
            -1.0752E-01  7.1490E-01 -4.4084E+00  7.9287E-01 -5.0230E-01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -1640.37037726980        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      129
 NPARAMETR:  1.6744E+00  1.5736E+00  7.8620E-01  2.3619E+00  5.8301E-02  2.6246E-02  3.5145E-03  2.1085E-02  9.9113E-02  3.3780E-02
            -1.0601E-02  5.3289E-02  3.3054E-02  8.2031E-02  9.6031E-03
 PARAMETER:  9.9927E-02  9.9943E-02  9.9864E-02  9.9930E-02  9.3771E-02  9.8665E-02  9.8201E-02  9.9629E-02  8.8692E-02  9.9109E-02
            -9.9450E-02  8.5415E-02  9.9419E-02  9.0949E-02  9.2777E-02
 GRADIENT:   4.1489E+01  1.1541E+01  2.8686E+01  6.0305E+01  1.0201E-01 -3.1434E-01  3.0489E-01  3.3888E-01 -4.1322E-02 -6.0072E-01
             6.5753E-01  5.3502E-01 -2.4025E+00  6.6911E-01  1.6435E-01
 
0ITERATION NO.:    6    OBJECTIVE VALUE:  -1640.37037726980        NO. OF FUNC. EVALS.:  33
 CUMULATIVE NO. OF FUNC. EVALS.:      162
 NPARAMETR:  1.6744E+00  1.5736E+00  7.8620E-01  2.3619E+00  5.8301E-02  2.6246E-02  3.5145E-03  2.1085E-02  9.9113E-02  3.3780E-02
            -1.0601E-02  5.3289E-02  3.3054E-02  8.2031E-02  9.6031E-03
 PARAMETER:  9.9927E-02  9.9943E-02  9.9864E-02  9.9930E-02  9.3771E-02  9.8665E-02  9.8201E-02  9.9629E-02  8.8692E-02  9.9109E-02
            -9.9450E-02  8.5415E-02  9.9419E-02  9.0949E-02  9.2777E-02
 GRADIENT:  -3.0895E+01 -2.9556E+01  1.0636E+01 -8.0686E+01  4.2010E-02 -5.0738E-01  2.7202E-01  1.4523E-01 -5.3433E-02 -8.4597E-01
             5.6818E-01  5.1676E-01 -3.0268E+00  6.5270E-01  1.1348E-01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:  -1640.37439666057        NO. OF FUNC. EVALS.:  34
 CUMULATIVE NO. OF FUNC. EVALS.:      196
 NPARAMETR:  1.6750E+00  1.5742E+00  7.8624E-01  2.3627E+00  5.8318E-02  2.6294E-02  3.4877E-03  2.1083E-02  9.9251E-02  3.3804E-02
            -1.0595E-02  5.3231E-02  3.3117E-02  8.2195E-02  9.6073E-03
 PARAMETER:  9.9960E-02  9.9981E-02  9.9870E-02  9.9965E-02  9.3924E-02  9.8828E-02  9.7436E-02  9.9607E-02  8.9258E-02  9.9158E-02
            -9.9432E-02  8.4615E-02  9.9690E-02  9.0433E-02  9.2997E-02
 GRADIENT:  -3.8818E+01  7.5028E+00 -1.3930E+01  7.0024E+00  1.6317E-02  2.8600E-01  1.3014E-01  5.8699E-01  9.0586E-02 -6.4640E-01
             5.7734E-01  2.4025E-01  2.6321E-01  7.1884E-01  5.4660E-03
 
0ITERATION NO.:    8    OBJECTIVE VALUE:  -1640.37662093602        NO. OF FUNC. EVALS.:  32
 CUMULATIVE NO. OF FUNC. EVALS.:      228
 NPARAMETR:  1.6757E+00  1.5743E+00  7.8657E-01  2.3631E+00  5.8322E-02  2.6277E-02  3.4491E-03  2.1044E-02  9.9249E-02  3.3847E-02
            -1.0705E-02  5.3162E-02  3.3029E-02  8.2162E-02  9.6088E-03
 PARAMETER:  1.0000E-01  9.9992E-02  9.9912E-02  9.9981E-02  9.3957E-02  9.8763E-02  9.6353E-02  9.9418E-02  8.9332E-02  9.9338E-02
            -9.9848E-02  8.3295E-02  9.9753E-02  8.9032E-02  9.3078E-02
 GRADIENT:   1.1995E+01 -7.5721E+00 -3.5799E+00 -1.2683E+01  8.7506E-02 -8.3337E-02  1.4972E-01  1.7076E-01  4.1827E-02  5.9272E-01
            -3.6969E-01 -4.3805E-02  1.5894E+00  5.1478E-01 -1.9835E-01
 
0ITERATION NO.:    9    OBJECTIVE VALUE:  -1640.37757031343        NO. OF FUNC. EVALS.:  32
 CUMULATIVE NO. OF FUNC. EVALS.:      260
 NPARAMETR:  1.6758E+00  1.5745E+00  7.8665E-01  2.3632E+00  5.8289E-02  2.6273E-02  3.3949E-03  2.1008E-02  9.9209E-02  3.3793E-02
            -1.0712E-02  5.3051E-02  3.2936E-02  8.1973E-02  9.6175E-03
 PARAMETER:  1.0001E-01  1.0000E-01  9.9921E-02  9.9987E-02  9.3670E-02  9.8775E-02  9.4868E-02  9.9277E-02  8.9088E-02  9.9270E-02
            -9.9849E-02  8.2220E-02  9.9664E-02  8.7483E-02  9.3529E-02
 GRADIENT:   9.5869E+00 -5.9501E-01 -6.0662E+00  5.2422E-01 -2.2415E-02  3.8192E-01  3.5716E-02  5.8125E-01 -4.3645E-02  1.5731E-01
            -9.8997E-02 -3.0351E-02  4.0625E-01  1.5270E-01 -1.9260E-01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -1640.37780262658        NO. OF FUNC. EVALS.:  32
 CUMULATIVE NO. OF FUNC. EVALS.:      292
 NPARAMETR:  1.6757E+00  1.5746E+00  7.8677E-01  2.3633E+00  5.8276E-02  2.6206E-02  3.3464E-03  2.0931E-02  9.9158E-02  3.3770E-02
            -1.0790E-02  5.2973E-02  3.2852E-02  8.1807E-02  9.6302E-03
 PARAMETER:  1.0000E-01  1.0001E-01  9.9937E-02  9.9990E-02  9.3559E-02  9.8535E-02  9.3524E-02  9.8924E-02  8.9128E-02  9.9275E-02
            -9.9949E-02  8.1320E-02  9.9616E-02  8.6153E-02  9.4187E-02
 GRADIENT:  -1.4439E+00  5.4425E+00 -5.8463E+00  9.2350E+00  1.6968E-01 -6.9834E-01  1.4805E-01 -4.6945E-01  3.2062E-02  1.2972E-01
            -5.5909E-02 -7.8689E-02 -1.3219E-01 -1.1720E-01 -2.0380E-02
 
0ITERATION NO.:   11    OBJECTIVE VALUE:  -1640.37795149918        NO. OF FUNC. EVALS.:  32
 CUMULATIVE NO. OF FUNC. EVALS.:      324
 NPARAMETR:  1.6757E+00  1.5746E+00  7.8683E-01  2.3632E+00  5.8261E-02  2.6230E-02  3.3340E-03  2.0934E-02  9.9180E-02  3.3765E-02
            -1.0777E-02  5.2982E-02  3.2858E-02  8.1823E-02  9.6308E-03
 PARAMETER:  1.0000E-01  1.0001E-01  9.9944E-02  9.9989E-02  9.3435E-02  9.8635E-02  9.3188E-02  9.8950E-02  8.9118E-02  9.9271E-02
            -9.9947E-02  8.1461E-02  9.9625E-02  8.6226E-02  9.4222E-02
 GRADIENT:   8.1633E-01  1.8048E+00 -2.5886E+00  1.6768E+00  4.2414E-02 -6.1162E-02  8.7687E-02  4.2409E-02  4.8203E-02  2.1856E-02
            -2.1564E-02 -3.3867E-02 -1.0133E-01 -1.0754E-01 -6.0627E-03
 
0ITERATION NO.:   12    OBJECTIVE VALUE:  -1640.37800369498        NO. OF FUNC. EVALS.:  32
 CUMULATIVE NO. OF FUNC. EVALS.:      356
 NPARAMETR:  1.6756E+00  1.5746E+00  7.8693E-01  2.3633E+00  5.8248E-02  2.6228E-02  3.3049E-03  2.0919E-02  9.9147E-02  3.3745E-02
            -1.0774E-02  5.2995E-02  3.2861E-02  8.1831E-02  9.6313E-03
 PARAMETER:  9.9999E-02  1.0001E-01  9.9957E-02  9.9990E-02  9.3321E-02  9.8639E-02  9.2383E-02  9.8889E-02  8.8919E-02  9.9269E-02
            -9.9925E-02  8.1660E-02  9.9636E-02  8.6402E-02  9.4244E-02
 GRADIENT:   1.1347E+00 -7.6211E-01  6.1984E-01 -3.3819E+00 -5.9202E-02  2.1539E-01  1.4133E-02  2.7249E-01 -2.3359E-02 -1.5368E-02
            -4.8891E-02 -1.8490E-02 -4.6578E-02 -3.1638E-02  1.7638E-02
 
0ITERATION NO.:   13    OBJECTIVE VALUE:  -1640.37802015086        NO. OF FUNC. EVALS.:  32
 CUMULATIVE NO. OF FUNC. EVALS.:      388
 NPARAMETR:  1.6756E+00  1.5746E+00  7.8697E-01  2.3633E+00  5.8254E-02  2.6222E-02  3.2897E-03  2.0906E-02  9.9141E-02  3.3743E-02
            -1.0775E-02  5.3015E-02  3.2870E-02  8.1834E-02  9.6300E-03
 PARAMETER:  9.9999E-02  1.0001E-01  9.9962E-02  9.9991E-02  9.3371E-02  9.8611E-02  9.1955E-02  9.8822E-02  8.8926E-02  9.9285E-02
            -9.9882E-02  8.1888E-02  9.9647E-02  8.6538E-02  9.4179E-02
 GRADIENT:   2.4413E-01 -6.3674E-01  1.1314E+00 -2.0859E+00 -6.7938E-03  1.7270E-01 -1.7894E-02  1.1787E-01  2.6176E-03 -3.9673E-02
            -9.4400E-03 -6.6819E-03 -7.6198E-03 -3.2316E-02  1.0963E-02
 
0ITERATION NO.:   14    OBJECTIVE VALUE:  -1640.37802586592        NO. OF FUNC. EVALS.:  32
 CUMULATIVE NO. OF FUNC. EVALS.:      420
 NPARAMETR:  1.6756E+00  1.5746E+00  7.8697E-01  2.3633E+00  5.8255E-02  2.6214E-02  3.2906E-03  2.0904E-02  9.9131E-02  3.3746E-02
            -1.0772E-02  5.3031E-02  3.2880E-02  8.1844E-02  9.6285E-03
 PARAMETER:  9.9999E-02  1.0001E-01  9.9961E-02  9.9992E-02  9.3378E-02  9.8580E-02  9.1979E-02  9.8813E-02  8.8913E-02  9.9294E-02
            -9.9852E-02  8.2056E-02  9.9652E-02  8.6708E-02  9.4099E-02
 GRADIENT:  -3.5258E-01 -1.7681E-01  6.2036E-01 -5.4606E-01  3.5499E-02 -5.5963E-03  2.6414E-02  2.6954E-02 -1.2186E-02 -1.0299E-02
             1.0937E-02  1.0634E-02  1.5461E-02 -7.1747E-03  1.8247E-02
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -1640.37802595714        NO. OF FUNC. EVALS.:  20
 CUMULATIVE NO. OF FUNC. EVALS.:      440             RESET HESSIAN, TYPE I
 NPARAMETR:  1.6756E+00  1.5746E+00  7.8695E-01  2.3633E+00  5.8245E-02  2.6212E-02  3.2867E-03  2.0901E-02  9.9136E-02  3.3745E-02
            -1.0774E-02  5.3030E-02  3.2877E-02  8.1845E-02  9.6280E-03
 PARAMETER:  9.9999E-02  1.0001E-01  9.9960E-02  9.9992E-02  9.3297E-02  9.8582E-02  9.1879E-02  9.8806E-02  8.8938E-02  9.9296E-02
            -9.9858E-02  8.2043E-02  9.9651E-02  8.6741E-02  9.4075E-02
 GRADIENT:   7.1716E+01  4.1931E+01  1.8021E+01  1.4290E+02  2.4257E-02  2.7736E-01  2.7904E-02  3.0633E-01 -7.4905E-03  2.6659E-01
             9.5071E-02  6.0329E-02  6.7502E-01  4.0206E-02  5.6475E-02
 
0ITERATION NO.:   16    OBJECTIVE VALUE:  -1640.37802595714        NO. OF FUNC. EVALS.:  30
 CUMULATIVE NO. OF FUNC. EVALS.:      470
 NPARAMETR:  1.6756E+00  1.5746E+00  7.8695E-01  2.3633E+00  5.8245E-02  2.6212E-02  3.2867E-03  2.0901E-02  9.9136E-02  3.3745E-02
            -1.0774E-02  5.3030E-02  3.2877E-02  8.1845E-02  9.6280E-03
 PARAMETER:  9.9999E-02  1.0001E-01  9.9960E-02  9.9992E-02  9.3297E-02  9.8582E-02  9.1879E-02  9.8806E-02  8.8938E-02  9.9296E-02
            -9.9858E-02  8.2043E-02  9.9651E-02  8.6741E-02  9.4075E-02
 GRADIENT:  -8.4841E-01  6.3610E-01 -1.9939E-01  1.2469E+00 -2.1815E-02  6.1073E-02 -1.7567E-02  9.3375E-02 -2.7493E-02 -1.1215E-03
            -1.0132E-02  3.4863E-02  3.8052E-02  3.3745E-03  3.7471E-03
 
0ITERATION NO.:   17    OBJECTIVE VALUE:  -1640.37802595714        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      470
 NPARAMETR:  1.6756E+00  1.5746E+00  7.8695E-01  2.3633E+00  5.8245E-02  2.6212E-02  3.2867E-03  2.0901E-02  9.9136E-02  3.3745E-02
            -1.0774E-02  5.3030E-02  3.2877E-02  8.1845E-02  9.6280E-03
 PARAMETER:  9.9999E-02  1.0001E-01  9.9960E-02  9.9992E-02  9.3297E-02  9.8582E-02  9.1879E-02  9.8806E-02  8.8938E-02  9.9296E-02
            -9.9858E-02  8.2043E-02  9.9651E-02  8.6741E-02  9.4075E-02
 GRADIENT:  -8.4841E-01  6.3610E-01 -1.9939E-01  1.2469E+00 -2.1815E-02  6.1073E-02 -1.7567E-02  9.3375E-02 -2.7493E-02 -1.1215E-03
            -1.0132E-02  3.4863E-02  3.8052E-02  3.3745E-03  3.7471E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.
 REGARD THE RESULTS OF THE ESTIMATION STEP CAREFULLY, AND ACCEPT THEM ONLY
 AFTER CHECKING THAT THE COVARIANCE STEP PRODUCES REASONABLE OUTPUT.
 NO. OF FUNCTION EVALUATIONS USED:      470
 NO. OF SIG. DIGITS IN FINAL EST.:  3.1

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         9.3627E-03  7.4786E-03  9.0091E-03  7.7814E-03
 SE:             2.3623E-02  2.9504E-02  1.9234E-02  2.6940E-02
 N:                     100         100         100         100
 
 P VAL.:         6.9185E-01  7.9990E-01  6.3949E-01  7.7271E-01
 
 ETAshrink(%):   1.6246E+00  5.8239E+00  1.6058E+01  5.3563E+00
 EBVshrink(%):   2.2488E+00  6.2751E+00  1.6963E+01  6.3502E+00
 EPSshrink(%):   4.0395E+01
 
 #TERE:
 Elapsed estimation time in seconds:    64.34
 Elapsed covariance time in seconds:    58.90
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1640.378       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.68E+00  1.57E+00  7.87E-01  2.36E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        5.82E-02
 
 ETA2
+        2.62E-02  9.91E-02
 
 ETA3
+        3.29E-03  3.37E-02  5.30E-02
 
 ETA4
+        2.09E-02 -1.08E-02  3.29E-02  8.18E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.63E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.41E-01
 
 ETA2
+        3.45E-01  3.15E-01
 
 ETA3
+        5.91E-02  4.65E-01  2.30E-01
 
 ETA4
+        3.03E-01 -1.20E-01  4.99E-01  2.86E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.81E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.48E-02  3.39E-02  2.94E-02  3.14E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        8.70E-03
 
 ETA2
+        8.95E-03  1.72E-02
 
 ETA3
+        7.02E-03  1.09E-02  1.27E-02
 
 ETA4
+        8.25E-03  1.08E-02  1.05E-02  1.40E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.29E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.80E-02
 
 ETA2
+        9.66E-02  2.73E-02
 
 ETA3
+        1.24E-01  1.16E-01  2.76E-02
 
 ETA4
+        1.01E-01  1.19E-01  9.70E-02  2.45E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.56E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        6.15E-04
 
 TH 2
+        3.00E-04  1.15E-03
 
 TH 3
+        9.38E-05  3.59E-04  8.64E-04
 
 TH 4
+        2.64E-04 -7.78E-05  5.45E-04  9.87E-04
 
 OM11
+        1.02E-06  4.96E-07  3.02E-06  2.55E-06  7.57E-05
 
 OM12
+       -2.16E-07 -3.85E-06  2.15E-06  8.58E-07  3.76E-05  8.01E-05
 
 OM13
+        1.65E-06  1.63E-06 -9.84E-07  1.74E-06  1.35E-05  2.50E-05  4.93E-05
 
 OM14
+        2.30E-06  4.30E-06  4.01E-06  4.55E-06  3.39E-05  3.23E-06  3.43E-05  6.81E-05
 
 OM22
+       -6.72E-06 -8.19E-06 -3.97E-05 -3.03E-05  2.07E-05  7.38E-05  2.20E-05 -6.31E-06  2.96E-04
 
 OM23
+        2.35E-07 -1.02E-05  3.44E-05  1.51E-05  5.90E-06  2.20E-05  2.70E-05  1.43E-05  7.51E-05  1.19E-04
 
 OM24
+        3.36E-06 -2.58E-06  3.00E-05  1.88E-05  1.50E-05  2.51E-05  2.50E-05  2.83E-05 -3.38E-05  6.10E-05  1.18E-04
 
 OM33
+        1.21E-06 -1.41E-05  3.79E-05  1.56E-05  5.00E-06  7.02E-06  1.65E-05  1.30E-05  5.09E-06  7.59E-05  5.05E-05  1.62E-04
 
 OM34
+        1.62E-06  2.54E-06 -6.22E-08 -5.62E-07  7.64E-06  7.57E-06  2.58E-05  2.50E-05 -2.16E-05  1.39E-05  3.82E-05  9.37E-05
          1.10E-04
 
 OM44
+        2.80E-06  1.37E-05 -7.42E-06 -1.47E-06  1.66E-05 -6.63E-06  2.69E-05  5.42E-05 -1.38E-05 -1.01E-05 -8.61E-06  5.95E-05
          1.06E-04  1.97E-04
 
 SG11
+       -1.43E-07  5.97E-07 -3.53E-07  9.35E-08 -8.80E-07 -4.26E-07 -1.13E-06 -1.20E-06 -2.01E-06 -5.77E-07 -7.58E-07 -4.13E-06
         -2.77E-06 -2.65E-06  1.66E-06
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.48E-02
 
 TH 2
+        3.57E-01  3.39E-02
 
 TH 3
+        1.29E-01  3.60E-01  2.94E-02
 
 TH 4
+        3.38E-01 -7.31E-02  5.90E-01  3.14E-02
 
 OM11
+        4.75E-03  1.68E-03  1.18E-02  9.34E-03  8.70E-03
 
 OM12
+       -9.75E-04 -1.27E-02  8.16E-03  3.05E-03  4.83E-01  8.95E-03
 
 OM13
+        9.49E-03  6.86E-03 -4.76E-03  7.87E-03  2.21E-01  3.99E-01  7.02E-03
 
 OM14
+        1.12E-02  1.54E-02  1.65E-02  1.76E-02  4.72E-01  4.37E-02  5.92E-01  8.25E-03
 
 OM22
+       -1.58E-02 -1.40E-02 -7.85E-02 -5.62E-02  1.39E-01  4.79E-01  1.82E-01 -4.44E-02  1.72E-02
 
 OM23
+        8.68E-04 -2.76E-02  1.07E-01  4.41E-02  6.21E-02  2.25E-01  3.52E-01  1.59E-01  4.00E-01  1.09E-02
 
 OM24
+        1.25E-02 -7.02E-03  9.40E-02  5.53E-02  1.59E-01  2.58E-01  3.28E-01  3.16E-01 -1.81E-01  5.14E-01  1.08E-02
 
 OM33
+        3.84E-03 -3.26E-02  1.01E-01  3.91E-02  4.52E-02  6.16E-02  1.84E-01  1.24E-01  2.33E-02  5.46E-01  3.66E-01  1.27E-02
 
 OM34
+        6.22E-03  7.14E-03 -2.01E-04 -1.70E-03  8.35E-02  8.05E-02  3.49E-01  2.88E-01 -1.19E-01  1.21E-01  3.35E-01  7.01E-01
          1.05E-02
 
 OM44
+        8.05E-03  2.87E-02 -1.80E-02 -3.34E-03  1.36E-01 -5.28E-02  2.73E-01  4.68E-01 -5.72E-02 -6.62E-02 -5.66E-02  3.33E-01
          7.17E-01  1.40E-02
 
 SG11
+       -4.49E-03  1.37E-02 -9.33E-03  2.31E-03 -7.85E-02 -3.69E-02 -1.24E-01 -1.13E-01 -9.08E-02 -4.10E-02 -5.43E-02 -2.52E-01
         -2.05E-01 -1.46E-01  1.29E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.58E+03
 
 TH 2
+       -1.10E+03  1.66E+03
 
 TH 3
+        1.05E+03 -1.34E+03  2.93E+03
 
 TH 4
+       -1.35E+03  1.15E+03 -1.97E+03  2.54E+03
 
 OM11
+        1.01E+02 -1.88E+02  3.34E+02 -2.04E+02  3.27E+04
 
 OM12
+       -2.86E+02  5.03E+02 -9.95E+02  4.81E+02 -2.64E+04  5.05E+04
 
 OM13
+        1.91E+02 -3.73E+02  7.57E+02 -5.25E+02  2.43E+04 -4.14E+04  8.39E+04
 
 OM14
+       -1.72E+02  2.93E+02 -5.05E+02  3.87E+02 -3.25E+04  3.95E+04 -5.99E+04  7.76E+04
 
 OM22
+        1.68E+02 -2.78E+02  6.72E+02 -2.35E+02  5.19E+03 -1.60E+04  1.28E+04 -1.12E+04  1.21E+04
 
 OM23
+       -1.81E+02  3.31E+02 -8.31E+02  4.22E+02 -9.49E+03  2.75E+04 -4.29E+04  3.00E+04 -2.00E+04  6.36E+04
 
 OM24
+        1.07E+02 -2.00E+02  3.35E+02 -3.19E+02  1.30E+04 -3.03E+04  3.53E+04 -3.87E+04  1.74E+04 -4.45E+04  5.12E+04
 
 OM33
+       -3.30E+02  5.28E+02 -8.17E+02  3.49E+02  5.28E+03 -1.49E+04  3.18E+04 -2.05E+04  1.08E+04 -4.30E+04  2.78E+04  4.53E+04
 
 OM34
+        2.56E+02 -4.27E+02  8.74E+02 -1.14E+02 -1.26E+04  2.69E+04 -5.77E+04  4.95E+04 -1.54E+04  6.06E+04 -5.50E+04 -5.95E+04
          1.14E+05
 
 OM44
+        5.21E+01 -9.20E+01 -3.70E+01 -1.84E+02  7.82E+03 -1.28E+04  2.39E+04 -3.09E+04  6.36E+03 -2.12E+04  2.67E+04  1.91E+04
         -4.79E+04  3.00E+04
 
 SG11
+        7.88E+02 -9.17E+02  1.43E+03 -1.05E+03  1.72E+04 -3.71E+04  5.38E+04 -3.00E+04  2.64E+04 -6.78E+04  4.40E+04  6.00E+04
         -6.00E+04  2.25E+04  7.32E+05
 
 #CPUT: Total CPU Time in Seconds,      339.812
Stop Time: 
Mon 09/09/2013 
08:27 AM

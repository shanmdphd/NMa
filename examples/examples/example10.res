Sat 04/22/2017 
10:41 AM
$PROB  F_FLAG04est2a.ctl
$INPUT C ID DOSE=AMT TIME DV WT TYPE
$DATA example10.csv IGNORE=@

$SUBROUTINES  ADVAN2 TRANS2


$PK
   CALLFL=1
   MU_1=DLOG(THETA(1))
   KA=DEXP(MU_1+ETA(1))
   MU_2=DLOG(THETA(2))
   V=DEXP(MU_2+ETA(2))
   MU_3=DLOG(THETA(3))
   CL=DEXP(MU_3+ETA(3))
   SC=V/1000

$THETA  5.0 10.0 2.0 0.1 0.1

$OMEGA BLOCK (3)
0.5
0.01 0.5
0.01 0.01 0.5


; Because THETA(4) and THETA(5) have no inter-subject variability 
; associated with them, the algorithm must use a more computationally 
; expensive gradient evaluation for these two parameters

$SIGMA 0.1


$PRIOR NWPRI
; Priors to Omegas
$OMEGAP BLOCK (3)
0.09 FIX
0.0 0.09
0.0 0.0 0.09
$OMEGAPD (3 FIX)

$ERROR
    EXPP=THETA(4)+F*THETA(5)
IF (TYPE.EQ.0) THEN
; PK Data
    F_FLAG=0
    Y=F+F*ERR(1) ; a prediction
 ELSE
; Categorical data
    F_FLAG=1
; Use protected exponent PEXP, to avoid numerical overflow
    A=PEXP(EXPP)
    B=1+A
    Y=DV*A/B+(1-DV)/B      ; a likelihood
 ENDIF



$EST METHOD=ITS INTER LAP NITER=1000 PRINT=5 SIGL=6 NSIG=2 
     NOABORT NOPRIOR=1 CTYPE=3 CITER=10 CALPHA=0.05 
     FILE=example10.ext
; Because of categorical data, which can make conditional density highly 
; non-normal, select a t-distribution with 4 degrees of freedom for 
; importance sampling proposal density
$EST METHOD=IMP INTER LAP NITER=1000 PRINT=1 ISAMPLE=300 DF=4 
     IACCEPT=1.0
$EST METHOD=IMP EONLY=1 NITER=5 ISAMPLE=1000 PRINT=1 DF=4 
     IACCEPT=1.0 MAPITER=0 

$EST METHOD=SAEM EONLY=0 INTER LAP NBURN=2000 NITER=1000 PRINT=50 
     DF=0 IACCEPT=0.4
$EST METHOD=IMP EONLY=1 NITER=5 ISAMPLE=1000 PRINT=1 DF=4 
     IACCEPT=1.0 MAPITER=0 

$EST METHOD=BAYES NBURN=3000 NSAMPLE=3000 PRINT=100 
     FILE=example10.txt DF=0 IACCEPT=0.4 NOPRIOR=0

$EST METHOD=COND LAP INTER MAXEVAL=9999 PRINT=1 FILE=example10.ext
     NOPRIOR=1 NOHABORT

$COV UNCONDITIONAL PRINT=E MATRIX=R SIGL=10
$TABLE ID DOSE WT TIME TYPE DV A NOPRINT FILE=example10.tab
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  3) THERE MAY BE AN ERROR IN THE ABBREVIATED CODE. THE FOLLOWING
 ONE OR MORE RANDOM VARIABLES ARE DEFINED WITH "IF" STATEMENTS THAT DO NOT
 PROVIDE DEFINITIONS FOR BOTH THE "THEN" AND "ELSE" CASES. IF ALL
 CONDITIONS FAIL, THE VALUES OF THESE VARIABLES WILL BE ZERO.
  
   A B

             
 (WARNING  87) WITH "LAPLACIAN" AND "INTERACTION", "NUMERICAL" AND "SLOW"
 ARE ALSO REQUIRED ON $ESTIM RECORD, AND "SLOW" IS REQUIRED ON $COV
 RECORD. NM-TRAN HAS SUPPLIED THESE OPTIONS.
             
 (WARNING  87) WITH "LAPLACIAN" AND "INTERACTION", "NUMERICAL" AND "SLOW"
 ARE ALSO REQUIRED ON $ESTIM RECORD, AND "SLOW" IS REQUIRED ON $COV
 RECORD. NM-TRAN HAS SUPPLIED THESE OPTIONS.
             
 (WARNING  87) WITH "LAPLACIAN" AND "INTERACTION", "NUMERICAL" AND "SLOW"
 ARE ALSO REQUIRED ON $ESTIM RECORD, AND "SLOW" IS REQUIRED ON $COV
 RECORD. NM-TRAN HAS SUPPLIED THESE OPTIONS.
             
 (WARNING  87) WITH "LAPLACIAN" AND "INTERACTION", "NUMERICAL" AND "SLOW"
 ARE ALSO REQUIRED ON $ESTIM RECORD, AND "SLOW" IS REQUIRED ON $COV
 RECORD. NM-TRAN HAS SUPPLIED THESE OPTIONS.
             
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
Current Date:       22 APR 2017
Days until program expires :4785
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 beta 2 (nm74b2)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 F_FLAG04est2a.ctl
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     4608
 NO. OF DATA ITEMS IN DATA SET:   9
 ID DATA ITEM IS DATA ITEM NO.:   2
 DEP VARIABLE IS DATA ITEM NO.:   5
 MDV DATA ITEM IS DATA ITEM NO.:  9
0INDICES PASSED TO SUBROUTINE PRED:
   8   4   3   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 C ID DOSE TIME DV WT TYPE EVID MDV
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 A
0FORMAT FOR DATA:
 (7E10.0,2F2.0)

 TOT. NO. OF OBS RECS:     4320
 TOT. NO. OF INDIVIDUALS:      288
0LENGTH OF THETA:   6
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
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.5000E+01     0.1000E+07
 -0.1000E+07     0.1000E+02     0.1000E+07
 -0.1000E+07     0.2000E+01     0.1000E+07
 -0.1000E+07     0.1000E+00     0.1000E+07
 -0.1000E+07     0.1000E+00     0.1000E+07
  0.3000E+01     0.3000E+01     0.3000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.5000E+00
                  0.1000E-01   0.5000E+00
                  0.1000E-01   0.1000E-01   0.5000E+00
        2                                                                                  YES
                  0.9000E-01
                  0.0000E+00   0.9000E-01
                  0.0000E+00   0.0000E+00   0.9000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
 GRADIENT METHOD USED:       SLOW
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                10
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 TURN OFF Cholesky Transposition of R Matrix (CHOLROFF): NO
 KNUTHSUMOFF:                                -1
 RESUME COV ANALYSIS (RESUME):               NO
 SIR SAMPLE SIZE (SIRSAMPLE):              -1
 NON-LINEARLY TRANSFORM THETAS DURING COV (THBND): 1
 PRECONDTIONING CYCLES (PRECOND):        0
 PRECONDTIONING TYPES (PRECONDS):        TOS
 FORCED PRECONDTIONING CYCLES (PFCOND):0
 PRECONDTIONING TYPE (PRETYPE):        0
 FORCED POS. DEFINITE SETTING: (FPOSDEF):0
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:             3U
 MC SAMPLES (ESAMPLE):    300
 WRES SQUARE ROOT TYPE (WRESCHOL): EIGENVALUE
0-- TABLE   1 --
0RECORDS ONLY:    ALL
04 COLUMNS APPENDED:    YES
 PRINTED:                NO
 HEADERS:               YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
 FIXED_EFFECT_ETAS:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME TYPE DV A
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 beta 2 (nm74b2)

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
   EVENT ID DATA ITEM IS DATA ITEM NO.:      8
   TIME DATA ITEM IS DATA ITEM NO.:          4
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   3

0PK SUBROUTINE CALLED ONCE PER INDIVIDUAL RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 
 
 #TBLN:      1
 #METH: Iterative Two Stage (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               SLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    YES
 NUMERICAL 2ND DERIVATIVES:               YES
 NO. OF FUNCT. EVALS. ALLOWED:            960
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      6
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     6
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example10.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                ITERATIVE TWO STAGE (ITS)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          5
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 ITERATIONS (NITER):                        1000
 ANEAL SETTING (CONSTRAIN):                 1

 
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

 iteration            0 OBJ=   14995.7150788788
 iteration            5 OBJ=   10046.2722817663
 iteration           10 OBJ=   10041.4341699735
 iteration           15 OBJ=   10041.4613622819
 iteration           20 OBJ=   10041.4631246500
 iteration           25 OBJ=   10041.4632337357
 iteration           30 OBJ=   10041.4632300575
 iteration           35 OBJ=   10041.4632184088
 iteration           40 OBJ=   10041.4632288709
 iteration           45 OBJ=   10041.4632336439
 iteration           50 OBJ=   10041.4632390670
 iteration           55 OBJ=   10041.4632391584
 Convergence achieved
 iteration           55 OBJ=   10041.4632444803
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         5.4984E-09  1.9109E-08  1.6056E-08
 SE:             1.4383E-02  1.5606E-02  1.6943E-02
 N:                     288         288         288
 
 P VAL.:         1.0000E+00  1.0000E+00  1.0000E+00
 
 ETASHRINKSD(%)  1.6452E+01  2.8167E+00  1.6167E+00
 ETASHRINKVR(%)  3.0197E+01  5.5540E+00  3.2073E+00
 EBVSHRINKSD(%)  1.6452E+01  2.8167E+00  1.6167E+00
 EBVSHRINKVR(%)  3.0197E+01  5.5540E+00  3.2073E+00
 EPSSHRINKSD(%)  1.2873E+01
 EPSSHRINKVR(%)  2.4089E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10041.4632444803     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15334.5491957392     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    25.34
 Elapsed covariance  time in seconds:     1.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    10041.463       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         2.92E+00  2.96E+01  1.15E+01 -6.48E-01  8.06E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        8.56E-02
 
 ETA2
+        3.73E-03  7.45E-02
 
 ETA3
+        3.59E-02  2.99E-02  8.57E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.27E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        2.93E-01
 
 ETA2
+        4.67E-02  2.73E-01
 
 ETA3
+        4.19E-01  3.75E-01  2.93E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.51E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         6.69E-02  4.94E-01  2.02E-01  9.97E-02  7.20E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.94E-03
 
 ETA2
+        6.31E-03  6.42E-03
 
 ETA3
+        7.19E-03  5.17E-03  7.27E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.73E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.70E-02
 
 ETA2
+        7.83E-02  1.18E-02
 
 ETA3
+        6.12E-02  5.31E-02  1.24E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.56E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.47E-03
 
 TH 2
+        2.92E-03  2.44E-01
 
 TH 3
+        4.51E-03  4.02E-02  4.09E-02
 
 TH 4
+       -9.27E-05 -2.68E-03 -1.10E-03  9.94E-03
 
 TH 5
+        2.35E-05  1.91E-04  6.44E-05 -4.92E-04  5.18E-05
 
 OM11
+        2.13E-04  8.75E-05 -2.28E-05  6.36E-05 -2.34E-06  9.88E-05
 
 OM12
+        4.05E-05  1.65E-04  7.00E-05 -9.16E-05  4.25E-06  1.21E-05  3.98E-05
 
 OM13
+        6.48E-05  1.26E-04  2.04E-05 -2.13E-05  8.51E-07  5.52E-05  1.82E-05  5.17E-05
 
 OM22
+        2.39E-05 -7.51E-05  1.12E-05  3.08E-06  1.76E-06  6.27E-06  4.40E-06  5.14E-06  4.12E-05
 
 OM23
+        1.55E-05  3.55E-05  2.35E-05 -3.01E-05  3.42E-06  5.23E-06  1.49E-05  9.61E-06  1.49E-05  2.67E-05
 
 OM33
+        7.24E-06  9.33E-05  4.98E-05 -5.95E-05  1.47E-06  6.37E-06  1.21E-05  1.99E-05  3.76E-06  1.79E-05  5.29E-05
 
 SG11
+        6.38E-07  2.88E-05  2.45E-06  7.06E-07  3.00E-07 -5.51E-08 -2.56E-07  1.71E-07 -6.57E-07 -6.40E-07  3.38E-07  5.97E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        6.69E-02
 
 TH 2
+        8.82E-02  4.94E-01
 
 TH 3
+        3.34E-01  4.02E-01  2.02E-01
 
 TH 4
+       -1.39E-02 -5.44E-02 -5.47E-02  9.97E-02
 
 TH 5
+        4.88E-02  5.36E-02  4.43E-02 -6.86E-01  7.20E-03
 
 OM11
+        3.21E-01  1.78E-02 -1.13E-02  6.42E-02 -3.28E-02  9.94E-03
 
 OM12
+        9.60E-02  5.28E-02  5.49E-02 -1.46E-01  9.36E-02  1.93E-01  6.31E-03
 
 OM13
+        1.35E-01  3.54E-02  1.40E-02 -2.97E-02  1.65E-02  7.72E-01  4.01E-01  7.19E-03
 
 OM22
+        5.56E-02 -2.37E-02  8.65E-03  4.81E-03  3.82E-02  9.82E-02  1.09E-01  1.11E-01  6.42E-03
 
 OM23
+        4.48E-02  1.39E-02  2.25E-02 -5.83E-02  9.20E-02  1.02E-01  4.57E-01  2.59E-01  4.50E-01  5.17E-03
 
 OM33
+        1.49E-02  2.59E-02  3.39E-02 -8.21E-02  2.81E-02  8.81E-02  2.64E-01  3.81E-01  8.05E-02  4.77E-01  7.27E-03
 
 SG11
+        1.23E-02  7.54E-02  1.57E-02  9.17E-03  5.40E-02 -7.17E-03 -5.25E-02  3.08E-02 -1.32E-01 -1.60E-01  6.01E-02  7.73E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        3.09E+02
 
 TH 2
+        2.71E+00  4.95E+00
 
 TH 3
+       -3.69E+01 -5.12E+00  3.37E+01
 
 TH 4
+       -2.21E+00  4.23E-01  1.58E+00  1.99E+02
 
 TH 5
+       -1.66E+02 -7.56E+00  1.60E+01  1.88E+03  3.76E+04
 
 OM11
+       -1.32E+03 -1.54E+01  1.72E+02 -1.87E+02  1.56E+02  3.54E+04
 
 OM12
+       -3.70E+02 -1.15E+01  1.00E+01  3.17E+02  1.78E+03  6.83E+03  3.87E+04
 
 OM13
+        1.25E+03  7.95E+00 -1.47E+02  1.38E+02 -3.40E+02 -4.24E+04 -1.86E+04  7.70E+04
 
 OM22
+       -9.46E+01  1.09E+01 -1.01E+01 -2.73E+01 -3.93E+02 -2.73E+02  4.67E+03 -2.25E+03  3.22E+04
 
 OM23
+        9.27E+01 -7.91E+00  1.54E+01 -3.70E+02 -6.01E+03 -1.73E+03 -2.16E+04  5.56E+03 -2.29E+04  7.74E+04
 
 OM33
+       -2.59E+02  1.22E+00  1.05E+01  2.06E+02  3.04E+03  1.05E+04  4.69E+03 -2.12E+04  5.22E+03 -2.22E+04  3.21E+04
 
 SG11
+       -7.21E+02 -2.23E+02  2.01E+02 -1.67E+03 -2.84E+04  1.18E+04  1.46E+03 -1.96E+04  1.03E+04  6.31E+04 -2.88E+04  1.80E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         1.33E-01  2.74E-01  3.51E-01  5.04E-01  6.96E-01  8.06E-01  8.78E-01  1.12E+00  1.33E+00  1.58E+00  1.75E+00  2.58E+00
 
1
 
 
 #TBLN:      2
 #METH: Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               SLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    YES
 NUMERICAL 2ND DERIVATIVES:               YES
 NO. OF FUNCT. EVALS. ALLOWED:            960
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      6
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     6
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example10.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          1
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 ITERATIONS (NITER):                        1000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          300
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 EXPECTATION ONLY (EONLY):                  0
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     0.100000000000000       ,10.0000000000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          1.00000000000000
 LONG TAIL SAMPLE ACCEPT. RATE (IACCEPTL):   0.00000000000000
 T-DIST. PROPOSAL DENSITY (DF):             4
 NO. ITERATIONS FOR MAP (MAPITER):          1
 INTERVAL ITER. FOR MAP (MAPINTER):         0
 MAP COVARIANCE/MODE SETTING (MAPCOV):      1
 Gradient Quick Value (GRDQ):               0.00000000000000

 
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

 iteration            0 OBJ=   10027.3084850926 eff.=     255. Smpl.=     300. Fit.= 0.96904
 iteration            1 OBJ=   10020.3653824392 eff.=     295. Smpl.=     300. Fit.= 0.95156
 iteration            2 OBJ=   10019.7854724837 eff.=     294. Smpl.=     300. Fit.= 0.94972
 iteration            3 OBJ=   10018.7955849844 eff.=     297. Smpl.=     300. Fit.= 0.94717
 iteration            4 OBJ=   10019.7825631991 eff.=     295. Smpl.=     300. Fit.= 0.94826
 iteration            5 OBJ=   10020.4357780962 eff.=     294. Smpl.=     300. Fit.= 0.94817
 iteration            6 OBJ=   10020.3137915088 eff.=     296. Smpl.=     300. Fit.= 0.94720
 iteration            7 OBJ=   10019.7173410975 eff.=     296. Smpl.=     300. Fit.= 0.94800
 iteration            8 OBJ=   10019.5149159175 eff.=     294. Smpl.=     300. Fit.= 0.94735
 iteration            9 OBJ=   10020.4455221250 eff.=     295. Smpl.=     300. Fit.= 0.94721
 iteration           10 OBJ=   10020.7540335677 eff.=     296. Smpl.=     300. Fit.= 0.94675
 iteration           11 OBJ=   10020.6654086177 eff.=     297. Smpl.=     300. Fit.= 0.94677
 iteration           12 OBJ=   10020.3839374123 eff.=     294. Smpl.=     300. Fit.= 0.94798
 iteration           13 OBJ=   10019.3982423254 eff.=     294. Smpl.=     300. Fit.= 0.94782
 iteration           14 OBJ=   10019.7609984413 eff.=     297. Smpl.=     300. Fit.= 0.94654
 iteration           15 OBJ=   10020.0835157797 eff.=     298. Smpl.=     300. Fit.= 0.94685
 iteration           16 OBJ=   10020.0786857308 eff.=     294. Smpl.=     300. Fit.= 0.94799
 Convergence achieved
 iteration           16 OBJ=   10021.8553796494 eff.=     295. Smpl.=     300. Fit.= 0.94730
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.2759E-03 -4.4902E-04 -1.6773E-04
 SE:             1.4849E-02  1.5636E-02  1.6916E-02
 N:                     288         288         288
 
 P VAL.:         9.3152E-01  9.7709E-01  9.9209E-01
 
 ETASHRINKSD(%)  1.7185E+01  2.9145E+00  1.5770E+00
 ETASHRINKVR(%)  3.1416E+01  5.7440E+00  3.1290E+00
 EBVSHRINKSD(%)  1.7278E+01  2.8492E+00  1.6523E+00
 EBVSHRINKVR(%)  3.1571E+01  5.6172E+00  3.2774E+00
 EPSSHRINKSD(%)  1.3530E+01
 EPSSHRINKVR(%)  2.5229E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10021.8553796494     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15314.9413309083     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    55.61
 Elapsed covariance  time in seconds:    20.91
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    10021.855       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         3.07E+00  2.96E+01  1.14E+01 -6.47E-01  8.07E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.29E-02
 
 ETA2
+        3.99E-03  7.50E-02
 
 ETA3
+        3.77E-02  2.95E-02  8.54E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.28E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.05E-01
 
 ETA2
+        4.79E-02  2.74E-01
 
 ETA3
+        4.24E-01  3.69E-01  2.92E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.51E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         6.89E-02  4.93E-01  2.00E-01  9.50E-02  6.16E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.14E-02
 
 ETA2
+        6.26E-03  6.65E-03
 
 ETA3
+        7.01E-03  5.31E-03  7.35E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.20E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.88E-02
 
 ETA2
+        7.45E-02  1.21E-02
 
 ETA3
+        6.18E-02  5.29E-02  1.26E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.38E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.75E-03
 
 TH 2
+        2.91E-03  2.43E-01
 
 TH 3
+        4.95E-03  3.90E-02  4.01E-02
 
 TH 4
+        5.26E-06 -4.10E-05 -7.90E-06  9.02E-03
 
 TH 5
+        5.49E-06  3.19E-05  1.01E-05 -3.61E-04  3.80E-05
 
 OM11
+        1.20E-04 -2.35E-05  2.25E-06  6.04E-07  9.21E-08  1.31E-04
 
 OM12
+        1.64E-05  4.26E-05  1.15E-05  8.77E-08  8.81E-09  8.62E-06  3.92E-05
 
 OM13
+        2.27E-05 -2.48E-05  6.80E-07  1.31E-06 -1.29E-07  4.16E-05  1.76E-05  4.91E-05
 
 OM22
+        6.90E-06  1.96E-05  1.45E-06 -1.03E-06  2.32E-07  7.48E-07  4.92E-06  2.18E-06  4.42E-05
 
 OM23
+       -1.28E-06 -3.34E-06  2.10E-06 -1.68E-07  7.03E-08  2.32E-06  1.23E-05  7.34E-06  1.83E-05  2.82E-05
 
 OM33
+       -4.54E-06  4.34E-06  4.05E-06  6.99E-08 -9.43E-09  1.08E-05  9.36E-06  2.46E-05  7.64E-06  2.02E-05  5.41E-05
 
 SG11
+        8.68E-07  1.32E-05  4.39E-06 -1.33E-08  3.25E-08 -6.67E-07 -9.21E-08 -4.93E-08 -9.14E-08 -7.31E-08 -5.41E-08  5.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        6.89E-02
 
 TH 2
+        8.55E-02  4.93E-01
 
 TH 3
+        3.59E-01  3.95E-01  2.00E-01
 
 TH 4
+        8.04E-04 -8.76E-04 -4.15E-04  9.50E-02
 
 TH 5
+        1.29E-02  1.05E-02  8.16E-03 -6.16E-01  6.16E-03
 
 OM11
+        1.52E-01 -4.16E-03  9.80E-04  5.56E-04  1.31E-03  1.14E-02
 
 OM12
+        3.79E-02  1.38E-02  9.15E-03  1.47E-04  2.28E-04  1.20E-01  6.26E-03
 
 OM13
+        4.70E-02 -7.19E-03  4.85E-04  1.97E-03 -2.98E-03  5.19E-01  4.02E-01  7.01E-03
 
 OM22
+        1.50E-02  5.99E-03  1.09E-03 -1.64E-03  5.65E-03  9.83E-03  1.18E-01  4.69E-02  6.65E-03
 
 OM23
+       -3.51E-03 -1.27E-03  1.97E-03 -3.32E-04  2.15E-03  3.82E-02  3.68E-01  1.97E-01  5.19E-01  5.31E-03
 
 OM33
+       -8.96E-03  1.20E-03  2.75E-03  1.00E-04 -2.08E-04  1.29E-01  2.03E-01  4.78E-01  1.56E-01  5.17E-01  7.35E-03
 
 SG11
+        1.75E-02  3.72E-02  3.05E-02 -1.94E-04  7.32E-03 -8.09E-02 -2.04E-02 -9.78E-03 -1.91E-02 -1.91E-02 -1.02E-02  7.20E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.50E+02
 
 TH 2
+        2.38E+00  4.90E+00
 
 TH 3
+       -3.31E+01 -5.05E+00  3.39E+01
 
 TH 4
+       -2.05E+00 -1.69E-01  3.65E-02  1.79E+02
 
 TH 5
+       -4.70E+01 -4.59E+00  2.28E-01  1.70E+03  4.25E+04
 
 OM11
+       -2.72E+02 -3.64E+00  3.41E+01 -1.12E+00 -5.50E+01  1.13E+04
 
 OM12
+       -1.26E+02 -9.46E+00  1.40E+01  6.11E-01 -3.03E+01  2.56E+03  3.59E+04
 
 OM13
+        1.46E+02  8.83E+00 -1.93E+01  1.35E+00  1.67E+02 -1.17E+04 -1.59E+04  4.37E+04
 
 OM22
+       -6.71E+01 -4.97E+00  1.15E+01 -3.95E+00 -1.83E+02  1.76E+02  3.42E+03 -1.48E+03  3.21E+04
 
 OM23
+        8.92E+01  9.25E+00 -1.51E+01 -1.90E+00  1.79E+01 -1.28E+03 -1.89E+04  1.07E+04 -2.56E+04  7.63E+04
 
 OM33
+        8.67E+00 -4.31E+00 -1.48E+00  5.79E-01 -4.13E+01  3.06E+03  7.07E+03 -1.86E+04  5.06E+03 -2.62E+04  3.42E+04
 
 SG11
+       -5.53E+02 -9.13E+01 -5.93E+01 -9.67E+01 -2.52E+03  1.43E+04  7.17E+03 -1.47E+04  3.42E+03 -7.25E+02  4.29E+03  1.95E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.29E-01  3.83E-01  4.03E-01  5.01E-01  7.68E-01  8.18E-01  9.29E-01  1.01E+00  1.39E+00  1.59E+00  1.62E+00  2.35E+00
 
1
 
 
 #TBLN:      3
 #METH: Objective Function Evaluation by Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               SLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            960
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      6
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     6
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example10.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          1
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 ITERATIONS (NITER):                        5
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1000
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 EXPECTATION ONLY (EONLY):                  1
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     0.100000000000000       ,10.0000000000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          1.00000000000000
 LONG TAIL SAMPLE ACCEPT. RATE (IACCEPTL):   0.00000000000000
 T-DIST. PROPOSAL DENSITY (DF):             4
 NO. ITERATIONS FOR MAP (MAPITER):          0
 INTERVAL ITER. FOR MAP (MAPINTER):         0
 MAP COVARIANCE/MODE SETTING (MAPCOV):      1
 Gradient Quick Value (GRDQ):               0.00000000000000

 
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

 iteration            0 OBJ=   10020.0310466075 eff.=     812. Smpl.=    1000. Fit.= 0.95861
 iteration            1 OBJ=   10019.2354587658 eff.=     963. Smpl.=    1000. Fit.= 0.95169
 iteration            2 OBJ=   10019.9112084954 eff.=     975. Smpl.=    1000. Fit.= 0.95085
 iteration            3 OBJ=   10020.2937108764 eff.=     973. Smpl.=    1000. Fit.= 0.95092
 iteration            4 OBJ=   10019.7288982880 eff.=     972. Smpl.=    1000. Fit.= 0.95083
 iteration            5 OBJ=   10020.3774475855 eff.=     977. Smpl.=    1000. Fit.= 0.95045
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.8896E-04 -2.7203E-04 -1.2471E-04
 SE:             1.4820E-02  1.5623E-02  1.6910E-02
 N:                     288         288         288
 
 P VAL.:         9.8444E-01  9.8611E-01  9.9412E-01
 
 ETASHRINKSD(%)  1.7346E+01  2.9960E+00  1.6104E+00
 ETASHRINKVR(%)  3.1682E+01  5.9022E+00  3.1949E+00
 EBVSHRINKSD(%)  1.7330E+01  2.8530E+00  1.6484E+00
 EBVSHRINKVR(%)  3.1656E+01  5.6246E+00  3.2696E+00
 EPSSHRINKSD(%)  1.3526E+01
 EPSSHRINKVR(%)  2.5223E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10020.3774475855     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15313.4633988444     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    13.35
 Elapsed covariance  time in seconds:    68.44
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    10020.377       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         3.07E+00  2.96E+01  1.14E+01 -6.47E-01  8.07E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.29E-02
 
 ETA2
+        3.99E-03  7.50E-02
 
 ETA3
+        3.77E-02  2.95E-02  8.54E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.28E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.05E-01
 
 ETA2
+        4.79E-02  2.74E-01
 
 ETA3
+        4.24E-01  3.69E-01  2.92E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.51E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         6.89E-02  4.93E-01  2.00E-01  9.50E-02  6.16E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.17E-02
 
 ETA2
+        6.28E-03  6.66E-03
 
 ETA3
+        7.03E-03  5.33E-03  7.36E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.20E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.91E-02
 
 ETA2
+        7.48E-02  1.22E-02
 
 ETA3
+        6.25E-02  5.30E-02  1.26E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.39E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.75E-03
 
 TH 2
+        2.91E-03  2.43E-01
 
 TH 3
+        4.95E-03  3.90E-02  4.01E-02
 
 TH 4
+        6.25E-06 -3.57E-05 -6.77E-06  9.02E-03
 
 TH 5
+        5.40E-06  3.11E-05  9.88E-06 -3.61E-04  3.80E-05
 
 OM11
+        1.21E-04 -2.57E-05 -2.97E-06  3.21E-07  1.41E-07  1.36E-04
 
 OM12
+        1.58E-05  2.85E-05  8.82E-06  8.42E-08  1.19E-08  8.63E-06  3.95E-05
 
 OM13
+        2.23E-05 -3.29E-05 -6.02E-06  1.26E-06 -1.26E-07  4.14E-05  1.80E-05  4.95E-05
 
 OM22
+        6.93E-06  1.37E-05  9.35E-08 -8.78E-07  2.11E-07  8.64E-07  5.11E-06  2.33E-06  4.44E-05
 
 OM23
+       -1.45E-06 -7.04E-06  5.22E-07 -8.21E-08  5.73E-08  2.54E-06  1.23E-05  7.49E-06  1.85E-05  2.84E-05
 
 OM33
+       -4.90E-06  2.06E-06  3.11E-06  1.02E-07 -1.54E-08  1.09E-05  9.43E-06  2.46E-05  7.74E-06  2.03E-05  5.42E-05
 
 SG11
+        8.39E-07  1.29E-05  4.26E-06 -8.35E-09  3.06E-08 -7.28E-07 -8.62E-08 -4.75E-08 -9.56E-08 -8.10E-08 -6.08E-08  5.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        6.89E-02
 
 TH 2
+        8.57E-02  4.93E-01
 
 TH 3
+        3.58E-01  3.95E-01  2.00E-01
 
 TH 4
+        9.54E-04 -7.63E-04 -3.56E-04  9.50E-02
 
 TH 5
+        1.27E-02  1.03E-02  8.01E-03 -6.16E-01  6.16E-03
 
 OM11
+        1.51E-01 -4.46E-03 -1.27E-03  2.90E-04  1.97E-03  1.17E-02
 
 OM12
+        3.64E-02  9.20E-03  7.01E-03  1.41E-04  3.06E-04  1.18E-01  6.28E-03
 
 OM13
+        4.59E-02 -9.49E-03 -4.28E-03  1.89E-03 -2.91E-03  5.05E-01  4.07E-01  7.03E-03
 
 OM22
+        1.51E-02  4.18E-03  7.01E-05 -1.39E-03  5.15E-03  1.11E-02  1.22E-01  4.98E-02  6.66E-03
 
 OM23
+       -3.95E-03 -2.68E-03  4.89E-04 -1.62E-04  1.74E-03  4.09E-02  3.69E-01  2.00E-01  5.21E-01  5.33E-03
 
 OM33
+       -9.65E-03  5.69E-04  2.11E-03  1.45E-04 -3.40E-04  1.27E-01  2.04E-01  4.76E-01  1.58E-01  5.18E-01  7.36E-03
 
 SG11
+        1.69E-02  3.64E-02  2.96E-02 -1.22E-04  6.90E-03 -8.67E-02 -1.91E-02 -9.38E-03 -1.99E-02 -2.11E-02 -1.15E-02  7.20E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.50E+02
 
 TH 2
+        2.35E+00  4.90E+00
 
 TH 3
+       -3.31E+01 -5.05E+00  3.39E+01
 
 TH 4
+       -2.06E+00 -1.68E-01  3.83E-02  1.79E+02
 
 TH 5
+       -4.63E+01 -4.51E+00  2.15E-01  1.70E+03  4.25E+04
 
 OM11
+       -2.60E+02 -3.57E+00  3.24E+01 -1.43E+00 -6.82E+01  1.06E+04
 
 OM12
+       -1.18E+02 -7.55E+00  1.09E+01  1.99E-01 -4.33E+01  2.45E+03  3.58E+04
 
 OM13
+        1.27E+02  8.14E+00 -1.17E+01  1.83E+00  1.79E+02 -1.09E+04 -1.58E+04  4.25E+04
 
 OM22
+       -6.74E+01 -4.29E+00  1.11E+01 -3.91E+00 -1.74E+02  1.89E+02  3.30E+03 -1.49E+03  3.20E+04
 
 OM23
+        8.58E+01  8.24E+00 -1.19E+01 -1.51E+00  3.20E+01 -1.25E+03 -1.88E+04  1.06E+04 -2.55E+04  7.61E+04
 
 OM33
+        1.65E+01 -3.94E+00 -5.03E+00  4.10E-01 -4.52E+01  2.81E+03  7.05E+03 -1.81E+04  5.07E+03 -2.62E+04  3.40E+04
 
 SG11
+       -5.60E+02 -8.97E+01 -5.37E+01 -9.30E+01 -2.41E+03  1.47E+04  6.74E+03 -1.51E+04  3.32E+03 -6.29E+01  4.41E+03  1.95E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.31E-01  3.83E-01  4.09E-01  5.01E-01  7.67E-01  8.17E-01  9.28E-01  1.02E+00  1.38E+00  1.59E+00  1.62E+00  2.36E+00
 
1
 
 
 #TBLN:      4
 #METH: Stochastic Approximation Expectation-Maximization (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               SLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            960
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      6
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     6
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example10.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                STOCHASTIC APPROXIMATION EXPECTATION MAXIMIZATION (SAEM)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          50
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 BURN-IN ITERATIONS (NBURN):                2000
 ITERATIONS (NITER):                        1000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          2
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 EXPECTATION ONLY (EONLY):                  0
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0
 SAMPLES FOR MASS/IMP/POST. MATRIX SEARCH (ISAMPLE_M1B): 2
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2
 PWR. WT. MASS/IMP/POST MATRIX ACCUM. FOR ETAS (IKAPPA): 1.00000000000000
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1

 
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
 iteration        -2000 SAEMOBJ=   5730.21504151490
 iteration        -1950 SAEMOBJ=   5541.37439089292
 iteration        -1900 SAEMOBJ=   5594.36294849395
 iteration        -1850 SAEMOBJ=   5584.87184794356
 iteration        -1800 SAEMOBJ=   5583.98756816364
 iteration        -1750 SAEMOBJ=   5606.77194025682
 iteration        -1700 SAEMOBJ=   5571.32543576347
 iteration        -1650 SAEMOBJ=   5584.06486967236
 iteration        -1600 SAEMOBJ=   5583.60210821693
 iteration        -1550 SAEMOBJ=   5565.92967785174
 iteration        -1500 SAEMOBJ=   5550.72122374248
 iteration        -1450 SAEMOBJ=   5596.32406708445
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=   5609.11285097521
 iteration           50 SAEMOBJ=   5493.59449870509
 iteration          100 SAEMOBJ=   5489.37532933486
 iteration          150 SAEMOBJ=   5487.64043762502
 iteration          200 SAEMOBJ=   5487.11864952502
 iteration          250 SAEMOBJ=   5487.77034243472
 iteration          300 SAEMOBJ=   5486.78812458434
 iteration          350 SAEMOBJ=   5486.74375374514
 iteration          400 SAEMOBJ=   5485.93639874012
 iteration          450 SAEMOBJ=   5486.03796567153
 iteration          500 SAEMOBJ=   5486.03062823463
 iteration          550 SAEMOBJ=   5485.87006634224
 iteration          600 SAEMOBJ=   5485.82714458368
 iteration          650 SAEMOBJ=   5485.94042557687
 iteration          700 SAEMOBJ=   5485.93568906061
 iteration          750 SAEMOBJ=   5486.22595690473
 iteration          800 SAEMOBJ=   5485.69132095161
 iteration          850 SAEMOBJ=   5485.56550263941
 iteration          900 SAEMOBJ=   5485.52410161672
 iteration          950 SAEMOBJ=   5485.52701208960
 iteration         1000 SAEMOBJ=   5485.64937887749
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.6633E-06 -1.3445E-06 -1.3377E-06
 SE:             1.4848E-02  1.5621E-02  1.6911E-02
 N:                     288         288         288
 
 P VAL.:         9.9991E-01  9.9993E-01  9.9994E-01
 
 ETASHRINKSD(%)  1.7178E+01  2.8588E+00  1.6408E+00
 ETASHRINKVR(%)  3.1405E+01  5.6358E+00  3.2546E+00
 EBVSHRINKSD(%)  1.7177E+01  2.8587E+00  1.6408E+00
 EBVSHRINKVR(%)  3.1404E+01  5.6356E+00  3.2547E+00
 EPSSHRINKSD(%)  1.3518E+01
 EPSSHRINKVR(%)  2.5209E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5485.64937887749     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       10778.7353301364     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    1587.92578537767     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5485.64937887749     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       7073.57516425517     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   142.16
 Elapsed covariance  time in seconds:     0.33
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     5485.649       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         3.07E+00  2.96E+01  1.14E+01 -6.48E-01  8.07E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.29E-02
 
 ETA2
+        3.85E-03  7.47E-02
 
 ETA3
+        3.79E-02  2.95E-02  8.54E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.28E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.05E-01
 
 ETA2
+        4.62E-02  2.73E-01
 
 ETA3
+        4.26E-01  3.69E-01  2.92E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.51E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         7.44E-02  4.95E-01  2.01E-01  9.97E-02  7.20E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.11E-02
 
 ETA2
+        6.67E-03  6.44E-03
 
 ETA3
+        7.59E-03  5.16E-03  7.25E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.76E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.81E-02
 
 ETA2
+        7.95E-02  1.18E-02
 
 ETA3
+        6.16E-02  5.33E-02  1.24E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.57E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        5.53E-03
 
 TH 2
+        3.19E-03  2.45E-01
 
 TH 3
+        4.92E-03  3.96E-02  4.05E-02
 
 TH 4
+       -7.33E-05 -2.71E-03 -1.13E-03  9.94E-03
 
 TH 5
+        2.55E-05  1.94E-04  6.54E-05 -4.93E-04  5.19E-05
 
 OM11
+        2.68E-04  8.57E-05 -5.03E-05  7.63E-05 -2.74E-06  1.22E-04
 
 OM12
+        4.79E-05  1.68E-04  7.02E-05 -9.75E-05  4.67E-06  1.39E-05  4.45E-05
 
 OM13
+        7.75E-05  1.23E-04  8.84E-06 -1.98E-05  8.12E-07  6.49E-05  2.00E-05  5.76E-05
 
 OM22
+        2.50E-05 -7.67E-05  7.19E-06  3.67E-06  1.75E-06  6.84E-06  4.39E-06  5.25E-06  4.14E-05
 
 OM23
+        1.47E-05  2.78E-05  2.03E-05 -3.00E-05  3.44E-06  5.56E-06  1.56E-05  9.88E-06  1.47E-05  2.67E-05
 
 OM33
+        6.76E-06  8.95E-05  5.51E-05 -6.03E-05  1.53E-06  7.07E-06  1.26E-05  2.10E-05  3.62E-06  1.77E-05  5.26E-05
 
 SG11
+        2.26E-06  3.00E-05  2.68E-06  7.04E-07  3.05E-07  6.17E-08 -2.60E-07  2.29E-07 -6.50E-07 -6.51E-07  3.29E-07  6.03E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        7.44E-02
 
 TH 2
+        8.65E-02  4.95E-01
 
 TH 3
+        3.28E-01  3.97E-01  2.01E-01
 
 TH 4
+       -9.88E-03 -5.48E-02 -5.64E-02  9.97E-02
 
 TH 5
+        4.77E-02  5.45E-02  4.51E-02 -6.86E-01  7.20E-03
 
 OM11
+        3.26E-01  1.56E-02 -2.26E-02  6.92E-02 -3.44E-02  1.11E-02
 
 OM12
+        9.64E-02  5.07E-02  5.23E-02 -1.46E-01  9.71E-02  1.89E-01  6.67E-03
 
 OM13
+        1.37E-01  3.27E-02  5.79E-03 -2.62E-02  1.49E-02  7.74E-01  3.96E-01  7.59E-03
 
 OM22
+        5.21E-02 -2.41E-02  5.55E-03  5.72E-03  3.78E-02  9.61E-02  1.02E-01  1.08E-01  6.44E-03
 
 OM23
+        3.84E-02  1.09E-02  1.95E-02 -5.83E-02  9.26E-02  9.73E-02  4.54E-01  2.52E-01  4.43E-01  5.16E-03
 
 OM33
+        1.25E-02  2.49E-02  3.77E-02 -8.34E-02  2.92E-02  8.82E-02  2.61E-01  3.82E-01  7.76E-02  4.73E-01  7.25E-03
 
 SG11
+        3.91E-02  7.81E-02  1.72E-02  9.09E-03  5.45E-02  7.19E-03 -5.01E-02  3.88E-02 -1.30E-01 -1.62E-01  5.84E-02  7.76E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.51E+02
 
 TH 2
+        2.46E+00  4.91E+00
 
 TH 3
+       -3.34E+01 -5.06E+00  3.39E+01
 
 TH 4
+       -2.12E+00  4.22E-01  1.63E+00  1.99E+02
 
 TH 5
+       -1.46E+02 -7.70E+00  1.54E+01  1.88E+03  3.75E+04
 
 OM11
+       -1.10E+03 -1.47E+01  1.64E+02 -1.76E+02  7.43E+01  2.91E+04
 
 OM12
+       -3.30E+02 -1.08E+01  1.10E+01  2.93E+02  1.51E+03  5.93E+03  3.44E+04
 
 OM13
+        1.10E+03  8.12E+00 -1.42E+02  1.37E+02 -2.05E+02 -3.67E+04 -1.67E+04  7.00E+04
 
 OM22
+       -8.66E+01  1.01E+01 -8.99E+00 -2.81E+01 -4.27E+02 -2.52E+02  4.44E+03 -2.12E+03  3.18E+04
 
 OM23
+        8.69E+01 -7.24E+00  1.91E+01 -3.66E+02 -5.92E+03 -1.65E+03 -2.03E+04  5.46E+03 -2.24E+04  7.66E+04
 
 OM33
+       -2.27E+02  1.86E+00  3.56E+00  2.05E+02  2.99E+03  9.63E+03  4.47E+03 -2.04E+04  5.10E+03 -2.20E+04  3.22E+04
 
 SG11
+       -1.16E+03 -2.32E+02  2.68E+02 -1.65E+03 -2.81E+04  1.05E+04  1.58E+03 -1.91E+04  1.02E+04  6.33E+04 -2.82E+04  1.79E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         1.31E-01  2.75E-01  3.53E-01  5.05E-01  7.00E-01  8.08E-01  8.86E-01  1.10E+00  1.34E+00  1.59E+00  1.75E+00  2.56E+00
 
1
 
 
 #TBLN:      5
 #METH: Objective Function Evaluation by Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               SLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            960
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      6
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     6
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example10.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          1
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 ITERATIONS (NITER):                        5
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1000
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 EXPECTATION ONLY (EONLY):                  1
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     0.100000000000000       ,10.0000000000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          1.00000000000000
 LONG TAIL SAMPLE ACCEPT. RATE (IACCEPTL):   0.00000000000000
 T-DIST. PROPOSAL DENSITY (DF):             4
 NO. ITERATIONS FOR MAP (MAPITER):          0
 INTERVAL ITER. FOR MAP (MAPINTER):         0
 MAP COVARIANCE/MODE SETTING (MAPCOV):      1
 Gradient Quick Value (GRDQ):               0.00000000000000

 
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

 iteration            0 OBJ=   10019.8735468595 eff.=     802. Smpl.=    1000. Fit.= 0.96270
 iteration            1 OBJ=   10019.2115228717 eff.=     974. Smpl.=    1000. Fit.= 0.95111
 iteration            2 OBJ=   10019.9104398791 eff.=     975. Smpl.=    1000. Fit.= 0.95092
 iteration            3 OBJ=   10020.2953628772 eff.=     974. Smpl.=    1000. Fit.= 0.95091
 iteration            4 OBJ=   10019.7295700276 eff.=     972. Smpl.=    1000. Fit.= 0.95084
 iteration            5 OBJ=   10020.3775187319 eff.=     977. Smpl.=    1000. Fit.= 0.95046
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -5.9396E-05 -2.5040E-05  2.0419E-05
 SE:             1.4818E-02  1.5620E-02  1.6911E-02
 N:                     288         288         288
 
 P VAL.:         9.9680E-01  9.9872E-01  9.9904E-01
 
 ETASHRINKSD(%)  1.7345E+01  2.8670E+00  1.6415E+00
 ETASHRINKVR(%)  3.1681E+01  5.6517E+00  3.2561E+00
 EBVSHRINKSD(%)  1.7292E+01  2.8602E+00  1.6464E+00
 EBVSHRINKVR(%)  3.1594E+01  5.6386E+00  3.2657E+00
 EPSSHRINKSD(%)  1.3500E+01
 EPSSHRINKVR(%)  2.5178E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10020.3775187319     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15313.4634699908     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    13.26
 Elapsed covariance  time in seconds:    69.17
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    10020.378       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         3.07E+00  2.96E+01  1.14E+01 -6.48E-01  8.07E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.29E-02
 
 ETA2
+        3.85E-03  7.47E-02
 
 ETA3
+        3.79E-02  2.95E-02  8.54E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.28E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.05E-01
 
 ETA2
+        4.62E-02  2.73E-01
 
 ETA3
+        4.26E-01  3.69E-01  2.92E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.51E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         6.88E-02  4.92E-01  2.00E-01  9.50E-02  6.16E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.16E-02
 
 ETA2
+        6.26E-03  6.62E-03
 
 ETA3
+        7.04E-03  5.31E-03  7.37E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.19E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.91E-02
 
 ETA2
+        7.47E-02  1.21E-02
 
 ETA3
+        6.22E-02  5.29E-02  1.26E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.38E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.74E-03
 
 TH 2
+        2.86E-03  2.42E-01
 
 TH 3
+        4.97E-03  3.90E-02  4.01E-02
 
 TH 4
+        6.33E-06 -3.56E-05 -6.82E-06  9.02E-03
 
 TH 5
+        5.38E-06  3.11E-05  9.87E-06 -3.61E-04  3.80E-05
 
 OM11
+        1.19E-04 -2.72E-05 -4.45E-06  4.34E-07  1.25E-07  1.35E-04
 
 OM12
+        1.49E-05  2.49E-05  7.82E-06  9.47E-08  9.83E-09  8.29E-06  3.92E-05
 
 OM13
+        2.22E-05 -3.42E-05 -7.55E-06  1.28E-06 -1.27E-07  4.20E-05  1.79E-05  4.96E-05
 
 OM22
+        6.80E-06  9.12E-06 -6.30E-07 -8.37E-07  2.05E-07  8.33E-07  4.91E-06  2.26E-06  4.39E-05
 
 OM23
+       -1.58E-06 -9.18E-06 -6.22E-07 -6.65E-08  5.50E-08  2.49E-06  1.24E-05  7.47E-06  1.83E-05  2.82E-05
 
 OM33
+       -4.97E-06  9.40E-07  1.99E-06  1.44E-07 -2.08E-08  1.12E-05  9.54E-06  2.50E-05  7.69E-06  2.03E-05  5.43E-05
 
 SG11
+        8.48E-07  1.29E-05  4.25E-06 -8.93E-09  3.06E-08 -7.12E-07 -8.55E-08 -4.65E-08 -9.20E-08 -7.91E-08 -5.67E-08  5.17E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        6.88E-02
 
 TH 2
+        8.44E-02  4.92E-01
 
 TH 3
+        3.60E-01  3.96E-01  2.00E-01
 
 TH 4
+        9.69E-04 -7.63E-04 -3.58E-04  9.50E-02
 
 TH 5
+        1.27E-02  1.03E-02  8.00E-03 -6.16E-01  6.16E-03
 
 OM11
+        1.49E-01 -4.75E-03 -1.91E-03  3.94E-04  1.75E-03  1.16E-02
 
 OM12
+        3.46E-02  8.09E-03  6.23E-03  1.59E-04  2.55E-04  1.14E-01  6.26E-03
 
 OM13
+        4.59E-02 -9.87E-03 -5.36E-03  1.91E-03 -2.94E-03  5.13E-01  4.06E-01  7.04E-03
 
 OM22
+        1.49E-02  2.80E-03 -4.75E-04 -1.33E-03  5.03E-03  1.08E-02  1.18E-01  4.85E-02  6.62E-03
 
 OM23
+       -4.32E-03 -3.51E-03 -5.84E-04 -1.32E-04  1.68E-03  4.04E-02  3.72E-01  1.99E-01  5.21E-01  5.31E-03
 
 OM33
+       -9.80E-03  2.59E-04  1.35E-03  2.06E-04 -4.59E-04  1.30E-01  2.07E-01  4.81E-01  1.58E-01  5.18E-01  7.37E-03
 
 SG11
+        1.71E-02  3.64E-02  2.95E-02 -1.31E-04  6.90E-03 -8.51E-02 -1.90E-02 -9.18E-03 -1.93E-02 -2.07E-02 -1.07E-02  7.19E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.51E+02
 
 TH 2
+        2.45E+00  4.93E+00
 
 TH 3
+       -3.34E+01 -5.08E+00  3.40E+01
 
 TH 4
+       -2.06E+00 -1.69E-01  4.16E-02  1.79E+02
 
 TH 5
+       -4.65E+01 -4.55E+00  2.92E-01  1.70E+03  4.25E+04
 
 OM11
+       -2.59E+02 -3.68E+00  3.25E+01 -1.41E+00 -6.59E+01  1.08E+04
 
 OM12
+       -1.17E+02 -7.28E+00  1.04E+01  1.60E-01 -4.43E+01  2.69E+03  3.63E+04
 
 OM13
+        1.27E+02  8.12E+00 -1.09E+01  1.80E+00  1.77E+02 -1.13E+04 -1.62E+04  4.33E+04
 
 OM22
+       -6.80E+01 -3.87E+00  1.08E+01 -3.91E+00 -1.72E+02  2.11E+02  3.58E+03 -1.62E+03  3.24E+04
 
 OM23
+        8.54E+01  8.10E+00 -1.06E+01 -1.52E+00  3.06E+01 -1.40E+03 -1.94E+04  1.11E+04 -2.59E+04  7.69E+04
 
 OM33
+        1.68E+01 -3.95E+00 -5.32E+00  4.52E-01 -4.20E+01  2.99E+03  7.26E+03 -1.87E+04  5.15E+03 -2.65E+04  3.43E+04
 
 SG11
+       -5.57E+02 -9.01E+01 -5.31E+01 -9.28E+01 -2.41E+03  1.47E+04  7.00E+03 -1.53E+04  3.24E+03 -1.31E+02  4.37E+03  1.96E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.26E-01  3.83E-01  4.06E-01  4.99E-01  7.64E-01  8.20E-01  9.29E-01  1.02E+00  1.39E+00  1.59E+00  1.62E+00  2.36E+00
 
1
 
 
 #TBLN:      6
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               SLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            960
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      6
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     6
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example10.txt
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                MCMC BAYESIAN (BAYES)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 KEEP ITERATIONS (THIN):            1
 CONVERGENCE INTERVAL (CINTERVAL):          100
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 BURN-IN ITERATIONS (NBURN):                3000
 ITERATIONS (NITER):                        3000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0
 SAMPLES FOR MASS/IMP/POST. MATRIX SEARCH (ISAMPLE_M1B): 2
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2
 PWR. WT. MASS/IMP/POST MATRIX ACCUM. FOR ETAS (IKAPPA): 1.00000000000000
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS
 SAMPLED THETAS AND SIGMAS:
 PROPOSAL DENSITY SCALING RANGE
              (PSCALE_MIN, PSCALE_MAX):   1.000000000000000E-02   ,1000.00000000000
 SAMPLE ACCEPTANCE RATE (PACCEPT):                       0.500000000000000
 SAMPLES FOR GLOBAL SEARCH KERNEL (PSAMPLE_M1):          1
 SAMPLES FOR LOCAL SEARCH KERNEL (PSAMPLE_M2):           5
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (PSAMPLE_M3):       1
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS
 SAMPLED OMEGAS:
 SAMPLE ACCEPTANCE RATE (OACCEPT):                       0.500000000000000
 SAMPLES FOR GLOBAL SEARCH KERNEL (OSAMPLE_M1):          -1
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           6
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):6
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000

 
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
 
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
   1   2   3   4   5
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -3000 MCMCOBJ=    5812.54056061266     
 iteration        -2900 MCMCOBJ=    5784.07856013529     
 iteration        -2800 MCMCOBJ=    5835.45587075528     
 iteration        -2700 MCMCOBJ=    5751.94728760917     
 iteration        -2600 MCMCOBJ=    5781.30903691082     
 iteration        -2500 MCMCOBJ=    5770.59663903346     
 iteration        -2400 MCMCOBJ=    5863.46041718448     
 iteration        -2300 MCMCOBJ=    5754.19098874087     
 iteration        -2200 MCMCOBJ=    5712.36205908591     
 iteration        -2100 MCMCOBJ=    5775.99789023461     
 iteration        -2000 MCMCOBJ=    5832.11714238260     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=    5792.26988010719     
 iteration          100 MCMCOBJ=    5742.64278740334     
 iteration          200 MCMCOBJ=    5866.64571050878     
 iteration          300 MCMCOBJ=    5784.75582012611     
 iteration          400 MCMCOBJ=    5894.93269440916     
 iteration          500 MCMCOBJ=    5864.26859501962     
 iteration          600 MCMCOBJ=    5799.06437496235     
 iteration          700 MCMCOBJ=    5792.14127041816     
 iteration          800 MCMCOBJ=    5879.52114080726     
 iteration          900 MCMCOBJ=    5810.05418706636     
 iteration         1000 MCMCOBJ=    5751.41161071257     
 iteration         1100 MCMCOBJ=    5832.71650720719     
 iteration         1200 MCMCOBJ=    5744.19928726248     
 iteration         1300 MCMCOBJ=    5748.92028340055     
 iteration         1400 MCMCOBJ=    5728.30957200720     
 iteration         1500 MCMCOBJ=    5824.58024121139     
 iteration         1600 MCMCOBJ=    5723.37041682031     
 iteration         1700 MCMCOBJ=    5773.81476772501     
 iteration         1800 MCMCOBJ=    5931.02563486523     
 iteration         1900 MCMCOBJ=    5680.64772580975     
 iteration         2000 MCMCOBJ=    5715.49519564555     
 iteration         2100 MCMCOBJ=    5755.53565816856     
 iteration         2200 MCMCOBJ=    5824.29272963218     
 iteration         2300 MCMCOBJ=    5841.21308651150     
 iteration         2400 MCMCOBJ=    5839.27378378501     
 iteration         2500 MCMCOBJ=    5764.70207566484     
 iteration         2600 MCMCOBJ=    5765.43859396824     
 iteration         2700 MCMCOBJ=    5715.50153591828     
 iteration         2800 MCMCOBJ=    5847.24273123521     
 iteration         2900 MCMCOBJ=    5739.71970199424     
 iteration         3000 MCMCOBJ=    5750.12221445968     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5800.40696953572     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       11093.4929207946     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    1587.92578537767     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5800.40696953572     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       7388.33275491339     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    22.3596795730206     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5800.40696953572     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       5822.76664910874     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   258.67
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     5800.407       **************************************************
 #OBJS:********************************************       49.776 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         3.08E+00  2.96E+01  1.14E+01 -6.50E-01  8.12E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.48E-02
 
 ETA2
+        4.05E-03  7.63E-02
 
 ETA3
+        3.77E-02  2.98E-02  8.69E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.28E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.07E-01
 
 ETA2
+        4.73E-02  2.76E-01
 
 ETA3
+        4.15E-01  3.65E-01  2.95E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.51E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         6.48E-02  4.98E-01  1.97E-01  9.83E-02  6.16E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.19E-02
 
 ETA2
+        6.26E-03  6.78E-03
 
 ETA3
+        7.03E-03  5.51E-03  7.74E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.22E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.93E-02
 
 ETA2
+        7.28E-02  1.22E-02
 
 ETA3
+        6.14E-02  5.35E-02  1.31E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.39E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.20E-03
 
 TH 2
+        2.49E-03  2.48E-01
 
 TH 3
+        4.63E-03  3.97E-02  3.88E-02
 
 TH 4
+       -5.05E-04 -1.94E-03 -6.60E-04  9.67E-03
 
 TH 5
+        2.73E-05  1.49E-05  2.18E-05 -3.75E-04  3.79E-05
 
 OM11
+        1.21E-04  1.19E-04  3.90E-05 -7.62E-05  3.44E-06  1.43E-04
 
 OM12
+        4.53E-06  5.04E-05  1.64E-05 -1.70E-05  1.47E-06  7.27E-06  3.91E-05
 
 OM13
+        1.98E-05  3.73E-06  2.40E-05 -2.02E-05  1.30E-06  4.11E-05  1.69E-05  4.94E-05
 
 OM22
+        1.22E-05  1.71E-05  4.46E-06 -9.65E-06  1.58E-07  1.49E-07  4.39E-06  1.74E-06  4.60E-05
 
 OM23
+       -2.08E-06  5.73E-06  1.18E-05 -1.68E-05  1.01E-06  2.76E-06  1.19E-05  7.62E-06  1.93E-05  3.04E-05
 
 OM33
+        2.17E-06 -6.02E-06  3.01E-05 -2.35E-05  1.70E-06  1.20E-05  8.47E-06  2.59E-05  8.49E-06  2.29E-05  5.99E-05
 
 SG11
+        3.09E-07  1.62E-06  9.76E-08 -1.53E-07  9.38E-08 -9.28E-07 -3.05E-07 -1.86E-07 -1.24E-07 -1.08E-07 -1.70E-07  5.21E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        6.48E-02
 
 TH 2
+        7.70E-02  4.98E-01
 
 TH 3
+        3.62E-01  4.05E-01  1.97E-01
 
 TH 4
+       -7.93E-02 -3.97E-02 -3.41E-02  9.83E-02
 
 TH 5
+        6.83E-02  4.87E-03  1.79E-02 -6.19E-01  6.16E-03
 
 OM11
+        1.56E-01  2.00E-02  1.66E-02 -6.48E-02  4.68E-02  1.19E-02
 
 OM12
+        1.12E-02  1.62E-02  1.33E-02 -2.77E-02  3.81E-02  9.73E-02  6.26E-03
 
 OM13
+        4.34E-02  1.07E-03  1.74E-02 -2.93E-02  3.01E-02  4.90E-01  3.83E-01  7.03E-03
 
 OM22
+        2.78E-02  5.06E-03  3.34E-03 -1.45E-02  3.78E-03  1.83E-03  1.04E-01  3.65E-02  6.78E-03
 
 OM23
+       -5.82E-03  2.09E-03  1.09E-02 -3.10E-02  2.98E-02  4.19E-02  3.45E-01  1.97E-01  5.16E-01  5.51E-03
 
 OM33
+        4.32E-03 -1.56E-03  1.97E-02 -3.08E-02  3.57E-02  1.30E-01  1.75E-01  4.76E-01  1.62E-01  5.36E-01  7.74E-03
 
 SG11
+        6.60E-03  4.52E-03  6.86E-04 -2.16E-03  2.11E-02 -1.08E-01 -6.76E-02 -3.67E-02 -2.54E-02 -2.72E-02 -3.04E-02  7.22E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.86E+02
 
 TH 2
+        3.29E+00  4.87E+00
 
 TH 3
+       -3.71E+01 -5.36E+00  3.57E+01
 
 TH 4
+        7.81E+00  1.16E+00 -2.24E-01  1.69E+02
 
 TH 5
+       -8.74E+01  1.09E+01  4.20E+00  1.66E+03  4.29E+04
 
 OM11
+       -2.73E+02 -7.63E+00  3.48E+01  4.80E+01 -1.21E+02  9.97E+03
 
 OM12
+       -6.03E+01 -8.50E+00  7.63E+00 -4.69E-01 -9.58E+02  2.54E+03  3.54E+04
 
 OM13
+        1.51E+02  9.41E+00 -2.66E+01 -2.72E+01  2.83E+02 -1.01E+04 -1.54E+04  4.15E+04
 
 OM22
+       -1.32E+02 -3.94E+00  1.88E+01  1.63E+01  4.09E+02  3.87E+02  3.45E+03 -1.37E+03  3.08E+04
 
 OM23
+        1.56E+02  5.56E+00 -2.04E+01  2.29E+01 -5.07E+01 -1.51E+03 -1.81E+04  1.06E+04 -2.44E+04  7.25E+04
 
 OM33
+       -3.03E+01  2.10E-01 -8.47E+00  9.57E+00 -5.92E+02  2.60E+03  7.64E+03 -1.76E+04  4.97E+03 -2.60E+04  3.20E+04
 
 SG11
+       -6.32E+02 -3.27E+01  8.56E+01 -1.70E+02 -8.00E+03  1.65E+04  1.96E+04 -1.63E+04  6.15E+03 -8.78E+03  9.15E+03  1.96E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           EIGENVALUES OF COR MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.26E-01  3.79E-01  4.17E-01  4.86E-01  7.67E-01  8.39E-01  9.53E-01  1.00E+00  1.38E+00  1.51E+00  1.70E+00  2.35E+00
 
1
 
 
 #TBLN:      7
 #METH: Laplacian Conditional Estimation with Interaction (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               SLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    YES
 NUMERICAL 2ND DERIVATIVES:               YES
 NO. OF FUNCT. EVALS. ALLOWED:            9999
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      6
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     6
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example10.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 ADDITIONAL CONVERGENCE TEST (CTYPE=4)?:    NO
 EM OR BAYESIAN METHOD USED:                 NONE

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   10036.1533256353        NO. OF FUNC. EVALS.:  13
 CUMULATIVE NO. OF FUNC. EVALS.:       13
 NPARAMETR:  3.0769E+00  2.9610E+01  1.1433E+01 -6.4953E-01  8.1151E-02  9.4789E-02  4.0532E-03  3.7665E-02  7.6297E-02  2.9759E-02
             8.6911E-02  2.2837E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:   8.6346E+01  1.7501E+01  5.5165E+01  4.3854E+00  2.1414E+01  2.6210E+01  6.3214E-01 -4.5746E+01  1.3322E+01 -6.8835E+00
             2.0654E+01  2.2917E+01
 
0ITERATION NO.:    1    OBJECTIVE VALUE:   10036.1417852777        NO. OF FUNC. EVALS.:  20
 CUMULATIVE NO. OF FUNC. EVALS.:       33
 NPARAMETR:  3.0668E+00  2.9590E+01  1.1409E+01 -6.4964E-01  8.1085E-02  9.4770E-02  4.0527E-03  3.7727E-02  7.6289E-02  2.9768E-02
             8.6960E-02  2.2833E-02
 PARAMETER:  9.9671E-02  9.9933E-02  9.9790E-02 -1.0002E-01  9.9918E-02  9.9900E-02  9.9998E-02  1.0017E-01  9.9949E-02  1.0003E-01
             9.9921E-02  9.9913E-02
 GRADIENT:  -2.0739E+01  1.8515E+01 -3.1083E+01  3.0702E+00  1.7703E+01  2.6730E+01  7.1947E-01 -4.4755E+01  1.3449E+01 -6.8449E+00
             2.0785E+01  2.3184E+01
 
0ITERATION NO.:    2    OBJECTIVE VALUE:   10036.1247884449        NO. OF FUNC. EVALS.:  20
 CUMULATIVE NO. OF FUNC. EVALS.:       53
 NPARAMETR:  3.0699E+00  2.9564E+01  1.1426E+01 -6.4973E-01  8.1015E-02  9.4746E-02  4.0520E-03  3.7803E-02  7.6280E-02  2.9779E-02
             8.7019E-02  2.2828E-02
 PARAMETER:  9.9771E-02  9.9844E-02  9.9940E-02 -1.0003E-01  9.9833E-02  9.9771E-02  9.9994E-02  1.0039E-01  9.9884E-02  1.0006E-01
             9.9821E-02  9.9800E-02
 GRADIENT:  -1.0188E+01 -1.0265E+02  1.0540E+02  1.2318E+00  1.2572E+01  2.6319E+01  7.0787E-01 -4.3050E+01  1.3212E+01 -6.9293E+00
             2.0736E+01  2.1884E+01
 
0ITERATION NO.:    3    OBJECTIVE VALUE:   10036.0631218832        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:       70
 NPARAMETR:  3.0830E+00  2.9609E+01  1.1434E+01 -6.5014E-01  8.0704E-02  9.4629E-02  4.0488E-03  3.8165E-02  7.6232E-02  2.9831E-02
             8.7302E-02  2.2804E-02
 PARAMETER:  1.0020E-01  9.9995E-02  1.0001E-01 -1.0009E-01  9.9449E-02  9.9157E-02  9.9978E-02  1.0141E-01  9.9575E-02  1.0022E-01
             9.9342E-02  9.9272E-02
 GRADIENT:   1.8596E+02  2.4353E+01  1.4050E+01 -6.9046E+00 -1.1041E+01  2.4401E+01  1.6526E-01 -3.1936E+01  1.2565E+01 -4.7716E+00
             2.1146E+01  1.6088E+01
 
0ITERATION NO.:    4    OBJECTIVE VALUE:   10035.9421349633        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:       87
 NPARAMETR:  3.0650E+00  2.9678E+01  1.1447E+01 -6.5110E-01  7.9662E-02  9.4149E-02  4.0360E-03  3.9621E-02  7.6037E-02  3.0039E-02
             8.8471E-02  2.2707E-02
 PARAMETER:  9.9613E-02  1.0023E-01  1.0013E-01 -1.0024E-01  9.8165E-02  9.6613E-02  9.9915E-02  1.0555E-01  9.8293E-02  1.0085E-01
             9.7334E-02  9.7142E-02
 GRADIENT:  -8.0006E+01  1.7199E+02  1.6661E+02 -3.1885E+01 -8.5072E+01  2.2118E+01 -1.2719E+00  5.2459E+00  1.0811E+01 -2.2428E-01
             2.2045E+01 -2.9649E+00
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   10035.9208561625        NO. OF FUNC. EVALS.:  18
 CUMULATIVE NO. OF FUNC. EVALS.:      105
 NPARAMETR:  3.0628E+00  2.9572E+01  1.1408E+01 -6.5092E-01  7.9473E-02  9.3973E-02  4.0315E-03  4.0117E-02  7.5966E-02  3.0110E-02
             8.8877E-02  2.2674E-02
 PARAMETER:  9.9541E-02  9.9872E-02  9.9778E-02 -1.0021E-01  9.7931E-02  9.5677E-02  9.9896E-02  1.0697E-01  9.7821E-02  1.0108E-01
             9.6584E-02  9.6411E-02
 GRADIENT:  -3.9291E+01 -1.2992E+01  8.2365E-01 -3.5354E+01 -9.6921E+01  2.0723E+01 -1.5833E+00  1.8440E+01  1.0167E+01  1.8590E+00
             2.2450E+01 -8.5167E+00
 
0ITERATION NO.:    6    OBJECTIVE VALUE:   10035.8191889666        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      122
 NPARAMETR:  3.0632E+00  2.9587E+01  1.1414E+01 -6.4663E-01  8.0530E-02  9.3617E-02  4.0237E-03  4.0791E-02  7.5822E-02  3.0208E-02
             8.9396E-02  2.2626E-02
 PARAMETER:  9.9554E-02  9.9920E-02  9.9835E-02 -9.9553E-02  9.9234E-02  9.3777E-02  9.9894E-02  1.0898E-01  9.6868E-02  1.0140E-01
             9.4954E-02  9.5359E-02
 GRADIENT:  -3.4882E+01  1.1979E+01  2.6920E+01 -1.7570E+00 -1.0236E+01  1.8194E+01 -2.3609E+00  3.7437E+01  8.6707E+00  4.8698E+00
             2.2086E+01 -1.9035E+01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:   10035.2162428726        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      137
 NPARAMETR:  3.0579E+00  2.9591E+01  1.1406E+01 -6.4465E-01  8.0080E-02  8.7946E-02  3.9581E-03  3.8416E-02  7.3533E-02  2.9797E-02
             8.4777E-02  2.2686E-02
 PARAMETER:  9.9382E-02  9.9936E-02  9.9765E-02 -9.9249E-02  9.8680E-02  6.2532E-02  1.0138E-01  1.0589E-01  8.1473E-02  1.0158E-01
             6.3959E-02  9.6688E-02
 GRADIENT:  -3.1094E+01  5.7775E+01 -3.3666E+01 -7.0310E+00 -3.5214E+01  3.5265E+00  3.5752E-01 -1.5748E-01 -9.3081E+00  2.4414E+01
            -8.4081E+00 -1.6110E+01
 
0ITERATION NO.:    8    OBJECTIVE VALUE:   10035.2002059110        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      153
 NPARAMETR:  3.0576E+00  2.9594E+01  1.1406E+01 -6.5032E-01  8.0335E-02  8.7125E-02  3.9413E-03  3.8767E-02  7.3507E-02  2.8585E-02
             8.4079E-02  2.2796E-02
 PARAMETER:  9.9370E-02  9.9946E-02  9.9762E-02 -1.0012E-01  9.8994E-02  5.7842E-02  1.0143E-01  1.0736E-01  8.1292E-02  9.7122E-02
             6.1804E-02  9.9107E-02
 GRADIENT:  -3.1496E+01  5.8679E+01 -4.1007E+01 -1.4667E+01 -3.3579E+01  7.8976E-01 -9.7625E-01  1.4997E+01 -5.0022E+00 -1.6047E+01
            -8.8075E+00  1.0139E+00
 
0ITERATION NO.:    9    OBJECTIVE VALUE:   10035.1665387400        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      168
 NPARAMETR:  3.0563E+00  2.9595E+01  1.1406E+01 -6.5225E-01  8.0423E-02  8.5626E-02  3.9220E-03  3.8375E-02  7.4041E-02  2.9125E-02
             8.4739E-02  2.2866E-02
 PARAMETER:  9.9329E-02  9.9950E-02  9.9768E-02 -1.0042E-01  9.9102E-02  4.9166E-02  1.0181E-01  1.0720E-01  8.4914E-02  9.8703E-02
             6.5199E-02  1.0065E-01
 GRADIENT:  -3.3643E+01  5.5689E+01 -3.8905E+01 -1.7272E+01 -3.3092E+01 -3.5238E+00 -9.2697E-01  1.9112E+01 -2.2947E+00 -5.0913E+00
            -5.3964E+00  1.2072E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   10035.1565412538        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      184
 NPARAMETR:  3.0557E+00  2.9595E+01  1.1405E+01 -6.5001E-01  8.0311E-02  8.4997E-02  3.9154E-03  3.7904E-02  7.4280E-02  2.8998E-02
             8.4505E-02  2.2806E-02
 PARAMETER:  9.9310E-02  9.9949E-02  9.9759E-02 -1.0007E-01  9.8964E-02  4.5479E-02  1.0201E-01  1.0627E-01  8.6523E-02  9.8129E-02
             6.6765E-02  9.9326E-02
 GRADIENT:  -3.4418E+01  5.8340E+01 -4.4351E+01 -1.4417E+01 -3.4491E+01 -5.4097E+00 -6.3425E-01  1.3715E+01  1.4956E-02 -1.1727E+01
            -4.7593E+00  1.1567E+00
 
0ITERATION NO.:   11    OBJECTIVE VALUE:   10035.1561445387        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      201
 NPARAMETR:  3.0556E+00  2.9595E+01  1.1405E+01 -6.5097E-01  8.0355E-02  8.4962E-02  3.9160E-03  3.7873E-02  7.4193E-02  2.8965E-02
             8.4527E-02  2.2805E-02
 PARAMETER:  9.9308E-02  9.9949E-02  9.9758E-02 -1.0022E-01  9.9019E-02  4.5273E-02  1.0205E-01  1.0621E-01  8.5939E-02  9.8068E-02
             6.7235E-02  9.9290E-02
 GRADIENT:  -3.4607E+01  5.8528E+01 -4.5021E+01 -1.5708E+01 -3.4018E+01 -5.5240E+00 -5.9914E-01  1.3510E+01 -6.2998E-01 -1.1739E+01
            -4.2485E+00  9.2552E-01
 
0ITERATION NO.:   12    OBJECTIVE VALUE:   10035.1560202382        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      218
 NPARAMETR:  3.0556E+00  2.9595E+01  1.1405E+01 -6.5006E-01  8.0314E-02  8.4952E-02  3.9162E-03  3.7870E-02  7.4165E-02  2.8951E-02
             8.4537E-02  2.2806E-02
 PARAMETER:  9.9308E-02  9.9949E-02  9.9758E-02 -1.0008E-01  9.8968E-02  4.5214E-02  1.0206E-01  1.0621E-01  8.5744E-02  9.8039E-02
             6.7391E-02  9.9332E-02
 GRADIENT:  -3.4582E+01  5.8412E+01 -4.5199E+01 -1.4645E+01 -3.4573E+01 -5.6448E+00 -6.4295E-01  1.3576E+01 -8.6760E-01 -1.1882E+01
            -4.0577E+00  1.1267E+00
 
0ITERATION NO.:   13    OBJECTIVE VALUE:   10035.1319530733        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      232
 NPARAMETR:  3.0568E+00  2.9595E+01  1.1407E+01 -6.4695E-01  8.0280E-02  8.5349E-02  4.3600E-03  3.8052E-02  7.4230E-02  2.9174E-02
             8.4798E-02  2.2810E-02
 PARAMETER:  9.9347E-02  9.9949E-02  9.9775E-02 -9.9603E-02  9.8927E-02  4.7549E-02  1.1336E-01  1.0647E-01  8.5897E-02  9.8111E-02
             6.8793E-02  9.9408E-02
 GRADIENT:  -2.6352E+01  5.2891E+01 -3.6444E+01 -8.0556E+00 -2.8315E+01 -4.4031E+00  2.8498E-01  1.1383E+01 -1.1024E+00 -9.3003E+00
            -2.9554E+00  2.6443E+00
 
0ITERATION NO.:   14    OBJECTIVE VALUE:   10035.0776177125        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      247
 NPARAMETR:  3.0600E+00  2.9582E+01  1.1412E+01 -6.4557E-01  8.0565E-02  8.6421E-02  3.7650E-03  3.7666E-02  7.4384E-02  2.9261E-02
             8.4961E-02  2.2786E-02
 PARAMETER:  9.9451E-02  9.9903E-02  9.9814E-02 -9.9391E-02  9.9278E-02  5.3789E-02  9.7283E-02  1.0473E-01  8.7337E-02  9.9373E-02
             7.2692E-02  9.8878E-02
 GRADIENT:  -1.0608E+00  2.7843E+00 -4.6989E+00  1.5272E+00 -4.7953E+00 -5.9826E-01 -4.8669E-01  1.0850E+00 -1.8331E-01 -2.1308E+00
            -3.0732E-01  5.4321E-01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:   10035.0738413116        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      261
 NPARAMETR:  3.0605E+00  2.9581E+01  1.1413E+01 -6.4752E-01  8.0718E-02  8.6587E-02  3.9958E-03  3.7739E-02  7.4461E-02  2.9422E-02
             8.5107E-02  2.2779E-02
 PARAMETER:  9.9465E-02  9.9902E-02  9.9822E-02 -9.9691E-02  9.9466E-02  5.4748E-02  1.0315E-01  1.0484E-01  8.7717E-02  9.9553E-02
             7.3349E-02  9.8725E-02
 GRADIENT:   1.0147E+00 -9.6375E-01  8.7442E-01  2.2855E-01  1.8053E-01 -1.5627E-01  1.3911E-01 -2.3964E-01 -1.1075E-01  1.8372E-01
             1.3371E-01 -4.1813E-01
 
0ITERATION NO.:   16    OBJECTIVE VALUE:   10035.0738287509        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      275
 NPARAMETR:  3.0605E+00  2.9581E+01  1.1412E+01 -6.4756E-01  8.0718E-02  8.6606E-02  3.9702E-03  3.7740E-02  7.4461E-02  2.9413E-02
             8.5099E-02  2.2780E-02
 PARAMETER:  9.9465E-02  9.9902E-02  9.9821E-02 -9.9697E-02  9.9466E-02  5.4857E-02  1.0248E-01  1.0483E-01  8.7733E-02  9.9561E-02
             7.3290E-02  9.8748E-02
 GRADIENT:   6.2282E-01 -5.0532E-01  5.2004E-01  1.2515E-01  1.8330E-01 -1.0384E-01  5.8052E-02 -9.4663E-02 -1.0586E-01  1.2941E-01
             1.0605E-01 -1.3371E-01
 
0ITERATION NO.:   17    OBJECTIVE VALUE:   10035.0738287509        NO. OF FUNC. EVALS.:  24
 CUMULATIVE NO. OF FUNC. EVALS.:      299
 NPARAMETR:  3.0605E+00  2.9581E+01  1.1412E+01 -6.4756E-01  8.0718E-02  8.6606E-02  3.9702E-03  3.7740E-02  7.4461E-02  2.9413E-02
             8.5099E-02  2.2780E-02
 PARAMETER:  9.9465E-02  9.9902E-02  9.9821E-02 -9.9697E-02  9.9466E-02  5.4857E-02  1.0248E-01  1.0483E-01  8.7733E-02  9.9561E-02
             7.3290E-02  9.8748E-02
 GRADIENT:  -2.7769E+01 -4.8134E+01 -4.8964E+01 -6.0097E-01 -2.9393E+00 -1.2762E-01 -7.0621E-03 -6.4166E-01 -1.3601E-01 -2.9248E-01
             4.3237E-02 -4.8356E-01
 
0ITERATION NO.:   18    OBJECTIVE VALUE:   10035.0738287509        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      299
 NPARAMETR:  3.0605E+00  2.9581E+01  1.1412E+01 -6.4756E-01  8.0718E-02  8.6606E-02  3.9702E-03  3.7740E-02  7.4461E-02  2.9413E-02
             8.5099E-02  2.2780E-02
 PARAMETER:  9.9465E-02  9.9902E-02  9.9821E-02 -9.9697E-02  9.9466E-02  5.4857E-02  1.0248E-01  1.0483E-01  8.7733E-02  9.9561E-02
             7.3290E-02  9.8748E-02
 GRADIENT:  -2.7769E+01 -4.8134E+01 -4.8964E+01 -6.0097E-01 -2.9393E+00 -1.2762E-01 -7.0621E-03 -6.4166E-01 -1.3601E-01 -2.9248E-01
             4.3237E-02 -4.8356E-01
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      299
 NO. OF SIG. DIGITS IN FINAL EST.:  2.3

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.8423E-02  2.2889E-03  5.5054E-03
 SE:             1.4520E-02  1.5630E-02  1.6920E-02
 N:                     288         288         288
 
 P VAL.:         5.0281E-02  8.8357E-01  7.4490E-01
 
 ETASHRINKSD(%)  1.6125E+01  2.6284E+00  1.3969E+00
 ETASHRINKVR(%)  2.9650E+01  5.1877E+00  2.7742E+00
 EBVSHRINKSD(%)  1.6837E+01  2.8240E+00  1.6306E+00
 EBVSHRINKVR(%)  3.0839E+01  5.5682E+00  3.2346E+00
 EPSSHRINKSD(%)  1.2981E+01
 EPSSHRINKVR(%)  2.4278E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10035.0738287509     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15328.1597800098     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    36.09
 Elapsed covariance  time in seconds:    33.73
 Elapsed postprocess time in seconds:     0.21
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    10035.074       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         3.06E+00  2.96E+01  1.14E+01 -6.48E-01  8.07E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        8.66E-02
 
 ETA2
+        3.97E-03  7.45E-02
 
 ETA3
+        3.77E-02  2.94E-02  8.51E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.28E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        2.94E-01
 
 ETA2
+        4.94E-02  2.73E-01
 
 ETA3
+        4.40E-01  3.70E-01  2.92E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.51E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         6.67E-02  4.91E-01  2.00E-01  9.50E-02  6.17E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.08E-02
 
 ETA2
+        6.45E-03  6.57E-03
 
 ETA3
+        6.86E-03  5.19E-03  7.30E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.22E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.83E-02
 
 ETA2
+        8.02E-02  1.20E-02
 
 ETA3
+        6.30E-02  5.28E-02  1.25E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.39E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        4.45E-03
 
 TH 2
+        2.87E-03  2.41E-01
 
 TH 3
+        4.91E-03  3.87E-02  3.99E-02
 
 TH 4
+        4.36E-06 -3.27E-05 -6.63E-06  9.02E-03
 
 TH 5
+        5.29E-06  3.12E-05  9.97E-06 -3.61E-04  3.81E-05
 
 OM11
+        9.37E-05 -3.66E-05 -1.10E-05  3.02E-06 -3.02E-07  1.16E-04
 
 OM12
+        1.06E-05  1.49E-05  4.68E-06 -2.87E-05  1.86E-06  4.78E-06  4.16E-05
 
 OM13
+        1.80E-05 -4.02E-05 -1.36E-05 -1.08E-05  5.77E-07  3.76E-05  1.81E-05  4.71E-05
 
 OM22
+        3.22E-06 -7.34E-06 -3.14E-06 -2.34E-06  4.13E-07 -2.43E-06 -3.76E-07 -5.35E-07  4.32E-05
 
 OM23
+       -4.03E-06 -2.13E-05 -5.04E-06 -8.00E-06  6.25E-07  5.26E-07  1.13E-05  6.64E-06  1.63E-05  2.70E-05
 
 OM33
+       -6.19E-06 -6.25E-06 -5.47E-06 -8.93E-09 -3.15E-09  1.15E-05  7.38E-06  2.40E-05  4.41E-06  1.85E-05  5.33E-05
 
 SG11
+        8.02E-07  1.31E-05  4.31E-06  2.07E-07  2.55E-08 -8.71E-07 -6.97E-08 -7.27E-08 -3.80E-08 -4.40E-08 -2.30E-08  5.21E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        6.67E-02
 
 TH 2
+        8.76E-02  4.91E-01
 
 TH 3
+        3.69E-01  3.96E-01  2.00E-01
 
 TH 4
+        6.88E-04 -7.01E-04 -3.50E-04  9.50E-02
 
 TH 5
+        1.29E-02  1.03E-02  8.09E-03 -6.17E-01  6.17E-03
 
 OM11
+        1.30E-01 -6.93E-03 -5.11E-03  2.96E-03 -4.55E-03  1.08E-02
 
 OM12
+        2.45E-02  4.72E-03  3.63E-03 -4.68E-02  4.68E-02  6.89E-02  6.45E-03
 
 OM13
+        3.93E-02 -1.19E-02 -9.95E-03 -1.66E-02  1.36E-02  5.09E-01  4.09E-01  6.86E-03
 
 OM22
+        7.34E-03 -2.28E-03 -2.40E-03 -3.76E-03  1.02E-02 -3.44E-02 -8.89E-03 -1.19E-02  6.57E-03
 
 OM23
+       -1.16E-02 -8.35E-03 -4.87E-03 -1.62E-02  1.95E-02  9.42E-03  3.37E-01  1.86E-01  4.79E-01  5.19E-03
 
 OM33
+       -1.27E-02 -1.75E-03 -3.76E-03 -1.29E-05 -7.01E-05  1.46E-01  1.57E-01  4.79E-01  9.19E-02  4.88E-01  7.30E-03
 
 SG11
+        1.67E-02  3.70E-02  2.99E-02  3.01E-03  5.72E-03 -1.12E-01 -1.50E-02 -1.47E-02 -8.02E-03 -1.17E-02 -4.37E-03  7.22E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.68E+02
 
 TH 2
+        2.54E+00  4.96E+00
 
 TH 3
+       -3.54E+01 -5.12E+00  3.44E+01
 
 TH 4
+       -2.18E+00 -1.72E-01  5.33E-02  1.79E+02
 
 TH 5
+       -4.92E+01 -4.59E+00  5.76E-01  1.70E+03  4.24E+04
 
 OM11
+       -2.61E+02 -3.70E+00  3.24E+01 -4.67E+00  8.19E+01  1.28E+04
 
 OM12
+       -1.14E+02 -6.70E+00  9.46E+00  4.36E+01 -7.49E+02  4.06E+03  3.61E+04
 
 OM13
+        1.16E+02  8.20E+00 -7.04E+00  1.31E+01  5.46E+01 -1.29E+04 -1.87E+04  4.70E+04
 
 OM22
+       -7.17E+01 -2.84E+00  9.89E+00 -6.43E+00 -2.87E+02  7.97E+02  7.54E+03 -2.71E+03  3.28E+04
 
 OM23
+        8.65E+01  8.90E+00 -9.06E+00  3.72E+00 -1.03E+02 -1.19E+03 -2.15E+04  1.16E+04 -2.69E+04  7.79E+04
 
 OM33
+        2.32E+01 -4.51E+00 -4.00E+00 -1.19E+01  1.17E+02  2.81E+03  9.40E+03 -1.96E+04  6.62E+03 -2.68E+04  3.44E+04
 
 SG11
+       -6.12E+02 -9.15E+01 -4.76E+01 -1.48E+02 -2.54E+03  2.04E+04  8.43E+03 -1.79E+04  2.49E+03 -1.09E+02  3.08E+03  1.96E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.18E-01  3.83E-01  4.07E-01  4.96E-01  7.55E-01  8.84E-01  9.36E-01  1.04E+00  1.44E+00  1.60E+00  1.62E+00  2.22E+00
 
 Elapsed finaloutput time in seconds:     0.65
 #CPUT: Total CPU Time in Seconds,      724.079
Stop Time: 
Sat 04/22/2017 
10:54 AM

Sat 04/22/2017 
10:55 AM
$PROB  F_FLAG04est2a.ctl
$INPUT C ID DOSE=AMT TIME DV WT TYPE
$DATA example10l.csv IGNORE=@

$SUBROUTINES  ADVAN2 TRANS2


$PK
   CALLFL=1
   MU_1=THETA(1)
   KA=DEXP(MU_1+ETA(1))
   MU_2=THETA(2)
   V=DEXP(MU_2+ETA(2))
   MU_3=THETA(3)
   CL=DEXP(MU_3+ETA(3))
   SC=V/1000

$THETA  1.6 2.3 0.7 0.1 0.1

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
; Put a limit on this, as it will be exponentiated, to avoid floating 
; overflow
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

$EST METHOD=ITS INTER LAP NITER=1000 PRINT=5 SIGL=6 NSIG=2 NOABORT 
     NOPRIOR=1 CTYPE=3 CITER=10 CALPHA=0.05 FILE=example10l.ext
; Because of categorical data, which can make conditional density highly 
; non-normal, select a t-distribution with 4 degrees of freedom for 
; importance sampling proposal density
$EST METHOD=IMP INTER LAP NITER=1000 PRINT=1 ISAMPLE=300 DF=4 IACCEPT=1.0
$EST METHOD=IMP EONLY=1 NITER=5 ISAMPLE=1000 PRINT=1 DF=4 IACCEPT=1.0 
     MAPITER=0 

$EST METHOD=SAEM EONLY=0 INTER LAP NBURN=2000 NITER=1000 PRINT=50 
     DF=0 IACCEPT=0.4
$EST METHOD=IMP EONLY=1 NITER=5 ISAMPLE=1000 PRINT=1 DF=4 
     IACCEPT=1.0 MAPITER=0 

$EST METHOD=BAYES NBURN=3000 NSAMPLE=3000 PRINT=100 
     FILE=example10l.txt DF=0 IACCEPT=0.4 NOPRIOR=0

$EST METHOD=COND LAP INTER MAXEVAL=9999 PRINT=1 FILE=example10l.ext 
     NOPRIOR=1

$COV UNCONDITIONAL PRINT=E MATRIX=R SIGL=10
$TABLE ID DOSE WT TIME TYPE DV A NOPRINT FILE=example10l.tab
  
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
 -0.1000E+07     0.1600E+01     0.1000E+07
 -0.1000E+07     0.2300E+01     0.1000E+07
 -0.1000E+07     0.7000E+00     0.1000E+07
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
 RAW OUTPUT FILE (FILE): example10l.ext
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

 iteration            0 OBJ=   14969.6386288799
 iteration            5 OBJ=   10045.8290957806
 iteration           10 OBJ=   10041.4423628737
 iteration           15 OBJ=   10041.4617939550
 iteration           20 OBJ=   10041.4631480578
 iteration           25 OBJ=   10041.4632150499
 iteration           30 OBJ=   10041.4632338376
 iteration           35 OBJ=   10041.4632267854
 iteration           40 OBJ=   10041.4632413309
 iteration           45 OBJ=   10041.4632375212
 iteration           50 OBJ=   10041.4632336674
 iteration           55 OBJ=   10041.4632371359
 Convergence achieved
 iteration           55 OBJ=   10041.4632424943
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         8.4848E-09  1.4928E-08  1.1213E-08
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
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10041.4632424943     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15334.5491937532     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    24.63
 Elapsed covariance  time in seconds:     0.67
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
 
         1.07E+00  3.39E+00  2.44E+00 -6.48E-01  8.06E-02
 


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
 
         2.29E-02  1.67E-02  1.76E-02  9.97E-02  7.20E-03
 


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
+        5.25E-04
 
 TH 2
+        3.38E-05  2.79E-04
 
 TH 3
+        1.35E-04  1.18E-04  3.11E-04
 
 TH 4
+       -3.18E-05 -9.05E-05 -9.62E-05  9.94E-03
 
 TH 5
+        8.05E-06  6.45E-06  5.62E-06 -4.92E-04  5.18E-05
 
 OM11
+        7.31E-05  2.96E-06 -1.99E-06  6.36E-05 -2.34E-06  9.88E-05
 
 OM12
+        1.39E-05  5.56E-06  6.11E-06 -9.16E-05  4.25E-06  1.21E-05  3.98E-05
 
 OM13
+        2.22E-05  4.26E-06  1.78E-06 -2.13E-05  8.52E-07  5.52E-05  1.82E-05  5.17E-05
 
 OM22
+        8.18E-06 -2.54E-06  9.79E-07  3.08E-06  1.76E-06  6.27E-06  4.40E-06  5.14E-06  4.12E-05
 
 OM23
+        5.30E-06  1.20E-06  2.05E-06 -3.01E-05  3.42E-06  5.23E-06  1.49E-05  9.61E-06  1.49E-05  2.67E-05
 
 OM33
+        2.48E-06  3.15E-06  4.35E-06 -5.95E-05  1.47E-06  6.37E-06  1.21E-05  1.99E-05  3.76E-06  1.79E-05  5.29E-05
 
 SG11
+        2.19E-07  9.73E-07  2.13E-07  7.06E-07  3.00E-07 -5.51E-08 -2.56E-07  1.71E-07 -6.57E-07 -6.40E-07  3.38E-07  5.97E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.29E-02
 
 TH 2
+        8.82E-02  1.67E-02
 
 TH 3
+        3.34E-01  4.02E-01  1.76E-02
 
 TH 4
+       -1.39E-02 -5.44E-02 -5.47E-02  9.97E-02
 
 TH 5
+        4.88E-02  5.36E-02  4.43E-02 -6.86E-01  7.20E-03
 
 OM11
+        3.21E-01  1.78E-02 -1.13E-02  6.42E-02 -3.27E-02  9.94E-03
 
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
+        1.24E-02  7.54E-02  1.57E-02  9.17E-03  5.40E-02 -7.18E-03 -5.25E-02  3.08E-02 -1.32E-01 -1.60E-01  6.01E-02  7.73E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.63E+03
 
 TH 2
+        2.34E+02  4.34E+03
 
 TH 3
+       -1.24E+03 -1.74E+03  4.43E+03
 
 TH 4
+       -6.46E+00  1.25E+01  1.81E+01  1.99E+02
 
 TH 5
+       -4.86E+02 -2.24E+02  1.83E+02  1.88E+03  3.76E+04
 
 OM11
+       -3.85E+03 -4.55E+02  1.97E+03 -1.87E+02  1.56E+02  3.54E+04
 
 OM12
+       -1.08E+03 -3.39E+02  1.15E+02  3.17E+02  1.78E+03  6.83E+03  3.87E+04
 
 OM13
+        3.66E+03  2.35E+02 -1.69E+03  1.38E+02 -3.39E+02 -4.24E+04 -1.86E+04  7.70E+04
 
 OM22
+       -2.76E+02  3.23E+02 -1.16E+02 -2.73E+01 -3.93E+02 -2.73E+02  4.67E+03 -2.25E+03  3.22E+04
 
 OM23
+        2.71E+02 -2.34E+02  1.77E+02 -3.70E+02 -6.02E+03 -1.73E+03 -2.16E+04  5.56E+03 -2.29E+04  7.74E+04
 
 OM33
+       -7.56E+02  3.62E+01  1.20E+02  2.06E+02  3.04E+03  1.05E+04  4.69E+03 -2.12E+04  5.22E+03 -2.22E+04  3.21E+04
 
 SG11
+       -2.10E+03 -6.61E+03  2.30E+03 -1.67E+03 -2.84E+04  1.18E+04  1.46E+03 -1.96E+04  1.03E+04  6.31E+04 -2.88E+04  1.80E+06
 
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
 RAW OUTPUT FILE (FILE): example10l.ext
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

 iteration            0 OBJ=   10027.3084798821 eff.=     255. Smpl.=     300. Fit.= 0.96904
 iteration            1 OBJ=   10020.3234084428 eff.=     295. Smpl.=     300. Fit.= 0.95157
 iteration            2 OBJ=   10019.7756687648 eff.=     294. Smpl.=     300. Fit.= 0.94970
 iteration            3 OBJ=   10018.7924777171 eff.=     297. Smpl.=     300. Fit.= 0.94716
 iteration            4 OBJ=   10019.7818306633 eff.=     295. Smpl.=     300. Fit.= 0.94826
 iteration            5 OBJ=   10020.4355335039 eff.=     294. Smpl.=     300. Fit.= 0.94817
 iteration            6 OBJ=   10020.3137734300 eff.=     296. Smpl.=     300. Fit.= 0.94720
 iteration            7 OBJ=   10019.7173320650 eff.=     296. Smpl.=     300. Fit.= 0.94800
 iteration            8 OBJ=   10019.5148942657 eff.=     294. Smpl.=     300. Fit.= 0.94735
 iteration            9 OBJ=   10020.4455212014 eff.=     295. Smpl.=     300. Fit.= 0.94721
 iteration           10 OBJ=   10020.7540523472 eff.=     296. Smpl.=     300. Fit.= 0.94675
 iteration           11 OBJ=   10020.6653952087 eff.=     297. Smpl.=     300. Fit.= 0.94677
 iteration           12 OBJ=   10020.3839384000 eff.=     294. Smpl.=     300. Fit.= 0.94798
 iteration           13 OBJ=   10019.3982442049 eff.=     294. Smpl.=     300. Fit.= 0.94782
 iteration           14 OBJ=   10019.7609982068 eff.=     297. Smpl.=     300. Fit.= 0.94654
 iteration           15 OBJ=   10020.0835175307 eff.=     298. Smpl.=     300. Fit.= 0.94685
 iteration           16 OBJ=   10020.0786859712 eff.=     294. Smpl.=     300. Fit.= 0.94799
 Convergence achieved
 iteration           16 OBJ=   10021.8553805188 eff.=     295. Smpl.=     300. Fit.= 0.94730
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.2761E-03 -4.4904E-04 -1.6775E-04
 SE:             1.4849E-02  1.5636E-02  1.6916E-02
 N:                     288         288         288
 
 P VAL.:         9.3151E-01  9.7709E-01  9.9209E-01
 
 ETASHRINKSD(%)  1.7185E+01  2.9145E+00  1.5770E+00
 ETASHRINKVR(%)  3.1416E+01  5.7440E+00  3.1290E+00
 EBVSHRINKSD(%)  1.7278E+01  2.8492E+00  1.6523E+00
 EBVSHRINKVR(%)  3.1571E+01  5.6172E+00  3.2774E+00
 EPSSHRINKSD(%)  1.3530E+01
 EPSSHRINKVR(%)  2.5229E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10021.8553805188     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15314.9413317777     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    51.00
 Elapsed covariance  time in seconds:    21.44
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
 
         1.12E+00  3.39E+00  2.44E+00 -6.47E-01  8.07E-02
 


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
 
         2.24E-02  1.66E-02  1.75E-02  9.50E-02  6.16E-03
 


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
+        5.02E-04
 
 TH 2
+        3.19E-05  2.77E-04
 
 TH 3
+        1.41E-04  1.15E-04  3.07E-04
 
 TH 4
+        1.71E-06 -1.38E-06 -6.93E-07  9.02E-03
 
 TH 5
+        1.78E-06  1.08E-06  8.80E-07 -3.61E-04  3.80E-05
 
 OM11
+        3.91E-05 -7.97E-07  1.71E-07  6.04E-07  9.17E-08  1.31E-04
 
 OM12
+        5.31E-06  1.44E-06  1.00E-06  8.76E-08  8.76E-09  8.62E-06  3.92E-05
 
 OM13
+        7.37E-06 -8.39E-07  5.49E-08  1.31E-06 -1.29E-07  4.16E-05  1.76E-05  4.91E-05
 
 OM22
+        2.24E-06  6.62E-07  1.25E-07 -1.03E-06  2.32E-07  7.48E-07  4.92E-06  2.18E-06  4.42E-05
 
 OM23
+       -4.17E-07 -1.13E-07  1.84E-07 -1.68E-07  7.03E-08  2.32E-06  1.23E-05  7.34E-06  1.83E-05  2.82E-05
 
 OM33
+       -1.47E-06  1.47E-07  3.56E-07  6.99E-08 -9.42E-09  1.08E-05  9.36E-06  2.46E-05  7.64E-06  2.02E-05  5.41E-05
 
 SG11
+        2.82E-07  4.45E-07  3.84E-07 -1.33E-08  3.25E-08 -6.67E-07 -9.21E-08 -4.93E-08 -9.14E-08 -7.31E-08 -5.41E-08  5.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.24E-02
 
 TH 2
+        8.54E-02  1.66E-02
 
 TH 3
+        3.58E-01  3.95E-01  1.75E-02
 
 TH 4
+        8.03E-04 -8.76E-04 -4.16E-04  9.50E-02
 
 TH 5
+        1.29E-02  1.05E-02  8.15E-03 -6.16E-01  6.16E-03
 
 OM11
+        1.52E-01 -4.19E-03  8.50E-04  5.56E-04  1.30E-03  1.14E-02
 
 OM12
+        3.79E-02  1.38E-02  9.12E-03  1.47E-04  2.27E-04  1.20E-01  6.26E-03
 
 OM13
+        4.69E-02 -7.20E-03  4.47E-04  1.97E-03 -2.98E-03  5.19E-01  4.02E-01  7.01E-03
 
 OM22
+        1.50E-02  5.98E-03  1.07E-03 -1.64E-03  5.65E-03  9.83E-03  1.18E-01  4.69E-02  6.65E-03
 
 OM23
+       -3.50E-03 -1.27E-03  1.98E-03 -3.32E-04  2.15E-03  3.82E-02  3.68E-01  1.97E-01  5.19E-01  5.31E-03
 
 OM33
+       -8.95E-03  1.20E-03  2.76E-03  1.00E-04 -2.08E-04  1.29E-01  2.03E-01  4.78E-01  1.56E-01  5.17E-01  7.35E-03
 
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
+        2.37E+03
 
 TH 2
+        2.16E+02  4.31E+03
 
 TH 3
+       -1.16E+03 -1.71E+03  4.43E+03
 
 TH 4
+       -6.31E+00 -4.99E+00  4.18E-01  1.79E+02
 
 TH 5
+       -1.44E+02 -1.36E+02  2.61E+00  1.70E+03  4.25E+04
 
 OM11
+       -8.35E+02 -1.08E+02  3.90E+02 -1.12E+00 -5.50E+01  1.13E+04
 
 OM12
+       -3.88E+02 -2.80E+02  1.60E+02  6.11E-01 -3.03E+01  2.56E+03  3.59E+04
 
 OM13
+        4.49E+02  2.62E+02 -2.20E+02  1.35E+00  1.67E+02 -1.17E+04 -1.59E+04  4.37E+04
 
 OM22
+       -2.06E+02 -1.47E+02  1.31E+02 -3.95E+00 -1.83E+02  1.76E+02  3.42E+03 -1.48E+03  3.21E+04
 
 OM23
+        2.74E+02  2.74E+02 -1.72E+02 -1.90E+00  1.79E+01 -1.28E+03 -1.89E+04  1.07E+04 -2.56E+04  7.63E+04
 
 OM33
+        2.66E+01 -1.28E+02 -1.69E+01  5.79E-01 -4.13E+01  3.06E+03  7.07E+03 -1.86E+04  5.06E+03 -2.62E+04  3.42E+04
 
 SG11
+       -1.70E+03 -2.70E+03 -6.78E+02 -9.67E+01 -2.52E+03  1.43E+04  7.17E+03 -1.47E+04  3.42E+03 -7.25E+02  4.29E+03  1.95E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.29E-01  3.83E-01  4.03E-01  5.02E-01  7.68E-01  8.18E-01  9.29E-01  1.01E+00  1.39E+00  1.59E+00  1.62E+00  2.35E+00
 
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
 RAW OUTPUT FILE (FILE): example10l.ext
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

 iteration            0 OBJ=   10020.0310474114 eff.=     812. Smpl.=    1000. Fit.= 0.95861
 iteration            1 OBJ=   10019.2354593787 eff.=     963. Smpl.=    1000. Fit.= 0.95169
 iteration            2 OBJ=   10019.9112092715 eff.=     975. Smpl.=    1000. Fit.= 0.95085
 iteration            3 OBJ=   10020.2937115271 eff.=     973. Smpl.=    1000. Fit.= 0.95092
 iteration            4 OBJ=   10019.7288988186 eff.=     972. Smpl.=    1000. Fit.= 0.95083
 iteration            5 OBJ=   10020.3774484068 eff.=     977. Smpl.=    1000. Fit.= 0.95045
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.8917E-04 -2.7205E-04 -1.2474E-04
 SE:             1.4820E-02  1.5623E-02  1.6910E-02
 N:                     288         288         288
 
 P VAL.:         9.8443E-01  9.8611E-01  9.9411E-01
 
 ETASHRINKSD(%)  1.7346E+01  2.9960E+00  1.6104E+00
 ETASHRINKVR(%)  3.1682E+01  5.9022E+00  3.1949E+00
 EBVSHRINKSD(%)  1.7330E+01  2.8530E+00  1.6484E+00
 EBVSHRINKVR(%)  3.1656E+01  5.6246E+00  3.2696E+00
 EPSSHRINKSD(%)  1.3526E+01
 EPSSHRINKVR(%)  2.5223E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10020.3774484068     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15313.4633996657     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    13.33
 Elapsed covariance  time in seconds:    68.79
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
 
         1.12E+00  3.39E+00  2.44E+00 -6.47E-01  8.07E-02
 


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
 
         2.24E-02  1.66E-02  1.75E-02  9.50E-02  6.16E-03
 


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
+        5.03E-04
 
 TH 2
+        3.20E-05  2.77E-04
 
 TH 3
+        1.41E-04  1.15E-04  3.07E-04
 
 TH 4
+        2.03E-06 -1.21E-06 -5.93E-07  9.02E-03
 
 TH 5
+        1.76E-06  1.05E-06  8.64E-07 -3.61E-04  3.80E-05
 
 OM11
+        3.95E-05 -8.67E-07 -2.65E-07  3.21E-07  1.41E-07  1.36E-04
 
 OM12
+        5.13E-06  9.62E-07  7.71E-07  8.42E-08  1.19E-08  8.63E-06  3.95E-05
 
 OM13
+        7.24E-06 -1.11E-06 -5.28E-07  1.26E-06 -1.26E-07  4.14E-05  1.80E-05  4.95E-05
 
 OM22
+        2.26E-06  4.63E-07  7.81E-09 -8.78E-07  2.11E-07  8.64E-07  5.11E-06  2.33E-06  4.44E-05
 
 OM23
+       -4.72E-07 -2.37E-07  4.58E-08 -8.21E-08  5.73E-08  2.54E-06  1.23E-05  7.49E-06  1.85E-05  2.84E-05
 
 OM33
+       -1.59E-06  6.97E-08  2.73E-07  1.02E-07 -1.54E-08  1.09E-05  9.43E-06  2.46E-05  7.74E-06  2.03E-05  5.42E-05
 
 SG11
+        2.73E-07  4.36E-07  3.73E-07 -8.35E-09  3.06E-08 -7.28E-07 -8.62E-08 -4.75E-08 -9.56E-08 -8.10E-08 -6.08E-08  5.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.24E-02
 
 TH 2
+        8.57E-02  1.66E-02
 
 TH 3
+        3.58E-01  3.95E-01  1.75E-02
 
 TH 4
+        9.54E-04 -7.63E-04 -3.56E-04  9.50E-02
 
 TH 5
+        1.27E-02  1.03E-02  8.00E-03 -6.16E-01  6.16E-03
 
 OM11
+        1.51E-01 -4.47E-03 -1.30E-03  2.90E-04  1.96E-03  1.17E-02
 
 OM12
+        3.64E-02  9.20E-03  7.00E-03  1.41E-04  3.06E-04  1.18E-01  6.28E-03
 
 OM13
+        4.59E-02 -9.49E-03 -4.28E-03  1.89E-03 -2.91E-03  5.05E-01  4.07E-01  7.03E-03
 
 OM22
+        1.51E-02  4.18E-03  6.68E-05 -1.39E-03  5.15E-03  1.11E-02  1.22E-01  4.98E-02  6.66E-03
 
 OM23
+       -3.95E-03 -2.68E-03  4.91E-04 -1.62E-04  1.74E-03  4.09E-02  3.69E-01  2.00E-01  5.21E-01  5.33E-03
 
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
+        2.36E+03
 
 TH 2
+        2.14E+02  4.30E+03
 
 TH 3
+       -1.16E+03 -1.71E+03  4.43E+03
 
 TH 4
+       -6.32E+00 -4.97E+00  4.38E-01  1.79E+02
 
 TH 5
+       -1.42E+02 -1.34E+02  2.46E+00  1.70E+03  4.25E+04
 
 OM11
+       -8.00E+02 -1.06E+02  3.71E+02 -1.43E+00 -6.82E+01  1.06E+04
 
 OM12
+       -3.64E+02 -2.24E+02  1.24E+02  1.99E-01 -4.33E+01  2.45E+03  3.58E+04
 
 OM13
+        3.91E+02  2.41E+02 -1.34E+02  1.83E+00  1.79E+02 -1.09E+04 -1.58E+04  4.25E+04
 
 OM22
+       -2.07E+02 -1.27E+02  1.27E+02 -3.91E+00 -1.74E+02  1.89E+02  3.30E+03 -1.49E+03  3.20E+04
 
 OM23
+        2.64E+02  2.44E+02 -1.36E+02 -1.51E+00  3.20E+01 -1.25E+03 -1.88E+04  1.06E+04 -2.55E+04  7.61E+04
 
 OM33
+        5.06E+01 -1.17E+02 -5.75E+01  4.10E-01 -4.52E+01  2.81E+03  7.05E+03 -1.81E+04  5.07E+03 -2.62E+04  3.40E+04
 
 SG11
+       -1.72E+03 -2.66E+03 -6.13E+02 -9.30E+01 -2.41E+03  1.47E+04  6.74E+03 -1.51E+04  3.32E+03 -6.28E+01  4.41E+03  1.95E+06
 
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
 RAW OUTPUT FILE (FILE): example10l.ext
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
 iteration        -2000 SAEMOBJ=   5730.21841062567
 iteration        -1950 SAEMOBJ=   5542.76462914462
 iteration        -1900 SAEMOBJ=   5599.44631968686
 iteration        -1850 SAEMOBJ=   5583.68323568148
 iteration        -1800 SAEMOBJ=   5567.72567102099
 iteration        -1750 SAEMOBJ=   5620.02341208816
 iteration        -1700 SAEMOBJ=   5586.72573049711
 iteration        -1650 SAEMOBJ=   5580.59369427554
 iteration        -1600 SAEMOBJ=   5582.89489080707
 iteration        -1550 SAEMOBJ=   5573.54586022507
 iteration        -1500 SAEMOBJ=   5557.78332363376
 iteration        -1450 SAEMOBJ=   5588.77094867730
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=   5618.61387047330
 iteration           50 SAEMOBJ=   5495.24716249636
 iteration          100 SAEMOBJ=   5491.80627979184
 iteration          150 SAEMOBJ=   5489.51296813119
 iteration          200 SAEMOBJ=   5488.56463497552
 iteration          250 SAEMOBJ=   5489.22581784636
 iteration          300 SAEMOBJ=   5488.28562961686
 iteration          350 SAEMOBJ=   5488.20387169437
 iteration          400 SAEMOBJ=   5487.38439838273
 iteration          450 SAEMOBJ=   5487.25799229555
 iteration          500 SAEMOBJ=   5487.12161572833
 iteration          550 SAEMOBJ=   5486.80496777220
 iteration          600 SAEMOBJ=   5486.83942329476
 iteration          650 SAEMOBJ=   5486.93784156606
 iteration          700 SAEMOBJ=   5486.85092615418
 iteration          750 SAEMOBJ=   5487.13850521362
 iteration          800 SAEMOBJ=   5486.48321378754
 iteration          850 SAEMOBJ=   5486.31366625798
 iteration          900 SAEMOBJ=   5486.26328657129
 iteration          950 SAEMOBJ=   5486.30426082553
 iteration         1000 SAEMOBJ=   5486.46948653389
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -3.9223E-06 -8.3919E-07 -9.4944E-07
 SE:             1.4862E-02  1.5622E-02  1.6913E-02
 N:                     288         288         288
 
 P VAL.:         9.9979E-01  9.9996E-01  9.9996E-01
 
 ETASHRINKSD(%)  1.7196E+01  2.8588E+00  1.6419E+00
 ETASHRINKVR(%)  3.1435E+01  5.6359E+00  3.2568E+00
 EBVSHRINKSD(%)  1.7194E+01  2.8587E+00  1.6418E+00
 EBVSHRINKVR(%)  3.1432E+01  5.6357E+00  3.2567E+00
 EPSSHRINKSD(%)  1.3526E+01
 EPSSHRINKVR(%)  2.5223E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5486.46948653389     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       10779.5554377928     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    1587.92578537767     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5486.46948653389     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       7074.39527191156     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   141.91
 Elapsed covariance  time in seconds:     0.35
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     5486.469       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.12E+00  3.39E+00  2.44E+00 -6.48E-01  8.07E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.31E-02
 
 ETA2
+        3.84E-03  7.47E-02
 
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
+        4.61E-02  2.73E-01
 
 ETA3
+        4.25E-01  3.69E-01  2.92E-01
 


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
 
         2.42E-02  1.67E-02  1.76E-02  9.97E-02  7.20E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.11E-02
 
 ETA2
+        6.68E-03  6.44E-03
 
 ETA3
+        7.60E-03  5.17E-03  7.25E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.77E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.82E-02
 
 ETA2
+        7.94E-02  1.18E-02
 
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
+        5.88E-04
 
 TH 2
+        3.53E-05  2.80E-04
 
 TH 3
+        1.40E-04  1.17E-04  3.10E-04
 
 TH 4
+       -2.39E-05 -9.10E-05 -9.88E-05  9.94E-03
 
 TH 5
+        8.30E-06  6.55E-06  5.71E-06 -4.93E-04  5.19E-05
 
 OM11
+        8.76E-05  3.05E-06 -4.32E-06  7.63E-05 -2.73E-06  1.23E-04
 
 OM12
+        1.56E-05  5.68E-06  6.19E-06 -9.76E-05  4.67E-06  1.40E-05  4.46E-05
 
 OM13
+        2.53E-05  4.21E-06  8.14E-07 -1.98E-05  8.18E-07  6.51E-05  2.01E-05  5.77E-05
 
 OM22
+        8.10E-06 -2.56E-06  6.51E-07  3.54E-06  1.77E-06  6.85E-06  4.41E-06  5.27E-06  4.14E-05
 
 OM23
+        4.80E-06  9.47E-07  1.79E-06 -3.01E-05  3.45E-06  5.56E-06  1.57E-05  9.89E-06  1.48E-05  2.67E-05
 
 OM33
+        2.23E-06  3.04E-06  4.84E-06 -6.03E-05  1.52E-06  7.05E-06  1.26E-05  2.10E-05  3.62E-06  1.77E-05  5.26E-05
 
 SG11
+        7.63E-07  1.02E-06  2.37E-07  7.29E-07  3.03E-07  6.97E-08 -2.64E-07  2.30E-07 -6.50E-07 -6.53E-07  3.30E-07  6.03E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.42E-02
 
 TH 2
+        8.69E-02  1.67E-02
 
 TH 3
+        3.28E-01  3.97E-01  1.76E-02
 
 TH 4
+       -9.88E-03 -5.46E-02 -5.63E-02  9.97E-02
 
 TH 5
+        4.75E-02  5.43E-02  4.50E-02 -6.86E-01  7.20E-03
 
 OM11
+        3.26E-01  1.65E-02 -2.21E-02  6.91E-02 -3.42E-02  1.11E-02
 
 OM12
+        9.63E-02  5.08E-02  5.26E-02 -1.46E-01  9.72E-02  1.89E-01  6.68E-03
 
 OM13
+        1.37E-01  3.32E-02  6.08E-03 -2.61E-02  1.49E-02  7.74E-01  3.96E-01  7.60E-03
 
 OM22
+        5.19E-02 -2.38E-02  5.74E-03  5.51E-03  3.81E-02  9.60E-02  1.03E-01  1.08E-01  6.44E-03
 
 OM23
+        3.83E-02  1.10E-02  1.97E-02 -5.85E-02  9.27E-02  9.71E-02  4.54E-01  2.52E-01  4.44E-01  5.17E-03
 
 OM33
+        1.27E-02  2.50E-02  3.79E-02 -8.33E-02  2.92E-02  8.77E-02  2.61E-01  3.81E-01  7.76E-02  4.73E-01  7.25E-03
 
 SG11
+        4.05E-02  7.86E-02  1.73E-02  9.41E-03  5.42E-02  8.09E-03 -5.09E-02  3.89E-02 -1.30E-01 -1.63E-01  5.86E-02  7.77E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.36E+03
 
 TH 2
+        2.23E+02  4.30E+03
 
 TH 3
+       -1.17E+03 -1.71E+03  4.42E+03
 
 TH 4
+       -6.33E+00  1.24E+01  1.85E+01  1.99E+02
 
 TH 5
+       -4.43E+02 -2.27E+02  1.74E+02  1.87E+03  3.75E+04
 
 OM11
+       -3.36E+03 -4.36E+02  1.86E+03 -1.75E+02  6.93E+01  2.90E+04
 
 OM12
+       -1.01E+03 -3.19E+02  1.20E+02  2.93E+02  1.50E+03  5.90E+03  3.43E+04
 
 OM13
+        3.35E+03  2.40E+02 -1.61E+03  1.36E+02 -2.02E+02 -3.65E+04 -1.66E+04  6.96E+04
 
 OM22
+       -2.64E+02  2.97E+02 -1.04E+02 -2.82E+01 -4.31E+02 -2.49E+02  4.41E+03 -2.12E+03  3.18E+04
 
 OM23
+        2.60E+02 -2.16E+02  2.22E+02 -3.66E+02 -5.91E+03 -1.63E+03 -2.02E+04  5.41E+03 -2.24E+04  7.65E+04
 
 OM33
+       -6.92E+02  5.56E+01  3.63E+01  2.06E+02  2.99E+03  9.57E+03  4.44E+03 -2.03E+04  5.10E+03 -2.20E+04  3.22E+04
 
 SG11
+       -3.63E+03 -6.92E+03  3.09E+03 -1.64E+03 -2.80E+04  1.03E+04  1.74E+03 -1.89E+04  1.01E+04  6.34E+04 -2.84E+04  1.79E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         1.31E-01  2.75E-01  3.53E-01  5.05E-01  7.00E-01  8.08E-01  8.87E-01  1.10E+00  1.34E+00  1.59E+00  1.75E+00  2.56E+00
 
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
 RAW OUTPUT FILE (FILE): example10l.ext
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

 iteration            0 OBJ=   10019.8378841711 eff.=     801. Smpl.=    1000. Fit.= 0.96264
 iteration            1 OBJ=   10019.1902703615 eff.=     974. Smpl.=    1000. Fit.= 0.95110
 iteration            2 OBJ=   10019.9074120484 eff.=     975. Smpl.=    1000. Fit.= 0.95089
 iteration            3 OBJ=   10020.2947873206 eff.=     974. Smpl.=    1000. Fit.= 0.95090
 iteration            4 OBJ=   10019.7282387760 eff.=     972. Smpl.=    1000. Fit.= 0.95083
 iteration            5 OBJ=   10020.3768847083 eff.=     977. Smpl.=    1000. Fit.= 0.95045
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.6178E-04 -5.6556E-05 -5.6526E-06
 SE:             1.4830E-02  1.5620E-02  1.6911E-02
 N:                     288         288         288
 
 P VAL.:         9.8592E-01  9.9711E-01  9.9973E-01
 
 ETASHRINKSD(%)  1.7375E+01  2.8730E+00  1.6537E+00
 ETASHRINKVR(%)  3.1732E+01  5.6635E+00  3.2800E+00
 EBVSHRINKSD(%)  1.7280E+01  2.8599E+00  1.6460E+00
 EBVSHRINKVR(%)  3.1575E+01  5.6380E+00  3.2650E+00
 EPSSHRINKSD(%)  1.3506E+01
 EPSSHRINKVR(%)  2.5188E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10020.3768847083     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15313.4628359672     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    13.22
 Elapsed covariance  time in seconds:    68.83
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
 
         1.12E+00  3.39E+00  2.44E+00 -6.48E-01  8.07E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.31E-02
 
 ETA2
+        3.84E-03  7.47E-02
 
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
+        4.61E-02  2.73E-01
 
 ETA3
+        4.25E-01  3.69E-01  2.92E-01
 


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
 
         2.24E-02  1.66E-02  1.75E-02  9.50E-02  6.16E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.17E-02
 
 ETA2
+        6.27E-03  6.62E-03
 
 ETA3
+        7.05E-03  5.32E-03  7.37E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.19E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.92E-02
 
 ETA2
+        7.47E-02  1.21E-02
 
 ETA3
+        6.24E-02  5.29E-02  1.26E-02
 


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
+        5.03E-04
 
 TH 2
+        3.14E-05  2.76E-04
 
 TH 3
+        1.41E-04  1.15E-04  3.07E-04
 
 TH 4
+        2.05E-06 -1.21E-06 -5.95E-07  9.02E-03
 
 TH 5
+        1.75E-06  1.05E-06  8.64E-07 -3.61E-04  3.80E-05
 
 OM11
+        3.95E-05 -8.89E-07 -2.86E-07  4.09E-07  1.25E-07  1.37E-04
 
 OM12
+        4.87E-06  9.35E-07  7.30E-07  9.48E-08  9.28E-09  8.25E-06  3.94E-05
 
 OM13
+        7.27E-06 -1.12E-06 -5.57E-07  1.28E-06 -1.29E-07  4.19E-05  1.80E-05  4.97E-05
 
 OM22
+        2.21E-06  3.31E-07 -4.51E-08 -8.43E-07  2.04E-07  8.32E-07  4.90E-06  2.26E-06  4.39E-05
 
 OM23
+       -5.10E-07 -2.96E-07 -3.73E-08 -6.97E-08  5.45E-08  2.49E-06  1.23E-05  7.45E-06  1.84E-05  2.83E-05
 
 OM33
+       -1.62E-06  3.98E-08  1.94E-07  1.37E-07 -2.22E-08  1.11E-05  9.52E-06  2.49E-05  7.71E-06  2.03E-05  5.44E-05
 
 SG11
+        2.75E-07  4.35E-07  3.72E-07 -8.87E-09  3.07E-08 -7.17E-07 -8.51E-08 -4.57E-08 -9.24E-08 -7.94E-08 -5.71E-08  5.17E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.24E-02
 
 TH 2
+        8.43E-02  1.66E-02
 
 TH 3
+        3.60E-01  3.96E-01  1.75E-02
 
 TH 4
+        9.62E-04 -7.64E-04 -3.57E-04  9.50E-02
 
 TH 5
+        1.27E-02  1.03E-02  8.00E-03 -6.16E-01  6.16E-03
 
 OM11
+        1.51E-01 -4.58E-03 -1.40E-03  3.68E-04  1.74E-03  1.17E-02
 
 OM12
+        3.46E-02  8.97E-03  6.64E-03  1.59E-04  2.40E-04  1.12E-01  6.27E-03
 
 OM13
+        4.59E-02 -9.54E-03 -4.50E-03  1.91E-03 -2.97E-03  5.09E-01  4.06E-01  7.05E-03
 
 OM22
+        1.49E-02  3.01E-03 -3.89E-04 -1.34E-03  5.00E-03  1.08E-02  1.18E-01  4.83E-02  6.62E-03
 
 OM23
+       -4.27E-03 -3.35E-03 -4.00E-04 -1.38E-04  1.66E-03  4.01E-02  3.70E-01  1.99E-01  5.21E-01  5.32E-03
 
 OM33
+       -9.79E-03  3.25E-04  1.50E-03  1.96E-04 -4.90E-04  1.29E-01  2.06E-01  4.79E-01  1.58E-01  5.19E-01  7.37E-03
 
 SG11
+        1.71E-02  3.64E-02  2.95E-02 -1.30E-04  6.91E-03 -8.53E-02 -1.89E-02 -9.02E-03 -1.94E-02 -2.08E-02 -1.08E-02  7.19E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.36E+03
 
 TH 2
+        2.22E+02  4.32E+03
 
 TH 3
+       -1.17E+03 -1.72E+03  4.43E+03
 
 TH 4
+       -6.32E+00 -5.01E+00  4.67E-01  1.79E+02
 
 TH 5
+       -1.42E+02 -1.35E+02  3.17E+00  1.70E+03  4.25E+04
 
 OM11
+       -8.00E+02 -1.09E+02  3.73E+02 -1.31E+00 -6.36E+01  1.06E+04
 
 OM12
+       -3.61E+02 -2.26E+02  1.24E+02  1.98E-01 -4.33E+01  2.65E+03  3.61E+04
 
 OM13
+        3.95E+02  2.44E+02 -1.37E+02  1.71E+00  1.75E+02 -1.11E+04 -1.61E+04  4.29E+04
 
 OM22
+       -2.09E+02 -1.18E+02  1.24E+02 -3.82E+00 -1.70E+02  2.10E+02  3.56E+03 -1.61E+03  3.24E+04
 
 OM23
+        2.62E+02  2.45E+02 -1.26E+02 -1.60E+00  2.89E+01 -1.38E+03 -1.92E+04  1.10E+04 -2.59E+04  7.68E+04
 
 OM33
+        4.99E+01 -1.18E+02 -5.55E+01  5.52E-01 -3.98E+01  2.91E+03  7.22E+03 -1.85E+04  5.15E+03 -2.64E+04  3.42E+04
 
 SG11
+       -1.72E+03 -2.67E+03 -6.03E+02 -9.29E+01 -2.41E+03  1.46E+04  6.97E+03 -1.52E+04  3.25E+03 -1.19E+02  4.33E+03  1.96E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.27E-01  3.83E-01  4.07E-01  5.00E-01  7.64E-01  8.21E-01  9.30E-01  1.02E+00  1.39E+00  1.59E+00  1.62E+00  2.36E+00
 
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
 RAW OUTPUT FILE (FILE): example10l.txt
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
 SAMPLES FOR LOCAL SEARCH KERNEL (PSAMPLE_M2):           2
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
   1   2   3
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
   4   5
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -3000 MCMCOBJ=    5815.69623542980     
 iteration        -2900 MCMCOBJ=    5751.98735085268     
 iteration        -2800 MCMCOBJ=    5779.75946826913     
 iteration        -2700 MCMCOBJ=    5827.78609751398     
 iteration        -2600 MCMCOBJ=    5849.39316476173     
 iteration        -2500 MCMCOBJ=    5827.95466130004     
 iteration        -2400 MCMCOBJ=    5796.61564141730     
 iteration        -2300 MCMCOBJ=    5887.61357863159     
 iteration        -2200 MCMCOBJ=    5738.96437289839     
 iteration        -2100 MCMCOBJ=    5817.55252310703     
 iteration        -2000 MCMCOBJ=    5829.41724890024     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=    5805.93808690987     
 iteration          100 MCMCOBJ=    5692.15778518363     
 iteration          200 MCMCOBJ=    5867.10157502097     
 iteration          300 MCMCOBJ=    5810.94061135192     
 iteration          400 MCMCOBJ=    5687.07190605793     
 iteration          500 MCMCOBJ=    5753.54169581289     
 iteration          600 MCMCOBJ=    5811.88733457621     
 iteration          700 MCMCOBJ=    5921.12809245585     
 iteration          800 MCMCOBJ=    5830.70621257271     
 iteration          900 MCMCOBJ=    5819.63637970556     
 iteration         1000 MCMCOBJ=    5806.85060165622     
 iteration         1100 MCMCOBJ=    5783.29733938879     
 iteration         1200 MCMCOBJ=    5815.68387938894     
 iteration         1300 MCMCOBJ=    5827.96970846285     
 iteration         1400 MCMCOBJ=    5843.51184332497     
 iteration         1500 MCMCOBJ=    5854.19987382512     
 iteration         1600 MCMCOBJ=    5723.79255070364     
 iteration         1700 MCMCOBJ=    5747.44111955274     
 iteration         1800 MCMCOBJ=    5803.77229601216     
 iteration         1900 MCMCOBJ=    5845.96696858048     
 iteration         2000 MCMCOBJ=    5802.67940567218     
 iteration         2100 MCMCOBJ=    5776.41021704135     
 iteration         2200 MCMCOBJ=    5809.52483014069     
 iteration         2300 MCMCOBJ=    5730.58588800940     
 iteration         2400 MCMCOBJ=    5855.76971573771     
 iteration         2500 MCMCOBJ=    5724.52010029299     
 iteration         2600 MCMCOBJ=    5858.86764605423     
 iteration         2700 MCMCOBJ=    5763.57864619736     
 iteration         2800 MCMCOBJ=    5750.86332134567     
 iteration         2900 MCMCOBJ=    5863.25094730859     
 iteration         3000 MCMCOBJ=    5792.60565161634     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5800.06213287476     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       11093.1480841337     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    1587.92578537767     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5800.06213287476     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       7387.98791825243     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    22.3596795730206     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    5800.06213287476     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       5822.42181244778     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   214.55
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     5800.062       **************************************************
 #OBJS:********************************************       49.469 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.12E+00  3.39E+00  2.44E+00 -6.52E-01  8.15E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.43E-02
 
 ETA2
+        4.05E-03  7.58E-02
 
 ETA3
+        3.73E-02  2.96E-02  8.67E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        2.28E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        3.06E-01
 
 ETA2
+        4.73E-02  2.75E-01
 
 ETA3
+        4.13E-01  3.64E-01  2.94E-01
 


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
 
         2.31E-02  1.66E-02  1.75E-02  9.44E-02  6.10E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.19E-02
 
 ETA2
+        6.27E-03  6.73E-03
 
 ETA3
+        7.01E-03  5.34E-03  7.44E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.02E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.93E-02
 
 ETA2
+        7.33E-02  1.22E-02
 
 ETA3
+        6.22E-02  5.23E-02  1.26E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.32E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        5.36E-04
 
 TH 2
+        2.35E-05  2.75E-04
 
 TH 3
+        1.42E-04  1.11E-04  3.05E-04
 
 TH 4
+       -4.37E-05  2.00E-05 -4.18E-05  8.91E-03
 
 TH 5
+        1.01E-06  4.12E-07  2.08E-06 -3.56E-04  3.72E-05
 
 OM11
+        5.06E-05 -4.26E-06  5.60E-07  1.30E-05 -7.40E-07  1.42E-04
 
 OM12
+        9.41E-06  1.39E-06  7.71E-07 -4.21E-06  1.95E-07  8.81E-06  3.93E-05
 
 OM13
+        1.09E-05  1.66E-08  1.73E-06 -2.40E-07 -3.62E-08  4.05E-05  1.83E-05  4.92E-05
 
 OM22
+        2.34E-06 -4.85E-06 -9.40E-07  3.92E-06  6.72E-08  1.45E-06  5.48E-06  2.51E-06  4.52E-05
 
 OM23
+       -2.40E-06 -2.79E-06 -1.12E-06  1.70E-05 -3.13E-07  2.02E-06  1.18E-05  7.49E-06  1.83E-05  2.85E-05
 
 OM33
+       -2.63E-06 -2.76E-06 -8.53E-07  1.17E-05  3.38E-08  9.19E-06  9.18E-06  2.38E-05  8.05E-06  2.08E-05  5.54E-05
 
 SG11
+        7.10E-07  3.39E-07  1.75E-07  2.02E-07 -1.41E-08 -8.22E-07 -1.25E-07 -3.80E-08 -8.54E-08  4.60E-08  9.97E-08  4.93E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.31E-02
 
 TH 2
+        6.13E-02  1.66E-02
 
 TH 3
+        3.51E-01  3.84E-01  1.75E-02
 
 TH 4
+       -2.00E-02  1.28E-02 -2.54E-02  9.44E-02
 
 TH 5
+        7.16E-03  4.07E-03  1.95E-02 -6.19E-01  6.10E-03
 
 OM11
+        1.84E-01 -2.16E-02  2.69E-03  1.16E-02 -1.02E-02  1.19E-02
 
 OM12
+        6.48E-02  1.34E-02  7.03E-03 -7.11E-03  5.09E-03  1.18E-01  6.27E-03
 
 OM13
+        6.71E-02  1.43E-04  1.41E-02 -3.62E-04 -8.46E-04  4.85E-01  4.17E-01  7.01E-03
 
 OM22
+        1.50E-02 -4.34E-02 -8.00E-03  6.18E-03  1.64E-03  1.81E-02  1.30E-01  5.31E-02  6.73E-03
 
 OM23
+       -1.94E-02 -3.14E-02 -1.20E-02  3.36E-02 -9.62E-03  3.18E-02  3.54E-01  2.00E-01  5.09E-01  5.34E-03
 
 OM33
+       -1.53E-02 -2.23E-02 -6.56E-03  1.67E-02  7.44E-04  1.04E-01  1.97E-01  4.56E-01  1.61E-01  5.22E-01  7.44E-03
 
 SG11
+        4.37E-02  2.91E-02  1.42E-02  3.05E-03 -3.29E-03 -9.83E-02 -2.85E-02 -7.72E-03 -1.81E-02  1.23E-02  1.91E-02  7.02E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.26E+03
 
 TH 2
+        2.63E+02  4.31E+03
 
 TH 3
+       -1.14E+03 -1.69E+03  4.42E+03
 
 TH 4
+        7.88E+00 -2.52E+01  1.81E+01  1.83E+02
 
 TH 5
+        6.41E+01 -1.99E+02 -1.89E+01  1.75E+03  4.36E+04
 
 OM11
+       -9.23E+02  4.17E+01  4.37E+02 -1.84E+01 -2.47E+00  1.01E+04
 
 OM12
+       -6.97E+02 -3.33E+02  3.99E+02  3.71E+01  5.07E+01  2.42E+03  3.58E+04
 
 OM13
+        4.86E+02 -3.26E+01 -3.93E+02  2.42E+01  2.52E+02 -1.01E+04 -1.58E+04  4.12E+04
 
 OM22
+       -2.68E+02  3.39E+02  1.94E+01  2.26E+01 -4.94E+01 -8.63E+00  2.30E+03 -8.71E+02  3.07E+04
 
 OM23
+        5.79E+02  2.15E+02 -2.34E+02 -1.15E+02 -3.53E+02 -1.02E+03 -1.72E+04  9.23E+03 -2.36E+04  7.33E+04
 
 OM33
+       -9.09E+00  1.43E+02  3.77E+01 -1.38E+01 -3.81E+02  2.59E+03  6.52E+03 -1.67E+04  4.37E+03 -2.49E+04  3.24E+04
 
 SG11
+       -4.81E+03 -2.75E+03  2.05E+03 -2.72E+01  7.13E+02  1.73E+04  1.37E+04 -1.58E+04  7.28E+03 -1.21E+04  1.10E+03  2.07E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           EIGENVALUES OF COR MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.42E-01  3.79E-01  4.15E-01  5.02E-01  7.51E-01  8.13E-01  9.75E-01  1.02E+00  1.35E+00  1.58E+00  1.64E+00  2.33E+00
 
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
 RAW OUTPUT FILE (FILE): example10l.ext
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   10036.0868468299        NO. OF FUNC. EVALS.:  13
 CUMULATIVE NO. OF FUNC. EVALS.:       13
 NPARAMETR:  1.1223E+00  3.3883E+00  2.4360E+00 -6.5247E-01  8.1545E-02  9.4298E-02  4.0451E-03  3.7325E-02  7.5817E-02  2.9562E-02
             8.6658E-02  2.2829E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:   3.1737E+01  4.8796E+02  2.5561E+02  6.4593E+00  4.0724E+01  2.5427E+01  1.1877E+00 -5.0378E+01  1.0244E+01 -7.1746E+00
             2.0221E+01  2.1242E+01
 
0ITERATION NO.:    1    OBJECTIVE VALUE:   10036.0868468299        NO. OF FUNC. EVALS.:  33
 CUMULATIVE NO. OF FUNC. EVALS.:       46
 NPARAMETR:  1.1223E+00  3.3883E+00  2.4360E+00 -6.5247E-01  8.1545E-02  9.4298E-02  4.0451E-03  3.7325E-02  7.5817E-02  2.9562E-02
             8.6658E-02  2.2829E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:   2.0950E+00  1.6533E+00  2.2593E-01  5.6573E+00  3.7749E+01  2.5334E+01  1.0049E+00 -5.0855E+01  1.0121E+01 -7.5338E+00
             2.0281E+01  2.0820E+01
 
0ITERATION NO.:    2    OBJECTIVE VALUE:   10035.9805944363        NO. OF FUNC. EVALS.:  29
 CUMULATIVE NO. OF FUNC. EVALS.:       75
 NPARAMETR:  1.1210E+00  3.3854E+00  2.4357E+00 -6.5439E-01  7.9945E-02  9.4050E-02  4.0377E-03  3.8261E-02  7.5738E-02  2.9698E-02
             8.7400E-02  2.2779E-02
 PARAMETER:  9.9891E-02  9.9914E-02  9.9988E-02 -1.0029E-01  9.8038E-02  9.8684E-02  9.9948E-02  1.0264E-01  9.9474E-02  1.0039E-01
             9.8946E-02  9.8918E-02
 GRADIENT:  -4.8306E+01 -7.8386E+02  2.3225E+02 -3.4007E+01 -7.7396E+01  2.3472E+01  1.0722E-01 -2.7249E+01  9.2828E+00 -4.7153E+00
             2.1275E+01  1.2019E+01
 
0ITERATION NO.:    3    OBJECTIVE VALUE:   10035.9755283701        NO. OF FUNC. EVALS.:  29
 CUMULATIVE NO. OF FUNC. EVALS.:      104
 NPARAMETR:  1.1208E+00  3.3889E+00  2.4347E+00 -6.5491E-01  7.9492E-02  9.3978E-02  4.0355E-03  3.8533E-02  7.5714E-02  2.9738E-02
             8.7620E-02  2.2765E-02
 PARAMETER:  9.9867E-02  1.0002E-01  9.9947E-02 -1.0037E-01  9.7483E-02  9.8298E-02  9.9933E-02  1.0341E-01  9.9320E-02  1.0051E-01
             9.8637E-02  9.8603E-02
 GRADIENT:  -1.4683E+01  3.3527E+02 -2.4631E+02 -4.5657E+01 -1.1194E+02  2.2708E+01 -1.9484E-01 -1.9815E+01  8.9906E+00 -3.5604E+00
             2.1451E+01  8.4181E+00
 
0ITERATION NO.:    4    OBJECTIVE VALUE:   10035.9731786763        NO. OF FUNC. EVALS.:  31
 CUMULATIVE NO. OF FUNC. EVALS.:      135
 NPARAMETR:  1.1210E+00  3.3900E+00  2.4367E+00 -6.5476E-01  7.9471E-02  9.3958E-02  4.0350E-03  3.8598E-02  7.5708E-02  2.9748E-02
             8.7672E-02  2.2761E-02
 PARAMETER:  9.9886E-02  1.0005E-01  1.0003E-01 -1.0035E-01  9.7457E-02  9.8195E-02  9.9929E-02  1.0360E-01  9.9279E-02  1.0053E-01
             9.8552E-02  9.8527E-02
 GRADIENT:  -4.8911E+01  4.3128E+02  7.1189E+01 -4.5863E+01 -1.1329E+02  2.2706E+01 -2.6537E-01 -1.8121E+01  8.9274E+00 -3.4339E+00
             2.1458E+01  7.2987E+00
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   10035.8956653235        NO. OF FUNC. EVALS.:  29
 CUMULATIVE NO. OF FUNC. EVALS.:      164
 NPARAMETR:  1.1258E+00  3.3899E+00  2.4376E+00 -6.5204E-01  7.9664E-02  9.3745E-02  4.0291E-03  3.9276E-02  7.5639E-02  2.9848E-02
             8.8216E-02  2.2725E-02
 PARAMETER:  1.0032E-01  1.0005E-01  1.0006E-01 -9.9934E-02  9.7693E-02  9.7057E-02  9.9899E-02  1.0554E-01  9.8826E-02  1.0083E-01
             9.7597E-02  9.7728E-02
 GRADIENT:   1.9671E+02  3.9367E+02 -2.4994E+01 -3.5436E+01 -9.2470E+01  1.9772E+01 -1.0807E+00  1.1008E+00  7.9302E+00 -2.4257E-01
             2.1965E+01 -1.1979E+00
 
0ITERATION NO.:    6    OBJECTIVE VALUE:   10035.7696189359        NO. OF FUNC. EVALS.:  29
 CUMULATIVE NO. OF FUNC. EVALS.:      193
 NPARAMETR:  1.1208E+00  3.3895E+00  2.4361E+00 -6.4677E-01  8.0213E-02  9.3373E-02  4.0193E-03  4.0399E-02  7.5520E-02  3.0015E-02
             8.9136E-02  2.2665E-02
 PARAMETER:  9.9872E-02  1.0003E-01  1.0000E-01 -9.9127E-02  9.8366E-02  9.5069E-02  9.9853E-02  1.0877E-01  9.8033E-02  1.0133E-01
             9.5905E-02  9.6398E-02
 GRADIENT:  -1.3904E+01  3.9598E+02 -4.4349E+01 -1.0242E+01 -3.6906E+01  1.7857E+01 -2.1224E+00  2.9791E+01  6.7317E+00  3.1687E+00
             2.2458E+01 -1.3092E+01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:   10035.1930489430        NO. OF FUNC. EVALS.:  27
 CUMULATIVE NO. OF FUNC. EVALS.:      220
 NPARAMETR:  1.1195E+00  3.3894E+00  2.4361E+00 -6.4124E-01  7.9834E-02  8.8090E-02  3.9609E-03  3.8494E-02  7.3845E-02  2.9919E-02
             8.4991E-02  2.2730E-02
 PARAMETER:  9.9751E-02  1.0003E-01  1.0000E-01 -9.8279E-02  9.7903E-02  6.5950E-02  1.0131E-01  1.0670E-01  8.6761E-02  1.0215E-01
             6.4969E-02  9.7841E-02
 GRADIENT:  -1.3383E+01  4.0993E+02 -4.1683E+01 -5.6623E+00 -4.7464E+01  3.8867E+00  1.8886E-01  8.4485E-01 -7.0825E+00  2.3825E+01
            -7.3933E+00 -9.1496E+00
 
0ITERATION NO.:    8    OBJECTIVE VALUE:   10035.1900982067        NO. OF FUNC. EVALS.:  28
 CUMULATIVE NO. OF FUNC. EVALS.:      248
 NPARAMETR:  1.1194E+00  3.3895E+00  2.4361E+00 -6.5412E-01  8.0434E-02  8.7223E-02  3.9449E-03  3.8723E-02  7.3839E-02  2.8586E-02
             8.4059E-02  2.2798E-02
 PARAMETER:  9.9744E-02  1.0003E-01  1.0000E-01 -1.0025E-01  9.8638E-02  6.1003E-02  1.0140E-01  1.0787E-01  8.6719E-02  9.7255E-02
             6.2426E-02  9.9324E-02
 GRADIENT:  -1.5155E+01  4.0349E+02 -3.5679E+01 -2.2435E+01 -4.1357E+01  9.5667E-01 -1.1124E+00  1.3353E+01 -2.4105E+00 -2.0098E+01
            -8.3766E+00  4.6618E-01
 
0ITERATION NO.:    9    OBJECTIVE VALUE:   10035.1563360797        NO. OF FUNC. EVALS.:  27
 CUMULATIVE NO. OF FUNC. EVALS.:      275
 NPARAMETR:  1.1189E+00  3.3895E+00  2.4361E+00 -6.5730E-01  8.0587E-02  8.5641E-02  3.9301E-03  3.8209E-02  7.4304E-02  2.8994E-02
             8.4624E-02  2.2847E-02
 PARAMETER:  9.9704E-02  1.0004E-01  1.0000E-01 -1.0074E-01  9.8826E-02  5.1853E-02  1.0195E-01  1.0742E-01  8.9849E-02  9.8415E-02
             6.6556E-02  1.0040E-01
 GRADIENT:  -1.7142E+01  4.0127E+02 -3.5077E+01 -2.6500E+01 -3.9761E+01 -3.6386E+00 -8.7115E-01  1.5709E+01  1.0155E-01 -1.2529E+01
            -4.5035E+00  7.8062E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   10035.1499798321        NO. OF FUNC. EVALS.:  29
 CUMULATIVE NO. OF FUNC. EVALS.:      304
 NPARAMETR:  1.1189E+00  3.3895E+00  2.4361E+00 -6.5388E-01  8.0426E-02  8.5357E-02  3.9280E-03  3.7969E-02  7.4386E-02  2.8895E-02
             8.4479E-02  2.2812E-02
 PARAMETER:  9.9696E-02  1.0004E-01  1.0000E-01 -1.0022E-01  9.8627E-02  5.0189E-02  1.0207E-01  1.0692E-01  9.0398E-02  9.8028E-02
             6.7356E-02  9.9634E-02
 GRADIENT:  -1.7726E+01  3.9869E+02 -3.3914E+01 -2.2077E+01 -4.1447E+01 -4.5035E+00 -8.0587E-01  1.2696E+01  1.0374E+00 -1.6412E+01
            -4.1396E+00  1.5252E+00
 
0ITERATION NO.:   11    OBJECTIVE VALUE:   10035.1496710569        NO. OF FUNC. EVALS.:  29
 CUMULATIVE NO. OF FUNC. EVALS.:      333
 NPARAMETR:  1.1188E+00  3.3895E+00  2.4361E+00 -6.5485E-01  8.0471E-02  8.5336E-02  3.9305E-03  3.7944E-02  7.4318E-02  2.8877E-02
             8.4487E-02  2.2807E-02
 PARAMETER:  9.9695E-02  1.0004E-01  1.0000E-01 -1.0037E-01  9.8683E-02  5.0068E-02  1.0214E-01  1.0686E-01  8.9940E-02  9.8005E-02
             6.7618E-02  9.9521E-02
 GRADIENT:  -1.7746E+01  3.9918E+02 -3.4190E+01 -2.3322E+01 -4.0940E+01 -4.5805E+00 -7.7062E-01  1.2422E+01  5.3158E-01 -1.6191E+01
            -3.9438E+00  6.1385E-01
 
0ITERATION NO.:   12    OBJECTIVE VALUE:   10035.1493851532        NO. OF FUNC. EVALS.:  29
 CUMULATIVE NO. OF FUNC. EVALS.:      362
 NPARAMETR:  1.1188E+00  3.3895E+00  2.4361E+00 -6.5400E-01  8.0432E-02  8.5318E-02  3.9334E-03  3.7941E-02  7.4247E-02  2.8843E-02
             8.4503E-02  2.2812E-02
 PARAMETER:  9.9695E-02  1.0004E-01  1.0000E-01 -1.0024E-01  9.8636E-02  4.9963E-02  1.0223E-01  1.0687E-01  8.9462E-02  9.7924E-02
             6.7894E-02  9.9634E-02
 GRADIENT:  -1.7735E+01  3.9909E+02 -3.4322E+01 -2.2207E+01 -4.1290E+01 -4.6137E+00 -7.7025E-01  1.2649E+01  7.0727E-02 -1.6448E+01
            -3.6701E+00  1.4935E+00
 
0ITERATION NO.:   13    OBJECTIVE VALUE:   10035.1296843459        NO. OF FUNC. EVALS.:  26
 CUMULATIVE NO. OF FUNC. EVALS.:      388
 NPARAMETR:  1.1191E+00  3.3892E+00  2.4361E+00 -6.5260E-01  8.0493E-02  8.5716E-02  4.5709E-03  3.8199E-02  7.4415E-02  2.9179E-02
             8.4775E-02  2.2805E-02
 PARAMETER:  9.9717E-02  1.0003E-01  1.0000E-01 -1.0002E-01  9.8710E-02  5.2290E-02  1.1852E-01  1.0734E-01  9.0174E-02  9.8029E-02
             6.8755E-02  9.9480E-02
 GRADIENT:  -1.3109E+01  3.1738E+02 -2.7808E+01 -1.7572E+01 -3.3145E+01 -3.6174E+00  6.8714E-01  1.0166E+01  2.9060E-01 -1.3310E+01
            -3.0617E+00  1.1436E+00
 
0ITERATION NO.:   14    OBJECTIVE VALUE:   10035.0761076830        NO. OF FUNC. EVALS.:  27
 CUMULATIVE NO. OF FUNC. EVALS.:      415
 NPARAMETR:  1.1194E+00  3.3885E+00  2.4359E+00 -6.4968E-01  8.0674E-02  8.6332E-02  4.1862E-03  3.7927E-02  7.4454E-02  2.9327E-02
             8.4977E-02  2.2792E-02
 PARAMETER:  9.9745E-02  1.0000E-01  9.9995E-02 -9.9572E-02  9.8932E-02  5.5871E-02  1.0816E-01  1.0620E-01  9.0711E-02  9.9226E-02
             7.1512E-02  9.9195E-02
 GRADIENT:  -3.9374E+00  1.1437E+02 -1.0358E+01 -6.6837E+00 -1.2361E+01 -1.3111E+00  2.1659E-01  3.4860E+00  9.6784E-02 -4.9939E+00
            -1.1530E+00  4.1555E-01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:   10035.0680056770        NO. OF FUNC. EVALS.:  26
 CUMULATIVE NO. OF FUNC. EVALS.:      441
 NPARAMETR:  1.1196E+00  3.3880E+00  2.4358E+00 -6.4779E-01  8.0776E-02  8.6693E-02  3.9946E-03  3.7796E-02  7.4488E-02  2.9434E-02
             8.5126E-02  2.2784E-02
 PARAMETER:  9.9758E-02  9.9992E-02  9.9991E-02 -9.9284E-02  9.9057E-02  5.7953E-02  1.0299E-01  1.0561E-01  9.1065E-02  9.9929E-02
             7.3128E-02  9.9031E-02
 GRADIENT:  -3.6372E-01 -6.2428E-02  6.5315E-01 -9.0785E-03 -5.8195E-02 -1.1548E-03 -1.2335E-04  5.2897E-02 -2.2185E-03 -5.0433E-02
             9.5329E-03 -2.7165E-02
 
0ITERATION NO.:   16    OBJECTIVE VALUE:   10035.0680056770        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      441
 NPARAMETR:  1.1196E+00  3.3880E+00  2.4358E+00 -6.4779E-01  8.0776E-02  8.6693E-02  3.9946E-03  3.7796E-02  7.4488E-02  2.9434E-02
             8.5126E-02  2.2784E-02
 PARAMETER:  9.9758E-02  9.9992E-02  9.9991E-02 -9.9284E-02  9.9057E-02  5.7953E-02  1.0299E-01  1.0561E-01  9.1065E-02  9.9929E-02
             7.3128E-02  9.9031E-02
 GRADIENT:  -3.6372E-01 -6.2428E-02  6.5315E-01 -9.0785E-03 -5.8195E-02 -1.1548E-03 -1.2335E-04  5.2897E-02 -2.2185E-03 -5.0433E-02
             9.5329E-03 -2.7165E-02
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      441
 NO. OF SIG. DIGITS IN FINAL EST.:  3.4

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.9143E-02  1.4796E-03  4.4648E-03
 SE:             1.4524E-02  1.5630E-02  1.6920E-02
 N:                     288         288         288
 
 P VAL.:         4.4799E-02  9.2458E-01  7.9188E-01
 
 ETASHRINKSD(%)  1.6141E+01  2.6415E+00  1.4116E+00
 ETASHRINKVR(%)  2.9677E+01  5.2133E+00  2.8033E+00
 EBVSHRINKSD(%)  1.6832E+01  2.8233E+00  1.6302E+00
 EBVSHRINKVR(%)  3.0831E+01  5.5668E+00  3.2338E+00
 EPSSHRINKSD(%)  1.2987E+01
 EPSSHRINKVR(%)  2.4287E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2880
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    5293.08595125891     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:    10035.0680056770     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:       15328.1539569360     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           864
  
 #TERE:
 Elapsed estimation  time in seconds:    52.72
 Elapsed covariance  time in seconds:    34.66
 Elapsed postprocess time in seconds:     0.29
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    10035.068       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.12E+00  3.39E+00  2.44E+00 -6.48E-01  8.08E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        8.67E-02
 
 ETA2
+        3.99E-03  7.45E-02
 
 ETA3
+        3.78E-02  2.94E-02  8.51E-02
 


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
+        4.97E-02  2.73E-01
 
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
 
         2.18E-02  1.66E-02  1.75E-02  9.50E-02  6.17E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.08E-02
 
 ETA2
+        6.05E-03  6.61E-03
 
 ETA3
+        6.88E-03  5.31E-03  7.33E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.22E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.84E-02
 
 ETA2
+        7.48E-02  1.21E-02
 
 ETA3
+        6.22E-02  5.30E-02  1.26E-02
 


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
+        4.77E-04
 
 TH 2
+        3.19E-05  2.75E-04
 
 TH 3
+        1.41E-04  1.15E-04  3.06E-04
 
 TH 4
+        1.50E-06 -1.36E-06 -8.49E-07  9.02E-03
 
 TH 5
+        1.75E-06  1.06E-06  8.82E-07 -3.61E-04  3.81E-05
 
 OM11
+        3.22E-05 -1.14E-06 -5.99E-07 -1.06E-06  1.10E-07  1.17E-04
 
 OM12
+        4.69E-06  7.20E-07  5.63E-07  1.41E-06 -5.48E-09  7.86E-06  3.67E-05
 
 OM13
+        7.17E-06 -1.22E-06 -7.48E-07  1.78E-06 -1.39E-07  3.95E-05  1.70E-05  4.74E-05
 
 OM22
+        2.22E-06  3.56E-07  5.54E-09 -9.85E-07  1.88E-07  7.36E-07  5.46E-06  2.50E-06  4.37E-05
 
 OM23
+       -5.59E-07 -3.06E-07 -5.34E-08  7.22E-08  4.74E-08  2.42E-06  1.23E-05  7.56E-06  1.84E-05  2.82E-05
 
 OM33
+       -1.81E-06  8.43E-09  1.43E-07 -9.98E-08 -1.82E-08  1.05E-05  8.92E-06  2.43E-05  7.72E-06  2.01E-05  5.37E-05
 
 SG11
+        2.85E-07  4.41E-07  3.78E-07 -2.59E-08  3.29E-08 -7.83E-07 -8.47E-08 -3.66E-08 -9.17E-08 -7.56E-08 -6.27E-08  5.21E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.18E-02
 
 TH 2
+        8.80E-02  1.66E-02
 
 TH 3
+        3.69E-01  3.96E-01  1.75E-02
 
 TH 4
+        7.23E-04 -8.63E-04 -5.11E-04  9.50E-02
 
 TH 5
+        1.30E-02  1.04E-02  8.16E-03 -6.16E-01  6.17E-03
 
 OM11
+        1.36E-01 -6.37E-03 -3.17E-03 -1.03E-03  1.65E-03  1.08E-02
 
 OM12
+        3.55E-02  7.16E-03  5.32E-03  2.45E-03 -1.47E-04  1.20E-01  6.05E-03
 
 OM13
+        4.77E-02 -1.07E-02 -6.21E-03  2.72E-03 -3.28E-03  5.31E-01  4.07E-01  6.88E-03
 
 OM22
+        1.54E-02  3.24E-03  4.79E-05 -1.57E-03  4.59E-03  1.03E-02  1.36E-01  5.49E-02  6.61E-03
 
 OM23
+       -4.82E-03 -3.48E-03 -5.75E-04  1.43E-04  1.45E-03  4.21E-02  3.83E-01  2.07E-01  5.24E-01  5.31E-03
 
 OM33
+       -1.13E-02  6.93E-05  1.12E-03 -1.43E-04 -4.03E-04  1.33E-01  2.01E-01  4.82E-01  1.59E-01  5.17E-01  7.33E-03
 
 SG11
+        1.81E-02  3.68E-02  2.99E-02 -3.77E-04  7.39E-03 -1.00E-01 -1.94E-02 -7.36E-03 -1.92E-02 -1.97E-02 -1.19E-02  7.22E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11  

 
 TH 1
+        2.50E+03
 
 TH 2
+        2.29E+02  4.33E+03
 
 TH 3
+       -1.24E+03 -1.73E+03  4.48E+03
 
 TH 4
+       -6.66E+00 -5.03E+00  6.80E-01  1.79E+02
 
 TH 5
+       -1.50E+02 -1.36E+02  6.48E+00  1.70E+03  4.24E+04
 
 OM11
+       -8.08E+02 -1.10E+02  3.74E+02  1.92E+00 -5.06E+01  1.29E+04
 
 OM12
+       -3.72E+02 -2.19E+02  1.26E+02 -5.31E+00 -9.30E+01  3.18E+03  3.93E+04
 
 OM13
+        3.39E+02  2.46E+02 -9.06E+01 -1.65E+00  1.74E+02 -1.34E+04 -1.77E+04  4.69E+04
 
 OM22
+       -2.20E+02 -1.18E+02  1.24E+02 -2.08E+00 -1.44E+02  2.74E+02  3.36E+03 -1.62E+03  3.26E+04
 
 OM23
+        2.71E+02  2.42E+02 -1.24E+02 -1.77E+00  3.02E+01 -1.58E+03 -2.07E+04  1.14E+04 -2.60E+04  7.76E+04
 
 OM33
+        8.25E+01 -1.19E+02 -7.86E+01  2.79E+00 -3.47E+01  3.53E+03  8.13E+03 -1.97E+04  5.17E+03 -2.67E+04  3.48E+04
 
 SG11
+       -1.90E+03 -2.72E+03 -5.42E+02 -8.93E+01 -2.51E+03  1.95E+04  8.80E+03 -2.10E+04  3.57E+03 -1.74E+03  6.58E+03  1.95E+06
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)         ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12

 
         2.21E-01  3.83E-01  3.96E-01  4.95E-01  7.66E-01  8.14E-01  9.21E-01  1.01E+00  1.39E+00  1.59E+00  1.62E+00  2.38E+00
 
 Elapsed finaloutput time in seconds:     0.75
 #CPUT: Total CPU Time in Seconds,      692.379
Stop Time: 
Sat 04/22/2017 
11:07 AM

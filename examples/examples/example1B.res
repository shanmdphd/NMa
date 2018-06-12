Sat 09/07/2013 
05:44 AM
;Model Desc: Two compartment Model, Using ADVAN3, TRANS4
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# Example 1 (from samp5l)
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
$DATA example1b.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4

;NTHETA=number of Thetas to be estimated
;NETA=number of Etas to be estimated (and to be described by NETAxBETA OMEGA matrix)
;NTHP=number of thetas which have a prior
;NETP=number of Omegas with prior
;Prior information is important for MCMC Bayesian analysis, not necessary for maximization
; methods
$PRIOR NWPRI NTHETA=4, NETA=4, NTHP=4, NETP=4

$PK
; The thetas are MU modeled.  Best that there is a linear relationship between THETAs and Mus
;  The linear MU modeling of THETAS allows them to be efficiently Gibbs sampled.
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
$THETA 
(0.001, 2.0) ;[LN(CL)]
(0.001, 2.0) ;[LN(V1)]
(0.001, 2.0) ;[LN(Q)]
(0.001, 2.0) ;[LN(V2)]
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

; Prior information of THETAS
$THETA (2.0 FIX) (2.0 FIX) (2.0 FIX) (2.0 FIX)

; Variance to prior information of THETAS.  Because variances are very large, this
; means that the prior information to the THETAS is highly uninformative.
$OMEGA BLOCK(4)
10000 FIX 
0.00 10000
0.00  0.00 10000
0.00  0.00 0.0 10000

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

; The first analysis is iterative two-stage, maximum of 500 iterations (NITER), iteration results
; are printed every 5 iterations, gradient precision (SIGL) is 4. Termination is tested on all of
; the population parameters (CITER=3), and for less then 2 significant digits change (NSIG).
; Prior information is not necessary for ITS, so NOPRIOR=1.  The intermediate and final results
; of the ITS method will be recoded in row/column format in example1B.ext
$EST METHOD=ITS INTERACTION FILE=EXAMPLE1B.ext NITER=500 PRINT=5 NOABORT SIGL=4 CTYPE=3 CITER=5   
     CALPHA=0.05 NOPRIOR=1 NSIG=2
; The results of ITS are used as the initial values for the SAEM method.  A maximum of 3000
; stochastic iterations (NBURN) is requested, but may end early if statistical test determines
; that variations in all parameters is stationary (note that any settings from the previous $EST
; carries over to the next $EST statement, within a $PROB).  The SAEM is a Monte Carlo process,
; so setting the SEED assures repeatability of results.  Each iteration obtains only 2 Monte
; Carlo samples ISAMPLE), so they are very fast.  But many iterations are needed, so PRINT only
; every 100th iteration.  After the stochastic phase, 500 accumulation iterations will be
; Performed (NITER), to obtain good parameters estimates with little stochastic noise.
; As a new FILE has not been given, the SAEM results will append to EXAMPLE1B.ext.
$EST METHOD=SAEM INTERACTION NBURN=3000 NITER=500 PRINT=100 SEED=1556678 ISAMPLE=2
; After the SAEM method, obtain good estimates of the marginal density (objective function),
; along with good estimates of the standard errors.  This is best done with importance sampling
; (IMP), performing the expectation step only (EONLY=1), so that final population parameters
; remain at the final SAEM result.  Five iterations (NITER) should allow the importance sampling
; proposal density to become stationary.  This is observed by the objective function settling 
; to a particular value (with some stochastic noise).  By using 3000 Monte Carlo samples
; (ISAMPLE), this assures a precise assessment of standard errors.
$EST METHOD=IMP  INTERACTION EONLY=1 NITER=5 ISAMPLE=3000 PRINT=1 SIGL=8 NOPRIOR=1
; The Bayesian analysis is performed.  While 10000 burn-in
; iterations are requested as a maximum, because the termination test is on (CTYPE<>0, set at the
; first $EST statement), and because the initial parameters are at the SAEM result, which is the
; maximum likelihood position, the analysis should settle down to a stationary distribution in
; several hundred iterations.  Prior information is also used to facilitate Bayesian analysis.
; The individual Bayesian iteration results are important, and may be need for post-processing
; analysis. So specify a separate FILE for the Bayesian analysis. 
$EST METHOD=BAYES INTERACTION FILE=EXAMPLE1B.txt NBURN=10000 NITER=10000 PRINT=100 NOPRIOR=0
; Just for old-times sake, let?s see what the traditional FOCE method will give us.  
; And, remember to introduce a new FILE, so its results won?t append to our Bayesian FILE. 
; Appending to EXAMPLE1B.ext with the EM methods is fine.
$EST METHOD=COND INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 PRINT=5 NOABORT NOPRIOR=1
     FILE=EXAMPLE1B.ext
; Time for the standard error results.  You may request a more precise gradient precision (SIGL)
; that differed from that used during estimation.
$COV MATRIX=R PRINT=E UNCONDITIONAL SIGL=12
; Print out results in tables. Include some of the new weighted residual types
$TABLE ID TIME PRED RES WRES CPRED CWRES EPRED ERES EWRES NOAPPEND ONEHEADER 
 FILE=EXAMPLE1B.TAB NOPRINT
$TABLE ID CL V1 Q V2 FIRSTONLY NOAPPEND NOPRINT FILE=EXAMPLE1B.PAR
$TABLE ID ETA1 ETA2 ETA3 ETA4 FIRSTONLY NOAPPEND NOPRINT FILE=EXAMPLE1B.ETA

  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (DATA WARNING   5) RECORD        20, DATA ITEM   6, CONTENTS: 11.815
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD        21, DATA ITEM   6, CONTENTS: 8.3643
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD        22, DATA ITEM   6, CONTENTS: 1.0929
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD        23, DATA ITEM   6, CONTENTS: 0.36815
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD        24, DATA ITEM   6, CONTENTS: 0.00306
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       140, DATA ITEM   6, CONTENTS: 11.843
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       141, DATA ITEM   6, CONTENTS: 6.9809
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       142, DATA ITEM   6, CONTENTS: 10.781
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       143, DATA ITEM   6, CONTENTS: 1.3893
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       144, DATA ITEM   6, CONTENTS: 0.11429
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       230, DATA ITEM   6, CONTENTS: 22.552
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       231, DATA ITEM   6, CONTENTS: 16.344
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       232, DATA ITEM   6, CONTENTS: 7.134
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       233, DATA ITEM   6, CONTENTS: 1.4253
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       234, DATA ITEM   6, CONTENTS: 0.067626
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       266, DATA ITEM   6, CONTENTS: 21.909
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       267, DATA ITEM   6, CONTENTS: 15.748
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       268, DATA ITEM   6, CONTENTS: 10.151
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       269, DATA ITEM   6, CONTENTS: 0.5087
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1

 (DATA WARNING   5) RECORD       270, DATA ITEM   6, CONTENTS: 0.17514
 THE DV DATA ITEM IS POSITIVE, BUT THE MDV DATA ITEM IS 1*

 * THE MAXIMUM NUMBER OF WARNINGS OF ONE OR MORE TYPES WAS REACHED.
 IT IS POSSIBLE THAT SOME WARNING MESSAGES WERE SUPPRESSED.
  
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
 NO. OF DATA RECS IN DATA SET:      600
 NO. OF DATA ITEMS IN DATA SET:  17
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   5   7   8   0   0  11   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID JID TIME CONC DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 CL V1 Q V2
0FORMAT FOR DATA:
 (2E2.0,3E4.0,E11.0,E4.0,4E2.0,2E7.0,E8.0,E7.0,E2.0,E5.0)

 TOT. NO. OF OBS RECS:      450
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
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.4000E+01     0.4000E+01     0.4000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.1500E+00
                  0.1000E-01   0.1500E+00
                  0.1000E-01   0.1000E-01   0.1500E+00
                  0.1000E-01   0.1000E-01   0.1000E-01   0.1500E+00
        2                                                                                  YES
                  0.1000E+05
                  0.0000E+00   0.1000E+05
                  0.0000E+00   0.0000E+00   0.1000E+05
                  0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+05
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
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                12
 RELATIVE TOLERANCE (TOL):                  -1
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY (ATOL): -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 RESUME COV ANALYSIS (RESUME):               NO
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           3
 SEED NUMBER (SEED):    11456
 RANMETHOD:
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE:            EIGENVALUE
0-- TABLE   1 --
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID TIME PRED RES WRES CPRED CWRES EPRED ERES EWRES
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID CL V1 Q V2
0-- TABLE   3 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID ETA1 ETA2 ETA3 ETA4
0
 PRIOR SUBROUTINE USER-SUPPLIED
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    4           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   4           
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
 CONVERGENCE ITERATIONS (CITER):          5           
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
   1   2   3   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -311.509948127436
 iteration            5 OBJ=  -1120.33881880970
 iteration           10 OBJ=  -1125.46333698686
 iteration           15 OBJ=  -1125.46901007004
 iteration           20 OBJ=  -1125.43842703242
 iteration           25 OBJ=  -1125.40865569762
 iteration           30 OBJ=  -1125.38708136066
 iteration           35 OBJ=  -1125.37273880922
 iteration           40 OBJ=  -1125.36321791659
 iteration           45 OBJ=  -1125.35678323897
 iteration           50 OBJ=  -1125.35208886145
 iteration           55 OBJ=  -1125.34933613422
 iteration           60 OBJ=  -1125.34713263847
 iteration           65 OBJ=  -1125.34580163946
 iteration           70 OBJ=  -1125.34517765635
 iteration           75 OBJ=  -1125.34450077126
 iteration           80 OBJ=  -1125.34416108152
 iteration           85 OBJ=  -1125.34388056353
 iteration           90 OBJ=  -1125.34363333340
 iteration           95 OBJ=  -1125.34349145994
 iteration          100 OBJ=  -1125.34329268608
 iteration          105 OBJ=  -1125.34322639062
 iteration          110 OBJ=  -1125.34316655538
 iteration          115 OBJ=  -1125.34315803223
 iteration          120 OBJ=  -1125.34323451310
 iteration          125 OBJ=  -1125.34321167782
 iteration          130 OBJ=  -1125.34311441912
 Convergence achieved
 iteration          130 OBJ=  -1125.34310369177
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         2.5013E-06  3.0003E-07  3.0823E-06  1.0971E-06
 SE:             4.1669E-02  2.9743E-02  3.4759E-02  3.5633E-02
 N:                      90          90          90          90
 
 P VAL.:         9.9995E-01  9.9999E-01  9.9993E-01  9.9998E-01
 
 ETAshrink(%):   3.4304E+00  2.1115E+01  2.3713E+01  1.4469E+01
 EBVshrink(%):   3.4303E+00  2.1115E+01  2.3714E+01  1.4469E+01
 EPSshrink(%):   3.0763E+01
 
 #TERE:
 Elapsed estimation time in seconds:    22.19
 Elapsed covariance time in seconds:     0.28
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1125.343       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.70E+00  1.58E+00  8.36E-01  2.39E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.69E-01
 
 ETA2
+        9.90E-03  1.29E-01
 
 ETA3
+        6.13E-03  3.56E-02  1.89E-01
 
 ETA4
+       -1.43E-02  2.48E-02  4.98E-02  1.58E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.84E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.12E-01
 
 ETA2
+        6.69E-02  3.60E-01
 
 ETA3
+        3.43E-02  2.28E-01  4.35E-01
 
 ETA4
+       -8.72E-02  1.73E-01  2.88E-01  3.97E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.42E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.88E-02  5.22E-02  6.66E-02  5.73E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.12E-02
 
 ETA2
+        2.54E-02  3.83E-02
 
 ETA3
+        3.39E-02  4.14E-02  6.63E-02
 
 ETA4
+        2.93E-02  3.09E-02  5.00E-02  4.25E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        8.76E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.79E-02
 
 ETA2
+        1.66E-01  5.33E-02
 
 ETA3
+        1.87E-01  2.44E-01  7.62E-02
 
 ETA4
+        1.84E-01  1.97E-01  2.22E-01  5.35E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.81E-02
 
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
+        2.38E-03
 
 TH 2
+        5.25E-04  2.73E-03
 
 TH 3
+        3.72E-04  4.94E-04  4.43E-03
 
 TH 4
+        3.89E-05  3.89E-04  1.94E-03  3.28E-03
 
 OM11
+       -2.17E-04  8.20E-05  1.29E-04 -8.47E-06  9.74E-04
 
 OM12
+        1.46E-05  1.32E-04  1.15E-04 -1.12E-04  3.90E-04  6.44E-04
 
 OM13
+       -1.33E-04  4.21E-05 -8.35E-05  1.78E-04  1.58E-04  3.76E-04  1.15E-03
 
 OM14
+       -1.66E-04 -1.90E-04  1.82E-04  1.89E-04  2.75E-04  2.80E-04  6.18E-04  8.59E-04
 
 OM22
+       -1.31E-04  3.80E-05 -2.04E-04 -8.75E-05  2.19E-04  2.70E-04  9.92E-05  2.03E-04  1.47E-03
 
 OM23
+       -1.25E-04 -2.15E-04  7.15E-05  3.26E-04  2.95E-04  9.21E-05 -1.59E-05  3.39E-05  6.50E-04  1.72E-03
 
 OM24
+       -2.96E-04 -2.01E-04  2.66E-04  2.34E-04  2.04E-04  1.47E-04  2.65E-05  1.51E-04  6.19E-04  8.72E-04  9.57E-04
 
 OM33
+       -3.33E-04 -2.65E-04  3.94E-04  7.82E-04  2.73E-04  2.08E-04  8.47E-04  4.66E-04  7.24E-04  8.86E-04  6.72E-04  4.39E-03
 
 OM34
+       -1.07E-04  1.86E-05  5.85E-04  6.30E-04  3.13E-04  1.32E-04  3.38E-04  3.01E-04  5.95E-04  7.30E-04  6.24E-04  2.60E-03
          2.50E-03
 
 OM44
+       -4.99E-05  3.55E-05  4.20E-04 -3.61E-05  2.98E-04  1.36E-04  1.38E-04  2.15E-04  4.61E-04  3.75E-04  4.63E-04  1.47E-03
          1.65E-03  1.81E-03
 
 SG11
+        6.71E-05  6.72E-05 -1.94E-05  2.43E-05 -6.89E-05 -5.18E-05 -6.93E-05 -9.19E-05 -1.02E-04 -3.10E-06 -5.77E-05 -1.42E-04
         -1.42E-04 -1.47E-04  7.68E-05
 
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
+        4.88E-02
 
 TH 2
+        2.06E-01  5.22E-02
 
 TH 3
+        1.15E-01  1.42E-01  6.66E-02
 
 TH 4
+        1.39E-02  1.30E-01  5.09E-01  5.73E-02
 
 OM11
+       -1.43E-01  5.03E-02  6.20E-02 -4.74E-03  3.12E-02
 
 OM12
+        1.18E-02  9.96E-02  6.79E-02 -7.74E-02  4.93E-01  2.54E-02
 
 OM13
+       -8.07E-02  2.38E-02 -3.70E-02  9.15E-02  1.50E-01  4.37E-01  3.39E-02
 
 OM14
+       -1.16E-01 -1.24E-01  9.32E-02  1.13E-01  3.01E-01  3.77E-01  6.23E-01  2.93E-02
 
 OM22
+       -7.02E-02  1.90E-02 -7.98E-02 -3.99E-02  1.83E-01  2.77E-01  7.64E-02  1.81E-01  3.83E-02
 
 OM23
+       -6.20E-02 -9.96E-02  2.59E-02  1.37E-01  2.28E-01  8.76E-02 -1.13E-02  2.79E-02  4.10E-01  4.14E-02
 
 OM24
+       -1.96E-01 -1.25E-01  1.29E-01  1.32E-01  2.11E-01  1.87E-01  2.53E-02  1.67E-01  5.22E-01  6.80E-01  3.09E-02
 
 OM33
+       -1.03E-01 -7.64E-02  8.93E-02  2.06E-01  1.32E-01  1.24E-01  3.77E-01  2.40E-01  2.85E-01  3.23E-01  3.28E-01  6.63E-02
 
 OM34
+       -4.38E-02  7.12E-03  1.76E-01  2.20E-01  2.01E-01  1.04E-01  1.99E-01  2.05E-01  3.11E-01  3.53E-01  4.04E-01  7.87E-01
          5.00E-02
 
 OM44
+       -2.41E-02  1.60E-02  1.48E-01 -1.48E-02  2.24E-01  1.26E-01  9.58E-02  1.72E-01  2.83E-01  2.13E-01  3.52E-01  5.23E-01
          7.76E-01  4.25E-02
 
 SG11
+        1.57E-01  1.47E-01 -3.32E-02  4.85E-02 -2.52E-01 -2.33E-01 -2.33E-01 -3.58E-01 -3.03E-01 -8.54E-03 -2.13E-01 -2.45E-01
         -3.24E-01 -3.94E-01  8.76E-03
 
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
+        4.96E+02
 
 TH 2
+       -6.52E+01  4.55E+02
 
 TH 3
+       -5.00E+01 -3.45E+01  3.58E+02
 
 TH 4
+        1.61E+01 -5.96E+01 -2.17E+02  5.18E+02
 
 OM11
+        1.64E+02 -9.53E+01  2.12E+01 -1.09E+01  1.65E+03
 
 OM12
+       -1.64E+02 -6.37E+01 -1.65E+02  2.22E+02 -1.01E+03  2.95E+03
 
 OM13
+        5.75E+01 -1.63E+02  1.66E+02 -9.27E+01  3.87E+02 -9.58E+02  2.12E+03
 
 OM14
+       -1.03E+01  2.24E+02 -9.82E+01 -7.32E+01 -4.25E+02  5.82E+01 -1.27E+03  2.40E+03
 
 OM22
+       -2.49E+01 -1.24E+02  1.20E+02  2.39E+01  1.41E+02 -3.78E+02  2.54E+02 -1.87E+02  1.15E+03
 
 OM23
+       -1.07E+02  7.73E+01  6.06E+01 -4.57E+01 -3.51E+02  1.90E+02 -6.08E+01  2.07E+02 -1.61E+02  1.32E+03
 
 OM24
+        2.60E+02  8.99E+01 -1.30E+02 -5.34E+01  2.26E+02 -3.52E+02  2.35E+02 -2.34E+02 -5.00E+02 -1.10E+03  2.70E+03
 
 OM33
+        4.44E+01  9.17E+01 -9.47E+00 -2.99E+00  2.36E+01  7.77E+01 -4.56E+02  1.65E+02 -1.05E+02 -5.29E+01  4.67E+01  7.72E+02
 
 OM34
+       -5.24E+01 -8.25E+01  3.78E+01 -2.48E+02 -4.47E+01  5.55E+01  2.19E+02 -9.77E+01  2.22E+01 -1.89E+02 -4.08E+01 -8.68E+02
          2.31E+03
 
 OM44
+       -3.73E+01 -4.51E+01 -1.11E+02  3.00E+02 -1.19E+02  8.65E+01  5.66E+01 -1.56E+01 -1.81E+00  1.85E+02 -2.73E+02  1.79E+02
         -1.34E+03  1.78E+03
 
 SG11
+       -2.46E+02 -5.01E+02  7.80E+01 -1.13E+02  6.64E+02  2.36E+01  4.29E+02  8.76E+02  1.02E+03 -1.01E+03  3.89E+02 -1.92E+02
          3.62E+02  1.00E+03  1.96E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.08E-01  2.14E-01  2.37E-01  3.14E-01  4.18E-01  4.84E-01  5.18E-01  7.67E-01  8.39E-01  9.86E-01  1.22E+00  1.36E+00
          1.69E+00  1.90E+00  3.95E+00
 
1
 
 
 #TBLN:      2
 #METH: Stochastic Approximation Expectation-Maximization (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            2400
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    4           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   4           
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
 EM OR BAYESIAN METHOD USED:              STOCHASTIC APPROXIMATION EXPECTATION MAXIMIZATION (SAEM)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        100         
 CONVERGENCE ITERATIONS (CITER):          5           
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              3000        
 ITERATIONS (NITER):                      500         
 ANEAL SETTING (CONSTRAIN):               1           
 STARTING SEED FOR MC METHODS (SEED):     1556678     
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
   1   2   3   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration        -3000 SAEMOBJ=  -2465.16242528933
 iteration        -2900 SAEMOBJ=  -2282.08385810427
 iteration        -2800 SAEMOBJ=  -2326.35425770561
 iteration        -2700 SAEMOBJ=  -2293.79915246223
 iteration        -2600 SAEMOBJ=  -2319.52195063455
 iteration        -2500 SAEMOBJ=  -2282.99576949370
 iteration        -2400 SAEMOBJ=  -2254.64199159453
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -2250.67272656971
 iteration          100 SAEMOBJ=  -2361.40465563846
 iteration          200 SAEMOBJ=  -2361.72195848464
 iteration          300 SAEMOBJ=  -2363.82315227591
 iteration          400 SAEMOBJ=  -2363.97588453297
 iteration          500 SAEMOBJ=  -2364.97330404121
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         3.4418E-06  4.3766E-05 -3.1056E-05  5.9666E-06
 SE:             4.1915E-02  2.8609E-02  3.3568E-02  3.4434E-02
 N:                      90          90          90          90
 
 P VAL.:         9.9993E-01  9.9878E-01  9.9926E-01  9.9986E-01
 
 ETAshrink(%):   3.5317E+00  2.4746E+01  2.6067E+01  1.5452E+01
 EBVshrink(%):   3.5350E+00  2.4762E+01  2.6040E+01  1.5443E+01
 EPSshrink(%):   3.0120E+01
 
 #TERE:
 Elapsed estimation time in seconds:    86.43
 Elapsed covariance time in seconds:     0.09
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -2364.973       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.65E+00  1.55E+00  7.97E-01  2.37E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.72E-01
 
 ETA2
+        8.66E-03  1.32E-01
 
 ETA3
+        5.52E-03  1.49E-02  1.88E-01
 
 ETA4
+       -1.70E-02  2.52E-02  3.49E-02  1.51E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.85E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.15E-01
 
 ETA2
+        5.76E-02  3.63E-01
 
 ETA3
+        3.08E-02  9.49E-02  4.33E-01
 
 ETA4
+       -1.06E-01  1.79E-01  2.08E-01  3.89E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.42E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.85E-02  5.48E-02  6.94E-02  5.56E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.06E-02
 
 ETA2
+        2.58E-02  3.97E-02
 
 ETA3
+        3.51E-02  4.53E-02  6.43E-02
 
 ETA4
+        2.90E-02  3.09E-02  4.65E-02  3.88E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        8.32E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.70E-02
 
 ETA2
+        1.67E-01  5.47E-02
 
 ETA3
+        1.94E-01  2.82E-01  7.42E-02
 
 ETA4
+        1.86E-01  2.03E-01  2.34E-01  4.99E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.72E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.36E-03
 
 TH 2
+        4.95E-04  3.00E-03
 
 TH 3
+        4.10E-04  1.03E-04  4.81E-03
 
 TH 4
+       -2.30E-05  2.82E-04  1.81E-03  3.09E-03
 
 OM11
+       -1.85E-04  1.27E-04  8.62E-05 -2.68E-05  9.38E-04
 
 OM12
+        7.81E-05  1.41E-04  7.69E-05 -9.34E-05  3.70E-04  6.65E-04
 
 OM13
+       -1.02E-04  6.58E-05 -2.91E-04  1.10E-04  1.10E-04  2.94E-04  1.23E-03
 
 OM14
+       -1.26E-04 -1.68E-04  9.00E-05  1.53E-04  2.16E-04  2.46E-04  5.82E-04  8.41E-04
 
 OM22
+       -1.40E-04 -1.78E-04 -3.94E-04 -2.10E-04  1.36E-04  2.39E-04 -3.18E-05  1.62E-04  1.57E-03
 
 OM23
+       -1.62E-04 -2.55E-04  1.33E-04  1.50E-04  3.04E-04  1.34E-05 -1.96E-04 -2.89E-05  4.58E-04  2.05E-03
 
 OM24
+       -2.68E-04 -2.74E-04  1.98E-04  1.55E-04  1.51E-04  1.08E-04 -7.57E-05  1.10E-04  5.87E-04  8.20E-04  9.52E-04
 
 OM33
+       -3.41E-04 -5.90E-04 -9.92E-06  5.72E-04  2.40E-04  9.38E-05  8.16E-04  4.13E-04  3.86E-04  6.44E-04  4.03E-04  4.13E-03
 
 OM34
+       -1.17E-04 -2.67E-04  4.20E-04  4.53E-04  2.74E-04  6.22E-05  2.57E-04  3.20E-04  3.51E-04  5.66E-04  3.80E-04  2.16E-03
          2.17E-03
 
 OM44
+       -2.84E-05 -1.22E-04  2.67E-04 -1.20E-04  2.53E-04  9.51E-05  1.14E-04  2.20E-04  3.24E-04  2.54E-04  3.33E-04  1.06E-03
          1.26E-03  1.51E-03
 
 SG11
+        2.78E-05  7.71E-05 -1.04E-05  1.26E-05 -5.94E-05 -4.27E-05 -5.82E-05 -8.40E-05 -8.52E-05 -2.62E-05 -4.98E-05 -9.97E-05
         -1.13E-04 -1.21E-04  6.91E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        4.85E-02
 
 TH 2
+        1.86E-01  5.48E-02
 
 TH 3
+        1.22E-01  2.71E-02  6.94E-02
 
 TH 4
+       -8.52E-03  9.27E-02  4.71E-01  5.56E-02
 
 OM11
+       -1.25E-01  7.58E-02  4.06E-02 -1.57E-02  3.06E-02
 
 OM12
+        6.24E-02  9.95E-02  4.30E-02 -6.52E-02  4.68E-01  2.58E-02
 
 OM13
+       -5.99E-02  3.42E-02 -1.19E-01  5.63E-02  1.02E-01  3.25E-01  3.51E-02
 
 OM14
+       -8.94E-02 -1.06E-01  4.48E-02  9.52E-02  2.43E-01  3.29E-01  5.71E-01  2.90E-02
 
 OM22
+       -7.25E-02 -8.18E-02 -1.43E-01 -9.54E-02  1.12E-01  2.33E-01 -2.28E-02  1.41E-01  3.97E-02
 
 OM23
+       -7.38E-02 -1.03E-01  4.24E-02  5.95E-02  2.19E-01  1.15E-02 -1.23E-01 -2.20E-02  2.55E-01  4.53E-02
 
 OM24
+       -1.79E-01 -1.62E-01  9.25E-02  9.03E-02  1.60E-01  1.36E-01 -6.99E-02  1.22E-01  4.79E-01  5.86E-01  3.09E-02
 
 OM33
+       -1.09E-01 -1.68E-01 -2.22E-03  1.60E-01  1.22E-01  5.66E-02  3.61E-01  2.21E-01  1.51E-01  2.21E-01  2.03E-01  6.43E-02
 
 OM34
+       -5.19E-02 -1.05E-01  1.30E-01  1.75E-01  1.92E-01  5.18E-02  1.57E-01  2.37E-01  1.90E-01  2.68E-01  2.65E-01  7.23E-01
          4.65E-02
 
 OM44
+       -1.51E-02 -5.75E-02  9.91E-02 -5.57E-02  2.13E-01  9.50E-02  8.34E-02  1.95E-01  2.11E-01  1.44E-01  2.78E-01  4.23E-01
          7.00E-01  3.88E-02
 
 SG11
+        6.89E-02  1.69E-01 -1.81E-02  2.73E-02 -2.33E-01 -1.99E-01 -1.99E-01 -3.48E-01 -2.58E-01 -6.96E-02 -1.94E-01 -1.87E-01
         -2.91E-01 -3.76E-01  8.32E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        4.88E+02
 
 TH 2
+       -6.69E+01  4.00E+02
 
 TH 3
+       -4.97E+01  1.57E+01  3.11E+02
 
 TH 4
+        2.20E+01 -7.16E+01 -1.85E+02  4.96E+02
 
 OM11
+        1.71E+02 -1.05E+02  1.31E+01  6.77E+00  1.64E+03
 
 OM12
+       -1.78E+02 -6.27E+01 -1.23E+02  1.39E+02 -9.30E+02  2.52E+03
 
 OM13
+        4.24E+01 -1.24E+02  1.33E+02 -5.35E+01  2.53E+02 -5.62E+02  1.71E+03
 
 OM14
+        2.12E+01  1.43E+02 -6.03E+01 -6.02E+01 -2.59E+02 -1.25E+02 -1.02E+03  2.20E+03
 
 OM22
+       -8.47E+00 -2.58E+01  1.14E+02  2.74E+01  1.31E+02 -3.35E+02  2.14E+02 -1.21E+02  9.69E+02
 
 OM23
+       -5.72E+01  8.90E+00  1.03E+01  2.77E+01 -2.74E+02  1.76E+02  5.11E+01  1.17E+02  1.79E+01  8.54E+02
 
 OM24
+        1.85E+02  7.92E+01 -7.04E+01 -1.17E+02  1.65E+02 -2.56E+02  1.53E+02 -1.72E+02 -5.35E+02 -7.40E+02  2.23E+03
 
 OM33
+        2.09E+01  8.91E+01  3.32E+01 -3.77E+01 -2.30E+01  5.60E+01 -4.21E+02  1.98E+02 -4.50E+01 -3.56E+01 -2.94E+01  6.58E+02
 
 OM34
+       -1.42E+01 -2.29E+01 -2.21E+01 -1.64E+02 -2.34E+01  4.42E+01  2.84E+02 -2.76E+02 -3.57E+01 -1.90E+02  1.90E+02 -6.78E+02
          1.81E+03
 
 OM44
+       -4.16E+01 -6.02E+01 -7.86E+01  2.50E+02 -1.36E+02  1.08E+02 -2.18E+01  4.58E+01  3.45E+00  2.04E+02 -3.75E+02  1.13E+02
         -1.03E+03  1.63E+03
 
 SG11
+        6.39E-01 -4.49E+02  2.50E+01  3.39E+00  6.53E+02  4.00E+01  4.35E+02  1.03E+03  6.83E+02 -1.19E+02  2.54E+01 -2.41E+02
          1.57E+02  1.18E+03  2.00E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.50E-01  2.70E-01  2.96E-01  3.58E-01  4.53E-01  5.55E-01  6.40E-01  7.74E-01  9.33E-01  9.94E-01  1.19E+00  1.45E+00
          1.66E+00  1.87E+00  3.41E+00
 
1
 
 
 #TBLN:      3
 #METH: Objective Function Evaluation by Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            2400
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
 EM OR BAYESIAN METHOD USED:              IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          5           
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      5           
 STARTING SEED FOR MC METHODS (SEED):     1556678     
 MC SAMPLES PER SUBJECT (ISAMPLE):        3000        
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                YES
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
   1   2   3   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -1144.65917393690 eff.=    3470. Smpl.=    3000. Fit.= 0.94973
 iteration            1 OBJ=  -1148.16239676294 eff.=    1066. Smpl.=    3000. Fit.= 0.89916
 iteration            2 OBJ=  -1148.29594281903 eff.=     993. Smpl.=    3000. Fit.= 0.89985
 iteration            3 OBJ=  -1148.42452355473 eff.=    1183. Smpl.=    3000. Fit.= 0.91103
 iteration            4 OBJ=  -1149.29088141030 eff.=    1216. Smpl.=    3000. Fit.= 0.91187
 iteration            5 OBJ=  -1148.17204601595 eff.=    1214. Smpl.=    3000. Fit.= 0.91342
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -3.4762E-04  4.0932E-04 -4.9135E-03 -2.5226E-03
 SE:             4.1739E-02  2.8795E-02  3.2960E-02  3.4205E-02
 N:                      90          90          90          90
 
 P VAL.:         9.9335E-01  9.8866E-01  8.8149E-01  9.4121E-01
 
 ETAshrink(%):   3.9361E+00  2.4256E+01  2.7406E+01  1.6015E+01
 EBVshrink(%):   3.6770E+00  2.6067E+01  2.7335E+01  1.6216E+01
 EPSshrink(%):   3.0092E+01
 
 #TERE:
 Elapsed estimation time in seconds:    40.03
 Elapsed covariance time in seconds:     9.42
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1148.172       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.65E+00  1.55E+00  7.97E-01  2.37E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.72E-01
 
 ETA2
+        8.66E-03  1.32E-01
 
 ETA3
+        5.52E-03  1.49E-02  1.88E-01
 
 ETA4
+       -1.70E-02  2.52E-02  3.49E-02  1.51E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.85E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.15E-01
 
 ETA2
+        5.76E-02  3.63E-01
 
 ETA3
+        3.08E-02  9.49E-02  4.33E-01
 
 ETA4
+       -1.06E-01  1.79E-01  2.08E-01  3.89E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.42E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.66E-02  5.48E-02  7.14E-02  5.45E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.98E-02
 
 ETA2
+        2.34E-02  3.57E-02
 
 ETA3
+        3.06E-02  3.62E-02  6.52E-02
 
 ETA4
+        2.47E-02  2.66E-02  3.78E-02  3.79E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        7.29E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.59E-02
 
 ETA2
+        1.53E-01  4.93E-02
 
 ETA3
+        1.68E-01  2.31E-01  7.53E-02
 
 ETA4
+        1.58E-01  1.78E-01  1.87E-01  4.88E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.51E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        2.17E-03
 
 TH 2
+        4.72E-04  3.00E-03
 
 TH 3
+        5.46E-04  4.32E-04  5.09E-03
 
 TH 4
+        2.19E-04  5.97E-04  2.07E-03  2.97E-03
 
 OM11
+        8.67E-05  1.91E-04  2.34E-04  1.71E-04  8.87E-04
 
 OM12
+        8.37E-05  1.60E-04  2.38E-04  1.53E-04  2.23E-04  5.46E-04
 
 OM13
+        1.31E-04  2.45E-04  8.55E-05  1.81E-04  2.37E-04  1.61E-04  9.37E-04
 
 OM14
+        1.01E-04  1.52E-04  2.31E-04  1.70E-04  1.23E-04  1.75E-04  4.07E-04  6.12E-04
 
 OM22
+       -8.92E-05 -2.47E-04 -4.53E-04 -2.56E-04 -4.70E-05  1.18E-04 -4.73E-05  3.19E-06  1.28E-03
 
 OM23
+        8.82E-05  3.87E-04  5.70E-04  1.78E-04  1.22E-04  1.83E-04  2.02E-04  1.22E-04 -5.80E-05  1.31E-03
 
 OM24
+        1.72E-05  5.53E-06  2.56E-04  1.63E-04  3.75E-05  7.86E-05  7.56E-05  1.39E-04  2.32E-04  4.14E-04  7.07E-04
 
 OM33
+        8.21E-05 -2.60E-04  6.50E-04  3.17E-04  1.80E-04  1.73E-04  5.21E-04  3.25E-04 -2.59E-05  2.07E-04  2.14E-04  4.26E-03
 
 OM34
+        5.91E-05 -4.91E-05  1.66E-04  5.98E-05  9.47E-05  1.08E-04  2.59E-04  2.48E-04  7.69E-05  2.12E-04  2.44E-04  1.67E-03
          1.43E-03
 
 OM44
+        4.21E-05  8.28E-05 -5.28E-05  6.06E-05  6.27E-05  7.05E-05  1.56E-04  1.55E-04  1.66E-04  1.39E-04  3.57E-04  6.56E-04
          8.93E-04  1.44E-03
 
 SG11
+        4.87E-06  2.62E-05  2.24E-05  1.96E-05 -1.56E-05 -1.97E-05 -3.50E-05 -3.18E-05 -6.19E-05 -5.51E-06 -1.98E-05 -1.55E-04
         -9.70E-05 -7.66E-05  5.31E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        4.66E-02
 
 TH 2
+        1.85E-01  5.48E-02
 
 TH 3
+        1.64E-01  1.11E-01  7.14E-02
 
 TH 4
+        8.64E-02  2.00E-01  5.31E-01  5.45E-02
 
 OM11
+        6.25E-02  1.17E-01  1.10E-01  1.05E-01  2.98E-02
 
 OM12
+        7.69E-02  1.25E-01  1.43E-01  1.20E-01  3.21E-01  2.34E-02
 
 OM13
+        9.19E-02  1.46E-01  3.91E-02  1.08E-01  2.60E-01  2.25E-01  3.06E-02
 
 OM14
+        8.73E-02  1.12E-01  1.31E-01  1.26E-01  1.67E-01  3.03E-01  5.37E-01  2.47E-02
 
 OM22
+       -5.36E-02 -1.26E-01 -1.78E-01 -1.31E-01 -4.42E-02  1.41E-01 -4.32E-02  3.61E-03  3.57E-02
 
 OM23
+        5.23E-02  1.95E-01  2.21E-01  9.02E-02  1.13E-01  2.16E-01  1.82E-01  1.36E-01 -4.48E-02  3.62E-02
 
 OM24
+        1.39E-02  3.79E-03  1.35E-01  1.13E-01  4.74E-02  1.27E-01  9.29E-02  2.11E-01  2.44E-01  4.30E-01  2.66E-02
 
 OM33
+        2.70E-02 -7.26E-02  1.40E-01  8.92E-02  9.29E-02  1.14E-01  2.61E-01  2.01E-01 -1.11E-02  8.76E-02  1.23E-01  6.52E-02
 
 OM34
+        3.35E-02 -2.37E-02  6.14E-02  2.90E-02  8.40E-02  1.23E-01  2.24E-01  2.64E-01  5.69E-02  1.55E-01  2.43E-01  6.75E-01
          3.78E-02
 
 OM44
+        2.39E-02  3.99E-02 -1.95E-02  2.93E-02  5.56E-02  7.97E-02  1.35E-01  1.66E-01  1.22E-01  1.01E-01  3.54E-01  2.65E-01
          6.23E-01  3.79E-02
 
 SG11
+        1.43E-02  6.57E-02  4.30E-02  4.94E-02 -7.19E-02 -1.16E-01 -1.57E-01 -1.76E-01 -2.38E-01 -2.09E-02 -1.02E-01 -3.26E-01
         -3.52E-01 -2.77E-01  7.29E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM14      OM22      OM23      OM24      OM33  
             OM34      OM44      SG11  
 
 TH 1
+        4.92E+02
 
 TH 2
+       -6.92E+01  3.92E+02
 
 TH 3
+       -5.37E+01  1.67E+01  3.08E+02
 
 TH 4
+        1.93E+01 -7.25E+01 -1.95E+02  4.97E+02
 
 OM11
+       -5.73E+00 -3.07E+01 -2.45E+01 -1.09E+01  1.33E+03
 
 OM12
+       -2.61E+01 -4.83E+01 -4.69E+01 -3.84E+01 -4.78E+02  2.38E+03
 
 OM13
+       -4.03E+01 -4.95E+01  9.47E+01 -6.33E+01 -2.64E+02  6.79E+00  1.70E+03
 
 OM14
+       -1.66E+01 -3.66E+01 -7.11E+01  1.44E+00  7.79E+01 -5.11E+02 -1.01E+03  2.62E+03
 
 OM22
+        5.44E+00  4.36E+01  7.08E+01  3.30E+01  7.96E+01 -3.06E+02  3.24E+01  9.44E+01  9.92E+02
 
 OM23
+        2.05E+01 -1.18E+02 -9.30E+01  8.06E+01  7.66E+00 -2.57E+02 -1.95E+02  1.87E+02  1.48E+02  1.11E+03
 
 OM24
+        5.40E+00  9.03E+01 -3.70E+01 -8.86E+01 -1.36E+01  1.47E+02  1.64E+02 -4.26E+02 -4.27E+02 -6.98E+02  2.26E+03
 
 OM33
+        3.22E+00  3.77E+01 -3.75E+01 -2.19E+01  2.21E+00 -3.18E+01 -1.58E+02  9.89E+01  4.63E+01  5.06E+01 -4.36E+01  5.08E+02
 
 OM34
+       -1.27E+01  2.23E+01  6.33E+00  5.34E+01 -2.08E+00  2.47E+01  1.13E+02 -3.40E+02 -3.36E+00 -1.81E+02  1.21E+02 -6.74E+02
          2.19E+03
 
 OM44
+       -5.96E+00 -5.67E+01  4.18E+01 -3.51E+01 -8.68E+00 -9.43E+00 -6.92E+01  1.24E+02  1.24E+01  1.53E+02 -5.07E+02  2.24E+02
         -1.03E+03  1.38E+03
 
 SG11
+       -5.53E+01 -1.07E+02 -4.48E+01 -1.29E+02  1.96E+02  1.11E+02  1.76E+02  6.02E+02  1.08E+03  2.12E+01 -4.57E+02  5.66E+02
          4.24E+02  6.58E+02  2.40E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.88E-01  3.47E-01  4.22E-01  4.64E-01  5.89E-01  6.72E-01  7.50E-01  8.27E-01  9.02E-01  9.90E-01  1.06E+00  1.32E+00
          1.36E+00  2.00E+00  3.10E+00
 
1
 
 
 #TBLN:      4
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            2400
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
 CONVERGENCE INTERVAL (CINTERVAL):        100         
 CONVERGENCE ITERATIONS (CITER):          5           
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              10000       
 ITERATIONS (NITER):                      10000       
 STARTING SEED FOR MC METHODS (SEED):     1556678     
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
 iteration       -10000 MCMCOBJ=   -2434.27915397684     
 iteration        -9900 MCMCOBJ=   -2209.14315599088     
 iteration        -9800 MCMCOBJ=   -2176.78631963776     
 iteration        -9700 MCMCOBJ=   -2215.94114345237     
 iteration        -9600 MCMCOBJ=   -2107.06653819408     
 iteration        -9500 MCMCOBJ=   -2174.67619953802     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -2193.83231057107     
 iteration          100 MCMCOBJ=   -2203.39680171369     
 iteration          200 MCMCOBJ=   -2217.49999061490     
 iteration          300 MCMCOBJ=   -2238.93861454540     
 iteration          400 MCMCOBJ=   -2219.97859116294     
 iteration          500 MCMCOBJ=   -2183.10918497118     
 iteration          600 MCMCOBJ=   -2159.59022460472     
 iteration          700 MCMCOBJ=   -2150.10683675337     
 iteration          800 MCMCOBJ=   -2328.89335955751     
 iteration          900 MCMCOBJ=   -2200.34482501244     
 iteration         1000 MCMCOBJ=   -2191.51230533056     
 iteration         1100 MCMCOBJ=   -2165.54567111733     
 iteration         1200 MCMCOBJ=   -2204.36189813339     
 iteration         1300 MCMCOBJ=   -2246.41171468713     
 iteration         1400 MCMCOBJ=   -2150.94190533938     
 iteration         1500 MCMCOBJ=   -2188.04715200128     
 iteration         1600 MCMCOBJ=   -2216.25895804933     
 iteration         1700 MCMCOBJ=   -2199.33853943427     
 iteration         1800 MCMCOBJ=   -2163.68374781605     
 iteration         1900 MCMCOBJ=   -2155.32866116259     
 iteration         2000 MCMCOBJ=   -2258.22896575442     
 iteration         2100 MCMCOBJ=   -2197.35760796899     
 iteration         2200 MCMCOBJ=   -2244.14779501399     
 iteration         2300 MCMCOBJ=   -2126.77087558835     
 iteration         2400 MCMCOBJ=   -2159.87387347494     
 iteration         2500 MCMCOBJ=   -2197.82901396400     
 iteration         2600 MCMCOBJ=   -2186.91237599895     
 iteration         2700 MCMCOBJ=   -2196.15127616777     
 iteration         2800 MCMCOBJ=   -2161.99543556240     
 iteration         2900 MCMCOBJ=   -2168.93039965410     
 iteration         3000 MCMCOBJ=   -2162.44115642344     
 iteration         3100 MCMCOBJ=   -2181.42080323153     
 iteration         3200 MCMCOBJ=   -2228.70632077253     
 iteration         3300 MCMCOBJ=   -2218.01594020004     
 iteration         3400 MCMCOBJ=   -2191.74658724556     
 iteration         3500 MCMCOBJ=   -2151.64810739126     
 iteration         3600 MCMCOBJ=   -2161.67668470315     
 iteration         3700 MCMCOBJ=   -2171.15946134260     
 iteration         3800 MCMCOBJ=   -2198.74124726648     
 iteration         3900 MCMCOBJ=   -2151.00307236442     
 iteration         4000 MCMCOBJ=   -2206.23434795978     
 iteration         4100 MCMCOBJ=   -2215.21143169995     
 iteration         4200 MCMCOBJ=   -2211.49033431211     
 iteration         4300 MCMCOBJ=   -2191.19074844744     
 iteration         4400 MCMCOBJ=   -2196.23451686859     
 iteration         4500 MCMCOBJ=   -2223.48276367124     
 iteration         4600 MCMCOBJ=   -2185.93049052921     
 iteration         4700 MCMCOBJ=   -2270.71674866961     
 iteration         4800 MCMCOBJ=   -2189.48911886864     
 iteration         4900 MCMCOBJ=   -2209.65420353135     
 iteration         5000 MCMCOBJ=   -2133.24591871083     
 iteration         5100 MCMCOBJ=   -2153.43460284307     
 iteration         5200 MCMCOBJ=   -2195.85813197711     
 iteration         5300 MCMCOBJ=   -2176.91378082688     
 iteration         5400 MCMCOBJ=   -2231.37589464385     
 iteration         5500 MCMCOBJ=   -2253.51091027634     
 iteration         5600 MCMCOBJ=   -2209.13898123161     
 iteration         5700 MCMCOBJ=   -2245.32167474994     
 iteration         5800 MCMCOBJ=   -2166.51272484903     
 iteration         5900 MCMCOBJ=   -2297.84311747547     
 iteration         6000 MCMCOBJ=   -2159.60946235997     
 iteration         6100 MCMCOBJ=   -2105.17734329424     
 iteration         6200 MCMCOBJ=   -2232.25771722066     
 iteration         6300 MCMCOBJ=   -2174.27187554933     
 iteration         6400 MCMCOBJ=   -2224.59451832318     
 iteration         6500 MCMCOBJ=   -2206.02536621370     
 iteration         6600 MCMCOBJ=   -2204.28726433584     
 iteration         6700 MCMCOBJ=   -2253.22251210095     
 iteration         6800 MCMCOBJ=   -2211.01647562125     
 iteration         6900 MCMCOBJ=   -2182.11278359845     
 iteration         7000 MCMCOBJ=   -2235.50415762668     
 iteration         7100 MCMCOBJ=   -2197.24599247804     
 iteration         7200 MCMCOBJ=   -2191.28862613321     
 iteration         7300 MCMCOBJ=   -2231.71971945893     
 iteration         7400 MCMCOBJ=   -2257.31354553362     
 iteration         7500 MCMCOBJ=   -2225.76309930631     
 iteration         7600 MCMCOBJ=   -2225.08262487830     
 iteration         7700 MCMCOBJ=   -2247.43702629846     
 iteration         7800 MCMCOBJ=   -2255.53012965585     
 iteration         7900 MCMCOBJ=   -2146.80853904952     
 iteration         8000 MCMCOBJ=   -2229.35367883821     
 iteration         8100 MCMCOBJ=   -2242.73883844112     
 iteration         8200 MCMCOBJ=   -2198.86143154313     
 iteration         8300 MCMCOBJ=   -2156.66818833428     
 iteration         8400 MCMCOBJ=   -2178.11623434194     
 iteration         8500 MCMCOBJ=   -2171.18811377579     
 iteration         8600 MCMCOBJ=   -2225.99959820662     
 iteration         8700 MCMCOBJ=   -2157.97005578769     
 iteration         8800 MCMCOBJ=   -2242.03206254627     
 iteration         8900 MCMCOBJ=   -2211.08200472945     
 iteration         9000 MCMCOBJ=   -2227.76261310995     
 iteration         9100 MCMCOBJ=   -2222.08229676894     
 iteration         9200 MCMCOBJ=   -2252.52727419108     
 iteration         9300 MCMCOBJ=   -2177.00350195645     
 iteration         9400 MCMCOBJ=   -2211.87414881472     
 iteration         9500 MCMCOBJ=   -2181.77976884032     
 iteration         9600 MCMCOBJ=   -2176.28252410570     
 iteration         9700 MCMCOBJ=   -2271.03423955943     
 iteration         9800 MCMCOBJ=   -2208.21946747850     
 iteration         9900 MCMCOBJ=   -2266.83846287498     
 iteration        10000 MCMCOBJ=   -2223.90746688503     
 
 #TERM:
 BURN-IN WAS COMPLETED
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation time in seconds:   391.59
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -2197.421       **************************************************
 #OBJS:********************************************       39.588 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.65E+00  1.54E+00  7.74E-01  2.36E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.83E-01
 
 ETA2
+        5.30E-03  1.58E-01
 
 ETA3
+        1.54E-02  1.42E-02  2.03E-01
 
 ETA4
+       -1.44E-02  2.47E-02  3.77E-02  1.65E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.27E-01
 
 ETA2
+        2.80E-02  3.94E-01
 
 ETA3
+        7.27E-02  7.77E-02  4.45E-01
 
 ETA4
+       -8.92E-02  1.50E-01  1.84E-01  4.04E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.45E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.78E-02  5.61E-02  7.44E-02  5.71E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.17E-02
 
 ETA2
+        2.54E-02  4.11E-02
 
 ETA3
+        3.06E-02  3.52E-02  6.21E-02
 
 ETA4
+        2.56E-02  2.74E-02  3.84E-02  4.00E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        7.59E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.65E-02
 
 ETA2
+        1.46E-01  5.09E-02
 
 ETA3
+        1.54E-01  1.93E-01  6.80E-02
 
 ETA4
+        1.46E-01  1.60E-01  1.74E-01  4.85E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.54E-02
 
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
+        2.28E-03
 
 TH 2
+        3.91E-04  3.15E-03
 
 TH 3
+        6.58E-04  2.15E-04  5.53E-03
 
 TH 4
+        2.53E-04  4.53E-04  2.31E-03  3.26E-03
 
 OM11
+        6.36E-05  1.63E-04  1.61E-04  1.19E-04  1.00E-03
 
 OM12
+        5.83E-05  1.27E-04  1.87E-04  1.14E-04  1.86E-04  6.45E-04
 
 OM13
+        6.09E-05  1.91E-04 -4.70E-05  6.21E-05  2.72E-04  1.16E-04  9.37E-04
 
 OM14
+        4.37E-05  1.22E-04  1.59E-04  1.02E-04  1.28E-04  1.49E-04  3.81E-04  6.53E-04
 
 OM22
+       -1.01E-04 -2.28E-04 -5.81E-04 -3.72E-04 -5.67E-05  7.66E-05 -8.51E-05 -6.31E-05  1.69E-03
 
 OM23
+        6.36E-05  1.74E-04  4.33E-04  1.26E-04  6.98E-05  1.69E-04  1.46E-04  7.59E-05  2.60E-05  1.24E-03
 
 OM24
+        1.21E-05 -4.38E-05  2.43E-04  1.88E-04  2.84E-05  3.91E-05  7.84E-05  1.20E-04  2.18E-04  3.69E-04  7.49E-04
 
 OM33
+        2.97E-05 -1.34E-04  1.94E-04  6.18E-05  1.58E-04  1.19E-04  5.30E-04  2.71E-04 -5.17E-05  3.97E-04  2.69E-04  3.86E-03
 
 OM34
+       -2.16E-05 -3.87E-05 -2.21E-04 -2.26E-04  5.39E-05  7.14E-05  2.10E-04  2.42E-04  9.80E-05  2.40E-04  2.36E-04  1.51E-03
          1.47E-03
 
 OM44
+       -4.03E-05  7.60E-05 -3.12E-04 -1.23E-04  4.39E-05  5.25E-05  1.10E-04  1.36E-04  1.86E-04  1.09E-04  3.28E-04  6.61E-04
          9.32E-04  1.60E-03
 
 SG11
+        1.28E-05  3.58E-05  7.16E-05  5.69E-05 -1.13E-05 -1.04E-05 -1.96E-05 -1.93E-05 -6.53E-05 -7.77E-06 -1.70E-05 -1.39E-04
         -9.68E-05 -7.91E-05  5.76E-05
 
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
+        4.78E-02
 
 TH 2
+        1.46E-01  5.61E-02
 
 TH 3
+        1.85E-01  5.14E-02  7.44E-02
 
 TH 4
+        9.28E-02  1.41E-01  5.43E-01  5.71E-02
 
 OM11
+        4.21E-02  9.15E-02  6.82E-02  6.56E-02  3.17E-02
 
 OM12
+        4.81E-02  8.92E-02  9.91E-02  7.87E-02  2.31E-01  2.54E-02
 
 OM13
+        4.17E-02  1.11E-01 -2.06E-02  3.55E-02  2.80E-01  1.49E-01  3.06E-02
 
 OM14
+        3.58E-02  8.52E-02  8.36E-02  7.00E-02  1.58E-01  2.30E-01  4.87E-01  2.56E-02
 
 OM22
+       -5.14E-02 -9.87E-02 -1.90E-01 -1.59E-01 -4.36E-02  7.34E-02 -6.77E-02 -6.01E-02  4.11E-02
 
 OM23
+        3.78E-02  8.78E-02  1.65E-01  6.25E-02  6.25E-02  1.89E-01  1.35E-01  8.43E-02  1.79E-02  3.52E-02
 
 OM24
+        9.29E-03 -2.85E-02  1.20E-01  1.20E-01  3.28E-02  5.63E-02  9.36E-02  1.71E-01  1.94E-01  3.83E-01  2.74E-02
 
 OM33
+        9.99E-03 -3.84E-02  4.19E-02  1.74E-02  8.04E-02  7.54E-02  2.78E-01  1.70E-01 -2.02E-02  1.81E-01  1.58E-01  6.21E-02
 
 OM34
+       -1.18E-02 -1.80E-02 -7.73E-02 -1.03E-01  4.44E-02  7.32E-02  1.78E-01  2.47E-01  6.21E-02  1.78E-01  2.25E-01  6.33E-01
          3.84E-02
 
 OM44
+       -2.11E-02  3.38E-02 -1.05E-01 -5.38E-02  3.46E-02  5.16E-02  8.98E-02  1.33E-01  1.13E-01  7.73E-02  2.99E-01  2.66E-01
          6.06E-01  4.00E-02
 
 SG11
+        3.53E-02  8.41E-02  1.27E-01  1.31E-01 -4.69E-02 -5.39E-02 -8.43E-02 -9.97E-02 -2.09E-01 -2.90E-02 -8.20E-02 -2.94E-01
         -3.32E-01 -2.60E-01  7.59E-03
 
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
+        4.64E+02
 
 TH 2
+       -5.38E+01  3.47E+02
 
 TH 3
+       -5.89E+01  2.08E+01  2.85E+02
 
 TH 4
+        1.40E+01 -5.18E+01 -1.81E+02  4.59E+02
 
 OM11
+       -5.30E+00 -2.80E+01 -2.37E+01 -4.96E+00  1.14E+03
 
 OM12
+       -1.54E+01 -3.28E+01 -3.08E+01 -3.04E+01 -2.74E+02  1.80E+03
 
 OM13
+       -2.45E+01 -4.51E+01  8.54E+01 -2.38E+01 -3.03E+02  3.16E+01  1.62E+03
 
 OM14
+        9.10E+00 -2.99E+01 -6.81E+01 -1.69E+00  3.08E+01 -3.69E+02 -8.59E+02  2.26E+03
 
 OM22
+        1.63E+00  2.77E+01  5.60E+01  3.58E+01  3.20E+01 -1.37E+02  3.65E+01  9.59E+01  6.97E+02
 
 OM23
+        7.45E+00 -6.13E+01 -7.16E+01  5.27E+01  2.04E+01 -2.35E+02 -1.20E+02  1.53E+02  3.30E+01  1.05E+03
 
 OM24
+        1.26E+00  6.70E+01 -3.98E+01 -9.99E+01 -1.08E+01  1.57E+02  3.68E+01 -2.93E+02 -2.53E+02 -5.18E+02  1.88E+03
 
 OM33
+        3.23E-01  2.29E+01 -2.41E+01 -3.03E+01  2.03E+00 -1.53E+01 -2.09E+02  1.31E+02  3.97E+01 -2.43E+01 -3.53E+01  5.01E+02
 
 OM34
+       -5.47E+00  9.91E+00  2.44E+01  9.29E+01  1.98E+01  3.79E+01  1.72E+02 -4.08E+02 -2.19E+00 -1.45E+02  6.00E+01 -5.65E+02
          1.87E+03
 
 OM44
+        6.71E+00 -4.63E+01  3.80E+01 -3.08E+01 -1.38E+01 -4.16E+01 -3.55E+01  1.05E+02 -6.47E+00  1.17E+02 -3.53E+02  1.46E+02
         -7.98E+02  1.12E+03
 
 SG11
+       -1.54E+01 -1.64E+02 -1.03E+02 -1.55E+02  1.89E+02  7.92E+01 -6.83E+01  3.27E+02  7.07E+02 -7.02E+01 -2.22E+02  5.06E+02
          4.83E+02  4.74E+02  2.13E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           EIGENVALUES OF COR MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         2.20E-01  3.85E-01  4.68E-01  4.83E-01  6.39E-01  7.60E-01  7.95E-01  8.76E-01  8.88E-01  9.84E-01  1.04E+00  1.21E+00
          1.42E+00  2.05E+00  2.78E+00
 
1
 
 
 #TBLN:      5
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -1120.20287513412        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  1.6495E+00  1.5449E+00  7.7369E-01  2.3638E+00  1.8325E-01  5.3023E-03  1.5371E-02 -1.4361E-02  1.5752E-01  1.4214E-02
             2.4715E-02  2.0272E-01  3.7677E-02  1.6547E-01  6.0191E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01
             1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:  -6.6763E+01 -5.2419E+01 -1.3214E+01 -3.6357E+01  1.3689E+01  2.8754E-01  7.6774E-01 -2.2696E+00  1.1150E+01 -4.3526E+00
             7.4712E-01  1.3990E+01 -1.6739E+01  1.5334E+01  2.0730E+01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -1124.44082782779        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       52
 NPARAMETR:  1.7091E+00  1.6100E+00  8.7288E-01  2.4124E+00  1.6154E-01  4.9023E-03  1.3805E-02 -1.0332E-02  1.3532E-01  1.7787E-02
             1.9478E-02  1.8151E-01  5.4882E-02  1.5625E-01  5.6512E-02
 PARAMETER:  1.3550E-01  1.4126E-01  2.2078E-01  1.2036E-01  3.6948E-02  9.8473E-02  9.5654E-02 -7.6625E-02  2.3976E-02  1.3610E-01
             8.4973E-02  4.1446E-02  1.5384E-01  4.1993E-02  6.8467E-02
 GRADIENT:   3.8611E-01  1.2101E+01  1.8531E+01 -2.7547E+01 -8.7334E+00  1.4245E-01  1.3780E+00 -2.3155E+00  1.5773E+00 -5.1637E+00
            -1.4056E+00 -7.3187E+00  1.8753E+01 -7.4021E+00 -1.4669E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -1125.34000326911        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       93
 NPARAMETR:  1.7063E+00  1.5995E+00  8.7919E-01  2.4137E+00  1.6250E-01  4.3980E-03  1.1768E-02 -9.0942E-03  1.1856E-01  3.6730E-02
             2.3212E-02  1.6341E-01  3.8249E-02  1.4126E-01  6.0916E-02
 PARAMETER:  1.3386E-01  1.3472E-01  2.2798E-01  1.2091E-01  3.9931E-02  8.8081E-02  8.1300E-02 -6.7244E-02 -4.2082E-02  3.0481E-01
             1.0759E-01 -3.9868E-02  9.9377E-02  1.4487E-02  1.0599E-01
 GRADIENT:  -2.4012E+00 -5.0072E+00  1.6357E+01 -2.5825E+01 -7.0683E+00  6.5739E-02  7.3291E-01  1.4151E+00 -2.9167E+00 -3.0015E-01
            -2.3090E+00 -6.7906E+00  1.3696E+01 -7.6271E+00 -4.6992E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -1125.91100685842        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      134
 NPARAMETR:  1.7055E+00  1.6015E+00  8.5938E-01  2.4127E+00  1.6594E-01  4.3234E-03  6.2372E-03 -1.4000E-02  1.2271E-01  3.6543E-02
             2.4512E-02  1.6408E-01  3.3095E-02  1.4445E-01  6.1388E-02
 PARAMETER:  1.3337E-01  1.3596E-01  2.0517E-01  1.2050E-01  5.0404E-02  8.5685E-02  4.2640E-02 -1.0244E-01 -2.4846E-02  2.9935E-01
             1.1215E-01 -3.4344E-02  8.1812E-02  3.0651E-02  1.0984E-01
 GRADIENT:   2.9286E-01  1.6760E-01  3.6379E+00 -5.9242E+00 -1.9296E+00  3.1396E-01 -1.3185E+00 -5.0116E-01 -1.3923E+00 -6.4980E-02
            -9.2743E-01 -1.7204E+00  3.7610E+00 -1.4180E+00 -6.3853E-01
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -1125.98864909211        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:      178
 NPARAMETR:  1.7052E+00  1.6023E+00  8.5409E-01  2.4139E+00  1.6917E-01  4.7337E-03  1.1027E-02 -1.1135E-02  1.2636E-01  3.7730E-02
             2.6605E-02  1.6958E-01  3.4028E-02  1.4690E-01  6.0918E-02
 PARAMETER:  1.3320E-01  1.3649E-01  1.9899E-01  1.2100E-01  6.0031E-02  9.2918E-02  7.4663E-02 -8.0698E-02 -1.0227E-02  3.0344E-01
             1.1959E-01 -1.9154E-02  8.2225E-02  3.8685E-02  1.0600E-01
 GRADIENT:  -2.3252E-01 -4.9607E-03 -3.5124E-02 -7.1367E-01  5.9723E-03  1.9348E-04 -3.6249E-03  5.3962E-04 -3.8160E-03  1.7593E-03
             2.2789E-03 -9.5450E-03  1.7499E-02 -3.2012E-03  8.5772E-03
 
0ITERATION NO.:   22    OBJECTIVE VALUE:  -1125.98874241063        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      201
 NPARAMETR:  1.7054E+00  1.6024E+00  8.5453E-01  2.4144E+00  1.6920E-01  4.7766E-03  1.1079E-02 -1.1090E-02  1.2635E-01  3.7772E-02
             2.6640E-02  1.6975E-01  3.4099E-02  1.4694E-01  6.0914E-02
 PARAMETER:  1.3332E-01  1.3655E-01  1.9951E-01  1.2120E-01  6.0121E-02  9.3750E-02  7.5009E-02 -8.0360E-02 -1.0272E-02  3.0376E-01
             1.1976E-01 -1.8707E-02  8.2348E-02  3.8738E-02  1.0598E-01
 GRADIENT:   5.1021E-03 -2.1085E-02 -1.6429E-02  2.5173E-02 -1.3167E-03  2.9972E-04  7.8827E-04 -1.5852E-03  2.4128E-03 -7.6981E-04
            -1.4313E-03 -5.5054E-03  1.5361E-02 -6.3024E-03  2.3737E-04
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      201
 NO. OF SIG. DIGITS IN FINAL EST.:  3.2

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -6.3393E-04 -9.9528E-03 -8.5527E-03 -1.2192E-02
 SE:             4.1776E-02  2.9113E-02  3.2471E-02  3.4476E-02
 N:                      90          90          90          90
 
 P VAL.:         9.8789E-01  7.3245E-01  7.9225E-01  7.2360E-01
 
 ETAshrink(%):   3.1103E+00  2.1867E+01  2.4814E+01  1.4199E+01
 EBVshrink(%):   3.4660E+00  2.1727E+01  2.5101E+01  1.4952E+01
 EPSshrink(%):   3.0018E+01
 
 #TERE:
 Elapsed estimation time in seconds:    12.87
 Elapsed covariance time in seconds:    18.33
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1125.989       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.71E+00  1.60E+00  8.55E-01  2.41E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.69E-01
 
 ETA2
+        4.78E-03  1.26E-01
 
 ETA3
+        1.11E-02  3.78E-02  1.70E-01
 
 ETA4
+       -1.11E-02  2.66E-02  3.41E-02  1.47E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.09E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        4.11E-01
 
 ETA2
+        3.27E-02  3.55E-01
 
 ETA3
+        6.54E-02  2.58E-01  4.12E-01
 
 ETA4
+       -7.03E-02  1.96E-01  2.16E-01  3.83E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.47E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.64E-02  5.14E-02  7.18E-02  5.49E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        2.92E-02
 
 ETA2
+        2.33E-02  3.64E-02
 
 ETA3
+        3.09E-02  3.22E-02  6.92E-02
 
 ETA4
+        2.56E-02  2.58E-02  4.20E-02  3.94E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.14E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        3.55E-02
 
 ETA2
+        1.58E-01  5.12E-02
 
 ETA3
+        1.76E-01  2.10E-01  8.39E-02
 
 ETA4
+        1.67E-01  1.77E-01  2.14E-01  5.14E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.85E-02
 
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
+        2.16E-03
 
 TH 2
+        3.78E-04  2.64E-03
 
 TH 3
+        6.69E-04  5.20E-04  5.16E-03
 
 TH 4
+        3.21E-04  5.27E-04  2.30E-03  3.01E-03
 
 OM11
+        9.01E-05  1.76E-04  2.36E-04  1.91E-04  8.55E-04
 
 OM12
+        9.58E-05  1.57E-04  3.14E-04  2.15E-04  1.88E-04  5.42E-04
 
 OM13
+        1.18E-04  2.49E-04  1.50E-04  2.22E-04  2.99E-04  2.50E-04  9.57E-04
 
 OM14
+        1.03E-04  1.86E-04  2.75E-04  2.30E-04  1.74E-04  2.14E-04  4.85E-04  6.53E-04
 
 OM22
+       -1.44E-05  1.22E-04 -3.14E-04 -1.50E-04  1.33E-05  1.54E-04  1.39E-04  1.26E-04  1.33E-03
 
 OM23
+        7.05E-05  8.07E-05  6.23E-04  2.74E-04  8.50E-05  2.01E-04  1.34E-04  9.58E-05  1.04E-04  1.04E-03
 
 OM24
+        4.30E-05 -6.28E-06  3.74E-04  2.23E-04  4.67E-05  1.03E-04  8.92E-05  1.51E-04  2.26E-04  4.10E-04  6.67E-04
 
 OM33
+        2.41E-04 -4.58E-06  1.36E-03  7.66E-04  2.68E-04  3.85E-04  8.18E-04  5.73E-04  2.74E-04  7.09E-04  4.41E-04  4.78E-03
 
 OM34
+        1.47E-04  9.36E-05  5.17E-04  3.17E-04  1.55E-04  2.22E-04  4.74E-04  4.29E-04  2.85E-04  3.15E-04  3.78E-04  2.24E-03
          1.77E-03
 
 OM44
+        1.02E-04  1.79E-04  1.79E-04  1.93E-04  1.07E-04  1.54E-04  3.12E-04  3.13E-04  3.09E-04  1.50E-04  3.89E-04  1.14E-03
          1.17E-03  1.55E-03
 
 SG11
+        1.34E-05  1.30E-06 -1.17E-06  1.74E-06 -2.44E-05 -5.14E-05 -8.90E-05 -7.77E-05 -1.32E-04 -7.59E-06 -3.80E-05 -2.98E-04
         -1.93E-04 -1.49E-04  8.35E-05
 
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
+        4.64E-02
 
 TH 2
+        1.58E-01  5.14E-02
 
 TH 3
+        2.01E-01  1.41E-01  7.18E-02
 
 TH 4
+        1.26E-01  1.87E-01  5.83E-01  5.49E-02
 
 OM11
+        6.64E-02  1.17E-01  1.13E-01  1.19E-01  2.92E-02
 
 OM12
+        8.86E-02  1.31E-01  1.88E-01  1.68E-01  2.77E-01  2.33E-02
 
 OM13
+        8.19E-02  1.56E-01  6.74E-02  1.30E-01  3.31E-01  3.48E-01  3.09E-02
 
 OM14
+        8.65E-02  1.41E-01  1.50E-01  1.64E-01  2.33E-01  3.60E-01  6.14E-01  2.56E-02
 
 OM22
+       -8.51E-03  6.54E-02 -1.20E-01 -7.49E-02  1.25E-02  1.81E-01  1.23E-01  1.35E-01  3.64E-02
 
 OM23
+        4.71E-02  4.88E-02  2.69E-01  1.55E-01  9.03E-02  2.68E-01  1.35E-01  1.16E-01  8.89E-02  3.22E-02
 
 OM24
+        3.58E-02 -4.74E-03  2.02E-01  1.57E-01  6.18E-02  1.72E-01  1.12E-01  2.29E-01  2.40E-01  4.93E-01  2.58E-02
 
 OM33
+        7.52E-02 -1.29E-03  2.74E-01  2.02E-01  1.33E-01  2.39E-01  3.82E-01  3.24E-01  1.09E-01  3.18E-01  2.47E-01  6.92E-02
 
 OM34
+        7.53E-02  4.34E-02  1.71E-01  1.37E-01  1.26E-01  2.27E-01  3.65E-01  4.00E-01  1.86E-01  2.33E-01  3.48E-01  7.70E-01
          4.20E-02
 
 OM44
+        5.60E-02  8.84E-02  6.31E-02  8.94E-02  9.32E-02  1.68E-01  2.56E-01  3.11E-01  2.15E-01  1.18E-01  3.82E-01  4.17E-01
          7.04E-01  3.94E-02
 
 SG11
+        3.16E-02  2.78E-03 -1.79E-03  3.46E-03 -9.14E-02 -2.41E-01 -3.15E-01 -3.32E-01 -3.96E-01 -2.58E-02 -1.61E-01 -4.72E-01
         -5.02E-01 -4.14E-01  9.14E-03
 
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
+        4.98E+02
 
 TH 2
+       -5.33E+01  4.25E+02
 
 TH 3
+       -5.94E+01 -2.00E+01  3.39E+02
 
 TH 4
+        9.02E+00 -5.48E+01 -2.17E+02  5.23E+02
 
 OM11
+       -9.79E+00 -3.33E+01 -2.26E+01 -1.47E+01  1.37E+03
 
 OM12
+       -3.43E+01 -3.45E+01 -6.09E+01 -4.45E+01 -3.11E+02  2.49E+03
 
 OM13
+       -2.55E+01 -7.41E+01  1.07E+02 -4.78E+01 -3.52E+02 -2.07E+02  1.98E+03
 
 OM14
+       -1.57E+01 -4.23E+01 -6.49E+01 -2.72E+01 -1.46E-01 -4.10E+02 -1.17E+03  2.83E+03
 
 OM22
+       -1.49E+01 -5.86E+01  7.63E+01  1.84E+01  5.02E+01 -1.70E+02 -9.52E+00  5.32E+01  9.95E+02
 
 OM23
+        2.42E+01 -3.99E+01 -8.05E+01  5.23E+01  4.61E+00 -3.81E+02 -7.66E+01  1.78E+02 -2.86E+01  1.53E+03
 
 OM24
+        1.13E+01  1.09E+02 -6.72E+01 -6.65E+01 -1.05E+01  1.15E+02  1.90E+02 -3.58E+02 -2.98E+02 -8.92E+02  2.51E+03
 
 OM33
+       -6.57E+00  4.86E+01 -6.83E+01 -3.45E+01  1.26E+01  1.05E+01 -1.98E+02  1.32E+02  5.78E+01 -2.20E+02  1.17E+02  6.75E+02
 
 OM34
+       -2.18E+01 -8.44E+00  1.88E+01  4.35E+01  1.96E+00  4.17E+01  6.79E+01 -3.67E+02 -1.52E+00  4.95E+01 -1.64E+02 -8.76E+02
          2.57E+03
 
 OM44
+       -1.48E+01 -6.26E+01  3.63E+01 -3.26E+01 -8.11E+00 -1.17E+01 -3.37E+01  3.57E+01 -9.71E+00  1.43E+02 -4.40E+02  2.46E+02
         -1.16E+03  1.51E+03
 
 SG11
+       -2.62E+02 -1.51E+02 -2.45E+01 -2.00E+02 -5.28E+01  7.11E+02  2.58E+02  7.62E+02  1.57E+03 -8.82E+02 -2.23E+02  8.78E+02
          4.20E+02  6.83E+02  2.10E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.35E-01  3.23E-01  3.37E-01  3.87E-01  4.45E-01  6.10E-01  7.12E-01  7.60E-01  8.28E-01  8.94E-01  1.05E+00  1.17E+00
          1.46E+00  1.85E+00  4.04E+00
 
 #CPUT: Total CPU Time in Seconds,      577.297
Stop Time: 
Sat 09/07/2013 
05:54 AM

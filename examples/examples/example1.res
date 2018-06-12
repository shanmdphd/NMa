Sat 04/22/2017 
09:11 AM
;Model Desc: Two compartment Model, Using ADVAN3, TRANS4
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# Example 1 (from samp5l)
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT CLX 
       V1X QX V2X SDIX SDSX
$DATA example1.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4

$PK
; The thetas are MU modeled.  
; Best that there is a linear relationship between THETAs and Mus
; The linear MU modeling of THETAS allows them to be efficiently 
; Gibbs sampled.

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

;Prior information is important for MCMC Bayesian analysis,
;not necessary for maximization methods
;Note the syntax used for defining priors that is available 
;as of NONMEM 7.3
$PRIOR NWPRI

; Prior information of THETAS
$THETAP (2.0 FIX)X4

; Variance to prior information of THETAS.  
; Because variances are very large, this means that the prior 
; information to the THETAS is highly uninformative.
$THETAPV BLOCK(4) FIX VALUES(10000,0.0)

; Prior information to the OMEGAS.
$OMEGAP BLOCK(4) FIX VALUES(0.2,0.0)
; Degrees of freedom to prior OMEGA matrix.  
; Because degrees of freedom is very low, equal to the
; the dimension of the prior OMEGA, this means that the 
; prior information to the OMEGAS is highly uninformative
$OMEGAPD (4 FIX)

; Prior information to the SIGMAS
$SIGMAP 0.06 FIX
; Degrees of freedom to prior SIGMA matrix.  
; Because degrees of freedom is very low, equal to the
; the dimension of the prior SIGMA, this means that the 
; prior information to the SIGMA is highly uninformative
$SIGMAPD (1 FIX)

; The first analysis is iterative two-stage, 
; maximum of 500 iterations (NITER), iteration results
; are printed every 5 iterations, gradient precision (SIGL) is 4. 
; Termination is tested on all of 
; the population parameters (CTYPE=3), 
; and for less then 2 significant digits change (NSIG).
; Prior information is not necessary for ITS, so NOPRIOR=1.  
; The intermediate and final results of the ITS method will be 
; recoded in row/column format in example1.ext

$EST METHOD=ITS MAPITER=0 INTERACTION FILE=example1.ext NITER=500 
     PRINT=5 NOABORT SIGL=4 CTYPE=3 CITER=10 
     CALPHA=0.05 NOPRIOR=1 NSIG=2

; The results of ITS are used as the initial values for the 
; SAEM method. A maximum of 3000 ; stochastic iterations (NBURN) 
; is requested, but may end early if statistical test determines
; that variations in all parameters is stationary 
; (note that any settings from the previous $EST
; carries over to the next $EST statement, within a $PROB).  
; The SAEM is a Monte Carlo process,
; so setting the SEED assures repeatability of results.  
; Each iteration obtains only 2 Monte Carlo samples ISAMPLE),
;  so they are very fast. 
; But many iterations are needed, so PRINT only
; every 100th iteration.  
; After the stochastic phase, 500 accumulation iterations will be
; Performed (NITER), to obtain good parameters estimates with 
; little stochastic noise.
; As a new FILE has not been given, the SAEM results will append to 
; example1.ext.

$EST METHOD=SAEM INTERACTION NBURN=3000 NITER=500 PRINT=100 
     SEED=1556678 ISAMPLE=2

; After the SAEM method, obtain good estimates of the marginal 
; density (objective function),
; along with good estimates of the standard errors.  
; This is best done with importance sampling ; (IMP), 
; performing the expectation step only (EONLY=1), so that 
; final population parameters remain at the final SAEM result.  
; Five iterations (NITER) should allow the importance sampling
; proposal density to become stationary.  
; This is observed by the objective function settling 
; to a particular value (with some stochastic noise).  
; By using 3000 Monte Carlo samples
; (ISAMPLE), this assures a precise assessment of standard errors.

$EST METHOD=IMP  INTERACTION EONLY=1 NITER=5 ISAMPLE=3000 PRINT=1 
     SIGL=8 NOPRIOR=1

; The Bayesian analysis is performed.  
; While 10000 burn-in iterations are requested as a maximum, 
; because the termination test is on (CTYPE<>0, set at the
; first $EST statement), and because the initial parameters are at 
; the SAEM result, which is the maximum likelihood position, 
; the analysis should settle down to a stationary distribution in
; several hundred iterations.  
; Prior information is also used to facilitate Bayesian analysis.
; The individual Bayesian iteration results are important, 
; and may be need for post-processing analysis. 
; So specify a separate FILE for the Bayesian analysis. 

$EST METHOD=BAYES INTERACTION FILE=example1.txt NBURN=10000     
     NITER=10000 PRINT=100 NOPRIOR=0

; Just for old-times sake, let's see what the traditional 
; FOCE method will give us.  
; And, remember to introduce a new FILE, so its results won't 
; append to our Bayesian FILE. 
; Appending to example1.ext with the EM methods is fine.

$EST METHOD=COND INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 
     PRINT=5 NOABORT NOPRIOR=1
     FILE=example1.ext

; Time for the standard error results.  
; You may request a more precise gradient precision (SIGL)
; that differed from that used during estimation.

$COV MATRIX=R PRINT=E UNCONDITIONAL SIGL=12

; Print out results in tables. Include some of the new weighted 
; residual types

$TABLE ID TIME PRED RES WRES CPRED CWRES EPRED ERES EWRES NOAPPEND 
       ONEHEADER FILE=example1.TAB NOPRINT
$TABLE ID CL V1 Q V2 FIRSTONLY NOAPPEND NOPRINT FILE=example1.PAR
$TABLE ID ETA1 ETA2 ETA3 ETA4 FIRSTONLY NOAPPEND 
        NOPRINT FILE=example1.ETA
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
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

 TOT. NO. OF OBS RECS:      500
 TOT. NO. OF INDIVIDUALS:      100
0LENGTH OF THETA:  10
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
0SIGMA HAS BLOCK FORM:
  1
  0  2
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
  0.1000E+01     0.1000E+01     0.1000E+01
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
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.6000E+00
        2                                                                                  YES
                  0.6000E-01
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
 GRADIENT METHOD USED:     NOSLOW
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                12
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
 NO. OF TABLES:           3
 SEED NUMBER (SEED):    11456
 RANMETHOD:             3U
 MC SAMPLES (ESAMPLE):    300
 WRES SQUARE ROOT TYPE (WRESCHOL): EIGENVALUE
0-- TABLE   1 --
0RECORDS ONLY:    ALL
04 COLUMNS APPENDED:    NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
 FIXED_EFFECT_ETAS:
0USER-CHOSEN ITEMS:
 ID TIME PRED RES WRES CPRED CWRES EPRED ERES EWRES
0-- TABLE   2 --
0RECORDS ONLY:    FIRSTONLY
04 COLUMNS APPENDED:    NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
 FIXED_EFFECT_ETAS:
0USER-CHOSEN ITEMS:
 ID CL V1 Q V2
0-- TABLE   3 --
0RECORDS ONLY:    FIRSTONLY
04 COLUMNS APPENDED:    NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
 FIXED_EFFECT_ETAS:
0USER-CHOSEN ITEMS:
 ID ETA1 ETA2 ETA3 ETA4
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 beta 2 (nm74b2)

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
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            2808
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      4
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     4
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example1.ext
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
 ITERATIONS (NITER):                        500
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
   1   2   3   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -234.362672489410
 iteration            5 OBJ=  -1112.97512482533
 iteration           10 OBJ=  -1119.94317663869
 iteration           15 OBJ=  -1120.29018012970
 iteration           20 OBJ=  -1120.35228857100
 iteration           25 OBJ=  -1120.34054383813
 iteration           30 OBJ=  -1120.31777402277
 iteration           35 OBJ=  -1120.29856760396
 iteration           40 OBJ=  -1120.28442334083
 iteration           45 OBJ=  -1120.27589962695
 iteration           50 OBJ=  -1120.26996052769
 iteration           55 OBJ=  -1120.26681840264
 iteration           60 OBJ=  -1120.26469769760
 iteration           65 OBJ=  -1120.26360712634
 iteration           70 OBJ=  -1120.26262376733
 iteration           75 OBJ=  -1120.26216006031
 iteration           80 OBJ=  -1120.26196594384
 iteration           85 OBJ=  -1120.26182949091
 iteration           90 OBJ=  -1120.26193456073
 iteration           95 OBJ=  -1120.26174936530
 iteration          100 OBJ=  -1120.26189730675
 iteration          105 OBJ=  -1120.26191490762
 Convergence achieved
 iteration          105 OBJ=  -1120.26170839200
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.3871E-07 -9.8699E-07 -8.5204E-07 -6.0569E-08
 SE:             3.9119E-02  2.9342E-02  3.5669E-02  3.4353E-02
 N:                     100         100         100         100
 
 P VAL.:         1.0000E+00  9.9997E-01  9.9998E-01  1.0000E+00
 
 ETASHRINKSD(%)  3.2922E+00  1.9372E+01  2.2485E+01  1.4424E+01
 ETASHRINKVR(%)  6.4760E+00  3.4991E+01  3.9914E+01  2.6767E+01
 EBVSHRINKSD(%)  3.2923E+00  1.9372E+01  2.2486E+01  1.4425E+01
 EBVSHRINKVR(%)  6.4762E+00  3.4992E+01  3.9916E+01  2.6768E+01
 EPSSHRINKSD(%)  3.1728E+01
 EPSSHRINKVR(%)  5.3389E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):          500
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    918.938533204673     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -1120.26170839200     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -201.323175187325     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:     5.59
 Elapsed covariance  time in seconds:     0.47
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1120.262       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.68E+00  1.59E+00  8.13E-01  2.37E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.65E-01
 
 ETA2
+        4.62E-03  1.34E-01
 
 ETA3
+        6.35E-03  1.69E-02  2.14E-01
 
 ETA4
+       -1.53E-02  1.26E-02  5.33E-02  1.63E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        5.45E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.07E-01
 
 ETA2
+        3.11E-02  3.66E-01
 
 ETA3
+        3.38E-02  1.00E-01  4.62E-01
 
 ETA4
+       -9.33E-02  8.53E-02  2.85E-01  4.03E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.33E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.62E-02  4.90E-02  6.53E-02  5.39E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.90E-02
 
 ETA2
+        2.31E-02  3.49E-02
 
 ETA3
+        3.25E-02  3.83E-02  6.48E-02
 
 ETA4
+        2.76E-02  2.73E-02  4.50E-02  4.06E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.61E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        3.56E-02
 
 ETA2
+        1.53E-01  4.77E-02
 
 ETA3
+        1.71E-01  2.22E-01  7.01E-02
 
 ETA4
+        1.73E-01  1.79E-01  1.86E-01  5.04E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.63E-02
 
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
+        2.13E-03
 
 TH 2
+        3.70E-04  2.40E-03
 
 TH 3
+        3.77E-04  2.21E-04  4.26E-03
 
 TH 4
+       -1.52E-05  1.67E-04  1.61E-03  2.91E-03
 
 OM11
+       -2.51E-04  9.51E-05  5.84E-05 -9.68E-05  8.40E-04
 
 OM12
+        1.09E-05  1.74E-04  2.55E-05 -1.43E-04  3.14E-04  5.35E-04
 
 OM13
+       -1.72E-04 -8.86E-06 -2.67E-04  2.16E-04  1.45E-04  2.38E-04  1.06E-03
 
 OM14
+       -1.84E-04 -2.18E-04  1.34E-04  1.63E-04  2.24E-04  1.82E-04  5.47E-04  7.62E-04
 
 OM22
+       -3.90E-05 -2.28E-05 -2.18E-04 -1.24E-04  1.40E-04  2.40E-04  8.20E-05  1.37E-04  1.22E-03
 
 OM23
+       -8.29E-05 -8.04E-05  6.23E-05  1.21E-04  1.88E-04  4.05E-05 -6.25E-05 -2.94E-05  2.80E-04  1.47E-03
 
 OM24
+       -2.62E-04 -1.01E-04  1.32E-04  1.46E-04  1.14E-04  8.62E-05  2.40E-05  9.07E-05  3.78E-04  5.88E-04  7.47E-04
 
 OM33
+       -3.31E-04 -3.25E-04 -2.60E-04  4.94E-04  2.20E-04  1.45E-04  8.13E-04  3.81E-04  5.41E-04  4.36E-04  4.02E-04  4.20E-03
 
 OM34
+       -2.35E-05 -8.77E-05  2.28E-04  2.47E-04  2.49E-04  1.15E-04  2.89E-04  2.11E-04  3.75E-04  3.82E-04  3.30E-04  2.18E-03
          2.03E-03
 
 OM44
+        2.93E-05 -1.97E-05  1.75E-04 -3.14E-04  2.71E-04  1.26E-04  6.08E-05  1.49E-04  3.24E-04  2.03E-04  2.25E-04  1.10E-03
          1.34E-03  1.65E-03
 
 SG11
+        4.47E-05  6.18E-05 -1.22E-06  3.02E-05 -5.92E-05 -4.22E-05 -5.65E-05 -7.69E-05 -7.90E-05  7.97E-06 -3.98E-05 -9.37E-05
         -1.06E-04 -1.16E-04  5.79E-05
 
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
+        4.62E-02
 
 TH 2
+        1.63E-01  4.90E-02
 
 TH 3
+        1.25E-01  6.89E-02  6.53E-02
 
 TH 4
+       -6.10E-03  6.32E-02  4.58E-01  5.39E-02
 
 OM11
+       -1.88E-01  6.70E-02  3.09E-02 -6.19E-02  2.90E-02
 
 OM12
+        1.02E-02  1.54E-01  1.69E-02 -1.15E-01  4.68E-01  2.31E-02
 
 OM13
+       -1.15E-01 -5.56E-03 -1.26E-01  1.24E-01  1.53E-01  3.17E-01  3.25E-02
 
 OM14
+       -1.44E-01 -1.61E-01  7.44E-02  1.10E-01  2.80E-01  2.84E-01  6.10E-01  2.76E-02
 
 OM22
+       -2.42E-02 -1.33E-02 -9.56E-02 -6.57E-02  1.39E-01  2.97E-01  7.23E-02  1.42E-01  3.49E-02
 
 OM23
+       -4.69E-02 -4.28E-02  2.49E-02  5.85E-02  1.70E-01  4.58E-02 -5.02E-02 -2.78E-02  2.10E-01  3.83E-02
 
 OM24
+       -2.07E-01 -7.57E-02  7.38E-02  9.90E-02  1.45E-01  1.36E-01  2.70E-02  1.20E-01  3.96E-01  5.61E-01  2.73E-02
 
 OM33
+       -1.11E-01 -1.02E-01 -6.15E-02  1.41E-01  1.17E-01  9.68E-02  3.86E-01  2.13E-01  2.39E-01  1.76E-01  2.27E-01  6.48E-02
 
 OM34
+       -1.13E-02 -3.98E-02  7.76E-02  1.02E-01  1.91E-01  1.10E-01  1.98E-01  1.70E-01  2.38E-01  2.21E-01  2.68E-01  7.46E-01
          4.50E-02
 
 OM44
+        1.56E-02 -9.87E-03  6.58E-02 -1.43E-01  2.30E-01  1.34E-01  4.60E-02  1.33E-01  2.28E-01  1.31E-01  2.02E-01  4.16E-01
          7.32E-01  4.06E-02
 
 SG11
+        1.27E-01  1.66E-01 -2.46E-03  7.35E-02 -2.69E-01 -2.40E-01 -2.28E-01 -3.66E-01 -2.97E-01  2.74E-02 -1.91E-01 -1.90E-01
         -3.09E-01 -3.74E-01  7.61E-03
 
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
+        5.64E+02
 
 TH 2
+       -5.88E+01  4.91E+02
 
 TH 3
+       -5.62E+01 -9.43E+00  3.56E+02
 
 TH 4
+        1.83E+01 -5.04E+01 -2.19E+02  5.50E+02
 
 OM11
+        2.21E+02 -9.23E+01  1.20E+00  1.08E+01  1.82E+03
 
 OM12
+       -1.53E+02 -1.74E+02 -1.30E+02  2.30E+02 -9.71E+02  3.07E+03
 
 OM13
+        2.24E+01 -1.37E+02  2.06E+02 -1.48E+02  2.28E+02 -6.54E+02  2.09E+03
 
 OM14
+        3.51E+01  2.37E+02 -1.57E+02 -3.40E+01 -3.84E+02  2.90E+00 -1.32E+03  2.61E+03
 
 OM22
+       -6.44E+01 -3.01E+01  9.39E+01 -5.87E+00  1.16E+02 -4.54E+02  2.14E+02 -1.17E+02  1.17E+03
 
 OM23
+       -7.98E+01  4.36E+01  2.09E+01 -1.29E+00 -2.72E+02  1.08E+02  1.42E+01  1.15E+02 -1.87E+01  1.10E+03
 
 OM24
+        2.75E+02  9.30E+00 -6.61E+01 -8.03E+01  1.88E+02 -1.82E+02  1.35E+02 -1.45E+02 -4.71E+02 -8.74E+02  2.48E+03
 
 OM33
+        6.36E+01  6.99E+01  5.11E+01 -3.32E+01  1.03E+01  5.46E+01 -3.82E+02  1.09E+02 -1.24E+02  5.82E+00 -1.17E+01  7.18E+02
 
 OM34
+       -1.02E+02 -3.13E+01 -3.09E+01 -1.81E+02 -5.19E+01  1.07E+01  1.25E+02  2.97E+01  7.00E+01 -1.43E+02 -6.85E+01 -8.66E+02
          2.35E+03
 
 OM44
+       -1.93E+01 -5.21E+01 -8.17E+01  2.87E+02 -1.26E+02  8.48E+01  1.29E+02 -8.95E+01 -5.86E+01  5.92E+01 -4.25E+01  2.04E+02
         -1.30E+03  1.69E+03
 
 SG11
+       -2.09E+02 -5.82E+02  1.71E+01 -1.36E+02  7.83E+02  3.07E+02  4.99E+02  1.30E+03  1.02E+03 -9.27E+02  7.70E+02 -4.76E+02
          6.92E+02  1.07E+03  2.60E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.29E-01  2.52E-01  3.20E-01  3.38E-01  4.49E-01  5.04E-01  6.18E-01  7.73E-01  9.51E-01  1.06E+00  1.37E+00  1.44E+00
          1.57E+00  1.76E+00  3.45E+00
 
1
 
 
 #TBLN:      2
 #METH: Stochastic Approximation Expectation-Maximization (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            2808
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      4
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     4
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example1.ext
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
 CONVERGENCE INTERVAL (CINTERVAL):          100
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 BURN-IN ITERATIONS (NBURN):                3000
 ITERATIONS (NITER):                        500
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       1556678
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
   1   2   3   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration        -3000 SAEMOBJ=  -2604.71396898173
 iteration        -2900 SAEMOBJ=  -2433.32722202469
 iteration        -2800 SAEMOBJ=  -2458.71826241190
 iteration        -2700 SAEMOBJ=  -2447.68807491891
 iteration        -2600 SAEMOBJ=  -2493.65782903621
 iteration        -2500 SAEMOBJ=  -2434.93131142831
 iteration        -2400 SAEMOBJ=  -2445.55804881440
 iteration        -2300 SAEMOBJ=  -2456.70780608124
 iteration        -2200 SAEMOBJ=  -2426.13882097189
 iteration        -2100 SAEMOBJ=  -2486.21215683838
 iteration        -2000 SAEMOBJ=  -2397.86889409119
 iteration        -1900 SAEMOBJ=  -2487.02485987004
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -2452.34470559883
 iteration          100 SAEMOBJ=  -2485.85472061194
 iteration          200 SAEMOBJ=  -2486.05446956442
 iteration          300 SAEMOBJ=  -2485.76688282198
 iteration          400 SAEMOBJ=  -2486.33402054589
 iteration          500 SAEMOBJ=  -2486.76781317480
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -7.7988E-06 -4.2058E-05  1.2834E-05  1.5648E-05
 SE:             3.9199E-02  2.8890E-02  3.3114E-02  3.3167E-02
 N:                     100         100         100         100
 
 P VAL.:         9.9984E-01  9.9884E-01  9.9969E-01  9.9962E-01
 
 ETASHRINKSD(%)  3.5658E+00  2.2869E+01  2.6983E+01  1.6667E+01
 ETASHRINKVR(%)  7.0044E+00  4.0507E+01  4.6686E+01  3.0556E+01
 EBVSHRINKSD(%)  3.5657E+00  2.2871E+01  2.6981E+01  1.6672E+01
 EBVSHRINKVR(%)  7.0042E+00  4.0511E+01  4.6682E+01  3.0564E+01
 EPSSHRINKSD(%)  3.0297E+01
 EPSSHRINKVR(%)  5.1415E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):          500
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    918.938533204673     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2486.76781317480     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1567.82927997012     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2486.76781317480     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1751.61698661106     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    26.32
 Elapsed covariance  time in seconds:     0.34
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -2486.768       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.63E+00  1.55E+00  7.44E-01  2.35E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.67E-01
 
 ETA2
+       -2.78E-03  1.42E-01
 
 ETA3
+        1.66E-02 -5.92E-03  2.08E-01
 
 ETA4
+       -1.57E-02  1.31E-02  3.91E-02  1.60E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        5.53E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.09E-01
 
 ETA2
+       -1.81E-02  3.76E-01
 
 ETA3
+        8.90E-02 -3.45E-02  4.56E-01
 
 ETA4
+       -9.63E-02  8.71E-02  2.15E-01  4.00E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.35E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.70E-02  5.40E-02  7.08E-02  5.52E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.89E-02
 
 ETA2
+        2.43E-02  3.81E-02
 
 ETA3
+        3.33E-02  4.57E-02  7.00E-02
 
 ETA4
+        2.72E-02  3.03E-02  4.79E-02  4.12E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.60E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        3.54E-02
 
 ETA2
+        1.59E-01  5.05E-02
 
 ETA3
+        1.72E-01  2.67E-01  7.67E-02
 
 ETA4
+        1.71E-01  1.96E-01  2.19E-01  5.15E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.62E-02
 
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
+        2.21E-03
 
 TH 2
+        3.82E-04  2.91E-03
 
 TH 3
+        5.69E-04 -9.56E-05  5.01E-03
 
 TH 4
+        1.19E-05  1.58E-04  1.76E-03  3.05E-03
 
 OM11
+       -2.89E-04  1.51E-04 -2.16E-05 -1.39E-04  8.37E-04
 
 OM12
+        2.27E-05  1.72E-04  8.89E-06 -1.42E-04  3.02E-04  5.90E-04
 
 OM13
+       -2.30E-04  4.06E-05 -5.45E-04  1.29E-04  1.93E-04  1.85E-04  1.11E-03
 
 OM14
+       -2.16E-04 -1.98E-04  4.86E-05  9.16E-05  2.17E-04  1.72E-04  4.91E-04  7.42E-04
 
 OM22
+       -1.13E-04 -3.41E-04 -3.15E-04 -2.98E-04  8.75E-05  1.82E-04 -8.14E-05  1.07E-04  1.45E-03
 
 OM23
+       -1.54E-04  2.46E-05  2.17E-04  1.57E-05  2.59E-04  7.57E-05 -1.39E-04 -3.06E-05  1.75E-04  2.09E-03
 
 OM24
+       -3.10E-04 -2.12E-04  1.90E-04  1.22E-04  1.29E-04  1.22E-04 -9.74E-06  7.93E-05  3.97E-04  7.02E-04  9.20E-04
 
 OM33
+       -6.28E-04 -7.30E-04 -5.21E-04  3.36E-04  1.69E-04  6.02E-05  8.76E-04  3.51E-04  3.22E-04  6.93E-05  3.30E-04  4.89E-03
 
 OM34
+       -1.88E-04 -3.26E-04  1.42E-04  3.07E-05  2.00E-04  6.78E-05  2.33E-04  2.14E-04  2.87E-04  4.21E-04  2.77E-04  2.46E-03
          2.29E-03
 
 OM44
+       -8.53E-05 -1.63E-04  1.99E-05 -4.71E-04  2.17E-04  9.09E-05  1.30E-05  1.28E-04  3.30E-04  2.27E-04  2.68E-04  1.15E-03
          1.36E-03  1.70E-03
 
 SG11
+        3.78E-05  8.10E-05 -1.41E-05  3.52E-05 -5.49E-05 -3.65E-05 -3.89E-05 -6.89E-05 -7.53E-05 -3.44E-05 -5.07E-05 -9.15E-05
         -1.10E-04 -1.19E-04  5.77E-05
 
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
+        4.70E-02
 
 TH 2
+        1.50E-01  5.40E-02
 
 TH 3
+        1.71E-01 -2.50E-02  7.08E-02
 
 TH 4
+        4.58E-03  5.29E-02  4.49E-01  5.52E-02
 
 OM11
+       -2.13E-01  9.69E-02 -1.06E-02 -8.70E-02  2.89E-02
 
 OM12
+        1.99E-02  1.31E-01  5.17E-03 -1.06E-01  4.29E-01  2.43E-02
 
 OM13
+       -1.47E-01  2.26E-02 -2.31E-01  7.01E-02  2.00E-01  2.29E-01  3.33E-02
 
 OM14
+       -1.69E-01 -1.34E-01  2.52E-02  6.09E-02  2.75E-01  2.60E-01  5.41E-01  2.72E-02
 
 OM22
+       -6.30E-02 -1.66E-01 -1.17E-01 -1.42E-01  7.95E-02  1.97E-01 -6.43E-02  1.03E-01  3.81E-02
 
 OM23
+       -7.17E-02  9.97E-03  6.71E-02  6.23E-03  1.95E-01  6.81E-02 -9.12E-02 -2.46E-02  1.01E-01  4.57E-02
 
 OM24
+       -2.17E-01 -1.29E-01  8.86E-02  7.26E-02  1.47E-01  1.66E-01 -9.65E-03  9.60E-02  3.44E-01  5.06E-01  3.03E-02
 
 OM33
+       -1.91E-01 -1.93E-01 -1.05E-01  8.70E-02  8.34E-02  3.55E-02  3.76E-01  1.84E-01  1.21E-01  2.17E-02  1.56E-01  7.00E-02
 
 OM34
+       -8.34E-02 -1.26E-01  4.17E-02  1.16E-02  1.44E-01  5.83E-02  1.46E-01  1.64E-01  1.57E-01  1.92E-01  1.91E-01  7.35E-01
          4.79E-02
 
 OM44
+       -4.40E-02 -7.32E-02  6.82E-03 -2.07E-01  1.82E-01  9.08E-02  9.47E-03  1.14E-01  2.11E-01  1.21E-01  2.14E-01  3.99E-01
          6.90E-01  4.12E-02
 
 SG11
+        1.06E-01  1.98E-01 -2.63E-02  8.40E-02 -2.50E-01 -1.98E-01 -1.54E-01 -3.33E-01 -2.60E-01 -9.90E-02 -2.20E-01 -1.72E-01
         -3.03E-01 -3.79E-01  7.60E-03
 
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
+        5.58E+02
 
 TH 2
+       -4.96E+01  4.22E+02
 
 TH 3
+       -7.56E+01  3.37E+01  3.18E+02
 
 TH 4
+        2.29E+01 -6.18E+01 -1.94E+02  5.06E+02
 
 OM11
+        2.25E+02 -1.01E+02 -3.05E+01  4.94E+01  1.74E+03
 
 OM12
+       -1.65E+02 -1.51E+02 -7.92E+01  1.47E+02 -7.30E+02  2.43E+03
 
 OM13
+       -2.97E+01 -1.07E+02  2.18E+02 -1.10E+02 -5.96E+01 -2.97E+02  1.85E+03
 
 OM14
+        1.10E+02  1.51E+02 -1.27E+02 -2.24E+01 -2.15E+02 -1.78E+02 -1.08E+03  2.36E+03
 
 OM22
+       -3.57E+01  5.68E+01  9.32E+01  2.42E+01  4.65E+01 -2.41E+02  2.51E+02 -1.36E+02  9.21E+02
 
 OM23
+       -2.70E+01 -2.12E+01  1.23E+01  2.59E+01 -1.97E+02  8.50E+01  3.36E+01  7.95E+01  6.82E+01  7.36E+02
 
 OM24
+        2.03E+02  5.87E+01 -6.57E+01 -1.17E+02  1.19E+02 -2.75E+02  2.52E+01 -3.63E+00 -3.72E+02 -6.03E+02  1.90E+03
 
 OM33
+        6.77E+01  9.27E+01  5.50E+01 -6.11E+01  8.62E+00  1.90E+01 -3.93E+02  1.80E+02 -3.63E+01  1.31E+02 -1.31E+02  6.46E+02
 
 OM34
+       -5.46E+01 -3.70E+01 -6.78E+01 -5.96E+01  1.32E+01 -3.47E+00  2.48E+02 -1.87E+02 -4.43E+00 -3.10E+02  2.69E+02 -7.59E+02
          1.85E+03
 
 OM44
+       -2.35E+01 -5.62E+01 -2.84E+01  2.22E+02 -1.20E+02  9.07E+01  8.26E+01  1.94E+01 -3.28E+01  1.58E+02 -2.64E+02  1.37E+02
         -9.45E+02  1.42E+03
 
 SG11
+       -3.72E+01 -5.23E+02  8.51E+01 -5.61E+01  6.93E+02  3.45E+02  3.97E+02  1.31E+03  6.32E+02 -4.32E+01  3.82E+02 -3.84E+02
          4.95E+02  1.13E+03  2.46E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.36E-01  2.87E-01  3.29E-01  3.88E-01  5.01E-01  5.19E-01  6.70E-01  7.63E-01  8.79E-01  1.14E+00  1.27E+00  1.54E+00
          1.62E+00  1.76E+00  3.21E+00
 
1
 
 
 #TBLN:      3
 #METH: Objective Function Evaluation by Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            2808
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      8
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     8
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example1.ext
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
 STARTING SEED FOR MC METHODS (SEED):       1556678
 MC SAMPLES PER SUBJECT (ISAMPLE):          3000
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 EXPECTATION ONLY (EONLY):                  1
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     0.100000000000000       ,10.0000000000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 LONG TAIL SAMPLE ACCEPT. RATE (IACCEPTL):   0.00000000000000
 T-DIST. PROPOSAL DENSITY (DF):             0
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
   1   2   3   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -1145.08864866640 eff.=    2282. Smpl.=    3000. Fit.= 0.94054
 iteration            1 OBJ=  -1145.65743257646 eff.=    1106. Smpl.=    3000. Fit.= 0.90922
 iteration            2 OBJ=  -1145.52298247107 eff.=    1018. Smpl.=    3000. Fit.= 0.90439
 iteration            3 OBJ=  -1145.13509037095 eff.=    1029. Smpl.=    3000. Fit.= 0.90557
 iteration            4 OBJ=  -1145.71520626779 eff.=    1051. Smpl.=    3000. Fit.= 0.90667
 iteration            5 OBJ=  -1145.39154666664 eff.=    1045. Smpl.=    3000. Fit.= 0.90616
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         4.7244E-04  7.2327E-04  1.6906E-03  5.2810E-04
 SE:             3.9138E-02  2.9069E-02  3.3156E-02  3.3207E-02
 N:                     100         100         100         100
 
 P VAL.:         9.9037E-01  9.8015E-01  9.5933E-01  9.8731E-01
 
 ETASHRINKSD(%)  3.7166E+00  2.2389E+01  2.6891E+01  1.6565E+01
 ETASHRINKVR(%)  7.2951E+00  3.9766E+01  4.6551E+01  3.0387E+01
 EBVSHRINKSD(%)  3.5518E+00  2.2950E+01  2.6574E+01  1.6518E+01
 EBVSHRINKVR(%)  6.9774E+00  4.0633E+01  4.6086E+01  3.0307E+01
 EPSSHRINKSD(%)  3.0414E+01
 EPSSHRINKVR(%)  5.1578E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):          500
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    918.938533204673     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -1145.39154666664     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -226.453013461969     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:     7.02
 Elapsed covariance  time in seconds:     2.11
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -1145.392       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.63E+00  1.55E+00  7.44E-01  2.35E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.67E-01
 
 ETA2
+       -2.78E-03  1.42E-01
 
 ETA3
+        1.66E-02 -5.92E-03  2.08E-01
 
 ETA4
+       -1.57E-02  1.31E-02  3.91E-02  1.60E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        5.53E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.09E-01
 
 ETA2
+       -1.81E-02  3.76E-01
 
 ETA3
+        8.90E-02 -3.45E-02  4.56E-01
 
 ETA4
+       -9.63E-02  8.71E-02  2.15E-01  4.00E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.35E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.33E-02  5.13E-02  6.84E-02  5.22E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.75E-02
 
 ETA2
+        2.22E-02  3.44E-02
 
 ETA3
+        2.95E-02  3.64E-02  6.26E-02
 
 ETA4
+        2.35E-02  2.55E-02  3.70E-02  3.78E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        6.60E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        3.36E-02
 
 ETA2
+        1.45E-01  4.57E-02
 
 ETA3
+        1.52E-01  2.12E-01  6.87E-02
 
 ETA4
+        1.47E-01  1.66E-01  1.69E-01  4.72E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.40E-02
 
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
+        1.87E-03
 
 TH 2
+        2.55E-04  2.63E-03
 
 TH 3
+        5.60E-04  1.46E-04  4.68E-03
 
 TH 4
+        1.65E-04  3.70E-04  1.76E-03  2.73E-03
 
 OM11
+        5.08E-05  1.44E-04  1.34E-04  1.18E-04  7.56E-04
 
 OM12
+        6.20E-05  1.42E-04  1.97E-04  1.12E-04  1.13E-04  4.92E-04
 
 OM13
+        6.23E-05  1.65E-04 -3.04E-05  1.20E-04  2.75E-04  6.63E-05  8.69E-04
 
 OM14
+        5.73E-05  1.11E-04  1.32E-04  9.19E-05  1.16E-04  1.08E-04  3.62E-04  5.50E-04
 
 OM22
+       -7.93E-05 -2.59E-04 -4.01E-04 -2.08E-04 -4.22E-05  2.67E-05 -4.09E-05 -1.87E-05  1.18E-03
 
 OM23
+        6.22E-05  2.78E-04  6.07E-04  1.39E-04  6.31E-05  2.24E-04  3.08E-05  5.64E-05 -1.71E-04  1.33E-03
 
 OM24
+        4.79E-06 -3.35E-05  2.01E-04  1.47E-04  2.17E-05  5.91E-05  3.08E-05  6.71E-05  1.37E-04  3.59E-04  6.50E-04
 
 OM33
+        3.21E-05 -1.84E-04  1.85E-04  1.37E-04  1.74E-04  8.22E-05  6.10E-04  3.16E-04 -1.33E-05 -4.09E-05  1.03E-04  3.92E-03
 
 OM34
+        2.69E-05 -4.59E-05 -4.46E-05 -8.15E-05  8.15E-05  6.52E-05  2.56E-04  2.55E-04  3.73E-05  6.40E-05  1.04E-04  1.52E-03
          1.37E-03
 
 OM44
+        1.68E-05  6.10E-05 -1.77E-04 -6.68E-06  5.89E-05  4.91E-05  1.44E-04  1.40E-04  1.08E-04  3.69E-05  2.08E-04  6.25E-04
          8.59E-04  1.43E-03
 
 SG11
+        5.12E-06  2.52E-05  2.44E-05  1.39E-05 -1.46E-05 -1.43E-05 -2.79E-05 -2.57E-05 -4.89E-05  2.63E-06 -1.35E-05 -1.36E-04
         -8.36E-05 -7.13E-05  4.36E-05
 
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
+        4.33E-02
 
 TH 2
+        1.15E-01  5.13E-02
 
 TH 3
+        1.89E-01  4.17E-02  6.84E-02
 
 TH 4
+        7.31E-02  1.38E-01  4.93E-01  5.22E-02
 
 OM11
+        4.27E-02  1.02E-01  7.11E-02  8.19E-02  2.75E-02
 
 OM12
+        6.46E-02  1.25E-01  1.30E-01  9.66E-02  1.86E-01  2.22E-02
 
 OM13
+        4.88E-02  1.09E-01 -1.51E-02  7.76E-02  3.39E-01  1.01E-01  2.95E-02
 
 OM14
+        5.65E-02  9.22E-02  8.25E-02  7.50E-02  1.80E-01  2.07E-01  5.23E-01  2.35E-02
 
 OM22
+       -5.33E-02 -1.47E-01 -1.70E-01 -1.16E-01 -4.46E-02  3.50E-02 -4.03E-02 -2.31E-02  3.44E-02
 
 OM23
+        3.95E-02  1.49E-01  2.44E-01  7.29E-02  6.31E-02  2.78E-01  2.87E-02  6.61E-02 -1.37E-01  3.64E-02
 
 OM24
+        4.34E-03 -2.56E-02  1.15E-01  1.10E-01  3.10E-02  1.04E-01  4.10E-02  1.12E-01  1.56E-01  3.86E-01  2.55E-02
 
 OM33
+        1.19E-02 -5.74E-02  4.32E-02  4.19E-02  1.01E-01  5.92E-02  3.30E-01  2.15E-01 -6.16E-03 -1.79E-02  6.48E-02  6.26E-02
 
 OM34
+        1.68E-02 -2.42E-02 -1.76E-02 -4.21E-02  8.01E-02  7.93E-02  2.34E-01  2.93E-01  2.93E-02  4.75E-02  1.11E-01  6.54E-01
          3.70E-02
 
 OM44
+        1.03E-02  3.15E-02 -6.84E-02 -3.39E-03  5.66E-02  5.85E-02  1.29E-01  1.58E-01  8.31E-02  2.69E-02  2.16E-01  2.64E-01
          6.14E-01  3.78E-02
 
 SG11
+        1.79E-02  7.44E-02  5.40E-02  4.04E-02 -8.02E-02 -9.76E-02 -1.43E-01 -1.66E-01 -2.15E-01  1.09E-02 -8.04E-02 -3.28E-01
         -3.42E-01 -2.86E-01  6.60E-03
 
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
+        5.64E+02
 
 TH 2
+       -5.03E+01  4.25E+02
 
 TH 3
+       -7.66E+01  3.31E+01  3.24E+02
 
 TH 4
+        2.40E+01 -6.26E+01 -1.96E+02  5.13E+02
 
 OM11
+       -2.41E+00 -3.72E+01 -3.38E+01 -9.07E+00  1.55E+03
 
 OM12
+       -3.74E+01 -7.00E+01 -2.32E+01 -4.84E+01 -2.82E+02  2.41E+03
 
 OM13
+       -3.26E+01 -5.19E+01  9.81E+01 -6.54E+01 -4.95E+02  1.32E+02  1.92E+03
 
 OM14
+       -6.45E+00 -4.23E+01 -8.31E+01  1.26E+01  7.79E+01 -4.21E+02 -1.11E+03  2.80E+03
 
 OM22
+        6.93E+00  6.11E+01  5.53E+01  3.32E+01  4.09E+01 -1.52E+02  1.52E+01  5.64E+01  9.99E+02
 
 OM23
+        2.48E+01 -9.17E+01 -1.17E+02  8.54E+01  1.56E+01 -4.00E+02 -5.42E+01  1.14E+02  1.93E+02  1.08E+03
 
 OM24
+        4.71E+00  8.24E+01 -5.59E+00 -1.02E+02 -1.14E+01  1.11E+02  6.85E+01 -2.28E+02 -3.17E+02 -6.17E+02  2.07E+03
 
 OM33
+        6.25E+00  2.46E+01 -2.26E+01 -2.93E+01  2.28E+01 -2.85E+01 -2.66E+02  1.61E+02  4.35E+01  7.72E+01 -7.10E+01  5.36E+02
 
 OM34
+       -1.03E+01  2.41E+01 -1.97E+00  9.77E+01 -7.98E+00  3.98E+01  2.01E+02 -5.16E+02  3.46E+00 -1.50E+02  1.83E+02 -6.65E+02
          2.16E+03
 
 OM44
+       -9.34E+00 -4.94E+01  4.67E+01 -5.09E+01 -7.52E+00 -2.67E+01 -7.16E+01  1.36E+02 -7.50E-01  1.05E+02 -3.42E+02  2.12E+02
         -9.86E+02  1.29E+03
 
 SG11
+       -4.76E+01 -1.90E+02 -6.43E+01 -6.09E+01  2.82E+02  4.37E+02 -6.27E+01  6.13E+02  1.08E+03  7.86E+01 -1.75E+02  6.93E+02
          3.19E+02  8.10E+02  2.89E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.97E-01  3.68E-01  4.49E-01  5.10E-01  6.38E-01  7.03E-01  8.08E-01  8.74E-01  8.89E-01  9.91E-01  1.08E+00  1.24E+00
          1.43E+00  2.02E+00  2.80E+00
 
1
 
 
 #TBLN:      4
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            2808
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      8
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     8
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example1.txt
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
 BURN-IN ITERATIONS (NBURN):                10000
 ITERATIONS (NITER):                        10000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       1556678
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
 SAMPLES FOR LOCAL SEARCH KERNEL (PSAMPLE_M2):           -1
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (PSAMPLE_M3):       1
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS
 SAMPLED OMEGAS:
 SAMPLE ACCEPTANCE RATE (OACCEPT):                       0.500000000000000
 SAMPLES FOR GLOBAL SEARCH KERNEL (OSAMPLE_M1):          -1
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           10
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):10
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
 iteration       -10000 MCMCOBJ=   -2347.12439035792     
 iteration        -9900 MCMCOBJ=   -2283.19544981755     
 iteration        -9800 MCMCOBJ=   -2282.87067425808     
 iteration        -9700 MCMCOBJ=   -2330.40730355132     
 iteration        -9600 MCMCOBJ=   -2257.08617886571     
 iteration        -9500 MCMCOBJ=   -2331.43752153458     
 iteration        -9400 MCMCOBJ=   -2241.74526952486     
 iteration        -9300 MCMCOBJ=   -2321.39087473685     
 iteration        -9200 MCMCOBJ=   -2335.77213099732     
 iteration        -9100 MCMCOBJ=   -2356.01920797067     
 iteration        -9000 MCMCOBJ=   -2247.98165208742     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -2249.66120707336     
 iteration          100 MCMCOBJ=   -2262.69913805080     
 iteration          200 MCMCOBJ=   -2406.60108100305     
 iteration          300 MCMCOBJ=   -2390.29097947600     
 iteration          400 MCMCOBJ=   -2295.10222475802     
 iteration          500 MCMCOBJ=   -2213.88198254150     
 iteration          600 MCMCOBJ=   -2382.73606447170     
 iteration          700 MCMCOBJ=   -2222.51067038064     
 iteration          800 MCMCOBJ=   -2280.78489814635     
 iteration          900 MCMCOBJ=   -2287.46419998762     
 iteration         1000 MCMCOBJ=   -2318.06176333062     
 iteration         1100 MCMCOBJ=   -2291.66457230681     
 iteration         1200 MCMCOBJ=   -2328.11901721349     
 iteration         1300 MCMCOBJ=   -2280.42754345164     
 iteration         1400 MCMCOBJ=   -2270.30750792118     
 iteration         1500 MCMCOBJ=   -2355.17671557576     
 iteration         1600 MCMCOBJ=   -2265.85500614630     
 iteration         1700 MCMCOBJ=   -2412.40046928168     
 iteration         1800 MCMCOBJ=   -2349.90117938168     
 iteration         1900 MCMCOBJ=   -2387.86703452151     
 iteration         2000 MCMCOBJ=   -2230.23658238109     
 iteration         2100 MCMCOBJ=   -2351.79045808044     
 iteration         2200 MCMCOBJ=   -2306.83010012282     
 iteration         2300 MCMCOBJ=   -2271.67609204096     
 iteration         2400 MCMCOBJ=   -2260.40452130351     
 iteration         2500 MCMCOBJ=   -2326.42436550857     
 iteration         2600 MCMCOBJ=   -2284.40424605644     
 iteration         2700 MCMCOBJ=   -2340.61458322244     
 iteration         2800 MCMCOBJ=   -2359.70544271340     
 iteration         2900 MCMCOBJ=   -2312.10267235123     
 iteration         3000 MCMCOBJ=   -2292.32786796979     
 iteration         3100 MCMCOBJ=   -2329.10906061734     
 iteration         3200 MCMCOBJ=   -2326.38473878466     
 iteration         3300 MCMCOBJ=   -2358.88646721367     
 iteration         3400 MCMCOBJ=   -2237.86493866656     
 iteration         3500 MCMCOBJ=   -2265.25304625152     
 iteration         3600 MCMCOBJ=   -2305.00505259540     
 iteration         3700 MCMCOBJ=   -2281.03908039057     
 iteration         3800 MCMCOBJ=   -2315.66291945290     
 iteration         3900 MCMCOBJ=   -2355.44216182833     
 iteration         4000 MCMCOBJ=   -2262.52800514039     
 iteration         4100 MCMCOBJ=   -2320.26550626297     
 iteration         4200 MCMCOBJ=   -2270.10629049581     
 iteration         4300 MCMCOBJ=   -2342.14318483522     
 iteration         4400 MCMCOBJ=   -2272.01942863315     
 iteration         4500 MCMCOBJ=   -2330.27052363352     
 iteration         4600 MCMCOBJ=   -2284.87666301155     
 iteration         4700 MCMCOBJ=   -2279.60216147808     
 iteration         4800 MCMCOBJ=   -2255.15941418189     
 iteration         4900 MCMCOBJ=   -2214.08177000488     
 iteration         5000 MCMCOBJ=   -2262.18654325962     
 iteration         5100 MCMCOBJ=   -2343.63358551192     
 iteration         5200 MCMCOBJ=   -2253.90064385119     
 iteration         5300 MCMCOBJ=   -2310.97970364940     
 iteration         5400 MCMCOBJ=   -2394.98920002703     
 iteration         5500 MCMCOBJ=   -2294.64240851607     
 iteration         5600 MCMCOBJ=   -2276.04808564855     
 iteration         5700 MCMCOBJ=   -2305.22931989662     
 iteration         5800 MCMCOBJ=   -2324.96049297659     
 iteration         5900 MCMCOBJ=   -2298.70378917790     
 iteration         6000 MCMCOBJ=   -2350.03618590831     
 iteration         6100 MCMCOBJ=   -2202.53827289637     
 iteration         6200 MCMCOBJ=   -2206.59181641394     
 iteration         6300 MCMCOBJ=   -2337.06707965068     
 iteration         6400 MCMCOBJ=   -2289.54113757341     
 iteration         6500 MCMCOBJ=   -2238.90554409893     
 iteration         6600 MCMCOBJ=   -2285.88902745137     
 iteration         6700 MCMCOBJ=   -2305.29365336870     
 iteration         6800 MCMCOBJ=   -2380.39879063670     
 iteration         6900 MCMCOBJ=   -2332.57223866766     
 iteration         7000 MCMCOBJ=   -2334.30523109682     
 iteration         7100 MCMCOBJ=   -2258.01089443448     
 iteration         7200 MCMCOBJ=   -2359.11714812754     
 iteration         7300 MCMCOBJ=   -2345.01039568104     
 iteration         7400 MCMCOBJ=   -2287.27321161319     
 iteration         7500 MCMCOBJ=   -2287.26986081311     
 iteration         7600 MCMCOBJ=   -2284.86917795907     
 iteration         7700 MCMCOBJ=   -2266.31488353397     
 iteration         7800 MCMCOBJ=   -2302.76322811530     
 iteration         7900 MCMCOBJ=   -2316.92377604999     
 iteration         8000 MCMCOBJ=   -2292.12466871036     
 iteration         8100 MCMCOBJ=   -2361.67862258893     
 iteration         8200 MCMCOBJ=   -2225.07445498976     
 iteration         8300 MCMCOBJ=   -2245.52599299301     
 iteration         8400 MCMCOBJ=   -2262.12296039692     
 iteration         8500 MCMCOBJ=   -2359.55718953507     
 iteration         8600 MCMCOBJ=   -2207.30632404265     
 iteration         8700 MCMCOBJ=   -2306.03153299366     
 iteration         8800 MCMCOBJ=   -2341.48701804107     
 iteration         8900 MCMCOBJ=   -2283.23732995617     
 iteration         9000 MCMCOBJ=   -2243.04152015721     
 iteration         9100 MCMCOBJ=   -2330.41477371269     
 iteration         9200 MCMCOBJ=   -2157.90730152193     
 iteration         9300 MCMCOBJ=   -2312.56708431041     
 iteration         9400 MCMCOBJ=   -2209.86035180316     
 iteration         9500 MCMCOBJ=   -2349.79339848250     
 iteration         9600 MCMCOBJ=   -2250.85080633192     
 iteration         9700 MCMCOBJ=   -2277.15181423386     
 iteration         9800 MCMCOBJ=   -2340.55795223117     
 iteration         9900 MCMCOBJ=   -2337.28516570623     
 iteration        10000 MCMCOBJ=   -2335.20138656211     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):          500
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    918.938533204673     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.43518222430     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1387.49664901963     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.43518222430     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1571.28435566057     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    71.2763539723735     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -2306.43518222430     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -2235.15882825193     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    98.00
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -2306.435       **************************************************
 #OBJS:********************************************       43.226 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.63E+00  1.56E+00  7.45E-01  2.35E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.75E-01
 
 ETA2
+       -2.03E-03  1.56E-01
 
 ETA3
+        1.05E-02 -6.16E-03  1.98E-01
 
 ETA4
+       -1.94E-02  1.52E-02  2.62E-02  1.65E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        5.91E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.17E-01
 
 ETA2
+       -1.49E-02  3.93E-01
 
 ETA3
+        4.99E-02 -3.32E-02  4.40E-01
 
 ETA4
+       -1.19E-01  9.16E-02  1.25E-01  4.04E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        2.43E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.46E-02  5.41E-02  6.85E-02  5.34E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.86E-02
 
 ETA2
+        2.28E-02  3.70E-02
 
 ETA3
+        2.87E-02  3.51E-02  6.03E-02
 
 ETA4
+        2.34E-02  2.64E-02  3.45E-02  3.78E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        7.04E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        3.37E-02
 
 ETA2
+        1.36E-01  4.61E-02
 
 ETA3
+        1.49E-01  1.94E-01  6.69E-02
 
 ETA4
+        1.38E-01  1.58E-01  1.72E-01  4.60E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.44E-02
 
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
+        1.99E-03
 
 TH 2
+        2.72E-04  2.92E-03
 
 TH 3
+        5.45E-04  1.31E-04  4.70E-03
 
 TH 4
+        1.51E-04  3.69E-04  1.69E-03  2.85E-03
 
 OM11
+        5.82E-05  1.49E-04  1.76E-04  1.39E-04  8.17E-04
 
 OM12
+        4.42E-05  1.02E-04  1.79E-04  1.08E-04  1.07E-04  5.22E-04
 
 OM13
+        6.65E-05  1.48E-04  5.43E-05  1.31E-04  2.14E-04  6.50E-05  8.25E-04
 
 OM14
+        4.96E-05  1.04E-04  1.73E-04  8.64E-05  7.24E-05  1.01E-04  3.07E-04  5.47E-04
 
 OM22
+       -8.63E-05 -3.16E-04 -3.79E-04 -1.59E-04 -5.87E-05  1.83E-05 -8.32E-05 -4.25E-05  1.37E-03
 
 OM23
+        6.89E-05  2.68E-04  5.57E-04  1.14E-04  7.69E-05  1.75E-04  8.68E-05  6.75E-05 -1.98E-04  1.23E-03
 
 OM24
+        7.92E-06 -5.68E-05  2.06E-04  1.81E-04  1.74E-05  1.69E-05  1.11E-05  4.12E-05  1.78E-04  2.79E-04  6.96E-04
 
 OM33
+        6.75E-05 -2.12E-04  2.34E-04  1.50E-04  1.19E-04  4.02E-05  4.48E-04  1.71E-04  4.43E-05 -2.36E-05  4.55E-05  3.63E-03
 
 OM34
+        2.95E-05 -4.23E-05 -3.23E-05 -1.09E-04  4.08E-05  4.19E-05  1.30E-04  1.44E-04  3.53E-05  7.26E-05  5.26E-05  1.19E-03
          1.19E-03
 
 OM44
+        2.60E-05  1.86E-05 -1.68E-04  1.89E-05  2.01E-05  8.59E-06  3.81E-05  3.35E-05  1.25E-04 -5.55E-07  2.05E-04  4.26E-04
          6.86E-04  1.43E-03
 
 SG11
+        2.77E-06  3.54E-05  2.91E-05  1.99E-05 -9.76E-06 -9.94E-06 -1.58E-05 -1.57E-05 -5.13E-05  6.73E-06 -9.18E-06 -1.12E-04
         -6.76E-05 -6.20E-05  4.96E-05
 
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
+        4.46E-02
 
 TH 2
+        1.13E-01  5.41E-02
 
 TH 3
+        1.78E-01  3.53E-02  6.85E-02
 
 TH 4
+        6.34E-02  1.28E-01  4.63E-01  5.34E-02
 
 OM11
+        4.56E-02  9.66E-02  8.99E-02  9.11E-02  2.86E-02
 
 OM12
+        4.33E-02  8.26E-02  1.14E-01  8.84E-02  1.63E-01  2.28E-02
 
 OM13
+        5.19E-02  9.56E-02  2.76E-02  8.54E-02  2.61E-01  9.90E-02  2.87E-02
 
 OM14
+        4.75E-02  8.23E-02  1.08E-01  6.92E-02  1.08E-01  1.89E-01  4.57E-01  2.34E-02
 
 OM22
+       -5.23E-02 -1.58E-01 -1.50E-01 -8.07E-02 -5.55E-02  2.17E-02 -7.83E-02 -4.91E-02  3.70E-02
 
 OM23
+        4.40E-02  1.41E-01  2.32E-01  6.10E-02  7.67E-02  2.19E-01  8.61E-02  8.22E-02 -1.52E-01  3.51E-02
 
 OM24
+        6.72E-03 -3.98E-02  1.14E-01  1.28E-01  2.31E-02  2.81E-02  1.47E-02  6.68E-02  1.83E-01  3.01E-01  2.64E-02
 
 OM33
+        2.51E-02 -6.52E-02  5.67E-02  4.66E-02  6.88E-02  2.92E-02  2.59E-01  1.21E-01  1.99E-02 -1.12E-02  2.86E-02  6.03E-02
 
 OM34
+        1.91E-02 -2.27E-02 -1.36E-02 -5.91E-02  4.13E-02  5.32E-02  1.31E-01  1.78E-01  2.76E-02  6.00E-02  5.78E-02  5.73E-01
          3.45E-02
 
 OM44
+        1.54E-02  9.12E-03 -6.49E-02  9.38E-03  1.86E-02  9.94E-03  3.51E-02  3.79E-02  8.90E-02 -4.18E-04  2.05E-01  1.87E-01
          5.25E-01  3.78E-02
 
 SG11
+        8.81E-03  9.30E-02  6.03E-02  5.30E-02 -4.85E-02 -6.18E-02 -7.83E-02 -9.52E-02 -1.97E-01  2.72E-02 -4.94E-02 -2.65E-01
         -2.78E-01 -2.33E-01  7.04E-03
 
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
+        5.27E+02
 
 TH 2
+       -4.67E+01  3.78E+02
 
 TH 3
+       -6.77E+01  2.98E+01  3.07E+02
 
 TH 4
+        2.07E+01 -5.57E+01 -1.72E+02  4.76E+02
 
 OM11
+       -8.59E+00 -4.18E+01 -3.11E+01 -1.92E+01  1.36E+03
 
 OM12
+       -1.60E+01 -3.02E+01 -2.15E+01 -5.16E+01 -2.25E+02  2.16E+03
 
 OM13
+       -2.73E+01 -3.18E+01  7.21E+01 -5.42E+01 -3.51E+02  5.94E+01  1.76E+03
 
 OM14
+       -9.41E-01 -4.19E+01 -8.09E+01  1.45E+01  9.14E+01 -3.52E+02 -9.24E+02  2.50E+03
 
 OM22
+        7.54E+00  5.60E+01  4.92E+01  1.72E+01  3.39E+01 -1.19E+02  4.99E+01  4.43E+01  8.60E+02
 
 OM23
+        1.45E+01 -8.03E+01 -1.09E+02  7.57E+01  2.61E+00 -3.10E+02 -1.00E+02  8.41E+01  1.72E+02  1.06E+03
 
 OM24
+        3.37E+00  6.34E+01 -1.90E+01 -1.01E+02 -2.38E+01  1.49E+02  6.52E+01 -1.70E+02 -2.95E+02 -4.82E+02  1.83E+03
 
 OM33
+       -3.67E+00  2.59E+01 -2.12E+01 -3.32E+01  8.01E+00  5.05E+00 -2.21E+02  1.20E+02  4.81E+00  6.09E+01 -3.71E+01  4.71E+02
 
 OM34
+        1.54E+00  7.66E-01 -2.44E+00  1.01E+02 -1.03E+01 -1.13E+01  1.72E+02 -3.67E+02  1.64E+01 -1.54E+02  1.67E+02 -5.15E+02
          1.84E+03
 
 OM44
+       -1.70E+01 -2.77E+01  4.22E+01 -5.42E+01 -2.92E-01  1.37E+00 -4.21E+01  1.39E+02 -5.82E+00  1.00E+02 -3.12E+02  1.34E+02
         -7.33E+02  1.09E+03
 
 SG11
+        3.99E-01 -1.94E+02 -7.79E+01 -9.46E+01  2.30E+02  2.96E+02 -2.46E+01  4.62E+02  7.74E+02  2.16E+01 -1.49E+02  5.00E+02
          3.79E+02  6.36E+02  2.38E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           EIGENVALUES OF COR MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         2.65E-01  4.11E-01  5.07E-01  5.51E-01  7.11E-01  7.53E-01  8.63E-01  9.21E-01  9.43E-01  9.93E-01  1.11E+00  1.21E+00
          1.41E+00  2.00E+00  2.36E+00
 
1
 
 
 #TBLN:      5
 #METH: First Order Conditional Estimation with Interaction (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
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
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      10
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     10
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example1.ext
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -1115.61465932764        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  1.6345E+00  1.5558E+00  7.4462E-01  2.3484E+00  1.7505E-01 -2.0338E-03  1.0521E-02 -1.9384E-02  1.5626E-01 -6.1561E-03
             1.5209E-02  1.9789E-01  2.6168E-02  1.6511E-01  5.9072E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01 -1.0000E-01
             1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:  -7.4229E+01 -6.4966E+01 -1.3139E+01 -3.4738E+01  1.3992E+01  1.1533E-01 -3.6177E-01 -2.6241E+00  1.1612E+01 -1.1897E+00
             8.2945E-01  1.0850E+01 -1.2155E+01  1.7853E+01  2.6236E+01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -1119.94863941808        NO. OF FUNC. EVALS.:  46
 CUMULATIVE NO. OF FUNC. EVALS.:       53
 NPARAMETR:  1.6879E+00  1.6164E+00  8.2353E-01  2.3800E+00  1.5706E-01 -1.9297E-03  1.0538E-02 -1.4048E-02  1.3650E-01 -5.1917E-03
             1.2060E-02  1.8565E-01  4.0003E-02  1.4964E-01  5.4495E-02
 PARAMETER:  1.3213E-01  1.3823E-01  2.0085E-01  1.1335E-01  4.5766E-02 -1.0017E-01  1.0575E-01 -7.6514E-02  3.2390E-02 -8.9764E-02
             8.4879E-02  6.7839E-02  1.5313E-01  3.3988E-02  5.9679E-02
 GRADIENT:   5.8635E-01  1.8875E+01  1.4478E+01 -3.8717E+01 -8.0759E+00  1.5217E-02 -6.5154E-02 -1.6533E+00  3.2008E-01 -2.5167E+00
             2.2425E+00 -7.7928E+00  8.2239E+00 -7.1439E+00 -1.5786E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -1120.33699901568        NO. OF FUNC. EVALS.:  41
 CUMULATIVE NO. OF FUNC. EVALS.:       94
 NPARAMETR:  1.6884E+00  1.6186E+00  8.3726E-01  2.3869E+00  1.6365E-01 -2.2582E-03  2.0239E-02 -6.8775E-03  1.3085E-01  1.1460E-02
             4.6783E-03  1.9028E-01  3.5436E-02  1.4204E-01  5.5907E-02
 PARAMETER:  1.3243E-01  1.3961E-01  2.1740E-01  1.1627E-01  6.6324E-02 -1.1484E-01  1.9896E-01 -3.6696E-02  1.1233E-02  2.1262E-01
             3.3427E-02  7.3139E-02  1.3203E-01  2.1580E-02  7.2468E-02
 GRADIENT:  -2.7249E+00  1.0873E+01  1.3748E+01 -2.9636E+01 -4.1718E+00 -2.6935E-01  1.9044E+00  4.2661E+00  3.9425E-01 -1.2479E-01
            -5.4271E+00 -3.7524E+00  3.2805E+00 -7.4493E+00 -9.0272E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -1120.72011273228        NO. OF FUNC. EVALS.:  40
 CUMULATIVE NO. OF FUNC. EVALS.:      134
 NPARAMETR:  1.6865E+00  1.6151E+00  8.3176E-01  2.3870E+00  1.6014E-01 -1.5553E-03  6.5821E-03 -1.6306E-02  1.2826E-01  2.6525E-02
             1.2388E-02  1.7987E-01  2.6854E-02  1.3908E-01  5.7709E-02
 PARAMETER:  1.3135E-01  1.3741E-01  2.1081E-01  1.1632E-01  5.5470E-02 -7.9955E-02  6.5412E-02 -8.7950E-02  1.2664E-03  4.8639E-01
             9.0083E-02  3.8119E-02  9.5244E-02  1.3969E-02  8.8327E-02
 GRADIENT:  -7.3731E-01  8.9004E+00  5.2865E+00 -1.6697E+01 -4.0316E+00 -4.6981E-02 -5.2382E-01 -1.7508E-01 -4.6928E-01  1.1156E+00
            -1.9857E+00 -1.2188E+00  1.3720E+00 -5.0813E+00 -4.7632E+00
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -1121.02828261429        NO. OF FUNC. EVALS.:  47
 CUMULATIVE NO. OF FUNC. EVALS.:      181
 NPARAMETR:  1.6867E+00  1.6112E+00  8.1913E-01  2.3911E+00  1.6502E-01 -7.8095E-04  1.2357E-02 -1.2789E-02  1.3144E-01  1.5906E-02
             1.3869E-02  1.8736E-01  3.3181E-02  1.4985E-01  5.7170E-02
 PARAMETER:  1.3147E-01  1.3501E-01  1.9548E-01  1.1804E-01  7.0501E-02 -3.9549E-02  1.2097E-01 -6.7952E-02  1.3579E-02  2.8847E-01
             1.0047E-01  6.7159E-02  1.2018E-01  4.6904E-02  8.3638E-02
 GRADIENT:  -2.3763E-01 -1.5356E-02 -5.1818E-02 -6.9119E-01  2.9783E-03  2.0554E-04 -1.8054E-03 -7.1871E-04  8.8317E-04  1.2130E-03
            -1.8691E-04 -4.8486E-04 -1.5243E-03 -1.1093E-04  1.0616E-02
 
0ITERATION NO.:   23    OBJECTIVE VALUE:  -1121.02837234299        NO. OF FUNC. EVALS.:  42
 CUMULATIVE NO. OF FUNC. EVALS.:      223
 NPARAMETR:  1.6869E+00  1.6113E+00  8.1960E-01  2.3916E+00  1.6506E-01 -7.4185E-04  1.2411E-02 -1.2736E-02  1.3143E-01  1.5956E-02
             1.3906E-02  1.8755E-01  3.3270E-02  1.4991E-01  5.7163E-02
 PARAMETER:  1.3158E-01  1.3506E-01  1.9606E-01  1.1824E-01  7.0610E-02 -3.7565E-02  1.2149E-01 -6.7665E-02  1.3530E-02  2.8934E-01
             1.0077E-01  6.7599E-02  1.2042E-01  4.7031E-02  8.3577E-02
 GRADIENT:   1.5088E-03 -5.6436E-04 -3.9919E-05  2.7438E-04 -2.6903E-04  2.4822E-05  1.9081E-04 -6.7215E-04  2.3228E-04  1.0964E-05
            -1.5163E-04 -3.4509E-04  5.7823E-05  4.3268E-04 -3.5372E-04
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      223
 NO. OF SIG. DIGITS IN FINAL EST.:  4.2

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.6566E-06 -1.1916E-02 -3.4864E-03 -1.1596E-02
 SE:             3.9193E-02  2.8850E-02  3.2735E-02  3.3062E-02
 N:                     100         100         100         100
 
 P VAL.:         9.9997E-01  6.7959E-01  9.1518E-01  7.2580E-01
 
 ETASHRINKSD(%)  3.0463E+00  2.0021E+01  2.4030E+01  1.4177E+01
 ETASHRINKVR(%)  5.9998E+00  3.6033E+01  4.2286E+01  2.6345E+01
 EBVSHRINKSD(%)  3.3162E+00  1.9861E+01  2.4449E+01  1.5063E+01
 EBVSHRINKVR(%)  6.5225E+00  3.5777E+01  4.2921E+01  2.7858E+01
 EPSSHRINKSD(%)  3.0941E+01
 EPSSHRINKVR(%)  5.2309E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):          500
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    918.938533204673     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -1121.02837234299     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -202.089839138315     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:     3.58
 Elapsed covariance  time in seconds:     3.27
 Elapsed postprocess time in seconds:     0.37
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
 
         1.69E+00  1.61E+00  8.20E-01  2.39E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.65E-01
 
 ETA2
+       -7.42E-04  1.31E-01
 
 ETA3
+        1.24E-02  1.60E-02  1.88E-01
 
 ETA4
+       -1.27E-02  1.39E-02  3.33E-02  1.50E-01
 


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
+       -5.04E-03  3.63E-01
 
 ETA3
+        7.05E-02  1.02E-01  4.33E-01
 
 ETA4
+       -8.10E-02  9.91E-02  1.98E-01  3.87E-01
 


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
 
         4.32E-02  4.77E-02  7.03E-02  5.15E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.71E-02
 
 ETA2
+        2.13E-02  3.13E-02
 
 ETA3
+        3.00E-02  3.61E-02  7.58E-02
 
 ETA4
+        2.35E-02  2.35E-02  4.47E-02  3.91E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        8.11E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        3.33E-02
 
 ETA2
+        1.45E-01  4.32E-02
 
 ETA3
+        1.63E-01  2.34E-01  8.76E-02
 
 ETA4
+        1.55E-01  1.63E-01  2.17E-01  5.04E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.69E-02
 
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
+        1.87E-03
 
 TH 2
+        2.50E-04  2.27E-03
 
 TH 3
+        5.95E-04  2.18E-04  4.94E-03
 
 TH 4
+        2.43E-04  3.09E-04  1.94E-03  2.66E-03
 
 OM11
+        7.15E-05  1.37E-04  1.88E-04  1.60E-04  7.32E-04
 
 OM12
+        7.69E-05  1.29E-04  2.47E-04  1.65E-04  1.35E-04  4.53E-04
 
 OM13
+        1.06E-04  1.88E-04  1.34E-05  2.03E-04  2.66E-04  1.45E-04  8.98E-04
 
 OM14
+        9.04E-05  1.32E-04  2.16E-04  2.05E-04  1.42E-04  1.36E-04  4.07E-04  5.54E-04
 
 OM22
+       -1.02E-05  3.92E-05 -2.69E-04 -7.85E-05  1.49E-05  8.91E-05  8.59E-05  5.28E-05  9.81E-04
 
 OM23
+        6.76E-05  9.87E-05  8.39E-04  2.38E-04  3.97E-05  1.47E-04 -9.94E-05 -7.74E-06 -9.22E-05  1.30E-03
 
 OM24
+        2.94E-05 -1.52E-05  3.04E-04  1.80E-04  3.65E-05  5.58E-05  4.79E-05  9.65E-05  1.37E-04  3.19E-04  5.52E-04
 
 OM33
+        2.32E-04  1.99E-05  7.97E-04  6.54E-04  3.29E-04  3.11E-04  9.79E-04  5.99E-04  3.25E-04 -2.04E-04  2.38E-04  5.75E-03
 
 OM34
+        1.42E-04  6.95E-05  2.84E-04  2.87E-04  1.84E-04  1.84E-04  5.17E-04  4.05E-04  2.23E-04 -1.49E-04  2.14E-04  2.74E-03
          2.00E-03
 
 OM44
+        9.46E-05  1.39E-04  8.10E-05  2.07E-04  1.29E-04  1.30E-04  3.14E-04  2.63E-04  1.95E-04 -9.85E-05  2.46E-04  1.42E-03
          1.24E-03  1.53E-03
 
 SG11
+        1.15E-05  9.60E-06  1.28E-05 -1.15E-05 -3.29E-05 -3.64E-05 -6.82E-05 -5.17E-05 -7.03E-05  5.77E-05 -2.54E-05 -3.33E-04
         -1.96E-04 -1.40E-04  6.57E-05
 
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
+        1.21E-01  4.77E-02
 
 TH 3
+        1.96E-01  6.52E-02  7.03E-02
 
 TH 4
+        1.09E-01  1.26E-01  5.35E-01  5.15E-02
 
 OM11
+        6.11E-02  1.06E-01  9.89E-02  1.14E-01  2.71E-02
 
 OM12
+        8.36E-02  1.28E-01  1.65E-01  1.50E-01  2.34E-01  2.13E-02
 
 OM13
+        8.21E-02  1.31E-01  6.37E-03  1.31E-01  3.28E-01  2.28E-01  3.00E-02
 
 OM14
+        8.89E-02  1.18E-01  1.31E-01  1.69E-01  2.23E-01  2.71E-01  5.77E-01  2.35E-02
 
 OM22
+       -7.57E-03  2.63E-02 -1.22E-01 -4.87E-02  1.76E-02  1.34E-01  9.15E-02  7.16E-02  3.13E-02
 
 OM23
+        4.34E-02  5.74E-02  3.31E-01  1.28E-01  4.07E-02  1.91E-01 -9.19E-02 -9.12E-03 -8.16E-02  3.61E-02
 
 OM24
+        2.89E-02 -1.36E-02  1.84E-01  1.49E-01  5.75E-02  1.12E-01  6.81E-02  1.74E-01  1.86E-01  3.77E-01  2.35E-02
 
 OM33
+        7.07E-02  5.51E-03  1.50E-01  1.67E-01  1.60E-01  1.93E-01  4.31E-01  3.36E-01  1.37E-01 -7.47E-02  1.34E-01  7.58E-02
 
 OM34
+        7.35E-02  3.26E-02  9.05E-02  1.25E-01  1.53E-01  1.93E-01  3.86E-01  3.85E-01  1.60E-01 -9.25E-02  2.04E-01  8.08E-01
          4.47E-02
 
 OM44
+        5.61E-02  7.47E-02  2.95E-02  1.03E-01  1.23E-01  1.57E-01  2.68E-01  2.86E-01  1.59E-01 -7.00E-02  2.68E-01  4.80E-01
          7.10E-01  3.91E-02
 
 SG11
+        3.29E-02  2.49E-02  2.25E-02 -2.75E-02 -1.50E-01 -2.11E-01 -2.81E-01 -2.71E-01 -2.77E-01  1.97E-01 -1.34E-01 -5.41E-01
         -5.41E-01 -4.42E-01  8.11E-03
 
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
+        5.72E+02
 
 TH 2
+       -4.83E+01  4.73E+02
 
 TH 3
+       -7.06E+01  7.43E+00  3.41E+02
 
 TH 4
+        1.29E+01 -4.65E+01 -2.14E+02  5.54E+02
 
 OM11
+       -1.16E+01 -4.02E+01 -3.27E+01 -1.63E+01  1.60E+03
 
 OM12
+       -4.42E+01 -7.52E+01 -4.16E+01 -5.16E+01 -2.88E+02  2.75E+03
 
 OM13
+       -3.66E+01 -6.88E+01  1.19E+02 -6.77E+01 -4.17E+02 -9.40E+01  2.04E+03
 
 OM14
+       -1.68E+01 -4.42E+01 -8.84E+01 -2.72E+01  1.05E+01 -3.67E+02 -1.18E+03  3.03E+03
 
 OM22
+       -1.35E+01 -3.24E+01  6.88E+01  1.36E+01  4.94E+01 -2.05E+02 -1.88E+01  6.68E+01  1.18E+03
 
 OM23
+        2.69E+01 -4.11E+01 -1.54E+02  6.72E+01 -3.19E+01 -3.81E+02  7.85E+01  7.04E+01  1.02E+02  1.14E+03
 
 OM24
+        6.11E+00  8.78E+01 -2.83E+01 -8.31E+01  3.78E-01  1.93E+02  5.52E+01 -2.95E+02 -3.45E+02 -6.89E+02  2.54E+03
 
 OM33
+       -5.25E+00  2.57E+01 -4.92E+01 -3.32E+01  2.17E+01  1.96E+00 -2.57E+02  1.37E+02  9.08E-01 -2.30E+01  5.57E+01  6.09E+02
 
 OM34
+       -2.48E+01  5.39E+00  8.17E+00  4.93E+01  4.47E+00 -7.35E+00  1.10E+02 -3.96E+02  3.14E+00  5.45E+01 -6.16E+01 -8.41E+02
          2.44E+03
 
 OM44
+       -1.67E+01 -6.07E+01  3.37E+01 -4.54E+01 -1.40E+01 -3.15E+01 -3.14E+01  4.78E+01  4.19E+00  6.30E+01 -3.39E+02  2.25E+02
         -1.11E+03  1.49E+03
 
 SG11
+       -3.31E+02 -1.86E+02 -3.27E+01 -1.80E+02  4.03E+02  1.12E+03 -1.97E+02  4.85E+02  1.01E+03 -1.02E+03  5.06E+02  9.50E+02
          3.94E+02  8.09E+02  2.62E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9        10        11        12
             13        14        15
 
         1.27E-01  3.40E-01  3.76E-01  4.44E-01  5.06E-01  5.71E-01  7.59E-01  8.21E-01  8.70E-01  8.97E-01  1.01E+00  1.19E+00
          1.36E+00  2.00E+00  3.74E+00
 
 Elapsed finaloutput time in seconds:     0.79
 #CPUT: Total CPU Time in Seconds,      133.568
Stop Time: 
Sat 04/22/2017 
09:13 AM

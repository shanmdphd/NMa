Sun 10/30/2016 
10:20 PM
;Model Desc: Receptor Mediated Clearance model with Dynamic Change in Receptors
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# example6 (from r2compl)
$INPUT C SET ID JID TIME DV=CONC DOSE=AMT RATE EVID MDV CMT
$DATA example6.csv IGNORE=C

; The new numerical integration solver is used, although ADVAN=9 is also efficient
; for this problem.
$SUBROUTINES ADVAN13 TRANS1 TOL=3
$MODEL NCOMPARTMENTS=3


$PK
include nonmem_reserved_general
MUFIRSTREC=1
OBJQUICK=2
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
MU_4=THETA(4)
MU_5=THETA(5)
MU_6=THETA(6)
MU_7=THETA(7)
MU_8=THETA(8)
VC=EXP(MU_1+ETA(1))
K10=EXP(MU_2+ETA(2))
K12=EXP(MU_3+ETA(3))
K21=EXP(MU_4+ETA(4))
VM=EXP(MU_5+ETA(5))
KMC=EXP(MU_6+ETA(6))
K03=EXP(MU_7+ETA(7))
K30=EXP(MU_8+ETA(8))
S3=VC
S1=VC
KM=KMC*S1
F3=K03/K30

$DES
DADT(1) = -(K10+K12)*A(1) + K21*A(2) - VM*A(1)*A(3)/(A(1)+KM)
DADT(2) = K12*A(1) - K21*A(2)
DADT(3) =  -VM*A(1)*A(3)/(A(1)+KM) - K30*A(3) + K03

$ERROR
CALLFL=0
ETYPE=1
IF(CMT.NE.1) ETYPE=0
IPRED=F
Y = F + F*ETYPE*EPS(1) + F*(1.0-ETYPE)*EPS(2)


;Initial Thetas
$THETA
( 4.0 )  ;[MU_1]
( -2 ) ;[MU_2]
( 1.0 )  ;[MU_3]
( -0.2 );[MU_4]      
( 2.2 ) ;[MU_5]
( 0.5 )  ;[MU_6]
( 4.0 )  ;[MU_7]
( -0.8) ;[MU_8]

;Initial Omegas
$OMEGA BLOCK(8) VALUES(0.8,0.001)

$SIGMA  
0.1 ;[p]
0.1 ;[p]

$PRIOR NWPRI
$OMEGAP BLOCK(8) FIXED VALUES(0.2,0.0)
$OMEGAPD 8.0 FIXED

$EST METHOD=ITS INTERACTION SIGL=4 NITER=15 PRINT=1 NOABORT NOPRIOR=1 file=example6hmto7_its.ext
$EST METHOD=bayes INTERACTION NBURN=2000 NITER=0 PRINT=10 MASSRESET=1 NOPRIOR=0 file=example6hmto17_bayes.ext
$EST METHOD=NUTS PRINT=5 OLKJDF=8.0  file=example6hmto17.ext
     MASSRESET=0 NUTS_INIT=10 NUTS_BASE=100 NBURN=1000 NITER=2000 PRINT=5
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       30 OCT 2016
Days until program expires :4962
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha12 (nm74a12)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 RUN# example6 (from r2compl)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     1750
 NO. OF DATA ITEMS IN DATA SET:  11
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   5   7   8   0   0  11   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID JID TIME CONC DOSE RATE EVID MDV CMT
0FORMAT FOR DATA:
 (2E2.0,2E3.0,E5.0,E10.0,2E5.0,3E2.0)

 TOT. NO. OF OBS RECS:     1568
 TOT. NO. OF INDIVIDUALS:     50
0LENGTH OF THETA:   9
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
  1  1  1  1
  1  1  1  1  1
  1  1  1  1  1  1
  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1
  0  0  0  0  0  0  0  0  2
  0  0  0  0  0  0  0  0  2  2
  0  0  0  0  0  0  0  0  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2  2  2  2  2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.4000E+01     0.1000E+07
 -0.1000E+07    -0.2000E+01     0.1000E+07
 -0.1000E+07     0.1000E+01     0.1000E+07
 -0.1000E+07    -0.2000E+00     0.1000E+07
 -0.1000E+07     0.2200E+01     0.1000E+07
 -0.1000E+07     0.5000E+00     0.1000E+07
 -0.1000E+07     0.4000E+01     0.1000E+07
 -0.1000E+07    -0.8000E+00     0.1000E+07
  0.8000E+01     0.8000E+01     0.8000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.8000E+00
                  0.1000E-02   0.8000E+00
                  0.1000E-02   0.1000E-02   0.8000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.8000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.8000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.8000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.8000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.8000E+00
        2                                                                                  YES
                  0.2000E+00
                  0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+00
 0.0000E+00   0.1000E+00
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha12 (nm74a12)

 GENERAL NONLINEAR KINETICS MODEL WITH STIFF/NONSTIFF EQUATIONS (ADVAN13)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   7
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         COMP 1       ON         YES        YES        YES        YES
    2         COMP 2       ON         YES        YES        NO         NO
    3         COMP 3       ON         YES        YES        NO         NO
    4         OUTPUT       OFF        YES        NO         NO         NO
 INITIAL (BASE) TOLERANCE SETTINGS:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   3
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            9           *           *           *           *
    2            *           *           *           *           *
    3            8          10           *           *           *
    4            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      9
   TIME DATA ITEM IS DATA ITEM NO.:          5
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   7
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     8
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:   11

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONLY WITH OBSERVATION EVENTS.
0DES SUBROUTINE USES COMPACT STORAGE MODE.
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
 NO. OF FUNCT. EVALS. ALLOWED:            3480
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
 RAW OUTPUT FILE (FILE): example6hmto7_its.ext
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
 CONVERGENCE TYPE (CTYPE):                  0
 ITERATIONS (NITER):                        15
 ANEAL SETTING (CONSTRAIN):                 1

 TOLERANCES FOR ESTIMATION/EVALUATION STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   3
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 TOLERANCES FOR COVARIANCE STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   3
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3   4   5   6   7   8
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -3179.40854899940
 iteration            1 OBJ=  -3563.43649683721
 iteration            2 OBJ=  -3698.51255720750
 iteration            3 OBJ=  -3813.48535115036
 iteration            4 OBJ=  -3921.23991413119
 iteration            5 OBJ=  -4025.61250136026
 iteration            6 OBJ=  -4127.59016738704
 iteration            7 OBJ=  -4227.44402452288
 iteration            8 OBJ=  -4325.13122278257
 iteration            9 OBJ=  -4419.69835025816
 iteration           10 OBJ=  -4509.82354337799
 iteration           11 OBJ=  -4592.15272667097
 iteration           12 OBJ=  -4659.77970973961
 iteration           13 OBJ=  -4700.13242868036
 iteration           14 OBJ=  -4709.66476414640
 iteration           15 OBJ=  -4710.58529567319
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -9.0322E-04 -1.5167E-03  2.3703E-03  1.3403E-03  1.1871E-03  1.6827E-03  3.1278E-04  8.4618E-04
 SE:             6.9351E-02  5.3992E-02  3.7877E-02  6.5130E-02  5.6768E-02  5.7458E-02  6.4411E-02  6.1392E-02
 N:                      50          50          50          50          50          50          50          50
 
 P VAL.:         9.8961E-01  9.7759E-01  9.5010E-01  9.8358E-01  9.8332E-01  9.7664E-01  9.9613E-01  9.8900E-01
 
 ETASHRINKSD(%)  6.4209E-01  4.4464E+00  8.2117E+00  1.6393E+00  1.4947E+00  5.8524E+00  3.9120E-01  1.5694E+00
 ETASHRINKVR(%)  1.2801E+00  8.6951E+00  1.5749E+01  3.2517E+00  2.9671E+00  1.1362E+01  7.8086E-01  3.1141E+00
 EBVSHRINKSD(%)  6.3620E-01  5.3894E+00  9.8841E+00  2.1069E+00  1.5737E+00  6.3438E+00  4.5704E-01  1.7629E+00
 EBVSHRINKVR(%)  1.2684E+00  1.0488E+01  1.8791E+01  4.1694E+00  3.1226E+00  1.2285E+01  9.1200E-01  3.4947E+00
 EPSSHRINKSD(%)  1.5708E+01  7.2308E+00
 EPSSHRINKVR(%)  2.8948E+01  1.3939E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -4710.58529567319     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1828.79405554334     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:    42.62
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -4710.585       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.20E+00  5.57E-01 -1.86E-01  2.26E+00  2.14E-01  3.71E+00 -7.07E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.49E-01
 
 ETA2
+       -3.80E-02  1.63E-01
 
 ETA3
+        4.71E-02 -1.53E-02  8.69E-02
 
 ETA4
+        3.18E-02  5.12E-02 -2.08E-02  2.24E-01
 
 ETA5
+        2.73E-02  2.62E-02 -2.34E-03 -3.35E-02  1.69E-01
 
 ETA6
+       -2.73E-02  8.31E-03  2.64E-02  1.84E-02 -8.03E-02  1.90E-01
 
 ETA7
+        2.97E-02 -3.96E-02  3.17E-02 -7.25E-02  2.44E-02  4.82E-03  2.13E-01
 
 ETA8
+        9.81E-02  8.20E-02  3.57E-02  4.41E-02  1.00E-03 -5.14E-02  5.56E-02  1.98E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.27E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        4.99E-01
 
 ETA2
+       -1.89E-01  4.04E-01
 
 ETA3
+        3.21E-01 -1.29E-01  2.95E-01
 
 ETA4
+        1.35E-01  2.68E-01 -1.49E-01  4.73E-01
 
 ETA5
+        1.33E-01  1.58E-01 -1.93E-02 -1.72E-01  4.12E-01
 
 ETA6
+       -1.26E-01  4.72E-02  2.05E-01  8.95E-02 -4.47E-01  4.36E-01
 
 ETA7
+        1.29E-01 -2.13E-01  2.33E-01 -3.32E-01  1.28E-01  2.39E-02  4.62E-01
 
 ETA8
+        4.42E-01  4.56E-01  2.72E-01  2.09E-01  5.47E-03 -2.64E-01  2.70E-01  4.46E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.63E-02
 
 EPS2
+        0.00E+00  1.50E-01
 
1
 
 
 #TBLN:      2
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            3480
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      4
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     4
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmto17_bayes.ext
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
 CONVERGENCE TYPE (CTYPE):                  0
 KEEP ITERATIONS (THIN):            1
 BURN-IN ITERATIONS (NBURN):                2000
 ITERATIONS (NITER):                        0
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
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      1
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
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):0
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000

 TOLERANCES FOR ESTIMATION/EVALUATION STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   3
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 TOLERANCES FOR COVARIANCE STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   3
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3   4   5   6   7   8
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3   4   5   6   7   8
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1   2
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -2000 MCMCOBJ=   -6805.72386568691     
 iteration        -1990 MCMCOBJ=   -6637.77391412957     
 iteration        -1980 MCMCOBJ=   -6679.48098122220     
 iteration        -1970 MCMCOBJ=   -6652.28026304804     
 iteration        -1960 MCMCOBJ=   -6637.01488849883     
 iteration        -1950 MCMCOBJ=   -6599.97050754232     
 iteration        -1940 MCMCOBJ=   -6603.91460399292     
 iteration        -1930 MCMCOBJ=   -6653.97733564795     
 iteration        -1920 MCMCOBJ=   -6637.73091232055     
 iteration        -1910 MCMCOBJ=   -6606.01291345995     
 iteration        -1900 MCMCOBJ=   -6567.57485741434     
 iteration        -1890 MCMCOBJ=   -6636.03959078413     
 iteration        -1880 MCMCOBJ=   -6579.39495770189     
 iteration        -1870 MCMCOBJ=   -6582.78374736529     
 iteration        -1860 MCMCOBJ=   -6630.69562762347     
 iteration        -1850 MCMCOBJ=   -6556.32722559013     
 iteration        -1840 MCMCOBJ=   -6598.90053857063     
 iteration        -1830 MCMCOBJ=   -6568.98139316245     
 iteration        -1820 MCMCOBJ=   -6546.48354969024     
 iteration        -1810 MCMCOBJ=   -6542.58419022934     
 iteration        -1800 MCMCOBJ=   -6590.73693299949     
 iteration        -1790 MCMCOBJ=   -6507.76507590008     
 iteration        -1780 MCMCOBJ=   -6531.87605220450     
 iteration        -1770 MCMCOBJ=   -6531.52577778506     
 iteration        -1760 MCMCOBJ=   -6594.51343658977     
 iteration        -1750 MCMCOBJ=   -6569.23166030245     
 iteration        -1740 MCMCOBJ=   -6569.06971953203     
 iteration        -1730 MCMCOBJ=   -6548.22886835046     
 iteration        -1720 MCMCOBJ=   -6559.07844878398     
 iteration        -1710 MCMCOBJ=   -6549.04289869389     
 iteration        -1700 MCMCOBJ=   -6594.75479501978     
 iteration        -1690 MCMCOBJ=   -6519.91204798686     
 iteration        -1680 MCMCOBJ=   -6550.84263626279     
 iteration        -1670 MCMCOBJ=   -6539.10527553365     
 iteration        -1660 MCMCOBJ=   -6560.18674471656     
 iteration        -1650 MCMCOBJ=   -6576.71290967496     
 iteration        -1640 MCMCOBJ=   -6519.63883366491     
 iteration        -1630 MCMCOBJ=   -6504.20933610337     
 iteration        -1620 MCMCOBJ=   -6513.46838406694     
 iteration        -1610 MCMCOBJ=   -6514.61587483261     
 iteration        -1600 MCMCOBJ=   -6532.75208045516     
 iteration        -1590 MCMCOBJ=   -6546.99742641948     
 iteration        -1580 MCMCOBJ=   -6559.79685184922     
 iteration        -1570 MCMCOBJ=   -6529.90481975707     
 iteration        -1560 MCMCOBJ=   -6485.30909615131     
 iteration        -1550 MCMCOBJ=   -6496.87768784148     
 iteration        -1540 MCMCOBJ=   -6564.80540512985     
 iteration        -1530 MCMCOBJ=   -6500.23680840953     
 iteration        -1520 MCMCOBJ=   -6555.40320146390     
 iteration        -1510 MCMCOBJ=   -6567.01095933145     
 iteration        -1500 MCMCOBJ=   -6493.67288584466     
 iteration        -1490 MCMCOBJ=   -6486.87557357271     
 iteration        -1480 MCMCOBJ=   -6543.64096741262     
 iteration        -1470 MCMCOBJ=   -6496.53772302394     
 iteration        -1460 MCMCOBJ=   -6497.03116395080     
 iteration        -1450 MCMCOBJ=   -6521.15860383625     
 iteration        -1440 MCMCOBJ=   -6536.35237544514     
 iteration        -1430 MCMCOBJ=   -6509.75487787749     
 iteration        -1420 MCMCOBJ=   -6507.45202281247     
 iteration        -1410 MCMCOBJ=   -6521.81504699101     
 iteration        -1400 MCMCOBJ=   -6543.58006335646     
 iteration        -1390 MCMCOBJ=   -6556.15889580052     
 iteration        -1380 MCMCOBJ=   -6481.57636687155     
 iteration        -1370 MCMCOBJ=   -6566.96010570870     
 iteration        -1360 MCMCOBJ=   -6502.61096117294     
 iteration        -1350 MCMCOBJ=   -6460.90610643480     
 iteration        -1340 MCMCOBJ=   -6509.18337412594     
 iteration        -1330 MCMCOBJ=   -6550.59136798416     
 iteration        -1320 MCMCOBJ=   -6544.44425590508     
 iteration        -1310 MCMCOBJ=   -6518.41295600302     
 iteration        -1300 MCMCOBJ=   -6496.12038675049     
 iteration        -1290 MCMCOBJ=   -6509.48679542965     
 iteration        -1280 MCMCOBJ=   -6441.62846276503     
 iteration        -1270 MCMCOBJ=   -6425.13184697645     
 iteration        -1260 MCMCOBJ=   -6474.02169798679     
 iteration        -1250 MCMCOBJ=   -6480.97108456519     
 iteration        -1240 MCMCOBJ=   -6527.64942314337     
 iteration        -1230 MCMCOBJ=   -6539.31172701929     
 iteration        -1220 MCMCOBJ=   -6477.09798277833     
 iteration        -1210 MCMCOBJ=   -6479.67924563958     
 iteration        -1200 MCMCOBJ=   -6451.93576070860     
 iteration        -1190 MCMCOBJ=   -6546.20751396619     
 iteration        -1180 MCMCOBJ=   -6484.35751475348     
 iteration        -1170 MCMCOBJ=   -6480.55366220179     
 iteration        -1160 MCMCOBJ=   -6540.38057437411     
 iteration        -1150 MCMCOBJ=   -6511.47773507421     
 iteration        -1140 MCMCOBJ=   -6513.00221552664     
 iteration        -1130 MCMCOBJ=   -6438.16193753847     
 iteration        -1120 MCMCOBJ=   -6535.72419629708     
 iteration        -1110 MCMCOBJ=   -6450.57538172607     
 iteration        -1100 MCMCOBJ=   -6434.80716188857     
 iteration        -1090 MCMCOBJ=   -6521.89943246258     
 iteration        -1080 MCMCOBJ=   -6488.34846721942     
 iteration        -1070 MCMCOBJ=   -6496.80608992146     
 iteration        -1060 MCMCOBJ=   -6485.37446407612     
 iteration        -1050 MCMCOBJ=   -6523.25091695150     
 iteration        -1040 MCMCOBJ=   -6526.59370818493     
 iteration        -1030 MCMCOBJ=   -6454.94143202450     
 iteration        -1020 MCMCOBJ=   -6514.57222105933     
 iteration        -1010 MCMCOBJ=   -6478.60033241022     
 iteration        -1000 MCMCOBJ=   -6497.17200766906     
 iteration         -990 MCMCOBJ=   -6476.49440069168     
 iteration         -980 MCMCOBJ=   -6476.49225562170     
 iteration         -970 MCMCOBJ=   -6485.08005407278     
 iteration         -960 MCMCOBJ=   -6515.56307491753     
 iteration         -950 MCMCOBJ=   -6516.16733962064     
 iteration         -940 MCMCOBJ=   -6495.26914508679     
 iteration         -930 MCMCOBJ=   -6524.78510227170     
 iteration         -920 MCMCOBJ=   -6489.67576219689     
 iteration         -910 MCMCOBJ=   -6519.96717410520     
 iteration         -900 MCMCOBJ=   -6497.42193639217     
 iteration         -890 MCMCOBJ=   -6500.02253130145     
 iteration         -880 MCMCOBJ=   -6514.56224222548     
 iteration         -870 MCMCOBJ=   -6485.64426696673     
 iteration         -860 MCMCOBJ=   -6455.81327394648     
 iteration         -850 MCMCOBJ=   -6482.59576654523     
 iteration         -840 MCMCOBJ=   -6514.49139670159     
 iteration         -830 MCMCOBJ=   -6477.37573231979     
 iteration         -820 MCMCOBJ=   -6432.49622334157     
 iteration         -810 MCMCOBJ=   -6508.51024940669     
 iteration         -800 MCMCOBJ=   -6448.79873209647     
 iteration         -790 MCMCOBJ=   -6530.73541411067     
 iteration         -780 MCMCOBJ=   -6491.84170270568     
 iteration         -770 MCMCOBJ=   -6516.74905889276     
 iteration         -760 MCMCOBJ=   -6536.66690204955     
 iteration         -750 MCMCOBJ=   -6500.91427105922     
 iteration         -740 MCMCOBJ=   -6451.59568235906     
 iteration         -730 MCMCOBJ=   -6433.09470665030     
 iteration         -720 MCMCOBJ=   -6543.43129050011     
 iteration         -710 MCMCOBJ=   -6413.66051497067     
 iteration         -700 MCMCOBJ=   -6495.07336560265     
 iteration         -690 MCMCOBJ=   -6490.72200181468     
 iteration         -680 MCMCOBJ=   -6445.69817025391     
 iteration         -670 MCMCOBJ=   -6516.95171131423     
 iteration         -660 MCMCOBJ=   -6484.68719369237     
 iteration         -650 MCMCOBJ=   -6429.29272346175     
 iteration         -640 MCMCOBJ=   -6432.72374862259     
 iteration         -630 MCMCOBJ=   -6485.60915792414     
 iteration         -620 MCMCOBJ=   -6493.49735355958     
 iteration         -610 MCMCOBJ=   -6485.25682160383     
 iteration         -600 MCMCOBJ=   -6529.12794485320     
 iteration         -590 MCMCOBJ=   -6446.20575745659     
 iteration         -580 MCMCOBJ=   -6471.89283089996     
 iteration         -570 MCMCOBJ=   -6524.18599699041     
 iteration         -560 MCMCOBJ=   -6494.67371582564     
 iteration         -550 MCMCOBJ=   -6506.50854423980     
 iteration         -540 MCMCOBJ=   -6465.59437510708     
 iteration         -530 MCMCOBJ=   -6432.32761762350     
 iteration         -520 MCMCOBJ=   -6521.61001327526     
 iteration         -510 MCMCOBJ=   -6509.25610566212     
 iteration         -500 MCMCOBJ=   -6568.66855712261     
 iteration         -490 MCMCOBJ=   -6484.36962105633     
 iteration         -480 MCMCOBJ=   -6522.19945597160     
 iteration         -470 MCMCOBJ=   -6481.98854173165     
 iteration         -460 MCMCOBJ=   -6436.09906637636     
 iteration         -450 MCMCOBJ=   -6506.38079882708     
 iteration         -440 MCMCOBJ=   -6493.80492521573     
 iteration         -430 MCMCOBJ=   -6527.49400208548     
 iteration         -420 MCMCOBJ=   -6469.73571755604     
 iteration         -410 MCMCOBJ=   -6476.23446161415     
 iteration         -400 MCMCOBJ=   -6474.89627234279     
 iteration         -390 MCMCOBJ=   -6500.76542058531     
 iteration         -380 MCMCOBJ=   -6524.18316323035     
 iteration         -370 MCMCOBJ=   -6479.66819406076     
 iteration         -360 MCMCOBJ=   -6444.02249508732     
 iteration         -350 MCMCOBJ=   -6476.26948280550     
 iteration         -340 MCMCOBJ=   -6490.98679030903     
 iteration         -330 MCMCOBJ=   -6465.35296982958     
 iteration         -320 MCMCOBJ=   -6470.38496085151     
 iteration         -310 MCMCOBJ=   -6497.03310860893     
 iteration         -300 MCMCOBJ=   -6452.21396966570     
 iteration         -290 MCMCOBJ=   -6477.53637300914     
 iteration         -280 MCMCOBJ=   -6390.64695568160     
 iteration         -270 MCMCOBJ=   -6444.93045760954     
 iteration         -260 MCMCOBJ=   -6465.10977640038     
 iteration         -250 MCMCOBJ=   -6524.28972251242     
 iteration         -240 MCMCOBJ=   -6514.03585416118     
 iteration         -230 MCMCOBJ=   -6480.20235641610     
 iteration         -220 MCMCOBJ=   -6452.18665764672     
 iteration         -210 MCMCOBJ=   -6510.64136545243     
 iteration         -200 MCMCOBJ=   -6504.64926698308     
 iteration         -190 MCMCOBJ=   -6457.08259516957     
 iteration         -180 MCMCOBJ=   -6520.75978206817     
 iteration         -170 MCMCOBJ=   -6483.26814790679     
 iteration         -160 MCMCOBJ=   -6535.79766470845     
 iteration         -150 MCMCOBJ=   -6496.15282493565     
 iteration         -140 MCMCOBJ=   -6461.74791224535     
 iteration         -130 MCMCOBJ=   -6479.41131030754     
 iteration         -120 MCMCOBJ=   -6482.26179070820     
 iteration         -110 MCMCOBJ=   -6509.10192106197     
 iteration         -100 MCMCOBJ=   -6502.84250318428     
 iteration          -90 MCMCOBJ=   -6503.77750816518     
 iteration          -80 MCMCOBJ=   -6441.47853667434     
 iteration          -70 MCMCOBJ=   -6493.52455424346     
 iteration          -60 MCMCOBJ=   -6543.13331378655     
 iteration          -50 MCMCOBJ=   -6448.34929355219     
 iteration          -40 MCMCOBJ=   -6504.37707314289     
 iteration          -30 MCMCOBJ=   -6497.48927777684     
 iteration          -20 MCMCOBJ=   -6513.28763781620     
 iteration          -10 MCMCOBJ=   -6490.10280063184     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6476.89178968263     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS NOT PERFORMED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6476.89178968263     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3595.10054955277     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6476.89178968263     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5741.74096311889     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    55.1779157436876     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6476.89178968263     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6421.71387393894     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   422.73
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6476.892       **************************************************
 #OBJS:********************************************        0.000 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         4.05E+00 -2.27E+00  6.20E-01 -1.95E-01  2.35E+00  2.22E-01  3.73E+00 -6.44E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        3.40E-01
 
 ETA2
+       -2.96E-02  1.58E-01
 
 ETA3
+        7.76E-02 -2.65E-02  1.93E-01
 
 ETA4
+        9.35E-03  4.89E-02 -3.46E-02  2.53E-01
 
 ETA5
+        5.23E-02  1.72E-03  3.45E-02 -4.84E-02  1.90E-01
 
 ETA6
+       -8.94E-02 -3.25E-03  1.15E-02  1.47E-02 -7.10E-02  2.45E-01
 
 ETA7
+        9.86E-02 -5.64E-02  8.48E-02 -6.63E-02  1.09E-01 -2.08E-02  3.35E-01
 
 ETA8
+        1.91E-01  3.74E-02  7.53E-02  6.58E-03  7.50E-02 -1.23E-01  1.69E-01  3.32E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.45E-03
 
 EPS2
+        0.00E+00  2.45E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.83E-01
 
 ETA2
+       -1.28E-01  3.97E-01
 
 ETA3
+        3.03E-01 -1.52E-01  4.40E-01
 
 ETA4
+        3.19E-02  2.45E-01 -1.56E-01  5.03E-01
 
 ETA5
+        2.06E-01  9.95E-03  1.80E-01 -2.21E-01  4.35E-01
 
 ETA6
+       -3.10E-01 -1.65E-02  5.29E-02  5.90E-02 -3.29E-01  4.95E-01
 
 ETA7
+        2.92E-01 -2.46E-01  3.33E-01 -2.27E-01  4.34E-01 -7.28E-02  5.79E-01
 
 ETA8
+        5.68E-01  1.63E-01  2.97E-01  2.27E-02  2.99E-01 -4.33E-01  5.07E-01  5.76E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.72E-02
 
 EPS2
+        0.00E+00  1.56E-01
 
1
 
 
 #TBLN:      3
 #METH: NUTS Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            3480
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      4
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     4
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmto17.ext
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
 CONVERGENCE TYPE (CTYPE):                  0
 KEEP ITERATIONS (THIN):            1
 BURN-IN ITERATIONS (NBURN):                1000
 ITERATIONS (NITER):                        2000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
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
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):0
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      0
 MASS MATRIX ACCUMULATION ITERATIONS (MADAPT):          -1
 MASS MATRIX BLOCKING TYPE (NUTS_MASS):                 B
 MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (NUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (KAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (NUTS_DELTA):                   0.800000000000000
 NUTS GAMMA SETTING (NUTS_GAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 8.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000
 NUTS WARMUP METHOD (NUTS_TEST):       0
 NUTS MAXIMAL DEPTH SEARCH (NUTS_MAXDEPTH):       10
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       10.0000000000000
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): 100.000000000000
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 5.000000000000000E-02
 INITIAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPITER): 1
 INTERVAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPINTER):0
 ETA PARAMETERIZATION (NUTS_EPARAM):0
 OMEGA PARAMETERIZATION (NUTS_OPARAM):1
 SIGMA PARAMETERIZATION (NUTS_SPARAM):1
 NUTS REGULARIZING METHOD (NUTS_REG): 0.00000000000000

 TOLERANCES FOR ESTIMATION/EVALUATION STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   3
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 TOLERANCES FOR COVARIANCE STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   3
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3   4   5   6   7   8
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3   4   5   6   7   8
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1   2
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -1000 MCMCOBJ=   -6482.40460515192     
 iteration         -995 MCMCOBJ=   -6620.53796957706     
 iteration         -990 MCMCOBJ=   -6630.39625196365     
 iteration         -985 MCMCOBJ=   -6635.90496313938     
 iteration         -980 MCMCOBJ=   -6657.37586053444     
 iteration         -975 MCMCOBJ=   -6615.80557681558     
 iteration         -970 MCMCOBJ=   -6632.84364071666     
 iteration         -965 MCMCOBJ=   -6629.48301535250     
 iteration         -960 MCMCOBJ=   -6589.86513883907     
 iteration         -955 MCMCOBJ=   -6569.93662398758     
 iteration         -950 MCMCOBJ=   -6575.02448006248     
 iteration         -945 MCMCOBJ=   -6571.83695390457     
 iteration         -940 MCMCOBJ=   -6623.38760344527     
 iteration         -935 MCMCOBJ=   -6576.27556672719     
 iteration         -930 MCMCOBJ=   -6616.08889212478     
 iteration         -925 MCMCOBJ=   -6560.60559512087     
 iteration         -920 MCMCOBJ=   -6587.65220454455     
 iteration         -915 MCMCOBJ=   -6620.53710368675     
 iteration         -910 MCMCOBJ=   -6562.21658868600     
 iteration         -905 MCMCOBJ=   -6551.64431523287     
 iteration         -900 MCMCOBJ=   -6565.07393219642     
 iteration         -895 MCMCOBJ=   -6640.84667928384     
 iteration         -890 MCMCOBJ=   -6686.21489281953     
 iteration         -885 MCMCOBJ=   -6564.98099951345     
 iteration         -880 MCMCOBJ=   -6607.66690128629     
 iteration         -875 MCMCOBJ=   -6676.72070874279     
 iteration         -870 MCMCOBJ=   -6566.97797214414     
 iteration         -865 MCMCOBJ=   -6598.42284749415     
 iteration         -860 MCMCOBJ=   -6620.18218564411     
 iteration         -855 MCMCOBJ=   -6573.22422668588     
 iteration         -850 MCMCOBJ=   -6563.52847026687     
 iteration         -845 MCMCOBJ=   -6601.68108114454     
 iteration         -840 MCMCOBJ=   -6608.28750381814     
 iteration         -835 MCMCOBJ=   -6585.39330522483     
 iteration         -830 MCMCOBJ=   -6602.87597276451     
 iteration         -825 MCMCOBJ=   -6682.04490789864     
 iteration         -820 MCMCOBJ=   -6617.69728232081     
 iteration         -815 MCMCOBJ=   -6549.16856686909     
 iteration         -810 MCMCOBJ=   -6567.89535335537     
 iteration         -805 MCMCOBJ=   -6557.48869277177     
 iteration         -800 MCMCOBJ=   -6594.19576549131     
 iteration         -795 MCMCOBJ=   -6636.70634205421     
 iteration         -790 MCMCOBJ=   -6569.69888895163     
 iteration         -785 MCMCOBJ=   -6602.31652308207     
 iteration         -780 MCMCOBJ=   -6619.08877492438     
 iteration         -775 MCMCOBJ=   -6620.33240080870     
 iteration         -770 MCMCOBJ=   -6587.24101976124     
 iteration         -765 MCMCOBJ=   -6591.93687386843     
 iteration         -760 MCMCOBJ=   -6617.54567548368     
 iteration         -755 MCMCOBJ=   -6588.30685675264     
 iteration         -750 MCMCOBJ=   -6592.35582991050     
 iteration         -745 MCMCOBJ=   -6624.09714879260     
 iteration         -740 MCMCOBJ=   -6622.40548614080     
 iteration         -735 MCMCOBJ=   -6624.61380026837     
 iteration         -730 MCMCOBJ=   -6664.72598760626     
 iteration         -725 MCMCOBJ=   -6623.09498290283     
 iteration         -720 MCMCOBJ=   -6621.75070953236     
 iteration         -715 MCMCOBJ=   -6653.52977492702     
 iteration         -710 MCMCOBJ=   -6605.71072212619     
 iteration         -705 MCMCOBJ=   -6603.36205930835     
 iteration         -700 MCMCOBJ=   -6582.45855934963     
 iteration         -695 MCMCOBJ=   -6619.12250443238     
 iteration         -690 MCMCOBJ=   -6564.10176615156     
 iteration         -685 MCMCOBJ=   -6574.67802787852     
 iteration         -680 MCMCOBJ=   -6572.83750414556     
 iteration         -675 MCMCOBJ=   -6603.73524301918     
 iteration         -670 MCMCOBJ=   -6610.73822212612     
 iteration         -665 MCMCOBJ=   -6614.48232003544     
 iteration         -660 MCMCOBJ=   -6630.20754525226     
 iteration         -655 MCMCOBJ=   -6585.31813415105     
 iteration         -650 MCMCOBJ=   -6607.75247506926     
 iteration         -645 MCMCOBJ=   -6608.81140098997     
 iteration         -640 MCMCOBJ=   -6594.35056479100     
 iteration         -635 MCMCOBJ=   -6626.65563650813     
 iteration         -630 MCMCOBJ=   -6626.84586162465     
 iteration         -625 MCMCOBJ=   -6650.17894998751     
 iteration         -620 MCMCOBJ=   -6582.35312437842     
 iteration         -615 MCMCOBJ=   -6626.51139922607     
 iteration         -610 MCMCOBJ=   -6593.19798840847     
 iteration         -605 MCMCOBJ=   -6638.67403169637     
 iteration         -600 MCMCOBJ=   -6591.70703075904     
 iteration         -595 MCMCOBJ=   -6640.09951742758     
 iteration         -590 MCMCOBJ=   -6616.30106801654     
 iteration         -585 MCMCOBJ=   -6622.38984889962     
 iteration         -580 MCMCOBJ=   -6636.44817402912     
 iteration         -575 MCMCOBJ=   -6625.95159485227     
 iteration         -570 MCMCOBJ=   -6602.41172991969     
 iteration         -565 MCMCOBJ=   -6584.59327624519     
 iteration         -560 MCMCOBJ=   -6550.78526914366     
 iteration         -555 MCMCOBJ=   -6557.33826930350     
 iteration         -550 MCMCOBJ=   -6607.29315735210     
 iteration         -545 MCMCOBJ=   -6590.35105064348     
 iteration         -540 MCMCOBJ=   -6607.37523534324     
 iteration         -535 MCMCOBJ=   -6627.09329810308     
 iteration         -530 MCMCOBJ=   -6592.56837329089     
 iteration         -525 MCMCOBJ=   -6622.73695664211     
 iteration         -520 MCMCOBJ=   -6619.01365682812     
 iteration         -515 MCMCOBJ=   -6612.98762426749     
 iteration         -510 MCMCOBJ=   -6595.54149444672     
 iteration         -505 MCMCOBJ=   -6627.39095993770     
 iteration         -500 MCMCOBJ=   -6640.16926121932     
 iteration         -495 MCMCOBJ=   -6592.76595124677     
 iteration         -490 MCMCOBJ=   -6518.83590729207     
 iteration         -485 MCMCOBJ=   -6586.67182454773     
 iteration         -480 MCMCOBJ=   -6614.67603824711     
 iteration         -475 MCMCOBJ=   -6580.62221423271     
 iteration         -470 MCMCOBJ=   -6650.44808100948     
 iteration         -465 MCMCOBJ=   -6647.49855628064     
 iteration         -460 MCMCOBJ=   -6559.44688486184     
 iteration         -455 MCMCOBJ=   -6600.38470792435     
 iteration         -450 MCMCOBJ=   -6550.35856139249     
 iteration         -445 MCMCOBJ=   -6644.47714925860     
 iteration         -440 MCMCOBJ=   -6591.54050887103     
 iteration         -435 MCMCOBJ=   -6578.34138285759     
 iteration         -430 MCMCOBJ=   -6537.06104602289     
 iteration         -425 MCMCOBJ=   -6584.11529197392     
 iteration         -420 MCMCOBJ=   -6523.33866150851     
 iteration         -415 MCMCOBJ=   -6586.76050569779     
 iteration         -410 MCMCOBJ=   -6627.25877355857     
 iteration         -405 MCMCOBJ=   -6587.48245733184     
 iteration         -400 MCMCOBJ=   -6600.74781878699     
 iteration         -395 MCMCOBJ=   -6638.48525043523     
 iteration         -390 MCMCOBJ=   -6655.85204807278     
 iteration         -385 MCMCOBJ=   -6647.31089839014     
 iteration         -380 MCMCOBJ=   -6571.94344429135     
 iteration         -375 MCMCOBJ=   -6532.99870282331     
 iteration         -370 MCMCOBJ=   -6553.96807881693     
 iteration         -365 MCMCOBJ=   -6560.66584506329     
 iteration         -360 MCMCOBJ=   -6578.86562129116     
 iteration         -355 MCMCOBJ=   -6578.16291371685     
 iteration         -350 MCMCOBJ=   -6593.62420479606     
 iteration         -345 MCMCOBJ=   -6603.72653688686     
 iteration         -340 MCMCOBJ=   -6613.86746682602     
 iteration         -335 MCMCOBJ=   -6607.69505890011     
 iteration         -330 MCMCOBJ=   -6580.46193128416     
 iteration         -325 MCMCOBJ=   -6588.15269304111     
 iteration         -320 MCMCOBJ=   -6587.26716926554     
 iteration         -315 MCMCOBJ=   -6596.07580600984     
 iteration         -310 MCMCOBJ=   -6553.96787150518     
 iteration         -305 MCMCOBJ=   -6596.23180153832     
 iteration         -300 MCMCOBJ=   -6612.68881889926     
 iteration         -295 MCMCOBJ=   -6630.59223760655     
 iteration         -290 MCMCOBJ=   -6589.69929272232     
 iteration         -285 MCMCOBJ=   -6530.55623968632     
 iteration         -280 MCMCOBJ=   -6543.25594056213     
 iteration         -275 MCMCOBJ=   -6644.13498656938     
 iteration         -270 MCMCOBJ=   -6617.20045191553     
 iteration         -265 MCMCOBJ=   -6575.80998773944     
 iteration         -260 MCMCOBJ=   -6516.36890541473     
 iteration         -255 MCMCOBJ=   -6581.04688905570     
 iteration         -250 MCMCOBJ=   -6618.59683361465     
 iteration         -245 MCMCOBJ=   -6622.95167044469     
 iteration         -240 MCMCOBJ=   -6615.74711351756     
 iteration         -235 MCMCOBJ=   -6614.49762847396     
 iteration         -230 MCMCOBJ=   -6592.24431690272     
 iteration         -225 MCMCOBJ=   -6519.35557497633     
 iteration         -220 MCMCOBJ=   -6572.57718790491     
 iteration         -215 MCMCOBJ=   -6587.24819935681     
 iteration         -210 MCMCOBJ=   -6639.74886075078     
 iteration         -205 MCMCOBJ=   -6652.30318183081     
 iteration         -200 MCMCOBJ=   -6583.66206740426     
 iteration         -195 MCMCOBJ=   -6657.87814172656     
 iteration         -190 MCMCOBJ=   -6581.61472807961     
 iteration         -185 MCMCOBJ=   -6611.58913230025     
 iteration         -180 MCMCOBJ=   -6628.74947345168     
 iteration         -175 MCMCOBJ=   -6564.07462369367     
 iteration         -170 MCMCOBJ=   -6549.08124004817     
 iteration         -165 MCMCOBJ=   -6555.16319549391     
 iteration         -160 MCMCOBJ=   -6652.01307700893     
 iteration         -155 MCMCOBJ=   -6598.28473019009     
 iteration         -150 MCMCOBJ=   -6534.67157995010     
 iteration         -145 MCMCOBJ=   -6568.41025850795     
 iteration         -140 MCMCOBJ=   -6650.42338349636     
 iteration         -135 MCMCOBJ=   -6599.98919116396     
 iteration         -130 MCMCOBJ=   -6573.22121144777     
 iteration         -125 MCMCOBJ=   -6519.25361380996     
 iteration         -120 MCMCOBJ=   -6575.05127127371     
 iteration         -115 MCMCOBJ=   -6572.95747162309     
 iteration         -110 MCMCOBJ=   -6596.86691793321     
 iteration         -105 MCMCOBJ=   -6599.05141097170     
 iteration         -100 MCMCOBJ=   -6544.40306050597     
 iteration          -95 MCMCOBJ=   -6593.17226858391     
 iteration          -90 MCMCOBJ=   -6581.10203301675     
 iteration          -85 MCMCOBJ=   -6606.46521096931     
 iteration          -80 MCMCOBJ=   -6593.53370397165     
 iteration          -75 MCMCOBJ=   -6640.61434150390     
 iteration          -70 MCMCOBJ=   -6582.28164386723     
 iteration          -65 MCMCOBJ=   -6556.54385032555     
 iteration          -60 MCMCOBJ=   -6484.62829886765     
 iteration          -55 MCMCOBJ=   -6607.85686713377     
 iteration          -50 MCMCOBJ=   -6622.53054754111     
 iteration          -45 MCMCOBJ=   -6612.58362184109     
 iteration          -40 MCMCOBJ=   -6589.71714026517     
 iteration          -35 MCMCOBJ=   -6562.20977622632     
 iteration          -30 MCMCOBJ=   -6657.35399564155     
 iteration          -25 MCMCOBJ=   -6595.67729115492     
 iteration          -20 MCMCOBJ=   -6612.55968560570     
 iteration          -15 MCMCOBJ=   -6619.30480153417     
 iteration          -10 MCMCOBJ=   -6598.95439335577     
 iteration           -5 MCMCOBJ=   -6579.76709212768     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6642.81321640615     
 iteration            5 MCMCOBJ=   -6555.96853664575     
 iteration           10 MCMCOBJ=   -6559.69923859077     
 iteration           15 MCMCOBJ=   -6581.86667713441     
 iteration           20 MCMCOBJ=   -6575.06191366816     
 iteration           25 MCMCOBJ=   -6605.50465301978     
 iteration           30 MCMCOBJ=   -6559.80717440573     
 iteration           35 MCMCOBJ=   -6616.69142235529     
 iteration           40 MCMCOBJ=   -6565.10626358564     
 iteration           45 MCMCOBJ=   -6565.40084347275     
 iteration           50 MCMCOBJ=   -6553.45159371945     
 iteration           55 MCMCOBJ=   -6607.30978282495     
 iteration           60 MCMCOBJ=   -6654.68196194837     
 iteration           65 MCMCOBJ=   -6619.44757006166     
 iteration           70 MCMCOBJ=   -6562.74296258989     
 iteration           75 MCMCOBJ=   -6627.31810760568     
 iteration           80 MCMCOBJ=   -6626.96551329916     
 iteration           85 MCMCOBJ=   -6613.40809863821     
 iteration           90 MCMCOBJ=   -6661.48747515425     
 iteration           95 MCMCOBJ=   -6649.90429630933     
 iteration          100 MCMCOBJ=   -6592.97240064492     
 iteration          105 MCMCOBJ=   -6665.11401475655     
 iteration          110 MCMCOBJ=   -6594.74249856134     
 iteration          115 MCMCOBJ=   -6638.53770584137     
 iteration          120 MCMCOBJ=   -6671.00489295328     
 iteration          125 MCMCOBJ=   -6565.31098047934     
 iteration          130 MCMCOBJ=   -6599.77579734731     
 iteration          135 MCMCOBJ=   -6608.96180336597     
 iteration          140 MCMCOBJ=   -6596.28309502381     
 iteration          145 MCMCOBJ=   -6507.76633167086     
 iteration          150 MCMCOBJ=   -6570.90441416442     
 iteration          155 MCMCOBJ=   -6637.30228382988     
 iteration          160 MCMCOBJ=   -6568.94614818581     
 iteration          165 MCMCOBJ=   -6596.80724909296     
 iteration          170 MCMCOBJ=   -6601.77180284417     
 iteration          175 MCMCOBJ=   -6589.16857515343     
 iteration          180 MCMCOBJ=   -6640.38558660796     
 iteration          185 MCMCOBJ=   -6566.91933108044     
 iteration          190 MCMCOBJ=   -6602.33003427835     
 iteration          195 MCMCOBJ=   -6567.79982114900     
 iteration          200 MCMCOBJ=   -6621.53424468876     
 iteration          205 MCMCOBJ=   -6576.63878098973     
 iteration          210 MCMCOBJ=   -6671.66228829370     
 iteration          215 MCMCOBJ=   -6568.59498557817     
 iteration          220 MCMCOBJ=   -6566.11750620921     
 iteration          225 MCMCOBJ=   -6537.05660032195     
 iteration          230 MCMCOBJ=   -6538.45217025871     
 iteration          235 MCMCOBJ=   -6577.84427146203     
 iteration          240 MCMCOBJ=   -6540.63746670404     
 iteration          245 MCMCOBJ=   -6607.25324943266     
 iteration          250 MCMCOBJ=   -6600.27894629394     
 iteration          255 MCMCOBJ=   -6543.01460534869     
 iteration          260 MCMCOBJ=   -6616.42437702213     
 iteration          265 MCMCOBJ=   -6630.43467774729     
 iteration          270 MCMCOBJ=   -6609.93845221006     
 iteration          275 MCMCOBJ=   -6605.81729850091     
 iteration          280 MCMCOBJ=   -6561.57412609608     
 iteration          285 MCMCOBJ=   -6607.79313210489     
 iteration          290 MCMCOBJ=   -6608.45825241468     
 iteration          295 MCMCOBJ=   -6620.14156291757     
 iteration          300 MCMCOBJ=   -6580.51086895568     
 iteration          305 MCMCOBJ=   -6597.28101761710     
 iteration          310 MCMCOBJ=   -6522.12628693797     
 iteration          315 MCMCOBJ=   -6577.15832185084     
 iteration          320 MCMCOBJ=   -6607.70736751109     
 iteration          325 MCMCOBJ=   -6593.04079165645     
 iteration          330 MCMCOBJ=   -6642.27371949470     
 iteration          335 MCMCOBJ=   -6629.48542518106     
 iteration          340 MCMCOBJ=   -6632.23750983259     
 iteration          345 MCMCOBJ=   -6643.09302180735     
 iteration          350 MCMCOBJ=   -6532.41754957347     
 iteration          355 MCMCOBJ=   -6543.46152776758     
 iteration          360 MCMCOBJ=   -6603.74197864259     
 iteration          365 MCMCOBJ=   -6635.95683911715     
 iteration          370 MCMCOBJ=   -6636.09245432847     
 iteration          375 MCMCOBJ=   -6569.38783710535     
 iteration          380 MCMCOBJ=   -6611.92779610221     
 iteration          385 MCMCOBJ=   -6589.06233718283     
 iteration          390 MCMCOBJ=   -6597.99025691099     
 iteration          395 MCMCOBJ=   -6626.11922647752     
 iteration          400 MCMCOBJ=   -6544.84464199142     
 iteration          405 MCMCOBJ=   -6629.64686304394     
 iteration          410 MCMCOBJ=   -6569.00803877987     
 iteration          415 MCMCOBJ=   -6648.20672879982     
 iteration          420 MCMCOBJ=   -6653.02254646844     
 iteration          425 MCMCOBJ=   -6632.80991318231     
 iteration          430 MCMCOBJ=   -6562.54211467242     
 iteration          435 MCMCOBJ=   -6616.26474530121     
 iteration          440 MCMCOBJ=   -6640.81956848995     
 iteration          445 MCMCOBJ=   -6612.03163457391     
 iteration          450 MCMCOBJ=   -6600.65242580703     
 iteration          455 MCMCOBJ=   -6580.90326181202     
 iteration          460 MCMCOBJ=   -6538.87268575407     
 iteration          465 MCMCOBJ=   -6562.35567217876     
 iteration          470 MCMCOBJ=   -6582.81229144637     
 iteration          475 MCMCOBJ=   -6608.01445247051     
 iteration          480 MCMCOBJ=   -6624.07203331364     
 iteration          485 MCMCOBJ=   -6584.20393930573     
 iteration          490 MCMCOBJ=   -6591.01623706367     
 iteration          495 MCMCOBJ=   -6600.61052142354     
 iteration          500 MCMCOBJ=   -6586.50130204033     
 iteration          505 MCMCOBJ=   -6515.02688581690     
 iteration          510 MCMCOBJ=   -6595.33972709323     
 iteration          515 MCMCOBJ=   -6555.63376279162     
 iteration          520 MCMCOBJ=   -6605.12424189226     
 iteration          525 MCMCOBJ=   -6596.52279677087     
 iteration          530 MCMCOBJ=   -6584.59599355911     
 iteration          535 MCMCOBJ=   -6614.00109955060     
 iteration          540 MCMCOBJ=   -6540.61733220401     
 iteration          545 MCMCOBJ=   -6633.37467506226     
 iteration          550 MCMCOBJ=   -6602.96834103515     
 iteration          555 MCMCOBJ=   -6591.21787720571     
 iteration          560 MCMCOBJ=   -6574.02389700106     
 iteration          565 MCMCOBJ=   -6610.45986657921     
 iteration          570 MCMCOBJ=   -6588.01782977616     
 iteration          575 MCMCOBJ=   -6577.86629019831     
 iteration          580 MCMCOBJ=   -6551.81467963645     
 iteration          585 MCMCOBJ=   -6588.28813314932     
 iteration          590 MCMCOBJ=   -6592.78040628855     
 iteration          595 MCMCOBJ=   -6582.86450625204     
 iteration          600 MCMCOBJ=   -6674.78091819423     
 iteration          605 MCMCOBJ=   -6618.91539396183     
 iteration          610 MCMCOBJ=   -6564.70099089320     
 iteration          615 MCMCOBJ=   -6544.61045829584     
 iteration          620 MCMCOBJ=   -6652.89725553498     
 iteration          625 MCMCOBJ=   -6557.05976122306     
 iteration          630 MCMCOBJ=   -6557.67863854998     
 iteration          635 MCMCOBJ=   -6579.19872826828     
 iteration          640 MCMCOBJ=   -6601.20640946786     
 iteration          645 MCMCOBJ=   -6585.77825393553     
 iteration          650 MCMCOBJ=   -6572.62959100571     
 iteration          655 MCMCOBJ=   -6601.42192181674     
 iteration          660 MCMCOBJ=   -6630.62378364270     
 iteration          665 MCMCOBJ=   -6612.85607311944     
 iteration          670 MCMCOBJ=   -6586.54627690392     
 iteration          675 MCMCOBJ=   -6586.96135694050     
 iteration          680 MCMCOBJ=   -6600.07440009727     
 iteration          685 MCMCOBJ=   -6618.13196265259     
 iteration          690 MCMCOBJ=   -6649.06096840995     
 iteration          695 MCMCOBJ=   -6591.44529345776     
 iteration          700 MCMCOBJ=   -6629.42585153208     
 iteration          705 MCMCOBJ=   -6585.94553564247     
 iteration          710 MCMCOBJ=   -6610.91095279072     
 iteration          715 MCMCOBJ=   -6601.71938372436     
 iteration          720 MCMCOBJ=   -6580.66829549352     
 iteration          725 MCMCOBJ=   -6563.05268216002     
 iteration          730 MCMCOBJ=   -6640.03939129166     
 iteration          735 MCMCOBJ=   -6654.57091444011     
 iteration          740 MCMCOBJ=   -6612.25120337348     
 iteration          745 MCMCOBJ=   -6602.05796470927     
 iteration          750 MCMCOBJ=   -6630.59626801950     
 iteration          755 MCMCOBJ=   -6647.22578708657     
 iteration          760 MCMCOBJ=   -6594.14398872098     
 iteration          765 MCMCOBJ=   -6602.73338716397     
 iteration          770 MCMCOBJ=   -6533.16869830683     
 iteration          775 MCMCOBJ=   -6642.92609213826     
 iteration          780 MCMCOBJ=   -6535.01347455771     
 iteration          785 MCMCOBJ=   -6571.97155595483     
 iteration          790 MCMCOBJ=   -6621.24021499893     
 iteration          795 MCMCOBJ=   -6583.59585376115     
 iteration          800 MCMCOBJ=   -6617.47512385106     
 iteration          805 MCMCOBJ=   -6573.66636364408     
 iteration          810 MCMCOBJ=   -6629.94959978788     
 iteration          815 MCMCOBJ=   -6578.98597261139     
 iteration          820 MCMCOBJ=   -6683.63354079750     
 iteration          825 MCMCOBJ=   -6539.88061352871     
 iteration          830 MCMCOBJ=   -6611.36727403381     
 iteration          835 MCMCOBJ=   -6595.82446009542     
 iteration          840 MCMCOBJ=   -6627.24455322878     
 iteration          845 MCMCOBJ=   -6581.58381665212     
 iteration          850 MCMCOBJ=   -6613.79963025523     
 iteration          855 MCMCOBJ=   -6614.23226287404     
 iteration          860 MCMCOBJ=   -6597.91312108973     
 iteration          865 MCMCOBJ=   -6583.57352682486     
 iteration          870 MCMCOBJ=   -6642.09512552836     
 iteration          875 MCMCOBJ=   -6625.76577178343     
 iteration          880 MCMCOBJ=   -6650.18027500587     
 iteration          885 MCMCOBJ=   -6598.38885131008     
 iteration          890 MCMCOBJ=   -6612.73871828630     
 iteration          895 MCMCOBJ=   -6580.52834960431     
 iteration          900 MCMCOBJ=   -6623.02858397240     
 iteration          905 MCMCOBJ=   -6589.49494070566     
 iteration          910 MCMCOBJ=   -6658.57437582393     
 iteration          915 MCMCOBJ=   -6599.33768457027     
 iteration          920 MCMCOBJ=   -6579.70632245721     
 iteration          925 MCMCOBJ=   -6600.56007393998     
 iteration          930 MCMCOBJ=   -6580.47513780202     
 iteration          935 MCMCOBJ=   -6631.49018766532     
 iteration          940 MCMCOBJ=   -6646.62319523193     
 iteration          945 MCMCOBJ=   -6618.54081080767     
 iteration          950 MCMCOBJ=   -6586.23225837523     
 iteration          955 MCMCOBJ=   -6619.89088044688     
 iteration          960 MCMCOBJ=   -6621.54004410852     
 iteration          965 MCMCOBJ=   -6592.18619852385     
 iteration          970 MCMCOBJ=   -6575.43755900333     
 iteration          975 MCMCOBJ=   -6624.50958425501     
 iteration          980 MCMCOBJ=   -6591.65776935652     
 iteration          985 MCMCOBJ=   -6609.73698653347     
 iteration          990 MCMCOBJ=   -6630.88355723120     
 iteration          995 MCMCOBJ=   -6624.51351539151     
 iteration         1000 MCMCOBJ=   -6636.71477556520     
 iteration         1005 MCMCOBJ=   -6594.58995176611     
 iteration         1010 MCMCOBJ=   -6618.24490945889     
 iteration         1015 MCMCOBJ=   -6606.32581805957     
 iteration         1020 MCMCOBJ=   -6578.57949609930     
 iteration         1025 MCMCOBJ=   -6646.29339318452     
 iteration         1030 MCMCOBJ=   -6577.38163894227     
 iteration         1035 MCMCOBJ=   -6594.07353566723     
 iteration         1040 MCMCOBJ=   -6640.61243842673     
 iteration         1045 MCMCOBJ=   -6601.11860118229     
 iteration         1050 MCMCOBJ=   -6601.97570960232     
 iteration         1055 MCMCOBJ=   -6624.60128034690     
 iteration         1060 MCMCOBJ=   -6591.91803161546     
 iteration         1065 MCMCOBJ=   -6596.24381567214     
 iteration         1070 MCMCOBJ=   -6591.86187570310     
 iteration         1075 MCMCOBJ=   -6616.44567226381     
 iteration         1080 MCMCOBJ=   -6655.98915586468     
 iteration         1085 MCMCOBJ=   -6618.71645831654     
 iteration         1090 MCMCOBJ=   -6552.78397946523     
 iteration         1095 MCMCOBJ=   -6633.21014655566     
 iteration         1100 MCMCOBJ=   -6649.24543850266     
 iteration         1105 MCMCOBJ=   -6524.07740061872     
 iteration         1110 MCMCOBJ=   -6687.32087130632     
 iteration         1115 MCMCOBJ=   -6589.56820991216     
 iteration         1120 MCMCOBJ=   -6577.73069109435     
 iteration         1125 MCMCOBJ=   -6593.03758632872     
 iteration         1130 MCMCOBJ=   -6585.24035703706     
 iteration         1135 MCMCOBJ=   -6642.96280163938     
 iteration         1140 MCMCOBJ=   -6633.19566814981     
 iteration         1145 MCMCOBJ=   -6666.28985991652     
 iteration         1150 MCMCOBJ=   -6638.46916041260     
 iteration         1155 MCMCOBJ=   -6632.90872434751     
 iteration         1160 MCMCOBJ=   -6649.67859978114     
 iteration         1165 MCMCOBJ=   -6532.32137738212     
 iteration         1170 MCMCOBJ=   -6606.11957362334     
 iteration         1175 MCMCOBJ=   -6604.54114347412     
 iteration         1180 MCMCOBJ=   -6568.36566057769     
 iteration         1185 MCMCOBJ=   -6527.27851257725     
 iteration         1190 MCMCOBJ=   -6631.93734353048     
 iteration         1195 MCMCOBJ=   -6590.29655675155     
 iteration         1200 MCMCOBJ=   -6568.54559187494     
 iteration         1205 MCMCOBJ=   -6617.10588603414     
 iteration         1210 MCMCOBJ=   -6570.82473857585     
 iteration         1215 MCMCOBJ=   -6616.98131491733     
 iteration         1220 MCMCOBJ=   -6521.91798904196     
 iteration         1225 MCMCOBJ=   -6588.04428075268     
 iteration         1230 MCMCOBJ=   -6573.99352420291     
 iteration         1235 MCMCOBJ=   -6636.00618885152     
 iteration         1240 MCMCOBJ=   -6531.54465636456     
 iteration         1245 MCMCOBJ=   -6550.06374117739     
 iteration         1250 MCMCOBJ=   -6598.63461311786     
 iteration         1255 MCMCOBJ=   -6566.10156154745     
 iteration         1260 MCMCOBJ=   -6613.43242062174     
 iteration         1265 MCMCOBJ=   -6602.26607692896     
 iteration         1270 MCMCOBJ=   -6584.66122721435     
 iteration         1275 MCMCOBJ=   -6564.88256103479     
 iteration         1280 MCMCOBJ=   -6615.30839865168     
 iteration         1285 MCMCOBJ=   -6640.37828632207     
 iteration         1290 MCMCOBJ=   -6561.76851570434     
 iteration         1295 MCMCOBJ=   -6548.08742076273     
 iteration         1300 MCMCOBJ=   -6615.89892792316     
 iteration         1305 MCMCOBJ=   -6570.91577267611     
 iteration         1310 MCMCOBJ=   -6570.87766700807     
 iteration         1315 MCMCOBJ=   -6642.03172810791     
 iteration         1320 MCMCOBJ=   -6632.13600498500     
 iteration         1325 MCMCOBJ=   -6647.35440449402     
 iteration         1330 MCMCOBJ=   -6543.46869196704     
 iteration         1335 MCMCOBJ=   -6599.62264966454     
 iteration         1340 MCMCOBJ=   -6593.08087866075     
 iteration         1345 MCMCOBJ=   -6615.88127311161     
 iteration         1350 MCMCOBJ=   -6602.92225019684     
 iteration         1355 MCMCOBJ=   -6574.51964886137     
 iteration         1360 MCMCOBJ=   -6618.32451459310     
 iteration         1365 MCMCOBJ=   -6614.87163466714     
 iteration         1370 MCMCOBJ=   -6586.80810943197     
 iteration         1375 MCMCOBJ=   -6534.98979126938     
 iteration         1380 MCMCOBJ=   -6584.46688477999     
 iteration         1385 MCMCOBJ=   -6531.62190900540     
 iteration         1390 MCMCOBJ=   -6576.81143582480     
 iteration         1395 MCMCOBJ=   -6592.84321067302     
 iteration         1400 MCMCOBJ=   -6622.42349756337     
 iteration         1405 MCMCOBJ=   -6660.27102525489     
 iteration         1410 MCMCOBJ=   -6617.03998169352     
 iteration         1415 MCMCOBJ=   -6586.79863540906     
 iteration         1420 MCMCOBJ=   -6570.41132372213     
 iteration         1425 MCMCOBJ=   -6631.00985655955     
 iteration         1430 MCMCOBJ=   -6533.01998151422     
 iteration         1435 MCMCOBJ=   -6640.86380706498     
 iteration         1440 MCMCOBJ=   -6610.40154385917     
 iteration         1445 MCMCOBJ=   -6603.66467855859     
 iteration         1450 MCMCOBJ=   -6596.92509008519     
 iteration         1455 MCMCOBJ=   -6567.99515329821     
 iteration         1460 MCMCOBJ=   -6593.71991957657     
 iteration         1465 MCMCOBJ=   -6631.03973155192     
 iteration         1470 MCMCOBJ=   -6587.48577486692     
 iteration         1475 MCMCOBJ=   -6561.47886723162     
 iteration         1480 MCMCOBJ=   -6651.81888988248     
 iteration         1485 MCMCOBJ=   -6593.37237674693     
 iteration         1490 MCMCOBJ=   -6652.73940296589     
 iteration         1495 MCMCOBJ=   -6601.92993181461     
 iteration         1500 MCMCOBJ=   -6590.84213208282     
 iteration         1505 MCMCOBJ=   -6602.35788554413     
 iteration         1510 MCMCOBJ=   -6637.08046129843     
 iteration         1515 MCMCOBJ=   -6608.97262773168     
 iteration         1520 MCMCOBJ=   -6594.43291727652     
 iteration         1525 MCMCOBJ=   -6580.47991444573     
 iteration         1530 MCMCOBJ=   -6624.42112125283     
 iteration         1535 MCMCOBJ=   -6575.56907378287     
 iteration         1540 MCMCOBJ=   -6561.98715700556     
 iteration         1545 MCMCOBJ=   -6604.92870775498     
 iteration         1550 MCMCOBJ=   -6541.47428620629     
 iteration         1555 MCMCOBJ=   -6603.16707695220     
 iteration         1560 MCMCOBJ=   -6605.27515900195     
 iteration         1565 MCMCOBJ=   -6578.73472537262     
 iteration         1570 MCMCOBJ=   -6569.90025367026     
 iteration         1575 MCMCOBJ=   -6605.68812895635     
 iteration         1580 MCMCOBJ=   -6586.57686246374     
 iteration         1585 MCMCOBJ=   -6586.25292594290     
 iteration         1590 MCMCOBJ=   -6626.59186258691     
 iteration         1595 MCMCOBJ=   -6593.57626614814     
 iteration         1600 MCMCOBJ=   -6578.28570247104     
 iteration         1605 MCMCOBJ=   -6580.21071401146     
 iteration         1610 MCMCOBJ=   -6588.30997531017     
 iteration         1615 MCMCOBJ=   -6670.97731076911     
 iteration         1620 MCMCOBJ=   -6608.35738936720     
 iteration         1625 MCMCOBJ=   -6645.22009867481     
 iteration         1630 MCMCOBJ=   -6587.32528686434     
 iteration         1635 MCMCOBJ=   -6636.74416099342     
 iteration         1640 MCMCOBJ=   -6561.21871986202     
 iteration         1645 MCMCOBJ=   -6597.90218882997     
 iteration         1650 MCMCOBJ=   -6575.64050819633     
 iteration         1655 MCMCOBJ=   -6600.29004766945     
 iteration         1660 MCMCOBJ=   -6584.29407627548     
 iteration         1665 MCMCOBJ=   -6559.24670534376     
 iteration         1670 MCMCOBJ=   -6580.29019328658     
 iteration         1675 MCMCOBJ=   -6578.08222485736     
 iteration         1680 MCMCOBJ=   -6647.84014671279     
 iteration         1685 MCMCOBJ=   -6633.78624496455     
 iteration         1690 MCMCOBJ=   -6610.26192590553     
 iteration         1695 MCMCOBJ=   -6644.54612365462     
 iteration         1700 MCMCOBJ=   -6606.42458829447     
 iteration         1705 MCMCOBJ=   -6515.21131610782     
 iteration         1710 MCMCOBJ=   -6563.77465458063     
 iteration         1715 MCMCOBJ=   -6591.16943775387     
 iteration         1720 MCMCOBJ=   -6617.02875871420     
 iteration         1725 MCMCOBJ=   -6598.08210085248     
 iteration         1730 MCMCOBJ=   -6604.76167892665     
 iteration         1735 MCMCOBJ=   -6596.50558928283     
 iteration         1740 MCMCOBJ=   -6562.62250171894     
 iteration         1745 MCMCOBJ=   -6624.51157322231     
 iteration         1750 MCMCOBJ=   -6564.92058965881     
 iteration         1755 MCMCOBJ=   -6586.59684297112     
 iteration         1760 MCMCOBJ=   -6548.13377262926     
 iteration         1765 MCMCOBJ=   -6536.53160244425     
 iteration         1770 MCMCOBJ=   -6595.70611892373     
 iteration         1775 MCMCOBJ=   -6580.42826426666     
 iteration         1780 MCMCOBJ=   -6547.96387862273     
 iteration         1785 MCMCOBJ=   -6613.67672619756     
 iteration         1790 MCMCOBJ=   -6584.46819267091     
 iteration         1795 MCMCOBJ=   -6562.42564586692     
 iteration         1800 MCMCOBJ=   -6631.51183606489     
 iteration         1805 MCMCOBJ=   -6600.14140844690     
 iteration         1810 MCMCOBJ=   -6568.72237844759     
 iteration         1815 MCMCOBJ=   -6591.61162397922     
 iteration         1820 MCMCOBJ=   -6638.93885925221     
 iteration         1825 MCMCOBJ=   -6597.92722655060     
 iteration         1830 MCMCOBJ=   -6615.17729238006     
 iteration         1835 MCMCOBJ=   -6567.87669248957     
 iteration         1840 MCMCOBJ=   -6527.53166764256     
 iteration         1845 MCMCOBJ=   -6624.84395921869     
 iteration         1850 MCMCOBJ=   -6594.19690920255     
 iteration         1855 MCMCOBJ=   -6660.49375933166     
 iteration         1860 MCMCOBJ=   -6657.38436528545     
 iteration         1865 MCMCOBJ=   -6605.39759623661     
 iteration         1870 MCMCOBJ=   -6620.18579928400     
 iteration         1875 MCMCOBJ=   -6605.28772032839     
 iteration         1880 MCMCOBJ=   -6563.33825402146     
 iteration         1885 MCMCOBJ=   -6570.31354991784     
 iteration         1890 MCMCOBJ=   -6618.23587265924     
 iteration         1895 MCMCOBJ=   -6604.39741015194     
 iteration         1900 MCMCOBJ=   -6588.03966477281     
 iteration         1905 MCMCOBJ=   -6587.28165886391     
 iteration         1910 MCMCOBJ=   -6601.80247632191     
 iteration         1915 MCMCOBJ=   -6550.69121337361     
 iteration         1920 MCMCOBJ=   -6576.91809739084     
 iteration         1925 MCMCOBJ=   -6618.38778049309     
 iteration         1930 MCMCOBJ=   -6624.34038258546     
 iteration         1935 MCMCOBJ=   -6570.43767141388     
 iteration         1940 MCMCOBJ=   -6617.40778973476     
 iteration         1945 MCMCOBJ=   -6616.99435002331     
 iteration         1950 MCMCOBJ=   -6651.56204525290     
 iteration         1955 MCMCOBJ=   -6613.43172082665     
 iteration         1960 MCMCOBJ=   -6601.07377027667     
 iteration         1965 MCMCOBJ=   -6569.95919994524     
 iteration         1970 MCMCOBJ=   -6615.54979868804     
 iteration         1975 MCMCOBJ=   -6672.74964661138     
 iteration         1980 MCMCOBJ=   -6660.48414712995     
 iteration         1985 MCMCOBJ=   -6621.41832123427     
 iteration         1990 MCMCOBJ=   -6608.07677842338     
 iteration         1995 MCMCOBJ=   -6601.05563846600     
 iteration         2000 MCMCOBJ=   -6657.86881553212     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6598.64527252068     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3716.85403239083     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6598.64527252068     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5863.49444595694     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -16.9020929079541     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6598.64527252068     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6615.54736542864     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  4306.37
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6598.645       **************************************************
 #OBJS:********************************************       35.727 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.21E+00  5.60E-01 -1.83E-01  2.27E+00  2.35E-01  3.71E+00 -7.03E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.63E-01
 
 ETA2
+       -2.93E-02  1.77E-01
 
 ETA3
+        2.77E-02 -1.06E-02  1.06E-01
 
 ETA4
+        2.28E-02  3.23E-02 -1.20E-02  2.51E-01
 
 ETA5
+        2.17E-02  1.88E-02 -6.89E-04 -2.35E-02  1.90E-01
 
 ETA6
+       -1.21E-02  1.23E-02  1.59E-02  1.15E-02 -5.48E-02  2.10E-01
 
 ETA7
+        1.22E-02 -3.45E-02  1.85E-02 -5.65E-02  1.91E-02  6.83E-03  2.26E-01
 
 ETA8
+        6.94E-02  6.20E-02  2.60E-02  3.20E-02 -7.43E-03 -4.05E-02  4.78E-02  1.93E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.36E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.10E-01
 
 ETA2
+       -1.34E-01  4.17E-01
 
 ETA3
+        1.70E-01 -8.03E-02  3.22E-01
 
 ETA4
+        8.91E-02  1.55E-01 -7.64E-02  4.98E-01
 
 ETA5
+        9.71E-02  1.04E-01 -8.18E-03 -1.08E-01  4.33E-01
 
 ETA6
+       -5.40E-02  6.53E-02  1.11E-01  5.00E-02 -2.79E-01  4.55E-01
 
 ETA7
+        5.17E-02 -1.70E-01  1.22E-01 -2.37E-01  8.99E-02  3.05E-02  4.73E-01
 
 ETA8
+        3.09E-01  3.40E-01  1.82E-01  1.48E-01 -3.78E-02 -2.03E-01  2.28E-01  4.37E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.67E-02
 
 EPS2
+        0.00E+00  1.50E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         7.23E-02  7.04E-02  5.18E-02  7.28E-02  6.56E-02  7.17E-02  6.51E-02  6.38E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.49E-02
 
 ETA2
+        2.94E-02  5.08E-02
 
 ETA3
+        2.17E-02  2.00E-02  3.06E-02
 
 ETA4
+        3.12E-02  3.02E-02  2.22E-02  5.85E-02
 
 ETA5
+        2.78E-02  2.44E-02  1.87E-02  2.81E-02  4.55E-02
 
 ETA6
+        3.06E-02  2.67E-02  2.10E-02  2.95E-02  2.75E-02  5.61E-02
 
 ETA7
+        2.91E-02  2.91E-02  2.06E-02  3.05E-02  2.72E-02  2.85E-02  4.87E-02
 
 ETA8
+        2.96E-02  2.42E-02  1.92E-02  2.65E-02  2.44E-02  2.60E-02  2.62E-02  4.05E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.45E-04
 
 EPS2
+        0.00E+00  1.23E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.20E-02
 
 ETA2
+        1.27E-01  5.77E-02
 
 ETA3
+        1.26E-01  1.43E-01  4.61E-02
 
 ETA4
+        1.17E-01  1.34E-01  1.32E-01  5.69E-02
 
 ETA5
+        1.20E-01  1.29E-01  1.30E-01  1.22E-01  5.04E-02
 
 ETA6
+        1.28E-01  1.34E-01  1.38E-01  1.25E-01  1.27E-01  5.97E-02
 
 ETA7
+        1.16E-01  1.32E-01  1.29E-01  1.14E-01  1.23E-01  1.27E-01  4.99E-02
 
 ETA8
+        1.12E-01  1.15E-01  1.23E-01  1.15E-01  1.23E-01  1.20E-01  1.10E-01  4.49E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.32E-03
 
 EPS2
+        0.00E+00  4.11E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        5.22E-03
 
 TH 2
+       -6.19E-04  4.96E-03
 
 TH 3
+        3.52E-04 -4.26E-05  2.68E-03
 
 TH 4
+        4.57E-04  6.10E-04 -4.50E-05  5.31E-03
 
 TH 5
+        2.77E-04  3.02E-05 -1.86E-05 -4.38E-04  4.31E-03
 
 TH 6
+       -6.92E-05 -1.37E-04  2.97E-04  1.15E-05 -7.44E-04  5.14E-03
 
 TH 7
+        9.06E-05 -1.05E-03  3.46E-04 -1.22E-03  4.24E-04  2.41E-04  4.24E-03
 
 TH 8
+        1.35E-03  9.01E-04  4.34E-04  8.26E-04 -2.56E-04 -7.30E-04  1.01E-03  4.07E-03
 
 OM11
+       -2.50E-05  3.92E-06 -6.48E-06  5.92E-06 -1.07E-04  1.89E-04 -2.90E-05 -1.17E-05  3.01E-03
 
 OM12
+       -2.48E-05  2.38E-04  6.33E-05 -2.02E-05  1.11E-04  8.08E-06  4.43E-05 -1.88E-06 -2.25E-04  8.65E-04
 
 OM13
+        8.41E-05  1.98E-05  4.77E-05  4.33E-05  1.66E-05  1.57E-05 -3.10E-05  3.65E-05  1.39E-04  1.18E-05  4.73E-04
 
 OM14
+        1.24E-04 -1.77E-05 -3.13E-05  1.72E-05  5.70E-05  5.74E-05 -1.30E-04 -5.40E-05  1.77E-04  1.91E-05  1.18E-05  9.72E-04
 
 OM15
+        1.44E-05 -4.27E-05  8.98E-05  2.21E-05  4.47E-05  7.14E-05  7.30E-05  3.84E-05  1.72E-04  1.70E-05 -1.68E-05 -5.60E-05
          7.73E-04
 
 OM16
+        1.07E-04  6.07E-05  4.25E-05  1.82E-06  1.43E-06 -1.68E-05 -8.44E-05  1.47E-04 -1.90E-05  1.77E-05  4.00E-05  8.16E-05
         -1.27E-04  9.35E-04
 
 OM17
+        1.59E-04 -6.45E-06  7.15E-05 -8.76E-06  7.99E-05 -4.90E-05  3.60E-06  7.91E-05 -1.07E-06 -7.92E-05  6.02E-05 -1.10E-04
          4.73E-05  4.02E-05  8.48E-04
 
 OM18
+        7.37E-05 -1.98E-05 -4.48E-05 -7.56E-06  1.06E-04  4.56E-05 -5.33E-05  2.42E-05  5.94E-04  1.04E-04  1.03E-04  9.97E-05
         -1.80E-05 -1.05E-04  2.09E-04  8.74E-04
 
 OM22
+        5.62E-05 -8.55E-04  3.92E-05  4.73E-05 -2.60E-06 -1.84E-06 -8.00E-05  6.75E-05  1.35E-05 -4.22E-04  2.50E-05 -6.46E-06
          9.40E-06  6.12E-05  4.38E-05 -2.98E-05  2.58E-03
 
 OM23
+       -1.92E-05  1.15E-04 -2.83E-05  8.52E-05 -5.45E-05 -9.63E-06  3.27E-05 -5.77E-05 -4.44E-05  4.10E-05 -3.93E-05  3.20E-05
          1.60E-05 -4.77E-05 -4.61E-05 -2.44E-05 -1.25E-04  4.02E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -6.58E-05 -2.74E-04  9.59E-05 -9.41E-05 -4.63E-05 -5.64E-05 -5.46E-05 -2.75E-05 -9.36E-05 -3.78E-05  1.33E-05 -9.24E-05
          6.44E-06 -1.39E-05  4.23E-05  1.67E-05  2.84E-04 -3.15E-05  9.10E-04
 
 OM25
+       -2.11E-06  1.76E-05 -7.90E-05 -6.83E-05  3.74E-06  4.11E-05  5.31E-05  1.67E-05  6.11E-05  4.52E-05  2.11E-05  3.97E-05
         -7.39E-05  3.70E-05 -1.34E-05  4.37E-05  1.20E-04  1.73E-05 -4.85E-05  5.96E-04
 
 OM26
+       -1.98E-05 -1.56E-05  2.54E-05 -3.06E-05 -1.45E-05  3.90E-05 -2.35E-05 -4.76E-05  1.44E-06  1.67E-05 -3.88E-06 -2.87E-05
          5.84E-05 -6.31E-05  4.98E-05  9.60E-06  1.01E-04  3.84E-05 -2.63E-05 -6.73E-05  7.12E-04
 
 OM27
+       -9.03E-05  5.36E-04 -8.00E-05  1.94E-05 -4.34E-05  5.72E-05 -7.26E-05 -3.54E-05 -3.68E-05  9.25E-05 -1.87E-05 -1.81E-05
          2.72E-06  2.08E-05 -9.83E-05 -4.15E-05 -5.38E-04  9.92E-05 -2.92E-04  6.17E-05  4.33E-06  8.46E-04
 
 OM28
+       -4.58E-06  2.34E-05  2.54E-05  5.26E-05 -7.00E-06  1.57E-05 -8.37E-05  2.09E-05 -1.37E-04  7.66E-05 -2.27E-05 -1.05E-05
          1.34E-05  2.66E-06 -4.88E-05 -4.79E-05  4.05E-04  4.84E-05  9.57E-05 -1.78E-07 -7.70E-05  8.47E-05  5.88E-04
 
 OM33
+        1.39E-05  3.71E-05 -6.57E-05 -1.84E-05 -4.17E-05  1.50E-04 -1.05E-04 -1.05E-04  5.66E-05  2.80E-05  9.93E-05  5.22E-05
          3.64E-05 -7.60E-06  3.74E-05  4.21E-05 -1.06E-05  2.06E-05 -5.85E-06  1.13E-05  2.70E-05  5.09E-05  5.69E-05  9.38E-04
 
 OM34
+        6.15E-06  2.51E-05  9.44E-05  1.58E-04  7.98E-05  1.85E-05 -3.52E-06  6.75E-07 -1.26E-05  8.74E-06  4.04E-05  3.04E-05
          1.07E-05  1.15E-05 -2.61E-05  2.75E-05 -2.02E-05  2.11E-05 -2.41E-05 -6.69E-06 -2.34E-05  2.68E-05  6.49E-06 -6.28E-05
         4.93E-04
 
 OM35
+        6.82E-06  2.14E-05 -1.62E-05  1.70E-07  2.86E-05  8.82E-05 -1.29E-05 -5.35E-05 -1.64E-05 -6.12E-06  2.54E-05 -9.89E-06
          5.26E-05 -1.48E-05  9.21E-06  1.62E-06  2.35E-05  2.94E-05 -2.17E-05 -2.76E-06  1.24E-05  2.67E-05  2.66E-05  8.62E-05
        -4.82E-05  3.52E-04
 
 OM36
+       -3.06E-05  5.13E-06  4.26E-05  7.85E-05 -7.58E-06 -5.16E-05 -3.63E-06 -1.40E-05 -2.30E-05  3.79E-05  2.26E-05  1.10E-05
          2.65E-06 -3.56E-06 -3.44E-05 -2.61E-05  2.09E-05  2.21E-05  6.62E-06 -1.67E-05 -6.65E-06 -3.64E-05 -8.98E-06  1.87E-05
         9.49E-06 -4.71E-05  4.40E-04
 
 OM37
+       -5.59E-05  2.85E-06 -5.60E-05 -9.50E-05 -5.42E-05 -1.39E-06  4.46E-05 -2.49E-05 -1.47E-05 -2.19E-06  1.51E-05 -1.02E-05
          5.90E-06  3.90E-05  5.92E-05  9.32E-06 -5.88E-06 -6.73E-05 -2.22E-05 -2.69E-05  8.91E-06 -8.73E-06 -1.97E-05  7.28E-05
        -6.96E-05  2.81E-05  1.12E-05  4.25E-04
 
 OM38
+       -1.41E-05  6.13E-06  1.59E-05  5.75E-06  7.44E-06  5.47E-05 -4.04E-05 -5.71E-06 -6.92E-06 -1.31E-06  8.18E-05  3.41E-05
         -8.11E-06  1.27E-05  3.28E-05  5.80E-05  3.14E-05  9.42E-05  1.19E-05  1.54E-06  1.30E-05  2.20E-05  3.44E-05  1.71E-04
         5.72E-05 -5.93E-06 -4.37E-05  6.83E-05  3.67E-04
 
 OM44
+        1.83E-04 -9.03E-06  1.77E-04  3.75E-04 -3.59E-05  1.18E-04 -1.76E-04 -5.19E-05 -6.55E-05 -4.03E-05  6.32E-05  2.29E-04
         -6.33E-05  1.45E-06 -5.04E-05  7.39E-05  1.40E-04  2.27E-05  2.59E-04  1.22E-05  2.77E-05 -7.76E-05  1.09E-04 -3.09E-05
         6.64E-05  2.81E-05  1.94E-05 -3.39E-05  1.47E-05  3.42E-03
 
 OM45
+        1.10E-05 -1.17E-05  1.96E-05 -1.14E-05 -7.94E-05 -1.07E-05  8.09E-05 -1.55E-05  9.75E-05 -1.71E-05 -4.62E-06  6.32E-05
          4.59E-05 -1.64E-05 -3.92E-06 -4.80E-06 -5.63E-05  3.50E-05  1.77E-05  2.77E-05  3.67E-05  5.45E-06  1.52E-06  4.95E-06
         4.50E-06  2.79E-06  8.57E-06 -1.25E-05  4.43E-07 -1.88E-04  7.92E-04
 
 OM46
+       -5.22E-05 -4.56E-05 -4.21E-05 -5.48E-05  4.67E-05  1.59E-05 -2.97E-06 -3.94E-05 -5.66E-05  4.46E-05  3.68E-05 -2.94E-05
         -1.75E-05  6.08E-05  2.69E-05  3.31E-05 -3.63E-05 -1.94E-05  2.04E-06 -7.75E-06  4.85E-05  1.50E-05  3.89E-06  2.49E-05
         1.66E-05 -2.85E-05 -1.31E-05 -5.24E-06  2.07E-05  7.77E-05 -7.97E-05  8.69E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.29E-05  1.52E-04 -9.43E-05  6.76E-05 -2.28E-05  4.95E-06 -9.10E-05  1.74E-05 -5.72E-05  5.74E-05 -2.25E-05  1.63E-05
          2.05E-05  4.74E-05  5.72E-05 -8.23E-07 -9.80E-05 -6.23E-06 -1.96E-04  1.56E-05 -2.37E-05  1.63E-04 -1.67E-05  4.73E-05
         5.46E-05 -3.32E-06 -1.56E-05 -3.21E-05  3.02E-05 -5.68E-04  4.95E-05  8.89E-05  9.33E-04
 
 OM48
+        5.87E-05  2.29E-05  5.90E-06  4.61E-05  4.29E-05  3.08E-05 -9.27E-05 -9.81E-05  5.61E-06  3.96E-05  2.87E-05  1.62E-04
         -1.19E-05  1.44E-05  1.00E-05  5.93E-05  2.17E-05  2.36E-05  1.42E-04  3.50E-05 -2.57E-05  1.61E-05  4.89E-05  2.87E-05
         8.85E-05 -2.69E-05  1.44E-05 -3.46E-05  2.29E-06  1.71E-04 -4.21E-05 -9.08E-05  1.17E-04  7.01E-04
 
 OM55
+       -6.68E-05  9.49E-05  6.81E-05 -1.14E-04  7.75E-06 -1.14E-04 -1.18E-04 -1.12E-05  1.10E-04 -1.78E-05  7.07E-06 -4.69E-05
          1.88E-04 -6.31E-06  1.02E-05 -3.75E-05  1.27E-04 -3.42E-05  7.92E-05  9.76E-05  2.53E-05 -3.78E-05  1.21E-05  6.87E-05
         4.87E-06  3.59E-05  3.58E-05 -5.49E-05 -1.28E-05  1.67E-04 -1.66E-04  4.12E-05 -5.87E-05 -1.86E-05  2.07E-03
 
 OM56
+       -3.56E-05  4.34E-05  4.85E-05  5.53E-05 -1.40E-05 -5.65E-05  2.38E-05  4.86E-05  4.90E-06  8.27E-06 -1.04E-05  6.83E-05
          2.47E-05  4.05E-05  3.26E-05  8.74E-06 -5.10E-05 -1.91E-06 -2.88E-05  5.77E-06  4.55E-05  4.09E-05 -4.05E-05 -2.48E-05
         1.51E-05 -1.85E-05  2.74E-05  2.54E-06 -9.37E-06  2.80E-06  2.88E-05 -5.11E-05  4.02E-05  3.40E-05 -2.67E-04  7.57E-04
 
 OM57
+        5.13E-06 -6.52E-05  6.36E-05  1.95E-05  2.22E-05 -2.04E-05 -1.14E-04 -8.40E-06 -1.82E-05 -5.53E-05 -2.05E-06 -2.30E-05
          3.73E-05 -2.23E-05  2.90E-05 -3.74E-05  4.51E-05 -3.05E-06  2.45E-05 -8.04E-05  1.07E-05  2.84E-05  6.55E-06 -7.01E-06
        -2.14E-08  1.73E-05  1.80E-05 -6.53E-07  6.36E-07  4.09E-05 -1.97E-04  2.24E-05 -6.02E-05 -1.31E-05  2.86E-04 -4.31E-06
          7.39E-04
 
 OM58
+        4.08E-05 -1.24E-04 -7.65E-06 -5.61E-05  2.31E-05 -6.03E-06  3.91E-05 -3.77E-06  3.85E-05 -2.59E-07  4.12E-06 -3.90E-06
          1.11E-04 -3.96E-05  1.40E-05  4.31E-05  7.23E-05  2.40E-05 -1.34E-05  1.29E-04 -8.18E-06  9.90E-07  2.24E-05 -1.11E-05
        -1.63E-05  5.23E-05  1.78E-05  7.50E-06  1.52E-05  3.21E-05  3.83E-05 -3.92E-05 -3.04E-05 -1.78E-05 -1.08E-04 -8.32E-05
          1.47E-04  5.97E-04
 
 OM66
+        8.90E-05  1.31E-04 -8.12E-05  1.55E-04 -3.75E-06  1.76E-05 -1.47E-04 -1.44E-05 -3.09E-05  5.81E-05 -3.83E-05 -7.46E-05
         -1.30E-05  8.50E-05 -4.70E-05 -4.58E-05 -6.86E-06 -4.11E-06  6.65E-05 -2.63E-05  4.08E-05 -1.50E-05  7.43E-05  1.06E-05
        -5.30E-05  1.85E-05  2.62E-05  2.95E-05 -4.85E-05  9.71E-05  7.57E-05  1.76E-04 -4.06E-05 -6.06E-05  6.60E-05 -4.07E-04
          5.88E-06  1.09E-05  3.15E-03
 
 OM67
+       -7.93E-05  1.22E-05  4.00E-05  3.96E-05 -1.10E-04 -1.48E-05  3.94E-06  1.54E-05  2.03E-05  3.04E-05 -1.25E-05  7.06E-06
          1.10E-05 -1.09E-05 -2.92E-05 -2.39E-05 -3.77E-05 -9.12E-06  4.25E-05  4.06E-06 -1.18E-04 -5.09E-05  1.38E-05 -1.94E-05
        -1.89E-05 -1.77E-05  4.99E-05  1.36E-05 -1.15E-05  4.30E-05  2.77E-05 -1.39E-04  8.14E-06  3.24E-05 -6.95E-05  3.58E-05
         -1.11E-04 -2.69E-05  1.10E-04  8.13E-04
 
 OM68
+        8.25E-06  2.47E-05  1.10E-04 -1.25E-05  7.30E-05  9.62E-06 -2.36E-05  2.25E-05 -9.73E-06 -3.78E-07  1.83E-05  9.78E-06
         -2.20E-05  1.58E-04  1.81E-05 -3.86E-05 -2.16E-05 -2.12E-05 -3.31E-06 -2.88E-05  1.50E-04 -3.08E-05 -4.81E-05  2.66E-06
         1.71E-05 -4.61E-05  5.98E-05  9.48E-06  1.37E-05 -1.76E-05 -1.86E-05  5.20E-05 -3.05E-05  3.82E-06  1.60E-05 -2.37E-05
         -2.27E-05 -9.03E-05 -3.40E-04  1.50E-04  6.76E-04
 
 OM77
+        8.43E-05 -1.99E-04  2.12E-04  6.87E-05  2.79E-05  1.48E-05  8.13E-05 -6.12E-05  9.54E-05 -1.12E-04 -2.38E-05 -4.17E-05
         -5.50E-06 -1.04E-04  1.53E-06 -3.84E-05  1.27E-04 -1.48E-05  1.64E-04 -3.18E-05 -1.42E-05 -2.98E-04 -4.40E-07 -2.05E-05
        -4.03E-05  3.59E-05  3.20E-06  8.69E-05 -2.64E-05  3.24E-04 -5.07E-05 -9.15E-05 -4.51E-04 -5.47E-05  1.23E-05 -3.78E-05
          1.99E-04  1.02E-04 -7.36E-05  7.24E-05  4.27E-05  2.37E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -2.05E-07  1.23E-05  2.75E-05  1.31E-05  3.25E-05 -2.24E-05 -2.20E-05  4.45E-05  3.55E-05 -9.03E-06 -5.01E-06 -5.59E-06
         -1.04E-05  1.30E-05  1.61E-04  6.62E-05 -6.27E-05  4.92E-06 -3.87E-05  2.16E-05  7.81E-06  6.87E-05 -3.30E-05  1.03E-05
        -1.32E-05  2.15E-05 -2.20E-06  5.75E-05  4.99E-05 -5.37E-05  1.48E-05 -3.31E-06  9.17E-05 -9.42E-05  1.58E-05 -8.81E-07
          2.43E-05  4.21E-05 -3.56E-05 -1.00E-04 -2.84E-05  3.82E-04  6.84E-04
 
 OM88
+        8.47E-05  1.05E-04 -5.41E-05  1.59E-04 -3.43E-05  1.68E-04 -8.81E-05  1.32E-05  1.53E-04  5.27E-05  5.30E-05  2.19E-05
          2.53E-05 -4.04E-05  1.16E-04  4.34E-04  4.84E-05  4.49E-05  2.00E-05  1.02E-05 -5.35E-05  7.43E-05  3.10E-04  1.24E-04
         3.65E-05  3.97E-05 -2.91E-05  9.36E-06  1.79E-04  1.25E-04  2.26E-06 -2.87E-05  9.77E-05  1.14E-04  3.78E-05 -1.15E-05
         -2.38E-05 -7.33E-05 -3.04E-05 -9.77E-05 -2.41E-04  5.81E-05  3.56E-04  1.64E-03
 
 SG11
+       -4.63E-07  1.40E-07  3.33E-07  1.04E-07  3.03E-07 -1.35E-06  4.69E-07  4.11E-07 -1.64E-07  1.60E-07  7.90E-08 -7.97E-07
          1.60E-07  8.21E-07 -5.27E-07 -3.13E-07  2.87E-08 -6.83E-07  1.94E-07 -4.50E-07 -3.60E-07 -3.71E-07  1.81E-07 -8.34E-07
         1.19E-09 -1.71E-07 -5.02E-07 -3.08E-07 -8.11E-07 -2.26E-06  5.19E-07  1.75E-08  1.06E-07 -5.87E-07  2.63E-07 -7.45E-07
          2.08E-07 -6.25E-07  1.13E-06  4.06E-07  6.93E-07  1.36E-07 -7.94E-07 -5.44E-07  4.16E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.83E-06  6.61E-07 -7.81E-07  1.14E-07 -2.63E-06 -3.86E-06  1.51E-06  8.16E-07  1.57E-06  1.67E-06 -8.80E-08  1.11E-07
         -4.92E-07 -1.19E-06 -1.17E-06 -9.36E-07 -2.20E-07  7.43E-07 -6.09E-07  1.27E-06 -9.12E-08  1.92E-06  4.93E-07 -1.26E-06
        -4.48E-07 -1.20E-06  8.52E-07  7.75E-08 -1.57E-06 -6.26E-07 -3.13E-07 -1.01E-06 -1.00E-06  5.05E-07 -3.61E-07 -8.40E-07
         -2.21E-06 -9.05E-07 -3.51E-06  8.60E-08  1.44E-07  6.82E-07 -3.86E-07 -1.71E-06  1.61E-08  0.00E+00  1.52E-06
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        7.23E-02
 
 TH 2
+       -1.22E-01  7.04E-02
 
 TH 3
+        9.41E-02 -1.17E-02  5.18E-02
 
 TH 4
+        8.68E-02  1.19E-01 -1.19E-02  7.28E-02
 
 TH 5
+        5.84E-02  6.54E-03 -5.46E-03 -9.15E-02  6.56E-02
 
 TH 6
+       -1.33E-02 -2.71E-02  7.99E-02  2.21E-03 -1.58E-01  7.17E-02
 
 TH 7
+        1.93E-02 -2.29E-01  1.03E-01 -2.56E-01  9.92E-02  5.17E-02  6.51E-02
 
 TH 8
+        2.93E-01  2.01E-01  1.31E-01  1.78E-01 -6.11E-02 -1.60E-01  2.44E-01  6.38E-02
 
 OM11
+       -6.29E-03  1.01E-03 -2.28E-03  1.48E-03 -2.98E-02  4.79E-02 -8.11E-03 -3.35E-03  5.49E-02
 
 OM12
+       -1.17E-02  1.15E-01  4.15E-02 -9.44E-03  5.75E-02  3.83E-03  2.31E-02 -1.00E-03 -1.39E-01  2.94E-02
 
 OM13
+        5.35E-02  1.29E-02  4.23E-02  2.73E-02  1.16E-02  1.01E-02 -2.19E-02  2.63E-02  1.16E-01  1.84E-02  2.17E-02
 
 OM14
+        5.49E-02 -8.05E-03 -1.94E-02  7.58E-03  2.78E-02  2.57E-02 -6.40E-02 -2.72E-02  1.03E-01  2.08E-02  1.75E-02  3.12E-02
 
 OM15
+        7.19E-03 -2.18E-02  6.23E-02  1.09E-02  2.45E-02  3.58E-02  4.04E-02  2.16E-02  1.12E-01  2.08E-02 -2.78E-02 -6.46E-02
          2.78E-02
 
 OM16
+        4.86E-02  2.82E-02  2.68E-02  8.19E-04  7.15E-04 -7.68E-03 -4.24E-02  7.56E-02 -1.13E-02  1.97E-02  6.01E-02  8.56E-02
         -1.50E-01  3.06E-02
 
 OM17
+        7.57E-02 -3.15E-03  4.74E-02 -4.13E-03  4.18E-02 -2.35E-02  1.90E-03  4.26E-02 -6.72E-04 -9.25E-02  9.50E-02 -1.21E-01
          5.85E-02  4.52E-02  2.91E-02
 
 OM18
+        3.45E-02 -9.50E-03 -2.92E-02 -3.51E-03  5.49E-02  2.15E-02 -2.77E-02  1.28E-02  3.66E-01  1.20E-01  1.60E-01  1.08E-01
         -2.19E-02 -1.16E-01  2.43E-01  2.96E-02
 
 OM22
+        1.53E-02 -2.39E-01  1.49E-02  1.28E-02 -7.80E-04 -5.06E-04 -2.42E-02  2.09E-02  4.86E-03 -2.82E-01  2.27E-02 -4.08E-03
          6.66E-03  3.94E-02  2.96E-02 -1.98E-02  5.08E-02
 
 OM23
+       -1.33E-02  8.16E-02 -2.73E-02  5.83E-02 -4.14E-02 -6.70E-03  2.50E-02 -4.51E-02 -4.04E-02  6.95E-02 -9.02E-02  5.12E-02
          2.88E-02 -7.79E-02 -7.90E-02 -4.13E-02 -1.23E-01  2.00E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -3.02E-02 -1.29E-01  6.14E-02 -4.28E-02 -2.34E-02 -2.61E-02 -2.78E-02 -1.43E-02 -5.66E-02 -4.26E-02  2.03E-02 -9.83E-02
          7.68E-03 -1.51E-02  4.81E-02  1.87E-02  1.85E-01 -5.22E-02  3.02E-02
 
 OM25
+       -1.20E-03  1.02E-02 -6.25E-02 -3.84E-02  2.33E-03  2.35E-02  3.34E-02  1.08E-02  4.56E-02  6.29E-02  3.97E-02  5.22E-02
         -1.09E-01  4.95E-02 -1.88E-02  6.05E-02  9.72E-02  3.54E-02 -6.58E-02  2.44E-02
 
 OM26
+       -1.03E-02 -8.29E-03  1.84E-02 -1.58E-02 -8.29E-03  2.04E-02 -1.35E-02 -2.80E-02  9.86E-04  2.13E-02 -6.69E-03 -3.46E-02
          7.88E-02 -7.74E-02  6.41E-02  1.22E-02  7.46E-02  7.18E-02 -3.27E-02 -1.03E-01  2.67E-02
 
 OM27
+       -4.29E-02  2.62E-01 -5.31E-02  9.16E-03 -2.27E-02  2.74E-02 -3.83E-02 -1.91E-02 -2.31E-02  1.08E-01 -2.95E-02 -1.99E-02
          3.36E-03  2.34E-02 -1.16E-01 -4.83E-02 -3.65E-01  1.70E-01 -3.32E-01  8.69E-02  5.58E-03  2.91E-02
 
 OM28
+       -2.61E-03  1.37E-02  2.03E-02  2.98E-02 -4.40E-03  9.06E-03 -5.30E-02  1.35E-02 -1.03E-01  1.07E-01 -4.30E-02 -1.39E-02
          1.99E-02  3.59E-03 -6.91E-02 -6.68E-02  3.29E-01  9.95E-02  1.31E-01 -3.01E-04 -1.19E-01  1.20E-01  2.42E-02
 
 OM33
+        6.26E-03  1.72E-02 -4.14E-02 -8.25E-03 -2.08E-02  6.81E-02 -5.26E-02 -5.38E-02  3.36E-02  3.11E-02  1.49E-01  5.47E-02
          4.27E-02 -8.11E-03  4.20E-02  4.65E-02 -6.84E-03  3.36E-02 -6.33E-03  1.52E-02  3.30E-02  5.72E-02  7.66E-02  3.06E-02
 
 OM34
+        3.84E-03  1.61E-02  8.21E-02  9.75E-02  5.48E-02  1.16E-02 -2.44E-03  4.77E-04 -1.03E-02  1.34E-02  8.37E-02  4.39E-02
          1.74E-02  1.69E-02 -4.03E-02  4.20E-02 -1.79E-02  4.74E-02 -3.60E-02 -1.24E-02 -3.95E-02  4.15E-02  1.21E-02 -9.24E-02
         2.22E-02
 
 OM35
+        5.03E-03  1.62E-02 -1.66E-02  1.24E-04  2.32E-02  6.56E-02 -1.06E-02 -4.48E-02 -1.60E-02 -1.11E-02  6.23E-02 -1.69E-02
          1.01E-01 -2.59E-02  1.69E-02  2.92E-03  2.47E-02  7.83E-02 -3.83E-02 -6.02E-03  2.47E-02  4.89E-02  5.84E-02  1.50E-01
        -1.16E-01  1.87E-02
 
 OM36
+       -2.02E-02  3.47E-03  3.92E-02  5.14E-02 -5.51E-03 -3.43E-02 -2.66E-03 -1.05E-02 -2.00E-02  6.15E-02  4.95E-02  1.69E-02
          4.55E-03 -5.56E-03 -5.63E-02 -4.21E-02  1.96E-02  5.27E-02  1.05E-02 -3.26E-02 -1.19E-02 -5.97E-02 -1.77E-02  2.91E-02
         2.04E-02 -1.20E-01  2.10E-02
 
 OM37
+       -3.75E-02  1.96E-03 -5.25E-02 -6.33E-02 -4.01E-02 -9.40E-04  3.32E-02 -1.90E-02 -1.30E-02 -3.62E-03  3.36E-02 -1.58E-02
          1.03E-02  6.19E-02  9.86E-02  1.53E-02 -5.62E-03 -1.63E-01 -3.57E-02 -5.34E-02  1.62E-02 -1.46E-02 -3.94E-02  1.15E-01
        -1.52E-01  7.27E-02  2.59E-02  2.06E-02
 
 OM38
+       -1.02E-02  4.54E-03  1.60E-02  4.12E-03  5.91E-03  3.98E-02 -3.24E-02 -4.67E-03 -6.58E-03 -2.32E-03  1.96E-01  5.70E-02
         -1.52E-02  2.16E-02  5.88E-02  1.02E-01  3.23E-02  2.45E-01  2.06E-02  3.28E-03  2.55E-02  3.94E-02  7.41E-02  2.91E-01
         1.35E-01 -1.65E-02 -1.09E-01  1.73E-01  1.92E-02
 
 OM44
+        4.34E-02 -2.19E-03  5.85E-02  8.81E-02 -9.35E-03  2.81E-02 -4.62E-02 -1.39E-02 -2.04E-02 -2.34E-02  4.97E-02  1.26E-01
         -3.89E-02  8.13E-04 -2.96E-02  4.28E-02  4.73E-02  1.93E-02  1.47E-01  8.53E-03  1.77E-02 -4.56E-02  7.67E-02 -1.73E-02
         5.11E-02  2.56E-02  1.58E-02 -2.81E-02  1.31E-02  5.85E-02
 
 OM45
+        5.42E-03 -5.89E-03  1.34E-02 -5.56E-03 -4.30E-02 -5.30E-03  4.41E-02 -8.64E-03  6.31E-02 -2.06E-02 -7.55E-03  7.21E-02
          5.86E-02 -1.91E-02 -4.79E-03 -5.77E-03 -3.94E-02  6.21E-02  2.09E-02  4.02E-02  4.89E-02  6.66E-03  2.23E-03  5.74E-03
         7.20E-03  5.29E-03  1.45E-02 -2.15E-02  8.22E-04 -1.14E-01  2.81E-02
 
 OM46
+       -2.45E-02 -2.19E-02 -2.75E-02 -2.55E-02  2.41E-02  7.52E-03 -1.55E-03 -2.10E-02 -3.50E-02  5.14E-02  5.74E-02 -3.20E-02
         -2.14E-02  6.75E-02  3.13E-02  3.80E-02 -2.43E-02 -3.28E-02  2.30E-03 -1.08E-02  6.17E-02  1.75E-02  5.44E-03  2.76E-02
         2.54E-02 -5.15E-02 -2.13E-02 -8.63E-03  3.66E-02  4.50E-02 -9.60E-02  2.95E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -5.84E-03  7.08E-02 -5.96E-02  3.04E-02 -1.14E-02  2.26E-03 -4.57E-02  8.93E-03 -3.41E-02  6.39E-02 -3.39E-02  1.71E-02
          2.41E-02  5.08E-02  6.43E-02 -9.11E-04 -6.32E-02 -1.02E-02 -2.13E-01  2.09E-02 -2.91E-02  1.84E-01 -2.25E-02  5.06E-02
         8.05E-02 -5.80E-03 -2.44E-02 -5.10E-02  5.16E-02 -3.18E-01  5.76E-02  9.88E-02  3.05E-02
 
 OM48
+        3.07E-02  1.23E-02  4.30E-03  2.39E-02  2.47E-02  1.62E-02 -5.37E-02 -5.81E-02  3.86E-03  5.09E-02  4.99E-02  1.96E-01
         -1.62E-02  1.78E-02  1.30E-02  7.58E-02  1.61E-02  4.45E-02  1.78E-01  5.42E-02 -3.63E-02  2.10E-02  7.62E-02  3.54E-02
         1.51E-01 -5.42E-02  2.59E-02 -6.34E-02  4.51E-03  1.10E-01 -5.65E-02 -1.16E-01  1.44E-01  2.65E-02
 
 OM55
+       -2.03E-02  2.96E-02  2.89E-02 -3.43E-02  2.59E-03 -3.48E-02 -3.99E-02 -3.84E-03  4.42E-02 -1.33E-02  7.14E-03 -3.30E-02
          1.49E-01 -4.53E-03  7.70E-03 -2.78E-02  5.52E-02 -3.75E-02  5.77E-02  8.78E-02  2.08E-02 -2.85E-02  1.09E-02  4.92E-02
         4.82E-03  4.20E-02  3.75E-02 -5.85E-02 -1.47E-02  6.27E-02 -1.29E-01  3.07E-02 -4.22E-02 -1.54E-02  4.55E-02
 
 OM56
+       -1.79E-02  2.24E-02  3.40E-02  2.76E-02 -7.78E-03 -2.86E-02  1.33E-02  2.77E-02  3.24E-03  1.02E-02 -1.74E-02  7.96E-02
          3.23E-02  4.82E-02  4.07E-02  1.08E-02 -3.65E-02 -3.47E-03 -3.47E-02  8.59E-03  6.20E-02  5.11E-02 -6.07E-02 -2.95E-02
         2.48E-02 -3.59E-02  4.75E-02  4.48E-03 -1.78E-02  1.74E-03  3.72E-02 -6.30E-02  4.78E-02  4.67E-02 -2.13E-01  2.75E-02
 
 OM57
+        2.61E-03 -3.41E-02  4.52E-02  9.86E-03  1.24E-02 -1.04E-02 -6.44E-02 -4.85E-03 -1.22E-02 -6.92E-02 -3.47E-03 -2.71E-02
          4.93E-02 -2.68E-02  3.66E-02 -4.66E-02  3.27E-02 -5.59E-03  2.99E-02 -1.21E-01  1.47E-02  3.59E-02  9.95E-03 -8.42E-03
        -3.55E-05  3.40E-02  3.16E-02 -1.17E-03  1.22E-03  2.57E-02 -2.58E-01  2.80E-02 -7.25E-02 -1.82E-02  2.31E-01 -5.76E-03
          2.72E-02
 
 OM58
+        2.31E-02 -7.18E-02 -6.05E-03 -3.15E-02  1.44E-02 -3.45E-03  2.46E-02 -2.42E-03  2.87E-02 -3.61E-04  7.76E-03 -5.12E-03
          1.64E-01 -5.30E-02  1.97E-02  5.96E-02  5.83E-02  4.89E-02 -1.82E-02  2.17E-01 -1.26E-02  1.39E-03  3.79E-02 -1.48E-02
        -3.01E-02  1.14E-01  3.48E-02  1.49E-02  3.25E-02  2.25E-02  5.57E-02 -5.44E-02 -4.08E-02 -2.75E-02 -9.69E-02 -1.24E-01
          2.22E-01  2.44E-02
 
 OM66
+        2.19E-02  3.31E-02 -2.80E-02  3.79E-02 -1.02E-03  4.38E-03 -4.02E-02 -4.03E-03 -1.00E-02  3.52E-02 -3.14E-02 -4.27E-02
         -8.32E-03  4.95E-02 -2.88E-02 -2.76E-02 -2.41E-03 -3.65E-03  3.93E-02 -1.92E-02  2.72E-02 -9.19E-03  5.46E-02  6.18E-03
        -4.26E-02  1.76E-02  2.22E-02  2.55E-02 -4.51E-02  2.96E-02  4.79E-02  1.06E-01 -2.37E-02 -4.08E-02  2.58E-02 -2.64E-01
          3.86E-03  7.96E-03  5.61E-02
 
 OM67
+       -3.85E-02  6.10E-03  2.71E-02  1.91E-02 -5.87E-02 -7.24E-03  2.12E-03  8.49E-03  1.30E-02  3.62E-02 -2.02E-02  7.94E-03
          1.38E-02 -1.25E-02 -3.52E-02 -2.83E-02 -2.61E-02 -1.60E-02  4.95E-02  5.83E-03 -1.55E-01 -6.14E-02  1.99E-02 -2.22E-02
        -2.99E-02 -3.31E-02  8.35E-02  2.32E-02 -2.10E-02  2.58E-02  3.45E-02 -1.66E-01  9.34E-03  4.29E-02 -5.35E-02  4.56E-02
         -1.43E-01 -3.86E-02  6.88E-02  2.85E-02
 
 OM68
+        4.39E-03  1.35E-02  8.19E-02 -6.62E-03  4.28E-02  5.16E-03 -1.40E-02  1.36E-02 -6.82E-03 -4.95E-04  3.25E-02  1.21E-02
         -3.04E-02  1.98E-01  2.39E-02 -5.02E-02 -1.63E-02 -4.07E-02 -4.23E-03 -4.54E-02  2.17E-01 -4.08E-02 -7.63E-02  3.33E-03
         2.97E-02 -9.46E-02  1.10E-01  1.77E-02  2.76E-02 -1.16E-02 -2.54E-02  6.78E-02 -3.84E-02  5.55E-03  1.35E-02 -3.32E-02
         -3.21E-02 -1.42E-01 -2.33E-01  2.02E-01  2.60E-02
 
 OM77
+        2.39E-02 -5.80E-02  8.39E-02  1.94E-02  8.72E-03  4.23E-03  2.56E-02 -1.97E-02  3.57E-02 -7.81E-02 -2.24E-02 -2.74E-02
         -4.06E-03 -6.98E-02  1.08E-03 -2.67E-02  5.15E-02 -1.51E-02  1.12E-01 -2.68E-02 -1.09E-02 -2.11E-01 -3.73E-04 -1.37E-02
        -3.73E-02  3.93E-02  3.14E-03  8.66E-02 -2.83E-02  1.14E-01 -3.69E-02 -6.37E-02 -3.03E-01 -4.24E-02  5.53E-03 -2.82E-02
          1.51E-01  8.59E-02 -2.69E-02  5.21E-02  3.37E-02  4.87E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -1.08E-04  6.66E-03  2.03E-02  6.89E-03  1.89E-02 -1.20E-02 -1.29E-02  2.67E-02  2.48E-02 -1.17E-02 -8.81E-03 -6.86E-03
         -1.43E-02  1.63E-02  2.11E-01  8.56E-02 -4.72E-02  9.39E-03 -4.91E-02  3.38E-02  1.12E-02  9.03E-02 -5.20E-02  1.28E-02
        -2.27E-02  4.39E-02 -4.01E-03  1.07E-01  9.95E-02 -3.51E-02  2.01E-02 -4.29E-03  1.15E-01 -1.36E-01  1.33E-02 -1.22E-03
          3.42E-02  6.60E-02 -2.42E-02 -1.34E-01 -4.18E-02  3.00E-01  2.62E-02
 
 OM88
+        2.89E-02  3.68E-02 -2.58E-02  5.39E-02 -1.29E-02  5.79E-02 -3.34E-02  5.10E-03  6.87E-02  4.42E-02  6.02E-02  1.73E-02
          2.24E-02 -3.26E-02  9.83E-02  3.62E-01  2.35E-02  5.53E-02  1.64E-02  1.03E-02 -4.95E-02  6.30E-02  3.15E-01  9.97E-02
         4.05E-02  5.22E-02 -3.42E-02  1.12E-02  2.31E-01  5.29E-02  1.98E-03 -2.40E-02  7.89E-02  1.07E-01  2.05E-02 -1.03E-02
         -2.16E-02 -7.40E-02 -1.34E-02 -8.45E-02 -2.29E-01  2.94E-02  3.36E-01  4.05E-02
 
 SG11
+       -9.94E-03  3.08E-03  9.96E-03  2.22E-03  7.15E-03 -2.91E-02  1.12E-02  1.00E-02 -4.64E-03  8.41E-03  5.63E-03 -3.96E-02
          8.93E-03  4.16E-02 -2.81E-02 -1.64E-02  8.76E-04 -5.28E-02  9.98E-03 -2.86E-02 -2.09E-02 -1.98E-02  1.16E-02 -4.22E-02
         8.31E-05 -1.41E-02 -3.71E-02 -2.31E-02 -6.55E-02 -5.99E-02  2.86E-02  9.21E-04  5.36E-03 -3.44E-02  8.97E-03 -4.20E-02
          1.19E-02 -3.97E-02  3.13E-02  2.21E-02  4.13E-02  4.34E-03 -4.71E-02 -2.08E-02  6.45E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -3.18E-02  7.62E-03 -1.23E-02  1.27E-03 -3.26E-02 -4.37E-02  1.89E-02  1.04E-02  2.33E-02  4.61E-02 -3.29E-03  2.89E-03
         -1.44E-02 -3.17E-02 -3.26E-02 -2.57E-02 -3.52E-03  3.01E-02 -1.64E-02  4.22E-02 -2.78E-03  5.37E-02  1.65E-02 -3.34E-02
        -1.64E-02 -5.19E-02  3.30E-02  3.06E-03 -6.66E-02 -8.70E-03 -9.03E-03 -2.77E-02 -2.66E-02  1.55E-02 -6.44E-03 -2.48E-02
         -6.61E-02 -3.01E-02 -5.08E-02  2.45E-03  4.49E-03  1.14E-02 -1.20E-02 -3.42E-02  2.03E-02  0.00E+00  1.23E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        2.33E+02
 
 TH 2
+        6.22E+01  2.84E+02
 
 TH 3
+       -1.73E+01 -8.22E-01  4.09E+02
 
 TH 4
+       -1.66E+00  4.94E+00  1.13E+01  2.28E+02
 
 TH 5
+       -2.63E+01 -2.74E+01  3.51E+00  8.71E+00  2.60E+02
 
 TH 6
+       -1.26E+01 -1.53E+01 -2.94E+01 -1.27E+01  4.74E+01  2.19E+02
 
 TH 7
+        3.87E+01  1.03E+02 -2.10E+01  8.35E+01 -4.72E+01 -3.99E+01  3.39E+02
 
 TH 8
+       -1.04E+02 -1.18E+02 -3.68E+01 -7.16E+01  5.15E+01  6.50E+01 -1.48E+02  3.85E+02
 
 OM11
+        4.30E+00 -1.12E+01 -7.19E+00 -3.38E+00  1.34E+01 -9.17E+00 -6.95E+00  9.69E+00  4.41E+02
 
 OM12
+       -4.11E+00 -3.81E+01 -5.17E+01 -8.62E+00 -2.56E+01 -3.52E+00 -3.06E+01  2.59E+01  1.83E+02  1.56E+03
 
 OM13
+       -3.41E+01 -2.35E+01 -2.78E+01 -1.68E+01  9.93E+00  1.31E+01 -5.23E+00  3.23E+00 -8.75E+01 -6.38E+01  2.45E+03
 
 OM14
+       -2.42E+01  7.73E+00  8.52E+00  1.45E+01 -1.81E+01 -1.30E+01  3.67E+01  7.86E-01 -3.23E+01  2.03E+01  4.43E+01  1.23E+03
 
 OM15
+        4.65E+00  2.57E+01 -3.12E+01 -8.92E+00 -3.18E+01 -2.96E+01  1.96E-01 -3.72E+01 -1.37E+02 -1.38E+02  5.27E+01  4.08E+01
          1.58E+03
 
 OM16
+       -5.57E+00  1.26E+01 -7.76E+00  1.86E+01 -2.96E-01 -6.38E+00  4.07E+01 -5.75E+01 -6.43E+01 -1.41E+02 -8.78E+01 -1.24E+02
          2.37E+02  1.31E+03
 
 OM17
+       -4.54E+01 -4.06E+01 -3.98E+01 -8.31E+00 -1.08E+01  1.12E+01 -2.04E+01  2.48E+01  1.16E+02  2.62E+02 -1.12E+02  2.33E+02
         -1.49E+02 -1.48E+02  1.52E+03
 
 OM18
+        1.16E+01  3.91E+01  3.24E+01  2.44E+01 -4.40E+01 -4.70E+00  5.51E+01 -5.79E+01 -3.79E+02 -4.57E+02 -1.44E+02 -1.86E+02
          2.54E+02  3.38E+02 -5.11E+02  2.01E+03
 
 OM22
+        1.30E+01  6.46E+01 -2.58E+00 -9.73E+00 -7.26E+00 -5.85E+00  1.99E+01 -2.19E+01  2.00E+01  3.78E+02 -2.13E+01  1.98E+01
         -5.06E+01 -9.59E+01  6.71E+01 -1.15E+02  7.14E+02
 
 OM23
+       -3.30E+01 -7.36E+01  4.20E+01 -6.49E+01  7.23E+01  5.02E+01 -1.13E+02  1.07E+02  1.44E+01  8.76E+00  4.08E+02 -6.86E+01
         -9.19E+01  3.99E+01  5.63E+01  1.57E+01  1.91E+02  3.28E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        3.42E+01  3.49E+01 -2.32E+01  3.43E+01  2.02E+01  7.97E+00  3.98E+01 -2.04E+01  5.81E+01  4.64E+01  2.21E+00  2.32E+02
         -3.73E+01 -3.13E+01  2.58E+01 -1.11E+02  1.17E+01  6.36E+01  1.50E+03
 
 OM25
+        5.54E+00 -4.80E+00  5.09E+01  1.96E+01 -3.99E+00 -3.03E+01 -1.48E+01 -2.13E+01 -3.98E+01 -2.32E+02 -7.95E+01 -2.70E+01
          3.46E+02  1.31E+01 -3.90E+01  5.36E+01 -2.66E+02 -1.61E+02  6.56E+01  2.16E+03
 
 OM26
+        1.22E+01  6.56E+00 -6.18E-01  2.06E+01  1.86E+01 -8.62E+00  2.19E+01 -4.13E+00 -1.24E+01 -2.22E+02  1.09E+01  3.41E+01
         -2.02E+01  2.62E+02 -1.19E+02  9.94E+01 -2.55E+02 -2.75E+02  3.53E+01  3.02E+02  1.83E+03
 
 OM27
+       -8.71E+00 -1.18E+02  8.02E+00  5.87E+00  2.03E+01 -6.90E+00 -1.85E+01  5.00E+01  2.47E+01  2.23E+02 -7.14E+00  1.77E+02
         -8.16E+01 -1.07E+02  2.92E+02 -1.11E+02  4.74E+02 -1.30E+02  4.89E+02 -3.14E+02 -1.90E+02  2.02E+03
 
 OM28
+        1.48E+01 -4.60E+00 -2.48E+01  2.05E+01 -1.81E+01  5.66E+00  5.44E+01 -4.81E+01 -4.69E+00 -5.93E+02  1.23E+02 -5.72E+01
          1.01E+02  1.52E+02 -1.17E+02  5.48E+02 -7.23E+02 -3.59E+02 -3.01E+02  3.41E+02  5.11E+02 -8.03E+02  3.04E+03
 
 OM33
+       -1.15E+01  1.37E+00  2.88E+01  9.46E-01  1.19E+01 -2.09E+01  1.23E+01  2.06E+01 -2.13E+01 -1.66E+01 -1.46E+02 -5.96E+01
         -5.55E+01  2.52E+01 -4.53E+01  1.29E+01  3.77E+01  1.28E+02 -2.08E+00 -4.66E+01 -4.41E+01 -5.16E+01 -1.27E+02  1.28E+03
 
 OM34
+        1.03E+01 -4.48E+00 -6.58E+01 -6.67E+01 -3.27E+01 -4.99E+00 -2.92E+01  2.93E+01  4.05E+01  4.12E+01 -1.55E+02  2.11E+01
         -9.32E+01 -3.50E+01  1.01E+02 -1.14E+02  1.44E+01  6.36E+01  1.24E+02  4.61E+01  8.40E+01 -2.20E+01 -3.70E+01  2.24E+02
         2.34E+03
 
 OM35
+       -8.59E-01 -1.44E+01 -5.80E+00 -1.08E+01 -4.52E+01 -4.91E+01  4.14E+00  3.07E+01  5.52E+01  2.64E+01 -2.99E+02  1.23E+01
         -1.33E+02 -2.08E+01  2.27E+01 -1.23E+01 -6.26E+01 -4.22E+02  4.59E+01  1.20E+02 -1.94E+01 -6.83E+01 -2.65E+01 -3.00E+02
         2.18E+02  3.26E+03
 
 OM36
+        1.52E+01 -8.28E+00 -3.63E+01 -3.48E+01 -3.65E+00  1.63E+01 -7.78E+00  2.00E+01  1.45E+01 -1.49E+02 -2.53E+02 -6.14E+00
          6.61E+01  9.08E+01  1.05E+02  1.12E+02 -1.01E+02 -4.07E+02  1.45E+01  1.83E+02  1.59E+02  1.02E+02  1.74E+02 -1.77E+02
        -7.42E+01  4.42E+02  2.60E+03
 
 OM37
+        1.15E+01 -3.58E+01  7.00E+01  2.04E+01  6.32E+01  2.43E+01 -6.59E+01  4.49E+01  3.03E+01 -1.79E+01  9.83E+01  4.97E+00
         -9.26E+01 -1.35E+02 -1.22E+02 -4.88E+01  2.40E+01  7.33E+02  1.30E+02  9.01E+01 -4.45E+01 -1.25E+01 -1.53E+01 -5.01E+01
         3.73E+02 -2.66E+02 -2.46E+02  2.84E+03
 
 OM38
+        4.20E+01  3.87E+01 -5.32E+01  2.74E+01 -4.71E+01 -3.02E+01  6.64E+01 -6.11E+01  4.22E+01  5.62E+00 -6.11E+02 -1.02E+02
          1.32E+02  3.42E+01 -1.98E+01  5.77E+01 -1.37E+02 -1.25E+03 -1.61E+02  1.24E+02  7.30E+01 -5.26E+01  2.16E+02 -6.35E+02
        -5.25E+02  4.98E+02  6.44E+02 -8.56E+02  4.09E+03
 
 OM44
+       -1.17E+01 -7.93E+00 -1.65E+01 -2.67E+01  5.25E+00 -3.91E+00 -1.10E-01  1.21E+01  1.85E+01  2.92E+01 -1.97E+01 -8.55E+01
          2.61E+01 -5.85E+00  7.29E+00 -1.58E+01  1.83E+00  1.36E+01 -5.42E+01  6.06E+00 -3.58E+01 -2.77E+01 -3.15E+01  1.81E+01
        -4.12E+01 -3.97E+01 -5.14E+00  2.58E+01 -5.20E+00  3.64E+02
 
 OM45
+       -7.75E+00 -1.29E-01 -2.26E+01 -5.94E+00  2.77E+01  1.55E+01 -2.57E+01  2.51E+01 -4.49E+01  7.41E+01 -2.81E+01 -1.54E+02
         -6.45E+01  2.29E+01 -3.34E+01  6.51E+01  5.74E+01 -7.88E+01 -1.47E+02 -3.48E+01 -1.06E+02 -2.06E+01 -6.53E+01 -4.42E+00
        -5.58E+01  4.04E+00 -4.52E+01  3.70E+01  3.63E+01  7.58E+01  1.49E+03
 
 OM46
+        2.02E+01  2.54E+01  2.15E+01  1.26E+01 -1.64E+00 -1.89E+00 -6.17E+00  6.42E+00  2.38E+01 -3.59E+01 -1.05E+02  8.58E-01
         -2.14E+01 -4.37E+01 -2.41E+01 -9.20E+01  4.16E+01  2.30E+01 -7.06E+01 -3.62E+01 -2.12E+01 -1.19E+00 -8.09E+01 -1.91E+01
        -3.25E+01  1.18E+02  2.53E+01  2.11E+01 -2.46E+01 -7.24E+01  1.36E+02  1.33E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -8.54E+00 -1.77E+01  1.63E+01 -2.24E+01  1.74E+01  7.76E-02  1.55E+01  3.02E+00  2.53E+01 -4.45E+01  1.27E+02  1.86E+01
         -3.99E+01 -3.63E+01 -4.57E+01  4.99E+01 -3.71E+01  1.55E+02  2.70E+02  3.69E+01  2.23E+01 -6.75E+01  4.89E+01 -4.92E+01
        -9.23E+01 -6.46E+01  1.42E+01  1.58E+02 -1.22E+02  2.31E+02 -7.35E+01 -2.44E+02  1.58E+03
 
 OM48
+       -1.75E+01 -3.50E+00 -3.79E-01  1.67E+00 -1.78E+01  8.40E-01  5.09E+00  4.14E+01 -8.34E+00 -7.08E+01 -1.03E+02 -3.20E+02
          1.75E+01  1.40E+01 -1.30E+02  3.34E+01 -2.34E+01 -1.45E+02 -4.39E+02 -9.96E+01  2.06E+01 -1.79E+02  7.29E+01 -5.36E+01
        -2.78E+02  1.38E+02 -8.80E+00 -4.07E+01  2.47E+02 -8.64E+01  1.72E+02  2.63E+02 -4.27E+02  1.88E+03
 
 OM55
+        2.78E+00 -2.19E+01 -1.15E+01  1.67E+01  9.48E+00  2.54E+01  3.21E+00  8.39E+00 -1.94E+01  5.53E+00  3.06E+01  6.26E+00
         -2.22E+02 -3.84E+01  6.24E+00  7.20E+00  4.43E+00  8.50E+01 -3.10E+01 -2.63E+02 -6.51E+01  5.38E+01 -2.07E+01 -3.05E+01
        -1.49E+01 -7.94E+01 -1.02E+02  9.40E+01 -3.14E+01 -3.19E+01  5.04E+01  6.30E+00  1.36E+01  2.29E+01  6.34E+02
 
 OM56
+        1.14E+01 -1.33E+01 -2.94E+01 -1.57E+01  1.60E+00  2.80E+01 -1.06E+01 -1.43E+00  8.04E+00  1.75E+01  7.05E+01 -8.09E+01
         -2.07E+02 -1.89E+02 -4.60E+01 -7.30E+01  3.51E+01  9.70E+01 -4.20E+00 -2.26E+02 -2.77E+02 -3.62E+01 -2.22E+01  3.10E+01
        -1.24E+01  3.51E-01 -2.14E+02  1.97E+01 -3.10E+01 -2.38E+01 -6.22E+01  2.42E+01 -1.14E+01 -2.75E+01  2.91E+02  1.73E+03
 
 OM57
+        1.66E+01  4.64E+01 -2.62E+01  5.04E-02 -7.21E+00 -9.83E+00  5.72E+01 -3.27E+01 -1.66E+01  2.08E+01 -2.30E+01 -5.92E+01
          1.30E+02  8.41E+01 -1.21E+02  1.49E+02 -5.10E+01 -5.57E+01 -7.49E+01  4.44E+02  9.11E+01 -2.69E+02  1.22E+02  2.19E+01
        -1.18E+01  3.61E+01 -2.69E+01 -1.91E+00  3.38E+01  4.14E+01  4.18E+02 -6.85E+00  8.08E+00  4.08E+01 -3.14E+02 -2.46E+02
          1.89E+03
 
 OM58
+       -1.30E+01  1.94E+01  7.02E+00  1.60E+01  5.33E+00  2.02E+01 -8.58E+00  7.69E+00  3.63E+01  7.27E+01  3.75E+01  4.83E+01
         -4.93E+02 -1.19E+02  6.28E+01 -3.34E+02  6.93E+01  6.67E+01  8.79E+01 -7.34E+02 -1.72E+02  1.30E+02 -3.44E+02  8.97E+01
         6.14E+01 -2.80E+02 -2.82E+02  5.59E+01 -2.97E+02 -4.51E+01 -1.82E+02  7.70E+01  2.66E+01 -3.12E+01  3.40E+02  5.01E+02
         -6.98E+02  2.46E+03
 
 OM66
+       -1.25E+01 -1.53E+01  4.71E-01 -1.38E+01 -9.34E+00 -1.65E+00  4.60E+00  6.58E+00 -8.61E-01  6.38E+00  3.93E+01  2.32E+01
         -2.13E+01 -1.16E+02  2.31E+01 -3.02E+01  2.82E+01  2.67E+01 -9.22E+00 -3.13E+01 -1.55E+02  3.15E+01 -8.86E+01 -3.62E+00
         1.42E+01  1.22E+00 -8.11E+01 -2.59E+01  1.15E+01 -5.44E+00 -5.50E+01 -1.01E+02  3.68E+01 -7.52E+00  3.16E+01  2.75E+02
         -6.10E+01  1.07E+02  4.18E+02
 
 OM67
+        3.34E+01  1.82E+01 -5.19E+00  2.95E+00  4.04E+01  7.81E+00  7.19E+00 -1.50E+01 -2.36E+01 -7.94E+01  8.45E+00 -1.15E+01
         -2.04E+01  1.44E+02 -4.68E+01  4.48E+01  4.63E+00 -1.31E+01 -5.75E+01  2.35E+01  3.93E+02 -2.57E+00  1.71E+01  3.70E+01
         7.99E+01  4.38E+00 -7.07E+01 -4.87E+01 -3.44E+01 -4.38E+01  3.01E+01  2.83E+02 -1.71E+02  4.83E+01 -8.24E+00 -1.96E+02
          2.52E+02 -8.31E+01 -1.65E+02  1.57E+03
 
 OM68
+       -1.74E+01 -2.26E+01 -4.83E+01 -1.33E+01 -4.39E+01 -1.58E+01  7.67E+00  8.23E+00  2.50E+01  1.31E+02  3.11E+01  3.01E+01
         -8.99E+01 -4.80E+02  3.71E+01 -2.10E+02  1.29E+02  1.76E+02  6.48E+01 -1.17E+02 -6.94E+02  1.19E+02 -3.06E+02  2.63E+00
        -2.79E+01  1.06E+02 -3.62E+02  5.83E+01 -2.82E+02  3.00E+01 -6.34E+00 -1.89E+02  1.25E+02 -1.14E+02  6.39E+01  4.32E+02
         -1.13E+02  4.90E+02  3.61E+02 -5.79E+02  2.34E+03
 
 OM77
+       -1.84E+01 -1.97E+01 -2.78E+01 -1.72E+01  4.09E-01  3.63E-01 -2.02E+01  3.05E+01 -1.54E+01  5.78E+01  2.65E+01  4.51E+01
          4.46E+00  4.08E+01  9.69E+01  1.81E+01  3.66E+01 -9.07E+00  2.09E+01 -2.36E+01 -1.00E+01  2.56E+02 -1.02E+02 -2.85E+01
        -1.17E+01 -3.57E+01  5.42E+01 -7.35E+01  6.29E+01 -8.86E+00 -6.58E+00 -1.00E+01  2.80E+02 -8.73E+01  3.07E+01  1.79E+01
         -1.69E+02 -9.67E+00  2.34E+01 -1.13E+02 -4.93E+00  6.09E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        4.34E+01  5.78E+01 -1.93E+01  1.87E+01 -2.71E+01  1.22E+01  5.27E+01 -6.33E+01 -4.17E+01 -1.42E+02  5.76E+01 -1.41E+02
          1.40E+02  4.53E+01 -4.51E+02  2.82E+02 -8.54E+01 -7.49E+01 -1.42E+02  1.95E+01  1.12E+02 -4.55E+02  5.67E+02  6.65E+01
        -2.49E+00 -1.30E+01 -5.98E+01 -1.84E+02  1.40E+01 -6.21E+00  7.94E+00  8.03E+01 -4.10E+02  4.66E+02 -5.66E+01 -5.35E+01
          1.69E+02 -2.78E+02 -5.17E+01  3.06E+02 -2.34E+02 -4.74E+02  2.39E+03
 
 OM88
+       -2.37E+01 -2.66E+01  1.91E+01 -2.57E+01  2.09E+01 -1.98E+01 -3.00E+01  3.01E+01  5.42E+01  1.65E+02  1.84E+01  9.51E+01
         -1.40E+02 -1.28E+02  1.25E+02 -6.37E+02  1.45E+02  1.20E+02  9.63E+01 -9.23E+01 -1.35E+02  1.80E+02 -8.01E+02  6.23E+00
         4.43E+01 -6.22E+01 -1.22E+02  1.04E+02 -4.46E+02 -2.09E+01 -2.04E+01  3.93E+01  9.90E-01 -2.11E+02  2.12E+01  1.07E+02
         -7.93E+01  3.87E+02  7.57E+01 -3.71E+01  4.64E+02  5.30E+01 -6.57E+02  1.18E+03
 
 SG11
+        2.18E+02 -2.05E+02 -2.44E+02 -2.73E+02 -1.91E+01  6.56E+02 -4.44E+02  2.70E+02  3.76E+02 -3.72E+02 -1.84E+03  1.22E+03
         -1.28E+03 -2.26E+03  1.69E+03 -8.92E+02  1.05E+02  2.51E+03 -4.05E+02  7.41E+02  1.22E+03  7.68E+02 -9.90E+02  1.11E+03
        -2.93E+02  6.82E+02  3.86E+03  1.20E+03  4.55E+03  1.41E+03 -2.17E+03  5.59E+02 -7.48E+02  2.05E+03  2.11E+02  2.10E+03
         -2.29E+03  2.49E+03 -8.30E+02 -3.53E+02 -2.93E+03 -7.24E+02  2.95E+03 -7.76E+02  2.47E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        3.61E+02  2.10E+02  7.01E+01 -7.15E+01  4.13E+02  5.21E+02 -9.28E+01 -2.16E+02 -7.15E+02 -1.71E+03 -8.06E+02 -3.89E+02
          1.67E+02  9.35E+02 -3.38E+02  1.08E+03 -6.47E+02 -1.78E+03 -6.46E+01 -1.05E+03 -1.90E+02 -2.42E+03 -2.41E+02  4.62E+02
         4.23E+02  2.27E+03 -1.01E+03 -1.14E+03  3.06E+03  1.62E+02  8.41E+02  4.90E+02  7.97E+02 -4.36E+02  1.51E+02  1.72E+03
          2.17E+03  1.16E+03  9.99E+02  3.32E+02  6.24E+02 -5.33E+02  1.82E+02  5.04E+02 -2.71E+04  0.00E+00  6.87E+05
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     4699.172
Stop Time: 
Sun 10/30/2016 
11:39 PM

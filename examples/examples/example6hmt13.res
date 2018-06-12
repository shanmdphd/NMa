Tue 04/26/2016 
11:50 AM
;Model Desc: Receptor Mediated Clearance model with Dynamic Change in Receptors
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# example6 (from r2compl)
$INPUT C SET ID JID TIME DV=CONC DOSE=AMT RATE EVID MDV CMT
$DATA example6.csv IGNORE=C

; The new numerical integration solver is used, although ADVAN=9 is also efficient
; for this problem.
$SUBROUTINES ADVAN13 TRANS1 TOL=4
$MODEL NCOMPARTMENTS=3

$PRIOR NWPRI NTHETA=8, NETA=8, NTHP=0, NETP=8, NPEXP=1

$PK
BAYES_EXTRA_REQUEST=1
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


$THETA 
;Initial Thetas
( 4.0 )  ;[MU_1]
( -2.1 ) ;[MU_2]
( 0.7 )  ;[MU_3]
( -0.17 );[MU_4]      
( 2.2 ) ;[MU_5]
( 0.14 )  ;[MU_6]
( 3.7 )  ;[MU_7]
( -0.7) ;[MU_8]
; degrees of freedom for OMEGA prior
(8 FIXED)           ;[dfo]


;Initial Omegas
$OMEGA BLOCK(8)
0.2 ;[p]
-0.0043  ;[f]
0.2 ;[p]
0.0048   ;[f]    
-0.0023  ;[f]     
0.2 ;[p]
0.0032   ;[f]   
0.0059   ;[f]  
-0.0014  ;[f]   
0.2 ;[p]
0.0029   ;[f]   
0.002703 ;[f]  
-0.00026 ;[f]  
-0.0032  ;[f]    
0.2 ;[p]
-0.0025  ;[f]  
0.00097  ;[f]   
0.0024   ;[f]  
0.00197  ;[f]  
-0.0080  ;[f]   
0.2 ;[p]
0.0031   ;[f]  
-0.00571 ;[f]    
0.0030   ;[f]   
-0.0074  ;[f]    
0.0025   ;[f]   
0.0034   ;[f]  
0.2 ;[p]
0.00973  ;[f]  
0.00862  ;[f]  
0.0041   ;[f]  
0.0046   ;[f]   
0.00061  ;[f] 
-0.0056  ;[f]   
0.0056   ;[f]  
0.2 ;[p]

; Omega prior
$OMEGA BLOCK(8)
0.2 FIX
0.0 0.2
0.0 0.0 0.2
0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.2

$SIGMA  
0.1 ;[p]
0.1 ;[p]

$EST METHOD=ITS INTERACTION SIGL=4 NITER=15 PRINT=1 NOABORT NOPRIOR=1 file=example6hmt13_its.ext
$EST METHOD=bayes INTERACTION NBURN=2000 NITER=0 PRINT=10 MASSRESET=1 NOPRIOR=0 file=example6hmt13_bayes.ext
$EST METHOD=NUTS INTERACTION  NBURN=1000 NITER=2000 PRINT=1 MASSRESET=0 PMADAPT=500  file=example6hmt13.ext
$COV MATRIX=R UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       26 APR 2016
Days until program expires :5146
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha9 (nm74a9)
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
 -0.1000E+07    -0.2100E+01     0.1000E+07
 -0.1000E+07     0.7000E+00     0.1000E+07
 -0.1000E+07    -0.1700E+00     0.1000E+07
 -0.1000E+07     0.2200E+01     0.1000E+07
 -0.1000E+07     0.1400E+00     0.1000E+07
 -0.1000E+07     0.3700E+01     0.1000E+07
 -0.1000E+07    -0.7000E+00     0.1000E+07
  0.8000E+01     0.8000E+01     0.8000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.2000E+00
                 -0.4300E-02   0.2000E+00
                  0.4800E-02  -0.2300E-02   0.2000E+00
                  0.3200E-02   0.5900E-02  -0.1400E-02   0.2000E+00
                  0.2900E-02   0.2703E-02  -0.2600E-03  -0.3200E-02   0.2000E+00
                 -0.2500E-02   0.9700E-03   0.2400E-02   0.1970E-02  -0.8000E-02   0.2000E+00
                  0.3100E-02  -0.5710E-02   0.3000E-02  -0.7400E-02   0.2500E-02   0.3400E-02   0.2000E+00
                  0.9730E-02   0.8620E-02   0.4100E-02   0.4600E-02   0.6100E-03  -0.5600E-02   0.5600E-02   0.2000E+00
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
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 GRADIENT METHOD USED:     NOSLOW
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 RELATIVE TOLERANCE (TOL):                  -1
 ABSOLUTE TOLERANCE-ADVAN 9,13,14 ONLY (ATOL): -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 TURN OFF Cholesky Transposition of R Matrix (CHOLROFF): NO
 KNUTHSUMOFF:                                -1
 RESUME COV ANALYSIS (RESUME):               NO
 SIR SAMPLE SIZE (SIRSAMPLE):              -1
 LINEARLY TRANSFORM THETAS DURING COV (NOTHBND): -1
 PRECONDTIONING CYCLES (PRECOND):        0
 PRECONDTIONING TYPES (PRECONDS):        TOS
 FORCED PRECONDTIONING CYCLES (PFCOND):0
 PRECONDTIONING TYPE (PRETYPE):        0
 FORCED POS. DEFINITE SETTING: (FPOSDEF):0
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha9 (nm74a9)

 GENERAL NONLINEAR KINETICS MODEL USING LSODA (ADVAN13)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   7
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         COMP 1       ON         YES        YES        YES        YES
    2         COMP 2       ON         YES        YES        NO         NO
    3         COMP 3       ON         YES        YES        NO         NO
    4         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE(S) FROM SUBROUTINE TOL:   4
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
0DES SUBROUTINE USES FULL STORAGE MODE.
 
 #PARA: PARAFILE=mpiwini8.pnm, PROTOCOL=MPI, NODES= 3
 
1
 
 
 #TBLN:      1
 #METH: Iterative Two Stage (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
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
 ABSOLUTE TOLERANCE-ADVAN 9,13,14 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmt13_its.ext
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

 iteration            0 OBJ=  -3444.74477733650
 iteration            1 OBJ=  -3598.21506628031
 iteration            2 OBJ=  -3711.98750068652
 iteration            3 OBJ=  -3819.34433264896
 iteration            4 OBJ=  -3923.75994986570
 iteration            5 OBJ=  -4026.30363478058
 iteration            6 OBJ=  -4127.39433702085
 iteration            7 OBJ=  -4226.86022028602
 iteration            8 OBJ=  -4324.35786144106
 iteration            9 OBJ=  -4419.01480109542
 iteration           10 OBJ=  -4509.15064442659
 iteration           11 OBJ=  -4591.55267302721
 iteration           12 OBJ=  -4659.22895582931
 iteration           13 OBJ=  -4699.40903741718
 iteration           14 OBJ=  -4708.74561908388
 iteration           15 OBJ=  -4709.84269496851
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -8.2536E-04 -3.0238E-03  2.4035E-03  1.4306E-03  1.4751E-03  2.0978E-03  5.5788E-04  1.1501E-03
 SE:             6.9305E-02  5.2691E-02  3.7772E-02  6.5150E-02  5.6772E-02  5.7214E-02  6.4185E-02  6.1424E-02
 N:                      50          50          50          50          50          50          50          50
 
 P VAL.:         9.9050E-01  9.5424E-01  9.4926E-01  9.8248E-01  9.7927E-01  9.7075E-01  9.9307E-01  9.8506E-01
 
 ETAshrink(%):   6.3547E-01  4.2235E+00  8.0379E+00  1.6190E+00  1.4870E+00  5.7546E+00  3.5292E-01  1.5689E+00
 EBVshrink(%):   6.3184E-01  5.4612E+00  9.8666E+00  2.1020E+00  1.5631E+00  6.3069E+00  4.5538E-01  1.7659E+00
 EPSshrink(%):   1.5698E+01  7.2359E+00
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -4709.84269496851     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1828.05145483866     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:    26.49
 Elapsed covariance  time in seconds:     0.23
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -4709.843       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.19E+00  5.58E-01 -1.86E-01  2.26E+00  2.10E-01  3.71E+00 -7.09E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.48E-01
 
 ETA2
+       -3.44E-02  1.54E-01
 
 ETA3
+        4.73E-02 -1.41E-02  8.61E-02
 
 ETA4
+        3.19E-02  4.69E-02 -2.10E-02  2.24E-01
 
 ETA5
+        2.66E-02  2.73E-02 -2.69E-03 -3.32E-02  1.69E-01
 
 ETA6
+       -2.88E-02  1.12E-02  2.67E-02  1.88E-02 -8.06E-02  1.88E-01
 
 ETA7
+        2.89E-02 -3.37E-02  3.17E-02 -7.21E-02  2.37E-02  3.37E-03  2.12E-01
 
 ETA8
+        9.78E-02  8.19E-02  3.48E-02  4.44E-02  1.08E-03 -5.09E-02  5.51E-02  1.99E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.28E-03
 
 EPS2
+        0.00E+00  2.25E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        4.98E-01
 
 ETA2
+       -1.76E-01  3.93E-01
 
 ETA3
+        3.24E-01 -1.22E-01  2.93E-01
 
 ETA4
+        1.35E-01  2.53E-01 -1.52E-01  4.73E-01
 
 ETA5
+        1.30E-01  1.69E-01 -2.23E-02 -1.71E-01  4.12E-01
 
 ETA6
+       -1.33E-01  6.55E-02  2.10E-01  9.18E-02 -4.52E-01  4.34E-01
 
 ETA7
+        1.26E-01 -1.86E-01  2.35E-01 -3.31E-01  1.25E-01  1.69E-02  4.60E-01
 
 ETA8
+        4.40E-01  4.67E-01  2.66E-01  2.10E-01  5.88E-03 -2.64E-01  2.69E-01  4.46E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.63E-02
 
 EPS2
+        0.00E+00  1.50E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.47E-01  2.23E-01  2.30E-01  3.09E-01  2.14E-01  2.66E-01  1.50E-01  3.80E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.50E-01
 
 ETA2
+        2.22E-01  2.80E-01
 
 ETA3
+        1.05E-01  1.23E-01  1.80E-01
 
 ETA4
+        8.68E-02  1.31E-01  1.03E-01  1.68E-01
 
 ETA5
+        1.40E-01  9.08E-02  8.62E-02  1.65E-01  3.06E-01
 
 ETA6
+        1.12E-01  1.62E-01  1.34E-01  1.53E-01  8.22E-02  1.56E-01
 
 ETA7
+        1.60E-01  1.05E-01  1.18E-01  9.43E-02  1.02E-01  1.02E-01  1.45E-01
 
 ETA8
+        2.00E-01  1.47E-01  5.89E-02  9.29E-02  1.41E-01  2.00E-01  1.29E-01  2.62E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        3.43E-03
 
 EPS2
+        0.00E+00  4.28E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.51E-01
 
 ETA2
+        1.03E+00  3.56E-01
 
 ETA3
+        7.07E-01  9.47E-01  3.07E-01
 
 ETA4
+        3.54E-01  6.30E-01  8.44E-01  1.78E-01
 
 ETA5
+        7.03E-01  5.66E-01  7.13E-01  9.38E-01  3.71E-01
 
 ETA6
+        5.43E-01  9.41E-01  1.03E+00  7.31E-01  8.09E-01  1.80E-01
 
 ETA7
+        6.83E-01  5.33E-01  9.65E-01  4.09E-01  5.97E-01  5.15E-01  1.58E-01
 
 ETA8
+        5.89E-01  7.38E-01  3.38E-01  3.62E-01  7.75E-01  9.10E-01  5.66E-01  2.94E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        1.78E-02
 
 EPS2
+       .........  1.43E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        1.21E-01
 
 TH 2
+       -3.18E-02  4.96E-02
 
 TH 3
+        3.03E-02 -1.46E-02  5.27E-02
 
 TH 4
+        7.99E-02 -2.43E-02 -2.68E-03  9.56E-02
 
 TH 5
+       -5.42E-02  2.58E-02 -1.79E-03 -4.90E-02  4.59E-02
 
 TH 6
+       -6.05E-02  2.12E-02 -1.86E-02 -3.84E-02  3.29E-02  7.10E-02
 
 TH 7
+       -1.53E-02 -3.99E-03  9.11E-03 -2.24E-02  1.18E-02  1.09E-02  2.26E-02
 
 TH 8
+        1.04E-01 -3.90E-02  1.88E-02  9.25E-02 -5.83E-02 -5.63E-02 -1.07E-02  1.44E-01
 
 OM11
+       -2.99E-02  5.53E-03 -2.07E-02 -9.43E-03  1.21E-02  1.55E-02  5.43E-04 -1.84E-02  2.25E-02
 
 OM12
+       -2.12E-02  3.01E-02  1.45E-03 -2.76E-02  2.22E-02  1.74E-02 -2.96E-03 -5.39E-02 -4.07E-03  4.94E-02
 
 OM13
+       -1.09E-02 -8.74E-03  2.91E-03 -7.96E-03  3.48E-03  2.99E-03  4.56E-03 -9.48E-05  6.81E-03 -1.32E-02  1.11E-02
 
 OM14
+       -8.02E-03 -3.32E-03  3.50E-03 -1.21E-02  3.82E-03 -3.43E-03  6.76E-03 -8.46E-03  5.55E-04 -3.53E-03  3.22E-03  7.53E-03
 
 OM15
+        1.20E-02 -5.72E-03 -3.92E-03  1.64E-02 -6.59E-03  7.66E-03 -6.60E-03  9.37E-03  3.81E-04  1.25E-03 -1.63E-03 -8.37E-03
          1.95E-02
 
 OM16
+       -1.69E-02  1.42E-02 -1.46E-03 -1.74E-02  1.28E-02  5.42E-03  2.10E-03 -1.45E-02  3.04E-03  8.63E-03 -3.04E-04  1.92E-03
         -8.96E-03  1.26E-02
 
 OM17
+        3.24E-03 -2.50E-03 -2.13E-02  2.38E-02 -7.04E-03  7.32E-04 -7.53E-03  2.45E-02  1.41E-02 -1.74E-02  3.15E-03 -4.91E-03
          6.37E-03 -2.79E-03  2.57E-02
 
 OM18
+       -3.04E-02  1.72E-02 -3.51E-02 -8.00E-04  1.26E-02  2.80E-02 -5.51E-03 -1.93E-02  2.17E-02  7.19E-03 -1.94E-03 -5.84E-03
          7.14E-03  1.87E-03  2.14E-02  4.01E-02
 
 OM22
+       -2.79E-02 -2.19E-02 -3.47E-02  9.91E-04 -8.28E-03  1.81E-02  2.87E-03  1.19E-02  2.07E-02 -4.20E-02  1.56E-02  1.89E-03
          4.64E-03 -7.25E-03  2.30E-02  1.76E-02  7.83E-02
 
 OM23
+        6.65E-03  5.54E-03  1.46E-02 -1.01E-02  5.01E-03 -5.78E-03  3.93E-03 -1.15E-02 -9.63E-03  1.45E-02 -6.47E-03  2.67E-03
         -5.70E-03  2.88E-03 -1.32E-02 -1.08E-02 -2.60E-02  1.52E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -1.24E-02 -2.94E-03 -1.34E-02  1.98E-03 -1.06E-03  1.34E-02 -5.31E-03 -6.51E-03  6.04E-03  2.48E-03  2.18E-04 -5.99E-03
          1.17E-02 -3.76E-03  6.23E-03  1.29E-02  1.66E-02 -8.94E-03  1.72E-02
 
 OM25
+        3.10E-03  5.04E-05 -1.13E-03 -1.80E-03 -8.87E-04 -2.38E-03  3.12E-03  1.88E-03  6.85E-04 -4.34E-03  4.51E-04  2.53E-03
         -4.57E-03  1.92E-03 -2.15E-04 -3.81E-03  1.87E-03  2.36E-03 -6.36E-03  8.25E-03
 
 OM26
+       -2.38E-02  2.34E-02 -1.94E-02 -1.35E-02  1.64E-02  2.71E-02 -2.84E-03 -2.78E-02  9.39E-03  1.95E-02 -6.12E-03 -6.14E-03
          4.84E-03  7.20E-03  4.00E-03  2.17E-02 -2.45E-03 -6.07E-05  8.24E-03 -1.76E-03  2.62E-02
 
 OM27
+       -1.33E-02  1.63E-02 -3.52E-03 -1.41E-02  1.32E-02  5.89E-03  1.30E-03 -2.30E-02  3.30E-03  1.47E-02 -3.73E-03  6.43E-04
         -4.95E-03  6.87E-03 -3.16E-03  5.67E-03 -1.44E-02  6.14E-03 -4.25E-03  2.12E-03  9.48E-03  1.09E-02
 
 OM28
+       -3.73E-02  1.60E-02 -1.72E-02 -2.64E-02  1.75E-02  1.95E-02  7.48E-04 -4.63E-02  1.13E-02  1.88E-02 -1.09E-03  1.69E-04
         -5.57E-04  6.23E-03 -2.60E-03  1.36E-02  3.16E-03 -5.32E-04  7.48E-03 -2.97E-06  1.39E-02  8.33E-03  2.15E-02
 
 OM33
+        4.41E-02 -2.51E-02  2.14E-03  3.89E-02 -3.07E-02 -2.48E-02 -6.52E-03  6.05E-02 -6.32E-03 -3.05E-02  2.60E-03 -3.01E-03
          6.84E-03 -1.03E-02  1.24E-02 -8.20E-03  1.95E-02 -8.45E-03  8.82E-04  9.36E-04 -1.42E-02 -1.36E-02 -1.94E-02  3.24E-02
 
 OM34
+        1.31E-02 -1.35E-02  8.72E-03  1.04E-02 -8.37E-03 -1.56E-02  3.83E-03  2.17E-02 -2.57E-03 -1.54E-02  4.29E-03  3.98E-03
         -4.74E-03 -2.21E-03  1.56E-03 -1.04E-02  3.96E-03 -2.67E-04 -6.43E-03  3.21E-03 -1.28E-02 -4.25E-03 -9.11E-03  9.42E-03
         1.07E-02
 
 OM35
+        2.70E-04  5.56E-03 -8.62E-03  3.31E-03 -5.16E-04  9.43E-03 -4.28E-03 -4.47E-03  9.79E-04  4.04E-03 -3.27E-03 -3.48E-03
          6.75E-03 -2.67E-03  2.98E-03  8.58E-03  2.61E-03 -1.63E-03  4.40E-03 -2.11E-03  6.90E-03  1.60E-05  2.01E-03 -1.54E-04
        -6.22E-03  7.43E-03
 
 OM36
+        1.48E-02  2.97E-03 -1.25E-02  1.55E-02 -1.26E-02 -1.56E-02 -1.13E-02  1.36E-02  8.61E-05  1.51E-03 -6.48E-03 -4.33E-03
          2.54E-03  1.73E-03  6.97E-03  4.52E-03 -1.11E-03 -1.08E-03  3.89E-03  3.64E-04  5.08E-03  1.32E-03  1.78E-03  7.26E-03
        -1.71E-03  1.02E-03  1.80E-02
 
 OM37
+       -2.22E-02  1.22E-02 -1.79E-02 -9.68E-03  7.49E-03  1.53E-02 -4.78E-03 -2.24E-02  1.03E-02  8.83E-03 -7.37E-05 -3.42E-03
          4.96E-03  2.86E-03  4.75E-03  1.56E-02  8.49E-03 -6.44E-03  9.61E-03 -3.26E-03  1.29E-02  3.13E-03  1.26E-02 -7.73E-03
        -9.26E-03  5.35E-03  4.20E-03  1.40E-02
 
 OM38
+        9.60E-03 -7.71E-03  1.38E-03  8.04E-03 -6.68E-03 -3.35E-03 -8.94E-04  1.36E-02 -3.35E-04 -8.91E-03  2.58E-03 -7.16E-04
          2.96E-03 -2.96E-03  3.50E-03 -1.62E-03  6.52E-03 -2.92E-03  5.00E-04 -4.00E-07 -3.28E-03 -3.58E-03 -4.91E-03  8.31E-03
         1.95E-03  6.73E-04  1.57E-04 -9.11E-04  3.48E-03
 
 OM44
+       -2.47E-02 -1.03E-02  1.39E-02 -2.47E-02  1.12E-02  3.55E-03  8.74E-03 -1.96E-02  3.44E-03 -2.74E-03  1.11E-02  5.26E-03
         -2.11E-03 -5.64E-04 -7.23E-03 -8.26E-03  6.87E-03 -1.83E-03  3.88E-03 -3.25E-03 -9.22E-03 -3.15E-03  4.20E-03 -6.72E-03
         3.24E-03 -6.67E-03 -9.75E-03 -5.84E-04 -6.32E-04  2.83E-02
 
 OM45
+       -2.20E-02  2.36E-02 -1.26E-02 -1.71E-02  1.70E-02  2.66E-02 -1.81E-03 -3.88E-02  4.62E-03  2.76E-02 -7.07E-03 -3.83E-03
          4.47E-03  5.40E-03 -4.86E-03  1.52E-02 -1.37E-02  3.34E-03  4.89E-03 -1.76E-03  2.17E-02  1.00E-02  1.49E-02 -1.97E-02
        -1.41E-02  7.72E-03  9.46E-05  1.22E-02 -4.01E-03 -8.32E-03  2.72E-02
 
 OM46
+       -2.01E-02 -5.31E-03 -6.83E-05 -1.60E-02  8.78E-03  1.30E-02  1.28E-02 -1.39E-03  7.15E-03 -1.71E-02  1.04E-02  5.43E-03
         -8.19E-03  4.38E-03  1.17E-03 -2.75E-03  2.29E-02 -4.89E-03 -3.86E-03  5.99E-03 -2.84E-03 -1.79E-03 -4.47E-04  6.16E-04
         5.68E-03 -4.61E-03 -1.02E-02 -2.59E-03  1.56E-03  7.51E-03 -6.71E-03  2.34E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.09E-02  8.75E-03 -2.44E-03 -6.22E-03  9.43E-03  1.03E-02  5.84E-03 -7.43E-03  4.83E-03  1.65E-03  9.47E-04  1.67E-03
         -4.68E-03  4.55E-03  1.11E-03  5.33E-03 -1.88E-03  5.37E-05 -4.04E-03  1.78E-03  4.46E-03  4.59E-03  2.63E-03 -7.20E-03
        -5.06E-04 -2.08E-04 -4.80E-03  1.02E-03 -1.31E-03 -2.99E-03  5.25E-03  6.10E-03  8.90E-03
 
 OM48
+       -2.08E-02  2.23E-03 -1.29E-02 -7.56E-03  5.72E-03  1.05E-02  1.74E-03 -1.57E-02  8.13E-03 -8.75E-04  2.39E-03  1.28E-03
          1.06E-03 -1.04E-03  4.20E-03  1.05E-02  1.41E-02 -5.47E-03  6.40E-03 -2.01E-03  3.82E-03  5.33E-04  7.54E-03 -4.70E-03
        -2.23E-03  1.67E-03 -1.12E-03  5.85E-03 -1.07E-03  4.65E-03  2.83E-03  1.73E-03  1.50E-03  8.63E-03
 
 OM55
+       -6.36E-02  3.38E-02 -1.48E-02 -5.84E-02  4.04E-02  3.42E-02  7.79E-03 -9.57E-02  1.72E-02  4.80E-02 -3.72E-03  1.22E-03
         -3.16E-03  1.46E-02 -1.49E-02  1.68E-02 -2.17E-02  9.60E-03  6.32E-03  2.76E-03  2.70E-02  2.22E-02  3.93E-02 -4.59E-02
        -1.61E-02 -5.44E-04  3.05E-04  1.84E-02 -1.18E-02  1.13E-02  3.14E-02 -3.17E-03  6.85E-03  8.70E-03  9.35E-02
 
 OM56
+       -8.21E-03  1.08E-02 -1.12E-02 -5.30E-03  3.40E-03  5.87E-03 -4.80E-03 -1.27E-02  3.01E-03  7.98E-03 -3.64E-03 -1.99E-03
          2.46E-04  3.38E-03  1.91E-03  8.34E-03 -4.44E-04 -1.79E-04  2.71E-03  3.33E-04  8.77E-03  4.41E-03  6.69E-03 -5.08E-03
        -5.67E-03  3.26E-03  5.45E-03  6.12E-03 -1.49E-03 -5.33E-03  7.98E-03 -3.91E-03  2.04E-04  1.76E-03  1.06E-02  6.76E-03
 
 OM57
+        5.50E-04 -1.85E-03 -1.41E-02  1.08E-02 -3.26E-03  1.10E-03 -3.40E-03  1.25E-02  7.60E-03 -1.23E-02  2.82E-03 -1.75E-03
          2.44E-03 -1.52E-03  1.28E-02  1.03E-02  1.74E-02 -8.93E-03  2.46E-03  1.29E-03  1.70E-03 -1.46E-03 -1.38E-03  8.06E-03
         8.76E-04  2.07E-03  2.69E-03  3.15E-03  2.75E-03 -4.62E-03 -2.43E-03  3.94E-03  8.14E-04  2.86E-03 -9.69E-03  1.61E-03
          1.03E-02
 
 OM58
+        2.51E-02 -1.26E-02 -8.76E-03  3.05E-02 -1.80E-02 -9.40E-03 -5.38E-03  3.85E-02  2.35E-03 -2.20E-02  2.18E-03 -3.39E-03
          8.07E-03 -7.39E-03  1.55E-02  3.19E-03  1.90E-02 -9.18E-03  1.41E-03  4.01E-03 -5.41E-03 -7.48E-03 -9.68E-03  2.04E-02
         5.63E-03  1.94E-03  5.12E-03 -2.25E-03  5.61E-03 -8.33E-03 -9.52E-03  2.33E-03 -2.07E-03 -9.86E-04 -2.58E-02 -2.07E-03
          9.83E-03  2.00E-02
 
 OM66
+       -3.23E-02  1.44E-02 -8.91E-03 -2.65E-02  1.81E-02  2.76E-02  4.74E-03 -3.38E-02  4.28E-03  1.27E-02 -6.21E-04  1.72E-04
          1.14E-03  3.95E-03 -5.32E-03  1.05E-02  5.41E-03  4.87E-04  6.12E-03 -4.63E-03  1.42E-02  3.36E-03  1.03E-02 -1.34E-02
        -1.03E-02  6.39E-03 -5.49E-03  9.71E-03 -1.94E-03  1.23E-03  1.53E-02  3.51E-03  3.45E-03  5.09E-03  1.43E-02  2.73E-03
         -2.11E-03 -9.52E-03  2.43E-02
 
 OM67
+        1.46E-02 -1.67E-02  6.48E-03  9.39E-03 -1.35E-02 -1.17E-02 -6.78E-04  1.64E-02 -6.04E-03 -9.46E-03  1.59E-03  6.03E-04
          2.63E-03 -6.02E-03 -2.16E-03 -1.03E-02  7.05E-03 -8.29E-04  2.02E-03 -8.92E-04 -9.70E-03 -7.63E-03 -6.33E-03  1.09E-02
         4.51E-03 -1.89E-03  1.78E-03 -4.57E-03  2.27E-03  4.21E-03 -9.74E-03 -1.26E-03 -6.51E-03 -1.56E-03 -1.43E-02 -3.89E-03
         -1.81E-03  3.71E-03 -4.45E-03  1.05E-02
 
 OM68
+        5.85E-02 -1.28E-02  8.91E-03  4.66E-02 -3.05E-02 -3.20E-02 -1.24E-02  6.28E-02 -1.32E-02 -1.15E-02 -7.51E-03 -7.68E-03
          7.10E-03 -4.54E-03  8.08E-03 -9.73E-03 -1.05E-02  7.10E-04 -1.55E-03 -2.77E-04 -5.97E-03 -7.36E-03 -1.88E-02  2.60E-02
         6.14E-03 -3.56E-04  1.54E-02 -8.86E-03  4.63E-03 -1.55E-02 -1.23E-02 -1.14E-02 -7.29E-03 -1.09E-02 -3.56E-02 -2.36E-03
          1.80E-03  1.46E-02 -1.70E-02  8.22E-03  4.01E-02
 
 OM77
+       -8.56E-03  1.01E-02 -5.13E-03 -2.64E-03  5.27E-03  1.61E-03 -8.18E-03 -2.41E-02  1.20E-03  1.95E-02 -6.47E-03 -2.71E-03
          4.24E-03 -1.62E-03 -1.81E-03  7.95E-03 -1.41E-02  4.08E-03  5.22E-03 -3.39E-03  5.76E-03  4.18E-03  9.32E-03 -1.11E-02
        -6.19E-03  2.87E-03  3.96E-03  5.50E-03 -4.23E-03  1.11E-03  8.47E-03 -1.42E-02 -4.12E-03  2.71E-03  1.98E-02  4.18E-03
         -3.74E-03 -6.25E-03  1.99E-03 -1.85E-03 -4.63E-03  2.10E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.39E-02  5.40E-03 -9.16E-03  2.34E-02 -4.97E-03  9.29E-04 -9.53E-03  1.54E-02  3.32E-03  2.65E-03 -5.71E-03 -6.94E-03
          7.97E-03 -2.59E-03  1.31E-02  1.55E-02 -5.07E-03 -2.64E-03  3.75E-03 -2.51E-03  7.39E-03  1.32E-03 -2.42E-03  4.40E-03
        -3.52E-03  5.40E-03  5.84E-03  3.11E-03  1.13E-03 -1.20E-02  4.74E-03 -9.36E-03  1.22E-03 -5.13E-04 -6.90E-03  2.84E-03
          4.99E-03  7.35E-03 -2.26E-03 -4.50E-03  1.03E-02  5.92E-03  1.67E-02
 
 OM88
+       -5.57E-02  3.56E-02 -3.65E-02 -2.58E-02  3.29E-02  4.63E-02 -2.54E-03 -6.22E-02  2.38E-02  3.10E-02 -5.73E-03 -5.44E-03
          5.64E-03  8.41E-03  1.10E-02  4.43E-02  5.96E-04 -3.43E-03  1.27E-02 -3.83E-03  3.42E-02  1.63E-02  2.82E-02 -3.02E-02
        -1.96E-02  1.12E-02  8.48E-04  2.29E-02 -6.05E-03 -6.98E-03  3.34E-02 -5.25E-03  1.02E-02  1.28E-02  4.98E-02  1.33E-02
          5.08E-03 -9.74E-03  2.19E-02 -1.89E-02 -2.67E-02  1.61E-02  1.43E-02  6.87E-02
 
 SG11
+        9.74E-04 -4.44E-04  1.26E-04  9.10E-04 -5.82E-04 -5.16E-04 -1.76E-04  1.13E-03 -1.71E-04 -4.00E-04 -2.28E-05 -9.97E-05
          1.65E-04 -1.91E-04  1.82E-04 -1.77E-04  6.67E-05 -1.15E-04  9.87E-07  5.81E-06 -2.62E-04 -2.08E-04 -3.53E-04  5.19E-04
         1.62E-04 -1.16E-05  1.37E-04 -1.63E-04  1.24E-04 -1.74E-04 -2.93E-04 -8.82E-05 -1.05E-04 -1.25E-04 -7.27E-04 -1.00E-04
          1.12E-04  3.45E-04 -3.38E-04  1.62E-04  5.34E-04 -1.46E-04  1.47E-04 -5.13E-04  1.17E-05
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.31E-04 -5.77E-04  2.57E-04  2.44E-05 -2.14E-04 -1.94E-04  1.95E-04  6.03E-04 -5.74E-05 -7.23E-04  2.95E-04  1.36E-04
         -7.05E-05 -1.62E-04  5.78E-05 -2.98E-04  6.17E-04 -1.75E-04 -5.27E-05 -5.07E-08 -4.53E-04 -2.80E-04 -3.35E-04  4.05E-04
         2.55E-04 -1.07E-04 -2.32E-04 -2.03E-04  1.44E-04  2.82E-04 -4.96E-04  3.37E-04 -5.39E-05  1.21E-05 -7.98E-04 -1.95E-04
          1.01E-04  1.83E-04 -8.80E-05  1.93E-04 -2.72E-05 -3.24E-04 -1.98E-04 -6.47E-04  3.81E-06  0.00E+00  1.83E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        3.47E-01
 
 TH 2
+       -4.11E-01  2.23E-01
 
 TH 3
+        3.79E-01 -2.86E-01  2.30E-01
 
 TH 4
+        7.44E-01 -3.53E-01 -3.78E-02  3.09E-01
 
 TH 5
+       -7.28E-01  5.41E-01 -3.65E-02 -7.39E-01  2.14E-01
 
 TH 6
+       -6.54E-01  3.58E-01 -3.04E-01 -4.66E-01  5.76E-01  2.66E-01
 
 TH 7
+       -2.93E-01 -1.19E-01  2.64E-01 -4.81E-01  3.66E-01  2.73E-01  1.50E-01
 
 TH 8
+        7.87E-01 -4.61E-01  2.16E-01  7.88E-01 -7.17E-01 -5.56E-01 -1.88E-01  3.80E-01
 
 OM11
+       -5.73E-01  1.65E-01 -6.01E-01 -2.03E-01  3.75E-01  3.88E-01  2.41E-02 -3.23E-01  1.50E-01
 
 OM12
+       -2.74E-01  6.08E-01  2.85E-02 -4.01E-01  4.67E-01  2.93E-01 -8.86E-02 -6.39E-01 -1.22E-01  2.22E-01
 
 OM13
+       -2.97E-01 -3.72E-01  1.20E-01 -2.44E-01  1.54E-01  1.06E-01  2.88E-01 -2.37E-03  4.30E-01 -5.64E-01  1.05E-01
 
 OM14
+       -2.66E-01 -1.72E-01  1.76E-01 -4.52E-01  2.06E-01 -1.49E-01  5.18E-01 -2.57E-01  4.26E-02 -1.83E-01  3.52E-01  8.68E-02
 
 OM15
+        2.48E-01 -1.84E-01 -1.22E-01  3.81E-01 -2.20E-01  2.06E-01 -3.14E-01  1.77E-01  1.82E-02  4.04E-02 -1.11E-01 -6.91E-01
          1.40E-01
 
 OM16
+       -4.33E-01  5.70E-01 -5.69E-02 -5.03E-01  5.33E-01  1.81E-01  1.24E-01 -3.41E-01  1.81E-01  3.46E-01 -2.57E-02  1.97E-01
         -5.72E-01  1.12E-01
 
 OM17
+        5.82E-02 -7.00E-02 -5.78E-01  4.81E-01 -2.05E-01  1.71E-02 -3.12E-01  4.02E-01  5.84E-01 -4.88E-01  1.87E-01 -3.53E-01
          2.84E-01 -1.55E-01  1.60E-01
 
 OM18
+       -4.37E-01  3.85E-01 -7.64E-01 -1.29E-02  2.93E-01  5.25E-01 -1.83E-01 -2.54E-01  7.21E-01  1.62E-01 -9.21E-02 -3.36E-01
          2.55E-01  8.31E-02  6.68E-01  2.00E-01
 
 OM22
+       -2.87E-01 -3.52E-01 -5.40E-01  1.15E-02 -1.38E-01  2.43E-01  6.82E-02  1.12E-01  4.92E-01 -6.76E-01  5.28E-01  7.79E-02
          1.19E-01 -2.31E-01  5.13E-01  3.14E-01  2.80E-01
 
 OM23
+        1.56E-01  2.02E-01  5.18E-01 -2.65E-01  1.90E-01 -1.76E-01  2.12E-01 -2.46E-01 -5.22E-01  5.28E-01 -4.99E-01  2.50E-01
         -3.31E-01  2.09E-01 -6.67E-01 -4.37E-01 -7.55E-01  1.23E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -2.72E-01 -1.01E-01 -4.45E-01  4.89E-02 -3.76E-02  3.83E-01 -2.69E-01 -1.31E-01  3.07E-01  8.51E-02  1.57E-02 -5.26E-01
          6.39E-01 -2.56E-01  2.96E-01  4.90E-01  4.51E-01 -5.53E-01  1.31E-01
 
 OM25
+        9.82E-02  2.49E-03 -5.41E-02 -6.40E-02 -4.56E-02 -9.85E-02  2.28E-01  5.44E-02  5.03E-02 -2.15E-01  4.72E-02  3.21E-01
         -3.60E-01  1.89E-01 -1.48E-02 -2.10E-01  7.37E-02  2.11E-01 -5.33E-01  9.08E-02
 
 OM26
+       -4.23E-01  6.48E-01 -5.21E-01 -2.69E-01  4.73E-01  6.29E-01 -1.17E-01 -4.52E-01  3.86E-01  5.43E-01 -3.59E-01 -4.37E-01
          2.14E-01  3.97E-01  1.54E-01  6.70E-01 -5.42E-02 -3.04E-03  3.88E-01 -1.19E-01  1.62E-01
 
 OM27
+       -3.67E-01  7.01E-01 -1.47E-01 -4.35E-01  5.89E-01  2.11E-01  8.27E-02 -5.78E-01  2.10E-01  6.31E-01 -3.39E-01  7.08E-02
         -3.39E-01  5.86E-01 -1.88E-01  2.71E-01 -4.92E-01  4.77E-01 -3.10E-01  2.23E-01  5.59E-01  1.05E-01
 
 OM28
+       -7.32E-01  4.89E-01 -5.12E-01 -5.83E-01  5.57E-01  5.00E-01  3.39E-02 -8.32E-01  5.15E-01  5.78E-01 -7.08E-02  1.33E-02
         -2.72E-02  3.79E-01 -1.11E-01  4.64E-01  7.71E-02 -2.95E-02  3.89E-01 -2.23E-04  5.86E-01  5.43E-01  1.47E-01
 
 OM33
+        7.05E-01 -6.25E-01  5.17E-02  7.00E-01 -7.96E-01 -5.16E-01 -2.41E-01  8.85E-01 -2.34E-01 -7.62E-01  1.37E-01 -1.93E-01
          2.72E-01 -5.13E-01  4.31E-01 -2.28E-01  3.88E-01 -3.81E-01  3.73E-02  5.73E-02 -4.87E-01 -7.21E-01 -7.37E-01  1.80E-01
 
 OM34
+        3.66E-01 -5.86E-01  3.68E-01  3.27E-01 -3.79E-01 -5.69E-01  2.47E-01  5.55E-01 -1.66E-01 -6.70E-01  3.94E-01  4.44E-01
         -3.29E-01 -1.91E-01  9.42E-02 -5.05E-01  1.37E-01 -2.11E-02 -4.75E-01  3.43E-01 -7.65E-01 -3.94E-01 -6.02E-01  5.08E-01
         1.03E-01
 
 OM35
+        9.02E-03  2.90E-01 -4.36E-01  1.24E-01 -2.79E-02  4.10E-01 -3.30E-01 -1.37E-01  7.57E-02  2.11E-01 -3.60E-01 -4.65E-01
          5.60E-01 -2.77E-01  2.16E-01  4.97E-01  1.08E-01 -1.54E-01  3.89E-01 -2.69E-01  4.94E-01  1.78E-03  1.59E-01 -9.95E-03
        -7.00E-01  8.62E-02
 
 OM36
+        3.17E-01  9.93E-02 -4.06E-01  3.72E-01 -4.39E-01 -4.35E-01 -5.62E-01  2.67E-01  4.27E-03  5.06E-02 -4.58E-01 -3.72E-01
          1.36E-01  1.15E-01  3.24E-01  1.68E-01 -2.95E-02 -6.53E-02  2.21E-01  2.99E-02  2.34E-01  9.38E-02  9.04E-02  3.01E-01
        -1.23E-01  8.79E-02  1.34E-01
 
 OM37
+       -5.39E-01  4.63E-01 -6.59E-01 -2.64E-01  2.95E-01  4.86E-01 -2.68E-01 -4.98E-01  5.80E-01  3.36E-01 -5.91E-03 -3.33E-01
          3.00E-01  2.16E-01  2.50E-01  6.59E-01  2.56E-01 -4.42E-01  6.19E-01 -3.03E-01  6.70E-01  2.53E-01  7.26E-01 -3.63E-01
        -7.58E-01  5.25E-01  2.65E-01  1.18E-01
 
 OM38
+        4.68E-01 -5.87E-01  1.02E-01  4.41E-01 -5.29E-01 -2.13E-01 -1.01E-01  6.08E-01 -3.79E-02 -6.81E-01  4.15E-01 -1.40E-01
          3.59E-01 -4.48E-01  3.71E-01 -1.37E-01  3.96E-01 -4.02E-01  6.46E-02 -7.48E-05 -3.43E-01 -5.81E-01 -5.68E-01  7.84E-01
         3.21E-01  1.33E-01  1.98E-02 -1.31E-01  5.89E-02
 
 OM44
+       -4.22E-01 -2.74E-01  3.60E-01 -4.75E-01  3.10E-01  7.92E-02  3.45E-01 -3.06E-01  1.36E-01 -7.32E-02  6.28E-01  3.60E-01
         -8.98E-02 -2.99E-02 -2.68E-01 -2.45E-01  1.46E-01 -8.81E-02  1.76E-01 -2.13E-01 -3.38E-01 -1.79E-01  1.70E-01 -2.22E-01
         1.87E-01 -4.60E-01 -4.32E-01 -2.93E-02 -6.37E-02  1.68E-01
 
 OM45
+       -3.85E-01  6.41E-01 -3.33E-01 -3.35E-01  4.81E-01  6.05E-01 -7.30E-02 -6.20E-01  1.87E-01  7.52E-01 -4.07E-01 -2.68E-01
          1.94E-01  2.92E-01 -1.84E-01  4.60E-01 -2.98E-01  1.65E-01  2.26E-01 -1.17E-01  8.11E-01  5.80E-01  6.15E-01 -6.65E-01
        -8.29E-01  5.43E-01  4.27E-03  6.26E-01 -4.13E-01 -3.00E-01  1.65E-01
 
 OM46
+       -3.78E-01 -1.56E-01 -1.95E-03 -3.38E-01  2.68E-01  3.19E-01  5.57E-01 -2.39E-02  3.12E-01 -5.03E-01  6.46E-01  4.10E-01
         -3.84E-01  2.56E-01  4.77E-02 -8.98E-02  5.36E-01 -2.60E-01 -1.92E-01  4.31E-01 -1.15E-01 -1.12E-01 -2.00E-02  2.24E-02
         3.60E-01 -3.50E-01 -4.96E-01 -1.43E-01  1.73E-01  2.92E-01 -2.66E-01  1.53E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.32E-01  4.16E-01 -1.13E-01 -2.13E-01  4.67E-01  4.10E-01  4.11E-01 -2.07E-01  3.41E-01  7.85E-02  9.52E-02  2.04E-01
         -3.55E-01  4.30E-01  7.36E-02  2.82E-01 -7.11E-02  4.62E-03 -3.26E-01  2.07E-01  2.92E-01  4.65E-01  1.90E-01 -4.24E-01
        -5.20E-02 -2.55E-02 -3.79E-01  9.09E-02 -2.36E-01 -1.89E-01  3.37E-01  4.23E-01  9.43E-02
 
 OM48
+       -6.45E-01  1.08E-01 -6.06E-01 -2.63E-01  2.87E-01  4.25E-01  1.25E-01 -4.46E-01  5.83E-01 -4.24E-02  2.45E-01  1.58E-01
          8.16E-02 -9.97E-02  2.82E-01  5.62E-01  5.42E-01 -4.79E-01  5.25E-01 -2.38E-01  2.54E-01  5.48E-02  5.54E-01 -2.81E-01
        -2.33E-01  2.09E-01 -9.02E-02  5.32E-01 -1.96E-01  2.98E-01  1.85E-01  1.22E-01  1.72E-01  9.29E-02
 
 OM55
+       -5.99E-01  4.96E-01 -2.11E-01 -6.18E-01  6.16E-01  4.19E-01  1.69E-01 -8.24E-01  3.74E-01  7.07E-01 -1.16E-01  4.62E-02
         -7.39E-02  4.26E-01 -3.03E-01  2.74E-01 -2.54E-01  2.55E-01  1.57E-01  9.96E-02  5.45E-01  6.95E-01  8.77E-01 -8.34E-01
        -5.10E-01 -2.07E-02  7.42E-03  5.09E-01 -6.56E-01  2.20E-01  6.22E-01 -6.78E-02  2.37E-01  3.06E-01  3.06E-01
 
 OM56
+       -2.87E-01  5.90E-01 -5.93E-01 -2.09E-01  1.93E-01  2.68E-01 -3.88E-01 -4.06E-01  2.44E-01  4.37E-01 -4.20E-01 -2.79E-01
          2.14E-02  3.66E-01  1.45E-01  5.07E-01 -1.93E-02 -1.77E-02  2.51E-01  4.45E-02  6.58E-01  5.13E-01  5.55E-01 -3.44E-01
        -6.68E-01  4.59E-01  4.94E-01  6.28E-01 -3.08E-01 -3.85E-01  5.88E-01 -3.11E-01  2.63E-02  2.30E-01  4.23E-01  8.22E-02
 
 OM57
+        1.56E-02 -8.19E-02 -6.06E-01  3.45E-01 -1.50E-01  4.06E-02 -2.23E-01  3.24E-01  4.98E-01 -5.46E-01  2.64E-01 -1.98E-01
          1.72E-01 -1.33E-01  7.84E-01  5.07E-01  6.14E-01 -7.14E-01  1.84E-01  1.40E-01  1.03E-01 -1.37E-01 -9.25E-02  4.41E-01
         8.36E-02  2.36E-01  1.97E-01  2.62E-01  4.59E-01 -2.70E-01 -1.45E-01  2.54E-01  8.49E-02  3.03E-01 -3.12E-01  1.93E-01
          1.02E-01
 
 OM58
+        5.11E-01 -4.01E-01 -2.70E-01  6.97E-01 -5.95E-01 -2.49E-01 -2.53E-01  7.18E-01  1.11E-01 -7.00E-01  1.46E-01 -2.76E-01
          4.08E-01 -4.66E-01  6.83E-01  1.12E-01  4.79E-01 -5.28E-01  7.57E-02  3.12E-01 -2.36E-01 -5.06E-01 -4.67E-01  8.02E-01
         3.86E-01  1.59E-01  2.69E-01 -1.34E-01  6.73E-01 -3.50E-01 -4.08E-01  1.08E-01 -1.55E-01 -7.51E-02 -5.96E-01 -1.78E-01
          6.84E-01  1.41E-01
 
 OM66
+       -5.96E-01  4.15E-01 -2.49E-01 -5.51E-01  5.43E-01  6.66E-01  2.02E-01 -5.71E-01  1.83E-01  3.66E-01 -3.78E-02  1.27E-02
          5.25E-02  2.26E-01 -2.13E-01  3.36E-01  1.24E-01  2.54E-02  2.99E-01 -3.28E-01  5.63E-01  2.06E-01  4.52E-01 -4.78E-01
        -6.42E-01  4.76E-01 -2.62E-01  5.26E-01 -2.11E-01  4.71E-02  5.95E-01  1.47E-01  2.35E-01  3.52E-01  3.01E-01  2.13E-01
         -1.33E-01 -4.32E-01  1.56E-01
 
 OM67
+        4.10E-01 -7.33E-01  2.76E-01  2.97E-01 -6.18E-01 -4.30E-01 -4.41E-02  4.24E-01 -3.93E-01 -4.16E-01  1.47E-01  6.79E-02
          1.84E-01 -5.25E-01 -1.32E-01 -5.03E-01  2.46E-01 -6.59E-02  1.50E-01 -9.60E-02 -5.86E-01 -7.13E-01 -4.22E-01  5.93E-01
         4.28E-01 -2.15E-01  1.30E-01 -3.77E-01  3.76E-01  2.45E-01 -5.78E-01 -8.06E-02 -6.75E-01 -1.64E-01 -4.58E-01 -4.62E-01
         -1.74E-01  2.57E-01 -2.79E-01  1.02E-01
 
 OM68
+        8.42E-01 -2.87E-01  1.94E-01  7.53E-01 -7.11E-01 -6.00E-01 -4.12E-01  8.26E-01 -4.38E-01 -2.59E-01 -3.56E-01 -4.42E-01
          2.54E-01 -2.02E-01  2.52E-01 -2.43E-01 -1.87E-01  2.88E-02 -5.89E-02 -1.52E-02 -1.84E-01 -3.51E-01 -6.41E-01  7.21E-01
         2.97E-01 -2.06E-02  5.73E-01 -3.74E-01  3.93E-01 -4.59E-01 -3.73E-01 -3.73E-01 -3.86E-01 -5.88E-01 -5.82E-01 -1.44E-01
          8.86E-02  5.15E-01 -5.46E-01  4.02E-01  2.00E-01
 
 OM77
+       -1.70E-01  3.13E-01 -1.54E-01 -5.89E-02  1.70E-01  4.18E-02 -3.75E-01 -4.38E-01  5.52E-02  6.05E-01 -4.24E-01 -2.16E-01
          2.09E-01 -9.97E-02 -7.78E-02  2.74E-01 -3.47E-01  2.29E-01  2.75E-01 -2.58E-01  2.46E-01  2.76E-01  4.38E-01 -4.27E-01
        -4.14E-01  2.30E-01  2.03E-01  3.21E-01 -4.96E-01  4.57E-02  3.54E-01 -6.42E-01 -3.01E-01  2.01E-01  4.47E-01  3.51E-01
         -2.54E-01 -3.05E-01  8.80E-02 -1.25E-01 -1.60E-01  1.45E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.09E-01  1.87E-01 -3.08E-01  5.86E-01 -1.79E-01  2.70E-02 -4.90E-01  3.13E-01  1.71E-01  9.22E-02 -4.19E-01 -6.19E-01
          4.41E-01 -1.78E-01  6.32E-01  5.98E-01 -1.40E-01 -1.66E-01  2.21E-01 -2.14E-01  3.52E-01  9.78E-02 -1.27E-01  1.89E-01
        -2.64E-01  4.84E-01  3.36E-01  2.03E-01  1.48E-01 -5.53E-01  2.22E-01 -4.73E-01  9.97E-02 -4.27E-02 -1.74E-01  2.67E-01
          3.80E-01  4.02E-01 -1.12E-01 -3.40E-01  3.97E-01  3.16E-01  1.29E-01
 
 OM88
+       -6.11E-01  6.10E-01 -6.06E-01 -3.18E-01  5.85E-01  6.63E-01 -6.44E-02 -6.25E-01  6.06E-01  5.32E-01 -2.07E-01 -2.39E-01
          1.54E-01  2.86E-01  2.62E-01  8.43E-01  8.13E-03 -1.06E-01  3.70E-01 -1.61E-01  8.06E-01  5.94E-01  7.33E-01 -6.39E-01
        -7.24E-01  4.95E-01  2.41E-02  7.38E-01 -3.92E-01 -1.58E-01  7.72E-01 -1.31E-01  4.14E-01  5.27E-01  6.21E-01  6.16E-01
          1.91E-01 -2.63E-01  5.36E-01 -7.06E-01 -5.09E-01  4.24E-01  4.21E-01  2.62E-01
 
 SG11
+        8.19E-01 -5.83E-01  1.61E-01  8.59E-01 -7.94E-01 -5.65E-01 -3.42E-01  8.70E-01 -3.32E-01 -5.26E-01 -6.32E-02 -3.36E-01
          3.46E-01 -4.97E-01  3.32E-01 -2.58E-01  6.96E-02 -2.73E-01  2.20E-03  1.87E-02 -4.72E-01 -5.80E-01 -7.03E-01  8.42E-01
         4.59E-01 -3.94E-02  2.97E-01 -4.02E-01  6.16E-01 -3.02E-01 -5.18E-01 -1.68E-01 -3.26E-01 -3.93E-01 -6.94E-01 -3.57E-01
          3.23E-01  7.12E-01 -6.34E-01  4.61E-01  7.78E-01 -2.93E-01  3.32E-01 -5.71E-01  3.43E-03
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        8.84E-02 -6.06E-01  2.62E-01  1.84E-02 -2.33E-01 -1.70E-01  3.03E-01  3.71E-01 -8.94E-02 -7.61E-01  6.55E-01  3.66E-01
         -1.18E-01 -3.38E-01  8.43E-02 -3.47E-01  5.16E-01 -3.32E-01 -9.39E-02 -1.31E-04 -6.53E-01 -6.25E-01 -5.34E-01  5.26E-01
         5.79E-01 -2.89E-01 -4.05E-01 -4.00E-01  5.71E-01  3.92E-01 -7.03E-01  5.15E-01 -1.34E-01  3.04E-02 -6.10E-01 -5.55E-01
          2.33E-01  3.03E-01 -1.32E-01  4.41E-01 -3.18E-02 -5.22E-01 -3.57E-01 -5.77E-01  2.60E-01  0.00E+00  4.28E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        3.08E+02
 
 TH 2
+        1.63E+02  4.52E+02
 
 TH 3
+       -5.90E+01  2.04E+01  5.46E+02
 
 TH 4
+       -3.02E+01 -1.20E+01  6.09E+01  2.83E+02
 
 TH 5
+       -8.11E+01 -1.30E+02 -7.17E+00  2.18E+01  3.64E+02
 
 TH 6
+       -2.04E+01 -7.69E+01 -8.17E+01 -5.55E+01  1.22E+02  2.91E+02
 
 TH 7
+        5.32E+01  1.53E+02 -1.83E+01  1.13E+02 -8.59E+01 -8.22E+01  3.63E+02
 
 TH 8
+       -1.99E+02 -2.87E+02 -9.99E+01 -9.86E+01  1.24E+02  1.50E+02 -2.18E+02  5.56E+02
 
 OM11
+        8.18E+01  3.19E+01 -1.02E+02  1.49E+01  2.41E+01  1.18E+02  1.10E+02 -4.00E+01  9.92E+02
 
 OM12
+        7.72E+01  4.56E+01  4.67E+01  4.30E+02  1.06E+02  5.66E+01  3.51E+02 -3.24E+02  8.73E+02  3.51E+03
 
 OM13
+       -2.31E+02 -3.49E+01 -2.84E+02  5.53E+01 -1.47E+02  1.12E+01  1.85E+02  2.07E+02 -9.18E+02 -8.06E+02  3.59E+03
 
 OM14
+        3.88E+01  4.76E+02  1.09E+02  9.12E+01 -9.66E+01  7.64E+01  1.15E+02 -1.50E+02 -3.22E+02  3.60E+02  3.68E+02  2.64E+03
 
 OM15
+        8.10E+01  1.69E+02 -1.81E+02 -9.11E+01 -4.37E+02 -3.76E+02 -1.33E+01 -1.21E+02 -6.99E+02 -1.77E+03  1.05E+03 -1.95E+02
          2.78E+03
 
 OM16
+        2.45E+02  6.79E+01 -2.87E+01  8.54E+01 -3.49E+02 -3.23E+02  1.27E+02 -2.49E+02  2.02E+02 -4.11E+02 -6.65E+02 -8.32E+02
          8.81E+02  1.90E+03
 
 OM17
+        1.63E+02  1.90E+02  1.90E+02  1.00E+02  1.93E+01  1.68E+02  1.08E+02 -2.11E+02  5.18E+02  1.93E+03 -1.47E+03  1.05E+03
         -1.59E+03 -3.88E+02  2.82E+03
 
 OM18
+       -7.18E+01 -2.69E+02  1.42E+02 -1.09E+02 -6.50E+01 -2.45E+02 -3.17E+02  2.28E+02 -1.11E+03 -2.88E+03  9.87E+02 -8.89E+02
          1.99E+03  6.22E+02 -2.38E+03  4.05E+03
 
 OM22
+        9.37E+01  1.78E+02  2.08E+01  2.66E+02 -4.05E+01 -3.33E+01  2.74E+02 -3.27E+02  3.56E+02  2.13E+03 -2.14E+02  1.58E+02
         -8.04E+02 -2.18E+02  9.83E+02 -1.75E+03  2.01E+03
 
 OM23
+        1.83E+01 -1.28E+02 -2.02E+02 -1.15E+02 -1.69E+02  1.87E+02 -2.23E+02  2.70E+02 -5.58E+02 -1.09E+03  1.76E+03 -4.15E+02
          8.62E+02 -6.58E+01 -8.30E+02  1.53E+03 -3.08E+02  3.76E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.82E+02  6.53E+02 -1.23E+02 -6.38E+01 -8.44E+01  1.20E+02  1.82E+02 -4.81E+02  2.17E+02  3.50E+02 -4.19E+02  1.91E+03
         -8.27E+02 -2.89E+02  1.46E+03 -1.10E+03 -1.44E+02 -3.46E+02  4.51E+03
 
 OM25
+        1.12E+02  8.71E+01 -2.03E+02 -5.21E+01 -2.32E+02 -4.52E+02  1.65E+01 -2.26E+02 -6.34E+02 -1.83E+03  5.90E+02 -8.33E+02
          2.90E+03  1.05E+03 -1.91E+03  2.29E+03 -1.14E+03 -2.72E+02 -6.04E+02  6.19E+03
 
 OM26
+        1.83E+01 -3.27E+01  1.21E+02  1.31E+02 -3.77E+02 -4.57E+02  2.40E+02 -6.05E+01 -3.14E+01 -4.23E+02 -2.99E+02 -2.96E+02
          9.97E+02  1.31E+03 -6.14E+02  7.61E+02 -5.64E+02 -7.02E+02 -1.10E+03  1.95E+03  3.66E+03
 
 OM27
+        2.39E+02  1.52E+02 -2.11E+02  1.55E+02  1.10E+02  3.44E+02  2.23E+02 -1.60E+02  6.12E+02  1.98E+03 -6.02E+02  1.44E+03
         -1.86E+03 -6.34E+02  2.10E+03 -2.33E+03  1.15E+03 -9.49E+02  2.81E+03 -2.43E+03 -1.50E+03  4.43E+03
 
 OM28
+       -3.69E+02 -4.65E+02  1.51E+02 -3.93E+02 -1.28E+02 -7.86E+01 -4.00E+02  8.90E+02 -9.30E+02 -4.28E+03  1.03E+03 -1.04E+03
          2.35E+03  8.79E+02 -2.57E+03  4.19E+03 -3.19E+03  1.42E+03 -2.19E+03  2.67E+03  2.41E+03 -3.55E+03  8.49E+03
 
 OM33
+       -1.15E+02 -5.94E+01  5.19E+01  1.52E+02 -5.43E+00  1.22E+01 -3.70E+00 -1.30E+02  3.00E+01 -6.81E+00  2.22E+02  6.75E+01
          9.06E+01  4.51E+02  1.68E+02 -4.62E+02 -3.17E+02 -1.35E+02  2.25E+02 -1.31E+02  1.51E+02  4.02E+02  1.52E+02  2.90E+03
 
 OM34
+        1.20E+02 -1.10E+02  2.67E+01 -2.72E+02  7.92E+01  1.16E+02 -1.96E+02  1.65E+02  2.24E+01 -4.85E+02 -1.85E+02 -8.36E+02
          1.63E+02  2.50E+02 -3.76E+02  1.85E+02 -3.48E+02  2.30E+02 -1.79E+02  9.38E+02  1.95E+02 -3.42E+02  8.59E+02  6.63E+02
         3.03E+03
 
 OM35
+       -1.76E+02 -1.31E+02  8.85E+01  1.02E+02  2.51E+02  5.88E+01  1.07E+02  2.88E+02  6.26E+02  9.07E+02 -9.33E+02  4.78E+01
         -1.24E+03  3.68E+02  7.70E+02 -1.31E+03 -9.00E+01 -2.01E+03  7.43E+02 -4.99E+02  7.67E+02  1.23E+03 -2.39E+02  9.06E+02
         9.92E+02  4.25E+03
 
 OM36
+       -3.33E+01  1.67E+02  1.21E+02  7.48E+01  1.07E+02  1.87E+01  1.64E+02 -1.17E+02 -2.67E+01  1.51E+02  2.93E+02  1.20E+02
          2.21E+02 -1.74E+02 -3.50E+02 -1.27E+01  1.50E+01 -7.47E+02  7.23E+01  5.15E+02  1.25E+02 -2.78E+02 -1.17E+02 -9.31E+01
        -2.48E+02  1.15E+03  1.91E+03
 
 OM37
+        1.79E+02 -2.78E+02 -4.04E+01 -1.69E+02  1.28E+02  1.88E+02 -2.60E+02  8.40E+01 -7.11E+02 -6.84E+02  9.85E+02 -3.49E+02
          7.25E+02 -4.37E+02 -6.53E+02  1.21E+03 -2.87E+02  2.48E+03 -2.40E+02  1.15E+03 -5.57E+02 -5.07E+02  4.45E+02 -1.24E+02
         1.39E+03 -1.85E+03 -9.19E+02  4.24E+03
 
 OM38
+        1.57E+02  2.71E+02 -1.35E+02  1.39E+02  3.20E+02 -1.40E+02  6.10E+01 -1.65E+02  9.13E+02  1.49E+03 -3.57E+03 -1.22E+02
         -1.33E+03  3.71E+02  1.33E+03 -8.34E+02  7.71E+02 -2.97E+03  6.53E+02 -1.56E+02  4.55E+02  7.17E+02 -1.93E+03 -2.44E+03
        -1.74E+03  8.58E+02  4.45E+02 -3.05E+03  9.42E+03
 
 OM44
+        6.77E+01 -1.16E+01 -1.86E+02  1.40E+02 -1.09E+02 -5.72E+01  1.19E+02 -1.12E+02  2.16E+02  5.02E+02 -3.31E+02 -3.03E+02
          1.87E+01  3.70E+02  9.31E+01 -2.63E+02  4.76E+02 -1.50E+02 -3.68E+02  2.26E+02  5.25E+02  9.03E+01 -3.48E+02  1.49E+02
         1.37E+02  3.03E+02 -6.27E+00 -3.25E+02  3.49E+02  7.48E+02
 
 OM45
+       -1.02E+02 -7.78E+01  9.81E+01 -1.79E+02  1.18E+02  1.34E+02 -2.20E+02  2.57E+02 -1.49E+02 -8.92E+02  1.05E+02 -4.17E+02
          1.88E+01 -7.06E+01 -1.59E+02  6.25E+02 -3.96E+02  7.95E+02 -2.93E+02 -6.18E+02 -1.03E+03 -2.50E+02  1.10E+03  3.16E+02
         5.24E+02  5.47E+01  7.44E+01  1.76E+02 -1.05E+03 -1.52E+02  1.55E+03
 
 OM46
+        6.14E+01  1.16E+02  1.47E+02 -9.72E+01  1.51E+02  2.57E+01 -1.36E+02  8.95E+00 -2.43E+02 -1.74E+02  1.41E+02  5.45E+02
         -1.62E+02 -5.85E+02  1.93E+02  6.76E+01 -3.58E+02  1.18E+02  5.97E+02 -1.17E+03 -1.23E+03  3.82E+02 -3.42E+02  3.49E+01
        -2.82E+02  4.70E+01  4.34E+02  1.58E+02 -4.29E+02 -5.73E+02  6.20E+02  1.72E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.31E+02  2.10E+02 -1.92E+02  2.37E+02 -2.42E+02 -1.59E+02  2.17E+02 -4.49E+02  1.55E+02  1.03E+03 -2.77E+02  5.46E+02
          8.77E+01  2.52E+02  6.01E+02 -6.12E+02  7.98E+02 -3.25E+02  1.08E+03  1.82E+02  4.90E+02  8.70E+02 -1.61E+03  2.43E+02
        -8.25E+02  1.72E+02  2.60E+02 -8.81E+02  1.31E+03  6.89E+02 -8.52E+02 -7.01E+02  2.58E+03
 
 OM48
+       -1.70E+02 -4.96E+02  2.05E+02 -1.94E+02  2.73E+02  6.78E+00 -4.19E+02  3.68E+02 -1.00E+02 -7.70E+02 -1.10E+02 -1.77E+03
          3.71E+02  2.00E+02 -1.03E+03  1.29E+03 -6.21E+02  7.37E+02 -2.33E+03  7.04E+02 -2.89E+02 -2.06E+03  1.56E+03 -6.10E+02
         2.59E+02 -1.08E+03 -3.90E+02  1.07E+03  4.22E+00 -4.69E+02  6.16E+02  3.47E+02 -1.74E+03  3.39E+03
 
 OM55
+       -2.04E+02 -1.30E+02  1.36E+02  5.75E+01  2.30E+02  1.46E+02 -1.67E+02  1.73E+02  1.34E+02  8.07E+02 -3.90E+02  1.07E+02
         -1.18E+03 -6.01E+02  6.91E+02 -9.85E+02  6.22E+02 -3.38E+02 -2.74E+02 -1.77E+03 -1.12E+03  4.96E+02 -1.54E+03 -1.24E+01
        -1.90E+02  1.42E+02 -3.47E+02 -4.52E+02  4.70E+02 -1.35E+02  2.05E+02  2.62E+02 -2.89E+02  2.77E+02  1.22E+03
 
 OM56
+       -3.30E+02 -4.33E+02  9.39E+01  1.45E+02  2.34E+02  1.93E+02 -7.93E+01  3.33E+02  9.61E+01  7.44E+02  5.92E+02  4.77E+00
         -1.26E+03 -1.18E+03  3.62E+02 -5.33E+02  5.56E+02  7.73E+02 -1.03E+03 -2.72E+03 -1.37E+03  3.72E+02 -9.19E+02 -2.03E+02
        -1.67E+02 -1.35E+03 -1.51E+03  3.49E+02 -9.86E+02 -4.89E+01  1.08E+02  1.69E+02 -4.19E+02  6.15E+02  1.36E+03  4.40E+03
 
 OM57
+        1.44E+01  1.60E+02  1.11E+02 -2.17E+02 -3.61E+02 -1.17E+02 -1.13E+02 -2.17E+01 -5.62E+02 -1.71E+03  6.67E+02 -1.51E+02
          1.66E+03  3.72E+02 -1.12E+03  1.67E+03 -9.27E+02  1.31E+03 -1.83E+02  1.97E+03  4.62E+02 -1.98E+03  2.33E+03 -4.10E+02
         2.54E+02 -1.08E+03  1.21E+02  7.38E+02 -1.03E+03 -1.51E+02  2.97E+02 -2.35E+02  2.64E+01  3.95E+02 -7.40E+02 -9.81E+02
          2.68E+03
 
 OM58
+       -7.62E+01 -2.65E+02  3.31E+02  2.38E+02  3.07E+02  3.53E+02  6.68E+01  4.18E+00  8.07E+02  2.50E+03 -9.96E+02  5.82E+02
         -3.50E+03 -9.71E+02  1.99E+03 -2.26E+03  1.32E+03 -1.13E+02  9.87E+02 -5.59E+03 -1.52E+03  2.77E+03 -3.53E+03 -4.13E+02
        -1.26E+03  2.33E+02 -9.20E+02 -8.18E+02  1.04E+03 -6.89E+01 -1.95E+02  4.90E+02  2.53E+02 -5.93E+02  1.71E+03  3.07E+03
         -2.43E+03  6.82E+03
 
 OM66
+       -1.35E+02 -2.10E+02  8.55E+00  2.47E+01  4.76E+01  9.64E+01 -9.56E+01  1.23E+02  2.14E+01  2.28E+02  1.72E+02 -1.85E+02
         -3.73E+02 -5.10E+02  1.49E+02 -1.64E+02  3.18E+02  1.24E+02 -5.66E+02 -3.92E+02 -7.28E+02  2.36E+01 -4.51E+02 -3.40E+02
         2.50E+01 -8.68E+02 -7.17E+02  1.22E+02 -1.07E+02  3.81E+01 -4.06E+01 -3.11E+02 -1.56E+02  4.31E+02  6.27E+02  1.65E+03
         -1.35E+02  7.48E+02  1.17E+03
 
 OM67
+        1.47E+02  3.57E+02  1.72E+02 -1.46E+02 -8.14E+01 -2.16E+02  5.68E+01 -1.91E+02  3.34E+01 -4.85E+02 -5.41E+02  1.37E+02
          3.47E+02  5.45E+02 -4.67E+01  9.66E+01 -5.50E+02 -5.28E+02  2.47E+02  5.39E+02  8.31E+02 -5.29E+02  7.71E+02 -7.59E+01
         2.20E+02  3.31E+02  7.81E+01 -2.74E+02  3.36E+02 -1.76E+02 -1.03E+02  2.40E+02 -1.82E+02  6.73E+01 -3.26E+02 -7.12E+02
          5.19E+02 -6.90E+02 -5.22E+02  1.63E+03
 
 OM68
+       -2.01E+02 -8.00E+01 -1.16E+02  4.75E+00  2.61E+02  2.75E+02 -1.26E+02  3.07E+01 -9.43E+01  6.46E+02  6.45E+02  3.39E+02
         -7.64E+02 -1.41E+03  2.53E+02 -7.03E+02  8.59E+02  3.69E+02 -1.10E+02 -1.28E+03 -2.34E+03  7.66E+02 -1.94E+03 -5.81E+02
        -5.05E+02 -1.15E+03 -4.90E+02  4.20E+02 -2.07E+02 -1.89E+02  3.06E+02  4.24E+02 -1.28E+02  3.46E+02  1.03E+03  1.83E+03
         -4.53E+02  1.34E+03  1.05E+03 -9.54E+02  2.64E+03
 
 OM77
+        4.88E+01  6.88E+01 -1.25E+02  1.03E+02 -5.96E+01  2.67E+01  1.37E+02 -5.06E+01  1.52E+02  5.42E+02 -1.48E+02  4.60E+02
         -3.24E+02  2.86E+01  5.39E+02 -5.05E+02  4.02E+02 -3.84E+02  6.78E+02 -6.75E+02 -8.76E+01  1.05E+03 -9.13E+02  1.04E+02
        -6.45E+02  3.96E+02  1.50E+02 -8.26E+02  9.93E+02  1.80E+02 -1.83E+02  1.84E+00  9.41E+02 -9.16E+02  4.08E+01 -8.72E+01
         -4.84E+02  6.40E+02 -9.53E+01 -3.16E+02  1.19E+02  8.78E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -2.40E+02 -1.04E+02  5.49E+01 -4.04E+02  1.77E+01 -1.92E+02 -1.36E+02  2.72E+02 -6.35E+02 -2.18E+03  1.09E+03 -1.17E+03
          1.76E+03  2.79E+02 -2.23E+03  1.92E+03 -1.19E+03  7.14E+02 -2.42E+03  2.46E+03  8.20E+02 -3.20E+03  3.61E+03 -4.93E+02
         1.20E+03 -9.03E+02  1.19E+02  1.23E+03 -1.88E+03 -3.72E+02  6.80E+02  4.49E+01 -2.04E+03  2.26E+03 -4.93E+02 -3.45E+02
          1.70E+03 -3.14E+03  2.78E+01  9.90E+02 -5.58E+02 -1.53E+03  4.99E+03
 
 OM88
+        1.50E+02  4.07E+02 -5.27E+01  1.47E+02 -6.75E+01  1.41E+01  2.27E+02 -4.08E+02  3.58E+02  1.78E+03  7.56E+01  9.59E+02
         -8.99E+02 -5.07E+02  1.10E+03 -2.54E+03  1.32E+03 -1.04E+03  1.11E+03 -1.33E+03 -9.41E+02  1.62E+03 -3.59E+03  8.27E+02
        -2.07E+02  5.66E+02  3.20E+02 -7.41E+02 -4.67E+02  9.30E+01 -5.39E+02  3.18E+02  6.49E+02 -1.40E+03  6.01E+02  1.27E+02
         -1.21E+03  1.20E+03 -6.13E+01 -1.41E+02  8.57E+02  4.69E+02 -1.86E+03  2.91E+03
 
 SG11
+       -1.70E+03  6.62E+03 -1.97E+03 -1.13E+03 -2.80E+03  1.82E+03  9.22E+03 -2.71E+03  5.62E+03  1.79E+04  1.77E+04  6.89E+03
         -8.74E+03 -5.31E+03  9.92E+03 -1.92E+04  2.32E+04  1.28E+04 -1.95E+04 -1.57E+04  1.88E+04  5.15E+03 -8.61E+03 -1.02E+04
        -3.66E+02 -9.48E+03 -8.74E+03 -9.74E+01 -1.47E+04  1.03E+04 -9.61E+03 -2.05E+04  6.50E+03 -1.30E+04  2.79E+02  2.48E+04
         -1.57E+04  1.70E+04  1.13E+04 -2.81E+03  5.15E+03  6.26E+03 -4.45E+03  8.75E+03  2.41E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.63E+03 -3.22E+03  2.68E+03  4.31E+03  2.07E+03  8.48E+02 -1.42E+03  3.96E+03  6.30E+03  6.41E+03 -6.48E+03 -8.76E+03
         -6.10E+03  5.42E+03  6.12E+02 -2.27E+03 -1.29E+02 -2.83E+03 -1.94E+04 -6.00E+03  1.01E+04 -5.92E+03  1.41E+04  2.31E+03
         2.23E+03  1.61E+04  4.36E+03 -8.15E+03 -5.42E+03  3.01E+03  6.30E+03 -2.78E+03 -6.78E+03  7.32E+03  3.76E+03  7.38E+02
         -9.49E+03  6.95E+03 -2.29E+03  3.10E+02 -2.09E+03 -9.88E+02 -1.15E+03 -4.25E+02  1.83E+05  0.00E+00  8.37E+05
 
1
 
 
 #TBLN:      2
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
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
 ABSOLUTE TOLERANCE-ADVAN 9,13,14 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmt13_bayes.ext
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
 iteration        -2000 MCMCOBJ=   -6807.18465308378     
 iteration        -1990 MCMCOBJ=   -6618.94417066592     
 iteration        -1980 MCMCOBJ=   -6652.53700155533     
 iteration        -1970 MCMCOBJ=   -6632.68940248933     
 iteration        -1960 MCMCOBJ=   -6634.35597461014     
 iteration        -1950 MCMCOBJ=   -6655.33845206986     
 iteration        -1940 MCMCOBJ=   -6634.58092642089     
 iteration        -1930 MCMCOBJ=   -6606.91179845765     
 iteration        -1920 MCMCOBJ=   -6603.29268448931     
 iteration        -1910 MCMCOBJ=   -6614.84912707858     
 iteration        -1900 MCMCOBJ=   -6594.39560317190     
 iteration        -1890 MCMCOBJ=   -6616.45067828523     
 iteration        -1880 MCMCOBJ=   -6636.21475373637     
 iteration        -1870 MCMCOBJ=   -6617.83324061983     
 iteration        -1860 MCMCOBJ=   -6584.22555159960     
 iteration        -1850 MCMCOBJ=   -6473.08294839672     
 iteration        -1840 MCMCOBJ=   -6618.96474157373     
 iteration        -1830 MCMCOBJ=   -6568.56080736569     
 iteration        -1820 MCMCOBJ=   -6583.41280068564     
 iteration        -1810 MCMCOBJ=   -6573.71602104014     
 iteration        -1800 MCMCOBJ=   -6551.80002469569     
 iteration        -1790 MCMCOBJ=   -6577.89740613232     
 iteration        -1780 MCMCOBJ=   -6546.10618660895     
 iteration        -1770 MCMCOBJ=   -6604.36017188114     
 iteration        -1760 MCMCOBJ=   -6592.11553765877     
 iteration        -1750 MCMCOBJ=   -6611.48087017638     
 iteration        -1740 MCMCOBJ=   -6555.97977111001     
 iteration        -1730 MCMCOBJ=   -6531.53721599108     
 iteration        -1720 MCMCOBJ=   -6501.79493619175     
 iteration        -1710 MCMCOBJ=   -6543.53913919392     
 iteration        -1700 MCMCOBJ=   -6495.13843898942     
 iteration        -1690 MCMCOBJ=   -6509.14092582208     
 iteration        -1680 MCMCOBJ=   -6562.70273513519     
 iteration        -1670 MCMCOBJ=   -6547.15201622454     
 iteration        -1660 MCMCOBJ=   -6550.08786787776     
 iteration        -1650 MCMCOBJ=   -6540.29582500860     
 iteration        -1640 MCMCOBJ=   -6561.43172575067     
 iteration        -1630 MCMCOBJ=   -6577.76361539334     
 iteration        -1620 MCMCOBJ=   -6544.72238214113     
 iteration        -1610 MCMCOBJ=   -6521.78778508150     
 iteration        -1600 MCMCOBJ=   -6516.77815049334     
 iteration        -1590 MCMCOBJ=   -6493.40636561718     
 iteration        -1580 MCMCOBJ=   -6490.50999546212     
 iteration        -1570 MCMCOBJ=   -6539.53322173293     
 iteration        -1560 MCMCOBJ=   -6518.59373467046     
 iteration        -1550 MCMCOBJ=   -6535.89589289727     
 iteration        -1540 MCMCOBJ=   -6528.63761097518     
 iteration        -1530 MCMCOBJ=   -6459.48026522716     
 iteration        -1520 MCMCOBJ=   -6462.76622004389     
 iteration        -1510 MCMCOBJ=   -6542.58924836678     
 iteration        -1500 MCMCOBJ=   -6546.50496362520     
 iteration        -1490 MCMCOBJ=   -6565.34983733327     
 iteration        -1480 MCMCOBJ=   -6506.51354299431     
 iteration        -1470 MCMCOBJ=   -6545.65232087150     
 iteration        -1460 MCMCOBJ=   -6528.13753392054     
 iteration        -1450 MCMCOBJ=   -6490.58371170488     
 iteration        -1440 MCMCOBJ=   -6544.82957648762     
 iteration        -1430 MCMCOBJ=   -6543.46512991891     
 iteration        -1420 MCMCOBJ=   -6513.29483004850     
 iteration        -1410 MCMCOBJ=   -6492.93061865906     
 iteration        -1400 MCMCOBJ=   -6516.81697358400     
 iteration        -1390 MCMCOBJ=   -6466.73603636213     
 iteration        -1380 MCMCOBJ=   -6506.80804475716     
 iteration        -1370 MCMCOBJ=   -6543.61278680519     
 iteration        -1360 MCMCOBJ=   -6506.75898627417     
 iteration        -1350 MCMCOBJ=   -6494.15856025877     
 iteration        -1340 MCMCOBJ=   -6485.09031290766     
 iteration        -1330 MCMCOBJ=   -6537.48287570967     
 iteration        -1320 MCMCOBJ=   -6521.02268758126     
 iteration        -1310 MCMCOBJ=   -6512.27995553255     
 iteration        -1300 MCMCOBJ=   -6551.43796780954     
 iteration        -1290 MCMCOBJ=   -6541.44821986296     
 iteration        -1280 MCMCOBJ=   -6475.67890852583     
 iteration        -1270 MCMCOBJ=   -6522.74412801978     
 iteration        -1260 MCMCOBJ=   -6489.36164269615     
 iteration        -1250 MCMCOBJ=   -6529.36720014656     
 iteration        -1240 MCMCOBJ=   -6509.99271000951     
 iteration        -1230 MCMCOBJ=   -6499.11386883561     
 iteration        -1220 MCMCOBJ=   -6535.88682831561     
 iteration        -1210 MCMCOBJ=   -6507.34256935991     
 iteration        -1200 MCMCOBJ=   -6522.80261318845     
 iteration        -1190 MCMCOBJ=   -6450.17304979332     
 iteration        -1180 MCMCOBJ=   -6518.99174344582     
 iteration        -1170 MCMCOBJ=   -6538.40406909141     
 iteration        -1160 MCMCOBJ=   -6518.20818228708     
 iteration        -1150 MCMCOBJ=   -6463.13604018829     
 iteration        -1140 MCMCOBJ=   -6538.80016991978     
 iteration        -1130 MCMCOBJ=   -6452.85258066774     
 iteration        -1120 MCMCOBJ=   -6512.78308751497     
 iteration        -1110 MCMCOBJ=   -6503.24305313574     
 iteration        -1100 MCMCOBJ=   -6489.62314985946     
 iteration        -1090 MCMCOBJ=   -6527.39938197976     
 iteration        -1080 MCMCOBJ=   -6485.17453650483     
 iteration        -1070 MCMCOBJ=   -6528.03173314366     
 iteration        -1060 MCMCOBJ=   -6511.00718141850     
 iteration        -1050 MCMCOBJ=   -6518.88784790013     
 iteration        -1040 MCMCOBJ=   -6543.81874437727     
 iteration        -1030 MCMCOBJ=   -6507.79260229028     
 iteration        -1020 MCMCOBJ=   -6490.07736868992     
 iteration        -1010 MCMCOBJ=   -6511.76165619797     
 iteration        -1000 MCMCOBJ=   -6476.18637046144     
 iteration         -990 MCMCOBJ=   -6487.87985722980     
 iteration         -980 MCMCOBJ=   -6493.35265303235     
 iteration         -970 MCMCOBJ=   -6538.22910203403     
 iteration         -960 MCMCOBJ=   -6510.63150198677     
 iteration         -950 MCMCOBJ=   -6515.43758117512     
 iteration         -940 MCMCOBJ=   -6521.52639210861     
 iteration         -930 MCMCOBJ=   -6494.56294212161     
 iteration         -920 MCMCOBJ=   -6482.26887148040     
 iteration         -910 MCMCOBJ=   -6539.06367531927     
 iteration         -900 MCMCOBJ=   -6487.40758861028     
 iteration         -890 MCMCOBJ=   -6505.27740868322     
 iteration         -880 MCMCOBJ=   -6495.36820194858     
 iteration         -870 MCMCOBJ=   -6521.00617301859     
 iteration         -860 MCMCOBJ=   -6503.16116703339     
 iteration         -850 MCMCOBJ=   -6498.06736632725     
 iteration         -840 MCMCOBJ=   -6460.65635401676     
 iteration         -830 MCMCOBJ=   -6518.23443894040     
 iteration         -820 MCMCOBJ=   -6515.21733095782     
 iteration         -810 MCMCOBJ=   -6450.46517530119     
 iteration         -800 MCMCOBJ=   -6457.62496536350     
 iteration         -790 MCMCOBJ=   -6501.98486264553     
 iteration         -780 MCMCOBJ=   -6448.89066278396     
 iteration         -770 MCMCOBJ=   -6531.14537472105     
 iteration         -760 MCMCOBJ=   -6533.09597123101     
 iteration         -750 MCMCOBJ=   -6493.87228348050     
 iteration         -740 MCMCOBJ=   -6476.13599135459     
 iteration         -730 MCMCOBJ=   -6484.57527937687     
 iteration         -720 MCMCOBJ=   -6491.81310673723     
 iteration         -710 MCMCOBJ=   -6436.50243224808     
 iteration         -700 MCMCOBJ=   -6516.01989867635     
 iteration         -690 MCMCOBJ=   -6492.78224932594     
 iteration         -680 MCMCOBJ=   -6569.50579647964     
 iteration         -670 MCMCOBJ=   -6540.62016687099     
 iteration         -660 MCMCOBJ=   -6489.71937825989     
 iteration         -650 MCMCOBJ=   -6558.37760759432     
 iteration         -640 MCMCOBJ=   -6515.09629942202     
 iteration         -630 MCMCOBJ=   -6503.69795627453     
 iteration         -620 MCMCOBJ=   -6506.54840069453     
 iteration         -610 MCMCOBJ=   -6488.79589045932     
 iteration         -600 MCMCOBJ=   -6450.98976472838     
 iteration         -590 MCMCOBJ=   -6529.55066175422     
 iteration         -580 MCMCOBJ=   -6462.64808316258     
 iteration         -570 MCMCOBJ=   -6468.19675643822     
 iteration         -560 MCMCOBJ=   -6521.48175782595     
 iteration         -550 MCMCOBJ=   -6420.05583884165     
 iteration         -540 MCMCOBJ=   -6524.36366802891     
 iteration         -530 MCMCOBJ=   -6468.96619222032     
 iteration         -520 MCMCOBJ=   -6476.03100109682     
 iteration         -510 MCMCOBJ=   -6457.99095555611     
 iteration         -500 MCMCOBJ=   -6505.87128093541     
 iteration         -490 MCMCOBJ=   -6520.16154758461     
 iteration         -480 MCMCOBJ=   -6519.49851663264     
 iteration         -470 MCMCOBJ=   -6461.63515481080     
 iteration         -460 MCMCOBJ=   -6554.30787858817     
 iteration         -450 MCMCOBJ=   -6457.02489544103     
 iteration         -440 MCMCOBJ=   -6523.32172368036     
 iteration         -430 MCMCOBJ=   -6494.31380038996     
 iteration         -420 MCMCOBJ=   -6451.50969573269     
 iteration         -410 MCMCOBJ=   -6468.50192198789     
 iteration         -400 MCMCOBJ=   -6436.76341878420     
 iteration         -390 MCMCOBJ=   -6497.13194901143     
 iteration         -380 MCMCOBJ=   -6499.75146021615     
 iteration         -370 MCMCOBJ=   -6502.05222901590     
 iteration         -360 MCMCOBJ=   -6460.53191369567     
 iteration         -350 MCMCOBJ=   -6495.69711557988     
 iteration         -340 MCMCOBJ=   -6530.13254474098     
 iteration         -330 MCMCOBJ=   -6517.22049678187     
 iteration         -320 MCMCOBJ=   -6499.95217850424     
 iteration         -310 MCMCOBJ=   -6424.69553215033     
 iteration         -300 MCMCOBJ=   -6481.82060629832     
 iteration         -290 MCMCOBJ=   -6469.92057886453     
 iteration         -280 MCMCOBJ=   -6505.03971984334     
 iteration         -270 MCMCOBJ=   -6508.26512698715     
 iteration         -260 MCMCOBJ=   -6474.03501255378     
 iteration         -250 MCMCOBJ=   -6465.66908852899     
 iteration         -240 MCMCOBJ=   -6470.56632299459     
 iteration         -230 MCMCOBJ=   -6518.16828875548     
 iteration         -220 MCMCOBJ=   -6476.77791986839     
 iteration         -210 MCMCOBJ=   -6523.33604910051     
 iteration         -200 MCMCOBJ=   -6488.89858845071     
 iteration         -190 MCMCOBJ=   -6495.87280200461     
 iteration         -180 MCMCOBJ=   -6514.37942756701     
 iteration         -170 MCMCOBJ=   -6458.33462160156     
 iteration         -160 MCMCOBJ=   -6494.38304515845     
 iteration         -150 MCMCOBJ=   -6526.01988530660     
 iteration         -140 MCMCOBJ=   -6480.42873673400     
 iteration         -130 MCMCOBJ=   -6430.72926191849     
 iteration         -120 MCMCOBJ=   -6459.74503571808     
 iteration         -110 MCMCOBJ=   -6489.01298708391     
 iteration         -100 MCMCOBJ=   -6405.17479234327     
 iteration          -90 MCMCOBJ=   -6474.37106554670     
 iteration          -80 MCMCOBJ=   -6467.13206801065     
 iteration          -70 MCMCOBJ=   -6542.86358228811     
 iteration          -60 MCMCOBJ=   -6512.67427998480     
 iteration          -50 MCMCOBJ=   -6512.95805804341     
 iteration          -40 MCMCOBJ=   -6437.62449364108     
 iteration          -30 MCMCOBJ=   -6438.06367982530     
 iteration          -20 MCMCOBJ=   -6568.97470985986     
 iteration          -10 MCMCOBJ=   -6545.37395902472     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6463.38416800032     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS NOT PERFORMED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6463.38416800032     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3581.59292787047     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6463.38416800032     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5728.23334143659     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    55.1779157436876     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6463.38416800032     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6408.20625225664     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   325.72
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6463.384       **************************************************
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
 
         4.02E+00 -2.24E+00  6.44E-01 -1.09E-01  2.18E+00  3.45E-01  3.75E+00 -5.75E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.91E-01
 
 ETA2
+       -5.42E-02  2.96E-01
 
 ETA3
+        4.89E-02 -4.70E-03  1.05E-01
 
 ETA4
+        1.20E-01  1.43E-01  2.98E-02  5.16E-01
 
 ETA5
+        2.24E-02 -9.50E-02  4.00E-02 -1.22E-01  2.56E-01
 
 ETA6
+        1.87E-02  8.27E-02 -1.40E-02  1.58E-01 -1.36E-01  3.32E-01
 
 ETA7
+        3.36E-03 -9.23E-02  8.35E-03 -1.37E-01  9.39E-02  6.34E-03  2.40E-01
 
 ETA8
+        1.14E-01  1.01E-01  4.19E-02  1.45E-01  1.22E-02  4.14E-02  1.63E-02  2.10E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.89E-03
 
 EPS2
+        0.00E+00  2.21E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.40E-01
 
 ETA2
+       -1.84E-01  5.44E-01
 
 ETA3
+        2.79E-01 -2.66E-02  3.25E-01
 
 ETA4
+        3.08E-01  3.66E-01  1.28E-01  7.18E-01
 
 ETA5
+        8.19E-02 -3.45E-01  2.43E-01 -3.35E-01  5.06E-01
 
 ETA6
+        6.00E-02  2.64E-01 -7.48E-02  3.81E-01 -4.65E-01  5.76E-01
 
 ETA7
+        1.27E-02 -3.46E-01  5.25E-02 -3.88E-01  3.78E-01  2.25E-02  4.90E-01
 
 ETA8
+        4.62E-01  4.07E-01  2.82E-01  4.40E-01  5.28E-02  1.57E-01  7.26E-02  4.58E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.94E-02
 
 EPS2
+        0.00E+00  1.49E-01
 
1
 
 
 #TBLN:      3
 #METH: NUTS Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
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
 ABSOLUTE TOLERANCE-ADVAN 9,13,14 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmt13.ext
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
 MASS MATRIX ACCUMULATION ITERATIONS (MADAPT):          500
 MASS MATRIX BLOCKING TYPE:                              B
 MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (NUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (KAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (NUTS_DELTA):                   0.800000000000000
 NUTS GAMMA SETTING (NUTS_GAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000
 NUTS WARMUP METHOD (NUTS_TEST):       NO
 NUTS MAXIMAL DEPTH SEARCH (NUTS_MAXDEPTH):                 
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       7.500000000000000E-02
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): 2.500000000000000E-02
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 5.000000000000000E-02
 INITIAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPITER): 1
 INTERVAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPINTER):0
 ETA PARAMETERIZATION (NUTS_EPARAM):0
 OMEGA PARAMETERIZATION (NUTS_OPARAM):1
 SIGMA PARAMETERIZATION (NUTS_SPARAM):1
 NUTS REGULARIZING METHOD (NUTS_REG): 0.00000000000000

 
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
 iteration        -1000 MCMCOBJ=   -6311.52056814835     
 iteration         -999 MCMCOBJ=   -6311.52065604754     
 iteration         -998 MCMCOBJ=   -6491.84510063278     
 iteration         -997 MCMCOBJ=   -6565.32188774501     
 iteration         -996 MCMCOBJ=   -6569.74766614494     
 iteration         -995 MCMCOBJ=   -6599.75588883012     
 iteration         -994 MCMCOBJ=   -6617.78529919122     
 iteration         -993 MCMCOBJ=   -6609.35094509948     
 iteration         -992 MCMCOBJ=   -6648.72296238860     
 iteration         -991 MCMCOBJ=   -6625.25890063024     
 iteration         -990 MCMCOBJ=   -6662.64305719917     
 iteration         -989 MCMCOBJ=   -6628.33895103537     
 iteration         -988 MCMCOBJ=   -6646.71510799140     
 iteration         -987 MCMCOBJ=   -6571.33682084335     
 iteration         -986 MCMCOBJ=   -6597.22226790625     
 iteration         -985 MCMCOBJ=   -6628.02143846330     
 iteration         -984 MCMCOBJ=   -6641.90477487105     
 iteration         -983 MCMCOBJ=   -6594.08119349992     
 iteration         -982 MCMCOBJ=   -6591.80229319015     
 iteration         -981 MCMCOBJ=   -6626.81714895360     
 iteration         -980 MCMCOBJ=   -6605.93527244309     
 iteration         -979 MCMCOBJ=   -6650.26468693580     
 iteration         -978 MCMCOBJ=   -6650.26468832383     
 iteration         -977 MCMCOBJ=   -6655.85383336780     
 iteration         -976 MCMCOBJ=   -6648.35093039533     
 iteration         -975 MCMCOBJ=   -6610.68185304327     
 iteration         -974 MCMCOBJ=   -6608.14194345866     
 iteration         -973 MCMCOBJ=   -6659.80047562109     
 iteration         -972 MCMCOBJ=   -6672.40501006185     
 iteration         -971 MCMCOBJ=   -6636.79499810091     
 iteration         -970 MCMCOBJ=   -6633.04157878186     
 iteration         -969 MCMCOBJ=   -6617.19817184304     
 iteration         -968 MCMCOBJ=   -6642.14695277538     
 iteration         -967 MCMCOBJ=   -6650.55450696084     
 iteration         -966 MCMCOBJ=   -6617.96766709864     
 iteration         -965 MCMCOBJ=   -6618.93789485950     
 iteration         -964 MCMCOBJ=   -6660.47865997887     
 iteration         -963 MCMCOBJ=   -6653.05560125190     
 iteration         -962 MCMCOBJ=   -6673.66518515069     
 iteration         -961 MCMCOBJ=   -6654.84461007007     
 iteration         -960 MCMCOBJ=   -6627.39083348358     
 iteration         -959 MCMCOBJ=   -6614.43429872144     
 iteration         -958 MCMCOBJ=   -6625.34820585494     
 iteration         -957 MCMCOBJ=   -6599.01977745614     
 iteration         -956 MCMCOBJ=   -6554.93082319258     
 iteration         -955 MCMCOBJ=   -6630.49923614145     
 iteration         -954 MCMCOBJ=   -6652.44331984040     
 iteration         -953 MCMCOBJ=   -6636.89395948959     
 iteration         -952 MCMCOBJ=   -6634.34335626844     
 iteration         -951 MCMCOBJ=   -6628.41835032592     
 iteration         -950 MCMCOBJ=   -6620.83406424964     
 iteration         -949 MCMCOBJ=   -6642.79467326359     
 iteration         -948 MCMCOBJ=   -6646.32040053014     
 iteration         -947 MCMCOBJ=   -6627.25570898597     
 iteration         -946 MCMCOBJ=   -6621.30231646529     
 iteration         -945 MCMCOBJ=   -6676.06706793123     
 iteration         -944 MCMCOBJ=   -6655.02902320581     
 iteration         -943 MCMCOBJ=   -6617.62103774177     
 iteration         -942 MCMCOBJ=   -6636.92134585546     
 iteration         -941 MCMCOBJ=   -6672.86732382272     
 iteration         -940 MCMCOBJ=   -6658.77986153450     
 iteration         -939 MCMCOBJ=   -6670.18826166094     
 iteration         -938 MCMCOBJ=   -6573.58761077159     
 iteration         -937 MCMCOBJ=   -6641.82784335396     
 iteration         -936 MCMCOBJ=   -6638.79235360786     
 iteration         -935 MCMCOBJ=   -6640.63196621467     
 iteration         -934 MCMCOBJ=   -6640.63196667181     
 iteration         -933 MCMCOBJ=   -6648.18639353718     
 iteration         -932 MCMCOBJ=   -6685.90214609445     
 iteration         -931 MCMCOBJ=   -6689.88253902844     
 iteration         -930 MCMCOBJ=   -6673.50136442955     
 iteration         -929 MCMCOBJ=   -6624.03059633035     
 iteration         -928 MCMCOBJ=   -6645.12368038716     
 iteration         -927 MCMCOBJ=   -6620.79664539297     
 iteration         -926 MCMCOBJ=   -6610.32349746131     
 iteration         -925 MCMCOBJ=   -6611.29913112250     
 iteration         -924 MCMCOBJ=   -6611.01477160329     
 iteration         -923 MCMCOBJ=   -6637.77434420597     
 iteration         -922 MCMCOBJ=   -6530.34882655126     
 iteration         -921 MCMCOBJ=   -6545.39286703473     
 iteration         -920 MCMCOBJ=   -6564.23572984683     
 iteration         -919 MCMCOBJ=   -6571.36107079548     
 iteration         -918 MCMCOBJ=   -6563.64867946088     
 iteration         -917 MCMCOBJ=   -6603.87980542923     
 iteration         -916 MCMCOBJ=   -6642.61573163217     
 iteration         -915 MCMCOBJ=   -6656.53246924229     
 iteration         -914 MCMCOBJ=   -6636.38530022864     
 iteration         -913 MCMCOBJ=   -6627.20487056866     
 iteration         -912 MCMCOBJ=   -6662.33771507481     
 iteration         -911 MCMCOBJ=   -6637.40223831044     
 iteration         -910 MCMCOBJ=   -6636.63792194611     
 iteration         -909 MCMCOBJ=   -6632.15458272525     
 iteration         -908 MCMCOBJ=   -6642.13982743759     
 iteration         -907 MCMCOBJ=   -6663.78011060498     
 iteration         -906 MCMCOBJ=   -6642.60714856072     
 iteration         -905 MCMCOBJ=   -6625.15481707589     
 iteration         -904 MCMCOBJ=   -6649.84124178335     
 iteration         -903 MCMCOBJ=   -6679.43407937641     
 iteration         -902 MCMCOBJ=   -6673.21652265556     
 iteration         -901 MCMCOBJ=   -6647.16447725895     
 iteration         -900 MCMCOBJ=   -6643.90569927059     
 iteration         -899 MCMCOBJ=   -6675.84876069932     
 iteration         -898 MCMCOBJ=   -6667.74830909762     
 iteration         -897 MCMCOBJ=   -6694.91719963168     
 iteration         -896 MCMCOBJ=   -6694.91719906388     
 iteration         -895 MCMCOBJ=   -6645.33330778766     
 iteration         -894 MCMCOBJ=   -6622.31118215030     
 iteration         -893 MCMCOBJ=   -6656.02116244174     
 iteration         -892 MCMCOBJ=   -6609.25893968583     
 iteration         -891 MCMCOBJ=   -6594.35999977701     
 iteration         -890 MCMCOBJ=   -6596.15018409798     
 iteration         -889 MCMCOBJ=   -6656.80869738412     
 iteration         -888 MCMCOBJ=   -6682.37366654966     
 iteration         -887 MCMCOBJ=   -6677.01223830267     
 iteration         -886 MCMCOBJ=   -6639.66328259680     
 iteration         -885 MCMCOBJ=   -6653.36130855841     
 iteration         -884 MCMCOBJ=   -6647.84168942173     
 iteration         -883 MCMCOBJ=   -6652.46996516605     
 iteration         -882 MCMCOBJ=   -6641.63952496535     
 iteration         -881 MCMCOBJ=   -6596.74662361702     
 iteration         -880 MCMCOBJ=   -6573.05764841538     
 iteration         -879 MCMCOBJ=   -6574.27821791834     
 iteration         -878 MCMCOBJ=   -6567.60772988516     
 iteration         -877 MCMCOBJ=   -6582.73336842766     
 iteration         -876 MCMCOBJ=   -6602.36486353729     
 iteration         -875 MCMCOBJ=   -6557.14537202979     
 iteration         -874 MCMCOBJ=   -6562.13119301022     
 iteration         -873 MCMCOBJ=   -6594.04517217747     
 iteration         -872 MCMCOBJ=   -6566.24797468375     
 iteration         -871 MCMCOBJ=   -6595.36016571560     
 iteration         -870 MCMCOBJ=   -6587.31509548484     
 iteration         -869 MCMCOBJ=   -6598.64487367943     
 iteration         -868 MCMCOBJ=   -6681.12393077172     
 iteration         -867 MCMCOBJ=   -6681.12391673666     
 iteration         -866 MCMCOBJ=   -6661.75620896909     
 iteration         -865 MCMCOBJ=   -6652.56077589621     
 iteration         -864 MCMCOBJ=   -6706.84187822891     
 iteration         -863 MCMCOBJ=   -6695.03365786394     
 iteration         -862 MCMCOBJ=   -6672.23109017718     
 iteration         -861 MCMCOBJ=   -6676.92426153935     
 iteration         -860 MCMCOBJ=   -6721.69621955656     
 iteration         -859 MCMCOBJ=   -6721.69620848323     
 iteration         -858 MCMCOBJ=   -6697.89898120725     
 iteration         -857 MCMCOBJ=   -6662.72843071080     
 iteration         -856 MCMCOBJ=   -6648.54394016490     
 iteration         -855 MCMCOBJ=   -6654.44881279730     
 iteration         -854 MCMCOBJ=   -6674.88633445924     
 iteration         -853 MCMCOBJ=   -6675.82661983236     
 iteration         -852 MCMCOBJ=   -6677.12042981242     
 iteration         -851 MCMCOBJ=   -6688.26723605117     
 iteration         -850 MCMCOBJ=   -6699.02834967678     
 iteration         -849 MCMCOBJ=   -6699.02835031023     
 iteration         -848 MCMCOBJ=   -6668.90797440116     
 iteration         -847 MCMCOBJ=   -6617.27843697250     
 iteration         -846 MCMCOBJ=   -6594.48737368168     
 iteration         -845 MCMCOBJ=   -6568.27644054078     
 iteration         -844 MCMCOBJ=   -6619.11249684760     
 iteration         -843 MCMCOBJ=   -6629.87829225089     
 iteration         -842 MCMCOBJ=   -6592.72423677351     
 iteration         -841 MCMCOBJ=   -6609.67694134025     
 iteration         -840 MCMCOBJ=   -6639.26004724416     
 iteration         -839 MCMCOBJ=   -6617.66559026990     
 iteration         -838 MCMCOBJ=   -6611.04775291140     
 iteration         -837 MCMCOBJ=   -6601.23249764446     
 iteration         -836 MCMCOBJ=   -6608.85428904647     
 iteration         -835 MCMCOBJ=   -6611.55515261638     
 iteration         -834 MCMCOBJ=   -6580.55449396061     
 iteration         -833 MCMCOBJ=   -6563.67967636229     
 iteration         -832 MCMCOBJ=   -6606.61857663382     
 iteration         -831 MCMCOBJ=   -6655.36626672821     
 iteration         -830 MCMCOBJ=   -6649.48613126528     
 iteration         -829 MCMCOBJ=   -6599.31897904235     
 iteration         -828 MCMCOBJ=   -6616.00219648440     
 iteration         -827 MCMCOBJ=   -6638.00553303286     
 iteration         -826 MCMCOBJ=   -6622.61082636023     
 iteration         -825 MCMCOBJ=   -6612.64607770623     
 iteration         -824 MCMCOBJ=   -6656.09404777846     
 iteration         -823 MCMCOBJ=   -6697.64069921209     
 iteration         -822 MCMCOBJ=   -6682.72109373269     
 iteration         -821 MCMCOBJ=   -6701.36063310931     
 iteration         -820 MCMCOBJ=   -6690.53938771562     
 iteration         -819 MCMCOBJ=   -6698.06006053228     
 iteration         -818 MCMCOBJ=   -6698.06006071949     
 iteration         -817 MCMCOBJ=   -6652.10617818727     
 iteration         -816 MCMCOBJ=   -6662.68830395722     
 iteration         -815 MCMCOBJ=   -6655.16406673436     
 iteration         -814 MCMCOBJ=   -6685.15454318341     
 iteration         -813 MCMCOBJ=   -6648.84928632305     
 iteration         -812 MCMCOBJ=   -6625.59050231785     
 iteration         -811 MCMCOBJ=   -6618.25462061288     
 iteration         -810 MCMCOBJ=   -6634.96114116127     
 iteration         -809 MCMCOBJ=   -6623.08239979705     
 iteration         -808 MCMCOBJ=   -6623.08239933466     
 iteration         -807 MCMCOBJ=   -6604.20560788116     
 iteration         -806 MCMCOBJ=   -6584.46841533473     
 iteration         -805 MCMCOBJ=   -6625.54930623278     
 iteration         -804 MCMCOBJ=   -6632.45309286678     
 iteration         -803 MCMCOBJ=   -6610.18852990170     
 iteration         -802 MCMCOBJ=   -6648.32150601993     
 iteration         -801 MCMCOBJ=   -6595.09922810583     
 iteration         -800 MCMCOBJ=   -6600.06818077969     
 iteration         -799 MCMCOBJ=   -6599.09034634010     
 iteration         -798 MCMCOBJ=   -6592.45682162253     
 iteration         -797 MCMCOBJ=   -6530.69950568350     
 iteration         -796 MCMCOBJ=   -6580.90458556319     
 iteration         -795 MCMCOBJ=   -6606.58039718229     
 iteration         -794 MCMCOBJ=   -6604.42971839159     
 iteration         -793 MCMCOBJ=   -6606.54759592853     
 iteration         -792 MCMCOBJ=   -6616.58844995796     
 iteration         -791 MCMCOBJ=   -6629.71853379162     
 iteration         -790 MCMCOBJ=   -6634.24957759051     
 iteration         -789 MCMCOBJ=   -6658.39529685320     
 iteration         -788 MCMCOBJ=   -6641.89271003974     
 iteration         -787 MCMCOBJ=   -6653.80064364824     
 iteration         -786 MCMCOBJ=   -6667.52861141904     
 iteration         -785 MCMCOBJ=   -6666.83522714722     
 iteration         -784 MCMCOBJ=   -6686.04197396965     
 iteration         -783 MCMCOBJ=   -6686.04197155516     
 iteration         -782 MCMCOBJ=   -6622.86229269224     
 iteration         -781 MCMCOBJ=   -6612.74242906175     
 iteration         -780 MCMCOBJ=   -6606.50398390124     
 iteration         -779 MCMCOBJ=   -6601.74609888890     
 iteration         -778 MCMCOBJ=   -6594.56707410475     
 iteration         -777 MCMCOBJ=   -6653.64820834935     
 iteration         -776 MCMCOBJ=   -6618.11995921952     
 iteration         -775 MCMCOBJ=   -6612.05093868924     
 iteration         -774 MCMCOBJ=   -6619.53445972866     
 iteration         -773 MCMCOBJ=   -6636.54202533481     
 iteration         -772 MCMCOBJ=   -6598.05691590354     
 iteration         -771 MCMCOBJ=   -6608.77503300570     
 iteration         -770 MCMCOBJ=   -6630.16913671424     
 iteration         -769 MCMCOBJ=   -6588.90727315451     
 iteration         -768 MCMCOBJ=   -6605.38949880152     
 iteration         -767 MCMCOBJ=   -6603.37387655185     
 iteration         -766 MCMCOBJ=   -6604.50979579857     
 iteration         -765 MCMCOBJ=   -6596.14324316866     
 iteration         -764 MCMCOBJ=   -6632.51488729160     
 iteration         -763 MCMCOBJ=   -6609.45466903065     
 iteration         -762 MCMCOBJ=   -6609.45464990955     
 iteration         -761 MCMCOBJ=   -6608.00240023111     
 iteration         -760 MCMCOBJ=   -6588.57680548640     
 iteration         -759 MCMCOBJ=   -6650.57177507445     
 iteration         -758 MCMCOBJ=   -6667.15993797554     
 iteration         -757 MCMCOBJ=   -6618.19503509169     
 iteration         -756 MCMCOBJ=   -6616.19045295825     
 iteration         -755 MCMCOBJ=   -6635.87958164627     
 iteration         -754 MCMCOBJ=   -6571.04165305544     
 iteration         -753 MCMCOBJ=   -6586.36770637594     
 iteration         -752 MCMCOBJ=   -6627.14340937439     
 iteration         -751 MCMCOBJ=   -6611.09596852375     
 iteration         -750 MCMCOBJ=   -6638.17271840619     
 iteration         -749 MCMCOBJ=   -6624.44162335843     
 iteration         -748 MCMCOBJ=   -6613.22957098299     
 iteration         -747 MCMCOBJ=   -6590.33153018293     
 iteration         -746 MCMCOBJ=   -6614.18751885709     
 iteration         -745 MCMCOBJ=   -6636.73978446193     
 iteration         -744 MCMCOBJ=   -6623.79542332950     
 iteration         -743 MCMCOBJ=   -6649.93821958568     
 iteration         -742 MCMCOBJ=   -6618.57409410530     
 iteration         -741 MCMCOBJ=   -6642.17869546055     
 iteration         -740 MCMCOBJ=   -6652.45743049494     
 iteration         -739 MCMCOBJ=   -6660.87199680822     
 iteration         -738 MCMCOBJ=   -6667.45871523516     
 iteration         -737 MCMCOBJ=   -6644.60436621917     
 iteration         -736 MCMCOBJ=   -6661.15754809028     
 iteration         -735 MCMCOBJ=   -6634.11153352259     
 iteration         -734 MCMCOBJ=   -6657.87491738857     
 iteration         -733 MCMCOBJ=   -6616.14945811673     
 iteration         -732 MCMCOBJ=   -6655.59701246984     
 iteration         -731 MCMCOBJ=   -6659.94212825893     
 iteration         -730 MCMCOBJ=   -6702.75013758118     
 iteration         -729 MCMCOBJ=   -6697.54301447481     
 iteration         -728 MCMCOBJ=   -6693.00538280579     
 iteration         -727 MCMCOBJ=   -6628.30991055155     
 iteration         -726 MCMCOBJ=   -6657.91797634419     
 iteration         -725 MCMCOBJ=   -6661.08060680944     
 iteration         -724 MCMCOBJ=   -6653.98295177322     
 iteration         -723 MCMCOBJ=   -6672.93495304500     
 iteration         -722 MCMCOBJ=   -6636.60513412831     
 iteration         -721 MCMCOBJ=   -6592.22414962900     
 iteration         -720 MCMCOBJ=   -6583.49355168909     
 iteration         -719 MCMCOBJ=   -6598.00252343438     
 iteration         -718 MCMCOBJ=   -6588.27128953298     
 iteration         -717 MCMCOBJ=   -6597.74781894133     
 iteration         -716 MCMCOBJ=   -6559.63554244538     
 iteration         -715 MCMCOBJ=   -6661.21897199250     
 iteration         -714 MCMCOBJ=   -6652.48439142367     
 iteration         -713 MCMCOBJ=   -6648.70745489660     
 iteration         -712 MCMCOBJ=   -6655.71575556455     
 iteration         -711 MCMCOBJ=   -6655.71575339371     
 iteration         -710 MCMCOBJ=   -6666.72837355452     
 iteration         -709 MCMCOBJ=   -6651.93528916034     
 iteration         -708 MCMCOBJ=   -6668.62955111645     
 iteration         -707 MCMCOBJ=   -6642.50063220498     
 iteration         -706 MCMCOBJ=   -6650.70004389307     
 iteration         -705 MCMCOBJ=   -6690.89854912410     
 iteration         -704 MCMCOBJ=   -6656.30156820438     
 iteration         -703 MCMCOBJ=   -6697.04216793015     
 iteration         -702 MCMCOBJ=   -6695.31145989927     
 iteration         -701 MCMCOBJ=   -6641.70702159813     
 iteration         -700 MCMCOBJ=   -6677.34400415753     
 iteration         -699 MCMCOBJ=   -6636.16160736314     
 iteration         -698 MCMCOBJ=   -6599.48410521646     
 iteration         -697 MCMCOBJ=   -6635.22604489518     
 iteration         -696 MCMCOBJ=   -6669.50972783265     
 iteration         -695 MCMCOBJ=   -6660.12237305745     
 iteration         -694 MCMCOBJ=   -6601.00351460976     
 iteration         -693 MCMCOBJ=   -6646.80114067688     
 iteration         -692 MCMCOBJ=   -6605.39103149751     
 iteration         -691 MCMCOBJ=   -6589.74364649864     
 iteration         -690 MCMCOBJ=   -6642.02377023793     
 iteration         -689 MCMCOBJ=   -6624.58079317585     
 iteration         -688 MCMCOBJ=   -6604.57343347105     
 iteration         -687 MCMCOBJ=   -6640.55401364704     
 iteration         -686 MCMCOBJ=   -6648.60147849461     
 iteration         -685 MCMCOBJ=   -6638.25022858313     
 iteration         -684 MCMCOBJ=   -6648.66920129031     
 iteration         -683 MCMCOBJ=   -6666.56813166577     
 iteration         -682 MCMCOBJ=   -6629.63204305393     
 iteration         -681 MCMCOBJ=   -6619.73097708927     
 iteration         -680 MCMCOBJ=   -6621.64395257605     
 iteration         -679 MCMCOBJ=   -6655.99044851124     
 iteration         -678 MCMCOBJ=   -6625.67474464535     
 iteration         -677 MCMCOBJ=   -6621.59418325609     
 iteration         -676 MCMCOBJ=   -6629.73434027900     
 iteration         -675 MCMCOBJ=   -6607.57264911785     
 iteration         -674 MCMCOBJ=   -6636.25667222409     
 iteration         -673 MCMCOBJ=   -6595.99304517226     
 iteration         -672 MCMCOBJ=   -6562.55818217444     
 iteration         -671 MCMCOBJ=   -6574.26825306967     
 iteration         -670 MCMCOBJ=   -6584.78487856517     
 iteration         -669 MCMCOBJ=   -6630.63660702509     
 iteration         -668 MCMCOBJ=   -6621.54824190549     
 iteration         -667 MCMCOBJ=   -6571.62031729368     
 iteration         -666 MCMCOBJ=   -6601.52814569995     
 iteration         -665 MCMCOBJ=   -6641.22034801851     
 iteration         -664 MCMCOBJ=   -6702.00259526967     
 iteration         -663 MCMCOBJ=   -6675.77203721277     
 iteration         -662 MCMCOBJ=   -6655.59374236157     
 iteration         -661 MCMCOBJ=   -6675.96510621640     
 iteration         -660 MCMCOBJ=   -6627.71150644092     
 iteration         -659 MCMCOBJ=   -6590.94535057826     
 iteration         -658 MCMCOBJ=   -6582.64023618045     
 iteration         -657 MCMCOBJ=   -6597.80133784998     
 iteration         -656 MCMCOBJ=   -6620.28449037419     
 iteration         -655 MCMCOBJ=   -6616.93365558801     
 iteration         -654 MCMCOBJ=   -6628.30561462050     
 iteration         -653 MCMCOBJ=   -6635.41214689052     
 iteration         -652 MCMCOBJ=   -6591.41103110359     
 iteration         -651 MCMCOBJ=   -6587.13532372308     
 iteration         -650 MCMCOBJ=   -6673.23479093593     
 iteration         -649 MCMCOBJ=   -6664.14286894794     
 iteration         -648 MCMCOBJ=   -6650.22463209489     
 iteration         -647 MCMCOBJ=   -6654.17430109754     
 iteration         -646 MCMCOBJ=   -6649.22042675423     
 iteration         -645 MCMCOBJ=   -6591.14863982218     
 iteration         -644 MCMCOBJ=   -6612.86013153330     
 iteration         -643 MCMCOBJ=   -6626.21411174100     
 iteration         -642 MCMCOBJ=   -6647.80379484586     
 iteration         -641 MCMCOBJ=   -6613.88468519601     
 iteration         -640 MCMCOBJ=   -6614.11006909192     
 iteration         -639 MCMCOBJ=   -6588.13573665241     
 iteration         -638 MCMCOBJ=   -6556.70642950106     
 iteration         -637 MCMCOBJ=   -6582.91555504110     
 iteration         -636 MCMCOBJ=   -6646.09310878863     
 iteration         -635 MCMCOBJ=   -6651.88280894071     
 iteration         -634 MCMCOBJ=   -6628.30519646869     
 iteration         -633 MCMCOBJ=   -6628.30519805715     
 iteration         -632 MCMCOBJ=   -6598.52894828236     
 iteration         -631 MCMCOBJ=   -6578.32739236810     
 iteration         -630 MCMCOBJ=   -6597.08092027432     
 iteration         -629 MCMCOBJ=   -6586.36395677849     
 iteration         -628 MCMCOBJ=   -6586.36395647912     
 iteration         -627 MCMCOBJ=   -6636.12983681949     
 iteration         -626 MCMCOBJ=   -6644.98814365597     
 iteration         -625 MCMCOBJ=   -6654.75182854835     
 iteration         -624 MCMCOBJ=   -6654.38552509479     
 iteration         -623 MCMCOBJ=   -6588.77108062420     
 iteration         -622 MCMCOBJ=   -6611.80706825072     
 iteration         -621 MCMCOBJ=   -6621.35881441804     
 iteration         -620 MCMCOBJ=   -6657.50930890228     
 iteration         -619 MCMCOBJ=   -6594.35114668609     
 iteration         -618 MCMCOBJ=   -6588.62126382928     
 iteration         -617 MCMCOBJ=   -6555.09926240375     
 iteration         -616 MCMCOBJ=   -6582.81867413554     
 iteration         -615 MCMCOBJ=   -6584.63586501997     
 iteration         -614 MCMCOBJ=   -6599.97946791708     
 iteration         -613 MCMCOBJ=   -6630.08976023536     
 iteration         -612 MCMCOBJ=   -6672.16494506205     
 iteration         -611 MCMCOBJ=   -6651.80943311691     
 iteration         -610 MCMCOBJ=   -6645.77207338157     
 iteration         -609 MCMCOBJ=   -6653.64754371949     
 iteration         -608 MCMCOBJ=   -6644.12626841247     
 iteration         -607 MCMCOBJ=   -6614.21736007522     
 iteration         -606 MCMCOBJ=   -6657.47894522608     
 iteration         -605 MCMCOBJ=   -6579.64920908445     
 iteration         -604 MCMCOBJ=   -6598.65719290486     
 iteration         -603 MCMCOBJ=   -6591.81636670066     
 iteration         -602 MCMCOBJ=   -6621.98253187489     
 iteration         -601 MCMCOBJ=   -6625.68513392361     
 iteration         -600 MCMCOBJ=   -6595.05568264610     
 iteration         -599 MCMCOBJ=   -6594.55638388844     
 iteration         -598 MCMCOBJ=   -6625.18015266074     
 iteration         -597 MCMCOBJ=   -6630.09559934506     
 iteration         -596 MCMCOBJ=   -6663.20554415680     
 iteration         -595 MCMCOBJ=   -6668.93408051299     
 iteration         -594 MCMCOBJ=   -6692.88579122068     
 iteration         -593 MCMCOBJ=   -6692.88579193939     
 iteration         -592 MCMCOBJ=   -6692.71719610102     
 iteration         -591 MCMCOBJ=   -6655.34273237361     
 iteration         -590 MCMCOBJ=   -6625.98677974615     
 iteration         -589 MCMCOBJ=   -6633.71295731981     
 iteration         -588 MCMCOBJ=   -6635.00525682237     
 iteration         -587 MCMCOBJ=   -6637.43766122751     
 iteration         -586 MCMCOBJ=   -6623.70466803943     
 iteration         -585 MCMCOBJ=   -6616.54497599172     
 iteration         -584 MCMCOBJ=   -6636.67190604238     
 iteration         -583 MCMCOBJ=   -6618.34274816100     
 iteration         -582 MCMCOBJ=   -6628.11786361389     
 iteration         -581 MCMCOBJ=   -6606.71706366485     
 iteration         -580 MCMCOBJ=   -6600.18442782005     
 iteration         -579 MCMCOBJ=   -6581.86990841043     
 iteration         -578 MCMCOBJ=   -6578.55684983118     
 iteration         -577 MCMCOBJ=   -6590.99300200489     
 iteration         -576 MCMCOBJ=   -6646.81951414050     
 iteration         -575 MCMCOBJ=   -6623.59305856745     
 iteration         -574 MCMCOBJ=   -6581.24622398944     
 iteration         -573 MCMCOBJ=   -6604.11521180710     
 iteration         -572 MCMCOBJ=   -6611.29639134007     
 iteration         -571 MCMCOBJ=   -6618.59854572259     
 iteration         -570 MCMCOBJ=   -6641.72895996088     
 iteration         -569 MCMCOBJ=   -6632.65622848437     
 iteration         -568 MCMCOBJ=   -6643.72276451974     
 iteration         -567 MCMCOBJ=   -6683.20432442021     
 iteration         -566 MCMCOBJ=   -6657.74752736219     
 iteration         -565 MCMCOBJ=   -6650.21443276352     
 iteration         -564 MCMCOBJ=   -6621.45560055561     
 iteration         -563 MCMCOBJ=   -6625.43611388826     
 iteration         -562 MCMCOBJ=   -6658.06998376660     
 iteration         -561 MCMCOBJ=   -6638.30950363614     
 iteration         -560 MCMCOBJ=   -6628.68771878116     
 iteration         -559 MCMCOBJ=   -6644.34050196336     
 iteration         -558 MCMCOBJ=   -6614.30479967706     
 iteration         -557 MCMCOBJ=   -6616.07654174048     
 iteration         -556 MCMCOBJ=   -6663.75296074533     
 iteration         -555 MCMCOBJ=   -6618.66039795154     
 iteration         -554 MCMCOBJ=   -6654.22779808924     
 iteration         -553 MCMCOBJ=   -6664.94488811463     
 iteration         -552 MCMCOBJ=   -6706.42071772872     
 iteration         -551 MCMCOBJ=   -6702.49256991839     
 iteration         -550 MCMCOBJ=   -6702.49254413274     
 iteration         -549 MCMCOBJ=   -6666.83861475253     
 iteration         -548 MCMCOBJ=   -6673.07674550829     
 iteration         -547 MCMCOBJ=   -6665.11140435724     
 iteration         -546 MCMCOBJ=   -6623.28763888368     
 iteration         -545 MCMCOBJ=   -6618.25330631177     
 iteration         -544 MCMCOBJ=   -6555.38755544188     
 iteration         -543 MCMCOBJ=   -6557.90520238722     
 iteration         -542 MCMCOBJ=   -6654.05842372112     
 iteration         -541 MCMCOBJ=   -6610.30894225968     
 iteration         -540 MCMCOBJ=   -6657.31123620264     
 iteration         -539 MCMCOBJ=   -6622.39468161325     
 iteration         -538 MCMCOBJ=   -6624.70851934888     
 iteration         -537 MCMCOBJ=   -6638.62061709560     
 iteration         -536 MCMCOBJ=   -6638.62061686166     
 iteration         -535 MCMCOBJ=   -6685.32855026406     
 iteration         -534 MCMCOBJ=   -6706.88101801897     
 iteration         -533 MCMCOBJ=   -6663.78466644263     
 iteration         -532 MCMCOBJ=   -6661.98200120679     
 iteration         -531 MCMCOBJ=   -6637.89382430511     
 iteration         -530 MCMCOBJ=   -6670.85528137521     
 iteration         -529 MCMCOBJ=   -6642.72679105580     
 iteration         -528 MCMCOBJ=   -6653.50260509240     
 iteration         -527 MCMCOBJ=   -6642.00647039740     
 iteration         -526 MCMCOBJ=   -6642.00646612106     
 iteration         -525 MCMCOBJ=   -6594.36540839193     
 iteration         -524 MCMCOBJ=   -6617.28666220605     
 iteration         -523 MCMCOBJ=   -6662.58933884084     
 iteration         -522 MCMCOBJ=   -6644.68204112756     
 iteration         -521 MCMCOBJ=   -6662.38820647753     
 iteration         -520 MCMCOBJ=   -6622.06621598820     
 iteration         -519 MCMCOBJ=   -6662.96489906192     
 iteration         -518 MCMCOBJ=   -6621.06441338045     
 iteration         -517 MCMCOBJ=   -6658.79829886555     
 iteration         -516 MCMCOBJ=   -6617.50139315033     
 iteration         -515 MCMCOBJ=   -6613.68965722594     
 iteration         -514 MCMCOBJ=   -6631.84214694963     
 iteration         -513 MCMCOBJ=   -6628.38288403660     
 iteration         -512 MCMCOBJ=   -6636.28044846050     
 iteration         -511 MCMCOBJ=   -6662.02467164342     
 iteration         -510 MCMCOBJ=   -6629.36114783819     
 iteration         -509 MCMCOBJ=   -6571.04873869682     
 iteration         -508 MCMCOBJ=   -6560.05960041984     
 iteration         -507 MCMCOBJ=   -6589.08192379276     
 iteration         -506 MCMCOBJ=   -6613.58208942682     
 iteration         -505 MCMCOBJ=   -6636.90949074420     
 iteration         -504 MCMCOBJ=   -6655.54189439869     
 iteration         -503 MCMCOBJ=   -6657.50979577786     
 iteration         -502 MCMCOBJ=   -6674.64409577366     
 iteration         -501 MCMCOBJ=   -6580.23738889586     
 iteration         -500 MCMCOBJ=   -6609.50170395773     
 iteration         -499 MCMCOBJ=   -6606.28919845471     
 iteration         -498 MCMCOBJ=   -6592.94172899302     
 iteration         -497 MCMCOBJ=   -6592.94172744465     
 iteration         -496 MCMCOBJ=   -6665.77645146413     
 iteration         -495 MCMCOBJ=   -6614.73763282460     
 iteration         -494 MCMCOBJ=   -6638.97879355775     
 iteration         -493 MCMCOBJ=   -6564.98939116404     
 iteration         -492 MCMCOBJ=   -6632.98852747068     
 iteration         -491 MCMCOBJ=   -6648.64287053199     
 iteration         -490 MCMCOBJ=   -6614.20755443501     
 iteration         -489 MCMCOBJ=   -6607.79674569507     
 iteration         -488 MCMCOBJ=   -6618.32072490217     
 iteration         -487 MCMCOBJ=   -6635.70212192374     
 iteration         -486 MCMCOBJ=   -6594.37357490186     
 iteration         -485 MCMCOBJ=   -6595.92870259350     
 iteration         -484 MCMCOBJ=   -6615.79150758766     
 iteration         -483 MCMCOBJ=   -6587.25348774773     
 iteration         -482 MCMCOBJ=   -6619.47754430121     
 iteration         -481 MCMCOBJ=   -6573.83515420231     
 iteration         -480 MCMCOBJ=   -6591.06726058613     
 iteration         -479 MCMCOBJ=   -6559.74384277655     
 iteration         -478 MCMCOBJ=   -6624.80654126376     
 iteration         -477 MCMCOBJ=   -6624.80654287994     
 iteration         -476 MCMCOBJ=   -6612.00258441534     
 iteration         -475 MCMCOBJ=   -6671.03650079451     
 iteration         -474 MCMCOBJ=   -6653.20657407221     
 iteration         -473 MCMCOBJ=   -6653.20657261695     
 iteration         -472 MCMCOBJ=   -6661.90148461005     
 iteration         -471 MCMCOBJ=   -6675.35180847177     
 iteration         -470 MCMCOBJ=   -6650.27217974410     
 iteration         -469 MCMCOBJ=   -6651.79321462431     
 iteration         -468 MCMCOBJ=   -6604.04026566377     
 iteration         -467 MCMCOBJ=   -6608.35322159762     
 iteration         -466 MCMCOBJ=   -6601.63822786970     
 iteration         -465 MCMCOBJ=   -6585.23567793422     
 iteration         -464 MCMCOBJ=   -6637.07726399430     
 iteration         -463 MCMCOBJ=   -6626.39812539506     
 iteration         -462 MCMCOBJ=   -6651.04038706199     
 iteration         -461 MCMCOBJ=   -6659.66553578788     
 iteration         -460 MCMCOBJ=   -6685.54570001728     
 iteration         -459 MCMCOBJ=   -6670.51018028864     
 iteration         -458 MCMCOBJ=   -6664.96338016134     
 iteration         -457 MCMCOBJ=   -6649.67853577676     
 iteration         -456 MCMCOBJ=   -6641.93375039169     
 iteration         -455 MCMCOBJ=   -6651.86101818311     
 iteration         -454 MCMCOBJ=   -6682.76758644342     
 iteration         -453 MCMCOBJ=   -6707.42185603342     
 iteration         -452 MCMCOBJ=   -6707.42185578496     
 iteration         -451 MCMCOBJ=   -6689.54950429284     
 iteration         -450 MCMCOBJ=   -6700.84291420560     
 iteration         -449 MCMCOBJ=   -6644.60552352393     
 iteration         -448 MCMCOBJ=   -6668.72987210127     
 iteration         -447 MCMCOBJ=   -6675.45223217607     
 iteration         -446 MCMCOBJ=   -6658.16934246667     
 iteration         -445 MCMCOBJ=   -6682.78167199429     
 iteration         -444 MCMCOBJ=   -6650.31469537493     
 iteration         -443 MCMCOBJ=   -6652.85082821690     
 iteration         -442 MCMCOBJ=   -6597.51567456384     
 iteration         -441 MCMCOBJ=   -6589.19292112291     
 iteration         -440 MCMCOBJ=   -6580.24365953254     
 iteration         -439 MCMCOBJ=   -6556.28981632553     
 iteration         -438 MCMCOBJ=   -6649.87128289816     
 iteration         -437 MCMCOBJ=   -6657.44266477534     
 iteration         -436 MCMCOBJ=   -6598.43055235927     
 iteration         -435 MCMCOBJ=   -6632.80354685109     
 iteration         -434 MCMCOBJ=   -6642.08968410242     
 iteration         -433 MCMCOBJ=   -6622.70468487693     
 iteration         -432 MCMCOBJ=   -6576.09550529757     
 iteration         -431 MCMCOBJ=   -6599.00869131495     
 iteration         -430 MCMCOBJ=   -6630.49460430438     
 iteration         -429 MCMCOBJ=   -6621.36550911664     
 iteration         -428 MCMCOBJ=   -6623.63804761720     
 iteration         -427 MCMCOBJ=   -6636.87844208824     
 iteration         -426 MCMCOBJ=   -6628.36358938226     
 iteration         -425 MCMCOBJ=   -6651.09086936770     
 iteration         -424 MCMCOBJ=   -6649.90201045146     
 iteration         -423 MCMCOBJ=   -6688.67277445527     
 iteration         -422 MCMCOBJ=   -6688.67277481463     
 iteration         -421 MCMCOBJ=   -6636.33448655503     
 iteration         -420 MCMCOBJ=   -6628.35937547992     
 iteration         -419 MCMCOBJ=   -6559.34390080871     
 iteration         -418 MCMCOBJ=   -6620.70620487390     
 iteration         -417 MCMCOBJ=   -6579.73537270162     
 iteration         -416 MCMCOBJ=   -6595.21132467032     
 iteration         -415 MCMCOBJ=   -6580.29944962750     
 iteration         -414 MCMCOBJ=   -6584.54373766116     
 iteration         -413 MCMCOBJ=   -6562.56712875280     
 iteration         -412 MCMCOBJ=   -6569.35305796533     
 iteration         -411 MCMCOBJ=   -6609.57203246477     
 iteration         -410 MCMCOBJ=   -6693.28813306214     
 iteration         -409 MCMCOBJ=   -6614.76276445246     
 iteration         -408 MCMCOBJ=   -6604.68699068651     
 iteration         -407 MCMCOBJ=   -6658.43959589572     
 iteration         -406 MCMCOBJ=   -6692.50094325690     
 iteration         -405 MCMCOBJ=   -6654.80275598882     
 iteration         -404 MCMCOBJ=   -6666.40489890528     
 iteration         -403 MCMCOBJ=   -6668.85329519330     
 iteration         -402 MCMCOBJ=   -6670.29059736460     
 iteration         -401 MCMCOBJ=   -6640.25998269179     
 iteration         -400 MCMCOBJ=   -6637.05565911864     
 iteration         -399 MCMCOBJ=   -6592.44714039467     
 iteration         -398 MCMCOBJ=   -6609.22030322214     
 iteration         -397 MCMCOBJ=   -6621.58564327629     
 iteration         -396 MCMCOBJ=   -6680.78957459147     
 iteration         -395 MCMCOBJ=   -6674.68890990326     
 iteration         -394 MCMCOBJ=   -6604.77005061907     
 iteration         -393 MCMCOBJ=   -6598.61799915114     
 iteration         -392 MCMCOBJ=   -6604.09640023449     
 iteration         -391 MCMCOBJ=   -6630.96702037600     
 iteration         -390 MCMCOBJ=   -6624.03717539183     
 iteration         -389 MCMCOBJ=   -6634.76575493311     
 iteration         -388 MCMCOBJ=   -6660.30158909616     
 iteration         -387 MCMCOBJ=   -6635.25390635791     
 iteration         -386 MCMCOBJ=   -6636.97969304565     
 iteration         -385 MCMCOBJ=   -6604.71720829931     
 iteration         -384 MCMCOBJ=   -6614.57277424556     
 iteration         -383 MCMCOBJ=   -6602.04617841155     
 iteration         -382 MCMCOBJ=   -6606.83384453138     
 iteration         -381 MCMCOBJ=   -6629.75670464689     
 iteration         -380 MCMCOBJ=   -6686.07343519840     
 iteration         -379 MCMCOBJ=   -6647.26919438920     
 iteration         -378 MCMCOBJ=   -6648.90038456035     
 iteration         -377 MCMCOBJ=   -6669.34901913177     
 iteration         -376 MCMCOBJ=   -6691.19617029076     
 iteration         -375 MCMCOBJ=   -6686.08191908258     
 iteration         -374 MCMCOBJ=   -6635.27547423607     
 iteration         -373 MCMCOBJ=   -6607.78331605238     
 iteration         -372 MCMCOBJ=   -6616.19770095585     
 iteration         -371 MCMCOBJ=   -6630.01059417221     
 iteration         -370 MCMCOBJ=   -6576.45191491925     
 iteration         -369 MCMCOBJ=   -6598.35969345357     
 iteration         -368 MCMCOBJ=   -6620.80512768150     
 iteration         -367 MCMCOBJ=   -6584.33951058167     
 iteration         -366 MCMCOBJ=   -6589.07819638393     
 iteration         -365 MCMCOBJ=   -6600.84222838711     
 iteration         -364 MCMCOBJ=   -6605.48284063970     
 iteration         -363 MCMCOBJ=   -6580.32566166730     
 iteration         -362 MCMCOBJ=   -6583.45467718273     
 iteration         -361 MCMCOBJ=   -6615.67365277458     
 iteration         -360 MCMCOBJ=   -6604.30390272740     
 iteration         -359 MCMCOBJ=   -6585.83709962526     
 iteration         -358 MCMCOBJ=   -6580.90726878880     
 iteration         -357 MCMCOBJ=   -6590.98553199445     
 iteration         -356 MCMCOBJ=   -6569.93663239169     
 iteration         -355 MCMCOBJ=   -6618.17583349219     
 iteration         -354 MCMCOBJ=   -6636.74860097841     
 iteration         -353 MCMCOBJ=   -6613.50888387892     
 iteration         -352 MCMCOBJ=   -6610.11049512385     
 iteration         -351 MCMCOBJ=   -6629.53125498306     
 iteration         -350 MCMCOBJ=   -6590.40045160226     
 iteration         -349 MCMCOBJ=   -6580.50493959233     
 iteration         -348 MCMCOBJ=   -6583.76911192299     
 iteration         -347 MCMCOBJ=   -6621.19343475627     
 iteration         -346 MCMCOBJ=   -6617.79823581003     
 iteration         -345 MCMCOBJ=   -6581.49264087924     
 iteration         -344 MCMCOBJ=   -6577.44077366055     
 iteration         -343 MCMCOBJ=   -6574.36542228086     
 iteration         -342 MCMCOBJ=   -6583.88504396629     
 iteration         -341 MCMCOBJ=   -6558.79633856580     
 iteration         -340 MCMCOBJ=   -6560.22986982864     
 iteration         -339 MCMCOBJ=   -6537.91860009591     
 iteration         -338 MCMCOBJ=   -6545.36292001602     
 iteration         -337 MCMCOBJ=   -6555.28773077451     
 iteration         -336 MCMCOBJ=   -6591.08688840367     
 iteration         -335 MCMCOBJ=   -6620.86998156752     
 iteration         -334 MCMCOBJ=   -6625.57508719032     
 iteration         -333 MCMCOBJ=   -6684.35118844807     
 iteration         -332 MCMCOBJ=   -6690.18818986867     
 iteration         -331 MCMCOBJ=   -6645.52914659618     
 iteration         -330 MCMCOBJ=   -6677.45716272523     
 iteration         -329 MCMCOBJ=   -6658.75244696963     
 iteration         -328 MCMCOBJ=   -6627.95714896627     
 iteration         -327 MCMCOBJ=   -6626.23540817805     
 iteration         -326 MCMCOBJ=   -6584.72573150484     
 iteration         -325 MCMCOBJ=   -6630.53409717453     
 iteration         -324 MCMCOBJ=   -6673.74108419545     
 iteration         -323 MCMCOBJ=   -6648.94174361978     
 iteration         -322 MCMCOBJ=   -6646.96351991683     
 iteration         -321 MCMCOBJ=   -6595.35553565464     
 iteration         -320 MCMCOBJ=   -6587.32640077277     
 iteration         -319 MCMCOBJ=   -6672.71669251673     
 iteration         -318 MCMCOBJ=   -6653.93351216887     
 iteration         -317 MCMCOBJ=   -6620.28390707158     
 iteration         -316 MCMCOBJ=   -6623.61315998788     
 iteration         -315 MCMCOBJ=   -6618.36116669747     
 iteration         -314 MCMCOBJ=   -6671.17703596345     
 iteration         -313 MCMCOBJ=   -6621.19638356033     
 iteration         -312 MCMCOBJ=   -6563.67934621052     
 iteration         -311 MCMCOBJ=   -6575.26731821242     
 iteration         -310 MCMCOBJ=   -6623.43993430572     
 iteration         -309 MCMCOBJ=   -6641.38629318557     
 iteration         -308 MCMCOBJ=   -6626.31149202956     
 iteration         -307 MCMCOBJ=   -6678.95352040189     
 iteration         -306 MCMCOBJ=   -6625.97294604014     
 iteration         -305 MCMCOBJ=   -6650.09837788969     
 iteration         -304 MCMCOBJ=   -6657.57820458205     
 iteration         -303 MCMCOBJ=   -6657.57820377205     
 iteration         -302 MCMCOBJ=   -6658.01611508884     
 iteration         -301 MCMCOBJ=   -6647.29448945103     
 iteration         -300 MCMCOBJ=   -6628.01322406634     
 iteration         -299 MCMCOBJ=   -6617.02012978377     
 iteration         -298 MCMCOBJ=   -6657.68936250927     
 iteration         -297 MCMCOBJ=   -6603.73704873840     
 iteration         -296 MCMCOBJ=   -6618.57738850383     
 iteration         -295 MCMCOBJ=   -6607.33366941186     
 iteration         -294 MCMCOBJ=   -6621.27367018986     
 iteration         -293 MCMCOBJ=   -6619.74383097036     
 iteration         -292 MCMCOBJ=   -6621.45202861153     
 iteration         -291 MCMCOBJ=   -6596.08785658302     
 iteration         -290 MCMCOBJ=   -6596.08785820088     
 iteration         -289 MCMCOBJ=   -6583.30445912963     
 iteration         -288 MCMCOBJ=   -6634.30543515880     
 iteration         -287 MCMCOBJ=   -6671.73893192511     
 iteration         -286 MCMCOBJ=   -6679.08992523888     
 iteration         -285 MCMCOBJ=   -6661.39048748953     
 iteration         -284 MCMCOBJ=   -6660.74540154916     
 iteration         -283 MCMCOBJ=   -6673.36127252488     
 iteration         -282 MCMCOBJ=   -6708.54092319971     
 iteration         -281 MCMCOBJ=   -6710.17953124403     
 iteration         -280 MCMCOBJ=   -6710.91394204597     
 iteration         -279 MCMCOBJ=   -6677.80668384258     
 iteration         -278 MCMCOBJ=   -6679.48822777182     
 iteration         -277 MCMCOBJ=   -6660.58172497764     
 iteration         -276 MCMCOBJ=   -6699.88600085205     
 iteration         -275 MCMCOBJ=   -6678.33620854095     
 iteration         -274 MCMCOBJ=   -6666.51937989158     
 iteration         -273 MCMCOBJ=   -6622.39146472914     
 iteration         -272 MCMCOBJ=   -6649.93956854152     
 iteration         -271 MCMCOBJ=   -6650.40578995063     
 iteration         -270 MCMCOBJ=   -6651.02831425122     
 iteration         -269 MCMCOBJ=   -6692.92315208782     
 iteration         -268 MCMCOBJ=   -6625.12398653462     
 iteration         -267 MCMCOBJ=   -6668.48854202278     
 iteration         -266 MCMCOBJ=   -6626.71713231589     
 iteration         -265 MCMCOBJ=   -6645.91539272572     
 iteration         -264 MCMCOBJ=   -6636.24012484712     
 iteration         -263 MCMCOBJ=   -6644.20724254839     
 iteration         -262 MCMCOBJ=   -6644.86039711844     
 iteration         -261 MCMCOBJ=   -6605.33475387281     
 iteration         -260 MCMCOBJ=   -6570.41666007554     
 iteration         -259 MCMCOBJ=   -6607.79446645540     
 iteration         -258 MCMCOBJ=   -6601.57969295255     
 iteration         -257 MCMCOBJ=   -6580.37542610479     
 iteration         -256 MCMCOBJ=   -6619.67090277718     
 iteration         -255 MCMCOBJ=   -6659.13976427186     
 iteration         -254 MCMCOBJ=   -6612.85780067680     
 iteration         -253 MCMCOBJ=   -6608.24591043253     
 iteration         -252 MCMCOBJ=   -6641.53896375631     
 iteration         -251 MCMCOBJ=   -6664.90968148174     
 iteration         -250 MCMCOBJ=   -6649.25606982796     
 iteration         -249 MCMCOBJ=   -6593.06692824136     
 iteration         -248 MCMCOBJ=   -6618.26944808075     
 iteration         -247 MCMCOBJ=   -6662.52043875330     
 iteration         -246 MCMCOBJ=   -6651.99006292787     
 iteration         -245 MCMCOBJ=   -6667.48615363671     
 iteration         -244 MCMCOBJ=   -6682.78812809481     
 iteration         -243 MCMCOBJ=   -6637.55486510654     
 iteration         -242 MCMCOBJ=   -6622.62263204709     
 iteration         -241 MCMCOBJ=   -6631.23053935019     
 iteration         -240 MCMCOBJ=   -6606.28528154398     
 iteration         -239 MCMCOBJ=   -6633.39642591257     
 iteration         -238 MCMCOBJ=   -6592.37227599725     
 iteration         -237 MCMCOBJ=   -6657.52615477726     
 iteration         -236 MCMCOBJ=   -6704.03145214031     
 iteration         -235 MCMCOBJ=   -6679.72382030356     
 iteration         -234 MCMCOBJ=   -6600.91507510532     
 iteration         -233 MCMCOBJ=   -6624.43336222928     
 iteration         -232 MCMCOBJ=   -6607.01194784400     
 iteration         -231 MCMCOBJ=   -6678.19107397442     
 iteration         -230 MCMCOBJ=   -6668.07581051091     
 iteration         -229 MCMCOBJ=   -6698.21234065084     
 iteration         -228 MCMCOBJ=   -6699.14750579056     
 iteration         -227 MCMCOBJ=   -6643.01683140760     
 iteration         -226 MCMCOBJ=   -6567.00362225627     
 iteration         -225 MCMCOBJ=   -6590.30993844997     
 iteration         -224 MCMCOBJ=   -6549.97769586493     
 iteration         -223 MCMCOBJ=   -6646.89904791225     
 iteration         -222 MCMCOBJ=   -6646.89904912713     
 iteration         -221 MCMCOBJ=   -6638.73928012436     
 iteration         -220 MCMCOBJ=   -6688.51005411677     
 iteration         -219 MCMCOBJ=   -6610.01752315867     
 iteration         -218 MCMCOBJ=   -6545.66689550215     
 iteration         -217 MCMCOBJ=   -6575.22821607183     
 iteration         -216 MCMCOBJ=   -6553.95003369405     
 iteration         -215 MCMCOBJ=   -6575.07862414376     
 iteration         -214 MCMCOBJ=   -6628.42894077022     
 iteration         -213 MCMCOBJ=   -6628.26133163888     
 iteration         -212 MCMCOBJ=   -6611.12587355907     
 iteration         -211 MCMCOBJ=   -6589.89228039624     
 iteration         -210 MCMCOBJ=   -6564.86004203982     
 iteration         -209 MCMCOBJ=   -6649.04521925207     
 iteration         -208 MCMCOBJ=   -6651.18044028408     
 iteration         -207 MCMCOBJ=   -6641.73126704643     
 iteration         -206 MCMCOBJ=   -6570.76865784874     
 iteration         -205 MCMCOBJ=   -6617.56613801295     
 iteration         -204 MCMCOBJ=   -6667.24823568642     
 iteration         -203 MCMCOBJ=   -6617.44020909792     
 iteration         -202 MCMCOBJ=   -6657.43752208288     
 iteration         -201 MCMCOBJ=   -6700.74160795019     
 iteration         -200 MCMCOBJ=   -6654.23430339326     
 iteration         -199 MCMCOBJ=   -6613.31878427879     
 iteration         -198 MCMCOBJ=   -6621.88484446932     
 iteration         -197 MCMCOBJ=   -6611.99377026504     
 iteration         -196 MCMCOBJ=   -6617.84869864977     
 iteration         -195 MCMCOBJ=   -6599.68573309678     
 iteration         -194 MCMCOBJ=   -6613.91021421223     
 iteration         -193 MCMCOBJ=   -6632.05055314042     
 iteration         -192 MCMCOBJ=   -6608.44230605428     
 iteration         -191 MCMCOBJ=   -6635.20717082074     
 iteration         -190 MCMCOBJ=   -6596.00821206723     
 iteration         -189 MCMCOBJ=   -6654.69085002311     
 iteration         -188 MCMCOBJ=   -6599.93517819145     
 iteration         -187 MCMCOBJ=   -6626.83264023629     
 iteration         -186 MCMCOBJ=   -6611.58330823413     
 iteration         -185 MCMCOBJ=   -6599.39094105486     
 iteration         -184 MCMCOBJ=   -6591.35389333247     
 iteration         -183 MCMCOBJ=   -6644.53633871333     
 iteration         -182 MCMCOBJ=   -6653.52568661873     
 iteration         -181 MCMCOBJ=   -6668.63200361040     
 iteration         -180 MCMCOBJ=   -6656.70365361916     
 iteration         -179 MCMCOBJ=   -6641.19746951421     
 iteration         -178 MCMCOBJ=   -6654.31109534700     
 iteration         -177 MCMCOBJ=   -6650.21849602389     
 iteration         -176 MCMCOBJ=   -6677.56736846052     
 iteration         -175 MCMCOBJ=   -6679.33249356205     
 iteration         -174 MCMCOBJ=   -6612.00215758888     
 iteration         -173 MCMCOBJ=   -6596.41797504519     
 iteration         -172 MCMCOBJ=   -6619.10915540600     
 iteration         -171 MCMCOBJ=   -6578.83248137705     
 iteration         -170 MCMCOBJ=   -6611.72948516590     
 iteration         -169 MCMCOBJ=   -6674.99647945755     
 iteration         -168 MCMCOBJ=   -6641.21709478889     
 iteration         -167 MCMCOBJ=   -6639.86014152235     
 iteration         -166 MCMCOBJ=   -6639.86014146504     
 iteration         -165 MCMCOBJ=   -6624.94780969233     
 iteration         -164 MCMCOBJ=   -6637.79587686572     
 iteration         -163 MCMCOBJ=   -6622.02714962216     
 iteration         -162 MCMCOBJ=   -6592.67924893013     
 iteration         -161 MCMCOBJ=   -6600.11420397244     
 iteration         -160 MCMCOBJ=   -6629.59033509017     
 iteration         -159 MCMCOBJ=   -6655.79499829380     
 iteration         -158 MCMCOBJ=   -6592.14856748019     
 iteration         -157 MCMCOBJ=   -6647.35221102031     
 iteration         -156 MCMCOBJ=   -6652.57237263426     
 iteration         -155 MCMCOBJ=   -6657.33945633265     
 iteration         -154 MCMCOBJ=   -6606.75530237011     
 iteration         -153 MCMCOBJ=   -6604.86462211244     
 iteration         -152 MCMCOBJ=   -6621.07292909493     
 iteration         -151 MCMCOBJ=   -6629.45254596059     
 iteration         -150 MCMCOBJ=   -6710.26962011237     
 iteration         -149 MCMCOBJ=   -6676.77062824006     
 iteration         -148 MCMCOBJ=   -6649.17676061931     
 iteration         -147 MCMCOBJ=   -6654.42809141735     
 iteration         -146 MCMCOBJ=   -6635.04095619321     
 iteration         -145 MCMCOBJ=   -6670.72910550179     
 iteration         -144 MCMCOBJ=   -6655.82632309403     
 iteration         -143 MCMCOBJ=   -6646.42900516675     
 iteration         -142 MCMCOBJ=   -6638.83877081598     
 iteration         -141 MCMCOBJ=   -6628.46373771662     
 iteration         -140 MCMCOBJ=   -6619.61925297920     
 iteration         -139 MCMCOBJ=   -6572.34230463725     
 iteration         -138 MCMCOBJ=   -6634.81779184281     
 iteration         -137 MCMCOBJ=   -6632.79611008759     
 iteration         -136 MCMCOBJ=   -6591.31737864499     
 iteration         -135 MCMCOBJ=   -6539.18616093779     
 iteration         -134 MCMCOBJ=   -6606.75073673445     
 iteration         -133 MCMCOBJ=   -6602.19811363690     
 iteration         -132 MCMCOBJ=   -6626.89906401119     
 iteration         -131 MCMCOBJ=   -6599.65062033154     
 iteration         -130 MCMCOBJ=   -6639.18875843940     
 iteration         -129 MCMCOBJ=   -6715.00021585284     
 iteration         -128 MCMCOBJ=   -6718.20639624191     
 iteration         -127 MCMCOBJ=   -6692.35405461365     
 iteration         -126 MCMCOBJ=   -6652.13957212791     
 iteration         -125 MCMCOBJ=   -6607.53093735414     
 iteration         -124 MCMCOBJ=   -6596.90310682743     
 iteration         -123 MCMCOBJ=   -6662.69955642718     
 iteration         -122 MCMCOBJ=   -6642.28789507476     
 iteration         -121 MCMCOBJ=   -6655.53132669439     
 iteration         -120 MCMCOBJ=   -6647.48075433852     
 iteration         -119 MCMCOBJ=   -6635.94618538931     
 iteration         -118 MCMCOBJ=   -6645.90347538378     
 iteration         -117 MCMCOBJ=   -6655.56732748476     
 iteration         -116 MCMCOBJ=   -6665.68327829723     
 iteration         -115 MCMCOBJ=   -6672.00293000449     
 iteration         -114 MCMCOBJ=   -6675.69139157232     
 iteration         -113 MCMCOBJ=   -6675.24004326992     
 iteration         -112 MCMCOBJ=   -6613.88338992558     
 iteration         -111 MCMCOBJ=   -6634.34343283828     
 iteration         -110 MCMCOBJ=   -6627.58003994941     
 iteration         -109 MCMCOBJ=   -6633.05960411163     
 iteration         -108 MCMCOBJ=   -6627.20185367624     
 iteration         -107 MCMCOBJ=   -6629.36981335074     
 iteration         -106 MCMCOBJ=   -6679.86804621329     
 iteration         -105 MCMCOBJ=   -6656.93419475260     
 iteration         -104 MCMCOBJ=   -6598.32251764541     
 iteration         -103 MCMCOBJ=   -6585.84957793374     
 iteration         -102 MCMCOBJ=   -6574.08725392134     
 iteration         -101 MCMCOBJ=   -6562.11151874857     
 iteration         -100 MCMCOBJ=   -6577.87298898503     
 iteration          -99 MCMCOBJ=   -6664.93549473348     
 iteration          -98 MCMCOBJ=   -6646.01242998004     
 iteration          -97 MCMCOBJ=   -6614.42474519122     
 iteration          -96 MCMCOBJ=   -6614.99071343654     
 iteration          -95 MCMCOBJ=   -6573.43030190808     
 iteration          -94 MCMCOBJ=   -6598.49363571539     
 iteration          -93 MCMCOBJ=   -6589.18959412112     
 iteration          -92 MCMCOBJ=   -6601.23890083505     
 iteration          -91 MCMCOBJ=   -6587.76015757132     
 iteration          -90 MCMCOBJ=   -6588.88736242087     
 iteration          -89 MCMCOBJ=   -6662.57993162173     
 iteration          -88 MCMCOBJ=   -6624.70254594045     
 iteration          -87 MCMCOBJ=   -6651.31361546818     
 iteration          -86 MCMCOBJ=   -6639.54517083199     
 iteration          -85 MCMCOBJ=   -6639.26089206133     
 iteration          -84 MCMCOBJ=   -6641.64169890880     
 iteration          -83 MCMCOBJ=   -6637.65045992509     
 iteration          -82 MCMCOBJ=   -6651.89646960182     
 iteration          -81 MCMCOBJ=   -6664.30841771889     
 iteration          -80 MCMCOBJ=   -6652.75792213986     
 iteration          -79 MCMCOBJ=   -6651.37438659919     
 iteration          -78 MCMCOBJ=   -6640.83985105676     
 iteration          -77 MCMCOBJ=   -6614.06891554679     
 iteration          -76 MCMCOBJ=   -6660.96232568600     
 iteration          -75 MCMCOBJ=   -6655.10430603933     
 iteration          -74 MCMCOBJ=   -6663.90785281145     
 iteration          -73 MCMCOBJ=   -6657.36890668772     
 iteration          -72 MCMCOBJ=   -6584.07710268804     
 iteration          -71 MCMCOBJ=   -6651.27134801409     
 iteration          -70 MCMCOBJ=   -6638.61332050109     
 iteration          -69 MCMCOBJ=   -6641.42567708022     
 iteration          -68 MCMCOBJ=   -6608.64855078947     
 iteration          -67 MCMCOBJ=   -6606.74623630522     
 iteration          -66 MCMCOBJ=   -6586.52690577167     
 iteration          -65 MCMCOBJ=   -6643.05507524141     
 iteration          -64 MCMCOBJ=   -6659.53522926617     
 iteration          -63 MCMCOBJ=   -6660.29882579438     
 iteration          -62 MCMCOBJ=   -6685.69139213402     
 iteration          -61 MCMCOBJ=   -6632.87759593393     
 iteration          -60 MCMCOBJ=   -6687.38791436144     
 iteration          -59 MCMCOBJ=   -6606.54079128514     
 iteration          -58 MCMCOBJ=   -6600.10682613234     
 iteration          -57 MCMCOBJ=   -6644.25376902444     
 iteration          -56 MCMCOBJ=   -6648.19694978399     
 iteration          -55 MCMCOBJ=   -6671.01603738032     
 iteration          -54 MCMCOBJ=   -6649.08818087468     
 iteration          -53 MCMCOBJ=   -6638.55378504770     
 iteration          -52 MCMCOBJ=   -6644.33205413522     
 iteration          -51 MCMCOBJ=   -6620.89842102287     
 iteration          -50 MCMCOBJ=   -6618.60008313115     
 iteration          -49 MCMCOBJ=   -6617.29421223892     
 iteration          -48 MCMCOBJ=   -6579.92248048510     
 iteration          -47 MCMCOBJ=   -6590.89336195021     
 iteration          -46 MCMCOBJ=   -6561.47285582857     
 iteration          -45 MCMCOBJ=   -6573.72685942521     
 iteration          -44 MCMCOBJ=   -6592.61666185689     
 iteration          -43 MCMCOBJ=   -6593.76710043427     
 iteration          -42 MCMCOBJ=   -6643.48292342296     
 iteration          -41 MCMCOBJ=   -6643.48299493327     
 iteration          -40 MCMCOBJ=   -6666.57449839495     
 iteration          -39 MCMCOBJ=   -6666.57450963068     
 iteration          -38 MCMCOBJ=   -6660.22490369780     
 iteration          -37 MCMCOBJ=   -6658.61156244870     
 iteration          -36 MCMCOBJ=   -6658.01474646683     
 iteration          -35 MCMCOBJ=   -6641.79342703629     
 iteration          -34 MCMCOBJ=   -6626.41601103208     
 iteration          -33 MCMCOBJ=   -6658.87701625148     
 iteration          -32 MCMCOBJ=   -6605.47573507483     
 iteration          -31 MCMCOBJ=   -6598.12582750829     
 iteration          -30 MCMCOBJ=   -6598.12582795348     
 iteration          -29 MCMCOBJ=   -6619.85283757670     
 iteration          -28 MCMCOBJ=   -6592.26573093720     
 iteration          -27 MCMCOBJ=   -6647.34178082908     
 iteration          -26 MCMCOBJ=   -6651.06459454332     
 iteration          -25 MCMCOBJ=   -6638.08924574430     
 iteration          -24 MCMCOBJ=   -6583.54962379755     
 iteration          -23 MCMCOBJ=   -6630.21295172117     
 iteration          -22 MCMCOBJ=   -6617.82671134837     
 iteration          -21 MCMCOBJ=   -6611.66935468269     
 iteration          -20 MCMCOBJ=   -6598.07115385619     
 iteration          -19 MCMCOBJ=   -6605.14962898498     
 iteration          -18 MCMCOBJ=   -6589.45425876065     
 iteration          -17 MCMCOBJ=   -6599.19264295552     
 iteration          -16 MCMCOBJ=   -6614.76474461411     
 iteration          -15 MCMCOBJ=   -6635.11318924268     
 iteration          -14 MCMCOBJ=   -6638.43495304736     
 iteration          -13 MCMCOBJ=   -6644.24615871837     
 iteration          -12 MCMCOBJ=   -6653.12640220342     
 iteration          -11 MCMCOBJ=   -6699.68232337356     
 iteration          -10 MCMCOBJ=   -6699.31337188238     
 iteration           -9 MCMCOBJ=   -6688.58938655048     
 iteration           -8 MCMCOBJ=   -6641.25022144270     
 iteration           -7 MCMCOBJ=   -6649.79034691880     
 iteration           -6 MCMCOBJ=   -6621.76879170927     
 iteration           -5 MCMCOBJ=   -6619.01381393262     
 iteration           -4 MCMCOBJ=   -6611.69691547462     
 iteration           -3 MCMCOBJ=   -6571.39025842091     
 iteration           -2 MCMCOBJ=   -6577.77333588545     
 iteration           -1 MCMCOBJ=   -6601.86145618548     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6639.22429944786     
 iteration            1 MCMCOBJ=   -6645.20972784472     
 iteration            2 MCMCOBJ=   -6595.30291693299     
 iteration            3 MCMCOBJ=   -6631.76825938626     
 iteration            4 MCMCOBJ=   -6652.04945696110     
 iteration            5 MCMCOBJ=   -6652.04945719277     
 iteration            6 MCMCOBJ=   -6668.32268608960     
 iteration            7 MCMCOBJ=   -6624.16686498747     
 iteration            8 MCMCOBJ=   -6615.50303407742     
 iteration            9 MCMCOBJ=   -6641.56820188576     
 iteration           10 MCMCOBJ=   -6579.24100067777     
 iteration           11 MCMCOBJ=   -6534.88083126694     
 iteration           12 MCMCOBJ=   -6644.30860008228     
 iteration           13 MCMCOBJ=   -6629.95770657644     
 iteration           14 MCMCOBJ=   -6618.80575596581     
 iteration           15 MCMCOBJ=   -6641.41576343101     
 iteration           16 MCMCOBJ=   -6644.74663869343     
 iteration           17 MCMCOBJ=   -6611.45857813056     
 iteration           18 MCMCOBJ=   -6613.71238967249     
 iteration           19 MCMCOBJ=   -6652.39201171958     
 iteration           20 MCMCOBJ=   -6668.02759149124     
 iteration           21 MCMCOBJ=   -6662.73725278058     
 iteration           22 MCMCOBJ=   -6665.77483112401     
 iteration           23 MCMCOBJ=   -6676.33840779589     
 iteration           24 MCMCOBJ=   -6611.94215158651     
 iteration           25 MCMCOBJ=   -6608.54413552123     
 iteration           26 MCMCOBJ=   -6614.72664032298     
 iteration           27 MCMCOBJ=   -6598.02789381490     
 iteration           28 MCMCOBJ=   -6557.84636625531     
 iteration           29 MCMCOBJ=   -6538.54293221101     
 iteration           30 MCMCOBJ=   -6564.75444153819     
 iteration           31 MCMCOBJ=   -6606.75853440075     
 iteration           32 MCMCOBJ=   -6697.74987344263     
 iteration           33 MCMCOBJ=   -6648.22612055782     
 iteration           34 MCMCOBJ=   -6605.66678036887     
 iteration           35 MCMCOBJ=   -6635.32860781014     
 iteration           36 MCMCOBJ=   -6635.95509082928     
 iteration           37 MCMCOBJ=   -6630.56218873412     
 iteration           38 MCMCOBJ=   -6652.48259213505     
 iteration           39 MCMCOBJ=   -6646.44933474733     
 iteration           40 MCMCOBJ=   -6580.42568226970     
 iteration           41 MCMCOBJ=   -6594.99450965465     
 iteration           42 MCMCOBJ=   -6618.69713503536     
 iteration           43 MCMCOBJ=   -6608.41009896484     
 iteration           44 MCMCOBJ=   -6597.43880512820     
 iteration           45 MCMCOBJ=   -6670.33991689443     
 iteration           46 MCMCOBJ=   -6558.05922273839     
 iteration           47 MCMCOBJ=   -6570.90975318895     
 iteration           48 MCMCOBJ=   -6585.51066478719     
 iteration           49 MCMCOBJ=   -6650.36544474504     
 iteration           50 MCMCOBJ=   -6650.36544394875     
 iteration           51 MCMCOBJ=   -6652.83468446601     
 iteration           52 MCMCOBJ=   -6643.61260203123     
 iteration           53 MCMCOBJ=   -6639.89209638269     
 iteration           54 MCMCOBJ=   -6675.46631110523     
 iteration           55 MCMCOBJ=   -6665.30263471769     
 iteration           56 MCMCOBJ=   -6635.52809125981     
 iteration           57 MCMCOBJ=   -6642.97321010734     
 iteration           58 MCMCOBJ=   -6703.41297055314     
 iteration           59 MCMCOBJ=   -6674.04875017807     
 iteration           60 MCMCOBJ=   -6639.49047765765     
 iteration           61 MCMCOBJ=   -6685.62925784985     
 iteration           62 MCMCOBJ=   -6610.65307919081     
 iteration           63 MCMCOBJ=   -6617.02825287815     
 iteration           64 MCMCOBJ=   -6618.98181617785     
 iteration           65 MCMCOBJ=   -6543.65242069936     
 iteration           66 MCMCOBJ=   -6567.71977294340     
 iteration           67 MCMCOBJ=   -6589.53735769451     
 iteration           68 MCMCOBJ=   -6630.19570868064     
 iteration           69 MCMCOBJ=   -6635.97666461223     
 iteration           70 MCMCOBJ=   -6607.83354617681     
 iteration           71 MCMCOBJ=   -6622.43640375175     
 iteration           72 MCMCOBJ=   -6616.65962228283     
 iteration           73 MCMCOBJ=   -6617.96810713297     
 iteration           74 MCMCOBJ=   -6617.96808929577     
 iteration           75 MCMCOBJ=   -6666.06118742069     
 iteration           76 MCMCOBJ=   -6674.93697390747     
 iteration           77 MCMCOBJ=   -6678.59040712855     
 iteration           78 MCMCOBJ=   -6641.26610773630     
 iteration           79 MCMCOBJ=   -6658.97942720514     
 iteration           80 MCMCOBJ=   -6604.89818506958     
 iteration           81 MCMCOBJ=   -6596.93007604808     
 iteration           82 MCMCOBJ=   -6587.51642483301     
 iteration           83 MCMCOBJ=   -6639.18244913655     
 iteration           84 MCMCOBJ=   -6668.56561286471     
 iteration           85 MCMCOBJ=   -6655.03960861167     
 iteration           86 MCMCOBJ=   -6651.94470796246     
 iteration           87 MCMCOBJ=   -6635.06077678040     
 iteration           88 MCMCOBJ=   -6642.35171870611     
 iteration           89 MCMCOBJ=   -6596.80662321530     
 iteration           90 MCMCOBJ=   -6639.91161018715     
 iteration           91 MCMCOBJ=   -6624.16214278398     
 iteration           92 MCMCOBJ=   -6600.49767336346     
 iteration           93 MCMCOBJ=   -6615.54861817621     
 iteration           94 MCMCOBJ=   -6635.79486324787     
 iteration           95 MCMCOBJ=   -6612.88198441796     
 iteration           96 MCMCOBJ=   -6595.49785798568     
 iteration           97 MCMCOBJ=   -6613.55106225802     
 iteration           98 MCMCOBJ=   -6603.12416611895     
 iteration           99 MCMCOBJ=   -6664.14648267561     
 iteration          100 MCMCOBJ=   -6672.33210640794     
 iteration          101 MCMCOBJ=   -6635.22292954281     
 iteration          102 MCMCOBJ=   -6591.88683174151     
 iteration          103 MCMCOBJ=   -6653.70538583190     
 iteration          104 MCMCOBJ=   -6668.39888819173     
 iteration          105 MCMCOBJ=   -6605.49667024160     
 iteration          106 MCMCOBJ=   -6638.58372216011     
 iteration          107 MCMCOBJ=   -6677.61267517569     
 iteration          108 MCMCOBJ=   -6683.93585508230     
 iteration          109 MCMCOBJ=   -6672.31352692179     
 iteration          110 MCMCOBJ=   -6668.64046696645     
 iteration          111 MCMCOBJ=   -6704.58622621366     
 iteration          112 MCMCOBJ=   -6639.89003835182     
 iteration          113 MCMCOBJ=   -6646.62740285683     
 iteration          114 MCMCOBJ=   -6587.26689097080     
 iteration          115 MCMCOBJ=   -6557.71825276764     
 iteration          116 MCMCOBJ=   -6594.76926418657     
 iteration          117 MCMCOBJ=   -6668.62870916117     
 iteration          118 MCMCOBJ=   -6636.99137638152     
 iteration          119 MCMCOBJ=   -6599.18194102054     
 iteration          120 MCMCOBJ=   -6589.10504367441     
 iteration          121 MCMCOBJ=   -6610.50921567187     
 iteration          122 MCMCOBJ=   -6610.50921995366     
 iteration          123 MCMCOBJ=   -6634.59219438327     
 iteration          124 MCMCOBJ=   -6655.85481755612     
 iteration          125 MCMCOBJ=   -6643.42740270111     
 iteration          126 MCMCOBJ=   -6581.97619272212     
 iteration          127 MCMCOBJ=   -6593.15628531878     
 iteration          128 MCMCOBJ=   -6619.32068293372     
 iteration          129 MCMCOBJ=   -6654.20587702685     
 iteration          130 MCMCOBJ=   -6600.15378157075     
 iteration          131 MCMCOBJ=   -6606.18586219563     
 iteration          132 MCMCOBJ=   -6597.25558669265     
 iteration          133 MCMCOBJ=   -6617.53193097300     
 iteration          134 MCMCOBJ=   -6657.68273253978     
 iteration          135 MCMCOBJ=   -6650.62072402245     
 iteration          136 MCMCOBJ=   -6656.52687924982     
 iteration          137 MCMCOBJ=   -6605.53637223785     
 iteration          138 MCMCOBJ=   -6628.41044254343     
 iteration          139 MCMCOBJ=   -6663.10207187125     
 iteration          140 MCMCOBJ=   -6650.93925952302     
 iteration          141 MCMCOBJ=   -6639.38124855582     
 iteration          142 MCMCOBJ=   -6699.14383716335     
 iteration          143 MCMCOBJ=   -6647.78402550735     
 iteration          144 MCMCOBJ=   -6630.94526526187     
 iteration          145 MCMCOBJ=   -6596.20885843470     
 iteration          146 MCMCOBJ=   -6635.24467316558     
 iteration          147 MCMCOBJ=   -6693.59676834803     
 iteration          148 MCMCOBJ=   -6652.90332423858     
 iteration          149 MCMCOBJ=   -6632.48918624672     
 iteration          150 MCMCOBJ=   -6605.22946255892     
 iteration          151 MCMCOBJ=   -6605.49247374059     
 iteration          152 MCMCOBJ=   -6669.47499071659     
 iteration          153 MCMCOBJ=   -6633.26580065690     
 iteration          154 MCMCOBJ=   -6620.89951787648     
 iteration          155 MCMCOBJ=   -6645.10087873509     
 iteration          156 MCMCOBJ=   -6608.38163948435     
 iteration          157 MCMCOBJ=   -6637.21806459317     
 iteration          158 MCMCOBJ=   -6634.39336749533     
 iteration          159 MCMCOBJ=   -6602.16984849007     
 iteration          160 MCMCOBJ=   -6706.73442039383     
 iteration          161 MCMCOBJ=   -6706.73437109766     
 iteration          162 MCMCOBJ=   -6670.12367859883     
 iteration          163 MCMCOBJ=   -6654.83826968538     
 iteration          164 MCMCOBJ=   -6642.05389117299     
 iteration          165 MCMCOBJ=   -6620.38970247796     
 iteration          166 MCMCOBJ=   -6603.45417259665     
 iteration          167 MCMCOBJ=   -6624.38978367742     
 iteration          168 MCMCOBJ=   -6616.84377776928     
 iteration          169 MCMCOBJ=   -6624.44518498574     
 iteration          170 MCMCOBJ=   -6642.81321316676     
 iteration          171 MCMCOBJ=   -6637.88892584902     
 iteration          172 MCMCOBJ=   -6625.55076485749     
 iteration          173 MCMCOBJ=   -6630.43339846157     
 iteration          174 MCMCOBJ=   -6625.54456981812     
 iteration          175 MCMCOBJ=   -6572.63660100790     
 iteration          176 MCMCOBJ=   -6594.93666088427     
 iteration          177 MCMCOBJ=   -6560.62955786375     
 iteration          178 MCMCOBJ=   -6609.21694413617     
 iteration          179 MCMCOBJ=   -6618.89771948720     
 iteration          180 MCMCOBJ=   -6639.87337311564     
 iteration          181 MCMCOBJ=   -6625.38337135923     
 iteration          182 MCMCOBJ=   -6620.23625121731     
 iteration          183 MCMCOBJ=   -6624.96200154923     
 iteration          184 MCMCOBJ=   -6616.23367796836     
 iteration          185 MCMCOBJ=   -6658.36692068880     
 iteration          186 MCMCOBJ=   -6660.94494419099     
 iteration          187 MCMCOBJ=   -6646.18863985992     
 iteration          188 MCMCOBJ=   -6647.75728436588     
 iteration          189 MCMCOBJ=   -6659.79209583174     
 iteration          190 MCMCOBJ=   -6651.23184535708     
 iteration          191 MCMCOBJ=   -6641.85985978242     
 iteration          192 MCMCOBJ=   -6653.78341986049     
 iteration          193 MCMCOBJ=   -6625.96638437319     
 iteration          194 MCMCOBJ=   -6626.02486105613     
 iteration          195 MCMCOBJ=   -6624.04547026841     
 iteration          196 MCMCOBJ=   -6633.78333229679     
 iteration          197 MCMCOBJ=   -6643.92261240859     
 iteration          198 MCMCOBJ=   -6642.58977563971     
 iteration          199 MCMCOBJ=   -6642.39269027041     
 iteration          200 MCMCOBJ=   -6611.11408805856     
 iteration          201 MCMCOBJ=   -6621.38249275265     
 iteration          202 MCMCOBJ=   -6685.31056356033     
 iteration          203 MCMCOBJ=   -6683.48410276668     
 iteration          204 MCMCOBJ=   -6685.07096018308     
 iteration          205 MCMCOBJ=   -6665.73273905124     
 iteration          206 MCMCOBJ=   -6695.23262100791     
 iteration          207 MCMCOBJ=   -6683.28758009865     
 iteration          208 MCMCOBJ=   -6687.46400669383     
 iteration          209 MCMCOBJ=   -6643.97052063161     
 iteration          210 MCMCOBJ=   -6644.62657094712     
 iteration          211 MCMCOBJ=   -6662.90602706474     
 iteration          212 MCMCOBJ=   -6627.90236664100     
 iteration          213 MCMCOBJ=   -6627.90235547372     
 iteration          214 MCMCOBJ=   -6676.89552592923     
 iteration          215 MCMCOBJ=   -6622.11918735651     
 iteration          216 MCMCOBJ=   -6614.00691186223     
 iteration          217 MCMCOBJ=   -6586.82907453510     
 iteration          218 MCMCOBJ=   -6603.11438156315     
 iteration          219 MCMCOBJ=   -6631.30267136423     
 iteration          220 MCMCOBJ=   -6662.16606469634     
 iteration          221 MCMCOBJ=   -6641.51883665487     
 iteration          222 MCMCOBJ=   -6600.89904032188     
 iteration          223 MCMCOBJ=   -6625.56554728856     
 iteration          224 MCMCOBJ=   -6596.32376556434     
 iteration          225 MCMCOBJ=   -6626.80499088811     
 iteration          226 MCMCOBJ=   -6580.74722451629     
 iteration          227 MCMCOBJ=   -6625.43507161239     
 iteration          228 MCMCOBJ=   -6632.78307866825     
 iteration          229 MCMCOBJ=   -6564.31034870153     
 iteration          230 MCMCOBJ=   -6629.50903272232     
 iteration          231 MCMCOBJ=   -6615.82952411893     
 iteration          232 MCMCOBJ=   -6556.84282968075     
 iteration          233 MCMCOBJ=   -6545.90547832154     
 iteration          234 MCMCOBJ=   -6560.72148108137     
 iteration          235 MCMCOBJ=   -6605.40782787244     
 iteration          236 MCMCOBJ=   -6668.46060426799     
 iteration          237 MCMCOBJ=   -6708.31263547644     
 iteration          238 MCMCOBJ=   -6613.50144970782     
 iteration          239 MCMCOBJ=   -6661.70959436038     
 iteration          240 MCMCOBJ=   -6666.49951333077     
 iteration          241 MCMCOBJ=   -6629.69355510410     
 iteration          242 MCMCOBJ=   -6596.59763452080     
 iteration          243 MCMCOBJ=   -6588.75888912823     
 iteration          244 MCMCOBJ=   -6508.69812552394     
 iteration          245 MCMCOBJ=   -6551.30697594569     
 iteration          246 MCMCOBJ=   -6573.18231129325     
 iteration          247 MCMCOBJ=   -6597.43087727394     
 iteration          248 MCMCOBJ=   -6599.86259930026     
 iteration          249 MCMCOBJ=   -6610.25528329106     
 iteration          250 MCMCOBJ=   -6586.05636824775     
 iteration          251 MCMCOBJ=   -6681.18847532564     
 iteration          252 MCMCOBJ=   -6688.29456289112     
 iteration          253 MCMCOBJ=   -6687.22028370266     
 iteration          254 MCMCOBJ=   -6714.37620930596     
 iteration          255 MCMCOBJ=   -6707.08323707153     
 iteration          256 MCMCOBJ=   -6683.22284057165     
 iteration          257 MCMCOBJ=   -6622.82652914957     
 iteration          258 MCMCOBJ=   -6633.67131133183     
 iteration          259 MCMCOBJ=   -6601.54998397722     
 iteration          260 MCMCOBJ=   -6581.88708337303     
 iteration          261 MCMCOBJ=   -6569.02634911507     
 iteration          262 MCMCOBJ=   -6614.96431183946     
 iteration          263 MCMCOBJ=   -6646.80618255252     
 iteration          264 MCMCOBJ=   -6670.00106644983     
 iteration          265 MCMCOBJ=   -6685.79287947315     
 iteration          266 MCMCOBJ=   -6665.94425679915     
 iteration          267 MCMCOBJ=   -6675.59734347201     
 iteration          268 MCMCOBJ=   -6669.12914408577     
 iteration          269 MCMCOBJ=   -6646.17940651861     
 iteration          270 MCMCOBJ=   -6656.70174756315     
 iteration          271 MCMCOBJ=   -6641.85957489585     
 iteration          272 MCMCOBJ=   -6632.76837026510     
 iteration          273 MCMCOBJ=   -6660.95897221684     
 iteration          274 MCMCOBJ=   -6679.20916064883     
 iteration          275 MCMCOBJ=   -6657.97234069522     
 iteration          276 MCMCOBJ=   -6662.73912490717     
 iteration          277 MCMCOBJ=   -6649.89518278494     
 iteration          278 MCMCOBJ=   -6634.41056011855     
 iteration          279 MCMCOBJ=   -6618.10377842402     
 iteration          280 MCMCOBJ=   -6623.52668950972     
 iteration          281 MCMCOBJ=   -6634.29552326491     
 iteration          282 MCMCOBJ=   -6629.63958303565     
 iteration          283 MCMCOBJ=   -6628.71158981608     
 iteration          284 MCMCOBJ=   -6623.59035479213     
 iteration          285 MCMCOBJ=   -6646.93435388744     
 iteration          286 MCMCOBJ=   -6619.89498618816     
 iteration          287 MCMCOBJ=   -6669.30570858358     
 iteration          288 MCMCOBJ=   -6707.53217054637     
 iteration          289 MCMCOBJ=   -6700.34934412116     
 iteration          290 MCMCOBJ=   -6695.08605325949     
 iteration          291 MCMCOBJ=   -6704.56494276505     
 iteration          292 MCMCOBJ=   -6704.76127626529     
 iteration          293 MCMCOBJ=   -6681.21749419364     
 iteration          294 MCMCOBJ=   -6599.17328943043     
 iteration          295 MCMCOBJ=   -6565.27849055791     
 iteration          296 MCMCOBJ=   -6601.29020375824     
 iteration          297 MCMCOBJ=   -6586.79334822356     
 iteration          298 MCMCOBJ=   -6631.85431392495     
 iteration          299 MCMCOBJ=   -6631.85431481895     
 iteration          300 MCMCOBJ=   -6637.74310157569     
 iteration          301 MCMCOBJ=   -6649.81489065465     
 iteration          302 MCMCOBJ=   -6607.89099271380     
 iteration          303 MCMCOBJ=   -6614.84301080207     
 iteration          304 MCMCOBJ=   -6654.64093592426     
 iteration          305 MCMCOBJ=   -6646.74043032051     
 iteration          306 MCMCOBJ=   -6596.81969496741     
 iteration          307 MCMCOBJ=   -6591.10858335606     
 iteration          308 MCMCOBJ=   -6605.97868430512     
 iteration          309 MCMCOBJ=   -6569.54489273288     
 iteration          310 MCMCOBJ=   -6588.91623691208     
 iteration          311 MCMCOBJ=   -6582.89298202593     
 iteration          312 MCMCOBJ=   -6643.21192124322     
 iteration          313 MCMCOBJ=   -6622.02391679578     
 iteration          314 MCMCOBJ=   -6658.80248111752     
 iteration          315 MCMCOBJ=   -6655.76917010277     
 iteration          316 MCMCOBJ=   -6622.04552562279     
 iteration          317 MCMCOBJ=   -6641.33064855034     
 iteration          318 MCMCOBJ=   -6645.31825149250     
 iteration          319 MCMCOBJ=   -6628.35604294626     
 iteration          320 MCMCOBJ=   -6611.07123420023     
 iteration          321 MCMCOBJ=   -6592.26681748947     
 iteration          322 MCMCOBJ=   -6621.62439923953     
 iteration          323 MCMCOBJ=   -6644.34754982000     
 iteration          324 MCMCOBJ=   -6622.83531060468     
 iteration          325 MCMCOBJ=   -6638.24284514659     
 iteration          326 MCMCOBJ=   -6628.32529811741     
 iteration          327 MCMCOBJ=   -6620.65211096975     
 iteration          328 MCMCOBJ=   -6593.93652310944     
 iteration          329 MCMCOBJ=   -6628.58735829260     
 iteration          330 MCMCOBJ=   -6628.58735343178     
 iteration          331 MCMCOBJ=   -6630.17290636380     
 iteration          332 MCMCOBJ=   -6630.48400511139     
 iteration          333 MCMCOBJ=   -6652.29493077920     
 iteration          334 MCMCOBJ=   -6650.36057979236     
 iteration          335 MCMCOBJ=   -6628.30477674148     
 iteration          336 MCMCOBJ=   -6632.58447170763     
 iteration          337 MCMCOBJ=   -6626.94766966674     
 iteration          338 MCMCOBJ=   -6631.01532705500     
 iteration          339 MCMCOBJ=   -6606.83636088171     
 iteration          340 MCMCOBJ=   -6654.22276469308     
 iteration          341 MCMCOBJ=   -6621.80355185686     
 iteration          342 MCMCOBJ=   -6618.31883324210     
 iteration          343 MCMCOBJ=   -6645.07004825200     
 iteration          344 MCMCOBJ=   -6611.67405317587     
 iteration          345 MCMCOBJ=   -6635.25173750790     
 iteration          346 MCMCOBJ=   -6610.43208756238     
 iteration          347 MCMCOBJ=   -6592.96482155126     
 iteration          348 MCMCOBJ=   -6611.16584994742     
 iteration          349 MCMCOBJ=   -6640.83945341854     
 iteration          350 MCMCOBJ=   -6626.03400785664     
 iteration          351 MCMCOBJ=   -6640.77988573301     
 iteration          352 MCMCOBJ=   -6604.59171767385     
 iteration          353 MCMCOBJ=   -6553.09513849016     
 iteration          354 MCMCOBJ=   -6607.53370950879     
 iteration          355 MCMCOBJ=   -6602.74047266923     
 iteration          356 MCMCOBJ=   -6635.42431493415     
 iteration          357 MCMCOBJ=   -6655.36846258194     
 iteration          358 MCMCOBJ=   -6610.15260412602     
 iteration          359 MCMCOBJ=   -6636.76734411676     
 iteration          360 MCMCOBJ=   -6598.11640179951     
 iteration          361 MCMCOBJ=   -6569.95305086808     
 iteration          362 MCMCOBJ=   -6580.90070199958     
 iteration          363 MCMCOBJ=   -6605.15653080828     
 iteration          364 MCMCOBJ=   -6659.47770272519     
 iteration          365 MCMCOBJ=   -6670.91104851744     
 iteration          366 MCMCOBJ=   -6697.03183029167     
 iteration          367 MCMCOBJ=   -6670.31068667762     
 iteration          368 MCMCOBJ=   -6647.02005204927     
 iteration          369 MCMCOBJ=   -6631.06091266303     
 iteration          370 MCMCOBJ=   -6605.29752640240     
 iteration          371 MCMCOBJ=   -6524.54304872934     
 iteration          372 MCMCOBJ=   -6632.12045401448     
 iteration          373 MCMCOBJ=   -6650.11580155233     
 iteration          374 MCMCOBJ=   -6677.42875921052     
 iteration          375 MCMCOBJ=   -6630.34550266605     
 iteration          376 MCMCOBJ=   -6653.65115155946     
 iteration          377 MCMCOBJ=   -6623.02813949358     
 iteration          378 MCMCOBJ=   -6612.19003183020     
 iteration          379 MCMCOBJ=   -6634.91107508161     
 iteration          380 MCMCOBJ=   -6592.66265528854     
 iteration          381 MCMCOBJ=   -6617.78927099858     
 iteration          382 MCMCOBJ=   -6622.31304935196     
 iteration          383 MCMCOBJ=   -6621.98652353171     
 iteration          384 MCMCOBJ=   -6636.41792125128     
 iteration          385 MCMCOBJ=   -6638.02945521959     
 iteration          386 MCMCOBJ=   -6617.18937215971     
 iteration          387 MCMCOBJ=   -6558.00986773349     
 iteration          388 MCMCOBJ=   -6548.41538687051     
 iteration          389 MCMCOBJ=   -6574.51799601916     
 iteration          390 MCMCOBJ=   -6642.38834220584     
 iteration          391 MCMCOBJ=   -6668.45765627298     
 iteration          392 MCMCOBJ=   -6612.16310236674     
 iteration          393 MCMCOBJ=   -6612.16310256085     
 iteration          394 MCMCOBJ=   -6693.87936140188     
 iteration          395 MCMCOBJ=   -6652.96398105703     
 iteration          396 MCMCOBJ=   -6662.34976993359     
 iteration          397 MCMCOBJ=   -6630.33353582575     
 iteration          398 MCMCOBJ=   -6641.11851140488     
 iteration          399 MCMCOBJ=   -6619.52675384452     
 iteration          400 MCMCOBJ=   -6629.65021806555     
 iteration          401 MCMCOBJ=   -6646.00258558968     
 iteration          402 MCMCOBJ=   -6647.66404013490     
 iteration          403 MCMCOBJ=   -6643.48853845357     
 iteration          404 MCMCOBJ=   -6629.27559422145     
 iteration          405 MCMCOBJ=   -6630.07732727266     
 iteration          406 MCMCOBJ=   -6628.76951043438     
 iteration          407 MCMCOBJ=   -6600.86011332587     
 iteration          408 MCMCOBJ=   -6660.18030966483     
 iteration          409 MCMCOBJ=   -6646.37489999167     
 iteration          410 MCMCOBJ=   -6645.32802783867     
 iteration          411 MCMCOBJ=   -6662.81215715805     
 iteration          412 MCMCOBJ=   -6631.91656398211     
 iteration          413 MCMCOBJ=   -6659.72533865048     
 iteration          414 MCMCOBJ=   -6606.02618377390     
 iteration          415 MCMCOBJ=   -6598.84345002598     
 iteration          416 MCMCOBJ=   -6566.20970244962     
 iteration          417 MCMCOBJ=   -6608.28608268575     
 iteration          418 MCMCOBJ=   -6599.68401276780     
 iteration          419 MCMCOBJ=   -6630.80358778761     
 iteration          420 MCMCOBJ=   -6660.38216262424     
 iteration          421 MCMCOBJ=   -6640.55086824776     
 iteration          422 MCMCOBJ=   -6633.51499448434     
 iteration          423 MCMCOBJ=   -6632.75127524421     
 iteration          424 MCMCOBJ=   -6587.24176272150     
 iteration          425 MCMCOBJ=   -6602.40098974814     
 iteration          426 MCMCOBJ=   -6559.94809851832     
 iteration          427 MCMCOBJ=   -6568.23910590057     
 iteration          428 MCMCOBJ=   -6610.55569991725     
 iteration          429 MCMCOBJ=   -6648.23608332129     
 iteration          430 MCMCOBJ=   -6618.15796894545     
 iteration          431 MCMCOBJ=   -6694.95937091753     
 iteration          432 MCMCOBJ=   -6694.95936861063     
 iteration          433 MCMCOBJ=   -6655.01308272917     
 iteration          434 MCMCOBJ=   -6650.26027379128     
 iteration          435 MCMCOBJ=   -6635.81920286519     
 iteration          436 MCMCOBJ=   -6663.50035400292     
 iteration          437 MCMCOBJ=   -6644.72448858731     
 iteration          438 MCMCOBJ=   -6642.71377205384     
 iteration          439 MCMCOBJ=   -6638.54187206897     
 iteration          440 MCMCOBJ=   -6596.08044628439     
 iteration          441 MCMCOBJ=   -6659.91173621690     
 iteration          442 MCMCOBJ=   -6674.19631416509     
 iteration          443 MCMCOBJ=   -6656.10680283621     
 iteration          444 MCMCOBJ=   -6655.86467637384     
 iteration          445 MCMCOBJ=   -6566.71926660029     
 iteration          446 MCMCOBJ=   -6658.36053446830     
 iteration          447 MCMCOBJ=   -6690.35422839215     
 iteration          448 MCMCOBJ=   -6683.82224480595     
 iteration          449 MCMCOBJ=   -6694.41123726495     
 iteration          450 MCMCOBJ=   -6666.55034417260     
 iteration          451 MCMCOBJ=   -6611.13695661101     
 iteration          452 MCMCOBJ=   -6638.66577026766     
 iteration          453 MCMCOBJ=   -6609.51728345688     
 iteration          454 MCMCOBJ=   -6572.10659877835     
 iteration          455 MCMCOBJ=   -6551.34947239493     
 iteration          456 MCMCOBJ=   -6576.56603906402     
 iteration          457 MCMCOBJ=   -6587.66382147680     
 iteration          458 MCMCOBJ=   -6583.24979703219     
 iteration          459 MCMCOBJ=   -6636.68020594258     
 iteration          460 MCMCOBJ=   -6626.78447986764     
 iteration          461 MCMCOBJ=   -6626.78448406987     
 iteration          462 MCMCOBJ=   -6644.10538825788     
 iteration          463 MCMCOBJ=   -6644.10538784086     
 iteration          464 MCMCOBJ=   -6624.63031516663     
 iteration          465 MCMCOBJ=   -6646.13957896642     
 iteration          466 MCMCOBJ=   -6613.43678652819     
 iteration          467 MCMCOBJ=   -6644.06081362480     
 iteration          468 MCMCOBJ=   -6650.76878494201     
 iteration          469 MCMCOBJ=   -6671.40556565121     
 iteration          470 MCMCOBJ=   -6654.53967469346     
 iteration          471 MCMCOBJ=   -6642.15253196360     
 iteration          472 MCMCOBJ=   -6638.63087086503     
 iteration          473 MCMCOBJ=   -6609.71796006449     
 iteration          474 MCMCOBJ=   -6585.34032804266     
 iteration          475 MCMCOBJ=   -6551.52137233903     
 iteration          476 MCMCOBJ=   -6624.85010251873     
 iteration          477 MCMCOBJ=   -6645.78946008333     
 iteration          478 MCMCOBJ=   -6652.51459709044     
 iteration          479 MCMCOBJ=   -6603.07164935152     
 iteration          480 MCMCOBJ=   -6627.48854653047     
 iteration          481 MCMCOBJ=   -6651.93241285274     
 iteration          482 MCMCOBJ=   -6637.18090688707     
 iteration          483 MCMCOBJ=   -6607.54592960314     
 iteration          484 MCMCOBJ=   -6603.73110058633     
 iteration          485 MCMCOBJ=   -6578.16360620715     
 iteration          486 MCMCOBJ=   -6600.47410511245     
 iteration          487 MCMCOBJ=   -6600.47411664345     
 iteration          488 MCMCOBJ=   -6653.80517899876     
 iteration          489 MCMCOBJ=   -6659.14299293230     
 iteration          490 MCMCOBJ=   -6650.31800058997     
 iteration          491 MCMCOBJ=   -6673.62137402189     
 iteration          492 MCMCOBJ=   -6661.54335275921     
 iteration          493 MCMCOBJ=   -6625.57955256298     
 iteration          494 MCMCOBJ=   -6644.05250529759     
 iteration          495 MCMCOBJ=   -6652.79605941888     
 iteration          496 MCMCOBJ=   -6656.93446488726     
 iteration          497 MCMCOBJ=   -6623.77930565528     
 iteration          498 MCMCOBJ=   -6614.25974202205     
 iteration          499 MCMCOBJ=   -6650.31493986952     
 iteration          500 MCMCOBJ=   -6574.17484979255     
 iteration          501 MCMCOBJ=   -6695.34749838390     
 iteration          502 MCMCOBJ=   -6633.64472600730     
 iteration          503 MCMCOBJ=   -6620.76095156105     
 iteration          504 MCMCOBJ=   -6579.20958702801     
 iteration          505 MCMCOBJ=   -6614.80367050133     
 iteration          506 MCMCOBJ=   -6629.93511873157     
 iteration          507 MCMCOBJ=   -6629.93510478108     
 iteration          508 MCMCOBJ=   -6575.48237208104     
 iteration          509 MCMCOBJ=   -6613.35392369762     
 iteration          510 MCMCOBJ=   -6626.09231161959     
 iteration          511 MCMCOBJ=   -6609.36512504820     
 iteration          512 MCMCOBJ=   -6581.60622848202     
 iteration          513 MCMCOBJ=   -6639.36340399744     
 iteration          514 MCMCOBJ=   -6684.74037093706     
 iteration          515 MCMCOBJ=   -6626.54200598542     
 iteration          516 MCMCOBJ=   -6645.02429496846     
 iteration          517 MCMCOBJ=   -6677.97911753528     
 iteration          518 MCMCOBJ=   -6637.62735187607     
 iteration          519 MCMCOBJ=   -6679.74092860072     
 iteration          520 MCMCOBJ=   -6639.34385741453     
 iteration          521 MCMCOBJ=   -6651.33810643431     
 iteration          522 MCMCOBJ=   -6667.41527660670     
 iteration          523 MCMCOBJ=   -6681.80243730407     
 iteration          524 MCMCOBJ=   -6673.78803910946     
 iteration          525 MCMCOBJ=   -6607.08211333400     
 iteration          526 MCMCOBJ=   -6588.71308572003     
 iteration          527 MCMCOBJ=   -6546.19712791498     
 iteration          528 MCMCOBJ=   -6504.54346599277     
 iteration          529 MCMCOBJ=   -6564.97609825918     
 iteration          530 MCMCOBJ=   -6602.98458784368     
 iteration          531 MCMCOBJ=   -6623.71104158928     
 iteration          532 MCMCOBJ=   -6611.90521496364     
 iteration          533 MCMCOBJ=   -6655.36519610337     
 iteration          534 MCMCOBJ=   -6624.89889858863     
 iteration          535 MCMCOBJ=   -6645.99754934964     
 iteration          536 MCMCOBJ=   -6631.49603536679     
 iteration          537 MCMCOBJ=   -6663.14629853245     
 iteration          538 MCMCOBJ=   -6648.23434182007     
 iteration          539 MCMCOBJ=   -6648.23434100975     
 iteration          540 MCMCOBJ=   -6697.43307511855     
 iteration          541 MCMCOBJ=   -6687.53027220273     
 iteration          542 MCMCOBJ=   -6686.16367352594     
 iteration          543 MCMCOBJ=   -6632.83646462916     
 iteration          544 MCMCOBJ=   -6656.74555893696     
 iteration          545 MCMCOBJ=   -6648.93719479346     
 iteration          546 MCMCOBJ=   -6655.76120835119     
 iteration          547 MCMCOBJ=   -6578.37985840799     
 iteration          548 MCMCOBJ=   -6601.73056529846     
 iteration          549 MCMCOBJ=   -6588.72268398955     
 iteration          550 MCMCOBJ=   -6601.45220027547     
 iteration          551 MCMCOBJ=   -6610.88956829585     
 iteration          552 MCMCOBJ=   -6624.81651667994     
 iteration          553 MCMCOBJ=   -6667.20303630527     
 iteration          554 MCMCOBJ=   -6619.49925341620     
 iteration          555 MCMCOBJ=   -6619.04153065204     
 iteration          556 MCMCOBJ=   -6587.23682603556     
 iteration          557 MCMCOBJ=   -6665.50644589702     
 iteration          558 MCMCOBJ=   -6660.02310190346     
 iteration          559 MCMCOBJ=   -6668.72583410369     
 iteration          560 MCMCOBJ=   -6662.49493734539     
 iteration          561 MCMCOBJ=   -6623.45843993435     
 iteration          562 MCMCOBJ=   -6566.67699675678     
 iteration          563 MCMCOBJ=   -6583.11797552799     
 iteration          564 MCMCOBJ=   -6582.57055931258     
 iteration          565 MCMCOBJ=   -6609.16400036158     
 iteration          566 MCMCOBJ=   -6593.68075421022     
 iteration          567 MCMCOBJ=   -6611.11192327564     
 iteration          568 MCMCOBJ=   -6623.18007803973     
 iteration          569 MCMCOBJ=   -6638.86297633879     
 iteration          570 MCMCOBJ=   -6679.37601278499     
 iteration          571 MCMCOBJ=   -6689.26048913469     
 iteration          572 MCMCOBJ=   -6692.03367053178     
 iteration          573 MCMCOBJ=   -6672.03431674244     
 iteration          574 MCMCOBJ=   -6655.32844468032     
 iteration          575 MCMCOBJ=   -6693.39636647317     
 iteration          576 MCMCOBJ=   -6664.24299987599     
 iteration          577 MCMCOBJ=   -6680.84929771816     
 iteration          578 MCMCOBJ=   -6651.96909881295     
 iteration          579 MCMCOBJ=   -6620.75660444193     
 iteration          580 MCMCOBJ=   -6645.23906498431     
 iteration          581 MCMCOBJ=   -6644.84092300598     
 iteration          582 MCMCOBJ=   -6644.33938095293     
 iteration          583 MCMCOBJ=   -6679.24144605236     
 iteration          584 MCMCOBJ=   -6653.28643940913     
 iteration          585 MCMCOBJ=   -6650.87285314173     
 iteration          586 MCMCOBJ=   -6655.01940945720     
 iteration          587 MCMCOBJ=   -6624.68104253277     
 iteration          588 MCMCOBJ=   -6676.67520170369     
 iteration          589 MCMCOBJ=   -6597.17235655729     
 iteration          590 MCMCOBJ=   -6578.40413878250     
 iteration          591 MCMCOBJ=   -6639.22310229676     
 iteration          592 MCMCOBJ=   -6622.12358037431     
 iteration          593 MCMCOBJ=   -6668.32795252709     
 iteration          594 MCMCOBJ=   -6649.35744785715     
 iteration          595 MCMCOBJ=   -6588.23201513862     
 iteration          596 MCMCOBJ=   -6648.03514883999     
 iteration          597 MCMCOBJ=   -6601.24840926997     
 iteration          598 MCMCOBJ=   -6676.90583758121     
 iteration          599 MCMCOBJ=   -6656.91647652891     
 iteration          600 MCMCOBJ=   -6657.34263239417     
 iteration          601 MCMCOBJ=   -6659.95829546607     
 iteration          602 MCMCOBJ=   -6633.57942745881     
 iteration          603 MCMCOBJ=   -6672.11553342639     
 iteration          604 MCMCOBJ=   -6656.11320168771     
 iteration          605 MCMCOBJ=   -6668.29687554786     
 iteration          606 MCMCOBJ=   -6597.72228075577     
 iteration          607 MCMCOBJ=   -6658.86742356107     
 iteration          608 MCMCOBJ=   -6662.87994875536     
 iteration          609 MCMCOBJ=   -6660.90100290032     
 iteration          610 MCMCOBJ=   -6616.61628648373     
 iteration          611 MCMCOBJ=   -6597.96026569984     
 iteration          612 MCMCOBJ=   -6599.37171814134     
 iteration          613 MCMCOBJ=   -6655.51171231655     
 iteration          614 MCMCOBJ=   -6638.83089843866     
 iteration          615 MCMCOBJ=   -6735.24250334576     
 iteration          616 MCMCOBJ=   -6675.51754462997     
 iteration          617 MCMCOBJ=   -6670.48244993586     
 iteration          618 MCMCOBJ=   -6646.87635621543     
 iteration          619 MCMCOBJ=   -6671.67104584850     
 iteration          620 MCMCOBJ=   -6654.11297669640     
 iteration          621 MCMCOBJ=   -6604.71793145743     
 iteration          622 MCMCOBJ=   -6565.10513107279     
 iteration          623 MCMCOBJ=   -6624.20220418284     
 iteration          624 MCMCOBJ=   -6595.02759515721     
 iteration          625 MCMCOBJ=   -6612.76909372318     
 iteration          626 MCMCOBJ=   -6626.61166601183     
 iteration          627 MCMCOBJ=   -6628.80233829063     
 iteration          628 MCMCOBJ=   -6621.45976992879     
 iteration          629 MCMCOBJ=   -6655.79404958555     
 iteration          630 MCMCOBJ=   -6659.00542380333     
 iteration          631 MCMCOBJ=   -6632.86646401769     
 iteration          632 MCMCOBJ=   -6669.39138562410     
 iteration          633 MCMCOBJ=   -6668.04751701249     
 iteration          634 MCMCOBJ=   -6655.06945833189     
 iteration          635 MCMCOBJ=   -6642.35382736038     
 iteration          636 MCMCOBJ=   -6596.97921120605     
 iteration          637 MCMCOBJ=   -6606.83394250994     
 iteration          638 MCMCOBJ=   -6604.08811438408     
 iteration          639 MCMCOBJ=   -6612.81326847436     
 iteration          640 MCMCOBJ=   -6628.89167032159     
 iteration          641 MCMCOBJ=   -6625.18530033955     
 iteration          642 MCMCOBJ=   -6615.78826868467     
 iteration          643 MCMCOBJ=   -6667.51173342321     
 iteration          644 MCMCOBJ=   -6672.76565118848     
 iteration          645 MCMCOBJ=   -6672.44974336598     
 iteration          646 MCMCOBJ=   -6669.02217819714     
 iteration          647 MCMCOBJ=   -6617.58117885394     
 iteration          648 MCMCOBJ=   -6638.86177374674     
 iteration          649 MCMCOBJ=   -6581.34791221720     
 iteration          650 MCMCOBJ=   -6564.03068838233     
 iteration          651 MCMCOBJ=   -6611.14681651982     
 iteration          652 MCMCOBJ=   -6627.69523074659     
 iteration          653 MCMCOBJ=   -6614.60779893498     
 iteration          654 MCMCOBJ=   -6591.88340277249     
 iteration          655 MCMCOBJ=   -6496.20070326175     
 iteration          656 MCMCOBJ=   -6541.67588220802     
 iteration          657 MCMCOBJ=   -6535.23137096740     
 iteration          658 MCMCOBJ=   -6641.29937833925     
 iteration          659 MCMCOBJ=   -6597.74079240173     
 iteration          660 MCMCOBJ=   -6593.50422244828     
 iteration          661 MCMCOBJ=   -6613.36581210713     
 iteration          662 MCMCOBJ=   -6629.70365649361     
 iteration          663 MCMCOBJ=   -6612.19977409836     
 iteration          664 MCMCOBJ=   -6617.81805217921     
 iteration          665 MCMCOBJ=   -6568.79685337954     
 iteration          666 MCMCOBJ=   -6568.79685331907     
 iteration          667 MCMCOBJ=   -6670.90509629513     
 iteration          668 MCMCOBJ=   -6664.91829030068     
 iteration          669 MCMCOBJ=   -6623.30750849924     
 iteration          670 MCMCOBJ=   -6651.50086559906     
 iteration          671 MCMCOBJ=   -6697.22490123427     
 iteration          672 MCMCOBJ=   -6679.73417556478     
 iteration          673 MCMCOBJ=   -6654.98075204026     
 iteration          674 MCMCOBJ=   -6589.18784688019     
 iteration          675 MCMCOBJ=   -6584.65843897573     
 iteration          676 MCMCOBJ=   -6605.24024681446     
 iteration          677 MCMCOBJ=   -6595.45483348816     
 iteration          678 MCMCOBJ=   -6660.90440169148     
 iteration          679 MCMCOBJ=   -6654.88397268270     
 iteration          680 MCMCOBJ=   -6671.51205379743     
 iteration          681 MCMCOBJ=   -6668.65034126380     
 iteration          682 MCMCOBJ=   -6631.29232789199     
 iteration          683 MCMCOBJ=   -6654.24206154536     
 iteration          684 MCMCOBJ=   -6651.94653142279     
 iteration          685 MCMCOBJ=   -6643.98284041695     
 iteration          686 MCMCOBJ=   -6668.08787976155     
 iteration          687 MCMCOBJ=   -6635.17618494594     
 iteration          688 MCMCOBJ=   -6631.05583026541     
 iteration          689 MCMCOBJ=   -6642.10130412246     
 iteration          690 MCMCOBJ=   -6603.56236595453     
 iteration          691 MCMCOBJ=   -6615.70138475548     
 iteration          692 MCMCOBJ=   -6628.81784941160     
 iteration          693 MCMCOBJ=   -6647.17398834939     
 iteration          694 MCMCOBJ=   -6644.25584194588     
 iteration          695 MCMCOBJ=   -6589.26885048288     
 iteration          696 MCMCOBJ=   -6679.59284723436     
 iteration          697 MCMCOBJ=   -6636.68898855883     
 iteration          698 MCMCOBJ=   -6660.75004700175     
 iteration          699 MCMCOBJ=   -6631.26307028096     
 iteration          700 MCMCOBJ=   -6606.68544087356     
 iteration          701 MCMCOBJ=   -6588.98201113560     
 iteration          702 MCMCOBJ=   -6616.34363801236     
 iteration          703 MCMCOBJ=   -6619.55119842764     
 iteration          704 MCMCOBJ=   -6601.62226078971     
 iteration          705 MCMCOBJ=   -6602.73951171331     
 iteration          706 MCMCOBJ=   -6606.80763733491     
 iteration          707 MCMCOBJ=   -6630.64908235826     
 iteration          708 MCMCOBJ=   -6640.05685490448     
 iteration          709 MCMCOBJ=   -6606.98008967212     
 iteration          710 MCMCOBJ=   -6643.28842731499     
 iteration          711 MCMCOBJ=   -6657.02708412994     
 iteration          712 MCMCOBJ=   -6651.52180885026     
 iteration          713 MCMCOBJ=   -6690.17774020433     
 iteration          714 MCMCOBJ=   -6663.65926043630     
 iteration          715 MCMCOBJ=   -6651.15746670394     
 iteration          716 MCMCOBJ=   -6611.20569631693     
 iteration          717 MCMCOBJ=   -6650.67752939324     
 iteration          718 MCMCOBJ=   -6656.90306025098     
 iteration          719 MCMCOBJ=   -6651.66503503284     
 iteration          720 MCMCOBJ=   -6651.66502440935     
 iteration          721 MCMCOBJ=   -6650.00540340613     
 iteration          722 MCMCOBJ=   -6629.40856723564     
 iteration          723 MCMCOBJ=   -6602.64115594470     
 iteration          724 MCMCOBJ=   -6572.41202740110     
 iteration          725 MCMCOBJ=   -6619.37986228579     
 iteration          726 MCMCOBJ=   -6653.19216740291     
 iteration          727 MCMCOBJ=   -6693.39844088000     
 iteration          728 MCMCOBJ=   -6693.39844319544     
 iteration          729 MCMCOBJ=   -6679.43931594030     
 iteration          730 MCMCOBJ=   -6647.43019064513     
 iteration          731 MCMCOBJ=   -6685.02701205665     
 iteration          732 MCMCOBJ=   -6608.07834806646     
 iteration          733 MCMCOBJ=   -6628.66743918768     
 iteration          734 MCMCOBJ=   -6626.27431200147     
 iteration          735 MCMCOBJ=   -6590.54654122724     
 iteration          736 MCMCOBJ=   -6592.10829993428     
 iteration          737 MCMCOBJ=   -6577.59548560571     
 iteration          738 MCMCOBJ=   -6572.01388778234     
 iteration          739 MCMCOBJ=   -6583.37785201464     
 iteration          740 MCMCOBJ=   -6649.62364007877     
 iteration          741 MCMCOBJ=   -6631.49457830258     
 iteration          742 MCMCOBJ=   -6657.01684221007     
 iteration          743 MCMCOBJ=   -6676.65855455248     
 iteration          744 MCMCOBJ=   -6652.94123263849     
 iteration          745 MCMCOBJ=   -6623.64467013113     
 iteration          746 MCMCOBJ=   -6627.81682804842     
 iteration          747 MCMCOBJ=   -6629.92279923781     
 iteration          748 MCMCOBJ=   -6629.69862199354     
 iteration          749 MCMCOBJ=   -6658.81879932241     
 iteration          750 MCMCOBJ=   -6639.56077620386     
 iteration          751 MCMCOBJ=   -6642.96381993467     
 iteration          752 MCMCOBJ=   -6597.03687576984     
 iteration          753 MCMCOBJ=   -6601.23874816004     
 iteration          754 MCMCOBJ=   -6625.53127405710     
 iteration          755 MCMCOBJ=   -6654.34124762908     
 iteration          756 MCMCOBJ=   -6633.39022523572     
 iteration          757 MCMCOBJ=   -6626.87472911546     
 iteration          758 MCMCOBJ=   -6642.28613242588     
 iteration          759 MCMCOBJ=   -6650.21361962494     
 iteration          760 MCMCOBJ=   -6645.04004109150     
 iteration          761 MCMCOBJ=   -6643.34165676432     
 iteration          762 MCMCOBJ=   -6663.86250935547     
 iteration          763 MCMCOBJ=   -6686.22775140485     
 iteration          764 MCMCOBJ=   -6624.69526446385     
 iteration          765 MCMCOBJ=   -6645.14315701352     
 iteration          766 MCMCOBJ=   -6638.29047082888     
 iteration          767 MCMCOBJ=   -6629.50519834587     
 iteration          768 MCMCOBJ=   -6610.15263890816     
 iteration          769 MCMCOBJ=   -6565.32507085239     
 iteration          770 MCMCOBJ=   -6584.02591990298     
 iteration          771 MCMCOBJ=   -6615.11839975088     
 iteration          772 MCMCOBJ=   -6602.82022177008     
 iteration          773 MCMCOBJ=   -6596.33755226900     
 iteration          774 MCMCOBJ=   -6577.74038279308     
 iteration          775 MCMCOBJ=   -6578.10993869333     
 iteration          776 MCMCOBJ=   -6561.20564938131     
 iteration          777 MCMCOBJ=   -6561.80465159526     
 iteration          778 MCMCOBJ=   -6561.69806257161     
 iteration          779 MCMCOBJ=   -6625.05447289553     
 iteration          780 MCMCOBJ=   -6627.67151352361     
 iteration          781 MCMCOBJ=   -6602.17389309483     
 iteration          782 MCMCOBJ=   -6602.46684711501     
 iteration          783 MCMCOBJ=   -6624.52467460063     
 iteration          784 MCMCOBJ=   -6654.05546779609     
 iteration          785 MCMCOBJ=   -6658.44258194070     
 iteration          786 MCMCOBJ=   -6658.95487572895     
 iteration          787 MCMCOBJ=   -6624.05062336072     
 iteration          788 MCMCOBJ=   -6608.87075922870     
 iteration          789 MCMCOBJ=   -6598.33640610174     
 iteration          790 MCMCOBJ=   -6593.49375332141     
 iteration          791 MCMCOBJ=   -6643.06088100964     
 iteration          792 MCMCOBJ=   -6596.38301070336     
 iteration          793 MCMCOBJ=   -6634.27842661857     
 iteration          794 MCMCOBJ=   -6600.87921943763     
 iteration          795 MCMCOBJ=   -6612.23985919934     
 iteration          796 MCMCOBJ=   -6620.61915332186     
 iteration          797 MCMCOBJ=   -6566.87777199360     
 iteration          798 MCMCOBJ=   -6583.25383738064     
 iteration          799 MCMCOBJ=   -6562.85400596799     
 iteration          800 MCMCOBJ=   -6622.11608256445     
 iteration          801 MCMCOBJ=   -6601.56487030577     
 iteration          802 MCMCOBJ=   -6602.91676428314     
 iteration          803 MCMCOBJ=   -6663.01287263508     
 iteration          804 MCMCOBJ=   -6657.94759620615     
 iteration          805 MCMCOBJ=   -6638.68069085320     
 iteration          806 MCMCOBJ=   -6577.04663500237     
 iteration          807 MCMCOBJ=   -6596.14423332423     
 iteration          808 MCMCOBJ=   -6596.14423261005     
 iteration          809 MCMCOBJ=   -6619.02084249200     
 iteration          810 MCMCOBJ=   -6659.09329626370     
 iteration          811 MCMCOBJ=   -6615.11227465218     
 iteration          812 MCMCOBJ=   -6614.11995921708     
 iteration          813 MCMCOBJ=   -6602.64043072369     
 iteration          814 MCMCOBJ=   -6660.51565422160     
 iteration          815 MCMCOBJ=   -6660.51565372205     
 iteration          816 MCMCOBJ=   -6658.61585317381     
 iteration          817 MCMCOBJ=   -6641.48319728337     
 iteration          818 MCMCOBJ=   -6613.47087263844     
 iteration          819 MCMCOBJ=   -6598.80146581516     
 iteration          820 MCMCOBJ=   -6638.60218853913     
 iteration          821 MCMCOBJ=   -6607.59244735255     
 iteration          822 MCMCOBJ=   -6612.66758706703     
 iteration          823 MCMCOBJ=   -6655.26429774455     
 iteration          824 MCMCOBJ=   -6655.26429787162     
 iteration          825 MCMCOBJ=   -6672.93290381912     
 iteration          826 MCMCOBJ=   -6636.27709439781     
 iteration          827 MCMCOBJ=   -6642.78816550181     
 iteration          828 MCMCOBJ=   -6625.44850878793     
 iteration          829 MCMCOBJ=   -6631.73931540342     
 iteration          830 MCMCOBJ=   -6576.14731794544     
 iteration          831 MCMCOBJ=   -6579.36772597168     
 iteration          832 MCMCOBJ=   -6630.99380232685     
 iteration          833 MCMCOBJ=   -6592.49032853251     
 iteration          834 MCMCOBJ=   -6594.15585347433     
 iteration          835 MCMCOBJ=   -6631.42687858140     
 iteration          836 MCMCOBJ=   -6609.58547626843     
 iteration          837 MCMCOBJ=   -6583.33606075806     
 iteration          838 MCMCOBJ=   -6570.75135297787     
 iteration          839 MCMCOBJ=   -6541.22889508522     
 iteration          840 MCMCOBJ=   -6573.80872668959     
 iteration          841 MCMCOBJ=   -6638.22883243619     
 iteration          842 MCMCOBJ=   -6602.48565883394     
 iteration          843 MCMCOBJ=   -6546.69594172869     
 iteration          844 MCMCOBJ=   -6583.24574423642     
 iteration          845 MCMCOBJ=   -6621.34826910068     
 iteration          846 MCMCOBJ=   -6608.77308935829     
 iteration          847 MCMCOBJ=   -6649.17660982008     
 iteration          848 MCMCOBJ=   -6680.29791639814     
 iteration          849 MCMCOBJ=   -6647.66584826942     
 iteration          850 MCMCOBJ=   -6648.81490350052     
 iteration          851 MCMCOBJ=   -6684.06894427063     
 iteration          852 MCMCOBJ=   -6643.67810436703     
 iteration          853 MCMCOBJ=   -6644.95106596722     
 iteration          854 MCMCOBJ=   -6704.84976713971     
 iteration          855 MCMCOBJ=   -6648.93970864162     
 iteration          856 MCMCOBJ=   -6648.11178743353     
 iteration          857 MCMCOBJ=   -6645.70279201262     
 iteration          858 MCMCOBJ=   -6677.61656637384     
 iteration          859 MCMCOBJ=   -6609.59956848786     
 iteration          860 MCMCOBJ=   -6646.35065224377     
 iteration          861 MCMCOBJ=   -6646.35065230099     
 iteration          862 MCMCOBJ=   -6627.61159448235     
 iteration          863 MCMCOBJ=   -6686.36808773651     
 iteration          864 MCMCOBJ=   -6672.88818401478     
 iteration          865 MCMCOBJ=   -6674.32422298022     
 iteration          866 MCMCOBJ=   -6561.92472237167     
 iteration          867 MCMCOBJ=   -6553.75084359489     
 iteration          868 MCMCOBJ=   -6572.96181940827     
 iteration          869 MCMCOBJ=   -6580.24747323839     
 iteration          870 MCMCOBJ=   -6601.49524886700     
 iteration          871 MCMCOBJ=   -6565.63990090307     
 iteration          872 MCMCOBJ=   -6621.62754639230     
 iteration          873 MCMCOBJ=   -6611.90592583972     
 iteration          874 MCMCOBJ=   -6607.76980320743     
 iteration          875 MCMCOBJ=   -6642.99945998634     
 iteration          876 MCMCOBJ=   -6645.98867173005     
 iteration          877 MCMCOBJ=   -6618.72674868367     
 iteration          878 MCMCOBJ=   -6619.33049451714     
 iteration          879 MCMCOBJ=   -6620.93306318769     
 iteration          880 MCMCOBJ=   -6652.15146316683     
 iteration          881 MCMCOBJ=   -6658.70709040943     
 iteration          882 MCMCOBJ=   -6644.80875939860     
 iteration          883 MCMCOBJ=   -6634.64933144204     
 iteration          884 MCMCOBJ=   -6629.74929457965     
 iteration          885 MCMCOBJ=   -6631.22409790800     
 iteration          886 MCMCOBJ=   -6623.93790637528     
 iteration          887 MCMCOBJ=   -6598.38659215372     
 iteration          888 MCMCOBJ=   -6664.99294139056     
 iteration          889 MCMCOBJ=   -6648.52251152678     
 iteration          890 MCMCOBJ=   -6628.44872204431     
 iteration          891 MCMCOBJ=   -6632.75266933008     
 iteration          892 MCMCOBJ=   -6609.56028822322     
 iteration          893 MCMCOBJ=   -6597.74145974603     
 iteration          894 MCMCOBJ=   -6625.28981155216     
 iteration          895 MCMCOBJ=   -6654.25169144771     
 iteration          896 MCMCOBJ=   -6609.90656336845     
 iteration          897 MCMCOBJ=   -6610.07884792082     
 iteration          898 MCMCOBJ=   -6636.00882529909     
 iteration          899 MCMCOBJ=   -6636.00880212248     
 iteration          900 MCMCOBJ=   -6639.18930261102     
 iteration          901 MCMCOBJ=   -6646.80625628213     
 iteration          902 MCMCOBJ=   -6581.51071282904     
 iteration          903 MCMCOBJ=   -6623.97523961562     
 iteration          904 MCMCOBJ=   -6614.63905436702     
 iteration          905 MCMCOBJ=   -6636.98781345726     
 iteration          906 MCMCOBJ=   -6650.01735596519     
 iteration          907 MCMCOBJ=   -6614.92748148151     
 iteration          908 MCMCOBJ=   -6632.96302491827     
 iteration          909 MCMCOBJ=   -6597.35897523029     
 iteration          910 MCMCOBJ=   -6606.58734941710     
 iteration          911 MCMCOBJ=   -6612.10092467986     
 iteration          912 MCMCOBJ=   -6615.96203461541     
 iteration          913 MCMCOBJ=   -6636.74493127476     
 iteration          914 MCMCOBJ=   -6654.76906124415     
 iteration          915 MCMCOBJ=   -6665.44123526900     
 iteration          916 MCMCOBJ=   -6655.53969300900     
 iteration          917 MCMCOBJ=   -6664.57491697459     
 iteration          918 MCMCOBJ=   -6572.54582315685     
 iteration          919 MCMCOBJ=   -6661.13274404703     
 iteration          920 MCMCOBJ=   -6658.33370938368     
 iteration          921 MCMCOBJ=   -6612.58703363056     
 iteration          922 MCMCOBJ=   -6685.43020627673     
 iteration          923 MCMCOBJ=   -6699.62121303641     
 iteration          924 MCMCOBJ=   -6659.13572552277     
 iteration          925 MCMCOBJ=   -6605.10504541546     
 iteration          926 MCMCOBJ=   -6634.42376998195     
 iteration          927 MCMCOBJ=   -6652.71703487399     
 iteration          928 MCMCOBJ=   -6630.90067842407     
 iteration          929 MCMCOBJ=   -6619.93470848446     
 iteration          930 MCMCOBJ=   -6556.58778133919     
 iteration          931 MCMCOBJ=   -6536.67474592017     
 iteration          932 MCMCOBJ=   -6555.76807086633     
 iteration          933 MCMCOBJ=   -6604.46905392132     
 iteration          934 MCMCOBJ=   -6607.21851990819     
 iteration          935 MCMCOBJ=   -6586.27029274079     
 iteration          936 MCMCOBJ=   -6596.33044048637     
 iteration          937 MCMCOBJ=   -6609.87484218920     
 iteration          938 MCMCOBJ=   -6626.38570680916     
 iteration          939 MCMCOBJ=   -6628.07717768864     
 iteration          940 MCMCOBJ=   -6602.04579035839     
 iteration          941 MCMCOBJ=   -6637.44492376472     
 iteration          942 MCMCOBJ=   -6640.03324436371     
 iteration          943 MCMCOBJ=   -6660.33451358377     
 iteration          944 MCMCOBJ=   -6655.77169685598     
 iteration          945 MCMCOBJ=   -6664.40437389306     
 iteration          946 MCMCOBJ=   -6669.67341438021     
 iteration          947 MCMCOBJ=   -6672.11335625218     
 iteration          948 MCMCOBJ=   -6668.36429648699     
 iteration          949 MCMCOBJ=   -6710.41294411750     
 iteration          950 MCMCOBJ=   -6695.76165471528     
 iteration          951 MCMCOBJ=   -6641.89584803980     
 iteration          952 MCMCOBJ=   -6649.02302259868     
 iteration          953 MCMCOBJ=   -6648.51638633648     
 iteration          954 MCMCOBJ=   -6681.10321983361     
 iteration          955 MCMCOBJ=   -6687.65395691848     
 iteration          956 MCMCOBJ=   -6618.28600473005     
 iteration          957 MCMCOBJ=   -6618.28600565580     
 iteration          958 MCMCOBJ=   -6721.74726729871     
 iteration          959 MCMCOBJ=   -6709.62749785033     
 iteration          960 MCMCOBJ=   -6664.65056217062     
 iteration          961 MCMCOBJ=   -6632.64500303591     
 iteration          962 MCMCOBJ=   -6577.25688637108     
 iteration          963 MCMCOBJ=   -6568.80744489867     
 iteration          964 MCMCOBJ=   -6648.46341375317     
 iteration          965 MCMCOBJ=   -6644.60978783892     
 iteration          966 MCMCOBJ=   -6607.37880446478     
 iteration          967 MCMCOBJ=   -6623.07385289718     
 iteration          968 MCMCOBJ=   -6669.57249240249     
 iteration          969 MCMCOBJ=   -6631.15729119692     
 iteration          970 MCMCOBJ=   -6619.45904400069     
 iteration          971 MCMCOBJ=   -6641.63652226603     
 iteration          972 MCMCOBJ=   -6656.96810619772     
 iteration          973 MCMCOBJ=   -6636.86779439212     
 iteration          974 MCMCOBJ=   -6613.05091933217     
 iteration          975 MCMCOBJ=   -6597.91520952815     
 iteration          976 MCMCOBJ=   -6644.67006655251     
 iteration          977 MCMCOBJ=   -6686.91687642907     
 iteration          978 MCMCOBJ=   -6688.83725487910     
 iteration          979 MCMCOBJ=   -6666.38749857395     
 iteration          980 MCMCOBJ=   -6668.04562444131     
 iteration          981 MCMCOBJ=   -6647.11633098266     
 iteration          982 MCMCOBJ=   -6672.16594827866     
 iteration          983 MCMCOBJ=   -6610.30714456076     
 iteration          984 MCMCOBJ=   -6628.01777881444     
 iteration          985 MCMCOBJ=   -6648.71243760606     
 iteration          986 MCMCOBJ=   -6657.64337380573     
 iteration          987 MCMCOBJ=   -6665.75264027573     
 iteration          988 MCMCOBJ=   -6631.75348883721     
 iteration          989 MCMCOBJ=   -6612.29823209818     
 iteration          990 MCMCOBJ=   -6643.70542415659     
 iteration          991 MCMCOBJ=   -6599.92901059603     
 iteration          992 MCMCOBJ=   -6617.28598257372     
 iteration          993 MCMCOBJ=   -6659.73485843596     
 iteration          994 MCMCOBJ=   -6615.06115899345     
 iteration          995 MCMCOBJ=   -6638.45688732585     
 iteration          996 MCMCOBJ=   -6660.31018929447     
 iteration          997 MCMCOBJ=   -6672.02511693037     
 iteration          998 MCMCOBJ=   -6653.66479533995     
 iteration          999 MCMCOBJ=   -6645.72730113850     
 iteration         1000 MCMCOBJ=   -6733.61953833233     
 iteration         1001 MCMCOBJ=   -6740.20312112960     
 iteration         1002 MCMCOBJ=   -6685.76481132979     
 iteration         1003 MCMCOBJ=   -6695.35910460131     
 iteration         1004 MCMCOBJ=   -6673.40809011358     
 iteration         1005 MCMCOBJ=   -6603.02110424401     
 iteration         1006 MCMCOBJ=   -6622.81404467367     
 iteration         1007 MCMCOBJ=   -6691.41842494928     
 iteration         1008 MCMCOBJ=   -6655.74393977765     
 iteration         1009 MCMCOBJ=   -6608.04640784079     
 iteration         1010 MCMCOBJ=   -6667.43934533936     
 iteration         1011 MCMCOBJ=   -6662.90056245159     
 iteration         1012 MCMCOBJ=   -6663.06062700416     
 iteration         1013 MCMCOBJ=   -6640.40385430733     
 iteration         1014 MCMCOBJ=   -6627.82240052083     
 iteration         1015 MCMCOBJ=   -6652.25579571710     
 iteration         1016 MCMCOBJ=   -6601.61943939425     
 iteration         1017 MCMCOBJ=   -6638.77615485385     
 iteration         1018 MCMCOBJ=   -6668.58040577753     
 iteration         1019 MCMCOBJ=   -6659.77377384247     
 iteration         1020 MCMCOBJ=   -6629.83912400164     
 iteration         1021 MCMCOBJ=   -6651.04245437190     
 iteration         1022 MCMCOBJ=   -6660.70734662490     
 iteration         1023 MCMCOBJ=   -6685.05453230459     
 iteration         1024 MCMCOBJ=   -6652.81162371430     
 iteration         1025 MCMCOBJ=   -6629.43772839910     
 iteration         1026 MCMCOBJ=   -6652.37141715644     
 iteration         1027 MCMCOBJ=   -6675.04193056375     
 iteration         1028 MCMCOBJ=   -6645.72226220210     
 iteration         1029 MCMCOBJ=   -6570.19686545013     
 iteration         1030 MCMCOBJ=   -6648.91108281131     
 iteration         1031 MCMCOBJ=   -6633.46590191185     
 iteration         1032 MCMCOBJ=   -6649.45067286080     
 iteration         1033 MCMCOBJ=   -6692.16683733636     
 iteration         1034 MCMCOBJ=   -6652.40160806466     
 iteration         1035 MCMCOBJ=   -6653.36183123365     
 iteration         1036 MCMCOBJ=   -6688.08376277831     
 iteration         1037 MCMCOBJ=   -6645.16991154281     
 iteration         1038 MCMCOBJ=   -6658.92675302929     
 iteration         1039 MCMCOBJ=   -6706.88464360604     
 iteration         1040 MCMCOBJ=   -6691.32457800507     
 iteration         1041 MCMCOBJ=   -6676.36411911563     
 iteration         1042 MCMCOBJ=   -6639.45567274048     
 iteration         1043 MCMCOBJ=   -6675.68667785056     
 iteration         1044 MCMCOBJ=   -6669.66898644707     
 iteration         1045 MCMCOBJ=   -6696.30614289191     
 iteration         1046 MCMCOBJ=   -6651.69013697397     
 iteration         1047 MCMCOBJ=   -6684.80844515969     
 iteration         1048 MCMCOBJ=   -6695.17013044905     
 iteration         1049 MCMCOBJ=   -6630.65946578365     
 iteration         1050 MCMCOBJ=   -6611.94219438428     
 iteration         1051 MCMCOBJ=   -6635.60083779689     
 iteration         1052 MCMCOBJ=   -6665.87241239076     
 iteration         1053 MCMCOBJ=   -6669.87462673325     
 iteration         1054 MCMCOBJ=   -6615.27270791474     
 iteration         1055 MCMCOBJ=   -6678.97858725918     
 iteration         1056 MCMCOBJ=   -6664.18164954656     
 iteration         1057 MCMCOBJ=   -6635.79499066787     
 iteration         1058 MCMCOBJ=   -6668.08740347047     
 iteration         1059 MCMCOBJ=   -6674.83319366115     
 iteration         1060 MCMCOBJ=   -6695.21944631938     
 iteration         1061 MCMCOBJ=   -6665.26757649037     
 iteration         1062 MCMCOBJ=   -6685.65834333687     
 iteration         1063 MCMCOBJ=   -6688.60748078002     
 iteration         1064 MCMCOBJ=   -6652.86649454576     
 iteration         1065 MCMCOBJ=   -6648.82787257590     
 iteration         1066 MCMCOBJ=   -6638.81776139690     
 iteration         1067 MCMCOBJ=   -6682.33672723285     
 iteration         1068 MCMCOBJ=   -6667.30251516610     
 iteration         1069 MCMCOBJ=   -6631.22288267319     
 iteration         1070 MCMCOBJ=   -6653.31631322671     
 iteration         1071 MCMCOBJ=   -6640.50125164430     
 iteration         1072 MCMCOBJ=   -6614.47365475262     
 iteration         1073 MCMCOBJ=   -6615.88759024430     
 iteration         1074 MCMCOBJ=   -6652.19972097753     
 iteration         1075 MCMCOBJ=   -6592.58617450257     
 iteration         1076 MCMCOBJ=   -6579.42981305745     
 iteration         1077 MCMCOBJ=   -6602.53287513842     
 iteration         1078 MCMCOBJ=   -6605.01177082810     
 iteration         1079 MCMCOBJ=   -6643.18814121788     
 iteration         1080 MCMCOBJ=   -6654.09336828662     
 iteration         1081 MCMCOBJ=   -6599.54799590666     
 iteration         1082 MCMCOBJ=   -6646.87729181768     
 iteration         1083 MCMCOBJ=   -6630.89039892839     
 iteration         1084 MCMCOBJ=   -6616.75830041539     
 iteration         1085 MCMCOBJ=   -6614.28840085666     
 iteration         1086 MCMCOBJ=   -6622.39337655804     
 iteration         1087 MCMCOBJ=   -6617.78126704573     
 iteration         1088 MCMCOBJ=   -6628.60511218275     
 iteration         1089 MCMCOBJ=   -6703.36398834044     
 iteration         1090 MCMCOBJ=   -6672.66992692834     
 iteration         1091 MCMCOBJ=   -6642.34518650995     
 iteration         1092 MCMCOBJ=   -6641.64190182893     
 iteration         1093 MCMCOBJ=   -6660.95594840803     
 iteration         1094 MCMCOBJ=   -6641.27471381707     
 iteration         1095 MCMCOBJ=   -6619.43794205843     
 iteration         1096 MCMCOBJ=   -6620.32007326222     
 iteration         1097 MCMCOBJ=   -6613.00557641694     
 iteration         1098 MCMCOBJ=   -6626.96202083609     
 iteration         1099 MCMCOBJ=   -6591.56395172790     
 iteration         1100 MCMCOBJ=   -6667.87580723152     
 iteration         1101 MCMCOBJ=   -6670.26891109819     
 iteration         1102 MCMCOBJ=   -6691.40003880886     
 iteration         1103 MCMCOBJ=   -6698.75474553843     
 iteration         1104 MCMCOBJ=   -6663.67304143464     
 iteration         1105 MCMCOBJ=   -6663.67304845071     
 iteration         1106 MCMCOBJ=   -6666.04791671510     
 iteration         1107 MCMCOBJ=   -6641.86908537012     
 iteration         1108 MCMCOBJ=   -6637.10931196238     
 iteration         1109 MCMCOBJ=   -6609.98734946376     
 iteration         1110 MCMCOBJ=   -6611.21521936970     
 iteration         1111 MCMCOBJ=   -6631.30356719033     
 iteration         1112 MCMCOBJ=   -6661.31442049383     
 iteration         1113 MCMCOBJ=   -6638.60719300449     
 iteration         1114 MCMCOBJ=   -6644.31961103764     
 iteration         1115 MCMCOBJ=   -6650.39753345563     
 iteration         1116 MCMCOBJ=   -6652.87568886631     
 iteration         1117 MCMCOBJ=   -6626.49998493525     
 iteration         1118 MCMCOBJ=   -6644.67646025967     
 iteration         1119 MCMCOBJ=   -6644.67646076019     
 iteration         1120 MCMCOBJ=   -6616.10611437715     
 iteration         1121 MCMCOBJ=   -6632.09528604371     
 iteration         1122 MCMCOBJ=   -6563.18492165097     
 iteration         1123 MCMCOBJ=   -6588.57230703503     
 iteration         1124 MCMCOBJ=   -6603.75646941447     
 iteration         1125 MCMCOBJ=   -6577.29226182186     
 iteration         1126 MCMCOBJ=   -6609.76609785009     
 iteration         1127 MCMCOBJ=   -6573.19579876289     
 iteration         1128 MCMCOBJ=   -6528.87347438373     
 iteration         1129 MCMCOBJ=   -6592.92106461500     
 iteration         1130 MCMCOBJ=   -6613.85677755519     
 iteration         1131 MCMCOBJ=   -6635.91766495984     
 iteration         1132 MCMCOBJ=   -6633.93859349994     
 iteration         1133 MCMCOBJ=   -6650.39462314424     
 iteration         1134 MCMCOBJ=   -6659.63644050976     
 iteration         1135 MCMCOBJ=   -6683.73018040215     
 iteration         1136 MCMCOBJ=   -6666.26735889474     
 iteration         1137 MCMCOBJ=   -6651.76485876124     
 iteration         1138 MCMCOBJ=   -6651.76486434186     
 iteration         1139 MCMCOBJ=   -6651.76482363846     
 iteration         1140 MCMCOBJ=   -6673.19340770162     
 iteration         1141 MCMCOBJ=   -6640.88247921592     
 iteration         1142 MCMCOBJ=   -6551.13591069575     
 iteration         1143 MCMCOBJ=   -6552.60422513403     
 iteration         1144 MCMCOBJ=   -6573.01714614006     
 iteration         1145 MCMCOBJ=   -6584.93226100983     
 iteration         1146 MCMCOBJ=   -6556.69216974621     
 iteration         1147 MCMCOBJ=   -6593.71329923873     
 iteration         1148 MCMCOBJ=   -6688.73372449529     
 iteration         1149 MCMCOBJ=   -6644.94865492464     
 iteration         1150 MCMCOBJ=   -6603.51603041874     
 iteration         1151 MCMCOBJ=   -6618.21390220383     
 iteration         1152 MCMCOBJ=   -6635.28240092525     
 iteration         1153 MCMCOBJ=   -6615.04170415983     
 iteration         1154 MCMCOBJ=   -6601.57581847887     
 iteration         1155 MCMCOBJ=   -6636.96523448178     
 iteration         1156 MCMCOBJ=   -6636.96522259490     
 iteration         1157 MCMCOBJ=   -6669.76911403997     
 iteration         1158 MCMCOBJ=   -6645.36137047080     
 iteration         1159 MCMCOBJ=   -6635.51351994547     
 iteration         1160 MCMCOBJ=   -6582.89071228343     
 iteration         1161 MCMCOBJ=   -6623.51005186521     
 iteration         1162 MCMCOBJ=   -6614.74254482636     
 iteration         1163 MCMCOBJ=   -6603.14478349871     
 iteration         1164 MCMCOBJ=   -6618.59429278989     
 iteration         1165 MCMCOBJ=   -6604.56793480654     
 iteration         1166 MCMCOBJ=   -6604.60988926106     
 iteration         1167 MCMCOBJ=   -6552.86495324942     
 iteration         1168 MCMCOBJ=   -6572.83944845356     
 iteration         1169 MCMCOBJ=   -6664.66220867453     
 iteration         1170 MCMCOBJ=   -6650.96043118856     
 iteration         1171 MCMCOBJ=   -6674.00485556282     
 iteration         1172 MCMCOBJ=   -6596.92421163766     
 iteration         1173 MCMCOBJ=   -6676.06814336353     
 iteration         1174 MCMCOBJ=   -6662.15560766667     
 iteration         1175 MCMCOBJ=   -6705.21741551276     
 iteration         1176 MCMCOBJ=   -6703.86325158291     
 iteration         1177 MCMCOBJ=   -6656.77540858412     
 iteration         1178 MCMCOBJ=   -6676.64918743393     
 iteration         1179 MCMCOBJ=   -6649.28497616068     
 iteration         1180 MCMCOBJ=   -6617.18680308240     
 iteration         1181 MCMCOBJ=   -6604.82909148215     
 iteration         1182 MCMCOBJ=   -6593.85269280547     
 iteration         1183 MCMCOBJ=   -6601.05337845187     
 iteration         1184 MCMCOBJ=   -6657.25455387799     
 iteration         1185 MCMCOBJ=   -6690.26670870478     
 iteration         1186 MCMCOBJ=   -6632.55098731392     
 iteration         1187 MCMCOBJ=   -6635.26336278583     
 iteration         1188 MCMCOBJ=   -6645.04005372509     
 iteration         1189 MCMCOBJ=   -6650.02458407744     
 iteration         1190 MCMCOBJ=   -6583.46601116418     
 iteration         1191 MCMCOBJ=   -6609.58580159163     
 iteration         1192 MCMCOBJ=   -6630.66475030622     
 iteration         1193 MCMCOBJ=   -6633.30152703577     
 iteration         1194 MCMCOBJ=   -6630.32753944649     
 iteration         1195 MCMCOBJ=   -6609.52900370859     
 iteration         1196 MCMCOBJ=   -6612.09156616543     
 iteration         1197 MCMCOBJ=   -6602.62509522144     
 iteration         1198 MCMCOBJ=   -6596.25383109697     
 iteration         1199 MCMCOBJ=   -6576.47795290853     
 iteration         1200 MCMCOBJ=   -6629.68039648104     
 iteration         1201 MCMCOBJ=   -6686.97319386236     
 iteration         1202 MCMCOBJ=   -6658.51348857023     
 iteration         1203 MCMCOBJ=   -6705.83249057370     
 iteration         1204 MCMCOBJ=   -6656.51175993469     
 iteration         1205 MCMCOBJ=   -6650.68340210852     
 iteration         1206 MCMCOBJ=   -6650.68340607181     
 iteration         1207 MCMCOBJ=   -6619.94925354138     
 iteration         1208 MCMCOBJ=   -6620.39025299738     
 iteration         1209 MCMCOBJ=   -6606.22461056002     
 iteration         1210 MCMCOBJ=   -6597.86495766849     
 iteration         1211 MCMCOBJ=   -6597.82989523234     
 iteration         1212 MCMCOBJ=   -6636.07740881067     
 iteration         1213 MCMCOBJ=   -6600.42694870721     
 iteration         1214 MCMCOBJ=   -6611.70116924100     
 iteration         1215 MCMCOBJ=   -6630.64813069642     
 iteration         1216 MCMCOBJ=   -6564.40752127569     
 iteration         1217 MCMCOBJ=   -6570.52459400577     
 iteration         1218 MCMCOBJ=   -6544.61511232802     
 iteration         1219 MCMCOBJ=   -6554.30598853770     
 iteration         1220 MCMCOBJ=   -6581.42119110475     
 iteration         1221 MCMCOBJ=   -6535.05150003049     
 iteration         1222 MCMCOBJ=   -6531.57264224043     
 iteration         1223 MCMCOBJ=   -6556.01608435233     
 iteration         1224 MCMCOBJ=   -6568.51968763168     
 iteration         1225 MCMCOBJ=   -6615.30289717383     
 iteration         1226 MCMCOBJ=   -6587.62539572120     
 iteration         1227 MCMCOBJ=   -6609.27827128378     
 iteration         1228 MCMCOBJ=   -6644.09195587252     
 iteration         1229 MCMCOBJ=   -6588.75278351929     
 iteration         1230 MCMCOBJ=   -6623.58402700752     
 iteration         1231 MCMCOBJ=   -6624.53967176038     
 iteration         1232 MCMCOBJ=   -6653.81288994280     
 iteration         1233 MCMCOBJ=   -6672.82110077309     
 iteration         1234 MCMCOBJ=   -6672.82109513892     
 iteration         1235 MCMCOBJ=   -6639.84112631665     
 iteration         1236 MCMCOBJ=   -6627.31641196973     
 iteration         1237 MCMCOBJ=   -6639.76340799712     
 iteration         1238 MCMCOBJ=   -6632.01270547585     
 iteration         1239 MCMCOBJ=   -6580.53130273882     
 iteration         1240 MCMCOBJ=   -6633.49398720200     
 iteration         1241 MCMCOBJ=   -6646.14870184873     
 iteration         1242 MCMCOBJ=   -6646.34801112306     
 iteration         1243 MCMCOBJ=   -6682.38547838454     
 iteration         1244 MCMCOBJ=   -6696.19308146375     
 iteration         1245 MCMCOBJ=   -6678.90934170501     
 iteration         1246 MCMCOBJ=   -6656.52751980762     
 iteration         1247 MCMCOBJ=   -6698.99448217983     
 iteration         1248 MCMCOBJ=   -6703.71093215854     
 iteration         1249 MCMCOBJ=   -6693.05400660865     
 iteration         1250 MCMCOBJ=   -6704.96162465648     
 iteration         1251 MCMCOBJ=   -6718.84112066924     
 iteration         1252 MCMCOBJ=   -6702.86079021638     
 iteration         1253 MCMCOBJ=   -6629.81929346560     
 iteration         1254 MCMCOBJ=   -6626.56794129382     
 iteration         1255 MCMCOBJ=   -6594.75656418720     
 iteration         1256 MCMCOBJ=   -6625.83140079711     
 iteration         1257 MCMCOBJ=   -6637.27403484017     
 iteration         1258 MCMCOBJ=   -6598.09712475687     
 iteration         1259 MCMCOBJ=   -6593.80318262796     
 iteration         1260 MCMCOBJ=   -6640.34448155585     
 iteration         1261 MCMCOBJ=   -6638.10697546432     
 iteration         1262 MCMCOBJ=   -6616.40250543576     
 iteration         1263 MCMCOBJ=   -6617.22612249326     
 iteration         1264 MCMCOBJ=   -6650.01884862668     
 iteration         1265 MCMCOBJ=   -6631.10192852265     
 iteration         1266 MCMCOBJ=   -6658.33625028981     
 iteration         1267 MCMCOBJ=   -6709.40632345762     
 iteration         1268 MCMCOBJ=   -6686.96302918055     
 iteration         1269 MCMCOBJ=   -6684.57820960028     
 iteration         1270 MCMCOBJ=   -6655.81608112789     
 iteration         1271 MCMCOBJ=   -6671.35200179852     
 iteration         1272 MCMCOBJ=   -6686.20853723371     
 iteration         1273 MCMCOBJ=   -6695.20620604705     
 iteration         1274 MCMCOBJ=   -6639.85860716679     
 iteration         1275 MCMCOBJ=   -6648.94202379411     
 iteration         1276 MCMCOBJ=   -6645.40621708473     
 iteration         1277 MCMCOBJ=   -6630.02967005972     
 iteration         1278 MCMCOBJ=   -6658.08440756170     
 iteration         1279 MCMCOBJ=   -6692.25040357640     
 iteration         1280 MCMCOBJ=   -6643.47510514625     
 iteration         1281 MCMCOBJ=   -6607.30322086041     
 iteration         1282 MCMCOBJ=   -6568.50297460761     
 iteration         1283 MCMCOBJ=   -6568.82400417975     
 iteration         1284 MCMCOBJ=   -6630.37658956257     
 iteration         1285 MCMCOBJ=   -6636.77133003494     
 iteration         1286 MCMCOBJ=   -6646.89635015843     
 iteration         1287 MCMCOBJ=   -6637.59183199403     
 iteration         1288 MCMCOBJ=   -6651.15240962289     
 iteration         1289 MCMCOBJ=   -6628.06788018142     
 iteration         1290 MCMCOBJ=   -6649.15070651484     
 iteration         1291 MCMCOBJ=   -6565.74271567261     
 iteration         1292 MCMCOBJ=   -6574.57666959298     
 iteration         1293 MCMCOBJ=   -6575.83951807690     
 iteration         1294 MCMCOBJ=   -6597.28920164943     
 iteration         1295 MCMCOBJ=   -6580.89351365782     
 iteration         1296 MCMCOBJ=   -6593.69251799646     
 iteration         1297 MCMCOBJ=   -6564.54183543290     
 iteration         1298 MCMCOBJ=   -6606.26326379895     
 iteration         1299 MCMCOBJ=   -6608.09795155657     
 iteration         1300 MCMCOBJ=   -6612.69584244659     
 iteration         1301 MCMCOBJ=   -6612.69584226521     
 iteration         1302 MCMCOBJ=   -6619.17946164352     
 iteration         1303 MCMCOBJ=   -6657.72685714512     
 iteration         1304 MCMCOBJ=   -6636.93637791795     
 iteration         1305 MCMCOBJ=   -6667.17450516740     
 iteration         1306 MCMCOBJ=   -6619.17156747768     
 iteration         1307 MCMCOBJ=   -6647.91569329790     
 iteration         1308 MCMCOBJ=   -6618.06641047619     
 iteration         1309 MCMCOBJ=   -6607.16015157532     
 iteration         1310 MCMCOBJ=   -6607.66546416396     
 iteration         1311 MCMCOBJ=   -6622.15807241020     
 iteration         1312 MCMCOBJ=   -6644.69975651153     
 iteration         1313 MCMCOBJ=   -6635.58881677390     
 iteration         1314 MCMCOBJ=   -6591.80588052941     
 iteration         1315 MCMCOBJ=   -6590.97562419194     
 iteration         1316 MCMCOBJ=   -6586.85156710086     
 iteration         1317 MCMCOBJ=   -6586.85156709264     
 iteration         1318 MCMCOBJ=   -6597.57974271568     
 iteration         1319 MCMCOBJ=   -6536.26957124180     
 iteration         1320 MCMCOBJ=   -6592.59423689468     
 iteration         1321 MCMCOBJ=   -6560.57793720636     
 iteration         1322 MCMCOBJ=   -6560.37730165524     
 iteration         1323 MCMCOBJ=   -6617.99156142078     
 iteration         1324 MCMCOBJ=   -6617.31192053133     
 iteration         1325 MCMCOBJ=   -6638.69865858413     
 iteration         1326 MCMCOBJ=   -6636.88397291583     
 iteration         1327 MCMCOBJ=   -6622.90751297253     
 iteration         1328 MCMCOBJ=   -6594.16779441742     
 iteration         1329 MCMCOBJ=   -6628.47310649891     
 iteration         1330 MCMCOBJ=   -6635.03680791069     
 iteration         1331 MCMCOBJ=   -6635.03680538209     
 iteration         1332 MCMCOBJ=   -6637.17837759775     
 iteration         1333 MCMCOBJ=   -6606.03194970201     
 iteration         1334 MCMCOBJ=   -6619.09378081435     
 iteration         1335 MCMCOBJ=   -6614.14785747810     
 iteration         1336 MCMCOBJ=   -6678.54549203551     
 iteration         1337 MCMCOBJ=   -6644.40281346507     
 iteration         1338 MCMCOBJ=   -6703.67952448820     
 iteration         1339 MCMCOBJ=   -6667.59958046099     
 iteration         1340 MCMCOBJ=   -6651.79449376223     
 iteration         1341 MCMCOBJ=   -6621.97432075296     
 iteration         1342 MCMCOBJ=   -6608.15895628667     
 iteration         1343 MCMCOBJ=   -6648.07969875948     
 iteration         1344 MCMCOBJ=   -6590.52917411286     
 iteration         1345 MCMCOBJ=   -6583.40720357842     
 iteration         1346 MCMCOBJ=   -6644.32930463399     
 iteration         1347 MCMCOBJ=   -6648.80215148561     
 iteration         1348 MCMCOBJ=   -6624.27017720575     
 iteration         1349 MCMCOBJ=   -6673.73287950890     
 iteration         1350 MCMCOBJ=   -6643.40394710558     
 iteration         1351 MCMCOBJ=   -6649.89532299974     
 iteration         1352 MCMCOBJ=   -6701.32536201377     
 iteration         1353 MCMCOBJ=   -6705.63440033356     
 iteration         1354 MCMCOBJ=   -6670.57959452137     
 iteration         1355 MCMCOBJ=   -6673.07793121136     
 iteration         1356 MCMCOBJ=   -6655.40606981096     
 iteration         1357 MCMCOBJ=   -6680.83239543251     
 iteration         1358 MCMCOBJ=   -6640.30881489672     
 iteration         1359 MCMCOBJ=   -6634.26305848346     
 iteration         1360 MCMCOBJ=   -6649.21509463439     
 iteration         1361 MCMCOBJ=   -6634.34139459238     
 iteration         1362 MCMCOBJ=   -6656.19328079394     
 iteration         1363 MCMCOBJ=   -6588.54898101167     
 iteration         1364 MCMCOBJ=   -6603.37708232387     
 iteration         1365 MCMCOBJ=   -6625.96224430784     
 iteration         1366 MCMCOBJ=   -6632.79139509516     
 iteration         1367 MCMCOBJ=   -6624.48676705654     
 iteration         1368 MCMCOBJ=   -6625.67603208209     
 iteration         1369 MCMCOBJ=   -6615.50897834087     
 iteration         1370 MCMCOBJ=   -6629.55129902037     
 iteration         1371 MCMCOBJ=   -6640.31085588429     
 iteration         1372 MCMCOBJ=   -6618.81695895221     
 iteration         1373 MCMCOBJ=   -6626.80607268631     
 iteration         1374 MCMCOBJ=   -6654.31906218845     
 iteration         1375 MCMCOBJ=   -6594.22748831207     
 iteration         1376 MCMCOBJ=   -6601.54572753522     
 iteration         1377 MCMCOBJ=   -6570.43652583931     
 iteration         1378 MCMCOBJ=   -6569.10415343749     
 iteration         1379 MCMCOBJ=   -6597.39916186735     
 iteration         1380 MCMCOBJ=   -6595.04958786161     
 iteration         1381 MCMCOBJ=   -6634.05497724263     
 iteration         1382 MCMCOBJ=   -6613.93245480073     
 iteration         1383 MCMCOBJ=   -6659.87732301755     
 iteration         1384 MCMCOBJ=   -6633.82367718843     
 iteration         1385 MCMCOBJ=   -6647.84091402082     
 iteration         1386 MCMCOBJ=   -6597.02243655392     
 iteration         1387 MCMCOBJ=   -6645.27749328803     
 iteration         1388 MCMCOBJ=   -6650.35416631729     
 iteration         1389 MCMCOBJ=   -6641.07689754470     
 iteration         1390 MCMCOBJ=   -6641.07689218019     
 iteration         1391 MCMCOBJ=   -6639.51307043385     
 iteration         1392 MCMCOBJ=   -6639.61125359509     
 iteration         1393 MCMCOBJ=   -6639.07964021121     
 iteration         1394 MCMCOBJ=   -6628.33690181367     
 iteration         1395 MCMCOBJ=   -6612.21184786218     
 iteration         1396 MCMCOBJ=   -6612.56153293350     
 iteration         1397 MCMCOBJ=   -6578.59252077591     
 iteration         1398 MCMCOBJ=   -6610.77716705717     
 iteration         1399 MCMCOBJ=   -6646.64614530715     
 iteration         1400 MCMCOBJ=   -6587.87275518766     
 iteration         1401 MCMCOBJ=   -6581.70984815928     
 iteration         1402 MCMCOBJ=   -6643.88989296885     
 iteration         1403 MCMCOBJ=   -6654.32749141244     
 iteration         1404 MCMCOBJ=   -6622.00869624267     
 iteration         1405 MCMCOBJ=   -6595.91279169745     
 iteration         1406 MCMCOBJ=   -6597.38149237668     
 iteration         1407 MCMCOBJ=   -6648.72513760496     
 iteration         1408 MCMCOBJ=   -6654.44687481375     
 iteration         1409 MCMCOBJ=   -6654.44687469354     
 iteration         1410 MCMCOBJ=   -6646.03777040615     
 iteration         1411 MCMCOBJ=   -6638.62585468464     
 iteration         1412 MCMCOBJ=   -6622.04638715466     
 iteration         1413 MCMCOBJ=   -6626.25547300737     
 iteration         1414 MCMCOBJ=   -6667.31752171140     
 iteration         1415 MCMCOBJ=   -6630.97569782652     
 iteration         1416 MCMCOBJ=   -6626.82838357121     
 iteration         1417 MCMCOBJ=   -6651.39348985829     
 iteration         1418 MCMCOBJ=   -6625.73666940529     
 iteration         1419 MCMCOBJ=   -6591.07859403376     
 iteration         1420 MCMCOBJ=   -6600.85355949554     
 iteration         1421 MCMCOBJ=   -6587.27235366087     
 iteration         1422 MCMCOBJ=   -6602.22756077098     
 iteration         1423 MCMCOBJ=   -6609.96043101609     
 iteration         1424 MCMCOBJ=   -6584.03470100712     
 iteration         1425 MCMCOBJ=   -6598.90489770332     
 iteration         1426 MCMCOBJ=   -6598.19169306687     
 iteration         1427 MCMCOBJ=   -6646.26791442166     
 iteration         1428 MCMCOBJ=   -6635.63767211271     
 iteration         1429 MCMCOBJ=   -6610.99680060656     
 iteration         1430 MCMCOBJ=   -6617.48871625429     
 iteration         1431 MCMCOBJ=   -6651.17105449565     
 iteration         1432 MCMCOBJ=   -6640.59071795609     
 iteration         1433 MCMCOBJ=   -6633.69209910099     
 iteration         1434 MCMCOBJ=   -6601.46608496942     
 iteration         1435 MCMCOBJ=   -6614.55947665119     
 iteration         1436 MCMCOBJ=   -6587.90319233587     
 iteration         1437 MCMCOBJ=   -6599.82509787271     
 iteration         1438 MCMCOBJ=   -6618.25580181818     
 iteration         1439 MCMCOBJ=   -6638.70012756580     
 iteration         1440 MCMCOBJ=   -6621.63068167329     
 iteration         1441 MCMCOBJ=   -6569.25894091565     
 iteration         1442 MCMCOBJ=   -6592.97797911932     
 iteration         1443 MCMCOBJ=   -6671.86309550297     
 iteration         1444 MCMCOBJ=   -6671.86314331027     
 iteration         1445 MCMCOBJ=   -6629.29535350918     
 iteration         1446 MCMCOBJ=   -6604.73182990816     
 iteration         1447 MCMCOBJ=   -6632.94006710992     
 iteration         1448 MCMCOBJ=   -6622.02821432220     
 iteration         1449 MCMCOBJ=   -6580.99668250584     
 iteration         1450 MCMCOBJ=   -6611.21102852428     
 iteration         1451 MCMCOBJ=   -6643.01922359766     
 iteration         1452 MCMCOBJ=   -6633.38787583993     
 iteration         1453 MCMCOBJ=   -6643.94751116133     
 iteration         1454 MCMCOBJ=   -6614.25409910683     
 iteration         1455 MCMCOBJ=   -6621.48163917159     
 iteration         1456 MCMCOBJ=   -6616.20336507834     
 iteration         1457 MCMCOBJ=   -6589.35189726579     
 iteration         1458 MCMCOBJ=   -6648.79396726879     
 iteration         1459 MCMCOBJ=   -6675.78044138453     
 iteration         1460 MCMCOBJ=   -6679.37845745995     
 iteration         1461 MCMCOBJ=   -6687.87510500863     
 iteration         1462 MCMCOBJ=   -6674.07186764662     
 iteration         1463 MCMCOBJ=   -6662.08323443312     
 iteration         1464 MCMCOBJ=   -6670.85052199857     
 iteration         1465 MCMCOBJ=   -6689.07776138176     
 iteration         1466 MCMCOBJ=   -6689.45440415680     
 iteration         1467 MCMCOBJ=   -6692.12950122707     
 iteration         1468 MCMCOBJ=   -6723.54364713208     
 iteration         1469 MCMCOBJ=   -6703.64546673493     
 iteration         1470 MCMCOBJ=   -6680.52647982291     
 iteration         1471 MCMCOBJ=   -6617.57102950491     
 iteration         1472 MCMCOBJ=   -6648.00384571551     
 iteration         1473 MCMCOBJ=   -6663.55072713302     
 iteration         1474 MCMCOBJ=   -6625.53577870019     
 iteration         1475 MCMCOBJ=   -6640.73175370132     
 iteration         1476 MCMCOBJ=   -6588.50300039414     
 iteration         1477 MCMCOBJ=   -6606.42687119099     
 iteration         1478 MCMCOBJ=   -6612.37350110116     
 iteration         1479 MCMCOBJ=   -6609.78260277810     
 iteration         1480 MCMCOBJ=   -6554.72379150521     
 iteration         1481 MCMCOBJ=   -6565.38965609115     
 iteration         1482 MCMCOBJ=   -6584.99045986867     
 iteration         1483 MCMCOBJ=   -6663.13376903710     
 iteration         1484 MCMCOBJ=   -6641.26755901309     
 iteration         1485 MCMCOBJ=   -6648.79128438227     
 iteration         1486 MCMCOBJ=   -6639.42387201881     
 iteration         1487 MCMCOBJ=   -6651.08669107030     
 iteration         1488 MCMCOBJ=   -6656.22545444031     
 iteration         1489 MCMCOBJ=   -6644.15696817984     
 iteration         1490 MCMCOBJ=   -6624.56342560701     
 iteration         1491 MCMCOBJ=   -6611.10987858248     
 iteration         1492 MCMCOBJ=   -6605.50542014220     
 iteration         1493 MCMCOBJ=   -6544.99975368101     
 iteration         1494 MCMCOBJ=   -6501.52562953117     
 iteration         1495 MCMCOBJ=   -6602.44849297332     
 iteration         1496 MCMCOBJ=   -6679.21312629904     
 iteration         1497 MCMCOBJ=   -6623.91057952857     
 iteration         1498 MCMCOBJ=   -6573.35229880537     
 iteration         1499 MCMCOBJ=   -6655.89984185637     
 iteration         1500 MCMCOBJ=   -6623.02335639058     
 iteration         1501 MCMCOBJ=   -6628.76180224610     
 iteration         1502 MCMCOBJ=   -6674.99973864206     
 iteration         1503 MCMCOBJ=   -6621.72369132144     
 iteration         1504 MCMCOBJ=   -6618.74167639281     
 iteration         1505 MCMCOBJ=   -6651.48391100538     
 iteration         1506 MCMCOBJ=   -6667.12416939808     
 iteration         1507 MCMCOBJ=   -6628.89590376026     
 iteration         1508 MCMCOBJ=   -6626.65107855084     
 iteration         1509 MCMCOBJ=   -6627.33736920709     
 iteration         1510 MCMCOBJ=   -6601.06335543623     
 iteration         1511 MCMCOBJ=   -6610.07542593867     
 iteration         1512 MCMCOBJ=   -6617.90199031920     
 iteration         1513 MCMCOBJ=   -6639.89718226329     
 iteration         1514 MCMCOBJ=   -6639.89716844768     
 iteration         1515 MCMCOBJ=   -6668.70133408322     
 iteration         1516 MCMCOBJ=   -6642.46457477661     
 iteration         1517 MCMCOBJ=   -6650.61928562275     
 iteration         1518 MCMCOBJ=   -6618.55045983119     
 iteration         1519 MCMCOBJ=   -6625.84415736067     
 iteration         1520 MCMCOBJ=   -6609.66938980989     
 iteration         1521 MCMCOBJ=   -6619.43410436369     
 iteration         1522 MCMCOBJ=   -6606.90246107878     
 iteration         1523 MCMCOBJ=   -6616.34851865942     
 iteration         1524 MCMCOBJ=   -6614.14586533172     
 iteration         1525 MCMCOBJ=   -6573.94503194165     
 iteration         1526 MCMCOBJ=   -6571.07558447126     
 iteration         1527 MCMCOBJ=   -6575.85560663531     
 iteration         1528 MCMCOBJ=   -6612.09439242735     
 iteration         1529 MCMCOBJ=   -6616.79392864540     
 iteration         1530 MCMCOBJ=   -6628.00515379576     
 iteration         1531 MCMCOBJ=   -6607.69459335300     
 iteration         1532 MCMCOBJ=   -6650.48609167513     
 iteration         1533 MCMCOBJ=   -6624.85999178374     
 iteration         1534 MCMCOBJ=   -6650.89891667711     
 iteration         1535 MCMCOBJ=   -6678.03196475984     
 iteration         1536 MCMCOBJ=   -6669.75595754077     
 iteration         1537 MCMCOBJ=   -6646.46246074378     
 iteration         1538 MCMCOBJ=   -6648.28167089470     
 iteration         1539 MCMCOBJ=   -6713.46065567687     
 iteration         1540 MCMCOBJ=   -6644.96742385453     
 iteration         1541 MCMCOBJ=   -6601.78620261672     
 iteration         1542 MCMCOBJ=   -6641.33376800018     
 iteration         1543 MCMCOBJ=   -6625.22574467267     
 iteration         1544 MCMCOBJ=   -6632.35458936610     
 iteration         1545 MCMCOBJ=   -6643.29246695119     
 iteration         1546 MCMCOBJ=   -6624.93380806853     
 iteration         1547 MCMCOBJ=   -6574.93120437301     
 iteration         1548 MCMCOBJ=   -6630.17976882972     
 iteration         1549 MCMCOBJ=   -6644.58397382550     
 iteration         1550 MCMCOBJ=   -6675.52298888717     
 iteration         1551 MCMCOBJ=   -6688.52144866493     
 iteration         1552 MCMCOBJ=   -6619.44779938387     
 iteration         1553 MCMCOBJ=   -6640.73579675730     
 iteration         1554 MCMCOBJ=   -6657.45911998022     
 iteration         1555 MCMCOBJ=   -6645.73341280602     
 iteration         1556 MCMCOBJ=   -6628.59467888127     
 iteration         1557 MCMCOBJ=   -6646.76761199730     
 iteration         1558 MCMCOBJ=   -6663.33908439119     
 iteration         1559 MCMCOBJ=   -6671.25687821464     
 iteration         1560 MCMCOBJ=   -6618.44396922322     
 iteration         1561 MCMCOBJ=   -6652.84258530130     
 iteration         1562 MCMCOBJ=   -6632.79518094948     
 iteration         1563 MCMCOBJ=   -6601.74346127091     
 iteration         1564 MCMCOBJ=   -6594.74459658583     
 iteration         1565 MCMCOBJ=   -6619.58791927099     
 iteration         1566 MCMCOBJ=   -6673.55379238515     
 iteration         1567 MCMCOBJ=   -6638.83054886919     
 iteration         1568 MCMCOBJ=   -6633.80334436237     
 iteration         1569 MCMCOBJ=   -6636.74260744359     
 iteration         1570 MCMCOBJ=   -6573.48216623904     
 iteration         1571 MCMCOBJ=   -6613.10163171665     
 iteration         1572 MCMCOBJ=   -6674.54552864589     
 iteration         1573 MCMCOBJ=   -6595.18609894581     
 iteration         1574 MCMCOBJ=   -6634.20630463169     
 iteration         1575 MCMCOBJ=   -6639.84236916360     
 iteration         1576 MCMCOBJ=   -6633.73867284961     
 iteration         1577 MCMCOBJ=   -6633.51877520956     
 iteration         1578 MCMCOBJ=   -6640.73754657972     
 iteration         1579 MCMCOBJ=   -6616.94249477296     
 iteration         1580 MCMCOBJ=   -6708.24532027342     
 iteration         1581 MCMCOBJ=   -6676.47612669733     
 iteration         1582 MCMCOBJ=   -6668.24893027755     
 iteration         1583 MCMCOBJ=   -6651.50135374750     
 iteration         1584 MCMCOBJ=   -6662.85824474897     
 iteration         1585 MCMCOBJ=   -6629.22550102561     
 iteration         1586 MCMCOBJ=   -6651.07892711206     
 iteration         1587 MCMCOBJ=   -6647.79620309346     
 iteration         1588 MCMCOBJ=   -6623.16337536819     
 iteration         1589 MCMCOBJ=   -6636.11132294545     
 iteration         1590 MCMCOBJ=   -6613.16230224529     
 iteration         1591 MCMCOBJ=   -6685.86791528074     
 iteration         1592 MCMCOBJ=   -6703.45348529931     
 iteration         1593 MCMCOBJ=   -6753.86591407537     
 iteration         1594 MCMCOBJ=   -6714.40589691482     
 iteration         1595 MCMCOBJ=   -6635.65130312069     
 iteration         1596 MCMCOBJ=   -6684.92347768386     
 iteration         1597 MCMCOBJ=   -6601.25879196197     
 iteration         1598 MCMCOBJ=   -6594.77161243741     
 iteration         1599 MCMCOBJ=   -6659.35059102155     
 iteration         1600 MCMCOBJ=   -6617.64499530508     
 iteration         1601 MCMCOBJ=   -6596.21521263378     
 iteration         1602 MCMCOBJ=   -6565.32999437742     
 iteration         1603 MCMCOBJ=   -6587.76555963848     
 iteration         1604 MCMCOBJ=   -6633.08736390930     
 iteration         1605 MCMCOBJ=   -6592.44067381136     
 iteration         1606 MCMCOBJ=   -6649.64780970476     
 iteration         1607 MCMCOBJ=   -6613.80483957295     
 iteration         1608 MCMCOBJ=   -6535.66267249748     
 iteration         1609 MCMCOBJ=   -6572.80632327839     
 iteration         1610 MCMCOBJ=   -6601.45182537459     
 iteration         1611 MCMCOBJ=   -6674.55899047400     
 iteration         1612 MCMCOBJ=   -6679.58025758196     
 iteration         1613 MCMCOBJ=   -6653.39247073183     
 iteration         1614 MCMCOBJ=   -6637.83910499967     
 iteration         1615 MCMCOBJ=   -6655.15393792892     
 iteration         1616 MCMCOBJ=   -6654.47126586913     
 iteration         1617 MCMCOBJ=   -6647.17173541251     
 iteration         1618 MCMCOBJ=   -6694.14437327523     
 iteration         1619 MCMCOBJ=   -6661.09606869981     
 iteration         1620 MCMCOBJ=   -6668.23996760178     
 iteration         1621 MCMCOBJ=   -6677.20311933325     
 iteration         1622 MCMCOBJ=   -6609.13603400496     
 iteration         1623 MCMCOBJ=   -6597.01657863522     
 iteration         1624 MCMCOBJ=   -6620.41105983587     
 iteration         1625 MCMCOBJ=   -6607.11516807632     
 iteration         1626 MCMCOBJ=   -6585.37897476699     
 iteration         1627 MCMCOBJ=   -6582.96342999453     
 iteration         1628 MCMCOBJ=   -6582.96341837919     
 iteration         1629 MCMCOBJ=   -6597.12697339247     
 iteration         1630 MCMCOBJ=   -6593.19047653687     
 iteration         1631 MCMCOBJ=   -6661.16175442731     
 iteration         1632 MCMCOBJ=   -6621.75442615088     
 iteration         1633 MCMCOBJ=   -6607.91615479726     
 iteration         1634 MCMCOBJ=   -6677.35057786165     
 iteration         1635 MCMCOBJ=   -6703.65424836923     
 iteration         1636 MCMCOBJ=   -6707.25856515057     
 iteration         1637 MCMCOBJ=   -6652.24821054693     
 iteration         1638 MCMCOBJ=   -6633.75401386518     
 iteration         1639 MCMCOBJ=   -6609.18494553110     
 iteration         1640 MCMCOBJ=   -6641.42064724869     
 iteration         1641 MCMCOBJ=   -6628.44771388141     
 iteration         1642 MCMCOBJ=   -6597.10960660586     
 iteration         1643 MCMCOBJ=   -6640.93532356260     
 iteration         1644 MCMCOBJ=   -6636.75078150552     
 iteration         1645 MCMCOBJ=   -6623.88568234367     
 iteration         1646 MCMCOBJ=   -6613.15133177221     
 iteration         1647 MCMCOBJ=   -6665.60894145314     
 iteration         1648 MCMCOBJ=   -6687.75036460468     
 iteration         1649 MCMCOBJ=   -6673.14407381491     
 iteration         1650 MCMCOBJ=   -6673.24167412487     
 iteration         1651 MCMCOBJ=   -6640.04584635011     
 iteration         1652 MCMCOBJ=   -6641.47555324781     
 iteration         1653 MCMCOBJ=   -6600.34964416544     
 iteration         1654 MCMCOBJ=   -6606.76307417415     
 iteration         1655 MCMCOBJ=   -6652.85789704906     
 iteration         1656 MCMCOBJ=   -6620.98720318557     
 iteration         1657 MCMCOBJ=   -6633.75669581905     
 iteration         1658 MCMCOBJ=   -6628.11213794322     
 iteration         1659 MCMCOBJ=   -6573.21623589216     
 iteration         1660 MCMCOBJ=   -6583.23954106620     
 iteration         1661 MCMCOBJ=   -6588.46315997035     
 iteration         1662 MCMCOBJ=   -6580.02402021745     
 iteration         1663 MCMCOBJ=   -6629.70990735739     
 iteration         1664 MCMCOBJ=   -6667.95085364920     
 iteration         1665 MCMCOBJ=   -6652.20113677450     
 iteration         1666 MCMCOBJ=   -6656.26116658837     
 iteration         1667 MCMCOBJ=   -6631.30162909133     
 iteration         1668 MCMCOBJ=   -6659.17728107819     
 iteration         1669 MCMCOBJ=   -6663.21518616778     
 iteration         1670 MCMCOBJ=   -6608.47242536886     
 iteration         1671 MCMCOBJ=   -6605.02033479419     
 iteration         1672 MCMCOBJ=   -6603.51784030476     
 iteration         1673 MCMCOBJ=   -6616.73970840730     
 iteration         1674 MCMCOBJ=   -6591.20593011199     
 iteration         1675 MCMCOBJ=   -6598.77505342595     
 iteration         1676 MCMCOBJ=   -6541.02269352935     
 iteration         1677 MCMCOBJ=   -6605.07278283058     
 iteration         1678 MCMCOBJ=   -6638.64938317472     
 iteration         1679 MCMCOBJ=   -6657.54530721022     
 iteration         1680 MCMCOBJ=   -6633.29188069689     
 iteration         1681 MCMCOBJ=   -6654.19624602717     
 iteration         1682 MCMCOBJ=   -6614.05091502508     
 iteration         1683 MCMCOBJ=   -6664.46764514999     
 iteration         1684 MCMCOBJ=   -6591.39398000313     
 iteration         1685 MCMCOBJ=   -6637.02600805916     
 iteration         1686 MCMCOBJ=   -6566.90864842418     
 iteration         1687 MCMCOBJ=   -6605.28612903264     
 iteration         1688 MCMCOBJ=   -6591.88404926308     
 iteration         1689 MCMCOBJ=   -6609.91636145774     
 iteration         1690 MCMCOBJ=   -6590.63341330543     
 iteration         1691 MCMCOBJ=   -6578.20684312506     
 iteration         1692 MCMCOBJ=   -6553.70437775702     
 iteration         1693 MCMCOBJ=   -6584.03869387175     
 iteration         1694 MCMCOBJ=   -6603.43825515329     
 iteration         1695 MCMCOBJ=   -6601.40758415583     
 iteration         1696 MCMCOBJ=   -6606.00126005325     
 iteration         1697 MCMCOBJ=   -6557.75433280659     
 iteration         1698 MCMCOBJ=   -6599.60695786536     
 iteration         1699 MCMCOBJ=   -6605.63065473784     
 iteration         1700 MCMCOBJ=   -6577.05138110533     
 iteration         1701 MCMCOBJ=   -6598.90841960882     
 iteration         1702 MCMCOBJ=   -6587.96673583446     
 iteration         1703 MCMCOBJ=   -6606.45160779092     
 iteration         1704 MCMCOBJ=   -6578.37395777300     
 iteration         1705 MCMCOBJ=   -6616.30365340547     
 iteration         1706 MCMCOBJ=   -6616.30365501227     
 iteration         1707 MCMCOBJ=   -6588.45137880265     
 iteration         1708 MCMCOBJ=   -6620.12926510099     
 iteration         1709 MCMCOBJ=   -6615.93537516919     
 iteration         1710 MCMCOBJ=   -6660.43298542145     
 iteration         1711 MCMCOBJ=   -6660.43298559409     
 iteration         1712 MCMCOBJ=   -6708.17859073727     
 iteration         1713 MCMCOBJ=   -6708.87379902756     
 iteration         1714 MCMCOBJ=   -6677.18219212023     
 iteration         1715 MCMCOBJ=   -6646.86756919200     
 iteration         1716 MCMCOBJ=   -6673.57612322138     
 iteration         1717 MCMCOBJ=   -6669.58962764708     
 iteration         1718 MCMCOBJ=   -6630.82175541268     
 iteration         1719 MCMCOBJ=   -6624.87403866494     
 iteration         1720 MCMCOBJ=   -6636.86551771465     
 iteration         1721 MCMCOBJ=   -6592.90765921043     
 iteration         1722 MCMCOBJ=   -6618.51885243964     
 iteration         1723 MCMCOBJ=   -6632.66791176499     
 iteration         1724 MCMCOBJ=   -6665.56265210136     
 iteration         1725 MCMCOBJ=   -6663.80898757654     
 iteration         1726 MCMCOBJ=   -6652.04210689254     
 iteration         1727 MCMCOBJ=   -6648.03448532396     
 iteration         1728 MCMCOBJ=   -6652.73807493018     
 iteration         1729 MCMCOBJ=   -6595.23293512887     
 iteration         1730 MCMCOBJ=   -6586.32269164961     
 iteration         1731 MCMCOBJ=   -6575.52109156570     
 iteration         1732 MCMCOBJ=   -6578.03205165812     
 iteration         1733 MCMCOBJ=   -6570.81810459164     
 iteration         1734 MCMCOBJ=   -6603.74881734313     
 iteration         1735 MCMCOBJ=   -6585.47603925024     
 iteration         1736 MCMCOBJ=   -6589.25454655726     
 iteration         1737 MCMCOBJ=   -6595.47317060779     
 iteration         1738 MCMCOBJ=   -6596.73151932970     
 iteration         1739 MCMCOBJ=   -6577.78434951369     
 iteration         1740 MCMCOBJ=   -6608.11600235041     
 iteration         1741 MCMCOBJ=   -6560.99451067328     
 iteration         1742 MCMCOBJ=   -6561.32406748336     
 iteration         1743 MCMCOBJ=   -6523.33037983216     
 iteration         1744 MCMCOBJ=   -6658.25956585437     
 iteration         1745 MCMCOBJ=   -6648.66367654204     
 iteration         1746 MCMCOBJ=   -6678.11508232067     
 iteration         1747 MCMCOBJ=   -6659.21396560651     
 iteration         1748 MCMCOBJ=   -6601.27911276842     
 iteration         1749 MCMCOBJ=   -6606.51892523497     
 iteration         1750 MCMCOBJ=   -6632.14074127487     
 iteration         1751 MCMCOBJ=   -6633.00566576078     
 iteration         1752 MCMCOBJ=   -6628.71217252818     
 iteration         1753 MCMCOBJ=   -6648.84464002439     
 iteration         1754 MCMCOBJ=   -6616.38279971794     
 iteration         1755 MCMCOBJ=   -6608.83692234499     
 iteration         1756 MCMCOBJ=   -6620.40672813656     
 iteration         1757 MCMCOBJ=   -6639.18441035061     
 iteration         1758 MCMCOBJ=   -6625.12733218343     
 iteration         1759 MCMCOBJ=   -6639.65513408823     
 iteration         1760 MCMCOBJ=   -6668.01448192389     
 iteration         1761 MCMCOBJ=   -6664.06301130958     
 iteration         1762 MCMCOBJ=   -6664.06300531635     
 iteration         1763 MCMCOBJ=   -6659.34964298045     
 iteration         1764 MCMCOBJ=   -6649.63675188536     
 iteration         1765 MCMCOBJ=   -6652.64430718269     
 iteration         1766 MCMCOBJ=   -6607.76273735418     
 iteration         1767 MCMCOBJ=   -6596.91233291005     
 iteration         1768 MCMCOBJ=   -6604.04645830307     
 iteration         1769 MCMCOBJ=   -6642.46034066897     
 iteration         1770 MCMCOBJ=   -6616.29502519955     
 iteration         1771 MCMCOBJ=   -6627.48400198504     
 iteration         1772 MCMCOBJ=   -6569.08370890376     
 iteration         1773 MCMCOBJ=   -6611.71891878243     
 iteration         1774 MCMCOBJ=   -6678.28533081077     
 iteration         1775 MCMCOBJ=   -6673.46531543311     
 iteration         1776 MCMCOBJ=   -6663.87440816574     
 iteration         1777 MCMCOBJ=   -6636.71008256553     
 iteration         1778 MCMCOBJ=   -6694.77686970220     
 iteration         1779 MCMCOBJ=   -6634.65672187940     
 iteration         1780 MCMCOBJ=   -6634.65672095709     
 iteration         1781 MCMCOBJ=   -6629.02907749527     
 iteration         1782 MCMCOBJ=   -6621.76944690424     
 iteration         1783 MCMCOBJ=   -6624.04264121890     
 iteration         1784 MCMCOBJ=   -6669.31055732233     
 iteration         1785 MCMCOBJ=   -6666.03732774582     
 iteration         1786 MCMCOBJ=   -6668.92784421955     
 iteration         1787 MCMCOBJ=   -6642.52190736909     
 iteration         1788 MCMCOBJ=   -6610.69734783271     
 iteration         1789 MCMCOBJ=   -6651.24085509203     
 iteration         1790 MCMCOBJ=   -6670.05321081967     
 iteration         1791 MCMCOBJ=   -6652.87300894576     
 iteration         1792 MCMCOBJ=   -6605.04052878392     
 iteration         1793 MCMCOBJ=   -6639.91331922462     
 iteration         1794 MCMCOBJ=   -6652.46798323547     
 iteration         1795 MCMCOBJ=   -6600.78230590118     
 iteration         1796 MCMCOBJ=   -6577.80646622769     
 iteration         1797 MCMCOBJ=   -6613.80571644245     
 iteration         1798 MCMCOBJ=   -6628.52568182836     
 iteration         1799 MCMCOBJ=   -6611.24466303380     
 iteration         1800 MCMCOBJ=   -6653.80588715567     
 iteration         1801 MCMCOBJ=   -6592.08777710314     
 iteration         1802 MCMCOBJ=   -6624.53627851186     
 iteration         1803 MCMCOBJ=   -6625.64423624520     
 iteration         1804 MCMCOBJ=   -6588.90878184639     
 iteration         1805 MCMCOBJ=   -6568.69569687432     
 iteration         1806 MCMCOBJ=   -6585.29335210233     
 iteration         1807 MCMCOBJ=   -6619.39500332652     
 iteration         1808 MCMCOBJ=   -6676.62231734532     
 iteration         1809 MCMCOBJ=   -6680.61654470608     
 iteration         1810 MCMCOBJ=   -6689.75218183640     
 iteration         1811 MCMCOBJ=   -6677.63253362944     
 iteration         1812 MCMCOBJ=   -6661.09036344638     
 iteration         1813 MCMCOBJ=   -6656.85765371881     
 iteration         1814 MCMCOBJ=   -6624.95486474386     
 iteration         1815 MCMCOBJ=   -6574.71269005800     
 iteration         1816 MCMCOBJ=   -6643.33881791762     
 iteration         1817 MCMCOBJ=   -6657.65665325669     
 iteration         1818 MCMCOBJ=   -6636.63017691263     
 iteration         1819 MCMCOBJ=   -6652.80966409241     
 iteration         1820 MCMCOBJ=   -6660.82040809391     
 iteration         1821 MCMCOBJ=   -6684.12572087283     
 iteration         1822 MCMCOBJ=   -6728.50748685972     
 iteration         1823 MCMCOBJ=   -6656.17253482892     
 iteration         1824 MCMCOBJ=   -6594.56322549664     
 iteration         1825 MCMCOBJ=   -6612.47089214895     
 iteration         1826 MCMCOBJ=   -6596.96234841106     
 iteration         1827 MCMCOBJ=   -6653.93721641820     
 iteration         1828 MCMCOBJ=   -6619.48423505833     
 iteration         1829 MCMCOBJ=   -6581.69595707525     
 iteration         1830 MCMCOBJ=   -6555.66802688882     
 iteration         1831 MCMCOBJ=   -6615.95375842948     
 iteration         1832 MCMCOBJ=   -6591.02530847426     
 iteration         1833 MCMCOBJ=   -6598.00887577159     
 iteration         1834 MCMCOBJ=   -6650.61591576022     
 iteration         1835 MCMCOBJ=   -6689.50714304442     
 iteration         1836 MCMCOBJ=   -6617.24153387230     
 iteration         1837 MCMCOBJ=   -6622.54702980557     
 iteration         1838 MCMCOBJ=   -6584.32290584857     
 iteration         1839 MCMCOBJ=   -6579.96908215194     
 iteration         1840 MCMCOBJ=   -6654.87290967990     
 iteration         1841 MCMCOBJ=   -6616.81003894538     
 iteration         1842 MCMCOBJ=   -6597.14948750353     
 iteration         1843 MCMCOBJ=   -6622.54791024186     
 iteration         1844 MCMCOBJ=   -6645.95223649265     
 iteration         1845 MCMCOBJ=   -6605.77734171346     
 iteration         1846 MCMCOBJ=   -6633.15425858677     
 iteration         1847 MCMCOBJ=   -6628.09987228651     
 iteration         1848 MCMCOBJ=   -6680.66997626362     
 iteration         1849 MCMCOBJ=   -6675.11029664041     
 iteration         1850 MCMCOBJ=   -6693.88957271930     
 iteration         1851 MCMCOBJ=   -6693.23470558281     
 iteration         1852 MCMCOBJ=   -6660.82744232910     
 iteration         1853 MCMCOBJ=   -6620.62166875606     
 iteration         1854 MCMCOBJ=   -6631.27374390091     
 iteration         1855 MCMCOBJ=   -6624.48457921675     
 iteration         1856 MCMCOBJ=   -6667.44381555630     
 iteration         1857 MCMCOBJ=   -6635.55670899448     
 iteration         1858 MCMCOBJ=   -6635.05131507293     
 iteration         1859 MCMCOBJ=   -6579.78590031657     
 iteration         1860 MCMCOBJ=   -6576.07626450184     
 iteration         1861 MCMCOBJ=   -6622.95076769214     
 iteration         1862 MCMCOBJ=   -6633.14225581664     
 iteration         1863 MCMCOBJ=   -6662.10223274654     
 iteration         1864 MCMCOBJ=   -6637.98583603465     
 iteration         1865 MCMCOBJ=   -6605.54526956595     
 iteration         1866 MCMCOBJ=   -6620.09559435515     
 iteration         1867 MCMCOBJ=   -6661.31222809068     
 iteration         1868 MCMCOBJ=   -6621.80402260978     
 iteration         1869 MCMCOBJ=   -6567.29260465732     
 iteration         1870 MCMCOBJ=   -6599.87007378083     
 iteration         1871 MCMCOBJ=   -6628.57395845823     
 iteration         1872 MCMCOBJ=   -6620.95850510823     
 iteration         1873 MCMCOBJ=   -6608.63169678220     
 iteration         1874 MCMCOBJ=   -6640.94006699646     
 iteration         1875 MCMCOBJ=   -6643.90042410465     
 iteration         1876 MCMCOBJ=   -6653.41280508855     
 iteration         1877 MCMCOBJ=   -6629.00611432602     
 iteration         1878 MCMCOBJ=   -6605.97687442317     
 iteration         1879 MCMCOBJ=   -6574.12222236371     
 iteration         1880 MCMCOBJ=   -6639.05793180749     
 iteration         1881 MCMCOBJ=   -6653.16332382295     
 iteration         1882 MCMCOBJ=   -6646.97987106761     
 iteration         1883 MCMCOBJ=   -6583.99212500558     
 iteration         1884 MCMCOBJ=   -6566.15061493766     
 iteration         1885 MCMCOBJ=   -6579.72069489438     
 iteration         1886 MCMCOBJ=   -6607.51401366421     
 iteration         1887 MCMCOBJ=   -6610.60532452792     
 iteration         1888 MCMCOBJ=   -6586.87577218195     
 iteration         1889 MCMCOBJ=   -6545.28897597637     
 iteration         1890 MCMCOBJ=   -6642.35720645401     
 iteration         1891 MCMCOBJ=   -6655.28642712705     
 iteration         1892 MCMCOBJ=   -6574.48053529971     
 iteration         1893 MCMCOBJ=   -6542.80136290247     
 iteration         1894 MCMCOBJ=   -6579.61379201647     
 iteration         1895 MCMCOBJ=   -6594.99499066881     
 iteration         1896 MCMCOBJ=   -6617.55020595715     
 iteration         1897 MCMCOBJ=   -6599.17788917943     
 iteration         1898 MCMCOBJ=   -6598.27465364827     
 iteration         1899 MCMCOBJ=   -6583.10891538040     
 iteration         1900 MCMCOBJ=   -6619.19323058120     
 iteration         1901 MCMCOBJ=   -6604.74561840568     
 iteration         1902 MCMCOBJ=   -6651.25402891966     
 iteration         1903 MCMCOBJ=   -6663.62166502351     
 iteration         1904 MCMCOBJ=   -6656.41815569659     
 iteration         1905 MCMCOBJ=   -6651.66311325855     
 iteration         1906 MCMCOBJ=   -6626.21261348672     
 iteration         1907 MCMCOBJ=   -6629.57103528018     
 iteration         1908 MCMCOBJ=   -6624.46925077230     
 iteration         1909 MCMCOBJ=   -6603.10317887335     
 iteration         1910 MCMCOBJ=   -6631.92216894554     
 iteration         1911 MCMCOBJ=   -6640.88260564949     
 iteration         1912 MCMCOBJ=   -6648.35714878662     
 iteration         1913 MCMCOBJ=   -6593.01585710255     
 iteration         1914 MCMCOBJ=   -6617.70813037656     
 iteration         1915 MCMCOBJ=   -6602.37692402530     
 iteration         1916 MCMCOBJ=   -6591.66856979697     
 iteration         1917 MCMCOBJ=   -6585.11416132193     
 iteration         1918 MCMCOBJ=   -6603.85448637999     
 iteration         1919 MCMCOBJ=   -6626.19366263952     
 iteration         1920 MCMCOBJ=   -6612.63731051604     
 iteration         1921 MCMCOBJ=   -6640.15833965082     
 iteration         1922 MCMCOBJ=   -6610.18918996709     
 iteration         1923 MCMCOBJ=   -6616.18089064617     
 iteration         1924 MCMCOBJ=   -6659.77660016838     
 iteration         1925 MCMCOBJ=   -6659.77660735693     
 iteration         1926 MCMCOBJ=   -6659.77659954076     
 iteration         1927 MCMCOBJ=   -6680.61456307069     
 iteration         1928 MCMCOBJ=   -6618.20489066881     
 iteration         1929 MCMCOBJ=   -6642.29376727810     
 iteration         1930 MCMCOBJ=   -6625.12478822325     
 iteration         1931 MCMCOBJ=   -6641.63959040773     
 iteration         1932 MCMCOBJ=   -6635.03898595237     
 iteration         1933 MCMCOBJ=   -6586.14739604505     
 iteration         1934 MCMCOBJ=   -6644.23314301128     
 iteration         1935 MCMCOBJ=   -6626.66600097972     
 iteration         1936 MCMCOBJ=   -6630.91730892496     
 iteration         1937 MCMCOBJ=   -6660.37682761263     
 iteration         1938 MCMCOBJ=   -6651.19646950358     
 iteration         1939 MCMCOBJ=   -6616.21404065007     
 iteration         1940 MCMCOBJ=   -6630.55376430457     
 iteration         1941 MCMCOBJ=   -6656.20463111099     
 iteration         1942 MCMCOBJ=   -6661.12279318766     
 iteration         1943 MCMCOBJ=   -6649.81651795706     
 iteration         1944 MCMCOBJ=   -6641.81346619295     
 iteration         1945 MCMCOBJ=   -6640.07743313773     
 iteration         1946 MCMCOBJ=   -6657.49644723771     
 iteration         1947 MCMCOBJ=   -6697.01455713066     
 iteration         1948 MCMCOBJ=   -6687.63662918463     
 iteration         1949 MCMCOBJ=   -6663.29002668912     
 iteration         1950 MCMCOBJ=   -6704.15129148933     
 iteration         1951 MCMCOBJ=   -6696.07647566588     
 iteration         1952 MCMCOBJ=   -6630.15828248023     
 iteration         1953 MCMCOBJ=   -6686.11367170938     
 iteration         1954 MCMCOBJ=   -6643.33894682603     
 iteration         1955 MCMCOBJ=   -6610.68327629336     
 iteration         1956 MCMCOBJ=   -6602.11801328860     
 iteration         1957 MCMCOBJ=   -6629.67403346042     
 iteration         1958 MCMCOBJ=   -6644.83531994164     
 iteration         1959 MCMCOBJ=   -6586.25149511138     
 iteration         1960 MCMCOBJ=   -6546.83403735953     
 iteration         1961 MCMCOBJ=   -6569.23723780915     
 iteration         1962 MCMCOBJ=   -6574.17159154501     
 iteration         1963 MCMCOBJ=   -6568.38321590920     
 iteration         1964 MCMCOBJ=   -6574.19872352742     
 iteration         1965 MCMCOBJ=   -6561.75284371454     
 iteration         1966 MCMCOBJ=   -6506.33825712656     
 iteration         1967 MCMCOBJ=   -6552.66358896628     
 iteration         1968 MCMCOBJ=   -6543.78693814866     
 iteration         1969 MCMCOBJ=   -6542.58511685034     
 iteration         1970 MCMCOBJ=   -6630.21265492240     
 iteration         1971 MCMCOBJ=   -6572.55401231209     
 iteration         1972 MCMCOBJ=   -6583.57463219859     
 iteration         1973 MCMCOBJ=   -6594.23528894749     
 iteration         1974 MCMCOBJ=   -6609.90340193892     
 iteration         1975 MCMCOBJ=   -6587.57964875907     
 iteration         1976 MCMCOBJ=   -6617.65323850318     
 iteration         1977 MCMCOBJ=   -6586.20124838201     
 iteration         1978 MCMCOBJ=   -6597.18536416070     
 iteration         1979 MCMCOBJ=   -6625.57364940594     
 iteration         1980 MCMCOBJ=   -6641.76436904980     
 iteration         1981 MCMCOBJ=   -6605.80444961402     
 iteration         1982 MCMCOBJ=   -6605.80446354264     
 iteration         1983 MCMCOBJ=   -6587.85405937221     
 iteration         1984 MCMCOBJ=   -6615.29073138738     
 iteration         1985 MCMCOBJ=   -6616.23103566878     
 iteration         1986 MCMCOBJ=   -6620.77960182834     
 iteration         1987 MCMCOBJ=   -6554.13360617675     
 iteration         1988 MCMCOBJ=   -6634.79332544202     
 iteration         1989 MCMCOBJ=   -6638.19796686632     
 iteration         1990 MCMCOBJ=   -6634.07737247121     
 iteration         1991 MCMCOBJ=   -6655.05087263801     
 iteration         1992 MCMCOBJ=   -6632.43071300376     
 iteration         1993 MCMCOBJ=   -6633.24222581599     
 iteration         1994 MCMCOBJ=   -6619.11172885358     
 iteration         1995 MCMCOBJ=   -6611.87343746768     
 iteration         1996 MCMCOBJ=   -6666.01073406567     
 iteration         1997 MCMCOBJ=   -6649.80026603220     
 iteration         1998 MCMCOBJ=   -6649.80026679495     
 iteration         1999 MCMCOBJ=   -6630.74338957557     
 iteration         2000 MCMCOBJ=   -6624.56619733932     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6630.17111000524     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3748.37986987538     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6630.17111000524     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5895.02028344150     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    55.1779157436876     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6630.17111000524     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6574.99319426155     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  2634.96
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6630.171       **************************************************
 #OBJS:********************************************       35.590 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.90E+00 -2.22E+00  5.55E-01 -1.84E-01  2.27E+00  2.38E-01  3.71E+00 -7.04E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.85E-01
 
 ETA2
+       -3.45E-02  2.25E-01
 
 ETA3
+        4.52E-02 -1.00E-02  1.46E-01
 
 ETA4
+        3.02E-02  5.94E-02 -1.25E-02  2.68E-01
 
 ETA5
+        2.99E-02  1.52E-02 -3.37E-04 -3.33E-02  2.12E-01
 
 ETA6
+       -2.78E-02  3.41E-03  1.44E-02  1.56E-02 -7.60E-02  2.42E-01
 
 ETA7
+        3.02E-02 -5.33E-02  3.23E-02 -7.54E-02  2.51E-02 -1.40E-03  2.53E-01
 
 ETA8
+        9.82E-02  7.39E-02  4.38E-02  4.66E-02  5.63E-03 -5.36E-02  5.80E-02  2.41E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.34E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.31E-01
 
 ETA2
+       -1.35E-01  4.70E-01
 
 ETA3
+        2.20E-01 -5.63E-02  3.80E-01
 
 ETA4
+        1.08E-01  2.39E-01 -6.53E-02  5.14E-01
 
 ETA5
+        1.20E-01  6.94E-02 -4.20E-03 -1.39E-01  4.58E-01
 
 ETA6
+       -1.07E-01  1.68E-02  7.95E-02  6.03E-02 -3.35E-01  4.89E-01
 
 ETA7
+        1.11E-01 -2.17E-01  1.67E-01 -2.88E-01  1.07E-01 -5.81E-03  5.00E-01
 
 ETA8
+        3.71E-01  3.16E-01  2.29E-01  1.82E-01  2.35E-02 -2.21E-01  2.32E-01  4.89E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.66E-02
 
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
 
         7.42E-02  7.88E-02  5.84E-02  7.64E-02  6.69E-02  7.73E-02  7.39E-02  6.89E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        6.01E-02
 
 ETA2
+        4.21E-02  5.92E-02
 
 ETA3
+        3.27E-02  3.10E-02  3.67E-02
 
 ETA4
+        4.16E-02  4.30E-02  3.20E-02  6.24E-02
 
 ETA5
+        3.67E-02  3.57E-02  2.88E-02  3.59E-02  4.71E-02
 
 ETA6
+        4.10E-02  3.92E-02  3.14E-02  4.13E-02  3.78E-02  5.73E-02
 
 ETA7
+        3.87E-02  4.30E-02  3.04E-02  4.00E-02  3.61E-02  3.88E-02  5.33E-02
 
 ETA8
+        4.23E-02  3.95E-02  3.09E-02  3.95E-02  3.35E-02  3.84E-02  3.77E-02  5.23E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.29E-04
 
 EPS2
+        0.00E+00  1.22E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.50E-02
 
 ETA2
+        1.54E-01  6.04E-02
 
 ETA3
+        1.44E-01  1.64E-01  4.65E-02
 
 ETA4
+        1.41E-01  1.53E-01  1.56E-01  5.85E-02
 
 ETA5
+        1.39E-01  1.55E-01  1.57E-01  1.40E-01  4.98E-02
 
 ETA6
+        1.49E-01  1.63E-01  1.61E-01  1.55E-01  1.40E-01  5.71E-02
 
 ETA7
+        1.35E-01  1.57E-01  1.48E-01  1.30E-01  1.45E-01  1.52E-01  5.16E-02
 
 ETA8
+        1.25E-01  1.43E-01  1.43E-01  1.42E-01  1.42E-01  1.42E-01  1.33E-01  5.19E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.25E-03
 
 EPS2
+        0.00E+00  4.05E-03
 
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
+        5.51E-03
 
 TH 2
+       -4.88E-04  6.20E-03
 
 TH 3
+        6.46E-04 -2.04E-04  3.41E-03
 
 TH 4
+        6.81E-04  1.12E-03  4.24E-05  5.83E-03
 
 TH 5
+        7.20E-04  3.58E-05  3.00E-05 -6.64E-04  4.48E-03
 
 TH 6
+       -2.85E-04 -2.19E-04  2.28E-04  5.33E-04 -1.24E-03  5.97E-03
 
 TH 7
+        5.85E-04 -1.56E-03  7.36E-04 -1.62E-03  9.75E-04 -1.65E-04  5.46E-03
 
 TH 8
+        1.82E-03  1.20E-03  9.15E-04  7.68E-04  5.74E-04 -1.20E-03  1.24E-03  4.75E-03
 
 OM11
+        2.22E-05 -4.90E-05 -3.79E-05 -1.86E-04  1.16E-04  1.45E-04  9.54E-05  7.89E-05  3.61E-03
 
 OM12
+       -9.17E-06  2.62E-04  8.82E-05 -1.31E-04 -1.40E-04  8.75E-05 -3.69E-07  4.83E-05 -3.40E-04  1.77E-03
 
 OM13
+       -2.36E-05 -6.23E-05  4.63E-06 -5.62E-05  1.80E-05  3.43E-05 -4.53E-05 -3.03E-05  5.45E-04  9.04E-06  1.07E-03
 
 OM14
+       -7.00E-07 -4.73E-05  1.68E-05 -2.32E-05 -5.14E-05  4.86E-05  4.27E-05 -3.54E-05  4.44E-04  3.30E-04 -2.65E-05  1.73E-03
 
 OM15
+       -9.07E-06 -1.77E-04 -5.32E-05 -2.87E-06  5.36E-05  1.73E-04  1.15E-04 -9.77E-05  3.77E-04  1.49E-04  6.01E-05 -1.61E-04
          1.34E-03
 
 OM16
+       -7.97E-06  4.29E-05 -5.96E-05  9.32E-05 -1.66E-04  1.67E-04 -1.05E-05 -7.57E-06 -2.48E-04  1.46E-05 -3.39E-05  9.06E-05
         -4.16E-04  1.68E-03
 
 OM17
+        5.67E-05 -1.17E-04 -8.81E-06 -3.02E-05  2.24E-04  6.54E-05 -5.34E-06  2.11E-05  4.85E-04 -3.60E-04  2.41E-04 -4.30E-04
          1.83E-04  4.19E-06  1.50E-03
 
 OM18
+       -7.06E-05  4.47E-05 -2.65E-06 -2.57E-04  1.61E-04  2.28E-05  4.96E-05  4.03E-05  1.31E-03  4.24E-04  4.32E-04  3.08E-04
          2.56E-04 -3.79E-04  5.02E-04  1.79E-03
 
 OM22
+        2.71E-05 -9.16E-04  8.94E-05 -8.72E-05 -1.78E-05 -8.22E-05  2.95E-05  1.79E-04  1.10E-04 -6.29E-04  5.81E-05 -1.06E-04
         -4.60E-06 -4.83E-05  8.63E-05 -9.59E-05  3.51E-03
 
 OM23
+       -3.72E-05  3.78E-04  5.15E-05  6.41E-05  6.18E-05  2.72E-05 -8.28E-05  7.83E-05  8.37E-06  2.46E-04 -9.30E-05  8.35E-05
          8.56E-06  6.19E-06 -1.04E-04  1.82E-05 -1.24E-04  9.62E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        3.22E-05 -5.51E-04  1.11E-04 -3.65E-05  2.79E-05 -1.56E-06  1.62E-04  1.67E-04 -1.57E-06 -3.88E-05  1.40E-04 -2.20E-04
          8.06E-05 -8.57E-05  1.25E-04  4.49E-05  1.02E-03 -7.48E-05  1.85E-03
 
 OM25
+        1.15E-05 -3.12E-06 -6.06E-05 -9.90E-05 -8.81E-05  5.37E-06 -4.61E-05 -3.64E-05 -2.29E-06  1.56E-04  4.53E-05  7.62E-05
         -1.64E-04  2.32E-05 -1.00E-04  5.27E-05  1.14E-04  1.06E-05 -1.53E-04  1.27E-03
 
 OM26
+       -1.64E-04  1.38E-04  1.64E-04  1.72E-05  8.86E-05  1.20E-04 -2.73E-05 -1.02E-05  1.43E-04 -1.33E-04  3.49E-05 -8.10E-05
          4.52E-05 -1.99E-04  3.42E-05  6.16E-05 -2.38E-04  5.76E-05  9.08E-05 -3.69E-04  1.54E-03
 
 OM27
+       -2.34E-05  5.32E-04  1.62E-05 -3.85E-05 -1.55E-04  2.23E-05 -4.19E-05 -2.04E-04 -1.40E-05  2.84E-04 -1.21E-04  9.34E-05
          8.66E-06  2.06E-05 -2.56E-04 -5.91E-05 -1.07E-03  2.35E-04 -7.31E-04  1.61E-04  1.02E-04  1.85E-03
 
 OM28
+        1.13E-04 -1.94E-06  1.08E-04 -6.48E-05 -4.83E-05 -4.46E-05  5.59E-05  9.89E-05 -8.07E-05  5.17E-04  2.21E-05  5.27E-05
          1.21E-04  3.35E-05 -1.30E-04  6.62E-06  8.67E-04  2.74E-04  3.88E-04  4.82E-05 -3.98E-04  2.75E-04  1.56E-03
 
 OM33
+       -1.15E-04  6.50E-05 -8.35E-05 -1.77E-04 -1.54E-05  9.72E-05  1.20E-04  5.83E-05  1.50E-04  8.82E-05  3.28E-04  4.07E-06
          3.10E-05 -2.85E-05  7.53E-05  1.75E-04  2.37E-05  7.54E-06  4.29E-05  2.51E-05  4.93E-05 -4.21E-05  2.56E-05  1.35E-03
 
 OM34
+       -9.72E-05  4.32E-05  1.42E-04  1.93E-04  4.02E-05  8.36E-05 -9.92E-05  1.38E-05  1.23E-05  1.57E-05  1.16E-04  2.34E-04
         -3.49E-05  7.04E-05 -6.42E-05 -3.12E-05  2.67E-05  2.27E-04  2.55E-05  3.22E-07  4.82E-05  1.72E-06  7.65E-05 -9.37E-05
         1.03E-03
 
 OM35
+        3.35E-05 -1.39E-04 -5.00E-05 -7.09E-06  3.17E-05 -2.24E-05 -8.13E-05 -3.26E-05  9.34E-05  4.34E-05  1.28E-04 -8.80E-06
          1.92E-04 -5.97E-05  4.47E-05  8.34E-05  5.50E-05  7.79E-05 -2.24E-05  1.77E-05 -2.48E-05  6.58E-05  7.59E-05  7.27E-05
        -1.52E-04  8.27E-04
 
 OM36
+       -7.88E-05 -3.36E-06 -6.87E-06 -5.00E-05 -2.52E-05  6.26E-05  7.77E-05 -6.86E-05 -5.92E-05  4.08E-05 -1.13E-04  5.54E-05
         -6.13E-05  1.89E-04  1.40E-07 -4.00E-05 -3.85E-05 -7.89E-06 -8.91E-06 -3.29E-05 -1.17E-05 -3.70E-05 -3.41E-05  1.06E-05
         6.32E-05 -2.62E-04  9.86E-04
 
 OM37
+       -8.57E-06 -1.30E-04 -7.44E-05 -1.85E-04  6.83E-05 -1.16E-05  1.99E-04  4.47E-05  1.04E-04 -6.88E-05  1.11E-04 -1.46E-04
          5.85E-05 -4.74E-05  2.41E-04  1.24E-04  2.56E-05 -2.16E-04  4.16E-05 -1.42E-06 -4.73E-05 -6.32E-05 -4.10E-05  2.50E-04
        -3.36E-04  1.30E-04 -3.50E-05  9.26E-04
 
 OM38
+       -1.16E-04  6.48E-05  7.46E-05 -3.31E-05  9.90E-05  7.47E-05  4.56E-05  1.03E-04  2.07E-04  6.09E-05  4.25E-04  9.61E-06
          2.36E-05 -4.67E-05  1.20E-04  3.54E-04  6.88E-05  2.85E-04  6.76E-05 -4.91E-06  5.92E-05 -3.19E-05  8.82E-05  4.63E-04
         1.62E-04  5.77E-05 -2.05E-04  2.45E-04  9.57E-04
 
 OM44
+        1.34E-04 -2.63E-04  3.99E-04  2.22E-04 -2.75E-05  5.65E-05  6.46E-05 -1.23E-04  1.67E-04 -1.92E-05  1.09E-04  4.55E-04
         -6.19E-05  3.02E-05 -3.05E-05  9.31E-05  1.44E-04  4.96E-05  7.38E-04 -3.28E-05  5.40E-05 -2.04E-04  6.73E-05 -2.43E-05
         1.24E-04 -6.46E-06  1.76E-05 -2.99E-05  4.88E-05  3.89E-03
 
 OM45
+       -7.54E-05  6.50E-05 -6.22E-05 -1.16E-04 -7.14E-05 -5.21E-05  7.01E-05  6.05E-06  4.67E-05  5.25E-05  1.81E-05  1.45E-04
          8.70E-05 -5.16E-05 -6.17E-05  8.42E-05  1.54E-04  6.81E-06  1.37E-04  2.31E-04 -2.43E-05 -3.50E-06  1.23E-04 -6.66E-06
        -7.51E-06 -2.92E-05 -1.89E-05 -5.84E-05 -8.76E-06 -4.07E-04  1.29E-03
 
 OM46
+        8.24E-05 -1.69E-04  1.63E-05  1.09E-04  8.63E-05  2.76E-04  3.00E-05  2.24E-06 -3.31E-05 -9.31E-05  2.55E-05 -2.11E-04
         -2.36E-06  1.12E-04  7.06E-05 -4.16E-05 -1.54E-04  1.92E-05 -1.83E-05 -6.18E-05  3.35E-04  9.74E-05 -1.73E-04  6.63E-06
         3.30E-05 -2.37E-05 -4.33E-05 -1.45E-06  6.26E-05  3.45E-04 -4.22E-04  1.71E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.03E-04 -6.79E-05 -1.02E-04 -1.47E-04  1.29E-04  4.36E-05  5.37E-05 -7.18E-05  1.40E-05  1.04E-05 -6.83E-05  5.33E-05
          5.05E-05 -2.91E-05  9.25E-05  7.17E-05 -1.64E-04 -2.47E-05 -5.08E-04  8.74E-05 -3.26E-05  4.87E-04  3.91E-05 -9.67E-05
         1.36E-04  8.80E-06  5.68E-06 -9.86E-05 -6.96E-05 -9.86E-04  2.24E-04 -5.33E-05  1.60E-03
 
 OM48
+       -7.00E-05  9.84E-05  1.05E-05 -1.66E-05  3.36E-05  3.58E-05 -1.08E-04  2.91E-05  1.40E-04  1.22E-04  9.44E-05  6.46E-04
         -4.34E-05  3.09E-05 -6.28E-05  2.02E-04  2.57E-04  8.67E-05  4.81E-04  4.75E-05 -7.57E-05  1.92E-05  4.59E-04  3.83E-05
         2.72E-04 -2.83E-05  3.44E-05 -1.05E-04  3.43E-05  6.20E-04  1.09E-04 -3.65E-04  2.25E-04  1.56E-03
 
 OM55
+        9.38E-05  2.77E-05  1.15E-05  7.85E-05 -1.46E-04 -4.18E-05  7.54E-06 -1.58E-05  1.89E-04  2.03E-05  1.12E-05 -8.32E-05
          3.55E-04 -1.06E-04  5.91E-05  8.74E-05  8.50E-05 -1.19E-05  6.50E-05  2.15E-04 -8.04E-05  2.39E-06  4.22E-06  4.98E-06
        -1.80E-05  6.53E-05 -7.30E-05  2.73E-05 -2.79E-05  2.08E-04 -3.39E-04  1.19E-04 -1.13E-04 -9.08E-06  2.22E-03
 
 OM56
+       -9.48E-05  3.14E-05 -3.61E-05 -1.11E-04 -1.15E-04  1.93E-05 -1.03E-04 -1.05E-04  5.28E-06  1.04E-07 -3.07E-05  1.01E-04
         -1.54E-04  2.50E-04 -6.59E-05 -4.63E-05  7.00E-06 -2.07E-06 -7.39E-05  1.92E-05  3.74E-05  1.71E-05  2.05E-06  2.90E-05
         4.77E-05  1.43E-05  7.28E-05 -8.62E-08 -1.52E-05 -1.31E-04  1.26E-04 -2.65E-04  6.81E-05  8.09E-05 -6.01E-04  1.43E-03
 
 OM57
+       -1.62E-05 -5.23E-05  1.69E-06 -1.76E-05 -1.11E-04  9.34E-05 -2.98E-05 -1.48E-04  4.65E-05 -1.11E-05  4.59E-05 -3.57E-05
          1.08E-04  4.01E-05  1.80E-04  1.11E-05 -7.80E-05  4.99E-05 -1.22E-06 -3.03E-04  1.04E-04  5.47E-05  4.27E-06  5.82E-05
         3.98E-06  1.53E-04 -3.95E-05 -3.30E-07  8.02E-05  9.87E-06 -3.85E-04  8.54E-05 -2.67E-04 -9.58E-05  2.25E-04 -4.48E-06
          1.30E-03
 
 OM58
+       -2.39E-05 -1.72E-06  1.12E-05 -1.19E-05 -2.03E-05  1.83E-04  1.68E-05 -4.16E-05  7.37E-05  1.21E-04  6.83E-05 -4.10E-06
          4.15E-04 -1.14E-04  2.88E-05  1.76E-04 -3.54E-05  5.06E-05 -7.19E-06  3.28E-04 -9.33E-05  5.19E-05  8.80E-05  6.23E-05
        -2.63E-05  2.21E-04 -6.79E-05  2.84E-05  4.53E-05 -1.68E-05  1.89E-04 -4.12E-05  4.49E-07 -1.28E-04  1.26E-04 -2.81E-04
          2.43E-04  1.12E-03
 
 OM66
+        2.48E-04  1.52E-05  8.23E-05  9.41E-05  1.28E-04 -1.61E-05  1.55E-05  8.92E-05  3.60E-05  4.62E-05 -7.05E-05 -6.74E-05
          8.74E-05 -1.51E-04  2.63E-05 -4.57E-06 -5.15E-06  4.82E-05  7.53E-05 -8.03E-05  5.53E-05  7.27E-05  1.31E-04 -5.77E-05
         1.28E-05  3.48E-05  1.72E-05 -3.32E-05 -2.51E-05  9.59E-05 -1.24E-04  1.02E-04 -8.96E-05 -5.35E-05  1.45E-04 -8.38E-04
          2.02E-05  1.21E-04  3.29E-03
 
 OM67
+        4.97E-06 -1.07E-04 -4.82E-05 -7.43E-05 -8.37E-05 -1.69E-04  1.63E-04  1.16E-06 -8.37E-05  5.96E-05 -9.54E-05  1.21E-04
         -1.02E-05  1.65E-04 -2.31E-04 -1.09E-04  1.39E-04 -5.23E-05 -2.36E-05  1.34E-04 -4.13E-04 -1.20E-04  1.05E-04  2.89E-05
        -5.03E-06 -1.38E-05  1.65E-04  4.28E-05 -1.01E-04 -7.95E-05  1.12E-04 -4.96E-04  1.52E-04  1.33E-04 -5.42E-05  1.89E-04
         -3.85E-04 -5.80E-05  2.37E-05  1.50E-03
 
 OM68
+       -1.25E-04 -2.80E-05 -1.36E-05  4.31E-05  3.22E-05  1.70E-04  1.01E-04 -1.14E-05 -1.41E-05 -8.74E-05 -7.84E-05  2.95E-05
         -1.00E-04  5.44E-04 -1.15E-05 -1.91E-04 -4.79E-05 -6.38E-05 -2.94E-05 -8.65E-05  4.35E-04  5.12E-05 -1.15E-04  4.31E-05
         2.52E-05 -8.85E-05  2.59E-04  2.53E-07 -4.23E-05 -2.81E-05 -1.66E-05  2.34E-04  8.21E-05  6.00E-05 -8.85E-05  1.59E-04
         -4.30E-05 -3.26E-04 -6.51E-04  3.00E-04  1.48E-03
 
 OM77
+       -3.83E-05  6.65E-06  6.66E-05  2.01E-05 -2.15E-04 -1.42E-04 -1.63E-04  2.58E-05  1.01E-04 -9.30E-05  1.28E-04 -9.23E-05
         -1.89E-05 -4.05E-05  2.68E-04  1.89E-04  2.38E-04 -5.77E-05  3.03E-04 -1.56E-04 -2.78E-05 -7.94E-04 -1.21E-04  6.48E-05
        -9.60E-05  6.37E-06 -4.54E-05  2.96E-04  1.14E-04  3.68E-04 -1.27E-04 -3.67E-05 -8.87E-04 -1.92E-04  1.75E-04 -1.03E-04
          3.38E-04  7.53E-05 -3.59E-06 -3.44E-05 -7.50E-05  2.85E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -1.11E-04 -7.72E-05  1.43E-04 -3.61E-05 -3.79E-05  6.62E-05  1.06E-04  3.43E-05  1.55E-04 -8.82E-05  9.16E-05 -1.89E-04
          8.44E-05 -1.34E-04  5.63E-04  3.69E-04 -1.95E-04 -6.17E-05 -4.50E-05 -6.08E-06  1.25E-04  2.42E-04 -2.21E-04  3.77E-05
        -5.60E-05  4.83E-05 -7.83E-05  2.77E-04  2.11E-04  2.74E-05 -2.84E-05  1.31E-04  6.78E-05 -3.25E-04  4.67E-05 -1.09E-04
          5.75E-05  1.58E-04  1.27E-05 -3.32E-04 -7.04E-05  6.42E-04  1.42E-03
 
 OM88
+       -3.70E-05  2.19E-06  1.61E-04 -1.54E-04  7.82E-05 -1.14E-05  1.38E-04  3.05E-05  4.72E-04  3.50E-04  3.13E-04  1.08E-04
          1.95E-04 -2.66E-04  3.76E-04  1.13E-03  1.96E-04  1.25E-04  2.85E-04  2.83E-05 -8.09E-05  1.48E-04  7.94E-04  2.05E-04
         1.68E-05  8.08E-05 -1.28E-04  1.68E-04  5.67E-04  3.89E-04  1.09E-04 -1.02E-04  1.82E-05  4.78E-04  1.16E-05 -3.07E-05
         -2.58E-05  1.39E-04  2.53E-04 -1.51E-04 -5.12E-04  3.16E-04  7.99E-04  2.74E-03
 
 SG11
+        1.97E-07 -3.39E-06 -9.65E-07 -2.32E-06  1.11E-06 -1.12E-06  1.76E-06 -1.90E-07 -2.47E-07  9.65E-07  9.52E-07 -5.74E-07
          2.09E-07  5.36E-07  1.03E-06  5.94E-07 -2.68E-06 -1.18E-06  3.90E-07  3.92E-07  2.73E-07  5.89E-07 -1.55E-06  4.19E-07
        -7.64E-07  3.96E-07 -1.08E-08  8.24E-07 -2.85E-07 -3.38E-07  1.05E-07 -2.06E-07 -3.29E-07 -4.15E-07 -2.18E-07 -2.56E-08
          3.86E-07  8.77E-07 -4.78E-07 -3.75E-07 -3.08E-07 -1.12E-06  2.55E-07  1.95E-07  3.96E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.10E-06 -1.26E-07  1.89E-06  8.78E-07 -1.09E-06 -7.93E-07 -9.37E-07 -2.47E-06  2.94E-06  1.87E-07 -1.55E-06  1.61E-06
          3.34E-07 -1.64E-07 -5.31E-07  3.74E-07  4.97E-07 -3.46E-07 -2.23E-06 -6.86E-08  1.49E-06 -1.10E-06 -6.69E-07 -1.27E-06
        -2.23E-07  9.31E-07 -1.41E-06  1.33E-06  5.90E-07 -3.36E-06 -5.09E-07 -5.50E-08  2.00E-06 -7.86E-07  1.81E-06  1.65E-06
         -2.26E-07 -2.50E-06 -5.14E-08  6.67E-07  2.66E-06 -1.03E-06  4.24E-07  6.77E-07 -1.34E-08  0.00E+00  1.48E-06
 
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
+        7.42E-02
 
 TH 2
+       -8.35E-02  7.88E-02
 
 TH 3
+        1.49E-01 -4.43E-02  5.84E-02
 
 TH 4
+        1.20E-01  1.87E-01  9.51E-03  7.64E-02
 
 TH 5
+        1.45E-01  6.79E-03  7.67E-03 -1.30E-01  6.69E-02
 
 TH 6
+       -4.97E-02 -3.60E-02  5.06E-02  9.04E-02 -2.39E-01  7.73E-02
 
 TH 7
+        1.07E-01 -2.67E-01  1.70E-01 -2.87E-01  1.97E-01 -2.89E-02  7.39E-02
 
 TH 8
+        3.56E-01  2.21E-01  2.27E-01  1.46E-01  1.24E-01 -2.25E-01  2.44E-01  6.89E-02
 
 OM11
+        4.97E-03 -1.04E-02 -1.08E-02 -4.06E-02  2.89E-02  3.13E-02  2.15E-02  1.90E-02  6.01E-02
 
 OM12
+       -2.94E-03  7.91E-02  3.59E-02 -4.08E-02 -4.98E-02  2.69E-02 -1.19E-04  1.67E-02 -1.35E-01  4.21E-02
 
 OM13
+       -9.73E-03 -2.42E-02  2.42E-03 -2.25E-02  8.21E-03  1.36E-02 -1.87E-02 -1.34E-02  2.77E-01  6.57E-03  3.27E-02
 
 OM14
+       -2.27E-04 -1.45E-02  6.93E-03 -7.30E-03 -1.85E-02  1.51E-02  1.39E-02 -1.23E-02  1.78E-01  1.89E-01 -1.94E-02  4.16E-02
 
 OM15
+       -3.34E-03 -6.12E-02 -2.48E-02 -1.02E-03  2.19E-02  6.12E-02  4.24E-02 -3.87E-02  1.71E-01  9.63E-02  5.00E-02 -1.05E-01
          3.67E-02
 
 OM16
+       -2.62E-03  1.33E-02 -2.49E-02  2.97E-02 -6.05E-02  5.26E-02 -3.45E-03 -2.68E-03 -1.01E-01  8.44E-03 -2.53E-02  5.31E-02
         -2.77E-01  4.10E-02
 
 OM17
+        1.98E-02 -3.85E-02 -3.90E-03 -1.02E-02  8.64E-02  2.19E-02 -1.87E-03  7.93E-03  2.09E-01 -2.21E-01  1.90E-01 -2.67E-01
          1.29E-01  2.64E-03  3.87E-02
 
 OM18
+       -2.25E-02  1.34E-02 -1.07E-03 -7.94E-02  5.68E-02  6.96E-03  1.59E-02  1.38E-02  5.13E-01  2.38E-01  3.12E-01  1.75E-01
          1.65E-01 -2.18E-01  3.06E-01  4.23E-02
 
 OM22
+        6.17E-03 -1.96E-01  2.59E-02 -1.93E-02 -4.50E-03 -1.80E-02  6.73E-03  4.38E-02  3.08E-02 -2.52E-01  3.00E-02 -4.30E-02
         -2.12E-03 -1.99E-02  3.77E-02 -3.83E-02  5.92E-02
 
 OM23
+       -1.62E-02  1.55E-01  2.84E-02  2.71E-02  2.98E-02  1.13E-02 -3.61E-02  3.66E-02  4.49E-03  1.88E-01 -9.16E-02  6.48E-02
          7.53E-03  4.86E-03 -8.63E-02  1.39E-02 -6.73E-02  3.10E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        1.01E-02 -1.63E-01  4.42E-02 -1.11E-02  9.69E-03 -4.70E-04  5.09E-02  5.63E-02 -6.07E-04 -2.14E-02  9.91E-02 -1.23E-01
          5.11E-02 -4.86E-02  7.49E-02  2.46E-02  4.00E-01 -5.60E-02  4.30E-02
 
 OM25
+        4.34E-03 -1.11E-03 -2.91E-02 -3.63E-02 -3.69E-02  1.95E-03 -1.75E-02 -1.48E-02 -1.07E-03  1.04E-01  3.88E-02  5.14E-02
         -1.26E-01  1.59E-02 -7.26E-02  3.49E-02  5.38E-02  9.61E-03 -9.98E-02  3.57E-02
 
 OM26
+       -5.65E-02  4.47E-02  7.16E-02  5.74E-03  3.38E-02  3.97E-02 -9.43E-03 -3.77E-03  6.06E-02 -8.04E-02  2.72E-02 -4.97E-02
          3.15E-02 -1.24E-01  2.25E-02  3.71E-02 -1.03E-01  4.74E-02  5.38E-02 -2.63E-01  3.92E-02
 
 OM27
+       -7.34E-03  1.57E-01  6.45E-03 -1.17E-02 -5.37E-02  6.70E-03 -1.32E-02 -6.87E-02 -5.42E-03  1.57E-01 -8.57E-02  5.22E-02
          5.49E-03  1.17E-02 -1.54E-01 -3.24E-02 -4.19E-01  1.76E-01 -3.94E-01  1.05E-01  6.07E-02  4.30E-02
 
 OM28
+        3.87E-02 -6.24E-04  4.70E-02 -2.15E-02 -1.83E-02 -1.46E-02  1.92E-02  3.64E-02 -3.40E-02  3.12E-01  1.71E-02  3.21E-02
          8.38E-02  2.07E-02 -8.53E-02  3.96E-03  3.71E-01  2.24E-01  2.29E-01  3.42E-02 -2.58E-01  1.62E-01  3.95E-02
 
 OM33
+       -4.22E-02  2.25E-02 -3.90E-02 -6.30E-02 -6.28E-03  3.43E-02  4.41E-02  2.31E-02  6.81E-02  5.71E-02  2.73E-01  2.67E-03
          2.31E-02 -1.89E-02  5.30E-02  1.13E-01  1.09E-02  6.63E-03  2.72E-02  1.91E-02  3.43E-02 -2.67E-02  1.77E-02  3.67E-02
 
 OM34
+       -4.09E-02  1.71E-02  7.61E-02  7.91E-02  1.87E-02  3.38E-02 -4.19E-02  6.27E-03  6.38E-03  1.17E-02  1.11E-01  1.76E-01
         -2.98E-02  5.36E-02 -5.18E-02 -2.30E-02  1.41E-02  2.29E-01  1.85E-02  2.82E-04  3.84E-02  1.25E-03  6.06E-02 -7.98E-02
         3.20E-02
 
 OM35
+        1.57E-02 -6.15E-02 -2.98E-02 -3.23E-03  1.65E-02 -1.01E-02 -3.82E-02 -1.65E-02  5.40E-02  3.59E-02  1.36E-01 -7.36E-03
          1.82E-01 -5.06E-02  4.02E-02  6.85E-02  3.23E-02  8.73E-02 -1.81E-02  1.72E-02 -2.20E-02  5.32E-02  6.69E-02  6.89E-02
        -1.65E-01  2.88E-02
 
 OM36
+       -3.38E-02 -1.36E-03 -3.75E-03 -2.09E-02 -1.20E-02  2.58E-02  3.35E-02 -3.17E-02 -3.14E-02  3.09E-02 -1.10E-01  4.24E-02
         -5.32E-02  1.47E-01  1.15E-04 -3.01E-02 -2.07E-02 -8.10E-03 -6.59E-03 -2.94E-02 -9.48E-03 -2.74E-02 -2.76E-02  9.23E-03
         6.29E-02 -2.90E-01  3.14E-02
 
 OM37
+       -3.80E-03 -5.41E-02 -4.19E-02 -7.96E-02  3.35E-02 -4.95E-03  8.86E-02  2.13E-02  5.68E-02 -5.38E-02  1.11E-01 -1.15E-01
          5.24E-02 -3.80E-02  2.04E-01  9.65E-02  1.42E-02 -2.29E-01  3.18E-02 -1.31E-03 -3.97E-02 -4.82E-02 -3.42E-02  2.24E-01
        -3.45E-01  1.49E-01 -3.67E-02  3.04E-02
 
 OM38
+       -5.06E-02  2.66E-02  4.13E-02 -1.40E-02  4.78E-02  3.12E-02  2.00E-02  4.82E-02  1.11E-01  4.68E-02  4.19E-01  7.47E-03
          2.08E-02 -3.68E-02  1.00E-01  2.70E-01  3.75E-02  2.97E-01  5.08E-02 -4.45E-03  4.88E-02 -2.40E-02  7.23E-02  4.08E-01
         1.63E-01  6.49E-02 -2.12E-01  2.60E-01  3.09E-02
 
 OM44
+        2.90E-02 -5.35E-02  1.10E-01  4.67E-02 -6.60E-03  1.17E-02  1.40E-02 -2.87E-02  4.46E-02 -7.33E-03  5.32E-02  1.75E-01
         -2.71E-02  1.18E-02 -1.27E-02  3.52E-02  3.90E-02  2.56E-02  2.75E-01 -1.47E-02  2.21E-02 -7.59E-02  2.73E-02 -1.06E-02
         6.23E-02 -3.60E-03  8.97E-03 -1.57E-02  2.53E-02  6.24E-02
 
 OM45
+       -2.83E-02  2.30E-02 -2.97E-02 -4.23E-02 -2.97E-02 -1.88E-02  2.64E-02  2.44E-03  2.16E-02  3.47E-02  1.54E-02  9.71E-02
          6.61E-02 -3.50E-02 -4.44E-02  5.54E-02  7.25E-02  6.11E-03  8.88E-02  1.80E-01 -1.72E-02 -2.27E-03  8.67E-02 -5.06E-03
        -6.53E-03 -2.83E-02 -1.67E-02 -5.34E-02 -7.88E-03 -1.82E-01  3.59E-02
 
 OM46
+        2.69E-02 -5.18E-02  6.76E-03  3.47E-02  3.12E-02  8.63E-02  9.81E-03  7.87E-04 -1.33E-02 -5.35E-02  1.88E-02 -1.23E-01
         -1.56E-03  6.63E-02  4.42E-02 -2.37E-02 -6.28E-02  1.50E-02 -1.03E-02 -4.18E-02  2.07E-01  5.47E-02 -1.06E-01  4.37E-03
         2.49E-02 -1.99E-02 -3.34E-02 -1.15E-03  4.89E-02  1.34E-01 -2.84E-01  4.13E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.48E-02 -2.15E-02 -4.37E-02 -4.81E-02  4.82E-02  1.41E-02  1.81E-02 -2.60E-02  5.84E-03  6.16E-03 -5.21E-02  3.20E-02
          3.44E-02 -1.77E-02  5.97E-02  4.23E-02 -6.90E-02 -1.99E-02 -2.95E-01  6.11E-02 -2.07E-02  2.82E-01  2.48E-02 -6.58E-02
         1.06E-01  7.64E-03  4.52E-03 -8.10E-02 -5.62E-02 -3.95E-01  1.56E-01 -3.22E-02  4.00E-02
 
 OM48
+       -2.39E-02  3.16E-02  4.57E-03 -5.50E-03  1.27E-02  1.17E-02 -3.70E-02  1.07E-02  5.91E-02  7.35E-02  7.30E-02  3.93E-01
         -2.99E-02  1.91E-02 -4.11E-02  1.21E-01  1.10E-01  7.07E-02  2.83E-01  3.37E-02 -4.89E-02  1.13E-02  2.95E-01  2.64E-02
         2.15E-01 -2.49E-02  2.77E-02 -8.77E-02  2.80E-02  2.52E-01  7.66E-02 -2.23E-01  1.42E-01  3.95E-02
 
 OM55
+        2.68E-02  7.45E-03  4.17E-03  2.18E-02 -4.63E-02 -1.15E-02  2.16E-03 -4.87E-03  6.69E-02  1.02E-02  7.27E-03 -4.25E-02
          2.05E-01 -5.46E-02  3.24E-02  4.38E-02  3.05E-02 -8.15E-03  3.21E-02  1.28E-01 -4.35E-02  1.18E-03  2.27E-03  2.88E-03
        -1.20E-02  4.82E-02 -4.93E-02  1.91E-02 -1.91E-02  7.08E-02 -2.00E-01  6.10E-02 -5.97E-02 -4.88E-03  4.71E-02
 
 OM56
+       -3.38E-02  1.06E-02 -1.64E-02 -3.83E-02 -4.53E-02  6.62E-03 -3.67E-02 -4.03E-02  2.33E-03  6.55E-05 -2.48E-02  6.45E-02
         -1.11E-01  1.61E-01 -4.51E-02 -2.90E-02  3.13E-03 -1.77E-03 -4.54E-02  1.42E-02  2.52E-02  1.05E-02  1.38E-03  2.09E-02
         3.94E-02  1.31E-02  6.14E-02 -7.50E-05 -1.30E-02 -5.54E-02  9.32E-02 -1.70E-01  4.50E-02  5.42E-02 -3.38E-01  3.78E-02
 
 OM57
+       -6.06E-03 -1.84E-02  8.03E-04 -6.37E-03 -4.60E-02  3.35E-02 -1.12E-02 -5.93E-02  2.14E-02 -7.27E-03  3.88E-02 -2.38E-02
          8.15E-02  2.71E-02  1.29E-01  7.26E-03 -3.65E-02  4.46E-02 -7.84E-04 -2.35E-01  7.34E-02  3.52E-02  3.00E-03  4.39E-02
         3.44E-03  1.48E-01 -3.48E-02 -3.00E-04  7.18E-02  4.38E-03 -2.97E-01  5.72E-02 -1.85E-01 -6.71E-02  1.32E-01 -3.29E-03
          3.61E-02
 
 OM58
+       -9.62E-03 -6.54E-04  5.72E-03 -4.66E-03 -9.05E-03  7.07E-02  6.78E-03 -1.80E-02  3.66E-02  8.60E-02  6.24E-02 -2.95E-03
          3.38E-01 -8.33E-02  2.23E-02  1.24E-01 -1.79E-02  4.87E-02 -4.99E-03  2.75E-01 -7.11E-02  3.60E-02  6.66E-02  5.07E-02
        -2.46E-02  2.30E-01 -6.46E-02  2.78E-02  4.37E-02 -8.03E-03  1.57E-01 -2.98E-02  3.35E-04 -9.71E-02  7.97E-02 -2.22E-01
          2.01E-01  3.35E-02
 
 OM66
+        5.83E-02  3.37E-03  2.46E-02  2.15E-02  3.34E-02 -3.62E-03  3.65E-03  2.26E-02  1.04E-02  1.91E-02 -3.75E-02 -2.83E-02
          4.16E-02 -6.44E-02  1.19E-02 -1.88E-03 -1.52E-03  2.71E-02  3.05E-02 -3.92E-02  2.46E-02  2.94E-02  5.78E-02 -2.74E-02
         6.94E-03  2.11E-02  9.54E-03 -1.90E-02 -1.42E-02  2.68E-02 -6.03E-02  4.32E-02 -3.90E-02 -2.36E-02  5.37E-02 -3.87E-01
          9.73E-03  6.28E-02  5.73E-02
 
 OM67
+        1.73E-03 -3.50E-02 -2.13E-02 -2.51E-02 -3.23E-02 -5.65E-02  5.68E-02  4.33E-04 -3.59E-02  3.66E-02 -7.52E-02  7.52E-02
         -7.20E-03  1.04E-01 -1.54E-01 -6.63E-02  6.06E-02 -4.35E-02 -1.42E-02  9.67E-02 -2.72E-01 -7.21E-02  6.88E-02  2.03E-02
        -4.05E-03 -1.23E-02  1.36E-01  3.63E-02 -8.42E-02 -3.29E-02  8.02E-02 -3.09E-01  9.77E-02  8.68E-02 -2.97E-02  1.29E-01
         -2.75E-01 -4.47E-02  1.07E-02  3.88E-02
 
 OM68
+       -4.39E-02 -9.26E-03 -6.05E-03  1.47E-02  1.25E-02  5.71E-02  3.57E-02 -4.31E-03 -6.12E-03 -5.41E-02 -6.24E-02  1.84E-02
         -7.12E-02  3.46E-01 -7.72E-03 -1.18E-01 -2.11E-02 -5.36E-02 -1.78E-02 -6.31E-02  2.89E-01  3.10E-02 -7.58E-02  3.05E-02
         2.05E-02 -8.01E-02  2.15E-01  2.17E-04 -3.56E-02 -1.17E-02 -1.21E-02  1.47E-01  5.34E-02  3.95E-02 -4.89E-02  1.10E-01
         -3.10E-02 -2.53E-01 -2.95E-01  2.02E-01  3.84E-02
 
 OM77
+       -9.69E-03  1.58E-03  2.14E-02  4.93E-03 -6.02E-02 -3.44E-02 -4.13E-02  7.01E-03  3.16E-02 -4.14E-02  7.34E-02 -4.16E-02
         -9.69E-03 -1.85E-02  1.30E-01  8.38E-02  7.53E-02 -3.49E-02  1.32E-01 -8.20E-02 -1.33E-02 -3.46E-01 -5.73E-02  3.31E-02
        -5.62E-02  4.15E-03 -2.71E-02  1.82E-01  6.92E-02  1.11E-01 -6.60E-02 -1.66E-02 -4.15E-01 -9.09E-02  6.97E-02 -5.09E-02
          1.76E-01  4.22E-02 -1.17E-03 -1.66E-02 -3.66E-02  5.33E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -3.95E-02 -2.60E-02  6.51E-02 -1.26E-02 -1.50E-02  2.27E-02  3.80E-02  1.32E-02  6.87E-02 -5.57E-02  7.43E-02 -1.21E-01
          6.11E-02 -8.68E-02  3.87E-01  2.31E-01 -8.73E-02 -5.28E-02 -2.77E-02 -4.52E-03  8.47E-02  1.49E-01 -1.48E-01  2.72E-02
        -4.64E-02  4.46E-02 -6.62E-02  2.42E-01  1.81E-01  1.17E-02 -2.10E-02  8.42E-02  4.50E-02 -2.18E-01  2.63E-02 -7.65E-02
          4.22E-02  1.25E-01  5.90E-03 -2.28E-01 -4.86E-02  3.20E-01  3.77E-02
 
 OM88
+       -9.52E-03  5.32E-04  5.28E-02 -3.86E-02  2.23E-02 -2.82E-03  3.58E-02  8.47E-03  1.50E-01  1.59E-01  1.83E-01  4.96E-02
          1.02E-01 -1.24E-01  1.86E-01  5.10E-01  6.32E-02  7.69E-02  1.26E-01  1.52E-02 -3.95E-02  6.57E-02  3.85E-01  1.07E-01
         1.01E-02  5.37E-02 -7.79E-02  1.06E-01  3.50E-01  1.19E-01  5.80E-02 -4.72E-02  8.70E-03  2.31E-01  4.71E-03 -1.55E-02
         -1.37E-02  7.93E-02  8.44E-02 -7.43E-02 -2.55E-01  1.13E-01  4.05E-01  5.23E-02
 
 SG11
+        4.22E-03 -6.85E-02 -2.63E-02 -4.83E-02  2.64E-02 -2.30E-02  3.79E-02 -4.38E-03 -6.52E-03  3.65E-02  4.62E-02 -2.20E-02
          9.05E-03  2.08E-02  4.23E-02  2.23E-02 -7.19E-02 -6.06E-02  1.44E-02  1.75E-02  1.11E-02  2.17E-02 -6.24E-02  1.82E-02
        -3.79E-02  2.19E-02 -5.49E-04  4.31E-02 -1.46E-02 -8.60E-03  4.65E-03 -7.93E-03 -1.31E-02 -1.67E-02 -7.37E-03 -1.08E-03
          1.70E-02  4.17E-02 -1.33E-02 -1.54E-02 -1.28E-02 -3.35E-02  1.07E-02  5.93E-03  6.29E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.22E-02 -1.31E-03  2.67E-02  9.45E-03 -1.33E-02 -8.44E-03 -1.04E-02 -2.94E-02  4.02E-02  3.66E-03 -3.89E-02  3.18E-02
          7.48E-03 -3.29E-03 -1.13E-02  7.26E-03  6.89E-03 -9.17E-03 -4.27E-02 -1.58E-03  3.13E-02 -2.10E-02 -1.39E-02 -2.85E-02
        -5.71E-03  2.66E-02 -3.69E-02  3.60E-02  1.57E-02 -4.43E-02 -1.17E-02 -1.09E-03  4.10E-02 -1.63E-02  3.15E-02  3.59E-02
         -5.14E-03 -6.13E-02 -7.36E-04  1.41E-02  5.69E-02 -1.59E-02  9.24E-03  1.06E-02 -1.75E-02  0.00E+00  1.22E-03
 
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
+        2.31E+02
 
 TH 2
+        4.66E+01  2.31E+02
 
 TH 3
+       -1.76E+01  1.63E+01  3.37E+02
 
 TH 4
+       -2.20E+01 -1.67E+01  4.39E+00  2.12E+02
 
 TH 5
+       -3.72E+01 -2.34E+01  5.87E+00  2.18E+01  2.69E+02
 
 TH 6
+       -1.41E+01 -1.21E+01 -2.63E+01 -2.35E+01  5.55E+01  2.02E+02
 
 TH 7
+        1.14E+01  7.46E+01 -2.76E+01  6.63E+01 -4.57E+01 -2.40E+01  2.64E+02
 
 TH 8
+       -9.86E+01 -9.88E+01 -6.39E+01 -4.91E+01  1.24E+01  7.03E+01 -1.00E+02  3.46E+02
 
 OM11
+        2.31E+00  1.44E+00  1.29E+00  7.06E+00  3.24E+00 -1.55E+01  2.58E+00 -2.03E+01  4.92E+02
 
 OM12
+        2.76E+00 -5.62E+00 -2.51E+01  1.94E+01  2.06E+01 -1.98E+01  2.42E+01 -1.97E+01  2.79E+02  1.22E+03
 
 OM13
+       -2.22E+01 -7.37E+00  5.14E-01  1.46E+00  1.45E+01  1.18E+01  2.66E+00  3.63E+01 -1.51E+02 -5.27E+00  1.50E+03
 
 OM14
+       -1.21E+01  3.76E+01  1.20E+01 -9.89E+00 -4.31E+00 -3.97E-01 -1.63E+01  6.89E+00 -1.38E+02 -1.64E+02  1.27E+02  1.01E+03
 
 OM15
+        3.34E+00  3.23E+01  2.99E+01 -1.48E+01 -5.83E+00 -1.63E+01 -2.38E+01  1.49E+01 -1.36E+02 -2.39E+02  1.39E+01  1.41E+02
          1.20E+03
 
 OM16
+        3.21E+00 -9.58E-01  1.59E+01 -2.91E-01  2.55E+01 -8.73E+00 -1.12E+01 -6.93E+00 -3.99E+01 -1.67E+02 -9.33E+01 -4.94E+01
          3.35E+02  9.36E+02
 
 OM17
+       -2.43E+01 -1.92E+00 -7.19E+00 -5.11E+00 -3.52E+01 -1.02E+01  2.48E+01  1.17E+01  9.59E+00  3.66E+02 -8.14E+01  3.11E+02
         -1.66E+02 -1.96E+02  1.26E+03
 
 OM18
+        1.05E+01 -1.21E+01  1.55E+01  2.43E+01 -1.30E+01  1.15E+01  7.49E-01 -9.93E+00 -4.45E+02 -6.73E+02 -1.78E+02 -1.56E+02
          1.37E+02  3.15E+02 -4.94E+02  1.67E+03
 
 OM22
+        1.07E+01  3.96E+01 -8.13E+00  8.64E+00  9.73E+00 -3.34E+00  3.14E+01 -1.94E+01  4.16E+01  4.38E+02  7.50E+01 -4.38E+01
         -5.83E+01 -5.87E+01  1.62E+02 -2.14E+02  7.03E+02
 
 OM23
+       -4.45E+00 -5.58E+01  2.29E+01  7.56E+00 -2.27E+01 -7.84E+00 -1.22E+01  2.42E+00 -8.28E+01 -9.88E+01  4.91E+02  5.50E+01
          2.12E+01 -4.16E+01 -6.79E+01  1.54E+01  9.43E+01  1.74E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        5.93E+00  6.05E+01  1.51E+01 -1.25E+00 -1.95E+00 -9.16E-01 -9.32E+00 -2.11E+01 -2.72E+01 -8.43E+01 -7.75E+00  3.24E+02
          4.14E+01  1.02E+01  7.40E+01 -2.51E+01 -1.14E+02  8.55E+01  1.10E+03
 
 OM25
+       -1.76E+00  2.46E+01  1.47E+01  1.52E+01  9.10E+00  5.26E+00  5.88E-01  1.63E+00 -5.61E+01 -2.88E+02 -1.04E+02  8.00E+01
          4.29E+02  1.85E+02 -1.15E+02  1.60E+02 -2.37E+02 -5.97E+01  1.18E+02  1.36E+03
 
 OM26
+        2.11E+01 -1.28E+01 -4.88E+01  9.07E+00  3.85E+00  6.99E+00  5.49E-01  3.49E-01 -3.53E+01 -1.48E+02 -1.16E+02  2.13E+01
          1.31E+02  3.26E+02 -4.18E+01  1.55E+02 -1.48E+02 -2.10E+02 -1.51E+02  4.21E+02  1.23E+03
 
 OM27
+       -3.11E+01 -4.81E+01 -1.45E+01  7.87E+00  4.36E+01  2.71E+01 -2.84E+00  6.84E+01 -1.81E+01  2.75E+02  5.52E+01  8.92E+01
         -6.52E+01 -8.41E+01  4.11E+02 -1.80E+02  4.91E+02 -7.13E+01  3.73E+02 -2.82E+02 -2.74E+02  1.45E+03
 
 OM28
+       -7.43E+00 -9.75E+00 -2.52E+01  3.43E+00  2.96E+00  1.50E+01 -2.25E+01 -1.25E+01 -1.35E+02 -8.29E+02 -1.79E+02  5.32E+01
          1.13E+02  2.02E+02 -3.43E+02  7.28E+02 -7.71E+02 -4.07E+02 -2.58E+02  4.21E+02  6.49E+02 -9.14E+02  2.34E+03
 
 OM33
+        7.65E+00 -1.45E+01  3.10E+01  1.88E+01  1.31E+01 -1.14E+01 -1.82E+01 -1.36E+01 -2.24E+01 -8.22E+01 -1.23E+02 -1.02E+01
          2.81E+01  4.83E+01 -4.92E+01  9.00E+01 -1.84E+01  1.21E+02  2.63E+01  8.54E+00 -3.44E+01 -1.91E+01  2.39E+01  9.96E+02
 
 OM34
+        2.73E+01  1.73E+01 -3.15E+01 -3.11E+01 -2.51E+01 -6.53E+00  9.02E+00 -7.47E+00  7.05E+00  4.83E+01 -1.71E+02 -1.16E+02
         -4.84E+01 -2.45E+01  2.96E+00  7.85E+01  3.93E+01 -1.17E+02  2.30E+01 -3.00E+01 -6.14E+01  9.46E+01 -1.21E+02  1.56E+02
         1.42E+03
 
 OM35
+        8.64E+00  5.42E+01  5.31E+00 -8.95E+00 -1.97E+01  9.40E+00  4.48E+01 -2.04E+01  2.99E+01 -1.88E+01 -2.91E+02 -5.14E+01
         -1.10E+02  8.79E+00  9.67E+00  5.90E+00 -7.36E+01 -3.55E+02  1.72E+01  6.68E+01  6.66E+01 -5.57E+01  8.43E+01 -5.03E+01
         2.05E+02  1.63E+03
 
 OM36
+        1.39E+01  6.15E+00 -9.38E+00  4.35E+00  3.97E+00 -1.83E-01 -6.26E+00  1.22E+01  2.79E+01 -4.89E+01 -7.14E+01 -2.98E+01
          1.49E+01 -2.63E+01 -3.71E+01 -4.11E+01 -4.51E+01 -2.46E+02 -1.85E+01  8.34E+01  1.22E+02 -6.27E+00  1.49E+02 -1.44E+02
        -1.20E+02  4.48E+02  1.34E+03
 
 OM37
+        2.13E+00  5.19E-01  4.01E+01  1.68E+01 -1.84E+01 -9.42E+00 -3.91E+01 -7.40E+00 -3.46E+01  1.27E+01  1.70E+02  3.07E+01
         -1.33E+01  1.09E+01 -1.53E+02  3.82E+01  5.95E+01  5.46E+02  2.97E+01 -2.40E+01 -4.48E+01 -1.01E+01 -2.02E+02 -3.79E+01
         5.10E+02 -2.54E+02 -1.93E+02  1.76E+03
 
 OM38
+        4.00E+01  1.80E+01 -4.14E+01 -8.67E+00 -2.42E+01 -1.66E+01  2.25E+01 -4.03E+01  1.04E+02  5.59E+01 -7.19E+02 -3.87E+01
          1.40E+01  5.44E+00  1.38E+02 -1.23E+02 -1.04E+02 -9.79E+02 -4.71E+01  5.59E+01  1.55E+02  2.34E+01  3.29E+02 -5.36E+02
        -4.26E+02  3.02E+02  5.33E+02 -7.99E+02  2.53E+03
 
 OM44
+       -7.14E+00  5.50E+00 -3.69E+01 -1.24E+01  1.94E+00  1.15E+01 -1.25E+01  3.34E+01 -1.27E+01 -7.40E+00 -1.54E+01 -8.42E+01
          1.83E+01  1.43E+01 -4.03E+01  5.00E+01 -8.47E+00 -1.08E+01 -8.30E+01  2.51E+01  2.18E+01 -5.15E+01  8.06E+01  2.66E+01
        -6.94E+00 -1.34E+01 -2.39E+00  2.15E+01  1.71E+01  4.10E+02
 
 OM45
+       -7.03E+00 -2.80E+01  8.78E+00  7.97E+00  3.96E+01  1.78E+01 -2.89E+01  2.14E+01  1.86E+01  4.37E+01 -3.21E+01 -1.68E+02
         -1.02E+02 -2.10E+00 -4.61E+01  1.03E+01 -1.30E+01 -2.81E+01 -2.29E+02 -1.25E+02 -2.62E+01 -7.60E+01  2.00E+01  5.93E+00
         4.25E+01  6.89E+01  5.76E+01  7.06E+01  5.81E-02  1.05E+02  1.16E+03
 
 OM46
+       -5.35E+00  2.06E+01  2.17E+01  1.06E+00 -3.02E+00 -2.76E+01  4.81E+00 -1.92E+01  3.20E+01  4.72E+01 -2.02E+01 -1.69E+01
         -4.81E+01 -6.10E+01  1.44E+01 -3.72E+01  1.29E+01 -4.49E+01 -1.08E+02 -7.19E+01 -4.62E+01 -6.56E+01  3.13E+01 -2.68E+01
        -6.79E+01  3.19E+01  5.91E+01 -3.59E+01  2.92E+01 -1.05E+02  2.72E+02  8.81E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.00E+01  4.60E+01  7.50E+00  9.66E+00 -1.89E+01 -3.28E-01 -3.61E+00  5.62E+00 -4.96E+00 -4.40E+01  4.54E+01  9.32E+01
          4.51E+01  3.82E+01 -1.04E+02 -3.56E+01 -5.82E+01  1.09E+02  3.32E+02  1.20E+02 -8.30E+00 -6.91E+01 -1.84E+01  6.23E+01
        -1.02E+02 -4.79E+01  3.35E+00  8.42E+01 -1.18E+01  2.84E+02 -1.18E+02 -1.59E+02  1.25E+03
 
 OM48
+        2.12E+01 -3.84E+01  9.33E+00  1.70E+01 -3.09E+00 -2.62E+01  3.89E+01 -3.81E+01  8.43E+01  1.41E+02 -9.46E+01 -5.00E+02
         -5.91E+01  1.11E+01 -1.47E+02 -2.22E+00  5.29E+01 -9.25E+01 -5.04E+02 -1.30E+02  3.85E+01 -1.74E+02 -5.46E+01 -7.26E+01
        -2.06E+02  2.06E+01  5.79E+01 -1.24E+02  2.04E+02 -1.88E+02  1.22E+02  3.20E+02 -4.77E+02  1.46E+03
 
 OM55
+       -1.33E+01 -2.36E+01 -5.85E+00 -2.12E+00  2.46E+01  1.18E+01 -6.57E+00  1.31E+01 -1.15E+01  3.56E+01  3.96E+01  3.79E+00
         -2.34E+02 -8.44E+01  3.98E+01 -4.89E+01 -4.43E-01  8.45E+00 -4.20E+01 -2.57E+02 -6.56E+01  6.83E+00 -2.41E+01 -2.23E+01
        -2.32E+01 -2.34E+01  8.98E+00 -1.16E+01  3.72E+01 -1.63E+01  1.44E+02  2.75E+01 -2.90E+01  1.18E+01  6.27E+02
 
 OM56
+       -1.09E+01 -1.35E+01 -4.95E+00  1.48E+01  8.89E+00 -1.25E+01  2.23E+01  8.84E+00 -1.75E+01  6.58E+01  1.03E+02 -3.64E+00
         -1.58E+02 -2.47E+02  6.48E+01 -6.63E+01  4.16E+01  5.52E+01  6.79E+00 -2.76E+02 -2.59E+02  5.55E+01 -1.31E+02 -3.92E+01
        -8.00E+01 -1.48E+02 -9.37E+01 -3.06E+01  4.98E+00 -1.13E+01 -4.61E+01  1.04E+02 -1.58E+01  3.87E+01  3.25E+02  1.19E+03
 
 OM57
+       -5.29E+00  1.42E+01  7.82E+00  1.06E+01  3.92E+01  1.46E+01 -3.15E+01  3.25E+01 -1.10E+01 -1.03E+02  1.23E+01 -7.27E+01
          1.33E+02  7.19E+01 -2.59E+02  1.26E+02 -8.92E+01  3.20E+01 -4.73E+01  4.27E+02  1.50E+02 -2.77E+02  1.81E+02  6.26E+00
        -2.57E+01 -1.07E+02  3.92E+00  1.07E+02 -1.29E+02  9.03E+01  3.62E+02  9.25E+01  1.38E+02  1.75E+01 -1.49E+02 -2.47E+02
          1.30E+03
 
 OM58
+        5.95E+00 -2.82E+01 -1.78E+01 -4.85E+00 -2.73E+01 -4.81E+01  1.71E+01 -9.08E+00  8.11E+01  2.48E+02  7.72E+01 -4.42E+01
         -5.89E+02 -2.94E+02  2.39E+02 -3.11E+02  1.91E+02  5.41E+01 -1.27E+01 -7.23E+02 -3.26E+02  2.53E+02 -4.70E+02 -6.98E+01
        -1.10E+01 -2.92E+02 -1.35E+02  2.43E+01 -2.61E+01 -7.13E+01 -2.53E+02  4.52E-01 -9.07E+01  1.40E+02  1.93E+02  5.01E+02
         -5.85E+02  1.82E+03
 
 OM66
+       -1.15E+01 -2.67E+00  4.54E-01 -1.50E+00 -1.29E+01 -9.42E+00  6.01E+00 -2.61E-02 -2.20E+01  1.35E+01  7.74E+01  2.13E+00
         -4.97E+01 -1.08E+02 -8.31E+00  4.41E+00  2.11E+01  6.06E+01 -1.28E+00 -6.94E+01 -1.87E+02 -1.36E+00 -9.58E+01  9.69E-01
        -1.81E+01 -6.69E+01 -9.16E+01  2.54E+01 -4.25E+01  4.15E+00  1.72E-02 -3.20E+01  2.59E+01 -2.52E+00  7.09E+01  3.27E+02
         -6.65E+01  1.52E+02  4.54E+02
 
 OM67
+       -1.31E+00  5.64E+00 -8.37E+00  1.27E+00  3.88E+01  2.99E+01 -3.21E+01  1.08E+01  7.36E+00 -4.38E+01 -1.43E+01 -2.97E+01
          1.44E+01  9.30E+01  2.23E+01  6.33E+01 -5.88E+01 -7.59E+01 -8.51E+01  1.09E+02  4.18E+02 -9.50E+01  2.41E+02 -4.54E+01
        -6.63E+01 -7.22E+00 -4.35E-01 -1.28E+02  9.85E+01 -2.71E+01  1.43E+02  3.18E+02 -1.77E+02  1.53E+02 -5.22E+01 -2.19E+02
          3.78E+02 -2.51E+02 -1.67E+02  1.17E+03
 
 OM68
+        8.45E+00  1.48E+00  1.18E+01 -1.81E+01 -4.49E+01 -3.41E+01 -4.97E+00 -4.07E+00 -3.92E+00  1.51E+02  1.96E+02  3.94E+01
         -2.19E+02 -5.14E+02  8.90E+01 -2.20E+02  1.28E+02  2.65E+02  9.06E+01 -2.82E+02 -7.05E+02  1.44E+02 -5.25E+02 -1.40E+01
         8.24E+01 -1.25E+02 -3.25E+02  1.29E+02 -3.03E+02 -4.16E+00 -1.20E+02 -2.22E+02  4.36E+01 -2.06E+02  1.00E+02  3.19E+02
         -2.35E+02  6.10E+02  3.47E+02 -5.61E+02  1.62E+03
 
 OM77
+       -9.99E+00 -1.28E+01 -5.17E+00  8.38E+00  2.18E+01  2.01E+01  1.96E+01  2.05E+01 -1.33E+00  5.78E+01 -3.01E+01  3.59E+01
          2.08E+01 -1.95E+01  1.17E+02 -8.04E+01  8.24E+01 -5.29E+01  1.28E+02 -4.48E+00 -5.14E+01  4.01E+02 -2.11E+02  4.54E+00
        -1.59E+00  2.94E+01  2.86E+01 -9.91E+01  8.22E+01  2.71E+01 -6.25E+01 -3.86E+01  3.22E+02 -1.26E+02 -2.11E+01  3.56E+01
         -1.76E+02  5.79E+01  1.40E+01 -1.39E+02  5.67E+01  6.55E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        4.39E+01  2.46E+01 -3.04E+01 -5.88E-01  2.70E+01 -1.05E+01 -1.39E+01 -5.67E+01  4.83E+00 -2.42E+02 -6.31E+00 -1.62E+02
          1.08E+02  1.81E+02 -6.18E+02  3.55E+02 -2.63E+02 -5.42E+01 -3.16E+02  1.65E+02  2.79E+02 -8.02E+02  9.59E+02  4.48E+01
        -1.70E+02  2.72E+01  8.47E+01 -2.72E+02  8.02E+01 -3.59E+01  1.27E+02  1.48E+02 -3.34E+02  5.83E+02 -2.00E+01 -6.02E+01
          3.02E+02 -3.93E+02 -4.09E+01  4.25E+02 -4.49E+02 -5.24E+02  2.05E+03
 
 OM88
+       -1.18E+01 -2.63E+00  2.52E+00 -1.10E+01 -8.15E+00  5.29E-01 -1.44E+01  3.26E+01  1.00E+02  2.83E+02  1.46E+02  1.06E+02
         -1.02E+02 -1.69E+02  2.20E+02 -7.02E+02  2.35E+02  2.24E+02  1.38E+02 -1.46E+02 -2.86E+02  3.13E+02 -9.68E+02  1.11E+01
         1.35E+02 -5.61E+01 -1.16E+02  1.93E+02 -4.76E+02 -4.87E+01 -7.00E+01 -6.83E+01  9.07E+01 -3.31E+02  2.61E+01  3.23E+01
         -7.95E+01  2.71E+02  2.69E+01 -1.93E+02  5.09E+02  1.09E+02 -8.53E+02  1.21E+03
 
 SG11
+        2.04E+02  1.71E+03  7.67E+02  5.75E+02 -4.72E+02  3.84E+02 -7.04E+01 -6.82E+02  1.33E+02 -3.19E+03 -2.46E+03  3.76E+02
          6.72E+02 -1.02E+03 -2.84E+03  1.66E+03  6.50E+02  1.44E+03 -1.96E+03  1.70E+02  7.32E+01 -2.95E+03  5.54E+03 -1.50E+02
        -1.07E+01 -1.27E+02  3.53E+02 -1.85E+03  2.38E+03  6.79E+02  1.20E+02  8.93E+02  1.78E+03  1.49E+02  1.77E+02  1.31E+02
          1.05E+02 -2.73E+03  5.00E+02  5.54E+02 -4.98E+01  1.07E+03  2.88E+03 -2.62E+03  2.62E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -4.05E+02 -3.14E+02 -6.35E+02 -1.58E+02  3.30E+02  3.72E+02 -4.79E+01  8.96E+02 -9.05E+02 -5.35E+02  1.46E+03 -7.62E+02
         -2.92E+02  3.93E+02  5.27E+02  7.77E+02  2.72E+01  2.26E+02  6.96E+02 -5.95E+02 -3.99E+02  1.98E+03 -1.09E+01  8.58E+02
        -2.00E+02 -8.82E+02  1.14E+03 -1.39E+03 -8.22E+02  5.25E+02  2.19E+02  9.50E+01 -7.25E+02  5.30E+02 -7.72E+02 -9.08E+02
         -3.19E+02  1.51E+03 -5.45E+02  2.01E+02 -1.77E+03  6.60E+02 -4.74E+02 -7.95E+02  1.03E+04  0.00E+00  7.01E+05
 
 Elapsed postprocess time in seconds:     0.03
 #CPUT: Total CPU Time in Seconds,     7226.540
Stop Time: 
Tue 04/26/2016 
12:40 PM

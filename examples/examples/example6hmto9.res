Wed 04/20/2016 
01:53 PM
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

$EST METHOD=ITS INTERACTION SIGL=4 NITER=15 PRINT=1 NOABORT NOPRIOR=1 file=example6hmto9_its.ext
$EST METHOD=bayes INTERACTION NBURN=2000 NITER=0 PRINT=10 MASSRESET=1 NOPRIOR=0 file=example6hmto9_bayes.ext
$EST METHOD=NUTS INTERACTION  NBURN=1000 NITER=2000 PRINT=1 MASSRESET=0 OLKJDF=8.0 file=example6hmto9.ext
     PMADAPT=500
$COV MATRIX=R UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       20 APR 2016
Days until program expires :5152
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
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY (ATOL): -1
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
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmto9_its.ext
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
 iteration            1 OBJ=  -3598.21506635562
 iteration            2 OBJ=  -3711.98787459366
 iteration            3 OBJ=  -3819.33626533416
 iteration            4 OBJ=  -3923.76725185384
 iteration            5 OBJ=  -4026.32377066653
 iteration            6 OBJ=  -4127.37768517388
 iteration            7 OBJ=  -4226.83356670354
 iteration            8 OBJ=  -4324.33278260022
 iteration            9 OBJ=  -4419.00156579333
 iteration           10 OBJ=  -4509.14958265951
 iteration           11 OBJ=  -4591.55241331424
 iteration           12 OBJ=  -4659.22823279541
 iteration           13 OBJ=  -4699.40190265713
 iteration           14 OBJ=  -4708.74408433246
 iteration           15 OBJ=  -4709.84144678724
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -8.1982E-04 -3.0054E-03  2.3810E-03  1.4347E-03  1.4647E-03  2.1233E-03  5.5637E-04  1.1409E-03
 SE:             6.9304E-02  5.2691E-02  3.7774E-02  6.5151E-02  5.6769E-02  5.7217E-02  6.4186E-02  6.1426E-02
 N:                      50          50          50          50          50          50          50          50
 
 P VAL.:         9.9056E-01  9.5452E-01  9.4974E-01  9.8243E-01  9.7942E-01  9.7040E-01  9.9308E-01  9.8518E-01
 
 ETAshrink(%):   6.3633E-01  4.2210E+00  8.0365E+00  1.6158E+00  1.4940E+00  5.7492E+00  3.5150E-01  1.5659E+00
 EBVshrink(%):   6.3166E-01  5.4602E+00  9.8638E+00  2.1012E+00  1.5630E+00  6.3061E+00  4.5533E-01  1.7658E+00
 EPSshrink(%):   1.5675E+01  7.2334E+00
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -4709.84144678724     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1828.05020665738     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:    52.49
 Elapsed covariance  time in seconds:     0.24
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -4709.841       **************************************************
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
+        2.89E-02 -3.37E-02  3.17E-02 -7.21E-02  2.37E-02  3.38E-03  2.12E-01
 
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
+        1.35E-01  2.52E-01 -1.52E-01  4.73E-01
 
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
 
         3.49E-01  2.25E-01  2.30E-01  3.12E-01  2.16E-01  2.66E-01  1.50E-01  3.84E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.50E-01
 
 ETA2
+        2.23E-01  2.80E-01
 
 ETA3
+        1.05E-01  1.23E-01  1.81E-01
 
 ETA4
+        8.69E-02  1.31E-01  1.04E-01  1.68E-01
 
 ETA5
+        1.40E-01  9.08E-02  8.63E-02  1.66E-01  3.09E-01
 
 ETA6
+        1.12E-01  1.62E-01  1.34E-01  1.53E-01  8.27E-02  1.55E-01
 
 ETA7
+        1.61E-01  1.06E-01  1.19E-01  9.44E-02  1.01E-01  1.03E-01  1.45E-01
 
 ETA8
+        2.01E-01  1.48E-01  5.91E-02  9.30E-02  1.42E-01  2.02E-01  1.29E-01  2.64E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        3.46E-03
 
 EPS2
+        0.00E+00  4.29E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.51E-01
 
 ETA2
+        1.04E+00  3.56E-01
 
 ETA3
+        7.09E-01  9.47E-01  3.09E-01
 
 ETA4
+        3.55E-01  6.30E-01  8.49E-01  1.78E-01
 
 ETA5
+        7.05E-01  5.66E-01  7.14E-01  9.43E-01  3.75E-01
 
 ETA6
+        5.44E-01  9.43E-01  1.03E+00  7.32E-01  8.17E-01  1.79E-01
 
 ETA7
+        6.85E-01  5.38E-01  9.70E-01  4.10E-01  5.96E-01  5.21E-01  1.58E-01
 
 ETA8
+        5.90E-01  7.44E-01  3.39E-01  3.62E-01  7.78E-01  9.17E-01  5.68E-01  2.96E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        1.80E-02
 
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
+        1.22E-01
 
 TH 2
+       -3.29E-02  5.04E-02
 
 TH 3
+        3.05E-02 -1.48E-02  5.28E-02
 
 TH 4
+        8.14E-02 -2.55E-02 -2.41E-03  9.74E-02
 
 TH 5
+       -5.53E-02  2.67E-02 -1.99E-03 -5.02E-02  4.68E-02
 
 TH 6
+       -6.04E-02  2.14E-02 -1.86E-02 -3.85E-02  3.30E-02  7.05E-02
 
 TH 7
+       -1.50E-02 -4.11E-03  9.21E-03 -2.22E-02  1.17E-02  1.07E-02  2.26E-02
 
 TH 8
+        1.06E-01 -4.07E-02  1.92E-02  9.49E-02 -6.01E-02 -5.64E-02 -1.04E-02  1.47E-01
 
 OM11
+       -3.03E-02  5.85E-03 -2.08E-02 -9.90E-03  1.24E-02  1.56E-02  5.12E-04 -1.91E-02  2.26E-02
 
 OM12
+       -2.19E-02  3.08E-02  1.30E-03 -2.85E-02  2.29E-02  1.74E-02 -3.11E-03 -5.52E-02 -3.82E-03  4.99E-02
 
 OM13
+       -1.09E-02 -8.74E-03  2.92E-03 -7.97E-03  3.49E-03  3.01E-03  4.59E-03 -8.64E-05  6.80E-03 -1.32E-02  1.11E-02
 
 OM14
+       -8.14E-03 -3.22E-03  3.49E-03 -1.23E-02  3.93E-03 -3.44E-03  6.75E-03 -8.66E-03  5.94E-04 -3.46E-03  3.22E-03  7.55E-03
 
 OM15
+        1.22E-02 -5.89E-03 -3.90E-03  1.67E-02 -6.78E-03  7.67E-03 -6.58E-03  9.72E-03  3.11E-04  1.13E-03 -1.65E-03 -8.41E-03
          1.96E-02
 
 OM16
+       -1.71E-02  1.44E-02 -1.49E-03 -1.78E-02  1.30E-02  5.38E-03  2.04E-03 -1.49E-02  3.12E-03  8.78E-03 -2.94E-04  1.95E-03
         -9.01E-03  1.26E-02
 
 OM17
+        3.51E-03 -2.73E-03 -2.13E-02  2.42E-02 -7.29E-03  7.23E-04 -7.51E-03  2.49E-02  1.40E-02 -1.76E-02  3.14E-03 -4.94E-03
          6.43E-03 -2.86E-03  2.58E-02
 
 OM18
+       -3.07E-02  1.74E-02 -3.52E-02 -1.07E-03  1.28E-02  2.80E-02 -5.63E-03 -1.97E-02  2.18E-02  7.37E-03 -1.96E-03 -5.83E-03
          7.13E-03  1.90E-03  2.14E-02  4.02E-02
 
 OM22
+       -2.74E-02 -2.23E-02 -3.46E-02  1.50E-03 -8.65E-03  1.80E-02  2.88E-03  1.26E-02  2.06E-02 -4.23E-02  1.56E-02  1.85E-03
          4.71E-03 -7.35E-03  2.31E-02  1.75E-02  7.84E-02
 
 OM23
+        6.50E-03  5.68E-03  1.46E-02 -1.03E-02  5.16E-03 -5.81E-03  3.89E-03 -1.18E-02 -9.58E-03  1.46E-02 -6.47E-03  2.69E-03
         -5.73E-03  2.92E-03 -1.32E-02 -1.08E-02 -2.61E-02  1.52E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -1.22E-02 -2.99E-03 -1.34E-02  2.14E-03 -1.15E-03  1.33E-02 -5.36E-03 -6.31E-03  6.02E-03  2.41E-03  2.06E-04 -6.01E-03
          1.18E-02 -3.80E-03  6.28E-03  1.29E-02  1.66E-02 -8.97E-03  1.72E-02
 
 OM25
+        2.95E-03  9.00E-05 -1.12E-03 -1.94E-03 -8.00E-04 -2.33E-03  3.15E-03  1.71E-03  7.07E-04 -4.30E-03  4.64E-04  2.55E-03
         -4.60E-03  1.94E-03 -2.48E-04 -3.80E-03  1.86E-03  2.37E-03 -6.36E-03  8.25E-03
 
 OM26
+       -2.41E-02  2.37E-02 -1.94E-02 -1.38E-02  1.67E-02  2.70E-02 -2.99E-03 -2.84E-02  9.51E-03  1.98E-02 -6.13E-03 -6.12E-03
          4.81E-03  7.24E-03  3.95E-03  2.18E-02 -2.59E-03 -2.98E-05  8.22E-03 -1.74E-03  2.63E-02
 
 OM27
+       -1.39E-02  1.67E-02 -3.61E-03 -1.47E-02  1.37E-02  5.99E-03  1.26E-03 -2.38E-02  3.45E-03  1.50E-02 -3.73E-03  6.99E-04
         -5.05E-03  6.99E-03 -3.29E-03  5.78E-03 -1.46E-02  6.23E-03 -4.29E-03  2.15E-03  9.63E-03  1.12E-02
 
 OM28
+       -3.80E-02  1.66E-02 -1.74E-02 -2.72E-02  1.81E-02  1.96E-02  6.48E-04 -4.75E-02  1.15E-02  1.93E-02 -1.10E-03  2.37E-04
         -6.77E-04  6.36E-03 -2.75E-03  1.38E-02  2.93E-03 -4.33E-04  7.42E-03  4.26E-05  1.41E-02  8.62E-03  2.19E-02
 
 OM33
+        4.49E-02 -2.58E-02  2.27E-03  3.99E-02 -3.14E-02 -2.48E-02 -6.37E-03  6.18E-02 -6.58E-03 -3.10E-02  2.60E-03 -3.09E-03
          6.99E-03 -1.05E-02  1.26E-02 -8.34E-03  1.98E-02 -8.57E-03  9.85E-04  8.62E-04 -1.44E-02 -1.39E-02 -1.99E-02  3.29E-02
 
 OM34
+        1.34E-02 -1.37E-02  8.80E-03  1.08E-02 -8.63E-03 -1.57E-02  3.90E-03  2.22E-02 -2.68E-03 -1.56E-02  4.30E-03  3.96E-03
         -4.71E-03 -2.27E-03  1.61E-03 -1.05E-02  4.07E-03 -3.00E-04 -6.42E-03  3.20E-03 -1.29E-02 -4.38E-03 -9.29E-03  9.62E-03
         1.07E-02
 
 OM35
+        2.48E-04  5.59E-03 -8.67E-03  3.32E-03 -5.11E-04  9.42E-03 -4.32E-03 -4.51E-03  1.00E-03  4.07E-03 -3.28E-03 -3.49E-03
          6.76E-03 -2.67E-03  3.00E-03  8.62E-03  2.61E-03 -1.64E-03  4.43E-03 -2.12E-03  6.93E-03  2.86E-05  2.05E-03 -1.63E-04
        -6.25E-03  7.45E-03
 
 OM36
+        1.49E-02  2.88E-03 -1.25E-02  1.56E-02 -1.27E-02 -1.55E-02 -1.13E-02  1.38E-02  4.61E-05  1.45E-03 -6.49E-03 -4.34E-03
          2.57E-03  1.69E-03  7.00E-03  4.53E-03 -1.05E-03 -1.09E-03  3.91E-03  3.36E-04  5.07E-03  1.26E-03  1.71E-03  7.34E-03
        -1.69E-03  1.04E-03  1.80E-02
 
 OM37
+       -2.25E-02  1.25E-02 -1.80E-02 -1.01E-02  7.77E-03  1.54E-02 -4.85E-03 -2.29E-02  1.04E-02  9.05E-03 -8.21E-05 -3.40E-03
          4.91E-03  2.92E-03  4.69E-03  1.57E-02  8.39E-03 -6.40E-03  9.60E-03 -3.25E-03  1.30E-02  3.27E-03  1.28E-02 -7.94E-03
        -9.36E-03  5.39E-03  4.17E-03  1.41E-02
 
 OM38
+        9.75E-03 -7.86E-03  1.41E-03  8.24E-03 -6.83E-03 -3.32E-03 -8.48E-04  1.39E-02 -3.93E-04 -9.02E-03  2.57E-03 -7.34E-04
          2.99E-03 -2.99E-03  3.54E-03 -1.65E-03  6.59E-03 -2.94E-03  5.25E-04 -1.97E-05 -3.31E-03 -3.66E-03 -5.01E-03  8.42E-03
         1.99E-03  6.71E-04  1.69E-04 -9.56E-04  3.50E-03
 
 OM44
+       -2.50E-02 -1.00E-02  1.39E-02 -2.51E-02  1.14E-02  3.62E-03  8.77E-03 -2.00E-02  3.51E-03 -2.56E-03  1.11E-02  5.30E-03
         -2.19E-03 -4.81E-04 -7.32E-03 -8.24E-03  6.75E-03 -1.76E-03  3.81E-03 -3.20E-03 -9.15E-03 -3.02E-03  4.34E-03 -6.93E-03
         3.18E-03 -6.69E-03 -9.80E-03 -5.35E-04 -6.76E-04  2.84E-02
 
 OM45
+       -2.25E-02  2.40E-02 -1.27E-02 -1.76E-02  1.74E-02  2.65E-02 -1.95E-03 -3.96E-02  4.79E-03  2.79E-02 -7.08E-03 -3.80E-03
          4.41E-03  5.48E-03 -4.96E-03  1.53E-02 -1.39E-02  3.40E-03  4.86E-03 -1.74E-03  2.18E-02  1.02E-02  1.52E-02 -2.01E-02
        -1.43E-02  7.74E-03  6.53E-05  1.24E-02 -4.07E-03 -8.21E-03  2.74E-02
 
 OM46
+       -1.99E-02 -5.41E-03  6.15E-06 -1.59E-02  8.69E-03  1.28E-02  1.28E-02 -1.17E-03  7.12E-03 -1.72E-02  1.04E-02  5.44E-03
         -8.19E-03  4.35E-03  1.18E-03 -2.84E-03  2.29E-02 -4.91E-03 -3.88E-03  6.01E-03 -2.95E-03 -1.83E-03 -5.28E-04  7.17E-04
         5.74E-03 -4.65E-03 -1.02E-02 -2.65E-03  1.59E-03  7.53E-03 -6.80E-03  2.34E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.10E-02  8.86E-03 -2.45E-03 -6.37E-03  9.55E-03  1.03E-02  5.80E-03 -7.64E-03  4.88E-03  1.73E-03  9.53E-04  1.69E-03
         -4.70E-03  4.57E-03  1.08E-03  5.34E-03 -1.93E-03  6.70E-05 -4.06E-03  1.79E-03  4.48E-03  4.65E-03  2.70E-03 -7.28E-03
        -5.37E-04 -2.08E-04 -4.81E-03  1.05E-03 -1.33E-03 -2.95E-03  5.29E-03  6.08E-03  8.91E-03
 
 OM48
+       -2.10E-02  2.37E-03 -1.30E-02 -7.72E-03  5.84E-03  1.05E-02  1.70E-03 -1.60E-02  8.19E-03 -7.84E-04  2.39E-03  1.29E-03
          1.04E-03 -1.01E-03  4.19E-03  1.05E-02  1.40E-02 -5.47E-03  6.39E-03 -1.99E-03  3.86E-03  5.99E-04  7.63E-03 -4.78E-03
        -2.28E-03  1.69E-03 -1.12E-03  5.91E-03 -1.09E-03  4.66E-03  2.89E-03  1.70E-03  1.52E-03  8.65E-03
 
 OM55
+       -6.51E-02  3.50E-02 -1.51E-02 -6.02E-02  4.17E-02  3.44E-02  7.60E-03 -9.82E-02  1.76E-02  4.90E-02 -3.73E-03  1.38E-03
         -3.43E-03  1.49E-02 -1.52E-02  1.71E-02 -2.22E-02  9.82E-03  6.17E-03  2.87E-03  2.74E-02  2.28E-02  4.02E-02 -4.69E-02
        -1.65E-02 -4.97E-04  1.50E-04  1.88E-02 -1.20E-02  1.17E-02  3.20E-02 -3.32E-03  7.01E-03  8.87E-03  9.53E-02
 
 OM56
+       -8.64E-03  1.11E-02 -1.13E-02 -5.74E-03  3.71E-03  6.00E-03 -4.82E-03 -1.33E-02  3.12E-03  8.22E-03 -3.64E-03 -1.95E-03
          1.82E-04  3.46E-03  1.83E-03  8.45E-03 -5.35E-04 -1.27E-04  2.71E-03  3.37E-04  8.89E-03  4.55E-03  6.89E-03 -5.34E-03
        -5.76E-03  3.28E-03  5.41E-03  6.22E-03 -1.55E-03 -5.25E-03  8.14E-03 -3.93E-03  2.48E-04  1.82E-03  1.11E-02  6.85E-03
 
 OM57
+        4.46E-04 -1.88E-03 -1.42E-02  1.08E-02 -3.26E-03  1.24E-03 -3.33E-03  1.24E-02  7.60E-03 -1.23E-02  2.83E-03 -1.74E-03
          2.43E-03 -1.50E-03  1.28E-02  1.04E-02  1.75E-02 -8.93E-03  2.49E-03  1.27E-03  1.74E-03 -1.48E-03 -1.37E-03  8.04E-03
         8.68E-04  2.08E-03  2.67E-03  3.15E-03  2.74E-03 -4.62E-03 -2.41E-03  3.97E-03  8.24E-04  2.89E-03 -9.69E-03  1.59E-03
          1.03E-02
 
 OM58
+        2.55E-02 -1.30E-02 -8.69E-03  3.10E-02 -1.84E-02 -9.35E-03 -5.28E-03  3.93E-02  2.20E-03 -2.23E-02  2.18E-03 -3.43E-03
          8.15E-03 -7.48E-03  1.56E-02  3.12E-03  1.92E-02 -9.25E-03  1.48E-03  3.96E-03 -5.51E-03 -7.70E-03 -9.94E-03  2.07E-02
         5.74E-03  1.94E-03  5.15E-03 -2.37E-03  5.67E-03 -8.44E-03 -9.69E-03  2.39E-03 -2.12E-03 -1.02E-03 -2.64E-02 -2.22E-03
          9.81E-03  2.02E-02
 
 OM66
+       -3.21E-02  1.45E-02 -8.93E-03 -2.65E-02  1.81E-02  2.73E-02  4.53E-03 -3.38E-02  4.33E-03  1.27E-02 -6.20E-04  1.56E-04
          1.17E-03  3.92E-03 -5.30E-03  1.05E-02  5.32E-03  4.55E-04  6.08E-03 -4.60E-03  1.41E-02  3.42E-03  1.04E-02 -1.34E-02
        -1.03E-02  6.39E-03 -5.43E-03  9.74E-03 -1.92E-03  1.25E-03  1.53E-02  3.40E-03  3.43E-03  5.08E-03  1.44E-02  2.82E-03
         -2.00E-03 -9.46E-03  2.40E-02
 
 OM67
+        1.53E-02 -1.72E-02  6.61E-03  1.01E-02 -1.40E-02 -1.19E-02 -6.82E-04  1.74E-02 -6.21E-03 -9.85E-03  1.59E-03  5.42E-04
          2.74E-03 -6.16E-03 -2.02E-03 -1.05E-02  7.20E-03 -9.21E-04  2.04E-03 -9.08E-04 -9.91E-03 -7.86E-03 -6.67E-03  1.13E-02
         4.67E-03 -1.91E-03  1.84E-03 -4.73E-03  2.37E-03  4.08E-03 -1.00E-02 -1.24E-03 -6.58E-03 -1.65E-03 -1.50E-02 -4.03E-03
         -1.77E-03  3.97E-03 -4.58E-03  1.07E-02
 
 OM68
+        5.94E-02 -1.35E-02  9.08E-03  4.76E-02 -3.12E-02 -3.21E-02 -1.23E-02  6.42E-02 -1.34E-02 -1.21E-02 -7.52E-03 -7.77E-03
          7.27E-03 -4.73E-03  8.28E-03 -9.89E-03 -1.02E-02  6.00E-04 -1.45E-03 -3.62E-04 -6.19E-03 -7.73E-03 -1.93E-02  2.65E-02
         6.34E-03 -3.55E-04  1.55E-02 -9.10E-03  4.75E-03 -1.57E-02 -1.26E-02 -1.13E-02 -7.39E-03 -1.10E-02 -3.67E-02 -2.63E-03
          1.77E-03  1.49E-02 -1.70E-02  8.64E-03  4.07E-02
 
 OM77
+       -8.81E-03  1.03E-02 -5.21E-03 -2.93E-03  5.49E-03  1.60E-03 -8.26E-03 -2.45E-02  1.29E-03  1.97E-02 -6.48E-03 -2.69E-03
          4.20E-03 -1.57E-03 -1.85E-03  8.03E-03 -1.42E-02  4.11E-03  5.20E-03 -3.37E-03  5.85E-03  4.30E-03  9.48E-03 -1.13E-02
        -6.27E-03  2.90E-03  3.95E-03  5.59E-03 -4.27E-03  1.15E-03  8.58E-03 -1.43E-02 -4.10E-03  2.74E-03  2.01E-02  4.27E-03
         -3.73E-03 -6.33E-03  1.98E-03 -1.99E-03 -4.80E-03  2.11E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.40E-02  5.30E-03 -9.17E-03  2.36E-02 -5.08E-03  9.70E-04 -9.52E-03  1.55E-02  3.28E-03  2.59E-03 -5.72E-03 -6.97E-03
          8.01E-03 -2.62E-03  1.32E-02  1.55E-02 -5.02E-03 -2.66E-03  3.80E-03 -2.55E-03  7.39E-03  1.26E-03 -2.48E-03  4.48E-03
        -3.51E-03  5.42E-03  5.86E-03  3.09E-03  1.14E-03 -1.21E-02  4.71E-03 -9.36E-03  1.20E-03 -5.14E-04 -7.04E-03  2.80E-03
          4.97E-03  7.39E-03 -2.21E-03 -4.42E-03  1.04E-02  5.92E-03  1.68E-02
 
 OM88
+       -5.67E-02  3.64E-02 -3.67E-02 -2.69E-02  3.37E-02  4.63E-02 -2.73E-03 -6.38E-02  2.42E-02  3.17E-02 -5.74E-03 -5.36E-03
          5.49E-03  8.60E-03  1.08E-02  4.45E-02  2.71E-04 -3.31E-03  1.27E-02 -3.78E-03  3.45E-02  1.67E-02  2.87E-02 -3.08E-02
        -1.98E-02  1.12E-02  7.87E-04  2.32E-02 -6.18E-03 -6.79E-03  3.38E-02 -5.41E-03  1.03E-02  1.30E-02  5.10E-02  1.36E-02
          5.09E-03 -1.01E-02  2.19E-02 -1.94E-02 -2.74E-02  1.63E-02  1.42E-02  6.95E-02
 
 SG11
+        9.92E-04 -4.60E-04  1.30E-04  9.31E-04 -5.98E-04 -5.16E-04 -1.73E-04  1.16E-03 -1.77E-04 -4.11E-04 -2.29E-05 -1.01E-04
          1.69E-04 -1.94E-04  1.86E-04 -1.81E-04  7.30E-05 -1.17E-04  2.72E-06  4.24E-06 -2.67E-04 -2.16E-04 -3.64E-04  5.31E-04
         1.67E-04 -1.18E-05  1.38E-04 -1.68E-04  1.27E-04 -1.79E-04 -3.00E-04 -8.61E-05 -1.07E-04 -1.27E-04 -7.49E-04 -1.06E-04
          1.12E-04  3.51E-04 -3.37E-04  1.71E-04  5.46E-04 -1.49E-04  1.49E-04 -5.27E-04  1.20E-05
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.42E-04 -5.86E-04  2.59E-04  3.65E-05 -2.23E-04 -1.95E-04  1.97E-04  6.21E-04 -6.04E-05 -7.30E-04  2.95E-04  1.35E-04
         -6.91E-05 -1.64E-04  6.02E-05 -3.00E-04  6.21E-04 -1.77E-04 -5.20E-05 -1.94E-07 -4.56E-04 -2.84E-04 -3.41E-04  4.12E-04
         2.58E-04 -1.07E-04 -2.31E-04 -2.06E-04  1.45E-04  2.80E-04 -5.00E-04  3.39E-04 -5.48E-05  1.08E-05 -8.10E-04 -1.98E-04
          1.01E-04  1.88E-04 -8.88E-05  1.98E-04 -2.03E-05 -3.26E-04 -1.97E-04 -6.56E-04  3.96E-06  0.00E+00  1.84E-05
 
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
+        3.49E-01
 
 TH 2
+       -4.20E-01  2.25E-01
 
 TH 3
+        3.80E-01 -2.87E-01  2.30E-01
 
 TH 4
+        7.48E-01 -3.64E-01 -3.37E-02  3.12E-01
 
 TH 5
+       -7.32E-01  5.49E-01 -4.00E-02 -7.44E-01  2.16E-01
 
 TH 6
+       -6.51E-01  3.59E-01 -3.04E-01 -4.65E-01  5.74E-01  2.66E-01
 
 TH 7
+       -2.86E-01 -1.22E-01  2.67E-01 -4.74E-01  3.59E-01  2.68E-01  1.50E-01
 
 TH 8
+        7.90E-01 -4.72E-01  2.18E-01  7.92E-01 -7.23E-01 -5.54E-01 -1.81E-01  3.84E-01
 
 OM11
+       -5.77E-01  1.73E-01 -6.02E-01 -2.11E-01  3.81E-01  3.91E-01  2.27E-02 -3.30E-01  1.50E-01
 
 OM12
+       -2.81E-01  6.14E-01  2.53E-02 -4.09E-01  4.74E-01  2.94E-01 -9.28E-02 -6.44E-01 -1.14E-01  2.23E-01
 
 OM13
+       -2.95E-01 -3.69E-01  1.21E-01 -2.43E-01  1.53E-01  1.08E-01  2.90E-01 -2.14E-03  4.29E-01 -5.61E-01  1.05E-01
 
 OM14
+       -2.68E-01 -1.65E-01  1.75E-01 -4.53E-01  2.09E-01 -1.49E-01  5.18E-01 -2.59E-01  4.54E-02 -1.78E-01  3.52E-01  8.69E-02
 
 OM15
+        2.51E-01 -1.87E-01 -1.21E-01  3.83E-01 -2.24E-01  2.07E-01 -3.13E-01  1.81E-01  1.48E-02  3.63E-02 -1.12E-01 -6.92E-01
          1.40E-01
 
 OM16
+       -4.37E-01  5.73E-01 -5.76E-02 -5.07E-01  5.36E-01  1.81E-01  1.21E-01 -3.46E-01  1.85E-01  3.50E-01 -2.48E-02  2.00E-01
         -5.74E-01  1.12E-01
 
 OM17
+        6.27E-02 -7.57E-02 -5.76E-01  4.83E-01 -2.10E-01  1.70E-02 -3.11E-01  4.04E-01  5.78E-01 -4.90E-01  1.86E-01 -3.54E-01
          2.86E-01 -1.58E-01  1.61E-01
 
 OM18
+       -4.38E-01  3.87E-01 -7.65E-01 -1.71E-02  2.94E-01  5.25E-01 -1.87E-01 -2.56E-01  7.22E-01  1.65E-01 -9.26E-02 -3.35E-01
          2.54E-01  8.42E-02  6.65E-01  2.01E-01
 
 OM22
+       -2.81E-01 -3.54E-01 -5.38E-01  1.72E-02 -1.43E-01  2.41E-01  6.85E-02  1.17E-01  4.88E-01 -6.76E-01  5.28E-01  7.58E-02
          1.20E-01 -2.34E-01  5.14E-01  3.12E-01  2.80E-01
 
 OM23
+        1.51E-01  2.05E-01  5.17E-01 -2.67E-01  1.94E-01 -1.77E-01  2.10E-01 -2.49E-01 -5.17E-01  5.29E-01 -4.98E-01  2.51E-01
         -3.32E-01  2.11E-01 -6.68E-01 -4.36E-01 -7.56E-01  1.23E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -2.67E-01 -1.02E-01 -4.45E-01  5.23E-02 -4.06E-02  3.82E-01 -2.72E-01 -1.25E-01  3.05E-01  8.23E-02  1.49E-02 -5.27E-01
          6.40E-01 -2.58E-01  2.98E-01  4.89E-01  4.52E-01 -5.54E-01  1.31E-01
 
 OM25
+        9.31E-02  4.41E-03 -5.39E-02 -6.83E-02 -4.07E-02 -9.66E-02  2.31E-01  4.92E-02  5.18E-02 -2.12E-01  4.85E-02  3.24E-01
         -3.62E-01  1.91E-01 -1.70E-02 -2.09E-01  7.33E-02  2.12E-01 -5.33E-01  9.08E-02
 
 OM26
+       -4.25E-01  6.51E-01 -5.21E-01 -2.74E-01  4.76E-01  6.28E-01 -1.23E-01 -4.56E-01  3.90E-01  5.46E-01 -3.58E-01 -4.35E-01
          2.12E-01  3.98E-01  1.51E-01  6.71E-01 -5.71E-02 -1.49E-03  3.86E-01 -1.18E-01  1.62E-01
 
 OM27
+       -3.77E-01  7.06E-01 -1.49E-01 -4.46E-01  5.98E-01  2.14E-01  7.95E-02 -5.87E-01  2.17E-01  6.36E-01 -3.35E-01  7.62E-02
         -3.42E-01  5.89E-01 -1.94E-01  2.73E-01 -4.93E-01  4.78E-01 -3.09E-01  2.24E-01  5.62E-01  1.06E-01
 
 OM28
+       -7.36E-01  4.99E-01 -5.11E-01 -5.90E-01  5.65E-01  4.99E-01  2.92E-02 -8.35E-01  5.18E-01  5.83E-01 -7.06E-02  1.84E-02
         -3.27E-02  3.83E-01 -1.16E-01  4.65E-01  7.07E-02 -2.37E-02  3.82E-01  3.17E-03  5.88E-01  5.52E-01  1.48E-01
 
 OM33
+        7.08E-01 -6.32E-01  5.43E-02  7.05E-01 -8.00E-01 -5.14E-01 -2.34E-01  8.87E-01 -2.41E-01 -7.65E-01  1.36E-01 -1.96E-01
          2.75E-01 -5.16E-01  4.33E-01 -2.29E-01  3.90E-01 -3.83E-01  4.14E-02  5.23E-02 -4.89E-01 -7.27E-01 -7.41E-01  1.81E-01
 
 OM34
+        3.71E-01 -5.90E-01  3.69E-01  3.33E-01 -3.85E-01 -5.69E-01  2.51E-01  5.59E-01 -1.72E-01 -6.73E-01  3.94E-01  4.40E-01
         -3.25E-01 -1.95E-01  9.69E-02 -5.06E-01  1.40E-01 -2.35E-02 -4.72E-01  3.40E-01 -7.66E-01 -4.00E-01 -6.06E-01  5.12E-01
         1.04E-01
 
 OM35
+        8.25E-03  2.89E-01 -4.37E-01  1.23E-01 -2.74E-02  4.11E-01 -3.33E-01 -1.36E-01  7.71E-02  2.11E-01 -3.60E-01 -4.66E-01
          5.60E-01 -2.76E-01  2.16E-01  4.98E-01  1.08E-01 -1.55E-01  3.91E-01 -2.71E-01  4.95E-01  3.13E-03  1.60E-01 -1.04E-02
        -6.99E-01  8.63E-02
 
 OM36
+        3.17E-01  9.55E-02 -4.05E-01  3.72E-01 -4.38E-01 -4.36E-01 -5.62E-01  2.67E-01  2.28E-03  4.85E-02 -4.59E-01 -3.72E-01
          1.37E-01  1.12E-01  3.25E-01  1.68E-01 -2.80E-02 -6.57E-02  2.22E-01  2.76E-02  2.33E-01  8.89E-02  8.62E-02  3.01E-01
        -1.22E-01  9.01E-02  1.34E-01
 
 OM37
+       -5.42E-01  4.68E-01 -6.59E-01 -2.71E-01  3.02E-01  4.88E-01 -2.71E-01 -5.02E-01  5.82E-01  3.41E-01 -6.56E-03 -3.29E-01
          2.95E-01  2.19E-01  2.46E-01  6.60E-01  2.52E-01 -4.37E-01  6.15E-01 -3.01E-01  6.72E-01  2.60E-01  7.28E-01 -3.68E-01
        -7.60E-01  5.25E-01  2.62E-01  1.19E-01
 
 OM38
+        4.72E-01 -5.92E-01  1.04E-01  4.47E-01 -5.34E-01 -2.11E-01 -9.55E-02  6.12E-01 -4.42E-02 -6.83E-01  4.13E-01 -1.43E-01
          3.61E-01 -4.50E-01  3.73E-01 -1.39E-01  3.98E-01 -4.03E-01  6.77E-02 -3.66E-03 -3.45E-01 -5.87E-01 -5.73E-01  7.85E-01
         3.25E-01  1.31E-01  2.13E-02 -1.36E-01  5.91E-02
 
 OM44
+       -4.25E-01 -2.66E-01  3.59E-01 -4.77E-01  3.14E-01  8.10E-02  3.47E-01 -3.10E-01  1.38E-01 -6.81E-02  6.27E-01  3.62E-01
         -9.30E-02 -2.55E-02 -2.71E-01 -2.44E-01  1.43E-01 -8.48E-02  1.72E-01 -2.09E-01 -3.35E-01 -1.70E-01  1.74E-01 -2.27E-01
         1.82E-01 -4.60E-01 -4.33E-01 -2.68E-02 -6.79E-02  1.68E-01
 
 OM45
+       -3.90E-01  6.45E-01 -3.35E-01 -3.42E-01  4.87E-01  6.04E-01 -7.85E-02 -6.24E-01  1.92E-01  7.54E-01 -4.06E-01 -2.64E-01
          1.90E-01  2.95E-01 -1.86E-01  4.62E-01 -3.00E-01  1.67E-01  2.24E-01 -1.15E-01  8.12E-01  5.85E-01  6.19E-01 -6.67E-01
        -8.31E-01  5.42E-01  2.94E-03  6.29E-01 -4.16E-01 -2.94E-01  1.66E-01
 
 OM46
+       -3.72E-01 -1.58E-01  1.75E-04 -3.33E-01  2.63E-01  3.16E-01  5.57E-01 -1.99E-02  3.09E-01 -5.04E-01  6.47E-01  4.09E-01
         -3.83E-01  2.53E-01  4.80E-02 -9.26E-02  5.36E-01 -2.61E-01 -1.93E-01  4.33E-01 -1.19E-01 -1.14E-01 -2.33E-02  2.58E-02
         3.62E-01 -3.52E-01 -4.96E-01 -1.46E-01  1.75E-01  2.92E-01 -2.69E-01  1.53E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.34E-01  4.18E-01 -1.13E-01 -2.16E-01  4.68E-01  4.10E-01  4.09E-01 -2.11E-01  3.43E-01  8.19E-02  9.58E-02  2.06E-01
         -3.56E-01  4.31E-01  7.10E-02  2.82E-01 -7.30E-02  5.76E-03 -3.27E-01  2.09E-01  2.93E-01  4.66E-01  1.94E-01 -4.25E-01
        -5.49E-02 -2.55E-02 -3.80E-01  9.37E-02 -2.38E-01 -1.86E-01  3.39E-01  4.21E-01  9.44E-02
 
 OM48
+       -6.45E-01  1.14E-01 -6.08E-01 -2.66E-01  2.90E-01  4.25E-01  1.21E-01 -4.47E-01  5.85E-01 -3.78E-02  2.44E-01  1.59E-01
          7.98E-02 -9.72E-02  2.80E-01  5.63E-01  5.39E-01 -4.77E-01  5.23E-01 -2.35E-01  2.56E-01  6.10E-02  5.54E-01 -2.83E-01
        -2.36E-01  2.11E-01 -9.00E-02  5.34E-01 -1.98E-01  2.97E-01  1.88E-01  1.19E-01  1.73E-01  9.30E-02
 
 OM55
+       -6.04E-01  5.05E-01 -2.12E-01 -6.25E-01  6.24E-01  4.19E-01  1.64E-01 -8.28E-01  3.79E-01  7.11E-01 -1.15E-01  5.14E-02
         -7.93E-02  4.30E-01 -3.07E-01  2.76E-01 -2.57E-01  2.58E-01  1.52E-01  1.02E-01  5.48E-01  7.01E-01  8.79E-01 -8.37E-01
        -5.15E-01 -1.87E-02  3.63E-03  5.12E-01 -6.59E-01  2.24E-01  6.25E-01 -7.03E-02  2.40E-01  3.09E-01  3.09E-01
 
 OM56
+       -2.99E-01  5.97E-01 -5.93E-01 -2.22E-01  2.07E-01  2.73E-01 -3.87E-01 -4.18E-01  2.50E-01  4.45E-01 -4.18E-01 -2.72E-01
          1.57E-02  3.72E-01  1.38E-01  5.09E-01 -2.31E-02 -1.24E-02  2.49E-01  4.49E-02  6.63E-01  5.21E-01  5.63E-01 -3.56E-01
        -6.72E-01  4.59E-01  4.87E-01  6.32E-01 -3.17E-01 -3.77E-01  5.94E-01 -3.11E-01  3.18E-02  2.36E-01  4.33E-01  8.27E-02
 
 OM57
+        1.26E-02 -8.24E-02 -6.07E-01  3.41E-01 -1.48E-01  4.59E-02 -2.19E-01  3.20E-01  4.98E-01 -5.44E-01  2.64E-01 -1.98E-01
          1.71E-01 -1.32E-01  7.84E-01  5.09E-01  6.16E-01 -7.14E-01  1.87E-01  1.38E-01  1.06E-01 -1.38E-01 -9.15E-02  4.36E-01
         8.26E-02  2.38E-01  1.96E-01  2.62E-01  4.57E-01 -2.71E-01 -1.44E-01  2.56E-01  8.60E-02  3.06E-01 -3.09E-01  1.89E-01
          1.01E-01
 
 OM58
+        5.15E-01 -4.09E-01 -2.66E-01  7.00E-01 -6.00E-01 -2.48E-01 -2.48E-01  7.20E-01  1.03E-01 -7.03E-01  1.46E-01 -2.78E-01
          4.10E-01 -4.69E-01  6.84E-01  1.10E-01  4.82E-01 -5.29E-01  7.94E-02  3.07E-01 -2.39E-01 -5.13E-01 -4.73E-01  8.04E-01
         3.90E-01  1.58E-01  2.70E-01 -1.40E-01  6.75E-01 -3.53E-01 -4.12E-01  1.10E-01 -1.58E-01 -7.72E-02 -6.01E-01 -1.89E-01
          6.81E-01  1.42E-01
 
 OM66
+       -5.93E-01  4.17E-01 -2.51E-01 -5.48E-01  5.41E-01  6.63E-01  1.95E-01 -5.68E-01  1.86E-01  3.66E-01 -3.79E-02  1.16E-02
          5.40E-02  2.25E-01 -2.13E-01  3.37E-01  1.23E-01  2.39E-02  2.99E-01 -3.27E-01  5.63E-01  2.09E-01  4.53E-01 -4.75E-01
        -6.43E-01  4.78E-01 -2.61E-01  5.29E-01 -2.09E-01  4.80E-02  5.95E-01  1.43E-01  2.34E-01  3.52E-01  3.02E-01  2.20E-01
         -1.27E-01 -4.30E-01  1.55E-01
 
 OM67
+        4.24E-01 -7.40E-01  2.78E-01  3.14E-01 -6.28E-01 -4.35E-01 -4.39E-02  4.39E-01 -3.99E-01 -4.27E-01  1.46E-01  6.04E-02
          1.89E-01 -5.31E-01 -1.21E-01 -5.04E-01  2.49E-01 -7.23E-02  1.50E-01 -9.68E-02 -5.92E-01 -7.20E-01 -4.36E-01  6.05E-01
         4.36E-01 -2.14E-01  1.33E-01 -3.85E-01  3.88E-01  2.35E-01 -5.85E-01 -7.85E-02 -6.75E-01 -1.72E-01 -4.71E-01 -4.71E-01
         -1.69E-01  2.71E-01 -2.86E-01  1.03E-01
 
 OM68
+        8.44E-01 -2.98E-01  1.96E-01  7.57E-01 -7.15E-01 -5.99E-01 -4.06E-01  8.29E-01 -4.43E-01 -2.68E-01 -3.54E-01 -4.44E-01
          2.58E-01 -2.09E-01  2.55E-01 -2.45E-01 -1.80E-01  2.41E-02 -5.49E-02 -1.97E-02 -1.89E-01 -3.63E-01 -6.47E-01  7.25E-01
         3.03E-01 -2.04E-02  5.72E-01 -3.80E-01  3.99E-01 -4.61E-01 -3.79E-01 -3.68E-01 -3.88E-01 -5.89E-01 -5.89E-01 -1.57E-01
          8.68E-02  5.20E-01 -5.44E-01  4.15E-01  2.02E-01
 
 OM77
+       -1.74E-01  3.17E-01 -1.56E-01 -6.48E-02  1.75E-01  4.16E-02 -3.79E-01 -4.40E-01  5.92E-02  6.07E-01 -4.24E-01 -2.14E-01
          2.07E-01 -9.66E-02 -7.94E-02  2.76E-01 -3.48E-01  2.30E-01  2.73E-01 -2.56E-01  2.48E-01  2.81E-01  4.41E-01 -4.29E-01
        -4.16E-01  2.31E-01  2.03E-01  3.24E-01 -4.97E-01  4.70E-02  3.57E-01 -6.44E-01 -2.99E-01  2.03E-01  4.49E-01  3.56E-01
         -2.53E-01 -3.07E-01  8.81E-02 -1.33E-01 -1.64E-01  1.45E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.10E-01  1.82E-01 -3.08E-01  5.84E-01 -1.82E-01  2.82E-02 -4.89E-01  3.13E-01  1.69E-01  8.97E-02 -4.19E-01 -6.19E-01
          4.42E-01 -1.80E-01  6.33E-01  5.98E-01 -1.38E-01 -1.67E-01  2.23E-01 -2.16E-01  3.52E-01  9.24E-02 -1.29E-01  1.91E-01
        -2.62E-01  4.85E-01  3.37E-01  2.01E-01  1.49E-01 -5.54E-01  2.20E-01 -4.73E-01  9.84E-02 -4.27E-02 -1.76E-01  2.62E-01
          3.79E-01  4.02E-01 -1.10E-01 -3.30E-01  3.98E-01  3.15E-01  1.29E-01
 
 OM88
+       -6.16E-01  6.15E-01 -6.05E-01 -3.27E-01  5.91E-01  6.62E-01 -6.90E-02 -6.30E-01  6.09E-01  5.38E-01 -2.07E-01 -2.34E-01
          1.49E-01  2.90E-01  2.55E-01  8.42E-01  3.67E-03 -1.02E-01  3.66E-01 -1.58E-01  8.07E-01  6.00E-01  7.37E-01 -6.44E-01
        -7.26E-01  4.94E-01  2.22E-02  7.40E-01 -3.97E-01 -1.53E-01  7.74E-01 -1.34E-01  4.15E-01  5.29E-01  6.26E-01  6.23E-01
          1.90E-01 -2.70E-01  5.37E-01 -7.12E-01 -5.15E-01  4.26E-01  4.16E-01  2.64E-01
 
 SG11
+        8.21E-01 -5.91E-01  1.63E-01  8.61E-01 -7.98E-01 -5.61E-01 -3.33E-01  8.73E-01 -3.40E-01 -5.32E-01 -6.29E-02 -3.37E-01
          3.48E-01 -5.00E-01  3.35E-01 -2.60E-01  7.53E-02 -2.75E-01  5.99E-03  1.35E-02 -4.75E-01 -5.90E-01 -7.10E-01  8.45E-01
         4.64E-01 -3.95E-02  2.97E-01 -4.09E-01  6.19E-01 -3.06E-01 -5.23E-01 -1.63E-01 -3.28E-01 -3.95E-01 -7.01E-01 -3.70E-01
          3.18E-01  7.14E-01 -6.29E-01  4.77E-01  7.82E-01 -2.97E-01  3.31E-01 -5.78E-01  3.46E-03
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        9.47E-02 -6.08E-01  2.63E-01  2.73E-02 -2.40E-01 -1.71E-01  3.06E-01  3.77E-01 -9.36E-02 -7.63E-01  6.53E-01  3.63E-01
         -1.15E-01 -3.40E-01  8.74E-02 -3.49E-01  5.17E-01 -3.34E-01 -9.24E-02 -4.98E-04 -6.55E-01 -6.27E-01 -5.37E-01  5.29E-01
         5.82E-01 -2.90E-01 -4.02E-01 -4.03E-01  5.74E-01  3.88E-01 -7.04E-01  5.17E-01 -1.35E-01  2.70E-02 -6.12E-01 -5.58E-01
          2.33E-01  3.08E-01 -1.34E-01  4.47E-01 -2.35E-02 -5.24E-01 -3.55E-01 -5.80E-01  2.67E-01  0.00E+00  4.29E-03
 
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
+       -5.89E+01  2.06E+01  5.46E+02
 
 TH 4
+       -3.02E+01 -1.20E+01  6.09E+01  2.83E+02
 
 TH 5
+       -8.10E+01 -1.30E+02 -7.30E+00  2.18E+01  3.64E+02
 
 TH 6
+       -2.03E+01 -7.69E+01 -8.18E+01 -5.55E+01  1.21E+02  2.91E+02
 
 TH 7
+        5.32E+01  1.53E+02 -1.82E+01  1.13E+02 -8.58E+01 -8.22E+01  3.63E+02
 
 TH 8
+       -1.99E+02 -2.87E+02 -1.00E+02 -9.86E+01  1.24E+02  1.50E+02 -2.18E+02  5.56E+02
 
 OM11
+        8.20E+01  3.22E+01 -1.02E+02  1.49E+01  2.38E+01  1.18E+02  1.10E+02 -4.04E+01  9.92E+02
 
 OM12
+        7.78E+01  4.63E+01  4.60E+01  4.30E+02  1.05E+02  5.64E+01  3.52E+02 -3.25E+02  8.72E+02  3.51E+03
 
 OM13
+       -2.32E+02 -3.55E+01 -2.83E+02  5.54E+01 -1.46E+02  1.14E+01  1.85E+02  2.08E+02 -9.17E+02 -8.02E+02  3.59E+03
 
 OM14
+        3.87E+01  4.76E+02  1.09E+02  9.12E+01 -9.66E+01  7.64E+01  1.15E+02 -1.50E+02 -3.22E+02  3.60E+02  3.68E+02  2.64E+03
 
 OM15
+        8.04E+01  1.68E+02 -1.81E+02 -9.10E+01 -4.36E+02 -3.76E+02 -1.39E+01 -1.20E+02 -6.97E+02 -1.77E+03  1.05E+03 -1.95E+02
          2.77E+03
 
 OM16
+        2.45E+02  6.77E+01 -2.86E+01  8.54E+01 -3.49E+02 -3.23E+02  1.27E+02 -2.49E+02  2.03E+02 -4.11E+02 -6.66E+02 -8.32E+02
          8.80E+02  1.90E+03
 
 OM17
+        1.63E+02  1.91E+02  1.90E+02  1.00E+02  1.87E+01  1.68E+02  1.09E+02 -2.12E+02  5.17E+02  1.93E+03 -1.47E+03  1.05E+03
         -1.59E+03 -3.87E+02  2.82E+03
 
 OM18
+       -7.25E+01 -2.70E+02  1.42E+02 -1.09E+02 -6.42E+01 -2.44E+02 -3.18E+02  2.29E+02 -1.11E+03 -2.88E+03  9.83E+02 -8.88E+02
          1.99E+03  6.22E+02 -2.38E+03  4.05E+03
 
 OM22
+        9.40E+01  1.79E+02  2.04E+01  2.66E+02 -4.09E+01 -3.33E+01  2.74E+02 -3.27E+02  3.55E+02  2.13E+03 -2.13E+02  1.58E+02
         -8.02E+02 -2.18E+02  9.82E+02 -1.75E+03  2.01E+03
 
 OM23
+        1.76E+01 -1.29E+02 -2.02E+02 -1.14E+02 -1.68E+02  1.87E+02 -2.24E+02  2.71E+02 -5.56E+02 -1.09E+03  1.76E+03 -4.15E+02
          8.56E+02 -6.64E+01 -8.26E+02  1.53E+03 -3.06E+02  3.76E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.82E+02  6.53E+02 -1.23E+02 -6.39E+01 -8.43E+01  1.20E+02  1.82E+02 -4.81E+02  2.16E+02  3.50E+02 -4.19E+02  1.91E+03
         -8.27E+02 -2.89E+02  1.46E+03 -1.10E+03 -1.45E+02 -3.46E+02  4.51E+03
 
 OM25
+        1.12E+02  8.62E+01 -2.02E+02 -5.20E+01 -2.31E+02 -4.52E+02  1.59E+01 -2.26E+02 -6.32E+02 -1.82E+03  5.85E+02 -8.34E+02
          2.89E+03  1.05E+03 -1.91E+03  2.29E+03 -1.14E+03 -2.79E+02 -6.04E+02  6.18E+03
 
 OM26
+        1.81E+01 -3.28E+01  1.22E+02  1.31E+02 -3.77E+02 -4.57E+02  2.41E+02 -6.03E+01 -3.13E+01 -4.24E+02 -3.00E+02 -2.96E+02
          9.97E+02  1.31E+03 -6.14E+02  7.62E+02 -5.65E+02 -7.03E+02 -1.10E+03  1.95E+03  3.66E+03
 
 OM27
+        2.40E+02  1.53E+02 -2.12E+02  1.55E+02  1.09E+02  3.44E+02  2.23E+02 -1.61E+02  6.11E+02  1.98E+03 -5.98E+02  1.44E+03
         -1.85E+03 -6.33E+02  2.10E+03 -2.33E+03  1.15E+03 -9.46E+02  2.81E+03 -2.42E+03 -1.50E+03  4.43E+03
 
 OM28
+       -3.70E+02 -4.66E+02  1.52E+02 -3.93E+02 -1.27E+02 -7.84E+01 -4.01E+02  8.91E+02 -9.29E+02 -4.28E+03  1.03E+03 -1.04E+03
          2.35E+03  8.79E+02 -2.57E+03  4.18E+03 -3.19E+03  1.42E+03 -2.19E+03  2.67E+03  2.41E+03 -3.55E+03  8.49E+03
 
 OM33
+       -1.15E+02 -5.93E+01  5.18E+01  1.52E+02 -5.66E+00  1.21E+01 -3.59E+00 -1.30E+02  2.94E+01 -8.24E+00  2.23E+02  6.74E+01
          9.21E+01  4.52E+02  1.67E+02 -4.60E+02 -3.18E+02 -1.33E+02  2.25E+02 -1.29E+02  1.51E+02  4.00E+02  1.54E+02  2.90E+03
 
 OM34
+        1.20E+02 -1.10E+02  2.66E+01 -2.72E+02  7.91E+01  1.16E+02 -1.96E+02  1.65E+02  2.24E+01 -4.85E+02 -1.85E+02 -8.37E+02
          1.63E+02  2.51E+02 -3.77E+02  1.85E+02 -3.48E+02  2.30E+02 -1.79E+02  9.38E+02  1.95E+02 -3.42E+02  8.59E+02  6.64E+02
         3.03E+03
 
 OM35
+       -1.75E+02 -1.30E+02  8.78E+01  1.02E+02  2.50E+02  5.85E+01  1.08E+02  2.87E+02  6.24E+02  9.02E+02 -9.28E+02  4.80E+01
         -1.23E+03  3.69E+02  7.66E+02 -1.31E+03 -9.26E+01 -2.00E+03  7.44E+02 -4.92E+02  7.68E+02  1.23E+03 -2.33E+02  9.04E+02
         9.92E+02  4.25E+03
 
 OM36
+       -3.31E+01  1.67E+02  1.21E+02  7.47E+01  1.07E+02  1.87E+01  1.65E+02 -1.17E+02 -2.70E+01  1.50E+02  2.94E+02  1.20E+02
          2.23E+02 -1.74E+02 -3.51E+02 -1.22E+01  1.48E+01 -7.47E+02  7.23E+01  5.16E+02  1.25E+02 -2.79E+02 -1.16E+02 -9.40E+01
        -2.49E+02  1.15E+03  1.91E+03
 
 OM37
+        1.79E+02 -2.79E+02 -4.01E+01 -1.69E+02  1.28E+02  1.89E+02 -2.61E+02  8.46E+01 -7.10E+02 -6.81E+02  9.82E+02 -3.49E+02
          7.21E+02 -4.38E+02 -6.50E+02  1.20E+03 -2.86E+02  2.48E+03 -2.40E+02  1.14E+03 -5.58E+02 -5.03E+02  4.41E+02 -1.23E+02
         1.39E+03 -1.84E+03 -9.18E+02  4.24E+03
 
 OM38
+        1.58E+02  2.72E+02 -1.36E+02  1.38E+02  3.19E+02 -1.40E+02  6.16E+01 -1.66E+02  9.11E+02  1.48E+03 -3.56E+03 -1.22E+02
         -1.32E+03  3.71E+02  1.32E+03 -8.29E+02  7.69E+02 -2.97E+03  6.53E+02 -1.48E+02  4.56E+02  7.13E+02 -1.92E+03 -2.44E+03
        -1.74E+03  8.52E+02  4.44E+02 -3.05E+03  9.42E+03
 
 OM44
+        6.76E+01 -1.17E+01 -1.86E+02  1.40E+02 -1.09E+02 -5.72E+01  1.19E+02 -1.12E+02  2.16E+02  5.03E+02 -3.32E+02 -3.04E+02
          1.85E+01  3.70E+02  9.30E+01 -2.63E+02  4.76E+02 -1.50E+02 -3.69E+02  2.25E+02  5.25E+02  9.02E+01 -3.48E+02  1.49E+02
         1.38E+02  3.04E+02 -6.25E+00 -3.25E+02  3.49E+02  7.49E+02
 
 OM45
+       -1.02E+02 -7.77E+01  9.81E+01 -1.79E+02  1.18E+02  1.34E+02 -2.20E+02  2.56E+02 -1.50E+02 -8.92E+02  1.05E+02 -4.17E+02
          1.91E+01 -7.05E+01 -1.59E+02  6.25E+02 -3.96E+02  7.95E+02 -2.93E+02 -6.18E+02 -1.03E+03 -2.51E+02  1.10E+03  3.16E+02
         5.24E+02  5.44E+01  7.42E+01  1.77E+02 -1.06E+03 -1.52E+02  1.55E+03
 
 OM46
+        6.14E+01  1.16E+02  1.47E+02 -9.71E+01  1.51E+02  2.58E+01 -1.36E+02  8.91E+00 -2.43E+02 -1.74E+02  1.41E+02  5.45E+02
         -1.62E+02 -5.85E+02  1.93E+02  6.75E+01 -3.58E+02  1.18E+02  5.97E+02 -1.17E+03 -1.23E+03  3.82E+02 -3.42E+02  3.48E+01
        -2.82E+02  4.66E+01  4.34E+02  1.58E+02 -4.30E+02 -5.73E+02  6.19E+02  1.72E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.31E+02  2.09E+02 -1.92E+02  2.37E+02 -2.42E+02 -1.58E+02  2.17E+02 -4.48E+02  1.56E+02  1.03E+03 -2.77E+02  5.45E+02
          8.72E+01  2.52E+02  6.01E+02 -6.13E+02  7.98E+02 -3.26E+02  1.08E+03  1.82E+02  4.90E+02  8.69E+02 -1.61E+03  2.43E+02
        -8.25E+02  1.72E+02  2.60E+02 -8.81E+02  1.31E+03  6.89E+02 -8.52E+02 -7.01E+02  2.58E+03
 
 OM48
+       -1.70E+02 -4.96E+02  2.05E+02 -1.94E+02  2.72E+02  6.77E+00 -4.19E+02  3.68E+02 -1.00E+02 -7.69E+02 -1.10E+02 -1.77E+03
          3.71E+02  2.00E+02 -1.03E+03  1.29E+03 -6.21E+02  7.37E+02 -2.33E+03  7.05E+02 -2.89E+02 -2.06E+03  1.56E+03 -6.10E+02
         2.59E+02 -1.08E+03 -3.90E+02  1.07E+03  4.14E+00 -4.69E+02  6.15E+02  3.47E+02 -1.74E+03  3.39E+03
 
 OM55
+       -2.04E+02 -1.29E+02  1.35E+02  5.75E+01  2.29E+02  1.46E+02 -1.66E+02  1.72E+02  1.33E+02  8.04E+02 -3.87E+02  1.08E+02
         -1.17E+03 -6.00E+02  6.88E+02 -9.83E+02  6.21E+02 -3.34E+02 -2.74E+02 -1.77E+03 -1.12E+03  4.93E+02 -1.54E+03 -1.33E+01
        -1.90E+02  1.38E+02 -3.48E+02 -4.49E+02  4.66E+02 -1.34E+02  2.04E+02  2.62E+02 -2.89E+02  2.77E+02  1.22E+03
 
 OM56
+       -3.30E+02 -4.32E+02  9.37E+01  1.45E+02  2.33E+02  1.93E+02 -7.92E+01  3.33E+02  9.59E+01  7.43E+02  5.93E+02  4.89E+00
         -1.26E+03 -1.18E+03  3.61E+02 -5.33E+02  5.56E+02  7.74E+02 -1.03E+03 -2.72E+03 -1.37E+03  3.71E+02 -9.20E+02 -2.03E+02
        -1.67E+02 -1.35E+03 -1.51E+03  3.50E+02 -9.87E+02 -4.89E+01  1.07E+02  1.69E+02 -4.20E+02  6.15E+02  1.35E+03  4.40E+03
 
 OM57
+        1.39E+01  1.60E+02  1.11E+02 -2.17E+02 -3.60E+02 -1.17E+02 -1.13E+02 -2.11E+01 -5.60E+02 -1.71E+03  6.63E+02 -1.51E+02
          1.65E+03  3.71E+02 -1.12E+03  1.67E+03 -9.25E+02  1.31E+03 -1.83E+02  1.97E+03  4.62E+02 -1.97E+03  2.33E+03 -4.09E+02
         2.54E+02 -1.07E+03  1.23E+02  7.34E+02 -1.02E+03 -1.51E+02  2.97E+02 -2.35E+02  2.62E+01  3.96E+02 -7.38E+02 -9.80E+02
          2.68E+03
 
 OM58
+       -7.54E+01 -2.64E+02  3.31E+02  2.38E+02  3.06E+02  3.53E+02  6.75E+01  3.34E+00  8.05E+02  2.49E+03 -9.90E+02  5.82E+02
         -3.50E+03 -9.70E+02  1.99E+03 -2.25E+03  1.32E+03 -1.06E+02  9.87E+02 -5.59E+03 -1.52E+03  2.76E+03 -3.53E+03 -4.14E+02
        -1.26E+03  2.25E+02 -9.22E+02 -8.13E+02  1.03E+03 -6.88E+01 -1.96E+02  4.90E+02  2.53E+02 -5.94E+02  1.70E+03  3.07E+03
         -2.42E+03  6.82E+03
 
 OM66
+       -1.35E+02 -2.10E+02  8.62E+00  2.47E+01  4.76E+01  9.64E+01 -9.56E+01  1.23E+02  2.16E+01  2.29E+02  1.71E+02 -1.85E+02
         -3.73E+02 -5.10E+02  1.50E+02 -1.65E+02  3.19E+02  1.24E+02 -5.66E+02 -3.93E+02 -7.28E+02  2.39E+01 -4.51E+02 -3.40E+02
         2.51E+01 -8.68E+02 -7.17E+02  1.22E+02 -1.07E+02  3.81E+01 -4.05E+01 -3.12E+02 -1.56E+02  4.31E+02  6.27E+02  1.65E+03
         -1.35E+02  7.48E+02  1.17E+03
 
 OM67
+        1.47E+02  3.57E+02  1.72E+02 -1.46E+02 -8.12E+01 -2.16E+02  5.68E+01 -1.91E+02  3.37E+01 -4.85E+02 -5.42E+02  1.37E+02
          3.46E+02  5.45E+02 -4.60E+01  9.62E+01 -5.50E+02 -5.29E+02  2.47E+02  5.39E+02  8.32E+02 -5.29E+02  7.71E+02 -7.56E+01
         2.21E+02  3.31E+02  7.84E+01 -2.75E+02  3.37E+02 -1.76E+02 -1.03E+02  2.40E+02 -1.81E+02  6.71E+01 -3.25E+02 -7.13E+02
          5.18E+02 -6.89E+02 -5.22E+02  1.63E+03
 
 OM68
+       -2.01E+02 -7.99E+01 -1.16E+02  4.76E+00  2.61E+02  2.75E+02 -1.26E+02  3.06E+01 -9.43E+01  6.46E+02  6.45E+02  3.39E+02
         -7.64E+02 -1.41E+03  2.53E+02 -7.04E+02  8.60E+02  3.69E+02 -1.10E+02 -1.28E+03 -2.34E+03  7.66E+02 -1.94E+03 -5.81E+02
        -5.05E+02 -1.15E+03 -4.90E+02  4.20E+02 -2.07E+02 -1.89E+02  3.07E+02  4.24E+02 -1.28E+02  3.46E+02  1.03E+03  1.83E+03
         -4.52E+02  1.34E+03  1.05E+03 -9.55E+02  2.64E+03
 
 OM77
+        4.90E+01  6.90E+01 -1.25E+02  1.03E+02 -5.98E+01  2.67E+01  1.37E+02 -5.07E+01  1.51E+02  5.41E+02 -1.46E+02  4.61E+02
         -3.23E+02  2.88E+01  5.39E+02 -5.04E+02  4.01E+02 -3.83E+02  6.78E+02 -6.74E+02 -8.74E+01  1.05E+03 -9.12E+02  1.03E+02
        -6.45E+02  3.94E+02  1.49E+02 -8.25E+02  9.91E+02  1.80E+02 -1.84E+02  2.05E+00  9.41E+02 -9.16E+02  4.01E+01 -8.74E+01
         -4.83E+02  6.39E+02 -9.51E+01 -3.15E+02  1.19E+02  8.78E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -2.40E+02 -1.05E+02  5.54E+01 -4.04E+02  1.83E+01 -1.92E+02 -1.36E+02  2.72E+02 -6.34E+02 -2.18E+03  1.08E+03 -1.17E+03
          1.76E+03  2.79E+02 -2.23E+03  1.92E+03 -1.19E+03  7.10E+02 -2.42E+03  2.45E+03  8.20E+02 -3.20E+03  3.60E+03 -4.91E+02
         1.20E+03 -8.99E+02  1.20E+02  1.23E+03 -1.88E+03 -3.72E+02  6.80E+02  4.46E+01 -2.04E+03  2.26E+03 -4.91E+02 -3.44E+02
          1.70E+03 -3.14E+03  2.74E+01  9.89E+02 -5.58E+02 -1.53E+03  4.99E+03
 
 OM88
+        1.50E+02  4.08E+02 -5.30E+01  1.47E+02 -6.79E+01  1.41E+01  2.27E+02 -4.08E+02  3.57E+02  1.78E+03  7.74E+01  9.58E+02
         -8.96E+02 -5.07E+02  1.10E+03 -2.54E+03  1.32E+03 -1.04E+03  1.11E+03 -1.33E+03 -9.42E+02  1.61E+03 -3.59E+03  8.27E+02
        -2.07E+02  5.63E+02  3.20E+02 -7.39E+02 -4.69E+02  9.29E+01 -5.39E+02  3.18E+02  6.49E+02 -1.40E+03  6.00E+02  1.27E+02
         -1.21E+03  1.20E+03 -6.11E+01 -1.41E+02  8.58E+02  4.68E+02 -1.86E+03  2.91E+03
 
 SG11
+       -1.72E+03  6.61E+03 -1.96E+03 -1.14E+03 -2.78E+03  1.79E+03  9.17E+03 -2.71E+03  5.64E+03  1.80E+04  1.76E+04  6.86E+03
         -8.76E+03 -5.36E+03  9.86E+03 -1.92E+04  2.32E+04  1.27E+04 -1.95E+04 -1.57E+04  1.88E+04  5.14E+03 -8.64E+03 -1.01E+04
        -3.47E+02 -9.49E+03 -8.76E+03 -1.02E+02 -1.47E+04  1.04E+04 -9.59E+03 -2.05E+04  6.53E+03 -1.30E+04  3.12E+02  2.49E+04
         -1.57E+04  1.71E+04  1.13E+04 -2.94E+03  5.18E+03  6.22E+03 -4.49E+03  8.78E+03  2.40E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.63E+03 -3.22E+03  2.68E+03  4.31E+03  2.07E+03  8.40E+02 -1.43E+03  3.95E+03  6.28E+03  6.38E+03 -6.47E+03 -8.76E+03
         -6.07E+03  5.40E+03  5.90E+02 -2.25E+03 -1.42E+02 -2.81E+03 -1.94E+04 -5.96E+03  1.01E+04 -5.94E+03  1.41E+04  2.33E+03
         2.23E+03  1.60E+04  4.34E+03 -8.13E+03 -5.46E+03  3.01E+03  6.29E+03 -2.78E+03 -6.77E+03  7.31E+03  3.75E+03  7.36E+02
         -9.47E+03  6.92E+03 -2.29E+03  2.91E+02 -2.08E+03 -1.00E+03 -1.14E+03 -4.31E+02  1.82E+05  0.00E+00  8.37E+05
 
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
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmto9_bayes.ext
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
 iteration        -2000 MCMCOBJ=   -6792.75016046779     
 iteration        -1990 MCMCOBJ=   -6649.99395006268     
 iteration        -1980 MCMCOBJ=   -6654.43875827289     
 iteration        -1970 MCMCOBJ=   -6633.26911222832     
 iteration        -1960 MCMCOBJ=   -6670.45302384948     
 iteration        -1950 MCMCOBJ=   -6586.40140522311     
 iteration        -1940 MCMCOBJ=   -6617.51449862481     
 iteration        -1930 MCMCOBJ=   -6608.45070501667     
 iteration        -1920 MCMCOBJ=   -6637.00742721945     
 iteration        -1910 MCMCOBJ=   -6608.45781961870     
 iteration        -1900 MCMCOBJ=   -6515.98305433289     
 iteration        -1890 MCMCOBJ=   -6562.21619861898     
 iteration        -1880 MCMCOBJ=   -6586.21452745788     
 iteration        -1870 MCMCOBJ=   -6534.99018185802     
 iteration        -1860 MCMCOBJ=   -6602.12138745951     
 iteration        -1850 MCMCOBJ=   -6558.53131756147     
 iteration        -1840 MCMCOBJ=   -6571.57295430044     
 iteration        -1830 MCMCOBJ=   -6571.94557566050     
 iteration        -1820 MCMCOBJ=   -6558.61968087434     
 iteration        -1810 MCMCOBJ=   -6528.10633126666     
 iteration        -1800 MCMCOBJ=   -6573.20386487064     
 iteration        -1790 MCMCOBJ=   -6566.25756470693     
 iteration        -1780 MCMCOBJ=   -6563.47747641069     
 iteration        -1770 MCMCOBJ=   -6544.79658018547     
 iteration        -1760 MCMCOBJ=   -6580.11622276643     
 iteration        -1750 MCMCOBJ=   -6589.47941271387     
 iteration        -1740 MCMCOBJ=   -6555.86480109111     
 iteration        -1730 MCMCOBJ=   -6545.91224829192     
 iteration        -1720 MCMCOBJ=   -6582.99611973837     
 iteration        -1710 MCMCOBJ=   -6565.13948432231     
 iteration        -1700 MCMCOBJ=   -6526.93453013463     
 iteration        -1690 MCMCOBJ=   -6552.78643274507     
 iteration        -1680 MCMCOBJ=   -6551.86600751519     
 iteration        -1670 MCMCOBJ=   -6543.96116705339     
 iteration        -1660 MCMCOBJ=   -6531.14111370169     
 iteration        -1650 MCMCOBJ=   -6529.15126020992     
 iteration        -1640 MCMCOBJ=   -6546.82213923844     
 iteration        -1630 MCMCOBJ=   -6523.72285357241     
 iteration        -1620 MCMCOBJ=   -6484.18367039425     
 iteration        -1610 MCMCOBJ=   -6552.58804179738     
 iteration        -1600 MCMCOBJ=   -6547.33074023211     
 iteration        -1590 MCMCOBJ=   -6546.20834786450     
 iteration        -1580 MCMCOBJ=   -6529.84108345970     
 iteration        -1570 MCMCOBJ=   -6548.72119139066     
 iteration        -1560 MCMCOBJ=   -6552.33711471174     
 iteration        -1550 MCMCOBJ=   -6490.00502822266     
 iteration        -1540 MCMCOBJ=   -6563.82290847567     
 iteration        -1530 MCMCOBJ=   -6501.59828017547     
 iteration        -1520 MCMCOBJ=   -6485.25383091096     
 iteration        -1510 MCMCOBJ=   -6497.19898773324     
 iteration        -1500 MCMCOBJ=   -6560.16535080217     
 iteration        -1490 MCMCOBJ=   -6476.24642022408     
 iteration        -1480 MCMCOBJ=   -6540.65105155759     
 iteration        -1470 MCMCOBJ=   -6546.71218622686     
 iteration        -1460 MCMCOBJ=   -6511.43884647966     
 iteration        -1450 MCMCOBJ=   -6549.93278349732     
 iteration        -1440 MCMCOBJ=   -6428.71448529939     
 iteration        -1430 MCMCOBJ=   -6552.38620034305     
 iteration        -1420 MCMCOBJ=   -6562.95096235686     
 iteration        -1410 MCMCOBJ=   -6523.04917513069     
 iteration        -1400 MCMCOBJ=   -6511.89065412356     
 iteration        -1390 MCMCOBJ=   -6504.65018723082     
 iteration        -1380 MCMCOBJ=   -6506.46272262281     
 iteration        -1370 MCMCOBJ=   -6533.21711853380     
 iteration        -1360 MCMCOBJ=   -6533.37176789585     
 iteration        -1350 MCMCOBJ=   -6493.74097243610     
 iteration        -1340 MCMCOBJ=   -6570.54452441630     
 iteration        -1330 MCMCOBJ=   -6553.52197397464     
 iteration        -1320 MCMCOBJ=   -6503.32273090377     
 iteration        -1310 MCMCOBJ=   -6556.03903244470     
 iteration        -1300 MCMCOBJ=   -6479.06999623082     
 iteration        -1290 MCMCOBJ=   -6458.04732730801     
 iteration        -1280 MCMCOBJ=   -6545.36097397444     
 iteration        -1270 MCMCOBJ=   -6505.21488191195     
 iteration        -1260 MCMCOBJ=   -6540.26506536337     
 iteration        -1250 MCMCOBJ=   -6523.92517200056     
 iteration        -1240 MCMCOBJ=   -6497.67127555811     
 iteration        -1230 MCMCOBJ=   -6549.79521258245     
 iteration        -1220 MCMCOBJ=   -6578.70066330560     
 iteration        -1210 MCMCOBJ=   -6485.65335189355     
 iteration        -1200 MCMCOBJ=   -6463.03343434738     
 iteration        -1190 MCMCOBJ=   -6614.19309326372     
 iteration        -1180 MCMCOBJ=   -6490.91791160558     
 iteration        -1170 MCMCOBJ=   -6535.11583394385     
 iteration        -1160 MCMCOBJ=   -6510.68269401611     
 iteration        -1150 MCMCOBJ=   -6516.87303354482     
 iteration        -1140 MCMCOBJ=   -6483.13477333412     
 iteration        -1130 MCMCOBJ=   -6425.62511020259     
 iteration        -1120 MCMCOBJ=   -6496.03753620530     
 iteration        -1110 MCMCOBJ=   -6440.81054130072     
 iteration        -1100 MCMCOBJ=   -6484.15963993837     
 iteration        -1090 MCMCOBJ=   -6504.10584393251     
 iteration        -1080 MCMCOBJ=   -6486.42702430916     
 iteration        -1070 MCMCOBJ=   -6505.11239794991     
 iteration        -1060 MCMCOBJ=   -6509.70540758728     
 iteration        -1050 MCMCOBJ=   -6538.96172375535     
 iteration        -1040 MCMCOBJ=   -6530.48671740848     
 iteration        -1030 MCMCOBJ=   -6464.80181410640     
 iteration        -1020 MCMCOBJ=   -6545.57846865059     
 iteration        -1010 MCMCOBJ=   -6501.97554383961     
 iteration        -1000 MCMCOBJ=   -6546.08107436019     
 iteration         -990 MCMCOBJ=   -6521.39353131196     
 iteration         -980 MCMCOBJ=   -6494.75298698422     
 iteration         -970 MCMCOBJ=   -6533.43328432209     
 iteration         -960 MCMCOBJ=   -6482.15016522345     
 iteration         -950 MCMCOBJ=   -6502.11422368350     
 iteration         -940 MCMCOBJ=   -6445.01241809581     
 iteration         -930 MCMCOBJ=   -6522.60840147858     
 iteration         -920 MCMCOBJ=   -6480.38226168596     
 iteration         -910 MCMCOBJ=   -6457.54107898404     
 iteration         -900 MCMCOBJ=   -6551.80371538509     
 iteration         -890 MCMCOBJ=   -6525.97519828324     
 iteration         -880 MCMCOBJ=   -6539.06864479701     
 iteration         -870 MCMCOBJ=   -6487.59654531159     
 iteration         -860 MCMCOBJ=   -6490.41418377362     
 iteration         -850 MCMCOBJ=   -6451.84737691757     
 iteration         -840 MCMCOBJ=   -6492.86636238633     
 iteration         -830 MCMCOBJ=   -6486.49932482636     
 iteration         -820 MCMCOBJ=   -6490.84198990018     
 iteration         -810 MCMCOBJ=   -6473.77395136979     
 iteration         -800 MCMCOBJ=   -6439.00759670221     
 iteration         -790 MCMCOBJ=   -6476.89269345184     
 iteration         -780 MCMCOBJ=   -6488.06433693892     
 iteration         -770 MCMCOBJ=   -6418.39269624860     
 iteration         -760 MCMCOBJ=   -6497.22484813977     
 iteration         -750 MCMCOBJ=   -6539.46341612797     
 iteration         -740 MCMCOBJ=   -6545.86717994615     
 iteration         -730 MCMCOBJ=   -6555.14020910879     
 iteration         -720 MCMCOBJ=   -6440.37214506398     
 iteration         -710 MCMCOBJ=   -6509.49751161248     
 iteration         -700 MCMCOBJ=   -6502.11954859845     
 iteration         -690 MCMCOBJ=   -6495.18144219567     
 iteration         -680 MCMCOBJ=   -6520.99324301324     
 iteration         -670 MCMCOBJ=   -6512.34371357913     
 iteration         -660 MCMCOBJ=   -6492.41654651928     
 iteration         -650 MCMCOBJ=   -6475.67993495001     
 iteration         -640 MCMCOBJ=   -6437.38334429791     
 iteration         -630 MCMCOBJ=   -6488.62243429984     
 iteration         -620 MCMCOBJ=   -6546.24029156093     
 iteration         -610 MCMCOBJ=   -6464.71315543105     
 iteration         -600 MCMCOBJ=   -6521.63147263803     
 iteration         -590 MCMCOBJ=   -6479.81598226265     
 iteration         -580 MCMCOBJ=   -6525.52368729467     
 iteration         -570 MCMCOBJ=   -6476.62088697031     
 iteration         -560 MCMCOBJ=   -6485.22643632310     
 iteration         -550 MCMCOBJ=   -6524.98568619537     
 iteration         -540 MCMCOBJ=   -6485.99521226514     
 iteration         -530 MCMCOBJ=   -6499.70829016258     
 iteration         -520 MCMCOBJ=   -6462.78290165445     
 iteration         -510 MCMCOBJ=   -6486.17432387533     
 iteration         -500 MCMCOBJ=   -6404.24239356957     
 iteration         -490 MCMCOBJ=   -6497.50981169561     
 iteration         -480 MCMCOBJ=   -6496.27335122278     
 iteration         -470 MCMCOBJ=   -6483.12450927118     
 iteration         -460 MCMCOBJ=   -6454.85362431214     
 iteration         -450 MCMCOBJ=   -6525.23856805305     
 iteration         -440 MCMCOBJ=   -6438.95973141080     
 iteration         -430 MCMCOBJ=   -6470.24903863458     
 iteration         -420 MCMCOBJ=   -6501.54118397054     
 iteration         -410 MCMCOBJ=   -6543.39745344085     
 iteration         -400 MCMCOBJ=   -6514.09743012388     
 iteration         -390 MCMCOBJ=   -6476.29946929233     
 iteration         -380 MCMCOBJ=   -6444.74158382728     
 iteration         -370 MCMCOBJ=   -6495.10088406824     
 iteration         -360 MCMCOBJ=   -6491.10482024556     
 iteration         -350 MCMCOBJ=   -6490.52604442112     
 iteration         -340 MCMCOBJ=   -6422.34454573049     
 iteration         -330 MCMCOBJ=   -6478.68353028956     
 iteration         -320 MCMCOBJ=   -6529.87501028973     
 iteration         -310 MCMCOBJ=   -6492.82009321055     
 iteration         -300 MCMCOBJ=   -6494.34124461081     
 iteration         -290 MCMCOBJ=   -6476.47030028017     
 iteration         -280 MCMCOBJ=   -6449.98980994466     
 iteration         -270 MCMCOBJ=   -6483.94773874016     
 iteration         -260 MCMCOBJ=   -6500.17456044071     
 iteration         -250 MCMCOBJ=   -6510.12655273634     
 iteration         -240 MCMCOBJ=   -6578.15917411222     
 iteration         -230 MCMCOBJ=   -6414.03478236985     
 iteration         -220 MCMCOBJ=   -6505.93908316998     
 iteration         -210 MCMCOBJ=   -6481.35145515842     
 iteration         -200 MCMCOBJ=   -6499.97460295009     
 iteration         -190 MCMCOBJ=   -6525.76519657840     
 iteration         -180 MCMCOBJ=   -6480.80527049367     
 iteration         -170 MCMCOBJ=   -6462.30461211113     
 iteration         -160 MCMCOBJ=   -6452.61174907307     
 iteration         -150 MCMCOBJ=   -6505.58279130489     
 iteration         -140 MCMCOBJ=   -6532.43587285581     
 iteration         -130 MCMCOBJ=   -6485.16832385739     
 iteration         -120 MCMCOBJ=   -6489.83729340687     
 iteration         -110 MCMCOBJ=   -6523.53957663891     
 iteration         -100 MCMCOBJ=   -6467.42484381914     
 iteration          -90 MCMCOBJ=   -6491.03819270503     
 iteration          -80 MCMCOBJ=   -6491.82251347067     
 iteration          -70 MCMCOBJ=   -6455.95658040231     
 iteration          -60 MCMCOBJ=   -6497.55470498370     
 iteration          -50 MCMCOBJ=   -6511.05353013265     
 iteration          -40 MCMCOBJ=   -6519.64873494688     
 iteration          -30 MCMCOBJ=   -6472.43043404535     
 iteration          -20 MCMCOBJ=   -6519.71470161497     
 iteration          -10 MCMCOBJ=   -6452.51246245751     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6448.95499030907     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS NOT PERFORMED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6448.95499030907     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3567.16375017922     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6448.95499030907     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5713.80416374534     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    55.1779157436876     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6448.95499030907     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6393.77707456539     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   582.92
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6448.955       **************************************************
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
 
         3.92E+00 -2.30E+00  6.01E-01 -2.52E-01  2.31E+00  1.15E-01  3.77E+00 -6.01E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        3.38E-01
 
 ETA2
+       -2.78E-02  2.11E-01
 
 ETA3
+        1.37E-01 -9.95E-03  1.79E-01
 
 ETA4
+        4.96E-02  7.71E-02 -7.09E-03  2.62E-01
 
 ETA5
+        8.88E-02  3.22E-02 -1.23E-04  3.04E-02  3.07E-01
 
 ETA6
+       -1.06E-01  4.21E-02 -4.72E-02  2.15E-02 -1.23E-01  2.18E-01
 
 ETA7
+        1.39E-01 -6.65E-02  9.93E-02 -6.96E-02  4.62E-02 -8.14E-02  3.08E-01
 
 ETA8
+        2.07E-01  8.34E-02  1.15E-01  1.05E-01  1.25E-01 -1.17E-01  8.36E-02  3.14E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.81E-03
 
 EPS2
+        0.00E+00  2.05E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.81E-01
 
 ETA2
+       -1.04E-01  4.60E-01
 
 ETA3
+        5.57E-01 -5.12E-02  4.23E-01
 
 ETA4
+        1.67E-01  3.28E-01 -3.28E-02  5.12E-01
 
 ETA5
+        2.76E-01  1.27E-01 -5.23E-04  1.07E-01  5.54E-01
 
 ETA6
+       -3.93E-01  1.96E-01 -2.39E-01  9.00E-02 -4.75E-01  4.66E-01
 
 ETA7
+        4.30E-01 -2.61E-01  4.23E-01 -2.45E-01  1.50E-01 -3.14E-01  5.55E-01
 
 ETA8
+        6.34E-01  3.24E-01  4.85E-01  3.65E-01  4.01E-01 -4.48E-01  2.69E-01  5.61E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.90E-02
 
 EPS2
+        0.00E+00  1.43E-01
 
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
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmto9.ext
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
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 8.00000000000000
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
 iteration        -1000 MCMCOBJ=   -6197.56996153921     
 iteration         -999 MCMCOBJ=   -6197.56997355489     
 iteration         -998 MCMCOBJ=   -6459.38004435539     
 iteration         -997 MCMCOBJ=   -6514.93410332460     
 iteration         -996 MCMCOBJ=   -6560.79570713102     
 iteration         -995 MCMCOBJ=   -6600.14865174371     
 iteration         -994 MCMCOBJ=   -6613.01438553017     
 iteration         -993 MCMCOBJ=   -6579.63715729215     
 iteration         -992 MCMCOBJ=   -6591.44539605959     
 iteration         -991 MCMCOBJ=   -6612.02766318452     
 iteration         -990 MCMCOBJ=   -6606.58268050167     
 iteration         -989 MCMCOBJ=   -6606.58269364391     
 iteration         -988 MCMCOBJ=   -6591.51949354316     
 iteration         -987 MCMCOBJ=   -6643.12857124514     
 iteration         -986 MCMCOBJ=   -6659.19748044084     
 iteration         -985 MCMCOBJ=   -6628.88310327477     
 iteration         -984 MCMCOBJ=   -6576.00174115639     
 iteration         -983 MCMCOBJ=   -6575.32806935724     
 iteration         -982 MCMCOBJ=   -6603.49638281811     
 iteration         -981 MCMCOBJ=   -6582.97120945668     
 iteration         -980 MCMCOBJ=   -6586.54428339282     
 iteration         -979 MCMCOBJ=   -6571.87981501491     
 iteration         -978 MCMCOBJ=   -6581.62242058990     
 iteration         -977 MCMCOBJ=   -6590.56547517094     
 iteration         -976 MCMCOBJ=   -6543.59076684177     
 iteration         -975 MCMCOBJ=   -6527.01063082980     
 iteration         -974 MCMCOBJ=   -6619.19092456550     
 iteration         -973 MCMCOBJ=   -6628.55123302326     
 iteration         -972 MCMCOBJ=   -6642.89099632939     
 iteration         -971 MCMCOBJ=   -6642.89099252174     
 iteration         -970 MCMCOBJ=   -6599.56202087898     
 iteration         -969 MCMCOBJ=   -6537.09712357209     
 iteration         -968 MCMCOBJ=   -6605.44850781360     
 iteration         -967 MCMCOBJ=   -6590.52478777022     
 iteration         -966 MCMCOBJ=   -6637.96967176862     
 iteration         -965 MCMCOBJ=   -6648.26061209126     
 iteration         -964 MCMCOBJ=   -6658.02103444752     
 iteration         -963 MCMCOBJ=   -6620.33238281792     
 iteration         -962 MCMCOBJ=   -6630.44976481420     
 iteration         -961 MCMCOBJ=   -6567.41562141195     
 iteration         -960 MCMCOBJ=   -6558.87064587445     
 iteration         -959 MCMCOBJ=   -6562.19933078694     
 iteration         -958 MCMCOBJ=   -6560.09496466482     
 iteration         -957 MCMCOBJ=   -6554.80452873218     
 iteration         -956 MCMCOBJ=   -6563.53268713743     
 iteration         -955 MCMCOBJ=   -6556.28978797137     
 iteration         -954 MCMCOBJ=   -6508.42936179318     
 iteration         -953 MCMCOBJ=   -6561.73742436273     
 iteration         -952 MCMCOBJ=   -6519.12375942749     
 iteration         -951 MCMCOBJ=   -6592.05594732659     
 iteration         -950 MCMCOBJ=   -6589.20599072842     
 iteration         -949 MCMCOBJ=   -6615.09937204529     
 iteration         -948 MCMCOBJ=   -6615.09937425672     
 iteration         -947 MCMCOBJ=   -6654.55048588785     
 iteration         -946 MCMCOBJ=   -6607.18877023354     
 iteration         -945 MCMCOBJ=   -6605.96832399999     
 iteration         -944 MCMCOBJ=   -6601.13786635914     
 iteration         -943 MCMCOBJ=   -6590.56246803895     
 iteration         -942 MCMCOBJ=   -6583.26995410371     
 iteration         -941 MCMCOBJ=   -6627.74490100624     
 iteration         -940 MCMCOBJ=   -6604.23222602413     
 iteration         -939 MCMCOBJ=   -6623.08343837462     
 iteration         -938 MCMCOBJ=   -6665.69652403956     
 iteration         -937 MCMCOBJ=   -6638.48182696511     
 iteration         -936 MCMCOBJ=   -6636.78708780821     
 iteration         -935 MCMCOBJ=   -6686.05141933496     
 iteration         -934 MCMCOBJ=   -6651.78160317521     
 iteration         -933 MCMCOBJ=   -6619.45613639622     
 iteration         -932 MCMCOBJ=   -6543.31739807901     
 iteration         -931 MCMCOBJ=   -6619.67652357315     
 iteration         -930 MCMCOBJ=   -6563.06666452020     
 iteration         -929 MCMCOBJ=   -6617.78384871762     
 iteration         -928 MCMCOBJ=   -6633.63480192632     
 iteration         -927 MCMCOBJ=   -6611.27692986088     
 iteration         -926 MCMCOBJ=   -6619.42087854674     
 iteration         -925 MCMCOBJ=   -6590.16955194245     
 iteration         -924 MCMCOBJ=   -6559.99286789426     
 iteration         -923 MCMCOBJ=   -6558.38054628418     
 iteration         -922 MCMCOBJ=   -6556.91727038730     
 iteration         -921 MCMCOBJ=   -6512.48420130688     
 iteration         -920 MCMCOBJ=   -6563.10245629074     
 iteration         -919 MCMCOBJ=   -6553.22951318744     
 iteration         -918 MCMCOBJ=   -6618.49247991159     
 iteration         -917 MCMCOBJ=   -6612.65566368344     
 iteration         -916 MCMCOBJ=   -6605.21268870476     
 iteration         -915 MCMCOBJ=   -6588.11815759943     
 iteration         -914 MCMCOBJ=   -6609.67678399192     
 iteration         -913 MCMCOBJ=   -6597.63894253385     
 iteration         -912 MCMCOBJ=   -6571.58996813217     
 iteration         -911 MCMCOBJ=   -6624.71348043757     
 iteration         -910 MCMCOBJ=   -6659.48947470479     
 iteration         -909 MCMCOBJ=   -6661.03169791067     
 iteration         -908 MCMCOBJ=   -6635.84735133366     
 iteration         -907 MCMCOBJ=   -6637.79999070197     
 iteration         -906 MCMCOBJ=   -6623.64059540154     
 iteration         -905 MCMCOBJ=   -6591.67736972940     
 iteration         -904 MCMCOBJ=   -6595.19064515517     
 iteration         -903 MCMCOBJ=   -6593.81071303182     
 iteration         -902 MCMCOBJ=   -6636.84671961373     
 iteration         -901 MCMCOBJ=   -6590.29038053140     
 iteration         -900 MCMCOBJ=   -6614.36908160226     
 iteration         -899 MCMCOBJ=   -6612.10997446511     
 iteration         -898 MCMCOBJ=   -6612.11002863599     
 iteration         -897 MCMCOBJ=   -6659.25788048053     
 iteration         -896 MCMCOBJ=   -6583.29391107724     
 iteration         -895 MCMCOBJ=   -6549.22752289842     
 iteration         -894 MCMCOBJ=   -6507.04899574176     
 iteration         -893 MCMCOBJ=   -6606.19750264253     
 iteration         -892 MCMCOBJ=   -6586.27723581621     
 iteration         -891 MCMCOBJ=   -6547.53608440647     
 iteration         -890 MCMCOBJ=   -6579.65776187989     
 iteration         -889 MCMCOBJ=   -6572.56514456395     
 iteration         -888 MCMCOBJ=   -6557.42379351042     
 iteration         -887 MCMCOBJ=   -6587.41527126080     
 iteration         -886 MCMCOBJ=   -6587.41529081593     
 iteration         -885 MCMCOBJ=   -6597.23853467755     
 iteration         -884 MCMCOBJ=   -6625.15165856009     
 iteration         -883 MCMCOBJ=   -6617.32369940637     
 iteration         -882 MCMCOBJ=   -6621.45918857138     
 iteration         -881 MCMCOBJ=   -6576.58438086548     
 iteration         -880 MCMCOBJ=   -6609.67207345427     
 iteration         -879 MCMCOBJ=   -6566.89613394077     
 iteration         -878 MCMCOBJ=   -6577.79180514251     
 iteration         -877 MCMCOBJ=   -6577.79180806644     
 iteration         -876 MCMCOBJ=   -6608.97799159278     
 iteration         -875 MCMCOBJ=   -6628.32037529563     
 iteration         -874 MCMCOBJ=   -6580.43455570436     
 iteration         -873 MCMCOBJ=   -6569.07089451437     
 iteration         -872 MCMCOBJ=   -6648.80783281406     
 iteration         -871 MCMCOBJ=   -6594.76961561540     
 iteration         -870 MCMCOBJ=   -6567.75669834196     
 iteration         -869 MCMCOBJ=   -6593.19693192574     
 iteration         -868 MCMCOBJ=   -6593.19689928043     
 iteration         -867 MCMCOBJ=   -6620.64543528404     
 iteration         -866 MCMCOBJ=   -6640.27104436033     
 iteration         -865 MCMCOBJ=   -6664.18055489428     
 iteration         -864 MCMCOBJ=   -6686.98593652076     
 iteration         -863 MCMCOBJ=   -6655.59792162534     
 iteration         -862 MCMCOBJ=   -6642.52823289997     
 iteration         -861 MCMCOBJ=   -6665.80728149565     
 iteration         -860 MCMCOBJ=   -6649.52357046172     
 iteration         -859 MCMCOBJ=   -6616.11699676496     
 iteration         -858 MCMCOBJ=   -6588.96053909440     
 iteration         -857 MCMCOBJ=   -6580.12766718895     
 iteration         -856 MCMCOBJ=   -6629.42450341518     
 iteration         -855 MCMCOBJ=   -6629.42448504534     
 iteration         -854 MCMCOBJ=   -6602.24097805106     
 iteration         -853 MCMCOBJ=   -6597.30886300727     
 iteration         -852 MCMCOBJ=   -6614.25939225857     
 iteration         -851 MCMCOBJ=   -6581.23968860947     
 iteration         -850 MCMCOBJ=   -6596.94062411601     
 iteration         -849 MCMCOBJ=   -6564.93673207958     
 iteration         -848 MCMCOBJ=   -6618.71142897890     
 iteration         -847 MCMCOBJ=   -6593.43321434325     
 iteration         -846 MCMCOBJ=   -6517.53147853135     
 iteration         -845 MCMCOBJ=   -6595.27072697772     
 iteration         -844 MCMCOBJ=   -6582.61804517875     
 iteration         -843 MCMCOBJ=   -6567.09498462629     
 iteration         -842 MCMCOBJ=   -6606.97915637369     
 iteration         -841 MCMCOBJ=   -6598.51995876863     
 iteration         -840 MCMCOBJ=   -6567.92937102270     
 iteration         -839 MCMCOBJ=   -6574.41274248990     
 iteration         -838 MCMCOBJ=   -6555.81966488617     
 iteration         -837 MCMCOBJ=   -6569.59668210910     
 iteration         -836 MCMCOBJ=   -6552.26448333852     
 iteration         -835 MCMCOBJ=   -6506.95088947408     
 iteration         -834 MCMCOBJ=   -6493.74001429692     
 iteration         -833 MCMCOBJ=   -6506.59024555762     
 iteration         -832 MCMCOBJ=   -6476.01001218015     
 iteration         -831 MCMCOBJ=   -6474.69388785899     
 iteration         -830 MCMCOBJ=   -6482.94639871766     
 iteration         -829 MCMCOBJ=   -6564.05246421952     
 iteration         -828 MCMCOBJ=   -6534.58842274346     
 iteration         -827 MCMCOBJ=   -6554.55563058657     
 iteration         -826 MCMCOBJ=   -6538.99400926143     
 iteration         -825 MCMCOBJ=   -6576.55808690765     
 iteration         -824 MCMCOBJ=   -6608.02809679956     
 iteration         -823 MCMCOBJ=   -6581.16335926190     
 iteration         -822 MCMCOBJ=   -6585.75886255579     
 iteration         -821 MCMCOBJ=   -6650.60630816426     
 iteration         -820 MCMCOBJ=   -6650.60631017009     
 iteration         -819 MCMCOBJ=   -6629.41596566831     
 iteration         -818 MCMCOBJ=   -6600.87085101785     
 iteration         -817 MCMCOBJ=   -6554.37091129028     
 iteration         -816 MCMCOBJ=   -6560.87386939314     
 iteration         -815 MCMCOBJ=   -6611.67471069581     
 iteration         -814 MCMCOBJ=   -6673.00585184523     
 iteration         -813 MCMCOBJ=   -6674.29917740275     
 iteration         -812 MCMCOBJ=   -6678.54426631620     
 iteration         -811 MCMCOBJ=   -6656.63851364111     
 iteration         -810 MCMCOBJ=   -6634.83722971222     
 iteration         -809 MCMCOBJ=   -6643.80842847256     
 iteration         -808 MCMCOBJ=   -6637.63308921438     
 iteration         -807 MCMCOBJ=   -6555.61147207623     
 iteration         -806 MCMCOBJ=   -6557.78100793169     
 iteration         -805 MCMCOBJ=   -6600.21911109214     
 iteration         -804 MCMCOBJ=   -6569.96540654460     
 iteration         -803 MCMCOBJ=   -6542.39484741639     
 iteration         -802 MCMCOBJ=   -6592.23899591746     
 iteration         -801 MCMCOBJ=   -6618.86576439094     
 iteration         -800 MCMCOBJ=   -6645.10111943523     
 iteration         -799 MCMCOBJ=   -6639.60451159555     
 iteration         -798 MCMCOBJ=   -6622.96705067508     
 iteration         -797 MCMCOBJ=   -6555.84899685591     
 iteration         -796 MCMCOBJ=   -6590.33207816810     
 iteration         -795 MCMCOBJ=   -6584.02855322047     
 iteration         -794 MCMCOBJ=   -6555.75843693293     
 iteration         -793 MCMCOBJ=   -6593.16660074441     
 iteration         -792 MCMCOBJ=   -6539.67042640527     
 iteration         -791 MCMCOBJ=   -6608.19601894000     
 iteration         -790 MCMCOBJ=   -6628.93808494751     
 iteration         -789 MCMCOBJ=   -6624.65076370486     
 iteration         -788 MCMCOBJ=   -6578.43422366654     
 iteration         -787 MCMCOBJ=   -6607.76860394100     
 iteration         -786 MCMCOBJ=   -6567.44569204066     
 iteration         -785 MCMCOBJ=   -6597.98026123360     
 iteration         -784 MCMCOBJ=   -6595.61747837748     
 iteration         -783 MCMCOBJ=   -6567.85355443515     
 iteration         -782 MCMCOBJ=   -6601.75435226103     
 iteration         -781 MCMCOBJ=   -6617.73914253310     
 iteration         -780 MCMCOBJ=   -6635.26516195611     
 iteration         -779 MCMCOBJ=   -6641.65132803873     
 iteration         -778 MCMCOBJ=   -6634.29417086095     
 iteration         -777 MCMCOBJ=   -6634.29418125842     
 iteration         -776 MCMCOBJ=   -6606.31143308641     
 iteration         -775 MCMCOBJ=   -6628.29686451050     
 iteration         -774 MCMCOBJ=   -6563.72531775392     
 iteration         -773 MCMCOBJ=   -6622.06697414707     
 iteration         -772 MCMCOBJ=   -6610.33030617722     
 iteration         -771 MCMCOBJ=   -6567.70422940879     
 iteration         -770 MCMCOBJ=   -6557.65735303562     
 iteration         -769 MCMCOBJ=   -6512.59593704385     
 iteration         -768 MCMCOBJ=   -6574.99137546342     
 iteration         -767 MCMCOBJ=   -6570.01611452581     
 iteration         -766 MCMCOBJ=   -6609.26010833196     
 iteration         -765 MCMCOBJ=   -6587.95895930722     
 iteration         -764 MCMCOBJ=   -6601.78694457787     
 iteration         -763 MCMCOBJ=   -6597.11598070846     
 iteration         -762 MCMCOBJ=   -6569.94902384713     
 iteration         -761 MCMCOBJ=   -6537.58562950697     
 iteration         -760 MCMCOBJ=   -6553.52356324156     
 iteration         -759 MCMCOBJ=   -6554.69959566628     
 iteration         -758 MCMCOBJ=   -6490.72554453966     
 iteration         -757 MCMCOBJ=   -6552.24418719882     
 iteration         -756 MCMCOBJ=   -6533.58927309472     
 iteration         -755 MCMCOBJ=   -6545.01759947795     
 iteration         -754 MCMCOBJ=   -6615.95917996552     
 iteration         -753 MCMCOBJ=   -6600.30696832919     
 iteration         -752 MCMCOBJ=   -6606.79448168017     
 iteration         -751 MCMCOBJ=   -6596.24292859110     
 iteration         -750 MCMCOBJ=   -6588.03820225461     
 iteration         -749 MCMCOBJ=   -6499.96447136951     
 iteration         -748 MCMCOBJ=   -6585.30084204212     
 iteration         -747 MCMCOBJ=   -6606.33119125502     
 iteration         -746 MCMCOBJ=   -6575.56528903478     
 iteration         -745 MCMCOBJ=   -6579.40947139919     
 iteration         -744 MCMCOBJ=   -6611.23389730870     
 iteration         -743 MCMCOBJ=   -6642.50852800110     
 iteration         -742 MCMCOBJ=   -6583.36133151684     
 iteration         -741 MCMCOBJ=   -6556.88663743546     
 iteration         -740 MCMCOBJ=   -6503.05889472133     
 iteration         -739 MCMCOBJ=   -6585.70324888124     
 iteration         -738 MCMCOBJ=   -6598.48285007559     
 iteration         -737 MCMCOBJ=   -6610.89246133021     
 iteration         -736 MCMCOBJ=   -6552.33790393657     
 iteration         -735 MCMCOBJ=   -6506.64358461058     
 iteration         -734 MCMCOBJ=   -6523.07405647459     
 iteration         -733 MCMCOBJ=   -6542.55710348600     
 iteration         -732 MCMCOBJ=   -6529.57670949225     
 iteration         -731 MCMCOBJ=   -6548.59805589094     
 iteration         -730 MCMCOBJ=   -6614.78835802688     
 iteration         -729 MCMCOBJ=   -6624.94155416696     
 iteration         -728 MCMCOBJ=   -6593.38162799484     
 iteration         -727 MCMCOBJ=   -6610.56466540008     
 iteration         -726 MCMCOBJ=   -6609.29808694551     
 iteration         -725 MCMCOBJ=   -6621.17512203261     
 iteration         -724 MCMCOBJ=   -6611.81788489860     
 iteration         -723 MCMCOBJ=   -6566.31553327788     
 iteration         -722 MCMCOBJ=   -6553.08185257860     
 iteration         -721 MCMCOBJ=   -6538.54931005907     
 iteration         -720 MCMCOBJ=   -6583.06418041150     
 iteration         -719 MCMCOBJ=   -6660.15981052472     
 iteration         -718 MCMCOBJ=   -6609.11099517868     
 iteration         -717 MCMCOBJ=   -6614.14930442619     
 iteration         -716 MCMCOBJ=   -6619.13103172969     
 iteration         -715 MCMCOBJ=   -6547.09557063790     
 iteration         -714 MCMCOBJ=   -6632.99606020538     
 iteration         -713 MCMCOBJ=   -6657.70309641345     
 iteration         -712 MCMCOBJ=   -6666.62844750550     
 iteration         -711 MCMCOBJ=   -6646.38706130059     
 iteration         -710 MCMCOBJ=   -6631.87819072165     
 iteration         -709 MCMCOBJ=   -6618.75850703436     
 iteration         -708 MCMCOBJ=   -6613.81230245306     
 iteration         -707 MCMCOBJ=   -6599.05406418619     
 iteration         -706 MCMCOBJ=   -6580.18652226956     
 iteration         -705 MCMCOBJ=   -6579.04353631617     
 iteration         -704 MCMCOBJ=   -6581.35648669794     
 iteration         -703 MCMCOBJ=   -6645.87213010209     
 iteration         -702 MCMCOBJ=   -6644.24196795114     
 iteration         -701 MCMCOBJ=   -6654.49489615731     
 iteration         -700 MCMCOBJ=   -6649.23453489783     
 iteration         -699 MCMCOBJ=   -6634.42400087615     
 iteration         -698 MCMCOBJ=   -6605.96916266387     
 iteration         -697 MCMCOBJ=   -6624.52718756991     
 iteration         -696 MCMCOBJ=   -6604.84093585750     
 iteration         -695 MCMCOBJ=   -6639.59981966897     
 iteration         -694 MCMCOBJ=   -6617.92488408286     
 iteration         -693 MCMCOBJ=   -6601.64993440175     
 iteration         -692 MCMCOBJ=   -6629.09791383147     
 iteration         -691 MCMCOBJ=   -6621.12194322577     
 iteration         -690 MCMCOBJ=   -6587.55025240071     
 iteration         -689 MCMCOBJ=   -6613.79608691015     
 iteration         -688 MCMCOBJ=   -6612.76219547087     
 iteration         -687 MCMCOBJ=   -6613.61404659747     
 iteration         -686 MCMCOBJ=   -6653.66770689738     
 iteration         -685 MCMCOBJ=   -6657.01044751461     
 iteration         -684 MCMCOBJ=   -6641.01682915688     
 iteration         -683 MCMCOBJ=   -6655.27636846199     
 iteration         -682 MCMCOBJ=   -6641.70435252537     
 iteration         -681 MCMCOBJ=   -6639.93819837842     
 iteration         -680 MCMCOBJ=   -6648.73887166954     
 iteration         -679 MCMCOBJ=   -6600.38232886458     
 iteration         -678 MCMCOBJ=   -6614.77132490280     
 iteration         -677 MCMCOBJ=   -6574.88431897519     
 iteration         -676 MCMCOBJ=   -6576.77104350064     
 iteration         -675 MCMCOBJ=   -6571.33118675950     
 iteration         -674 MCMCOBJ=   -6582.38290617780     
 iteration         -673 MCMCOBJ=   -6568.27625686373     
 iteration         -672 MCMCOBJ=   -6565.17086287403     
 iteration         -671 MCMCOBJ=   -6633.95972606184     
 iteration         -670 MCMCOBJ=   -6634.88274887340     
 iteration         -669 MCMCOBJ=   -6635.29963766685     
 iteration         -668 MCMCOBJ=   -6616.22783729402     
 iteration         -667 MCMCOBJ=   -6609.83388933471     
 iteration         -666 MCMCOBJ=   -6631.06594849437     
 iteration         -665 MCMCOBJ=   -6636.42448823040     
 iteration         -664 MCMCOBJ=   -6637.17804114596     
 iteration         -663 MCMCOBJ=   -6634.66280431435     
 iteration         -662 MCMCOBJ=   -6618.58136094440     
 iteration         -661 MCMCOBJ=   -6596.03033550859     
 iteration         -660 MCMCOBJ=   -6593.09981670383     
 iteration         -659 MCMCOBJ=   -6604.95974043445     
 iteration         -658 MCMCOBJ=   -6601.80093781632     
 iteration         -657 MCMCOBJ=   -6600.61813530911     
 iteration         -656 MCMCOBJ=   -6607.86242995998     
 iteration         -655 MCMCOBJ=   -6602.56038313226     
 iteration         -654 MCMCOBJ=   -6633.08759816516     
 iteration         -653 MCMCOBJ=   -6623.36415345821     
 iteration         -652 MCMCOBJ=   -6638.26261701124     
 iteration         -651 MCMCOBJ=   -6596.37681471710     
 iteration         -650 MCMCOBJ=   -6603.79626010071     
 iteration         -649 MCMCOBJ=   -6596.29866094768     
 iteration         -648 MCMCOBJ=   -6619.12250862055     
 iteration         -647 MCMCOBJ=   -6640.29056981660     
 iteration         -646 MCMCOBJ=   -6678.99924281195     
 iteration         -645 MCMCOBJ=   -6648.20749997668     
 iteration         -644 MCMCOBJ=   -6648.20749869547     
 iteration         -643 MCMCOBJ=   -6591.17963535405     
 iteration         -642 MCMCOBJ=   -6558.19363666379     
 iteration         -641 MCMCOBJ=   -6564.64081527741     
 iteration         -640 MCMCOBJ=   -6569.87432718648     
 iteration         -639 MCMCOBJ=   -6575.75720859517     
 iteration         -638 MCMCOBJ=   -6575.75721871176     
 iteration         -637 MCMCOBJ=   -6592.26538155985     
 iteration         -636 MCMCOBJ=   -6568.61662491031     
 iteration         -635 MCMCOBJ=   -6608.28748051751     
 iteration         -634 MCMCOBJ=   -6599.79602475034     
 iteration         -633 MCMCOBJ=   -6606.78633406838     
 iteration         -632 MCMCOBJ=   -6609.89984941148     
 iteration         -631 MCMCOBJ=   -6577.40914862112     
 iteration         -630 MCMCOBJ=   -6492.52698278628     
 iteration         -629 MCMCOBJ=   -6549.68610485514     
 iteration         -628 MCMCOBJ=   -6533.98486514396     
 iteration         -627 MCMCOBJ=   -6582.35162491853     
 iteration         -626 MCMCOBJ=   -6614.45056274968     
 iteration         -625 MCMCOBJ=   -6633.89358868380     
 iteration         -624 MCMCOBJ=   -6605.83065179724     
 iteration         -623 MCMCOBJ=   -6576.38921007274     
 iteration         -622 MCMCOBJ=   -6649.12129957268     
 iteration         -621 MCMCOBJ=   -6576.88839520911     
 iteration         -620 MCMCOBJ=   -6628.39266568585     
 iteration         -619 MCMCOBJ=   -6645.79377043860     
 iteration         -618 MCMCOBJ=   -6645.79377147575     
 iteration         -617 MCMCOBJ=   -6584.95924734947     
 iteration         -616 MCMCOBJ=   -6558.81032760681     
 iteration         -615 MCMCOBJ=   -6619.23967650570     
 iteration         -614 MCMCOBJ=   -6604.37922388505     
 iteration         -613 MCMCOBJ=   -6627.17993270200     
 iteration         -612 MCMCOBJ=   -6627.09779318775     
 iteration         -611 MCMCOBJ=   -6629.66061960991     
 iteration         -610 MCMCOBJ=   -6630.15061016883     
 iteration         -609 MCMCOBJ=   -6680.76573248752     
 iteration         -608 MCMCOBJ=   -6663.66823617794     
 iteration         -607 MCMCOBJ=   -6654.09865114398     
 iteration         -606 MCMCOBJ=   -6627.46394204748     
 iteration         -605 MCMCOBJ=   -6635.61544474411     
 iteration         -604 MCMCOBJ=   -6542.08116172915     
 iteration         -603 MCMCOBJ=   -6588.02759355642     
 iteration         -602 MCMCOBJ=   -6605.13934495975     
 iteration         -601 MCMCOBJ=   -6616.15534488243     
 iteration         -600 MCMCOBJ=   -6641.17324541964     
 iteration         -599 MCMCOBJ=   -6620.00512840347     
 iteration         -598 MCMCOBJ=   -6586.83455519014     
 iteration         -597 MCMCOBJ=   -6577.86696057913     
 iteration         -596 MCMCOBJ=   -6571.94196617417     
 iteration         -595 MCMCOBJ=   -6656.53164131200     
 iteration         -594 MCMCOBJ=   -6673.94804504751     
 iteration         -593 MCMCOBJ=   -6596.74748140734     
 iteration         -592 MCMCOBJ=   -6576.54035101140     
 iteration         -591 MCMCOBJ=   -6587.00290895353     
 iteration         -590 MCMCOBJ=   -6619.25406768696     
 iteration         -589 MCMCOBJ=   -6619.25408753006     
 iteration         -588 MCMCOBJ=   -6624.27840132098     
 iteration         -587 MCMCOBJ=   -6570.41029003108     
 iteration         -586 MCMCOBJ=   -6610.69697819912     
 iteration         -585 MCMCOBJ=   -6602.81832040312     
 iteration         -584 MCMCOBJ=   -6588.80830610367     
 iteration         -583 MCMCOBJ=   -6592.13253406412     
 iteration         -582 MCMCOBJ=   -6627.43854908771     
 iteration         -581 MCMCOBJ=   -6619.44123493966     
 iteration         -580 MCMCOBJ=   -6574.75692059561     
 iteration         -579 MCMCOBJ=   -6629.96054535294     
 iteration         -578 MCMCOBJ=   -6589.32134463480     
 iteration         -577 MCMCOBJ=   -6595.98535181112     
 iteration         -576 MCMCOBJ=   -6584.67367308671     
 iteration         -575 MCMCOBJ=   -6664.71875979213     
 iteration         -574 MCMCOBJ=   -6664.71876092491     
 iteration         -573 MCMCOBJ=   -6587.41295235564     
 iteration         -572 MCMCOBJ=   -6572.87588166004     
 iteration         -571 MCMCOBJ=   -6605.19068090697     
 iteration         -570 MCMCOBJ=   -6599.89379668869     
 iteration         -569 MCMCOBJ=   -6581.19427739811     
 iteration         -568 MCMCOBJ=   -6616.04361884785     
 iteration         -567 MCMCOBJ=   -6620.21749168856     
 iteration         -566 MCMCOBJ=   -6613.30683245612     
 iteration         -565 MCMCOBJ=   -6563.31175594001     
 iteration         -564 MCMCOBJ=   -6564.72284953821     
 iteration         -563 MCMCOBJ=   -6569.93454952987     
 iteration         -562 MCMCOBJ=   -6619.95611848582     
 iteration         -561 MCMCOBJ=   -6619.95611858060     
 iteration         -560 MCMCOBJ=   -6600.39625128716     
 iteration         -559 MCMCOBJ=   -6625.26486163773     
 iteration         -558 MCMCOBJ=   -6622.16666657591     
 iteration         -557 MCMCOBJ=   -6597.86384842909     
 iteration         -556 MCMCOBJ=   -6634.69950532329     
 iteration         -555 MCMCOBJ=   -6628.79708040652     
 iteration         -554 MCMCOBJ=   -6625.75405855201     
 iteration         -553 MCMCOBJ=   -6519.66107370512     
 iteration         -552 MCMCOBJ=   -6625.23536682737     
 iteration         -551 MCMCOBJ=   -6669.62344873836     
 iteration         -550 MCMCOBJ=   -6584.59369421375     
 iteration         -549 MCMCOBJ=   -6584.13850385958     
 iteration         -548 MCMCOBJ=   -6559.33071229549     
 iteration         -547 MCMCOBJ=   -6551.34275698858     
 iteration         -546 MCMCOBJ=   -6550.62951105914     
 iteration         -545 MCMCOBJ=   -6535.32235366999     
 iteration         -544 MCMCOBJ=   -6593.13845833064     
 iteration         -543 MCMCOBJ=   -6549.42028844019     
 iteration         -542 MCMCOBJ=   -6545.19657568288     
 iteration         -541 MCMCOBJ=   -6605.64380549083     
 iteration         -540 MCMCOBJ=   -6552.68711898513     
 iteration         -539 MCMCOBJ=   -6515.53002321431     
 iteration         -538 MCMCOBJ=   -6518.31882366901     
 iteration         -537 MCMCOBJ=   -6569.33421253903     
 iteration         -536 MCMCOBJ=   -6537.43075404760     
 iteration         -535 MCMCOBJ=   -6576.07589253918     
 iteration         -534 MCMCOBJ=   -6590.50294972811     
 iteration         -533 MCMCOBJ=   -6592.03398491340     
 iteration         -532 MCMCOBJ=   -6633.66503102685     
 iteration         -531 MCMCOBJ=   -6587.02351814838     
 iteration         -530 MCMCOBJ=   -6607.62895495187     
 iteration         -529 MCMCOBJ=   -6574.28430847194     
 iteration         -528 MCMCOBJ=   -6635.87242218648     
 iteration         -527 MCMCOBJ=   -6640.26411063994     
 iteration         -526 MCMCOBJ=   -6583.99719340508     
 iteration         -525 MCMCOBJ=   -6558.51858588052     
 iteration         -524 MCMCOBJ=   -6610.27640474548     
 iteration         -523 MCMCOBJ=   -6578.02496652885     
 iteration         -522 MCMCOBJ=   -6625.97395454097     
 iteration         -521 MCMCOBJ=   -6602.67236868027     
 iteration         -520 MCMCOBJ=   -6590.72609759657     
 iteration         -519 MCMCOBJ=   -6629.32933286397     
 iteration         -518 MCMCOBJ=   -6615.97283662371     
 iteration         -517 MCMCOBJ=   -6596.56924353871     
 iteration         -516 MCMCOBJ=   -6631.01817969674     
 iteration         -515 MCMCOBJ=   -6604.17567412600     
 iteration         -514 MCMCOBJ=   -6623.05587872778     
 iteration         -513 MCMCOBJ=   -6680.30794737189     
 iteration         -512 MCMCOBJ=   -6667.73139490978     
 iteration         -511 MCMCOBJ=   -6653.24111920639     
 iteration         -510 MCMCOBJ=   -6630.40759947205     
 iteration         -509 MCMCOBJ=   -6607.60729835624     
 iteration         -508 MCMCOBJ=   -6629.59272178091     
 iteration         -507 MCMCOBJ=   -6609.56642453229     
 iteration         -506 MCMCOBJ=   -6654.23471387943     
 iteration         -505 MCMCOBJ=   -6654.23472735420     
 iteration         -504 MCMCOBJ=   -6679.40112483153     
 iteration         -503 MCMCOBJ=   -6622.78135774849     
 iteration         -502 MCMCOBJ=   -6622.78138291076     
 iteration         -501 MCMCOBJ=   -6607.53239694389     
 iteration         -500 MCMCOBJ=   -6618.05103051362     
 iteration         -499 MCMCOBJ=   -6638.03704233544     
 iteration         -498 MCMCOBJ=   -6612.59462234127     
 iteration         -497 MCMCOBJ=   -6593.39995832237     
 iteration         -496 MCMCOBJ=   -6582.35421151603     
 iteration         -495 MCMCOBJ=   -6633.14983208284     
 iteration         -494 MCMCOBJ=   -6619.77395124024     
 iteration         -493 MCMCOBJ=   -6605.59819211967     
 iteration         -492 MCMCOBJ=   -6584.46915863136     
 iteration         -491 MCMCOBJ=   -6619.19653283350     
 iteration         -490 MCMCOBJ=   -6628.86371803271     
 iteration         -489 MCMCOBJ=   -6670.73513706597     
 iteration         -488 MCMCOBJ=   -6637.20153088439     
 iteration         -487 MCMCOBJ=   -6621.47196939240     
 iteration         -486 MCMCOBJ=   -6575.24584132917     
 iteration         -485 MCMCOBJ=   -6591.26659416997     
 iteration         -484 MCMCOBJ=   -6570.44510408160     
 iteration         -483 MCMCOBJ=   -6555.84046981931     
 iteration         -482 MCMCOBJ=   -6573.06769117382     
 iteration         -481 MCMCOBJ=   -6584.01360259195     
 iteration         -480 MCMCOBJ=   -6562.13308692997     
 iteration         -479 MCMCOBJ=   -6590.56347298719     
 iteration         -478 MCMCOBJ=   -6620.54797967226     
 iteration         -477 MCMCOBJ=   -6598.97477564018     
 iteration         -476 MCMCOBJ=   -6594.76544534319     
 iteration         -475 MCMCOBJ=   -6671.51156797664     
 iteration         -474 MCMCOBJ=   -6618.91202068194     
 iteration         -473 MCMCOBJ=   -6597.82781023956     
 iteration         -472 MCMCOBJ=   -6600.02719689497     
 iteration         -471 MCMCOBJ=   -6584.45752136922     
 iteration         -470 MCMCOBJ=   -6604.83165280239     
 iteration         -469 MCMCOBJ=   -6604.40460761717     
 iteration         -468 MCMCOBJ=   -6599.88205670969     
 iteration         -467 MCMCOBJ=   -6575.59560466902     
 iteration         -466 MCMCOBJ=   -6590.11214218718     
 iteration         -465 MCMCOBJ=   -6679.83325302270     
 iteration         -464 MCMCOBJ=   -6631.63505107664     
 iteration         -463 MCMCOBJ=   -6595.00065098767     
 iteration         -462 MCMCOBJ=   -6615.24479082698     
 iteration         -461 MCMCOBJ=   -6581.62917230385     
 iteration         -460 MCMCOBJ=   -6593.87681602936     
 iteration         -459 MCMCOBJ=   -6604.87108409428     
 iteration         -458 MCMCOBJ=   -6647.94752896081     
 iteration         -457 MCMCOBJ=   -6652.84630447690     
 iteration         -456 MCMCOBJ=   -6635.84339012884     
 iteration         -455 MCMCOBJ=   -6611.23841794687     
 iteration         -454 MCMCOBJ=   -6664.55521049807     
 iteration         -453 MCMCOBJ=   -6674.40955161327     
 iteration         -452 MCMCOBJ=   -6667.64124378750     
 iteration         -451 MCMCOBJ=   -6698.22316796684     
 iteration         -450 MCMCOBJ=   -6671.42092501973     
 iteration         -449 MCMCOBJ=   -6647.09133122045     
 iteration         -448 MCMCOBJ=   -6645.13577169452     
 iteration         -447 MCMCOBJ=   -6624.76519236456     
 iteration         -446 MCMCOBJ=   -6609.91740532802     
 iteration         -445 MCMCOBJ=   -6635.24439450775     
 iteration         -444 MCMCOBJ=   -6630.40190028227     
 iteration         -443 MCMCOBJ=   -6643.97078402176     
 iteration         -442 MCMCOBJ=   -6586.63957765320     
 iteration         -441 MCMCOBJ=   -6570.59169346045     
 iteration         -440 MCMCOBJ=   -6567.58301259613     
 iteration         -439 MCMCOBJ=   -6528.32452327754     
 iteration         -438 MCMCOBJ=   -6613.05387251878     
 iteration         -437 MCMCOBJ=   -6663.42371316394     
 iteration         -436 MCMCOBJ=   -6625.71342068165     
 iteration         -435 MCMCOBJ=   -6637.76713676059     
 iteration         -434 MCMCOBJ=   -6669.84049979142     
 iteration         -433 MCMCOBJ=   -6667.48660695319     
 iteration         -432 MCMCOBJ=   -6650.91566199956     
 iteration         -431 MCMCOBJ=   -6612.08461794104     
 iteration         -430 MCMCOBJ=   -6585.22936637130     
 iteration         -429 MCMCOBJ=   -6546.45555654564     
 iteration         -428 MCMCOBJ=   -6566.31168584551     
 iteration         -427 MCMCOBJ=   -6543.31678822179     
 iteration         -426 MCMCOBJ=   -6570.30791627850     
 iteration         -425 MCMCOBJ=   -6581.24863571418     
 iteration         -424 MCMCOBJ=   -6568.22112041904     
 iteration         -423 MCMCOBJ=   -6568.22106082163     
 iteration         -422 MCMCOBJ=   -6578.41835459587     
 iteration         -421 MCMCOBJ=   -6595.71424849666     
 iteration         -420 MCMCOBJ=   -6613.74989797068     
 iteration         -419 MCMCOBJ=   -6628.14404685647     
 iteration         -418 MCMCOBJ=   -6615.37281637225     
 iteration         -417 MCMCOBJ=   -6567.19977734714     
 iteration         -416 MCMCOBJ=   -6570.79970270110     
 iteration         -415 MCMCOBJ=   -6582.69252558931     
 iteration         -414 MCMCOBJ=   -6563.52319366922     
 iteration         -413 MCMCOBJ=   -6539.24798430166     
 iteration         -412 MCMCOBJ=   -6492.47973476336     
 iteration         -411 MCMCOBJ=   -6514.69439508357     
 iteration         -410 MCMCOBJ=   -6551.36395665271     
 iteration         -409 MCMCOBJ=   -6536.78612899677     
 iteration         -408 MCMCOBJ=   -6519.92849595890     
 iteration         -407 MCMCOBJ=   -6615.24388564664     
 iteration         -406 MCMCOBJ=   -6669.26382664666     
 iteration         -405 MCMCOBJ=   -6635.66027219333     
 iteration         -404 MCMCOBJ=   -6617.83963497361     
 iteration         -403 MCMCOBJ=   -6634.26925172838     
 iteration         -402 MCMCOBJ=   -6610.01628884088     
 iteration         -401 MCMCOBJ=   -6627.12437876574     
 iteration         -400 MCMCOBJ=   -6614.71049590434     
 iteration         -399 MCMCOBJ=   -6571.26271999035     
 iteration         -398 MCMCOBJ=   -6596.39475962596     
 iteration         -397 MCMCOBJ=   -6591.70591234118     
 iteration         -396 MCMCOBJ=   -6593.91680774792     
 iteration         -395 MCMCOBJ=   -6598.14460950449     
 iteration         -394 MCMCOBJ=   -6589.02326265472     
 iteration         -393 MCMCOBJ=   -6571.37633654393     
 iteration         -392 MCMCOBJ=   -6563.11136818378     
 iteration         -391 MCMCOBJ=   -6601.67460126786     
 iteration         -390 MCMCOBJ=   -6612.14586437073     
 iteration         -389 MCMCOBJ=   -6629.38308622947     
 iteration         -388 MCMCOBJ=   -6644.12204583093     
 iteration         -387 MCMCOBJ=   -6642.52525830233     
 iteration         -386 MCMCOBJ=   -6686.08519754504     
 iteration         -385 MCMCOBJ=   -6654.97616387578     
 iteration         -384 MCMCOBJ=   -6632.31165811458     
 iteration         -383 MCMCOBJ=   -6619.27862438089     
 iteration         -382 MCMCOBJ=   -6586.77878332089     
 iteration         -381 MCMCOBJ=   -6570.15792440970     
 iteration         -380 MCMCOBJ=   -6657.82924970226     
 iteration         -379 MCMCOBJ=   -6667.99777116199     
 iteration         -378 MCMCOBJ=   -6643.87755435859     
 iteration         -377 MCMCOBJ=   -6628.95433175283     
 iteration         -376 MCMCOBJ=   -6647.47765629984     
 iteration         -375 MCMCOBJ=   -6620.11846158087     
 iteration         -374 MCMCOBJ=   -6603.42206465290     
 iteration         -373 MCMCOBJ=   -6561.11285367815     
 iteration         -372 MCMCOBJ=   -6599.06858530646     
 iteration         -371 MCMCOBJ=   -6626.02887265311     
 iteration         -370 MCMCOBJ=   -6572.12371835973     
 iteration         -369 MCMCOBJ=   -6555.40572701323     
 iteration         -368 MCMCOBJ=   -6567.88496053403     
 iteration         -367 MCMCOBJ=   -6580.59187309165     
 iteration         -366 MCMCOBJ=   -6586.86733129407     
 iteration         -365 MCMCOBJ=   -6594.31175167582     
 iteration         -364 MCMCOBJ=   -6586.47743589814     
 iteration         -363 MCMCOBJ=   -6594.60749757311     
 iteration         -362 MCMCOBJ=   -6581.84415202052     
 iteration         -361 MCMCOBJ=   -6625.78488755561     
 iteration         -360 MCMCOBJ=   -6624.58490334073     
 iteration         -359 MCMCOBJ=   -6603.40786828335     
 iteration         -358 MCMCOBJ=   -6594.62294009394     
 iteration         -357 MCMCOBJ=   -6609.69716472048     
 iteration         -356 MCMCOBJ=   -6622.55347216396     
 iteration         -355 MCMCOBJ=   -6574.31344668687     
 iteration         -354 MCMCOBJ=   -6584.38385164894     
 iteration         -353 MCMCOBJ=   -6616.62140596232     
 iteration         -352 MCMCOBJ=   -6610.30817000972     
 iteration         -351 MCMCOBJ=   -6561.25497888172     
 iteration         -350 MCMCOBJ=   -6548.77392509332     
 iteration         -349 MCMCOBJ=   -6590.62831070187     
 iteration         -348 MCMCOBJ=   -6591.48369059338     
 iteration         -347 MCMCOBJ=   -6603.45163658946     
 iteration         -346 MCMCOBJ=   -6604.34704561476     
 iteration         -345 MCMCOBJ=   -6610.70515284190     
 iteration         -344 MCMCOBJ=   -6598.71058654510     
 iteration         -343 MCMCOBJ=   -6605.06032122155     
 iteration         -342 MCMCOBJ=   -6619.15243175006     
 iteration         -341 MCMCOBJ=   -6633.20302627289     
 iteration         -340 MCMCOBJ=   -6575.43175901331     
 iteration         -339 MCMCOBJ=   -6568.08365019577     
 iteration         -338 MCMCOBJ=   -6594.94055775522     
 iteration         -337 MCMCOBJ=   -6592.12974582408     
 iteration         -336 MCMCOBJ=   -6583.63810079551     
 iteration         -335 MCMCOBJ=   -6577.56015612904     
 iteration         -334 MCMCOBJ=   -6655.52981963798     
 iteration         -333 MCMCOBJ=   -6637.07206592331     
 iteration         -332 MCMCOBJ=   -6634.85500437659     
 iteration         -331 MCMCOBJ=   -6668.08994887192     
 iteration         -330 MCMCOBJ=   -6681.11293949804     
 iteration         -329 MCMCOBJ=   -6672.62526775649     
 iteration         -328 MCMCOBJ=   -6649.25453503451     
 iteration         -327 MCMCOBJ=   -6678.30788960935     
 iteration         -326 MCMCOBJ=   -6670.52776311646     
 iteration         -325 MCMCOBJ=   -6665.88741999621     
 iteration         -324 MCMCOBJ=   -6652.77150641483     
 iteration         -323 MCMCOBJ=   -6658.96784634007     
 iteration         -322 MCMCOBJ=   -6654.89423980021     
 iteration         -321 MCMCOBJ=   -6666.69852646442     
 iteration         -320 MCMCOBJ=   -6626.32436740914     
 iteration         -319 MCMCOBJ=   -6593.92367162898     
 iteration         -318 MCMCOBJ=   -6599.36382740923     
 iteration         -317 MCMCOBJ=   -6573.00749110892     
 iteration         -316 MCMCOBJ=   -6561.98447387078     
 iteration         -315 MCMCOBJ=   -6606.01475466475     
 iteration         -314 MCMCOBJ=   -6643.01595492630     
 iteration         -313 MCMCOBJ=   -6656.01709622000     
 iteration         -312 MCMCOBJ=   -6607.64103137049     
 iteration         -311 MCMCOBJ=   -6583.22038430842     
 iteration         -310 MCMCOBJ=   -6538.22087828252     
 iteration         -309 MCMCOBJ=   -6583.00631546048     
 iteration         -308 MCMCOBJ=   -6601.75869961451     
 iteration         -307 MCMCOBJ=   -6624.00648106834     
 iteration         -306 MCMCOBJ=   -6607.60382405344     
 iteration         -305 MCMCOBJ=   -6642.39263273443     
 iteration         -304 MCMCOBJ=   -6602.39159712561     
 iteration         -303 MCMCOBJ=   -6591.24322651769     
 iteration         -302 MCMCOBJ=   -6604.10427159414     
 iteration         -301 MCMCOBJ=   -6634.88377687324     
 iteration         -300 MCMCOBJ=   -6616.84883847264     
 iteration         -299 MCMCOBJ=   -6596.93941755888     
 iteration         -298 MCMCOBJ=   -6606.14882166628     
 iteration         -297 MCMCOBJ=   -6594.89967082047     
 iteration         -296 MCMCOBJ=   -6544.17821057820     
 iteration         -295 MCMCOBJ=   -6551.33068282654     
 iteration         -294 MCMCOBJ=   -6584.64745164543     
 iteration         -293 MCMCOBJ=   -6625.07447766840     
 iteration         -292 MCMCOBJ=   -6603.03446101463     
 iteration         -291 MCMCOBJ=   -6587.78206053004     
 iteration         -290 MCMCOBJ=   -6672.17664508883     
 iteration         -289 MCMCOBJ=   -6683.15422726120     
 iteration         -288 MCMCOBJ=   -6683.15422233095     
 iteration         -287 MCMCOBJ=   -6591.73717950226     
 iteration         -286 MCMCOBJ=   -6574.49384985714     
 iteration         -285 MCMCOBJ=   -6593.35282298641     
 iteration         -284 MCMCOBJ=   -6545.04128022155     
 iteration         -283 MCMCOBJ=   -6592.31629974346     
 iteration         -282 MCMCOBJ=   -6602.50011844167     
 iteration         -281 MCMCOBJ=   -6547.09687225802     
 iteration         -280 MCMCOBJ=   -6538.20239711183     
 iteration         -279 MCMCOBJ=   -6541.05986720279     
 iteration         -278 MCMCOBJ=   -6545.30567169442     
 iteration         -277 MCMCOBJ=   -6645.09134847659     
 iteration         -276 MCMCOBJ=   -6614.05100924917     
 iteration         -275 MCMCOBJ=   -6630.34722758402     
 iteration         -274 MCMCOBJ=   -6698.06541522823     
 iteration         -273 MCMCOBJ=   -6632.61592389839     
 iteration         -272 MCMCOBJ=   -6541.95957987876     
 iteration         -271 MCMCOBJ=   -6569.46506480013     
 iteration         -270 MCMCOBJ=   -6586.76842001228     
 iteration         -269 MCMCOBJ=   -6638.98986569911     
 iteration         -268 MCMCOBJ=   -6560.73211192711     
 iteration         -267 MCMCOBJ=   -6622.53861048813     
 iteration         -266 MCMCOBJ=   -6554.22565803856     
 iteration         -265 MCMCOBJ=   -6551.46818890180     
 iteration         -264 MCMCOBJ=   -6619.82021172412     
 iteration         -263 MCMCOBJ=   -6603.18769688188     
 iteration         -262 MCMCOBJ=   -6574.56329610937     
 iteration         -261 MCMCOBJ=   -6561.67166527386     
 iteration         -260 MCMCOBJ=   -6583.40735077264     
 iteration         -259 MCMCOBJ=   -6591.92004483895     
 iteration         -258 MCMCOBJ=   -6604.99624328743     
 iteration         -257 MCMCOBJ=   -6600.26799368333     
 iteration         -256 MCMCOBJ=   -6581.27900435910     
 iteration         -255 MCMCOBJ=   -6592.53029076134     
 iteration         -254 MCMCOBJ=   -6590.20076852665     
 iteration         -253 MCMCOBJ=   -6586.53894013389     
 iteration         -252 MCMCOBJ=   -6619.18254357385     
 iteration         -251 MCMCOBJ=   -6624.85945203000     
 iteration         -250 MCMCOBJ=   -6633.23798100642     
 iteration         -249 MCMCOBJ=   -6593.86516371833     
 iteration         -248 MCMCOBJ=   -6577.41972442346     
 iteration         -247 MCMCOBJ=   -6538.06091117455     
 iteration         -246 MCMCOBJ=   -6552.95000057393     
 iteration         -245 MCMCOBJ=   -6547.83979963085     
 iteration         -244 MCMCOBJ=   -6570.40777403307     
 iteration         -243 MCMCOBJ=   -6566.81914292951     
 iteration         -242 MCMCOBJ=   -6546.91697469961     
 iteration         -241 MCMCOBJ=   -6558.40973845880     
 iteration         -240 MCMCOBJ=   -6590.88955500202     
 iteration         -239 MCMCOBJ=   -6617.56587248122     
 iteration         -238 MCMCOBJ=   -6556.09778577942     
 iteration         -237 MCMCOBJ=   -6533.38928095898     
 iteration         -236 MCMCOBJ=   -6638.88014647555     
 iteration         -235 MCMCOBJ=   -6638.88015113149     
 iteration         -234 MCMCOBJ=   -6638.88014744051     
 iteration         -233 MCMCOBJ=   -6651.45885060199     
 iteration         -232 MCMCOBJ=   -6613.11319885060     
 iteration         -231 MCMCOBJ=   -6595.93707411128     
 iteration         -230 MCMCOBJ=   -6616.13338285319     
 iteration         -229 MCMCOBJ=   -6638.41745841219     
 iteration         -228 MCMCOBJ=   -6635.48519061629     
 iteration         -227 MCMCOBJ=   -6606.26805552699     
 iteration         -226 MCMCOBJ=   -6601.97692461447     
 iteration         -225 MCMCOBJ=   -6634.73907001210     
 iteration         -224 MCMCOBJ=   -6597.78563670323     
 iteration         -223 MCMCOBJ=   -6607.93056392688     
 iteration         -222 MCMCOBJ=   -6615.83732486738     
 iteration         -221 MCMCOBJ=   -6632.76622058565     
 iteration         -220 MCMCOBJ=   -6641.96955870537     
 iteration         -219 MCMCOBJ=   -6607.08328141384     
 iteration         -218 MCMCOBJ=   -6601.08106542049     
 iteration         -217 MCMCOBJ=   -6574.40331188090     
 iteration         -216 MCMCOBJ=   -6526.82387486102     
 iteration         -215 MCMCOBJ=   -6543.46913268628     
 iteration         -214 MCMCOBJ=   -6573.81635586973     
 iteration         -213 MCMCOBJ=   -6608.74883636876     
 iteration         -212 MCMCOBJ=   -6637.59614351509     
 iteration         -211 MCMCOBJ=   -6586.41773875453     
 iteration         -210 MCMCOBJ=   -6581.77208473929     
 iteration         -209 MCMCOBJ=   -6603.84287539360     
 iteration         -208 MCMCOBJ=   -6598.88588235668     
 iteration         -207 MCMCOBJ=   -6546.89427605510     
 iteration         -206 MCMCOBJ=   -6526.83078969346     
 iteration         -205 MCMCOBJ=   -6575.49887868521     
 iteration         -204 MCMCOBJ=   -6578.53318472509     
 iteration         -203 MCMCOBJ=   -6572.64962956762     
 iteration         -202 MCMCOBJ=   -6576.95364818108     
 iteration         -201 MCMCOBJ=   -6585.42720936055     
 iteration         -200 MCMCOBJ=   -6576.72936958297     
 iteration         -199 MCMCOBJ=   -6600.44272233951     
 iteration         -198 MCMCOBJ=   -6577.83406774767     
 iteration         -197 MCMCOBJ=   -6584.71023608963     
 iteration         -196 MCMCOBJ=   -6627.37249235760     
 iteration         -195 MCMCOBJ=   -6610.62175099912     
 iteration         -194 MCMCOBJ=   -6613.69220891018     
 iteration         -193 MCMCOBJ=   -6564.77171322189     
 iteration         -192 MCMCOBJ=   -6564.77171254243     
 iteration         -191 MCMCOBJ=   -6595.45502386612     
 iteration         -190 MCMCOBJ=   -6607.71551197513     
 iteration         -189 MCMCOBJ=   -6646.14079781535     
 iteration         -188 MCMCOBJ=   -6593.26128524392     
 iteration         -187 MCMCOBJ=   -6632.61288959460     
 iteration         -186 MCMCOBJ=   -6609.28902936475     
 iteration         -185 MCMCOBJ=   -6568.84363873203     
 iteration         -184 MCMCOBJ=   -6586.96324558507     
 iteration         -183 MCMCOBJ=   -6588.65896143507     
 iteration         -182 MCMCOBJ=   -6619.41382519912     
 iteration         -181 MCMCOBJ=   -6639.64307659177     
 iteration         -180 MCMCOBJ=   -6582.61217913441     
 iteration         -179 MCMCOBJ=   -6580.05657070807     
 iteration         -178 MCMCOBJ=   -6592.69005677903     
 iteration         -177 MCMCOBJ=   -6635.29985537708     
 iteration         -176 MCMCOBJ=   -6635.29985522989     
 iteration         -175 MCMCOBJ=   -6603.65101523146     
 iteration         -174 MCMCOBJ=   -6570.11382308630     
 iteration         -173 MCMCOBJ=   -6569.20555308858     
 iteration         -172 MCMCOBJ=   -6583.03552453236     
 iteration         -171 MCMCOBJ=   -6580.42389450805     
 iteration         -170 MCMCOBJ=   -6578.27664801896     
 iteration         -169 MCMCOBJ=   -6653.40407018062     
 iteration         -168 MCMCOBJ=   -6630.42616159006     
 iteration         -167 MCMCOBJ=   -6638.01962472405     
 iteration         -166 MCMCOBJ=   -6645.24895893549     
 iteration         -165 MCMCOBJ=   -6626.17256306211     
 iteration         -164 MCMCOBJ=   -6629.36531196641     
 iteration         -163 MCMCOBJ=   -6573.49399969548     
 iteration         -162 MCMCOBJ=   -6599.53884481701     
 iteration         -161 MCMCOBJ=   -6635.77750940150     
 iteration         -160 MCMCOBJ=   -6616.19973570901     
 iteration         -159 MCMCOBJ=   -6615.98478742912     
 iteration         -158 MCMCOBJ=   -6600.61137693446     
 iteration         -157 MCMCOBJ=   -6582.60193637354     
 iteration         -156 MCMCOBJ=   -6576.20684003371     
 iteration         -155 MCMCOBJ=   -6577.46834188928     
 iteration         -154 MCMCOBJ=   -6538.01667123532     
 iteration         -153 MCMCOBJ=   -6563.45580606494     
 iteration         -152 MCMCOBJ=   -6606.45717484965     
 iteration         -151 MCMCOBJ=   -6601.84284902478     
 iteration         -150 MCMCOBJ=   -6575.39419294892     
 iteration         -149 MCMCOBJ=   -6563.05604977170     
 iteration         -148 MCMCOBJ=   -6591.44719343909     
 iteration         -147 MCMCOBJ=   -6581.30147394206     
 iteration         -146 MCMCOBJ=   -6583.13136331363     
 iteration         -145 MCMCOBJ=   -6625.39789617354     
 iteration         -144 MCMCOBJ=   -6585.11903107818     
 iteration         -143 MCMCOBJ=   -6593.91407175107     
 iteration         -142 MCMCOBJ=   -6633.16866347128     
 iteration         -141 MCMCOBJ=   -6584.47534327587     
 iteration         -140 MCMCOBJ=   -6556.91286639740     
 iteration         -139 MCMCOBJ=   -6528.25793037423     
 iteration         -138 MCMCOBJ=   -6551.82752127259     
 iteration         -137 MCMCOBJ=   -6571.88091003239     
 iteration         -136 MCMCOBJ=   -6557.58583922221     
 iteration         -135 MCMCOBJ=   -6549.21232872894     
 iteration         -134 MCMCOBJ=   -6528.14108825614     
 iteration         -133 MCMCOBJ=   -6528.14108734409     
 iteration         -132 MCMCOBJ=   -6561.34881773759     
 iteration         -131 MCMCOBJ=   -6573.55294336196     
 iteration         -130 MCMCOBJ=   -6573.55294366367     
 iteration         -129 MCMCOBJ=   -6586.25283873839     
 iteration         -128 MCMCOBJ=   -6564.25754366616     
 iteration         -127 MCMCOBJ=   -6610.23299205695     
 iteration         -126 MCMCOBJ=   -6590.62627103313     
 iteration         -125 MCMCOBJ=   -6546.09435160760     
 iteration         -124 MCMCOBJ=   -6549.04045558886     
 iteration         -123 MCMCOBJ=   -6597.36365374887     
 iteration         -122 MCMCOBJ=   -6638.90423112666     
 iteration         -121 MCMCOBJ=   -6661.58948406786     
 iteration         -120 MCMCOBJ=   -6664.57081997014     
 iteration         -119 MCMCOBJ=   -6653.33124901659     
 iteration         -118 MCMCOBJ=   -6646.31784764348     
 iteration         -117 MCMCOBJ=   -6579.62294146958     
 iteration         -116 MCMCOBJ=   -6626.04770597450     
 iteration         -115 MCMCOBJ=   -6557.34898768622     
 iteration         -114 MCMCOBJ=   -6561.30688695268     
 iteration         -113 MCMCOBJ=   -6592.65350424644     
 iteration         -112 MCMCOBJ=   -6581.60779684931     
 iteration         -111 MCMCOBJ=   -6622.10185552735     
 iteration         -110 MCMCOBJ=   -6616.60582251125     
 iteration         -109 MCMCOBJ=   -6619.87189014980     
 iteration         -108 MCMCOBJ=   -6572.05912097385     
 iteration         -107 MCMCOBJ=   -6613.35184612013     
 iteration         -106 MCMCOBJ=   -6555.30935108761     
 iteration         -105 MCMCOBJ=   -6587.46789724164     
 iteration         -104 MCMCOBJ=   -6605.24177284306     
 iteration         -103 MCMCOBJ=   -6595.56330131402     
 iteration         -102 MCMCOBJ=   -6614.92174959355     
 iteration         -101 MCMCOBJ=   -6634.02682634564     
 iteration         -100 MCMCOBJ=   -6618.53172348294     
 iteration          -99 MCMCOBJ=   -6594.61491443556     
 iteration          -98 MCMCOBJ=   -6602.86021240868     
 iteration          -97 MCMCOBJ=   -6576.50579129760     
 iteration          -96 MCMCOBJ=   -6560.81596496158     
 iteration          -95 MCMCOBJ=   -6597.74867339123     
 iteration          -94 MCMCOBJ=   -6605.48208041239     
 iteration          -93 MCMCOBJ=   -6629.47437779955     
 iteration          -92 MCMCOBJ=   -6577.61453071401     
 iteration          -91 MCMCOBJ=   -6585.92751310933     
 iteration          -90 MCMCOBJ=   -6533.78026526349     
 iteration          -89 MCMCOBJ=   -6595.28810845970     
 iteration          -88 MCMCOBJ=   -6658.64225968462     
 iteration          -87 MCMCOBJ=   -6622.45366976708     
 iteration          -86 MCMCOBJ=   -6610.82533473280     
 iteration          -85 MCMCOBJ=   -6621.06166182613     
 iteration          -84 MCMCOBJ=   -6608.35798885146     
 iteration          -83 MCMCOBJ=   -6651.76149584781     
 iteration          -82 MCMCOBJ=   -6655.06629511471     
 iteration          -81 MCMCOBJ=   -6636.45147519333     
 iteration          -80 MCMCOBJ=   -6649.44030978428     
 iteration          -79 MCMCOBJ=   -6573.16038700266     
 iteration          -78 MCMCOBJ=   -6602.07540145005     
 iteration          -77 MCMCOBJ=   -6610.06147559864     
 iteration          -76 MCMCOBJ=   -6650.71360503658     
 iteration          -75 MCMCOBJ=   -6628.77315061685     
 iteration          -74 MCMCOBJ=   -6638.52986777739     
 iteration          -73 MCMCOBJ=   -6671.88341564294     
 iteration          -72 MCMCOBJ=   -6667.07573141404     
 iteration          -71 MCMCOBJ=   -6630.19277249317     
 iteration          -70 MCMCOBJ=   -6641.78727759138     
 iteration          -69 MCMCOBJ=   -6582.25008221117     
 iteration          -68 MCMCOBJ=   -6628.36237311337     
 iteration          -67 MCMCOBJ=   -6649.27494717685     
 iteration          -66 MCMCOBJ=   -6649.27494383331     
 iteration          -65 MCMCOBJ=   -6576.64979724584     
 iteration          -64 MCMCOBJ=   -6581.94709969420     
 iteration          -63 MCMCOBJ=   -6580.23165536415     
 iteration          -62 MCMCOBJ=   -6590.09749607153     
 iteration          -61 MCMCOBJ=   -6610.73327467934     
 iteration          -60 MCMCOBJ=   -6579.37331683136     
 iteration          -59 MCMCOBJ=   -6623.73850550940     
 iteration          -58 MCMCOBJ=   -6631.04057865650     
 iteration          -57 MCMCOBJ=   -6658.79023786688     
 iteration          -56 MCMCOBJ=   -6614.06851961159     
 iteration          -55 MCMCOBJ=   -6604.50238990202     
 iteration          -54 MCMCOBJ=   -6555.52417235075     
 iteration          -53 MCMCOBJ=   -6615.40213019898     
 iteration          -52 MCMCOBJ=   -6602.55976137945     
 iteration          -51 MCMCOBJ=   -6577.35078035612     
 iteration          -50 MCMCOBJ=   -6574.10457676111     
 iteration          -49 MCMCOBJ=   -6593.21249654963     
 iteration          -48 MCMCOBJ=   -6570.51325815144     
 iteration          -47 MCMCOBJ=   -6581.13169415442     
 iteration          -46 MCMCOBJ=   -6577.49996513915     
 iteration          -45 MCMCOBJ=   -6551.36137161708     
 iteration          -44 MCMCOBJ=   -6566.27243108596     
 iteration          -43 MCMCOBJ=   -6555.50468467831     
 iteration          -42 MCMCOBJ=   -6563.60233171299     
 iteration          -41 MCMCOBJ=   -6630.02353536117     
 iteration          -40 MCMCOBJ=   -6630.02353511885     
 iteration          -39 MCMCOBJ=   -6624.60749873012     
 iteration          -38 MCMCOBJ=   -6612.09535612257     
 iteration          -37 MCMCOBJ=   -6611.25116333663     
 iteration          -36 MCMCOBJ=   -6636.63924478229     
 iteration          -35 MCMCOBJ=   -6616.81142794403     
 iteration          -34 MCMCOBJ=   -6567.55671738787     
 iteration          -33 MCMCOBJ=   -6583.76564485118     
 iteration          -32 MCMCOBJ=   -6616.88440866910     
 iteration          -31 MCMCOBJ=   -6589.24609200912     
 iteration          -30 MCMCOBJ=   -6609.51702093158     
 iteration          -29 MCMCOBJ=   -6654.29714393629     
 iteration          -28 MCMCOBJ=   -6675.33026528779     
 iteration          -27 MCMCOBJ=   -6614.79425826260     
 iteration          -26 MCMCOBJ=   -6618.45925757071     
 iteration          -25 MCMCOBJ=   -6603.20824743799     
 iteration          -24 MCMCOBJ=   -6620.14851612887     
 iteration          -23 MCMCOBJ=   -6623.47052549933     
 iteration          -22 MCMCOBJ=   -6582.84001841388     
 iteration          -21 MCMCOBJ=   -6593.92632257953     
 iteration          -20 MCMCOBJ=   -6593.92630932303     
 iteration          -19 MCMCOBJ=   -6598.24835290518     
 iteration          -18 MCMCOBJ=   -6597.66772791854     
 iteration          -17 MCMCOBJ=   -6537.60351533807     
 iteration          -16 MCMCOBJ=   -6543.40168087005     
 iteration          -15 MCMCOBJ=   -6618.35471103344     
 iteration          -14 MCMCOBJ=   -6622.02775407071     
 iteration          -13 MCMCOBJ=   -6639.20213223615     
 iteration          -12 MCMCOBJ=   -6590.31195520811     
 iteration          -11 MCMCOBJ=   -6632.67050170889     
 iteration          -10 MCMCOBJ=   -6633.80468166164     
 iteration           -9 MCMCOBJ=   -6579.97934953878     
 iteration           -8 MCMCOBJ=   -6588.84498589369     
 iteration           -7 MCMCOBJ=   -6587.91852961565     
 iteration           -6 MCMCOBJ=   -6536.90019948029     
 iteration           -5 MCMCOBJ=   -6617.14205079692     
 iteration           -4 MCMCOBJ=   -6613.66027923910     
 iteration           -3 MCMCOBJ=   -6561.21498438034     
 iteration           -2 MCMCOBJ=   -6569.19700185794     
 iteration           -1 MCMCOBJ=   -6637.98901127757     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6623.20139063176     
 iteration            1 MCMCOBJ=   -6581.09597391189     
 iteration            2 MCMCOBJ=   -6609.76824305301     
 iteration            3 MCMCOBJ=   -6607.60500668229     
 iteration            4 MCMCOBJ=   -6653.54747828795     
 iteration            5 MCMCOBJ=   -6648.49471760987     
 iteration            6 MCMCOBJ=   -6634.84070320583     
 iteration            7 MCMCOBJ=   -6633.76561559552     
 iteration            8 MCMCOBJ=   -6630.10817353281     
 iteration            9 MCMCOBJ=   -6610.56787193875     
 iteration           10 MCMCOBJ=   -6623.56902826723     
 iteration           11 MCMCOBJ=   -6646.21821300342     
 iteration           12 MCMCOBJ=   -6611.95548261977     
 iteration           13 MCMCOBJ=   -6653.14261365052     
 iteration           14 MCMCOBJ=   -6653.17580561058     
 iteration           15 MCMCOBJ=   -6640.21015998768     
 iteration           16 MCMCOBJ=   -6623.01324410361     
 iteration           17 MCMCOBJ=   -6631.10984133077     
 iteration           18 MCMCOBJ=   -6649.87910023501     
 iteration           19 MCMCOBJ=   -6616.63640941548     
 iteration           20 MCMCOBJ=   -6624.31068948892     
 iteration           21 MCMCOBJ=   -6617.50975972208     
 iteration           22 MCMCOBJ=   -6638.15679020973     
 iteration           23 MCMCOBJ=   -6576.50373195113     
 iteration           24 MCMCOBJ=   -6588.21609188378     
 iteration           25 MCMCOBJ=   -6628.28456285719     
 iteration           26 MCMCOBJ=   -6637.18300451579     
 iteration           27 MCMCOBJ=   -6620.27859320370     
 iteration           28 MCMCOBJ=   -6601.10693469625     
 iteration           29 MCMCOBJ=   -6581.38693264887     
 iteration           30 MCMCOBJ=   -6614.48952018518     
 iteration           31 MCMCOBJ=   -6611.86536599683     
 iteration           32 MCMCOBJ=   -6601.96912427800     
 iteration           33 MCMCOBJ=   -6618.50968190675     
 iteration           34 MCMCOBJ=   -6625.95906921023     
 iteration           35 MCMCOBJ=   -6622.31790644380     
 iteration           36 MCMCOBJ=   -6582.43986106896     
 iteration           37 MCMCOBJ=   -6592.50898411131     
 iteration           38 MCMCOBJ=   -6584.79719183641     
 iteration           39 MCMCOBJ=   -6569.61357060938     
 iteration           40 MCMCOBJ=   -6540.89172157252     
 iteration           41 MCMCOBJ=   -6538.38622895013     
 iteration           42 MCMCOBJ=   -6626.07259442659     
 iteration           43 MCMCOBJ=   -6596.11217901822     
 iteration           44 MCMCOBJ=   -6613.43665082979     
 iteration           45 MCMCOBJ=   -6624.65894707965     
 iteration           46 MCMCOBJ=   -6648.34202797439     
 iteration           47 MCMCOBJ=   -6672.20717214047     
 iteration           48 MCMCOBJ=   -6647.91712347164     
 iteration           49 MCMCOBJ=   -6594.81518405627     
 iteration           50 MCMCOBJ=   -6591.63678220702     
 iteration           51 MCMCOBJ=   -6606.93022362712     
 iteration           52 MCMCOBJ=   -6599.76746688446     
 iteration           53 MCMCOBJ=   -6665.23331589438     
 iteration           54 MCMCOBJ=   -6679.67515131854     
 iteration           55 MCMCOBJ=   -6684.31247341369     
 iteration           56 MCMCOBJ=   -6682.05595027688     
 iteration           57 MCMCOBJ=   -6682.74617439783     
 iteration           58 MCMCOBJ=   -6696.67882169010     
 iteration           59 MCMCOBJ=   -6651.25978342810     
 iteration           60 MCMCOBJ=   -6669.05007605995     
 iteration           61 MCMCOBJ=   -6662.19279733247     
 iteration           62 MCMCOBJ=   -6637.13233297304     
 iteration           63 MCMCOBJ=   -6597.64443585862     
 iteration           64 MCMCOBJ=   -6569.26452651339     
 iteration           65 MCMCOBJ=   -6590.18681441548     
 iteration           66 MCMCOBJ=   -6556.11550067712     
 iteration           67 MCMCOBJ=   -6626.48362308081     
 iteration           68 MCMCOBJ=   -6609.56843288545     
 iteration           69 MCMCOBJ=   -6609.28154244248     
 iteration           70 MCMCOBJ=   -6609.10718456270     
 iteration           71 MCMCOBJ=   -6603.07822651526     
 iteration           72 MCMCOBJ=   -6558.44389852698     
 iteration           73 MCMCOBJ=   -6603.35769634831     
 iteration           74 MCMCOBJ=   -6573.70014935993     
 iteration           75 MCMCOBJ=   -6568.97400608724     
 iteration           76 MCMCOBJ=   -6568.97399363856     
 iteration           77 MCMCOBJ=   -6566.33398800642     
 iteration           78 MCMCOBJ=   -6552.85497995310     
 iteration           79 MCMCOBJ=   -6559.40289469206     
 iteration           80 MCMCOBJ=   -6579.61582857766     
 iteration           81 MCMCOBJ=   -6582.51840008273     
 iteration           82 MCMCOBJ=   -6583.69924142610     
 iteration           83 MCMCOBJ=   -6588.79013916451     
 iteration           84 MCMCOBJ=   -6644.59972381547     
 iteration           85 MCMCOBJ=   -6596.02506769495     
 iteration           86 MCMCOBJ=   -6609.27151859500     
 iteration           87 MCMCOBJ=   -6569.18321975890     
 iteration           88 MCMCOBJ=   -6608.06056157373     
 iteration           89 MCMCOBJ=   -6606.03493608604     
 iteration           90 MCMCOBJ=   -6669.47796982742     
 iteration           91 MCMCOBJ=   -6575.51592464100     
 iteration           92 MCMCOBJ=   -6548.35059158164     
 iteration           93 MCMCOBJ=   -6479.46988418702     
 iteration           94 MCMCOBJ=   -6530.78667826836     
 iteration           95 MCMCOBJ=   -6537.30881220291     
 iteration           96 MCMCOBJ=   -6630.37949249568     
 iteration           97 MCMCOBJ=   -6645.60257605740     
 iteration           98 MCMCOBJ=   -6636.92033918896     
 iteration           99 MCMCOBJ=   -6669.84039449018     
 iteration          100 MCMCOBJ=   -6646.94042694788     
 iteration          101 MCMCOBJ=   -6634.55879919710     
 iteration          102 MCMCOBJ=   -6619.75778740860     
 iteration          103 MCMCOBJ=   -6623.87596020183     
 iteration          104 MCMCOBJ=   -6643.54934080961     
 iteration          105 MCMCOBJ=   -6617.56357077641     
 iteration          106 MCMCOBJ=   -6624.42392487569     
 iteration          107 MCMCOBJ=   -6635.03389070333     
 iteration          108 MCMCOBJ=   -6623.75311014021     
 iteration          109 MCMCOBJ=   -6668.15755958153     
 iteration          110 MCMCOBJ=   -6662.12192649596     
 iteration          111 MCMCOBJ=   -6661.65330672858     
 iteration          112 MCMCOBJ=   -6676.04216700096     
 iteration          113 MCMCOBJ=   -6677.02239986847     
 iteration          114 MCMCOBJ=   -6668.57012617018     
 iteration          115 MCMCOBJ=   -6640.84959252517     
 iteration          116 MCMCOBJ=   -6667.62894533587     
 iteration          117 MCMCOBJ=   -6650.92321070169     
 iteration          118 MCMCOBJ=   -6611.85391954571     
 iteration          119 MCMCOBJ=   -6662.81154814026     
 iteration          120 MCMCOBJ=   -6667.91927281695     
 iteration          121 MCMCOBJ=   -6642.78035060147     
 iteration          122 MCMCOBJ=   -6616.10053785527     
 iteration          123 MCMCOBJ=   -6560.79462624294     
 iteration          124 MCMCOBJ=   -6612.65015111186     
 iteration          125 MCMCOBJ=   -6614.38797922487     
 iteration          126 MCMCOBJ=   -6653.37369106964     
 iteration          127 MCMCOBJ=   -6655.03583144992     
 iteration          128 MCMCOBJ=   -6659.76906694672     
 iteration          129 MCMCOBJ=   -6663.35055009886     
 iteration          130 MCMCOBJ=   -6656.54664238933     
 iteration          131 MCMCOBJ=   -6603.83300961980     
 iteration          132 MCMCOBJ=   -6561.75587858955     
 iteration          133 MCMCOBJ=   -6585.70804793194     
 iteration          134 MCMCOBJ=   -6618.65754260688     
 iteration          135 MCMCOBJ=   -6631.84477148232     
 iteration          136 MCMCOBJ=   -6648.46056199936     
 iteration          137 MCMCOBJ=   -6632.58578800749     
 iteration          138 MCMCOBJ=   -6640.62368157539     
 iteration          139 MCMCOBJ=   -6652.59618086094     
 iteration          140 MCMCOBJ=   -6638.41802945916     
 iteration          141 MCMCOBJ=   -6638.44296695537     
 iteration          142 MCMCOBJ=   -6599.43423523289     
 iteration          143 MCMCOBJ=   -6603.42618523584     
 iteration          144 MCMCOBJ=   -6642.02394431395     
 iteration          145 MCMCOBJ=   -6588.36336623961     
 iteration          146 MCMCOBJ=   -6601.39855157972     
 iteration          147 MCMCOBJ=   -6584.64770550070     
 iteration          148 MCMCOBJ=   -6613.61169630309     
 iteration          149 MCMCOBJ=   -6589.86703058702     
 iteration          150 MCMCOBJ=   -6634.72972241950     
 iteration          151 MCMCOBJ=   -6631.82734210073     
 iteration          152 MCMCOBJ=   -6659.93284709468     
 iteration          153 MCMCOBJ=   -6630.43667815238     
 iteration          154 MCMCOBJ=   -6602.69420039152     
 iteration          155 MCMCOBJ=   -6602.02288809961     
 iteration          156 MCMCOBJ=   -6594.51956200540     
 iteration          157 MCMCOBJ=   -6577.57396197832     
 iteration          158 MCMCOBJ=   -6591.30763018678     
 iteration          159 MCMCOBJ=   -6544.57327130558     
 iteration          160 MCMCOBJ=   -6601.36186832007     
 iteration          161 MCMCOBJ=   -6650.46922906750     
 iteration          162 MCMCOBJ=   -6617.21616815460     
 iteration          163 MCMCOBJ=   -6617.21616817536     
 iteration          164 MCMCOBJ=   -6554.43035855219     
 iteration          165 MCMCOBJ=   -6577.13199356416     
 iteration          166 MCMCOBJ=   -6656.91649082968     
 iteration          167 MCMCOBJ=   -6664.42841375493     
 iteration          168 MCMCOBJ=   -6636.37868458964     
 iteration          169 MCMCOBJ=   -6628.35214541043     
 iteration          170 MCMCOBJ=   -6599.12176618700     
 iteration          171 MCMCOBJ=   -6594.91193377873     
 iteration          172 MCMCOBJ=   -6661.11078459244     
 iteration          173 MCMCOBJ=   -6667.24866540120     
 iteration          174 MCMCOBJ=   -6546.50808184013     
 iteration          175 MCMCOBJ=   -6569.19098199571     
 iteration          176 MCMCOBJ=   -6574.58507352869     
 iteration          177 MCMCOBJ=   -6625.98103619594     
 iteration          178 MCMCOBJ=   -6625.42656460917     
 iteration          179 MCMCOBJ=   -6605.49137789313     
 iteration          180 MCMCOBJ=   -6559.74195684910     
 iteration          181 MCMCOBJ=   -6585.02911092188     
 iteration          182 MCMCOBJ=   -6567.14647067149     
 iteration          183 MCMCOBJ=   -6554.98556358275     
 iteration          184 MCMCOBJ=   -6589.05289789134     
 iteration          185 MCMCOBJ=   -6571.86566557332     
 iteration          186 MCMCOBJ=   -6578.89968895497     
 iteration          187 MCMCOBJ=   -6560.37368914944     
 iteration          188 MCMCOBJ=   -6533.48495793405     
 iteration          189 MCMCOBJ=   -6529.96221193658     
 iteration          190 MCMCOBJ=   -6587.18893650288     
 iteration          191 MCMCOBJ=   -6610.72910666758     
 iteration          192 MCMCOBJ=   -6599.99626874241     
 iteration          193 MCMCOBJ=   -6608.08870140203     
 iteration          194 MCMCOBJ=   -6567.96621857490     
 iteration          195 MCMCOBJ=   -6578.38107389115     
 iteration          196 MCMCOBJ=   -6627.63354300096     
 iteration          197 MCMCOBJ=   -6605.84205810576     
 iteration          198 MCMCOBJ=   -6595.36595877128     
 iteration          199 MCMCOBJ=   -6564.04775810580     
 iteration          200 MCMCOBJ=   -6574.32776217148     
 iteration          201 MCMCOBJ=   -6610.48623499717     
 iteration          202 MCMCOBJ=   -6620.82919458547     
 iteration          203 MCMCOBJ=   -6608.56745400953     
 iteration          204 MCMCOBJ=   -6585.86176915697     
 iteration          205 MCMCOBJ=   -6584.47989882212     
 iteration          206 MCMCOBJ=   -6552.71004746740     
 iteration          207 MCMCOBJ=   -6559.68606069537     
 iteration          208 MCMCOBJ=   -6547.09917665465     
 iteration          209 MCMCOBJ=   -6562.29079381063     
 iteration          210 MCMCOBJ=   -6609.88653261319     
 iteration          211 MCMCOBJ=   -6605.09339763494     
 iteration          212 MCMCOBJ=   -6592.65696722731     
 iteration          213 MCMCOBJ=   -6601.80315711120     
 iteration          214 MCMCOBJ=   -6591.58022418222     
 iteration          215 MCMCOBJ=   -6592.71638802542     
 iteration          216 MCMCOBJ=   -6623.36844771413     
 iteration          217 MCMCOBJ=   -6611.07605344821     
 iteration          218 MCMCOBJ=   -6643.02898558609     
 iteration          219 MCMCOBJ=   -6647.83127241826     
 iteration          220 MCMCOBJ=   -6580.37978340452     
 iteration          221 MCMCOBJ=   -6551.83140319068     
 iteration          222 MCMCOBJ=   -6578.32292975601     
 iteration          223 MCMCOBJ=   -6560.06715512738     
 iteration          224 MCMCOBJ=   -6582.25324259818     
 iteration          225 MCMCOBJ=   -6599.18821002857     
 iteration          226 MCMCOBJ=   -6601.54578613305     
 iteration          227 MCMCOBJ=   -6564.08436772134     
 iteration          228 MCMCOBJ=   -6604.74665498447     
 iteration          229 MCMCOBJ=   -6525.24930137531     
 iteration          230 MCMCOBJ=   -6560.10611684173     
 iteration          231 MCMCOBJ=   -6577.14628103494     
 iteration          232 MCMCOBJ=   -6541.86036975783     
 iteration          233 MCMCOBJ=   -6523.12038114376     
 iteration          234 MCMCOBJ=   -6543.45848095139     
 iteration          235 MCMCOBJ=   -6582.31449907595     
 iteration          236 MCMCOBJ=   -6584.32928622965     
 iteration          237 MCMCOBJ=   -6541.55268327314     
 iteration          238 MCMCOBJ=   -6533.37999762453     
 iteration          239 MCMCOBJ=   -6568.60136646581     
 iteration          240 MCMCOBJ=   -6602.19568895237     
 iteration          241 MCMCOBJ=   -6616.24923806165     
 iteration          242 MCMCOBJ=   -6596.35618096745     
 iteration          243 MCMCOBJ=   -6644.26249817210     
 iteration          244 MCMCOBJ=   -6595.05297938069     
 iteration          245 MCMCOBJ=   -6543.71422648346     
 iteration          246 MCMCOBJ=   -6553.15444495876     
 iteration          247 MCMCOBJ=   -6549.30023343416     
 iteration          248 MCMCOBJ=   -6539.98802152839     
 iteration          249 MCMCOBJ=   -6545.10727571057     
 iteration          250 MCMCOBJ=   -6499.84228415501     
 iteration          251 MCMCOBJ=   -6525.23720405402     
 iteration          252 MCMCOBJ=   -6542.59973010142     
 iteration          253 MCMCOBJ=   -6572.98699942561     
 iteration          254 MCMCOBJ=   -6553.79114112794     
 iteration          255 MCMCOBJ=   -6618.32970456154     
 iteration          256 MCMCOBJ=   -6687.21594434384     
 iteration          257 MCMCOBJ=   -6539.90444425230     
 iteration          258 MCMCOBJ=   -6564.92180893857     
 iteration          259 MCMCOBJ=   -6528.56636252326     
 iteration          260 MCMCOBJ=   -6524.22612171892     
 iteration          261 MCMCOBJ=   -6575.01171685003     
 iteration          262 MCMCOBJ=   -6565.92192807990     
 iteration          263 MCMCOBJ=   -6599.90930753919     
 iteration          264 MCMCOBJ=   -6615.62427545204     
 iteration          265 MCMCOBJ=   -6621.98025455574     
 iteration          266 MCMCOBJ=   -6605.24818951766     
 iteration          267 MCMCOBJ=   -6637.91453179775     
 iteration          268 MCMCOBJ=   -6647.53747224711     
 iteration          269 MCMCOBJ=   -6648.13642707748     
 iteration          270 MCMCOBJ=   -6630.22271701738     
 iteration          271 MCMCOBJ=   -6639.63908056166     
 iteration          272 MCMCOBJ=   -6565.30921869494     
 iteration          273 MCMCOBJ=   -6525.49162737432     
 iteration          274 MCMCOBJ=   -6547.08218253313     
 iteration          275 MCMCOBJ=   -6567.47450055379     
 iteration          276 MCMCOBJ=   -6585.04935418502     
 iteration          277 MCMCOBJ=   -6547.52867650822     
 iteration          278 MCMCOBJ=   -6559.93686695734     
 iteration          279 MCMCOBJ=   -6612.04379340626     
 iteration          280 MCMCOBJ=   -6626.80901706626     
 iteration          281 MCMCOBJ=   -6606.53626012196     
 iteration          282 MCMCOBJ=   -6623.52482407484     
 iteration          283 MCMCOBJ=   -6664.02485720854     
 iteration          284 MCMCOBJ=   -6651.05295568093     
 iteration          285 MCMCOBJ=   -6636.62045781040     
 iteration          286 MCMCOBJ=   -6640.61348064434     
 iteration          287 MCMCOBJ=   -6640.61348145056     
 iteration          288 MCMCOBJ=   -6622.56267296476     
 iteration          289 MCMCOBJ=   -6618.55140796198     
 iteration          290 MCMCOBJ=   -6595.47465709847     
 iteration          291 MCMCOBJ=   -6638.07690700720     
 iteration          292 MCMCOBJ=   -6580.09329663510     
 iteration          293 MCMCOBJ=   -6575.92770011399     
 iteration          294 MCMCOBJ=   -6584.94443888094     
 iteration          295 MCMCOBJ=   -6626.55800659068     
 iteration          296 MCMCOBJ=   -6594.17109636458     
 iteration          297 MCMCOBJ=   -6564.82112088465     
 iteration          298 MCMCOBJ=   -6553.90878813319     
 iteration          299 MCMCOBJ=   -6582.81114971665     
 iteration          300 MCMCOBJ=   -6546.42104886927     
 iteration          301 MCMCOBJ=   -6553.70979683605     
 iteration          302 MCMCOBJ=   -6553.27207768856     
 iteration          303 MCMCOBJ=   -6627.67108288173     
 iteration          304 MCMCOBJ=   -6639.34694283679     
 iteration          305 MCMCOBJ=   -6648.08959416807     
 iteration          306 MCMCOBJ=   -6649.86490904337     
 iteration          307 MCMCOBJ=   -6624.15696741122     
 iteration          308 MCMCOBJ=   -6607.25225852946     
 iteration          309 MCMCOBJ=   -6603.33250065112     
 iteration          310 MCMCOBJ=   -6564.67353776515     
 iteration          311 MCMCOBJ=   -6614.81105571457     
 iteration          312 MCMCOBJ=   -6591.16690389684     
 iteration          313 MCMCOBJ=   -6617.10418570309     
 iteration          314 MCMCOBJ=   -6636.10855897933     
 iteration          315 MCMCOBJ=   -6590.98725681205     
 iteration          316 MCMCOBJ=   -6572.82672934426     
 iteration          317 MCMCOBJ=   -6581.59873050003     
 iteration          318 MCMCOBJ=   -6610.98731914379     
 iteration          319 MCMCOBJ=   -6586.18663638986     
 iteration          320 MCMCOBJ=   -6600.02716838894     
 iteration          321 MCMCOBJ=   -6611.04811677182     
 iteration          322 MCMCOBJ=   -6614.85820218487     
 iteration          323 MCMCOBJ=   -6675.55838859478     
 iteration          324 MCMCOBJ=   -6649.97997071596     
 iteration          325 MCMCOBJ=   -6611.40047485143     
 iteration          326 MCMCOBJ=   -6636.96320512073     
 iteration          327 MCMCOBJ=   -6612.15764917757     
 iteration          328 MCMCOBJ=   -6611.52893536903     
 iteration          329 MCMCOBJ=   -6640.96417159166     
 iteration          330 MCMCOBJ=   -6642.27618982379     
 iteration          331 MCMCOBJ=   -6613.04356655201     
 iteration          332 MCMCOBJ=   -6670.26577002555     
 iteration          333 MCMCOBJ=   -6644.86641416891     
 iteration          334 MCMCOBJ=   -6635.96829500147     
 iteration          335 MCMCOBJ=   -6685.43367211413     
 iteration          336 MCMCOBJ=   -6656.72468765492     
 iteration          337 MCMCOBJ=   -6588.99637278400     
 iteration          338 MCMCOBJ=   -6584.99153213408     
 iteration          339 MCMCOBJ=   -6628.43903083748     
 iteration          340 MCMCOBJ=   -6630.77027635270     
 iteration          341 MCMCOBJ=   -6668.93302147427     
 iteration          342 MCMCOBJ=   -6652.82121620517     
 iteration          343 MCMCOBJ=   -6633.98882627871     
 iteration          344 MCMCOBJ=   -6626.28139566081     
 iteration          345 MCMCOBJ=   -6569.22563050741     
 iteration          346 MCMCOBJ=   -6623.31213114073     
 iteration          347 MCMCOBJ=   -6641.16376933249     
 iteration          348 MCMCOBJ=   -6590.09793801274     
 iteration          349 MCMCOBJ=   -6621.31900339756     
 iteration          350 MCMCOBJ=   -6575.26233233630     
 iteration          351 MCMCOBJ=   -6582.67731829858     
 iteration          352 MCMCOBJ=   -6626.05315878170     
 iteration          353 MCMCOBJ=   -6586.71093489905     
 iteration          354 MCMCOBJ=   -6636.98378288266     
 iteration          355 MCMCOBJ=   -6604.54657569316     
 iteration          356 MCMCOBJ=   -6628.23766996126     
 iteration          357 MCMCOBJ=   -6646.50192185147     
 iteration          358 MCMCOBJ=   -6655.81884292456     
 iteration          359 MCMCOBJ=   -6640.40983627647     
 iteration          360 MCMCOBJ=   -6575.39766996167     
 iteration          361 MCMCOBJ=   -6582.78966081618     
 iteration          362 MCMCOBJ=   -6570.96362528269     
 iteration          363 MCMCOBJ=   -6583.49547349360     
 iteration          364 MCMCOBJ=   -6557.30614719411     
 iteration          365 MCMCOBJ=   -6608.50596868184     
 iteration          366 MCMCOBJ=   -6572.00114620660     
 iteration          367 MCMCOBJ=   -6622.67711391835     
 iteration          368 MCMCOBJ=   -6634.80266912322     
 iteration          369 MCMCOBJ=   -6650.65905323732     
 iteration          370 MCMCOBJ=   -6608.19121329821     
 iteration          371 MCMCOBJ=   -6569.81951312409     
 iteration          372 MCMCOBJ=   -6569.90229236041     
 iteration          373 MCMCOBJ=   -6569.72995313884     
 iteration          374 MCMCOBJ=   -6564.68491087847     
 iteration          375 MCMCOBJ=   -6584.27098344595     
 iteration          376 MCMCOBJ=   -6580.25675063429     
 iteration          377 MCMCOBJ=   -6572.11077365153     
 iteration          378 MCMCOBJ=   -6542.73391190591     
 iteration          379 MCMCOBJ=   -6591.04484738895     
 iteration          380 MCMCOBJ=   -6591.04484676854     
 iteration          381 MCMCOBJ=   -6610.38354090695     
 iteration          382 MCMCOBJ=   -6573.89681943169     
 iteration          383 MCMCOBJ=   -6640.11113915978     
 iteration          384 MCMCOBJ=   -6586.38139095976     
 iteration          385 MCMCOBJ=   -6629.59945078820     
 iteration          386 MCMCOBJ=   -6609.69279399308     
 iteration          387 MCMCOBJ=   -6620.81690819167     
 iteration          388 MCMCOBJ=   -6636.34168017514     
 iteration          389 MCMCOBJ=   -6619.76149878384     
 iteration          390 MCMCOBJ=   -6619.76149958558     
 iteration          391 MCMCOBJ=   -6623.87510735582     
 iteration          392 MCMCOBJ=   -6608.97120954476     
 iteration          393 MCMCOBJ=   -6549.25314947790     
 iteration          394 MCMCOBJ=   -6606.17478706614     
 iteration          395 MCMCOBJ=   -6586.67562977681     
 iteration          396 MCMCOBJ=   -6602.59351588878     
 iteration          397 MCMCOBJ=   -6585.46194447596     
 iteration          398 MCMCOBJ=   -6622.60095888985     
 iteration          399 MCMCOBJ=   -6566.34857955713     
 iteration          400 MCMCOBJ=   -6583.59773613658     
 iteration          401 MCMCOBJ=   -6587.98002500164     
 iteration          402 MCMCOBJ=   -6604.38788168183     
 iteration          403 MCMCOBJ=   -6626.29970136033     
 iteration          404 MCMCOBJ=   -6646.86898764008     
 iteration          405 MCMCOBJ=   -6658.27725539721     
 iteration          406 MCMCOBJ=   -6642.53753210450     
 iteration          407 MCMCOBJ=   -6619.69561002318     
 iteration          408 MCMCOBJ=   -6624.90245950319     
 iteration          409 MCMCOBJ=   -6609.79925109871     
 iteration          410 MCMCOBJ=   -6571.31366370054     
 iteration          411 MCMCOBJ=   -6532.30597237124     
 iteration          412 MCMCOBJ=   -6604.54323525224     
 iteration          413 MCMCOBJ=   -6623.38521293295     
 iteration          414 MCMCOBJ=   -6566.37571707185     
 iteration          415 MCMCOBJ=   -6501.38646230665     
 iteration          416 MCMCOBJ=   -6559.09972492978     
 iteration          417 MCMCOBJ=   -6563.50180203180     
 iteration          418 MCMCOBJ=   -6531.95309268165     
 iteration          419 MCMCOBJ=   -6502.97491388902     
 iteration          420 MCMCOBJ=   -6463.05204519505     
 iteration          421 MCMCOBJ=   -6567.83736343702     
 iteration          422 MCMCOBJ=   -6576.74189701094     
 iteration          423 MCMCOBJ=   -6590.45622388732     
 iteration          424 MCMCOBJ=   -6576.14315402616     
 iteration          425 MCMCOBJ=   -6570.64519819817     
 iteration          426 MCMCOBJ=   -6598.48879066725     
 iteration          427 MCMCOBJ=   -6587.65542276110     
 iteration          428 MCMCOBJ=   -6607.59784676972     
 iteration          429 MCMCOBJ=   -6608.91752496006     
 iteration          430 MCMCOBJ=   -6606.39556650487     
 iteration          431 MCMCOBJ=   -6606.39556655980     
 iteration          432 MCMCOBJ=   -6560.69587472799     
 iteration          433 MCMCOBJ=   -6607.16501455339     
 iteration          434 MCMCOBJ=   -6546.16532810010     
 iteration          435 MCMCOBJ=   -6602.41763595806     
 iteration          436 MCMCOBJ=   -6614.67978811383     
 iteration          437 MCMCOBJ=   -6591.74514675860     
 iteration          438 MCMCOBJ=   -6647.01190978451     
 iteration          439 MCMCOBJ=   -6561.89810046657     
 iteration          440 MCMCOBJ=   -6629.21157205408     
 iteration          441 MCMCOBJ=   -6616.34907512052     
 iteration          442 MCMCOBJ=   -6611.88343485642     
 iteration          443 MCMCOBJ=   -6664.67940998813     
 iteration          444 MCMCOBJ=   -6574.16609076044     
 iteration          445 MCMCOBJ=   -6556.86132579461     
 iteration          446 MCMCOBJ=   -6573.46860493228     
 iteration          447 MCMCOBJ=   -6558.38807184374     
 iteration          448 MCMCOBJ=   -6599.43150064517     
 iteration          449 MCMCOBJ=   -6623.31567049429     
 iteration          450 MCMCOBJ=   -6686.49867130470     
 iteration          451 MCMCOBJ=   -6660.16608717551     
 iteration          452 MCMCOBJ=   -6654.38987946051     
 iteration          453 MCMCOBJ=   -6631.33256499274     
 iteration          454 MCMCOBJ=   -6556.97419901134     
 iteration          455 MCMCOBJ=   -6588.81445800201     
 iteration          456 MCMCOBJ=   -6547.82743141002     
 iteration          457 MCMCOBJ=   -6549.56512357993     
 iteration          458 MCMCOBJ=   -6572.79991643043     
 iteration          459 MCMCOBJ=   -6587.31493424669     
 iteration          460 MCMCOBJ=   -6580.23829679852     
 iteration          461 MCMCOBJ=   -6536.17145872581     
 iteration          462 MCMCOBJ=   -6557.90963145824     
 iteration          463 MCMCOBJ=   -6482.85383570434     
 iteration          464 MCMCOBJ=   -6580.46244600532     
 iteration          465 MCMCOBJ=   -6568.42060558673     
 iteration          466 MCMCOBJ=   -6535.98510194994     
 iteration          467 MCMCOBJ=   -6583.19021292197     
 iteration          468 MCMCOBJ=   -6585.18464215745     
 iteration          469 MCMCOBJ=   -6640.67170269956     
 iteration          470 MCMCOBJ=   -6632.91729676039     
 iteration          471 MCMCOBJ=   -6598.75580282844     
 iteration          472 MCMCOBJ=   -6590.38836646749     
 iteration          473 MCMCOBJ=   -6580.54439952954     
 iteration          474 MCMCOBJ=   -6618.02536600882     
 iteration          475 MCMCOBJ=   -6640.63967005003     
 iteration          476 MCMCOBJ=   -6585.24772463477     
 iteration          477 MCMCOBJ=   -6606.55509189378     
 iteration          478 MCMCOBJ=   -6583.93334226037     
 iteration          479 MCMCOBJ=   -6606.64501247103     
 iteration          480 MCMCOBJ=   -6607.21550134438     
 iteration          481 MCMCOBJ=   -6608.72897111138     
 iteration          482 MCMCOBJ=   -6655.80239083476     
 iteration          483 MCMCOBJ=   -6645.87411122048     
 iteration          484 MCMCOBJ=   -6645.87411128413     
 iteration          485 MCMCOBJ=   -6645.02062832419     
 iteration          486 MCMCOBJ=   -6641.14652303824     
 iteration          487 MCMCOBJ=   -6657.99442608143     
 iteration          488 MCMCOBJ=   -6654.26803189941     
 iteration          489 MCMCOBJ=   -6654.80970410190     
 iteration          490 MCMCOBJ=   -6609.88788344083     
 iteration          491 MCMCOBJ=   -6569.94867405982     
 iteration          492 MCMCOBJ=   -6572.39792560680     
 iteration          493 MCMCOBJ=   -6546.70070162840     
 iteration          494 MCMCOBJ=   -6585.87369362144     
 iteration          495 MCMCOBJ=   -6602.53004862226     
 iteration          496 MCMCOBJ=   -6620.03047155665     
 iteration          497 MCMCOBJ=   -6632.69151032868     
 iteration          498 MCMCOBJ=   -6628.38595313267     
 iteration          499 MCMCOBJ=   -6631.83967959988     
 iteration          500 MCMCOBJ=   -6606.93592252333     
 iteration          501 MCMCOBJ=   -6550.99838572564     
 iteration          502 MCMCOBJ=   -6594.48796987959     
 iteration          503 MCMCOBJ=   -6561.67827633814     
 iteration          504 MCMCOBJ=   -6572.01260230804     
 iteration          505 MCMCOBJ=   -6563.24586964712     
 iteration          506 MCMCOBJ=   -6549.62385500182     
 iteration          507 MCMCOBJ=   -6591.14866728442     
 iteration          508 MCMCOBJ=   -6593.29942188554     
 iteration          509 MCMCOBJ=   -6569.05771777471     
 iteration          510 MCMCOBJ=   -6615.06885443388     
 iteration          511 MCMCOBJ=   -6636.34574153383     
 iteration          512 MCMCOBJ=   -6613.69540095460     
 iteration          513 MCMCOBJ=   -6609.03971584542     
 iteration          514 MCMCOBJ=   -6621.00193947547     
 iteration          515 MCMCOBJ=   -6641.11994979411     
 iteration          516 MCMCOBJ=   -6671.15146908284     
 iteration          517 MCMCOBJ=   -6637.95893862000     
 iteration          518 MCMCOBJ=   -6605.13799797453     
 iteration          519 MCMCOBJ=   -6609.68546510129     
 iteration          520 MCMCOBJ=   -6682.57885997995     
 iteration          521 MCMCOBJ=   -6675.04112363149     
 iteration          522 MCMCOBJ=   -6619.08509745071     
 iteration          523 MCMCOBJ=   -6626.63226398204     
 iteration          524 MCMCOBJ=   -6575.44915374731     
 iteration          525 MCMCOBJ=   -6613.25008243692     
 iteration          526 MCMCOBJ=   -6618.85584031712     
 iteration          527 MCMCOBJ=   -6641.08600450038     
 iteration          528 MCMCOBJ=   -6613.59638787684     
 iteration          529 MCMCOBJ=   -6596.66386636644     
 iteration          530 MCMCOBJ=   -6622.13559230903     
 iteration          531 MCMCOBJ=   -6552.95634464327     
 iteration          532 MCMCOBJ=   -6599.70376868664     
 iteration          533 MCMCOBJ=   -6570.39007529047     
 iteration          534 MCMCOBJ=   -6543.50574416608     
 iteration          535 MCMCOBJ=   -6583.67879394552     
 iteration          536 MCMCOBJ=   -6673.65136715467     
 iteration          537 MCMCOBJ=   -6669.29203304881     
 iteration          538 MCMCOBJ=   -6634.33698334587     
 iteration          539 MCMCOBJ=   -6606.41886435661     
 iteration          540 MCMCOBJ=   -6572.52271515536     
 iteration          541 MCMCOBJ=   -6595.01117446975     
 iteration          542 MCMCOBJ=   -6579.20873528458     
 iteration          543 MCMCOBJ=   -6553.11009366608     
 iteration          544 MCMCOBJ=   -6592.34917034972     
 iteration          545 MCMCOBJ=   -6628.47509682325     
 iteration          546 MCMCOBJ=   -6629.45291358783     
 iteration          547 MCMCOBJ=   -6632.76286641264     
 iteration          548 MCMCOBJ=   -6580.39921739614     
 iteration          549 MCMCOBJ=   -6561.24737570973     
 iteration          550 MCMCOBJ=   -6594.96531728191     
 iteration          551 MCMCOBJ=   -6572.84087315447     
 iteration          552 MCMCOBJ=   -6576.39149981646     
 iteration          553 MCMCOBJ=   -6549.34866832986     
 iteration          554 MCMCOBJ=   -6572.90077194299     
 iteration          555 MCMCOBJ=   -6596.92913935754     
 iteration          556 MCMCOBJ=   -6553.75270045013     
 iteration          557 MCMCOBJ=   -6552.24551969272     
 iteration          558 MCMCOBJ=   -6584.36628229263     
 iteration          559 MCMCOBJ=   -6573.90852725726     
 iteration          560 MCMCOBJ=   -6564.57423556483     
 iteration          561 MCMCOBJ=   -6602.10116438213     
 iteration          562 MCMCOBJ=   -6596.98723901943     
 iteration          563 MCMCOBJ=   -6623.25974144327     
 iteration          564 MCMCOBJ=   -6622.24406254289     
 iteration          565 MCMCOBJ=   -6625.78498651231     
 iteration          566 MCMCOBJ=   -6593.74155323291     
 iteration          567 MCMCOBJ=   -6564.49661656339     
 iteration          568 MCMCOBJ=   -6580.21812257847     
 iteration          569 MCMCOBJ=   -6569.64605371875     
 iteration          570 MCMCOBJ=   -6634.48900676358     
 iteration          571 MCMCOBJ=   -6610.45987192199     
 iteration          572 MCMCOBJ=   -6608.12563786106     
 iteration          573 MCMCOBJ=   -6589.81918296539     
 iteration          574 MCMCOBJ=   -6609.34893283620     
 iteration          575 MCMCOBJ=   -6527.34246492319     
 iteration          576 MCMCOBJ=   -6585.77083137449     
 iteration          577 MCMCOBJ=   -6577.76796068388     
 iteration          578 MCMCOBJ=   -6609.33321187838     
 iteration          579 MCMCOBJ=   -6588.04815737689     
 iteration          580 MCMCOBJ=   -6619.16062306870     
 iteration          581 MCMCOBJ=   -6622.77810146720     
 iteration          582 MCMCOBJ=   -6617.91955190338     
 iteration          583 MCMCOBJ=   -6613.89293571818     
 iteration          584 MCMCOBJ=   -6639.82263223308     
 iteration          585 MCMCOBJ=   -6630.37943880749     
 iteration          586 MCMCOBJ=   -6609.09032876581     
 iteration          587 MCMCOBJ=   -6609.09033125534     
 iteration          588 MCMCOBJ=   -6627.91643333705     
 iteration          589 MCMCOBJ=   -6622.73980786176     
 iteration          590 MCMCOBJ=   -6624.65814469202     
 iteration          591 MCMCOBJ=   -6608.03119631097     
 iteration          592 MCMCOBJ=   -6560.89061345709     
 iteration          593 MCMCOBJ=   -6562.56076078907     
 iteration          594 MCMCOBJ=   -6603.83579253602     
 iteration          595 MCMCOBJ=   -6591.78054411738     
 iteration          596 MCMCOBJ=   -6605.03438088081     
 iteration          597 MCMCOBJ=   -6620.50278151542     
 iteration          598 MCMCOBJ=   -6598.09426308153     
 iteration          599 MCMCOBJ=   -6604.91848671436     
 iteration          600 MCMCOBJ=   -6598.23039594267     
 iteration          601 MCMCOBJ=   -6594.70261376976     
 iteration          602 MCMCOBJ=   -6608.75692157080     
 iteration          603 MCMCOBJ=   -6609.15868288858     
 iteration          604 MCMCOBJ=   -6622.00880667437     
 iteration          605 MCMCOBJ=   -6585.75859240628     
 iteration          606 MCMCOBJ=   -6544.42016960970     
 iteration          607 MCMCOBJ=   -6528.63004121450     
 iteration          608 MCMCOBJ=   -6560.47769400258     
 iteration          609 MCMCOBJ=   -6560.69531557949     
 iteration          610 MCMCOBJ=   -6584.95051140994     
 iteration          611 MCMCOBJ=   -6537.32658233657     
 iteration          612 MCMCOBJ=   -6574.88554776547     
 iteration          613 MCMCOBJ=   -6587.99957255063     
 iteration          614 MCMCOBJ=   -6654.71581584962     
 iteration          615 MCMCOBJ=   -6641.28203908728     
 iteration          616 MCMCOBJ=   -6647.46722455190     
 iteration          617 MCMCOBJ=   -6665.89284710417     
 iteration          618 MCMCOBJ=   -6603.14377685439     
 iteration          619 MCMCOBJ=   -6641.43308126545     
 iteration          620 MCMCOBJ=   -6665.98691067480     
 iteration          621 MCMCOBJ=   -6669.64093043071     
 iteration          622 MCMCOBJ=   -6580.16195616832     
 iteration          623 MCMCOBJ=   -6620.17804331487     
 iteration          624 MCMCOBJ=   -6647.37321849079     
 iteration          625 MCMCOBJ=   -6554.36916619774     
 iteration          626 MCMCOBJ=   -6612.98096540606     
 iteration          627 MCMCOBJ=   -6598.96009618647     
 iteration          628 MCMCOBJ=   -6602.77091663490     
 iteration          629 MCMCOBJ=   -6629.36213641437     
 iteration          630 MCMCOBJ=   -6619.06297285789     
 iteration          631 MCMCOBJ=   -6576.61182010375     
 iteration          632 MCMCOBJ=   -6592.93563024461     
 iteration          633 MCMCOBJ=   -6576.16139073184     
 iteration          634 MCMCOBJ=   -6621.75122343265     
 iteration          635 MCMCOBJ=   -6597.66653415849     
 iteration          636 MCMCOBJ=   -6589.85694990150     
 iteration          637 MCMCOBJ=   -6547.43685245760     
 iteration          638 MCMCOBJ=   -6602.81205767595     
 iteration          639 MCMCOBJ=   -6564.32035438150     
 iteration          640 MCMCOBJ=   -6541.29880270682     
 iteration          641 MCMCOBJ=   -6562.07677809770     
 iteration          642 MCMCOBJ=   -6554.65236989666     
 iteration          643 MCMCOBJ=   -6614.35416684155     
 iteration          644 MCMCOBJ=   -6630.59977268459     
 iteration          645 MCMCOBJ=   -6626.76959176132     
 iteration          646 MCMCOBJ=   -6617.51751487140     
 iteration          647 MCMCOBJ=   -6622.74031643011     
 iteration          648 MCMCOBJ=   -6622.74031657229     
 iteration          649 MCMCOBJ=   -6588.14938865473     
 iteration          650 MCMCOBJ=   -6519.57569971164     
 iteration          651 MCMCOBJ=   -6535.46323081033     
 iteration          652 MCMCOBJ=   -6537.43287845956     
 iteration          653 MCMCOBJ=   -6551.89125771070     
 iteration          654 MCMCOBJ=   -6568.54789680586     
 iteration          655 MCMCOBJ=   -6586.47940161391     
 iteration          656 MCMCOBJ=   -6614.84396685510     
 iteration          657 MCMCOBJ=   -6616.10641185100     
 iteration          658 MCMCOBJ=   -6584.34031909182     
 iteration          659 MCMCOBJ=   -6559.27492446189     
 iteration          660 MCMCOBJ=   -6520.21054434527     
 iteration          661 MCMCOBJ=   -6536.80887362028     
 iteration          662 MCMCOBJ=   -6588.79625232078     
 iteration          663 MCMCOBJ=   -6560.03081535237     
 iteration          664 MCMCOBJ=   -6574.79592400422     
 iteration          665 MCMCOBJ=   -6598.17376392898     
 iteration          666 MCMCOBJ=   -6609.18743221376     
 iteration          667 MCMCOBJ=   -6613.26918113748     
 iteration          668 MCMCOBJ=   -6620.14212722704     
 iteration          669 MCMCOBJ=   -6598.09914604542     
 iteration          670 MCMCOBJ=   -6577.86580575015     
 iteration          671 MCMCOBJ=   -6594.29240662039     
 iteration          672 MCMCOBJ=   -6603.87753797289     
 iteration          673 MCMCOBJ=   -6617.01757205816     
 iteration          674 MCMCOBJ=   -6624.82239910661     
 iteration          675 MCMCOBJ=   -6572.43998216008     
 iteration          676 MCMCOBJ=   -6579.76370461339     
 iteration          677 MCMCOBJ=   -6607.49685884748     
 iteration          678 MCMCOBJ=   -6547.00967546332     
 iteration          679 MCMCOBJ=   -6482.62944646628     
 iteration          680 MCMCOBJ=   -6574.29218742000     
 iteration          681 MCMCOBJ=   -6586.36243786900     
 iteration          682 MCMCOBJ=   -6607.59313001991     
 iteration          683 MCMCOBJ=   -6604.55234368516     
 iteration          684 MCMCOBJ=   -6583.19882018837     
 iteration          685 MCMCOBJ=   -6602.42066365517     
 iteration          686 MCMCOBJ=   -6613.78405764155     
 iteration          687 MCMCOBJ=   -6613.89531918078     
 iteration          688 MCMCOBJ=   -6597.95959408464     
 iteration          689 MCMCOBJ=   -6631.83794165978     
 iteration          690 MCMCOBJ=   -6581.93907538290     
 iteration          691 MCMCOBJ=   -6536.84096581629     
 iteration          692 MCMCOBJ=   -6575.51827192502     
 iteration          693 MCMCOBJ=   -6590.84073824723     
 iteration          694 MCMCOBJ=   -6627.18234207329     
 iteration          695 MCMCOBJ=   -6590.62611888291     
 iteration          696 MCMCOBJ=   -6564.41545251307     
 iteration          697 MCMCOBJ=   -6559.07935450076     
 iteration          698 MCMCOBJ=   -6566.83016749993     
 iteration          699 MCMCOBJ=   -6570.31418514523     
 iteration          700 MCMCOBJ=   -6541.00807478103     
 iteration          701 MCMCOBJ=   -6572.09825128742     
 iteration          702 MCMCOBJ=   -6599.26602974256     
 iteration          703 MCMCOBJ=   -6598.71373246483     
 iteration          704 MCMCOBJ=   -6598.71373424216     
 iteration          705 MCMCOBJ=   -6551.07361515095     
 iteration          706 MCMCOBJ=   -6522.26456529311     
 iteration          707 MCMCOBJ=   -6529.26116729063     
 iteration          708 MCMCOBJ=   -6526.96469554463     
 iteration          709 MCMCOBJ=   -6595.82679140923     
 iteration          710 MCMCOBJ=   -6595.82678873395     
 iteration          711 MCMCOBJ=   -6580.11441920202     
 iteration          712 MCMCOBJ=   -6550.96387978469     
 iteration          713 MCMCOBJ=   -6578.17865330096     
 iteration          714 MCMCOBJ=   -6619.24488329152     
 iteration          715 MCMCOBJ=   -6603.34572627614     
 iteration          716 MCMCOBJ=   -6596.03585107826     
 iteration          717 MCMCOBJ=   -6607.89601741034     
 iteration          718 MCMCOBJ=   -6590.73608377038     
 iteration          719 MCMCOBJ=   -6617.98043956565     
 iteration          720 MCMCOBJ=   -6645.39923603114     
 iteration          721 MCMCOBJ=   -6600.36570058213     
 iteration          722 MCMCOBJ=   -6576.73119859239     
 iteration          723 MCMCOBJ=   -6552.23370579831     
 iteration          724 MCMCOBJ=   -6631.21018363297     
 iteration          725 MCMCOBJ=   -6593.92804507993     
 iteration          726 MCMCOBJ=   -6591.17380108818     
 iteration          727 MCMCOBJ=   -6604.80380163398     
 iteration          728 MCMCOBJ=   -6644.74947362639     
 iteration          729 MCMCOBJ=   -6603.79034221892     
 iteration          730 MCMCOBJ=   -6596.44748465707     
 iteration          731 MCMCOBJ=   -6659.90842085768     
 iteration          732 MCMCOBJ=   -6656.73346122582     
 iteration          733 MCMCOBJ=   -6649.33445581975     
 iteration          734 MCMCOBJ=   -6598.06477749793     
 iteration          735 MCMCOBJ=   -6598.24303138180     
 iteration          736 MCMCOBJ=   -6608.38107350764     
 iteration          737 MCMCOBJ=   -6563.69500398497     
 iteration          738 MCMCOBJ=   -6606.07756231270     
 iteration          739 MCMCOBJ=   -6602.70408795713     
 iteration          740 MCMCOBJ=   -6603.67883128058     
 iteration          741 MCMCOBJ=   -6615.60908306947     
 iteration          742 MCMCOBJ=   -6617.42663833875     
 iteration          743 MCMCOBJ=   -6607.11673596150     
 iteration          744 MCMCOBJ=   -6611.70815447355     
 iteration          745 MCMCOBJ=   -6594.64068267853     
 iteration          746 MCMCOBJ=   -6630.19759556386     
 iteration          747 MCMCOBJ=   -6604.79096201208     
 iteration          748 MCMCOBJ=   -6566.96626549542     
 iteration          749 MCMCOBJ=   -6562.45409578994     
 iteration          750 MCMCOBJ=   -6568.88963257613     
 iteration          751 MCMCOBJ=   -6557.19564939986     
 iteration          752 MCMCOBJ=   -6573.35410811066     
 iteration          753 MCMCOBJ=   -6565.50155363832     
 iteration          754 MCMCOBJ=   -6520.80289272086     
 iteration          755 MCMCOBJ=   -6569.50524467185     
 iteration          756 MCMCOBJ=   -6597.86115396393     
 iteration          757 MCMCOBJ=   -6581.62649625493     
 iteration          758 MCMCOBJ=   -6570.11608309794     
 iteration          759 MCMCOBJ=   -6636.17296306329     
 iteration          760 MCMCOBJ=   -6640.36566156615     
 iteration          761 MCMCOBJ=   -6635.49789577962     
 iteration          762 MCMCOBJ=   -6598.46898530996     
 iteration          763 MCMCOBJ=   -6594.24598438114     
 iteration          764 MCMCOBJ=   -6637.85464052178     
 iteration          765 MCMCOBJ=   -6644.77218519181     
 iteration          766 MCMCOBJ=   -6610.20540451208     
 iteration          767 MCMCOBJ=   -6610.20540457493     
 iteration          768 MCMCOBJ=   -6612.29629278179     
 iteration          769 MCMCOBJ=   -6624.82487587048     
 iteration          770 MCMCOBJ=   -6621.72009844596     
 iteration          771 MCMCOBJ=   -6586.60969861041     
 iteration          772 MCMCOBJ=   -6542.54205625640     
 iteration          773 MCMCOBJ=   -6567.97879736065     
 iteration          774 MCMCOBJ=   -6567.79635753545     
 iteration          775 MCMCOBJ=   -6575.90992718739     
 iteration          776 MCMCOBJ=   -6616.27717946198     
 iteration          777 MCMCOBJ=   -6543.90784944595     
 iteration          778 MCMCOBJ=   -6532.36056775631     
 iteration          779 MCMCOBJ=   -6551.13338892343     
 iteration          780 MCMCOBJ=   -6553.99957511667     
 iteration          781 MCMCOBJ=   -6547.35698245668     
 iteration          782 MCMCOBJ=   -6533.97446354662     
 iteration          783 MCMCOBJ=   -6575.92077905037     
 iteration          784 MCMCOBJ=   -6601.63134141695     
 iteration          785 MCMCOBJ=   -6601.17406653486     
 iteration          786 MCMCOBJ=   -6593.33023188660     
 iteration          787 MCMCOBJ=   -6575.92488974091     
 iteration          788 MCMCOBJ=   -6575.92486242635     
 iteration          789 MCMCOBJ=   -6560.20331689754     
 iteration          790 MCMCOBJ=   -6594.97594537103     
 iteration          791 MCMCOBJ=   -6574.91757479090     
 iteration          792 MCMCOBJ=   -6590.68484099298     
 iteration          793 MCMCOBJ=   -6589.37112129159     
 iteration          794 MCMCOBJ=   -6554.91574827477     
 iteration          795 MCMCOBJ=   -6561.29736601366     
 iteration          796 MCMCOBJ=   -6572.45015590786     
 iteration          797 MCMCOBJ=   -6579.55012534597     
 iteration          798 MCMCOBJ=   -6624.96496857180     
 iteration          799 MCMCOBJ=   -6573.91736293028     
 iteration          800 MCMCOBJ=   -6577.65790444133     
 iteration          801 MCMCOBJ=   -6554.84849288426     
 iteration          802 MCMCOBJ=   -6581.48100677135     
 iteration          803 MCMCOBJ=   -6579.70008690189     
 iteration          804 MCMCOBJ=   -6591.27521863202     
 iteration          805 MCMCOBJ=   -6580.04426415352     
 iteration          806 MCMCOBJ=   -6597.02273754248     
 iteration          807 MCMCOBJ=   -6599.11775294219     
 iteration          808 MCMCOBJ=   -6595.01126297049     
 iteration          809 MCMCOBJ=   -6618.91493452483     
 iteration          810 MCMCOBJ=   -6572.94246678870     
 iteration          811 MCMCOBJ=   -6605.74025455390     
 iteration          812 MCMCOBJ=   -6573.35833819578     
 iteration          813 MCMCOBJ=   -6590.54700520268     
 iteration          814 MCMCOBJ=   -6577.20243525989     
 iteration          815 MCMCOBJ=   -6579.43683267386     
 iteration          816 MCMCOBJ=   -6567.78292341670     
 iteration          817 MCMCOBJ=   -6582.41487078265     
 iteration          818 MCMCOBJ=   -6544.43204156225     
 iteration          819 MCMCOBJ=   -6584.40523470352     
 iteration          820 MCMCOBJ=   -6571.13148878571     
 iteration          821 MCMCOBJ=   -6578.98112520583     
 iteration          822 MCMCOBJ=   -6578.98112490798     
 iteration          823 MCMCOBJ=   -6625.61357904031     
 iteration          824 MCMCOBJ=   -6583.99471952678     
 iteration          825 MCMCOBJ=   -6600.37857912760     
 iteration          826 MCMCOBJ=   -6638.46196089925     
 iteration          827 MCMCOBJ=   -6630.14526572998     
 iteration          828 MCMCOBJ=   -6658.58204567300     
 iteration          829 MCMCOBJ=   -6635.24423975799     
 iteration          830 MCMCOBJ=   -6605.65276298054     
 iteration          831 MCMCOBJ=   -6580.54128520487     
 iteration          832 MCMCOBJ=   -6531.28141869869     
 iteration          833 MCMCOBJ=   -6581.96884070927     
 iteration          834 MCMCOBJ=   -6589.01476554744     
 iteration          835 MCMCOBJ=   -6569.26677474574     
 iteration          836 MCMCOBJ=   -6582.41194531948     
 iteration          837 MCMCOBJ=   -6583.18642588674     
 iteration          838 MCMCOBJ=   -6624.34130489376     
 iteration          839 MCMCOBJ=   -6542.74215697922     
 iteration          840 MCMCOBJ=   -6549.52010955791     
 iteration          841 MCMCOBJ=   -6518.44945354367     
 iteration          842 MCMCOBJ=   -6518.23396806071     
 iteration          843 MCMCOBJ=   -6584.63038012439     
 iteration          844 MCMCOBJ=   -6571.40949586109     
 iteration          845 MCMCOBJ=   -6536.84036190833     
 iteration          846 MCMCOBJ=   -6590.00285794659     
 iteration          847 MCMCOBJ=   -6607.80976705753     
 iteration          848 MCMCOBJ=   -6607.72335984812     
 iteration          849 MCMCOBJ=   -6646.26125348130     
 iteration          850 MCMCOBJ=   -6623.29378530275     
 iteration          851 MCMCOBJ=   -6612.84092359389     
 iteration          852 MCMCOBJ=   -6622.16999731709     
 iteration          853 MCMCOBJ=   -6587.41332555198     
 iteration          854 MCMCOBJ=   -6574.38641333524     
 iteration          855 MCMCOBJ=   -6585.14619256419     
 iteration          856 MCMCOBJ=   -6583.59028583605     
 iteration          857 MCMCOBJ=   -6622.10962242719     
 iteration          858 MCMCOBJ=   -6637.58432985623     
 iteration          859 MCMCOBJ=   -6665.90652116421     
 iteration          860 MCMCOBJ=   -6665.90652314887     
 iteration          861 MCMCOBJ=   -6665.90652257836     
 iteration          862 MCMCOBJ=   -6664.01159660335     
 iteration          863 MCMCOBJ=   -6664.01159560753     
 iteration          864 MCMCOBJ=   -6596.98203929566     
 iteration          865 MCMCOBJ=   -6671.37239200857     
 iteration          866 MCMCOBJ=   -6652.21593532058     
 iteration          867 MCMCOBJ=   -6649.39897036086     
 iteration          868 MCMCOBJ=   -6612.46482653344     
 iteration          869 MCMCOBJ=   -6637.17518913743     
 iteration          870 MCMCOBJ=   -6640.93728078090     
 iteration          871 MCMCOBJ=   -6596.51288102168     
 iteration          872 MCMCOBJ=   -6622.73143774401     
 iteration          873 MCMCOBJ=   -6615.93343184249     
 iteration          874 MCMCOBJ=   -6620.38941896497     
 iteration          875 MCMCOBJ=   -6665.39630117563     
 iteration          876 MCMCOBJ=   -6637.17248772385     
 iteration          877 MCMCOBJ=   -6585.70263056512     
 iteration          878 MCMCOBJ=   -6604.41308370937     
 iteration          879 MCMCOBJ=   -6652.18493810515     
 iteration          880 MCMCOBJ=   -6569.60230891408     
 iteration          881 MCMCOBJ=   -6581.60735297234     
 iteration          882 MCMCOBJ=   -6555.61737368337     
 iteration          883 MCMCOBJ=   -6585.75495398995     
 iteration          884 MCMCOBJ=   -6571.54216394490     
 iteration          885 MCMCOBJ=   -6598.09068930202     
 iteration          886 MCMCOBJ=   -6608.83732119119     
 iteration          887 MCMCOBJ=   -6613.86628945350     
 iteration          888 MCMCOBJ=   -6610.45757367904     
 iteration          889 MCMCOBJ=   -6618.34962063807     
 iteration          890 MCMCOBJ=   -6579.62977498107     
 iteration          891 MCMCOBJ=   -6598.59968321426     
 iteration          892 MCMCOBJ=   -6599.93625638622     
 iteration          893 MCMCOBJ=   -6548.64074399962     
 iteration          894 MCMCOBJ=   -6613.58460831941     
 iteration          895 MCMCOBJ=   -6631.15077172446     
 iteration          896 MCMCOBJ=   -6605.08202570184     
 iteration          897 MCMCOBJ=   -6643.84411620632     
 iteration          898 MCMCOBJ=   -6581.74074730932     
 iteration          899 MCMCOBJ=   -6578.99805130929     
 iteration          900 MCMCOBJ=   -6550.24500043320     
 iteration          901 MCMCOBJ=   -6567.12134190375     
 iteration          902 MCMCOBJ=   -6564.71777605134     
 iteration          903 MCMCOBJ=   -6598.48438137482     
 iteration          904 MCMCOBJ=   -6568.40220874817     
 iteration          905 MCMCOBJ=   -6598.64952786912     
 iteration          906 MCMCOBJ=   -6539.27510131315     
 iteration          907 MCMCOBJ=   -6577.52276080385     
 iteration          908 MCMCOBJ=   -6602.08481241034     
 iteration          909 MCMCOBJ=   -6619.28103192325     
 iteration          910 MCMCOBJ=   -6628.80409774409     
 iteration          911 MCMCOBJ=   -6634.27913857541     
 iteration          912 MCMCOBJ=   -6578.82949615226     
 iteration          913 MCMCOBJ=   -6603.87847602888     
 iteration          914 MCMCOBJ=   -6587.79801664501     
 iteration          915 MCMCOBJ=   -6585.62179010056     
 iteration          916 MCMCOBJ=   -6567.48396659030     
 iteration          917 MCMCOBJ=   -6570.44871261505     
 iteration          918 MCMCOBJ=   -6624.78274119531     
 iteration          919 MCMCOBJ=   -6614.13407860408     
 iteration          920 MCMCOBJ=   -6547.62593888621     
 iteration          921 MCMCOBJ=   -6540.85637515197     
 iteration          922 MCMCOBJ=   -6578.58286299072     
 iteration          923 MCMCOBJ=   -6584.89022075864     
 iteration          924 MCMCOBJ=   -6576.84641257495     
 iteration          925 MCMCOBJ=   -6655.12028031140     
 iteration          926 MCMCOBJ=   -6646.71833598906     
 iteration          927 MCMCOBJ=   -6597.88175331220     
 iteration          928 MCMCOBJ=   -6622.89651347338     
 iteration          929 MCMCOBJ=   -6685.95617446812     
 iteration          930 MCMCOBJ=   -6624.99868411895     
 iteration          931 MCMCOBJ=   -6643.47727135141     
 iteration          932 MCMCOBJ=   -6639.55085150539     
 iteration          933 MCMCOBJ=   -6621.41402233430     
 iteration          934 MCMCOBJ=   -6634.01994891266     
 iteration          935 MCMCOBJ=   -6583.49210576149     
 iteration          936 MCMCOBJ=   -6582.17869935486     
 iteration          937 MCMCOBJ=   -6575.81611645186     
 iteration          938 MCMCOBJ=   -6554.53956488769     
 iteration          939 MCMCOBJ=   -6570.70913123827     
 iteration          940 MCMCOBJ=   -6573.93957114479     
 iteration          941 MCMCOBJ=   -6578.08932135851     
 iteration          942 MCMCOBJ=   -6536.96498393109     
 iteration          943 MCMCOBJ=   -6552.98135508089     
 iteration          944 MCMCOBJ=   -6564.55636488498     
 iteration          945 MCMCOBJ=   -6555.46123809055     
 iteration          946 MCMCOBJ=   -6594.45280908411     
 iteration          947 MCMCOBJ=   -6603.41797958097     
 iteration          948 MCMCOBJ=   -6575.41990879136     
 iteration          949 MCMCOBJ=   -6575.35772368718     
 iteration          950 MCMCOBJ=   -6579.15780730836     
 iteration          951 MCMCOBJ=   -6637.45323437863     
 iteration          952 MCMCOBJ=   -6649.64241707589     
 iteration          953 MCMCOBJ=   -6622.68953618227     
 iteration          954 MCMCOBJ=   -6650.57216121603     
 iteration          955 MCMCOBJ=   -6643.86637880276     
 iteration          956 MCMCOBJ=   -6659.14352455838     
 iteration          957 MCMCOBJ=   -6685.62646518212     
 iteration          958 MCMCOBJ=   -6685.62646833530     
 iteration          959 MCMCOBJ=   -6571.08633957075     
 iteration          960 MCMCOBJ=   -6613.78082917282     
 iteration          961 MCMCOBJ=   -6605.26931211526     
 iteration          962 MCMCOBJ=   -6626.32474954224     
 iteration          963 MCMCOBJ=   -6621.13787876980     
 iteration          964 MCMCOBJ=   -6627.51959513628     
 iteration          965 MCMCOBJ=   -6604.36782611287     
 iteration          966 MCMCOBJ=   -6518.74859506988     
 iteration          967 MCMCOBJ=   -6558.07194612131     
 iteration          968 MCMCOBJ=   -6578.83844852416     
 iteration          969 MCMCOBJ=   -6588.71783380566     
 iteration          970 MCMCOBJ=   -6633.33067789615     
 iteration          971 MCMCOBJ=   -6627.71274946558     
 iteration          972 MCMCOBJ=   -6576.92788420988     
 iteration          973 MCMCOBJ=   -6625.53123515008     
 iteration          974 MCMCOBJ=   -6585.90859789375     
 iteration          975 MCMCOBJ=   -6567.86601754954     
 iteration          976 MCMCOBJ=   -6597.79345209281     
 iteration          977 MCMCOBJ=   -6568.44438455187     
 iteration          978 MCMCOBJ=   -6567.43270374146     
 iteration          979 MCMCOBJ=   -6597.80854001673     
 iteration          980 MCMCOBJ=   -6638.02731887632     
 iteration          981 MCMCOBJ=   -6632.81210575963     
 iteration          982 MCMCOBJ=   -6588.77623996930     
 iteration          983 MCMCOBJ=   -6565.31299184894     
 iteration          984 MCMCOBJ=   -6548.76150061439     
 iteration          985 MCMCOBJ=   -6595.32430080222     
 iteration          986 MCMCOBJ=   -6633.91117113670     
 iteration          987 MCMCOBJ=   -6604.05880291929     
 iteration          988 MCMCOBJ=   -6578.11807372228     
 iteration          989 MCMCOBJ=   -6605.03250880455     
 iteration          990 MCMCOBJ=   -6612.55620780208     
 iteration          991 MCMCOBJ=   -6552.09849755577     
 iteration          992 MCMCOBJ=   -6568.38217566923     
 iteration          993 MCMCOBJ=   -6580.07789232785     
 iteration          994 MCMCOBJ=   -6598.83193349852     
 iteration          995 MCMCOBJ=   -6601.28441999902     
 iteration          996 MCMCOBJ=   -6591.40713154947     
 iteration          997 MCMCOBJ=   -6633.32877743258     
 iteration          998 MCMCOBJ=   -6629.07031788659     
 iteration          999 MCMCOBJ=   -6602.16505573698     
 iteration         1000 MCMCOBJ=   -6605.08674080566     
 iteration         1001 MCMCOBJ=   -6567.71680855682     
 iteration         1002 MCMCOBJ=   -6606.17624578974     
 iteration         1003 MCMCOBJ=   -6648.99795179497     
 iteration         1004 MCMCOBJ=   -6655.11460749728     
 iteration         1005 MCMCOBJ=   -6633.42246956730     
 iteration         1006 MCMCOBJ=   -6586.85621881333     
 iteration         1007 MCMCOBJ=   -6596.58323897791     
 iteration         1008 MCMCOBJ=   -6606.94693989336     
 iteration         1009 MCMCOBJ=   -6621.00676376149     
 iteration         1010 MCMCOBJ=   -6593.91353085181     
 iteration         1011 MCMCOBJ=   -6552.45594469502     
 iteration         1012 MCMCOBJ=   -6570.48020517325     
 iteration         1013 MCMCOBJ=   -6561.54075025196     
 iteration         1014 MCMCOBJ=   -6615.37123710657     
 iteration         1015 MCMCOBJ=   -6603.17854737391     
 iteration         1016 MCMCOBJ=   -6639.82326798272     
 iteration         1017 MCMCOBJ=   -6607.46530221699     
 iteration         1018 MCMCOBJ=   -6597.86751450179     
 iteration         1019 MCMCOBJ=   -6612.82096955497     
 iteration         1020 MCMCOBJ=   -6578.96808661853     
 iteration         1021 MCMCOBJ=   -6549.21827214345     
 iteration         1022 MCMCOBJ=   -6567.29428567991     
 iteration         1023 MCMCOBJ=   -6600.98389431743     
 iteration         1024 MCMCOBJ=   -6569.81377229730     
 iteration         1025 MCMCOBJ=   -6605.69652509008     
 iteration         1026 MCMCOBJ=   -6620.72678418350     
 iteration         1027 MCMCOBJ=   -6627.23123866034     
 iteration         1028 MCMCOBJ=   -6623.77245476723     
 iteration         1029 MCMCOBJ=   -6634.13973639963     
 iteration         1030 MCMCOBJ=   -6617.59753025565     
 iteration         1031 MCMCOBJ=   -6622.01933553931     
 iteration         1032 MCMCOBJ=   -6603.55091156593     
 iteration         1033 MCMCOBJ=   -6570.65685230876     
 iteration         1034 MCMCOBJ=   -6562.55282576317     
 iteration         1035 MCMCOBJ=   -6544.80335721101     
 iteration         1036 MCMCOBJ=   -6582.85198527864     
 iteration         1037 MCMCOBJ=   -6541.07352715736     
 iteration         1038 MCMCOBJ=   -6536.83083776089     
 iteration         1039 MCMCOBJ=   -6572.37476746322     
 iteration         1040 MCMCOBJ=   -6596.09428360017     
 iteration         1041 MCMCOBJ=   -6551.73715177624     
 iteration         1042 MCMCOBJ=   -6574.85475583228     
 iteration         1043 MCMCOBJ=   -6548.47707668367     
 iteration         1044 MCMCOBJ=   -6564.69883859241     
 iteration         1045 MCMCOBJ=   -6547.94901769534     
 iteration         1046 MCMCOBJ=   -6587.17625004361     
 iteration         1047 MCMCOBJ=   -6638.14244333151     
 iteration         1048 MCMCOBJ=   -6624.93475993230     
 iteration         1049 MCMCOBJ=   -6650.20195010071     
 iteration         1050 MCMCOBJ=   -6650.20195202971     
 iteration         1051 MCMCOBJ=   -6650.20194996417     
 iteration         1052 MCMCOBJ=   -6696.52342397724     
 iteration         1053 MCMCOBJ=   -6657.42306498634     
 iteration         1054 MCMCOBJ=   -6657.60580801225     
 iteration         1055 MCMCOBJ=   -6634.01983884658     
 iteration         1056 MCMCOBJ=   -6626.37293502844     
 iteration         1057 MCMCOBJ=   -6645.01041697639     
 iteration         1058 MCMCOBJ=   -6634.72181246101     
 iteration         1059 MCMCOBJ=   -6587.58495985639     
 iteration         1060 MCMCOBJ=   -6637.25455564239     
 iteration         1061 MCMCOBJ=   -6620.49551665918     
 iteration         1062 MCMCOBJ=   -6621.07760052670     
 iteration         1063 MCMCOBJ=   -6565.21794225592     
 iteration         1064 MCMCOBJ=   -6580.31859368272     
 iteration         1065 MCMCOBJ=   -6567.95744282281     
 iteration         1066 MCMCOBJ=   -6586.67895993385     
 iteration         1067 MCMCOBJ=   -6553.63472671106     
 iteration         1068 MCMCOBJ=   -6583.86102778011     
 iteration         1069 MCMCOBJ=   -6619.88292066289     
 iteration         1070 MCMCOBJ=   -6630.30616267626     
 iteration         1071 MCMCOBJ=   -6660.16593367980     
 iteration         1072 MCMCOBJ=   -6651.26887334743     
 iteration         1073 MCMCOBJ=   -6674.57232694982     
 iteration         1074 MCMCOBJ=   -6674.66621164679     
 iteration         1075 MCMCOBJ=   -6632.52729743304     
 iteration         1076 MCMCOBJ=   -6610.86547057330     
 iteration         1077 MCMCOBJ=   -6614.26859439919     
 iteration         1078 MCMCOBJ=   -6563.91231634415     
 iteration         1079 MCMCOBJ=   -6574.35263210374     
 iteration         1080 MCMCOBJ=   -6564.67371547179     
 iteration         1081 MCMCOBJ=   -6548.87804886849     
 iteration         1082 MCMCOBJ=   -6597.19263032441     
 iteration         1083 MCMCOBJ=   -6625.58861601795     
 iteration         1084 MCMCOBJ=   -6564.14929683931     
 iteration         1085 MCMCOBJ=   -6598.42765536232     
 iteration         1086 MCMCOBJ=   -6592.32394274378     
 iteration         1087 MCMCOBJ=   -6574.73555834951     
 iteration         1088 MCMCOBJ=   -6564.30645803314     
 iteration         1089 MCMCOBJ=   -6581.09528840810     
 iteration         1090 MCMCOBJ=   -6583.18390757839     
 iteration         1091 MCMCOBJ=   -6607.97784468226     
 iteration         1092 MCMCOBJ=   -6599.66283393947     
 iteration         1093 MCMCOBJ=   -6601.80615430971     
 iteration         1094 MCMCOBJ=   -6652.73418771130     
 iteration         1095 MCMCOBJ=   -6629.89431167050     
 iteration         1096 MCMCOBJ=   -6625.10087925897     
 iteration         1097 MCMCOBJ=   -6611.67527154627     
 iteration         1098 MCMCOBJ=   -6578.03380634899     
 iteration         1099 MCMCOBJ=   -6623.52050640813     
 iteration         1100 MCMCOBJ=   -6630.72904331620     
 iteration         1101 MCMCOBJ=   -6605.54762734487     
 iteration         1102 MCMCOBJ=   -6571.33957254837     
 iteration         1103 MCMCOBJ=   -6569.83055153178     
 iteration         1104 MCMCOBJ=   -6550.60549798706     
 iteration         1105 MCMCOBJ=   -6576.08987165854     
 iteration         1106 MCMCOBJ=   -6614.33204995070     
 iteration         1107 MCMCOBJ=   -6649.39651033792     
 iteration         1108 MCMCOBJ=   -6659.72967278648     
 iteration         1109 MCMCOBJ=   -6639.35784530816     
 iteration         1110 MCMCOBJ=   -6615.60576439605     
 iteration         1111 MCMCOBJ=   -6574.15081468377     
 iteration         1112 MCMCOBJ=   -6581.83813662585     
 iteration         1113 MCMCOBJ=   -6597.21854448698     
 iteration         1114 MCMCOBJ=   -6583.33980381404     
 iteration         1115 MCMCOBJ=   -6556.87583252907     
 iteration         1116 MCMCOBJ=   -6581.88910704767     
 iteration         1117 MCMCOBJ=   -6568.01863007502     
 iteration         1118 MCMCOBJ=   -6617.98540213986     
 iteration         1119 MCMCOBJ=   -6658.43581067929     
 iteration         1120 MCMCOBJ=   -6667.22456961125     
 iteration         1121 MCMCOBJ=   -6611.17126273096     
 iteration         1122 MCMCOBJ=   -6584.77890275486     
 iteration         1123 MCMCOBJ=   -6598.39111787323     
 iteration         1124 MCMCOBJ=   -6581.69710010677     
 iteration         1125 MCMCOBJ=   -6551.70923878198     
 iteration         1126 MCMCOBJ=   -6547.30552319880     
 iteration         1127 MCMCOBJ=   -6562.15943713029     
 iteration         1128 MCMCOBJ=   -6617.39775537353     
 iteration         1129 MCMCOBJ=   -6614.35878311232     
 iteration         1130 MCMCOBJ=   -6627.27547313786     
 iteration         1131 MCMCOBJ=   -6609.16585389045     
 iteration         1132 MCMCOBJ=   -6623.92416131233     
 iteration         1133 MCMCOBJ=   -6584.15077451478     
 iteration         1134 MCMCOBJ=   -6626.03123855699     
 iteration         1135 MCMCOBJ=   -6675.33716440753     
 iteration         1136 MCMCOBJ=   -6706.21229978072     
 iteration         1137 MCMCOBJ=   -6654.91070515522     
 iteration         1138 MCMCOBJ=   -6665.66182154683     
 iteration         1139 MCMCOBJ=   -6669.20939290842     
 iteration         1140 MCMCOBJ=   -6669.20935404287     
 iteration         1141 MCMCOBJ=   -6611.02221252418     
 iteration         1142 MCMCOBJ=   -6617.09680600853     
 iteration         1143 MCMCOBJ=   -6610.64384414313     
 iteration         1144 MCMCOBJ=   -6644.09661526025     
 iteration         1145 MCMCOBJ=   -6654.47137955485     
 iteration         1146 MCMCOBJ=   -6622.45607009501     
 iteration         1147 MCMCOBJ=   -6663.08599167560     
 iteration         1148 MCMCOBJ=   -6659.78134101766     
 iteration         1149 MCMCOBJ=   -6613.31295280333     
 iteration         1150 MCMCOBJ=   -6624.20094026989     
 iteration         1151 MCMCOBJ=   -6595.68617388868     
 iteration         1152 MCMCOBJ=   -6648.16836183053     
 iteration         1153 MCMCOBJ=   -6630.20692509266     
 iteration         1154 MCMCOBJ=   -6630.20690639385     
 iteration         1155 MCMCOBJ=   -6569.77453305575     
 iteration         1156 MCMCOBJ=   -6568.39331444741     
 iteration         1157 MCMCOBJ=   -6580.87223368846     
 iteration         1158 MCMCOBJ=   -6597.67828836746     
 iteration         1159 MCMCOBJ=   -6560.36623433836     
 iteration         1160 MCMCOBJ=   -6519.79510789814     
 iteration         1161 MCMCOBJ=   -6585.70664879163     
 iteration         1162 MCMCOBJ=   -6582.57998382538     
 iteration         1163 MCMCOBJ=   -6569.33217623510     
 iteration         1164 MCMCOBJ=   -6595.13525263560     
 iteration         1165 MCMCOBJ=   -6638.85321767268     
 iteration         1166 MCMCOBJ=   -6609.19983546684     
 iteration         1167 MCMCOBJ=   -6587.03492769692     
 iteration         1168 MCMCOBJ=   -6590.14181398458     
 iteration         1169 MCMCOBJ=   -6602.86280703847     
 iteration         1170 MCMCOBJ=   -6642.47547815787     
 iteration         1171 MCMCOBJ=   -6613.26906134383     
 iteration         1172 MCMCOBJ=   -6642.60989002349     
 iteration         1173 MCMCOBJ=   -6628.39676719312     
 iteration         1174 MCMCOBJ=   -6576.17282059972     
 iteration         1175 MCMCOBJ=   -6600.53925441586     
 iteration         1176 MCMCOBJ=   -6613.98952102166     
 iteration         1177 MCMCOBJ=   -6585.70652080878     
 iteration         1178 MCMCOBJ=   -6588.89604472693     
 iteration         1179 MCMCOBJ=   -6577.95082265839     
 iteration         1180 MCMCOBJ=   -6556.87400787308     
 iteration         1181 MCMCOBJ=   -6572.54036181440     
 iteration         1182 MCMCOBJ=   -6576.44220778927     
 iteration         1183 MCMCOBJ=   -6606.59242067111     
 iteration         1184 MCMCOBJ=   -6586.17800080720     
 iteration         1185 MCMCOBJ=   -6572.42997284841     
 iteration         1186 MCMCOBJ=   -6537.69754198700     
 iteration         1187 MCMCOBJ=   -6528.37010817894     
 iteration         1188 MCMCOBJ=   -6535.59016152733     
 iteration         1189 MCMCOBJ=   -6527.28790152065     
 iteration         1190 MCMCOBJ=   -6555.50237199663     
 iteration         1191 MCMCOBJ=   -6578.22358313067     
 iteration         1192 MCMCOBJ=   -6615.94702298976     
 iteration         1193 MCMCOBJ=   -6558.41769372196     
 iteration         1194 MCMCOBJ=   -6602.65961692559     
 iteration         1195 MCMCOBJ=   -6558.36789129298     
 iteration         1196 MCMCOBJ=   -6588.28191324792     
 iteration         1197 MCMCOBJ=   -6572.08040856030     
 iteration         1198 MCMCOBJ=   -6554.63178284739     
 iteration         1199 MCMCOBJ=   -6580.57648425364     
 iteration         1200 MCMCOBJ=   -6559.51052621806     
 iteration         1201 MCMCOBJ=   -6540.71137252605     
 iteration         1202 MCMCOBJ=   -6549.69397784693     
 iteration         1203 MCMCOBJ=   -6566.93169787769     
 iteration         1204 MCMCOBJ=   -6587.13155272811     
 iteration         1205 MCMCOBJ=   -6607.48087929224     
 iteration         1206 MCMCOBJ=   -6628.16574636137     
 iteration         1207 MCMCOBJ=   -6560.68948731935     
 iteration         1208 MCMCOBJ=   -6595.47349638939     
 iteration         1209 MCMCOBJ=   -6592.34643115065     
 iteration         1210 MCMCOBJ=   -6532.03106672061     
 iteration         1211 MCMCOBJ=   -6575.54367908103     
 iteration         1212 MCMCOBJ=   -6553.72986914107     
 iteration         1213 MCMCOBJ=   -6544.71544550772     
 iteration         1214 MCMCOBJ=   -6595.25210500861     
 iteration         1215 MCMCOBJ=   -6552.72892701835     
 iteration         1216 MCMCOBJ=   -6566.93257530996     
 iteration         1217 MCMCOBJ=   -6631.54578278371     
 iteration         1218 MCMCOBJ=   -6603.84864652794     
 iteration         1219 MCMCOBJ=   -6606.75521684722     
 iteration         1220 MCMCOBJ=   -6606.75521412248     
 iteration         1221 MCMCOBJ=   -6661.75068337434     
 iteration         1222 MCMCOBJ=   -6665.52791344499     
 iteration         1223 MCMCOBJ=   -6600.93795002171     
 iteration         1224 MCMCOBJ=   -6577.21476310342     
 iteration         1225 MCMCOBJ=   -6568.22331499695     
 iteration         1226 MCMCOBJ=   -6555.76421588146     
 iteration         1227 MCMCOBJ=   -6584.56270734026     
 iteration         1228 MCMCOBJ=   -6653.35715077600     
 iteration         1229 MCMCOBJ=   -6573.30993379967     
 iteration         1230 MCMCOBJ=   -6574.43273469214     
 iteration         1231 MCMCOBJ=   -6537.84250658344     
 iteration         1232 MCMCOBJ=   -6539.21852872348     
 iteration         1233 MCMCOBJ=   -6556.04035605479     
 iteration         1234 MCMCOBJ=   -6556.52879901698     
 iteration         1235 MCMCOBJ=   -6554.91053064818     
 iteration         1236 MCMCOBJ=   -6618.34483760693     
 iteration         1237 MCMCOBJ=   -6663.39882151862     
 iteration         1238 MCMCOBJ=   -6588.54519023054     
 iteration         1239 MCMCOBJ=   -6601.77634911311     
 iteration         1240 MCMCOBJ=   -6622.17794419721     
 iteration         1241 MCMCOBJ=   -6631.15306771854     
 iteration         1242 MCMCOBJ=   -6574.80882385995     
 iteration         1243 MCMCOBJ=   -6639.94961809941     
 iteration         1244 MCMCOBJ=   -6605.31253179944     
 iteration         1245 MCMCOBJ=   -6573.94680738970     
 iteration         1246 MCMCOBJ=   -6580.27637017467     
 iteration         1247 MCMCOBJ=   -6542.42166599581     
 iteration         1248 MCMCOBJ=   -6565.24650988099     
 iteration         1249 MCMCOBJ=   -6566.00789747266     
 iteration         1250 MCMCOBJ=   -6615.45456172238     
 iteration         1251 MCMCOBJ=   -6611.49051030006     
 iteration         1252 MCMCOBJ=   -6636.63944824662     
 iteration         1253 MCMCOBJ=   -6587.02685130824     
 iteration         1254 MCMCOBJ=   -6532.65321985976     
 iteration         1255 MCMCOBJ=   -6541.89731814456     
 iteration         1256 MCMCOBJ=   -6582.99640104830     
 iteration         1257 MCMCOBJ=   -6602.61072500775     
 iteration         1258 MCMCOBJ=   -6615.85225261418     
 iteration         1259 MCMCOBJ=   -6578.44497603084     
 iteration         1260 MCMCOBJ=   -6576.73433208798     
 iteration         1261 MCMCOBJ=   -6588.37983680558     
 iteration         1262 MCMCOBJ=   -6607.86350345517     
 iteration         1263 MCMCOBJ=   -6606.01650288695     
 iteration         1264 MCMCOBJ=   -6606.01649345585     
 iteration         1265 MCMCOBJ=   -6573.04357446337     
 iteration         1266 MCMCOBJ=   -6579.01274794490     
 iteration         1267 MCMCOBJ=   -6599.46945324509     
 iteration         1268 MCMCOBJ=   -6630.95345003315     
 iteration         1269 MCMCOBJ=   -6583.18750621872     
 iteration         1270 MCMCOBJ=   -6643.00138325043     
 iteration         1271 MCMCOBJ=   -6627.83238993389     
 iteration         1272 MCMCOBJ=   -6617.22454781075     
 iteration         1273 MCMCOBJ=   -6608.66708207572     
 iteration         1274 MCMCOBJ=   -6631.06873462709     
 iteration         1275 MCMCOBJ=   -6589.89846882335     
 iteration         1276 MCMCOBJ=   -6580.05709175790     
 iteration         1277 MCMCOBJ=   -6592.46354979316     
 iteration         1278 MCMCOBJ=   -6621.08123635024     
 iteration         1279 MCMCOBJ=   -6580.28696994628     
 iteration         1280 MCMCOBJ=   -6598.71860048382     
 iteration         1281 MCMCOBJ=   -6653.04156477902     
 iteration         1282 MCMCOBJ=   -6627.20423291346     
 iteration         1283 MCMCOBJ=   -6588.48594226180     
 iteration         1284 MCMCOBJ=   -6561.42281857062     
 iteration         1285 MCMCOBJ=   -6584.58458210096     
 iteration         1286 MCMCOBJ=   -6624.86881746462     
 iteration         1287 MCMCOBJ=   -6624.86881829128     
 iteration         1288 MCMCOBJ=   -6621.46383225439     
 iteration         1289 MCMCOBJ=   -6594.21146082892     
 iteration         1290 MCMCOBJ=   -6615.62599688220     
 iteration         1291 MCMCOBJ=   -6613.13753479842     
 iteration         1292 MCMCOBJ=   -6613.13753214025     
 iteration         1293 MCMCOBJ=   -6654.24548206687     
 iteration         1294 MCMCOBJ=   -6653.55263399911     
 iteration         1295 MCMCOBJ=   -6634.98044616871     
 iteration         1296 MCMCOBJ=   -6594.44072226033     
 iteration         1297 MCMCOBJ=   -6592.68334782340     
 iteration         1298 MCMCOBJ=   -6611.26146238012     
 iteration         1299 MCMCOBJ=   -6579.33107182002     
 iteration         1300 MCMCOBJ=   -6575.07525514940     
 iteration         1301 MCMCOBJ=   -6614.52603326087     
 iteration         1302 MCMCOBJ=   -6617.13011104579     
 iteration         1303 MCMCOBJ=   -6609.78729827397     
 iteration         1304 MCMCOBJ=   -6627.18272209925     
 iteration         1305 MCMCOBJ=   -6696.52900399601     
 iteration         1306 MCMCOBJ=   -6625.30051396506     
 iteration         1307 MCMCOBJ=   -6631.56476117778     
 iteration         1308 MCMCOBJ=   -6622.03252567914     
 iteration         1309 MCMCOBJ=   -6648.92428716051     
 iteration         1310 MCMCOBJ=   -6648.92428719507     
 iteration         1311 MCMCOBJ=   -6587.56124468261     
 iteration         1312 MCMCOBJ=   -6578.12881528813     
 iteration         1313 MCMCOBJ=   -6620.45736733904     
 iteration         1314 MCMCOBJ=   -6614.65005903339     
 iteration         1315 MCMCOBJ=   -6591.86736688192     
 iteration         1316 MCMCOBJ=   -6543.40999835729     
 iteration         1317 MCMCOBJ=   -6530.58255521753     
 iteration         1318 MCMCOBJ=   -6581.78239531631     
 iteration         1319 MCMCOBJ=   -6565.86623869900     
 iteration         1320 MCMCOBJ=   -6558.63380627443     
 iteration         1321 MCMCOBJ=   -6636.10208816833     
 iteration         1322 MCMCOBJ=   -6612.96427033448     
 iteration         1323 MCMCOBJ=   -6608.91323084684     
 iteration         1324 MCMCOBJ=   -6558.58161347292     
 iteration         1325 MCMCOBJ=   -6641.00056737748     
 iteration         1326 MCMCOBJ=   -6588.24262798352     
 iteration         1327 MCMCOBJ=   -6601.13878375379     
 iteration         1328 MCMCOBJ=   -6614.43375726023     
 iteration         1329 MCMCOBJ=   -6580.43224702578     
 iteration         1330 MCMCOBJ=   -6561.93345822854     
 iteration         1331 MCMCOBJ=   -6581.13632124569     
 iteration         1332 MCMCOBJ=   -6599.81966444793     
 iteration         1333 MCMCOBJ=   -6599.81966467705     
 iteration         1334 MCMCOBJ=   -6566.06399477202     
 iteration         1335 MCMCOBJ=   -6596.26775903492     
 iteration         1336 MCMCOBJ=   -6629.25419062444     
 iteration         1337 MCMCOBJ=   -6603.01504507840     
 iteration         1338 MCMCOBJ=   -6587.05900572872     
 iteration         1339 MCMCOBJ=   -6636.53763943379     
 iteration         1340 MCMCOBJ=   -6635.25837085698     
 iteration         1341 MCMCOBJ=   -6521.89995560642     
 iteration         1342 MCMCOBJ=   -6544.09572082436     
 iteration         1343 MCMCOBJ=   -6603.70900699308     
 iteration         1344 MCMCOBJ=   -6576.33272674999     
 iteration         1345 MCMCOBJ=   -6578.12679547397     
 iteration         1346 MCMCOBJ=   -6618.23533096896     
 iteration         1347 MCMCOBJ=   -6608.29411951197     
 iteration         1348 MCMCOBJ=   -6596.88860072041     
 iteration         1349 MCMCOBJ=   -6585.47236864139     
 iteration         1350 MCMCOBJ=   -6570.70061635916     
 iteration         1351 MCMCOBJ=   -6586.81321251801     
 iteration         1352 MCMCOBJ=   -6540.47918332568     
 iteration         1353 MCMCOBJ=   -6582.64808825488     
 iteration         1354 MCMCOBJ=   -6561.52562280017     
 iteration         1355 MCMCOBJ=   -6552.38265324893     
 iteration         1356 MCMCOBJ=   -6555.44015583994     
 iteration         1357 MCMCOBJ=   -6589.01995662732     
 iteration         1358 MCMCOBJ=   -6597.01105506528     
 iteration         1359 MCMCOBJ=   -6629.32046964741     
 iteration         1360 MCMCOBJ=   -6627.00715130101     
 iteration         1361 MCMCOBJ=   -6627.00714831684     
 iteration         1362 MCMCOBJ=   -6605.06680505002     
 iteration         1363 MCMCOBJ=   -6592.89739179078     
 iteration         1364 MCMCOBJ=   -6623.21943034304     
 iteration         1365 MCMCOBJ=   -6623.21943871397     
 iteration         1366 MCMCOBJ=   -6595.06035092866     
 iteration         1367 MCMCOBJ=   -6598.15577844607     
 iteration         1368 MCMCOBJ=   -6555.71124297106     
 iteration         1369 MCMCOBJ=   -6623.49122839772     
 iteration         1370 MCMCOBJ=   -6598.36670968082     
 iteration         1371 MCMCOBJ=   -6579.29182358167     
 iteration         1372 MCMCOBJ=   -6637.31423864896     
 iteration         1373 MCMCOBJ=   -6605.14983120183     
 iteration         1374 MCMCOBJ=   -6610.03320713648     
 iteration         1375 MCMCOBJ=   -6616.91573114604     
 iteration         1376 MCMCOBJ=   -6591.47952730017     
 iteration         1377 MCMCOBJ=   -6600.52674379008     
 iteration         1378 MCMCOBJ=   -6591.66481901961     
 iteration         1379 MCMCOBJ=   -6606.94115677451     
 iteration         1380 MCMCOBJ=   -6525.57583271903     
 iteration         1381 MCMCOBJ=   -6591.64706432376     
 iteration         1382 MCMCOBJ=   -6556.09485648018     
 iteration         1383 MCMCOBJ=   -6566.77043320669     
 iteration         1384 MCMCOBJ=   -6614.95901038742     
 iteration         1385 MCMCOBJ=   -6590.73199941968     
 iteration         1386 MCMCOBJ=   -6612.32739088870     
 iteration         1387 MCMCOBJ=   -6591.23012954356     
 iteration         1388 MCMCOBJ=   -6624.83059070469     
 iteration         1389 MCMCOBJ=   -6591.97541931604     
 iteration         1390 MCMCOBJ=   -6545.26001730667     
 iteration         1391 MCMCOBJ=   -6581.25471664523     
 iteration         1392 MCMCOBJ=   -6528.65006469488     
 iteration         1393 MCMCOBJ=   -6558.78745516846     
 iteration         1394 MCMCOBJ=   -6603.34474368058     
 iteration         1395 MCMCOBJ=   -6599.24189122947     
 iteration         1396 MCMCOBJ=   -6586.23463792776     
 iteration         1397 MCMCOBJ=   -6615.41775038629     
 iteration         1398 MCMCOBJ=   -6607.06887074991     
 iteration         1399 MCMCOBJ=   -6624.93054432698     
 iteration         1400 MCMCOBJ=   -6607.90125344934     
 iteration         1401 MCMCOBJ=   -6602.50391304523     
 iteration         1402 MCMCOBJ=   -6614.31238943012     
 iteration         1403 MCMCOBJ=   -6623.07073206799     
 iteration         1404 MCMCOBJ=   -6598.66253327902     
 iteration         1405 MCMCOBJ=   -6606.40772203825     
 iteration         1406 MCMCOBJ=   -6664.43449996043     
 iteration         1407 MCMCOBJ=   -6657.01738623736     
 iteration         1408 MCMCOBJ=   -6665.01870534741     
 iteration         1409 MCMCOBJ=   -6617.03619509993     
 iteration         1410 MCMCOBJ=   -6658.58196083919     
 iteration         1411 MCMCOBJ=   -6643.10920241757     
 iteration         1412 MCMCOBJ=   -6638.24792442423     
 iteration         1413 MCMCOBJ=   -6588.91850126358     
 iteration         1414 MCMCOBJ=   -6586.81093571959     
 iteration         1415 MCMCOBJ=   -6594.25204631829     
 iteration         1416 MCMCOBJ=   -6627.40015270425     
 iteration         1417 MCMCOBJ=   -6575.43964381356     
 iteration         1418 MCMCOBJ=   -6578.84549292691     
 iteration         1419 MCMCOBJ=   -6500.62825317948     
 iteration         1420 MCMCOBJ=   -6488.67816822927     
 iteration         1421 MCMCOBJ=   -6564.01249939778     
 iteration         1422 MCMCOBJ=   -6564.41337233488     
 iteration         1423 MCMCOBJ=   -6560.59671855234     
 iteration         1424 MCMCOBJ=   -6563.72693875697     
 iteration         1425 MCMCOBJ=   -6536.21759816796     
 iteration         1426 MCMCOBJ=   -6576.04820623166     
 iteration         1427 MCMCOBJ=   -6553.81508001635     
 iteration         1428 MCMCOBJ=   -6600.88539861824     
 iteration         1429 MCMCOBJ=   -6627.45995185480     
 iteration         1430 MCMCOBJ=   -6628.96258765366     
 iteration         1431 MCMCOBJ=   -6628.97935586695     
 iteration         1432 MCMCOBJ=   -6639.35436973719     
 iteration         1433 MCMCOBJ=   -6592.88072713137     
 iteration         1434 MCMCOBJ=   -6615.89339807144     
 iteration         1435 MCMCOBJ=   -6632.56596534647     
 iteration         1436 MCMCOBJ=   -6554.76573245130     
 iteration         1437 MCMCOBJ=   -6588.95005124966     
 iteration         1438 MCMCOBJ=   -6587.46001158198     
 iteration         1439 MCMCOBJ=   -6596.76130741826     
 iteration         1440 MCMCOBJ=   -6583.11609794918     
 iteration         1441 MCMCOBJ=   -6565.87371408418     
 iteration         1442 MCMCOBJ=   -6544.37529003962     
 iteration         1443 MCMCOBJ=   -6500.65650823924     
 iteration         1444 MCMCOBJ=   -6534.07475028640     
 iteration         1445 MCMCOBJ=   -6596.80693764751     
 iteration         1446 MCMCOBJ=   -6633.54148425062     
 iteration         1447 MCMCOBJ=   -6624.04874543168     
 iteration         1448 MCMCOBJ=   -6615.45683417600     
 iteration         1449 MCMCOBJ=   -6656.45199997088     
 iteration         1450 MCMCOBJ=   -6607.50307211842     
 iteration         1451 MCMCOBJ=   -6571.94421840650     
 iteration         1452 MCMCOBJ=   -6608.66872978712     
 iteration         1453 MCMCOBJ=   -6607.95813327565     
 iteration         1454 MCMCOBJ=   -6628.91128552481     
 iteration         1455 MCMCOBJ=   -6626.14875121910     
 iteration         1456 MCMCOBJ=   -6637.80821616434     
 iteration         1457 MCMCOBJ=   -6576.01085301894     
 iteration         1458 MCMCOBJ=   -6582.67879115439     
 iteration         1459 MCMCOBJ=   -6578.14794461501     
 iteration         1460 MCMCOBJ=   -6580.45585982238     
 iteration         1461 MCMCOBJ=   -6580.45585986657     
 iteration         1462 MCMCOBJ=   -6586.02703957120     
 iteration         1463 MCMCOBJ=   -6627.16959485147     
 iteration         1464 MCMCOBJ=   -6570.58156015775     
 iteration         1465 MCMCOBJ=   -6577.46194062433     
 iteration         1466 MCMCOBJ=   -6607.30664973807     
 iteration         1467 MCMCOBJ=   -6601.67289980980     
 iteration         1468 MCMCOBJ=   -6646.64797644855     
 iteration         1469 MCMCOBJ=   -6609.14009141044     
 iteration         1470 MCMCOBJ=   -6604.88521451115     
 iteration         1471 MCMCOBJ=   -6598.38604291080     
 iteration         1472 MCMCOBJ=   -6602.32763118441     
 iteration         1473 MCMCOBJ=   -6607.97981293519     
 iteration         1474 MCMCOBJ=   -6607.97981266571     
 iteration         1475 MCMCOBJ=   -6616.08471973633     
 iteration         1476 MCMCOBJ=   -6642.58423120136     
 iteration         1477 MCMCOBJ=   -6624.98096534937     
 iteration         1478 MCMCOBJ=   -6604.46449789782     
 iteration         1479 MCMCOBJ=   -6581.55716439099     
 iteration         1480 MCMCOBJ=   -6565.03602947100     
 iteration         1481 MCMCOBJ=   -6585.66204946502     
 iteration         1482 MCMCOBJ=   -6560.61169679303     
 iteration         1483 MCMCOBJ=   -6593.48688822939     
 iteration         1484 MCMCOBJ=   -6593.48681916535     
 iteration         1485 MCMCOBJ=   -6604.64976967177     
 iteration         1486 MCMCOBJ=   -6555.15490293047     
 iteration         1487 MCMCOBJ=   -6635.74807389098     
 iteration         1488 MCMCOBJ=   -6584.33448891102     
 iteration         1489 MCMCOBJ=   -6586.11948856405     
 iteration         1490 MCMCOBJ=   -6614.17592412558     
 iteration         1491 MCMCOBJ=   -6630.04028266214     
 iteration         1492 MCMCOBJ=   -6658.33172248683     
 iteration         1493 MCMCOBJ=   -6594.57465760843     
 iteration         1494 MCMCOBJ=   -6566.68159356511     
 iteration         1495 MCMCOBJ=   -6512.91738040921     
 iteration         1496 MCMCOBJ=   -6542.47467267563     
 iteration         1497 MCMCOBJ=   -6561.14963405469     
 iteration         1498 MCMCOBJ=   -6563.00555443529     
 iteration         1499 MCMCOBJ=   -6524.11370373636     
 iteration         1500 MCMCOBJ=   -6624.72661031326     
 iteration         1501 MCMCOBJ=   -6605.24273966384     
 iteration         1502 MCMCOBJ=   -6608.13600907641     
 iteration         1503 MCMCOBJ=   -6582.61581419222     
 iteration         1504 MCMCOBJ=   -6582.70276164655     
 iteration         1505 MCMCOBJ=   -6596.78479008686     
 iteration         1506 MCMCOBJ=   -6583.22242703633     
 iteration         1507 MCMCOBJ=   -6599.20884237734     
 iteration         1508 MCMCOBJ=   -6580.56727207389     
 iteration         1509 MCMCOBJ=   -6583.16160936607     
 iteration         1510 MCMCOBJ=   -6586.47615130978     
 iteration         1511 MCMCOBJ=   -6590.21467786166     
 iteration         1512 MCMCOBJ=   -6597.44599143741     
 iteration         1513 MCMCOBJ=   -6599.53053080332     
 iteration         1514 MCMCOBJ=   -6586.71579420048     
 iteration         1515 MCMCOBJ=   -6647.83404285817     
 iteration         1516 MCMCOBJ=   -6645.91489149208     
 iteration         1517 MCMCOBJ=   -6610.35709341058     
 iteration         1518 MCMCOBJ=   -6567.94560040711     
 iteration         1519 MCMCOBJ=   -6599.82333078730     
 iteration         1520 MCMCOBJ=   -6599.82333439592     
 iteration         1521 MCMCOBJ=   -6626.57303851048     
 iteration         1522 MCMCOBJ=   -6646.56906209236     
 iteration         1523 MCMCOBJ=   -6630.16059464266     
 iteration         1524 MCMCOBJ=   -6619.43520459574     
 iteration         1525 MCMCOBJ=   -6600.58039772231     
 iteration         1526 MCMCOBJ=   -6634.41119469907     
 iteration         1527 MCMCOBJ=   -6621.23945971900     
 iteration         1528 MCMCOBJ=   -6631.91740877466     
 iteration         1529 MCMCOBJ=   -6624.92383457317     
 iteration         1530 MCMCOBJ=   -6597.82254776076     
 iteration         1531 MCMCOBJ=   -6626.75531174529     
 iteration         1532 MCMCOBJ=   -6599.21961991006     
 iteration         1533 MCMCOBJ=   -6603.10244062126     
 iteration         1534 MCMCOBJ=   -6615.59665462670     
 iteration         1535 MCMCOBJ=   -6633.35554654466     
 iteration         1536 MCMCOBJ=   -6645.58158700903     
 iteration         1537 MCMCOBJ=   -6618.94852975060     
 iteration         1538 MCMCOBJ=   -6575.93084431056     
 iteration         1539 MCMCOBJ=   -6561.86263500254     
 iteration         1540 MCMCOBJ=   -6567.50157430094     
 iteration         1541 MCMCOBJ=   -6595.62366893041     
 iteration         1542 MCMCOBJ=   -6619.88185832174     
 iteration         1543 MCMCOBJ=   -6602.59616816663     
 iteration         1544 MCMCOBJ=   -6622.76459699734     
 iteration         1545 MCMCOBJ=   -6643.63925317031     
 iteration         1546 MCMCOBJ=   -6643.63924770381     
 iteration         1547 MCMCOBJ=   -6675.08269864689     
 iteration         1548 MCMCOBJ=   -6635.73953188521     
 iteration         1549 MCMCOBJ=   -6615.30954163469     
 iteration         1550 MCMCOBJ=   -6608.89002429032     
 iteration         1551 MCMCOBJ=   -6618.25295408714     
 iteration         1552 MCMCOBJ=   -6642.97931032868     
 iteration         1553 MCMCOBJ=   -6631.15511031819     
 iteration         1554 MCMCOBJ=   -6604.43846432025     
 iteration         1555 MCMCOBJ=   -6552.50858266532     
 iteration         1556 MCMCOBJ=   -6599.81653804934     
 iteration         1557 MCMCOBJ=   -6621.42589524752     
 iteration         1558 MCMCOBJ=   -6598.91397619096     
 iteration         1559 MCMCOBJ=   -6544.10964144430     
 iteration         1560 MCMCOBJ=   -6550.42698946870     
 iteration         1561 MCMCOBJ=   -6547.43073267744     
 iteration         1562 MCMCOBJ=   -6557.41420505213     
 iteration         1563 MCMCOBJ=   -6585.32189039875     
 iteration         1564 MCMCOBJ=   -6585.32189105715     
 iteration         1565 MCMCOBJ=   -6577.75622014866     
 iteration         1566 MCMCOBJ=   -6664.46934308167     
 iteration         1567 MCMCOBJ=   -6674.57171357720     
 iteration         1568 MCMCOBJ=   -6665.98705251220     
 iteration         1569 MCMCOBJ=   -6620.52775108856     
 iteration         1570 MCMCOBJ=   -6646.25178875615     
 iteration         1571 MCMCOBJ=   -6612.01876036432     
 iteration         1572 MCMCOBJ=   -6605.80135371668     
 iteration         1573 MCMCOBJ=   -6561.34211184744     
 iteration         1574 MCMCOBJ=   -6549.22421534048     
 iteration         1575 MCMCOBJ=   -6538.57931061900     
 iteration         1576 MCMCOBJ=   -6571.36084351813     
 iteration         1577 MCMCOBJ=   -6577.81103024010     
 iteration         1578 MCMCOBJ=   -6607.74654628555     
 iteration         1579 MCMCOBJ=   -6604.84116259877     
 iteration         1580 MCMCOBJ=   -6612.50538328570     
 iteration         1581 MCMCOBJ=   -6503.31817158807     
 iteration         1582 MCMCOBJ=   -6530.79596695554     
 iteration         1583 MCMCOBJ=   -6565.82596982765     
 iteration         1584 MCMCOBJ=   -6645.13703176187     
 iteration         1585 MCMCOBJ=   -6665.98915479955     
 iteration         1586 MCMCOBJ=   -6641.28164760113     
 iteration         1587 MCMCOBJ=   -6658.62765134784     
 iteration         1588 MCMCOBJ=   -6649.77181433254     
 iteration         1589 MCMCOBJ=   -6629.18291517628     
 iteration         1590 MCMCOBJ=   -6595.59589861281     
 iteration         1591 MCMCOBJ=   -6605.19519930476     
 iteration         1592 MCMCOBJ=   -6605.70650897986     
 iteration         1593 MCMCOBJ=   -6630.88826751541     
 iteration         1594 MCMCOBJ=   -6586.65154252907     
 iteration         1595 MCMCOBJ=   -6642.35088941751     
 iteration         1596 MCMCOBJ=   -6649.53103478645     
 iteration         1597 MCMCOBJ=   -6613.27113560039     
 iteration         1598 MCMCOBJ=   -6569.70039619441     
 iteration         1599 MCMCOBJ=   -6561.34444973741     
 iteration         1600 MCMCOBJ=   -6607.73937579429     
 iteration         1601 MCMCOBJ=   -6620.46093625582     
 iteration         1602 MCMCOBJ=   -6633.78847319365     
 iteration         1603 MCMCOBJ=   -6658.43777943551     
 iteration         1604 MCMCOBJ=   -6617.77219850522     
 iteration         1605 MCMCOBJ=   -6584.77174209570     
 iteration         1606 MCMCOBJ=   -6567.62907452671     
 iteration         1607 MCMCOBJ=   -6574.41608455452     
 iteration         1608 MCMCOBJ=   -6617.05437523272     
 iteration         1609 MCMCOBJ=   -6614.54343031803     
 iteration         1610 MCMCOBJ=   -6550.58345354789     
 iteration         1611 MCMCOBJ=   -6566.54060707394     
 iteration         1612 MCMCOBJ=   -6566.94502372304     
 iteration         1613 MCMCOBJ=   -6569.79406748221     
 iteration         1614 MCMCOBJ=   -6543.95875127932     
 iteration         1615 MCMCOBJ=   -6516.75655270862     
 iteration         1616 MCMCOBJ=   -6499.62305191004     
 iteration         1617 MCMCOBJ=   -6591.76130108732     
 iteration         1618 MCMCOBJ=   -6593.32181033499     
 iteration         1619 MCMCOBJ=   -6609.02614271435     
 iteration         1620 MCMCOBJ=   -6642.59775608866     
 iteration         1621 MCMCOBJ=   -6608.69584851266     
 iteration         1622 MCMCOBJ=   -6576.52219994646     
 iteration         1623 MCMCOBJ=   -6599.60309334743     
 iteration         1624 MCMCOBJ=   -6625.16519864944     
 iteration         1625 MCMCOBJ=   -6614.71706009476     
 iteration         1626 MCMCOBJ=   -6639.05353006667     
 iteration         1627 MCMCOBJ=   -6647.02882899866     
 iteration         1628 MCMCOBJ=   -6609.21365839827     
 iteration         1629 MCMCOBJ=   -6598.31909116436     
 iteration         1630 MCMCOBJ=   -6610.79489347952     
 iteration         1631 MCMCOBJ=   -6637.74922133264     
 iteration         1632 MCMCOBJ=   -6629.88250394899     
 iteration         1633 MCMCOBJ=   -6614.44384181952     
 iteration         1634 MCMCOBJ=   -6647.20526395818     
 iteration         1635 MCMCOBJ=   -6647.90800979978     
 iteration         1636 MCMCOBJ=   -6590.24831278310     
 iteration         1637 MCMCOBJ=   -6592.48192530393     
 iteration         1638 MCMCOBJ=   -6583.70800729133     
 iteration         1639 MCMCOBJ=   -6574.40539009929     
 iteration         1640 MCMCOBJ=   -6579.40656492765     
 iteration         1641 MCMCOBJ=   -6583.07493985338     
 iteration         1642 MCMCOBJ=   -6513.85999932818     
 iteration         1643 MCMCOBJ=   -6563.68295469497     
 iteration         1644 MCMCOBJ=   -6572.16652641384     
 iteration         1645 MCMCOBJ=   -6621.28691095366     
 iteration         1646 MCMCOBJ=   -6621.28690149608     
 iteration         1647 MCMCOBJ=   -6634.26213794139     
 iteration         1648 MCMCOBJ=   -6640.27310188670     
 iteration         1649 MCMCOBJ=   -6666.19784476892     
 iteration         1650 MCMCOBJ=   -6646.92650079420     
 iteration         1651 MCMCOBJ=   -6646.92650055870     
 iteration         1652 MCMCOBJ=   -6591.01883688185     
 iteration         1653 MCMCOBJ=   -6583.06236908099     
 iteration         1654 MCMCOBJ=   -6603.69368926443     
 iteration         1655 MCMCOBJ=   -6581.98957279302     
 iteration         1656 MCMCOBJ=   -6555.35110159358     
 iteration         1657 MCMCOBJ=   -6564.44965183826     
 iteration         1658 MCMCOBJ=   -6612.73918475078     
 iteration         1659 MCMCOBJ=   -6614.37714818015     
 iteration         1660 MCMCOBJ=   -6584.15912180265     
 iteration         1661 MCMCOBJ=   -6578.41785063697     
 iteration         1662 MCMCOBJ=   -6558.27839119265     
 iteration         1663 MCMCOBJ=   -6496.84544538448     
 iteration         1664 MCMCOBJ=   -6564.40272754202     
 iteration         1665 MCMCOBJ=   -6627.42377780432     
 iteration         1666 MCMCOBJ=   -6646.56880225828     
 iteration         1667 MCMCOBJ=   -6589.61990528073     
 iteration         1668 MCMCOBJ=   -6618.95637839485     
 iteration         1669 MCMCOBJ=   -6601.39960411451     
 iteration         1670 MCMCOBJ=   -6623.25592374933     
 iteration         1671 MCMCOBJ=   -6604.32697478789     
 iteration         1672 MCMCOBJ=   -6617.41853126903     
 iteration         1673 MCMCOBJ=   -6600.38602111771     
 iteration         1674 MCMCOBJ=   -6627.48879679518     
 iteration         1675 MCMCOBJ=   -6608.64475909325     
 iteration         1676 MCMCOBJ=   -6606.59481920868     
 iteration         1677 MCMCOBJ=   -6606.59481943414     
 iteration         1678 MCMCOBJ=   -6619.96956402289     
 iteration         1679 MCMCOBJ=   -6592.49144628513     
 iteration         1680 MCMCOBJ=   -6623.59470409597     
 iteration         1681 MCMCOBJ=   -6628.84017788544     
 iteration         1682 MCMCOBJ=   -6625.55265680771     
 iteration         1683 MCMCOBJ=   -6619.70994705770     
 iteration         1684 MCMCOBJ=   -6617.88064069355     
 iteration         1685 MCMCOBJ=   -6620.64130164882     
 iteration         1686 MCMCOBJ=   -6617.42230693941     
 iteration         1687 MCMCOBJ=   -6575.16454182270     
 iteration         1688 MCMCOBJ=   -6548.43507935876     
 iteration         1689 MCMCOBJ=   -6515.11136785040     
 iteration         1690 MCMCOBJ=   -6569.42522205879     
 iteration         1691 MCMCOBJ=   -6570.23553368653     
 iteration         1692 MCMCOBJ=   -6544.50965243557     
 iteration         1693 MCMCOBJ=   -6541.80313204937     
 iteration         1694 MCMCOBJ=   -6562.76555604073     
 iteration         1695 MCMCOBJ=   -6575.46581034351     
 iteration         1696 MCMCOBJ=   -6573.23025518636     
 iteration         1697 MCMCOBJ=   -6590.51614873063     
 iteration         1698 MCMCOBJ=   -6575.87270055834     
 iteration         1699 MCMCOBJ=   -6585.54081932314     
 iteration         1700 MCMCOBJ=   -6598.12988313507     
 iteration         1701 MCMCOBJ=   -6623.36072214755     
 iteration         1702 MCMCOBJ=   -6617.29209803849     
 iteration         1703 MCMCOBJ=   -6615.31034185511     
 iteration         1704 MCMCOBJ=   -6615.08503862157     
 iteration         1705 MCMCOBJ=   -6611.00042753576     
 iteration         1706 MCMCOBJ=   -6588.76637186862     
 iteration         1707 MCMCOBJ=   -6624.39723550007     
 iteration         1708 MCMCOBJ=   -6562.66585277899     
 iteration         1709 MCMCOBJ=   -6557.22286296534     
 iteration         1710 MCMCOBJ=   -6587.55660377127     
 iteration         1711 MCMCOBJ=   -6584.06559257433     
 iteration         1712 MCMCOBJ=   -6595.76412615809     
 iteration         1713 MCMCOBJ=   -6661.26973524420     
 iteration         1714 MCMCOBJ=   -6667.69550883923     
 iteration         1715 MCMCOBJ=   -6661.79578806465     
 iteration         1716 MCMCOBJ=   -6629.52130618447     
 iteration         1717 MCMCOBJ=   -6641.04857423898     
 iteration         1718 MCMCOBJ=   -6650.54567304311     
 iteration         1719 MCMCOBJ=   -6610.55068584541     
 iteration         1720 MCMCOBJ=   -6570.63159075443     
 iteration         1721 MCMCOBJ=   -6571.55250143147     
 iteration         1722 MCMCOBJ=   -6593.93644050468     
 iteration         1723 MCMCOBJ=   -6639.20183968912     
 iteration         1724 MCMCOBJ=   -6624.39304218717     
 iteration         1725 MCMCOBJ=   -6571.12014317637     
 iteration         1726 MCMCOBJ=   -6634.08718568806     
 iteration         1727 MCMCOBJ=   -6650.33629837782     
 iteration         1728 MCMCOBJ=   -6637.55593982228     
 iteration         1729 MCMCOBJ=   -6637.55592741683     
 iteration         1730 MCMCOBJ=   -6577.77877898784     
 iteration         1731 MCMCOBJ=   -6587.04286781838     
 iteration         1732 MCMCOBJ=   -6588.43949096403     
 iteration         1733 MCMCOBJ=   -6552.22823851223     
 iteration         1734 MCMCOBJ=   -6564.43649142698     
 iteration         1735 MCMCOBJ=   -6591.53576403469     
 iteration         1736 MCMCOBJ=   -6629.29293917294     
 iteration         1737 MCMCOBJ=   -6599.50125563263     
 iteration         1738 MCMCOBJ=   -6623.90810617814     
 iteration         1739 MCMCOBJ=   -6554.70782150394     
 iteration         1740 MCMCOBJ=   -6599.17365932529     
 iteration         1741 MCMCOBJ=   -6563.55147793453     
 iteration         1742 MCMCOBJ=   -6633.48436575096     
 iteration         1743 MCMCOBJ=   -6628.29592527059     
 iteration         1744 MCMCOBJ=   -6534.67103078396     
 iteration         1745 MCMCOBJ=   -6536.27543709202     
 iteration         1746 MCMCOBJ=   -6604.45782506387     
 iteration         1747 MCMCOBJ=   -6607.23149258348     
 iteration         1748 MCMCOBJ=   -6641.01915421818     
 iteration         1749 MCMCOBJ=   -6636.62688012363     
 iteration         1750 MCMCOBJ=   -6590.19752532192     
 iteration         1751 MCMCOBJ=   -6623.47184302362     
 iteration         1752 MCMCOBJ=   -6597.93056993162     
 iteration         1753 MCMCOBJ=   -6564.35185681112     
 iteration         1754 MCMCOBJ=   -6604.85177873939     
 iteration         1755 MCMCOBJ=   -6609.96384739584     
 iteration         1756 MCMCOBJ=   -6583.86972851231     
 iteration         1757 MCMCOBJ=   -6606.92757197861     
 iteration         1758 MCMCOBJ=   -6608.46663198356     
 iteration         1759 MCMCOBJ=   -6660.10372494331     
 iteration         1760 MCMCOBJ=   -6601.78190118642     
 iteration         1761 MCMCOBJ=   -6611.14276793714     
 iteration         1762 MCMCOBJ=   -6597.16598659165     
 iteration         1763 MCMCOBJ=   -6605.91935206043     
 iteration         1764 MCMCOBJ=   -6513.85145584710     
 iteration         1765 MCMCOBJ=   -6608.06830725239     
 iteration         1766 MCMCOBJ=   -6550.49418051308     
 iteration         1767 MCMCOBJ=   -6565.39437873486     
 iteration         1768 MCMCOBJ=   -6554.34505502163     
 iteration         1769 MCMCOBJ=   -6597.06761098976     
 iteration         1770 MCMCOBJ=   -6552.82638819594     
 iteration         1771 MCMCOBJ=   -6584.79870436580     
 iteration         1772 MCMCOBJ=   -6597.63885578711     
 iteration         1773 MCMCOBJ=   -6573.39005684048     
 iteration         1774 MCMCOBJ=   -6517.34937379748     
 iteration         1775 MCMCOBJ=   -6544.48039668102     
 iteration         1776 MCMCOBJ=   -6569.99418344104     
 iteration         1777 MCMCOBJ=   -6552.64779375516     
 iteration         1778 MCMCOBJ=   -6579.16971338436     
 iteration         1779 MCMCOBJ=   -6607.78205401822     
 iteration         1780 MCMCOBJ=   -6636.00341253570     
 iteration         1781 MCMCOBJ=   -6629.02299873571     
 iteration         1782 MCMCOBJ=   -6648.55133800283     
 iteration         1783 MCMCOBJ=   -6631.67633080640     
 iteration         1784 MCMCOBJ=   -6594.17860809110     
 iteration         1785 MCMCOBJ=   -6571.27329822936     
 iteration         1786 MCMCOBJ=   -6581.00424463585     
 iteration         1787 MCMCOBJ=   -6602.26476047844     
 iteration         1788 MCMCOBJ=   -6612.61483146026     
 iteration         1789 MCMCOBJ=   -6585.41191808913     
 iteration         1790 MCMCOBJ=   -6585.41201574842     
 iteration         1791 MCMCOBJ=   -6537.64491043918     
 iteration         1792 MCMCOBJ=   -6521.72040544351     
 iteration         1793 MCMCOBJ=   -6548.87472000010     
 iteration         1794 MCMCOBJ=   -6580.57669396712     
 iteration         1795 MCMCOBJ=   -6605.37320410668     
 iteration         1796 MCMCOBJ=   -6602.57720947131     
 iteration         1797 MCMCOBJ=   -6589.30082262981     
 iteration         1798 MCMCOBJ=   -6596.45590925077     
 iteration         1799 MCMCOBJ=   -6577.75089904151     
 iteration         1800 MCMCOBJ=   -6611.53448668724     
 iteration         1801 MCMCOBJ=   -6586.99335764780     
 iteration         1802 MCMCOBJ=   -6563.93437757095     
 iteration         1803 MCMCOBJ=   -6548.66031414980     
 iteration         1804 MCMCOBJ=   -6547.24377697682     
 iteration         1805 MCMCOBJ=   -6566.15294671875     
 iteration         1806 MCMCOBJ=   -6561.66114252981     
 iteration         1807 MCMCOBJ=   -6522.40851890633     
 iteration         1808 MCMCOBJ=   -6647.04176706451     
 iteration         1809 MCMCOBJ=   -6644.80310465671     
 iteration         1810 MCMCOBJ=   -6618.77458272553     
 iteration         1811 MCMCOBJ=   -6637.72371878040     
 iteration         1812 MCMCOBJ=   -6639.39722708537     
 iteration         1813 MCMCOBJ=   -6639.39722690157     
 iteration         1814 MCMCOBJ=   -6640.79421444236     
 iteration         1815 MCMCOBJ=   -6635.04524614604     
 iteration         1816 MCMCOBJ=   -6664.50291753311     
 iteration         1817 MCMCOBJ=   -6599.36841070902     
 iteration         1818 MCMCOBJ=   -6560.75805826068     
 iteration         1819 MCMCOBJ=   -6548.17127442455     
 iteration         1820 MCMCOBJ=   -6601.66954855436     
 iteration         1821 MCMCOBJ=   -6586.45890937583     
 iteration         1822 MCMCOBJ=   -6603.23715056703     
 iteration         1823 MCMCOBJ=   -6611.66722006436     
 iteration         1824 MCMCOBJ=   -6576.74147844826     
 iteration         1825 MCMCOBJ=   -6581.38828843343     
 iteration         1826 MCMCOBJ=   -6584.61805959635     
 iteration         1827 MCMCOBJ=   -6608.24203200483     
 iteration         1828 MCMCOBJ=   -6606.70320989804     
 iteration         1829 MCMCOBJ=   -6524.04844404870     
 iteration         1830 MCMCOBJ=   -6545.47102592394     
 iteration         1831 MCMCOBJ=   -6510.48203390945     
 iteration         1832 MCMCOBJ=   -6582.93471009980     
 iteration         1833 MCMCOBJ=   -6513.10037207233     
 iteration         1834 MCMCOBJ=   -6552.66334500637     
 iteration         1835 MCMCOBJ=   -6523.86725958724     
 iteration         1836 MCMCOBJ=   -6516.76599121001     
 iteration         1837 MCMCOBJ=   -6561.78304877526     
 iteration         1838 MCMCOBJ=   -6610.98142095604     
 iteration         1839 MCMCOBJ=   -6570.50475230153     
 iteration         1840 MCMCOBJ=   -6536.43674974350     
 iteration         1841 MCMCOBJ=   -6528.90759971236     
 iteration         1842 MCMCOBJ=   -6521.26322171831     
 iteration         1843 MCMCOBJ=   -6542.56873203112     
 iteration         1844 MCMCOBJ=   -6634.54394433339     
 iteration         1845 MCMCOBJ=   -6614.40869646193     
 iteration         1846 MCMCOBJ=   -6619.02968810960     
 iteration         1847 MCMCOBJ=   -6630.97694755710     
 iteration         1848 MCMCOBJ=   -6626.50548553647     
 iteration         1849 MCMCOBJ=   -6581.99890681438     
 iteration         1850 MCMCOBJ=   -6574.33197065874     
 iteration         1851 MCMCOBJ=   -6532.89241377220     
 iteration         1852 MCMCOBJ=   -6574.13699147064     
 iteration         1853 MCMCOBJ=   -6544.44792333181     
 iteration         1854 MCMCOBJ=   -6587.97569807680     
 iteration         1855 MCMCOBJ=   -6587.89466156111     
 iteration         1856 MCMCOBJ=   -6648.77251507360     
 iteration         1857 MCMCOBJ=   -6666.82472585006     
 iteration         1858 MCMCOBJ=   -6607.60971523538     
 iteration         1859 MCMCOBJ=   -6610.28534743938     
 iteration         1860 MCMCOBJ=   -6592.47399024803     
 iteration         1861 MCMCOBJ=   -6604.56251224952     
 iteration         1862 MCMCOBJ=   -6602.36319342324     
 iteration         1863 MCMCOBJ=   -6596.20215607534     
 iteration         1864 MCMCOBJ=   -6583.44043549816     
 iteration         1865 MCMCOBJ=   -6604.71647396689     
 iteration         1866 MCMCOBJ=   -6563.16953105571     
 iteration         1867 MCMCOBJ=   -6593.98610440293     
 iteration         1868 MCMCOBJ=   -6529.89276863286     
 iteration         1869 MCMCOBJ=   -6541.65341079482     
 iteration         1870 MCMCOBJ=   -6567.87075882651     
 iteration         1871 MCMCOBJ=   -6555.83522630437     
 iteration         1872 MCMCOBJ=   -6577.83235622953     
 iteration         1873 MCMCOBJ=   -6593.76879919150     
 iteration         1874 MCMCOBJ=   -6595.22581714633     
 iteration         1875 MCMCOBJ=   -6586.73854818459     
 iteration         1876 MCMCOBJ=   -6625.95343142199     
 iteration         1877 MCMCOBJ=   -6583.52459203335     
 iteration         1878 MCMCOBJ=   -6528.00709179143     
 iteration         1879 MCMCOBJ=   -6577.47876665306     
 iteration         1880 MCMCOBJ=   -6622.53203054742     
 iteration         1881 MCMCOBJ=   -6649.91596986652     
 iteration         1882 MCMCOBJ=   -6619.53370037615     
 iteration         1883 MCMCOBJ=   -6572.90682046692     
 iteration         1884 MCMCOBJ=   -6581.33453636912     
 iteration         1885 MCMCOBJ=   -6581.64051778056     
 iteration         1886 MCMCOBJ=   -6570.59384195268     
 iteration         1887 MCMCOBJ=   -6546.08238632488     
 iteration         1888 MCMCOBJ=   -6531.53000145192     
 iteration         1889 MCMCOBJ=   -6578.10416582297     
 iteration         1890 MCMCOBJ=   -6563.78940105682     
 iteration         1891 MCMCOBJ=   -6561.55137373313     
 iteration         1892 MCMCOBJ=   -6545.03163827141     
 iteration         1893 MCMCOBJ=   -6592.17384022336     
 iteration         1894 MCMCOBJ=   -6610.04448366974     
 iteration         1895 MCMCOBJ=   -6645.30504946501     
 iteration         1896 MCMCOBJ=   -6618.65991922999     
 iteration         1897 MCMCOBJ=   -6601.74051455144     
 iteration         1898 MCMCOBJ=   -6649.68907257488     
 iteration         1899 MCMCOBJ=   -6627.56332602833     
 iteration         1900 MCMCOBJ=   -6646.97293793849     
 iteration         1901 MCMCOBJ=   -6653.98121077548     
 iteration         1902 MCMCOBJ=   -6620.35986315433     
 iteration         1903 MCMCOBJ=   -6621.39411194075     
 iteration         1904 MCMCOBJ=   -6610.85162912842     
 iteration         1905 MCMCOBJ=   -6569.84361122457     
 iteration         1906 MCMCOBJ=   -6604.76752519048     
 iteration         1907 MCMCOBJ=   -6603.09727132266     
 iteration         1908 MCMCOBJ=   -6627.89873624902     
 iteration         1909 MCMCOBJ=   -6673.38095923687     
 iteration         1910 MCMCOBJ=   -6657.72676595534     
 iteration         1911 MCMCOBJ=   -6670.44646277040     
 iteration         1912 MCMCOBJ=   -6670.44646173756     
 iteration         1913 MCMCOBJ=   -6629.88591458007     
 iteration         1914 MCMCOBJ=   -6612.11392828258     
 iteration         1915 MCMCOBJ=   -6586.40048190352     
 iteration         1916 MCMCOBJ=   -6573.37846098469     
 iteration         1917 MCMCOBJ=   -6574.69401093671     
 iteration         1918 MCMCOBJ=   -6560.84049788296     
 iteration         1919 MCMCOBJ=   -6548.29433369655     
 iteration         1920 MCMCOBJ=   -6571.58624701056     
 iteration         1921 MCMCOBJ=   -6566.84421620969     
 iteration         1922 MCMCOBJ=   -6562.88089180367     
 iteration         1923 MCMCOBJ=   -6521.15772755018     
 iteration         1924 MCMCOBJ=   -6560.21473283897     
 iteration         1925 MCMCOBJ=   -6573.79714516584     
 iteration         1926 MCMCOBJ=   -6579.58935176188     
 iteration         1927 MCMCOBJ=   -6616.85817436489     
 iteration         1928 MCMCOBJ=   -6585.34237744661     
 iteration         1929 MCMCOBJ=   -6616.62489373213     
 iteration         1930 MCMCOBJ=   -6575.76847494245     
 iteration         1931 MCMCOBJ=   -6607.99489203962     
 iteration         1932 MCMCOBJ=   -6615.20294568894     
 iteration         1933 MCMCOBJ=   -6622.32380066807     
 iteration         1934 MCMCOBJ=   -6625.67786008307     
 iteration         1935 MCMCOBJ=   -6601.05397181643     
 iteration         1936 MCMCOBJ=   -6602.61890054287     
 iteration         1937 MCMCOBJ=   -6619.86723132275     
 iteration         1938 MCMCOBJ=   -6600.43978728634     
 iteration         1939 MCMCOBJ=   -6648.66210473783     
 iteration         1940 MCMCOBJ=   -6654.72410299056     
 iteration         1941 MCMCOBJ=   -6611.81191158743     
 iteration         1942 MCMCOBJ=   -6620.14451765731     
 iteration         1943 MCMCOBJ=   -6626.63271901555     
 iteration         1944 MCMCOBJ=   -6606.20499923203     
 iteration         1945 MCMCOBJ=   -6622.38122946854     
 iteration         1946 MCMCOBJ=   -6596.35739438330     
 iteration         1947 MCMCOBJ=   -6603.77631743442     
 iteration         1948 MCMCOBJ=   -6613.09323622593     
 iteration         1949 MCMCOBJ=   -6582.90658237696     
 iteration         1950 MCMCOBJ=   -6633.35389980032     
 iteration         1951 MCMCOBJ=   -6607.15934841683     
 iteration         1952 MCMCOBJ=   -6610.29036170995     
 iteration         1953 MCMCOBJ=   -6630.50650012319     
 iteration         1954 MCMCOBJ=   -6657.19559213938     
 iteration         1955 MCMCOBJ=   -6608.09063855591     
 iteration         1956 MCMCOBJ=   -6624.33146009868     
 iteration         1957 MCMCOBJ=   -6613.74083885598     
 iteration         1958 MCMCOBJ=   -6605.30270374318     
 iteration         1959 MCMCOBJ=   -6571.24880788369     
 iteration         1960 MCMCOBJ=   -6529.79626572076     
 iteration         1961 MCMCOBJ=   -6497.26832527383     
 iteration         1962 MCMCOBJ=   -6551.15606665124     
 iteration         1963 MCMCOBJ=   -6590.32074172282     
 iteration         1964 MCMCOBJ=   -6608.71238400407     
 iteration         1965 MCMCOBJ=   -6623.12928236204     
 iteration         1966 MCMCOBJ=   -6626.54852651296     
 iteration         1967 MCMCOBJ=   -6580.86523748802     
 iteration         1968 MCMCOBJ=   -6574.60242960808     
 iteration         1969 MCMCOBJ=   -6589.20647478363     
 iteration         1970 MCMCOBJ=   -6575.16809096750     
 iteration         1971 MCMCOBJ=   -6605.82080520569     
 iteration         1972 MCMCOBJ=   -6631.69587653358     
 iteration         1973 MCMCOBJ=   -6577.84447884080     
 iteration         1974 MCMCOBJ=   -6593.54621350857     
 iteration         1975 MCMCOBJ=   -6544.73360241495     
 iteration         1976 MCMCOBJ=   -6621.96347639322     
 iteration         1977 MCMCOBJ=   -6621.96346488297     
 iteration         1978 MCMCOBJ=   -6585.37673787056     
 iteration         1979 MCMCOBJ=   -6576.19456592202     
 iteration         1980 MCMCOBJ=   -6587.11667288808     
 iteration         1981 MCMCOBJ=   -6609.67835191999     
 iteration         1982 MCMCOBJ=   -6605.33370488343     
 iteration         1983 MCMCOBJ=   -6616.54607315412     
 iteration         1984 MCMCOBJ=   -6591.78369508980     
 iteration         1985 MCMCOBJ=   -6591.00951632649     
 iteration         1986 MCMCOBJ=   -6606.80313856733     
 iteration         1987 MCMCOBJ=   -6564.01159959028     
 iteration         1988 MCMCOBJ=   -6558.14137636930     
 iteration         1989 MCMCOBJ=   -6601.56536602406     
 iteration         1990 MCMCOBJ=   -6616.44227173987     
 iteration         1991 MCMCOBJ=   -6600.04559015788     
 iteration         1992 MCMCOBJ=   -6571.04123250985     
 iteration         1993 MCMCOBJ=   -6558.51481135670     
 iteration         1994 MCMCOBJ=   -6625.61160815977     
 iteration         1995 MCMCOBJ=   -6600.40674031501     
 iteration         1996 MCMCOBJ=   -6565.68005427141     
 iteration         1997 MCMCOBJ=   -6534.64981713558     
 iteration         1998 MCMCOBJ=   -6586.92068974927     
 iteration         1999 MCMCOBJ=   -6631.92362189931     
 iteration         2000 MCMCOBJ=   -6629.52841881078     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6598.62158934764     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3716.83034921779     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6598.62158934764     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5863.47076278390     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -16.9020929079542     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6598.62158934764     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6615.52368225559     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  4214.11
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6598.622       **************************************************
 #OBJS:********************************************       35.597 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.21E+00  5.58E-01 -1.81E-01  2.27E+00  2.36E-01  3.71E+00 -7.04E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.64E-01
 
 ETA2
+       -2.97E-02  1.78E-01
 
 ETA3
+        2.82E-02 -1.11E-02  1.06E-01
 
 ETA4
+        2.28E-02  3.27E-02 -1.23E-02  2.47E-01
 
 ETA5
+        2.15E-02  1.80E-02 -5.84E-04 -2.31E-02  1.87E-01
 
 ETA6
+       -1.27E-02  1.04E-02  1.60E-02  1.13E-02 -5.53E-02  2.12E-01
 
 ETA7
+        1.31E-02 -3.61E-02  1.82E-02 -5.47E-02  1.82E-02  6.84E-03  2.24E-01
 
 ETA8
+        6.90E-02  6.11E-02  2.61E-02  3.37E-02 -6.62E-03 -4.16E-02  4.68E-02  1.92E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.40E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.11E-01
 
 ETA2
+       -1.35E-01  4.18E-01
 
 ETA3
+        1.73E-01 -8.25E-02  3.23E-01
 
 ETA4
+        8.93E-02  1.58E-01 -7.83E-02  4.94E-01
 
 ETA5
+        9.58E-02  9.97E-02 -4.89E-03 -1.08E-01  4.30E-01
 
 ETA6
+       -5.54E-02  5.63E-02  1.10E-01  5.02E-02 -2.82E-01  4.57E-01
 
 ETA7
+        5.47E-02 -1.78E-01  1.21E-01 -2.33E-01  8.88E-02  3.15E-02  4.71E-01
 
 ETA8
+        3.07E-01  3.36E-01  1.83E-01  1.54E-01 -3.42E-02 -2.08E-01  2.24E-01  4.36E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.69E-02
 
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
 
         7.23E-02  7.05E-02  5.31E-02  7.29E-02  6.25E-02  6.90E-02  6.62E-02  6.33E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.58E-02
 
 ETA2
+        2.95E-02  4.93E-02
 
 ETA3
+        2.17E-02  1.92E-02  3.09E-02
 
 ETA4
+        3.21E-02  2.86E-02  2.24E-02  5.47E-02
 
 ETA5
+        2.81E-02  2.29E-02  1.90E-02  2.66E-02  4.09E-02
 
 ETA6
+        3.06E-02  2.61E-02  2.10E-02  2.97E-02  2.69E-02  5.81E-02
 
 ETA7
+        2.81E-02  2.86E-02  1.93E-02  3.02E-02  2.59E-02  2.77E-02  4.62E-02
 
 ETA8
+        2.85E-02  2.38E-02  1.93E-02  2.70E-02  2.19E-02  2.62E-02  2.63E-02  3.85E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.45E-04
 
 EPS2
+        0.00E+00  1.17E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.29E-02
 
 ETA2
+        1.26E-01  5.67E-02
 
 ETA3
+        1.27E-01  1.37E-01  4.58E-02
 
 ETA4
+        1.20E-01  1.30E-01  1.34E-01  5.39E-02
 
 ETA5
+        1.22E-01  1.22E-01  1.32E-01  1.20E-01  4.63E-02
 
 ETA6
+        1.26E-01  1.33E-01  1.35E-01  1.25E-01  1.24E-01  6.11E-02
 
 ETA7
+        1.12E-01  1.29E-01  1.22E-01  1.16E-01  1.21E-01  1.24E-01  4.74E-02
 
 ETA8
+        1.10E-01  1.14E-01  1.24E-01  1.16E-01  1.13E-01  1.21E-01  1.12E-01  4.32E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.32E-03
 
 EPS2
+        0.00E+00  3.89E-03
 
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
+       -5.89E-04  4.97E-03
 
 TH 3
+        1.04E-04 -6.09E-05  2.82E-03
 
 TH 4
+        6.99E-04  5.69E-04  5.67E-05  5.31E-03
 
 TH 5
+        3.86E-04  2.30E-04 -8.33E-05 -3.18E-04  3.91E-03
 
 TH 6
+       -1.92E-04 -5.04E-05  2.93E-04  3.37E-04 -6.68E-04  4.76E-03
 
 TH 7
+        2.18E-04 -9.98E-04  3.79E-04 -8.99E-04  2.11E-04  4.97E-04  4.38E-03
 
 TH 8
+        1.45E-03  1.05E-03  6.02E-04  7.67E-04 -2.07E-04 -6.79E-04  9.67E-04  4.01E-03
 
 OM11
+        9.91E-05 -1.12E-04 -1.32E-04  4.31E-05  1.07E-04  3.77E-05  2.69E-04  8.84E-06  3.12E-03
 
 OM12
+       -1.06E-04  2.08E-04 -2.11E-05  1.44E-04 -6.03E-05 -1.31E-05 -9.95E-05 -1.04E-04 -3.53E-04  8.69E-04
 
 OM13
+       -1.60E-05 -5.58E-06  1.02E-04  2.95E-05 -3.17E-05 -2.00E-07  1.56E-05  7.74E-06  1.36E-05 -4.12E-05  4.72E-04
 
 OM14
+       -5.00E-05 -8.93E-06 -2.13E-05 -2.08E-05  3.21E-06 -8.85E-05 -1.18E-05  1.13E-05  1.34E-04  9.09E-05  6.72E-07  1.03E-03
 
 OM15
+        9.60E-06 -4.21E-06 -4.77E-05  7.82E-05  7.48E-05 -3.63E-05  7.23E-05 -8.78E-06  2.43E-04  1.77E-05 -1.84E-05 -1.50E-05
          7.88E-04
 
 OM16
+       -3.28E-05 -5.86E-05 -1.76E-05 -3.04E-05  3.39E-05 -9.24E-06 -1.02E-06 -9.63E-05 -7.21E-05  4.84E-05 -2.39E-06  5.67E-05
         -1.38E-04  9.36E-04
 
 OM17
+        2.48E-05 -1.92E-06 -2.04E-05  1.06E-04 -3.75E-05 -1.54E-05  6.76E-05  1.97E-05  5.73E-05 -1.27E-04  4.18E-05 -1.02E-04
          6.14E-05  3.50E-05  7.92E-04
 
 OM18
+        3.21E-05 -3.60E-05  3.48E-05  4.51E-05 -6.36E-05  1.03E-05  2.38E-06  7.22E-05  4.66E-04  1.10E-04  6.96E-05  1.31E-04
          3.44E-05 -1.58E-04  1.50E-04  8.15E-04
 
 OM22
+        6.49E-05 -7.72E-04  5.93E-05 -1.86E-05  1.50E-04  7.50E-05 -6.53E-06 -3.75E-05  7.19E-05 -3.73E-04  5.68E-06  5.67E-06
         -4.05E-05  9.00E-06  9.70E-05 -3.62E-06  2.43E-03
 
 OM23
+       -9.29E-05  1.35E-04  2.75E-05  5.33E-05 -3.35E-05  7.27E-06 -7.15E-05 -5.27E-05 -8.45E-06  5.53E-05 -2.97E-05  4.52E-06
         -4.25E-06  1.18E-05 -2.02E-05 -5.36E-06 -8.34E-05  3.70E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        9.35E-06 -2.27E-04  3.99E-06 -1.91E-06  9.04E-05  7.20E-05  1.11E-04  6.34E-05 -3.06E-06 -6.05E-05  2.38E-05 -6.74E-05
          1.27E-05 -1.61E-05  7.09E-06  2.93E-05  2.44E-04 -3.91E-05  8.17E-04
 
 OM25
+       -1.76E-05  4.07E-05 -1.65E-05 -2.15E-05  5.63E-07  2.41E-05 -5.73E-06  1.83E-05  2.45E-05 -1.67E-05  3.46E-06  1.62E-05
         -7.79E-05 -7.82E-07  2.28E-05  2.49E-07  9.73E-05 -1.83E-05 -2.62E-05  5.24E-04
 
 OM26
+       -1.11E-04  5.93E-05  5.52E-05  3.46E-05 -3.29E-05  8.88E-05 -7.60E-05 -1.24E-04  2.85E-05  1.66E-05  2.66E-05 -3.72E-05
          9.39E-06 -4.42E-05 -5.41E-06  9.59E-06 -1.82E-05  4.27E-05 -1.52E-05 -7.98E-05  6.82E-04
 
 OM27
+        1.96E-05  4.34E-04  2.82E-05  1.19E-05 -8.63E-05  4.11E-05  2.51E-05  8.67E-05 -9.32E-05  1.37E-04 -2.17E-05  8.08E-06
         -6.08E-05 -1.94E-05 -1.09E-04  1.20E-05 -5.53E-04  4.82E-05 -2.08E-04  4.79E-05  2.38E-05  8.18E-04
 
 OM28
+        4.23E-05  2.41E-05 -4.33E-06 -2.34E-05  1.02E-05  2.46E-05  3.53E-05  6.34E-05 -1.20E-04  1.19E-04  5.74E-06  4.28E-05
          1.97E-06  7.00E-06 -1.23E-05 -2.42E-05  3.45E-04  4.67E-05  7.24E-05  1.49E-06 -1.14E-04  6.14E-05  5.66E-04
 
 OM33
+        6.54E-05 -9.03E-05 -1.50E-04 -6.38E-05 -7.69E-06  3.05E-07 -3.09E-05  4.33E-05 -9.23E-05  3.63E-06  7.92E-05  3.56E-06
          1.08E-05  5.63E-05  4.10E-05 -2.08E-05  4.94E-05 -4.76E-05  1.35E-05  1.42E-05 -3.98E-06 -1.80E-05  3.37E-05  9.55E-04
 
 OM34
+       -4.52E-05  2.88E-05  1.88E-04  1.45E-04 -3.37E-05  6.30E-05 -6.93E-05 -5.06E-06 -2.77E-05 -1.44E-05  5.69E-05  1.16E-05
          1.61E-05 -2.59E-05  1.29E-05  1.98E-05  3.93E-05  3.73E-05  1.10E-05 -6.39E-06  8.88E-06 -1.45E-05  3.08E-05 -5.80E-05
         5.00E-04
 
 OM35
+        2.44E-05  9.43E-06 -5.93E-05 -7.65E-05  6.14E-05  2.11E-05  3.01E-05 -1.03E-05  3.76E-05 -1.90E-05  9.44E-06  1.91E-05
          1.62E-05 -4.80E-06 -9.11E-06 -1.53E-05 -2.68E-05  9.78E-06 -1.28E-05 -1.31E-05 -7.95E-06  6.05E-06  1.31E-06  5.34E-06
        -4.91E-05  3.61E-04
 
 OM36
+       -1.40E-05 -9.09E-05 -7.20E-06  1.90E-05 -5.53E-06 -1.02E-04 -4.13E-06  3.10E-05  4.43E-05  1.09E-06  1.57E-05  2.78E-05
         -1.20E-05  4.37E-05 -1.12E-07  1.67E-05  9.06E-06  1.57E-05  1.66E-05 -4.74E-06  2.18E-05 -6.33E-06 -1.15E-05  2.60E-05
         1.29E-05 -6.24E-05  4.42E-04
 
 OM37
+       -2.48E-06 -5.78E-05 -7.24E-05 -5.32E-05 -1.08E-05  3.30E-05  2.31E-05 -2.43E-06 -7.19E-06 -5.97E-06 -5.42E-06 -1.02E-05
          6.72E-06 -1.76E-05  3.60E-05  3.16E-05  2.97E-05 -6.47E-05  1.19E-05 -5.73E-07 -4.61E-05 -4.30E-05  1.61E-06  3.95E-05
        -5.49E-05  4.02E-05  3.90E-06  3.71E-04
 
 OM38
+       -9.00E-05  1.75E-05  2.13E-05  2.08E-05 -4.51E-05  7.42E-05 -1.89E-05 -6.36E-06 -7.69E-06 -5.16E-06  1.06E-04 -1.33E-05
         -1.99E-06 -5.43E-06  2.53E-05  4.33E-05  5.20E-05  6.90E-05  9.90E-06  8.58E-06 -1.98E-05 -2.74E-05  4.10E-05  1.81E-04
         6.19E-05 -2.15E-05 -6.19E-05  6.91E-05  3.72E-04
 
 OM44
+       -1.48E-04  4.19E-06  2.26E-04  2.27E-04 -4.93E-05  3.09E-05  2.58E-04  3.75E-05  3.01E-04  2.46E-05  7.04E-05  2.22E-04
         -7.74E-06  5.16E-05 -5.13E-05  1.04E-04  3.55E-05  6.93E-06  2.21E-04  1.45E-05  1.02E-05 -6.81E-05  4.64E-05 -8.46E-05
         3.32E-05 -5.29E-05  4.55E-05 -1.66E-05  6.23E-06  3.00E-03
 
 OM45
+        3.08E-05 -6.39E-05 -4.35E-05  1.51E-05  7.88E-06 -2.00E-05 -1.92E-05 -2.54E-05  5.12E-05  1.36E-05 -1.97E-05  1.16E-04
          2.53E-05 -3.36E-05  9.06E-06  2.65E-05  1.37E-05  1.33E-05  2.59E-05  1.98E-05  2.33E-05 -1.22E-06 -2.61E-05 -2.73E-05
        -1.23E-05 -8.37E-06  6.07E-06 -2.23E-05 -3.80E-05 -1.65E-04  7.07E-04
 
 OM46
+       -2.39E-06  1.51E-05  2.02E-06  1.34E-04 -7.91E-06  6.43E-05 -1.05E-05 -3.45E-05  2.25E-05 -2.12E-05 -1.45E-05 -4.38E-05
         -3.51E-05  3.50E-05 -1.95E-05 -7.55E-05 -7.32E-06 -1.28E-05  4.97E-05 -1.99E-05  6.13E-05 -2.89E-06  8.05E-06  1.84E-06
         1.40E-05 -4.40E-06 -1.61E-06 -1.75E-05  2.45E-05  7.22E-05 -1.06E-04  8.84E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -7.64E-05  3.94E-05 -9.26E-05 -4.22E-05 -5.62E-05  1.01E-04 -1.23E-05  1.91E-05 -3.44E-05 -1.31E-05 -7.04E-05  1.76E-05
         -9.14E-07 -7.34E-05 -6.09E-06  3.61E-05 -1.31E-05 -2.57E-06 -1.64E-04  2.37E-05 -1.04E-05  9.85E-05  1.01E-06 -1.64E-05
        -1.59E-07 -3.91E-07 -2.12E-05 -1.61E-05 -3.53E-05 -5.27E-04  5.53E-05  1.94E-05  9.15E-04
 
 OM48
+       -6.08E-05  9.04E-05 -1.47E-05  3.16E-05 -2.62E-05  1.39E-04  4.04E-05  3.58E-05  3.94E-05  1.07E-04  2.67E-05  2.09E-04
         -3.53E-05 -1.14E-05 -2.71E-05  1.36E-04  1.92E-05  1.47E-05  1.14E-04  7.70E-06 -3.62E-05  2.63E-05  8.48E-05  1.47E-06
         6.92E-05 -1.25E-05  1.69E-05 -1.53E-05  1.13E-05  3.47E-04 -3.78E-05 -1.32E-04  1.11E-04  7.30E-04
 
 OM55
+       -1.72E-04  6.20E-05 -5.31E-06 -1.26E-04 -1.07E-04 -9.61E-05  1.17E-04  8.11E-05  6.25E-05 -2.26E-06  8.40E-06  5.76E-05
          1.46E-04 -3.09E-05 -2.95E-05 -2.05E-05  6.94E-05 -5.63E-06  4.60E-05  1.41E-04 -3.45E-05 -2.00E-05 -2.74E-05  4.91E-05
         1.98E-05  1.83E-05 -1.14E-05  1.61E-05  8.19E-06  2.59E-05 -9.84E-05 -5.84E-05 -1.02E-04  4.02E-05  1.67E-03
 
 OM56
+       -2.94E-05 -1.25E-05  6.05E-05  9.92E-05  4.25E-05 -8.39E-06 -1.07E-04 -7.99E-06  2.81E-05 -1.88E-05  8.83E-06  3.15E-05
         -4.70E-05  4.83E-05  4.26E-05  6.02E-05  4.32E-05  1.14E-05 -3.14E-05 -2.26E-05  2.43E-05 -4.63E-05 -2.84E-05 -1.05E-05
        -5.31E-06  8.71E-06  2.08E-05  1.15E-05 -5.60E-06  1.04E-05  3.85E-05 -5.18E-05  1.06E-05 -1.89E-06 -2.66E-04  7.23E-04
 
 OM57
+        1.42E-05  7.32E-05  6.19E-06 -1.13E-05 -1.79E-05  2.14E-06  4.05E-05  4.33E-05 -5.23E-05  5.37E-07  1.90E-05 -1.31E-05
          2.24E-05  2.94E-05 -5.87E-06 -3.89E-05  2.87E-05 -1.21E-05 -1.16E-05 -7.23E-05  3.07E-06  1.47E-05  6.81E-06 -2.20E-05
        -1.55E-05  4.14E-05  1.16E-05  4.87E-07 -3.01E-05  1.52E-05 -1.19E-04 -2.41E-05 -7.62E-05 -5.15E-06  1.33E-04  5.52E-05
          6.72E-04
 
 OM58
+        3.97E-05  5.68E-05  3.04E-05 -9.64E-06  4.63E-06 -6.26E-06 -3.10E-05  4.42E-05  8.34E-05 -2.42E-06 -3.73E-06 -6.06E-06
          1.49E-04 -2.68E-05  5.45E-06 -7.57E-06 -3.16E-06 -1.79E-05 -5.18E-06  7.70E-05  6.17E-06 -1.46E-05  5.07E-06  2.46E-06
        -1.22E-05  5.31E-05 -8.41E-06  1.07E-06 -1.46E-05  3.72E-06  4.95E-05 -3.79E-07 -1.71E-05 -5.51E-05 -3.36E-05 -9.71E-05
          8.69E-05  4.80E-04
 
 OM66
+       -1.41E-04 -4.06E-05 -4.20E-05 -2.73E-05 -8.71E-05  2.18E-04  4.82E-05  2.73E-05  1.65E-04  9.63E-06 -7.00E-06 -1.93E-05
         -1.77E-05  6.14E-05 -4.21E-05  2.27E-05 -1.30E-05 -5.46E-06  1.15E-04  2.26E-05  5.65E-05  3.02E-05  5.11E-05  1.02E-04
         2.36E-05  1.43E-05  1.48E-04  3.95E-05  1.79E-05  1.61E-04  1.00E-05  9.78E-05  4.19E-05  3.63E-05  5.13E-05 -3.85E-04
         -1.12E-05  3.76E-05  3.38E-03
 
 OM67
+        4.35E-05 -8.05E-05 -1.57E-05  9.04E-05  2.27E-05 -7.37E-05 -5.12E-05 -5.91E-05  4.24E-05  7.33E-06 -1.35E-05  2.94E-05
         -1.43E-05  2.96E-05 -1.27E-05  2.16E-05 -1.40E-05  1.86E-05 -3.52E-05  2.17E-05 -8.86E-05  2.42E-05  9.10E-06  1.51E-05
        -3.35E-06  1.52E-05  3.96E-05  5.74E-06 -2.06E-05 -5.15E-05  2.95E-05 -7.55E-05 -1.32E-05  7.15E-06  3.00E-05  5.97E-05
         -6.32E-05 -4.60E-05  6.65E-05  7.65E-04
 
 OM68
+       -1.89E-05 -3.21E-05  7.23E-05  3.23E-05 -7.92E-06  1.14E-05  3.61E-05 -5.34E-05 -3.34E-05  1.55E-05 -1.93E-07 -1.72E-05
         -2.75E-05  1.72E-04  1.43E-05 -3.82E-05 -5.04E-05  5.98E-06 -2.85E-05 -3.86E-05  1.43E-04  7.14E-06 -2.70E-05 -3.05E-05
         1.41E-06 -2.17E-05  4.40E-05 -1.66E-05 -1.51E-05 -9.25E-05 -2.16E-05  1.12E-04 -2.60E-05 -5.09E-05 -4.01E-05 -1.22E-05
          5.29E-06 -7.20E-05 -3.68E-04  1.25E-04  6.88E-04
 
 OM77
+        9.32E-05 -9.65E-05  1.08E-05 -1.64E-05  1.73E-04  6.10E-05  2.78E-06 -5.65E-05  1.06E-04 -4.65E-06  2.96E-05 -8.53E-06
         -1.46E-05  6.47E-06  2.42E-05  6.17E-07  3.17E-05 -3.80E-05  8.18E-05 -2.56E-05  9.11E-06 -2.92E-04 -6.27E-05  4.16E-05
        -5.59E-05 -1.21E-06 -1.49E-06  5.15E-05  4.80E-05  2.12E-04 -3.29E-05 -3.27E-06 -3.76E-04 -4.03E-05  1.36E-06 -6.12E-06
          9.43E-05  3.54E-05  4.50E-05  1.20E-05 -5.85E-06  2.13E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -7.62E-05 -7.26E-05  3.73E-05  1.82E-05 -1.73E-05  3.08E-05  3.37E-05  3.31E-05  1.80E-05  1.49E-05  1.62E-06  2.70E-05
         -3.46E-05 -2.23E-05  1.33E-04  8.24E-05 -7.90E-05 -1.22E-05 -2.50E-05 -3.35E-06  3.17E-05  7.54E-05 -6.07E-05  3.51E-05
        -3.04E-05 -3.14E-05 -5.84E-06  6.39E-05  4.68E-05 -5.19E-05  2.89E-06 -1.24E-05  4.73E-05 -5.92E-05 -6.24E-05 -8.80E-06
         -4.97E-05  3.29E-05 -5.06E-05 -1.11E-04  8.89E-06  3.95E-04  6.94E-04
 
 OM88
+       -1.01E-04  1.71E-05  1.78E-05  1.20E-05 -1.00E-04  1.45E-04  1.18E-04  1.00E-04  1.51E-04  1.26E-04  5.46E-05  9.59E-05
          7.91E-06 -1.06E-04  1.06E-04  3.95E-04  7.35E-05  5.40E-05  4.63E-05 -2.79E-06 -2.16E-05  6.47E-05  2.61E-04  7.19E-05
         3.26E-05 -1.29E-05  1.14E-06  3.50E-05  1.82E-04  1.23E-04 -1.88E-05 -6.25E-05  5.70E-05  2.51E-04  8.14E-06  2.35E-05
         -8.47E-05 -8.93E-05  8.81E-05 -4.57E-05 -2.06E-04  1.40E-04  3.58E-04  1.48E-03
 
 SG11
+        1.69E-06 -1.25E-06  8.24E-07  6.71E-08  1.14E-06  1.51E-06  4.42E-07  3.09E-08  1.01E-06  1.35E-07 -3.50E-08 -4.74E-09
          5.13E-08  8.78E-08  6.38E-07  8.57E-07 -1.35E-06 -7.12E-07  4.61E-07 -3.33E-07 -5.29E-07  6.04E-07  2.69E-07  1.91E-08
        -2.19E-07 -1.14E-07 -1.05E-07  3.87E-07  1.90E-07  4.89E-08  3.21E-07 -2.30E-07 -1.18E-06 -4.71E-07 -5.55E-07 -2.44E-07
         -1.08E-06 -1.39E-07  1.13E-06  8.48E-07  4.61E-07 -1.06E-07  3.17E-07  1.41E-06  4.16E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        4.67E-06  1.37E-06  1.81E-07 -1.02E-06 -4.14E-07 -2.53E-06  3.04E-06  9.65E-09  6.10E-07  4.82E-07  1.13E-06 -7.83E-07
          3.65E-08 -1.27E-06 -6.44E-07 -2.25E-08 -9.53E-08 -1.05E-08 -2.95E-07 -1.02E-06 -2.36E-07 -1.26E-06  5.10E-07 -1.41E-06
         5.28E-07 -9.44E-07  5.59E-08  5.99E-07 -4.26E-07 -1.93E-06  8.66E-07  9.25E-07 -1.81E-06 -8.51E-07 -1.25E-06 -4.38E-07
          1.73E-06  5.75E-07 -2.84E-06 -7.07E-07  9.29E-07  5.94E-07 -6.74E-07 -3.26E-07  1.43E-08  0.00E+00  1.36E-06
 
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
+       -1.16E-01  7.05E-02
 
 TH 3
+        2.72E-02 -1.62E-02  5.31E-02
 
 TH 4
+        1.33E-01  1.11E-01  1.46E-02  7.29E-02
 
 TH 5
+        8.54E-02  5.23E-02 -2.51E-02 -6.97E-02  6.25E-02
 
 TH 6
+       -3.85E-02 -1.04E-02  8.00E-02  6.70E-02 -1.55E-01  6.90E-02
 
 TH 7
+        4.57E-02 -2.14E-01  1.08E-01 -1.86E-01  5.10E-02  1.09E-01  6.62E-02
 
 TH 8
+        3.17E-01  2.35E-01  1.79E-01  1.66E-01 -5.21E-02 -1.55E-01  2.31E-01  6.33E-02
 
 OM11
+        2.46E-02 -2.85E-02 -4.44E-02  1.06E-02  3.07E-02  9.78E-03  7.27E-02  2.50E-03  5.58E-02
 
 OM12
+       -4.96E-02  1.00E-01 -1.35E-02  6.72E-02 -3.27E-02 -6.46E-03 -5.10E-02 -5.56E-02 -2.14E-01  2.95E-02
 
 OM13
+       -1.02E-02 -3.64E-03  8.83E-02  1.86E-02 -2.33E-02 -1.33E-04  1.08E-02  5.62E-03  1.12E-02 -6.43E-02  2.17E-02
 
 OM14
+       -2.16E-02 -3.95E-03 -1.25E-02 -8.91E-03  1.60E-03 -4.00E-02 -5.56E-03  5.56E-03  7.48E-02  9.61E-02  9.64E-04  3.21E-02
 
 OM15
+        4.73E-03 -2.13E-03 -3.20E-02  3.82E-02  4.26E-02 -1.88E-02  3.89E-02 -4.94E-03  1.55E-01  2.14E-02 -3.01E-02 -1.67E-02
          2.81E-02
 
 OM16
+       -1.48E-02 -2.72E-02 -1.08E-02 -1.36E-02  1.77E-02 -4.38E-03 -5.04E-04 -4.97E-02 -4.22E-02  5.36E-02 -3.60E-03  5.78E-02
         -1.61E-01  3.06E-02
 
 OM17
+        1.22E-02 -9.68E-04 -1.36E-02  5.16E-02 -2.13E-02 -7.94E-03  3.63E-02  1.11E-02  3.65E-02 -1.53E-01  6.83E-02 -1.12E-01
          7.77E-02  4.07E-02  2.81E-02
 
 OM18
+        1.56E-02 -1.79E-02  2.29E-02  2.17E-02 -3.56E-02  5.25E-03  1.26E-03  3.99E-02  2.92E-01  1.31E-01  1.12E-01  1.43E-01
          4.30E-02 -1.81E-01  1.86E-01  2.85E-02
 
 OM22
+        1.82E-02 -2.22E-01  2.26E-02 -5.18E-03  4.86E-02  2.20E-02 -2.00E-03 -1.20E-02  2.61E-02 -2.56E-01  5.30E-03  3.58E-03
         -2.92E-02  5.96E-03  6.98E-02 -2.57E-03  4.93E-02
 
 OM23
+       -6.69E-02  9.97E-02  2.69E-02  3.81E-02 -2.79E-02  5.48E-03 -5.62E-02 -4.33E-02 -7.87E-03  9.76E-02 -7.11E-02  7.33E-03
         -7.87E-03  2.01E-02 -3.74E-02 -9.76E-03 -8.80E-02  1.92E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.53E-03 -1.13E-01  2.63E-03 -9.16E-04  5.06E-02  3.65E-02  5.85E-02  3.50E-02 -1.92E-03 -7.19E-02  3.83E-02 -7.35E-02
          1.58E-02 -1.84E-02  8.82E-03  3.60E-02  1.73E-01 -7.12E-02  2.86E-02
 
 OM25
+       -1.06E-02  2.52E-02 -1.35E-02 -1.29E-02  3.93E-04  1.53E-02 -3.78E-03  1.26E-02  1.91E-02 -2.48E-02  6.95E-03  2.21E-02
         -1.21E-01 -1.12E-03  3.54E-02  3.80E-04  8.62E-02 -4.16E-02 -4.00E-02  2.29E-02
 
 OM26
+       -5.86E-02  3.22E-02  3.97E-02  1.82E-02 -2.01E-02  4.93E-02 -4.40E-02 -7.48E-02  1.95E-02  2.16E-02  4.69E-02 -4.44E-02
          1.28E-02 -5.53E-02 -7.37E-03  1.29E-02 -1.41E-02  8.51E-02 -2.04E-02 -1.33E-01  2.61E-02
 
 OM27
+        9.48E-03  2.15E-01  1.85E-02  5.69E-03 -4.82E-02  2.08E-02  1.33E-02  4.79E-02 -5.83E-02  1.63E-01 -3.49E-02  8.81E-03
         -7.58E-02 -2.21E-02 -1.35E-01  1.47E-02 -3.92E-01  8.77E-02 -2.54E-01  7.31E-02  3.19E-02  2.86E-02
 
 OM28
+        2.46E-02  1.44E-02 -3.43E-03 -1.35E-02  6.86E-03  1.50E-02  2.24E-02  4.20E-02 -9.07E-02  1.70E-01  1.11E-02  5.60E-02
          2.94E-03  9.61E-03 -1.83E-02 -3.56E-02  2.94E-01  1.02E-01  1.06E-01  2.73E-03 -1.84E-01  9.02E-02  2.38E-02
 
 OM33
+        2.93E-02 -4.14E-02 -9.12E-02 -2.83E-02 -3.98E-03  1.43E-04 -1.51E-02  2.21E-02 -5.35E-02  3.99E-03  1.18E-01  3.59E-03
          1.25E-02  5.95E-02  4.72E-02 -2.36E-02  3.24E-02 -8.01E-02  1.53E-02  2.01E-02 -4.93E-03 -2.04E-02  4.59E-02  3.09E-02
 
 OM34
+       -2.80E-02  1.82E-02  1.58E-01  8.89E-02 -2.41E-02  4.08E-02 -4.68E-02 -3.57E-03 -2.22E-02 -2.18E-02  1.17E-01  1.61E-02
          2.56E-02 -3.79E-02  2.05E-02  3.10E-02  3.56E-02  8.68E-02  1.72E-02 -1.25E-02  1.52E-02 -2.27E-02  5.79E-02 -8.38E-02
         2.24E-02
 
 OM35
+        1.78E-02  7.04E-03 -5.87E-02 -5.53E-02  5.17E-02  1.61E-02  2.39E-02 -8.60E-03  3.54E-02 -3.39E-02  2.29E-02  3.13E-02
          3.03E-02 -8.25E-03 -1.70E-02 -2.81E-02 -2.86E-02  2.68E-02 -2.36E-02 -3.01E-02 -1.60E-02  1.11E-02  2.89E-03  9.10E-03
        -1.16E-01  1.90E-02
 
 OM36
+       -9.19E-03 -6.13E-02 -6.44E-03  1.24E-02 -4.20E-03 -7.05E-02 -2.97E-03  2.33E-02  3.77E-02  1.76E-03  3.44E-02  4.12E-02
         -2.04E-02  6.79E-02 -1.89E-04  2.78E-02  8.73E-03  3.89E-02  2.76E-02 -9.85E-03  3.97E-02 -1.05E-02 -2.29E-02  4.00E-02
         2.74E-02 -1.56E-01  2.10E-02
 
 OM37
+       -1.78E-03 -4.26E-02 -7.07E-02 -3.79E-02 -8.98E-03  2.48E-02  1.81E-02 -1.99E-03 -6.69E-03 -1.05E-02 -1.30E-02 -1.65E-02
          1.24E-02 -2.99E-02  6.65E-02  5.75E-02  3.12E-02 -1.75E-01  2.17E-02 -1.30E-03 -9.17E-02 -7.81E-02  3.51E-03  6.63E-02
        -1.27E-01  1.10E-01  9.63E-03  1.93E-02
 
 OM38
+       -6.46E-02  1.28E-02  2.07E-02  1.48E-02 -3.73E-02  5.57E-02 -1.48E-02 -5.20E-03 -7.14E-03 -9.07E-03  2.52E-01 -2.14E-02
         -3.68E-03 -9.20E-03  4.66E-02  7.86E-02  5.46E-02  1.86E-01  1.80E-02  1.94E-02 -3.93E-02 -4.96E-02  8.93E-02  3.03E-01
         1.43E-01 -5.88E-02 -1.53E-01  1.86E-01  1.93E-02
 
 OM44
+       -3.74E-02  1.09E-03  7.75E-02  5.69E-02 -1.44E-02  8.18E-03  7.11E-02  1.08E-02  9.84E-02  1.52E-02  5.92E-02  1.26E-01
         -5.04E-03  3.08E-02 -3.33E-02  6.63E-02  1.31E-02  6.58E-03  1.41E-01  1.16E-02  7.12E-03 -4.35E-02  3.56E-02 -5.00E-02
         2.71E-02 -5.08E-02  3.96E-02 -1.57E-02  5.90E-03  5.47E-02
 
 OM45
+        1.60E-02 -3.41E-02 -3.08E-02  7.80E-03  4.74E-03 -1.09E-02 -1.09E-02 -1.51E-02  3.45E-02  1.73E-02 -3.41E-02  1.36E-01
          3.39E-02 -4.13E-02  1.21E-02  3.49E-02  1.04E-02  2.60E-02  3.41E-02  3.25E-02  3.36E-02 -1.61E-03 -4.12E-02 -3.32E-02
        -2.07E-02 -1.66E-02  1.09E-02 -4.35E-02 -7.41E-02 -1.13E-01  2.66E-02
 
 OM46
+       -1.11E-03  7.18E-03  1.28E-03  6.17E-02 -4.25E-03  3.14E-02 -5.35E-03 -1.83E-02  1.36E-02 -2.42E-02 -2.24E-02 -4.60E-02
         -4.20E-02  3.85E-02 -2.32E-02 -8.90E-02 -4.99E-03 -2.23E-02  5.85E-02 -2.93E-02  7.89E-02 -3.40E-03  1.14E-02  2.00E-03
         2.11E-02 -7.78E-03 -2.58E-03 -3.05E-02  4.27E-02  4.44E-02 -1.34E-01  2.97E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.49E-02  1.85E-02 -5.76E-02 -1.92E-02 -2.97E-02  4.85E-02 -6.12E-03  9.99E-03 -2.04E-02 -1.47E-02 -1.07E-01  1.81E-02
         -1.08E-03 -7.93E-02 -7.16E-03  4.18E-02 -8.76E-03 -4.41E-03 -1.90E-01  3.42E-02 -1.32E-02  1.14E-01  1.40E-03 -1.76E-02
        -2.35E-04 -6.81E-04 -3.33E-02 -2.77E-02 -6.05E-02 -3.19E-01  6.88E-02  2.16E-02  3.02E-02
 
 OM48
+       -3.12E-02  4.74E-02 -1.02E-02  1.61E-02 -1.55E-02  7.43E-02  2.26E-02  2.09E-02  2.61E-02  1.34E-01  4.55E-02  2.42E-01
         -4.65E-02 -1.38E-02 -3.57E-02  1.77E-01  1.44E-02  2.82E-02  1.48E-01  1.24E-02 -5.13E-02  3.41E-02  1.32E-01  1.76E-03
         1.14E-01 -2.44E-02  2.97E-02 -2.93E-02  2.16E-02  2.34E-01 -5.26E-02 -1.64E-01  1.36E-01  2.70E-02
 
 OM55
+       -5.82E-02  2.15E-02 -2.44E-03 -4.23E-02 -4.20E-02 -3.41E-02  4.32E-02  3.13E-02  2.74E-02 -1.87E-03  9.46E-03  4.39E-02
          1.27E-01 -2.47E-02 -2.56E-02 -1.76E-02  3.44E-02 -7.16E-03  3.94E-02  1.51E-01 -3.23E-02 -1.71E-02 -2.82E-02  3.89E-02
         2.17E-02  2.36E-02 -1.33E-02  2.05E-02  1.04E-02  1.16E-02 -9.06E-02 -4.80E-02 -8.23E-02  3.64E-02  4.09E-02
 
 OM56
+       -1.51E-02 -6.59E-03  4.23E-02  5.06E-02  2.53E-02 -4.52E-03 -6.00E-02 -4.69E-03  1.87E-02 -2.37E-02  1.51E-02  3.65E-02
         -6.22E-02  5.87E-02  5.62E-02  7.84E-02  3.26E-02  2.21E-02 -4.09E-02 -3.67E-02  3.46E-02 -6.01E-02 -4.44E-02 -1.26E-02
        -8.83E-03  1.71E-02  3.68E-02  2.21E-02 -1.08E-02  7.05E-03  5.38E-02 -6.48E-02  1.30E-02 -2.60E-03 -2.42E-01  2.69E-02
 
 OM57
+        7.56E-03  4.01E-02  4.49E-03 -6.01E-03 -1.11E-02  1.20E-03  2.36E-02  2.63E-02 -3.61E-02  7.03E-04  3.37E-02 -1.57E-02
          3.08E-02  3.71E-02 -8.05E-03 -5.26E-02  2.24E-02 -2.43E-02 -1.56E-02 -1.22E-01  4.53E-03  1.99E-02  1.10E-02 -2.75E-02
        -2.68E-02  8.41E-02  2.12E-02  9.75E-04 -6.01E-02  1.07E-02 -1.72E-01 -3.12E-02 -9.71E-02 -7.35E-03  1.25E-01  7.91E-02
          2.59E-02
 
 OM58
+        2.51E-02  3.68E-02  2.61E-02 -6.04E-03  3.38E-03 -4.14E-03 -2.13E-02  3.18E-02  6.81E-02 -3.75E-03 -7.84E-03 -8.62E-03
          2.42E-01 -4.00E-02  8.84E-03 -1.21E-02 -2.92E-03 -4.25E-02 -8.27E-03  1.54E-01  1.08E-02 -2.33E-02  9.72E-03  3.64E-03
        -2.48E-02  1.28E-01 -1.82E-02  2.53E-03 -3.45E-02  3.10E-03  8.49E-02 -5.82E-04 -2.58E-02 -9.31E-02 -3.75E-02 -1.65E-01
          1.53E-01  2.19E-02
 
 OM66
+       -3.36E-02 -9.90E-03 -1.36E-02 -6.45E-03 -2.39E-02  5.44E-02  1.25E-02  7.41E-03  5.09E-02  5.62E-03 -5.54E-03 -1.03E-02
         -1.09E-02  3.45E-02 -2.57E-02  1.37E-02 -4.54E-03 -4.88E-03  6.94E-02  1.70E-02  3.72E-02  1.82E-02  3.69E-02  5.68E-02
         1.82E-02  1.29E-02  1.21E-01  3.53E-02  1.59E-02  5.05E-02  6.48E-03  5.66E-02  2.38E-02  2.31E-02  2.16E-02 -2.46E-01
         -7.43E-03  2.95E-02  5.81E-02
 
 OM67
+        2.18E-02 -4.13E-02 -1.07E-02  4.49E-02  1.31E-02 -3.86E-02 -2.80E-02 -3.38E-02  2.75E-02  8.99E-03 -2.24E-02  3.31E-02
         -1.84E-02  3.49E-02 -1.64E-02  2.73E-02 -1.03E-02  3.51E-02 -4.45E-02  3.44E-02 -1.23E-01  3.06E-02  1.38E-02  1.77E-02
        -5.42E-03  2.89E-02  6.81E-02  1.08E-02 -3.85E-02 -3.41E-02  4.01E-02 -9.18E-02 -1.57E-02  9.57E-03  2.66E-02  8.03E-02
         -8.82E-02 -7.59E-02  4.14E-02  2.77E-02
 
 OM68
+       -9.95E-03 -1.74E-02  5.19E-02  1.69E-02 -4.83E-03  6.33E-03  2.08E-02 -3.21E-02 -2.28E-02  2.00E-02 -3.39E-04 -2.04E-02
         -3.74E-02  2.14E-01  1.94E-02 -5.11E-02 -3.90E-02  1.19E-02 -3.80E-02 -6.43E-02  2.09E-01  9.52E-03 -4.32E-02 -3.77E-02
         2.40E-03 -4.35E-02  7.99E-02 -3.29E-02 -2.99E-02 -6.44E-02 -3.10E-02  1.43E-01 -3.28E-02 -7.18E-02 -3.74E-02 -1.73E-02
          7.78E-03 -1.25E-01 -2.41E-01  1.72E-01  2.62E-02
 
 OM77
+        2.79E-02 -2.97E-02  4.39E-03 -4.88E-03  5.98E-02  1.92E-02  9.09E-04 -1.93E-02  4.11E-02 -3.42E-03  2.95E-02 -5.76E-03
         -1.12E-02  4.58E-03  1.86E-02  4.68E-04  1.39E-02 -4.28E-02  6.20E-02 -2.43E-02  7.56E-03 -2.21E-01 -5.71E-02  2.91E-02
        -5.42E-02 -1.37E-03 -1.54E-03  5.79E-02  5.39E-02  8.40E-02 -2.68E-02 -2.38E-03 -2.70E-01 -3.23E-02  7.19E-04 -4.93E-03
          7.88E-02  3.50E-02  1.68E-02  9.40E-03 -4.83E-03  4.62E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -4.00E-02 -3.91E-02  2.67E-02  9.49E-03 -1.05E-02  1.69E-02  1.93E-02  1.98E-02  1.23E-02  1.92E-02  2.83E-03  3.19E-02
         -4.68E-02 -2.77E-02  1.79E-01  1.10E-01 -6.08E-02 -2.41E-02 -3.32E-02 -5.55E-03  4.60E-02  1.00E-01 -9.69E-02  4.32E-02
        -5.16E-02 -6.28E-02 -1.05E-02  1.26E-01  9.21E-02 -3.60E-02  4.12E-03 -1.59E-02  5.94E-02 -8.32E-02 -5.79E-02 -1.24E-02
         -7.28E-02  5.70E-02 -3.31E-02 -1.52E-01  1.29E-02  3.25E-01  2.63E-02
 
 OM88
+       -3.64E-02  6.30E-03  8.69E-03  4.27E-03 -4.16E-02  5.46E-02  4.62E-02  4.11E-02  7.01E-02  1.11E-01  6.53E-02  7.77E-02
          7.32E-03 -8.98E-02  9.79E-02  3.60E-01  3.87E-02  7.30E-02  4.21E-02 -3.17E-03 -2.15E-02  5.88E-02  2.85E-01  6.05E-02
         3.78E-02 -1.77E-02  1.41E-03  4.72E-02  2.45E-01  5.83E-02 -1.84E-02 -5.46E-02  4.90E-02  2.42E-01  5.17E-03  2.27E-02
         -8.49E-02 -1.06E-01  3.94E-02 -4.30E-02 -2.04E-01  7.86E-02  3.53E-01  3.85E-02
 
 SG11
+        3.63E-02 -2.76E-02  2.40E-02  1.43E-03  2.82E-02  3.40E-02  1.03E-02  7.56E-04  2.81E-02  7.08E-03 -2.50E-03 -2.29E-04
          2.83E-03  4.45E-03  3.52E-02  4.65E-02 -4.24E-02 -5.74E-02  2.50E-02 -2.26E-02 -3.14E-02  3.27E-02  1.75E-02  9.57E-04
        -1.52E-02 -9.28E-03 -7.73E-03  3.12E-02  1.52E-02  1.38E-03  1.87E-02 -1.20E-02 -6.06E-02 -2.70E-02 -2.11E-02 -1.41E-02
         -6.47E-02 -9.82E-03  3.02E-02  4.76E-02  2.72E-02 -3.58E-03  1.87E-02  5.70E-02  6.45E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        5.54E-02  1.67E-02  2.92E-03 -1.19E-02 -5.68E-03 -3.15E-02  3.94E-02  1.31E-04  9.37E-03  1.40E-02  4.45E-02 -2.09E-02
          1.11E-03 -3.54E-02 -1.96E-02 -6.76E-04 -1.66E-03 -4.68E-04 -8.85E-03 -3.81E-02 -7.74E-03 -3.79E-02  1.84E-02 -3.92E-02
         2.03E-02 -4.26E-02  2.28E-03  2.67E-02 -1.89E-02 -3.02E-02  2.79E-02  2.67E-02 -5.13E-02 -2.70E-02 -2.63E-02 -1.40E-02
          5.73E-02  2.25E-02 -4.18E-02 -2.19E-02  3.04E-02  1.10E-02 -2.19E-02 -7.26E-03  1.91E-02  0.00E+00  1.17E-03
 
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
+        2.41E+02
 
 TH 2
+        6.63E+01  2.88E+02
 
 TH 3
+        9.71E+00  2.22E+01  4.03E+02
 
 TH 4
+       -2.09E+01 -3.39E+00  1.07E+01  2.21E+02
 
 TH 5
+       -3.71E+01 -4.77E+01 -5.95E+00  8.19E+00  2.84E+02
 
 TH 6
+       -1.47E+01 -2.97E+01 -3.43E+01 -3.04E+01  5.11E+01  2.47E+02
 
 TH 7
+        2.68E+01  9.52E+01 -1.20E+01  5.98E+01 -3.89E+01 -5.79E+01  3.08E+02
 
 TH 8
+       -1.12E+02 -1.32E+02 -7.62E+01 -5.84E+01  5.66E+01  8.30E+01 -1.29E+02  4.03E+02
 
 OM11
+       -6.66E+00 -8.79E+00  2.41E+01 -1.11E+01 -5.65E+00 -8.63E-01 -2.83E+01  1.58E+01  4.11E+02
 
 OM12
+        1.26E+01 -2.91E+01 -6.87E+00 -6.29E+01  1.12E+01  2.00E+01 -1.84E+01  7.25E+01  2.30E+02  1.63E+03
 
 OM13
+        4.37E-01 -1.27E+01 -7.81E+01 -2.11E+01  1.68E+01  2.04E+01 -1.62E+01  3.43E+01  3.32E+01  1.83E+02  2.50E+03
 
 OM14
+        7.13E+00  2.14E+00  5.03E+00  5.24E+00 -7.35E-01  2.20E+01  1.90E+00 -4.23E+00 -1.67E+01 -9.51E+00  2.46E+01  1.17E+03
 
 OM15
+        1.24E+00  7.26E+00  2.53E+01 -3.17E+01 -3.56E+01  6.30E+00 -2.90E+01  2.01E+01 -1.23E+02 -1.28E+02  5.30E+01 -9.74E+00
          1.58E+03
 
 OM16
+       -3.21E+00  1.10E+01  1.07E+01  7.42E+00 -1.24E+01 -4.90E-01  4.36E-01  2.02E+01 -4.18E+01 -1.70E+02 -3.38E+01 -1.11E+02
          2.43E+02  1.29E+03
 
 OM17
+       -1.62E+01 -5.90E+01  1.39E+01 -4.09E+01  2.98E+01  2.43E+01 -5.85E+01  4.68E+01  6.91E+01  3.22E+02 -4.20E+01  2.10E+02
         -1.78E+02 -1.70E+02  1.56E+03
 
 OM18
+       -1.42E+01  3.69E+01 -1.27E+01  2.56E+01  6.52E+00  1.20E+00  4.79E+01 -4.28E+01 -2.83E+02 -4.68E+02 -2.50E+02 -2.03E+02
          1.03E+02  3.39E+02 -4.20E+02  1.93E+03
 
 OM22
+        1.00E+01  6.63E+01 -1.92E+01 -1.47E+01 -2.51E+01 -1.49E+01  2.46E+01 -1.18E+01  1.69E+01  3.06E+02  9.39E+01  1.48E+01
          3.22E+01 -3.91E+01  4.21E+01 -1.14E+02  6.83E+02
 
 OM23
+        1.37E+01 -5.45E+01 -2.98E+01 -2.15E+01  1.89E+01  6.47E+00  1.39E+01  4.93E+01 -1.10E+01 -4.67E+01  4.63E+02  4.48E+01
          7.50E+00 -6.41E+01  1.97E+01 -2.78E+00  1.51E+02  3.36E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        1.99E+01  4.29E+01  1.78E+01 -8.26E-01 -3.79E+01 -2.75E+01 -9.07E+00 -4.62E+01  4.73E+01  1.36E+02  8.35E+00  2.06E+02
         -2.34E+01  7.44E+00  1.00E+02 -1.39E+02  9.93E-01  1.19E+02  1.55E+03
 
 OM25
+        3.55E+00 -1.28E+01  3.01E+01  4.30E+00 -1.69E+01 -2.04E+01 -9.34E-01  1.86E+00 -2.92E+01 -7.82E+01 -5.95E+01 -2.73E+01
          3.83E+02  7.19E+01 -1.60E+02  9.90E+01 -1.84E+02  5.43E+01  3.90E+01  2.32E+03
 
 OM26
+        8.10E+00 -2.44E+01 -9.94E+00  5.27E-01  3.49E+00 -1.75E+01  1.63E+01  4.73E+01 -1.97E+01 -1.44E+02 -1.73E+02  2.11E+01
          4.98E+01  2.19E+02 -3.26E+01  1.06E+02 -1.64E+02 -2.81E+02 -1.95E+01  3.25E+02  1.86E+03
 
 OM27
+       -3.80E+01 -1.06E+02 -2.79E+01 -3.55E+00  1.80E+01 -1.19E+01 -4.60E+01  2.68E+01  3.53E+01  1.09E+02  7.24E+01  1.17E+02
          5.55E+01 -1.79E+01  3.22E+02 -1.89E+02  4.65E+02  3.88E+01  3.86E+02 -3.17E+02 -2.07E+02  2.00E+03
 
 OM28
+       -2.05E+01 -2.05E+01  2.92E+01  3.89E+01 -2.68E+00  4.27E+00  2.26E-01 -2.99E+01  2.23E+00 -5.73E+02 -1.68E+02 -1.50E+02
          1.66E+01  1.15E+02 -1.89E+02  5.10E+02 -6.14E+02 -4.24E+02 -2.76E+02  2.30E+02  5.82E+02 -6.76E+02  2.94E+03
 
 OM33
+       -1.92E+01  2.54E+01  6.09E+01  1.85E+01 -2.92E+00 -1.03E+01  2.19E+01 -3.22E+01  3.54E+01 -1.87E+01 -6.43E+01 -1.79E+01
         -4.42E+01 -7.39E+01 -5.51E+01  4.96E+01  1.07E+01  3.12E+02  7.24E+00 -1.65E+00 -8.04E+01 -4.12E+00 -8.37E+01  1.27E+03
 
 OM34
+        8.07E+00  2.73E+00 -1.29E+02 -4.75E+01 -2.98E+00 -1.59E+01  3.31E+01  2.02E+01  3.68E+01  9.57E+01 -1.52E+02 -1.05E+01
         -6.95E+01  4.76E+01 -3.84E+01 -2.75E+01  1.43E+01 -3.02E+01  3.84E+01  3.15E+01 -1.40E+01  6.83E+01 -1.29E+02  2.15E+02
         2.29E+03
 
 OM35
+       -4.12E+00  1.59E+01  5.21E+01  2.83E+01 -4.96E+01 -2.12E+01 -1.15E+01 -1.03E+01 -3.76E+01  5.82E+01 -2.56E+02 -1.12E+02
          6.03E+01  3.29E+01 -1.53E+01  1.11E+02  1.59E+01 -3.52E+02  5.31E+00  1.61E+02  4.66E+01 -6.95E+01  8.02E+01 -6.99E+01
         1.93E+02  3.16E+03
 
 OM36
+        2.66E+01  6.00E+01  2.78E+01 -1.16E+00 -6.99E+00  4.60E+01  1.43E+01 -4.64E+01 -3.38E+01 -6.15E+00 -2.43E+02 -5.82E+01
          3.47E+01 -3.73E+01  4.82E+00  2.88E+01 -3.20E+01 -3.80E+02 -3.63E+01  2.30E+01  3.06E+01 -4.49E+01  1.54E+02 -2.00E+02
        -1.20E+02  5.58E+02  2.63E+03
 
 OM37
+       -4.96E+00 -1.11E+00  5.33E+01  1.81E+01  1.42E+01 -2.91E+01  5.10E-01  1.91E+00  4.17E+01 -4.55E+00  2.76E+02  4.48E+01
         -2.93E+01  5.54E+01 -4.27E+01 -1.43E+02  3.79E+01  7.45E+02  3.58E+01  2.25E+01  1.06E+02  1.85E+02 -1.61E+02  1.22E+02
         3.46E+02 -4.82E+02 -2.93E+02  3.25E+03
 
 OM38
+        5.72E+01 -2.40E+00  1.25E+00 -5.01E+00  6.06E+00 -2.11E+01  5.16E+00 -1.79E+01 -1.03E+01  3.45E+01 -8.35E+02  1.30E+01
          1.53E+01  3.09E+01  5.16E+01  3.20E+01 -9.52E+01 -1.14E+03  1.23E+01 -3.08E+01  2.37E+02  2.94E+01  1.90E+02 -7.63E+02
        -4.84E+02  4.86E+02  8.01E+02 -9.82E+02  4.21E+03
 
 OM44
+        1.29E+01 -5.30E+00 -2.58E+01 -2.16E+01  1.06E+01  9.26E+00 -3.10E+01  1.19E+01 -2.63E+01  2.49E+01 -1.58E+01 -5.87E+01
         -1.10E+01 -1.71E+01  2.95E+01 -2.12E+01  5.89E+00 -5.81E+00 -2.68E+01 -2.86E+01 -2.31E+01  1.39E+01 -1.98E+01  3.68E+01
         2.98E+01  6.29E+01 -6.25E+00  1.53E+00  2.97E+01  4.39E+02
 
 OM45
+       -4.72E+00  1.36E+01  2.47E+01 -9.56E+00  5.14E+00 -4.07E-01 -7.10E+00  5.31E+00 -2.09E+01 -7.66E+01 -1.43E+01 -2.53E+02
          1.93E+00  6.05E+01 -7.21E+01  5.15E+01 -4.81E+01 -9.90E+01 -1.77E+02  1.10E+01 -3.38E+01 -9.95E+01  1.44E+02  2.58E+01
         1.48E+01  8.06E+01  2.34E+01  5.47E+01  1.36E+02  8.21E+01  1.64E+03
 
 OM46
+       -4.53E+00 -5.37E+00  9.81E+00 -3.67E+01 -3.58E-01 -7.55E+00  2.17E-01  1.80E+01 -3.46E+01 -2.66E+01  5.99E+01 -5.70E+01
          9.12E+01  2.22E+01 -1.83E+01  1.13E+02  4.48E+00  8.73E+01 -1.59E+02  4.70E+01 -3.14E+01 -5.57E+01 -2.60E+00  3.01E+00
        -4.75E+01 -2.28E+01 -8.41E-01  6.97E+01 -1.28E+02 -7.05E+01  2.26E+02  1.32E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        2.50E+01  2.70E+00  2.82E+01  5.54E+00  3.57E+00 -2.67E+01 -1.23E+01 -1.99E+01  1.99E+01  1.15E+02  1.48E+02  4.43E+01
         -6.14E+01  6.89E+01  8.57E+01 -8.89E+01 -3.19E+00  5.43E+01  3.18E+02 -6.76E+01  1.06E+01  6.70E+01 -5.93E+01  1.77E+01
         3.59E+01  1.30E+01  5.26E+01  6.95E+01  1.06E+02  2.81E+02 -7.74E+01 -1.63E+02  1.57E+03
 
 OM48
+        1.67E+00 -2.30E+01  3.30E+01 -1.56E-01 -8.98E+00 -4.73E+01  5.02E+00 -8.79E+00 -1.73E+01 -2.06E+02 -8.02E+01 -3.45E+02
          1.23E+02  1.40E+01 -6.15E+01 -2.90E+01 -2.84E+01 -2.98E+01 -3.74E+02  3.30E+01  7.29E+01 -1.50E+02  8.27E+01 -4.14E+01
        -2.33E+02  3.23E+01 -1.52E+01 -1.68E+01  9.89E+01 -2.37E+02  1.84E+02  3.48E+02 -4.81E+02  2.01E+03
 
 OM55
+        2.68E+01 -1.08E+01 -1.30E+01  1.21E+01  2.48E+01  1.55E+01 -8.05E+00 -1.87E+01 -1.50E+01 -1.35E+01  2.83E+00 -5.27E+01
         -1.87E+02 -1.72E+01  2.93E+01  1.61E+01 -2.52E+01 -2.70E+01 -3.92E+01 -2.82E+02 -3.45E+01  2.73E+01  9.07E+01 -4.22E+01
        -4.08E+01 -4.71E+01  1.15E+01 -5.11E+01  3.56E+01  2.12E+01  5.93E+01  2.58E+01  8.00E+01 -1.55E+01  7.44E+02
 
 OM56
+        3.43E+01  4.23E+00 -5.77E+01 -1.76E+01 -1.52E+01 -1.18E+01  4.11E+01 -2.93E+01 -1.87E+01  3.86E+01  2.50E+01 -2.40E+01
         -4.85E+01 -1.70E+02 -2.41E+01 -1.46E+02  1.37E+01 -1.58E+01  3.81E+01 -1.57E+02 -1.98E+02  1.46E+02 -6.60E+00 -1.89E+01
         1.51E+00 -1.27E+02 -1.42E+02 -9.12E+01 -7.12E+00 -2.02E+01 -1.23E+02  2.41E+01 -2.63E+01  4.68E+01  3.36E+02  1.83E+03
 
 OM57
+       -3.23E+00 -1.23E+01  2.42E+01 -6.13E+00  1.02E+01 -6.35E+00 -2.29E+01 -1.26E-01  2.96E+01 -3.16E+01 -8.70E+01 -3.12E+01
          8.33E+01  8.41E+00 -6.98E+01  6.13E+01 -9.10E+01  1.69E+01  9.78E+00  4.07E+02  1.02E+02 -2.29E+02  7.50E+01  4.55E+01
         4.45E+01 -7.92E+01 -3.67E+01  6.69E+00  1.10E+02  2.60E+01  3.25E+02  1.01E+02  6.60E+01  3.92E+00 -2.21E+02 -3.49E+02
          1.84E+03
 
 OM58
+       -1.90E+00 -2.95E+01 -7.29E+01  1.68E+01  2.15E+01 -6.76E+00  4.05E+01 -2.89E+01 -3.80E+01  3.00E+01  6.88E+01  4.54E+01
         -5.93E+02 -1.22E+02  9.94E+01 -1.63E+02  8.34E+01  1.44E+02  4.50E+01 -6.68E+02 -2.36E+02  2.34E+02 -3.36E+02 -5.18E+00
         1.12E+01 -4.50E+02 -1.18E+02  8.38E+01 -1.02E+02 -1.86E+01 -2.59E+02 -6.62E+01  5.48E+01  4.67E+01  2.62E+02  5.47E+02
         -5.45E+02  2.87E+03
 
 OM66
+        1.54E+01  4.73E+00 -7.49E+00  1.12E+00  3.16E+00 -1.69E+01  3.88E+00 -1.40E+01 -1.82E+01  4.65E+00  2.68E+01  1.65E+01
         -1.51E+00 -9.20E+01  9.96E+00 -3.75E+01  2.14E+01  2.48E+01 -3.56E+01 -3.38E+01 -1.28E+02  8.45E+00 -6.16E+01 -2.00E+01
        -2.30E+01 -4.23E+01 -1.52E+02 -4.29E+01 -3.64E+01 -1.35E+01 -2.87E+01 -5.73E+01 -4.14E+01  1.16E+01  3.75E+01  2.55E+02
         -5.52E+01  8.54E+01  3.79E+02
 
 OM67
+       -6.09E+00  2.83E+01  7.13E-01 -3.72E+01 -5.56E+00  2.53E+01  9.10E+00  3.10E+01 -1.46E+01 -1.13E+01  3.98E+00 -3.90E+01
          5.01E+01  7.43E+01 -2.93E+01  4.26E+00 -3.20E+01 -1.22E+02  2.86E+01  2.50E+01  3.23E+02 -1.66E+02  1.04E+02 -6.02E+01
        -4.55E+00 -4.56E+01 -3.88E+01 -7.05E+01  1.32E+02  1.89E+01  6.15E-01  1.69E+02 -7.24E+00  4.90E+01 -7.34E+01 -2.30E+02
          2.23E+02 -6.35E+01 -1.10E+02  1.57E+03
 
 OM68
+        1.52E+01 -3.27E+00 -5.37E+01 -3.21E+00  1.33E+01 -1.57E+01 -2.88E+01 -5.50E+00  3.45E+00  4.13E+01  6.59E+01  4.78E+01
         -1.06E+02 -4.29E+02  2.63E+01 -2.07E+02  9.35E+01  1.20E+02  5.02E+01 -8.42E+01 -6.01E+02  1.41E+02 -3.42E+02  8.83E+01
         1.75E+01 -4.38E+01 -2.75E+02  5.17E+01 -2.08E+02  6.82E+01 -2.32E+01 -2.75E+02  6.54E+01 -9.00E+01  9.05E+01  3.61E+02
         -1.54E+02  4.98E+02  3.03E+02 -4.90E+02  2.19E+03
 
 OM77
+       -2.07E+01 -1.97E+01  2.25E-01  6.72E+00 -1.89E+01 -1.54E+01 -9.04E-01  1.24E+01 -1.23E+01  8.97E+00  1.36E+01  4.14E+01
         -1.23E+00  8.42E+00  9.58E+01 -5.10E+00  5.46E+01  5.69E+01  5.96E+01 -4.46E+01 -2.89E+01  3.35E+02 -7.63E+01  4.09E+00
         5.62E+01 -2.46E+01 -2.50E+00  5.09E+01 -4.95E+01  3.72E+00 -2.83E+01 -3.39E+01  2.80E+02 -9.22E+01  1.52E+01  2.75E+01
         -1.32E+02  5.69E+01 -1.16E+01 -1.08E+02  5.48E+01  6.58E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        5.23E+01  8.40E+01 -1.52E+01 -1.72E+01 -2.11E+01  4.20E+00  2.65E+01 -4.54E+01 -1.33E+01 -1.37E+02 -2.70E+01 -2.05E+02
          2.07E+02  8.49E+01 -4.31E+02  2.36E+02 -9.81E+01 -8.37E+01 -1.46E+02  1.86E+02  1.24E+02 -6.02E+02  6.82E+02 -5.64E+01
         1.24E+01  2.73E+02  9.45E+01 -3.90E+02  1.37E+02 -1.13E+01  1.17E+02  1.42E+02 -3.30E+02  4.52E+02  1.80E+01 -2.68E+01
          2.54E+02 -4.61E+02  4.40E+00  3.98E+02 -3.77E+02 -5.14E+02  2.53E+03
 
 OM88
+        8.59E+00 -1.24E+01 -3.92E+00  1.19E+00  1.81E+01 -1.44E+01 -2.60E+01 -5.12E+00  8.85E+00  5.42E+01  7.75E+01  7.47E+01
         -9.42E+01 -5.64E+01  5.95E+01 -5.15E+02  7.87E+01  5.47E+01  6.92E+01 -6.86E+01 -1.80E+02  1.35E+02 -7.11E+02  4.24E+01
         5.04E+01 -1.42E+02 -1.40E+02  1.57E+02 -4.78E+02  1.26E+01 -3.48E+01 -5.17E+01  4.75E+01 -3.34E+02 -1.63E+01  3.16E+01
         -7.38E-01  3.55E+02  3.33E+01 -9.54E+01  4.46E+02  5.03E+01 -7.55E+02  1.27E+03
 
 SG11
+       -4.62E+02  8.14E+02 -6.79E+02  5.68E+01 -1.05E+03 -1.08E+03  5.25E+02 -1.42E+02 -5.90E+02 -1.62E+00  1.28E+03 -5.70E+02
          5.84E+02  1.71E+02 -2.24E+03 -7.30E+02  1.67E+03  5.71E+03 -1.31E+03  2.59E+03  2.16E+03 -2.46E+03 -7.73E+02  7.00E+02
         1.09E+03  9.52E+02  9.35E+02 -8.45E+02 -1.42E+03  3.21E+02 -3.43E+02  1.27E+03  3.76E+03  2.45E+03  1.54E+02  1.46E+01
          4.06E+03 -1.19E+03 -1.18E+03 -1.62E+03 -3.22E+03  6.99E+02  1.30E+03 -3.02E+03  2.50E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -8.84E+02 -7.92E+02  3.52E+01  1.07E+02  4.09E+02  5.94E+02 -9.80E+02  8.20E+02 -3.03E+02 -4.74E+02 -2.42E+03  5.67E+02
          5.99E+02  1.26E+03  1.12E+03  1.08E+02  8.21E+01 -1.00E+03  9.03E+02  8.73E+02  7.15E+02  2.05E+03 -6.68E+02  6.08E+02
        -9.59E+02  2.89E+03  4.09E+02 -2.45E+03  2.21E+03  8.46E+02 -1.50E+03 -1.18E+03  1.82E+03 -9.45E+01  7.13E+02  7.63E+02
         -2.16E+03 -8.47E+02  4.63E+02  5.91E+02 -1.02E+03  2.50E+02  4.37E+02 -5.79E+02 -2.96E+04  0.00E+00  7.66E+05
 
 Elapsed postprocess time in seconds:     0.02
 #CPUT: Total CPU Time in Seconds,     4626.834
Stop Time: 
Wed 04/20/2016 
03:14 PM

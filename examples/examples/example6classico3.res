Wed 11/02/2016 
01:35 AM
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


$PK
;BAYES_EXTRA_REQUEST=1
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

;Initial Omegas
$OMEGA BLOCK(8) VALUES(0.3,0.001)

; Omega prior
$PRIOR NWPRI 
$OMEGAP BLOCK(8) FIXED VALUES(0.2,0.0)
$OMEGAPD (8.0 FIXED)

$SIGMA  
0.1 ;[p]
(0.3);[p]

$EST METHOD=ITS INTERACTION NITER=20 SIGL=4 file=example6classico3_its.ext NOPRIOR=1 PRINT=1 NOABORT
$EST METHOD=bayes INTERACTION NBURN=1000 NITER=10000 PRINT=50 RANMETHOD=3P file=example6classico3.ext
     OLKJDF=8.0 OSAMPLE_M1=8 OSAMPLE_M2=8 OSAMPLE_M3=8 NOPRIOR=0
$COV MATRIX=R UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        2 NOV 2016
Days until program expires :4960
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
                  0.3000E+00
                  0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.3000E+00
                  0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.1000E-02   0.3000E+00
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
 0.0000E+00   0.3000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 GRADIENT METHOD USED:     NOSLOW
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 TURN OFF Cholesky Transposition of R Matrix (CHOLROFF): NO
 KNUTHSUMOFF:                                -1
 RESUME COV ANALYSIS (RESUME):               NO
 SIR SAMPLE SIZE (SIRSAMPLE):              -1
 NON-LINEARLY TRANSFORM THETAS DURING COV (THBND): 0
 PRECONDTIONING CYCLES (PRECOND):        0
 PRECONDTIONING TYPES (PRECONDS):        TOS
 FORCED PRECONDTIONING CYCLES (PFCOND):0
 PRECONDTIONING TYPE (PRETYPE):        0
 FORCED POS. DEFINITE SETTING: (FPOSDEF):0
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
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   4
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
 RAW OUTPUT FILE (FILE): example6classico3_its.ext
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
 ITERATIONS (NITER):                        20
 ANEAL SETTING (CONSTRAIN):                 1

 TOLERANCES FOR ESTIMATION/EVALUATION STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   4
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 TOLERANCES FOR COVARIANCE STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   4
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

 iteration            0 OBJ=  -2767.90326060841
 iteration            1 OBJ=  -2945.79551570364
 iteration            2 OBJ=  -3066.68176060081
 iteration            3 OBJ=  -3178.93689300852
 iteration            4 OBJ=  -3287.32858407963
 iteration            5 OBJ=  -3393.52770515406
 iteration            6 OBJ=  -3498.14510483500
 iteration            7 OBJ=  -3601.58924645730
 iteration            8 OBJ=  -3704.06176282430
 iteration            9 OBJ=  -3805.72400249198
 iteration           10 OBJ=  -3906.61063461626
 iteration           11 OBJ=  -4006.72509749640
 iteration           12 OBJ=  -4106.01265723516
 iteration           13 OBJ=  -4204.26457678902
 iteration           14 OBJ=  -4301.02961511683
 iteration           15 OBJ=  -4395.56223545047
 iteration           16 OBJ=  -4486.33059984632
 iteration           17 OBJ=  -4570.67020510727
 iteration           18 OBJ=  -4642.90750539041
 iteration           19 OBJ=  -4691.59241202575
 iteration           20 OBJ=  -4707.62806233262
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -3.2737E-03 -1.9211E-03  6.1341E-03  3.8643E-03  1.7761E-03  3.9008E-03  3.5100E-04  1.1303E-03
 SE:             6.9342E-02  5.1795E-02  3.6684E-02  6.4436E-02  5.6569E-02  5.6616E-02  6.4116E-02  6.1169E-02
 N:                      50          50          50          50          50          50          50          50
 
 P VAL.:         9.6235E-01  9.7041E-01  8.6720E-01  9.5218E-01  9.7495E-01  9.4507E-01  9.9563E-01  9.8526E-01
 
 ETASHRINKSD(%)  5.8986E-01  3.5467E+00  6.7581E+00  8.4186E-01  1.3364E+00  5.1164E+00  2.9368E-01  1.2777E+00
 ETASHRINKVR(%)  1.1762E+00  6.9677E+00  1.3060E+01  1.6766E+00  2.6550E+00  9.9710E+00  5.8650E-01  2.5391E+00
 EBVSHRINKSD(%)  6.2400E-01  5.4511E+00  9.9701E+00  2.1194E+00  1.5709E+00  6.3996E+00  4.4774E-01  1.7801E+00
 EBVSHRINKVR(%)  1.2441E+00  1.0605E+01  1.8946E+01  4.1939E+00  3.1171E+00  1.2390E+01  8.9348E-01  3.5286E+00
 EPSSHRINKSD(%)  1.6485E+01  8.8636E+00
 EPSSHRINKVR(%)  3.0253E+01  1.6942E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -4707.62806233262     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1825.83682220277     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:    70.57
 Elapsed covariance  time in seconds:     0.25
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -4707.628       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.19E+00  5.47E-01 -1.92E-01  2.26E+00  2.09E-01  3.71E+00 -7.10E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.48E-01
 
 ETA2
+       -3.36E-02  1.47E-01
 
 ETA3
+        4.67E-02 -1.49E-02  7.90E-02
 
 ETA4
+        3.20E-02  4.56E-02 -2.91E-02  2.15E-01
 
 ETA5
+        2.60E-02  2.85E-02 -3.78E-03 -3.37E-02  1.68E-01
 
 ETA6
+       -3.04E-02  8.99E-03  2.91E-02  2.11E-02 -8.23E-02  1.82E-01
 
 ETA7
+        2.82E-02 -3.13E-02  3.28E-02 -7.10E-02  2.35E-02  5.28E-03  2.11E-01
 
 ETA8
+        9.87E-02  8.08E-02  2.89E-02  4.13E-02  7.55E-05 -4.73E-02  5.52E-02  1.96E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.55E-03
 
 EPS2
+        0.00E+00  2.35E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        4.98E-01
 
 ETA2
+       -1.76E-01  3.84E-01
 
 ETA3
+        3.34E-01 -1.38E-01  2.81E-01
 
 ETA4
+        1.38E-01  2.56E-01 -2.23E-01  4.64E-01
 
 ETA5
+        1.28E-01  1.82E-01 -3.28E-02 -1.77E-01  4.10E-01
 
 ETA6
+       -1.43E-01  5.50E-02  2.43E-01  1.06E-01 -4.71E-01  4.26E-01
 
 ETA7
+        1.23E-01 -1.78E-01  2.54E-01 -3.33E-01  1.25E-01  2.70E-02  4.59E-01
 
 ETA8
+        4.48E-01  4.76E-01  2.32E-01  2.01E-01  4.17E-04 -2.51E-01  2.72E-01  4.43E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.77E-02
 
 EPS2
+        0.00E+00  1.53E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         2.77E-01  1.91E-01  2.57E-01  2.94E-01  1.87E-01  2.45E-01  1.56E-01  2.89E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.57E-01
 
 ETA2
+        2.07E-01  3.82E-01
 
 ETA3
+        1.02E-01  1.64E-01  1.33E-01
 
 ETA4
+        8.89E-02  1.67E-01  7.45E-02  1.56E-01
 
 ETA5
+        1.62E-01  8.89E-02  8.33E-02  1.09E-01  2.37E-01
 
 ETA6
+        1.13E-01  1.01E-01  1.21E-01  1.60E-01  6.64E-02  1.22E-01
 
 ETA7
+        1.98E-01  1.08E-01  1.01E-01  8.96E-02  1.25E-01  8.84E-02  1.36E-01
 
 ETA8
+        2.07E-01  9.08E-02  4.90E-02  9.29E-02  1.58E-01  1.54E-01  1.36E-01  1.74E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        3.25E-03
 
 EPS2
+        0.00E+00  4.55E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.57E-01
 
 ETA2
+        8.53E-01  4.98E-01
 
 ETA3
+        6.86E-01  1.29E+00  2.37E-01
 
 ETA4
+        3.81E-01  7.17E-01  5.68E-01  1.68E-01
 
 ETA5
+        8.22E-01  5.66E-01  7.25E-01  5.98E-01  2.89E-01
 
 ETA6
+        5.23E-01  6.15E-01  9.65E-01  7.90E-01  4.71E-01  1.43E-01
 
 ETA7
+        8.47E-01  4.78E-01  7.03E-01  3.79E-01  7.29E-01  4.50E-01  1.48E-01
 
 ETA8
+        6.96E-01  7.11E-01  2.68E-01  3.99E-01  8.74E-01  7.89E-01  5.89E-01  1.96E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        1.67E-02
 
 EPS2
+       .........  1.48E-02
 
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
+        7.66E-02
 
 TH 2
+        3.39E-03  3.64E-02
 
 TH 3
+        1.28E-02  1.37E-02  6.59E-02
 
 TH 4
+        4.44E-02 -1.22E-02 -3.80E-02  8.67E-02
 
 TH 5
+       -2.47E-02  1.32E-02  2.32E-02 -3.88E-02  3.50E-02
 
 TH 6
+       -3.23E-02 -1.11E-02 -2.02E-02 -5.25E-04  4.60E-03  5.99E-02
 
 TH 7
+       -1.83E-02 -3.75E-03  1.03E-02 -2.52E-02  1.38E-02  1.14E-02  2.44E-02
 
 TH 8
+        3.53E-02 -1.21E-02 -3.32E-02  6.49E-02 -3.17E-02  4.08E-03 -1.21E-02  8.37E-02
 
 OM11
+       -2.20E-02 -8.77E-03 -2.47E-02  4.21E-03  9.86E-04  1.46E-02  1.39E-03  5.87E-03  2.46E-02
 
 OM12
+        1.85E-02  2.01E-02  3.63E-02 -1.78E-02  1.14E-02 -2.02E-02 -3.26E-03 -2.98E-02 -2.18E-02  4.30E-02
 
 OM13
+       -1.80E-02 -7.02E-03 -4.41E-03 -8.89E-03  4.93E-03  9.99E-03  4.64E-03 -2.45E-03  1.10E-02 -1.28E-02  1.05E-02
 
 OM14
+       -7.30E-03  9.80E-04  8.40E-03 -1.62E-02  6.95E-03 -6.82E-03  6.93E-03 -1.41E-02 -1.19E-03  2.46E-03  1.15E-03  7.90E-03
 
 OM15
+        8.82E-03 -1.25E-02 -1.76E-02  2.67E-02 -1.30E-02  1.67E-02 -8.10E-03  2.05E-02  5.25E-03 -8.79E-03  5.65E-04 -1.09E-02
          2.61E-02
 
 OM16
+       -5.81E-03  1.11E-02  1.16E-02 -1.75E-02  1.11E-02 -6.93E-03  3.38E-03 -8.52E-03 -2.34E-03  6.67E-03  4.91E-04  4.33E-03
         -1.33E-02  1.27E-02
 
 OM17
+       -6.24E-03 -1.06E-02 -4.16E-02  3.38E-02 -1.29E-02  1.77E-02 -7.71E-03  3.60E-02  2.28E-02 -3.00E-02  6.67E-03 -8.87E-03
          1.61E-02 -7.62E-03  3.93E-02
 
 OM18
+       -1.29E-02 -7.63E-03 -4.25E-02  2.82E-02 -9.00E-03  2.57E-02 -5.58E-03  2.59E-02  2.30E-02 -2.29E-02  4.33E-03 -8.81E-03
          1.66E-02 -8.73E-03  3.68E-02  4.28E-02
 
 OM22
+       -4.81E-02 -4.01E-02 -7.85E-02  2.50E-02 -2.18E-02  5.25E-02  3.09E-03  4.00E-02  4.13E-02 -7.05E-02  2.23E-02 -8.27E-03
          2.60E-02 -1.73E-02  5.43E-02  5.16E-02  1.46E-01
 
 OM23
+        1.46E-02  1.36E-02  3.34E-02 -2.02E-02  1.13E-02 -2.07E-02  4.30E-03 -2.35E-02 -1.79E-02  2.67E-02 -9.18E-03  6.84E-03
         -1.51E-02  7.62E-03 -2.63E-02 -2.49E-02 -5.55E-02  2.70E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -8.97E-03 -1.72E-02 -2.79E-02  2.06E-02 -1.34E-02  2.08E-02 -6.29E-03  1.97E-02  1.13E-02 -1.74E-02  4.49E-03 -9.39E-03
          2.08E-02 -1.10E-02  2.06E-02  2.25E-02  4.74E-02 -2.24E-02  2.78E-02
 
 OM25
+       -8.83E-05  2.07E-03  2.64E-04 -6.23E-03  1.82E-03 -2.39E-03  3.73E-03 -3.91E-03  1.04E-03 -1.71E-03  5.29E-04  3.10E-03
         -6.04E-03  3.12E-03 -2.05E-03 -4.38E-03 -1.03E-03  3.55E-03 -7.26E-03  7.90E-03
 
 OM26
+        2.34E-03  4.85E-03 -7.79E-03  5.60E-03 -4.92E-05  9.11E-03 -2.60E-03  5.84E-03  2.30E-03  7.54E-04 -2.26E-03 -4.88E-03
          5.24E-03  4.93E-04  6.32E-03  9.49E-03  2.92E-03 -2.20E-03  4.73E-03 -9.71E-04  1.03E-02
 
 OM27
+        3.60E-03  1.36E-02  1.38E-02 -1.27E-02  9.94E-03 -1.13E-02  1.83E-03 -1.54E-02 -4.97E-03  1.40E-02 -3.94E-03  3.60E-03
         -1.06E-02  7.03E-03 -1.05E-02 -8.80E-03 -3.02E-02  1.32E-02 -1.42E-02  3.58E-03  1.38E-03  1.17E-02
 
 OM28
+       -1.29E-02 -4.93E-04 -7.00E-03 -8.84E-03  2.58E-03  3.01E-03  7.28E-04 -1.30E-02  5.99E-03 -3.06E-04  2.64E-03  6.65E-04
          9.09E-05  3.82E-04  8.69E-04  3.15E-03  1.05E-02 -3.60E-03  3.93E-03  1.34E-03  4.36E-04  5.50E-04  8.25E-03
 
 OM33
+        7.56E-03 -1.30E-02 -2.38E-02  2.46E-02 -1.73E-02  6.28E-03 -6.20E-03  3.04E-02  6.97E-03 -2.00E-02  2.26E-03 -6.03E-03
          1.27E-02 -7.48E-03  1.87E-02  1.45E-02  3.62E-02 -1.56E-02  1.49E-02 -1.67E-03  1.89E-03 -1.04E-02 -2.16E-03  1.78E-02
 
 OM34
+       -2.17E-03 -1.10E-03  5.63E-03 -4.45E-03  3.36E-03 -6.52E-03  4.56E-03 -2.08E-03  1.17E-04 -1.30E-03  1.66E-03  4.15E-03
         -6.88E-03  2.91E-03 -2.98E-03 -5.99E-03 -4.92E-03  3.51E-03 -6.64E-03  3.00E-03 -4.14E-03  1.91E-03 -9.76E-04 -2.57E-03
         5.55E-03
 
 OM35
+        4.19E-03 -1.28E-03 -1.09E-02  1.16E-02 -6.56E-03  8.70E-03 -4.53E-03  7.37E-03  1.33E-03 -3.97E-03 -1.54E-03 -4.20E-03
          9.14E-03 -5.60E-03  7.27E-03  9.41E-03  1.18E-02 -5.53E-03  7.28E-03 -2.47E-03  3.51E-03 -3.75E-03 -5.55E-04  5.37E-03
        -4.75E-03  6.94E-03
 
 OM36
+        1.47E-02  1.20E-03 -1.20E-02  1.49E-02 -1.26E-02 -1.44E-02 -1.10E-02  1.38E-02 -3.58E-04 -3.98E-04 -4.77E-03 -3.39E-03
          3.01E-03  2.33E-04  6.70E-03  3.96E-03  1.29E-03 -2.10E-03  4.05E-03 -2.21E-04  3.42E-03  9.88E-05  9.21E-04  6.91E-03
        -1.76E-03  1.05E-03  1.46E-02
 
 OM37
+       -7.53E-03 -2.82E-03 -1.79E-02  7.04E-03 -5.95E-03  8.67E-03 -5.41E-03  5.37E-03  9.05E-03 -8.78E-03  3.27E-03 -4.47E-03
          8.89E-03 -3.34E-03  1.19E-02  1.33E-02  2.42E-02 -1.26E-02  1.24E-02 -2.88E-03  3.77E-03 -5.00E-03  4.57E-03  6.64E-03
        -4.79E-03  4.57E-03  4.15E-03  1.02E-02
 
 OM38
+        4.89E-04 -4.54E-03 -5.91E-03  5.13E-03 -3.78E-03  4.54E-03 -1.16E-03  6.81E-03  3.28E-03 -6.26E-03  2.12E-03 -1.89E-03
          4.56E-03 -2.08E-03  5.70E-03  4.71E-03  1.12E-02 -4.91E-03  4.48E-03 -6.21E-04  1.09E-03 -2.88E-03 -3.36E-04  4.90E-03
        -1.16E-03  2.01E-03  7.94E-04  2.75E-03  2.40E-03
 
 OM44
+       -2.61E-02 -8.38E-03  1.15E-02 -2.54E-02  1.22E-02  5.61E-03  8.65E-03 -1.91E-02  5.26E-03 -2.90E-03  1.01E-02  4.00E-03
         -1.73E-03  7.12E-04 -5.74E-03 -6.04E-03  9.10E-03 -2.88E-03  4.03E-03 -2.31E-03 -6.68E-03 -3.02E-03  4.68E-03 -4.77E-03
         2.09E-03 -5.10E-03 -7.99E-03  7.60E-04 -4.25E-04  2.45E-02
 
 OM45
+        7.07E-03  8.06E-03  4.31E-03 -2.59E-04  1.92E-03  4.11E-03 -2.26E-03 -7.76E-03 -4.87E-03  1.25E-02 -4.67E-03 -1.93E-03
          2.40E-03  1.97E-04 -5.86E-03 -8.11E-04 -1.70E-02  5.06E-03 -2.24E-03 -5.80E-04  6.11E-03  4.07E-03  3.45E-04 -5.06E-03
        -4.22E-03  2.99E-03 -5.57E-04  9.61E-04 -4.48E-04 -6.73E-03  1.19E-02
 
 OM46
+       -2.83E-02 -6.94E-03 -8.56E-03 -1.51E-02  9.11E-03  2.19E-02  1.42E-02 -3.33E-04  1.26E-02 -2.05E-02  1.13E-02  3.07E-03
         -4.64E-03  3.77E-03  6.96E-03  5.19E-03  3.49E-02 -9.92E-03  2.22E-03  5.45E-03 -3.14E-04 -3.72E-03  2.65E-03  2.44E-03
         3.56E-03 -2.42E-03 -9.41E-03  9.31E-04  1.87E-03  8.21E-03 -5.94E-03  2.57E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.80E-03  6.15E-03  2.46E-03 -2.96E-03  6.64E-03  4.94E-03  5.87E-03 -9.99E-04  2.16E-03 -2.03E-04  7.22E-04  1.88E-03
         -5.51E-03  3.91E-03  2.87E-04  1.47E-03 -5.04E-03  1.76E-03 -6.17E-03  2.18E-03  1.41E-03  3.93E-03 -6.28E-04 -4.33E-03
         1.87E-03 -1.29E-03 -4.50E-03 -2.31E-03 -8.36E-04 -3.10E-03  2.18E-03  5.68E-03  8.02E-03
 
 OM48
+       -1.43E-02 -5.90E-03 -1.59E-02  3.10E-03 -1.84E-03  1.03E-02  1.63E-03  1.20E-03  9.04E-03 -1.13E-02  4.09E-03 -5.01E-04
          4.82E-03 -4.42E-03  1.02E-02  1.21E-02  2.62E-02 -1.07E-02  9.91E-03 -1.83E-03  3.62E-04 -4.57E-03  3.90E-03  4.03E-03
        -1.17E-03  2.39E-03 -7.06E-04  5.33E-03  1.07E-03  4.22E-03 -2.42E-03  4.58E-03 -8.08E-05  8.64E-03
 
 OM55
+       -8.95E-03  1.27E-02  2.82E-02 -4.00E-02  2.08E-02 -1.49E-02  8.80E-03 -4.97E-02 -3.15E-03  2.86E-02 -7.99E-04  7.42E-03
         -1.40E-02  9.88E-03 -2.55E-02 -2.09E-02 -4.52E-02  2.02E-02 -1.69E-02  7.19E-03 -1.00E-03  1.65E-02  1.16E-02 -2.32E-02
         3.64E-03 -1.05E-02 -2.36E-03 -5.01E-03 -6.21E-03  1.12E-02  5.92E-03 -3.73E-03  2.27E-03 -4.91E-03  5.61E-02
 
 OM56
+        2.27E-03  3.99E-03 -6.79E-03  2.47E-03 -3.01E-03 -7.99E-04 -4.80E-03  9.13E-04  3.68E-04  3.87E-04 -2.04E-03 -1.59E-03
          5.62E-04  6.21E-04  3.14E-03  4.00E-03  2.02E-03 -1.18E-03  1.36E-03  5.62E-04  3.04E-03  1.22E-03  1.26E-03  1.37E-03
        -2.38E-03  2.13E-03  4.43E-03  2.73E-03  3.48E-04 -4.60E-03  2.23E-03 -2.92E-03 -8.61E-04  3.96E-04 -9.35E-04  4.41E-03
 
 OM57
+       -6.91E-03 -6.32E-03 -2.72E-02  1.62E-02 -6.42E-03  1.23E-02 -3.39E-03  1.82E-02  1.38E-02 -1.98E-02  5.02E-03 -4.40E-03
          8.48E-03 -4.31E-03  2.15E-02  2.04E-02  3.72E-02 -1.71E-02  1.18E-02  2.55E-04  3.57E-03 -5.75E-03  1.72E-03  1.14E-02
        -2.05E-03  4.62E-03  2.82E-03  7.86E-03  3.77E-03 -3.09E-03 -2.92E-03  7.53E-03  1.28E-04  6.86E-03 -1.49E-02  2.58E-03
          1.57E-02
 
 OM58
+        3.28E-03 -1.22E-02 -3.24E-02  3.01E-02 -1.59E-02  1.33E-02 -5.88E-03  3.16E-02  1.36E-02 -2.49E-02  4.18E-03 -7.00E-03
          1.61E-02 -8.95E-03  2.63E-02  2.20E-02  4.50E-02 -2.03E-02  1.60E-02  1.60E-03  3.00E-03 -1.03E-02  1.06E-04  1.77E-02
        -1.96E-03  6.52E-03  5.36E-03  8.28E-03  5.24E-03 -6.44E-03 -4.09E-03  6.27E-03 -1.53E-03  6.51E-03 -2.17E-02  1.55E-03
          1.65E-02  2.51E-02
 
 OM66
+       -1.14E-02 -4.79E-04 -4.44E-03 -6.39E-03  2.83E-03  1.48E-02  3.63E-03 -3.08E-03  3.60E-04 -3.02E-03  1.11E-03 -9.12E-04
          3.87E-03 -1.70E-03 -9.47E-05  4.57E-03  1.33E-02 -3.34E-03  6.16E-03 -3.94E-03  3.71E-03 -3.73E-03  4.96E-04  1.65E-03
        -4.33E-03  4.69E-03 -3.54E-03  4.06E-03  1.48E-03  1.25E-03  3.46E-03  4.94E-03  1.49E-04  2.78E-03 -8.84E-03 -7.98E-04
          1.04E-03 -1.96E-04  1.49E-02
 
 OM67
+        1.71E-03 -1.08E-02 -2.91E-03  3.75E-03 -8.08E-03 -1.02E-03 -1.02E-03  4.19E-03 -1.43E-03 -4.07E-03  6.57E-04 -5.59E-04
          4.44E-03 -4.48E-03 -6.16E-04 -2.22E-03  1.09E-02 -2.98E-03  6.28E-03 -1.68E-03 -2.84E-03 -5.82E-03 -3.81E-05  5.21E-03
        -2.43E-04  4.75E-04  2.23E-03  1.12E-03  9.14E-04  3.34E-03 -3.24E-03 -1.42E-03 -5.26E-03  1.13E-03 -4.85E-03 -1.28E-03
         -8.84E-04  2.31E-03  1.37E-03  7.82E-03
 
 OM68
+        3.33E-02  5.56E-04 -1.15E-03  2.81E-02 -1.58E-02 -1.40E-02 -1.18E-02  3.02E-02 -8.23E-03  3.46E-03 -8.36E-03 -6.58E-03
          6.57E-03 -6.65E-04  4.59E-03 -2.67E-04 -1.30E-02  1.93E-03  1.98E-03 -2.29E-03  4.76E-03 -1.35E-03 -7.23E-03  8.93E-03
        -1.47E-03  2.06E-03  1.21E-02 -1.16E-03  1.17E-03 -1.34E-02  5.77E-04 -1.24E-02 -3.47E-03 -6.26E-03 -1.25E-02  1.76E-03
         -5.66E-04  5.30E-03 -4.83E-03  2.55E-03  2.38E-02
 
 OM77
+        8.08E-03  4.34E-03  7.00E-03  3.59E-03 -5.57E-04 -1.24E-02 -8.60E-03 -1.13E-02 -5.54E-03  1.50E-02 -5.89E-03 -6.86E-04
          1.66E-03 -3.35E-03 -5.58E-03 -3.06E-03 -2.18E-02  7.16E-03 -1.32E-03 -2.60E-03 -1.92E-03  2.56E-03  1.17E-03 -5.43E-03
        -1.15E-03  2.18E-04  2.91E-03 -7.82E-04 -2.65E-03  5.44E-04  2.00E-03 -1.49E-02 -5.02E-03 -1.32E-03  9.19E-03  8.84E-04
         -5.64E-03 -5.70E-03 -3.57E-03  8.70E-04  1.46E-03  1.84E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.56E-02  5.01E-04 -1.43E-02  3.04E-02 -9.55E-03  4.09E-03 -1.04E-02  2.34E-02  3.90E-03 -2.88E-03 -4.43E-03 -7.71E-03
          1.14E-02 -5.64E-03  1.73E-02  1.88E-02  4.46E-03 -6.54E-03  7.96E-03 -3.67E-03  6.14E-03 -2.05E-03 -3.54E-03  7.53E-03
        -3.96E-03  6.24E-03  5.20E-03  3.73E-03  2.20E-03 -1.13E-02  2.70E-03 -7.91E-03  8.11E-04  1.06E-03 -1.44E-02  2.53E-03
          7.30E-03  1.10E-02 -1.10E-03 -2.92E-03  1.02E-02  4.36E-03  1.86E-02
 
 OM88
+       -1.13E-02  3.98E-03 -2.21E-02  1.12E-02  3.00E-03  1.99E-02 -3.14E-03  1.41E-03  1.42E-02 -3.78E-03  2.85E-04 -5.65E-03
          9.40E-03 -3.49E-03  1.96E-02  2.87E-02  1.73E-02 -1.02E-02  1.04E-02 -2.61E-03  1.03E-02  9.12E-04  5.26E-03  8.94E-04
        -6.18E-03  7.12E-03  2.09E-04  9.12E-03  1.90E-03 -5.53E-03  7.22E-03  2.49E-04  4.47E-03  7.45E-03 -1.82E-03  4.28E-03
          1.11E-02  8.54E-03  4.79E-03 -7.05E-03 -5.22E-03  1.93E-03  1.45E-02  3.02E-02
 
 SG11
+        4.77E-04 -2.43E-04 -3.90E-04  8.27E-04 -4.34E-04  1.51E-05 -2.46E-04  7.35E-04  4.86E-05 -2.24E-04 -5.65E-05 -1.84E-04
          3.26E-04 -1.77E-04  3.43E-04  2.64E-04  3.67E-04 -2.64E-04  2.74E-04 -5.47E-05  4.76E-05 -1.72E-04 -8.12E-05  3.12E-04
        -5.47E-05  1.09E-04  1.69E-04  9.21E-05  7.70E-05 -2.13E-04 -2.38E-05 -1.05E-04 -6.21E-05  2.33E-05 -4.02E-04  1.85E-05
          1.93E-04  3.49E-04 -8.07E-05  7.01E-05  3.11E-04 -3.41E-05  2.68E-04  5.00E-05  1.06E-05
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -7.56E-04 -4.00E-04 -4.20E-04 -2.44E-04  3.90E-05  5.69E-04  2.57E-04  1.29E-04  3.31E-04 -6.86E-04  3.23E-04  3.55E-05
          9.47E-05 -1.09E-04  3.27E-04  2.96E-04  1.30E-03 -4.43E-04  3.29E-04 -3.73E-05 -9.49E-05 -2.82E-04  3.24E-05  2.47E-04
         1.46E-05  4.07E-05 -1.97E-04  1.37E-04  1.02E-04  3.16E-04 -2.43E-04  4.80E-04 -1.67E-05  2.35E-04 -4.55E-04 -5.81E-05
          2.68E-04  2.55E-04  2.12E-04  9.71E-05 -3.26E-04 -2.71E-04 -1.26E-04  1.40E-06 -1.45E-06  0.00E+00  2.07E-05
 
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
+        2.77E-01
 
 TH 2
+        6.41E-02  1.91E-01
 
 TH 3
+        1.79E-01  2.79E-01  2.57E-01
 
 TH 4
+        5.45E-01 -2.18E-01 -5.03E-01  2.94E-01
 
 TH 5
+       -4.78E-01  3.71E-01  4.83E-01 -7.05E-01  1.87E-01
 
 TH 6
+       -4.77E-01 -2.39E-01 -3.21E-01 -7.28E-03  1.00E-01  2.45E-01
 
 TH 7
+       -4.23E-01 -1.26E-01  2.56E-01 -5.47E-01  4.74E-01  3.00E-01  1.56E-01
 
 TH 8
+        4.40E-01 -2.19E-01 -4.47E-01  7.62E-01 -5.85E-01  5.76E-02 -2.68E-01  2.89E-01
 
 OM11
+       -5.08E-01 -2.93E-01 -6.13E-01  9.13E-02  3.36E-02  3.81E-01  5.68E-02  1.29E-01  1.57E-01
 
 OM12
+        3.22E-01  5.07E-01  6.83E-01 -2.92E-01  2.94E-01 -3.99E-01 -1.01E-01 -4.96E-01 -6.70E-01  2.07E-01
 
 OM13
+       -6.37E-01 -3.60E-01 -1.68E-01 -2.95E-01  2.58E-01  3.99E-01  2.91E-01 -8.27E-02  6.84E-01 -6.05E-01  1.02E-01
 
 OM14
+       -2.97E-01  5.78E-02  3.68E-01 -6.18E-01  4.18E-01 -3.13E-01  4.99E-01 -5.47E-01 -8.52E-02  1.33E-01  1.27E-01  8.89E-02
 
 OM15
+        1.97E-01 -4.07E-01 -4.24E-01  5.61E-01 -4.30E-01  4.23E-01 -3.21E-01  4.38E-01  2.07E-01 -2.62E-01  3.42E-02 -7.60E-01
          1.62E-01
 
 OM16
+       -1.86E-01  5.17E-01  4.00E-01 -5.29E-01  5.25E-01 -2.51E-01  1.92E-01 -2.61E-01 -1.33E-01  2.86E-01  4.26E-02  4.32E-01
         -7.31E-01  1.13E-01
 
 OM17
+       -1.14E-01 -2.80E-01 -8.17E-01  5.79E-01 -3.48E-01  3.64E-01 -2.49E-01  6.27E-01  7.32E-01 -7.30E-01  3.29E-01 -5.03E-01
          5.02E-01 -3.41E-01  1.98E-01
 
 OM18
+       -2.25E-01 -1.93E-01 -8.00E-01  4.63E-01 -2.32E-01  5.08E-01 -1.73E-01  4.33E-01  7.08E-01 -5.34E-01  2.05E-01 -4.79E-01
          4.97E-01 -3.75E-01  8.97E-01  2.07E-01
 
 OM22
+       -4.55E-01 -5.50E-01 -8.01E-01  2.22E-01 -3.05E-01  5.61E-01  5.18E-02  3.62E-01  6.90E-01 -8.91E-01  5.70E-01 -2.43E-01
          4.21E-01 -4.01E-01  7.17E-01  6.53E-01  3.82E-01
 
 OM23
+        3.20E-01  4.35E-01  7.91E-01 -4.17E-01  3.66E-01 -5.14E-01  1.67E-01 -4.94E-01 -6.94E-01  7.83E-01 -5.46E-01  4.68E-01
         -5.67E-01  4.11E-01 -8.06E-01 -7.32E-01 -8.83E-01  1.64E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -1.94E-01 -5.39E-01 -6.52E-01  4.19E-01 -4.31E-01  5.09E-01 -2.42E-01  4.08E-01  4.34E-01 -5.04E-01  2.64E-01 -6.34E-01
          7.72E-01 -5.85E-01  6.22E-01  6.51E-01  7.45E-01 -8.16E-01  1.67E-01
 
 OM25
+       -3.59E-03  1.22E-01  1.16E-02 -2.38E-01  1.09E-01 -1.10E-01  2.69E-01 -1.52E-01  7.49E-02 -9.26E-02  5.82E-02  3.92E-01
         -4.21E-01  3.11E-01 -1.16E-01 -2.38E-01 -3.02E-02  2.43E-01 -4.90E-01  8.89E-02
 
 OM26
+        8.33E-02  2.51E-01 -2.99E-01  1.88E-01 -2.59E-03  3.67E-01 -1.64E-01  1.99E-01  1.45E-01  3.59E-02 -2.18E-01 -5.41E-01
          3.20E-01  4.32E-02  3.14E-01  4.52E-01  7.53E-02 -1.32E-01  2.80E-01 -1.08E-01  1.01E-01
 
 OM27
+        1.20E-01  6.58E-01  4.98E-01 -4.00E-01  4.91E-01 -4.28E-01  1.08E-01 -4.91E-01 -2.93E-01  6.22E-01 -3.56E-01  3.74E-01
         -6.08E-01  5.76E-01 -4.87E-01 -3.93E-01 -7.31E-01  7.41E-01 -7.89E-01  3.72E-01  1.26E-01  1.08E-01
 
 OM28
+       -5.14E-01 -2.84E-02 -3.00E-01 -3.31E-01  1.52E-01  1.35E-01  5.13E-02 -4.95E-01  4.21E-01 -1.62E-02  2.84E-01  8.24E-02
          6.20E-03  3.73E-02  4.82E-02  1.68E-01  3.02E-01 -2.41E-01  2.60E-01  1.66E-01  4.73E-02  5.59E-02  9.08E-02
 
 OM33
+        2.05E-01 -5.11E-01 -6.95E-01  6.26E-01 -6.95E-01  1.93E-01 -2.98E-01  7.89E-01  3.34E-01 -7.24E-01  1.66E-01 -5.09E-01
          5.88E-01 -4.98E-01  7.06E-01  5.25E-01  7.11E-01 -7.14E-01  6.69E-01 -1.41E-01  1.40E-01 -7.23E-01 -1.79E-01  1.33E-01
 
 OM34
+       -1.05E-01 -7.73E-02  2.94E-01 -2.03E-01  2.41E-01 -3.57E-01  3.92E-01 -9.66E-02  9.98E-03 -8.44E-02  2.17E-01  6.27E-01
         -5.72E-01  3.47E-01 -2.01E-01 -3.89E-01 -1.73E-01  2.87E-01 -5.35E-01  4.54E-01 -5.48E-01  2.37E-01 -1.44E-01 -2.59E-01
         7.45E-02
 
 OM35
+        1.82E-01 -8.04E-02 -5.08E-01  4.72E-01 -4.20E-01  4.26E-01 -3.49E-01  3.06E-01  1.02E-01 -2.30E-01 -1.81E-01 -5.67E-01
          6.79E-01 -5.96E-01  4.40E-01  5.46E-01  3.71E-01 -4.04E-01  5.24E-01 -3.33E-01  4.15E-01 -4.16E-01 -7.33E-02  4.84E-01
        -7.66E-01  8.33E-02
 
 OM36
+        4.41E-01  5.21E-02 -3.88E-01  4.20E-01 -5.57E-01 -4.86E-01 -5.82E-01  3.95E-01 -1.89E-02 -1.59E-02 -3.86E-01 -3.16E-01
          1.54E-01  1.71E-02  2.80E-01  1.59E-01  2.80E-02 -1.06E-01  2.01E-01 -2.05E-02  2.79E-01  7.55E-03  8.39E-02  4.29E-01
        -1.96E-01  1.04E-01  1.21E-01
 
 OM37
+       -2.70E-01 -1.47E-01 -6.90E-01  2.37E-01 -3.15E-01  3.52E-01 -3.44E-01  1.84E-01  5.73E-01 -4.20E-01  3.17E-01 -4.99E-01
          5.46E-01 -2.94E-01  5.96E-01  6.36E-01  6.29E-01 -7.57E-01  7.35E-01 -3.22E-01  3.68E-01 -4.58E-01  4.99E-01  4.94E-01
        -6.38E-01  5.44E-01  3.41E-01  1.01E-01
 
 OM38
+        3.60E-02 -4.85E-01 -4.69E-01  3.56E-01 -4.12E-01  3.78E-01 -1.51E-01  4.80E-01  4.27E-01 -6.16E-01  4.22E-01 -4.33E-01
          5.76E-01 -3.77E-01  5.87E-01  4.65E-01  5.97E-01 -6.09E-01  5.48E-01 -1.42E-01  2.19E-01 -5.42E-01 -7.55E-02  7.50E-01
        -3.19E-01  4.93E-01  1.34E-01  5.57E-01  4.90E-02
 
 OM44
+       -6.03E-01 -2.81E-01  2.85E-01 -5.52E-01  4.17E-01  1.47E-01  3.54E-01 -4.23E-01  2.15E-01 -8.94E-02  6.34E-01  2.88E-01
         -6.86E-02  4.04E-02 -1.85E-01 -1.86E-01  1.52E-01 -1.12E-01  1.54E-01 -1.66E-01 -4.21E-01 -1.78E-01  3.30E-01 -2.29E-01
         1.80E-01 -3.91E-01 -4.23E-01  4.82E-02 -5.54E-02  1.56E-01
 
 OM45
+        2.34E-01  3.87E-01  1.54E-01 -8.06E-03  9.39E-02  1.54E-01 -1.33E-01 -2.46E-01 -2.85E-01  5.54E-01 -4.19E-01 -2.00E-01
          1.36E-01  1.61E-02 -2.71E-01 -3.59E-02 -4.08E-01  2.82E-01 -1.23E-01 -5.98E-02  5.53E-01  3.44E-01  3.48E-02 -3.48E-01
        -5.20E-01  3.29E-01 -4.23E-02  8.75E-02 -8.39E-02 -3.95E-01  1.09E-01
 
 OM46
+       -6.38E-01 -2.27E-01 -2.08E-01 -3.20E-01  3.04E-01  5.59E-01  5.67E-01 -7.18E-03  5.01E-01 -6.18E-01  6.92E-01  2.16E-01
         -1.79E-01  2.09E-01  2.19E-01  1.56E-01  5.70E-01 -3.77E-01  8.30E-02  3.83E-01 -1.93E-02 -2.14E-01  1.82E-01  1.14E-01
         2.98E-01 -1.81E-01 -4.86E-01  5.76E-02  2.38E-01  3.28E-01 -3.40E-01  1.60E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.53E-01  3.60E-01  1.07E-01 -1.12E-01  3.96E-01  2.26E-01  4.20E-01 -3.86E-02  1.54E-01 -1.09E-02  7.88E-02  2.37E-01
         -3.81E-01  3.87E-01  1.61E-02  7.94E-02 -1.47E-01  1.20E-01 -4.13E-01  2.73E-01  1.55E-01  4.05E-01 -7.72E-02 -3.63E-01
         2.80E-01 -1.72E-01 -4.16E-01 -2.56E-01 -1.90E-01 -2.21E-01  2.23E-01  3.96E-01  8.96E-02
 
 OM48
+       -5.56E-01 -3.33E-01 -6.66E-01  1.13E-01 -1.06E-01  4.55E-01  1.12E-01  4.47E-02  6.20E-01 -5.87E-01  4.30E-01 -6.06E-02
          3.21E-01 -4.22E-01  5.53E-01  6.31E-01  7.39E-01 -7.03E-01  6.40E-01 -2.22E-01  3.84E-02 -4.55E-01  4.61E-01  3.25E-01
        -1.69E-01  3.09E-01 -6.28E-02  5.69E-01  2.34E-01  2.90E-01 -2.39E-01  3.08E-01 -9.71E-03  9.29E-02
 
 OM55
+       -1.37E-01  2.81E-01  4.64E-01 -5.74E-01  4.70E-01 -2.57E-01  2.38E-01 -7.26E-01 -8.48E-02  5.83E-01 -3.30E-02  3.52E-01
         -3.65E-01  3.70E-01 -5.42E-01 -4.26E-01 -4.99E-01  5.18E-01 -4.27E-01  3.41E-01 -4.18E-02  6.42E-01  5.37E-01 -7.34E-01
         2.06E-01 -5.30E-01 -8.24E-02 -2.10E-01 -5.34E-01  3.03E-01  2.29E-01 -9.83E-02  1.07E-01 -2.23E-01  2.37E-01
 
 OM56
+        1.24E-01  3.15E-01 -3.98E-01  1.27E-01 -2.42E-01 -4.92E-02 -4.62E-01  4.75E-02  3.53E-02  2.81E-02 -3.00E-01 -2.70E-01
          5.23E-02  8.30E-02  2.38E-01  2.91E-01  7.96E-02 -1.08E-01  1.23E-01  9.52E-02  4.52E-01  1.70E-01  2.10E-01  1.55E-01
        -4.81E-01  3.85E-01  5.53E-01  4.07E-01  1.07E-01 -4.43E-01  3.07E-01 -2.74E-01 -1.45E-01  6.42E-02 -5.94E-02  6.64E-02
 
 OM57
+       -2.00E-01 -2.64E-01 -8.46E-01  4.39E-01 -2.74E-01  4.02E-01 -1.74E-01  5.02E-01  7.01E-01 -7.63E-01  3.92E-01 -3.95E-01
          4.20E-01 -3.06E-01  8.64E-01  7.89E-01  7.79E-01 -8.29E-01  5.65E-01  2.29E-02  2.81E-01 -4.24E-01  1.51E-01  6.86E-01
        -2.20E-01  4.43E-01  1.87E-01  6.23E-01  6.14E-01 -1.58E-01 -2.14E-01  3.75E-01  1.14E-02  5.90E-01 -5.02E-01  3.10E-01
          1.25E-01
 
 OM58
+        7.49E-02 -4.03E-01 -7.97E-01  6.46E-01 -5.37E-01  3.42E-01 -2.38E-01  6.91E-01  5.47E-01 -7.59E-01  2.58E-01 -4.98E-01
          6.29E-01 -5.02E-01  8.38E-01  6.72E-01  7.43E-01 -7.79E-01  6.05E-01  1.14E-01  1.87E-01 -6.02E-01  7.38E-03  8.40E-01
        -1.66E-01  4.94E-01  2.80E-01  5.19E-01  6.75E-01 -2.60E-01 -2.37E-01  2.47E-01 -1.08E-01  4.43E-01 -5.78E-01  1.48E-01
          8.31E-01  1.58E-01
 
 OM66
+       -3.36E-01 -2.05E-02 -1.42E-01 -1.78E-01  1.24E-01  4.95E-01  1.90E-01 -8.72E-02  1.88E-02 -1.19E-01  8.89E-02 -8.39E-02
          1.96E-01 -1.23E-01 -3.91E-03  1.81E-01  2.86E-01 -1.66E-01  3.03E-01 -3.62E-01  2.99E-01 -2.82E-01  4.47E-02  1.01E-01
        -4.75E-01  4.60E-01 -2.40E-01  3.30E-01  2.48E-01  6.53E-02  2.59E-01  2.52E-01  1.36E-02  2.45E-01 -3.05E-01 -9.83E-02
          6.78E-02 -1.01E-02  1.22E-01
 
 OM67
+        6.97E-02 -6.40E-01 -1.28E-01  1.44E-01 -4.88E-01 -4.71E-02 -7.40E-02  1.64E-01 -1.03E-01 -2.22E-01  7.27E-02 -7.11E-02
          3.10E-01 -4.49E-01 -3.51E-02 -1.21E-01  3.24E-01 -2.05E-01  4.26E-01 -2.13E-01 -3.17E-01 -6.08E-01 -4.74E-03  4.42E-01
        -3.69E-02  6.45E-02  2.08E-01  1.25E-01  2.11E-01  2.42E-01 -3.36E-01 -1.00E-01 -6.64E-01  1.38E-01 -2.32E-01 -2.19E-01
         -7.99E-02  1.65E-01  1.27E-01  8.84E-02
 
 OM68
+        7.79E-01  1.89E-02 -2.91E-02  6.18E-01 -5.45E-01 -3.71E-01 -4.91E-01  6.75E-01 -3.40E-01  1.08E-01 -5.30E-01 -4.79E-01
          2.63E-01 -3.82E-02  1.50E-01 -8.35E-03 -2.20E-01  7.60E-02  7.68E-02 -1.67E-01  3.04E-01 -8.05E-02 -5.16E-01  4.34E-01
        -1.28E-01  1.60E-01  6.46E-01 -7.45E-02  1.55E-01 -5.54E-01  3.43E-02 -5.01E-01 -2.51E-01 -4.36E-01 -3.43E-01  1.72E-01
         -2.93E-02  2.17E-01 -2.56E-01  1.87E-01  1.54E-01
 
 OM77
+        2.15E-01  1.68E-01  2.01E-01  9.00E-02 -2.20E-02 -3.73E-01 -4.06E-01 -2.89E-01 -2.61E-01  5.34E-01 -4.25E-01 -5.69E-02
          7.57E-02 -2.19E-01 -2.07E-01 -1.09E-01 -4.21E-01  3.21E-01 -5.84E-02 -2.16E-01 -1.39E-01  1.75E-01  9.52E-02 -3.00E-01
        -1.13E-01  1.93E-02  1.78E-01 -5.73E-02 -3.99E-01  2.57E-02  1.36E-01 -6.87E-01 -4.13E-01 -1.05E-01  2.86E-01  9.82E-02
         -3.33E-01 -2.65E-01 -2.15E-01  7.26E-02  6.99E-02  1.36E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        4.14E-01  1.92E-02 -4.08E-01  7.58E-01 -3.74E-01  1.23E-01 -4.88E-01  5.94E-01  1.83E-01 -1.02E-01 -3.18E-01 -6.36E-01
          5.19E-01 -3.67E-01  6.41E-01  6.65E-01  8.57E-02 -2.92E-01  3.50E-01 -3.03E-01  4.44E-01 -1.39E-01 -2.86E-01  4.14E-01
        -3.89E-01  5.49E-01  3.15E-01  2.72E-01  3.30E-01 -5.29E-01  1.82E-01 -3.62E-01  6.64E-02  8.33E-02 -4.47E-01  2.79E-01
          4.28E-01  5.08E-01 -6.61E-02 -2.42E-01  4.86E-01  2.36E-01  1.36E-01
 
 OM88
+       -2.36E-01  1.20E-01 -4.95E-01  2.20E-01  9.23E-02  4.68E-01 -1.16E-01  2.80E-02  5.20E-01 -1.05E-01  1.60E-02 -3.66E-01
          3.34E-01 -1.78E-01  5.70E-01  7.97E-01  2.60E-01 -3.58E-01  3.58E-01 -1.69E-01  5.83E-01  4.84E-02  3.33E-01  3.86E-02
        -4.77E-01  4.91E-01  9.97E-03  5.21E-01  2.23E-01 -2.03E-01  3.81E-01  8.94E-03  2.87E-01  4.61E-01 -4.42E-02  3.71E-01
          5.08E-01  3.10E-01  2.26E-01 -4.59E-01 -1.94E-01  8.21E-02  6.13E-01  1.74E-01
 
 SG11
+        5.29E-01 -3.91E-01 -4.67E-01  8.63E-01 -7.12E-01  1.90E-02 -4.84E-01  7.81E-01  9.52E-02 -3.32E-01 -1.70E-01 -6.34E-01
          6.20E-01 -4.83E-01  5.32E-01  3.92E-01  2.95E-01 -4.93E-01  5.05E-01 -1.89E-01  1.44E-01 -4.87E-01 -2.75E-01  7.20E-01
        -2.26E-01  4.02E-01  4.29E-01  2.81E-01  4.83E-01 -4.19E-01 -6.72E-02 -2.01E-01 -2.13E-01  7.71E-02 -5.22E-01  8.57E-02
          4.74E-01  6.78E-01 -2.03E-01  2.43E-01  6.19E-01 -7.72E-02  6.04E-01  8.84E-02  3.25E-03
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -6.00E-01 -4.60E-01 -3.59E-01 -1.82E-01  4.59E-02  5.11E-01  3.61E-01  9.77E-02  4.64E-01 -7.27E-01  6.95E-01  8.78E-02
          1.29E-01 -2.12E-01  3.63E-01  3.14E-01  7.48E-01 -5.92E-01  4.33E-01 -9.23E-02 -2.06E-01 -5.71E-01  7.85E-02  4.07E-01
         4.32E-02  1.07E-01 -3.58E-01  2.99E-01  4.57E-01  4.44E-01 -4.91E-01  6.58E-01 -4.10E-02  5.56E-01 -4.22E-01 -1.92E-01
          4.71E-01  3.53E-01  3.82E-01  2.41E-01 -4.64E-01 -4.40E-01 -2.03E-01  1.77E-03 -9.82E-02  0.00E+00  4.55E-03
 
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
+        3.26E+02
 
 TH 2
+        1.83E+02  4.95E+02
 
 TH 3
+       -1.01E+02 -9.19E+00  6.44E+02
 
 TH 4
+       -4.90E+01 -2.65E+01  1.14E+02  3.15E+02
 
 TH 5
+       -9.06E+01 -1.48E+02  1.27E+01  2.81E+01  3.83E+02
 
 TH 6
+       -6.29E+00 -6.70E+01 -9.97E+01 -7.09E+01  1.29E+02  3.07E+02
 
 TH 7
+        5.91E+01  1.61E+02 -2.86E+01  1.13E+02 -9.58E+01 -8.68E+01  3.69E+02
 
 TH 8
+       -2.12E+02 -3.07E+02 -6.89E+01 -9.30E+01  1.37E+02  1.40E+02 -2.26E+02  5.64E+02
 
 OM11
+        8.68E+01  2.07E+01 -1.06E+02  1.31E+01  3.41E+01  1.24E+02  1.07E+02 -4.63E+01  1.15E+03
 
 OM12
+        6.06E+01  2.46E+01  1.09E+02  5.00E+02  1.38E+02  3.82E+01  3.70E+02 -3.55E+02  1.10E+03  4.31E+03
 
 OM13
+       -2.16E+02  3.81E+01 -3.26E+02  6.86E+01 -2.07E+02 -1.20E+01  2.29E+02  1.84E+02 -1.39E+03 -1.28E+03  4.95E+03
 
 OM14
+        4.16E+01  5.57E+02  1.39E+02  1.09E+02 -1.21E+02  6.82E+01  1.39E+02 -1.79E+02 -5.29E+02  2.50E+02  1.04E+03  3.22E+03
 
 OM15
+        1.08E+02  2.11E+02 -2.43E+02 -1.15E+02 -5.09E+02 -4.15E+02  9.94E+00 -1.58E+02 -8.89E+02 -2.21E+03  1.55E+03 -9.05E+01
          3.35E+03
 
 OM16
+        2.57E+02  5.02E+01 -5.56E+01  7.90E+01 -3.88E+02 -3.52E+02  1.36E+02 -2.55E+02  3.37E+02 -3.95E+02 -1.05E+03 -1.12E+03
          9.39E+02  2.21E+03
 
 OM17
+        1.53E+02  1.94E+02  2.33E+02  1.22E+02  4.54E+01  1.77E+02  1.00E+02 -2.07E+02  6.65E+02  2.25E+03 -1.84E+03  1.05E+03
         -1.94E+03 -4.06E+02  3.09E+03
 
 OM18
+       -9.09E+01 -2.97E+02  1.30E+02 -1.30E+02 -9.29E+01 -2.48E+02 -3.22E+02  2.61E+02 -1.33E+03 -3.50E+03  1.60E+03 -8.14E+02
          2.43E+03  5.95E+02 -2.73E+03  4.60E+03
 
 OM22
+        9.58E+01  1.90E+02  4.43E+01  3.11E+02 -4.11E+01 -4.46E+01  2.98E+02 -3.63E+02  4.87E+02  2.70E+03 -3.69E+02  1.18E+02
         -1.04E+03 -2.33E+02  1.17E+03 -2.20E+03  2.51E+03
 
 OM23
+        9.26E+01 -4.20E+01 -2.71E+02 -1.65E+02 -2.60E+02  2.06E+02 -2.03E+02  2.39E+02 -7.78E+02 -1.72E+03  2.50E+03 -2.28E+02
          1.27E+03 -1.34E+02 -1.15E+03  2.14E+03 -6.13E+02  5.04E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        5.62E+02  7.76E+02 -1.67E+02 -1.04E+02 -1.30E+02  1.52E+02  2.07E+02 -5.52E+02  1.78E+02  1.60E+02 -2.50E+02  2.28E+03
         -8.64E+02 -3.78E+02  1.58E+03 -1.11E+03 -3.37E+02  2.50E+01  5.34E+03
 
 OM25
+        1.46E+02  1.12E+02 -2.89E+02 -9.07E+01 -2.90E+02 -4.97E+02  4.00E+01 -2.56E+02 -8.04E+02 -2.34E+03  8.84E+02 -9.07E+02
          3.57E+03  1.26E+03 -2.35E+03  2.81E+03 -1.53E+03 -4.41E+01 -7.36E+02  7.33E+03
 
 OM26
+       -2.87E+00 -5.66E+01  1.45E+02  1.64E+02 -4.17E+02 -5.24E+02  2.59E+02 -6.60E+01 -2.18E+01 -3.46E+02 -4.18E+02 -4.09E+02
          1.16E+03  1.57E+03 -6.81E+02  7.90E+02 -5.10E+02 -8.92E+02 -1.36E+03  2.41E+03  4.25E+03
 
 OM27
+        2.51E+02  1.67E+02 -1.92E+02  1.75E+02  1.41E+02  3.64E+02  2.39E+02 -1.86E+02  7.49E+02  2.41E+03 -8.14E+02  1.60E+03
         -2.30E+03 -7.51E+02  2.46E+03 -2.79E+03  1.38E+03 -1.34E+03  3.14E+03 -3.03E+03 -1.73E+03  5.06E+03
 
 OM28
+       -4.10E+02 -5.30E+02  1.38E+02 -4.45E+02 -1.41E+02 -8.06E+01 -4.45E+02  9.87E+02 -1.20E+03 -5.31E+03  1.49E+03 -1.09E+03
          2.90E+03  9.69E+02 -3.03E+03  5.09E+03 -3.97E+03  2.16E+03 -2.25E+03  3.46E+03  2.50E+03 -4.24E+03  1.01E+04
 
 OM33
+       -1.27E+02 -8.31E+01  1.57E+02  1.51E+02  2.74E+01  9.15E+00 -3.03E+01 -1.46E+02  1.81E+02  1.13E+02 -4.57E+02 -2.05E+02
         -1.31E+02  6.49E+02  3.09E+02 -5.62E+02 -3.38E+02 -6.29E+02  1.60E+02 -2.23E+02  2.46E+02  4.79E+02  1.85E+02  3.77E+03
 
 OM34
+        1.53E+02 -1.49E+02 -2.36E+01 -3.22E+02  1.00E+02  1.56E+02 -2.23E+02  1.68E+02  1.95E+02 -4.64E+02 -8.71E+02 -1.37E+03
          2.68E+01  5.25E+02 -3.52E+02  1.25E+02 -4.01E+02 -5.46E+01 -3.59E+02  1.06E+03  3.21E+02 -3.80E+02  1.03E+03  1.44E+03
         4.18E+03
 
 OM35
+       -2.44E+02 -2.24E+02  1.82E+02  1.37E+02  3.64E+02  8.49E+01  8.04E+01  3.70E+02  8.69E+02  1.32E+03 -1.70E+03 -2.01E+02
         -1.93E+03  4.30E+02  1.10E+03 -1.72E+03  8.47E+00 -2.77E+03  6.83E+02 -1.18E+03  7.25E+02  1.70E+03 -5.25E+02  1.54E+03
         1.48E+03  5.60E+03
 
 OM36
+       -6.61E+01  1.82E+02  1.44E+02  1.13E+02  1.41E+02  3.23E+01  1.72E+02 -1.11E+02 -1.30E+02  2.06E+02  6.36E+02  3.23E+02
          2.08E+02 -4.41E+02 -4.06E+02 -1.48E+01  7.12E+01 -7.19E+02  8.57E+01  3.43E+02 -1.50E+02 -2.10E+02 -2.79E+02 -2.22E+02
        -5.59E+02  1.29E+03  2.45E+03
 
 OM37
+        2.25E+02 -2.46E+02 -8.96E+01 -1.93E+02  9.97E+01  1.91E+02 -2.21E+02  2.81E+01 -8.87E+02 -9.73E+02  1.44E+03 -2.66E+02
          1.05E+03 -5.34E+02 -9.44E+02  1.54E+03 -4.49E+02  3.03E+03 -1.26E+02  1.61E+03 -5.87E+02 -8.22E+02  7.55E+02 -3.44E+02
         1.44E+03 -2.44E+03 -9.70E+02  4.96E+03
 
 OM38
+        1.27E+02  2.32E+02 -1.41E+02  1.55E+02  4.01E+02 -1.39E+02  1.55E+01 -1.19E+02  1.23E+03  2.09E+03 -4.43E+03 -4.53E+02
         -1.75E+03  5.50E+02  1.68E+03 -1.59E+03  1.05E+03 -3.77E+03  5.12E+02 -4.01E+02  5.52E+02  1.01E+03 -2.78E+03 -2.26E+03
        -1.72E+03  1.43E+03  3.48E+02 -3.62E+03  1.08E+04
 
 OM44
+        8.58E+01 -2.85E+01 -2.38E+02  1.24E+02 -1.15E+02 -4.56E+01  1.21E+02 -1.11E+02  2.90E+02  5.91E+02 -5.03E+02 -5.27E+02
         -2.03E-02  4.91E+02  8.25E+01 -3.14E+02  5.62E+02 -1.80E+02 -5.16E+02  3.02E+02  6.45E+02  6.15E+01 -3.78E+02  2.67E+02
         4.92E+02  3.97E+02 -1.17E+02 -2.85E+02  3.43E+02  9.31E+02
 
 OM45
+       -1.29E+02 -1.24E+02  1.27E+02 -1.81E+02  1.70E+02  1.56E+02 -2.47E+02  3.12E+02 -1.01E+02 -9.31E+02 -1.20E+02 -5.52E+02
         -1.91E+02 -8.65E+01 -9.67E+01  6.28E+02 -4.74E+02  7.55E+02 -3.22E+02 -8.79E+02 -1.21E+03 -1.97E+02  1.26E+03  5.07E+02
         7.81E+02  4.97E+02  1.84E+02  3.35E+01 -1.04E+03 -1.16E+02  1.80E+03
 
 OM46
+        4.67E+01  1.45E+02  2.04E+02 -7.44E+01  1.77E+02  2.94E+01 -1.38E+02  4.63E+00 -3.23E+02 -1.94E+02  3.30E+02  8.11E+02
         -2.16E+02 -8.23E+02  2.48E+02  5.33E+01 -3.98E+02  1.38E+02  8.28E+02 -1.42E+03 -1.57E+03  5.38E+02 -4.51E+02 -5.00E+01
        -5.60E+02  1.60E+02  8.25E+02  1.11E+02 -4.55E+02 -7.55E+02  7.26E+02  2.13E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.60E+02  2.41E+02 -2.28E+02  2.39E+02 -2.73E+02 -1.63E+02  2.51E+02 -4.99E+02  1.26E+02  1.10E+03 -1.03E+02  6.28E+02
          1.83E+02  2.57E+02  5.71E+02 -6.34E+02  9.01E+02 -1.30E+02  1.19E+03  2.93E+02  5.66E+02  8.86E+02 -1.79E+03  1.83E+02
        -8.97E+02 -3.20E+01  2.37E+02 -6.63E+02  1.18E+03  7.20E+02 -1.00E+03 -7.65E+02  2.77E+03
 
 OM48
+       -2.06E+02 -5.72E+02  2.28E+02 -1.84E+02  3.30E+02 -9.15E-01 -4.64E+02  4.33E+02 -2.30E+01 -7.17E+02 -4.85E+02 -2.11E+03
          3.39E+02  2.99E+02 -1.08E+03  1.29E+03 -6.20E+02  5.49E+02 -2.68E+03  8.29E+02 -2.57E+02 -2.28E+03  1.62E+03 -5.96E+02
         4.29E+02 -1.01E+03 -4.17E+02  1.01E+03  5.01E+02 -4.32E+02  6.68E+02  2.54E+02 -1.85E+03  3.79E+03
 
 OM55
+       -2.34E+02 -1.63E+02  1.78E+02  7.77E+01  2.68E+02  1.68E+02 -1.85E+02  2.08E+02  2.15E+02  1.04E+03 -5.64E+02  7.71E+01
         -1.45E+03 -6.79E+02  8.52E+02 -1.21E+03  7.84E+02 -5.69E+02 -3.51E+02 -2.21E+03 -1.26E+03  7.03E+02 -1.84E+03  6.61E+01
        -1.86E+02  4.08E+02 -3.23E+02 -6.43E+02  6.70E+02 -1.56E+02  3.19E+02  3.22E+02 -3.73E+02  3.43E+02  1.41E+03
 
 OM56
+       -3.62E+02 -4.74E+02  1.12E+02  1.64E+02  2.68E+02  2.13E+02 -9.72E+01  3.64E+02  6.37E+01  8.24E+02  7.86E+02  5.55E+01
         -1.28E+03 -1.33E+03  3.91E+02 -5.48E+02  6.92E+02  8.04E+02 -1.16E+03 -2.90E+03 -1.59E+03  4.56E+02 -1.10E+03 -3.54E+02
        -2.50E+02 -1.53E+03 -1.56E+03  4.08E+02 -1.12E+03 -6.72E+01  7.08E+01  1.32E+02 -4.45E+02  6.41E+02  1.53E+03  4.81E+03
 
 OM57
+        3.87E+01  1.98E+02  8.80E+01 -2.42E+02 -3.98E+02 -1.36E+02 -1.05E+02 -4.58E+01 -6.94E+02 -2.11E+03  9.19E+02 -1.18E+02
          2.02E+03  4.47E+02 -1.37E+03  2.03E+03 -1.17E+03  1.77E+03 -1.38E+02  2.45E+03  5.86E+02 -2.38E+03  2.85E+03 -5.51E+02
         2.16E+02 -1.53E+03  9.02E+01  1.07E+03 -1.38E+03 -1.60E+02  1.79E+02 -2.91E+02  8.67E+01  3.86E+02 -9.33E+02 -1.10E+03
          3.01E+03
 
 OM58
+       -1.06E+02 -2.95E+02  4.06E+02  2.87E+02  3.68E+02  3.83E+02  5.15E+01  2.69E+01  9.94E+02  3.04E+03 -1.31E+03  6.17E+02
         -4.11E+03 -1.10E+03  2.39E+03 -2.78E+03  1.69E+03 -3.84E+02  1.15E+03 -6.44E+03 -1.82E+03  3.36E+03 -4.39E+03 -4.00E+02
        -1.42E+03  8.38E+02 -8.70E+02 -1.19E+03  1.39E+03 -1.27E+02 -6.07E+01  5.87E+02  2.22E+02 -6.94E+02  2.06E+03  3.16E+03
         -2.88E+03  7.56E+03
 
 OM66
+       -1.42E+02 -2.41E+02 -1.04E+00  2.37E+01  5.13E+01  1.06E+02 -9.95E+01  1.43E+02  3.86E+01  2.58E+02  1.88E+02 -2.38E+02
         -3.87E+02 -4.91E+02  1.66E+02 -1.73E+02  3.64E+02  1.02E+02 -6.84E+02 -4.19E+02 -6.75E+02  1.97E+01 -4.53E+02 -4.00E+02
         5.37E+01 -9.98E+02 -8.65E+02  1.31E+02 -1.35E+02  7.85E+01 -1.10E+02 -4.63E+02 -1.64E+02  4.68E+02  6.80E+02  1.87E+03
         -1.84E+02  8.14E+02  1.31E+03
 
 OM67
+        1.53E+02  3.76E+02  1.83E+02 -1.49E+02 -9.61E+01 -2.21E+02  4.80E+01 -1.86E+02  5.99E+01 -5.15E+02 -7.11E+02  1.35E+02
          3.77E+02  6.52E+02  4.83E+00  7.04E+01 -6.00E+02 -5.86E+02  3.27E+02  6.31E+02  9.32E+02 -5.13E+02  8.14E+02  1.75E+01
         2.76E+02  3.92E+02 -7.78E+00 -3.18E+02  4.38E+02 -1.82E+02 -1.04E+02  2.14E+02 -2.03E+02  4.59E+01 -3.76E+02 -8.81E+02
          5.95E+02 -7.72E+02 -5.87E+02  1.75E+03
 
 OM68
+       -1.98E+02 -7.95E+01 -1.29E+02 -6.61E+00  2.86E+02  3.17E+02 -1.22E+02  3.33E+01 -1.34E+02  6.49E+02  9.06E+02  4.66E+02
         -8.39E+02 -1.63E+03  2.61E+02 -6.89E+02  8.97E+02  4.68E+02 -7.93E+01 -1.55E+03 -2.60E+03  8.84E+02 -2.00E+03 -7.63E+02
        -6.88E+02 -1.26E+03 -3.22E+02  5.01E+02 -3.99E+02 -2.46E+02  3.27E+02  5.51E+02 -1.25E+02  2.72E+02  1.10E+03  2.09E+03
         -5.51E+02  1.56E+03  1.07E+03 -1.09E+03  2.86E+03
 
 OM77
+        4.97E+01  7.47E+01 -1.19E+02  1.14E+02 -5.88E+01  2.25E+01  1.50E+02 -6.40E+01  1.80E+02  6.56E+02 -1.49E+02  5.19E+02
         -4.20E+02 -2.60E+00  6.27E+02 -6.24E+02  4.78E+02 -4.59E+02  7.45E+02 -8.28E+02 -1.23E+02  1.18E+03 -1.10E+03  9.55E+01
        -7.40E+02  5.03E+02  2.01E+02 -9.51E+02  1.10E+03  1.62E+02 -1.89E+02  4.42E+01  9.67E+02 -9.75E+02  8.54E+01 -6.88E+01
         -5.97E+02  7.98E+02 -9.80E+01 -3.36E+02  1.54E+02  9.35E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -2.45E+02 -1.31E+02  1.84E+01 -4.43E+02  1.70E+00 -1.87E+02 -1.66E+02  3.12E+02 -7.73E+02 -2.61E+03  1.30E+03 -1.29E+03
          2.14E+03  3.54E+02 -2.55E+03  2.36E+03 -1.46E+03  9.96E+02 -2.68E+03  3.00E+03  9.54E+02 -3.69E+03  4.30E+03 -5.42E+02
         1.35E+03 -1.24E+03  9.06E+01  1.61E+03 -2.29E+03 -3.40E+02  7.03E+02 -4.59E+01 -2.11E+03  2.42E+03 -6.70E+02 -4.08E+02
          2.06E+03 -3.70E+03  1.42E+01  9.73E+02 -6.70E+02 -1.69E+03  5.48E+03
 
 OM88
+        1.77E+02  4.58E+02 -5.28E+01  1.65E+02 -6.59E+01  1.49E+01  2.52E+02 -4.58E+02  4.60E+02  2.19E+03 -1.24E+02  1.06E+03
         -1.14E+03 -5.82E+02  1.33E+03 -2.89E+03  1.65E+03 -1.41E+03  1.20E+03 -1.71E+03 -1.01E+03  1.97E+03 -4.28E+03  8.28E+02
        -2.94E+02  6.91E+02  4.18E+02 -9.53E+02 -5.74E+00  8.93E+01 -6.23E+02  3.97E+02  7.19E+02 -1.53E+03  7.19E+02  1.85E+02
         -1.46E+03  1.60E+03 -8.27E+01 -1.46E+02  9.00E+02  5.75E+02 -2.22E+03  3.18E+03
 
 SG11
+       -1.14E+03  5.98E+03 -2.51E+03 -8.42E+02 -2.61E+03  1.06E+03  8.29E+03 -2.62E+03  3.87E+03  1.58E+04  2.06E+04  6.31E+03
         -6.06E+03 -4.47E+03  7.52E+03 -1.64E+04  2.14E+04  1.41E+04 -1.76E+04 -1.11E+04  1.67E+04  3.33E+03 -8.91E+03 -1.07E+04
        -1.77E+03 -9.91E+03 -7.68E+03  1.07E+03 -1.83E+04  9.70E+03 -9.42E+03 -1.96E+04  6.71E+03 -1.27E+04  1.23E+01  2.18E+04
         -1.33E+04  1.24E+04  1.07E+04 -3.69E+03  5.63E+03  5.26E+03 -3.90E+03  9.11E+03  1.79E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.20E+03 -2.68E+03  2.53E+03  3.64E+03  1.67E+03  4.64E+02 -1.19E+03  3.10E+03  5.95E+03  7.36E+03 -6.00E+03 -7.53E+03
         -7.33E+03  3.32E+03  2.43E+03 -4.27E+03  9.77E+02 -2.64E+03 -1.61E+04 -7.52E+03  6.36E+03 -2.85E+03  8.11E+03  1.22E+03
         1.20E+03  1.48E+04  3.58E+03 -8.29E+03 -4.20E+03  2.47E+03  5.95E+03 -2.09E+03 -5.84E+03  5.87E+03  4.33E+03  1.60E+03
         -9.31E+03  8.50E+03 -1.24E+03 -2.77E+02  5.33E+02 -2.33E+02 -3.00E+03  1.33E+03  2.53E+05  0.00E+00  6.29E+05
 
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
 RAW OUTPUT FILE (FILE): example6classico3.ext
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
 ITERATIONS (NITER):                        10000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1
 RANDOM SAMPLING METHOD (RANMETHOD):        3UP
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
 SAMPLES FOR GLOBAL SEARCH KERNEL (OSAMPLE_M1):          8
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           8
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):8
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 8.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000

 TOLERANCES FOR ESTIMATION/EVALUATION STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   4
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 TOLERANCES FOR COVARIANCE STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   4
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
 
 OMEGAS ARE METROPOLIS-HASTINGS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -1000 MCMCOBJ=   -6781.29269356199     
 iteration         -950 MCMCOBJ=   -6630.42043148059     
 iteration         -900 MCMCOBJ=   -6548.98071895863     
 iteration         -850 MCMCOBJ=   -6505.62472800903     
 iteration         -800 MCMCOBJ=   -6560.00439458514     
 iteration         -750 MCMCOBJ=   -6567.08996457311     
 iteration         -700 MCMCOBJ=   -6496.75674558936     
 iteration         -650 MCMCOBJ=   -6521.40875165847     
 iteration         -600 MCMCOBJ=   -6481.08826055499     
 iteration         -550 MCMCOBJ=   -6455.08520100603     
 iteration         -500 MCMCOBJ=   -6409.34307111700     
 iteration         -450 MCMCOBJ=   -6481.98898900267     
 iteration         -400 MCMCOBJ=   -6498.44986781814     
 iteration         -350 MCMCOBJ=   -6440.41360507445     
 iteration         -300 MCMCOBJ=   -6440.60284928409     
 iteration         -250 MCMCOBJ=   -6457.65926834196     
 iteration         -200 MCMCOBJ=   -6519.36761685907     
 iteration         -150 MCMCOBJ=   -6460.08498936059     
 iteration         -100 MCMCOBJ=   -6488.49860539882     
 iteration          -50 MCMCOBJ=   -6388.68777089321     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6428.19845072484     
 iteration           50 MCMCOBJ=   -6446.23078570884     
 iteration          100 MCMCOBJ=   -6508.34188722712     
 iteration          150 MCMCOBJ=   -6404.23343325548     
 iteration          200 MCMCOBJ=   -6463.55966002249     
 iteration          250 MCMCOBJ=   -6469.68221311063     
 iteration          300 MCMCOBJ=   -6511.11115155889     
 iteration          350 MCMCOBJ=   -6427.73514779679     
 iteration          400 MCMCOBJ=   -6427.56123505922     
 iteration          450 MCMCOBJ=   -6435.76956592290     
 iteration          500 MCMCOBJ=   -6446.31192496488     
 iteration          550 MCMCOBJ=   -6431.45020416143     
 iteration          600 MCMCOBJ=   -6461.91190605740     
 iteration          650 MCMCOBJ=   -6450.64397849424     
 iteration          700 MCMCOBJ=   -6428.27968397111     
 iteration          750 MCMCOBJ=   -6448.64497595549     
 iteration          800 MCMCOBJ=   -6481.67531072538     
 iteration          850 MCMCOBJ=   -6453.08189055293     
 iteration          900 MCMCOBJ=   -6436.93274673140     
 iteration          950 MCMCOBJ=   -6506.00568869351     
 iteration         1000 MCMCOBJ=   -6453.19328328155     
 iteration         1050 MCMCOBJ=   -6417.95252584436     
 iteration         1100 MCMCOBJ=   -6480.91363560140     
 iteration         1150 MCMCOBJ=   -6438.87562967281     
 iteration         1200 MCMCOBJ=   -6452.53055267406     
 iteration         1250 MCMCOBJ=   -6499.75682830773     
 iteration         1300 MCMCOBJ=   -6452.74732865845     
 iteration         1350 MCMCOBJ=   -6472.02572596669     
 iteration         1400 MCMCOBJ=   -6422.72821801757     
 iteration         1450 MCMCOBJ=   -6483.31399573674     
 iteration         1500 MCMCOBJ=   -6437.45958597417     
 iteration         1550 MCMCOBJ=   -6404.96702869943     
 iteration         1600 MCMCOBJ=   -6423.44979300912     
 iteration         1650 MCMCOBJ=   -6480.41845425266     
 iteration         1700 MCMCOBJ=   -6430.14264739315     
 iteration         1750 MCMCOBJ=   -6410.66427958994     
 iteration         1800 MCMCOBJ=   -6429.39001525476     
 iteration         1850 MCMCOBJ=   -6423.79091913375     
 iteration         1900 MCMCOBJ=   -6471.40232032082     
 iteration         1950 MCMCOBJ=   -6484.08667471163     
 iteration         2000 MCMCOBJ=   -6441.75338612051     
 iteration         2050 MCMCOBJ=   -6430.79348229376     
 iteration         2100 MCMCOBJ=   -6433.70865990105     
 iteration         2150 MCMCOBJ=   -6434.83074419032     
 iteration         2200 MCMCOBJ=   -6481.16811505551     
 iteration         2250 MCMCOBJ=   -6427.28962707133     
 iteration         2300 MCMCOBJ=   -6418.66288761019     
 iteration         2350 MCMCOBJ=   -6413.25072857362     
 iteration         2400 MCMCOBJ=   -6457.81041573671     
 iteration         2450 MCMCOBJ=   -6437.19491511202     
 iteration         2500 MCMCOBJ=   -6431.68961636690     
 iteration         2550 MCMCOBJ=   -6435.55942750837     
 iteration         2600 MCMCOBJ=   -6468.36819093600     
 iteration         2650 MCMCOBJ=   -6431.71161846484     
 iteration         2700 MCMCOBJ=   -6442.60084373722     
 iteration         2750 MCMCOBJ=   -6439.01168420502     
 iteration         2800 MCMCOBJ=   -6444.17963721619     
 iteration         2850 MCMCOBJ=   -6367.05462831182     
 iteration         2900 MCMCOBJ=   -6470.73194349886     
 iteration         2950 MCMCOBJ=   -6417.73441931616     
 iteration         3000 MCMCOBJ=   -6426.23607948749     
 iteration         3050 MCMCOBJ=   -6450.59714071287     
 iteration         3100 MCMCOBJ=   -6344.25764431593     
 iteration         3150 MCMCOBJ=   -6483.30467518654     
 iteration         3200 MCMCOBJ=   -6422.19936256221     
 iteration         3250 MCMCOBJ=   -6486.66889446783     
 iteration         3300 MCMCOBJ=   -6377.00790980888     
 iteration         3350 MCMCOBJ=   -6432.63560333521     
 iteration         3400 MCMCOBJ=   -6412.57777877726     
 iteration         3450 MCMCOBJ=   -6459.71473373732     
 iteration         3500 MCMCOBJ=   -6433.16219876811     
 iteration         3550 MCMCOBJ=   -6404.40492879785     
 iteration         3600 MCMCOBJ=   -6432.26210270314     
 iteration         3650 MCMCOBJ=   -6465.70401176370     
 iteration         3700 MCMCOBJ=   -6453.36123906087     
 iteration         3750 MCMCOBJ=   -6437.54137621770     
 iteration         3800 MCMCOBJ=   -6488.53397649916     
 iteration         3850 MCMCOBJ=   -6372.83152304241     
 iteration         3900 MCMCOBJ=   -6424.71142019100     
 iteration         3950 MCMCOBJ=   -6427.40228500540     
 iteration         4000 MCMCOBJ=   -6475.66543783022     
 iteration         4050 MCMCOBJ=   -6432.31578951852     
 iteration         4100 MCMCOBJ=   -6474.35685464634     
 iteration         4150 MCMCOBJ=   -6435.68832229041     
 iteration         4200 MCMCOBJ=   -6481.00193446652     
 iteration         4250 MCMCOBJ=   -6462.10101140906     
 iteration         4300 MCMCOBJ=   -6484.49251280928     
 iteration         4350 MCMCOBJ=   -6484.95206673559     
 iteration         4400 MCMCOBJ=   -6389.00260224663     
 iteration         4450 MCMCOBJ=   -6424.78173775309     
 iteration         4500 MCMCOBJ=   -6431.09327634093     
 iteration         4550 MCMCOBJ=   -6447.09812471492     
 iteration         4600 MCMCOBJ=   -6486.43386993282     
 iteration         4650 MCMCOBJ=   -6428.66015308686     
 iteration         4700 MCMCOBJ=   -6444.21528064971     
 iteration         4750 MCMCOBJ=   -6440.07440411982     
 iteration         4800 MCMCOBJ=   -6411.05684743835     
 iteration         4850 MCMCOBJ=   -6429.28747299439     
 iteration         4900 MCMCOBJ=   -6426.45389060322     
 iteration         4950 MCMCOBJ=   -6428.74397101366     
 iteration         5000 MCMCOBJ=   -6446.18698985920     
 iteration         5050 MCMCOBJ=   -6411.94113631110     
 iteration         5100 MCMCOBJ=   -6467.66310186822     
 iteration         5150 MCMCOBJ=   -6423.18518595008     
 iteration         5200 MCMCOBJ=   -6477.08295783858     
 iteration         5250 MCMCOBJ=   -6424.55778540733     
 iteration         5300 MCMCOBJ=   -6403.69059702966     
 iteration         5350 MCMCOBJ=   -6415.51355398685     
 iteration         5400 MCMCOBJ=   -6451.16090716899     
 iteration         5450 MCMCOBJ=   -6430.71708928657     
 iteration         5500 MCMCOBJ=   -6403.26283718408     
 iteration         5550 MCMCOBJ=   -6436.30486875662     
 iteration         5600 MCMCOBJ=   -6452.08841278889     
 iteration         5650 MCMCOBJ=   -6473.09646738741     
 iteration         5700 MCMCOBJ=   -6405.82828361562     
 iteration         5750 MCMCOBJ=   -6441.48256449616     
 iteration         5800 MCMCOBJ=   -6393.05529593663     
 iteration         5850 MCMCOBJ=   -6452.80678660719     
 iteration         5900 MCMCOBJ=   -6452.56009564373     
 iteration         5950 MCMCOBJ=   -6501.53291377272     
 iteration         6000 MCMCOBJ=   -6435.57256671532     
 iteration         6050 MCMCOBJ=   -6475.25865495864     
 iteration         6100 MCMCOBJ=   -6448.06555442731     
 iteration         6150 MCMCOBJ=   -6479.04770782533     
 iteration         6200 MCMCOBJ=   -6413.24246400129     
 iteration         6250 MCMCOBJ=   -6431.38496206006     
 iteration         6300 MCMCOBJ=   -6419.29376900361     
 iteration         6350 MCMCOBJ=   -6465.46171010098     
 iteration         6400 MCMCOBJ=   -6392.45045617953     
 iteration         6450 MCMCOBJ=   -6502.98189849712     
 iteration         6500 MCMCOBJ=   -6471.18233683559     
 iteration         6550 MCMCOBJ=   -6430.68429135094     
 iteration         6600 MCMCOBJ=   -6449.82881374314     
 iteration         6650 MCMCOBJ=   -6495.34452201054     
 iteration         6700 MCMCOBJ=   -6446.36631757759     
 iteration         6750 MCMCOBJ=   -6471.78904392770     
 iteration         6800 MCMCOBJ=   -6429.67584989928     
 iteration         6850 MCMCOBJ=   -6487.66933153618     
 iteration         6900 MCMCOBJ=   -6426.17558186908     
 iteration         6950 MCMCOBJ=   -6459.59739190847     
 iteration         7000 MCMCOBJ=   -6405.36164171763     
 iteration         7050 MCMCOBJ=   -6403.57728577824     
 iteration         7100 MCMCOBJ=   -6475.85663513370     
 iteration         7150 MCMCOBJ=   -6467.12951808103     
 iteration         7200 MCMCOBJ=   -6424.29261628982     
 iteration         7250 MCMCOBJ=   -6481.12145232430     
 iteration         7300 MCMCOBJ=   -6397.15267142913     
 iteration         7350 MCMCOBJ=   -6423.90087441641     
 iteration         7400 MCMCOBJ=   -6390.81066663099     
 iteration         7450 MCMCOBJ=   -6461.38245633838     
 iteration         7500 MCMCOBJ=   -6415.16926046899     
 iteration         7550 MCMCOBJ=   -6408.62876012852     
 iteration         7600 MCMCOBJ=   -6405.32622877887     
 iteration         7650 MCMCOBJ=   -6464.78532064327     
 iteration         7700 MCMCOBJ=   -6404.32842193358     
 iteration         7750 MCMCOBJ=   -6449.72071517298     
 iteration         7800 MCMCOBJ=   -6467.89668216607     
 iteration         7850 MCMCOBJ=   -6438.32167335172     
 iteration         7900 MCMCOBJ=   -6433.32174539235     
 iteration         7950 MCMCOBJ=   -6468.62792791790     
 iteration         8000 MCMCOBJ=   -6385.01577526479     
 iteration         8050 MCMCOBJ=   -6485.93331696572     
 iteration         8100 MCMCOBJ=   -6498.10086729362     
 iteration         8150 MCMCOBJ=   -6524.09800531541     
 iteration         8200 MCMCOBJ=   -6388.02002283259     
 iteration         8250 MCMCOBJ=   -6412.40678805715     
 iteration         8300 MCMCOBJ=   -6395.34218834373     
 iteration         8350 MCMCOBJ=   -6467.34232374466     
 iteration         8400 MCMCOBJ=   -6445.04421039997     
 iteration         8450 MCMCOBJ=   -6447.55901117944     
 iteration         8500 MCMCOBJ=   -6424.94821860614     
 iteration         8550 MCMCOBJ=   -6419.95306717896     
 iteration         8600 MCMCOBJ=   -6466.22324118835     
 iteration         8650 MCMCOBJ=   -6448.62119034056     
 iteration         8700 MCMCOBJ=   -6419.90313714523     
 iteration         8750 MCMCOBJ=   -6451.24687998722     
 iteration         8800 MCMCOBJ=   -6454.59335022810     
 iteration         8850 MCMCOBJ=   -6415.87019127360     
 iteration         8900 MCMCOBJ=   -6411.58306046596     
 iteration         8950 MCMCOBJ=   -6458.83030995936     
 iteration         9000 MCMCOBJ=   -6438.74718355459     
 iteration         9050 MCMCOBJ=   -6443.38544505535     
 iteration         9100 MCMCOBJ=   -6474.38324268982     
 iteration         9150 MCMCOBJ=   -6432.28071016767     
 iteration         9200 MCMCOBJ=   -6421.37113739339     
 iteration         9250 MCMCOBJ=   -6481.40945197816     
 iteration         9300 MCMCOBJ=   -6391.04100098774     
 iteration         9350 MCMCOBJ=   -6463.70549252497     
 iteration         9400 MCMCOBJ=   -6408.28198949172     
 iteration         9450 MCMCOBJ=   -6470.94936640505     
 iteration         9500 MCMCOBJ=   -6452.50363210228     
 iteration         9550 MCMCOBJ=   -6458.97856186713     
 iteration         9600 MCMCOBJ=   -6446.57293181233     
 iteration         9650 MCMCOBJ=   -6436.41034501195     
 iteration         9700 MCMCOBJ=   -6483.92203009551     
 iteration         9750 MCMCOBJ=   -6457.58422069519     
 iteration         9800 MCMCOBJ=   -6443.05085860170     
 iteration         9850 MCMCOBJ=   -6436.76453606140     
 iteration         9900 MCMCOBJ=   -6445.26552552322     
 iteration         9950 MCMCOBJ=   -6367.33192279799     
 iteration        10000 MCMCOBJ=   -6431.50930448838     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6441.22596821104     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3559.43472808119     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6441.22596821104     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5706.07514164730     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -16.9020929079541     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6441.22596821104     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6458.12806111900     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  3008.21
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6441.226       **************************************************
 #OBJS:********************************************       33.871 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.21E+00  5.60E-01 -1.78E-01  2.27E+00  2.37E-01  3.71E+00 -7.05E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.59E-01
 
 ETA2
+       -2.68E-02  1.71E-01
 
 ETA3
+        2.73E-02 -9.05E-03  1.08E-01
 
 ETA4
+        2.35E-02  3.24E-02 -1.10E-02  2.62E-01
 
 ETA5
+        2.12E-02  1.82E-02 -1.42E-03 -2.40E-02  2.03E-01
 
 ETA6
+       -1.21E-02  1.08E-02  1.52E-02  1.18E-02 -5.72E-02  2.46E-01
 
 ETA7
+        1.28E-02 -3.44E-02  1.95E-02 -5.60E-02  1.93E-02  8.43E-03  2.51E-01
 
 ETA8
+        6.82E-02  5.83E-02  2.58E-02  3.43E-02 -6.32E-03 -4.35E-02  5.19E-02  2.17E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.38E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.07E-01
 
 ETA2
+       -1.26E-01  4.10E-01
 
 ETA3
+        1.66E-01 -6.88E-02  3.26E-01
 
 ETA4
+        9.05E-02  1.55E-01 -6.77E-02  5.09E-01
 
 ETA5
+        9.27E-02  9.86E-02 -1.04E-02 -1.05E-01  4.48E-01
 
 ETA6
+       -4.97E-02  5.46E-02  9.65E-02  4.64E-02 -2.60E-01  4.92E-01
 
 ETA7
+        5.13E-02 -1.63E-01  1.21E-01 -2.19E-01  8.54E-02  3.36E-02  4.99E-01
 
 ETA8
+        2.89E-01  3.06E-01  1.68E-01  1.44E-01 -2.98E-02 -1.89E-01  2.21E-01  4.63E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.68E-02
 
 EPS2
+        0.00E+00  1.50E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         7.19E-02  6.94E-02  5.34E-02  7.43E-02  6.45E-02  7.60E-02  7.13E-02  6.80E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        4.80E-02
 
 ETA2
+        2.83E-02  4.41E-02
 
 ETA3
+        2.23E-02  1.92E-02  2.93E-02
 
 ETA4
+        3.13E-02  2.86E-02  2.37E-02  5.62E-02
 
 ETA5
+        2.79E-02  2.43E-02  2.00E-02  2.94E-02  4.28E-02
 
 ETA6
+        3.28E-02  2.80E-02  2.29E-02  3.37E-02  3.06E-02  6.18E-02
 
 ETA7
+        3.04E-02  2.94E-02  2.16E-02  3.31E-02  2.88E-02  3.27E-02  5.04E-02
 
 ETA8
+        2.94E-02  2.53E-02  2.07E-02  3.01E-02  2.64E-02  3.08E-02  3.05E-02  4.50E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.47E-04
 
 EPS2
+        0.00E+00  1.19E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        4.64E-02
 
 ETA2
+        1.27E-01  5.23E-02
 
 ETA3
+        1.29E-01  1.39E-01  4.37E-02
 
 ETA4
+        1.16E-01  1.29E-01  1.38E-01  5.41E-02
 
 ETA5
+        1.18E-01  1.26E-01  1.32E-01  1.23E-01  4.66E-02
 
 ETA6
+        1.27E-01  1.33E-01  1.36E-01  1.30E-01  1.28E-01  6.11E-02
 
 ETA7
+        1.16E-01  1.30E-01  1.28E-01  1.19E-01  1.23E-01  1.28E-01  4.92E-02
 
 ETA8
+        1.11E-01  1.17E-01  1.26E-01  1.19E-01  1.23E-01  1.25E-01  1.18E-01  4.73E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.33E-03
 
 EPS2
+        0.00E+00  3.98E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        5.17E-03
 
 TH 2
+       -6.83E-04  4.81E-03
 
 TH 3
+        3.19E-04  8.92E-06  2.86E-03
 
 TH 4
+        3.63E-04  6.05E-04  1.31E-04  5.52E-03
 
 TH 5
+        4.08E-04  2.46E-04  4.51E-05 -5.04E-04  4.16E-03
 
 TH 6
+       -1.77E-04 -1.78E-05  1.37E-04  1.85E-04 -8.02E-04  5.77E-03
 
 TH 7
+        2.52E-04 -8.99E-04  3.78E-04 -1.03E-03  4.47E-04  2.36E-04  5.09E-03
 
 TH 8
+        1.28E-03  8.58E-04  6.80E-04  7.06E-04 -2.49E-05 -1.01E-03  1.19E-03  4.62E-03
 
 OM11
+       -1.84E-05 -4.56E-05  1.39E-05  2.73E-05  7.21E-06  1.23E-04 -2.15E-05 -3.87E-05  2.30E-03
 
 OM12
+       -2.08E-05  1.52E-04 -1.66E-05  6.31E-05 -1.20E-07 -1.95E-05 -6.82E-05 -1.55E-05 -2.03E-04  8.02E-04
 
 OM13
+        5.04E-06  1.77E-05  6.48E-06  3.52E-06  1.40E-05  1.49E-06  7.68E-06 -1.34E-05  9.45E-05 -1.61E-05  4.95E-04
 
 OM14
+       -1.27E-05  2.08E-06  1.82E-05  5.95E-05  2.19E-05  3.47E-06  3.80E-05  2.10E-05  1.39E-04  4.50E-05 -8.69E-06  9.81E-04
 
 OM15
+        3.52E-06 -2.26E-05 -7.31E-06 -2.86E-06  3.37E-05 -3.70E-07  3.04E-05  3.17E-05  1.16E-04  3.59E-05  8.58E-06 -4.94E-05
          7.81E-04
 
 OM16
+        2.42E-05 -1.41E-05  1.62E-05  2.51E-05  4.99E-05  1.65E-05 -3.18E-05  7.58E-06 -9.88E-06 -4.13E-06  2.43E-06  4.81E-05
         -1.11E-04  1.07E-03
 
 OM17
+       -2.03E-05  3.16E-05 -2.90E-05 -4.11E-07 -2.66E-05  3.57E-05 -4.14E-05 -1.06E-05  3.40E-05 -1.21E-04  4.19E-05 -1.15E-04
          6.42E-05  5.04E-05  9.23E-04
 
 OM18
+       -4.21E-05  9.80E-06  1.14E-05  2.48E-05  4.62E-05  1.01E-05 -2.07E-05  4.98E-06  3.78E-04  1.09E-04  8.06E-05  1.07E-04
         -1.06E-05 -1.22E-04  1.64E-04  8.64E-04
 
 OM22
+        3.62E-05 -6.24E-04  7.53E-05 -2.35E-05  1.28E-05  6.50E-05  8.27E-05  1.20E-04  4.78E-05 -3.21E-04 -1.72E-05 -1.41E-05
         -2.83E-05  3.06E-05  3.44E-05 -4.56E-05  1.95E-03
 
 OM23
+       -6.40E-06  1.24E-04  6.00E-05  1.39E-05  1.00E-05 -7.53E-06 -3.40E-06  1.17E-05 -3.55E-05  4.42E-05 -3.12E-05  1.17E-07
         -1.78E-06 -6.92E-06  1.81E-06 -7.51E-06 -3.73E-05  3.70E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        3.47E-05 -2.36E-04  6.67E-05  3.87E-05  3.91E-05 -2.87E-05  8.41E-05  7.53E-05 -3.01E-05 -1.59E-05  7.80E-06 -6.59E-05
          1.79E-06 -1.50E-05 -5.25E-06  4.56E-06  2.11E-04 -3.44E-06  8.17E-04
 
 OM25
+        1.80E-05  9.98E-06  1.16E-05  9.34E-06  8.55E-06  1.12E-05  3.22E-05  1.73E-05 -1.45E-05  2.53E-05 -7.16E-06  6.13E-06
         -6.81E-05  1.26E-05 -1.89E-05 -1.14E-05  8.01E-05  9.50E-06 -4.01E-05  5.91E-04
 
 OM26
+       -6.56E-06  7.06E-05  2.22E-05  1.92E-05 -2.42E-05 -1.45E-05 -1.90E-05 -4.24E-06  1.99E-06  1.05E-05 -1.04E-05 -1.44E-05
          3.10E-06 -9.09E-05 -6.34E-06  1.10E-05  1.52E-05  2.87E-05  1.87E-05 -8.49E-05  7.86E-04
 
 OM27
+       -2.10E-05  3.78E-04 -3.42E-05 -2.01E-05 -8.46E-07 -2.99E-05 -4.86E-05 -7.06E-05 -6.05E-06  8.76E-05  6.47E-06  1.30E-05
         -6.19E-06 -6.08E-08 -6.34E-05 -1.45E-05 -4.45E-04  5.24E-05 -2.17E-04  4.57E-05  3.42E-05  8.64E-04
 
 OM28
+       -2.20E-05 -2.46E-05  2.87E-05 -7.00E-06 -2.51E-06  2.49E-05  3.06E-05  2.67E-05 -4.43E-05  8.28E-05 -5.87E-06  1.84E-05
          3.74E-06  2.76E-05 -2.06E-05 -3.54E-05  3.36E-04  5.45E-05  8.70E-05 -1.40E-07 -9.97E-05  9.47E-05  6.38E-04
 
 OM33
+       -4.57E-06  1.30E-05 -2.04E-04 -1.20E-04 -1.04E-05 -2.89E-05  7.01E-06 -4.26E-06  2.16E-05 -3.86E-06  8.42E-05  5.35E-07
          7.12E-06  4.02E-07  1.00E-05  2.76E-05  5.80E-06 -7.45E-06 -2.64E-06  5.67E-07 -1.35E-05  1.43E-05  8.68E-06  8.59E-04
 
 OM34
+       -2.39E-05  1.99E-05  1.47E-04  8.73E-05  2.63E-06 -3.42E-05  6.76E-06  2.97E-05  7.61E-06  5.95E-07  5.78E-05  5.22E-05
         -1.27E-05  7.07E-06 -1.66E-05  1.18E-05  2.23E-05  3.21E-05  1.31E-05 -5.12E-06  9.30E-06  2.43E-06  2.05E-05 -3.33E-05
         5.63E-04
 
 OM35
+        4.30E-06 -1.07E-05 -4.06E-05 -1.60E-05 -1.02E-05  8.78E-07 -4.64E-06  9.67E-06 -7.03E-06  1.24E-06  2.51E-05 -2.50E-06
          2.26E-05 -5.19E-06  3.52E-06  6.71E-06 -7.22E-06  2.86E-05  6.35E-06 -2.28E-06 -6.87E-06  7.05E-06  4.72E-06  1.32E-05
        -3.31E-05  4.00E-04
 
 OM36
+        1.97E-05 -2.24E-05  3.87E-05 -5.25E-06  2.39E-05 -1.61E-05  4.72E-06  1.39E-05 -1.20E-05 -3.98E-06  8.89E-06  5.18E-06
         -1.01E-05  4.21E-05 -1.01E-05 -1.67E-05 -6.74E-06  1.18E-05  4.55E-06 -3.45E-06 -1.05E-05 -1.51E-07  5.58E-06  1.33E-05
         2.34E-05 -6.02E-05  5.25E-04
 
 OM37
+        2.90E-05 -3.85E-05 -6.08E-05 -3.60E-05 -1.16E-05  1.49E-05 -1.83E-06 -1.47E-05  2.17E-06 -1.08E-05 -1.01E-06 -1.95E-05
          4.95E-06 -1.86E-07  5.03E-05  2.22E-05  4.58E-06 -5.77E-05 -5.15E-06  7.87E-06 -1.02E-05 -9.26E-06 -3.56E-06  4.64E-05
        -8.81E-05  2.71E-05  1.67E-05  4.65E-04
 
 OM38
+       -7.45E-06 -1.79E-05  2.87E-07 -5.79E-06  8.23E-06  4.40E-06  2.67E-05  9.60E-06  2.89E-05  2.81E-08  1.07E-04  1.35E-05
          4.34E-06  4.31E-06  2.37E-05  6.77E-05  1.50E-05  7.40E-05  6.91E-06 -4.39E-06 -1.53E-06  4.42E-06  2.74E-05  1.39E-04
         5.78E-05 -2.20E-05 -6.94E-05  7.16E-05  4.27E-04
 
 OM44
+       -4.79E-05  9.72E-05  3.70E-04  3.72E-04 -1.48E-06 -2.30E-05 -2.20E-06  3.45E-05  1.65E-05  3.76E-05  1.86E-05  2.05E-04
         -3.70E-05  3.14E-05 -4.36E-06  3.40E-05  8.95E-05  3.78E-06  2.36E-04 -5.93E-06  3.54E-05 -4.29E-05  6.20E-05 -1.04E-04
         4.30E-05 -1.16E-05 -1.82E-05 -2.98E-06 -3.72E-06  3.16E-03
 
 OM45
+        5.86E-07  5.95E-05  6.80E-06  3.25E-06  1.43E-05  3.66E-05  5.11E-05  6.50E-06  4.27E-06  1.60E-05  4.28E-06  4.68E-05
          5.92E-05 -1.23E-05  6.73E-06  1.75E-05  1.64E-05 -1.11E-06  2.10E-05  3.82E-05  4.21E-06  1.28E-05  5.35E-06  5.39E-06
        -2.21E-06 -3.25E-06 -9.61E-06 -6.77E-06  8.01E-06 -1.50E-04  8.63E-04
 
 OM46
+        2.07E-05 -5.80E-06  9.29E-06 -2.29E-05  4.10E-05  6.89E-05 -1.54E-06  1.65E-05 -7.48E-06 -1.54E-05  2.02E-05  1.53E-05
         -1.67E-05  8.53E-05  1.13E-05 -1.51E-05 -4.18E-06  4.98E-06  3.32E-05 -4.08E-06  4.23E-05  1.98E-05 -1.67E-06  2.33E-06
         1.52E-05  1.53E-06 -1.28E-06 -1.79E-05  2.34E-06  1.30E-04 -1.10E-04  1.14E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.93E-05 -3.69E-06 -8.62E-05 -3.03E-05  1.58E-05  2.37E-05 -7.88E-07  5.35E-06 -1.56E-05 -1.27E-05  2.57E-06 -5.71E-06
          2.43E-05  2.16E-05  6.31E-05  1.32E-05 -3.31E-05  8.71E-06 -1.28E-04  1.11E-05  1.45E-05  1.09E-04 -4.06E-06  2.01E-05
         4.55E-05 -9.11E-06 -7.18E-07 -7.13E-06  1.26E-05 -4.18E-04  6.97E-05  4.18E-05  1.09E-03
 
 OM48
+        2.26E-05  3.62E-05  2.67E-05  2.70E-05  1.39E-05  3.59E-05  3.75E-05  4.23E-05 -9.27E-06  6.14E-05  8.39E-06  1.99E-04
         -1.38E-06  1.78E-05  2.00E-06  9.17E-05  4.07E-05  2.01E-05  1.61E-04 -2.48E-05  2.50E-05 -1.12E-05  1.03E-04  1.38E-05
         1.00E-04 -4.67E-06 -7.18E-06 -1.50E-05  2.11E-05  2.89E-04 -5.20E-05 -1.10E-04  1.55E-04  9.05E-04
 
 OM55
+        6.19E-05 -6.32E-05  6.00E-06  5.25E-05 -4.98E-05 -6.17E-05 -1.94E-06  2.34E-05  3.43E-05  4.38E-06  1.60E-06 -1.06E-05
          1.38E-04 -4.02E-06  1.02E-05  1.43E-05  7.31E-06 -1.61E-05  1.12E-05  1.25E-04 -2.99E-05 -1.18E-05 -1.64E-05  1.18E-05
         1.02E-05  1.16E-06  3.75E-06 -6.36E-07 -4.88E-06  2.43E-05 -1.47E-04  2.03E-05 -3.17E-05  1.86E-05  1.83E-03
 
 OM56
+       -1.27E-05 -3.61E-05  1.87E-06  5.30E-06 -2.04E-05 -1.29E-05 -1.99E-05 -1.98E-05 -3.92E-06 -9.05E-06 -1.25E-05  1.50E-06
         -1.02E-05  6.81E-05  1.78E-06 -1.48E-05  1.63E-05 -9.39E-07  1.12E-05  5.10E-06  5.85E-05 -2.81E-05 -1.23E-05 -4.66E-06
         1.11E-06  1.33E-05 -1.37E-05 -8.84E-06 -1.51E-06  2.07E-05  6.00E-05 -7.41E-05 -4.11E-06  1.74E-05 -2.06E-04  9.37E-04
 
 OM57
+        1.75E-06 -5.28E-05 -9.78E-06  4.54E-05 -1.78E-05 -1.92E-05 -5.96E-05 -4.94E-07  1.32E-05 -1.04E-05  7.44E-06 -1.08E-05
          2.16E-05  1.86E-05  4.04E-05  4.60E-06  3.67E-06 -2.55E-06  1.00E-05 -9.19E-05  6.20E-06  8.34E-06  3.68E-06 -1.63E-05
        -1.14E-07  2.94E-05  8.94E-06  7.68E-06 -1.31E-05  4.10E-05 -1.34E-04 -4.96E-06 -7.32E-05  6.97E-06  1.52E-04  3.30E-05
          8.30E-04
 
 OM58
+       -1.25E-05  1.03E-05 -1.51E-05 -3.33E-07  3.45E-06 -5.32E-06 -1.07E-05 -9.86E-06  5.21E-05  1.40E-05 -4.27E-07  7.28E-06
          1.31E-04 -3.09E-05  1.75E-05  4.14E-05  9.61E-06  4.34E-06 -8.59E-06  1.18E-04 -3.38E-05  2.39E-05  2.48E-05 -1.74E-06
        -9.02E-06  6.15E-05 -8.52E-06  8.23E-06 -9.72E-06  6.54E-06  8.49E-05 -1.89E-05 -6.47E-06 -4.79E-05 -6.61E-05 -1.14E-04
          1.21E-04  6.96E-04
 
 OM66
+        6.51E-06 -1.47E-05 -9.04E-06  4.25E-05  1.25E-04 -2.22E-06  2.97E-05  3.11E-05 -1.08E-05 -5.73E-06 -5.72E-06 -1.51E-05
          1.65E-05  2.31E-06 -1.11E-05  2.08E-05 -2.45E-05 -1.18E-05 -1.42E-05 -7.17E-07  6.15E-05 -2.12E-05 -2.42E-05 -1.86E-05
         5.47E-05  1.79E-05  1.07E-04  3.69E-05  1.39E-05  7.33E-05 -2.82E-05  9.87E-05 -1.25E-05 -2.61E-05 -1.81E-05 -4.74E-04
         -2.48E-05  4.66E-05  3.81E-03
 
 OM67
+       -2.27E-05 -6.03E-05 -3.70E-06 -3.41E-06 -4.71E-06 -8.82E-05  5.61E-05  2.58E-05 -8.72E-06  9.42E-06 -1.22E-06  1.47E-05
         -3.51E-06  1.67E-05 -2.26E-05 -9.71E-06 -1.06E-05 -4.30E-06 -8.13E-06  2.57E-05 -1.32E-04 -5.16E-06  2.17E-06  2.88E-05
         1.42E-09 -1.19E-05  4.38E-05  2.45E-05  5.89E-06 -3.40E-06  3.16E-06 -1.46E-04 -3.21E-06  1.31E-05  2.99E-05  6.07E-05
         -1.02E-04 -4.56E-05  1.03E-04  1.07E-03
 
 OM68
+        1.58E-05 -1.81E-05  1.08E-05  3.95E-05 -1.55E-06  6.71E-06 -3.98E-06  3.83E-05 -3.83E-05 -3.72E-06 -1.02E-06  1.98E-05
         -4.48E-05  1.92E-04 -2.23E-06 -6.34E-05  1.56E-06  1.33E-05  3.29E-05 -2.63E-06  1.53E-04  1.12E-05  1.37E-06  3.00E-06
         9.63E-07 -3.39E-06  5.94E-05  1.10E-05  1.09E-05  2.54E-05 -2.25E-05  1.06E-04  9.99E-06  1.21E-05 -7.10E-06 -4.51E-06
         -4.08E-05 -1.23E-04 -4.27E-04  1.67E-04  9.48E-04
 
 OM77
+        1.05E-05 -8.32E-05 -1.18E-05 -5.42E-05 -5.74E-06  1.91E-05 -2.27E-05 -7.72E-05 -8.16E-06 -1.20E-05 -7.01E-06 -1.12E-05
          2.73E-07  1.11E-05  2.64E-05  8.18E-06  7.04E-05 -1.17E-05  3.62E-05 -3.65E-05 -1.30E-05 -3.47E-04 -5.73E-05  1.77E-05
        -2.19E-06  3.47E-06  1.50E-05  8.72E-05  9.01E-06  1.49E-04 -3.91E-05 -5.17E-05 -4.29E-04 -6.75E-05  1.74E-05  3.67E-05
          1.51E-04  3.25E-05  6.31E-05  1.08E-04  4.37E-06  2.54E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.20E-05 -8.73E-05 -2.30E-05 -2.01E-05  5.63E-05 -1.33E-07 -1.25E-05  3.52E-06  2.87E-05 -3.10E-05  9.69E-06 -2.37E-05
          9.73E-06  1.24E-05  1.83E-04  5.67E-05 -1.55E-05 -1.13E-05 -1.79E-05 -4.58E-07  2.36E-05  6.30E-05 -5.19E-05  1.97E-05
        -1.83E-05 -1.56E-06  2.78E-07  9.18E-05  5.47E-05 -3.37E-05  1.70E-05  2.24E-05  4.87E-05 -8.96E-05 -2.50E-05 -7.01E-06
         -3.80E-06  4.99E-05  1.32E-05 -1.30E-04 -1.36E-05  4.11E-04  9.30E-04
 
 OM88
+       -3.85E-05  2.71E-05  1.76E-05 -1.37E-05  7.98E-05  9.96E-05  4.93E-05  7.08E-06  1.39E-04  5.51E-05  4.72E-05  4.84E-05
          5.83E-06 -5.84E-05  1.20E-04  4.20E-04  9.01E-05  6.43E-05  6.65E-05 -3.61E-05 -4.75E-06  4.84E-05  3.32E-04  7.90E-06
         6.79E-06  5.65E-06 -3.77E-05  5.13E-05  1.81E-04  6.08E-05 -2.54E-06 -1.68E-05  5.64E-05  2.78E-04  2.06E-05 -2.36E-05
          4.50E-06 -4.62E-05  7.43E-05 -1.02E-04 -2.92E-04  1.23E-04  4.45E-04  2.02E-03
 
 SG11
+       -2.89E-07  3.03E-07  2.13E-07 -8.73E-08  6.52E-08  6.46E-07 -7.46E-07 -6.05E-07  2.61E-07  4.77E-07 -6.65E-08 -1.01E-07
         -2.70E-07  2.80E-07 -2.59E-07 -9.89E-08 -1.43E-06  8.86E-08 -3.63E-07 -8.63E-08  1.21E-08 -1.76E-08 -4.05E-07 -7.32E-07
        -1.71E-08 -1.73E-07 -2.18E-08 -4.21E-07 -1.44E-07 -5.02E-07  1.61E-07  7.99E-08  3.84E-07  1.42E-08  8.99E-08  1.86E-07
         -7.16E-08 -1.39E-07 -1.95E-07 -5.47E-07  4.45E-07 -1.62E-07 -1.10E-07 -1.42E-07  4.19E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        2.22E-06  2.43E-06 -4.94E-07  1.29E-06  2.93E-07 -2.54E-07 -5.52E-07 -1.13E-06 -3.03E-07  3.57E-07 -2.40E-07 -7.22E-07
         -2.24E-08 -9.37E-07  6.04E-07  1.53E-07 -1.08E-06  7.01E-07 -4.84E-07 -1.58E-07  2.95E-07  1.15E-06  7.86E-08 -1.59E-08
         2.32E-07 -3.55E-07 -2.72E-07 -4.47E-07 -1.25E-07  4.66E-07  3.66E-07 -2.38E-07  1.91E-07  1.25E-07 -8.94E-07  4.69E-07
         -7.19E-07  3.18E-07 -3.28E-06 -5.42E-07 -1.15E-07 -2.11E-06 -8.31E-07  4.68E-07 -2.91E-08  0.00E+00  1.42E-06
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        7.19E-02
 
 TH 2
+       -1.37E-01  6.94E-02
 
 TH 3
+        8.30E-02  2.41E-03  5.34E-02
 
 TH 4
+        6.81E-02  1.17E-01  3.29E-02  7.43E-02
 
 TH 5
+        8.80E-02  5.50E-02  1.31E-02 -1.05E-01  6.45E-02
 
 TH 6
+       -3.25E-02 -3.38E-03  3.38E-02  3.28E-02 -1.64E-01  7.60E-02
 
 TH 7
+        4.91E-02 -1.82E-01  9.91E-02 -1.95E-01  9.71E-02  4.35E-02  7.13E-02
 
 TH 8
+        2.63E-01  1.82E-01  1.87E-01  1.40E-01 -5.68E-03 -1.96E-01  2.45E-01  6.80E-02
 
 OM11
+       -5.33E-03 -1.37E-02  5.44E-03  7.67E-03  2.33E-03  3.39E-02 -6.29E-03 -1.19E-02  4.80E-02
 
 OM12
+       -1.02E-02  7.73E-02 -1.09E-02  3.00E-02 -6.58E-05 -9.05E-03 -3.38E-02 -8.03E-03 -1.49E-01  2.83E-02
 
 OM13
+        3.15E-03  1.15E-02  5.45E-03  2.13E-03  9.74E-03  8.81E-04  4.84E-03 -8.88E-03  8.85E-02 -2.55E-02  2.23E-02
 
 OM14
+       -5.62E-03  9.59E-04  1.09E-02  2.56E-02  1.08E-02  1.46E-03  1.70E-02  9.88E-03  9.27E-02  5.08E-02 -1.25E-02  3.13E-02
 
 OM15
+        1.75E-03 -1.16E-02 -4.89E-03 -1.38E-03  1.87E-02 -1.74E-04  1.53E-02  1.67E-02  8.68E-02  4.53E-02  1.38E-02 -5.65E-02
          2.79E-02
 
 OM16
+        1.03E-02 -6.19E-03  9.27E-03  1.03E-02  2.36E-02  6.62E-03 -1.36E-02  3.40E-03 -6.28E-03 -4.45E-03  3.32E-03  4.69E-02
         -1.21E-01  3.28E-02
 
 OM17
+       -9.30E-03  1.50E-02 -1.79E-02 -1.82E-04 -1.36E-02  1.55E-02 -1.91E-02 -5.12E-03  2.33E-02 -1.40E-01  6.19E-02 -1.21E-01
          7.56E-02  5.06E-02  3.04E-02
 
 OM18
+       -1.99E-02  4.80E-03  7.23E-03  1.13E-02  2.44E-02  4.51E-03 -9.86E-03  2.49E-03  2.68E-01  1.31E-01  1.23E-01  1.17E-01
         -1.29E-02 -1.26E-01  1.83E-01  2.94E-02
 
 OM22
+        1.14E-02 -2.04E-01  3.19E-02 -7.16E-03  4.50E-03  1.94E-02  2.63E-02  3.98E-02  2.26E-02 -2.57E-01 -1.75E-02 -1.02E-02
         -2.29E-02  2.12E-02  2.56E-02 -3.52E-02  4.41E-02
 
 OM23
+       -4.63E-03  9.27E-02  5.83E-02  9.76E-03  8.07E-03 -5.15E-03 -2.48E-03  8.97E-03 -3.84E-02  8.11E-02 -7.28E-02  1.94E-04
         -3.32E-03 -1.10E-02  3.10E-03 -1.33E-02 -4.40E-02  1.92E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        1.69E-02 -1.19E-01  4.37E-02  1.82E-02  2.12E-02 -1.32E-02  4.13E-02  3.87E-02 -2.20E-02 -1.96E-02  1.23E-02 -7.35E-02
          2.24E-03 -1.60E-02 -6.04E-03  5.42E-03  1.67E-01 -6.26E-03  2.86E-02
 
 OM25
+        1.03E-02  5.92E-03  8.94E-03  5.17E-03  5.46E-03  6.05E-03  1.86E-02  1.05E-02 -1.24E-02  3.67E-02 -1.32E-02  8.05E-03
         -1.00E-01  1.58E-02 -2.55E-02 -1.60E-02  7.46E-02  2.03E-02 -5.77E-02  2.43E-02
 
 OM26
+       -3.26E-03  3.63E-02  1.48E-02  9.23E-03 -1.34E-02 -6.82E-03 -9.50E-03 -2.22E-03  1.48E-03  1.32E-02 -1.67E-02 -1.64E-02
          3.96E-03 -9.89E-02 -7.45E-03  1.33E-02  1.22E-02  5.33E-02  2.33E-02 -1.25E-01  2.80E-02
 
 OM27
+       -9.92E-03  1.86E-01 -2.18E-02 -9.19E-03 -4.46E-04 -1.34E-02 -2.32E-02 -3.53E-02 -4.29E-03  1.05E-01  9.89E-03  1.41E-02
         -7.54E-03 -6.31E-05 -7.09E-02 -1.68E-02 -3.43E-01  9.26E-02 -2.59E-01  6.40E-02  4.15E-02  2.94E-02
 
 OM28
+       -1.21E-02 -1.40E-02  2.13E-02 -3.73E-03 -1.54E-03  1.30E-02  1.70E-02  1.56E-02 -3.65E-02  1.16E-01 -1.04E-02  2.32E-02
          5.30E-03  3.34E-02 -2.69E-02 -4.76E-02  3.01E-01  1.12E-01  1.20E-01 -2.29E-04 -1.41E-01  1.28E-01  2.53E-02
 
 OM33
+       -2.17E-03  6.41E-03 -1.30E-01 -5.53E-02 -5.52E-03 -1.30E-02  3.35E-03 -2.14E-03  1.54E-02 -4.64E-03  1.29E-01  5.83E-04
          8.69E-03  4.19E-04  1.12E-02  3.20E-02  4.48E-03 -1.32E-02 -3.15E-03  7.95E-04 -1.64E-02  1.66E-02  1.17E-02  2.93E-02
 
 OM34
+       -1.40E-02  1.21E-02  1.16E-01  4.95E-02  1.72E-03 -1.90E-02  3.99E-03  1.84E-02  6.69E-03  8.85E-04  1.09E-01  7.03E-02
         -1.91E-02  9.09E-03 -2.30E-02  1.70E-02  2.13E-02  7.04E-02  1.93E-02 -8.88E-03  1.40E-02  3.49E-03  3.42E-02 -4.78E-02
         2.37E-02
 
 OM35
+        2.99E-03 -7.71E-03 -3.80E-02 -1.08E-02 -7.93E-03  5.78E-04 -3.25E-03  7.11E-03 -7.33E-03  2.19E-03  5.64E-02 -3.98E-03
          4.05E-02 -7.92E-03  5.79E-03  1.14E-02 -8.18E-03  7.45E-02  1.11E-02 -4.68E-03 -1.23E-02  1.20E-02  9.34E-03  2.26E-02
        -6.98E-02  2.00E-02
 
 OM36
+        1.20E-02 -1.41E-02  3.16E-02 -3.08E-03  1.62E-02 -9.24E-03  2.89E-03  8.93E-03 -1.10E-02 -6.13E-03  1.74E-02  7.21E-03
         -1.57E-02  5.61E-02 -1.44E-02 -2.48E-02 -6.67E-03  2.67E-02  6.94E-03 -6.20E-03 -1.64E-02 -2.24E-04  9.63E-03  1.98E-02
         4.30E-02 -1.31E-01  2.29E-02
 
 OM37
+        1.87E-02 -2.57E-02 -5.28E-02 -2.25E-02 -8.31E-03  9.07E-03 -1.19E-03 -1.00E-02  2.09E-03 -1.77E-02 -2.11E-03 -2.89E-02
          8.22E-03 -2.63E-04  7.67E-02  3.50E-02  4.81E-03 -1.39E-01 -8.34E-03  1.50E-02 -1.69E-02 -1.46E-02 -6.54E-03  7.33E-02
        -1.72E-01  6.29E-02  3.38E-02  2.16E-02
 
 OM38
+       -5.02E-03 -1.25E-02  2.60E-04 -3.77E-03  6.17E-03  2.80E-03  1.81E-02  6.84E-03  2.91E-02  4.81E-05  2.33E-01  2.09E-02
          7.53E-03  6.36E-03  3.77E-02  1.11E-01  1.65E-02  1.86E-01  1.17E-02 -8.74E-03 -2.63E-03  7.27E-03  5.25E-02  2.30E-01
         1.18E-01 -5.32E-02 -1.47E-01  1.61E-01  2.07E-02
 
 OM44
+       -1.18E-02  2.49E-02  1.23E-01  8.90E-02 -4.07E-04 -5.39E-03 -5.50E-04  9.01E-03  6.12E-03  2.36E-02  1.49E-02  1.16E-01
         -2.35E-02  1.70E-02 -2.55E-03  2.06E-02  3.60E-02  3.49E-03  1.47E-01 -4.34E-03  2.24E-02 -2.59E-02  4.36E-02 -6.31E-02
         3.22E-02 -1.03E-02 -1.41E-02 -2.45E-03 -3.20E-03  5.62E-02
 
 OM45
+        2.78E-04  2.92E-02  4.34E-03  1.49E-03  7.55E-03  1.64E-02  2.44E-02  3.25E-03  3.03E-03  1.93E-02  6.54E-03  5.09E-02
          7.21E-02 -1.28E-02  7.54E-03  2.03E-02  1.27E-02 -1.96E-03  2.51E-02  5.35E-02  5.12E-03  1.48E-02  7.21E-03  6.26E-03
        -3.17E-03 -5.54E-03 -1.43E-02 -1.07E-02  1.32E-02 -9.06E-02  2.94E-02
 
 OM46
+        8.56E-03 -2.48E-03  5.16E-03 -9.16E-03  1.88E-02  2.69E-02 -6.39E-04  7.22E-03 -4.62E-03 -1.61E-02  2.70E-02  1.45E-02
         -1.78E-02  7.72E-02  1.10E-02 -1.52E-02 -2.81E-03  7.68E-03  3.45E-02 -4.98E-03  4.48E-02  2.00E-02 -1.96E-03  2.36E-03
         1.90E-02  2.27E-03 -1.65E-03 -2.46E-02  3.37E-03  6.84E-02 -1.11E-01  3.37E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -8.10E-03 -1.61E-03 -4.87E-02 -1.23E-02  7.42E-03  9.44E-03 -3.34E-04  2.38E-03 -9.82E-03 -1.35E-02  3.48E-03 -5.51E-03
          2.63E-02  1.99E-02  6.28E-02  1.36E-02 -2.27E-02  1.37E-02 -1.35E-01  1.38E-02  1.56E-02  1.12E-01 -4.85E-03  2.07E-02
         5.80E-02 -1.38E-02 -9.48E-04 -9.98E-03  1.84E-02 -2.25E-01  7.18E-02  3.75E-02  3.31E-02
 
 OM48
+        1.05E-02  1.74E-02  1.66E-02  1.21E-02  7.18E-03  1.57E-02  1.75E-02  2.07E-02 -6.42E-03  7.20E-02  1.25E-02  2.11E-01
         -1.65E-03  1.81E-02  2.18E-03  1.04E-01  3.06E-02  3.47E-02  1.88E-01 -3.39E-02  2.96E-02 -1.27E-02  1.35E-01  1.57E-02
         1.41E-01 -7.76E-03 -1.04E-02 -2.31E-02  3.40E-02  1.71E-01 -5.89E-02 -1.09E-01  1.55E-01  3.01E-02
 
 OM55
+        2.01E-02 -2.13E-02  2.62E-03  1.65E-02 -1.80E-02 -1.90E-02 -6.35E-04  8.05E-03  1.67E-02  3.61E-03  1.68E-03 -7.89E-03
          1.15E-01 -2.86E-03  7.82E-03  1.14E-02  3.87E-03 -1.95E-02  9.11E-03  1.20E-01 -2.49E-02 -9.40E-03 -1.51E-02  9.38E-03
         1.00E-02  1.36E-03  3.82E-03 -6.89E-04 -5.52E-03  1.01E-02 -1.17E-01  1.41E-02 -2.24E-02  1.45E-02  4.28E-02
 
 OM56
+       -5.75E-03 -1.70E-02  1.14E-03  2.33E-03 -1.03E-02 -5.54E-03 -9.11E-03 -9.52E-03 -2.67E-03 -1.04E-02 -1.83E-02  1.57E-03
         -1.20E-02  6.78E-02  1.91E-03 -1.65E-02  1.21E-02 -1.59E-03  1.28E-02  6.85E-03  6.82E-02 -3.13E-02 -1.60E-02 -5.19E-03
         1.52E-03  2.17E-02 -1.96E-02 -1.34E-02 -2.38E-03  1.20E-02  6.67E-02 -7.18E-02 -4.06E-03  1.89E-02 -1.57E-01  3.06E-02
 
 OM57
+        8.43E-04 -2.64E-02 -6.35E-03  2.12E-02 -9.60E-03 -8.79E-03 -2.90E-02 -2.52E-04  9.51E-03 -1.28E-02  1.16E-02 -1.20E-02
          2.68E-02  1.97E-02  4.62E-02  5.43E-03  2.88E-03 -4.60E-03  1.22E-02 -1.31E-01  7.67E-03  9.84E-03  5.06E-03 -1.93E-02
        -1.67E-04  5.11E-02  1.35E-02  1.24E-02 -2.20E-02  2.53E-02 -1.58E-01 -5.11E-03 -7.68E-02  8.04E-03  1.23E-01  3.74E-02
          2.88E-02
 
 OM58
+       -6.62E-03  5.60E-03 -1.07E-02 -1.70E-04  2.02E-03 -2.65E-03 -5.71E-03 -5.50E-03  4.11E-02  1.88E-02 -7.28E-04  8.81E-03
          1.77E-01 -3.58E-02  2.19E-02  5.34E-02  8.26E-03  8.55E-03 -1.14E-02  1.84E-01 -4.58E-02  3.08E-02  3.72E-02 -2.25E-03
        -1.44E-02  1.17E-01 -1.41E-02  1.45E-02 -1.78E-02  4.41E-03  1.10E-01 -2.13E-02 -7.42E-03 -6.04E-02 -5.85E-02 -1.41E-01
          1.59E-01  2.64E-02
 
 OM66
+        1.47E-03 -3.42E-03 -2.74E-03  9.25E-03  3.15E-02 -4.73E-04  6.74E-03  7.40E-03 -3.65E-03 -3.28E-03 -4.16E-03 -7.80E-03
          9.56E-03  1.14E-03 -5.92E-03  1.15E-02 -9.00E-03 -9.96E-03 -8.05E-03 -4.78E-04  3.56E-02 -1.17E-02 -1.55E-02 -1.03E-02
         3.73E-02  1.45E-02  7.59E-02  2.77E-02  1.09E-02  2.11E-02 -1.56E-02  4.74E-02 -6.14E-03 -1.40E-02 -6.83E-03 -2.50E-01
         -1.39E-02  2.86E-02  6.18E-02
 
 OM67
+       -9.63E-03 -2.65E-02 -2.11E-03 -1.40E-03 -2.23E-03 -3.55E-02  2.40E-02  1.16E-02 -5.55E-03  1.02E-02 -1.67E-03  1.43E-02
         -3.83E-03  1.56E-02 -2.27E-02 -1.01E-02 -7.32E-03 -6.83E-03 -8.69E-03  3.24E-02 -1.44E-01 -5.36E-03  2.62E-03  3.00E-02
         1.82E-06 -1.81E-02  5.84E-02  3.47E-02  8.71E-03 -1.85E-03  3.29E-03 -1.32E-01 -2.96E-03  1.33E-02  2.13E-02  6.05E-02
         -1.08E-01 -5.27E-02  5.10E-02  3.27E-02
 
 OM68
+        7.13E-03 -8.49E-03  6.58E-03  1.73E-02 -7.81E-04  2.87E-03 -1.81E-03  1.83E-02 -2.59E-02 -4.27E-03 -1.49E-03  2.05E-02
         -5.21E-02  1.90E-01 -2.38E-03 -7.01E-02  1.15E-03  2.25E-02  3.73E-02 -3.51E-03  1.78E-01  1.23E-02  1.76E-03  3.32E-03
         1.32E-03 -5.51E-03  8.42E-02  1.66E-02  1.72E-02  1.47E-02 -2.49E-02  1.03E-01  9.81E-03  1.31E-02 -5.38E-03 -4.79E-03
         -4.59E-02 -1.51E-01 -2.25E-01  1.65E-01  3.08E-02
 
 OM77
+        2.89E-03 -2.38E-02 -4.39E-03 -1.45E-02 -1.77E-03  4.99E-03 -6.32E-03 -2.25E-02 -3.38E-03 -8.39E-03 -6.25E-03 -7.07E-03
          1.94E-04  6.72E-03  1.72E-02  5.52E-03  3.16E-02 -1.21E-02  2.51E-02 -2.98E-02 -9.22E-03 -2.34E-01 -4.50E-02  1.20E-02
        -1.83E-03  3.44E-03  1.30E-02  8.02E-02  8.66E-03  5.25E-02 -2.64E-02 -3.04E-02 -2.57E-01 -4.45E-02  8.04E-03  2.38E-02
          1.04E-01  2.45E-02  2.03E-02  6.52E-02  2.82E-03  5.04E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        5.48E-03 -4.12E-02 -1.41E-02 -8.88E-03  2.86E-02 -5.75E-05 -5.76E-03  1.70E-03  1.96E-02 -3.59E-02  1.43E-02 -2.48E-02
          1.14E-02  1.24E-02  1.98E-01  6.33E-02 -1.15E-02 -1.92E-02 -2.06E-02 -6.17E-04  2.77E-02  7.03E-02 -6.73E-02  2.21E-02
        -2.53E-02 -2.56E-03  3.97E-04  1.40E-01  8.69E-02 -1.97E-02  1.89E-02  2.18E-02  4.83E-02 -9.76E-02 -1.91E-02 -7.51E-03
         -4.32E-03  6.20E-02  7.01E-03 -1.30E-01 -1.44E-02  2.68E-01  3.05E-02
 
 OM88
+       -1.19E-02  8.68E-03  7.33E-03 -4.11E-03  2.75E-02  2.91E-02  1.54E-02  2.31E-03  6.43E-02  4.32E-02  4.71E-02  3.43E-02
          4.63E-03 -3.96E-02  8.79E-02  3.17E-01  4.53E-02  7.42E-02  5.17E-02 -3.30E-02 -3.76E-03  3.66E-02  2.92E-01  5.99E-03
         6.35E-03  6.28E-03 -3.66E-02  5.29E-02  1.95E-01  2.40E-02 -1.92E-03 -1.11E-02  3.79E-02  2.05E-01  1.07E-02 -1.71E-02
          3.47E-03 -3.89E-02  2.67E-02 -6.95E-02 -2.11E-01  5.43E-02  3.25E-01  4.50E-02
 
 SG11
+       -6.22E-03  6.75E-03  6.17E-03 -1.82E-03  1.56E-03  1.31E-02 -1.62E-02 -1.37E-02  8.40E-03  2.60E-02 -4.62E-03 -4.96E-03
         -1.49E-02  1.32E-02 -1.32E-02 -5.20E-03 -5.02E-02  7.11E-03 -1.96E-02 -5.49E-03  6.64E-04 -9.23E-04 -2.48E-02 -3.86E-02
        -1.12E-03 -1.34E-02 -1.47E-03 -3.01E-02 -1.08E-02 -1.38E-02  8.45E-03  3.66E-03  1.80E-02  7.30E-04  3.24E-03  9.38E-03
         -3.84E-03 -8.14E-03 -4.87E-03 -2.58E-02  2.23E-02 -4.97E-03 -5.58E-03 -4.89E-03  6.47E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        2.58E-02  2.93E-02 -7.75E-03  1.46E-02  3.81E-03 -2.80E-03 -6.48E-03 -1.39E-02 -5.29E-03  1.06E-02 -9.05E-03 -1.93E-02
         -6.72E-04 -2.39E-02  1.67E-02  4.37E-03 -2.05E-02  3.05E-02 -1.42E-02 -5.44E-03  8.83E-03  3.29E-02  2.61E-03 -4.55E-04
         8.18E-03 -1.49E-02 -9.94E-03 -1.73E-02 -5.05E-03  6.94E-03  1.04E-02 -5.92E-03  4.85E-03  3.49E-03 -1.75E-02  1.28E-02
         -2.09E-02  1.01E-02 -4.46E-02 -1.39E-02 -3.12E-03 -3.51E-02 -2.28E-02  8.72E-03 -3.77E-02  0.00E+00  1.19E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        2.24E+02
 
 TH 2
+        5.64E+01  2.70E+02
 
 TH 3
+       -9.60E+00  7.07E+00  3.86E+02
 
 TH 4
+       -1.04E+01 -1.21E+01  2.02E+00  2.03E+02
 
 TH 5
+       -3.15E+01 -3.62E+01 -5.75E+00  1.85E+01  2.62E+02
 
 TH 6
+       -1.12E+01 -2.11E+01 -2.03E+01 -1.39E+01  4.26E+01  1.94E+02
 
 TH 7
+        1.85E+01  6.46E+01 -1.16E+01  4.80E+01 -3.28E+01 -3.17E+01  2.45E+02
 
 TH 8
+       -7.83E+01 -9.08E+01 -5.59E+01 -4.06E+01  3.40E+01  6.34E+01 -9.32E+01  3.10E+02
 
 OM11
+       -4.56E-01  9.07E-01 -3.03E-01 -1.99E+00  7.72E-01 -1.01E+01  3.62E+00  2.31E+00  5.03E+02
 
 OM12
+        2.31E-01 -6.69E+00  1.17E+01 -1.06E+01  1.65E+00 -1.98E+00  1.62E+01  1.26E+00  1.80E+02  1.57E+03
 
 OM13
+       -1.17E+01 -2.18E+01 -1.06E+01 -3.11E+00 -2.14E+00  3.27E+00 -9.18E+00  2.10E+01 -5.78E+01  3.76E+01  2.28E+03
 
 OM14
+        5.07E+00  9.61E+00  7.64E+00 -9.45E+00 -4.76E+00  1.55E+00 -8.28E+00 -3.98E+00 -5.51E+01 -8.63E+00  6.02E+01  1.16E+03
 
 OM15
+        6.77E+00  1.60E+01 -2.15E+00 -2.08E+00 -2.04E+01 -4.45E+00 -3.36E+00 -1.80E+01 -1.00E+02 -1.42E+02 -1.65E+01  6.57E+01
          1.46E+03
 
 OM16
+       -8.33E-01  2.95E+00 -8.18E+00 -2.61E+00 -1.53E+01 -1.98E+00  7.16E+00 -2.70E+00 -3.77E+01 -7.26E+01 -1.44E+01 -5.21E+01
          1.69E+02  1.06E+03
 
 OM17
+       -2.49E+00 -2.66E+01  1.13E+01  2.03E+00  1.66E+01 -3.65E+00  2.45E+00  9.47E+00  5.33E+01  2.57E+02 -6.54E+01  1.71E+02
         -1.34E+02 -1.13E+02  1.28E+03
 
 OM18
+        1.53E+01  1.69E+01 -1.34E+01 -2.75E+00 -1.98E+01  3.76E+00  1.02E+01 -1.44E+01 -2.56E+02 -3.71E+02 -1.49E+02 -1.45E+02
          1.62E+02  2.10E+02 -3.36E+02  1.64E+03
 
 OM22
+        1.17E+01  6.65E+01 -1.16E+00  1.58E+00 -1.23E+01 -1.27E+01  2.05E+01 -3.14E+01  8.97E+00  3.36E+02  3.09E+01  1.20E+01
         -5.51E+00 -3.14E+01  4.54E+01 -6.09E+01  8.00E+02
 
 OM23
+       -1.27E+01 -7.07E+01 -5.44E+01 -2.10E-02  1.08E+00  8.88E+00 -1.03E+01  2.47E+01  1.14E+01 -1.03E+02  3.77E+02  1.86E+01
          9.59E+00  3.25E+01 -8.00E+01  6.87E+01  5.44E+01  3.14E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        7.58E+00  5.13E+01 -7.30E+00 -1.08E+01 -1.68E+01  6.59E+00 -4.96E+00 -1.91E+01  1.06E+01  2.60E+01 -1.84E+01  1.91E+02
          2.05E+01  3.08E+01  5.79E+01 -1.94E+01 -9.52E+00 -7.51E+00  1.50E+03
 
 OM25
+       -2.34E+00 -3.13E+00 -8.16E+00 -5.94E+00 -6.92E+00 -4.71E+00 -1.32E+01 -5.08E+00 -9.76E+00 -1.54E+02 -1.58E+01  9.38E+00
          2.93E+02  4.62E+01 -2.04E+01  8.38E+01 -1.90E+02 -9.21E+01  6.59E+01  2.02E+03
 
 OM26
+       -1.87E+00 -2.20E+01 -1.27E+01 -2.82E-01  1.42E+01  1.17E+01 -6.94E+00  1.14E+01 -1.44E+01 -1.14E+02  6.21E+00  2.02E+01
          4.02E+01  2.02E+02 -1.28E+01  6.30E+01 -1.56E+02 -1.38E+02 -4.76E+01  2.75E+02  1.55E+03
 
 OM27
+       -2.18E+01 -8.05E+01  4.63E+00  1.03E+01  1.18E+01  1.33E+01 -1.40E+01  4.48E+01 -4.46E+00  1.03E+02 -2.05E+01  4.31E+01
         -6.20E+00 -2.23E+01  1.71E+02 -4.52E+01  4.42E+02 -9.49E+01  3.76E+02 -2.03E+02 -1.93E+02  1.74E+03
 
 OM28
+        1.09E+01 -1.71E+00 -9.02E+00  3.49E+00  8.08E+00  1.86E+00 -6.35E+00 -6.16E+00 -1.87E+01 -4.46E+02 -1.39E+01 -3.92E+01
          3.60E+01  4.00E+01 -1.04E+02  3.75E+02 -5.91E+02 -2.24E+02 -2.21E+02  2.20E+02  4.37E+02 -6.21E+02  2.48E+03
 
 OM33
+       -3.59E+00 -1.17E+01  8.28E+01  2.44E+01  7.76E+00  2.78E+00  9.25E-02 -1.05E+01 -2.77E+00  3.07E+00 -1.27E+02 -2.59E-02
         -3.11E+00 -1.94E+00  7.68E+00 -2.29E+01 -8.08E+00  8.91E+01 -6.37E+00 -2.07E+00 -4.41E+00 -2.27E+01 -3.49E+01  1.29E+03
 
 OM34
+        1.35E+01 -3.82E+00 -8.56E+01 -2.65E+01  3.71E+00  1.76E+01 -2.44E+00  9.01E+00  1.47E+00  1.67E+01 -1.87E+02 -5.07E+01
          3.02E+01  7.89E+00  2.28E+01 -5.46E+00 -1.63E+01 -4.26E+01  1.11E+01  1.47E+01 -9.50E+00 -7.89E+00 -4.58E+01  1.13E+02
         1.99E+03
 
 OM35
+        2.80E+00  2.01E+01  2.74E+01  6.80E+00  3.53E+00 -3.98E+00  6.96E+00 -2.08E+01  2.28E+01  1.53E+01 -2.57E+02 -1.89E+01
         -2.62E+01  8.61E+00  1.68E+01 -4.75E+00 -3.43E+00 -3.80E+02 -2.98E+01  7.93E+01  7.21E+01 -2.91E+01  4.03E+01 -6.68E+01
         9.99E+01  2.71E+03
 
 OM36
+       -1.17E+00  1.59E+01 -2.46E+01  3.05E+00 -8.71E+00  1.23E+00  4.09E+00 -6.27E+00  1.12E+01  2.15E+01 -1.62E+02 -2.13E+01
          4.58E+00 -4.80E+01  3.15E+01 -1.57E+00  1.22E+01 -2.48E+02 -1.98E+01  3.42E+01  6.32E+01  5.05E+00 -1.04E+01 -1.11E+02
        -1.21E+02  3.91E+02  2.09E+03
 
 OM37
+       -1.77E+01 -7.55E+00  1.84E+01  8.15E+00  1.05E+01 -2.68E+00 -7.41E-02  8.99E+00  5.30E+00  4.71E+00  1.52E+02  3.31E+01
         -8.94E-01  1.59E+01 -7.99E+01 -2.53E+01  1.64E+01  5.10E+02  2.19E+01 -6.15E+01 -1.07E+01  1.31E+01 -6.13E+01 -8.14E+00
         3.87E+02 -2.46E+02 -2.10E+02  2.47E+03
 
 OM38
+        1.03E+01  3.36E+01 -7.39E+00 -3.87E+00 -3.08E+00 -2.90E+00 -3.56E+00 -1.34E+01  1.56E+01  2.82E+01 -6.21E+02 -3.78E+01
         -2.41E+01 -2.51E+01  3.29E+01 -6.47E+01 -2.18E+01 -7.99E+02 -7.03E+00  4.76E+01  6.68E+01  1.77E+01  7.01E+01 -4.53E+02
        -3.40E+02  3.92E+02  5.48E+02 -6.13E+02  3.16E+03
 
 OM44
+        5.52E+00 -9.51E+00 -4.11E+01 -2.29E+01  6.98E-01  6.01E+00 -4.91E+00  1.16E+01 -8.87E-01 -1.17E+01 -1.88E+01 -5.74E+01
          8.30E+00 -2.21E+00 -1.83E+01  5.90E+00 -1.52E+01  6.71E+00 -6.51E+01 -7.75E-03 -7.58E+00 -2.31E+01  8.86E-01  3.31E+01
         7.59E+00  1.54E+01  1.84E+01 -1.21E+01  2.74E+00  3.70E+02
 
 OM45
+       -6.16E+00 -2.38E+01 -8.92E+00 -5.06E+00  3.82E-01 -6.50E+00 -1.54E+01  8.48E+00  1.16E+01 -2.61E+01 -1.98E+01 -1.09E+02
         -9.38E+01 -7.96E+00 -2.41E+01 -1.45E+01 -2.66E+01  2.11E+01 -1.15E+02 -3.57E+01 -9.96E+00 -4.30E+01  1.39E+01 -1.31E+00
        -1.99E+00  3.02E+01  1.66E+01  1.97E+01 -1.09E+01  4.84E+01  1.29E+03
 
 OM46
+       -2.95E+00  2.19E+00  5.63E+00  1.03E+01 -8.92E+00 -1.48E+01  3.67E+00 -9.12E+00  7.61E+00  6.23E+00 -3.75E+01 -5.48E+01
         -7.38E+00 -5.69E+01 -1.42E+01  2.83E+00  2.53E+00 -1.00E+01 -8.80E+01 -1.82E+00 -6.40E+00 -3.38E+01  1.12E+01 -1.75E+01
        -3.71E+01  3.81E+00  2.60E+01  2.80E+01  2.31E+01 -5.36E+01  1.36E+02  9.72E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        9.00E+00  1.58E+01  1.75E+01 -4.34E+00 -5.35E+00 -3.48E+00  3.44E+00 -7.66E+00  1.10E+01  2.69E+01  6.20E+00  5.00E+01
         -2.53E+01 -1.74E+01 -4.22E+01 -1.37E+00 -6.80E+00 -1.71E+01  1.92E+02 -7.86E-01 -1.77E+01 -1.72E+01  1.25E+01 -2.12E+00
        -6.71E+01  1.69E+01  1.25E+00 -6.95E+00  6.00E+00  1.47E+02 -8.28E+01 -9.32E+01  1.14E+03
 
 OM48
+       -1.49E+01 -1.73E+01  1.43E+01  1.21E+01  4.92E-01 -1.22E+01 -3.78E+00 -4.19E+00  2.65E+01 -6.64E+01  5.02E+00 -2.85E+02
         -3.34E+01 -2.72E+01 -5.25E+01 -2.48E+01 -3.14E+00 -8.34E+00 -3.19E+02  1.06E+01 -3.25E+01 -3.86E+01 -2.86E+01 -5.25E+01
        -2.10E+02  9.63E+00  3.59E+01 -2.81E+01  7.49E+01 -1.26E+02  1.32E+02  2.03E+02 -3.06E+02  1.48E+03
 
 OM55
+       -7.03E+00  2.34E+00 -3.27E-01 -4.45E+00  1.02E+01  7.92E+00 -7.93E-01  4.37E+00  4.60E-02  1.34E+01  1.61E+01 -1.58E+00
         -1.61E+02 -2.79E+01  7.70E+00 -2.68E+01  1.12E+01  3.18E+01 -1.52E+01 -2.22E+02 -2.81E+01  2.02E+01 -2.85E-01 -1.01E+01
        -1.85E+01 -2.42E+01 -7.37E+00  1.23E+01 -2.23E+00 -1.04E+00  7.31E+01 -3.68E+00  9.35E+00 -2.02E+00  6.20E+02
 
 OM56
+        1.48E+00  9.12E+00  4.19E+00 -1.82E+00  1.92E+00  2.23E+00  6.04E+00  2.30E+00  2.82E+00  3.85E+01  5.04E+01  9.67E+00
         -8.39E+01 -1.33E+02  1.81E+01 -3.75E+01  3.12E+01  3.14E+01 -3.58E+00 -1.73E+02 -2.01E+02  6.73E+01 -6.21E+01  9.74E+00
        -2.09E+01 -1.12E+02 -2.48E+01  2.75E+01 -4.34E+01 -1.86E+01 -9.21E+01  3.79E+01  1.38E+00 -1.34E+01  1.81E+02  1.29E+03
 
 OM57
+        6.95E+00  2.49E+01  3.57E+00 -1.07E+01 -4.43E+00  2.63E-02  1.31E+01 -1.49E+01 -2.67E+00 -1.96E+01 -3.13E+01 -3.93E+00
          6.40E+01 -1.07E+01 -8.25E+01  3.70E+01 -3.85E+01 -7.07E+00 -1.68E+01  3.25E+02  4.64E+01 -1.30E+02  6.57E+01  2.13E+01
        -1.85E+00 -2.97E+01 -3.07E+01 -2.79E+01  2.83E+01  7.89E+00  2.14E+02  3.19E+01  4.28E+01 -5.83E+00 -1.55E+02 -1.56E+02
          1.41E+03
 
 OM58
+       -4.87E-02 -6.97E+00  8.06E+00  1.81E+00  5.35E+00  3.36E+00  2.52E+00  8.16E+00 -4.71E+00  5.06E+01  4.67E+01 -1.88E+01
         -3.49E+02 -5.97E+01  4.01E+01 -1.72E+02  4.60E+01  1.07E+01 -1.43E+01 -5.10E+02 -8.82E+01  4.42E+01 -2.17E+02  1.14E+01
        -8.03E+00 -2.68E+02 -3.47E+01  1.83E+01 -2.84E+01 -2.33E+01 -1.70E+02  4.78E+00 -5.03E+00  4.84E+01  1.76E+02  3.29E+02
         -3.69E+02  1.85E+03
 
 OM66
+        1.06E+00  3.63E+00  5.95E+00 -3.92E+00 -9.21E+00 -2.85E+00  9.52E-01 -3.60E+00  4.93E+00  1.57E+01  2.36E+01  8.35E+00
         -1.79E+01 -4.99E+01  9.56E+00 -1.71E+01  1.70E+01  2.16E+01  5.11E+00 -3.53E+01 -1.04E+02  2.29E+01 -2.82E+01  1.42E+01
        -3.07E+01 -4.94E+01 -8.28E+01 -1.51E+01 -4.17E+01 -9.04E+00 -4.58E+00 -3.77E+01  2.66E+00 -5.75E-01  3.11E+01  1.88E+02
         -1.06E+01  5.26E+01  3.18E+02
 
 OM67
+        9.89E+00  1.74E+01 -2.30E+00 -2.20E+00  1.42E+00  1.52E+01 -7.38E+00 -7.24E+00 -4.09E+00 -2.97E+01 -5.56E+00 -1.57E+01
          8.59E+00  5.33E+01 -2.77E+01  1.77E+01 -2.43E+01 -9.84E+00 -9.89E+00  3.61E+01  2.65E+02 -9.16E+01  9.17E+01 -3.04E+01
         1.02E+00  4.15E+01 -3.63E+01 -5.83E+01  4.72E+00 -4.98E+00  2.94E+01  1.54E+02 -4.17E+01  3.01E+01 -4.38E+01 -1.45E+02
          1.60E+02 -3.91E+01 -8.58E+01  1.10E+03
 
 OM68
+        2.60E+00  7.37E+00  7.98E+00 -9.05E+00 -7.57E+00 -1.08E+01  4.98E+00 -1.20E+01  1.45E+01  5.76E+01  2.84E+01 -7.04E+00
         -2.88E+01 -2.70E+02  2.09E+01 -9.06E+01  6.01E+01 -1.40E+00 -3.74E+01 -1.18E+02 -4.22E+02  4.75E+01 -2.49E+02  3.79E+01
         2.07E+01 -1.04E+02 -1.75E+02 -1.38E+01 -1.61E+02 -8.35E-01 -6.71E+00 -1.42E+02  8.36E+00 -7.40E+01  4.67E+01  2.19E+02
         -2.24E+01  3.02E+02  2.08E+02 -3.05E+02  1.48E+03
 
 OM77
+       -6.41E+00 -1.55E+01  4.84E+00  6.52E+00  5.23E+00 -5.48E-01 -2.57E+00  1.53E+01  5.23E+00  7.04E+00  2.78E+00  1.46E+01
         -4.91E+00 -7.64E+00  5.09E+01 -1.06E+01  3.60E+01 -3.31E+01  6.58E+01 -5.27E+00 -1.92E+01  2.55E+02 -5.05E+01 -1.30E+01
        -3.08E+01  3.03E+00  3.61E+00 -4.50E+01  2.75E+01 -1.66E+00 -9.31E+00 -1.00E+00  1.94E+02 -3.32E+01  3.63E+00 -3.95E+00
         -9.78E+01  4.72E+00 -2.62E+00 -9.30E+01  9.00E+00  5.15E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        9.70E+00  5.12E+01 -6.42E-01 -2.73E+00 -2.18E+01  1.60E+00  1.49E+01 -2.89E+01 -1.22E+01 -6.41E+01  1.18E+01 -5.26E+01
          3.63E+01  2.09E+01 -2.91E+02  1.82E+02 -8.74E+01  5.43E+01 -9.02E+01  5.90E+01  7.43E+01 -3.42E+02  4.28E+02 -2.56E+01
        -2.41E+01  4.78E+01 -1.22E+01 -1.87E+02 -1.05E+01 -9.18E+00  1.74E+01  3.31E+01 -1.63E+02  2.62E+02 -1.77E+00 -5.10E+01
          1.21E+02 -1.81E+02 -2.01E+01  2.27E+02 -1.78E+02 -2.92E+02  1.60E+03
 
 OM88
+        8.89E-01 -1.22E+01  2.22E+00 -8.17E-01 -5.88E+00 -1.04E+01 -6.16E+00  4.47E+00  1.33E+01  8.67E+01  2.42E+01  4.09E+01
         -2.66E+01 -4.20E+01  6.02E+01 -3.83E+02  7.21E+01 -3.44E+01  2.43E+01 -3.76E+01 -1.12E+02  9.04E+01 -5.30E+02  6.04E+01
         6.69E+01 -4.71E+01 -2.22E+01  2.88E+01 -2.48E+02  8.48E+00 -1.19E+01 -3.24E+01  2.00E+01 -2.26E+02  1.28E+00  5.82E+01
         -3.12E+01  1.74E+02  2.63E+01 -4.25E+01  3.01E+02  2.51E+01 -4.51E+02  8.39E+02
 
 SG11
+        8.75E+01  1.14E+02 -1.82E+02  1.21E+02 -1.28E+02 -2.40E+02  3.46E+02  9.20E+01 -5.85E+02 -1.20E+03 -1.19E+02  5.99E+02
          1.15E+03 -1.19E+02  4.04E+02  7.35E+02  1.67E+03 -3.91E+02  9.17E+02  5.70E+02  7.73E+02  1.13E+03  9.34E+02  1.88E+03
         5.40E+02  1.03E+03  2.26E+02  1.87E+03 -3.16E+01  2.48E+02 -5.87E+02  6.35E+01 -7.38E+02 -3.46E+02 -4.56E+02 -8.77E+02
          3.58E+02 -4.72E+02 -2.20E+02  1.67E+03 -1.86E+03 -2.82E+01  4.93E+02 -5.04E+02  2.41E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -4.58E+02 -3.31E+02  1.46E+02 -1.72E+02 -1.17E+01  9.09E+01 -1.14E+02  3.97E+02  1.57E+01 -2.31E+02  2.96E+02  5.73E+02
          2.51E+02  5.76E+02 -6.84E+02  1.48E+02  4.46E+01 -1.13E+03  2.94E+02  5.04E+02  1.40E+01 -6.16E+02  3.94E+02 -1.18E+02
        -4.04E+02  7.91E+02  2.63E+02  2.28E+02  4.88E+02 -1.80E+02 -1.29E+02  7.86E+01  1.13E+02  1.61E+01  1.88E+02 -2.49E+02
          7.22E+02 -7.27E+02  5.66E+02  3.52E+02 -7.98E+01  3.60E+02  8.75E+02 -4.73E+02  5.16E+04  0.00E+00  7.11E+05
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     3052.643
Stop Time: 
Wed 11/02/2016 
02:27 AM

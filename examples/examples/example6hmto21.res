Wed 11/02/2016 
07:05 PM
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

$EST METHOD=ITS INTERACTION SIGL=4 NITER=15 PRINT=1 NOABORT NOPRIOR=1 file=example6hmto21_its.ext
$EST METHOD=bayes INTERACTION NBURN=2000 NITER=0 PRINT=10 MASSRESET=1 NOPRIOR=0 file=example6hmto21_bayes.ext
$EST METHOD=NUTS INTERACTION  NBURN=250 NITER=2000 PRINT=5 MASSRESET=0 PMADAPT=200  file=example6hmto21.ext
     OLKJDF=1.0
$COV MATRIX=R UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.
  
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
 RAW OUTPUT FILE (FILE): example6hmto21_its.ext
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

 iteration            0 OBJ=  -3179.34804709958
 iteration            1 OBJ=  -3563.39825052570
 iteration            2 OBJ=  -3698.49131024100
 iteration            3 OBJ=  -3813.49052442310
 iteration            4 OBJ=  -3921.38645757702
 iteration            5 OBJ=  -4025.66889419349
 iteration            6 OBJ=  -4127.61073505300
 iteration            7 OBJ=  -4227.50665677951
 iteration            8 OBJ=  -4325.17010194533
 iteration            9 OBJ=  -4419.82458046677
 iteration           10 OBJ=  -4509.87267715267
 iteration           11 OBJ=  -4592.19272921940
 iteration           12 OBJ=  -4659.87003784235
 iteration           13 OBJ=  -4700.24430332201
 iteration           14 OBJ=  -4709.62334419566
 iteration           15 OBJ=  -4710.51279864924
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -8.7799E-04 -1.6780E-03  2.3470E-03  1.3344E-03  1.1374E-03  1.5436E-03  2.9935E-04  8.2864E-04
 SE:             6.9351E-02  5.3938E-02  3.7889E-02  6.5123E-02  5.6761E-02  5.7438E-02  6.4405E-02  6.1401E-02
 N:                      50          50          50          50          50          50          50          50
 
 P VAL.:         9.8990E-01  9.7518E-01  9.5061E-01  9.8365E-01  9.8401E-01  9.7856E-01  9.9629E-01  9.8923E-01
 
 ETASHRINKSD(%)  6.4398E-01  4.5093E+00  8.2195E+00  1.6470E+00  1.4992E+00  5.8680E+00  3.9429E-01  1.5720E+00
 ETASHRINKVR(%)  1.2838E+00  8.8152E+00  1.5763E+01  3.2670E+00  2.9760E+00  1.1392E+01  7.8702E-01  3.1193E+00
 EBVSHRINKSD(%)  6.3658E-01  5.3912E+00  9.8831E+00  2.1078E+00  1.5741E+00  6.3487E+00  4.5677E-01  1.7624E+00
 EBVSHRINKVR(%)  1.2691E+00  1.0492E+01  1.8789E+01  4.1712E+00  3.1235E+00  1.2294E+01  9.1146E-01  3.4938E+00
 EPSSHRINKSD(%)  1.5721E+01  7.2546E+00
 EPSSHRINKVR(%)  2.8970E+01  1.3983E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -4710.51279864924     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1828.72155851938     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:    52.19
 Elapsed covariance  time in seconds:     0.23
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -4710.513       **************************************************
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
+        4.71E-02 -1.52E-02  8.69E-02
 
 ETA4
+        3.18E-02  5.11E-02 -2.08E-02  2.24E-01
 
 ETA5
+        2.73E-02  2.64E-02 -2.32E-03 -3.34E-02  1.69E-01
 
 ETA6
+       -2.74E-02  8.22E-03  2.64E-02  1.84E-02 -8.02E-02  1.90E-01
 
 ETA7
+        2.97E-02 -3.96E-02  3.17E-02 -7.25E-02  2.44E-02  4.85E-03  2.13E-01
 
 ETA8
+        9.81E-02  8.20E-02  3.57E-02  4.41E-02  9.85E-04 -5.14E-02  5.57E-02  1.99E-01
 


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
+       -1.89E-01  4.03E-01
 
 ETA3
+        3.21E-01 -1.28E-01  2.95E-01
 
 ETA4
+        1.35E-01  2.68E-01 -1.49E-01  4.73E-01
 
 ETA5
+        1.33E-01  1.59E-01 -1.91E-02 -1.72E-01  4.12E-01
 
 ETA6
+       -1.26E-01  4.67E-02  2.05E-01  8.92E-02 -4.47E-01  4.36E-01
 
 ETA7
+        1.29E-01 -2.12E-01  2.33E-01 -3.32E-01  1.28E-01  2.41E-02  4.62E-01
 
 ETA8
+        4.42E-01  4.56E-01  2.72E-01  2.09E-01  5.37E-03 -2.64E-01  2.70E-01  4.46E-01
 


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
 
         3.19E-01  2.38E-01  2.27E-01  2.96E-01  2.05E-01  2.63E-01  1.50E-01  3.74E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.51E-01
 
 ETA2
+        2.63E-01  3.85E-01
 
 ETA3
+        1.10E-01  1.56E-01  1.93E-01
 
 ETA4
+        9.06E-02  1.54E-01  1.00E-01  1.71E-01
 
 ETA5
+        1.48E-01  9.39E-02  8.81E-02  1.58E-01  3.04E-01
 
 ETA6
+        1.14E-01  1.57E-01  1.38E-01  1.62E-01  7.80E-02  1.51E-01
 
 ETA7
+        1.83E-01  1.24E-01  1.15E-01  9.59E-02  1.17E-01  1.04E-01  1.51E-01
 
 ETA8
+        1.99E-01  1.40E-01  6.45E-02  9.48E-02  1.57E-01  1.94E-01  1.32E-01  2.40E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        3.23E-03
 
 EPS2
+        0.00E+00  4.48E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.51E-01
 
 ETA2
+        1.12E+00  4.77E-01
 
 ETA3
+        7.03E-01  1.12E+00  3.27E-01
 
 ETA4
+        3.75E-01  6.59E-01  8.12E-01  1.81E-01
 
 ETA5
+        7.47E-01  5.93E-01  7.27E-01  8.95E-01  3.69E-01
 
 ETA6
+        5.38E-01  8.88E-01  1.05E+00  7.73E-01  7.33E-01  1.73E-01
 
 ETA7
+        7.83E-01  5.37E-01  9.07E-01  4.14E-01  6.98E-01  5.18E-01  1.64E-01
 
 ETA8
+        6.23E-01  8.71E-01  3.37E-01  3.85E-01  8.59E-01  8.93E-01  5.82E-01  2.69E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        1.68E-02
 
 EPS2
+       .........  1.49E-02
 
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
+        1.02E-01
 
 TH 2
+       -1.90E-02  5.67E-02
 
 TH 3
+        1.90E-02 -7.01E-04  5.16E-02
 
 TH 4
+        6.47E-02 -2.23E-02 -1.67E-02  8.77E-02
 
 TH 5
+       -4.43E-02  2.38E-02  5.84E-03 -4.36E-02  4.19E-02
 
 TH 6
+       -4.78E-02  6.00E-03 -1.82E-02 -2.23E-02  2.31E-02  6.90E-02
 
 TH 7
+       -1.33E-02 -5.87E-03  9.56E-03 -2.02E-02  1.07E-02  9.99E-03  2.25E-02
 
 TH 8
+        8.67E-02 -3.79E-02 -1.06E-03  8.62E-02 -5.28E-02 -3.56E-02 -9.09E-03  1.40E-01
 
 OM11
+       -2.32E-02 -2.20E-03 -2.09E-02 -6.74E-04  7.60E-03  1.51E-02 -1.75E-04 -6.91E-03  2.28E-02
 
 OM12
+       -1.51E-02  4.13E-02  1.72E-02 -3.49E-02  2.47E-02  3.20E-03 -2.66E-03 -6.43E-02 -1.28E-02  6.90E-02
 
 OM13
+       -1.17E-02 -1.24E-02 -7.77E-04 -4.96E-03  2.37E-03  6.38E-03  4.11E-03  2.70E-03  8.52E-03 -1.78E-02  1.21E-02
 
 OM14
+       -8.05E-03 -1.89E-03  5.35E-03 -1.34E-02  4.58E-03 -5.24E-03  6.73E-03 -1.12E-02 -7.29E-04 -5.05E-04  2.29E-03  8.20E-03
 
 OM15
+        1.04E-02 -1.01E-02 -8.40E-03  1.84E-02 -7.55E-03  1.29E-02 -6.03E-03  1.37E-02  3.38E-03 -5.62E-03  6.40E-04 -9.52E-03
          2.20E-02
 
 OM16
+       -1.26E-02  1.60E-02  3.07E-03 -1.67E-02  1.19E-02 -1.46E-06  1.51E-03 -1.42E-02  4.83E-04  1.23E-02 -1.49E-03  2.55E-03
         -1.04E-02  1.31E-02
 
 OM17
+        5.76E-03 -1.13E-02 -2.71E-02  3.15E-02 -1.08E-02  6.15E-03 -7.71E-03  3.64E-02  1.78E-02 -3.10E-02  6.29E-03 -7.04E-03
          1.08E-02 -5.82E-03  3.35E-02
 
 OM18
+       -1.96E-02  5.85E-03 -3.45E-02  1.14E-02  5.52E-03  2.65E-02 -6.20E-03 -1.45E-03  2.21E-02 -5.82E-03  1.04E-03 -7.63E-03
          1.11E-02 -1.99E-03  2.68E-02  3.97E-02
 
 OM22
+       -2.34E-02 -5.30E-02 -5.31E-02  2.63E-02 -2.16E-02  3.37E-02  1.68E-03  4.65E-02  3.07E-02 -8.30E-02  2.54E-02 -3.43E-03
          1.83E-02 -1.77E-02  4.54E-02  3.15E-02  1.48E-01
 
 OM23
+        6.72E-03  1.47E-02  2.21E-02 -1.75E-02  8.42E-03 -1.32E-02  4.48E-03 -2.16E-02 -1.40E-02  2.77E-02 -9.94E-03  5.02E-03
         -1.08E-02  5.87E-03 -2.13E-02 -1.71E-02 -5.05E-02  2.44E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -1.06E-02 -1.25E-02 -1.76E-02  9.05E-03 -5.20E-03  1.76E-02 -5.67E-03  3.66E-03  8.64E-03 -8.74E-03  3.38E-03 -7.90E-03
          1.62E-02 -7.10E-03  1.23E-02  1.63E-02  3.57E-02 -1.64E-02  2.39E-02
 
 OM25
+        5.36E-03  1.05E-03  1.04E-03 -1.46E-03 -1.21E-03 -4.71E-03  3.11E-03  2.35E-03 -9.49E-04 -2.69E-03 -4.35E-04  3.01E-03
         -5.46E-03  2.20E-03 -1.84E-03 -5.72E-03 -2.69E-03  4.31E-03 -8.45E-03  8.82E-03
 
 OM26
+       -1.55E-02  2.01E-02 -1.52E-02 -7.15E-03  1.28E-02  2.27E-02 -3.48E-03 -1.94E-02  7.76E-03  1.73E-02 -5.53E-03 -6.70E-03
          5.72E-03  5.87E-03  3.95E-03  1.86E-02 -5.62E-03 -8.25E-04  7.35E-03 -2.52E-03  2.45E-02
 
 OM27
+       -8.86E-03  2.17E-02  4.24E-03 -1.60E-02  1.42E-02 -1.64E-03  1.08E-03 -2.63E-02 -7.50E-04  2.35E-02 -6.11E-03  1.90E-03
         -8.15E-03  8.82E-03 -9.24E-03 -4.11E-04 -3.44E-02  1.24E-02 -1.02E-02  2.90E-03  8.59E-03  1.54E-02
 
 OM28
+       -3.08E-02  1.32E-02 -9.92E-03 -2.34E-02  1.50E-02  1.31E-02  6.59E-04 -4.29E-02  7.50E-03  1.99E-02 -1.14E-03  9.80E-04
         -1.45E-03  5.52E-03 -6.27E-03  7.20E-03 -5.30E-03  1.59E-03  5.01E-03 -7.19E-04  1.04E-02  8.33E-03  1.95E-02
 
 OM33
+        3.75E-02 -2.89E-02 -8.97E-03  4.01E-02 -3.03E-02 -1.35E-02 -6.20E-03  6.35E-02 -2.24E-04 -4.05E-02  5.03E-03 -5.06E-03
          1.08E-02 -1.17E-02  2.07E-02  1.44E-03  4.43E-02 -1.62E-02  8.17E-03  4.52E-04 -1.11E-02 -1.81E-02 -1.92E-02  3.73E-02
 
 OM34
+        8.68E-03 -1.23E-02  5.74E-03  7.57E-03 -6.42E-03 -1.25E-02  4.33E-03  1.81E-02 -1.13E-03 -1.52E-02  4.17E-03  4.09E-03
         -4.81E-03 -1.71E-03  2.32E-03 -7.97E-03  7.50E-03 -4.72E-04 -5.59E-03  3.55E-03 -1.15E-02 -4.04E-03 -7.48E-03  8.24E-03
         1.00E-02
 
 OM35
+        2.22E-03  3.07E-03 -9.26E-03  6.24E-03 -2.22E-03  9.86E-03 -4.55E-03 -1.78E-04  1.58E-03  5.11E-04 -2.28E-03 -4.04E-03
          7.84E-03 -3.61E-03  4.79E-03  9.09E-03  7.31E-03 -3.75E-03  5.65E-03 -2.46E-03  6.41E-03 -1.68E-03  5.47E-04  2.50E-03
        -5.83E-03  7.77E-03
 
 OM36
+        1.69E-02  3.63E-03 -1.13E-02  1.61E-02 -1.31E-02 -1.71E-02 -1.18E-02  1.56E-02 -2.59E-04  1.26E-03 -6.53E-03 -4.64E-03
          2.44E-03  1.82E-03  7.05E-03  4.10E-03 -2.42E-03 -1.13E-03  3.74E-03  2.34E-04  4.81E-03  1.37E-03  6.75E-04  8.04E-03
        -1.69E-03  1.05E-03  1.89E-02
 
 OM37
+       -1.62E-02  7.68E-03 -1.61E-02 -3.61E-03  4.02E-03  1.34E-02 -5.59E-03 -1.45E-02  9.55E-03  4.41E-03  9.96E-04 -4.23E-03
          6.58E-03  1.29E-03  6.10E-03  1.42E-02  1.16E-02 -8.79E-03  1.07E-02 -4.39E-03  1.13E-02  8.83E-04  9.80E-03 -3.89E-03
        -8.31E-03  5.50E-03  4.17E-03  1.33E-02
 
 OM38
+        8.01E-03 -9.41E-03 -2.03E-03  9.03E-03 -6.87E-03  7.26E-05 -8.78E-04  1.51E-02  1.56E-03 -1.24E-02  3.46E-03 -1.42E-03
          4.41E-03 -3.55E-03  6.19E-03  1.30E-03  1.45E-02 -5.62E-03  2.85E-03 -2.50E-04 -2.38E-03 -5.12E-03 -4.94E-03  1.01E-02
         1.64E-03  1.52E-03  3.13E-04  2.99E-04  4.17E-03
 
 OM44
+       -2.55E-02 -1.15E-02  1.42E-02 -2.49E-02  1.12E-02  3.24E-03  8.49E-03 -2.13E-02  2.69E-03 -1.49E-03  1.07E-02  5.33E-03
         -1.79E-03 -8.21E-04 -8.04E-03 -8.77E-03  6.22E-03 -1.31E-03  4.82E-03 -3.94E-03 -9.69E-03 -3.45E-03  5.53E-03 -7.77E-03
         3.27E-03 -6.89E-03 -9.94E-03 -8.43E-04 -1.02E-03  2.92E-02
 
 OM45
+       -1.41E-02  2.29E-02 -6.04E-03 -1.35E-02  1.41E-02  1.95E-02 -2.15E-03 -3.38E-02  1.21E-03  2.92E-02 -7.38E-03 -3.21E-03
          3.32E-03  5.02E-03 -7.82E-03  9.40E-03 -2.42E-02  5.60E-03  1.26E-03 -1.58E-03  1.91E-02  1.09E-02  1.21E-02 -1.88E-02
        -1.27E-02  6.50E-03 -2.84E-04  9.72E-03 -3.84E-03 -8.47E-03  2.49E-02
 
 OM46
+       -1.95E-02 -1.17E-02 -4.87E-03 -9.87E-03  6.13E-03  1.67E-02  1.23E-02  4.94E-03  9.47E-03 -2.49E-02  1.21E-02  4.31E-03
         -5.05E-03  2.33E-03  5.83E-03  7.00E-04  3.72E-02 -9.95E-03 -1.72E-04  5.06E-03 -2.80E-03 -5.41E-03 -1.36E-03  5.03E-03
         6.22E-03 -3.58E-03 -1.07E-02 -1.74E-03  3.05E-03  6.67E-03 -7.97E-03  2.63E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -9.81E-03  8.20E-03 -1.82E-03 -5.26E-03  8.93E-03  9.17E-03  5.90E-03 -6.45E-03  4.75E-03  1.25E-03  1.14E-03  1.92E-03
         -4.90E-03  4.51E-03  9.08E-04  4.55E-03 -2.87E-03  1.25E-04 -4.99E-03  1.85E-03  4.17E-03  4.97E-03  2.29E-03 -7.02E-03
        -1.02E-04 -5.38E-04 -5.10E-03  6.30E-04 -1.24E-03 -3.09E-03  4.72E-03  6.54E-03  9.20E-03
 
 OM48
+       -1.80E-02 -3.07E-03 -1.30E-02 -3.10E-03  3.31E-03  1.07E-02  1.49E-03 -1.01E-02  8.36E-03 -6.03E-03  3.70E-03  6.90E-04
          2.71E-03 -2.62E-03  6.12E-03  1.04E-02  2.10E-02 -8.15E-03  8.41E-03 -3.03E-03  2.44E-03 -2.24E-03  5.87E-03 -1.37E-03
        -1.26E-03  1.82E-03 -1.66E-03  5.39E-03 -4.69E-05  4.96E-03  5.70E-04  3.39E-03  1.34E-03  8.99E-03
 
 OM55
+       -4.79E-02  3.57E-02  3.23E-03 -5.48E-02  3.66E-02  1.56E-02  6.78E-03 -9.24E-02  6.83E-03  5.94E-02 -6.96E-03  3.48E-03
         -7.39E-03  1.51E-02 -2.60E-02  1.48E-03 -5.63E-02  1.96E-02 -3.43E-03  2.49E-03  2.10E-02  2.69E-02  3.61E-02 -4.93E-02
        -1.35E-02 -4.16E-03 -4.34E-04  1.18E-02 -1.34E-02  1.23E-02  2.79E-02 -9.63E-03  6.04E-03  3.19E-03  9.24E-02
 
 OM56
+       -3.61E-03  9.63E-03 -8.32E-03 -2.27E-03  1.45E-03  2.56E-03 -5.43E-03 -8.71E-03  1.57E-03  7.46E-03 -3.65E-03 -2.12E-03
          3.33E-04  2.85E-03  1.34E-03  6.12E-03 -3.41E-03  8.30E-05  1.85E-03 -5.45E-06  7.33E-03  4.06E-03  4.87E-03 -3.71E-03
        -4.99E-03  2.91E-03  5.62E-03  5.09E-03 -1.17E-03 -5.48E-03  6.60E-03 -4.58E-03 -1.95E-04  7.92E-04  7.78E-03  6.08E-03
 
 OM57
+        1.96E-03 -7.52E-03 -1.84E-02  1.63E-02 -5.70E-03  4.91E-03 -3.86E-03  1.99E-02  1.02E-02 -2.07E-02  4.77E-03 -3.29E-03
          5.61E-03 -3.41E-03  1.79E-02  1.41E-02  3.20E-02 -1.44E-02  6.61E-03  2.04E-04  2.05E-03 -5.27E-03 -3.32E-03  1.32E-02
         1.16E-03  3.34E-03  2.90E-03  4.35E-03  4.49E-03 -5.44E-03 -3.85E-03  6.83E-03  7.83E-04  4.17E-03 -1.63E-02  1.36E-03
          1.37E-02
 
 OM58
+        2.29E-02 -1.76E-02 -1.57E-02  3.35E-02 -1.89E-02 -1.96E-03 -5.01E-03  4.41E-02  6.44E-03 -3.15E-02  4.52E-03 -4.99E-03
          1.14E-02 -9.09E-03  2.19E-02  9.40E-03  3.87E-02 -1.56E-02  6.78E-03  3.30E-03 -3.99E-03 -1.17E-02 -1.10E-02  2.56E-02
         5.37E-03  3.79E-03  5.23E-03 -9.02E-05  7.46E-03 -9.07E-03 -1.01E-02  6.32E-03 -2.01E-03  1.14E-03 -3.15E-02 -1.74E-03
          1.40E-02  2.46E-02
 
 OM66
+       -2.69E-02  9.32E-03 -6.98E-03 -2.07E-02  1.44E-02  2.51E-02  4.20E-03 -2.67E-02  3.38E-03  8.50E-03  5.48E-04 -6.10E-05
          2.27E-03  2.20E-03 -4.50E-03  8.25E-03  7.87E-03 -1.29E-03  6.36E-03 -5.23E-03  1.16E-02  1.10E-03  7.94E-03 -9.98E-03
        -8.83E-03  6.08E-03 -6.38E-03  8.43E-03 -9.18E-04  1.41E-03  1.24E-02  4.43E-03  3.05E-03  4.73E-03  7.91E-03  1.23E-03
         -1.44E-03 -7.45E-03  2.28E-02
 
 OM67
+        1.09E-02 -1.77E-02  2.70E-03  8.46E-03 -1.28E-02 -7.52E-03 -3.57E-04  1.56E-02 -4.27E-03 -1.18E-02  2.19E-03  1.51E-04
          3.70E-03 -6.40E-03  8.18E-05 -7.16E-03  1.49E-02 -2.86E-03  4.65E-03 -1.03E-03 -8.85E-03 -9.15E-03 -5.48E-03  1.17E-02
         4.02E-03 -1.05E-03  1.83E-03 -3.35E-03  2.59E-03  4.36E-03 -9.22E-03 -1.18E-04 -6.56E-03 -2.45E-04 -1.44E-02 -3.36E-03
         -5.34E-04  4.91E-03 -2.92E-03  1.08E-02
 
 OM68
+        5.13E-02 -8.00E-03  3.66E-03  4.04E-02 -2.67E-02 -2.62E-02 -1.15E-02  5.72E-02 -9.87E-03 -1.07E-02 -7.46E-03 -8.05E-03
          6.81E-03 -3.09E-03  1.00E-02 -4.51E-03 -5.78E-03 -2.44E-04  1.25E-04  5.27E-04 -2.49E-03 -6.25E-03 -1.69E-02  2.44E-02
         4.30E-03  6.95E-04  1.67E-02 -6.14E-03  4.29E-03 -1.57E-02 -9.42E-03 -1.08E-02 -7.14E-03 -9.66E-03 -3.02E-02 -3.48E-04
          3.02E-03  1.43E-02 -1.49E-02  7.10E-03  3.75E-02
 
 OM77
+       -5.14E-03  1.33E-02  2.07E-03 -5.35E-03  5.72E-03 -4.70E-03 -7.74E-03 -2.65E-02 -2.73E-03  2.54E-02 -7.93E-03 -1.31E-03
          1.30E-03 -6.77E-04 -7.04E-03  2.04E-03 -2.87E-02  9.31E-03  8.88E-04 -2.86E-03  3.57E-03  6.49E-03  8.62E-03 -1.40E-02
        -5.50E-03  1.39E-03  3.51E-03  2.94E-03 -5.36E-03  2.12E-03  7.77E-03 -1.69E-02 -4.63E-03  5.26E-04  2.21E-02  3.57E-03
         -6.96E-03 -9.62E-03  6.82E-05 -2.08E-03 -3.90E-03  2.29E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.50E-02  4.60E-03 -1.05E-02  2.45E-02 -5.65E-03  1.88E-03 -9.37E-03  1.86E-02  4.63E-03 -3.73E-04 -4.86E-03 -7.56E-03
          8.73E-03 -3.00E-03  1.52E-02  1.70E-02 -8.93E-04 -4.55E-03  4.85E-03 -2.67E-03  7.88E-03  4.26E-04 -3.82E-03  6.54E-03
        -3.50E-03  5.82E-03  6.37E-03  3.70E-03  1.82E-03 -1.24E-02  4.02E-03 -8.32E-03  9.50E-04 -3.02E-04 -9.04E-03  3.03E-03
          6.50E-03  8.93E-03 -2.50E-03 -4.12E-03  1.10E-02  4.54E-03  1.75E-02
 
 OM88
+       -4.07E-02  2.82E-02 -2.76E-02 -1.54E-02  2.60E-02  3.67E-02 -3.42E-03 -4.78E-02  1.98E-02  2.64E-02 -4.63E-03 -5.61E-03
          6.39E-03  5.84E-03  9.77E-03  3.68E-02 -4.83E-03 -3.81E-03  1.05E-02 -5.26E-03  2.92E-02  1.41E-02  2.19E-02 -2.47E-02
        -1.66E-02  9.81E-03 -6.46E-05  1.90E-02 -4.55E-03 -6.82E-03  2.74E-02 -5.25E-03  9.32E-03  1.02E-02  3.81E-02  1.03E-02
          4.89E-03 -7.82E-03  1.74E-02 -1.66E-02 -2.09E-02  1.25E-02  1.41E-02  5.77E-02
 
 SG11
+        7.77E-04 -4.15E-04 -4.96E-05  8.12E-04 -5.10E-04 -3.14E-04 -1.56E-04  1.03E-03 -6.71E-05 -4.59E-04  2.84E-06 -1.19E-04
          1.96E-04 -1.78E-04  2.70E-04 -1.99E-05  3.52E-04 -1.99E-04  9.46E-05  7.30E-06 -1.75E-04 -2.25E-04 -3.00E-04  5.18E-04
         1.21E-04  2.43E-05  1.52E-04 -8.44E-05  1.31E-04 -1.78E-04 -2.34E-04 -3.10E-05 -9.46E-05 -7.05E-05 -6.52E-04 -5.92E-05
          1.71E-04  3.73E-04 -2.64E-04  1.46E-04  4.59E-04 -1.61E-04  1.65E-04 -3.65E-04  1.04E-05
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -1.85E-06 -7.04E-04  1.97E-05  9.38E-05 -2.24E-04  3.03E-05  1.86E-04  6.61E-04  6.08E-05 -9.32E-04  3.41E-04  9.31E-05
          2.57E-05 -2.03E-04  2.27E-04 -1.06E-04  1.14E-03 -3.39E-04  1.09E-04 -2.01E-05 -4.02E-04 -3.86E-04 -3.08E-04  4.99E-04
         2.37E-04 -5.48E-05 -2.29E-04 -1.30E-04  1.78E-04  2.64E-04 -4.81E-04  4.22E-04 -4.50E-05  8.91E-05 -8.79E-04 -1.77E-04
          1.99E-04  2.94E-04 -1.42E-05  2.14E-04 -6.08E-05 -3.71E-04 -1.58E-04 -5.34E-04  4.06E-06  0.00E+00  2.00E-05
 
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
+        3.19E-01
 
 TH 2
+       -2.49E-01  2.38E-01
 
 TH 3
+        2.61E-01 -1.30E-02  2.27E-01
 
 TH 4
+        6.84E-01 -3.17E-01 -2.47E-01  2.96E-01
 
 TH 5
+       -6.77E-01  4.89E-01  1.26E-01 -7.19E-01  2.05E-01
 
 TH 6
+       -5.70E-01  9.59E-02 -3.05E-01 -2.87E-01  4.30E-01  2.63E-01
 
 TH 7
+       -2.78E-01 -1.64E-01  2.81E-01 -4.54E-01  3.50E-01  2.53E-01  1.50E-01
 
 TH 8
+        7.26E-01 -4.26E-01 -1.24E-02  7.79E-01 -6.91E-01 -3.63E-01 -1.62E-01  3.74E-01
 
 OM11
+       -4.81E-01 -6.11E-02 -6.10E-01 -1.51E-02  2.46E-01  3.82E-01 -7.73E-03 -1.22E-01  1.51E-01
 
 OM12
+       -1.80E-01  6.61E-01  2.89E-01 -4.49E-01  4.60E-01  4.63E-02 -6.74E-02 -6.55E-01 -3.22E-01  2.63E-01
 
 OM13
+       -3.32E-01 -4.74E-01 -3.11E-02 -1.52E-01  1.05E-01  2.21E-01  2.50E-01  6.59E-02  5.13E-01 -6.18E-01  1.10E-01
 
 OM14
+       -2.78E-01 -8.75E-02  2.60E-01 -5.00E-01  2.47E-01 -2.20E-01  4.95E-01 -3.30E-01 -5.33E-02 -2.12E-02  2.30E-01  9.06E-02
 
 OM15
+        2.19E-01 -2.86E-01 -2.49E-01  4.20E-01 -2.49E-01  3.31E-01 -2.71E-01  2.48E-01  1.51E-01 -1.44E-01  3.93E-02 -7.09E-01
          1.48E-01
 
 OM16
+       -3.46E-01  5.90E-01  1.18E-01 -4.93E-01  5.10E-01 -4.85E-05  8.79E-02 -3.32E-01  2.80E-02  4.09E-01 -1.19E-01  2.47E-01
         -6.14E-01  1.14E-01
 
 OM17
+        9.85E-02 -2.60E-01 -6.52E-01  5.81E-01 -2.88E-01  1.28E-01 -2.81E-01  5.33E-01  6.45E-01 -6.44E-01  3.13E-01 -4.25E-01
          3.99E-01 -2.78E-01  1.83E-01
 
 OM18
+       -3.08E-01  1.23E-01 -7.61E-01  1.94E-01  1.35E-01  5.06E-01 -2.07E-01 -1.95E-02  7.33E-01 -1.11E-01  4.73E-02 -4.22E-01
          3.74E-01 -8.72E-02  7.34E-01  1.99E-01
 
 OM22
+       -1.91E-01 -5.79E-01 -6.07E-01  2.30E-01 -2.74E-01  3.33E-01  2.92E-02  3.24E-01  5.28E-01 -8.21E-01  6.02E-01 -9.84E-02
          3.20E-01 -4.02E-01  6.45E-01  4.10E-01  3.85E-01
 
 OM23
+        1.35E-01  3.97E-01  6.24E-01 -3.80E-01  2.63E-01 -3.22E-01  1.91E-01 -3.70E-01 -5.95E-01  6.75E-01 -5.80E-01  3.55E-01
         -4.67E-01  3.29E-01 -7.46E-01 -5.50E-01 -8.42E-01  1.56E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -2.14E-01 -3.41E-01 -5.03E-01  1.98E-01 -1.64E-01  4.34E-01 -2.45E-01  6.35E-02  3.70E-01 -2.16E-01  1.99E-01 -5.64E-01
          7.06E-01 -4.02E-01  4.36E-01  5.30E-01  6.00E-01 -6.81E-01  1.54E-01
 
 OM25
+        1.79E-01  4.68E-02  4.87E-02 -5.26E-02 -6.28E-02 -1.91E-01  2.20E-01  6.70E-02 -6.69E-02 -1.09E-01 -4.21E-02  3.53E-01
         -3.92E-01  2.05E-01 -1.07E-01 -3.05E-01 -7.44E-02  2.94E-01 -5.82E-01  9.39E-02
 
 OM26
+       -3.09E-01  5.39E-01 -4.28E-01 -1.54E-01  3.98E-01  5.52E-01 -1.48E-01 -3.31E-01  3.28E-01  4.20E-01 -3.22E-01 -4.72E-01
          2.46E-01  3.28E-01  1.38E-01  5.97E-01 -9.33E-02 -3.38E-02  3.04E-01 -1.71E-01  1.57E-01
 
 OM27
+       -2.23E-01  7.34E-01  1.50E-01 -4.36E-01  5.57E-01 -5.02E-02  5.81E-02 -5.66E-01 -4.00E-02  7.19E-01 -4.47E-01  1.68E-01
         -4.43E-01  6.21E-01 -4.06E-01 -1.66E-02 -7.19E-01  6.39E-01 -5.32E-01  2.48E-01  4.41E-01  1.24E-01
 
 OM28
+       -6.90E-01  3.98E-01 -3.12E-01 -5.65E-01  5.26E-01  3.56E-01  3.14E-02 -8.22E-01  3.55E-01  5.43E-01 -7.43E-02  7.74E-02
         -7.01E-02  3.45E-01 -2.45E-01  2.58E-01 -9.86E-02  7.31E-02  2.32E-01 -5.48E-02  4.78E-01  4.80E-01  1.40E-01
 
 OM33
+        6.09E-01 -6.30E-01 -2.04E-01  7.02E-01 -7.67E-01 -2.67E-01 -2.14E-01  8.80E-01 -7.67E-03 -7.99E-01  2.37E-01 -2.89E-01
          3.77E-01 -5.31E-01  5.86E-01  3.75E-02  5.97E-01 -5.38E-01  2.74E-01  2.49E-02 -3.66E-01 -7.53E-01 -7.11E-01  1.93E-01
 
 OM34
+        2.71E-01 -5.17E-01  2.52E-01  2.55E-01 -3.13E-01 -4.76E-01  2.88E-01  4.83E-01 -7.49E-02 -5.78E-01  3.79E-01  4.51E-01
         -3.24E-01 -1.49E-01  1.26E-01 -3.99E-01  1.95E-01 -3.02E-02 -3.61E-01  3.77E-01 -7.34E-01 -3.25E-01 -5.34E-01  4.26E-01
         1.00E-01
 
 OM35
+        7.90E-02  1.46E-01 -4.62E-01  2.39E-01 -1.23E-01  4.26E-01 -3.44E-01 -5.42E-03  1.19E-01  2.21E-02 -2.36E-01 -5.06E-01
          6.00E-01 -3.58E-01  2.97E-01  5.17E-01  2.15E-01 -2.73E-01  4.15E-01 -2.98E-01  4.65E-01 -1.53E-01  4.44E-02  1.47E-01
        -6.61E-01  8.81E-02
 
 OM36
+        3.86E-01  1.11E-01 -3.61E-01  3.96E-01 -4.67E-01 -4.73E-01 -5.70E-01  3.03E-01 -1.24E-02  3.48E-02 -4.32E-01 -3.72E-01
          1.20E-01  1.16E-01  2.80E-01  1.49E-01 -4.58E-02 -5.27E-02  1.76E-01  1.81E-02  2.23E-01  8.01E-02  3.51E-02  3.03E-01
        -1.23E-01  8.63E-02  1.38E-01
 
 OM37
+       -4.40E-01  2.80E-01 -6.13E-01 -1.06E-01  1.71E-01  4.44E-01 -3.24E-01 -3.38E-01  5.49E-01  1.46E-01  7.86E-02 -4.05E-01
          3.85E-01  9.78E-02  2.89E-01  6.20E-01  2.62E-01 -4.89E-01  6.01E-01 -4.06E-01  6.25E-01  6.17E-02  6.09E-01 -1.75E-01
        -7.20E-01  5.41E-01  2.63E-01  1.15E-01
 
 OM38
+        3.89E-01 -6.13E-01 -1.38E-01  4.73E-01 -5.20E-01  4.28E-03 -9.07E-02  6.27E-01  1.60E-01 -7.29E-01  4.88E-01 -2.42E-01
          4.60E-01 -4.81E-01  5.24E-01  1.01E-01  5.84E-01 -5.58E-01  2.86E-01 -4.13E-02 -2.35E-01 -6.39E-01 -5.48E-01  8.12E-01
         2.53E-01  2.67E-01  3.53E-02  4.02E-02  6.45E-02
 
 OM44
+       -4.68E-01 -2.82E-01  3.66E-01 -4.92E-01  3.20E-01  7.21E-02  3.31E-01 -3.34E-01  1.04E-01 -3.31E-02  5.69E-01  3.45E-01
         -7.05E-02 -4.21E-02 -2.57E-01 -2.58E-01  9.47E-02 -4.92E-02  1.83E-01 -2.46E-01 -3.62E-01 -1.62E-01  2.32E-01 -2.36E-01
         1.91E-01 -4.58E-01 -4.23E-01 -4.28E-02 -9.24E-02  1.71E-01
 
 OM45
+       -2.81E-01  6.09E-01 -1.69E-01 -2.90E-01  4.36E-01  4.72E-01 -9.08E-02 -5.74E-01  5.09E-02  7.04E-01 -4.26E-01 -2.25E-01
          1.42E-01  2.78E-01 -2.71E-01  2.99E-01 -3.99E-01  2.27E-01  5.16E-02 -1.07E-01  7.72E-01  5.57E-01  5.50E-01 -6.18E-01
        -8.05E-01  4.67E-01 -1.31E-02  5.35E-01 -3.78E-01 -3.14E-01  1.58E-01
 
 OM46
+       -3.77E-01 -3.04E-01 -1.32E-01 -2.06E-01  1.85E-01  3.91E-01  5.05E-01  8.16E-02  3.87E-01 -5.84E-01  6.76E-01  2.94E-01
         -2.10E-01  1.26E-01  1.97E-01  2.16E-02  5.96E-01 -3.93E-01 -6.86E-03  3.32E-01 -1.10E-01 -2.68E-01 -6.02E-02  1.61E-01
         3.83E-01 -2.51E-01 -4.79E-01 -9.31E-02  2.91E-01  2.41E-01 -3.12E-01  1.62E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.20E-01  3.59E-01 -8.34E-02 -1.85E-01  4.55E-01  3.64E-01  4.10E-01 -1.80E-01  3.28E-01  4.97E-02  1.08E-01  2.22E-01
         -3.44E-01  4.11E-01  5.17E-02  2.38E-01 -7.78E-02  8.35E-03 -3.37E-01  2.06E-01  2.78E-01  4.17E-01  1.71E-01 -3.79E-01
        -1.06E-02 -6.37E-02 -3.87E-01  5.70E-02 -2.00E-01 -1.89E-01  3.12E-01  4.21E-01  9.59E-02
 
 OM48
+       -5.96E-01 -1.36E-01 -6.03E-01 -1.10E-01  1.71E-01  4.30E-01  1.05E-01 -2.84E-01  5.84E-01 -2.42E-01  3.55E-01  8.04E-02
          1.93E-01 -2.42E-01  3.53E-01  5.49E-01  5.76E-01 -5.50E-01  5.74E-01 -3.41E-01  1.64E-01 -1.90E-01  4.43E-01 -7.50E-02
        -1.33E-01  2.18E-01 -1.27E-01  4.93E-01 -7.67E-03  3.06E-01  3.81E-02  2.20E-01  1.47E-01  9.48E-02
 
 OM55
+       -4.94E-01  4.94E-01  4.68E-02 -6.08E-01  5.88E-01  1.95E-01  1.49E-01 -8.13E-01  1.49E-01  7.45E-01 -2.08E-01  1.26E-01
         -1.64E-01  4.34E-01 -4.67E-01  2.44E-02 -4.82E-01  4.14E-01 -7.31E-02  8.73E-02  4.41E-01  7.12E-01  8.50E-01 -8.41E-01
        -4.44E-01 -1.55E-01 -1.04E-02  3.37E-01 -6.82E-01  2.37E-01  5.82E-01 -1.95E-01  2.07E-01  1.11E-01  3.04E-01
 
 OM56
+       -1.45E-01  5.19E-01 -4.70E-01 -9.84E-02  9.11E-02  1.25E-01 -4.64E-01 -2.99E-01  1.33E-01  3.64E-01 -4.26E-01 -3.01E-01
          2.88E-02  3.19E-01  9.37E-02  3.94E-01 -1.14E-01  6.82E-03  1.53E-01 -7.43E-04  6.00E-01  4.19E-01  4.46E-01 -2.46E-01
        -6.38E-01  4.23E-01  5.23E-01  5.66E-01 -2.33E-01 -4.11E-01  5.37E-01 -3.62E-01 -2.61E-02  1.07E-01  3.28E-01  7.80E-02
 
 OM57
+        5.23E-02 -2.70E-01 -6.90E-01  4.71E-01 -2.38E-01  1.60E-01 -2.20E-01  4.54E-01  5.76E-01 -6.73E-01  3.71E-01 -3.10E-01
          3.23E-01 -2.55E-01  8.36E-01  6.03E-01  7.10E-01 -7.89E-01  3.65E-01  1.86E-02  1.12E-01 -3.62E-01 -2.03E-01  5.86E-01
         9.90E-02  3.24E-01  1.80E-01  3.22E-01  5.94E-01 -2.72E-01 -2.09E-01  3.60E-01  6.98E-02  3.76E-01 -4.59E-01  1.49E-01
          1.17E-01
 
 OM58
+        4.57E-01 -4.73E-01 -4.42E-01  7.21E-01 -5.90E-01 -4.75E-02 -2.13E-01  7.53E-01  2.72E-01 -7.65E-01  2.62E-01 -3.51E-01
          4.91E-01 -5.07E-01  7.62E-01  3.01E-01  6.41E-01 -6.37E-01  2.80E-01  2.24E-01 -1.62E-01 -6.03E-01 -5.02E-01  8.46E-01
         3.42E-01  2.75E-01  2.43E-01 -4.99E-03  7.37E-01 -3.39E-01 -4.07E-01  2.49E-01 -1.34E-01  7.69E-02 -6.60E-01 -1.42E-01
          7.63E-01  1.57E-01
 
 OM66
+       -5.58E-01  2.59E-01 -2.03E-01 -4.62E-01  4.67E-01  6.33E-01  1.85E-01 -4.73E-01  1.48E-01  2.14E-01  3.30E-02 -4.46E-03
          1.02E-01  1.28E-01 -1.63E-01  2.74E-01  1.35E-01 -5.48E-02  2.73E-01 -3.69E-01  4.92E-01  5.84E-02  3.76E-01 -3.42E-01
        -5.83E-01  4.57E-01 -3.07E-01  4.84E-01 -9.41E-02  5.47E-02  5.20E-01  1.81E-01  2.11E-01  3.30E-01  1.72E-01  1.05E-01
         -8.12E-02 -3.14E-01  1.51E-01
 
 OM67
+        3.29E-01 -7.19E-01  1.15E-01  2.76E-01 -6.03E-01 -2.76E-01 -2.29E-02  4.03E-01 -2.72E-01 -4.35E-01  1.92E-01  1.61E-02
          2.41E-01 -5.40E-01  4.31E-03 -3.46E-01  3.74E-01 -1.76E-01  2.90E-01 -1.05E-01 -5.45E-01 -7.10E-01 -3.78E-01  5.86E-01
         3.87E-01 -1.15E-01  1.28E-01 -2.80E-01  3.86E-01  2.46E-01 -5.64E-01 -7.02E-03 -6.59E-01 -2.49E-02 -4.58E-01 -4.15E-01
         -4.40E-02  3.02E-01 -1.86E-01  1.04E-01
 
 OM68
+        8.29E-01 -1.74E-01  8.32E-02  7.03E-01 -6.74E-01 -5.15E-01 -3.96E-01  7.90E-01 -3.37E-01 -2.10E-01 -3.50E-01 -4.59E-01
          2.37E-01 -1.39E-01  2.83E-01 -1.17E-01 -7.75E-02 -8.06E-03  4.16E-03  2.89E-02 -8.21E-02 -2.60E-01 -6.25E-01  6.53E-01
         2.22E-01  4.07E-02  6.25E-01 -2.75E-01  3.43E-01 -4.75E-01 -3.08E-01 -3.44E-01 -3.84E-01 -5.26E-01 -5.13E-01 -2.31E-02
          1.33E-01  4.72E-01 -5.10E-01  3.54E-01  1.94E-01
 
 OM77
+       -1.06E-01  3.68E-01  6.03E-02 -1.19E-01  1.84E-01 -1.18E-01 -3.41E-01 -4.69E-01 -1.19E-01  6.40E-01 -4.77E-01 -9.56E-02
          5.81E-02 -3.91E-02 -2.54E-01  6.77E-02 -4.92E-01  3.94E-01  3.80E-02 -2.01E-01  1.50E-01  3.45E-01  4.07E-01 -4.80E-01
        -3.62E-01  1.04E-01  1.69E-01  1.69E-01 -5.48E-01  8.19E-02  3.25E-01 -6.90E-01 -3.19E-01  3.66E-02  4.80E-01  3.02E-01
         -3.93E-01 -4.05E-01  2.98E-03 -1.32E-01 -1.33E-01  1.51E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.54E-01  1.46E-01 -3.49E-01  6.25E-01 -2.09E-01  5.42E-02 -4.73E-01  3.76E-01  2.32E-01 -1.08E-02 -3.35E-01 -6.32E-01
          4.46E-01 -1.99E-01  6.29E-01  6.46E-01 -1.76E-02 -2.20E-01  2.38E-01 -2.15E-01  3.81E-01  2.60E-02 -2.07E-01  2.56E-01
        -2.65E-01  5.00E-01  3.50E-01  2.43E-01  2.13E-01 -5.47E-01  1.93E-01 -3.88E-01  7.50E-02 -2.41E-02 -2.25E-01  2.94E-01
          4.20E-01  4.31E-01 -1.25E-01 -3.01E-01  4.31E-01  2.27E-01  1.32E-01
 
 OM88
+       -5.30E-01  4.93E-01 -5.07E-01 -2.16E-01  5.29E-01  5.82E-01 -9.49E-02 -5.32E-01  5.45E-01  4.19E-01 -1.76E-01 -2.58E-01
          1.80E-01  2.13E-01  2.22E-01  7.69E-01 -5.23E-02 -1.02E-01  2.84E-01 -2.33E-01  7.76E-01  4.71E-01  6.53E-01 -5.32E-01
        -6.90E-01  4.64E-01 -1.95E-03  6.88E-01 -2.94E-01 -1.66E-01  7.24E-01 -1.35E-01  4.05E-01  4.50E-01  5.23E-01  5.50E-01
          1.74E-01 -2.08E-01  4.79E-01 -6.67E-01 -4.48E-01  3.45E-01  4.43E-01  2.40E-01
 
 SG11
+        7.54E-01 -5.40E-01 -6.77E-02  8.49E-01 -7.72E-01 -3.71E-01 -3.23E-01  8.54E-01 -1.38E-01 -5.41E-01  8.02E-03 -4.08E-01
          4.09E-01 -4.82E-01  4.58E-01 -3.10E-02  2.84E-01 -3.94E-01  1.90E-01  2.41E-02 -3.47E-01 -5.60E-01 -6.65E-01  8.31E-01
         3.73E-01  8.55E-02  3.41E-01 -2.27E-01  6.30E-01 -3.22E-01 -4.60E-01 -5.91E-02 -3.06E-01 -2.30E-01 -6.65E-01 -2.35E-01
          4.53E-01  7.37E-01 -5.42E-01  4.37E-01  7.34E-01 -3.29E-01  3.87E-01 -4.71E-01  3.23E-03
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -1.29E-03 -6.61E-01  1.94E-02  7.07E-02 -2.45E-01  2.57E-02  2.77E-01  3.95E-01  9.00E-02 -7.93E-01  6.93E-01  2.30E-01
          3.87E-02 -3.97E-01  2.78E-01 -1.19E-01  6.65E-01 -4.85E-01  1.57E-01 -4.77E-02 -5.74E-01 -6.93E-01 -4.92E-01  5.77E-01
         5.29E-01 -1.39E-01 -3.72E-01 -2.51E-01  6.17E-01  3.45E-01 -6.82E-01  5.82E-01 -1.05E-01  2.10E-01 -6.46E-01 -5.07E-01
          3.80E-01  4.19E-01 -2.10E-02  4.62E-01 -7.01E-02 -5.47E-01 -2.66E-01 -4.97E-01  2.81E-01  0.00E+00  4.48E-03
 
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
+        3.15E+02
 
 TH 2
+        1.70E+02  4.35E+02
 
 TH 3
+       -5.24E+01  2.05E+01  5.36E+02
 
 TH 4
+       -3.17E+01 -1.78E+01  5.84E+01  2.84E+02
 
 TH 5
+       -8.66E+01 -1.25E+02 -8.10E+00  2.34E+01  3.64E+02
 
 TH 6
+       -2.45E+01 -6.73E+01 -7.75E+01 -5.39E+01  1.18E+02  2.84E+02
 
 TH 7
+        6.15E+01  1.62E+02 -1.34E+01  1.11E+02 -9.18E+01 -8.62E+01  3.72E+02
 
 TH 8
+       -2.10E+02 -2.87E+02 -1.05E+02 -9.54E+01  1.28E+02  1.49E+02 -2.32E+02  5.69E+02
 
 OM11
+        8.90E+01  4.18E+01 -1.00E+02  2.20E+01  2.20E+01  1.15E+02  1.17E+02 -4.98E+01  1.02E+03
 
 OM12
+        1.16E+02  8.86E+01  4.29E+01  4.35E+02  8.00E+01  4.57E+01  3.74E+02 -3.62E+02  9.59E+02  3.64E+03
 
 OM13
+       -2.21E+02 -3.45E+01 -2.74E+02  5.58E+01 -1.51E+02  1.26E+01  1.84E+02  1.99E+02 -9.01E+02 -8.29E+02  3.52E+03
 
 OM14
+        5.59E+01  4.85E+02  1.08E+02  8.68E+01 -1.04E+02  8.18E+01  1.31E+02 -1.65E+02 -2.92E+02  4.02E+02  3.16E+02  2.68E+03
 
 OM15
+        6.84E+01  1.45E+02 -1.82E+02 -9.70E+01 -4.24E+02 -3.67E+02 -1.81E+01 -1.03E+02 -7.55E+02 -1.84E+03  1.08E+03 -2.10E+02
          2.86E+03
 
 OM16
+        2.28E+02  5.40E+01 -2.45E+01  9.12E+01 -3.37E+02 -3.17E+02  1.28E+02 -2.39E+02  1.65E+02 -3.81E+02 -5.83E+02 -8.00E+02
          8.71E+02  1.83E+03
 
 OM17
+        1.79E+02  2.08E+02  1.88E+02  1.15E+02  1.46E+01  1.70E+02  1.33E+02 -2.34E+02  5.91E+02  2.13E+03 -1.48E+03  1.12E+03
         -1.72E+03 -4.29E+02  3.03E+03
 
 OM18
+       -1.06E+02 -3.00E+02  1.38E+02 -1.22E+02 -4.71E+01 -2.40E+02 -3.43E+02  2.71E+02 -1.20E+03 -3.07E+03  1.01E+03 -9.63E+02
          2.12E+03  6.30E+02 -2.62E+03  4.33E+03
 
 OM22
+        1.14E+02  2.02E+02  1.91E+01  2.66E+02 -5.85E+01 -3.51E+01  3.03E+02 -3.46E+02  4.11E+02  2.20E+03 -2.20E+02  2.13E+02
         -8.48E+02 -1.93E+02  1.10E+03 -1.85E+03  1.94E+03
 
 OM23
+        1.36E+01 -1.24E+02 -1.67E+02 -9.44E+01 -1.63E+02  1.76E+02 -2.09E+02  2.51E+02 -5.63E+02 -1.03E+03  1.77E+03 -4.13E+02
          8.51E+02 -6.03E+01 -8.53E+02  1.51E+03 -3.16E+02  3.56E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.88E+02  6.53E+02 -1.01E+02 -5.40E+01 -9.28E+01  1.14E+02  2.03E+02 -4.98E+02  2.53E+02  4.75E+02 -4.24E+02  1.98E+03
         -8.63E+02 -3.08E+02  1.55E+03 -1.23E+03 -4.71E+01 -3.66E+02  4.42E+03
 
 OM25
+        8.63E+01  3.95E+01 -1.96E+02 -6.21E+01 -2.01E+02 -4.24E+02 -1.09E+01 -1.80E+02 -6.87E+02 -1.93E+03  6.06E+02 -8.59E+02
          2.94E+03  1.00E+03 -2.03E+03  2.43E+03 -1.16E+03 -1.61E+02 -7.18E+02  5.90E+03
 
 OM26
+        6.26E+00 -4.50E+01  1.13E+02  1.25E+02 -3.51E+02 -4.27E+02  2.21E+02 -4.76E+01 -4.88E+01 -4.14E+02 -2.67E+02 -3.08E+02
          9.66E+02  1.24E+03 -6.34E+02  7.60E+02 -4.48E+02 -6.09E+02 -1.10E+03  1.78E+03  3.30E+03
 
 OM27
+        2.60E+02  2.17E+02 -1.97E+02  1.76E+02  8.13E+01  3.23E+02  2.73E+02 -2.14E+02  6.90E+02  2.21E+03 -6.32E+02  1.53E+03
         -1.96E+03 -6.36E+02  2.33E+03 -2.59E+03  1.32E+03 -9.66E+02  2.81E+03 -2.56E+03 -1.46E+03  4.53E+03
 
 OM28
+       -4.07E+02 -5.09E+02  1.35E+02 -4.11E+02 -8.59E+01 -6.95E+01 -4.54E+02  9.43E+02 -1.04E+03 -4.47E+03  1.05E+03 -1.16E+03
          2.47E+03  8.33E+02 -2.86E+03  4.48E+03 -3.18E+03  1.39E+03 -2.32E+03  2.81E+03  2.21E+03 -3.83E+03  8.55E+03
 
 OM33
+       -1.18E+02 -6.16E+01  5.80E+01  1.55E+02 -7.59E+00  1.04E+01 -1.53E+00 -1.29E+02  8.55E+00 -1.26E+01  3.12E+02  1.01E+02
          1.12E+02  4.32E+02  1.55E+02 -4.66E+02 -2.92E+02 -8.07E+01  2.45E+02 -1.23E+02  1.30E+02  3.73E+02  1.05E+02  2.86E+03
 
 OM34
+        1.17E+02 -9.22E+01  3.80E+01 -2.58E+02  7.59E+01  1.05E+02 -1.87E+02  1.50E+02  3.03E+00 -4.78E+02 -1.46E+02 -7.73E+02
          1.73E+02  2.28E+02 -3.85E+02  1.84E+02 -3.42E+02  2.00E+02 -1.68E+02  9.17E+02  2.07E+02 -3.75E+02  8.49E+02  6.45E+02
         2.93E+03
 
 OM35
+       -1.77E+02 -1.27E+02  6.12E+01  9.65E+01  2.49E+02  6.19E+01  9.78E+01  2.98E+02  6.40E+02  8.97E+02 -9.25E+02  5.74E+01
         -1.22E+03  3.63E+02  7.98E+02 -1.32E+03 -3.72E+01 -1.89E+03  7.37E+02 -5.50E+02  6.73E+02  1.23E+03 -3.07E+02  8.32E+02
         9.82E+02  4.13E+03
 
 OM36
+       -2.67E+01  1.62E+02  9.43E+01  6.30E+01  1.06E+02  3.23E+01  1.58E+02 -1.06E+02  4.64E+00  1.50E+02  2.36E+02  9.26E+01
          2.17E+02 -1.34E+02 -3.25E+02 -3.12E+01  2.62E+01 -6.48E+02  8.81E+01  4.43E+02  8.04E+01 -2.42E+02 -1.43E+02 -8.73E+01
        -2.19E+02  1.09E+03  1.79E+03
 
 OM37
+        1.76E+02 -2.65E+02 -2.43E+01 -1.58E+02  1.18E+02  1.79E+02 -2.56E+02  7.39E+01 -7.28E+02 -7.10E+02  1.03E+03 -3.57E+02
          7.54E+02 -4.15E+02 -6.83E+02  1.25E+03 -3.12E+02  2.48E+03 -2.77E+02  1.14E+03 -5.11E+02 -5.96E+02  5.43E+02 -6.13E+01
         1.34E+03 -1.84E+03 -9.20E+02  4.25E+03
 
 OM38
+        1.56E+02  2.60E+02 -1.67E+02  1.20E+02  3.25E+02 -1.28E+02  5.29E+01 -1.39E+02  9.33E+02  1.49E+03 -3.63E+03 -1.21E+02
         -1.36E+03  3.35E+02  1.37E+03 -8.28E+02  7.66E+02 -2.97E+03  6.51E+02 -2.30E+02  3.97E+02  8.19E+02 -1.91E+03 -2.56E+03
        -1.73E+03  8.75E+02  4.70E+02 -3.13E+03  9.61E+03
 
 OM44
+        6.18E+01 -1.63E+01 -1.82E+02  1.43E+02 -1.09E+02 -6.08E+01  1.17E+02 -1.08E+02  2.17E+02  5.16E+02 -3.18E+02 -3.19E+02
          2.13E+01  3.68E+02  9.43E+01 -2.69E+02  4.79E+02 -1.51E+02 -4.11E+02  2.09E+02  5.23E+02  9.10E+01 -3.50E+02  1.33E+02
         1.26E+02  3.00E+02  1.01E+01 -3.30E+02  3.51E+02  7.55E+02
 
 OM45
+       -1.08E+02 -8.93E+01  9.18E+01 -1.90E+02  1.27E+02  1.44E+02 -2.33E+02  2.75E+02 -1.61E+02 -9.23E+02  1.18E+02 -4.40E+02
         -6.32E+00 -8.99E+01 -1.91E+02  6.61E+02 -4.46E+02  7.89E+02 -2.96E+02 -6.07E+02 -9.97E+02 -2.80E+02  1.16E+03  2.98E+02
         5.05E+02  4.75E+01  6.44E+01  2.01E+02 -1.06E+03 -1.59E+02  1.59E+03
 
 OM46
+        6.98E+01  1.13E+02  1.35E+02 -1.16E+02  1.56E+02  3.76E+01 -1.42E+02  1.42E+01 -2.27E+02 -1.84E+02  1.26E+02  5.18E+02
         -1.87E+02 -5.94E+02  1.98E+02  5.04E+01 -3.68E+02  1.48E+02  6.42E+02 -1.13E+03 -1.25E+03  3.89E+02 -3.47E+02  3.75E+01
        -2.38E+02  3.06E+01  3.85E+02  1.94E+02 -4.50E+02 -5.88E+02  6.48E+02  1.73E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.45E+02  2.26E+02 -1.85E+02  2.41E+02 -2.51E+02 -1.62E+02  2.33E+02 -4.70E+02  1.87E+02  1.10E+03 -3.02E+02  5.99E+02
          7.11E+01  2.59E+02  6.79E+02 -7.07E+02  8.14E+02 -3.62E+02  1.15E+03  1.44E+02  4.64E+02  9.53E+02 -1.72E+03  2.40E+02
        -7.99E+02  2.00E+02  2.80E+02 -9.08E+02  1.34E+03  6.79E+02 -8.98E+02 -7.30E+02  2.64E+03
 
 OM48
+       -1.81E+02 -5.08E+02  1.90E+02 -2.05E+02  2.85E+02  1.14E+01 -4.42E+02  3.89E+02 -1.37E+02 -8.75E+02 -8.66E+01 -1.84E+03
          3.93E+02  1.82E+02 -1.13E+03  1.40E+03 -7.02E+02  7.47E+02 -2.34E+03  7.82E+02 -2.73E+02 -2.15E+03  1.70E+03 -6.32E+02
         2.20E+02 -1.09E+03 -3.92E+02  1.10E+03  1.12E+00 -4.55E+02  6.56E+02  3.77E+02 -1.82E+03  3.49E+03
 
 OM55
+       -2.01E+02 -1.23E+02  1.31E+02  6.15E+01  2.24E+02  1.39E+02 -1.65E+02  1.65E+02  1.46E+02  8.04E+02 -4.08E+02  9.98E+01
         -1.18E+03 -5.72E+02  7.19E+02 -1.01E+03  5.84E+02 -3.64E+02 -2.43E+02 -1.66E+03 -1.01E+03  5.24E+02 -1.51E+03 -2.12E+01
        -1.82E+02  1.57E+02 -3.24E+02 -4.61E+02  5.05E+02 -1.23E+02  2.01E+02  2.49E+02 -2.83E+02  2.67E+02  1.19E+03
 
 OM56
+       -3.21E+02 -4.08E+02  9.38E+01  1.53E+02  2.11E+02  1.64E+02 -7.35E+01  3.11E+02  1.00E+02  7.02E+02  5.63E+02 -1.49E+01
         -1.23E+03 -1.11E+03  3.64E+02 -5.07E+02  4.92E+02  6.77E+02 -9.77E+02 -2.44E+03 -1.14E+03  3.52E+02 -8.20E+02 -1.96E+02
        -1.68E+02 -1.26E+03 -1.43E+03  3.39E+02 -9.17E+02 -2.52E+01  9.17E+01  1.42E+02 -4.11E+02  5.88E+02  1.25E+03  4.15E+03
 
 OM57
+        1.05E+01  1.35E+02  1.02E+02 -2.30E+02 -3.51E+02 -1.10E+02 -1.29E+02 -2.12E+00 -6.09E+02 -1.81E+03  6.98E+02 -1.83E+02
          1.76E+03  3.77E+02 -1.27E+03  1.81E+03 -9.97E+02  1.30E+03 -2.21E+02  2.11E+03  4.68E+02 -2.08E+03  2.48E+03 -3.87E+02
         2.80E+02 -1.06E+03  1.24E+02  7.98E+02 -1.07E+03 -1.55E+02  2.98E+02 -2.41E+02  1.12E+01  4.38E+02 -7.83E+02 -1.01E+03
          2.79E+03
 
 OM58
+       -5.62E+01 -2.20E+02  3.40E+02  2.56E+02  2.74E+02  3.27E+02  8.91E+01 -4.59E+01  8.72E+02  2.62E+03 -1.02E+03  6.12E+02
         -3.62E+03 -9.43E+02  2.16E+03 -2.45E+03  1.38E+03 -1.84E+02  1.06E+03 -5.52E+03 -1.41E+03  2.92E+03 -3.71E+03 -3.98E+02
        -1.26E+03  2.62E+02 -8.70E+02 -8.52E+02  1.07E+03 -5.99E+01 -1.85E+02  4.81E+02  2.94E+02 -6.60E+02  1.66E+03  2.92E+03
         -2.61E+03  6.94E+03
 
 OM66
+       -1.36E+02 -2.04E+02  1.33E+01  3.10E+01  3.73E+01  7.88E+01 -9.17E+01  1.17E+02  7.92E+00  1.84E+02  1.67E+02 -1.85E+02
         -3.36E+02 -4.82E+02  1.27E+02 -1.18E+02  2.59E+02  8.74E+01 -5.62E+02 -2.87E+02 -5.78E+02 -2.03E+01 -3.34E+02 -3.37E+02
         9.61E+00 -8.17E+02 -6.63E+02  1.03E+02 -7.83E+01  4.92E+01 -5.46E+01 -3.12E+02 -1.47E+02  4.16E+02  5.75E+02  1.54E+03
         -1.23E+02  6.69E+02  1.10E+03
 
 OM67
+        1.49E+02  3.37E+02  1.64E+02 -1.54E+02 -7.37E+01 -2.03E+02  5.23E+01 -1.80E+02  2.48E+01 -4.85E+02 -5.15E+02  1.32E+02
          3.49E+02  5.29E+02 -9.36E+01  1.16E+02 -5.17E+02 -4.85E+02  2.37E+02  5.28E+02  7.84E+02 -5.10E+02  7.62E+02 -6.60E+01
         2.52E+02  3.30E+02  6.91E+01 -2.37E+02  2.80E+02 -1.76E+02 -1.03E+02  2.29E+02 -1.87E+02  7.24E+01 -3.28E+02 -7.02E+02
          5.20E+02 -6.99E+02 -5.14E+02  1.62E+03
 
 OM68
+       -1.93E+02 -7.30E+01 -1.03E+02  1.38E+01  2.39E+02  2.53E+02 -1.15E+02  1.70E+01 -8.67E+01  6.04E+02  5.96E+02  3.32E+02
         -7.33E+02 -1.34E+03  2.82E+02 -6.87E+02  7.56E+02  3.05E+02 -9.99E+01 -1.16E+03 -2.09E+03  7.50E+02 -1.76E+03 -5.44E+02
        -5.12E+02 -1.09E+03 -4.70E+02  3.66E+02 -1.74E+02 -1.77E+02  2.91E+02  4.25E+02 -1.13E+02  3.31E+02  9.59E+02  1.69E+03
         -4.62E+02  1.27E+03  9.54E+02 -9.45E+02  2.48E+03
 
 OM77
+        5.62E+01  8.00E+01 -1.27E+02  1.12E+02 -6.15E+01  3.14E+01  1.51E+02 -6.11E+01  1.82E+02  6.36E+02 -1.83E+02  5.14E+02
         -3.84E+02  1.50E+01  6.47E+02 -6.11E+02  4.54E+02 -4.29E+02  7.52E+02 -7.50E+02 -1.20E+02  1.19E+03 -1.05E+03  1.02E+02
        -6.45E+02  4.28E+02  1.56E+02 -8.54E+02  1.05E+03  1.81E+02 -2.04E+02 -6.20E+00  9.85E+02 -9.95E+02  5.70E+01 -7.48E+01
         -5.63E+02  7.34E+02 -9.43E+01 -3.45E+02  1.47E+02  9.43E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -2.61E+02 -1.48E+02  4.74E+01 -4.31E+02  3.67E+01 -1.84E+02 -1.79E+02  3.25E+02 -7.20E+02 -2.44E+03  1.14E+03 -1.30E+03
          1.91E+03  3.04E+02 -2.50E+03  2.24E+03 -1.34E+03  8.05E+02 -2.54E+03  2.62E+03  8.53E+02 -3.44E+03  4.01E+03 -4.89E+02
         1.22E+03 -9.39E+02  7.76E+01  1.31E+03 -1.97E+03 -3.70E+02  7.38E+02  4.59E+01 -2.16E+03  2.42E+03 -5.38E+02 -3.52E+02
          1.88E+03 -3.37E+03  4.92E+01  1.03E+03 -6.13E+02 -1.69E+03  5.38E+03
 
 OM88
+        1.70E+02  4.22E+02 -4.08E+01  1.60E+02 -8.64E+01  1.31E+01  2.55E+02 -4.45E+02  4.10E+02  1.89E+03  6.10E+01  1.03E+03
         -9.71E+02 -4.86E+02  1.26E+03 -2.73E+03  1.36E+03 -1.02E+03  1.21E+03 -1.44E+03 -8.92E+02  1.78E+03 -3.74E+03  8.80E+02
        -1.93E+02  5.78E+02  3.09E+02 -7.88E+02 -5.35E+02  9.69E+01 -5.83E+02  3.15E+02  7.30E+02 -1.51E+03  6.10E+02  1.07E+02
         -1.31E+03  1.35E+03 -9.97E+01 -1.56E+02  8.27E+02  5.47E+02 -2.12E+03  3.07E+03
 
 SG11
+       -1.57E+03  6.76E+03 -1.75E+03 -1.21E+03 -2.96E+03  1.73E+03  9.45E+03 -2.88E+03  5.65E+03  1.78E+04  1.78E+04  6.97E+03
         -8.65E+03 -4.93E+03  1.05E+04 -1.92E+04  2.35E+04  1.26E+04 -1.93E+04 -1.55E+04  1.89E+04  7.03E+03 -9.50E+03 -9.87E+03
        -5.13E+02 -9.18E+03 -8.22E+03  4.44E+02 -1.50E+04  1.05E+04 -9.76E+03 -2.08E+04  6.29E+03 -1.34E+04 -6.85E+00  2.42E+04
         -1.64E+04  1.73E+04  1.10E+04 -2.73E+03  5.03E+03  6.65E+03 -5.51E+03  9.28E+03  2.42E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.72E+03 -3.30E+03  2.56E+03  4.37E+03  2.13E+03  8.52E+02 -1.53E+03  4.07E+03  6.35E+03  6.55E+03 -6.11E+03 -8.91E+03
         -6.17E+03  5.46E+03  7.34E+02 -2.39E+03  2.74E+02 -2.26E+03 -1.90E+04 -6.15E+03  9.78E+03 -5.44E+03  1.32E+04  2.39E+03
         2.14E+03  1.57E+04  4.22E+03 -7.92E+03 -5.91E+03  3.27E+03  6.12E+03 -3.11E+03 -6.85E+03  7.00E+03  3.75E+03  7.83E+02
         -9.79E+03  7.34E+03 -2.25E+03  3.07E+02 -1.73E+03 -9.30E+02 -1.41E+03  3.69E+01  1.84E+05  0.00E+00  8.40E+05
 
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
 RAW OUTPUT FILE (FILE): example6hmto21_bayes.ext
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
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -2000 MCMCOBJ=   -6804.69822710761     
 iteration        -1990 MCMCOBJ=   -6650.59904646967     
 iteration        -1980 MCMCOBJ=   -6669.35050846901     
 iteration        -1970 MCMCOBJ=   -6658.26908774069     
 iteration        -1960 MCMCOBJ=   -6632.97388907082     
 iteration        -1950 MCMCOBJ=   -6620.39942483948     
 iteration        -1940 MCMCOBJ=   -6608.10936939112     
 iteration        -1930 MCMCOBJ=   -6600.90692270886     
 iteration        -1920 MCMCOBJ=   -6648.64920730454     
 iteration        -1910 MCMCOBJ=   -6621.71926858169     
 iteration        -1900 MCMCOBJ=   -6553.58694466133     
 iteration        -1890 MCMCOBJ=   -6623.54764884824     
 iteration        -1880 MCMCOBJ=   -6580.22640140196     
 iteration        -1870 MCMCOBJ=   -6558.81360629037     
 iteration        -1860 MCMCOBJ=   -6620.33902605476     
 iteration        -1850 MCMCOBJ=   -6580.89258685854     
 iteration        -1840 MCMCOBJ=   -6605.75318602879     
 iteration        -1830 MCMCOBJ=   -6584.01639555086     
 iteration        -1820 MCMCOBJ=   -6531.63830436374     
 iteration        -1810 MCMCOBJ=   -6508.59509536754     
 iteration        -1800 MCMCOBJ=   -6562.40351998927     
 iteration        -1790 MCMCOBJ=   -6482.15238199588     
 iteration        -1780 MCMCOBJ=   -6555.84217502524     
 iteration        -1770 MCMCOBJ=   -6531.43449937059     
 iteration        -1760 MCMCOBJ=   -6555.22626010639     
 iteration        -1750 MCMCOBJ=   -6564.33498008596     
 iteration        -1740 MCMCOBJ=   -6564.40160619052     
 iteration        -1730 MCMCOBJ=   -6532.62470012466     
 iteration        -1720 MCMCOBJ=   -6544.74044738135     
 iteration        -1710 MCMCOBJ=   -6564.73488607305     
 iteration        -1700 MCMCOBJ=   -6602.35007987349     
 iteration        -1690 MCMCOBJ=   -6550.07817962979     
 iteration        -1680 MCMCOBJ=   -6536.31879940821     
 iteration        -1670 MCMCOBJ=   -6536.10171411276     
 iteration        -1660 MCMCOBJ=   -6549.67486039358     
 iteration        -1650 MCMCOBJ=   -6559.22599196363     
 iteration        -1640 MCMCOBJ=   -6530.27739518522     
 iteration        -1630 MCMCOBJ=   -6534.96305427894     
 iteration        -1620 MCMCOBJ=   -6563.84126048425     
 iteration        -1610 MCMCOBJ=   -6538.55584960950     
 iteration        -1600 MCMCOBJ=   -6548.09142936561     
 iteration        -1590 MCMCOBJ=   -6554.92590465467     
 iteration        -1580 MCMCOBJ=   -6537.53829707825     
 iteration        -1570 MCMCOBJ=   -6555.79905082050     
 iteration        -1560 MCMCOBJ=   -6491.72899695317     
 iteration        -1550 MCMCOBJ=   -6500.43441860961     
 iteration        -1540 MCMCOBJ=   -6596.75324176193     
 iteration        -1530 MCMCOBJ=   -6503.80577875503     
 iteration        -1520 MCMCOBJ=   -6558.48156477721     
 iteration        -1510 MCMCOBJ=   -6539.52861838156     
 iteration        -1500 MCMCOBJ=   -6533.35660974920     
 iteration        -1490 MCMCOBJ=   -6523.36004417712     
 iteration        -1480 MCMCOBJ=   -6544.29725644519     
 iteration        -1470 MCMCOBJ=   -6489.30939445032     
 iteration        -1460 MCMCOBJ=   -6532.84650489651     
 iteration        -1450 MCMCOBJ=   -6510.61501785137     
 iteration        -1440 MCMCOBJ=   -6545.06612349415     
 iteration        -1430 MCMCOBJ=   -6505.19240935793     
 iteration        -1420 MCMCOBJ=   -6546.21464418706     
 iteration        -1410 MCMCOBJ=   -6501.88124463855     
 iteration        -1400 MCMCOBJ=   -6557.94597158694     
 iteration        -1390 MCMCOBJ=   -6566.59028804776     
 iteration        -1380 MCMCOBJ=   -6505.93615105854     
 iteration        -1370 MCMCOBJ=   -6581.76368592821     
 iteration        -1360 MCMCOBJ=   -6490.66297299779     
 iteration        -1350 MCMCOBJ=   -6488.42592120995     
 iteration        -1340 MCMCOBJ=   -6495.06399192266     
 iteration        -1330 MCMCOBJ=   -6540.25565094557     
 iteration        -1320 MCMCOBJ=   -6489.02398721736     
 iteration        -1310 MCMCOBJ=   -6531.24267864620     
 iteration        -1300 MCMCOBJ=   -6543.05530338319     
 iteration        -1290 MCMCOBJ=   -6534.25661404501     
 iteration        -1280 MCMCOBJ=   -6442.32628679643     
 iteration        -1270 MCMCOBJ=   -6479.50035128108     
 iteration        -1260 MCMCOBJ=   -6484.46811026531     
 iteration        -1250 MCMCOBJ=   -6501.66577519103     
 iteration        -1240 MCMCOBJ=   -6493.26756077095     
 iteration        -1230 MCMCOBJ=   -6513.66179721493     
 iteration        -1220 MCMCOBJ=   -6481.87829978366     
 iteration        -1210 MCMCOBJ=   -6525.09639269064     
 iteration        -1200 MCMCOBJ=   -6454.71422199724     
 iteration        -1190 MCMCOBJ=   -6541.92504775091     
 iteration        -1180 MCMCOBJ=   -6518.59073757098     
 iteration        -1170 MCMCOBJ=   -6483.14694301589     
 iteration        -1160 MCMCOBJ=   -6533.47112999928     
 iteration        -1150 MCMCOBJ=   -6508.64220487172     
 iteration        -1140 MCMCOBJ=   -6530.53940664225     
 iteration        -1130 MCMCOBJ=   -6449.16575220687     
 iteration        -1120 MCMCOBJ=   -6557.93480148820     
 iteration        -1110 MCMCOBJ=   -6449.89793438356     
 iteration        -1100 MCMCOBJ=   -6468.64226771548     
 iteration        -1090 MCMCOBJ=   -6544.52328652071     
 iteration        -1080 MCMCOBJ=   -6483.69004843377     
 iteration        -1070 MCMCOBJ=   -6528.36714951391     
 iteration        -1060 MCMCOBJ=   -6491.84297851758     
 iteration        -1050 MCMCOBJ=   -6569.95768089925     
 iteration        -1040 MCMCOBJ=   -6545.81443168744     
 iteration        -1030 MCMCOBJ=   -6535.06279570608     
 iteration        -1020 MCMCOBJ=   -6480.09650057602     
 iteration        -1010 MCMCOBJ=   -6496.45183233563     
 iteration        -1000 MCMCOBJ=   -6478.90233910945     
 iteration         -990 MCMCOBJ=   -6500.41455036408     
 iteration         -980 MCMCOBJ=   -6475.38288688662     
 iteration         -970 MCMCOBJ=   -6522.61151666402     
 iteration         -960 MCMCOBJ=   -6497.70872092726     
 iteration         -950 MCMCOBJ=   -6499.03831975545     
 iteration         -940 MCMCOBJ=   -6501.19902753851     
 iteration         -930 MCMCOBJ=   -6528.30788174548     
 iteration         -920 MCMCOBJ=   -6457.33404032128     
 iteration         -910 MCMCOBJ=   -6526.63021107017     
 iteration         -900 MCMCOBJ=   -6444.83415095904     
 iteration         -890 MCMCOBJ=   -6481.76478451849     
 iteration         -880 MCMCOBJ=   -6539.32395179015     
 iteration         -870 MCMCOBJ=   -6502.43053677876     
 iteration         -860 MCMCOBJ=   -6480.77488127217     
 iteration         -850 MCMCOBJ=   -6470.66953846261     
 iteration         -840 MCMCOBJ=   -6465.15892232813     
 iteration         -830 MCMCOBJ=   -6438.04108723935     
 iteration         -820 MCMCOBJ=   -6499.15287050731     
 iteration         -810 MCMCOBJ=   -6527.02000006493     
 iteration         -800 MCMCOBJ=   -6453.63634508061     
 iteration         -790 MCMCOBJ=   -6516.34261722345     
 iteration         -780 MCMCOBJ=   -6522.50831839508     
 iteration         -770 MCMCOBJ=   -6497.21682795328     
 iteration         -760 MCMCOBJ=   -6536.42839259567     
 iteration         -750 MCMCOBJ=   -6552.93401993612     
 iteration         -740 MCMCOBJ=   -6478.91220360398     
 iteration         -730 MCMCOBJ=   -6439.79410379789     
 iteration         -720 MCMCOBJ=   -6534.62996640807     
 iteration         -710 MCMCOBJ=   -6495.96031280365     
 iteration         -700 MCMCOBJ=   -6492.35329421198     
 iteration         -690 MCMCOBJ=   -6499.59526368066     
 iteration         -680 MCMCOBJ=   -6497.32110952907     
 iteration         -670 MCMCOBJ=   -6496.26967057358     
 iteration         -660 MCMCOBJ=   -6471.29676265255     
 iteration         -650 MCMCOBJ=   -6457.51504820452     
 iteration         -640 MCMCOBJ=   -6434.72127559294     
 iteration         -630 MCMCOBJ=   -6535.40942123359     
 iteration         -620 MCMCOBJ=   -6508.63873951356     
 iteration         -610 MCMCOBJ=   -6450.91914288657     
 iteration         -600 MCMCOBJ=   -6512.09848212694     
 iteration         -590 MCMCOBJ=   -6462.48892847727     
 iteration         -580 MCMCOBJ=   -6504.02047141348     
 iteration         -570 MCMCOBJ=   -6513.43314385363     
 iteration         -560 MCMCOBJ=   -6473.67023575417     
 iteration         -550 MCMCOBJ=   -6517.90232992146     
 iteration         -540 MCMCOBJ=   -6447.56776409126     
 iteration         -530 MCMCOBJ=   -6412.16955091420     
 iteration         -520 MCMCOBJ=   -6511.49126714304     
 iteration         -510 MCMCOBJ=   -6503.10828237682     
 iteration         -500 MCMCOBJ=   -6550.59788166360     
 iteration         -490 MCMCOBJ=   -6491.85973766824     
 iteration         -480 MCMCOBJ=   -6480.01560457449     
 iteration         -470 MCMCOBJ=   -6466.53884082137     
 iteration         -460 MCMCOBJ=   -6427.34355882532     
 iteration         -450 MCMCOBJ=   -6494.28287294496     
 iteration         -440 MCMCOBJ=   -6501.47967241261     
 iteration         -430 MCMCOBJ=   -6495.08767641229     
 iteration         -420 MCMCOBJ=   -6432.18607161532     
 iteration         -410 MCMCOBJ=   -6490.63745177665     
 iteration         -400 MCMCOBJ=   -6500.01702985147     
 iteration         -390 MCMCOBJ=   -6511.83575633074     
 iteration         -380 MCMCOBJ=   -6508.49679886018     
 iteration         -370 MCMCOBJ=   -6468.30423604878     
 iteration         -360 MCMCOBJ=   -6451.11973388539     
 iteration         -350 MCMCOBJ=   -6458.78308681960     
 iteration         -340 MCMCOBJ=   -6463.71353268355     
 iteration         -330 MCMCOBJ=   -6478.06485285963     
 iteration         -320 MCMCOBJ=   -6476.51367964115     
 iteration         -310 MCMCOBJ=   -6528.23566830730     
 iteration         -300 MCMCOBJ=   -6489.70677907945     
 iteration         -290 MCMCOBJ=   -6490.54300369535     
 iteration         -280 MCMCOBJ=   -6404.93768867731     
 iteration         -270 MCMCOBJ=   -6443.21538743692     
 iteration         -260 MCMCOBJ=   -6482.76889207538     
 iteration         -250 MCMCOBJ=   -6509.05681731104     
 iteration         -240 MCMCOBJ=   -6516.23719510039     
 iteration         -230 MCMCOBJ=   -6468.83218589159     
 iteration         -220 MCMCOBJ=   -6463.77909721393     
 iteration         -210 MCMCOBJ=   -6508.19787156600     
 iteration         -200 MCMCOBJ=   -6516.35180624232     
 iteration         -190 MCMCOBJ=   -6446.10995037513     
 iteration         -180 MCMCOBJ=   -6467.42227382290     
 iteration         -170 MCMCOBJ=   -6500.41182146635     
 iteration         -160 MCMCOBJ=   -6501.92035962380     
 iteration         -150 MCMCOBJ=   -6473.08104205972     
 iteration         -140 MCMCOBJ=   -6459.45615199719     
 iteration         -130 MCMCOBJ=   -6455.98202335588     
 iteration         -120 MCMCOBJ=   -6478.53354224336     
 iteration         -110 MCMCOBJ=   -6491.69402596856     
 iteration         -100 MCMCOBJ=   -6494.11060143523     
 iteration          -90 MCMCOBJ=   -6464.67539158902     
 iteration          -80 MCMCOBJ=   -6447.37661646950     
 iteration          -70 MCMCOBJ=   -6511.31683153783     
 iteration          -60 MCMCOBJ=   -6528.46256476858     
 iteration          -50 MCMCOBJ=   -6438.22340232397     
 iteration          -40 MCMCOBJ=   -6524.26876053784     
 iteration          -30 MCMCOBJ=   -6524.97390296531     
 iteration          -20 MCMCOBJ=   -6515.68928045292     
 iteration          -10 MCMCOBJ=   -6465.16939006239     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6479.79857755139     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS NOT PERFORMED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6479.79857755139     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3598.00733742153     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6479.79857755139     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5744.64775098765     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    55.1779157436876     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6479.79857755139     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6424.62066180770     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   497.56
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6479.799       **************************************************
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
 
         4.05E+00 -2.27E+00  6.30E-01 -2.07E-01  2.35E+00  2.21E-01  3.73E+00 -6.40E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        3.57E-01
 
 ETA2
+       -3.69E-02  1.57E-01
 
 ETA3
+        5.57E-02 -2.66E-02  1.77E-01
 
 ETA4
+        6.40E-03  4.70E-02 -3.30E-02  2.43E-01
 
 ETA5
+        5.01E-02  1.95E-03  2.89E-02 -4.93E-02  1.88E-01
 
 ETA6
+       -7.20E-02  1.85E-03  1.32E-02  1.16E-02 -6.82E-02  2.25E-01
 
 ETA7
+        9.75E-02 -5.45E-02  8.39E-02 -6.80E-02  1.10E-01 -1.79E-02  3.38E-01
 
 ETA8
+        1.82E-01  3.72E-02  5.62E-02  1.24E-02  6.62E-02 -1.01E-01  1.62E-01  3.08E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.69E-03
 
 EPS2
+        0.00E+00  2.43E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.97E-01
 
 ETA2
+       -1.56E-01  3.96E-01
 
 ETA3
+        2.22E-01 -1.59E-01  4.21E-01
 
 ETA4
+        2.17E-02  2.41E-01 -1.59E-01  4.93E-01
 
 ETA5
+        1.93E-01  1.13E-02  1.58E-01 -2.31E-01  4.33E-01
 
 ETA6
+       -2.54E-01  9.86E-03  6.62E-02  4.96E-02 -3.32E-01  4.74E-01
 
 ETA7
+        2.81E-01 -2.37E-01  3.43E-01 -2.38E-01  4.35E-01 -6.51E-02  5.81E-01
 
 ETA8
+        5.50E-01  1.69E-01  2.40E-01  4.55E-02  2.75E-01 -3.85E-01  5.03E-01  5.55E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.84E-02
 
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
 RAW OUTPUT FILE (FILE): example6hmto21.ext
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
 BURN-IN ITERATIONS (NBURN):                250
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
 MASS MATRIX ACCUMULATION ITERATIONS (MADAPT):          200
 MASS MATRIX BLOCKING TYPE (NUTS_MASS):                 B
 MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (NUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (KAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (NUTS_DELTA):                   0.800000000000000
 NUTS GAMMA SETTING (NUTS_GAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 1.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000
 NUTS WARMUP METHOD (NUTS_TEST):       0
 NUTS MAXIMAL DEPTH SEARCH (NUTS_MAXDEPTH):       10
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       7.500000000000000E-02
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): 2.500000000000000E-02
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 5.000000000000000E-02
 INITIAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPITER): 1
 INTERVAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPINTER):0
 ETA PARAMETERIZATION (NUTS_EPARAM):0
 OMEGA PARAMETERIZATION (NUTS_OPARAM):1
 SIGMA PARAMETERIZATION (NUTS_SPARAM):1
 NUTS REGULARIZING METHOD (NUTS_REG): 0.00000000000000

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
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration         -250 MCMCOBJ=   -6593.45490077237     
 iteration         -245 MCMCOBJ=   -6605.96648865020     
 iteration         -240 MCMCOBJ=   -6691.30396037720     
 iteration         -235 MCMCOBJ=   -6628.12622635600     
 iteration         -230 MCMCOBJ=   -6623.53409579164     
 iteration         -225 MCMCOBJ=   -6631.65136071555     
 iteration         -220 MCMCOBJ=   -6622.78118734474     
 iteration         -215 MCMCOBJ=   -6664.15920348554     
 iteration         -210 MCMCOBJ=   -6645.31338297154     
 iteration         -205 MCMCOBJ=   -6625.97250246436     
 iteration         -200 MCMCOBJ=   -6617.30436985149     
 iteration         -195 MCMCOBJ=   -6592.75590726496     
 iteration         -190 MCMCOBJ=   -6644.77701475957     
 iteration         -185 MCMCOBJ=   -6740.81107157528     
 iteration         -180 MCMCOBJ=   -6679.02489361969     
 iteration         -175 MCMCOBJ=   -6756.56108546408     
 iteration         -170 MCMCOBJ=   -6748.30341302401     
 iteration         -165 MCMCOBJ=   -6744.27148714122     
 iteration         -160 MCMCOBJ=   -6728.32285249184     
 iteration         -155 MCMCOBJ=   -6667.38167125715     
 iteration         -150 MCMCOBJ=   -6649.42023377666     
 iteration         -145 MCMCOBJ=   -6655.37936906372     
 iteration         -140 MCMCOBJ=   -6667.04060018905     
 iteration         -135 MCMCOBJ=   -6617.54471652724     
 iteration         -130 MCMCOBJ=   -6718.25798046344     
 iteration         -125 MCMCOBJ=   -6732.69104514285     
 iteration         -120 MCMCOBJ=   -6592.55011266032     
 iteration         -115 MCMCOBJ=   -6570.91717838034     
 iteration         -110 MCMCOBJ=   -6605.25241534920     
 iteration         -105 MCMCOBJ=   -6720.29194464862     
 iteration         -100 MCMCOBJ=   -6693.78680302464     
 iteration          -95 MCMCOBJ=   -6675.24722804479     
 iteration          -90 MCMCOBJ=   -6673.72157019872     
 iteration          -85 MCMCOBJ=   -6693.13730016900     
 iteration          -80 MCMCOBJ=   -6571.02696448339     
 iteration          -75 MCMCOBJ=   -6613.88866008925     
 iteration          -70 MCMCOBJ=   -6611.55345221296     
 iteration          -65 MCMCOBJ=   -6654.22532972054     
 iteration          -60 MCMCOBJ=   -6680.75954991681     
 iteration          -55 MCMCOBJ=   -6667.60171012698     
 iteration          -50 MCMCOBJ=   -6615.76025741939     
 iteration          -45 MCMCOBJ=   -6659.90486854101     
 iteration          -40 MCMCOBJ=   -6699.40495870881     
 iteration          -35 MCMCOBJ=   -6747.08370193417     
 iteration          -30 MCMCOBJ=   -6693.64480779285     
 iteration          -25 MCMCOBJ=   -6646.51358542304     
 iteration          -20 MCMCOBJ=   -6638.03855329526     
 iteration          -15 MCMCOBJ=   -6676.25137823784     
 iteration          -10 MCMCOBJ=   -6656.94959303617     
 iteration           -5 MCMCOBJ=   -6716.17718696211     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6695.94669396524     
 iteration            5 MCMCOBJ=   -6692.14488197014     
 iteration           10 MCMCOBJ=   -6728.34863696033     
 iteration           15 MCMCOBJ=   -6680.79355215765     
 iteration           20 MCMCOBJ=   -6716.42769851520     
 iteration           25 MCMCOBJ=   -6702.86706875411     
 iteration           30 MCMCOBJ=   -6662.15321144025     
 iteration           35 MCMCOBJ=   -6674.69787709441     
 iteration           40 MCMCOBJ=   -6592.04429825665     
 iteration           45 MCMCOBJ=   -6644.78748366949     
 iteration           50 MCMCOBJ=   -6686.51378340287     
 iteration           55 MCMCOBJ=   -6652.12550360518     
 iteration           60 MCMCOBJ=   -6589.15387506638     
 iteration           65 MCMCOBJ=   -6610.67463697827     
 iteration           70 MCMCOBJ=   -6659.17905072482     
 iteration           75 MCMCOBJ=   -6644.35711224922     
 iteration           80 MCMCOBJ=   -6649.78568061920     
 iteration           85 MCMCOBJ=   -6646.19499393479     
 iteration           90 MCMCOBJ=   -6653.99445396912     
 iteration           95 MCMCOBJ=   -6614.22240290262     
 iteration          100 MCMCOBJ=   -6659.45968825678     
 iteration          105 MCMCOBJ=   -6692.04743045218     
 iteration          110 MCMCOBJ=   -6679.52203602361     
 iteration          115 MCMCOBJ=   -6673.26207643201     
 iteration          120 MCMCOBJ=   -6689.94722180324     
 iteration          125 MCMCOBJ=   -6698.13409216185     
 iteration          130 MCMCOBJ=   -6680.05060856531     
 iteration          135 MCMCOBJ=   -6718.89419410383     
 iteration          140 MCMCOBJ=   -6672.05916043389     
 iteration          145 MCMCOBJ=   -6660.29980393164     
 iteration          150 MCMCOBJ=   -6714.02627789204     
 iteration          155 MCMCOBJ=   -6601.90429557285     
 iteration          160 MCMCOBJ=   -6637.11845273865     
 iteration          165 MCMCOBJ=   -6637.13341665042     
 iteration          170 MCMCOBJ=   -6643.20154572077     
 iteration          175 MCMCOBJ=   -6660.45736160921     
 iteration          180 MCMCOBJ=   -6665.22625228985     
 iteration          185 MCMCOBJ=   -6668.65130003835     
 iteration          190 MCMCOBJ=   -6710.32567706253     
 iteration          195 MCMCOBJ=   -6661.36191933141     
 iteration          200 MCMCOBJ=   -6587.85263851367     
 iteration          205 MCMCOBJ=   -6661.46338372604     
 iteration          210 MCMCOBJ=   -6668.97942435662     
 iteration          215 MCMCOBJ=   -6706.39096926334     
 iteration          220 MCMCOBJ=   -6621.56228600620     
 iteration          225 MCMCOBJ=   -6594.23734127785     
 iteration          230 MCMCOBJ=   -6644.15636467528     
 iteration          235 MCMCOBJ=   -6661.98073195143     
 iteration          240 MCMCOBJ=   -6679.75926130966     
 iteration          245 MCMCOBJ=   -6662.42180576038     
 iteration          250 MCMCOBJ=   -6703.43161656539     
 iteration          255 MCMCOBJ=   -6660.09469619582     
 iteration          260 MCMCOBJ=   -6665.83160015052     
 iteration          265 MCMCOBJ=   -6633.10457140445     
 iteration          270 MCMCOBJ=   -6647.78033255635     
 iteration          275 MCMCOBJ=   -6643.30450789699     
 iteration          280 MCMCOBJ=   -6682.06024233286     
 iteration          285 MCMCOBJ=   -6633.03680395408     
 iteration          290 MCMCOBJ=   -6596.15697121702     
 iteration          295 MCMCOBJ=   -6695.89889861841     
 iteration          300 MCMCOBJ=   -6740.02635703788     
 iteration          305 MCMCOBJ=   -6663.68416112756     
 iteration          310 MCMCOBJ=   -6771.80508830144     
 iteration          315 MCMCOBJ=   -6642.63055418538     
 iteration          320 MCMCOBJ=   -6656.10460195003     
 iteration          325 MCMCOBJ=   -6715.65472948446     
 iteration          330 MCMCOBJ=   -6685.46824925161     
 iteration          335 MCMCOBJ=   -6690.58058144025     
 iteration          340 MCMCOBJ=   -6732.06474301022     
 iteration          345 MCMCOBJ=   -6685.50267122320     
 iteration          350 MCMCOBJ=   -6702.80456572491     
 iteration          355 MCMCOBJ=   -6628.09172574271     
 iteration          360 MCMCOBJ=   -6623.37729394942     
 iteration          365 MCMCOBJ=   -6665.62619725364     
 iteration          370 MCMCOBJ=   -6654.49878864413     
 iteration          375 MCMCOBJ=   -6670.85278669884     
 iteration          380 MCMCOBJ=   -6661.51856985895     
 iteration          385 MCMCOBJ=   -6697.23761092625     
 iteration          390 MCMCOBJ=   -6627.04956187278     
 iteration          395 MCMCOBJ=   -6652.96799246176     
 iteration          400 MCMCOBJ=   -6639.19102896393     
 iteration          405 MCMCOBJ=   -6679.20979872433     
 iteration          410 MCMCOBJ=   -6611.33579668840     
 iteration          415 MCMCOBJ=   -6636.25724570176     
 iteration          420 MCMCOBJ=   -6677.65929394981     
 iteration          425 MCMCOBJ=   -6598.02061057370     
 iteration          430 MCMCOBJ=   -6614.26605714198     
 iteration          435 MCMCOBJ=   -6649.59359967895     
 iteration          440 MCMCOBJ=   -6598.00093051418     
 iteration          445 MCMCOBJ=   -6616.50043974254     
 iteration          450 MCMCOBJ=   -6706.20547406994     
 iteration          455 MCMCOBJ=   -6668.14905604819     
 iteration          460 MCMCOBJ=   -6591.71621222325     
 iteration          465 MCMCOBJ=   -6673.17799702364     
 iteration          470 MCMCOBJ=   -6641.13574542570     
 iteration          475 MCMCOBJ=   -6647.53819480844     
 iteration          480 MCMCOBJ=   -6606.89692315656     
 iteration          485 MCMCOBJ=   -6661.89114594726     
 iteration          490 MCMCOBJ=   -6669.31208602970     
 iteration          495 MCMCOBJ=   -6623.93503726375     
 iteration          500 MCMCOBJ=   -6657.64365904990     
 iteration          505 MCMCOBJ=   -6655.08492727438     
 iteration          510 MCMCOBJ=   -6689.12311653358     
 iteration          515 MCMCOBJ=   -6587.73458772561     
 iteration          520 MCMCOBJ=   -6588.58213183315     
 iteration          525 MCMCOBJ=   -6624.33469509224     
 iteration          530 MCMCOBJ=   -6592.00459419552     
 iteration          535 MCMCOBJ=   -6571.15998854878     
 iteration          540 MCMCOBJ=   -6618.01821594113     
 iteration          545 MCMCOBJ=   -6655.47175119346     
 iteration          550 MCMCOBJ=   -6616.34800589475     
 iteration          555 MCMCOBJ=   -6593.93213084783     
 iteration          560 MCMCOBJ=   -6623.98444163679     
 iteration          565 MCMCOBJ=   -6692.20155787028     
 iteration          570 MCMCOBJ=   -6672.70737593381     
 iteration          575 MCMCOBJ=   -6636.03259613466     
 iteration          580 MCMCOBJ=   -6604.92508680482     
 iteration          585 MCMCOBJ=   -6608.59768830841     
 iteration          590 MCMCOBJ=   -6600.76957250409     
 iteration          595 MCMCOBJ=   -6642.50108965072     
 iteration          600 MCMCOBJ=   -6596.31637005508     
 iteration          605 MCMCOBJ=   -6623.51458601693     
 iteration          610 MCMCOBJ=   -6639.32203692810     
 iteration          615 MCMCOBJ=   -6634.43938457138     
 iteration          620 MCMCOBJ=   -6630.36545861729     
 iteration          625 MCMCOBJ=   -6659.53774329248     
 iteration          630 MCMCOBJ=   -6596.57387635027     
 iteration          635 MCMCOBJ=   -6654.58870458740     
 iteration          640 MCMCOBJ=   -6647.73331303696     
 iteration          645 MCMCOBJ=   -6581.25114403068     
 iteration          650 MCMCOBJ=   -6688.00383960320     
 iteration          655 MCMCOBJ=   -6626.49141699227     
 iteration          660 MCMCOBJ=   -6602.10234500703     
 iteration          665 MCMCOBJ=   -6657.56631545079     
 iteration          670 MCMCOBJ=   -6639.15240248052     
 iteration          675 MCMCOBJ=   -6658.14131626702     
 iteration          680 MCMCOBJ=   -6702.66497359928     
 iteration          685 MCMCOBJ=   -6584.93415563033     
 iteration          690 MCMCOBJ=   -6654.97633795947     
 iteration          695 MCMCOBJ=   -6598.78760840305     
 iteration          700 MCMCOBJ=   -6698.31893674087     
 iteration          705 MCMCOBJ=   -6733.49754100450     
 iteration          710 MCMCOBJ=   -6762.55842337677     
 iteration          715 MCMCOBJ=   -6626.32388500710     
 iteration          720 MCMCOBJ=   -6607.00415162383     
 iteration          725 MCMCOBJ=   -6628.48899699605     
 iteration          730 MCMCOBJ=   -6670.27386870398     
 iteration          735 MCMCOBJ=   -6615.86774183380     
 iteration          740 MCMCOBJ=   -6625.25626319047     
 iteration          745 MCMCOBJ=   -6685.13534936922     
 iteration          750 MCMCOBJ=   -6655.86002969740     
 iteration          755 MCMCOBJ=   -6689.39337563069     
 iteration          760 MCMCOBJ=   -6674.69505788139     
 iteration          765 MCMCOBJ=   -6677.88695569041     
 iteration          770 MCMCOBJ=   -6576.72530081443     
 iteration          775 MCMCOBJ=   -6655.49445381292     
 iteration          780 MCMCOBJ=   -6629.37279516763     
 iteration          785 MCMCOBJ=   -6668.12530067213     
 iteration          790 MCMCOBJ=   -6678.81231905856     
 iteration          795 MCMCOBJ=   -6679.13111266702     
 iteration          800 MCMCOBJ=   -6652.70390262826     
 iteration          805 MCMCOBJ=   -6652.01898213369     
 iteration          810 MCMCOBJ=   -6666.05338297801     
 iteration          815 MCMCOBJ=   -6680.41753151235     
 iteration          820 MCMCOBJ=   -6649.03161286220     
 iteration          825 MCMCOBJ=   -6635.67544812544     
 iteration          830 MCMCOBJ=   -6651.63133317415     
 iteration          835 MCMCOBJ=   -6637.29092173464     
 iteration          840 MCMCOBJ=   -6699.49277203681     
 iteration          845 MCMCOBJ=   -6650.77254185991     
 iteration          850 MCMCOBJ=   -6687.63103357422     
 iteration          855 MCMCOBJ=   -6685.19554460938     
 iteration          860 MCMCOBJ=   -6651.12691181914     
 iteration          865 MCMCOBJ=   -6652.28354899796     
 iteration          870 MCMCOBJ=   -6646.89852888632     
 iteration          875 MCMCOBJ=   -6624.84286199634     
 iteration          880 MCMCOBJ=   -6614.55552069462     
 iteration          885 MCMCOBJ=   -6680.87028159897     
 iteration          890 MCMCOBJ=   -6692.72936015841     
 iteration          895 MCMCOBJ=   -6666.00819120442     
 iteration          900 MCMCOBJ=   -6689.85804684845     
 iteration          905 MCMCOBJ=   -6684.93749595639     
 iteration          910 MCMCOBJ=   -6714.44627298528     
 iteration          915 MCMCOBJ=   -6636.32053421961     
 iteration          920 MCMCOBJ=   -6535.98141214851     
 iteration          925 MCMCOBJ=   -6731.67798194872     
 iteration          930 MCMCOBJ=   -6597.69293585448     
 iteration          935 MCMCOBJ=   -6668.23803853145     
 iteration          940 MCMCOBJ=   -6674.61848430029     
 iteration          945 MCMCOBJ=   -6677.53596794292     
 iteration          950 MCMCOBJ=   -6699.37100372732     
 iteration          955 MCMCOBJ=   -6624.64402074411     
 iteration          960 MCMCOBJ=   -6709.63060862441     
 iteration          965 MCMCOBJ=   -6673.60921237248     
 iteration          970 MCMCOBJ=   -6604.01484014705     
 iteration          975 MCMCOBJ=   -6684.70566516452     
 iteration          980 MCMCOBJ=   -6607.84257392213     
 iteration          985 MCMCOBJ=   -6634.40645016838     
 iteration          990 MCMCOBJ=   -6618.38173368059     
 iteration          995 MCMCOBJ=   -6557.87903578454     
 iteration         1000 MCMCOBJ=   -6626.08501517993     
 iteration         1005 MCMCOBJ=   -6712.21389877355     
 iteration         1010 MCMCOBJ=   -6690.95929246806     
 iteration         1015 MCMCOBJ=   -6679.69960309498     
 iteration         1020 MCMCOBJ=   -6600.91380551422     
 iteration         1025 MCMCOBJ=   -6674.49000396478     
 iteration         1030 MCMCOBJ=   -6647.53189820669     
 iteration         1035 MCMCOBJ=   -6670.08855760137     
 iteration         1040 MCMCOBJ=   -6669.57692987438     
 iteration         1045 MCMCOBJ=   -6661.43409324490     
 iteration         1050 MCMCOBJ=   -6639.30842292878     
 iteration         1055 MCMCOBJ=   -6661.89037937243     
 iteration         1060 MCMCOBJ=   -6711.80062338459     
 iteration         1065 MCMCOBJ=   -6706.21587604111     
 iteration         1070 MCMCOBJ=   -6688.60423979902     
 iteration         1075 MCMCOBJ=   -6651.14921790591     
 iteration         1080 MCMCOBJ=   -6664.35759558076     
 iteration         1085 MCMCOBJ=   -6718.03815466559     
 iteration         1090 MCMCOBJ=   -6687.78620110391     
 iteration         1095 MCMCOBJ=   -6701.88312256200     
 iteration         1100 MCMCOBJ=   -6673.25909371979     
 iteration         1105 MCMCOBJ=   -6636.07248560106     
 iteration         1110 MCMCOBJ=   -6667.19014200186     
 iteration         1115 MCMCOBJ=   -6706.32100133628     
 iteration         1120 MCMCOBJ=   -6651.81079528288     
 iteration         1125 MCMCOBJ=   -6740.61714994829     
 iteration         1130 MCMCOBJ=   -6628.46211331557     
 iteration         1135 MCMCOBJ=   -6628.47739624531     
 iteration         1140 MCMCOBJ=   -6725.17348514940     
 iteration         1145 MCMCOBJ=   -6580.65208634815     
 iteration         1150 MCMCOBJ=   -6617.01235856651     
 iteration         1155 MCMCOBJ=   -6618.49845859733     
 iteration         1160 MCMCOBJ=   -6713.00874253884     
 iteration         1165 MCMCOBJ=   -6675.95390403516     
 iteration         1170 MCMCOBJ=   -6658.00728604518     
 iteration         1175 MCMCOBJ=   -6653.89801142028     
 iteration         1180 MCMCOBJ=   -6574.01197556657     
 iteration         1185 MCMCOBJ=   -6534.89379982093     
 iteration         1190 MCMCOBJ=   -6523.34330779959     
 iteration         1195 MCMCOBJ=   -6674.88109505547     
 iteration         1200 MCMCOBJ=   -6598.93601703242     
 iteration         1205 MCMCOBJ=   -6619.43427044292     
 iteration         1210 MCMCOBJ=   -6590.98346792251     
 iteration         1215 MCMCOBJ=   -6660.36200243933     
 iteration         1220 MCMCOBJ=   -6562.09576693628     
 iteration         1225 MCMCOBJ=   -6637.99213757791     
 iteration         1230 MCMCOBJ=   -6601.24420812557     
 iteration         1235 MCMCOBJ=   -6647.71726653844     
 iteration         1240 MCMCOBJ=   -6655.82089706690     
 iteration         1245 MCMCOBJ=   -6628.71487943310     
 iteration         1250 MCMCOBJ=   -6684.83294215241     
 iteration         1255 MCMCOBJ=   -6698.62152725388     
 iteration         1260 MCMCOBJ=   -6590.62588459083     
 iteration         1265 MCMCOBJ=   -6588.20037477966     
 iteration         1270 MCMCOBJ=   -6649.21158860762     
 iteration         1275 MCMCOBJ=   -6684.56856898123     
 iteration         1280 MCMCOBJ=   -6663.54586705904     
 iteration         1285 MCMCOBJ=   -6639.09703689816     
 iteration         1290 MCMCOBJ=   -6562.19973786076     
 iteration         1295 MCMCOBJ=   -6635.81420189701     
 iteration         1300 MCMCOBJ=   -6633.74136426692     
 iteration         1305 MCMCOBJ=   -6645.21580676989     
 iteration         1310 MCMCOBJ=   -6705.44846355834     
 iteration         1315 MCMCOBJ=   -6698.90232422538     
 iteration         1320 MCMCOBJ=   -6689.01249206366     
 iteration         1325 MCMCOBJ=   -6690.08485840494     
 iteration         1330 MCMCOBJ=   -6643.80185871758     
 iteration         1335 MCMCOBJ=   -6688.82390172900     
 iteration         1340 MCMCOBJ=   -6706.67024585925     
 iteration         1345 MCMCOBJ=   -6640.22551248060     
 iteration         1350 MCMCOBJ=   -6672.47220415019     
 iteration         1355 MCMCOBJ=   -6611.14751219079     
 iteration         1360 MCMCOBJ=   -6602.65969967089     
 iteration         1365 MCMCOBJ=   -6611.86241654702     
 iteration         1370 MCMCOBJ=   -6573.19751833541     
 iteration         1375 MCMCOBJ=   -6566.42421317767     
 iteration         1380 MCMCOBJ=   -6618.33013037794     
 iteration         1385 MCMCOBJ=   -6587.68921116073     
 iteration         1390 MCMCOBJ=   -6684.32703825164     
 iteration         1395 MCMCOBJ=   -6627.67734867683     
 iteration         1400 MCMCOBJ=   -6645.81846933142     
 iteration         1405 MCMCOBJ=   -6627.28880622353     
 iteration         1410 MCMCOBJ=   -6651.33718072365     
 iteration         1415 MCMCOBJ=   -6687.44608071385     
 iteration         1420 MCMCOBJ=   -6676.11396784833     
 iteration         1425 MCMCOBJ=   -6659.13979547840     
 iteration         1430 MCMCOBJ=   -6705.86252305670     
 iteration         1435 MCMCOBJ=   -6624.34354403430     
 iteration         1440 MCMCOBJ=   -6650.89156102472     
 iteration         1445 MCMCOBJ=   -6599.87206063768     
 iteration         1450 MCMCOBJ=   -6682.01403584269     
 iteration         1455 MCMCOBJ=   -6667.78163566768     
 iteration         1460 MCMCOBJ=   -6666.17247969425     
 iteration         1465 MCMCOBJ=   -6689.38435538858     
 iteration         1470 MCMCOBJ=   -6642.51310399326     
 iteration         1475 MCMCOBJ=   -6681.60310444498     
 iteration         1480 MCMCOBJ=   -6624.41558633607     
 iteration         1485 MCMCOBJ=   -6638.64108455465     
 iteration         1490 MCMCOBJ=   -6640.79028032081     
 iteration         1495 MCMCOBJ=   -6653.83587564213     
 iteration         1500 MCMCOBJ=   -6678.67263722636     
 iteration         1505 MCMCOBJ=   -6597.93396634471     
 iteration         1510 MCMCOBJ=   -6617.68981511692     
 iteration         1515 MCMCOBJ=   -6641.45964266801     
 iteration         1520 MCMCOBJ=   -6647.65500186822     
 iteration         1525 MCMCOBJ=   -6592.42081774600     
 iteration         1530 MCMCOBJ=   -6669.64447089867     
 iteration         1535 MCMCOBJ=   -6609.60265538067     
 iteration         1540 MCMCOBJ=   -6686.85368705002     
 iteration         1545 MCMCOBJ=   -6652.51261358295     
 iteration         1550 MCMCOBJ=   -6627.08952893604     
 iteration         1555 MCMCOBJ=   -6696.55174849949     
 iteration         1560 MCMCOBJ=   -6767.45603459498     
 iteration         1565 MCMCOBJ=   -6708.70534781942     
 iteration         1570 MCMCOBJ=   -6768.31859818111     
 iteration         1575 MCMCOBJ=   -6681.50289214789     
 iteration         1580 MCMCOBJ=   -6753.09675451027     
 iteration         1585 MCMCOBJ=   -6730.94515706094     
 iteration         1590 MCMCOBJ=   -6802.43727342491     
 iteration         1595 MCMCOBJ=   -6758.48763530887     
 iteration         1600 MCMCOBJ=   -6723.41263162830     
 iteration         1605 MCMCOBJ=   -6699.92278374572     
 iteration         1610 MCMCOBJ=   -6660.76209061146     
 iteration         1615 MCMCOBJ=   -6690.96403187316     
 iteration         1620 MCMCOBJ=   -6666.19192115604     
 iteration         1625 MCMCOBJ=   -6679.18178185861     
 iteration         1630 MCMCOBJ=   -6609.53361476961     
 iteration         1635 MCMCOBJ=   -6658.81397342562     
 iteration         1640 MCMCOBJ=   -6614.81978156916     
 iteration         1645 MCMCOBJ=   -6645.39922295498     
 iteration         1650 MCMCOBJ=   -6599.89210105395     
 iteration         1655 MCMCOBJ=   -6602.08072376298     
 iteration         1660 MCMCOBJ=   -6657.91363476533     
 iteration         1665 MCMCOBJ=   -6608.72698936237     
 iteration         1670 MCMCOBJ=   -6618.64684420773     
 iteration         1675 MCMCOBJ=   -6557.72299057540     
 iteration         1680 MCMCOBJ=   -6672.03738109832     
 iteration         1685 MCMCOBJ=   -6574.10419067259     
 iteration         1690 MCMCOBJ=   -6669.53242976836     
 iteration         1695 MCMCOBJ=   -6661.18482590519     
 iteration         1700 MCMCOBJ=   -6633.69538310513     
 iteration         1705 MCMCOBJ=   -6582.33802101514     
 iteration         1710 MCMCOBJ=   -6649.50418132749     
 iteration         1715 MCMCOBJ=   -6687.26389572076     
 iteration         1720 MCMCOBJ=   -6675.82969396879     
 iteration         1725 MCMCOBJ=   -6620.87568542710     
 iteration         1730 MCMCOBJ=   -6635.37425390015     
 iteration         1735 MCMCOBJ=   -6638.98164322326     
 iteration         1740 MCMCOBJ=   -6663.57571090646     
 iteration         1745 MCMCOBJ=   -6645.21849954086     
 iteration         1750 MCMCOBJ=   -6672.98506888385     
 iteration         1755 MCMCOBJ=   -6625.39511121542     
 iteration         1760 MCMCOBJ=   -6635.27252736958     
 iteration         1765 MCMCOBJ=   -6739.11967279209     
 iteration         1770 MCMCOBJ=   -6586.06165793151     
 iteration         1775 MCMCOBJ=   -6703.21293683781     
 iteration         1780 MCMCOBJ=   -6605.55786150760     
 iteration         1785 MCMCOBJ=   -6626.65353008833     
 iteration         1790 MCMCOBJ=   -6620.98274208673     
 iteration         1795 MCMCOBJ=   -6662.57853083483     
 iteration         1800 MCMCOBJ=   -6558.04833254375     
 iteration         1805 MCMCOBJ=   -6694.33578532969     
 iteration         1810 MCMCOBJ=   -6675.51109325990     
 iteration         1815 MCMCOBJ=   -6693.15466031735     
 iteration         1820 MCMCOBJ=   -6679.75496640903     
 iteration         1825 MCMCOBJ=   -6574.00122485712     
 iteration         1830 MCMCOBJ=   -6641.53149850224     
 iteration         1835 MCMCOBJ=   -6728.06022349131     
 iteration         1840 MCMCOBJ=   -6732.89859329843     
 iteration         1845 MCMCOBJ=   -6630.43449018962     
 iteration         1850 MCMCOBJ=   -6574.24558064195     
 iteration         1855 MCMCOBJ=   -6664.19446773940     
 iteration         1860 MCMCOBJ=   -6711.50959593165     
 iteration         1865 MCMCOBJ=   -6736.34008415210     
 iteration         1870 MCMCOBJ=   -6660.22065033934     
 iteration         1875 MCMCOBJ=   -6617.20435570358     
 iteration         1880 MCMCOBJ=   -6516.40892842041     
 iteration         1885 MCMCOBJ=   -6590.29524073521     
 iteration         1890 MCMCOBJ=   -6599.26462599337     
 iteration         1895 MCMCOBJ=   -6697.76444346111     
 iteration         1900 MCMCOBJ=   -6688.94962628545     
 iteration         1905 MCMCOBJ=   -6730.18688973827     
 iteration         1910 MCMCOBJ=   -6650.40639806142     
 iteration         1915 MCMCOBJ=   -6660.90806130025     
 iteration         1920 MCMCOBJ=   -6646.50545341562     
 iteration         1925 MCMCOBJ=   -6693.53808840101     
 iteration         1930 MCMCOBJ=   -6692.38877319862     
 iteration         1935 MCMCOBJ=   -6717.85557473693     
 iteration         1940 MCMCOBJ=   -6691.38299815349     
 iteration         1945 MCMCOBJ=   -6744.20081789194     
 iteration         1950 MCMCOBJ=   -6662.64183521873     
 iteration         1955 MCMCOBJ=   -6640.49658406506     
 iteration         1960 MCMCOBJ=   -6677.79488443228     
 iteration         1965 MCMCOBJ=   -6611.39432697047     
 iteration         1970 MCMCOBJ=   -6586.14812875144     
 iteration         1975 MCMCOBJ=   -6563.72596358354     
 iteration         1980 MCMCOBJ=   -6617.03007771374     
 iteration         1985 MCMCOBJ=   -6626.26264268469     
 iteration         1990 MCMCOBJ=   -6584.06401389547     
 iteration         1995 MCMCOBJ=   -6616.56249874951     
 iteration         2000 MCMCOBJ=   -6642.77922284468     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6653.59373759923     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3771.80249746937     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6653.59373759923     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5918.44291103549     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    20.1027031132081     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6653.59373759923     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6633.49103448602     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  3242.65
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6653.594       **************************************************
 #OBJS:********************************************       42.726 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.22E+00  5.57E-01 -1.82E-01  2.27E+00  2.36E-01  3.72E+00 -7.01E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.80E-01
 
 ETA2
+       -4.02E-02  1.95E-01
 
 ETA3
+        3.99E-02 -1.74E-02  1.10E-01
 
 ETA4
+        3.22E-02  4.96E-02 -1.48E-02  2.62E-01
 
 ETA5
+        2.76E-02  2.53E-02 -2.48E-03 -3.22E-02  1.95E-01
 
 ETA6
+       -2.14E-02  1.38E-02  2.41E-02  1.70E-02 -7.49E-02  2.18E-01
 
 ETA7
+        2.06E-02 -4.76E-02  2.59E-02 -7.19E-02  2.36E-02  8.00E-03  2.42E-01
 
 ETA8
+        9.06E-02  8.36E-02  3.72E-02  4.42E-02 -5.28E-03 -5.39E-02  5.78E-02  2.11E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.45E-03
 
 EPS2
+        0.00E+00  2.25E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.26E-01
 
 ETA2
+       -1.69E-01  4.38E-01
 
 ETA3
+        2.30E-01 -1.21E-01  3.28E-01
 
 ETA4
+        1.19E-01  2.19E-01 -9.08E-02  5.09E-01
 
 ETA5
+        1.18E-01  1.32E-01 -1.81E-02 -1.42E-01  4.39E-01
 
 ETA6
+       -8.99E-02  7.03E-02  1.59E-01  7.05E-02 -3.68E-01  4.63E-01
 
 ETA7
+        7.95E-02 -2.14E-01  1.60E-01 -2.84E-01  1.08E-01  3.34E-02  4.90E-01
 
 ETA8
+        3.72E-01  4.15E-01  2.42E-01  1.87E-01 -2.51E-02 -2.52E-01  2.53E-01  4.57E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.72E-02
 
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
 
         7.77E-02  7.39E-02  5.29E-02  7.51E-02  6.35E-02  7.37E-02  6.84E-02  6.73E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        6.14E-02
 
 ETA2
+        3.59E-02  5.53E-02
 
 ETA3
+        2.71E-02  2.42E-02  3.22E-02
 
 ETA4
+        3.92E-02  3.65E-02  2.84E-02  6.28E-02
 
 ETA5
+        3.32E-02  2.96E-02  2.27E-02  3.28E-02  4.42E-02
 
 ETA6
+        3.69E-02  3.32E-02  2.55E-02  3.70E-02  3.35E-02  5.90E-02
 
 ETA7
+        3.60E-02  3.66E-02  2.55E-02  3.71E-02  3.01E-02  3.52E-02  5.12E-02
 
 ETA8
+        3.56E-02  3.23E-02  2.46E-02  3.44E-02  2.84E-02  3.27E-02  3.27E-02  4.47E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.45E-04
 
 EPS2
+        0.00E+00  1.21E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.68E-02
 
 ETA2
+        1.42E-01  6.09E-02
 
 ETA3
+        1.43E-01  1.59E-01  4.71E-02
 
 ETA4
+        1.35E-01  1.45E-01  1.62E-01  5.94E-02
 
 ETA5
+        1.36E-01  1.44E-01  1.50E-01  1.37E-01  4.89E-02
 
 ETA6
+        1.45E-01  1.57E-01  1.58E-01  1.50E-01  1.41E-01  6.15E-02
 
 ETA7
+        1.33E-01  1.49E-01  1.47E-01  1.27E-01  1.31E-01  1.48E-01  5.10E-02
 
 ETA8
+        1.17E-01  1.27E-01  1.41E-01  1.33E-01  1.35E-01  1.38E-01  1.26E-01  4.79E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.31E-03
 
 EPS2
+        0.00E+00  4.02E-03
 
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
+        6.04E-03
 
 TH 2
+       -9.22E-04  5.46E-03
 
 TH 3
+        7.14E-04 -2.73E-04  2.80E-03
 
 TH 4
+        8.36E-04  8.15E-04  4.56E-05  5.65E-03
 
 TH 5
+        4.36E-04  2.59E-04  5.51E-05 -8.40E-04  4.03E-03
 
 TH 6
+       -2.27E-04  1.55E-05  3.79E-04  2.67E-04 -9.94E-04  5.43E-03
 
 TH 7
+        5.31E-04 -1.08E-03  4.25E-04 -1.40E-03  7.55E-04  2.26E-04  4.68E-03
 
 TH 8
+        2.04E-03  1.44E-03  8.74E-04  1.01E-03 -1.07E-04 -1.05E-03  1.16E-03  4.53E-03
 
 OM11
+        1.54E-04 -4.33E-05  6.42E-05  8.95E-05  1.58E-04 -3.46E-05  7.56E-05  1.30E-04  3.77E-03
 
 OM12
+       -4.66E-05  2.33E-04 -1.31E-05 -7.27E-05  1.75E-05 -3.03E-05  1.19E-04 -6.11E-05 -5.70E-04  1.29E-03
 
 OM13
+       -8.35E-05  3.79E-05  3.80E-06  2.24E-05  5.30E-06 -2.59E-05 -5.00E-06 -4.35E-05  3.17E-04 -7.63E-05  7.34E-04
 
 OM14
+        1.04E-04 -8.96E-05 -5.44E-05  2.41E-05  1.16E-06  2.89E-05 -8.88E-06  3.74E-05  3.01E-04  1.29E-04 -4.27E-05  1.53E-03
 
 OM15
+        2.93E-05 -7.26E-05  7.16E-06 -1.28E-05  1.85E-04  4.95E-05  1.17E-04 -4.37E-06  3.43E-04  5.90E-05  2.44E-05 -1.93E-04
          1.10E-03
 
 OM16
+        9.56E-05  2.30E-05 -2.92E-05  5.81E-05  5.24E-05 -1.91E-05 -9.22E-05  4.34E-05 -1.15E-04 -4.68E-05  4.91E-05  1.18E-04
         -3.09E-04  1.36E-03
 
 OM17
+        3.07E-05 -1.48E-05  6.18E-05  7.49E-05  8.01E-05 -4.49E-05 -1.80E-06  6.30E-05  1.47E-04 -2.50E-04  8.95E-05 -3.15E-04
          2.17E-04  7.01E-05  1.29E-03
 
 OM18
+       -1.55E-05 -1.44E-04 -2.48E-05 -4.36E-05  6.68E-05 -5.67E-05  1.17E-04 -3.16E-05  9.66E-04  2.49E-04  2.46E-04  2.98E-04
          1.00E-04 -3.02E-04  3.30E-04  1.26E-03
 
 OM22
+       -1.07E-04 -1.20E-03  7.08E-05  1.20E-04 -9.58E-05  1.91E-04 -1.07E-04 -2.51E-05  8.38E-05 -6.21E-04  1.84E-05  1.14E-05
          3.02E-05 -6.38E-05  1.31E-04 -7.04E-05  3.06E-03
 
 OM23
+       -9.16E-05  1.90E-04  5.71E-05 -2.68E-05  1.26E-05  1.45E-05  1.24E-05 -9.35E-05 -4.94E-05  1.29E-04 -8.85E-05  6.45E-05
          3.62E-06 -3.52E-05 -4.22E-05  3.03E-05 -2.11E-04  5.85E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -2.24E-05 -4.91E-04  1.21E-04 -2.05E-05  6.73E-05  5.61E-05  6.40E-05 -2.44E-05 -8.35E-05  3.50E-06 -7.57E-06 -1.16E-04
          2.83E-05 -4.64E-05  2.99E-05  6.11E-06  6.54E-04 -3.73E-05  1.33E-03
 
 OM25
+        4.72E-05 -9.54E-06  1.83E-05 -7.77E-08 -3.81E-06  1.09E-04 -3.23E-05  3.76E-05 -6.50E-05  1.04E-04 -3.50E-05  4.42E-05
         -1.48E-04  2.69E-05 -7.14E-05  2.21E-05  2.10E-04 -2.12E-05 -4.76E-05  8.75E-04
 
 OM26
+       -1.08E-04  9.30E-05 -2.69E-05  1.12E-05  9.38E-05 -2.48E-05  6.62E-05 -1.51E-04  2.00E-05 -1.61E-07  2.56E-05  8.48E-06
          3.23E-05 -2.07E-04 -1.90E-05  2.68E-05 -1.18E-04  5.37E-05  5.14E-06 -2.33E-04  1.10E-03
 
 OM27
+        1.15E-04  6.58E-04 -1.25E-04  4.90E-06 -1.21E-05 -9.89E-06 -1.11E-04 -7.06E-05 -1.29E-08  1.70E-04 -4.74E-06  5.51E-05
         -9.54E-05  3.49E-06 -2.37E-04 -4.56E-05 -8.69E-04  1.41E-04 -4.93E-04  5.63E-05  7.53E-05  1.34E-03
 
 OM28
+        3.46E-05 -7.30E-05 -1.70E-05 -1.23E-05  1.02E-05  5.44E-05 -2.66E-05  5.50E-06 -6.27E-05  2.34E-04 -5.63E-05  7.53E-05
          9.19E-06 -6.70E-06 -6.27E-05 -1.52E-05  7.83E-04  1.39E-04  2.43E-04  5.49E-05 -2.63E-04  1.27E-04  1.04E-03
 
 OM33
+        5.10E-06 -1.97E-05 -1.52E-04 -1.64E-04  6.15E-05 -2.40E-05  4.12E-05 -3.13E-06  2.57E-05  2.58E-05  2.07E-04  1.69E-05
          1.24E-05  1.87E-05  5.85E-06  6.97E-05 -1.15E-05 -2.94E-05  1.78E-05  6.90E-06  3.37E-05  8.02E-06 -1.04E-05  1.04E-03
 
 OM34
+       -3.42E-05  4.64E-05  1.19E-04  2.33E-04  1.93E-05  2.40E-05 -1.39E-04  2.93E-05  6.85E-05  2.12E-05  1.08E-04  1.07E-04
          5.84E-05 -3.50E-05 -1.04E-05  7.85E-05  5.27E-05  7.41E-05 -1.76E-05  4.90E-05  1.95E-05 -6.64E-06  2.34E-05 -5.10E-05
         8.08E-04
 
 OM35
+        5.88E-05 -5.90E-05 -3.26E-05 -2.44E-05 -2.79E-05  2.15E-06  2.15E-05  2.65E-05 -2.64E-06 -1.48E-06  9.72E-05 -3.38E-05
          6.47E-05  7.66E-06  1.78E-05  1.96E-05 -7.61E-06  3.06E-05 -9.97E-06 -3.26E-05 -1.51E-05  6.30E-05  3.61E-05  1.54E-05
        -9.85E-05  5.14E-04
 
 OM36
+       -6.79E-05  5.22E-05  4.24E-05  2.82E-05  1.84E-05 -8.81E-05  6.68E-05  1.17E-05  3.61E-05 -3.71E-05 -1.42E-05  1.73E-05
         -1.27E-05  9.96E-05  3.74E-06 -2.53E-05  2.71E-05  5.79E-06 -1.44E-06  9.76E-06 -3.00E-05 -2.03E-05 -5.06E-06  7.75E-05
         4.76E-05 -1.52E-04  6.52E-04
 
 OM37
+        1.09E-04 -4.12E-05 -6.11E-05 -7.06E-05 -3.09E-05  2.77E-05  2.52E-05  3.15E-05 -3.17E-06 -2.62E-05 -4.27E-06 -5.48E-05
          4.04E-05 -3.88E-06  1.24E-04  4.18E-05 -6.13E-06 -1.10E-04  3.89E-06 -2.57E-05 -9.84E-06 -6.51E-05 -3.75E-05  1.17E-04
        -1.88E-04  8.60E-05  3.48E-05  6.51E-04
 
 OM38
+       -4.36E-06 -1.98E-05 -5.07E-06 -3.20E-05  8.06E-05 -1.56E-05 -1.92E-05 -1.87E-05  7.56E-05  5.56E-05  2.44E-04  2.99E-05
          1.05E-05 -3.03E-05  6.19E-05  2.08E-04 -1.97E-05  1.79E-04 -1.31E-05 -1.77E-05  2.84E-05  5.50E-06  6.35E-05  2.84E-04
         9.78E-05 -1.28E-05 -1.13E-04  1.46E-04  6.07E-04
 
 OM44
+       -1.09E-04  6.42E-05  4.79E-04  3.05E-04  1.81E-04  1.10E-04  1.89E-05  1.84E-05  2.15E-04  5.33E-05  6.37E-05  3.55E-04
         -3.71E-05  5.62E-05 -4.75E-05  3.85E-06  2.12E-04 -3.33E-05  5.57E-04 -1.26E-04  5.78E-05 -2.35E-04  1.02E-04 -4.40E-05
         6.74E-05 -9.47E-05  1.75E-05 -4.17E-05 -2.49E-05  3.94E-03
 
 OM45
+       -3.09E-05 -2.03E-05 -9.26E-05 -1.06E-05 -4.39E-05  3.85E-06 -3.21E-05  3.22E-05 -2.29E-05  3.29E-05 -3.10E-05  1.20E-04
          1.04E-04 -9.86E-05 -3.30E-05  4.03E-05  5.00E-05  6.33E-06  6.70E-05  1.71E-04 -1.36E-05  8.97E-06  3.34E-05  1.43E-05
        -1.92E-06  9.59E-06  2.67E-05 -8.84E-07 -1.98E-06 -3.91E-04  1.07E-03
 
 OM46
+        1.25E-04 -1.02E-04  2.15E-05 -4.68E-05  1.35E-04  5.31E-05  1.59E-05  4.10E-05 -4.14E-05 -4.59E-06  6.05E-05 -3.76E-05
         -6.90E-05  1.65E-04  1.73E-05 -7.99E-05 -4.02E-05 -7.83E-06  5.82E-05 -2.33E-05  1.03E-04  2.25E-05 -5.45E-05  4.52E-05
         4.64E-05 -4.38E-06 -4.49E-05 -1.32E-05  3.72E-05  3.60E-04 -3.25E-04  1.37E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        3.85E-05  6.82E-05 -1.14E-04  1.54E-05 -1.94E-04  7.05E-06 -8.38E-05  3.25E-05  5.96E-05 -4.68E-05  3.06E-05 -3.26E-05
          1.82E-05  2.22E-05  1.23E-04  7.90E-05 -1.83E-04  9.78E-06 -4.32E-04  4.02E-05 -2.86E-05  3.07E-04 -5.23E-05 -3.55E-05
         1.14E-04  2.32E-05 -5.07E-05 -3.86E-05  2.67E-05 -8.74E-04  1.43E-04 -2.27E-05  1.38E-03
 
 OM48
+       -3.68E-05  1.99E-05  5.79E-05  1.33E-06  2.45E-06  2.35E-05 -8.16E-05 -5.68E-05  1.47E-04  1.12E-04  1.26E-05  4.47E-04
          1.39E-05 -4.40E-05 -3.41E-05  2.36E-04  1.56E-04  3.04E-05  3.66E-04 -1.72E-06  3.17E-06 -5.84E-05  2.26E-04  2.75E-05
         2.37E-04 -2.52E-05  1.25E-05 -8.75E-05  2.33E-05  5.72E-04 -8.64E-05 -2.56E-04  1.76E-04  1.18E-03
 
 OM55
+        7.24E-05  3.69E-06 -1.22E-05  1.04E-04  1.51E-04 -7.13E-05  9.84E-06 -5.19E-05  1.19E-04  5.24E-06  9.71E-05 -7.06E-05
          1.93E-04 -3.34E-05 -4.39E-05 -4.94E-06  1.49E-04 -4.41E-05 -5.61E-05  1.24E-04 -2.30E-05 -1.21E-05  6.92E-06  3.63E-05
         5.10E-05 -1.24E-05 -1.05E-05 -1.37E-05 -1.55E-05 -3.41E-05 -2.67E-04  8.03E-05  2.95E-06  2.64E-05  1.96E-03
 
 OM56
+        5.14E-05  2.34E-05 -1.19E-05  6.91E-05 -6.54E-05 -4.68E-05  6.05E-06  1.11E-04 -1.65E-04  6.50E-06 -6.68E-06  1.77E-05
         -7.81E-05  1.49E-04  7.96E-05 -3.27E-05 -5.18E-05  4.43E-06  1.20E-05 -3.56E-05  1.05E-04  7.36E-06 -4.58E-05  1.73E-06
         3.65E-05  9.48E-05 -8.97E-06 -1.84E-05  3.24E-07 -6.76E-05  1.02E-04 -1.72E-04  2.36E-05 -1.45E-06 -4.85E-04  1.12E-03
 
 OM57
+       -3.19E-05  2.22E-05 -4.59E-05  3.57E-05  1.20E-04 -4.12E-05  1.93E-05 -3.83E-05  9.32E-06 -3.88E-05  5.21E-05 -1.00E-04
          4.66E-05  4.65E-05  1.90E-04  2.72E-05 -7.61E-05  1.62E-05 -3.42E-07 -2.15E-04  5.76E-05  5.21E-05 -4.60E-06  2.01E-05
         1.59E-05  6.46E-05 -5.14E-06  2.55E-05  3.76E-05  7.47E-05 -2.78E-04  4.75E-05 -9.60E-05  5.25E-06  2.39E-04  2.87E-05
          9.03E-04
 
 OM58
+        8.86E-06 -3.26E-05 -1.34E-05 -9.85E-06  6.68E-05  5.55E-05  4.91E-05 -5.47E-05  6.91E-05  7.37E-05  1.66E-05 -5.86E-05
          2.64E-04 -9.69E-05  9.38E-05  9.55E-05  4.32E-05  2.33E-05  2.18E-05  2.95E-04 -1.17E-04 -1.42E-07  8.27E-05  2.52E-05
         1.49E-05  1.16E-04 -2.14E-06  3.76E-05  2.44E-05 -8.86E-05  1.91E-04 -2.57E-05 -9.90E-06 -7.11E-05 -1.30E-04 -1.89E-04
          1.62E-04  8.07E-04
 
 OM66
+       -1.75E-04  1.30E-04  2.74E-05 -6.11E-05  3.07E-04 -3.83E-05  1.23E-04 -1.22E-04  2.49E-04  1.87E-05 -8.15E-06 -6.87E-05
          9.90E-05  1.41E-05 -3.78E-05  1.07E-04 -1.93E-05  3.69E-05  1.84E-05 -1.91E-05  1.20E-04  2.06E-05 -1.01E-05  8.54E-05
         2.05E-05 -3.00E-05  2.15E-04  6.43E-05  1.45E-05 -3.80E-05 -2.06E-05  1.15E-04  5.96E-05 -6.01E-06 -2.94E-05 -5.40E-04
         -2.96E-06  1.41E-04  3.48E-03
 
 OM67
+        5.42E-05 -1.16E-05  1.04E-04  3.80E-05 -1.53E-04  9.68E-05  9.52E-05  1.30E-04 -7.79E-05  6.85E-05 -5.71E-05  8.15E-06
          2.50E-05 -2.25E-05 -1.28E-04 -6.28E-05  4.07E-05  3.37E-05  9.63E-05  5.12E-05 -2.21E-04  3.90E-06  1.19E-04  3.46E-05
        -1.48E-05  2.58E-05  1.27E-04  5.82E-05  1.92E-05 -3.39E-05  7.99E-05 -3.03E-04  7.86E-06  9.93E-05 -3.18E-05  1.44E-04
         -2.12E-04 -4.98E-05  1.90E-04  1.24E-03
 
 OM68
+        4.49E-05 -6.41E-05  2.73E-05 -2.01E-05 -2.42E-05  7.64E-05  3.54E-05  1.40E-05 -3.46E-05 -2.77E-05  1.41E-05  9.03E-05
         -8.72E-05  3.57E-04 -2.80E-05 -1.60E-04 -4.98E-05  3.70E-06  1.67E-05 -3.65E-05  2.96E-04  3.12E-05 -5.17E-05  4.37E-05
         1.26E-05 -4.24E-05  1.56E-04  1.96E-05  3.29E-05  8.14E-05 -7.59E-05  1.69E-04 -4.12E-05  1.87E-05  7.12E-05  3.93E-05
         -4.66E-05 -2.00E-04 -5.19E-04  2.66E-04  1.07E-03
 
 OM77
+       -1.09E-04 -1.75E-04  1.27E-04 -1.00E-04  2.29E-04  4.42E-05 -2.16E-05 -4.86E-05  3.59E-05 -1.28E-04  2.00E-05 -3.28E-05
          6.77E-05 -8.35E-05  1.45E-04  1.24E-05  4.08E-04 -4.04E-05  2.11E-04  2.58E-05  4.03E-05 -5.99E-04 -4.65E-05  1.97E-04
        -4.89E-05 -2.17E-05  1.33E-04  2.38E-04  9.58E-05  2.20E-04  2.18E-05 -1.02E-04 -6.30E-04 -1.23E-04  4.77E-05 -8.85E-06
          1.76E-04  9.92E-05  6.79E-05  1.64E-04  8.73E-05  2.62E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.10E-05 -1.10E-04  3.69E-05 -1.10E-05  8.82E-05 -2.81E-06 -6.71E-05  6.87E-06  5.44E-05 -1.14E-04  5.60E-05 -9.02E-05
          5.14E-05 -1.99E-05  4.14E-04  1.54E-04 -6.36E-05 -1.68E-05 -9.25E-05  3.67E-05  5.98E-05  2.17E-04 -1.01E-04  9.89E-05
        -2.06E-05  2.75E-05 -6.77E-06  1.93E-04  1.37E-04 -2.13E-04  7.62E-05  5.21E-05  1.60E-04 -2.19E-04 -7.38E-05  1.07E-06
          1.36E-05  9.55E-05  7.34E-06 -1.87E-04 -7.18E-06  6.22E-04  1.07E-03
 
 OM88
+        8.41E-05 -9.74E-05 -1.30E-05 -3.77E-05  1.74E-04 -4.44E-05 -1.52E-05 -8.02E-06  3.87E-04  1.76E-04  1.12E-04  1.80E-04
          7.07E-05 -2.26E-04  2.69E-04  7.72E-04  2.95E-04  1.46E-04  1.37E-04 -1.75E-05 -1.31E-04  8.94E-05  6.31E-04  1.27E-04
         9.64E-05  7.23E-06 -7.85E-05  9.32E-05  3.80E-04  3.94E-06  4.65E-05 -1.39E-04  1.10E-04  3.88E-04  1.93E-05 -2.16E-05
         -4.98E-06 -3.25E-05  1.46E-04 -6.44E-05 -4.39E-04  2.17E-04  5.37E-04  2.00E-03
 
 SG11
+       -1.55E-06  1.26E-06 -3.45E-07  6.46E-07 -1.09E-07  2.25E-06 -1.40E-07 -9.89E-07  1.81E-06  3.75E-07 -2.34E-08 -9.00E-07
          6.71E-07 -4.31E-07 -8.31E-07 -6.91E-07 -1.08E-06 -2.47E-07  7.98E-07 -4.75E-07  3.79E-07  3.21E-07  7.32E-08 -1.16E-06
        -1.12E-06 -1.51E-07  5.53E-07  7.62E-07 -1.51E-07 -6.57E-07  8.83E-07  3.75E-07 -3.37E-07 -1.37E-06  2.73E-07 -1.67E-07
          2.21E-07  1.89E-07  1.10E-06  1.14E-06  1.00E-06 -2.50E-07  9.62E-08 -7.67E-07  4.16E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        2.28E-06 -5.51E-07 -2.53E-06 -2.58E-06 -2.96E-06 -1.28E-06  2.31E-06  4.14E-07  2.71E-06  1.32E-06  1.28E-06  1.06E-06
          1.08E-06 -7.07E-07  1.17E-06  2.19E-06 -2.27E-06  4.76E-07 -2.14E-06 -1.17E-06  3.99E-07  1.45E-06  7.11E-07 -2.49E-07
        -6.49E-07 -8.89E-07 -7.27E-07  4.34E-07  4.56E-09  1.14E-06 -2.28E-06  2.85E-06  4.46E-07  9.03E-07  1.07E-06 -1.50E-06
          1.43E-07  3.07E-07 -8.26E-06 -1.02E-06  9.68E-07 -1.50E-06  4.39E-07  2.08E-06 -2.77E-08  0.00E+00  1.46E-06
 
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
+        7.77E-02
 
 TH 2
+       -1.61E-01  7.39E-02
 
 TH 3
+        1.74E-01 -7.00E-02  5.29E-02
 
 TH 4
+        1.43E-01  1.47E-01  1.15E-02  7.51E-02
 
 TH 5
+        8.84E-02  5.53E-02  1.64E-02 -1.76E-01  6.35E-02
 
 TH 6
+       -3.97E-02  2.84E-03  9.73E-02  4.82E-02 -2.12E-01  7.37E-02
 
 TH 7
+        9.98E-02 -2.14E-01  1.18E-01 -2.72E-01  1.74E-01  4.48E-02  6.84E-02
 
 TH 8
+        3.89E-01  2.91E-01  2.46E-01  1.99E-01 -2.50E-02 -2.12E-01  2.52E-01  6.73E-02
 
 OM11
+        3.23E-02 -9.54E-03  1.98E-02  1.94E-02  4.05E-02 -7.65E-03  1.80E-02  3.16E-02  6.14E-02
 
 OM12
+       -1.67E-02  8.77E-02 -6.89E-03 -2.69E-02  7.69E-03 -1.14E-02  4.85E-02 -2.53E-02 -2.58E-01  3.59E-02
 
 OM13
+       -3.97E-02  1.89E-02  2.65E-03  1.10E-02  3.08E-03 -1.29E-02 -2.70E-03 -2.38E-02  1.90E-01 -7.84E-02  2.71E-02
 
 OM14
+        3.41E-02 -3.10E-02 -2.63E-02  8.20E-03  4.66E-04  1.00E-02 -3.31E-03  1.42E-02  1.25E-01  9.16E-02 -4.02E-02  3.92E-02
 
 OM15
+        1.14E-02 -2.96E-02  4.08E-03 -5.13E-03  8.76E-02  2.02E-02  5.17E-02 -1.96E-03  1.69E-01  4.95E-02  2.72E-02 -1.49E-01
          3.32E-02
 
 OM16
+        3.34E-02  8.43E-03 -1.50E-02  2.10E-02  2.24E-02 -7.02E-03 -3.66E-02  1.75E-02 -5.10E-02 -3.54E-02  4.91E-02  8.16E-02
         -2.53E-01  3.69E-02
 
 OM17
+        1.10E-02 -5.56E-03  3.25E-02  2.77E-02  3.51E-02 -1.69E-02 -7.32E-04  2.60E-02  6.64E-02 -1.93E-01  9.18E-02 -2.24E-01
          1.82E-01  5.29E-02  3.60E-02
 
 OM18
+       -5.59E-03 -5.47E-02 -1.32E-02 -1.63E-02  2.96E-02 -2.16E-02  4.81E-02 -1.32E-02  4.42E-01  1.95E-01  2.55E-01  2.14E-01
          8.50E-02 -2.30E-01  2.58E-01  3.56E-02
 
 OM22
+       -2.49E-02 -2.93E-01  2.42E-02  2.88E-02 -2.73E-02  4.68E-02 -2.84E-02 -6.74E-03  2.47E-02 -3.13E-01  1.23E-02  5.25E-03
          1.65E-02 -3.13E-02  6.57E-02 -3.58E-02  5.53E-02
 
 OM23
+       -4.87E-02  1.06E-01  4.46E-02 -1.48E-02  8.21E-03  8.14E-03  7.49E-03 -5.75E-02 -3.32E-02  1.49E-01 -1.35E-01  6.81E-02
          4.52E-03 -3.95E-02 -4.86E-02  3.53E-02 -1.58E-01  2.42E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -7.90E-03 -1.82E-01  6.26E-02 -7.47E-03  2.91E-02  2.09E-02  2.57E-02 -9.93E-03 -3.73E-02  2.68E-03 -7.67E-03 -8.15E-02
          2.34E-02 -3.45E-02  2.28E-02  4.71E-03  3.24E-01 -4.23E-02  3.65E-02
 
 OM25
+        2.05E-02 -4.37E-03  1.17E-02 -3.49E-05 -2.03E-03  4.99E-02 -1.60E-02  1.89E-02 -3.58E-02  9.81E-02 -4.37E-02  3.82E-02
         -1.51E-01  2.46E-02 -6.71E-02  2.10E-02  1.28E-01 -2.97E-02 -4.42E-02  2.96E-02
 
 OM26
+       -4.16E-02  3.79E-02 -1.53E-02  4.48E-03  4.45E-02 -1.01E-02  2.91E-02 -6.78E-02  9.79E-03 -1.35E-04  2.85E-02  6.52E-03
          2.93E-02 -1.69E-01 -1.59E-02  2.27E-02 -6.41E-02  6.68E-02  4.24E-03 -2.37E-01  3.32E-02
 
 OM27
+        4.04E-02  2.43E-01 -6.46E-02  1.78E-03 -5.20E-03 -3.67E-03 -4.43E-02 -2.87E-02 -5.74E-06  1.29E-01 -4.78E-03  3.84E-02
         -7.87E-02  2.58E-03 -1.80E-01 -3.50E-02 -4.30E-01  1.60E-01 -3.70E-01  5.21E-02  6.20E-02  3.66E-02
 
 OM28
+        1.38E-02 -3.06E-02 -9.97E-03 -5.08E-03  4.96E-03  2.28E-02 -1.20E-02  2.53E-03 -3.16E-02  2.01E-01 -6.44E-02  5.96E-02
          8.58E-03 -5.62E-03 -5.40E-02 -1.32E-02  4.38E-01  1.78E-01  2.07E-01  5.75E-02 -2.45E-01  1.07E-01  3.23E-02
 
 OM33
+        2.04E-03 -8.28E-03 -8.94E-02 -6.77E-02  3.01E-02 -1.01E-02  1.87E-02 -1.45E-03  1.30E-02  2.23E-02  2.37E-01  1.34E-02
          1.16E-02  1.58E-02  5.05E-03  6.09E-02 -6.47E-03 -3.78E-02  1.52E-02  7.24E-03  3.15E-02  6.81E-03 -9.95E-03  3.22E-02
 
 OM34
+       -1.55E-02  2.21E-02  7.95E-02  1.09E-01  1.07E-02  1.15E-02 -7.15E-02  1.53E-02  3.92E-02  2.08E-02  1.40E-01  9.61E-02
          6.20E-02 -3.34E-02 -1.01E-02  7.77E-02  3.35E-02  1.08E-01 -1.70E-02  5.83E-02  2.07E-02 -6.39E-03  2.55E-02 -5.57E-02
         2.84E-02
 
 OM35
+        3.33E-02 -3.52E-02 -2.72E-02 -1.43E-02 -1.94E-02  1.29E-03  1.39E-02  1.74E-02 -1.89E-03 -1.82E-03  1.58E-01 -3.81E-02
          8.60E-02  9.16E-03  2.18E-02  2.43E-02 -6.07E-03  5.58E-02 -1.21E-02 -4.86E-02 -2.00E-02  7.60E-02  4.93E-02  2.10E-02
        -1.53E-01  2.27E-02
 
 OM36
+       -3.42E-02  2.77E-02  3.14E-02  1.47E-02  1.13E-02 -4.68E-02  3.82E-02  6.79E-03  2.30E-02 -4.05E-02 -2.05E-02  1.73E-02
         -1.50E-02  1.06E-01  4.07E-03 -2.78E-02  1.92E-02  9.37E-03 -1.55E-03  1.29E-02 -3.53E-02 -2.17E-02 -6.14E-03  9.42E-02
         6.56E-02 -2.63E-01  2.55E-02
 
 OM37
+        5.50E-02 -2.19E-02 -4.53E-02 -3.68E-02 -1.91E-02  1.47E-02  1.44E-02  1.84E-02 -2.02E-03 -2.86E-02 -6.17E-03 -5.48E-02
          4.77E-02 -4.12E-03  1.35E-01  4.61E-02 -4.34E-03 -1.78E-01  4.18E-03 -3.41E-02 -1.16E-02 -6.98E-02 -4.55E-02  1.42E-01
        -2.60E-01  1.49E-01  5.34E-02  2.55E-02
 
 OM38
+       -2.27E-03 -1.09E-02 -3.89E-03 -1.73E-02  5.15E-02 -8.60E-03 -1.14E-02 -1.12E-02  4.99E-02  6.28E-02  3.66E-01  3.09E-02
          1.28E-02 -3.33E-02  6.98E-02  2.37E-01 -1.45E-02  3.00E-01 -1.46E-02 -2.42E-02  3.47E-02  6.10E-03  7.97E-02  3.58E-01
         1.40E-01 -2.29E-02 -1.79E-01  2.33E-01  2.46E-02
 
 OM44
+       -2.24E-02  1.38E-02  1.44E-01  6.47E-02  4.53E-02  2.37E-02  4.40E-03  4.35E-03  5.59E-02  2.37E-02  3.75E-02  1.44E-01
         -1.78E-02  2.43E-02 -2.11E-02  1.72E-03  6.12E-02 -2.19E-02  2.44E-01 -6.80E-02  2.77E-02 -1.03E-01  5.04E-02 -2.18E-02
         3.78E-02 -6.66E-02  1.09E-02 -2.60E-02 -1.61E-02  6.28E-02
 
 OM45
+       -1.21E-02 -8.41E-03 -5.35E-02 -4.29E-03 -2.11E-02  1.59E-03 -1.43E-02  1.46E-02 -1.14E-02  2.79E-02 -3.49E-02  9.33E-02
          9.53E-02 -8.16E-02 -2.80E-02  3.46E-02  2.76E-02  7.99E-03  5.61E-02  1.76E-01 -1.25E-02  7.48E-03  3.15E-02  1.35E-02
        -2.06E-03  1.29E-02  3.19E-02 -1.06E-03 -2.45E-03 -1.90E-01  3.28E-02
 
 OM46
+        4.36E-02 -3.72E-02  1.10E-02 -1.68E-02  5.72E-02  1.95E-02  6.28E-03  1.65E-02 -1.82E-02 -3.45E-03  6.02E-02 -2.59E-02
         -5.62E-02  1.21E-01  1.30E-02 -6.07E-02 -1.96E-02 -8.74E-03  4.31E-02 -2.12E-02  8.34E-02  1.66E-02 -4.56E-02  3.79E-02
         4.41E-02 -5.22E-03 -4.75E-02 -1.39E-02  4.08E-02  1.55E-01 -2.68E-01  3.70E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.34E-02  2.49E-02 -5.84E-02  5.54E-03 -8.23E-02  2.58E-03 -3.31E-02  1.30E-02  2.62E-02 -3.51E-02  3.05E-02 -2.24E-02
          1.48E-02  1.63E-02  9.19E-02  5.99E-02 -8.94E-02  1.09E-02 -3.19E-01  3.66E-02 -2.32E-02  2.27E-01 -4.36E-02 -2.98E-02
         1.08E-01  2.76E-02 -5.36E-02 -4.08E-02  2.93E-02 -3.76E-01  1.18E-01 -1.65E-02  3.71E-02
 
 OM48
+       -1.37E-02  7.83E-03  3.18E-02  5.15E-04  1.12E-03  9.25E-03 -3.47E-02 -2.45E-02  6.94E-02  9.06E-02  1.36E-02  3.31E-01
          1.22E-02 -3.47E-02 -2.76E-02  1.93E-01  8.22E-02  3.65E-02  2.91E-01 -1.69E-03  2.77E-03 -4.64E-02  2.03E-01  2.48E-02
         2.43E-01 -3.23E-02  1.43E-02 -9.96E-02  2.75E-02  2.65E-01 -7.66E-02 -2.01E-01  1.38E-01  3.44E-02
 
 OM55
+        2.11E-02  1.13E-03 -5.20E-03  3.13E-02  5.36E-02 -2.19E-02  3.25E-03 -1.74E-02  4.38E-02  3.30E-03  8.10E-02 -4.07E-02
          1.32E-01 -2.05E-02 -2.76E-02 -3.14E-03  6.07E-02 -4.12E-02 -3.48E-02  9.46E-02 -1.56E-02 -7.48E-03  4.84E-03  2.55E-02
         4.06E-02 -1.23E-02 -9.32E-03 -1.21E-02 -1.42E-02 -1.23E-02 -1.84E-01  4.90E-02  1.80E-03  1.73E-02  4.42E-02
 
 OM56
+        1.98E-02  9.47E-03 -6.74E-03  2.75E-02 -3.07E-02 -1.90E-02  2.64E-03  4.93E-02 -8.01E-02  5.41E-03 -7.36E-03  1.35E-02
         -7.04E-02  1.21E-01  6.61E-02 -2.75E-02 -2.80E-02  5.47E-03  9.86E-03 -3.59E-02  9.45E-02  6.01E-03 -4.24E-02  1.61E-03
         3.83E-02  1.25E-01 -1.05E-02 -2.15E-02  3.93E-04 -3.22E-02  9.28E-02 -1.39E-01  1.90E-02 -1.26E-03 -3.28E-01  3.35E-02
 
 OM57
+       -1.36E-02  1.00E-02 -2.89E-02  1.58E-02  6.31E-02 -1.86E-02  9.39E-03 -1.90E-02  5.05E-03 -3.60E-02  6.40E-02 -8.51E-02
          4.67E-02  4.20E-02  1.76E-01  2.55E-02 -4.58E-02  2.24E-02 -3.12E-04 -2.42E-01  5.77E-02  4.74E-02 -4.74E-03  2.08E-02
         1.86E-02  9.48E-02 -6.70E-03  3.33E-02  5.08E-02  3.96E-02 -2.82E-01  4.27E-02 -8.61E-02  5.07E-03  1.80E-01  2.85E-02
          3.01E-02
 
 OM58
+        4.01E-03 -1.55E-02 -8.93E-03 -4.62E-03  3.70E-02  2.65E-02  2.53E-02 -2.86E-02  3.96E-02  7.23E-02  2.15E-02 -5.26E-02
          2.80E-01 -9.25E-02  9.18E-02  9.46E-02  2.75E-02  3.39E-02  2.10E-02  3.51E-01 -1.25E-01 -1.37E-04  9.01E-02  2.76E-02
         1.84E-02  1.80E-01 -2.95E-03  5.19E-02  3.49E-02 -4.97E-02  2.05E-01 -2.44E-02 -9.40E-03 -7.28E-02 -1.03E-01 -1.99E-01
          1.90E-01  2.84E-02
 
 OM66
+       -3.82E-02  2.99E-02  8.77E-03 -1.38E-02  8.19E-02 -8.81E-03  3.06E-02 -3.08E-02  6.87E-02  8.80E-03 -5.10E-03 -2.97E-02
          5.06E-02  6.49E-03 -1.78E-02  5.10E-02 -5.90E-03  2.59E-02  8.54E-03 -1.09E-02  6.10E-02  9.55E-03 -5.28E-03  4.50E-02
         1.22E-02 -2.25E-02  1.43E-01  4.27E-02  9.97E-03 -1.02E-02 -1.07E-02  5.25E-02  2.72E-02 -2.96E-03 -1.13E-02 -2.73E-01
         -1.67E-03  8.39E-02  5.90E-02
 
 OM67
+        1.98E-02 -4.47E-03  5.60E-02  1.44E-02 -6.86E-02  3.73E-02  3.96E-02  5.48E-02 -3.60E-02  5.42E-02 -5.98E-02  5.91E-03
          2.14E-02 -1.74E-02 -1.01E-01 -5.02E-02  2.09E-02  3.96E-02  7.50E-02  4.92E-02 -1.89E-01  3.03E-03  1.05E-01  3.05E-02
        -1.47E-02  3.23E-02  1.41E-01  6.48E-02  2.21E-02 -1.54E-02  6.92E-02 -2.32E-01  6.02E-03  8.19E-02 -2.04E-02  1.22E-01
         -2.00E-01 -4.98E-02  9.17E-02  3.52E-02
 
 OM68
+        1.77E-02 -2.65E-02  1.58E-02 -8.16E-03 -1.16E-02  3.17E-02  1.58E-02  6.37E-03 -1.72E-02 -2.36E-02  1.59E-02  7.05E-02
         -8.04E-02  2.96E-01 -2.38E-02 -1.38E-01 -2.75E-02  4.68E-03  1.40E-02 -3.77E-02  2.73E-01  2.61E-02 -4.89E-02  4.15E-02
         1.35E-02 -5.72E-02  1.86E-01  2.35E-02  4.08E-02  3.96E-02 -7.08E-02  1.39E-01 -3.40E-02  1.66E-02  4.92E-02  3.59E-02
         -4.74E-02 -2.15E-01 -2.69E-01  2.31E-01  3.27E-02
 
 OM77
+       -2.74E-02 -4.63E-02  4.67E-02 -2.60E-02  7.04E-02  1.17E-02 -6.17E-03 -1.41E-02  1.14E-02 -6.96E-02  1.44E-02 -1.64E-02
          3.98E-02 -4.42E-02  7.87E-02  6.79E-03  1.44E-01 -3.26E-02  1.13E-01  1.71E-02  2.37E-02 -3.19E-01 -2.81E-02  1.20E-01
        -3.36E-02 -1.87E-02  1.01E-01  1.82E-01  7.59E-02  6.86E-02  1.30E-02 -5.36E-02 -3.32E-01 -6.98E-02  2.11E-02 -5.16E-03
          1.14E-01  6.81E-02  2.25E-02  9.10E-02  5.21E-02  5.12E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.22E-02 -4.55E-02  2.13E-02 -4.50E-03  4.25E-02 -1.17E-03 -3.00E-02  3.13E-03  2.71E-02 -9.74E-02  6.32E-02 -7.05E-02
          4.74E-02 -1.65E-02  3.53E-01  1.32E-01 -3.52E-02 -2.12E-02 -7.76E-02  3.79E-02  5.50E-02  1.81E-01 -9.59E-02  9.40E-02
        -2.22E-02  3.71E-02 -8.11E-03  2.31E-01  1.70E-01 -1.04E-01  7.11E-02  4.30E-02  1.32E-01 -1.95E-01 -5.10E-02  9.73E-04
          1.38E-02  1.03E-01  3.80E-03 -1.63E-01 -6.72E-03  3.72E-01  3.27E-02
 
 OM88
+        2.42E-02 -2.95E-02 -5.49E-03 -1.12E-02  6.13E-02 -1.35E-02 -4.95E-03 -2.67E-03  1.41E-01  1.10E-01  9.20E-02  1.03E-01
          4.76E-02 -1.37E-01  1.67E-01  4.85E-01  1.19E-01  1.35E-01  8.43E-02 -1.32E-02 -8.79E-02  5.47E-02  4.37E-01  8.79E-02
         7.58E-02  7.13E-03 -6.88E-02  8.16E-02  3.44E-01  1.40E-03  3.18E-02 -8.37E-02  6.65E-02  2.52E-01  9.74E-03 -1.44E-02
         -3.70E-03 -2.56E-02  5.55E-02 -4.09E-02 -3.00E-01  9.48E-02  3.67E-01  4.47E-02
 
 SG11
+       -3.08E-02  2.64E-02 -1.01E-02  1.33E-02 -2.66E-03  4.73E-02 -3.17E-03 -2.28E-02  4.57E-02  1.62E-02 -1.34E-03 -3.56E-02
          3.14E-02 -1.81E-02 -3.58E-02 -3.01E-02 -3.02E-02 -1.58E-02  3.39E-02 -2.49E-02  1.77E-02  1.36E-02  3.51E-03 -5.58E-02
        -6.12E-02 -1.03E-02  3.36E-02  4.63E-02 -9.50E-03 -1.62E-02  4.17E-02  1.57E-02 -1.41E-02 -6.17E-02  9.57E-03 -7.75E-03
          1.14E-02  1.03E-02  2.88E-02  5.01E-02  4.74E-02 -7.55E-03  4.56E-03 -2.66E-02  6.45E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        2.44E-02 -6.18E-03 -3.97E-02 -2.85E-02 -3.86E-02 -1.44E-02  2.80E-02  5.10E-03  3.66E-02  3.04E-02  3.93E-02  2.23E-02
          2.69E-02 -1.59E-02  2.69E-02  5.11E-02 -3.40E-02  1.63E-02 -4.87E-02 -3.27E-02  9.96E-03  3.29E-02  1.82E-02 -6.41E-03
        -1.89E-02 -3.25E-02 -2.36E-02  1.41E-02  1.53E-04  1.50E-02 -5.77E-02  6.39E-02  9.96E-03  2.17E-02  2.01E-02 -3.70E-02
          3.95E-03  8.95E-03 -1.16E-01 -2.40E-02  2.45E-02 -2.43E-02  1.11E-02  3.86E-02 -3.56E-02  0.00E+00  1.21E-03
 
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
+        2.48E+02
 
 TH 2
+        1.22E+02  3.67E+02
 
 TH 3
+       -7.91E+00  7.00E+01  4.37E+02
 
 TH 4
+       -2.24E+01 -1.28E+00  2.37E+01  2.25E+02
 
 TH 5
+       -6.09E+01 -8.81E+01 -2.63E+01  2.74E+01  3.18E+02
 
 TH 6
+       -3.53E+01 -7.67E+01 -6.38E+01 -2.64E+01  8.11E+01  2.39E+02
 
 TH 7
+        4.67E+01  1.44E+02  2.35E+01  8.17E+01 -8.40E+01 -7.61E+01  3.38E+02
 
 TH 8
+       -1.67E+02 -2.51E+02 -1.33E+02 -7.12E+01  1.04E+02  1.38E+02 -2.01E+02  5.16E+02
 
 OM11
+       -1.07E+01 -1.48E+01 -1.65E+01 -6.46E+00  2.48E-01  5.71E+00 -3.06E+00  4.43E+00  4.81E+02
 
 OM12
+       -1.19E+01 -4.77E+01 -4.35E+01 -1.81E+01  3.35E+01  2.87E+01 -3.83E+01  6.94E+01  4.66E+02  1.88E+03
 
 OM13
+       -1.00E+01 -1.08E+02 -5.13E+01 -8.41E+00  6.00E+01  2.84E+01 -6.11E+01  1.12E+02 -1.43E+01  2.74E+02  2.30E+03
 
 OM14
+       -1.08E+01  2.88E+01  3.33E+01 -4.05E+00 -1.08E+01 -1.60E+01  5.34E+00 -2.86E+01 -7.35E+00  5.23E+01  1.36E+02  1.02E+03
 
 OM15
+        2.10E+01  6.10E+01  3.75E+01  8.41E+00 -7.34E+01 -4.84E+01  1.93E+01 -6.71E+01 -2.35E+02 -4.69E+02 -7.01E+01  1.26E+02
          1.54E+03
 
 OM16
+        5.36E+00  2.67E+01  2.33E+01 -6.59E+00 -3.59E+01 -1.22E+01  2.61E+01 -2.53E+01 -1.34E+02 -3.42E+02 -2.60E+02 -1.63E+02
          4.35E+02  1.29E+03
 
 OM17
+       -4.41E+01 -1.04E+02 -4.01E+01 -2.00E+01  1.74E+01  2.88E+01 -3.31E+01  7.85E+01  2.01E+02  6.33E+02  9.68E+01  3.45E+02
         -3.84E+02 -3.36E+02  1.55E+03
 
 OM18
+        4.77E+01  9.96E+01  5.20E+01  1.22E+01 -1.95E+01 -1.74E+01  1.39E+01 -7.13E+01 -6.16E+02 -1.21E+03 -5.76E+02 -3.82E+02
          4.98E+02  6.94E+02 -9.22E+02  2.70E+03
 
 OM22
+        3.33E+01  9.84E+01  3.60E+00 -1.36E+01 -2.18E-01 -2.45E+01  4.56E+01 -3.19E+01  1.49E+02  8.18E+02  1.13E+02 -1.63E+01
         -2.33E+02 -1.58E+02  2.81E+02 -5.10E+02  1.14E+03
 
 OM23
+       -1.49E+01 -1.23E+02 -8.74E+01 -2.72E-01  6.92E+01  2.37E+01 -6.91E+01  1.51E+02  4.32E+01  2.40E+02  9.34E+02 -8.74E+01
         -1.43E+02 -1.28E+02  4.97E+01 -2.49E+02  4.34E+02  2.92E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        6.83E-01  5.74E+01  2.62E+00 -5.92E+00 -1.79E+01 -1.28E+01  8.22E+00 -2.04E+01  1.77E+01  3.14E+01 -4.82E+01  3.69E+02
          7.29E+01 -3.80E+01  1.90E+02 -1.29E+02 -4.41E+01 -6.54E+01  1.46E+03
 
 OM25
+        2.52E+01  3.97E+01  1.88E+01  1.78E+01 -4.78E+01 -5.67E+01  3.08E+01 -9.01E+01 -1.13E+02 -5.87E+02 -5.32E+01  7.52E+01
          7.98E+02  2.77E+02 -2.56E+02  3.98E+02 -5.81E+02 -2.26E+02  9.34E+01  2.44E+03
 
 OM26
+        4.93E+00 -3.55E+01 -5.96E+00 -2.62E+01 -1.42E+01  1.49E+01 -4.21E+01  3.66E+01 -9.43E+01 -4.96E+02 -1.84E+02 -2.32E+01
          2.98E+02  6.49E+02 -2.14E+02  5.02E+02 -5.19E+02 -4.34E+02 -1.16E+02  7.79E+02  1.92E+03
 
 OM27
+       -9.66E+01 -1.66E+02 -9.30E+00 -5.58E+00  3.61E+01  2.90E+01 -3.38E+01  1.65E+02  9.33E+01  6.36E+02  7.31E+01  1.23E+02
         -2.06E+02 -1.86E+02  7.75E+02 -5.75E+02  8.70E+02  1.78E+02  5.53E+02 -6.86E+02 -6.42E+02  2.36E+03
 
 OM28
+        2.36E+01  1.29E+01  4.14E+01  2.13E+01 -2.58E+01 -1.18E+01  6.57E+00 -8.24E+01 -3.47E+02 -1.58E+03 -3.11E+02 -1.25E+02
          5.54E+02  5.39E+02 -7.90E+02  1.69E+03 -1.60E+03 -8.85E+02 -3.71E+02  1.05E+03  1.34E+03 -1.80E+03  4.43E+03
 
 OM33
+       -4.83E+00  9.96E+00  7.36E+01  3.30E+01  2.70E+00 -1.88E+01  6.92E+00 -2.92E+01 -9.88E+00 -4.59E+01 -1.12E+02 -5.44E+00
         -1.31E+01 -4.23E+00  3.56E+00  7.15E+01  1.42E+01  2.90E+02 -4.48E-01 -1.52E+01 -6.16E+01 -4.65E+01  1.65E+00  1.28E+03
 
 OM34
+        1.39E+01  1.64E+01 -3.94E+01 -4.79E+01 -1.41E+01 -1.58E+01  4.09E+01 -2.67E+01  1.16E+00 -1.53E+01 -2.06E+02  6.53E+00
         -9.29E+01  6.55E+01  5.95E+01  3.68E+01 -2.21E+01 -4.69E+01  1.60E+02 -4.78E+01  1.04E+01  8.13E+01  1.59E+01  2.03E+02
         1.66E+03
 
 OM35
+        2.47E+01  9.09E+01  1.91E+01 -1.46E-01 -2.06E+01 -7.59E+00  3.59E+01 -9.52E+01  1.95E+01 -6.26E+01 -8.03E+02  3.60E+01
          6.89E+01  6.11E+01  3.11E+01  8.71E+01 -1.58E+02 -7.72E+02  8.82E+01  4.42E+02  2.51E+02 -2.47E+02  2.36E+02 -1.19E+02
         2.50E+02  2.90E+03
 
 OM36
+        2.39E+01 -6.11E+00 -4.13E+01 -3.02E+01 -1.16E+00  5.73E+01 -4.10E+01  1.05E+01 -2.27E+00 -5.45E+01 -4.13E+02  2.06E+01
          1.09E+02  6.31E+01 -3.08E+01  8.30E+01 -1.83E+02 -6.30E+02  6.02E+01  2.56E+02  3.99E+02 -1.81E+02  3.65E+02 -3.54E+02
        -1.90E+02  9.54E+02  2.33E+03
 
 OM37
+       -6.14E+01 -6.88E+01  2.84E+01  2.09E+01  6.41E+01 -8.36E+00 -1.90E+01  5.41E+01  2.22E+01  1.16E+02  5.63E+02 -2.33E+00
         -1.14E+02 -6.79E+01 -5.98E+00 -1.50E+02  1.77E+02  9.26E+02  1.97E+01 -1.25E+02 -1.82E+02  2.80E+02 -3.34E+02  9.71E+01
         4.55E+02 -6.94E+02 -5.68E+02  2.37E+03
 
 OM38
+        3.86E+01  9.43E+01  3.56E-01 -9.42E+00 -8.35E+01  4.65E+00  5.83E+01 -9.36E+01  1.86E+01 -2.24E+02 -1.38E+03  2.50E+00
          1.95E+02  2.15E+02 -1.66E+01  2.29E+02 -2.63E+02 -1.76E+03  8.20E+01  2.88E+02  4.00E+02 -1.40E+02  7.10E+02 -7.91E+02
        -3.87E+02  1.07E+03  1.27E+03 -1.31E+03  4.16E+03
 
 OM44
+        1.22E+01 -1.20E+01 -5.07E+01 -2.61E+01 -5.47E+00  1.81E+00 -1.13E+01  1.31E+01 -3.39E+01 -3.83E+01 -5.94E+01 -3.35E+01
          9.48E+00  5.72E-02 -2.81E+01  6.41E+01 -2.43E+01  8.44E+00  2.66E+00  4.71E+01  4.36E+00 -9.09E+00  2.65E+01  3.17E+01
         3.72E+01  6.86E+01  1.71E+01 -1.44E+01  6.38E+00  3.84E+02
 
 OM45
+        1.89E+01  2.02E+01  3.67E+01  8.69E+00 -1.22E+01 -9.20E+00  2.96E+01 -4.53E+01  2.37E+01 -1.56E-01 -1.72E+01 -2.52E+02
         -1.02E+02  5.03E+01 -7.26E+01  5.84E+01  1.83E+01  2.75E+01 -3.45E+02 -7.09E+01 -3.91E+01 -1.53E+02  5.21E+01 -2.69E+01
        -5.82E+01  1.31E+01 -5.69E+01  2.00E+01  3.12E+00  3.83E+01  1.40E+03
 
 OM46
+       -2.68E+00  3.52E+01  2.41E+01  2.38E+01 -2.92E+01 -2.53E+01  3.22E+01 -5.13E+01  1.49E+01 -5.44E+01 -2.50E+01 -1.27E+02
          2.02E+01  8.90E+00 -8.10E+01  8.01E+01  1.45E+00 -9.07E+00 -3.01E+02 -2.06E+01  5.05E+01 -1.36E+02  1.04E+02 -7.63E+01
        -1.77E+02 -6.21E+01  7.58E+01 -7.54E+00  2.64E+01 -1.44E+02  3.73E+02  1.10E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        5.64E-01  1.02E+01  8.81E+00 -4.11E+00  2.87E+01 -5.86E+00 -2.98E+00 -7.49E+00 -1.86E+01  4.64E+01 -3.33E+01  2.12E+02
          3.13E+01 -5.86E+01  4.51E+01 -6.18E+01 -2.17E+01 -9.56E+00  5.69E+02  6.01E+01 -2.79E+01  1.58E+02 -1.02E+02  5.58E+01
         1.02E+01  5.50E+01  5.35E+01  3.51E+01 -3.83E+01  2.81E+02 -2.70E+02 -2.68E+02  1.43E+03
 
 OM48
+       -1.71E+00 -3.14E+01 -1.41E+01  3.32E+01  4.77E+00  8.12E+00  2.62E+01  2.11E+01  2.50E+01 -3.84E+01  4.47E+01 -5.41E+02
         -1.06E+02  7.44E+01 -2.12E+02  8.43E+01  2.54E+01  8.73E+01 -7.94E+02 -1.61E+02  1.43E+01 -2.73E+02  1.73E+02 -1.39E+02
        -4.32E+02 -1.39E+02  1.32E+01 -2.05E+01  1.84E+02 -2.72E+02  4.06E+02  5.54E+02 -7.65E+02  2.01E+03
 
 OM55
+       -2.49E+01 -3.50E+01 -1.11E+01 -2.32E+01 -3.30E+00  3.04E+01 -2.59E+01  5.23E+01  1.52E+01  3.69E+01 -6.80E+01 -1.22E+01
         -3.29E+02 -1.01E+02  8.55E+01 -5.67E+01  1.37E+01  4.14E+01 -3.61E+00 -5.04E+02 -1.50E+02  1.00E+02 -9.83E+01 -3.91E+01
        -5.44E+01 -1.43E+02 -3.27E+01  1.60E+01  3.72E+01  1.46E+01  7.05E+01  8.78E+00 -3.83E+01  6.31E+01  8.09E+02
 
 OM56
+       -2.50E+01 -3.15E+01  1.62E+00 -1.08E+01  5.93E+00  2.20E+01 -2.63E+01  3.31E+01  6.51E+01  1.47E+02  1.36E+02 -1.84E+01
         -3.10E+02 -4.13E+02  3.82E+01 -2.51E+02  1.62E+02  2.00E+02 -3.00E+01 -6.07E+02 -5.91E+02  2.39E+02 -4.09E+02 -1.99E+01
        -1.55E+02 -5.10E+02 -2.59E+02  1.59E+02 -2.45E+02 -3.22E+00 -1.12E+02  6.29E+01 -1.98E+01  8.66E+01  5.21E+02  1.66E+03
 
 OM57
+        5.30E+01  8.89E+01  4.89E+01  1.06E+00 -4.43E+01 -3.96E+01  3.31E+01 -1.04E+02 -2.91E+01 -1.95E+02 -1.63E+01  3.60E+00
          3.91E+02  1.22E+02 -4.38E+02  2.46E+02 -1.84E+02 -8.08E+01 -1.19E+02  9.74E+02  2.96E+02 -6.48E+02  5.13E+02  2.66E+01
        -4.89E+01  9.18E+01  4.73E+01 -8.70E+01 -9.78E+00  1.40E+01  4.56E+02  1.23E+02 -2.68E+01  2.01E+01 -4.48E+02 -5.33E+02
          2.04E+03
 
 OM58
+       -6.26E+01 -9.12E+01 -3.25E+01 -2.61E+01  3.33E+01  3.99E+01 -6.76E+01  1.48E+02  1.29E+02  4.67E+02  2.35E+02  1.10E+01
         -9.77E+02 -4.03E+02  3.34E+02 -7.63E+02  4.78E+02  3.11E+02  8.24E+00 -1.66E+03 -6.33E+02  7.11E+02 -1.36E+03 -2.21E+00
        -7.81E+01 -7.98E+02 -4.17E+02  2.22E+02 -5.57E+02 -1.46E+01 -3.68E+02 -8.42E+01  4.90E+01  2.93E+01  6.27E+02  1.02E+03
         -1.22E+03  3.26E+03
 
 OM66
+       -3.29E-01 -1.49E+01 -4.40E+00 -1.64E+00 -1.73E+01  3.17E+00 -1.36E+01  2.09E+01  2.62E+00  8.62E+01  8.43E+01  2.17E+01
         -1.13E+02 -2.37E+02  7.10E+01 -1.57E+02  8.11E+01  8.27E+01  1.35E+01 -1.68E+02 -3.63E+02  1.14E+02 -2.43E+02 -3.06E+00
        -2.50E+01 -1.12E+02 -2.50E+02  3.38E+01 -1.53E+02  1.08E+01 -6.91E+00 -9.40E+01 -2.49E+00 -3.37E+01  1.07E+02  3.77E+02
         -1.21E+02  2.52E+02  4.77E+02
 
 OM67
+        2.10E+01  3.80E+01 -2.06E+01 -9.85E+00  1.74E+01 -1.90E+01 -5.12E-01 -5.68E+01 -2.01E+01 -2.21E+02 -8.78E+00 -3.37E+01
          1.37E+02  3.67E+02 -1.88E+02  3.00E+02 -1.91E+02 -1.39E+02 -2.35E+02  3.44E+02  7.20E+02 -5.19E+02  5.66E+02 -2.60E+01
        -1.12E+01 -2.28E-01  5.78E+01 -1.73E+02  8.07E+01 -3.12E+01  1.25E+02  3.43E+02 -2.17E+02  1.85E+02 -1.43E+02 -4.84E+02
          5.31E+02 -4.74E+02 -3.08E+02  1.51E+03
 
 OM68
+       -2.11E+01  2.56E+00  7.45E+00  1.48E+01  1.49E+00 -1.86E+01 -8.28E+00  2.42E+01  7.80E+01  4.68E+02  2.99E+02  2.31E+01
         -3.82E+02 -8.67E+02  2.68E+02 -7.16E+02  4.64E+02  4.16E+02  1.29E+02 -6.54E+02 -1.31E+03  5.69E+02 -1.42E+03  9.21E+01
         7.94E+00 -3.20E+02 -7.33E+02  2.61E+02 -8.70E+02  4.02E+01 -6.07E+01 -3.31E+02  1.59E+02 -3.30E+02  1.45E+02  6.97E+02
         -3.51E+02  1.15E+03  5.83E+02 -9.86E+02  2.74E+03
 
 OM77
+       -2.27E+01 -5.90E+01 -1.45E+01  7.08E+00 -6.59E+00  5.15E+00 -4.50E+00  5.21E+01  4.96E+00  1.37E+02  1.70E+00  5.87E+01
         -5.53E+01 -2.94E+01  2.84E+02 -1.30E+02  1.23E+02 -2.47E+00  2.20E+02 -1.88E+02 -1.72E+02  6.91E+02 -4.05E+02 -5.99E+01
         2.57E+01 -9.09E+00 -8.52E+01 -6.13E+00 -8.54E+00  2.88E+01 -1.08E+02 -4.86E+01  4.04E+02 -2.06E+02  2.19E+01  7.63E+01
         -3.44E+02  2.10E+02  3.41E+01 -3.18E+02  1.71E+02  8.19E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        9.59E+01  1.76E+02 -1.40E+01  3.87E+00 -4.42E+01 -3.34E+01  7.64E+01 -1.61E+02 -1.00E+02 -5.54E+02 -1.67E+02 -2.50E+02
          2.88E+02  3.47E+02 -1.06E+03  9.49E+02 -5.61E+02 -2.04E+02 -5.29E+02  4.90E+02  6.02E+02 -1.66E+03  1.92E+03 -4.96E+01
        -1.43E+02  1.30E+02  2.40E+02 -4.85E+02  3.90E+02 -5.31E+01  1.99E+02  2.26E+02 -6.50E+02  8.58E+02 -5.00E+01 -2.85E+02
          6.90E+02 -9.65E+02 -1.81E+02  8.30E+02 -1.07E+03 -9.65E+02  3.41E+03
 
 OM88
+       -4.71E+01 -5.07E+01 -3.19E+00 -6.75E+00 -4.14E+00  4.39E+00 -2.21E+01  6.29E+01  1.80E+02  6.44E+02  3.55E+02  1.49E+02
         -3.15E+02 -3.95E+02  4.66E+02 -1.36E+03  5.43E+02  3.43E+02  2.04E+02 -4.41E+02 -6.15E+02  7.70E+02 -2.06E+03  6.16E+01
         4.78E+01 -2.00E+02 -3.09E+02  2.65E+02 -9.35E+02  2.86E+01 -1.06E+02 -1.25E+02  1.98E+02 -5.37E+02  3.90E+01  2.97E+02
         -2.84E+02  9.91E+02  1.93E+02 -4.29E+02  1.27E+03  2.58E+02 -1.61E+03  2.17E+03
 
 SG11
+        5.05E+02 -5.68E+02  2.23E+02 -3.68E+02 -1.69E+02 -9.65E+02  4.44E+01  4.36E+02 -2.55E+03 -2.07E+03 -1.72E+03  3.57E+02
         -1.55E+02  2.25E+03  1.87E+03  3.58E+03  1.22E+03  1.66E+03 -2.21E+03  1.19E+03  2.31E+02  1.34E+03 -8.31E+02  4.29E+03
         3.62E+03  1.80E+03 -1.93E+03 -2.01E+03 -1.30E+03  4.75E+02 -2.62E+03 -1.41E+03  1.97E+02  2.00E+03 -8.18E+02 -1.13E+03
         -2.30E+03 -9.20E+02 -9.50E+02 -2.04E+03 -3.09E+03  1.60E+03 -2.02E+03 -6.54E+02  2.51E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.90E+02  1.50E+02  8.04E+02  3.38E+02  6.77E+02  2.21E+02 -3.44E+02 -1.09E+02 -4.52E+02 -8.26E+02 -2.66E+03  1.61E+01
         -3.48E+02 -1.11E+02 -5.96E+02  5.52E+01 -9.09E-01 -2.02E+03  1.86E+03  1.40E+03 -7.93E+02 -1.85E+02 -2.35E+02  2.61E+02
         1.01E+03  3.32E+03  9.89E+02 -1.63E+03  2.94E+03  1.50E+02  8.95E+02 -2.11E+03  7.43E+02 -1.62E+03 -3.67E+01  1.18E+03
          6.46E+02 -1.69E+03  2.17E+03 -1.08E+03  5.52E+02  2.92E+02 -3.04E+02 -7.84E+02  4.37E+04  0.00E+00  7.25E+05
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     3738.408
Stop Time: 
Wed 11/02/2016 
08:09 PM

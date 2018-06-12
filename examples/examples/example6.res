Sat 04/22/2017 
09:49 AM
;Model Desc: Receptor Mediated Clearance model with Dynamic Change 
;            in Receptors
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# example6 (from r2compl)
$INPUT C SET ID JID TIME DV=CONC DOSE=AMT RATE EVID MDV CMT
$DATA example6.csv IGNORE=C

; The new numerical integration solver is used, although ADVAN=9 
; is also efficient for this problem.

$SUBROUTINES ADVAN13 TRANS1 TOL=4
$MODEL NCOMPARTMENTS=3

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
0.0027 ;[f]  
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

$SIGMA  
0.1 ;[p]
0.1 ;[p]

$PRIOR NWPRI
; Omega prior
$OMEGAP BLOCK(8)
0.2 FIX
0.0 0.2
0.0 0.0 0.2
0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.2
; degrees of freedom for OMEGA prior
$OMEGAPD
(8 FIXED)           ;[dfo]

; Starting with a short iterative two stage analysis brings the 
; results closer so less time needs to be spent during the 
; burn-in of the BAYES analysis

$EST METHOD=ITS INTERACTION SIGL=4 NITER=15 PRINT=1 
     FILE=example6.ext NOABORT NOPRIOR=1

$EST METHOD=BAYES INTERACTION NBURN=4000 SIGL=4 NITER=10000
     PRINT=10 CTYPE=3 FILE=example6.txt NOABORT NOPRIOR=0

; By default, ISAMPLE_M* are 2.  Since there are many data points 
; per subject, setting these to 1 is enough, and it reduces the 
; time of the analysis

     ISAMPLE_M1=1 ISAMPLE_M2=1 ISAMPLE_M3=1 IACCEPT=0.4

$COV MATRIX=R UNCONDITIONAL
  
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
 TOT. NO. OF INDIVIDUALS:       50
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
                  0.2900E-02   0.2700E-02  -0.2600E-03  -0.3200E-02   0.2000E+00
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
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 beta 2 (nm74b2)

 GENERAL NONLINEAR KINETICS MODEL WITH STIFF/NONSTIFF EQUATIONS (LSODA, ADVAN13)
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
 RAW OUTPUT FILE (FILE): example6.ext
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

 iteration            0 OBJ=  -3444.74513231043
 iteration            1 OBJ=  -3598.19519559445
 iteration            2 OBJ=  -3711.98390790863
 iteration            3 OBJ=  -3819.33411926495
 iteration            4 OBJ=  -3923.74027494773
 iteration            5 OBJ=  -4026.33535813976
 iteration            6 OBJ=  -4127.41378265047
 iteration            7 OBJ=  -4226.89144824212
 iteration            8 OBJ=  -4324.35005656716
 iteration            9 OBJ=  -4419.01653164054
 iteration           10 OBJ=  -4509.15026784207
 iteration           11 OBJ=  -4591.54895251657
 iteration           12 OBJ=  -4659.23573624927
 iteration           13 OBJ=  -4699.40463711791
 iteration           14 OBJ=  -4708.74313138994
 iteration           15 OBJ=  -4709.84103096734
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -8.2453E-04 -3.0347E-03  2.3821E-03  1.3998E-03  1.4658E-03  2.0881E-03  5.5232E-04  1.1314E-03
 SE:             6.9304E-02  5.2692E-02  3.7770E-02  6.5150E-02  5.6770E-02  5.7215E-02  6.4185E-02  6.1423E-02
 N:                      50          50          50          50          50          50          50          50
 
 P VAL.:         9.9051E-01  9.5407E-01  9.4971E-01  9.8286E-01  9.7940E-01  9.7089E-01  9.9313E-01  9.8530E-01
 
 ETASHRINKSD(%)  6.3537E-01  4.2239E+00  8.0427E+00  1.6173E+00  1.4905E+00  5.7642E+00  3.5321E-01  1.5713E+00
 ETASHRINKVR(%)  1.2667E+00  8.2693E+00  1.5439E+01  3.2084E+00  2.9587E+00  1.1196E+01  7.0517E-01  3.1179E+00
 EBVSHRINKSD(%)  6.3176E-01  5.4609E+00  9.8651E+00  2.1018E+00  1.5631E+00  6.3053E+00  4.5539E-01  1.7659E+00
 EBVSHRINKVR(%)  1.2595E+00  1.0623E+01  1.8757E+01  4.1593E+00  3.1018E+00  1.2213E+01  9.0870E-01  3.5006E+00
 EPSSHRINKSD(%)  1.5690E+01  7.2362E+00
 EPSSHRINKVR(%)  2.8918E+01  1.3949E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -4709.84103096734     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -1828.04979083749     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
  
 #TERE:
 Elapsed estimation  time in seconds:    48.12
 Elapsed covariance  time in seconds:     0.47
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
+        2.89E-02 -3.36E-02  3.17E-02 -7.20E-02  2.37E-02  3.40E-03  2.12E-01
 
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
+       -1.33E-01  6.56E-02  2.10E-01  9.19E-02 -4.52E-01  4.34E-01
 
 ETA7
+        1.26E-01 -1.86E-01  2.35E-01 -3.31E-01  1.25E-01  1.70E-02  4.60E-01
 
 ETA8
+        4.40E-01  4.67E-01  2.66E-01  2.10E-01  5.90E-03 -2.63E-01  2.69E-01  4.46E-01
 


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
 
         3.48E-01  2.23E-01  2.30E-01  3.09E-01  2.15E-01  2.66E-01  1.50E-01  3.80E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.50E-01
 
 ETA2
+        2.22E-01  2.80E-01
 
 ETA3
+        1.05E-01  1.23E-01  1.80E-01
 
 ETA4
+        8.67E-02  1.31E-01  1.03E-01  1.68E-01
 
 ETA5
+        1.40E-01  9.08E-02  8.62E-02  1.65E-01  3.06E-01
 
 ETA6
+        1.12E-01  1.62E-01  1.34E-01  1.53E-01  8.22E-02  1.55E-01
 
 ETA7
+        1.61E-01  1.05E-01  1.18E-01  9.44E-02  1.02E-01  1.02E-01  1.45E-01
 
 ETA8
+        2.01E-01  1.47E-01  5.90E-02  9.30E-02  1.41E-01  2.00E-01  1.29E-01  2.62E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        3.43E-03
 
 EPS2
+        0.00E+00  4.27E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.51E-01
 
 ETA2
+        1.03E+00  3.57E-01
 
 ETA3
+        7.07E-01  9.49E-01  3.07E-01
 
 ETA4
+        3.54E-01  6.30E-01  8.44E-01  1.78E-01
 
 ETA5
+        7.04E-01  5.66E-01  7.14E-01  9.38E-01  3.71E-01
 
 ETA6
+        5.44E-01  9.40E-01  1.02E+00  7.32E-01  8.09E-01  1.79E-01
 
 ETA7
+        6.84E-01  5.34E-01  9.64E-01  4.10E-01  5.97E-01  5.16E-01  1.58E-01
 
 ETA8
+        5.90E-01  7.38E-01  3.38E-01  3.62E-01  7.75E-01  9.10E-01  5.66E-01  2.94E-01
 


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
+       -3.20E-02  4.97E-02
 
 TH 3
+        3.05E-02 -1.46E-02  5.29E-02
 
 TH 4
+        8.00E-02 -2.45E-02 -2.62E-03  9.57E-02
 
 TH 5
+       -5.43E-02  2.60E-02 -1.85E-03 -4.91E-02  4.60E-02
 
 TH 6
+       -6.03E-02  2.12E-02 -1.87E-02 -3.82E-02  3.28E-02  7.08E-02
 
 TH 7
+       -1.51E-02 -3.99E-03  9.14E-03 -2.23E-02  1.17E-02  1.08E-02  2.26E-02
 
 TH 8
+        1.04E-01 -3.92E-02  1.90E-02  9.27E-02 -5.85E-02 -5.60E-02 -1.06E-02  1.44E-01
 
 OM11
+       -3.00E-02  5.56E-03 -2.08E-02 -9.48E-03  1.21E-02  1.56E-02  5.25E-04 -1.85E-02  2.26E-02
 
 OM12
+       -2.12E-02  3.01E-02  1.50E-03 -2.76E-02  2.23E-02  1.72E-02 -2.97E-03 -5.40E-02 -4.09E-03  4.94E-02
 
 OM13
+       -1.09E-02 -8.72E-03  2.88E-03 -7.96E-03  3.48E-03  3.00E-03  4.54E-03 -8.73E-05  6.83E-03 -1.32E-02  1.11E-02
 
 OM14
+       -8.03E-03 -3.27E-03  3.48E-03 -1.22E-02  3.85E-03 -3.47E-03  6.73E-03 -8.50E-03  5.69E-04 -3.49E-03  3.21E-03  7.53E-03
 
 OM15
+        1.20E-02 -5.79E-03 -3.95E-03  1.65E-02 -6.65E-03  7.71E-03 -6.60E-03  9.44E-03  3.86E-04  1.20E-03 -1.63E-03 -8.39E-03
          1.96E-02
 
 OM16
+       -1.70E-02  1.43E-02 -1.42E-03 -1.76E-02  1.29E-02  5.42E-03  2.12E-03 -1.46E-02  3.03E-03  8.64E-03 -2.86E-04  1.95E-03
         -9.02E-03  1.26E-02
 
 OM17
+        3.15E-03 -2.54E-03 -2.13E-02  2.39E-02 -7.05E-03  8.59E-04 -7.52E-03  2.44E-02  1.41E-02 -1.75E-02  3.18E-03 -4.91E-03
          6.41E-03 -2.84E-03  2.58E-02
 
 OM18
+       -3.05E-02  1.72E-02 -3.53E-02 -7.76E-04  1.26E-02  2.81E-02 -5.56E-03 -1.93E-02  2.17E-02  7.15E-03 -1.92E-03 -5.84E-03
          7.18E-03  1.84E-03  2.15E-02  4.02E-02
 
 OM22
+       -2.80E-02 -2.19E-02 -3.49E-02  1.05E-03 -8.31E-03  1.82E-02  2.80E-03  1.19E-02  2.08E-02 -4.21E-02  1.56E-02  1.85E-03
          4.72E-03 -7.27E-03  2.31E-02  1.78E-02  7.85E-02
 
 OM23
+        6.71E-03  5.57E-03  1.47E-02 -1.01E-02  5.02E-03 -5.88E-03  3.92E-03 -1.15E-02 -9.68E-03  1.45E-02 -6.50E-03  2.68E-03
         -5.74E-03  2.91E-03 -1.32E-02 -1.09E-02 -2.61E-02  1.52E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -1.24E-02 -2.97E-03 -1.35E-02  2.02E-03 -1.07E-03  1.35E-02 -5.32E-03 -6.51E-03  6.08E-03  2.44E-03  2.31E-04 -6.00E-03
          1.18E-02 -3.80E-03  6.28E-03  1.29E-02  1.67E-02 -8.99E-03  1.73E-02
 
 OM25
+        3.06E-03  6.66E-05 -1.09E-03 -1.85E-03 -8.48E-04 -2.37E-03  3.14E-03  1.83E-03  6.74E-04 -4.33E-03  4.57E-04  2.55E-03
         -4.59E-03  1.93E-03 -2.44E-04 -3.83E-03  1.85E-03  2.38E-03 -6.38E-03  8.25E-03
 
 OM26
+       -2.38E-02  2.34E-02 -1.94E-02 -1.35E-02  1.64E-02  2.71E-02 -2.86E-03 -2.78E-02  9.41E-03  1.95E-02 -6.10E-03 -6.13E-03
          4.85E-03  7.18E-03  4.03E-03  2.18E-02 -2.35E-03 -8.81E-05  8.27E-03 -1.76E-03  2.62E-02
 
 OM27
+       -1.34E-02  1.64E-02 -3.51E-03 -1.42E-02  1.33E-02  5.86E-03  1.31E-03 -2.31E-02  3.30E-03  1.47E-02 -3.72E-03  6.71E-04
         -4.99E-03  6.89E-03 -3.19E-03  5.66E-03 -1.44E-02  6.17E-03 -4.27E-03  2.13E-03  9.46E-03  1.10E-02
 
 OM28
+       -3.74E-02  1.60E-02 -1.73E-02 -2.65E-02  1.76E-02  1.95E-02  7.33E-04 -4.64E-02  1.14E-02  1.88E-02 -1.07E-03  2.01E-04
         -5.81E-04  6.23E-03 -2.60E-03  1.37E-02  3.23E-03 -5.39E-04  7.49E-03 -8.31E-06  1.39E-02  8.35E-03  2.15E-02
 
 OM33
+        4.41E-02 -2.51E-02  2.14E-03  3.90E-02 -3.08E-02 -2.46E-02 -6.48E-03  6.05E-02 -6.32E-03 -3.05E-02  2.61E-03 -3.03E-03
          6.89E-03 -1.04E-02  1.24E-02 -8.17E-03  1.96E-02 -8.47E-03  9.06E-04  9.12E-04 -1.42E-02 -1.36E-02 -1.95E-02  3.24E-02
 
 OM34
+        1.31E-02 -1.35E-02  8.74E-03  1.04E-02 -8.38E-03 -1.56E-02  3.84E-03  2.17E-02 -2.58E-03 -1.53E-02  4.27E-03  3.97E-03
         -4.74E-03 -2.20E-03  1.55E-03 -1.04E-02  3.91E-03 -2.53E-04 -6.44E-03  3.22E-03 -1.28E-02 -4.24E-03 -9.10E-03  9.40E-03
         1.06E-02
 
 OM35
+        2.83E-04  5.54E-03 -8.68E-03  3.37E-03 -5.54E-04  9.41E-03 -4.30E-03 -4.42E-03  1.00E-03  4.00E-03 -3.26E-03 -3.49E-03
          6.77E-03 -2.69E-03  3.02E-03  8.62E-03  2.67E-03 -1.66E-03  4.43E-03 -2.11E-03  6.91E-03  2.79E-06  2.02E-03 -1.16E-04
        -6.22E-03  7.44E-03
 
 OM36
+        1.47E-02  2.94E-03 -1.25E-02  1.54E-02 -1.26E-02 -1.54E-02 -1.13E-02  1.35E-02  6.76E-05  1.49E-03 -6.46E-03 -4.29E-03
          2.53E-03  1.67E-03  6.93E-03  4.53E-03 -1.05E-03 -1.06E-03  3.87E-03  3.31E-04  5.06E-03  1.29E-03  1.74E-03  7.23E-03
        -1.69E-03  1.04E-03  1.79E-02
 
 OM37
+       -2.23E-02  1.22E-02 -1.80E-02 -9.70E-03  7.51E-03  1.54E-02 -4.78E-03 -2.24E-02  1.03E-02  8.77E-03 -4.27E-05 -3.40E-03
          4.96E-03  2.84E-03  4.78E-03  1.57E-02  8.61E-03 -6.47E-03  9.63E-03 -3.27E-03  1.28E-02  3.11E-03  1.26E-02 -7.70E-03
        -9.25E-03  5.37E-03  4.16E-03  1.40E-02
 
 OM38
+        9.60E-03 -7.73E-03  1.37E-03  8.05E-03 -6.69E-03 -3.30E-03 -8.85E-04  1.36E-02 -3.29E-04 -8.93E-03  2.58E-03 -7.24E-04
          2.97E-03 -2.97E-03  3.52E-03 -1.61E-03  6.55E-03 -2.93E-03  5.12E-04 -6.26E-06 -3.27E-03 -3.60E-03 -4.91E-03  8.32E-03
         1.94E-03  6.84E-04  1.53E-04 -8.98E-04  3.48E-03
 
 OM44
+       -2.47E-02 -1.02E-02  1.39E-02 -2.47E-02  1.12E-02  3.51E-03  8.72E-03 -1.96E-02  3.45E-03 -2.72E-03  1.11E-02  5.26E-03
         -2.13E-03 -5.32E-04 -7.23E-03 -8.28E-03  6.83E-03 -1.82E-03  3.86E-03 -3.23E-03 -9.22E-03 -3.13E-03  4.22E-03 -6.74E-03
         3.24E-03 -6.69E-03 -9.72E-03 -5.74E-04 -6.34E-04  2.83E-02
 
 OM45
+       -2.21E-02  2.36E-02 -1.27E-02 -1.71E-02  1.70E-02  2.65E-02 -1.84E-03 -3.89E-02  4.64E-03  2.75E-02 -7.06E-03 -3.82E-03
          4.46E-03  5.41E-03 -4.85E-03  1.52E-02 -1.37E-02  3.33E-03  4.90E-03 -1.75E-03  2.17E-02  1.00E-02  1.49E-02 -1.97E-02
        -1.41E-02  7.71E-03  1.02E-04  1.22E-02 -4.01E-03 -8.32E-03  2.72E-02
 
 OM46
+       -2.00E-02 -5.29E-03 -8.49E-05 -1.60E-02  8.78E-03  1.30E-02  1.28E-02 -1.34E-03  7.17E-03 -1.71E-02  1.04E-02  5.42E-03
         -8.20E-03  4.42E-03  1.19E-03 -2.76E-03  2.30E-02 -4.92E-03 -3.85E-03  6.00E-03 -2.84E-03 -1.79E-03 -4.30E-04  6.30E-04
         5.67E-03 -4.62E-03 -1.01E-02 -2.56E-03  1.57E-03  7.52E-03 -6.71E-03  2.34E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.09E-02  8.78E-03 -2.45E-03 -6.26E-03  9.47E-03  1.03E-02  5.83E-03 -7.46E-03  4.85E-03  1.66E-03  9.54E-04  1.68E-03
         -4.70E-03  4.58E-03  1.11E-03  5.33E-03 -1.87E-03  4.79E-05 -4.04E-03  1.79E-03  4.47E-03  4.60E-03  2.66E-03 -7.21E-03
        -5.11E-04 -2.17E-04 -4.78E-03  1.03E-03 -1.32E-03 -2.98E-03  5.25E-03  6.11E-03  8.91E-03
 
 OM48
+       -2.09E-02  2.26E-03 -1.30E-02 -7.53E-03  5.72E-03  1.05E-02  1.70E-03 -1.57E-02  8.17E-03 -8.78E-04  2.39E-03  1.26E-03
          1.08E-03 -1.04E-03  4.26E-03  1.05E-02  1.42E-02 -5.51E-03  6.44E-03 -2.01E-03  3.85E-03  5.40E-04  7.58E-03 -4.68E-03
        -2.25E-03  1.69E-03 -1.08E-03  5.89E-03 -1.07E-03  4.63E-03  2.85E-03  1.71E-03  1.50E-03  8.66E-03
 
 OM55
+       -6.38E-02  3.38E-02 -1.48E-02 -5.86E-02  4.05E-02  3.41E-02  7.77E-03 -9.59E-02  1.72E-02  4.80E-02 -3.70E-03  1.30E-03
         -3.23E-03  1.46E-02 -1.49E-02  1.67E-02 -2.17E-02  9.63E-03  6.28E-03  2.77E-03  2.69E-02  2.22E-02  3.93E-02 -4.59E-02
        -1.60E-02 -5.67E-04  2.13E-04  1.83E-02 -1.18E-02  1.14E-02  3.14E-02 -3.14E-03  6.89E-03  8.73E-03  9.34E-02
 
 OM56
+       -8.35E-03  1.08E-02 -1.12E-02 -5.41E-03  3.48E-03  5.92E-03 -4.78E-03 -1.28E-02  3.03E-03  7.99E-03 -3.62E-03 -1.96E-03
          2.24E-04  3.38E-03  1.89E-03  8.37E-03 -3.98E-04 -1.74E-04  2.71E-03  3.24E-04  8.77E-03  4.42E-03  6.71E-03 -5.12E-03
        -5.67E-03  3.26E-03  5.40E-03  6.11E-03 -1.50E-03 -5.30E-03  8.00E-03 -3.88E-03  2.21E-04  1.79E-03  1.06E-02  6.76E-03
 
 OM57
+        4.44E-04 -1.85E-03 -1.42E-02  1.08E-02 -3.23E-03  1.21E-03 -3.38E-03  1.24E-02  7.63E-03 -1.23E-02  2.84E-03 -1.74E-03
          2.45E-03 -1.53E-03  1.28E-02  1.04E-02  1.75E-02 -8.96E-03  2.49E-03  1.28E-03  1.73E-03 -1.47E-03 -1.36E-03  8.04E-03
         8.57E-04  2.10E-03  2.65E-03  3.18E-03  2.75E-03 -4.61E-03 -2.41E-03  3.97E-03  8.22E-04  2.90E-03 -9.67E-03  1.61E-03
          1.03E-02
 
 OM58
+        2.51E-02 -1.27E-02 -8.78E-03  3.05E-02 -1.80E-02 -9.27E-03 -5.35E-03  3.86E-02  2.36E-03 -2.20E-02  2.19E-03 -3.40E-03
          8.10E-03 -7.43E-03  1.55E-02  3.23E-03  1.91E-02 -9.21E-03  1.44E-03  3.99E-03 -5.39E-03 -7.51E-03 -9.70E-03  2.04E-02
         5.62E-03  1.97E-03  5.07E-03 -2.23E-03  5.62E-03 -8.33E-03 -9.51E-03  2.35E-03 -2.08E-03 -9.60E-04 -2.58E-02 -2.10E-03
          9.83E-03  2.00E-02
 
 OM66
+       -3.22E-02  1.44E-02 -9.01E-03 -2.64E-02  1.80E-02  2.75E-02  4.64E-03 -3.36E-02  4.32E-03  1.26E-02 -6.24E-04  1.49E-04
          1.17E-03  3.98E-03 -5.24E-03  1.05E-02  5.47E-03  4.33E-04  6.16E-03 -4.62E-03  1.42E-02  3.37E-03  1.04E-02 -1.33E-02
        -1.03E-02  6.38E-03 -5.37E-03  9.74E-03 -1.92E-03  1.20E-03  1.53E-02  3.48E-03  3.44E-03  5.09E-03  1.43E-02  2.78E-03
         -2.04E-03 -9.45E-03  2.42E-02
 
 OM67
+        1.47E-02 -1.68E-02  6.49E-03  9.51E-03 -1.36E-02 -1.17E-02 -6.96E-04  1.66E-02 -6.06E-03 -9.48E-03  1.57E-03  5.75E-04
          2.66E-03 -6.05E-03 -2.13E-03 -1.03E-02  7.03E-03 -8.40E-04  2.03E-03 -8.98E-04 -9.71E-03 -7.65E-03 -6.36E-03  1.10E-02
         4.51E-03 -1.88E-03  1.81E-03 -4.57E-03  2.28E-03  4.18E-03 -9.76E-03 -1.29E-03 -6.53E-03 -1.57E-03 -1.44E-02 -3.90E-03
         -1.81E-03  3.75E-03 -4.47E-03  1.05E-02
 
 OM68
+        5.86E-02 -1.29E-02  9.03E-03  4.66E-02 -3.05E-02 -3.19E-02 -1.23E-02  6.29E-02 -1.32E-02 -1.16E-02 -7.50E-03 -7.68E-03
          7.12E-03 -4.62E-03  8.03E-03 -9.77E-03 -1.05E-02  7.40E-04 -1.57E-03 -3.08E-04 -6.00E-03 -7.42E-03 -1.89E-02  2.60E-02
         6.16E-03 -3.35E-04  1.53E-02 -8.93E-03  4.64E-03 -1.55E-02 -1.23E-02 -1.14E-02 -7.31E-03 -1.09E-02 -3.58E-02 -2.45E-03
          1.75E-03  1.46E-02 -1.69E-02  8.30E-03  4.01E-02
 
 OM77
+       -8.54E-03  1.01E-02 -5.17E-03 -2.58E-03  5.24E-03  1.54E-03 -8.21E-03 -2.41E-02  1.20E-03  1.95E-02 -6.49E-03 -2.71E-03
          4.25E-03 -1.65E-03 -1.78E-03  7.98E-03 -1.41E-02  4.08E-03  5.23E-03 -3.40E-03  5.75E-03  4.18E-03  9.31E-03 -1.11E-02
        -6.17E-03  2.87E-03  3.98E-03  5.49E-03 -4.24E-03  1.09E-03  8.46E-03 -1.43E-02 -4.13E-03  2.72E-03  1.98E-02  4.19E-03
         -3.73E-03 -6.23E-03  1.95E-03 -1.84E-03 -4.61E-03  2.10E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.38E-02  5.36E-03 -9.19E-03  2.34E-02 -4.98E-03  9.98E-04 -9.50E-03  1.53E-02  3.33E-03  2.62E-03 -5.70E-03 -6.94E-03
          7.99E-03 -2.62E-03  1.31E-02  1.55E-02 -5.00E-03 -2.66E-03  3.78E-03 -2.53E-03  7.39E-03  1.30E-03 -2.42E-03  4.41E-03
        -3.52E-03  5.41E-03  5.81E-03  3.11E-03  1.13E-03 -1.20E-02  4.74E-03 -9.35E-03  1.21E-03 -4.80E-04 -6.93E-03  2.82E-03
          4.99E-03  7.35E-03 -2.22E-03 -4.47E-03  1.03E-02  5.94E-03  1.67E-02
 
 OM88
+       -5.59E-02  3.56E-02 -3.66E-02 -2.58E-02  3.30E-02  4.62E-02 -2.59E-03 -6.23E-02  2.39E-02  3.10E-02 -5.69E-03 -5.41E-03
          5.63E-03  8.41E-03  1.11E-02  4.44E-02  7.69E-04 -3.49E-03  1.28E-02 -3.84E-03  3.42E-02  1.63E-02  2.82E-02 -3.02E-02
        -1.96E-02  1.12E-02  8.51E-04  2.29E-02 -6.04E-03 -6.97E-03  3.34E-02 -5.25E-03  1.03E-02  1.29E-02  4.98E-02  1.33E-02
          5.14E-03 -9.71E-03  2.19E-02 -1.90E-02 -2.68E-02  1.61E-02  1.43E-02  6.88E-02
 
 SG11
+        9.75E-04 -4.47E-04  1.28E-04  9.11E-04 -5.84E-04 -5.13E-04 -1.75E-04  1.13E-03 -1.72E-04 -4.01E-04 -2.26E-05 -1.00E-04
          1.66E-04 -1.92E-04  1.82E-04 -1.77E-04  6.71E-05 -1.15E-04  1.12E-06  5.21E-06 -2.62E-04 -2.09E-04 -3.55E-04  5.19E-04
         1.62E-04 -1.10E-05  1.35E-04 -1.63E-04  1.25E-04 -1.74E-04 -2.93E-04 -8.76E-05 -1.06E-04 -1.25E-04 -7.29E-04 -1.02E-04
          1.12E-04  3.45E-04 -3.37E-04  1.63E-04  5.34E-04 -1.45E-04  1.47E-04 -5.14E-04  1.17E-05
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.33E-04 -5.77E-04  2.55E-04  2.56E-05 -2.15E-04 -1.94E-04  1.94E-04  6.05E-04 -5.64E-05 -7.23E-04  2.95E-04  1.35E-04
         -6.96E-05 -1.61E-04  5.92E-05 -2.97E-04  6.17E-04 -1.76E-04 -5.18E-05  2.49E-07 -4.51E-04 -2.79E-04 -3.34E-04  4.05E-04
         2.55E-04 -1.06E-04 -2.30E-04 -2.01E-04  1.44E-04  2.82E-04 -4.95E-04  3.37E-04 -5.41E-05  1.17E-05 -7.95E-04 -1.94E-04
          1.02E-04  1.84E-04 -8.87E-05  1.93E-04 -2.57E-05 -3.24E-04 -1.97E-04 -6.46E-04  3.83E-06  0.00E+00  1.83E-05
 
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
+        3.48E-01
 
 TH 2
+       -4.13E-01  2.23E-01
 
 TH 3
+        3.82E-01 -2.85E-01  2.30E-01
 
 TH 4
+        7.44E-01 -3.55E-01 -3.68E-02  3.09E-01
 
 TH 5
+       -7.28E-01  5.43E-01 -3.75E-02 -7.40E-01  2.15E-01
 
 TH 6
+       -6.52E-01  3.57E-01 -3.06E-01 -4.64E-01  5.74E-01  2.66E-01
 
 TH 7
+       -2.90E-01 -1.19E-01  2.64E-01 -4.79E-01  3.64E-01  2.70E-01  1.50E-01
 
 TH 8
+        7.87E-01 -4.63E-01  2.17E-01  7.88E-01 -7.17E-01 -5.54E-01 -1.86E-01  3.80E-01
 
 OM11
+       -5.75E-01  1.66E-01 -6.02E-01 -2.04E-01  3.76E-01  3.89E-01  2.33E-02 -3.24E-01  1.50E-01
 
 OM12
+       -2.74E-01  6.08E-01  2.93E-02 -4.02E-01  4.68E-01  2.92E-01 -8.88E-02 -6.39E-01 -1.23E-01  2.22E-01
 
 OM13
+       -2.97E-01 -3.71E-01  1.19E-01 -2.44E-01  1.54E-01  1.07E-01  2.87E-01 -2.18E-03  4.31E-01 -5.64E-01  1.05E-01
 
 OM14
+       -2.66E-01 -1.69E-01  1.75E-01 -4.53E-01  2.07E-01 -1.50E-01  5.17E-01 -2.58E-01  4.37E-02 -1.81E-01  3.51E-01  8.67E-02
 
 OM15
+        2.48E-01 -1.86E-01 -1.23E-01  3.82E-01 -2.22E-01  2.07E-01 -3.14E-01  1.78E-01  1.84E-02  3.87E-02 -1.10E-01 -6.91E-01
          1.40E-01
 
 OM16
+       -4.35E-01  5.70E-01 -5.50E-02 -5.06E-01  5.35E-01  1.82E-01  1.26E-01 -3.42E-01  1.80E-01  3.47E-01 -2.42E-02  2.01E-01
         -5.74E-01  1.12E-01
 
 OM17
+        5.64E-02 -7.10E-02 -5.78E-01  4.80E-01 -2.05E-01  2.01E-02 -3.12E-01  4.01E-01  5.85E-01 -4.89E-01  1.88E-01 -3.53E-01
          2.85E-01 -1.58E-01  1.61E-01
 
 OM18
+       -4.38E-01  3.84E-01 -7.65E-01 -1.25E-02  2.92E-01  5.26E-01 -1.85E-01 -2.54E-01  7.21E-01  1.60E-01 -9.10E-02 -3.36E-01
          2.56E-01  8.15E-02  6.69E-01  2.01E-01
 
 OM22
+       -2.87E-01 -3.51E-01 -5.42E-01  1.21E-02 -1.38E-01  2.45E-01  6.66E-02  1.12E-01  4.94E-01 -6.76E-01  5.28E-01  7.61E-02
          1.20E-01 -2.31E-01  5.15E-01  3.17E-01  2.80E-01
 
 OM23
+        1.57E-01  2.03E-01  5.19E-01 -2.64E-01  1.90E-01 -1.79E-01  2.12E-01 -2.46E-01 -5.22E-01  5.29E-01 -5.00E-01  2.50E-01
         -3.32E-01  2.10E-01 -6.68E-01 -4.39E-01 -7.56E-01  1.23E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -2.72E-01 -1.01E-01 -4.46E-01  4.97E-02 -3.79E-02  3.85E-01 -2.69E-01 -1.30E-01  3.08E-01  8.35E-02  1.67E-02 -5.26E-01
          6.40E-01 -2.58E-01  2.98E-01  4.91E-01  4.53E-01 -5.54E-01  1.31E-01
 
 OM25
+        9.70E-02  3.29E-03 -5.22E-02 -6.58E-02 -4.35E-02 -9.80E-02  2.30E-01  5.31E-02  4.94E-02 -2.15E-01  4.77E-02  3.23E-01
         -3.62E-01  1.89E-01 -1.68E-02 -2.10E-01  7.27E-02  2.12E-01 -5.34E-01  9.08E-02
 
 OM26
+       -4.24E-01  6.48E-01 -5.21E-01 -2.69E-01  4.73E-01  6.29E-01 -1.18E-01 -4.52E-01  3.87E-01  5.42E-01 -3.58E-01 -4.36E-01
          2.14E-01  3.95E-01  1.55E-01  6.71E-01 -5.18E-02 -4.41E-03  3.88E-01 -1.20E-01  1.62E-01
 
 OM27
+       -3.69E-01  7.02E-01 -1.46E-01 -4.37E-01  5.91E-01  2.10E-01  8.31E-02 -5.79E-01  2.10E-01  6.31E-01 -3.38E-01  7.38E-02
         -3.41E-01  5.86E-01 -1.90E-01  2.70E-01 -4.92E-01  4.78E-01 -3.10E-01  2.24E-01  5.58E-01  1.05E-01
 
 OM28
+       -7.34E-01  4.90E-01 -5.12E-01 -5.84E-01  5.58E-01  5.00E-01  3.33E-02 -8.33E-01  5.15E-01  5.77E-01 -6.95E-02  1.58E-02
         -2.83E-02  3.78E-01 -1.10E-01  4.65E-01  7.86E-02 -2.98E-02  3.88E-01 -6.24E-04  5.85E-01  5.43E-01  1.47E-01
 
 OM33
+        7.04E-01 -6.26E-01  5.16E-02  7.00E-01 -7.97E-01 -5.14E-01 -2.40E-01  8.85E-01 -2.34E-01 -7.63E-01  1.37E-01 -1.94E-01
          2.74E-01 -5.14E-01  4.31E-01 -2.26E-01  3.88E-01 -3.82E-01  3.83E-02  5.58E-02 -4.86E-01 -7.22E-01 -7.37E-01  1.80E-01
 
 OM34
+        3.66E-01 -5.85E-01  3.68E-01  3.27E-01 -3.79E-01 -5.69E-01  2.48E-01  5.54E-01 -1.67E-01 -6.69E-01  3.93E-01  4.44E-01
         -3.29E-01 -1.90E-01  9.34E-02 -5.04E-01  1.35E-01 -1.99E-02 -4.75E-01  3.43E-01 -7.64E-01 -3.93E-01 -6.01E-01  5.07E-01
         1.03E-01
 
 OM35
+        9.43E-03  2.88E-01 -4.38E-01  1.26E-01 -3.00E-02  4.10E-01 -3.32E-01 -1.35E-01  7.72E-02  2.09E-01 -3.59E-01 -4.66E-01
          5.61E-01 -2.78E-01  2.18E-01  4.98E-01  1.11E-01 -1.57E-01  3.91E-01 -2.70E-01  4.95E-01  3.09E-04  1.59E-01 -7.49E-03
        -7.00E-01  8.62E-02
 
 OM36
+        3.15E-01  9.86E-02 -4.05E-01  3.71E-01 -4.38E-01 -4.34E-01 -5.61E-01  2.66E-01  3.36E-03  5.02E-02 -4.58E-01 -3.70E-01
          1.35E-01  1.11E-01  3.23E-01  1.69E-01 -2.80E-02 -6.42E-02  2.20E-01  2.73E-02  2.34E-01  9.22E-02  8.89E-02  3.01E-01
        -1.23E-01  9.04E-02  1.34E-01
 
 OM37
+       -5.41E-01  4.62E-01 -6.60E-01 -2.65E-01  2.96E-01  4.88E-01 -2.69E-01 -4.98E-01  5.81E-01  3.33E-01 -3.42E-03 -3.31E-01
          3.00E-01  2.14E-01  2.51E-01  6.59E-01  2.60E-01 -4.44E-01  6.19E-01 -3.04E-01  6.70E-01  2.51E-01  7.26E-01 -3.62E-01
        -7.58E-01  5.26E-01  2.63E-01  1.18E-01
 
 OM38
+        4.68E-01 -5.88E-01  1.01E-01  4.41E-01 -5.29E-01 -2.11E-01 -9.99E-02  6.08E-01 -3.71E-02 -6.81E-01  4.15E-01 -1.41E-01
          3.60E-01 -4.48E-01  3.71E-01 -1.36E-01  3.96E-01 -4.03E-01  6.60E-02 -1.17E-03 -3.42E-01 -5.82E-01 -5.68E-01  7.84E-01
         3.20E-01  1.35E-01  1.94E-02 -1.29E-01  5.90E-02
 
 OM44
+       -4.22E-01 -2.73E-01  3.59E-01 -4.75E-01  3.10E-01  7.84E-02  3.45E-01 -3.06E-01  1.36E-01 -7.28E-02  6.27E-01  3.60E-01
         -9.03E-02 -2.82E-02 -2.68E-01 -2.45E-01  1.45E-01 -8.78E-02  1.75E-01 -2.12E-01 -3.38E-01 -1.77E-01  1.71E-01 -2.22E-01
         1.87E-01 -4.61E-01 -4.32E-01 -2.88E-02 -6.39E-02  1.68E-01
 
 OM45
+       -3.85E-01  6.41E-01 -3.34E-01 -3.35E-01  4.82E-01  6.04E-01 -7.43E-02 -6.20E-01  1.87E-01  7.52E-01 -4.06E-01 -2.67E-01
          1.93E-01  2.92E-01 -1.83E-01  4.60E-01 -2.96E-01  1.64E-01  2.26E-01 -1.17E-01  8.11E-01  5.80E-01  6.15E-01 -6.65E-01
        -8.29E-01  5.42E-01  4.63E-03  6.25E-01 -4.12E-01 -3.00E-01  1.65E-01
 
 OM46
+       -3.76E-01 -1.55E-01 -2.41E-03 -3.38E-01  2.68E-01  3.18E-01  5.56E-01 -2.30E-02  3.12E-01 -5.03E-01  6.46E-01  4.09E-01
         -3.83E-01  2.58E-01  4.85E-02 -9.00E-02  5.35E-01 -2.61E-01 -1.91E-01  4.32E-01 -1.15E-01 -1.12E-01 -1.91E-02  2.29E-02
         3.59E-01 -3.51E-01 -4.95E-01 -1.42E-01  1.74E-01  2.92E-01 -2.66E-01  1.53E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.33E-01  4.17E-01 -1.13E-01 -2.14E-01  4.68E-01  4.09E-01  4.11E-01 -2.08E-01  3.42E-01  7.90E-02  9.60E-02  2.05E-01
         -3.56E-01  4.32E-01  7.33E-02  2.81E-01 -7.07E-02  4.11E-03 -3.25E-01  2.08E-01  2.92E-01  4.66E-01  1.92E-01 -4.25E-01
        -5.25E-02 -2.66E-02 -3.79E-01  9.21E-02 -2.37E-01 -1.87E-01  3.37E-01  4.23E-01  9.44E-02
 
 OM48
+       -6.45E-01  1.09E-01 -6.09E-01 -2.62E-01  2.86E-01  4.24E-01  1.21E-01 -4.45E-01  5.85E-01 -4.25E-02  2.44E-01  1.57E-01
          8.33E-02 -9.96E-02  2.85E-01  5.64E-01  5.43E-01 -4.81E-01  5.27E-01 -2.38E-01  2.56E-01  5.55E-02  5.55E-01 -2.80E-01
        -2.34E-01  2.11E-01 -8.70E-02  5.35E-01 -1.95E-01  2.96E-01  1.86E-01  1.20E-01  1.71E-01  9.30E-02
 
 OM55
+       -6.00E-01  4.96E-01 -2.10E-01 -6.19E-01  6.18E-01  4.19E-01  1.69E-01 -8.26E-01  3.74E-01  7.07E-01 -1.15E-01  4.89E-02
         -7.55E-02  4.25E-01 -3.04E-01  2.73E-01 -2.53E-01  2.56E-01  1.56E-01  9.96E-02  5.44E-01  6.95E-01  8.77E-01 -8.34E-01
        -5.09E-01 -2.15E-02  5.22E-03  5.07E-01 -6.56E-01  2.21E-01  6.22E-01 -6.72E-02  2.39E-01  3.07E-01  3.06E-01
 
 OM56
+       -2.92E-01  5.91E-01 -5.93E-01 -2.13E-01  1.97E-01  2.71E-01 -3.87E-01 -4.10E-01  2.45E-01  4.37E-01 -4.18E-01 -2.75E-01
          1.95E-02  3.66E-01  1.43E-01  5.08E-01 -1.73E-02 -1.72E-02  2.51E-01  4.34E-02  6.59E-01  5.14E-01  5.56E-01 -3.46E-01
        -6.68E-01  4.60E-01  4.91E-01  6.28E-01 -3.09E-01 -3.83E-01  5.90E-01 -3.09E-01  2.85E-02  2.34E-01  4.24E-01  8.22E-02
 
 OM57
+        1.26E-02 -8.18E-02 -6.07E-01  3.43E-01 -1.48E-01  4.46E-02 -2.21E-01  3.22E-01  5.00E-01 -5.46E-01  2.66E-01 -1.98E-01
          1.73E-01 -1.34E-01  7.84E-01  5.09E-01  6.16E-01 -7.15E-01  1.86E-01  1.38E-01  1.05E-01 -1.38E-01 -9.10E-02  4.40E-01
         8.18E-02  2.39E-01  1.95E-01  2.64E-01  4.59E-01 -2.69E-01 -1.44E-01  2.55E-01  8.57E-02  3.06E-01 -3.11E-01  1.93E-01
          1.02E-01
 
 OM58
+        5.10E-01 -4.02E-01 -2.70E-01  6.97E-01 -5.95E-01 -2.46E-01 -2.52E-01  7.17E-01  1.11E-01 -7.01E-01  1.47E-01 -2.77E-01
          4.09E-01 -4.68E-01  6.83E-01  1.14E-01  4.81E-01 -5.28E-01  7.72E-02  3.10E-01 -2.35E-01 -5.07E-01 -4.67E-01  8.02E-01
         3.85E-01  1.62E-01  2.68E-01 -1.33E-01  6.73E-01 -3.50E-01 -4.08E-01  1.09E-01 -1.56E-01 -7.29E-02 -5.97E-01 -1.81E-01
          6.84E-01  1.41E-01
 
 OM66
+       -5.95E-01  4.16E-01 -2.52E-01 -5.49E-01  5.41E-01  6.64E-01  1.99E-01 -5.69E-01  1.85E-01  3.65E-01 -3.81E-02  1.10E-02
          5.37E-02  2.28E-01 -2.10E-01  3.37E-01  1.26E-01  2.26E-02  3.01E-01 -3.27E-01  5.65E-01  2.07E-01  4.54E-01 -4.77E-01
        -6.43E-01  4.76E-01 -2.58E-01  5.29E-01 -2.09E-01  4.59E-02  5.96E-01  1.46E-01  2.35E-01  3.52E-01  3.02E-01  2.18E-01
         -1.29E-01 -4.30E-01  1.55E-01
 
 OM67
+        4.13E-01 -7.34E-01  2.76E-01  3.00E-01 -6.20E-01 -4.31E-01 -4.52E-02  4.26E-01 -3.94E-01 -4.17E-01  1.46E-01  6.47E-02
          1.86E-01 -5.27E-01 -1.30E-01 -5.02E-01  2.45E-01 -6.65E-02  1.51E-01 -9.65E-02 -5.86E-01 -7.14E-01 -4.24E-01  5.95E-01
         4.28E-01 -2.13E-01  1.32E-01 -3.77E-01  3.77E-01  2.43E-01 -5.78E-01 -8.22E-02 -6.75E-01 -1.65E-01 -4.59E-01 -4.63E-01
         -1.74E-01  2.59E-01 -2.81E-01  1.02E-01
 
 OM68
+        8.42E-01 -2.89E-01  1.96E-01  7.53E-01 -7.11E-01 -5.98E-01 -4.09E-01  8.26E-01 -4.40E-01 -2.60E-01 -3.56E-01 -4.42E-01
          2.54E-01 -2.05E-01  2.50E-01 -2.43E-01 -1.87E-01  3.00E-02 -5.98E-02 -1.69E-02 -1.85E-01 -3.54E-01 -6.44E-01  7.21E-01
         2.98E-01 -1.94E-02  5.71E-01 -3.77E-01  3.93E-01 -4.59E-01 -3.74E-01 -3.72E-01 -3.87E-01 -5.88E-01 -5.85E-01 -1.49E-01
          8.58E-02  5.14E-01 -5.44E-01  4.05E-01  2.00E-01
 
 OM77
+       -1.69E-01  3.12E-01 -1.55E-01 -5.75E-02  1.68E-01  4.00E-02 -3.77E-01 -4.37E-01  5.50E-02  6.05E-01 -4.25E-01 -2.16E-01
          2.10E-01 -1.01E-01 -7.67E-02  2.74E-01 -3.46E-01  2.28E-01  2.74E-01 -2.58E-01  2.45E-01  2.75E-01  4.38E-01 -4.25E-01
        -4.13E-01  2.30E-01  2.05E-01  3.20E-01 -4.95E-01  4.45E-02  3.53E-01 -6.44E-01 -3.02E-01  2.02E-01  4.46E-01  3.51E-01
         -2.53E-01 -3.04E-01  8.66E-02 -1.24E-01 -1.59E-01  1.45E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.08E-01  1.86E-01 -3.09E-01  5.86E-01 -1.80E-01  2.90E-02 -4.89E-01  3.12E-01  1.71E-01  9.12E-02 -4.18E-01 -6.19E-01
          4.41E-01 -1.81E-01  6.33E-01  5.99E-01 -1.38E-01 -1.67E-01  2.22E-01 -2.15E-01  3.53E-01  9.60E-02 -1.28E-01  1.89E-01
        -2.64E-01  4.85E-01  3.36E-01  2.03E-01  1.49E-01 -5.53E-01  2.22E-01 -4.73E-01  9.89E-02 -3.99E-02 -1.75E-01  2.65E-01
          3.79E-01  4.02E-01 -1.10E-01 -3.37E-01  3.96E-01  3.17E-01  1.29E-01
 
 OM88
+       -6.13E-01  6.09E-01 -6.07E-01 -3.18E-01  5.86E-01  6.63E-01 -6.58E-02 -6.25E-01  6.07E-01  5.31E-01 -2.06E-01 -2.38E-01
          1.54E-01  2.86E-01  2.63E-01  8.43E-01  1.05E-02 -1.08E-01  3.71E-01 -1.61E-01  8.06E-01  5.94E-01  7.33E-01 -6.39E-01
        -7.23E-01  4.94E-01  2.43E-02  7.38E-01 -3.91E-01 -1.58E-01  7.71E-01 -1.31E-01  4.15E-01  5.29E-01  6.21E-01  6.18E-01
          1.93E-01 -2.62E-01  5.37E-01 -7.06E-01 -5.10E-01  4.23E-01  4.21E-01  2.62E-01
 
 SG11
+        8.18E-01 -5.84E-01  1.62E-01  8.59E-01 -7.94E-01 -5.62E-01 -3.39E-01  8.70E-01 -3.33E-01 -5.26E-01 -6.27E-02 -3.37E-01
          3.47E-01 -4.99E-01  3.31E-01 -2.58E-01  6.99E-02 -2.73E-01  2.49E-03  1.67E-02 -4.72E-01 -5.82E-01 -7.05E-01  8.42E-01
         4.59E-01 -3.72E-02  2.95E-01 -4.03E-01  6.16E-01 -3.02E-01 -5.18E-01 -1.67E-01 -3.27E-01 -3.92E-01 -6.96E-01 -3.62E-01
          3.21E-01  7.11E-01 -6.32E-01  4.64E-01  7.78E-01 -2.92E-01  3.31E-01 -5.72E-01  3.43E-03
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        8.95E-02 -6.05E-01  2.60E-01  1.94E-02 -2.35E-01 -1.70E-01  3.02E-01  3.72E-01 -8.78E-02 -7.61E-01  6.55E-01  3.64E-01
         -1.16E-01 -3.36E-01  8.63E-02 -3.46E-01  5.16E-01 -3.34E-01 -9.23E-02  6.42E-04 -6.52E-01 -6.25E-01 -5.32E-01  5.26E-01
         5.78E-01 -2.88E-01 -4.03E-01 -3.97E-01  5.72E-01  3.92E-01 -7.03E-01  5.15E-01 -1.34E-01  2.95E-02 -6.09E-01 -5.53E-01
          2.34E-01  3.05E-01 -1.34E-01  4.41E-01 -3.00E-02 -5.22E-01 -3.56E-01 -5.76E-01  2.62E-01  0.00E+00  4.27E-03
 
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
+       -5.90E+01  2.05E+01  5.46E+02
 
 TH 4
+       -3.02E+01 -1.20E+01  6.09E+01  2.83E+02
 
 TH 5
+       -8.10E+01 -1.30E+02 -7.25E+00  2.18E+01  3.64E+02
 
 TH 6
+       -2.04E+01 -7.70E+01 -8.18E+01 -5.55E+01  1.22E+02  2.91E+02
 
 TH 7
+        5.32E+01  1.53E+02 -1.83E+01  1.13E+02 -8.59E+01 -8.23E+01  3.63E+02
 
 TH 8
+       -1.99E+02 -2.87E+02 -1.00E+02 -9.86E+01  1.24E+02  1.50E+02 -2.18E+02  5.56E+02
 
 OM11
+        8.20E+01  3.21E+01 -1.02E+02  1.47E+01  2.38E+01  1.18E+02  1.10E+02 -4.02E+01  9.92E+02
 
 OM12
+        7.76E+01  4.60E+01  4.59E+01  4.29E+02  1.05E+02  5.65E+01  3.51E+02 -3.25E+02  8.71E+02  3.51E+03
 
 OM13
+       -2.32E+02 -3.56E+01 -2.83E+02  5.53E+01 -1.46E+02  1.13E+01  1.85E+02  2.08E+02 -9.17E+02 -8.02E+02  3.59E+03
 
 OM14
+        3.84E+01  4.76E+02  1.09E+02  9.12E+01 -9.64E+01  7.63E+01  1.15E+02 -1.49E+02 -3.23E+02  3.59E+02  3.69E+02  2.64E+03
 
 OM15
+        8.04E+01  1.68E+02 -1.80E+02 -9.09E+01 -4.37E+02 -3.76E+02 -1.37E+01 -1.21E+02 -6.98E+02 -1.77E+03  1.05E+03 -1.95E+02
          2.77E+03
 
 OM16
+        2.45E+02  6.79E+01 -2.87E+01  8.54E+01 -3.49E+02 -3.23E+02  1.27E+02 -2.49E+02  2.02E+02 -4.12E+02 -6.66E+02 -8.33E+02
          8.81E+02  1.90E+03
 
 OM17
+        1.63E+02  1.90E+02  1.90E+02  9.99E+01  1.89E+01  1.68E+02  1.08E+02 -2.12E+02  5.17E+02  1.93E+03 -1.47E+03  1.05E+03
         -1.59E+03 -3.88E+02  2.82E+03
 
 OM18
+       -7.23E+01 -2.69E+02  1.43E+02 -1.09E+02 -6.45E+01 -2.45E+02 -3.17E+02  2.29E+02 -1.11E+03 -2.88E+03  9.83E+02 -8.88E+02
          1.99E+03  6.23E+02 -2.38E+03  4.05E+03
 
 OM22
+        9.38E+01  1.78E+02  2.05E+01  2.65E+02 -4.07E+01 -3.34E+01  2.74E+02 -3.27E+02  3.55E+02  2.13E+03 -2.13E+02  1.57E+02
         -8.01E+02 -2.19E+02  9.81E+02 -1.75E+03  2.01E+03
 
 OM23
+        1.75E+01 -1.29E+02 -2.02E+02 -1.15E+02 -1.69E+02  1.87E+02 -2.24E+02  2.71E+02 -5.56E+02 -1.09E+03  1.76E+03 -4.15E+02
          8.56E+02 -6.59E+01 -8.26E+02  1.53E+03 -3.06E+02  3.76E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.82E+02  6.53E+02 -1.23E+02 -6.40E+01 -8.42E+01  1.20E+02  1.82E+02 -4.81E+02  2.16E+02  3.49E+02 -4.19E+02  1.91E+03
         -8.27E+02 -2.90E+02  1.46E+03 -1.10E+03 -1.45E+02 -3.46E+02  4.51E+03
 
 OM25
+        1.12E+02  8.67E+01 -2.02E+02 -5.19E+01 -2.31E+02 -4.52E+02  1.61E+01 -2.26E+02 -6.32E+02 -1.82E+03  5.85E+02 -8.33E+02
          2.90E+03  1.05E+03 -1.91E+03  2.29E+03 -1.14E+03 -2.78E+02 -6.04E+02  6.18E+03
 
 OM26
+        1.82E+01 -3.29E+01  1.21E+02  1.31E+02 -3.77E+02 -4.57E+02  2.40E+02 -6.03E+01 -3.16E+01 -4.24E+02 -2.99E+02 -2.96E+02
          9.98E+02  1.31E+03 -6.15E+02  7.63E+02 -5.66E+02 -7.02E+02 -1.10E+03  1.95E+03  3.66E+03
 
 OM27
+        2.39E+02  1.52E+02 -2.12E+02  1.54E+02  1.10E+02  3.44E+02  2.23E+02 -1.60E+02  6.11E+02  1.98E+03 -5.98E+02  1.44E+03
         -1.85E+03 -6.34E+02  2.09E+03 -2.32E+03  1.15E+03 -9.45E+02  2.81E+03 -2.42E+03 -1.51E+03  4.43E+03
 
 OM28
+       -3.69E+02 -4.66E+02  1.52E+02 -3.93E+02 -1.27E+02 -7.85E+01 -4.00E+02  8.91E+02 -9.28E+02 -4.28E+03  1.03E+03 -1.04E+03
          2.35E+03  8.80E+02 -2.57E+03  4.18E+03 -3.19E+03  1.42E+03 -2.18E+03  2.67E+03  2.41E+03 -3.54E+03  8.49E+03
 
 OM33
+       -1.15E+02 -5.93E+01  5.15E+01  1.52E+02 -5.82E+00  1.20E+01 -3.61E+00 -1.30E+02  2.96E+01 -8.50E+00  2.22E+02  6.71E+01
          9.25E+01  4.52E+02  1.67E+02 -4.60E+02 -3.18E+02 -1.34E+02  2.25E+02 -1.29E+02  1.51E+02  4.00E+02  1.55E+02  2.90E+03
 
 OM34
+        1.20E+02 -1.10E+02  2.64E+01 -2.72E+02  7.90E+01  1.16E+02 -1.96E+02  1.65E+02  2.28E+01 -4.85E+02 -1.86E+02 -8.37E+02
          1.63E+02  2.51E+02 -3.76E+02  1.85E+02 -3.48E+02  2.29E+02 -1.80E+02  9.38E+02  1.96E+02 -3.42E+02  8.59E+02  6.63E+02
         3.03E+03
 
 OM35
+       -1.75E+02 -1.31E+02  8.76E+01  1.02E+02  2.50E+02  5.88E+01  1.07E+02  2.87E+02  6.24E+02  9.02E+02 -9.28E+02  4.76E+01
         -1.23E+03  3.69E+02  7.66E+02 -1.31E+03 -9.26E+01 -2.00E+03  7.43E+02 -4.93E+02  7.67E+02  1.23E+03 -2.33E+02  9.05E+02
         9.93E+02  4.25E+03
 
 OM36
+       -3.32E+01  1.67E+02  1.21E+02  7.47E+01  1.07E+02  1.89E+01  1.65E+02 -1.17E+02 -2.71E+01  1.51E+02  2.94E+02  1.20E+02
          2.22E+02 -1.74E+02 -3.51E+02 -1.25E+01  1.51E+01 -7.46E+02  7.26E+01  5.15E+02  1.25E+02 -2.78E+02 -1.17E+02 -9.41E+01
        -2.49E+02  1.15E+03  1.91E+03
 
 OM37
+        1.79E+02 -2.79E+02 -4.02E+01 -1.69E+02  1.28E+02  1.88E+02 -2.60E+02  8.46E+01 -7.10E+02 -6.80E+02  9.81E+02 -3.49E+02
          7.21E+02 -4.38E+02 -6.50E+02  1.20E+03 -2.85E+02  2.47E+03 -2.40E+02  1.14E+03 -5.58E+02 -5.03E+02  4.41E+02 -1.24E+02
         1.39E+03 -1.84E+03 -9.18E+02  4.24E+03
 
 OM38
+        1.58E+02  2.72E+02 -1.35E+02  1.39E+02  3.19E+02 -1.40E+02  6.16E+01 -1.67E+02  9.11E+02  1.48E+03 -3.56E+03 -1.23E+02
         -1.33E+03  3.71E+02  1.32E+03 -8.29E+02  7.69E+02 -2.97E+03  6.54E+02 -1.49E+02  4.55E+02  7.13E+02 -1.92E+03 -2.44E+03
        -1.74E+03  8.51E+02  4.44E+02 -3.05E+03  9.41E+03
 
 OM44
+        6.76E+01 -1.17E+01 -1.86E+02  1.40E+02 -1.09E+02 -5.72E+01  1.19E+02 -1.12E+02  2.16E+02  5.02E+02 -3.32E+02 -3.04E+02
          1.85E+01  3.70E+02  9.29E+01 -2.63E+02  4.76E+02 -1.50E+02 -3.69E+02  2.25E+02  5.25E+02  8.99E+01 -3.47E+02  1.49E+02
         1.38E+02  3.04E+02 -6.33E+00 -3.25E+02  3.49E+02  7.48E+02
 
 OM45
+       -1.02E+02 -7.76E+01  9.80E+01 -1.79E+02  1.18E+02  1.34E+02 -2.20E+02  2.56E+02 -1.49E+02 -8.92E+02  1.05E+02 -4.17E+02
          1.92E+01 -7.00E+01 -1.59E+02  6.25E+02 -3.95E+02  7.95E+02 -2.92E+02 -6.18E+02 -1.03E+03 -2.50E+02  1.10E+03  3.16E+02
         5.24E+02  5.46E+01  7.40E+01  1.76E+02 -1.06E+03 -1.52E+02  1.55E+03
 
 OM46
+        6.14E+01  1.16E+02  1.47E+02 -9.72E+01  1.51E+02  2.57E+01 -1.36E+02  8.94E+00 -2.43E+02 -1.74E+02  1.42E+02  5.45E+02
         -1.62E+02 -5.85E+02  1.93E+02  6.78E+01 -3.58E+02  1.19E+02  5.96E+02 -1.17E+03 -1.23E+03  3.82E+02 -3.42E+02  3.47E+01
        -2.82E+02  4.64E+01  4.34E+02  1.58E+02 -4.30E+02 -5.73E+02  6.20E+02  1.72E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.31E+02  2.09E+02 -1.92E+02  2.37E+02 -2.42E+02 -1.59E+02  2.17E+02 -4.48E+02  1.55E+02  1.03E+03 -2.77E+02  5.45E+02
          8.76E+01  2.51E+02  6.00E+02 -6.12E+02  7.97E+02 -3.26E+02  1.07E+03  1.82E+02  4.90E+02  8.68E+02 -1.61E+03  2.43E+02
        -8.25E+02  1.72E+02  2.60E+02 -8.81E+02  1.31E+03  6.88E+02 -8.52E+02 -7.02E+02  2.58E+03
 
 OM48
+       -1.69E+02 -4.96E+02  2.05E+02 -1.94E+02  2.72E+02  6.77E+00 -4.18E+02  3.67E+02 -1.00E+02 -7.69E+02 -1.10E+02 -1.77E+03
          3.71E+02  2.00E+02 -1.03E+03  1.28E+03 -6.20E+02  7.37E+02 -2.33E+03  7.04E+02 -2.88E+02 -2.06E+03  1.56E+03 -6.09E+02
         2.59E+02 -1.08E+03 -3.90E+02  1.07E+03  3.65E+00 -4.69E+02  6.16E+02  3.47E+02 -1.73E+03  3.39E+03
 
 OM55
+       -2.04E+02 -1.30E+02  1.35E+02  5.74E+01  2.29E+02  1.46E+02 -1.67E+02  1.72E+02  1.33E+02  8.05E+02 -3.88E+02  1.08E+02
         -1.18E+03 -6.01E+02  6.89E+02 -9.83E+02  6.21E+02 -3.34E+02 -2.74E+02 -1.77E+03 -1.12E+03  4.94E+02 -1.54E+03 -1.34E+01
        -1.90E+02  1.39E+02 -3.47E+02 -4.49E+02  4.67E+02 -1.35E+02  2.04E+02  2.62E+02 -2.89E+02  2.76E+02  1.22E+03
 
 OM56
+       -3.30E+02 -4.33E+02  9.40E+01  1.45E+02  2.34E+02  1.93E+02 -7.92E+01  3.33E+02  9.63E+01  7.44E+02  5.92E+02  5.40E+00
         -1.26E+03 -1.18E+03  3.62E+02 -5.34E+02  5.56E+02  7.74E+02 -1.03E+03 -2.72E+03 -1.37E+03  3.72E+02 -9.20E+02 -2.03E+02
        -1.68E+02 -1.35E+03 -1.51E+03  3.50E+02 -9.86E+02 -4.89E+01  1.06E+02  1.69E+02 -4.19E+02  6.14E+02  1.36E+03  4.40E+03
 
 OM57
+        1.41E+01  1.60E+02  1.11E+02 -2.17E+02 -3.60E+02 -1.17E+02 -1.13E+02 -2.13E+01 -5.60E+02 -1.71E+03  6.63E+02 -1.50E+02
          1.65E+03  3.72E+02 -1.12E+03  1.66E+03 -9.25E+02  1.31E+03 -1.82E+02  1.97E+03  4.62E+02 -1.97E+03  2.33E+03 -4.09E+02
         2.54E+02 -1.07E+03  1.22E+02  7.34E+02 -1.02E+03 -1.51E+02  2.97E+02 -2.35E+02  2.66E+01  3.95E+02 -7.38E+02 -9.81E+02
          2.68E+03
 
 OM58
+       -7.57E+01 -2.64E+02  3.31E+02  2.38E+02  3.06E+02  3.53E+02  6.73E+01  3.74E+00  8.05E+02  2.49E+03 -9.91E+02  5.82E+02
         -3.50E+03 -9.72E+02  1.99E+03 -2.25E+03  1.32E+03 -1.06E+02  9.87E+02 -5.59E+03 -1.52E+03  2.76E+03 -3.53E+03 -4.15E+02
        -1.26E+03  2.26E+02 -9.21E+02 -8.12E+02  1.03E+03 -6.88E+01 -1.96E+02  4.90E+02  2.53E+02 -5.93E+02  1.70E+03  3.07E+03
         -2.42E+03  6.82E+03
 
 OM66
+       -1.35E+02 -2.10E+02  8.66E+00  2.47E+01  4.77E+01  9.64E+01 -9.56E+01  1.23E+02  2.17E+01  2.29E+02  1.71E+02 -1.85E+02
         -3.74E+02 -5.10E+02  1.50E+02 -1.65E+02  3.19E+02  1.24E+02 -5.66E+02 -3.93E+02 -7.28E+02  2.44E+01 -4.52E+02 -3.40E+02
         2.51E+01 -8.67E+02 -7.17E+02  1.22E+02 -1.07E+02  3.82E+01 -4.07E+01 -3.12E+02 -1.55E+02  4.30E+02  6.27E+02  1.65E+03
         -1.36E+02  7.49E+02  1.17E+03
 
 OM67
+        1.47E+02  3.57E+02  1.72E+02 -1.46E+02 -8.13E+01 -2.16E+02  5.67E+01 -1.91E+02  3.35E+01 -4.85E+02 -5.42E+02  1.37E+02
          3.46E+02  5.45E+02 -4.67E+01  9.68E+01 -5.50E+02 -5.28E+02  2.47E+02  5.39E+02  8.31E+02 -5.29E+02  7.71E+02 -7.52E+01
         2.21E+02  3.31E+02  7.78E+01 -2.74E+02  3.36E+02 -1.76E+02 -1.02E+02  2.40E+02 -1.82E+02  6.75E+01 -3.26E+02 -7.13E+02
          5.18E+02 -6.90E+02 -5.22E+02  1.63E+03
 
 OM68
+       -2.01E+02 -7.99E+01 -1.16E+02  4.77E+00  2.61E+02  2.75E+02 -1.26E+02  3.06E+01 -9.41E+01  6.47E+02  6.45E+02  3.40E+02
         -7.65E+02 -1.41E+03  2.54E+02 -7.05E+02  8.60E+02  3.69E+02 -1.09E+02 -1.28E+03 -2.34E+03  7.67E+02 -1.95E+03 -5.82E+02
        -5.05E+02 -1.15E+03 -4.89E+02  4.20E+02 -2.06E+02 -1.89E+02  3.06E+02  4.24E+02 -1.27E+02  3.45E+02  1.03E+03  1.83E+03
         -4.53E+02  1.34E+03  1.05E+03 -9.55E+02  2.64E+03
 
 OM77
+        4.89E+01  6.88E+01 -1.25E+02  1.03E+02 -5.98E+01  2.67E+01  1.37E+02 -5.06E+01  1.51E+02  5.41E+02 -1.46E+02  4.60E+02
         -3.22E+02  2.87E+01  5.38E+02 -5.03E+02  4.01E+02 -3.83E+02  6.77E+02 -6.74E+02 -8.75E+01  1.05E+03 -9.11E+02  1.03E+02
        -6.45E+02  3.94E+02  1.49E+02 -8.25E+02  9.91E+02  1.79E+02 -1.83E+02  1.84E+00  9.40E+02 -9.15E+02  4.01E+01 -8.72E+01
         -4.82E+02  6.38E+02 -9.50E+01 -3.16E+02  1.19E+02  8.77E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -2.40E+02 -1.04E+02  5.54E+01 -4.04E+02  1.81E+01 -1.92E+02 -1.36E+02  2.72E+02 -6.34E+02 -2.18E+03  1.08E+03 -1.17E+03
          1.76E+03  2.79E+02 -2.22E+03  1.91E+03 -1.19E+03  7.10E+02 -2.41E+03  2.45E+03  8.21E+02 -3.20E+03  3.60E+03 -4.90E+02
         1.20E+03 -8.98E+02  1.20E+02  1.23E+03 -1.88E+03 -3.72E+02  6.80E+02  4.50E+01 -2.04E+03  2.26E+03 -4.91E+02 -3.45E+02
          1.70E+03 -3.14E+03  2.69E+01  9.90E+02 -5.59E+02 -1.53E+03  4.99E+03
 
 OM88
+        1.50E+02  4.07E+02 -5.33E+01  1.47E+02 -6.77E+01  1.41E+01  2.27E+02 -4.08E+02  3.57E+02  1.77E+03  7.79E+01  9.58E+02
         -8.96E+02 -5.07E+02  1.10E+03 -2.54E+03  1.32E+03 -1.04E+03  1.11E+03 -1.33E+03 -9.42E+02  1.61E+03 -3.59E+03  8.26E+02
        -2.07E+02  5.64E+02  3.21E+02 -7.40E+02 -4.68E+02  9.28E+01 -5.39E+02  3.18E+02  6.48E+02 -1.40E+03  6.01E+02  1.27E+02
         -1.21E+03  1.20E+03 -6.09E+01 -1.41E+02  8.58E+02  4.68E+02 -1.86E+03  2.91E+03
 
 SG11
+       -1.70E+03  6.62E+03 -1.96E+03 -1.12E+03 -2.79E+03  1.81E+03  9.21E+03 -2.71E+03  5.63E+03  1.80E+04  1.77E+04  6.89E+03
         -8.75E+03 -5.31E+03  9.92E+03 -1.92E+04  2.32E+04  1.27E+04 -1.95E+04 -1.57E+04  1.88E+04  5.15E+03 -8.64E+03 -1.02E+04
        -3.57E+02 -9.48E+03 -8.71E+03 -1.11E+02 -1.47E+04  1.04E+04 -9.60E+03 -2.05E+04  6.53E+03 -1.30E+04  2.83E+02  2.48E+04
         -1.57E+04  1.71E+04  1.13E+04 -2.83E+03  5.14E+03  6.25E+03 -4.47E+03  8.75E+03  2.41E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.63E+03 -3.22E+03  2.68E+03  4.31E+03  2.06E+03  8.39E+02 -1.41E+03  3.95E+03  6.28E+03  6.38E+03 -6.45E+03 -8.77E+03
         -6.06E+03  5.44E+03  5.80E+02 -2.23E+03 -1.44E+02 -2.81E+03 -1.94E+04 -5.97E+03  1.01E+04 -5.93E+03  1.41E+04  2.32E+03
         2.20E+03  1.61E+04  4.36E+03 -8.17E+03 -5.42E+03  3.00E+03  6.30E+03 -2.77E+03 -6.76E+03  7.31E+03  3.74E+03  7.06E+02
         -9.47E+03  6.91E+03 -2.30E+03  3.13E+02 -2.11E+03 -9.75E+02 -1.15E+03 -4.45E+02  1.82E+05  0.00E+00  8.37E+05
 
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
 RAW OUTPUT FILE (FILE): example6.txt
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
 CONVERGENCE INTERVAL (CINTERVAL):          10
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 BURN-IN ITERATIONS (NBURN):                4000
 ITERATIONS (NITER):                        10000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          1
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0
 SAMPLES FOR MASS/IMP/POST. MATRIX SEARCH (ISAMPLE_M1B): 2
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           1
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       1
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
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           36
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):36
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
 iteration        -4000 MCMCOBJ=   -6810.70450199400     
 iteration        -3990 MCMCOBJ=   -6694.73455706606     
 iteration        -3980 MCMCOBJ=   -6719.62099716167     
 iteration        -3970 MCMCOBJ=   -6694.62810019649     
 iteration        -3960 MCMCOBJ=   -6704.48282350269     
 iteration        -3950 MCMCOBJ=   -6664.77351421632     
 iteration        -3940 MCMCOBJ=   -6641.78085013210     
 iteration        -3930 MCMCOBJ=   -6650.55259137232     
 iteration        -3920 MCMCOBJ=   -6628.18524967277     
 iteration        -3910 MCMCOBJ=   -6653.52881663299     
 iteration        -3900 MCMCOBJ=   -6631.03591014291     
 iteration        -3890 MCMCOBJ=   -6636.47845044987     
 iteration        -3880 MCMCOBJ=   -6634.94849675361     
 iteration        -3870 MCMCOBJ=   -6653.40937230755     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -6664.84311327507     
 iteration           10 MCMCOBJ=   -6645.86934512538     
 iteration           20 MCMCOBJ=   -6581.16030835291     
 iteration           30 MCMCOBJ=   -6593.25215416714     
 iteration           40 MCMCOBJ=   -6595.34766972153     
 iteration           50 MCMCOBJ=   -6562.70435620770     
 iteration           60 MCMCOBJ=   -6594.04112891762     
 iteration           70 MCMCOBJ=   -6630.48149982445     
 iteration           80 MCMCOBJ=   -6634.64548455322     
 iteration           90 MCMCOBJ=   -6603.02847707618     
 iteration          100 MCMCOBJ=   -6572.23167619780     
 iteration          110 MCMCOBJ=   -6635.59055100729     
 iteration          120 MCMCOBJ=   -6574.84730099662     
 iteration          130 MCMCOBJ=   -6559.56542389304     
 iteration          140 MCMCOBJ=   -6568.21119940435     
 iteration          150 MCMCOBJ=   -6587.76468563527     
 iteration          160 MCMCOBJ=   -6580.04132487074     
 iteration          170 MCMCOBJ=   -6581.22420270885     
 iteration          180 MCMCOBJ=   -6555.83677573115     
 iteration          190 MCMCOBJ=   -6591.11612568538     
 iteration          200 MCMCOBJ=   -6585.89937826911     
 iteration          210 MCMCOBJ=   -6543.51659231548     
 iteration          220 MCMCOBJ=   -6582.76136502649     
 iteration          230 MCMCOBJ=   -6637.88178845847     
 iteration          240 MCMCOBJ=   -6538.64676752277     
 iteration          250 MCMCOBJ=   -6567.86667141275     
 iteration          260 MCMCOBJ=   -6565.69292407906     
 iteration          270 MCMCOBJ=   -6589.20106647825     
 iteration          280 MCMCOBJ=   -6544.43655321458     
 iteration          290 MCMCOBJ=   -6577.04736315342     
 iteration          300 MCMCOBJ=   -6530.77566901512     
 iteration          310 MCMCOBJ=   -6583.64330097917     
 iteration          320 MCMCOBJ=   -6551.82205606997     
 iteration          330 MCMCOBJ=   -6563.75986400560     
 iteration          340 MCMCOBJ=   -6548.14787968336     
 iteration          350 MCMCOBJ=   -6589.35182201842     
 iteration          360 MCMCOBJ=   -6540.60803594004     
 iteration          370 MCMCOBJ=   -6561.61211975076     
 iteration          380 MCMCOBJ=   -6592.69200689265     
 iteration          390 MCMCOBJ=   -6534.92152401439     
 iteration          400 MCMCOBJ=   -6488.65086650225     
 iteration          410 MCMCOBJ=   -6480.11906778973     
 iteration          420 MCMCOBJ=   -6504.85951373681     
 iteration          430 MCMCOBJ=   -6499.15941594464     
 iteration          440 MCMCOBJ=   -6551.96029945080     
 iteration          450 MCMCOBJ=   -6566.12782110885     
 iteration          460 MCMCOBJ=   -6589.47721131753     
 iteration          470 MCMCOBJ=   -6559.76227333664     
 iteration          480 MCMCOBJ=   -6547.68366966862     
 iteration          490 MCMCOBJ=   -6494.50034350235     
 iteration          500 MCMCOBJ=   -6560.05189854891     
 iteration          510 MCMCOBJ=   -6576.49291766519     
 iteration          520 MCMCOBJ=   -6540.17695575378     
 iteration          530 MCMCOBJ=   -6491.23439878827     
 iteration          540 MCMCOBJ=   -6568.19717402712     
 iteration          550 MCMCOBJ=   -6506.94168781099     
 iteration          560 MCMCOBJ=   -6567.28739911711     
 iteration          570 MCMCOBJ=   -6542.63496022980     
 iteration          580 MCMCOBJ=   -6530.41833520288     
 iteration          590 MCMCOBJ=   -6552.84043282187     
 iteration          600 MCMCOBJ=   -6547.98758537126     
 iteration          610 MCMCOBJ=   -6482.92424380831     
 iteration          620 MCMCOBJ=   -6541.49035599805     
 iteration          630 MCMCOBJ=   -6539.09861264028     
 iteration          640 MCMCOBJ=   -6496.94375632466     
 iteration          650 MCMCOBJ=   -6563.70669447859     
 iteration          660 MCMCOBJ=   -6549.68850830357     
 iteration          670 MCMCOBJ=   -6481.39954977126     
 iteration          680 MCMCOBJ=   -6491.49937438400     
 iteration          690 MCMCOBJ=   -6518.34692699764     
 iteration          700 MCMCOBJ=   -6451.19947004047     
 iteration          710 MCMCOBJ=   -6547.45733117508     
 iteration          720 MCMCOBJ=   -6512.31352489261     
 iteration          730 MCMCOBJ=   -6534.00812788921     
 iteration          740 MCMCOBJ=   -6481.92770450384     
 iteration          750 MCMCOBJ=   -6529.02121551336     
 iteration          760 MCMCOBJ=   -6492.84482635688     
 iteration          770 MCMCOBJ=   -6530.16247667059     
 iteration          780 MCMCOBJ=   -6603.97609094522     
 iteration          790 MCMCOBJ=   -6543.10150132692     
 iteration          800 MCMCOBJ=   -6526.57172815055     
 iteration          810 MCMCOBJ=   -6522.25337052085     
 iteration          820 MCMCOBJ=   -6528.82279814125     
 iteration          830 MCMCOBJ=   -6544.03303923848     
 iteration          840 MCMCOBJ=   -6496.67207538070     
 iteration          850 MCMCOBJ=   -6516.68486877619     
 iteration          860 MCMCOBJ=   -6462.08659781688     
 iteration          870 MCMCOBJ=   -6496.63128642618     
 iteration          880 MCMCOBJ=   -6444.69695192100     
 iteration          890 MCMCOBJ=   -6524.83140879775     
 iteration          900 MCMCOBJ=   -6484.86223704844     
 iteration          910 MCMCOBJ=   -6491.93260534880     
 iteration          920 MCMCOBJ=   -6525.60112535579     
 iteration          930 MCMCOBJ=   -6505.97518766317     
 iteration          940 MCMCOBJ=   -6533.91715481485     
 iteration          950 MCMCOBJ=   -6523.78331104870     
 iteration          960 MCMCOBJ=   -6429.75759444612     
 iteration          970 MCMCOBJ=   -6460.32150962266     
 iteration          980 MCMCOBJ=   -6514.40046537528     
 iteration          990 MCMCOBJ=   -6512.24949056045     
 iteration         1000 MCMCOBJ=   -6482.08614625737     
 iteration         1010 MCMCOBJ=   -6528.24112121321     
 iteration         1020 MCMCOBJ=   -6531.65734264039     
 iteration         1030 MCMCOBJ=   -6478.76764797913     
 iteration         1040 MCMCOBJ=   -6499.19576988650     
 iteration         1050 MCMCOBJ=   -6472.10618869139     
 iteration         1060 MCMCOBJ=   -6488.14968282716     
 iteration         1070 MCMCOBJ=   -6459.28058381758     
 iteration         1080 MCMCOBJ=   -6530.74038854037     
 iteration         1090 MCMCOBJ=   -6500.88608039380     
 iteration         1100 MCMCOBJ=   -6525.86384537808     
 iteration         1110 MCMCOBJ=   -6587.61083247443     
 iteration         1120 MCMCOBJ=   -6540.62604162187     
 iteration         1130 MCMCOBJ=   -6536.19137147468     
 iteration         1140 MCMCOBJ=   -6507.86355733887     
 iteration         1150 MCMCOBJ=   -6535.50992643848     
 iteration         1160 MCMCOBJ=   -6520.24009493642     
 iteration         1170 MCMCOBJ=   -6494.66797658081     
 iteration         1180 MCMCOBJ=   -6520.77976651174     
 iteration         1190 MCMCOBJ=   -6539.45760518285     
 iteration         1200 MCMCOBJ=   -6484.56779903916     
 iteration         1210 MCMCOBJ=   -6477.64012274373     
 iteration         1220 MCMCOBJ=   -6560.47219908067     
 iteration         1230 MCMCOBJ=   -6487.71319311336     
 iteration         1240 MCMCOBJ=   -6523.22594215735     
 iteration         1250 MCMCOBJ=   -6492.76247506612     
 iteration         1260 MCMCOBJ=   -6534.23018030183     
 iteration         1270 MCMCOBJ=   -6481.16120393208     
 iteration         1280 MCMCOBJ=   -6512.22312805616     
 iteration         1290 MCMCOBJ=   -6539.06630366638     
 iteration         1300 MCMCOBJ=   -6498.86076076834     
 iteration         1310 MCMCOBJ=   -6531.55426851601     
 iteration         1320 MCMCOBJ=   -6487.96741011940     
 iteration         1330 MCMCOBJ=   -6506.40050218613     
 iteration         1340 MCMCOBJ=   -6477.09641442833     
 iteration         1350 MCMCOBJ=   -6534.94475115242     
 iteration         1360 MCMCOBJ=   -6456.22231589401     
 iteration         1370 MCMCOBJ=   -6484.70634385679     
 iteration         1380 MCMCOBJ=   -6481.34456726388     
 iteration         1390 MCMCOBJ=   -6503.69299456515     
 iteration         1400 MCMCOBJ=   -6517.41959087517     
 iteration         1410 MCMCOBJ=   -6606.65809336667     
 iteration         1420 MCMCOBJ=   -6510.25439459502     
 iteration         1430 MCMCOBJ=   -6516.90751174590     
 iteration         1440 MCMCOBJ=   -6512.29029605927     
 iteration         1450 MCMCOBJ=   -6462.82065926100     
 iteration         1460 MCMCOBJ=   -6408.54740654679     
 iteration         1470 MCMCOBJ=   -6555.30134848536     
 iteration         1480 MCMCOBJ=   -6489.78645956602     
 iteration         1490 MCMCOBJ=   -6455.82596060335     
 iteration         1500 MCMCOBJ=   -6483.99130054233     
 iteration         1510 MCMCOBJ=   -6480.73814221572     
 iteration         1520 MCMCOBJ=   -6554.97608412630     
 iteration         1530 MCMCOBJ=   -6520.26895340004     
 iteration         1540 MCMCOBJ=   -6457.79838070105     
 iteration         1550 MCMCOBJ=   -6508.06108377639     
 iteration         1560 MCMCOBJ=   -6545.55134808745     
 iteration         1570 MCMCOBJ=   -6481.28218058544     
 iteration         1580 MCMCOBJ=   -6512.37132227537     
 iteration         1590 MCMCOBJ=   -6477.37924274211     
 iteration         1600 MCMCOBJ=   -6501.71834061832     
 iteration         1610 MCMCOBJ=   -6518.84732239551     
 iteration         1620 MCMCOBJ=   -6500.81947829235     
 iteration         1630 MCMCOBJ=   -6491.52253254334     
 iteration         1640 MCMCOBJ=   -6506.38559342475     
 iteration         1650 MCMCOBJ=   -6501.05489715344     
 iteration         1660 MCMCOBJ=   -6502.96605827959     
 iteration         1670 MCMCOBJ=   -6482.98499822667     
 iteration         1680 MCMCOBJ=   -6480.21752789504     
 iteration         1690 MCMCOBJ=   -6493.25702227403     
 iteration         1700 MCMCOBJ=   -6481.33606991581     
 iteration         1710 MCMCOBJ=   -6528.54352851285     
 iteration         1720 MCMCOBJ=   -6513.05972254606     
 iteration         1730 MCMCOBJ=   -6543.66585372425     
 iteration         1740 MCMCOBJ=   -6400.45034991702     
 iteration         1750 MCMCOBJ=   -6552.74932953974     
 iteration         1760 MCMCOBJ=   -6526.57032041029     
 iteration         1770 MCMCOBJ=   -6561.92642799294     
 iteration         1780 MCMCOBJ=   -6518.51271144078     
 iteration         1790 MCMCOBJ=   -6490.76147589010     
 iteration         1800 MCMCOBJ=   -6462.01804004242     
 iteration         1810 MCMCOBJ=   -6520.68599520025     
 iteration         1820 MCMCOBJ=   -6477.18902120976     
 iteration         1830 MCMCOBJ=   -6518.24554038790     
 iteration         1840 MCMCOBJ=   -6460.98799179723     
 iteration         1850 MCMCOBJ=   -6472.69816459007     
 iteration         1860 MCMCOBJ=   -6550.33706553556     
 iteration         1870 MCMCOBJ=   -6514.54552770155     
 iteration         1880 MCMCOBJ=   -6496.77275963026     
 iteration         1890 MCMCOBJ=   -6481.13848296470     
 iteration         1900 MCMCOBJ=   -6485.56334740002     
 iteration         1910 MCMCOBJ=   -6488.44578950020     
 iteration         1920 MCMCOBJ=   -6517.00167962482     
 iteration         1930 MCMCOBJ=   -6463.96176649783     
 iteration         1940 MCMCOBJ=   -6441.21295327604     
 iteration         1950 MCMCOBJ=   -6527.78021294598     
 iteration         1960 MCMCOBJ=   -6531.45851886644     
 iteration         1970 MCMCOBJ=   -6541.89979921795     
 iteration         1980 MCMCOBJ=   -6537.21985444739     
 iteration         1990 MCMCOBJ=   -6513.06720689000     
 iteration         2000 MCMCOBJ=   -6544.50289509031     
 iteration         2010 MCMCOBJ=   -6464.91078783246     
 iteration         2020 MCMCOBJ=   -6472.94489086524     
 iteration         2030 MCMCOBJ=   -6496.72187313524     
 iteration         2040 MCMCOBJ=   -6507.26338449352     
 iteration         2050 MCMCOBJ=   -6452.14743479109     
 iteration         2060 MCMCOBJ=   -6476.22602289542     
 iteration         2070 MCMCOBJ=   -6482.63093684090     
 iteration         2080 MCMCOBJ=   -6556.07388447351     
 iteration         2090 MCMCOBJ=   -6525.46154906808     
 iteration         2100 MCMCOBJ=   -6496.58756596959     
 iteration         2110 MCMCOBJ=   -6528.18905345719     
 iteration         2120 MCMCOBJ=   -6521.21343746043     
 iteration         2130 MCMCOBJ=   -6513.73288683994     
 iteration         2140 MCMCOBJ=   -6517.52220239938     
 iteration         2150 MCMCOBJ=   -6528.85329382224     
 iteration         2160 MCMCOBJ=   -6531.98343666636     
 iteration         2170 MCMCOBJ=   -6511.54541314782     
 iteration         2180 MCMCOBJ=   -6480.36097809495     
 iteration         2190 MCMCOBJ=   -6499.66854853812     
 iteration         2200 MCMCOBJ=   -6429.60210994788     
 iteration         2210 MCMCOBJ=   -6522.48562828279     
 iteration         2220 MCMCOBJ=   -6436.36174647004     
 iteration         2230 MCMCOBJ=   -6502.35443227176     
 iteration         2240 MCMCOBJ=   -6476.48539467155     
 iteration         2250 MCMCOBJ=   -6451.98790979776     
 iteration         2260 MCMCOBJ=   -6570.23901842125     
 iteration         2270 MCMCOBJ=   -6409.56042832792     
 iteration         2280 MCMCOBJ=   -6476.41891188883     
 iteration         2290 MCMCOBJ=   -6471.05233329527     
 iteration         2300 MCMCOBJ=   -6477.17087793368     
 iteration         2310 MCMCOBJ=   -6494.11550041347     
 iteration         2320 MCMCOBJ=   -6481.74036975837     
 iteration         2330 MCMCOBJ=   -6500.71700283882     
 iteration         2340 MCMCOBJ=   -6516.67413490137     
 iteration         2350 MCMCOBJ=   -6413.88247168320     
 iteration         2360 MCMCOBJ=   -6529.40076446957     
 iteration         2370 MCMCOBJ=   -6465.98919116836     
 iteration         2380 MCMCOBJ=   -6492.13137326078     
 iteration         2390 MCMCOBJ=   -6466.08816286837     
 iteration         2400 MCMCOBJ=   -6466.07677417346     
 iteration         2410 MCMCOBJ=   -6514.19770563407     
 iteration         2420 MCMCOBJ=   -6456.11383938124     
 iteration         2430 MCMCOBJ=   -6478.19081937249     
 iteration         2440 MCMCOBJ=   -6488.36393517347     
 iteration         2450 MCMCOBJ=   -6467.39076797741     
 iteration         2460 MCMCOBJ=   -6475.42460579355     
 iteration         2470 MCMCOBJ=   -6534.38614449271     
 iteration         2480 MCMCOBJ=   -6470.50312082414     
 iteration         2490 MCMCOBJ=   -6512.02246722699     
 iteration         2500 MCMCOBJ=   -6403.16257789673     
 iteration         2510 MCMCOBJ=   -6520.73180276077     
 iteration         2520 MCMCOBJ=   -6519.40129997077     
 iteration         2530 MCMCOBJ=   -6489.69981665281     
 iteration         2540 MCMCOBJ=   -6443.24401790854     
 iteration         2550 MCMCOBJ=   -6490.57353100079     
 iteration         2560 MCMCOBJ=   -6484.49522433562     
 iteration         2570 MCMCOBJ=   -6508.49900808775     
 iteration         2580 MCMCOBJ=   -6442.34970263003     
 iteration         2590 MCMCOBJ=   -6473.54911443600     
 iteration         2600 MCMCOBJ=   -6447.15968340217     
 iteration         2610 MCMCOBJ=   -6421.32704207422     
 iteration         2620 MCMCOBJ=   -6522.12160325573     
 iteration         2630 MCMCOBJ=   -6482.51103115027     
 iteration         2640 MCMCOBJ=   -6478.08773992592     
 iteration         2650 MCMCOBJ=   -6507.45770502058     
 iteration         2660 MCMCOBJ=   -6495.91417855107     
 iteration         2670 MCMCOBJ=   -6533.55796602207     
 iteration         2680 MCMCOBJ=   -6468.12613141779     
 iteration         2690 MCMCOBJ=   -6461.24259240450     
 iteration         2700 MCMCOBJ=   -6459.61667736093     
 iteration         2710 MCMCOBJ=   -6514.20685438329     
 iteration         2720 MCMCOBJ=   -6418.17402180809     
 iteration         2730 MCMCOBJ=   -6443.58553938277     
 iteration         2740 MCMCOBJ=   -6458.38261910837     
 iteration         2750 MCMCOBJ=   -6448.70577876064     
 iteration         2760 MCMCOBJ=   -6500.51777165931     
 iteration         2770 MCMCOBJ=   -6452.38777901673     
 iteration         2780 MCMCOBJ=   -6492.25347702771     
 iteration         2790 MCMCOBJ=   -6489.76336965856     
 iteration         2800 MCMCOBJ=   -6475.32905882563     
 iteration         2810 MCMCOBJ=   -6509.83691757540     
 iteration         2820 MCMCOBJ=   -6465.97063649026     
 iteration         2830 MCMCOBJ=   -6525.33716387675     
 iteration         2840 MCMCOBJ=   -6455.18782599602     
 iteration         2850 MCMCOBJ=   -6506.20097901171     
 iteration         2860 MCMCOBJ=   -6496.66856954090     
 iteration         2870 MCMCOBJ=   -6510.61304516385     
 iteration         2880 MCMCOBJ=   -6551.69281545809     
 iteration         2890 MCMCOBJ=   -6532.82130107340     
 iteration         2900 MCMCOBJ=   -6527.54097253036     
 iteration         2910 MCMCOBJ=   -6487.43693882550     
 iteration         2920 MCMCOBJ=   -6469.75203406924     
 iteration         2930 MCMCOBJ=   -6512.69394857329     
 iteration         2940 MCMCOBJ=   -6533.10462440098     
 iteration         2950 MCMCOBJ=   -6508.45803705767     
 iteration         2960 MCMCOBJ=   -6490.69934906987     
 iteration         2970 MCMCOBJ=   -6461.23352864118     
 iteration         2980 MCMCOBJ=   -6461.19105829808     
 iteration         2990 MCMCOBJ=   -6471.04781478406     
 iteration         3000 MCMCOBJ=   -6482.31102533211     
 iteration         3010 MCMCOBJ=   -6456.62906439144     
 iteration         3020 MCMCOBJ=   -6472.21399618157     
 iteration         3030 MCMCOBJ=   -6524.57904421189     
 iteration         3040 MCMCOBJ=   -6512.19958835410     
 iteration         3050 MCMCOBJ=   -6503.40818399293     
 iteration         3060 MCMCOBJ=   -6472.26510010583     
 iteration         3070 MCMCOBJ=   -6501.78258426046     
 iteration         3080 MCMCOBJ=   -6422.77235262982     
 iteration         3090 MCMCOBJ=   -6522.47360344734     
 iteration         3100 MCMCOBJ=   -6530.21261313055     
 iteration         3110 MCMCOBJ=   -6547.67637660145     
 iteration         3120 MCMCOBJ=   -6533.25511834045     
 iteration         3130 MCMCOBJ=   -6515.84409337039     
 iteration         3140 MCMCOBJ=   -6512.58479105493     
 iteration         3150 MCMCOBJ=   -6501.89584589006     
 iteration         3160 MCMCOBJ=   -6503.64633436073     
 iteration         3170 MCMCOBJ=   -6468.22110336834     
 iteration         3180 MCMCOBJ=   -6537.20017016738     
 iteration         3190 MCMCOBJ=   -6395.83972642405     
 iteration         3200 MCMCOBJ=   -6491.26230568897     
 iteration         3210 MCMCOBJ=   -6492.32464725440     
 iteration         3220 MCMCOBJ=   -6409.51221277844     
 iteration         3230 MCMCOBJ=   -6545.99288854872     
 iteration         3240 MCMCOBJ=   -6539.81800068717     
 iteration         3250 MCMCOBJ=   -6509.67639497486     
 iteration         3260 MCMCOBJ=   -6514.14242118575     
 iteration         3270 MCMCOBJ=   -6518.87674112256     
 iteration         3280 MCMCOBJ=   -6553.47088925226     
 iteration         3290 MCMCOBJ=   -6465.07476352821     
 iteration         3300 MCMCOBJ=   -6460.87997077473     
 iteration         3310 MCMCOBJ=   -6512.69857795917     
 iteration         3320 MCMCOBJ=   -6451.99387776906     
 iteration         3330 MCMCOBJ=   -6474.40574116282     
 iteration         3340 MCMCOBJ=   -6504.83346353644     
 iteration         3350 MCMCOBJ=   -6502.20639875187     
 iteration         3360 MCMCOBJ=   -6457.03165355977     
 iteration         3370 MCMCOBJ=   -6513.67864599321     
 iteration         3380 MCMCOBJ=   -6498.16032765861     
 iteration         3390 MCMCOBJ=   -6435.50178131890     
 iteration         3400 MCMCOBJ=   -6409.58722732682     
 iteration         3410 MCMCOBJ=   -6443.84304097825     
 iteration         3420 MCMCOBJ=   -6480.79616196635     
 iteration         3430 MCMCOBJ=   -6523.74978910637     
 iteration         3440 MCMCOBJ=   -6475.24680708704     
 iteration         3450 MCMCOBJ=   -6535.39635672232     
 iteration         3460 MCMCOBJ=   -6487.34485768237     
 iteration         3470 MCMCOBJ=   -6508.81695479616     
 iteration         3480 MCMCOBJ=   -6490.51936995681     
 iteration         3490 MCMCOBJ=   -6495.04294376393     
 iteration         3500 MCMCOBJ=   -6446.40815170122     
 iteration         3510 MCMCOBJ=   -6523.84349377084     
 iteration         3520 MCMCOBJ=   -6486.87526056017     
 iteration         3530 MCMCOBJ=   -6562.76474165155     
 iteration         3540 MCMCOBJ=   -6504.94651334147     
 iteration         3550 MCMCOBJ=   -6493.80116456442     
 iteration         3560 MCMCOBJ=   -6486.51231065481     
 iteration         3570 MCMCOBJ=   -6503.62656040279     
 iteration         3580 MCMCOBJ=   -6443.20146093731     
 iteration         3590 MCMCOBJ=   -6464.23642135206     
 iteration         3600 MCMCOBJ=   -6442.25062293542     
 iteration         3610 MCMCOBJ=   -6484.97930617456     
 iteration         3620 MCMCOBJ=   -6452.73652514246     
 iteration         3630 MCMCOBJ=   -6458.38938978793     
 iteration         3640 MCMCOBJ=   -6422.93268126461     
 iteration         3650 MCMCOBJ=   -6430.78689863492     
 iteration         3660 MCMCOBJ=   -6537.95033824522     
 iteration         3670 MCMCOBJ=   -6481.94563432227     
 iteration         3680 MCMCOBJ=   -6497.64010569760     
 iteration         3690 MCMCOBJ=   -6491.82822640302     
 iteration         3700 MCMCOBJ=   -6505.38578195854     
 iteration         3710 MCMCOBJ=   -6496.93860846411     
 iteration         3720 MCMCOBJ=   -6501.70011628350     
 iteration         3730 MCMCOBJ=   -6463.12299896795     
 iteration         3740 MCMCOBJ=   -6502.37132482360     
 iteration         3750 MCMCOBJ=   -6504.43621473564     
 iteration         3760 MCMCOBJ=   -6467.96111948521     
 iteration         3770 MCMCOBJ=   -6437.15089504056     
 iteration         3780 MCMCOBJ=   -6535.81284681996     
 iteration         3790 MCMCOBJ=   -6426.91186822247     
 iteration         3800 MCMCOBJ=   -6525.78182848813     
 iteration         3810 MCMCOBJ=   -6518.83624665004     
 iteration         3820 MCMCOBJ=   -6521.44500724416     
 iteration         3830 MCMCOBJ=   -6513.83160799883     
 iteration         3840 MCMCOBJ=   -6494.69451228809     
 iteration         3850 MCMCOBJ=   -6505.14355746213     
 iteration         3860 MCMCOBJ=   -6543.55172510563     
 iteration         3870 MCMCOBJ=   -6484.72897826383     
 iteration         3880 MCMCOBJ=   -6492.50933180621     
 iteration         3890 MCMCOBJ=   -6520.15000833722     
 iteration         3900 MCMCOBJ=   -6524.07962270089     
 iteration         3910 MCMCOBJ=   -6505.12897244349     
 iteration         3920 MCMCOBJ=   -6463.57278263476     
 iteration         3930 MCMCOBJ=   -6522.49098994389     
 iteration         3940 MCMCOBJ=   -6488.58830120314     
 iteration         3950 MCMCOBJ=   -6475.15171746624     
 iteration         3960 MCMCOBJ=   -6517.70232635649     
 iteration         3970 MCMCOBJ=   -6513.19601046856     
 iteration         3980 MCMCOBJ=   -6544.48653754223     
 iteration         3990 MCMCOBJ=   -6507.22956452843     
 iteration         4000 MCMCOBJ=   -6436.77233623836     
 iteration         4010 MCMCOBJ=   -6502.64118367095     
 iteration         4020 MCMCOBJ=   -6502.19532609553     
 iteration         4030 MCMCOBJ=   -6503.90714788465     
 iteration         4040 MCMCOBJ=   -6479.06069345202     
 iteration         4050 MCMCOBJ=   -6552.47419107980     
 iteration         4060 MCMCOBJ=   -6518.83568559194     
 iteration         4070 MCMCOBJ=   -6498.31204359446     
 iteration         4080 MCMCOBJ=   -6479.47852795191     
 iteration         4090 MCMCOBJ=   -6476.98905697057     
 iteration         4100 MCMCOBJ=   -6497.37096646769     
 iteration         4110 MCMCOBJ=   -6517.63868823845     
 iteration         4120 MCMCOBJ=   -6495.26081276757     
 iteration         4130 MCMCOBJ=   -6467.54128399554     
 iteration         4140 MCMCOBJ=   -6398.08215239040     
 iteration         4150 MCMCOBJ=   -6503.41766052537     
 iteration         4160 MCMCOBJ=   -6526.21457367946     
 iteration         4170 MCMCOBJ=   -6503.22254549295     
 iteration         4180 MCMCOBJ=   -6465.89119649364     
 iteration         4190 MCMCOBJ=   -6459.09834236135     
 iteration         4200 MCMCOBJ=   -6509.18505495617     
 iteration         4210 MCMCOBJ=   -6507.03967180296     
 iteration         4220 MCMCOBJ=   -6481.20485507148     
 iteration         4230 MCMCOBJ=   -6533.10894688482     
 iteration         4240 MCMCOBJ=   -6517.87029865083     
 iteration         4250 MCMCOBJ=   -6453.28150701979     
 iteration         4260 MCMCOBJ=   -6473.10595424838     
 iteration         4270 MCMCOBJ=   -6479.45778978711     
 iteration         4280 MCMCOBJ=   -6454.44108478226     
 iteration         4290 MCMCOBJ=   -6421.55257750464     
 iteration         4300 MCMCOBJ=   -6469.57367807425     
 iteration         4310 MCMCOBJ=   -6448.15669736592     
 iteration         4320 MCMCOBJ=   -6464.97230985413     
 iteration         4330 MCMCOBJ=   -6453.85274182212     
 iteration         4340 MCMCOBJ=   -6476.31312157323     
 iteration         4350 MCMCOBJ=   -6384.31869671269     
 iteration         4360 MCMCOBJ=   -6520.28093297186     
 iteration         4370 MCMCOBJ=   -6513.13433787042     
 iteration         4380 MCMCOBJ=   -6471.21851091233     
 iteration         4390 MCMCOBJ=   -6489.61602188681     
 iteration         4400 MCMCOBJ=   -6441.11407819434     
 iteration         4410 MCMCOBJ=   -6458.16029613911     
 iteration         4420 MCMCOBJ=   -6487.32901805105     
 iteration         4430 MCMCOBJ=   -6529.84583738965     
 iteration         4440 MCMCOBJ=   -6505.81569051434     
 iteration         4450 MCMCOBJ=   -6491.21652071878     
 iteration         4460 MCMCOBJ=   -6515.17920937695     
 iteration         4470 MCMCOBJ=   -6473.75411285029     
 iteration         4480 MCMCOBJ=   -6445.74922756022     
 iteration         4490 MCMCOBJ=   -6445.91040846328     
 iteration         4500 MCMCOBJ=   -6508.96188328826     
 iteration         4510 MCMCOBJ=   -6544.77166988302     
 iteration         4520 MCMCOBJ=   -6514.47652270166     
 iteration         4530 MCMCOBJ=   -6565.81803043471     
 iteration         4540 MCMCOBJ=   -6498.54651780953     
 iteration         4550 MCMCOBJ=   -6558.05341871132     
 iteration         4560 MCMCOBJ=   -6484.05993924506     
 iteration         4570 MCMCOBJ=   -6430.30022660031     
 iteration         4580 MCMCOBJ=   -6475.95867924010     
 iteration         4590 MCMCOBJ=   -6426.67892211168     
 iteration         4600 MCMCOBJ=   -6461.79579369059     
 iteration         4610 MCMCOBJ=   -6527.40459362800     
 iteration         4620 MCMCOBJ=   -6464.33373559694     
 iteration         4630 MCMCOBJ=   -6446.14022589414     
 iteration         4640 MCMCOBJ=   -6457.82791980386     
 iteration         4650 MCMCOBJ=   -6478.64874068794     
 iteration         4660 MCMCOBJ=   -6454.13591321505     
 iteration         4670 MCMCOBJ=   -6427.36751664225     
 iteration         4680 MCMCOBJ=   -6450.56431847190     
 iteration         4690 MCMCOBJ=   -6480.25829858478     
 iteration         4700 MCMCOBJ=   -6476.72573052998     
 iteration         4710 MCMCOBJ=   -6491.89164771773     
 iteration         4720 MCMCOBJ=   -6500.11759191751     
 iteration         4730 MCMCOBJ=   -6491.87240509919     
 iteration         4740 MCMCOBJ=   -6523.00127153284     
 iteration         4750 MCMCOBJ=   -6473.92097682965     
 iteration         4760 MCMCOBJ=   -6460.79058944319     
 iteration         4770 MCMCOBJ=   -6515.91166864162     
 iteration         4780 MCMCOBJ=   -6513.00113450409     
 iteration         4790 MCMCOBJ=   -6448.80362840859     
 iteration         4800 MCMCOBJ=   -6367.63027132294     
 iteration         4810 MCMCOBJ=   -6453.35876941789     
 iteration         4820 MCMCOBJ=   -6524.85354566642     
 iteration         4830 MCMCOBJ=   -6484.22869703848     
 iteration         4840 MCMCOBJ=   -6484.13858681997     
 iteration         4850 MCMCOBJ=   -6416.72950931712     
 iteration         4860 MCMCOBJ=   -6476.44425878387     
 iteration         4870 MCMCOBJ=   -6455.36981785436     
 iteration         4880 MCMCOBJ=   -6541.14229763180     
 iteration         4890 MCMCOBJ=   -6537.41880429922     
 iteration         4900 MCMCOBJ=   -6513.84186617298     
 iteration         4910 MCMCOBJ=   -6409.99511222436     
 iteration         4920 MCMCOBJ=   -6472.19164825189     
 iteration         4930 MCMCOBJ=   -6490.05735139990     
 iteration         4940 MCMCOBJ=   -6447.42920184157     
 iteration         4950 MCMCOBJ=   -6516.01922770443     
 iteration         4960 MCMCOBJ=   -6524.29244167959     
 iteration         4970 MCMCOBJ=   -6483.08729541267     
 iteration         4980 MCMCOBJ=   -6470.33565264525     
 iteration         4990 MCMCOBJ=   -6494.27378125934     
 iteration         5000 MCMCOBJ=   -6458.65082431773     
 iteration         5010 MCMCOBJ=   -6520.13055080833     
 iteration         5020 MCMCOBJ=   -6512.43781034049     
 iteration         5030 MCMCOBJ=   -6460.90071678538     
 iteration         5040 MCMCOBJ=   -6482.61696948381     
 iteration         5050 MCMCOBJ=   -6444.98732388019     
 iteration         5060 MCMCOBJ=   -6409.40803906988     
 iteration         5070 MCMCOBJ=   -6510.85938310960     
 iteration         5080 MCMCOBJ=   -6465.33483674864     
 iteration         5090 MCMCOBJ=   -6496.23402034428     
 iteration         5100 MCMCOBJ=   -6494.51642038255     
 iteration         5110 MCMCOBJ=   -6419.04396739263     
 iteration         5120 MCMCOBJ=   -6506.50531717024     
 iteration         5130 MCMCOBJ=   -6548.87295850110     
 iteration         5140 MCMCOBJ=   -6488.61087790850     
 iteration         5150 MCMCOBJ=   -6462.75873651231     
 iteration         5160 MCMCOBJ=   -6501.93263252689     
 iteration         5170 MCMCOBJ=   -6512.63312030997     
 iteration         5180 MCMCOBJ=   -6491.30213593421     
 iteration         5190 MCMCOBJ=   -6503.14933338066     
 iteration         5200 MCMCOBJ=   -6489.58077802998     
 iteration         5210 MCMCOBJ=   -6534.98583872008     
 iteration         5220 MCMCOBJ=   -6424.43755849990     
 iteration         5230 MCMCOBJ=   -6452.34223243224     
 iteration         5240 MCMCOBJ=   -6531.14212192132     
 iteration         5250 MCMCOBJ=   -6515.10449297722     
 iteration         5260 MCMCOBJ=   -6541.66482849994     
 iteration         5270 MCMCOBJ=   -6519.39638748128     
 iteration         5280 MCMCOBJ=   -6454.65995132130     
 iteration         5290 MCMCOBJ=   -6442.46808606995     
 iteration         5300 MCMCOBJ=   -6439.81901668339     
 iteration         5310 MCMCOBJ=   -6445.71598661505     
 iteration         5320 MCMCOBJ=   -6520.43997194492     
 iteration         5330 MCMCOBJ=   -6491.06203862828     
 iteration         5340 MCMCOBJ=   -6444.51517948184     
 iteration         5350 MCMCOBJ=   -6465.80948690297     
 iteration         5360 MCMCOBJ=   -6498.75525336529     
 iteration         5370 MCMCOBJ=   -6481.65177495335     
 iteration         5380 MCMCOBJ=   -6501.73967254955     
 iteration         5390 MCMCOBJ=   -6468.30369123158     
 iteration         5400 MCMCOBJ=   -6468.37403240700     
 iteration         5410 MCMCOBJ=   -6433.33956119260     
 iteration         5420 MCMCOBJ=   -6457.51588370667     
 iteration         5430 MCMCOBJ=   -6455.92300285671     
 iteration         5440 MCMCOBJ=   -6474.83482986887     
 iteration         5450 MCMCOBJ=   -6495.69020498661     
 iteration         5460 MCMCOBJ=   -6507.60545845128     
 iteration         5470 MCMCOBJ=   -6465.12636117556     
 iteration         5480 MCMCOBJ=   -6527.04853773988     
 iteration         5490 MCMCOBJ=   -6469.66396974525     
 iteration         5500 MCMCOBJ=   -6482.37009314925     
 iteration         5510 MCMCOBJ=   -6457.42358479541     
 iteration         5520 MCMCOBJ=   -6533.16621690296     
 iteration         5530 MCMCOBJ=   -6452.26457496462     
 iteration         5540 MCMCOBJ=   -6498.87248713418     
 iteration         5550 MCMCOBJ=   -6524.75783494933     
 iteration         5560 MCMCOBJ=   -6476.46587721616     
 iteration         5570 MCMCOBJ=   -6552.98635453939     
 iteration         5580 MCMCOBJ=   -6467.59468218639     
 iteration         5590 MCMCOBJ=   -6514.09060526218     
 iteration         5600 MCMCOBJ=   -6521.59513495816     
 iteration         5610 MCMCOBJ=   -6460.33463318840     
 iteration         5620 MCMCOBJ=   -6480.56904937502     
 iteration         5630 MCMCOBJ=   -6482.84131161094     
 iteration         5640 MCMCOBJ=   -6525.21294633548     
 iteration         5650 MCMCOBJ=   -6559.11396761713     
 iteration         5660 MCMCOBJ=   -6494.60793316751     
 iteration         5670 MCMCOBJ=   -6494.17917213594     
 iteration         5680 MCMCOBJ=   -6435.84978306230     
 iteration         5690 MCMCOBJ=   -6491.39552508368     
 iteration         5700 MCMCOBJ=   -6446.25630279602     
 iteration         5710 MCMCOBJ=   -6434.56163280050     
 iteration         5720 MCMCOBJ=   -6503.07996591413     
 iteration         5730 MCMCOBJ=   -6433.42864145986     
 iteration         5740 MCMCOBJ=   -6447.35021870237     
 iteration         5750 MCMCOBJ=   -6437.39510675871     
 iteration         5760 MCMCOBJ=   -6507.10309473055     
 iteration         5770 MCMCOBJ=   -6509.76140891348     
 iteration         5780 MCMCOBJ=   -6445.50307057301     
 iteration         5790 MCMCOBJ=   -6482.30894587696     
 iteration         5800 MCMCOBJ=   -6448.20244807986     
 iteration         5810 MCMCOBJ=   -6449.52189502902     
 iteration         5820 MCMCOBJ=   -6497.01485601477     
 iteration         5830 MCMCOBJ=   -6421.89038283302     
 iteration         5840 MCMCOBJ=   -6431.18426485974     
 iteration         5850 MCMCOBJ=   -6428.24224509131     
 iteration         5860 MCMCOBJ=   -6498.78667253073     
 iteration         5870 MCMCOBJ=   -6456.35030451075     
 iteration         5880 MCMCOBJ=   -6511.07977984534     
 iteration         5890 MCMCOBJ=   -6466.24368964397     
 iteration         5900 MCMCOBJ=   -6496.89740887716     
 iteration         5910 MCMCOBJ=   -6523.40911121493     
 iteration         5920 MCMCOBJ=   -6450.08213551947     
 iteration         5930 MCMCOBJ=   -6491.38657365405     
 iteration         5940 MCMCOBJ=   -6525.87455758942     
 iteration         5950 MCMCOBJ=   -6437.05950024133     
 iteration         5960 MCMCOBJ=   -6474.12142869939     
 iteration         5970 MCMCOBJ=   -6484.38983772589     
 iteration         5980 MCMCOBJ=   -6454.37495565381     
 iteration         5990 MCMCOBJ=   -6436.06800583834     
 iteration         6000 MCMCOBJ=   -6513.10095640070     
 iteration         6010 MCMCOBJ=   -6513.68251605499     
 iteration         6020 MCMCOBJ=   -6466.03879018071     
 iteration         6030 MCMCOBJ=   -6520.38994585594     
 iteration         6040 MCMCOBJ=   -6487.01024885821     
 iteration         6050 MCMCOBJ=   -6474.19231315006     
 iteration         6060 MCMCOBJ=   -6514.82665332387     
 iteration         6070 MCMCOBJ=   -6515.44311815732     
 iteration         6080 MCMCOBJ=   -6467.48873061641     
 iteration         6090 MCMCOBJ=   -6423.63010408109     
 iteration         6100 MCMCOBJ=   -6414.80681258440     
 iteration         6110 MCMCOBJ=   -6503.37524395715     
 iteration         6120 MCMCOBJ=   -6498.51359085986     
 iteration         6130 MCMCOBJ=   -6462.00827755955     
 iteration         6140 MCMCOBJ=   -6442.21968417207     
 iteration         6150 MCMCOBJ=   -6555.50975411370     
 iteration         6160 MCMCOBJ=   -6451.49348053538     
 iteration         6170 MCMCOBJ=   -6481.56017211639     
 iteration         6180 MCMCOBJ=   -6508.19534403940     
 iteration         6190 MCMCOBJ=   -6524.69479279958     
 iteration         6200 MCMCOBJ=   -6462.69877925711     
 iteration         6210 MCMCOBJ=   -6490.10178710469     
 iteration         6220 MCMCOBJ=   -6475.11957398399     
 iteration         6230 MCMCOBJ=   -6486.56805905193     
 iteration         6240 MCMCOBJ=   -6511.11481540834     
 iteration         6250 MCMCOBJ=   -6453.69513891303     
 iteration         6260 MCMCOBJ=   -6472.31340927205     
 iteration         6270 MCMCOBJ=   -6453.37564814492     
 iteration         6280 MCMCOBJ=   -6463.63836137707     
 iteration         6290 MCMCOBJ=   -6435.92458733712     
 iteration         6300 MCMCOBJ=   -6510.39555333798     
 iteration         6310 MCMCOBJ=   -6479.69171161684     
 iteration         6320 MCMCOBJ=   -6422.36272035204     
 iteration         6330 MCMCOBJ=   -6462.27912674049     
 iteration         6340 MCMCOBJ=   -6512.98864504073     
 iteration         6350 MCMCOBJ=   -6497.77412714895     
 iteration         6360 MCMCOBJ=   -6488.88012290456     
 iteration         6370 MCMCOBJ=   -6465.41784770887     
 iteration         6380 MCMCOBJ=   -6515.75242308022     
 iteration         6390 MCMCOBJ=   -6433.68618123013     
 iteration         6400 MCMCOBJ=   -6506.66627891511     
 iteration         6410 MCMCOBJ=   -6463.56542548111     
 iteration         6420 MCMCOBJ=   -6501.48325088009     
 iteration         6430 MCMCOBJ=   -6501.60846885359     
 iteration         6440 MCMCOBJ=   -6516.27302987806     
 iteration         6450 MCMCOBJ=   -6500.93256643952     
 iteration         6460 MCMCOBJ=   -6512.90640003065     
 iteration         6470 MCMCOBJ=   -6465.96126429357     
 iteration         6480 MCMCOBJ=   -6479.46577164064     
 iteration         6490 MCMCOBJ=   -6528.48126767591     
 iteration         6500 MCMCOBJ=   -6476.62138751784     
 iteration         6510 MCMCOBJ=   -6465.20066695491     
 iteration         6520 MCMCOBJ=   -6435.34453129065     
 iteration         6530 MCMCOBJ=   -6468.03643240865     
 iteration         6540 MCMCOBJ=   -6432.93777111550     
 iteration         6550 MCMCOBJ=   -6520.80262691453     
 iteration         6560 MCMCOBJ=   -6461.76899305285     
 iteration         6570 MCMCOBJ=   -6484.30637434540     
 iteration         6580 MCMCOBJ=   -6528.84937365374     
 iteration         6590 MCMCOBJ=   -6500.20243757577     
 iteration         6600 MCMCOBJ=   -6558.43854840989     
 iteration         6610 MCMCOBJ=   -6493.25127233875     
 iteration         6620 MCMCOBJ=   -6514.31546788905     
 iteration         6630 MCMCOBJ=   -6464.44341064959     
 iteration         6640 MCMCOBJ=   -6512.57323691391     
 iteration         6650 MCMCOBJ=   -6499.00032988009     
 iteration         6660 MCMCOBJ=   -6416.71352762828     
 iteration         6670 MCMCOBJ=   -6504.64226177213     
 iteration         6680 MCMCOBJ=   -6543.17432010892     
 iteration         6690 MCMCOBJ=   -6499.47521305623     
 iteration         6700 MCMCOBJ=   -6471.01669041865     
 iteration         6710 MCMCOBJ=   -6558.78366892043     
 iteration         6720 MCMCOBJ=   -6431.16766255506     
 iteration         6730 MCMCOBJ=   -6545.73025638998     
 iteration         6740 MCMCOBJ=   -6500.56916968951     
 iteration         6750 MCMCOBJ=   -6493.73779387714     
 iteration         6760 MCMCOBJ=   -6438.23012699269     
 iteration         6770 MCMCOBJ=   -6484.79556635185     
 iteration         6780 MCMCOBJ=   -6514.10780252343     
 iteration         6790 MCMCOBJ=   -6461.88391021117     
 iteration         6800 MCMCOBJ=   -6508.91176742603     
 iteration         6810 MCMCOBJ=   -6535.14802372297     
 iteration         6820 MCMCOBJ=   -6527.29645429363     
 iteration         6830 MCMCOBJ=   -6456.55044219542     
 iteration         6840 MCMCOBJ=   -6508.11156158477     
 iteration         6850 MCMCOBJ=   -6478.97933998832     
 iteration         6860 MCMCOBJ=   -6498.22401460806     
 iteration         6870 MCMCOBJ=   -6446.81629748613     
 iteration         6880 MCMCOBJ=   -6492.63798666152     
 iteration         6890 MCMCOBJ=   -6509.73680983338     
 iteration         6900 MCMCOBJ=   -6522.97797568581     
 iteration         6910 MCMCOBJ=   -6402.78834710820     
 iteration         6920 MCMCOBJ=   -6468.33240812033     
 iteration         6930 MCMCOBJ=   -6449.69872705464     
 iteration         6940 MCMCOBJ=   -6489.81357581179     
 iteration         6950 MCMCOBJ=   -6484.21144417212     
 iteration         6960 MCMCOBJ=   -6454.69564126135     
 iteration         6970 MCMCOBJ=   -6494.39332237751     
 iteration         6980 MCMCOBJ=   -6419.40711095959     
 iteration         6990 MCMCOBJ=   -6552.68361233974     
 iteration         7000 MCMCOBJ=   -6471.26988072597     
 iteration         7010 MCMCOBJ=   -6450.43414391186     
 iteration         7020 MCMCOBJ=   -6482.60338268546     
 iteration         7030 MCMCOBJ=   -6483.46069285884     
 iteration         7040 MCMCOBJ=   -6487.01909177132     
 iteration         7050 MCMCOBJ=   -6473.34116112490     
 iteration         7060 MCMCOBJ=   -6513.38519746181     
 iteration         7070 MCMCOBJ=   -6424.53535565892     
 iteration         7080 MCMCOBJ=   -6487.36752505062     
 iteration         7090 MCMCOBJ=   -6489.31046841317     
 iteration         7100 MCMCOBJ=   -6512.85287591291     
 iteration         7110 MCMCOBJ=   -6480.82718924332     
 iteration         7120 MCMCOBJ=   -6445.03305918100     
 iteration         7130 MCMCOBJ=   -6430.80335214278     
 iteration         7140 MCMCOBJ=   -6494.92732555490     
 iteration         7150 MCMCOBJ=   -6503.64672082568     
 iteration         7160 MCMCOBJ=   -6507.98444350615     
 iteration         7170 MCMCOBJ=   -6459.53651397418     
 iteration         7180 MCMCOBJ=   -6452.80958313430     
 iteration         7190 MCMCOBJ=   -6560.27810895693     
 iteration         7200 MCMCOBJ=   -6467.02177795140     
 iteration         7210 MCMCOBJ=   -6454.34872295027     
 iteration         7220 MCMCOBJ=   -6471.76343458061     
 iteration         7230 MCMCOBJ=   -6453.89847047450     
 iteration         7240 MCMCOBJ=   -6476.19696666640     
 iteration         7250 MCMCOBJ=   -6521.09704453918     
 iteration         7260 MCMCOBJ=   -6406.81614715797     
 iteration         7270 MCMCOBJ=   -6470.72366271986     
 iteration         7280 MCMCOBJ=   -6521.43276554239     
 iteration         7290 MCMCOBJ=   -6492.37533672392     
 iteration         7300 MCMCOBJ=   -6489.24360504334     
 iteration         7310 MCMCOBJ=   -6462.29632567212     
 iteration         7320 MCMCOBJ=   -6497.61308919421     
 iteration         7330 MCMCOBJ=   -6516.25342080798     
 iteration         7340 MCMCOBJ=   -6456.68858108237     
 iteration         7350 MCMCOBJ=   -6513.36243754314     
 iteration         7360 MCMCOBJ=   -6500.34435335593     
 iteration         7370 MCMCOBJ=   -6477.18379690969     
 iteration         7380 MCMCOBJ=   -6471.89463130646     
 iteration         7390 MCMCOBJ=   -6507.88294352265     
 iteration         7400 MCMCOBJ=   -6484.33668770478     
 iteration         7410 MCMCOBJ=   -6530.06432726188     
 iteration         7420 MCMCOBJ=   -6500.97654055493     
 iteration         7430 MCMCOBJ=   -6492.02777384948     
 iteration         7440 MCMCOBJ=   -6454.20739877206     
 iteration         7450 MCMCOBJ=   -6513.81681772235     
 iteration         7460 MCMCOBJ=   -6499.90105978912     
 iteration         7470 MCMCOBJ=   -6514.40949612244     
 iteration         7480 MCMCOBJ=   -6512.26838865131     
 iteration         7490 MCMCOBJ=   -6489.08809428452     
 iteration         7500 MCMCOBJ=   -6472.00638085159     
 iteration         7510 MCMCOBJ=   -6439.93548064060     
 iteration         7520 MCMCOBJ=   -6536.71770676150     
 iteration         7530 MCMCOBJ=   -6528.86382650323     
 iteration         7540 MCMCOBJ=   -6452.59200781693     
 iteration         7550 MCMCOBJ=   -6460.64190003635     
 iteration         7560 MCMCOBJ=   -6432.58766451283     
 iteration         7570 MCMCOBJ=   -6496.20747420811     
 iteration         7580 MCMCOBJ=   -6491.69581909510     
 iteration         7590 MCMCOBJ=   -6428.98116129267     
 iteration         7600 MCMCOBJ=   -6439.16481149200     
 iteration         7610 MCMCOBJ=   -6530.95749103150     
 iteration         7620 MCMCOBJ=   -6485.61643462093     
 iteration         7630 MCMCOBJ=   -6512.59155839679     
 iteration         7640 MCMCOBJ=   -6513.12180577217     
 iteration         7650 MCMCOBJ=   -6484.52983691616     
 iteration         7660 MCMCOBJ=   -6532.21498866355     
 iteration         7670 MCMCOBJ=   -6512.08573155542     
 iteration         7680 MCMCOBJ=   -6513.15730676580     
 iteration         7690 MCMCOBJ=   -6421.49066350369     
 iteration         7700 MCMCOBJ=   -6465.96002853082     
 iteration         7710 MCMCOBJ=   -6485.56759593655     
 iteration         7720 MCMCOBJ=   -6473.77772609716     
 iteration         7730 MCMCOBJ=   -6518.07316459594     
 iteration         7740 MCMCOBJ=   -6449.43871008139     
 iteration         7750 MCMCOBJ=   -6515.35645052190     
 iteration         7760 MCMCOBJ=   -6459.03716387595     
 iteration         7770 MCMCOBJ=   -6444.81271370594     
 iteration         7780 MCMCOBJ=   -6494.49043456090     
 iteration         7790 MCMCOBJ=   -6479.62843931916     
 iteration         7800 MCMCOBJ=   -6556.20073561444     
 iteration         7810 MCMCOBJ=   -6548.39697627402     
 iteration         7820 MCMCOBJ=   -6499.31837948401     
 iteration         7830 MCMCOBJ=   -6459.89824183150     
 iteration         7840 MCMCOBJ=   -6514.26938618247     
 iteration         7850 MCMCOBJ=   -6521.25978164947     
 iteration         7860 MCMCOBJ=   -6458.38014093743     
 iteration         7870 MCMCOBJ=   -6460.62316105731     
 iteration         7880 MCMCOBJ=   -6477.13974471074     
 iteration         7890 MCMCOBJ=   -6497.43824545351     
 iteration         7900 MCMCOBJ=   -6508.51819413722     
 iteration         7910 MCMCOBJ=   -6519.99046693482     
 iteration         7920 MCMCOBJ=   -6528.58019477305     
 iteration         7930 MCMCOBJ=   -6483.27445619401     
 iteration         7940 MCMCOBJ=   -6511.87963966628     
 iteration         7950 MCMCOBJ=   -6430.32987245014     
 iteration         7960 MCMCOBJ=   -6512.43954031125     
 iteration         7970 MCMCOBJ=   -6460.24212300686     
 iteration         7980 MCMCOBJ=   -6474.48055508340     
 iteration         7990 MCMCOBJ=   -6454.79917742264     
 iteration         8000 MCMCOBJ=   -6523.67828999267     
 iteration         8010 MCMCOBJ=   -6545.72430050485     
 iteration         8020 MCMCOBJ=   -6484.75367791157     
 iteration         8030 MCMCOBJ=   -6510.72093777630     
 iteration         8040 MCMCOBJ=   -6456.01769903379     
 iteration         8050 MCMCOBJ=   -6532.12074315835     
 iteration         8060 MCMCOBJ=   -6460.68241719940     
 iteration         8070 MCMCOBJ=   -6486.62262468281     
 iteration         8080 MCMCOBJ=   -6488.83690478038     
 iteration         8090 MCMCOBJ=   -6487.91699636220     
 iteration         8100 MCMCOBJ=   -6489.13135645844     
 iteration         8110 MCMCOBJ=   -6492.50289924833     
 iteration         8120 MCMCOBJ=   -6514.60267814229     
 iteration         8130 MCMCOBJ=   -6450.88248291465     
 iteration         8140 MCMCOBJ=   -6489.41240203530     
 iteration         8150 MCMCOBJ=   -6523.08708140503     
 iteration         8160 MCMCOBJ=   -6476.07283514634     
 iteration         8170 MCMCOBJ=   -6485.42353084745     
 iteration         8180 MCMCOBJ=   -6410.84195883255     
 iteration         8190 MCMCOBJ=   -6491.48056756490     
 iteration         8200 MCMCOBJ=   -6476.21638024022     
 iteration         8210 MCMCOBJ=   -6503.64365896943     
 iteration         8220 MCMCOBJ=   -6480.56059051019     
 iteration         8230 MCMCOBJ=   -6510.76303545037     
 iteration         8240 MCMCOBJ=   -6518.43564955018     
 iteration         8250 MCMCOBJ=   -6525.02224273880     
 iteration         8260 MCMCOBJ=   -6557.84250156595     
 iteration         8270 MCMCOBJ=   -6452.89223455431     
 iteration         8280 MCMCOBJ=   -6504.66826361105     
 iteration         8290 MCMCOBJ=   -6468.10515337126     
 iteration         8300 MCMCOBJ=   -6459.05156174525     
 iteration         8310 MCMCOBJ=   -6520.19976409214     
 iteration         8320 MCMCOBJ=   -6469.07334093501     
 iteration         8330 MCMCOBJ=   -6449.63281894480     
 iteration         8340 MCMCOBJ=   -6417.25662685816     
 iteration         8350 MCMCOBJ=   -6486.23277111968     
 iteration         8360 MCMCOBJ=   -6466.66085577510     
 iteration         8370 MCMCOBJ=   -6483.88950363981     
 iteration         8380 MCMCOBJ=   -6534.32033953113     
 iteration         8390 MCMCOBJ=   -6453.36376819839     
 iteration         8400 MCMCOBJ=   -6523.62245927379     
 iteration         8410 MCMCOBJ=   -6497.33355024887     
 iteration         8420 MCMCOBJ=   -6498.71544588228     
 iteration         8430 MCMCOBJ=   -6521.42635516660     
 iteration         8440 MCMCOBJ=   -6556.79769219921     
 iteration         8450 MCMCOBJ=   -6424.17727568780     
 iteration         8460 MCMCOBJ=   -6447.97329346689     
 iteration         8470 MCMCOBJ=   -6511.84391560131     
 iteration         8480 MCMCOBJ=   -6509.81080197222     
 iteration         8490 MCMCOBJ=   -6415.49346258678     
 iteration         8500 MCMCOBJ=   -6487.11141014716     
 iteration         8510 MCMCOBJ=   -6551.80179049657     
 iteration         8520 MCMCOBJ=   -6490.41421397543     
 iteration         8530 MCMCOBJ=   -6439.04598801717     
 iteration         8540 MCMCOBJ=   -6488.56386869558     
 iteration         8550 MCMCOBJ=   -6408.63000677909     
 iteration         8560 MCMCOBJ=   -6548.00141827634     
 iteration         8570 MCMCOBJ=   -6459.07268562392     
 iteration         8580 MCMCOBJ=   -6412.40446484923     
 iteration         8590 MCMCOBJ=   -6484.76982774814     
 iteration         8600 MCMCOBJ=   -6490.56082270785     
 iteration         8610 MCMCOBJ=   -6488.70138420202     
 iteration         8620 MCMCOBJ=   -6466.65172226829     
 iteration         8630 MCMCOBJ=   -6429.72103876259     
 iteration         8640 MCMCOBJ=   -6529.80679675113     
 iteration         8650 MCMCOBJ=   -6515.68098578145     
 iteration         8660 MCMCOBJ=   -6517.87699296555     
 iteration         8670 MCMCOBJ=   -6463.42134206867     
 iteration         8680 MCMCOBJ=   -6484.44966058463     
 iteration         8690 MCMCOBJ=   -6433.63692590496     
 iteration         8700 MCMCOBJ=   -6439.21366017350     
 iteration         8710 MCMCOBJ=   -6520.61329566529     
 iteration         8720 MCMCOBJ=   -6502.45481259190     
 iteration         8730 MCMCOBJ=   -6416.84652910114     
 iteration         8740 MCMCOBJ=   -6373.56513854687     
 iteration         8750 MCMCOBJ=   -6487.18048063784     
 iteration         8760 MCMCOBJ=   -6543.24037191527     
 iteration         8770 MCMCOBJ=   -6473.52978252754     
 iteration         8780 MCMCOBJ=   -6523.24662256210     
 iteration         8790 MCMCOBJ=   -6482.95190897340     
 iteration         8800 MCMCOBJ=   -6519.97582622142     
 iteration         8810 MCMCOBJ=   -6521.53758432646     
 iteration         8820 MCMCOBJ=   -6469.41878581800     
 iteration         8830 MCMCOBJ=   -6468.60350998170     
 iteration         8840 MCMCOBJ=   -6476.85203703189     
 iteration         8850 MCMCOBJ=   -6467.70243822414     
 iteration         8860 MCMCOBJ=   -6491.98417423366     
 iteration         8870 MCMCOBJ=   -6474.41325856294     
 iteration         8880 MCMCOBJ=   -6460.18850299276     
 iteration         8890 MCMCOBJ=   -6532.94296750172     
 iteration         8900 MCMCOBJ=   -6518.34093526749     
 iteration         8910 MCMCOBJ=   -6453.25645003979     
 iteration         8920 MCMCOBJ=   -6491.39809542101     
 iteration         8930 MCMCOBJ=   -6468.35113184556     
 iteration         8940 MCMCOBJ=   -6468.23000085381     
 iteration         8950 MCMCOBJ=   -6506.94215356807     
 iteration         8960 MCMCOBJ=   -6440.37822644168     
 iteration         8970 MCMCOBJ=   -6478.83572137146     
 iteration         8980 MCMCOBJ=   -6481.21362151839     
 iteration         8990 MCMCOBJ=   -6488.41674851053     
 iteration         9000 MCMCOBJ=   -6468.98649797409     
 iteration         9010 MCMCOBJ=   -6456.30322209452     
 iteration         9020 MCMCOBJ=   -6445.80033401458     
 iteration         9030 MCMCOBJ=   -6443.35633727960     
 iteration         9040 MCMCOBJ=   -6573.13729220743     
 iteration         9050 MCMCOBJ=   -6483.02262863273     
 iteration         9060 MCMCOBJ=   -6467.29013610558     
 iteration         9070 MCMCOBJ=   -6487.47335311299     
 iteration         9080 MCMCOBJ=   -6495.57704021164     
 iteration         9090 MCMCOBJ=   -6534.60743088405     
 iteration         9100 MCMCOBJ=   -6536.75376452277     
 iteration         9110 MCMCOBJ=   -6513.69174767340     
 iteration         9120 MCMCOBJ=   -6465.34677201362     
 iteration         9130 MCMCOBJ=   -6525.79762996508     
 iteration         9140 MCMCOBJ=   -6444.33451557283     
 iteration         9150 MCMCOBJ=   -6488.57020932800     
 iteration         9160 MCMCOBJ=   -6479.40739309756     
 iteration         9170 MCMCOBJ=   -6455.05505190715     
 iteration         9180 MCMCOBJ=   -6500.22663933418     
 iteration         9190 MCMCOBJ=   -6496.55890404800     
 iteration         9200 MCMCOBJ=   -6472.60294809243     
 iteration         9210 MCMCOBJ=   -6530.36346651561     
 iteration         9220 MCMCOBJ=   -6441.50055344835     
 iteration         9230 MCMCOBJ=   -6502.47892025231     
 iteration         9240 MCMCOBJ=   -6458.33246385499     
 iteration         9250 MCMCOBJ=   -6426.30328210364     
 iteration         9260 MCMCOBJ=   -6523.03746463760     
 iteration         9270 MCMCOBJ=   -6505.77182648768     
 iteration         9280 MCMCOBJ=   -6492.88756096513     
 iteration         9290 MCMCOBJ=   -6490.75935635717     
 iteration         9300 MCMCOBJ=   -6567.29488221894     
 iteration         9310 MCMCOBJ=   -6426.65047202861     
 iteration         9320 MCMCOBJ=   -6489.88945292807     
 iteration         9330 MCMCOBJ=   -6563.06876252544     
 iteration         9340 MCMCOBJ=   -6442.61203957908     
 iteration         9350 MCMCOBJ=   -6472.18726589840     
 iteration         9360 MCMCOBJ=   -6425.44552429764     
 iteration         9370 MCMCOBJ=   -6492.90587225365     
 iteration         9380 MCMCOBJ=   -6512.87700891464     
 iteration         9390 MCMCOBJ=   -6473.14371952687     
 iteration         9400 MCMCOBJ=   -6462.85475724826     
 iteration         9410 MCMCOBJ=   -6502.84961118005     
 iteration         9420 MCMCOBJ=   -6484.53565910212     
 iteration         9430 MCMCOBJ=   -6533.30052993160     
 iteration         9440 MCMCOBJ=   -6553.50356387883     
 iteration         9450 MCMCOBJ=   -6481.50448912788     
 iteration         9460 MCMCOBJ=   -6472.58931962748     
 iteration         9470 MCMCOBJ=   -6514.89532996575     
 iteration         9480 MCMCOBJ=   -6516.79593641408     
 iteration         9490 MCMCOBJ=   -6578.96243742570     
 iteration         9500 MCMCOBJ=   -6475.94498703304     
 iteration         9510 MCMCOBJ=   -6467.03741263446     
 iteration         9520 MCMCOBJ=   -6500.08729799202     
 iteration         9530 MCMCOBJ=   -6518.72094155436     
 iteration         9540 MCMCOBJ=   -6492.38193460188     
 iteration         9550 MCMCOBJ=   -6462.12539272741     
 iteration         9560 MCMCOBJ=   -6482.04451456226     
 iteration         9570 MCMCOBJ=   -6421.58789168377     
 iteration         9580 MCMCOBJ=   -6456.58401291676     
 iteration         9590 MCMCOBJ=   -6478.94593902683     
 iteration         9600 MCMCOBJ=   -6416.61974760780     
 iteration         9610 MCMCOBJ=   -6454.05037703454     
 iteration         9620 MCMCOBJ=   -6482.08137867083     
 iteration         9630 MCMCOBJ=   -6499.01352077399     
 iteration         9640 MCMCOBJ=   -6484.29057608619     
 iteration         9650 MCMCOBJ=   -6497.72167263348     
 iteration         9660 MCMCOBJ=   -6476.87288923864     
 iteration         9670 MCMCOBJ=   -6450.22279361684     
 iteration         9680 MCMCOBJ=   -6431.93481766836     
 iteration         9690 MCMCOBJ=   -6527.51222399514     
 iteration         9700 MCMCOBJ=   -6496.13119499974     
 iteration         9710 MCMCOBJ=   -6448.22188571890     
 iteration         9720 MCMCOBJ=   -6473.81025705283     
 iteration         9730 MCMCOBJ=   -6451.90176329089     
 iteration         9740 MCMCOBJ=   -6462.82445535864     
 iteration         9750 MCMCOBJ=   -6463.07073257116     
 iteration         9760 MCMCOBJ=   -6443.79040437479     
 iteration         9770 MCMCOBJ=   -6458.19636912667     
 iteration         9780 MCMCOBJ=   -6575.64933725730     
 iteration         9790 MCMCOBJ=   -6484.35921495707     
 iteration         9800 MCMCOBJ=   -6444.30441500174     
 iteration         9810 MCMCOBJ=   -6473.26509249255     
 iteration         9820 MCMCOBJ=   -6501.79599825298     
 iteration         9830 MCMCOBJ=   -6537.09995981682     
 iteration         9840 MCMCOBJ=   -6495.14328426524     
 iteration         9850 MCMCOBJ=   -6485.62246902651     
 iteration         9860 MCMCOBJ=   -6467.00194350825     
 iteration         9870 MCMCOBJ=   -6505.73252305077     
 iteration         9880 MCMCOBJ=   -6456.48503252135     
 iteration         9890 MCMCOBJ=   -6467.20337221405     
 iteration         9900 MCMCOBJ=   -6515.73255314955     
 iteration         9910 MCMCOBJ=   -6543.53937755200     
 iteration         9920 MCMCOBJ=   -6529.07748427929     
 iteration         9930 MCMCOBJ=   -6497.70362656454     
 iteration         9940 MCMCOBJ=   -6449.28184116699     
 iteration         9950 MCMCOBJ=   -6413.73490440635     
 iteration         9960 MCMCOBJ=   -6517.15162501333     
 iteration         9970 MCMCOBJ=   -6464.98463452873     
 iteration         9980 MCMCOBJ=   -6513.73956402299     
 iteration         9990 MCMCOBJ=   -6440.74203434948     
 iteration        10000 MCMCOBJ=   -6490.21333944960     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6492.00543363034     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3610.21419350048     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6492.00543363034     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5756.85460706660     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    55.1779157436876     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6492.00543363034     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6436.82751788665     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  1638.33
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6492.005       **************************************************
 #OBJS:********************************************       39.300 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.22E+00  5.54E-01 -1.83E-01  2.27E+00  2.39E-01  3.71E+00 -7.04E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.87E-01
 
 ETA2
+       -3.66E-02  2.20E-01
 
 ETA3
+        4.61E-02 -9.70E-03  1.43E-01
 
 ETA4
+        3.10E-02  5.70E-02 -1.39E-02  2.68E-01
 
 ETA5
+        2.80E-02  1.67E-02 -4.88E-04 -3.30E-02  2.10E-01
 
 ETA6
+       -2.66E-02  4.13E-03  1.60E-02  1.49E-02 -7.54E-02  2.41E-01
 
 ETA7
+        3.05E-02 -4.99E-02  3.24E-02 -7.57E-02  2.43E-02  3.53E-06  2.52E-01
 
 ETA8
+        9.78E-02  7.53E-02  4.25E-02  4.70E-02  4.10E-03 -5.25E-02  5.87E-02  2.42E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.31E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.33E-01
 
 ETA2
+       -1.42E-01  4.66E-01
 
 ETA3
+        2.27E-01 -5.47E-02  3.75E-01
 
 ETA4
+        1.11E-01  2.32E-01 -7.32E-02  5.14E-01
 
 ETA5
+        1.13E-01  7.88E-02 -4.26E-03 -1.38E-01  4.55E-01
 
 ETA6
+       -1.02E-01  1.97E-02  8.82E-02  5.79E-02 -3.35E-01  4.87E-01
 
 ETA7
+        1.12E-01 -2.06E-01  1.70E-01 -2.89E-01  1.04E-01 -3.44E-04  4.99E-01
 
 ETA8
+        3.68E-01  3.27E-01  2.26E-01  1.83E-01  1.71E-02 -2.16E-01  2.36E-01  4.89E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.64E-02
 
 EPS2
+        0.00E+00  1.49E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         7.71E-02  7.60E-02  5.98E-02  7.46E-02  6.62E-02  7.55E-02  7.23E-02  7.09E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.99E-02
 
 ETA2
+        4.16E-02  5.74E-02
 
 ETA3
+        3.25E-02  2.99E-02  3.53E-02
 
 ETA4
+        4.14E-02  4.05E-02  3.26E-02  5.89E-02
 
 ETA5
+        3.64E-02  3.36E-02  2.75E-02  3.58E-02  4.52E-02
 
 ETA6
+        4.19E-02  3.88E-02  3.07E-02  4.10E-02  3.64E-02  5.78E-02
 
 ETA7
+        3.99E-02  4.17E-02  3.01E-02  4.01E-02  3.45E-02  3.87E-02  5.25E-02
 
 ETA8
+        4.18E-02  3.82E-02  3.06E-02  3.89E-02  3.43E-02  3.93E-02  3.76E-02  5.23E-02
 


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
+        5.47E-02
 
 ETA2
+        1.52E-01  5.93E-02
 
 ETA3
+        1.46E-01  1.61E-01  4.55E-02
 
 ETA4
+        1.41E-01  1.47E-01  1.60E-01  5.57E-02
 
 ETA5
+        1.40E-01  1.50E-01  1.52E-01  1.42E-01  4.81E-02
 
 ETA6
+        1.52E-01  1.62E-01  1.58E-01  1.55E-01  1.38E-01  5.75E-02
 
 ETA7
+        1.40E-01  1.55E-01  1.47E-01  1.31E-01  1.41E-01  1.51E-01  5.12E-02
 
 ETA8
+        1.25E-01  1.41E-01  1.45E-01  1.39E-01  1.46E-01  1.46E-01  1.35E-01  5.19E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.35E-03
 
 EPS2
+        0.00E+00  3.99E-03
 
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
+        5.94E-03
 
 TH 2
+       -8.77E-04  5.78E-03
 
 TH 3
+        6.73E-04  4.05E-06  3.57E-03
 
 TH 4
+        5.77E-04  9.81E-04  5.28E-05  5.56E-03
 
 TH 5
+        5.25E-04  2.56E-04  6.16E-05 -5.81E-04  4.39E-03
 
 TH 6
+       -3.61E-04 -1.90E-04  1.77E-04  3.09E-04 -1.19E-03  5.70E-03
 
 TH 7
+        6.23E-04 -1.22E-03  5.97E-04 -1.56E-03  5.57E-04  1.62E-04  5.23E-03
 
 TH 8
+        1.99E-03  1.21E-03  1.01E-03  9.13E-04  1.38E-04 -9.89E-04  1.24E-03  5.03E-03
 
 OM11
+        6.69E-05 -1.89E-05 -5.85E-05  6.38E-05  2.27E-05  3.24E-05 -1.13E-05  4.13E-05  3.59E-03
 
 OM12
+       -2.80E-05  2.04E-04 -3.31E-05 -7.02E-05 -3.91E-05 -6.60E-06 -3.10E-05 -7.88E-05 -5.10E-04  1.73E-03
 
 OM13
+        1.89E-05  1.95E-05  6.09E-05  5.93E-05  8.37E-07  1.47E-05  2.04E-05  3.29E-05  4.58E-04 -6.73E-05  1.06E-03
 
 OM14
+        4.17E-05  2.82E-05 -1.93E-05  4.53E-05 -2.55E-05  2.17E-05  2.40E-05  3.65E-05  3.57E-04  3.15E-04 -6.58E-06  1.71E-03
 
 OM15
+       -6.32E-05  9.84E-06  3.42E-05 -1.06E-05  2.10E-05  5.43E-05 -2.06E-05 -2.80E-05  3.31E-04  5.98E-05  3.45E-05 -1.67E-04
          1.32E-03
 
 OM16
+        1.36E-05 -1.11E-05  3.22E-05 -1.50E-05  4.27E-05  1.12E-05  6.03E-05  5.30E-05 -2.44E-04 -4.31E-05  2.01E-05  8.81E-05
         -3.79E-04  1.76E-03
 
 OM17
+        2.99E-05 -2.81E-05  3.86E-05  1.86E-05 -1.16E-05  5.02E-05  2.33E-05  3.21E-05  4.20E-04 -4.15E-04  2.38E-04 -4.65E-04
          1.65E-04  2.13E-05  1.59E-03
 
 OM18
+        5.54E-05 -3.91E-05 -2.26E-05  5.55E-05 -1.05E-05  3.31E-05 -2.72E-05  3.60E-05  1.18E-03  3.19E-04  3.75E-04  3.48E-04
          8.45E-05 -3.51E-04  4.68E-04  1.75E-03
 
 OM22
+        4.65E-05 -7.15E-04  1.12E-04  1.04E-04  1.38E-05  5.63E-05  3.85E-05  1.53E-04  1.34E-04 -7.00E-04  6.80E-05 -9.64E-05
         -2.36E-06  2.26E-05  1.72E-04 -7.71E-05  3.29E-03
 
 OM23
+       -1.02E-05  1.80E-04  9.11E-05  6.01E-05 -3.34E-05  4.44E-06 -4.89E-05 -3.40E-05 -8.19E-05  2.92E-04 -1.23E-04  7.16E-05
         -2.27E-06 -2.49E-05 -8.34E-05  2.75E-05 -1.77E-04  8.94E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        3.42E-05 -4.00E-04  1.06E-04  3.43E-05 -1.03E-05  5.87E-05  2.81E-05  4.99E-05 -5.32E-05 -2.66E-05  5.12E-05 -1.83E-04
          3.59E-05 -2.83E-05  6.18E-05  2.85E-05  8.64E-04 -6.34E-05  1.64E-03
 
 OM25
+        6.15E-05  4.35E-05 -3.83E-05 -3.44E-05  1.48E-05 -1.50E-05  2.52E-05  6.30E-05 -2.03E-05  1.71E-04 -2.34E-05  6.34E-05
         -1.59E-04  3.87E-05 -5.60E-05  4.36E-05  6.26E-05  1.67E-05 -1.89E-04  1.13E-03
 
 OM26
+        1.63E-05  5.83E-05  1.29E-05  2.25E-05 -1.25E-06 -1.65E-05 -3.09E-05 -9.13E-06  1.42E-05 -1.09E-04  1.50E-05 -3.49E-05
          7.73E-05 -2.22E-04  4.97E-05  3.57E-05 -4.30E-05  6.89E-05  8.35E-05 -3.71E-04  1.51E-03
 
 OM27
+       -2.84E-05  5.78E-04 -6.30E-05 -6.21E-05 -1.14E-05 -1.31E-05 -4.70E-05 -7.36E-05 -5.73E-05  3.80E-04 -5.62E-05  1.12E-04
         -1.35E-05 -5.11E-05 -2.61E-04 -1.07E-05 -9.57E-04  2.49E-04 -6.84E-04  2.03E-04 -7.44E-06  1.74E-03
 
 OM28
+        1.87E-05  1.11E-04  4.41E-05 -3.87E-06 -2.96E-05  1.84E-05 -1.56E-05  1.59E-05 -1.43E-04  4.80E-04 -1.48E-05  8.47E-05
          4.02E-05  1.71E-05 -1.74E-04 -7.77E-05  7.13E-04  2.56E-04  2.78E-04  7.90E-05 -3.48E-04  3.43E-04  1.46E-03
 
 OM33
+        7.49E-05 -6.36E-05 -1.20E-04 -8.23E-05 -3.56E-05  5.93E-05  1.06E-05  1.24E-06  6.99E-05 -2.79E-05  3.24E-04  1.58E-05
         -9.70E-06  5.75E-06  5.18E-05  6.87E-05  9.36E-05 -2.86E-05  4.73E-05  2.23E-06 -1.81E-05 -6.68E-05  1.76E-05  1.25E-03
 
 OM34
+       -4.36E-05  3.71E-06  2.20E-04  1.17E-04 -7.73E-06 -2.70E-05  1.14E-07  3.66E-05  1.15E-05  7.24E-05  1.49E-04  2.64E-04
         -1.45E-05  2.32E-05 -6.71E-05  4.66E-05  2.55E-05  2.15E-04  1.28E-05 -8.59E-06  4.88E-05  1.41E-05  6.74E-05 -6.29E-05
         1.06E-03
 
 OM35
+        1.79E-05 -6.26E-05 -6.61E-05 -4.80E-05  4.98E-05  2.29E-05  3.87E-05 -1.38E-05  1.88E-05 -1.90E-06  6.99E-05 -3.64E-05
          1.84E-04 -5.60E-05  3.96E-05  2.32E-05 -9.93E-06  7.70E-05 -2.90E-06 -4.67E-06  7.04E-06  2.02E-05  2.14E-05  3.81E-05
        -1.52E-04  7.54E-04
 
 OM36
+       -2.20E-05 -3.14E-06  1.83E-05 -2.26E-05 -2.01E-06 -4.10E-05 -1.52E-05 -5.98E-06 -2.73E-05 -1.71E-05 -7.22E-05  1.76E-05
         -5.94E-05  2.62E-04 -3.10E-06 -6.64E-05  4.80E-05 -4.04E-06  1.76E-05 -1.27E-05  1.98E-06 -2.91E-05  8.76E-06  2.18E-05
         8.45E-05 -2.61E-04  9.42E-04
 
 OM37
+        2.21E-05 -5.73E-06 -1.16E-04 -8.25E-05 -1.57E-05 -7.53E-06  4.69E-06  5.61E-06  6.99E-05 -8.69E-05  1.05E-04 -7.69E-05
          1.32E-05  6.87E-06  2.69E-04  7.88E-05  3.19E-05 -2.17E-04 -1.99E-05  4.28E-06 -1.13E-05 -7.70E-05 -8.75E-05  2.20E-04
        -2.99E-04  9.78E-05 -7.78E-06  9.04E-04
 
 OM38
+        1.62E-05  2.46E-05  6.17E-05  6.56E-05 -1.72E-05  4.70E-05 -7.97E-06  5.12E-06  1.37E-04  5.92E-05  4.16E-04  5.79E-05
         -2.38E-06 -2.62E-05  1.08E-04  3.05E-04  4.40E-05  2.63E-04  2.94E-05  1.22E-05  3.52E-06  2.46E-06  7.81E-05  3.88E-04
         1.88E-04  2.63E-05 -2.02E-04  2.29E-04  9.39E-04
 
 OM44
+        2.03E-05 -1.15E-05  3.09E-04  2.60E-04 -1.59E-06  7.88E-05  7.11E-06  4.31E-05  9.01E-05  8.10E-05  8.21E-05  3.82E-04
         -5.86E-06  8.63E-06 -7.49E-05  1.32E-04  2.38E-04  2.05E-05  6.96E-04 -9.05E-05  6.70E-05 -2.54E-04  1.38E-04  1.74E-05
         6.27E-05 -4.27E-05  6.50E-06 -1.51E-05  5.46E-05  3.47E-03
 
 OM45
+        5.19E-05  2.25E-05 -2.57E-06 -2.65E-05  5.74E-05 -4.95E-07 -1.33E-05  2.33E-05  4.38E-05  5.38E-05  2.20E-06  1.62E-04
          9.84E-05  1.13E-06 -2.25E-05  8.30E-05  3.31E-06  1.33E-05  4.29E-05  2.54E-04 -4.66E-05  4.28E-05  1.32E-05 -2.07E-05
        -7.87E-06 -2.16E-05 -2.90E-06 -1.23E-05  9.32E-07 -4.14E-04  1.28E-03
 
 OM46
+        2.94E-05  4.11E-05 -9.52E-06  5.89E-05  1.99E-05  1.27E-04  7.27E-06  6.17E-05 -4.73E-05 -4.28E-05  1.96E-05 -1.49E-04
          5.04E-06  1.95E-04  7.10E-05 -6.55E-05 -4.62E-06  7.76E-06  1.64E-05 -6.28E-05  3.25E-04 -1.13E-05 -5.06E-05  1.85E-05
         4.50E-05 -1.71E-05 -4.10E-06 -2.61E-05 -5.91E-06  2.51E-04 -3.75E-04  1.68E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        3.18E-05  2.07E-05 -6.89E-05 -7.26E-05 -1.02E-05 -2.92E-05 -3.33E-06  1.92E-05  1.43E-05  1.43E-05 -2.43E-05  1.52E-04
         -1.85E-05  2.29E-05  1.16E-04  6.41E-05 -2.04E-04  3.11E-05 -4.59E-04  1.01E-04 -4.00E-06  4.73E-04  1.82E-05 -2.80E-05
         1.64E-04 -3.41E-05  1.92E-05 -7.63E-05 -1.55E-05 -9.15E-04  2.39E-04 -4.26E-05  1.61E-03
 
 OM48
+        6.09E-05  3.51E-05  5.20E-05  2.42E-05 -6.37E-06  3.83E-05  2.82E-05  1.60E-05  1.21E-04  1.72E-04  4.22E-05  6.50E-04
         -3.42E-05  1.49E-05 -1.28E-04  2.64E-04  1.26E-04  7.34E-05  4.32E-04 -2.91E-05 -2.19E-05  3.31E-06  3.61E-04  2.80E-05
         3.07E-04 -3.72E-05  1.72E-05 -9.92E-05  4.53E-05  5.99E-04  2.44E-05 -3.12E-04  2.62E-04  1.51E-03
 
 OM55
+        2.06E-05 -3.14E-05 -6.58E-06 -4.96E-05 -1.41E-05 -3.02E-05 -2.52E-05 -2.66E-05  4.98E-05  1.40E-05  2.35E-05 -4.61E-05
          2.70E-04 -9.13E-05  3.17E-05 -9.67E-07  7.51E-05  2.27E-05 -3.06E-05  1.32E-04 -1.20E-05 -1.90E-05  2.68E-06  4.25E-05
        -2.56E-07  5.08E-05 -4.94E-05 -1.34E-05  1.89E-05  8.79E-05 -2.89E-04  6.22E-05 -6.22E-05 -2.66E-05  2.04E-03
 
 OM56
+        5.02E-05 -6.62E-05  2.12E-05 -2.19E-06  2.62E-05 -5.95E-05  2.21E-05  2.17E-05 -3.11E-05  1.90E-05 -7.11E-06  3.33E-05
         -1.59E-04  1.89E-04  6.98E-06 -1.75E-05  2.21E-05 -1.64E-06  3.80E-05 -3.44E-05  7.77E-05 -1.27E-05 -1.31E-05 -1.68E-05
         1.57E-05  3.77E-05  8.33E-06  8.00E-06 -6.75E-06  1.43E-06  1.19E-04 -2.30E-04  3.84E-06  5.12E-05 -6.00E-04  1.32E-03
 
 OM57
+       -5.01E-05 -1.31E-06  1.46E-05  2.07E-05 -4.38E-07 -4.32E-06  1.29E-05 -5.85E-05  4.44E-05 -3.57E-05  6.87E-06 -8.82E-05
          1.64E-04 -8.86E-06  1.65E-04  3.47E-06 -9.79E-06  7.64E-06  3.70E-05 -2.61E-04  8.53E-05 -4.56E-06  1.29E-06  3.68E-06
        -1.88E-05  1.39E-04 -3.00E-05  2.26E-05 -2.61E-06  1.16E-04 -3.64E-04  9.16E-05 -2.10E-04 -4.90E-05  2.46E-04 -2.46E-05
          1.19E-03
 
 OM58
+       -5.87E-06 -3.18E-05  1.23E-05 -3.68E-05  3.22E-05  1.02E-05  5.14E-06 -3.55E-06  1.20E-04  6.05E-05  2.04E-05 -1.47E-05
          4.53E-04 -1.29E-04  8.45E-05  1.63E-04  4.10E-05  3.54E-05 -2.77E-05  3.27E-04 -9.63E-05  4.78E-05  9.69E-05  1.96E-05
        -2.59E-05  2.13E-04 -6.49E-05  2.05E-05  2.79E-05 -6.10E-05  2.12E-04 -1.77E-05 -2.47E-06 -1.34E-04  7.36E-05 -2.64E-04
          3.03E-04  1.17E-03
 
 OM66
+       -1.31E-05 -2.55E-05 -4.26E-05  4.88E-05  2.91E-05  4.92E-05  1.83E-05 -3.79E-05  6.41E-05 -1.96E-05  1.32E-06  8.29E-06
          5.04E-05 -1.92E-04 -1.08E-05  4.04E-05  6.21E-05 -1.07E-05 -3.67E-05  2.10E-05 -7.65E-05 -3.33E-05  2.55E-05  7.06E-05
         2.02E-05 -1.19E-05  1.09E-04  4.70E-06  1.70E-05  5.45E-05 -4.60E-05  1.66E-04 -2.59E-05 -2.22E-05  1.37E-04 -7.76E-04
         -1.42E-05  1.14E-04  3.34E-03
 
 OM67
+        2.69E-06 -2.08E-05 -1.54E-05  7.39E-06  3.48E-06 -1.49E-04 -3.17E-05  2.49E-05 -5.73E-05  3.88E-05 -5.13E-05  5.20E-05
         -6.71E-05  1.87E-04 -1.40E-04 -8.72E-05 -6.34E-06 -7.35E-06 -1.10E-05  8.36E-05 -3.69E-04 -1.74E-05  5.12E-05 -2.90E-07
        -1.27E-05 -3.68E-05  1.77E-04  5.42E-05 -2.44E-05 -1.06E-04  1.04E-04 -4.61E-04  7.56E-05  8.64E-05 -7.40E-05  1.77E-04
         -3.41E-04 -1.15E-04  3.33E-05  1.49E-03
 
 OM68
+        1.01E-06  2.49E-05  1.47E-05  3.30E-05  3.95E-05 -3.96E-06  1.81E-06  3.93E-05 -1.16E-04 -3.98E-05 -1.44E-05  4.64E-06
         -1.13E-04  6.25E-04 -2.72E-06 -2.35E-04 -4.93E-05 -2.97E-06  2.41E-05 -8.69E-05  4.60E-04 -3.92E-05 -1.20E-04 -7.29E-06
         4.69E-05 -8.20E-05  3.05E-04  1.38E-05 -2.13E-05  5.83E-06 -3.88E-05  3.01E-04  2.75E-05  4.29E-05 -6.19E-05  1.10E-04
         -7.66E-05 -3.52E-04 -6.58E-04  3.23E-04  1.54E-03
 
 OM77
+       -2.09E-05 -8.25E-05  6.94E-05  8.19E-05 -3.90E-05  2.44E-05 -2.12E-05 -6.33E-05  8.50E-05 -1.30E-04  6.70E-05 -1.48E-04
          2.28E-05  2.29E-06  3.60E-04  8.89E-05  2.76E-04 -7.07E-05  2.75E-04 -9.03E-05  7.26E-06 -7.15E-04 -1.63E-04  7.61E-05
        -9.72E-05  4.74E-05 -8.17E-07  3.45E-04  9.78E-05  3.45E-04 -1.19E-04 -1.24E-05 -8.27E-04 -1.79E-04  9.91E-05 -1.14E-05
          2.76E-04  6.37E-05  5.08E-05  2.99E-05  8.44E-06  2.75E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.55E-05 -7.44E-05  4.46E-05  1.98E-05 -1.43E-05  3.80E-05  1.73E-05  2.90E-05  1.48E-04 -8.48E-05  1.03E-04 -1.53E-04
          4.73E-05 -2.58E-05  5.81E-04  3.10E-04 -1.07E-04 -1.03E-05 -1.38E-04  5.55E-05  1.01E-04  2.68E-04 -2.09E-04  5.66E-05
        -5.77E-05  4.25E-05 -5.68E-05  3.07E-04  2.30E-04 -1.49E-04  3.53E-05  7.79E-05  1.88E-04 -3.41E-04  2.27E-05 -2.41E-05
          3.68E-05  1.42E-04 -2.01E-05 -2.69E-04 -6.20E-05  6.23E-04  1.41E-03
 
 OM88
+        6.75E-05 -1.06E-05  6.39E-05  3.54E-05  2.51E-06  1.14E-04  1.53E-05  3.59E-05  4.23E-04  2.93E-04  2.15E-04  1.85E-04
          5.41E-05 -1.86E-04  2.79E-04  1.05E-03  2.57E-04  1.69E-04  1.69E-04  5.49E-05 -1.73E-04  1.98E-04  7.89E-04  1.26E-04
         7.32E-05  4.42E-05 -1.39E-04  1.32E-04  5.29E-04  2.07E-04  4.85E-05 -1.28E-04  1.11E-04  5.24E-04  5.58E-05 -3.44E-05
         -1.41E-05  1.23E-04  1.76E-04 -1.45E-04 -6.09E-04  1.69E-04  6.71E-04  2.73E-03
 
 SG11
+       -3.12E-07  2.86E-08 -3.09E-07 -5.70E-07  2.91E-08  9.00E-07 -4.66E-07 -8.84E-07  2.34E-07  7.11E-07  4.41E-07 -4.21E-08
          4.23E-08 -6.44E-08  3.27E-07  7.30E-08 -3.90E-07 -8.38E-07  2.54E-07 -6.56E-11 -3.81E-07 -1.69E-07 -3.06E-07  3.10E-07
        -3.33E-07 -5.77E-08 -1.70E-07  8.34E-08  9.28E-08 -8.86E-07 -2.67E-07 -2.68E-07 -2.16E-07 -7.93E-08  1.74E-07 -8.27E-08
          1.80E-07  3.46E-07  9.85E-07  5.55E-07 -2.93E-07  1.36E-07  1.84E-07 -7.81E-08  4.18E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -4.22E-07  4.35E-08 -2.09E-06  6.64E-07  2.15E-08 -1.01E-06 -9.25E-08  4.44E-07  1.52E-06 -1.16E-08  4.12E-07  1.21E-07
         -5.29E-08  2.48E-08 -6.39E-08  3.76E-07  1.20E-06  2.91E-07 -3.28E-07 -7.25E-07 -2.76E-07 -1.28E-07 -8.99E-07  1.04E-06
         4.05E-07 -1.05E-06  2.44E-07 -8.67E-07 -2.64E-07  7.35E-07 -1.61E-07  6.64E-07  1.23E-07  7.73E-08 -9.71E-07  3.35E-07
         -4.33E-07 -8.34E-08 -3.77E-06 -2.09E-07  1.10E-06 -2.31E-07 -4.24E-07 -1.19E-06 -1.76E-08  0.00E+00  1.42E-06
 
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
+        7.71E-02
 
 TH 2
+       -1.50E-01  7.60E-02
 
 TH 3
+        1.46E-01  8.92E-04  5.98E-02
 
 TH 4
+        1.00E-01  1.73E-01  1.18E-02  7.46E-02
 
 TH 5
+        1.03E-01  5.08E-02  1.56E-02 -1.18E-01  6.62E-02
 
 TH 6
+       -6.21E-02 -3.31E-02  3.91E-02  5.49E-02 -2.38E-01  7.55E-02
 
 TH 7
+        1.12E-01 -2.22E-01  1.38E-01 -2.89E-01  1.16E-01  2.96E-02  7.23E-02
 
 TH 8
+        3.64E-01  2.23E-01  2.38E-01  1.73E-01  2.93E-02 -1.85E-01  2.41E-01  7.09E-02
 
 OM11
+        1.45E-02 -4.16E-03 -1.63E-02  1.43E-02  5.71E-03  7.15E-03 -2.60E-03  9.72E-03  5.99E-02
 
 OM12
+       -8.75E-03  6.44E-02 -1.33E-02 -2.26E-02 -1.42E-02 -2.10E-03 -1.03E-02 -2.67E-02 -2.05E-01  4.16E-02
 
 OM13
+        7.54E-03  7.88E-03  3.13E-02  2.44E-02  3.89E-04  5.98E-03  8.68E-03  1.43E-02  2.35E-01 -4.98E-02  3.25E-02
 
 OM14
+        1.31E-02  8.97E-03 -7.80E-03  1.47E-02 -9.31E-03  6.93E-03  8.02E-03  1.24E-02  1.44E-01  1.83E-01 -4.89E-03  4.14E-02
 
 OM15
+       -2.25E-02  3.56E-03  1.57E-02 -3.90E-03  8.71E-03  1.97E-02 -7.82E-03 -1.08E-02  1.52E-01  3.96E-02  2.91E-02 -1.11E-01
          3.64E-02
 
 OM16
+        4.21E-03 -3.50E-03  1.28E-02 -4.79E-03  1.54E-02  3.53E-03  1.99E-02  1.78E-02 -9.70E-02 -2.47E-02  1.47E-02  5.08E-02
         -2.48E-01  4.19E-02
 
 OM17
+        9.72E-03 -9.27E-03  1.62E-02  6.26E-03 -4.40E-03  1.67E-02  8.06E-03  1.14E-02  1.76E-01 -2.50E-01  1.84E-01 -2.82E-01
          1.14E-01  1.28E-02  3.99E-02
 
 OM18
+        1.72E-02 -1.23E-02 -9.06E-03  1.78E-02 -3.80E-03  1.05E-02 -8.98E-03  1.22E-02  4.70E-01  1.84E-01  2.76E-01  2.01E-01
          5.55E-02 -2.00E-01  2.81E-01  4.18E-02
 
 OM22
+        1.05E-02 -1.64E-01  3.27E-02  2.43E-02  3.63E-03  1.30E-02  9.27E-03  3.77E-02  3.91E-02 -2.93E-01  3.64E-02 -4.06E-02
         -1.13E-03  9.39E-03  7.50E-02 -3.22E-02  5.74E-02
 
 OM23
+       -4.43E-03  7.91E-02  5.10E-02  2.70E-02 -1.68E-02  1.96E-03 -2.26E-02 -1.60E-02 -4.57E-02  2.35E-01 -1.26E-01  5.79E-02
         -2.09E-03 -1.99E-02 -6.99E-02  2.20E-02 -1.03E-01  2.99E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        1.10E-02 -1.30E-01  4.39E-02  1.13E-02 -3.84E-03  1.92E-02  9.58E-03  1.74E-02 -2.19E-02 -1.58E-02  3.88E-02 -1.09E-01
          2.44E-02 -1.66E-02  3.82E-02  1.68E-02  3.72E-01 -5.23E-02  4.05E-02
 
 OM25
+        2.37E-02  1.70E-02 -1.91E-02 -1.37E-02  6.64E-03 -5.89E-03  1.04E-02  2.64E-02 -1.01E-02  1.22E-01 -2.14E-02  4.56E-02
         -1.30E-01  2.74E-02 -4.18E-02  3.10E-02  3.24E-02  1.66E-02 -1.39E-01  3.36E-02
 
 OM26
+        5.46E-03  1.97E-02  5.54E-03  7.75E-03 -4.85E-04 -5.63E-03 -1.10E-02 -3.32E-03  6.10E-03 -6.76E-02  1.19E-02 -2.17E-02
          5.47E-02 -1.36E-01  3.21E-02  2.20E-02 -1.93E-02  5.93E-02  5.31E-02 -2.85E-01  3.88E-02
 
 OM27
+       -8.84E-03  1.82E-01 -2.53E-02 -1.99E-02 -4.14E-03 -4.17E-03 -1.56E-02 -2.49E-02 -2.29E-02  2.19E-01 -4.14E-02  6.52E-02
         -8.92E-03 -2.92E-02 -1.57E-01 -6.15E-03 -4.00E-01  2.00E-01 -4.05E-01  1.45E-01 -4.60E-03  4.17E-02
 
 OM28
+        6.36E-03  3.84E-02  1.94E-02 -1.36E-03 -1.17E-02  6.38E-03 -5.66E-03  5.87E-03 -6.24E-02  3.03E-01 -1.19E-02  5.36E-02
          2.90E-02  1.07E-02 -1.14E-01 -4.87E-02  3.26E-01  2.24E-01  1.80E-01  6.15E-02 -2.35E-01  2.16E-01  3.82E-02
 
 OM33
+        2.75E-02 -2.37E-02 -5.69E-02 -3.12E-02 -1.52E-02  2.22E-02  4.13E-03  4.93E-04  3.30E-02 -1.90E-02  2.82E-01  1.08E-02
         -7.55E-03  3.88E-03  3.67E-02  4.65E-02  4.61E-02 -2.70E-02  3.30E-02  1.88E-03 -1.32E-02 -4.53E-02  1.30E-02  3.53E-02
 
 OM34
+       -1.74E-02  1.50E-03  1.13E-01  4.82E-02 -3.58E-03 -1.10E-02  4.82E-05  1.59E-02  5.92E-03  5.34E-02  1.41E-01  1.96E-01
         -1.23E-02  1.70E-02 -5.16E-02  3.42E-02  1.36E-02  2.21E-01  9.73E-03 -7.85E-03  3.86E-02  1.04E-02  5.43E-02 -5.46E-02
         3.26E-02
 
 OM35
+        8.47E-03 -3.00E-02 -4.03E-02 -2.34E-02  2.74E-02  1.10E-02  1.95E-02 -7.09E-03  1.14E-02 -1.66E-03  7.83E-02 -3.20E-02
          1.84E-01 -4.87E-02  3.62E-02  2.03E-02 -6.30E-03  9.38E-02 -2.61E-03 -5.06E-03  6.60E-03  1.76E-02  2.04E-02  3.92E-02
        -1.70E-01  2.75E-02
 
 OM36
+       -9.29E-03 -1.35E-03  9.97E-03 -9.85E-03 -9.88E-04 -1.77E-02 -6.85E-03 -2.75E-03 -1.49E-02 -1.34E-02 -7.23E-02  1.39E-02
         -5.32E-02  2.03E-01 -2.53E-03 -5.18E-02  2.72E-02 -4.41E-03  1.41E-02 -1.23E-02  1.66E-03 -2.28E-02  7.48E-03  2.01E-02
         8.45E-02 -3.09E-01  3.07E-02
 
 OM37
+        9.52E-03 -2.51E-03 -6.47E-02 -3.68E-02 -7.91E-03 -3.32E-03  2.16E-03  2.63E-03  3.88E-02 -6.95E-02  1.08E-01 -6.18E-02
          1.21E-02  5.45E-03  2.24E-01  6.27E-02  1.85E-02 -2.41E-01 -1.64E-02  4.23E-03 -9.68E-03 -6.14E-02 -7.62E-02  2.07E-01
        -3.05E-01  1.18E-01 -8.43E-03  3.01E-02
 
 OM38
+        6.87E-03  1.05E-02  3.37E-02  2.87E-02 -8.50E-03  2.03E-02 -3.60E-03  2.36E-03  7.45E-02  4.65E-02  4.18E-01  4.56E-02
         -2.13E-03 -2.04E-02  8.82E-02  2.39E-01  2.50E-02  2.87E-01  2.37E-02  1.18E-02  2.96E-03  1.92E-03  6.68E-02  3.58E-01
         1.88E-01  3.13E-02 -2.15E-01  2.49E-01  3.06E-02
 
 OM44
+        4.48E-03 -2.58E-03  8.78E-02  5.91E-02 -4.07E-04  1.77E-02  1.67E-03  1.03E-02  2.55E-02  3.30E-02  4.28E-02  1.57E-01
         -2.73E-03  3.49E-03 -3.18E-02  5.37E-02  7.03E-02  1.17E-02  2.92E-01 -4.57E-02  2.93E-02 -1.03E-01  6.11E-02  8.34E-03
         3.27E-02 -2.64E-02  3.59E-03 -8.54E-03  3.02E-02  5.89E-02
 
 OM45
+        1.88E-02  8.25E-03 -1.20E-03 -9.93E-03  2.42E-02 -1.83E-04 -5.12E-03  9.19E-03  2.04E-02  3.61E-02  1.89E-03  1.09E-01
          7.55E-02  7.55E-04 -1.57E-02  5.55E-02  1.61E-03  1.24E-02  2.96E-02  2.11E-01 -3.35E-02  2.86E-02  9.63E-03 -1.64E-02
        -6.75E-03 -2.20E-02 -2.64E-03 -1.14E-02  8.50E-04 -1.96E-01  3.58E-02
 
 OM46
+        9.29E-03  1.32E-02 -3.88E-03  1.92E-02  7.33E-03  4.10E-02  2.45E-03  2.12E-02 -1.92E-02 -2.51E-02  1.47E-02 -8.79E-02
          3.37E-03  1.13E-01  4.34E-02 -3.82E-02 -1.96E-03  6.32E-03  9.85E-03 -4.55E-02  2.04E-01 -6.59E-03 -3.23E-02  1.28E-02
         3.37E-02 -1.52E-02 -3.26E-03 -2.12E-02 -4.70E-03  1.04E-01 -2.55E-01  4.10E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.03E-02  6.79E-03 -2.87E-02 -2.42E-02 -3.82E-03 -9.62E-03 -1.15E-03  6.76E-03  5.95E-03  8.58E-03 -1.86E-02  9.13E-02
         -1.26E-02  1.36E-02  7.25E-02  3.82E-02 -8.87E-02  2.59E-02 -2.83E-01  7.50E-02 -2.57E-03  2.83E-01  1.19E-02 -1.98E-02
         1.25E-01 -3.10E-02  1.56E-02 -6.32E-02 -1.26E-02 -3.87E-01  1.66E-01 -2.59E-02  4.01E-02
 
 OM48
+        2.03E-02  1.19E-02  2.24E-02  8.32E-03 -2.47E-03  1.30E-02  1.00E-02  5.78E-03  5.20E-02  1.06E-01  3.33E-02  4.04E-01
         -2.41E-02  9.15E-03 -8.26E-02  1.62E-01  5.62E-02  6.31E-02  2.74E-01 -2.22E-02 -1.45E-02  2.04E-03  2.43E-01  2.04E-02
         2.42E-01 -3.48E-02  1.44E-02 -8.48E-02  3.80E-02  2.61E-01  1.75E-02 -1.95E-01  1.68E-01  3.89E-02
 
 OM55
+        5.93E-03 -9.14E-03 -2.44E-03 -1.47E-02 -4.71E-03 -8.83E-03 -7.71E-03 -8.29E-03  1.84E-02  7.45E-03  1.60E-02 -2.47E-02
          1.64E-01 -4.82E-02  1.76E-02 -5.12E-04  2.90E-02  1.68E-02 -1.67E-02  8.71E-02 -6.83E-03 -1.01E-02  1.55E-03  2.66E-02
        -1.74E-04  4.10E-02 -3.56E-02 -9.86E-03  1.36E-02  3.30E-02 -1.79E-01  3.35E-02 -3.43E-02 -1.52E-02  4.52E-02
 
 OM56
+        1.79E-02 -2.39E-02  9.75E-03 -8.06E-04  1.09E-02 -2.17E-02  8.41E-03  8.41E-03 -1.43E-02  1.26E-02 -6.01E-03  2.22E-02
         -1.20E-01  1.24E-01  4.82E-03 -1.15E-02  1.06E-02 -1.51E-03  2.58E-02 -2.81E-02  5.50E-02 -8.37E-03 -9.41E-03 -1.31E-02
         1.33E-02  3.78E-02  7.47E-03  7.32E-03 -6.06E-03  6.67E-04  9.13E-02 -1.54E-01  2.63E-03  3.62E-02 -3.65E-01  3.64E-02
 
 OM57
+       -1.89E-02 -5.01E-04  7.06E-03  8.03E-03 -1.92E-04 -1.66E-03  5.17E-03 -2.39E-02  2.15E-02 -2.49E-02  6.12E-03 -6.18E-02
          1.31E-01 -6.13E-03  1.20E-01  2.41E-03 -4.95E-03  7.41E-03  2.65E-02 -2.25E-01  6.37E-02 -3.17E-03  9.79E-04  3.02E-03
        -1.67E-02  1.47E-01 -2.83E-02  2.18E-02 -2.47E-03  5.70E-02 -2.95E-01  6.47E-02 -1.52E-01 -3.65E-02  1.58E-01 -1.96E-02
          3.45E-02
 
 OM58
+       -2.23E-03 -1.22E-02  5.99E-03 -1.44E-02  1.42E-02  3.95E-03  2.08E-03 -1.46E-03  5.85E-02  4.25E-02  1.83E-02 -1.04E-02
          3.63E-01 -9.01E-02  6.18E-02  1.14E-01  2.09E-02  3.45E-02 -2.00E-02  2.84E-01 -7.24E-02  3.35E-02  7.41E-02  1.62E-02
        -2.32E-02  2.27E-01 -6.17E-02  1.99E-02  2.66E-02 -3.02E-02  1.73E-01 -1.26E-02 -1.80E-03 -1.01E-01  4.75E-02 -2.12E-01
          2.56E-01  3.43E-02
 
 OM66
+       -2.95E-03 -5.79E-03 -1.23E-02  1.13E-02  7.59E-03  1.13E-02  4.37E-03 -9.25E-03  1.85E-02 -8.17E-03  7.02E-04  3.46E-03
          2.40E-02 -7.91E-02 -4.70E-03  1.67E-02  1.87E-02 -6.22E-03 -1.57E-02  1.08E-02 -3.41E-02 -1.38E-02  1.16E-02  3.46E-02
         1.07E-02 -7.50E-03  6.12E-02  2.70E-03  9.60E-03  1.60E-02 -2.22E-02  6.98E-02 -1.12E-02 -9.87E-03  5.25E-02 -3.69E-01
         -7.11E-03  5.74E-02  5.78E-02
 
 OM67
+        9.04E-04 -7.08E-03 -6.65E-03  2.56E-03  1.36E-03 -5.11E-02 -1.13E-02  9.07E-03 -2.47E-02  2.41E-02 -4.08E-02  3.25E-02
         -4.77E-02  1.16E-01 -9.07E-02 -5.39E-02 -2.86E-03 -6.36E-03 -7.03E-03  6.43E-02 -2.46E-01 -1.08E-02  3.47E-02 -2.13E-04
        -1.01E-02 -3.47E-02  1.49E-01  4.66E-02 -2.06E-02 -4.67E-02  7.54E-02 -2.90E-01  4.87E-02  5.74E-02 -4.24E-02  1.26E-01
         -2.56E-01 -8.71E-02  1.49E-02  3.87E-02
 
 OM68
+        3.35E-04  8.32E-03  6.25E-03  1.13E-02  1.52E-02 -1.34E-03  6.37E-04  1.41E-02 -4.94E-02 -2.44E-02 -1.12E-02  2.86E-03
         -7.88E-02  3.79E-01 -1.74E-03 -1.43E-01 -2.19E-02 -2.53E-03  1.51E-02 -6.58E-02  3.02E-01 -2.39E-02 -8.00E-02 -5.25E-03
         3.66E-02 -7.61E-02  2.53E-01  1.17E-02 -1.77E-02  2.52E-03 -2.76E-02  1.87E-01  1.75E-02  2.81E-02 -3.49E-02  7.68E-02
         -5.66E-02 -2.61E-01 -2.90E-01  2.13E-01  3.93E-02
 
 OM77
+       -5.17E-03 -2.07E-02  2.21E-02  2.09E-02 -1.12E-02  6.16E-03 -5.57E-03 -1.70E-02  2.70E-02 -5.97E-02  3.92E-02 -6.80E-02
          1.19E-02  1.04E-03  1.72E-01  4.05E-02  9.18E-02 -4.50E-02  1.29E-01 -5.12E-02  3.56E-03 -3.27E-01 -8.13E-02  4.10E-02
        -5.69E-02  3.29E-02 -5.07E-04  2.19E-01  6.08E-02  1.12E-01 -6.33E-02 -5.77E-03 -3.93E-01 -8.77E-02  4.18E-02 -5.97E-03
          1.52E-01  3.54E-02  1.67E-02  1.47E-02  4.09E-03  5.25E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        5.36E-03 -2.60E-02  1.99E-02  7.05E-03 -5.74E-03  1.34E-02  6.37E-03  1.09E-02  6.59E-02 -5.43E-02  8.43E-02 -9.84E-02
          3.46E-02 -1.64E-02  3.88E-01  1.98E-01 -4.95E-02 -9.13E-03 -9.07E-02  4.40E-02  6.91E-02  1.71E-01 -1.46E-01  4.26E-02
        -4.72E-02  4.12E-02 -4.93E-02  2.72E-01  2.00E-01 -6.73E-02  2.62E-02  5.05E-02  1.25E-01 -2.33E-01  1.34E-02 -1.76E-02
          2.84E-02  1.10E-01 -9.24E-03 -1.85E-01 -4.20E-02  3.16E-01  3.76E-02
 
 OM88
+        1.67E-02 -2.68E-03  2.04E-02  9.06E-03  7.26E-04  2.89E-02  4.03E-03  9.68E-03  1.35E-01  1.35E-01  1.26E-01  8.56E-02
          2.84E-02 -8.47E-02  1.34E-01  4.79E-01  8.56E-02  1.08E-01  7.97E-02  3.12E-02 -8.53E-02  9.06E-02  3.95E-01  6.82E-02
         4.30E-02  3.08E-02 -8.67E-02  8.41E-02  3.30E-01  6.72E-02  2.59E-02 -5.94E-02  5.27E-02  2.57E-01  2.36E-02 -1.81E-02
         -7.80E-03  6.88E-02  5.81E-02 -7.17E-02 -2.97E-01  6.18E-02  3.41E-01  5.23E-02
 
 SG11
+       -6.26E-03  5.82E-04 -7.98E-03 -1.18E-02  6.80E-04  1.84E-02 -9.96E-03 -1.93E-02  6.05E-03  2.64E-02  2.09E-02 -1.57E-03
          1.80E-03 -2.38E-03  1.27E-02  2.70E-03 -1.05E-02 -4.33E-02  9.70E-03 -3.02E-06 -1.52E-02 -6.28E-03 -1.24E-02  1.36E-02
        -1.58E-02 -3.25E-03 -8.59E-03  4.29E-03  4.68E-03 -2.32E-02 -1.15E-02 -1.01E-02 -8.34E-03 -3.15E-03  5.96E-03 -3.52E-03
          8.08E-03  1.56E-02  2.63E-02  2.22E-02 -1.15E-02  4.00E-03  7.59E-03 -2.31E-03  6.47E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -4.59E-03  4.80E-04 -2.93E-02  7.47E-03  2.73E-04 -1.13E-02 -1.07E-03  5.25E-03  2.13E-02 -2.34E-04  1.06E-02  2.46E-03
         -1.22E-03  4.96E-04 -1.34E-03  7.54E-03  1.75E-02  8.15E-03 -6.79E-03 -1.81E-02 -5.96E-03 -2.57E-03 -1.98E-02  2.46E-02
         1.04E-02 -3.22E-02  6.66E-03 -2.42E-02 -7.22E-03  1.05E-02 -3.76E-03  1.36E-02  2.58E-03  1.67E-03 -1.80E-02  7.72E-03
         -1.05E-02 -2.04E-03 -5.47E-02 -4.53E-03  2.36E-02 -3.70E-03 -9.48E-03 -1.90E-02 -2.29E-02  0.00E+00  1.19E-03
 
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
+        2.17E+02
 
 TH 2
+        6.50E+01  2.39E+02
 
 TH 3
+       -1.65E+01  4.49E+00  3.14E+02
 
 TH 4
+       -1.82E+01 -1.64E+01  7.95E+00  2.20E+02
 
 TH 5
+       -3.26E+01 -3.45E+01 -3.79E+00  1.89E+01  2.56E+02
 
 TH 6
+       -6.93E+00 -1.46E+01 -2.19E+01 -2.10E+01  5.65E+01  2.01E+02
 
 TH 7
+        1.26E+01  6.95E+01 -1.58E+01  7.40E+01 -3.40E+01 -3.25E+01  2.58E+02
 
 TH 8
+       -9.90E+01 -1.03E+02 -5.91E+01 -5.26E+01  3.16E+01  6.09E+01 -1.02E+02  3.23E+02
 
 OM11
+       -4.13E+00 -2.59E+00  6.85E+00  1.56E+00 -2.48E+00 -1.93E+00 -1.33E+00  2.97E+00  4.58E+02
 
 OM12
+       -7.23E-01 -2.06E+00  4.97E+00  8.57E+00 -3.34E-01 -3.82E+00 -2.13E+00  1.11E+01  2.89E+02  1.21E+03
 
 OM13
+        4.30E-01 -1.53E+01 -2.20E+01 -1.13E+01  3.38E+00  6.74E+00 -1.01E+01  9.66E+00 -1.02E+02  1.09E+01  1.50E+03
 
 OM14
+        3.33E-01  4.48E+00  1.32E+01 -1.47E+00  5.89E+00 -6.11E+00 -4.16E+00 -8.52E+00 -8.72E+01 -9.53E+01  9.43E+01  9.80E+02
 
 OM15
+        1.22E+01 -1.16E+00 -8.92E+00 -1.51E+00 -1.68E+00 -7.62E+00  2.61E+00 -6.89E+00 -1.61E+02 -2.42E+02  1.87E+01  1.27E+02
          1.18E+03
 
 OM16
+        1.62E+00 -2.84E+00 -2.45E+00  3.91E+00  7.72E-01  5.63E-01 -1.91E+00 -5.10E+00 -3.44E+01 -1.17E+02 -1.04E+02 -8.03E+01
          2.93E+02  9.07E+02
 
 OM17
+       -1.06E+01 -2.87E+01 -2.04E+00  4.08E+00  5.48E+00 -7.05E+00 -1.18E+01  1.61E+01  5.35E+01  3.57E+02 -1.09E+02  3.26E+02
         -1.56E+02 -1.37E+02  1.16E+03
 
 OM18
+        3.15E+00  1.57E+01  9.49E+00 -2.70E+00  4.60E+00  7.34E+00  1.38E+01 -1.86E+01 -4.12E+02 -6.58E+02 -2.32E+02 -2.08E+02
          2.50E+02  3.30E+02 -5.12E+02  1.63E+03
 
 OM22
+        1.50E+01  3.91E+01 -3.92E+00 -6.81E+00 -1.28E+01 -1.10E+01  1.12E+01 -1.93E+01  5.89E+01  4.23E+02  3.89E+01 -3.69E+01
         -7.78E+01 -6.71E+01  1.19E+02 -2.27E+02  6.85E+02
 
 OM23
+       -6.78E+00 -2.38E+01 -2.33E+01 -6.44E+00  1.34E+01  4.84E+00  2.44E-01  1.76E+01 -5.39E+01 -1.60E+02  5.49E+02  4.52E+01
          8.09E+01 -2.56E+01 -1.19E+02 -1.11E+00  9.69E+01  1.88E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        9.21E+00  3.59E+01  7.97E-01  5.07E+00  3.42E-01 -7.76E+00  1.08E+01 -1.91E+01 -9.66E+00 -9.07E+01  2.24E+01  3.10E+02
          6.57E+01  7.80E+00  7.41E+01 -4.56E+01 -1.26E+02  8.66E+01  1.15E+03
 
 OM25
+       -6.16E+00 -7.50E+00  1.46E+01  3.07E+00  7.64E+00  4.59E+00 -5.18E+00 -1.28E+01 -7.29E+01 -3.02E+02  7.23E+00  6.51E+01
          4.44E+02  1.76E+02 -1.17E+02  2.01E+02 -2.47E+02 -7.69E+00  1.36E+02  1.54E+03
 
 OM26
+       -1.21E+01 -2.29E+01  2.64E+00  5.86E+00  1.60E+01  1.88E+01 -1.78E+00  8.91E+00 -1.26E+01 -1.33E+02 -8.93E+01 -2.21E+01
          1.24E+02  3.57E+02 -5.65E+01  1.71E+02 -1.94E+02 -2.26E+02 -1.00E+02  4.91E+02  1.24E+03
 
 OM27
+       -1.47E+01 -6.26E+01  2.45E+00  9.17E+00  3.94E+00 -6.83E+00 -1.38E+01  3.66E+01  2.35E+01  2.17E+02 -2.38E+01  1.12E+02
         -7.01E+01 -5.98E+01  3.58E+02 -2.35E+02  4.44E+02 -8.04E+01  3.95E+02 -3.12E+02 -2.75E+02  1.45E+03
 
 OM28
+       -1.24E+01 -2.64E+01 -6.79E-01  3.98E+00  2.24E+01  2.11E+01 -1.66E+00  1.93E+00 -1.56E+02 -7.83E+02 -1.78E+02 -5.34E+01
          1.64E+02  2.36E+02 -3.37E+02  8.36E+02 -7.36E+02 -3.66E+02 -2.45E+02  4.08E+02  6.22E+02 -9.03E+02  2.31E+03
 
 OM33
+       -1.31E+01  6.97E+00  3.50E+01  2.01E+01  7.18E+00 -1.06E+01  6.50E+00 -1.11E+01 -3.00E+00 -1.70E+01 -1.77E+02 -1.78E+01
          2.62E+01  2.85E+01  2.74E+00  7.44E+01 -1.06E+01  9.32E+01  3.44E+00  1.32E+01  4.13E+00  9.06E+00 -2.27E+00  1.02E+03
 
 OM34
+        2.10E+01  1.56E+01 -4.93E+01 -1.41E+01 -2.68E+00  1.10E+01  2.79E+00 -5.70E+00  1.15E+01 -2.08E+01 -1.96E+02 -1.25E+02
         -2.26E+01  3.54E+01 -2.80E+01  9.59E+01 -1.80E+01 -1.34E+02  5.09E+01  3.31E+00 -1.70E+01  5.05E+01  2.25E+01  1.48E+02
         1.36E+03
 
 OM35
+        1.92E+00  2.30E+01  2.45E+01  5.30E+00 -2.09E+01 -9.51E+00 -2.39E+00 -1.09E+01  4.46E+01  6.06E+01 -2.74E+02 -4.65E+01
         -1.54E+02 -9.20E+00  4.40E+01  9.48E+00 -1.35E+01 -4.35E+02 -4.01E+01  5.27E+01  6.80E+01 -1.68E+01  7.45E+01 -4.83E+01
         1.95E+02  1.79E+03
 
 OM36
+        3.56E-01 -1.59E+00 -2.05E+00  7.26E+00  5.63E-01  5.44E+00  2.21E+00  6.19E+00  2.10E+01  3.99E+01 -1.02E+02 -1.39E+01
         -3.82E+01 -1.03E+02  2.70E+01 -5.85E+01 -2.86E+01 -2.98E+02 -5.45E+01  3.96E+01  9.25E+01 -1.36E+01  5.60E+01 -1.71E+02
        -1.68E+02  5.56E+02  1.51E+03
 
 OM37
+       -4.58E+00 -1.31E+01  2.41E+01  2.09E+01  1.16E+01  6.74E+00  4.50E+00 -6.56E+00 -2.63E+01 -6.68E+01  1.72E+02 -2.93E+01
          2.76E+01  1.55E+01 -2.00E+02  8.36E+01  1.63E+01  6.01E+02  1.03E+02 -2.00E+01 -8.77E+01  3.06E+01 -9.45E+01 -5.62E+01
         4.56E+02 -2.72E+02 -2.61E+02  1.77E+03
 
 OM38
+       -1.65E-01 -6.69E+00 -1.36E+01 -1.29E+01 -3.93E+00 -2.72E+00 -5.05E+00  1.47E+01  7.73E+01  7.27E+01 -7.44E+02 -4.40E+01
         -2.91E+01  2.69E+01  1.43E+02 -7.01E+01 -5.48E+01 -9.99E+02 -8.86E+01  1.49E+01  1.71E+02  3.76E+01  2.44E+02 -4.61E+02
        -3.84E+02  3.73E+02  6.50E+02 -7.70E+02  2.46E+03
 
 OM44
+        3.23E+00 -3.06E-01 -3.01E+01 -1.59E+01 -2.74E+00  2.03E+00 -1.24E+00  7.09E+00  2.01E+00 -4.43E+00 -2.57E+01 -8.96E+01
         -1.17E+01  5.46E+00 -3.15E+01  1.99E+01 -3.30E+00 -9.39E+00 -9.39E+01 -1.13E+01  1.89E+00 -3.56E+01  3.02E+01  1.24E+01
         3.65E+01  3.64E+01 -6.71E+00  1.16E+01 -3.09E+00  4.35E+02
 
 OM45
+       -9.17E+00 -1.23E+01 -5.73E+00 -1.78E+00 -1.33E+01 -4.02E+00  2.44E+00  5.65E+00  2.22E+01  3.03E+01 -4.46E+01 -1.75E+02
         -1.34E+02 -4.15E+01 -3.52E+01 -4.47E+00  1.85E+01 -6.23E+01 -2.18E+02 -1.49E+02 -1.84E+01 -5.82E+01  5.28E+01  7.16E+00
         3.37E+01  1.04E+02  5.93E+01 -1.66E+01  5.96E+01  1.06E+02  1.14E+03
 
 OM46
+       -6.92E+00 -4.67E+00  1.43E+01 -3.04E-01 -7.73E+00 -1.45E+01  2.11E+00 -1.09E+01  1.57E+01  2.91E+01 -5.38E+00 -3.23E+01
         -5.18E+01 -9.33E+01 -9.94E+00 -3.61E+01  2.41E+01 -3.20E+01 -1.16E+02 -5.50E+01 -6.64E+01 -2.94E+01 -1.97E+01 -3.60E+01
        -9.66E+01  3.59E+01  9.85E+01 -4.44E+01  9.62E+01 -8.68E+01  2.67E+02  8.81E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        2.57E+00  1.79E+01 -8.53E-01  8.69E-01  4.20E+00  2.23E+00  8.00E+00 -4.65E+00 -6.84E-01 -2.19E+01  3.40E+01  2.59E+01
          3.91E+01  8.33E+00 -1.03E+02  1.90E+01 -3.96E+01  6.04E+01  2.93E+02  4.59E+01 -3.16E+01 -9.59E+00 -4.37E+01 -3.08E+00
        -5.63E+01 -8.42E+00 -2.96E+01  7.38E+01 -9.63E+00  2.77E+02 -1.31E+02 -1.53E+02  1.20E+03
 
 OM48
+       -1.61E+01 -2.11E+01  8.44E+00  4.85E+00 -6.53E-01 -3.19E+00 -9.27E+00  1.08E+01  4.38E+01  8.50E+01 -1.81E+01 -4.49E+02
         -9.53E+01 -6.33E+00 -1.10E+02 -2.36E+01  6.71E+01 -5.64E+01 -5.12E+02 -8.82E+01  2.40E+01 -1.74E+02  1.86E+01 -6.48E+01
        -2.72E+02  8.00E+00  1.11E+02 -1.40E+02  2.14E+02 -2.04E+02  1.51E+02  3.42E+02 -5.14E+02  1.51E+03
 
 OM55
+       -7.46E+00  4.68E+00  6.76E-01  7.56E+00 -3.86E-01  4.49E+00  5.92E+00  2.21E+00  8.83E+00  1.41E+01 -8.62E+00 -1.75E+01
         -1.95E+02 -6.84E+01  1.77E+01 -1.80E+01 -7.92E-01 -1.65E+01 -1.28E+00 -2.50E+02 -1.04E+02  4.20E+01 -2.72E+01 -2.58E+01
        -9.30E+00 -3.63E+01  9.28E+00  2.60E+01  7.42E+00 -9.70E+00  1.08E+02  4.11E+01 -1.66E+01  3.12E+01  6.64E+02
 
 OM56
+       -5.28E+00  1.63E+01 -1.55E+00 -7.95E-01 -1.01E+01  1.19E+00  1.94E+00 -1.58E+00 -1.03E+00  5.64E+00  5.44E+01  1.35E+01
         -9.46E+01 -2.08E+02  7.55E+00 -7.47E+01  2.22E+01  6.70E+01  2.60E+01 -2.50E+02 -2.92E+02  8.15E+01 -1.44E+02 -1.84E+01
        -5.12E+01 -1.90E+02 -7.59E+01  4.40E+01 -6.45E+01 -2.88E+01 -9.39E+01  7.49E+01  1.58E+01  9.66E+00  3.70E+02  1.26E+03
 
 OM57
+        4.66E+00  6.73E-03 -2.24E+00 -1.24E+01  2.59E+00  1.20E+01 -7.30E+00  5.68E+00 -2.92E+01 -1.02E+02  3.28E+01 -3.05E+01
          1.18E+02  4.23E+01 -1.97E+02  1.29E+02 -7.82E+01  1.72E+01 -5.16E+01  4.83E+02  1.76E+02 -2.65E+02  2.16E+02  1.40E+01
        -6.36E+00 -6.19E+01 -1.19E+01 -5.94E+00 -1.27E+01  3.48E+01  3.53E+02  8.42E+01  4.96E+01  1.41E+01 -1.93E+02 -2.60E+02
          1.38E+03
 
 OM58
+        1.03E+00  1.17E+01 -8.63E+00  5.62E+00 -1.19E+01 -4.68E+00  5.43E+00  5.65E-01  8.10E+01  2.17E+02  7.86E+01 -2.51E+01
         -5.93E+02 -2.54E+02  1.52E+02 -3.53E+02  1.54E+02  6.03E+01  7.36E+00 -7.79E+02 -3.50E+02  2.53E+02 -4.89E+02 -2.90E+01
        -4.36E+01 -3.21E+02 -1.32E+02  6.02E+01 -9.88E+01 -2.80E+01 -2.57E+02 -3.94E+01 -1.17E+01  9.79E+01  2.33E+02  5.07E+02
         -6.53E+02  1.81E+03
 
 OM66
+        1.08E+00  6.23E+00  3.09E+00 -6.33E+00 -8.41E+00 -4.05E+00 -3.11E+00  8.41E-01 -7.46E+00  2.42E+00  3.45E+01  1.01E+01
         -2.79E+01 -7.18E+01  2.30E+00 -1.54E+01  1.04E+01  5.35E+01  3.97E+01 -7.42E+01 -1.44E+02  4.19E+01 -7.04E+01 -9.76E+00
        -5.31E+00 -7.17E+01 -1.36E+02  3.55E+01 -8.23E+01 -4.48E+00 -3.94E+01 -7.98E+01  2.14E+01 -4.26E+01  7.06E+01  3.14E+02
         -6.55E+01  1.55E+02  4.35E+02
 
 OM67
+        9.99E-01  3.94E+00 -5.65E-01 -5.51E+00  8.18E+00  2.27E+01  4.24E+00 -5.84E+00 -3.49E+00 -4.41E+01  1.75E-01 -2.66E+01
          3.59E+01  8.49E+01 -3.72E+01  6.89E+01 -6.09E+01 -7.67E+01 -1.05E+02  1.67E+02  4.11E+02 -2.01E+02  2.51E+02  6.41E+00
        -2.25E+01  1.96E+01 -2.02E+01 -1.40E+02  5.79E+01 -1.17E+01  1.20E+02  3.06E+02 -1.50E+02  1.66E+02 -7.48E+01 -2.53E+02
          3.78E+02 -2.50E+02 -1.55E+02  1.12E+03
 
 OM68
+        8.23E+00  1.47E+01 -1.93E+00 -8.79E+00 -2.27E+01 -1.80E+01  8.10E-01 -6.77E+00  1.68E+01  1.07E+02  1.25E+02  6.85E+01
         -1.93E+02 -5.14E+02  8.15E+01 -2.40E+02  1.41E+02  1.95E+02  1.11E+02 -3.27E+02 -7.15E+02  2.35E+02 -5.25E+02  3.67E+01
         4.47E+01 -1.61E+02 -3.84E+02  1.23E+02 -3.71E+02  2.23E+01 -1.09E+02 -2.66E+02  9.31E+01 -2.64E+02  1.08E+02  3.67E+02
         -2.25E+02  6.32E+02  3.50E+02 -5.51E+02  1.67E+03
 
 OM77
+       -6.86E+00 -2.20E+01 -5.91E+00 -2.23E+00  6.03E+00  4.95E-01 -4.98E+00  2.29E+01 -1.74E+00  2.53E+01 -1.17E+01  3.33E+01
          9.05E+00 -1.67E+00  5.40E+01 -3.43E+01  5.61E+01 -4.16E+01  1.10E+02 -4.60E+01 -4.96E+01  3.68E+02 -1.76E+02 -7.00E+00
        -1.74E+01  4.27E+00  6.13E+00 -8.82E+01  6.02E+01  2.98E+01 -4.74E+01 -3.58E+01  3.50E+02 -1.57E+02 -1.92E+00  2.89E+01
         -1.60E+02  6.71E+01  6.61E+00 -1.56E+02  7.03E+01  6.52E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.29E+01  4.51E+01 -1.21E+01 -1.01E+01  5.55E-01  8.02E+00  8.71E+00 -2.99E+01 -2.87E+01 -2.17E+02 -6.90E-01 -1.78E+02
          7.61E+01  9.74E+01 -5.36E+02  3.42E+02 -2.26E+02 -5.56E+01 -2.99E+02  1.56E+02  2.19E+02 -7.94E+02  8.85E+02  1.62E+01
        -8.15E+01  3.82E+01  5.57E+01 -2.76E+02  9.49E+00 -5.47E+01  1.03E+02  1.40E+02 -4.36E+02  6.39E+02 -2.82E+01 -1.08E+02
          2.80E+02 -3.32E+02 -5.55E+01  4.33E+02 -4.26E+02 -5.26E+02  1.97E+03
 
 OM88
+        1.93E+00  2.76E+00 -2.38E+00  2.18E-01 -1.41E+01 -1.65E+01 -1.83E+00  3.52E-01  9.30E+01  2.66E+02  1.68E+02  1.38E+02
         -9.37E+01 -1.97E+02  2.21E+02 -6.97E+02  1.99E+02  1.70E+02  1.50E+02 -1.43E+02 -2.61E+02  3.25E+02 -9.41E+02  3.47E+01
         6.48E+01 -7.47E+01 -1.07E+02  1.17E+02 -4.27E+02  1.00E+01 -5.72E+01 -7.33E+01  1.14E+02 -3.82E+02  5.06E+00  9.81E+01
         -1.00E+02  2.86E+02  6.60E+01 -1.86E+02  5.62E+02  1.14E+02 -7.60E+02  1.14E+03
 
 SG11
+       -5.60E+01 -1.72E+02  5.11E+00  2.50E+02 -1.01E+02 -3.83E+02  2.51E+02  2.76E+02 -7.20E+02 -2.70E+03 -6.45E+02 -4.52E+02
          8.90E+02  5.25E+02 -1.47E+03  1.94E+03 -3.50E+02  2.90E+03 -7.03E+02  9.13E+02  6.18E+02 -4.83E+02  1.73E+03 -1.43E+02
         9.07E+02  5.25E+02  3.72E+02  1.53E+03 -1.18E+03  1.17E+03  9.88E+02 -6.02E+01  1.09E+03 -8.00E+02 -3.51E+02 -7.34E+02
          2.12E+02 -1.85E+03 -8.22E+02 -1.08E+03 -2.61E+02  2.96E+02 -4.37E+02 -2.97E+02  2.42E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        5.46E+01  1.89E+01  4.96E+02 -7.18E+01  7.37E+00  1.23E+02 -5.12E+01 -1.35E+02 -4.69E+02 -1.06E+03 -4.72E+02  2.34E+02
          3.58E+02  4.60E+02 -3.13E+02  6.19E+02 -1.23E+03 -8.70E+02  3.96E+02  1.43E+03  1.20E+03 -9.29E+02  2.15E+03 -8.60E+02
        -3.00E+01  1.21E+03  2.93E+02  4.95E+02  8.48E+02 -2.18E+02  6.06E+01 -3.27E+02 -4.59E+01 -2.38E+02  2.48E+02  5.43E+01
          7.02E+02 -1.14E+03  7.62E+02  4.04E+02 -8.71E+02 -2.48E+02  7.05E+02 -4.73E+02  2.89E+04  0.00E+00  7.14E+05
 
 Elapsed postprocess time in seconds:     0.00
 Elapsed finaloutput time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     1602.645
Stop Time: 
Sat 04/22/2017 
10:18 AM

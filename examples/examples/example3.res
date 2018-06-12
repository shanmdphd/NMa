Sat 04/22/2017 
09:27 AM
;Model Desc: Population Mixture Problem in 1 Compartment model, 
; with Volume and rate constant parameters and their inter-subject 
; variances modeled from two sub-populations
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# example3 (from ad1tr1m2s)
$INPUT C SET ID JID TIME CONC=DV DOSE=AMT RATE EVID MDV CMT VC1 K101 
   VC2 K102 SIGZ PROB
$DATA example3.csv IGNORE=C

$SUBROUTINES ADVAN1 TRANS1

; The mixture model uses THETA(5) as the mixture proportion parameter, 
; defining the proportion of subjects in sub-population 1 (P(1), 
; and in sub-population 2 (P(2)

$MIX
P(1)=THETA(5)
P(2)=1.0-THETA(5)
NSPOP=2


$PK
;  The MUs should always be unconditionally defined, that is, 
;  they should never be defined in IF/THEN blocks
; THETA(1) models the Volume of sub-population 1
MU_1=THETA(1)
; THETA(2) models the clearance of sub-population 1
MU_2=THETA(2)
; THETA(3) models the Volume of sub-population 2
MU_3=THETA(3)
; THETA(4) models the clearance of sub-population 2
MU_4=THETA(4)
VCM=DEXP(MU_1+ETA(1))
K10M=DEXP(MU_2+ETA(2))
VCF=DEXP(MU_3+ETA(3))
K10F=DEXP(MU_4+ETA(4))
Q=1
IF(MIXNUM.EQ.2) Q=0
V=Q*VCM+(1.0-Q)*VCF
K=Q*K10M+(1.0-Q)*K10F
S1=V

$ERROR
Y = F + F*EPS(1)

; Initial THETAs
$THETA
(-1000.0  4.3 1000.0) ;[MU_1]
(-1000.0 -2.9 1000.0) ;[MU_2] 
(-1000.0 4.3 1000.0)  ;[MU_3]
(-1000.0 -0.67 1000.0) ;[MU_4]
(0.0001 0.667 0.9999)   ;[P(1)]

;Initial OMEGA block 1, for sub-population 1
$OMEGA BLOCK(2)
 .04 ;[p]
 .01 ; [f]
 .027; [p]

;Initial OMEGA block 2, for sub-population 2
$OMEGA BLOCK(2)
 .05; [p]
 .01; [f]
 .06; [p]

$SIGMA 
0.01 ;[p]

; Prior information setup for OMEGAS only
$PRIOR NWPRI

; Prior OMEGA block 1.  Note that because the OMEGA is separated 
; into blocks, so their priors should have the same block design.

$OMEGAP BLOCK(2)
 0.05 FIX
 0.0 0.05

; Prior OMEGA block 2

$OMEGAP BLOCK(2)
0.05 FIX
0.0 0.05

; Degrees of Freedom defined for Priors. 
; One for each OMEGA block defining each sub-popluation
$OMEGAPD (2 FIX) (2 FIX)


$EST METHOD=ITS INTERACTION NITER=20 PRINT=1 NOABORT SIGL=8 
     FILE=example3.ext CTYPE=3 CITER=10
     CALPHA=0.05 NOPRIOR=1

$EST NBURN=500 NITER=500 METHOD=SAEM INTERACTION PRINT=10 SIGL=6 
     ISAMPLE=2

$EST METHOD=IMP INTERACTION NITER=5 ISAMPLE=1000 PRINT=1 NOABORT 
     SIGL=6 EONLY=1 MAPITER=0

$EST METHOD=BAYES INTERACTION NBURN=2000 NITER=1000 PRINT=10  
     FILE=example3.txt SIGL=8 NOPRIOR=0

$EST MAXEVAL=9999 NSIG=3 SIGL=12 PRINT=1 FILE=example3.ext 
     METHOD=CONDITIONAL INTERACTION NOABORT
     NOPRIOR=1

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
 RUN# example3 (from ad1tr1m2s)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     2700
 NO. OF DATA ITEMS IN DATA SET:  17
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   5   7   8   0   0  11   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID JID TIME CONC DOSE RATE EVID MDV CMT VC1 K101 VC2 K102 SIGZ PROB
0FORMAT FOR DATA:
 (2E1.0,3E3.0,E10.0,E3.0,4E1.0,E6.0,E8.0,E6.0,E7.0,E3.0,E5.0)

 TOT. NO. OF OBS RECS:     2400
 TOT. NO. OF INDIVIDUALS:      300
0LENGTH OF THETA:   7
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  0  0  2
  0  0  2  2
  0  0  0  0  3
  0  0  0  0  3  3
  0  0  0  0  0  0  4
  0  0  0  0  0  0  4  4
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+04     0.4300E+01     0.1000E+04
 -0.1000E+04    -0.2900E+01     0.1000E+04
 -0.1000E+04     0.4300E+01     0.1000E+04
 -0.1000E+04    -0.6700E+00     0.1000E+04
  0.1000E-03     0.6670E+00     0.9999E+00
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.4000E-01
                  0.1000E-01   0.2700E-01
        2                                                                                   NO
                  0.5000E-01
                  0.1000E-01   0.6000E-01
        3                                                                                  YES
                  0.5000E-01
                  0.0000E+00   0.5000E-01
        4                                                                                  YES
                  0.5000E-01
                  0.0000E+00   0.5000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E-01
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
 MIX SUBROUTINE USER-SUPPLIED
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 beta 2 (nm74b2)

 ONE COMPARTMENT MODEL (ADVAN1)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   2
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1

0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         NO         YES        YES        YES
    2         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            3           *           *           *           *
    2            *           -           -           -           -
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
 NO. OF FUNCT. EVALS. ALLOWED:            1368
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
 RAW OUTPUT FILE (FILE): example3.ext
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
 CONVERGENCE INTERVAL (CINTERVAL):          1
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 ITERATIONS (NITER):                        20
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
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   5
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -7374.61519428428
 iteration            1 OBJ=  -9627.77072704561
 iteration            2 OBJ=  -9856.38204154988
 iteration            3 OBJ=  -9927.45182955687
 iteration            4 OBJ=  -9950.94052329500
 iteration            5 OBJ=  -9957.98579859582
 iteration            6 OBJ=  -9960.23054561306
 iteration            7 OBJ=  -9961.05769027513
 iteration            8 OBJ=  -9961.40325520310
 iteration            9 OBJ=  -9961.55833421530
 iteration           10 OBJ=  -9961.63029724236
 iteration           11 OBJ=  -9961.66424874172
 iteration           12 OBJ=  -9961.68046627653
 iteration           13 OBJ=  -9961.68832541685
 iteration           14 OBJ=  -9961.69220848256
 iteration           15 OBJ=  -9961.69417640778
 iteration           16 OBJ=  -9961.69520557596
 iteration           17 OBJ=  -9961.69576307627
 iteration           18 OBJ=  -9961.69607685571
 iteration           19 OBJ=  -9961.69625996048
 iteration           20 OBJ=  -9961.69637049949
 
 #TERM:
 OPTIMIZATION WAS NOT COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -2.5047E-07  1.5195E-06  0.0000E+00  0.0000E+00
 SE:             1.4891E-02  1.1044E-02  0.0000E+00  0.0000E+00
 N:                     200         200         200         200
 
 ETASHRINKSD(%)  1.9092E+00  5.0008E+00  1.0000E+02  1.0000E+02
 ETASHRINKVR(%)  3.7820E+00  9.7516E+00  1.0000E+02  1.0000E+02
 EBVSHRINKSD(%)  1.9912E+00  5.0815E+00  1.0000E+02  1.0000E+02
 EBVSHRINKVR(%)  3.9427E+00  9.9047E+00  1.0000E+02  1.0000E+02
 EPSSHRINKSD(%)  1.1410E+01
 EPSSHRINKVR(%)  2.1518E+01
 

 SUBMODEL    2
 
 ETABAR:         0.0000E+00  0.0000E+00 -1.0424E-05  2.6611E-05
 SE:             0.0000E+00  0.0000E+00  2.1782E-02  2.4454E-02
 N:                     100         100         100         100
 
 ETASHRINKSD(%)  1.0000E+02  1.0000E+02  1.5795E+00  1.0000E-10
 ETASHRINKVR(%)  1.0000E+02  1.0000E+02  3.1340E+00  1.0000E-10
 EBVSHRINKSD(%)  1.0000E+02  1.0000E+02  1.9072E+00  1.0440E-01
 EBVSHRINKVR(%)  1.0000E+02  1.0000E+02  3.7780E+00  2.0869E-01
 EPSSHRINKSD(%)  1.4366E+01
 EPSSHRINKVR(%)  2.6669E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -9961.69637049949     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5550.79141111706     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1200
  
 #TERE:
 Elapsed estimation  time in seconds:     8.47
 Elapsed covariance  time in seconds:     0.84
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9961.696       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.24E+00 -2.29E+00  4.26E+00 -6.75E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.63E-02
 
 ETA2
+       -8.56E-03  2.72E-02
 
 ETA3
+        0.00E+00  0.00E+00  4.95E-02
 
 ETA4
+        0.00E+00  0.00E+00 -1.10E-02  6.02E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.15E-01
 
 ETA2
+       -2.41E-01  1.65E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.22E-01
 
 ETA4
+        0.00E+00  0.00E+00 -2.02E-01  2.45E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.57E-02  1.26E-02  2.27E-02  2.46E-02  2.72E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        5.41E-03
 
 ETA2
+        2.73E-03  2.51E-03
 
 ETA3
+        0.00E+00  0.00E+00  7.40E-03
 
 ETA4
+        0.00E+00  0.00E+00  5.80E-03  8.90E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.43E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.26E-02
 
 ETA2
+        6.75E-02  7.61E-03
 
 ETA3
+       ......... .........  1.66E-02
 
 ETA4
+       ......... .........  9.69E-02  1.81E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.70E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        2.48E-04
 
 TH 2
+       -5.26E-05  1.58E-04
 
 TH 3
+        1.71E-07 -4.40E-07  5.18E-04
 
 TH 4
+        1.44E-07 -3.56E-07 -1.13E-04  6.07E-04
 
 TH 5
+       -2.70E-07  7.07E-07 -9.20E-07 -6.72E-07  7.42E-04
 
 OM11
+       -8.31E-06  3.04E-06  3.16E-07  2.63E-07 -5.01E-07  2.93E-05
 
 OM12
+        3.48E-06  3.41E-06 -9.09E-08 -7.65E-08  1.43E-07 -7.69E-06  7.46E-06
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.17E-06 -5.21E-06 -4.79E-08 -3.49E-08  7.98E-08  1.02E-07 -2.28E-06  0.00E+00  0.00E+00  6.30E-06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.33E-08  3.57E-08 -5.67E-06  4.20E-06  7.12E-08 -2.54E-08  7.29E-09  0.00E+00  0.00E+00  4.09E-09  0.00E+00  0.00E+00
          5.48E-05
 
 OM34
+       -5.19E-08  1.35E-07  4.22E-06 -4.38E-06  2.45E-07 -9.72E-08  2.80E-08  0.00E+00  0.00E+00  1.48E-08  0.00E+00  0.00E+00
         -1.48E-05  3.36E-05
 
 OM44
+       -1.09E-07  2.65E-07 -5.92E-06 -7.12E-06  4.97E-07 -1.98E-07  5.75E-08  0.00E+00  0.00E+00  2.52E-08  0.00E+00  0.00E+00
          3.57E-06 -2.06E-05  7.92E-05
 
 SG11
+        7.80E-08 -2.02E-07  2.59E-07  2.18E-07 -4.09E-07  1.44E-07 -4.13E-08  0.00E+00  0.00E+00 -2.22E-08  0.00E+00  0.00E+00
         -2.06E-08 -7.94E-08 -1.64E-07  1.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        1.57E-02
 
 TH 2
+       -2.66E-01  1.26E-02
 
 TH 3
+        4.77E-04 -1.54E-03  2.27E-02
 
 TH 4
+        3.72E-04 -1.15E-03 -2.02E-01  2.46E-02
 
 TH 5
+       -6.31E-04  2.07E-03 -1.49E-03 -1.00E-03  2.72E-02
 
 OM11
+       -9.75E-02  4.47E-02  2.57E-03  1.97E-03 -3.40E-03  5.41E-03
 
 OM12
+        8.10E-02  9.94E-02 -1.46E-03 -1.14E-03  1.92E-03 -5.20E-01  2.73E-03
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        8.02E-02 -1.65E-01 -8.39E-04 -5.64E-04  1.17E-03  7.47E-03 -3.33E-01  0.00E+00  0.00E+00  2.51E-03
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.14E-04  3.85E-04 -3.37E-02  2.30E-02  3.53E-04 -6.33E-04  3.61E-04  0.00E+00  0.00E+00  2.20E-04  0.00E+00  0.00E+00
          7.40E-03
 
 OM34
+       -5.70E-04  1.86E-03  3.20E-02 -3.06E-02  1.55E-03 -3.10E-03  1.77E-03  0.00E+00  0.00E+00  1.01E-03  0.00E+00  0.00E+00
         -3.45E-01  5.80E-03
 
 OM44
+       -7.79E-04  2.37E-03 -2.92E-02 -3.24E-02  2.05E-03 -4.10E-03  2.37E-03  0.00E+00  0.00E+00  1.13E-03  0.00E+00  0.00E+00
          5.43E-02 -3.98E-01  8.90E-03
 
 SG11
+        1.44E-02 -4.68E-02  3.32E-02  2.57E-02 -4.38E-02  7.75E-02 -4.40E-02  0.00E+00  0.00E+00 -2.58E-02  0.00E+00  0.00E+00
         -8.09E-03 -3.99E-02 -5.36E-02  3.43E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        4.43E+03
 
 TH 2
+        1.47E+03  7.10E+03
 
 TH 3
+       -3.49E-03 -7.51E-02  2.02E+03
 
 TH 4
+       -1.99E-02 -2.19E-01  3.79E+02  1.72E+03
 
 TH 5
+       -7.78E-03 -8.34E-02  2.88E-02 -1.45E-01  1.35E+03
 
 OM11
+        3.14E+02 -1.53E+03  2.25E-02  7.92E-02  3.01E-02  4.94E+04
 
 OM12
+       -3.08E+03 -4.33E+03 -4.35E-02 -2.84E-01 -1.12E-01  5.69E+04  2.20E+05
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.14E+03  3.63E+03 -5.94E-01 -1.75E+00 -6.77E-01  1.82E+04  7.66E+04  0.00E+00  0.00E+00  1.90E+05
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.82E-02  1.64E-01  1.54E+02 -2.57E+01  6.10E-01  2.00E-03 -3.37E-01  0.00E+00  0.00E+00  1.23E+00  0.00E+00  0.00E+00
          2.10E+04
 
 OM34
+        5.75E-02  1.43E+00 -5.45E+01  3.16E+02  2.64E+00 -4.15E-01  6.74E-01  0.00E+00  0.00E+00  1.13E+01  0.00E+00  0.00E+00
          1.03E+04  4.07E+04
 
 OM44
+        1.81E-01  2.05E+00  1.54E+02  2.59E+02  1.88E+00 -7.28E-01  2.55E+00  0.00E+00  0.00E+00  1.63E+01  0.00E+00  0.00E+00
          1.77E+03  1.02E+04  1.53E+04
 
 SG11
+       -2.29E+03  1.22E+04 -4.93E+03 -3.44E+03  4.69E+03 -3.98E+04  1.65E+04  0.00E+00  0.00E+00  4.81E+04  0.00E+00  0.00E+00
          1.28E+04  4.29E+04  2.76E+04  8.67E+06
 
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
 NO. OF FUNCT. EVALS. ALLOWED:            1368
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
 RAW OUTPUT FILE (FILE): example3.ext
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
 CONVERGENCE INTERVAL (CINTERVAL):          10
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 BURN-IN ITERATIONS (NBURN):                500
 ITERATIONS (NITER):                        500
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
   1   2   3   4
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   5
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration         -500 SAEMOBJ=  -15214.9133891495
 iteration         -490 SAEMOBJ=  -14931.8377597415
 iteration         -480 SAEMOBJ=  -15061.3666018877
 iteration         -470 SAEMOBJ=  -14939.3332416468
 iteration         -460 SAEMOBJ=  -15005.3228619070
 iteration         -450 SAEMOBJ=  -14960.6800773120
 iteration         -440 SAEMOBJ=  -14957.1857504366
 iteration         -430 SAEMOBJ=  -15029.9186326823
 iteration         -420 SAEMOBJ=  -14950.8149377926
 iteration         -410 SAEMOBJ=  -14910.5103554276
 iteration         -400 SAEMOBJ=  -14914.6914355993
 iteration         -390 SAEMOBJ=  -15004.6652607702
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -14952.8614090641
 iteration           10 SAEMOBJ=  -15107.0464075857
 iteration           20 SAEMOBJ=  -15102.3144221839
 iteration           30 SAEMOBJ=  -15099.4375677340
 iteration           40 SAEMOBJ=  -15097.4487807275
 iteration           50 SAEMOBJ=  -15095.3349042478
 iteration           60 SAEMOBJ=  -15094.3969844807
 iteration           70 SAEMOBJ=  -15096.5461230304
 iteration           80 SAEMOBJ=  -15096.6156798295
 iteration           90 SAEMOBJ=  -15096.1879349024
 iteration          100 SAEMOBJ=  -15095.4519446017
 iteration          110 SAEMOBJ=  -15093.8906731620
 iteration          120 SAEMOBJ=  -15093.4531950909
 iteration          130 SAEMOBJ=  -15093.5253419635
 iteration          140 SAEMOBJ=  -15093.6334638189
 iteration          150 SAEMOBJ=  -15094.3018135079
 iteration          160 SAEMOBJ=  -15094.5428559886
 iteration          170 SAEMOBJ=  -15094.4027655108
 iteration          180 SAEMOBJ=  -15094.2037089024
 iteration          190 SAEMOBJ=  -15093.9171142785
 iteration          200 SAEMOBJ=  -15093.5155391944
 iteration          210 SAEMOBJ=  -15093.1165382510
 iteration          220 SAEMOBJ=  -15092.9314988795
 iteration          230 SAEMOBJ=  -15092.7706278304
 iteration          240 SAEMOBJ=  -15092.8627156660
 iteration          250 SAEMOBJ=  -15092.9498958549
 iteration          260 SAEMOBJ=  -15092.9451784824
 iteration          270 SAEMOBJ=  -15092.8854195135
 iteration          280 SAEMOBJ=  -15092.7448616856
 iteration          290 SAEMOBJ=  -15092.5400111190
 iteration          300 SAEMOBJ=  -15092.4080973076
 iteration          310 SAEMOBJ=  -15092.4072135842
 iteration          320 SAEMOBJ=  -15092.4479089933
 iteration          330 SAEMOBJ=  -15092.5045570878
 iteration          340 SAEMOBJ=  -15092.1160463318
 iteration          350 SAEMOBJ=  -15092.0966506205
 iteration          360 SAEMOBJ=  -15092.1826912488
 iteration          370 SAEMOBJ=  -15092.0917178183
 iteration          380 SAEMOBJ=  -15091.9733880233
 iteration          390 SAEMOBJ=  -15092.0617517877
 iteration          400 SAEMOBJ=  -15091.9494455569
 iteration          410 SAEMOBJ=  -15091.8285140462
 iteration          420 SAEMOBJ=  -15091.8641832689
 iteration          430 SAEMOBJ=  -15091.8543858637
 iteration          440 SAEMOBJ=  -15091.9466375597
 iteration          450 SAEMOBJ=  -15091.8969779384
 iteration          460 SAEMOBJ=  -15091.8887778658
 iteration          470 SAEMOBJ=  -15091.7870664894
 iteration          480 SAEMOBJ=  -15091.6221150250
 iteration          490 SAEMOBJ=  -15091.6186588059
 iteration          500 SAEMOBJ=  -15091.8960344244
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:         7.1828E-04 -4.7651E-04 -1.2154E-03  2.4972E-03
 SE:             1.4883E-02  1.1107E-02  5.0408E-04  5.4390E-04
 N:                     200         200         200         200
 
 ETASHRINKSD(%)  1.7793E+00  4.8847E+00  9.6805E+01  9.6820E+01
 ETASHRINKVR(%)  3.5270E+00  9.5309E+00  9.9898E+01  9.9899E+01
 EBVSHRINKSD(%)  1.9937E+00  5.1737E+00  1.0000E+02  8.8285E+01
 EBVSHRINKVR(%)  3.9476E+00  1.0080E+01  1.0000E+02  9.8628E+01
 EPSSHRINKSD(%)  1.1592E+01
 EPSSHRINKVR(%)  2.1841E+01
 

 SUBMODEL    2
 
 ETABAR:        -1.4368E-03  9.6958E-04  2.4538E-03 -4.9734E-03
 SE:             6.4966E-04  5.5820E-04  2.1810E-02  2.4478E-02
 N:                     100         100         100         100
 
 ETASHRINKSD(%)  9.6961E+01  9.6611E+01  1.9989E+00  1.0000E-10
 ETASHRINKVR(%)  9.9908E+01  9.9885E+01  3.9579E+00  1.0000E-10
 EBVSHRINKSD(%)  9.2034E+01  9.0544E+01  1.8923E+00  1.1058E-01
 EBVSHRINKVR(%)  9.9365E+01  9.9106E+01  3.7488E+00  2.2104E-01
 EPSSHRINKSD(%)  1.4601E+01
 EPSSHRINKVR(%)  2.7070E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -15091.8960344244     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -10680.9910750420     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1200
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2205.45247969121     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -15091.8960344244     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -12886.4435547332     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    78.86
 Elapsed covariance  time in seconds:     0.64
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -15091.896       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.24E+00 -2.30E+00  4.26E+00 -6.71E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.62E-02
 
 ETA2
+       -8.58E-03  2.74E-02
 
 ETA3
+        0.00E+00  0.00E+00  5.00E-02
 
 ETA4
+        0.00E+00  0.00E+00 -1.06E-02  5.88E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.15E-01
 
 ETA2
+       -2.41E-01  1.66E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.24E-01
 
 ETA4
+        0.00E+00  0.00E+00 -1.96E-01  2.43E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.57E-02  1.26E-02  2.30E-02  2.40E-02  2.73E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        5.38E-03
 
 ETA2
+        2.73E-03  2.54E-03
 
 ETA3
+        0.00E+00  0.00E+00  7.53E-03
 
 ETA4
+        0.00E+00  0.00E+00  5.69E-03  8.48E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.43E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.25E-02
 
 ETA2
+        6.74E-02  7.68E-03
 
 ETA3
+       ......... .........  1.68E-02
 
 ETA4
+       ......... .........  9.63E-02  1.75E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.70E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        2.46E-04
 
 TH 2
+       -5.28E-05  1.59E-04
 
 TH 3
+       -6.69E-07 -1.45E-06  5.28E-04
 
 TH 4
+       -1.89E-06 -9.13E-07 -1.06E-04  5.78E-04
 
 TH 5
+       -2.89E-06  2.97E-06  7.18E-06 -1.71E-05  7.43E-04
 
 OM11
+       -8.83E-06  3.17E-06  9.60E-07  1.95E-07 -8.67E-07  2.89E-05
 
 OM12
+        3.72E-06  3.22E-06 -3.46E-07 -2.12E-07  2.80E-07 -7.66E-06  7.48E-06
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.12E-06 -5.34E-06  1.93E-07 -5.40E-08 -2.70E-07  1.00E-07 -2.30E-06  0.00E+00  0.00E+00  6.46E-06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -2.72E-07 -7.21E-08 -8.39E-06  4.62E-06 -1.64E-06 -9.81E-08 -6.01E-08  0.00E+00  0.00E+00  4.31E-08  0.00E+00  0.00E+00
          5.68E-05
 
 OM34
+        2.28E-07  4.24E-07  6.88E-06 -6.61E-06 -1.21E-06 -1.42E-07  2.62E-08  0.00E+00  0.00E+00  6.02E-08  0.00E+00  0.00E+00
         -1.42E-05  3.24E-05
 
 OM44
+       -1.62E-07 -3.48E-08 -7.02E-06 -4.71E-07  5.72E-06 -1.25E-07  6.77E-08  0.00E+00  0.00E+00  4.47E-08  0.00E+00  0.00E+00
          3.07E-06 -1.86E-05  7.19E-05
 
 SG11
+        7.18E-08 -2.57E-07  2.37E-07  2.19E-07 -4.38E-07  1.41E-07 -3.92E-08  0.00E+00  0.00E+00 -2.50E-08  0.00E+00  0.00E+00
         -1.18E-08 -8.95E-08 -1.50E-07  1.17E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        1.57E-02
 
 TH 2
+       -2.67E-01  1.26E-02
 
 TH 3
+       -1.86E-03 -5.02E-03  2.30E-02
 
 TH 4
+       -5.02E-03 -3.02E-03 -1.92E-01  2.40E-02
 
 TH 5
+       -6.76E-03  8.64E-03  1.15E-02 -2.61E-02  2.73E-02
 
 OM11
+       -1.05E-01  4.68E-02  7.77E-03  1.51E-03 -5.91E-03  5.38E-03
 
 OM12
+        8.67E-02  9.33E-02 -5.51E-03 -3.22E-03  3.76E-03 -5.21E-01  2.73E-03
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        7.81E-02 -1.67E-01  3.31E-03 -8.83E-04 -3.89E-03  7.35E-03 -3.31E-01  0.00E+00  0.00E+00  2.54E-03
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -2.30E-03 -7.60E-04 -4.85E-02  2.55E-02 -7.97E-03 -2.42E-03 -2.92E-03  0.00E+00  0.00E+00  2.25E-03  0.00E+00  0.00E+00
          7.53E-03
 
 OM34
+        2.55E-03  5.91E-03  5.26E-02 -4.83E-02 -7.78E-03 -4.63E-03  1.68E-03  0.00E+00  0.00E+00  4.16E-03  0.00E+00  0.00E+00
         -3.32E-01  5.69E-03
 
 OM44
+       -1.22E-03 -3.26E-04 -3.60E-02 -2.31E-03  2.48E-02 -2.74E-03  2.92E-03  0.00E+00  0.00E+00  2.08E-03  0.00E+00  0.00E+00
          4.81E-02 -3.85E-01  8.48E-03
 
 SG11
+        1.34E-02 -5.95E-02  3.00E-02  2.66E-02 -4.69E-02  7.67E-02 -4.18E-02  0.00E+00  0.00E+00 -2.87E-02  0.00E+00  0.00E+00
         -4.57E-03 -4.59E-02 -5.18E-02  3.43E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        4.46E+03
 
 TH 2
+        1.47E+03  7.05E+03
 
 TH 3
+        1.26E+01  1.64E+01  1.98E+03
 
 TH 4
+        1.82E+01  1.14E+01  3.61E+02  1.80E+03
 
 TH 5
+        1.17E+01 -1.23E+01 -1.45E+01  3.55E+01  1.35E+03
 
 OM11
+        3.99E+02 -1.49E+03 -4.30E+01  1.29E+01  2.85E+01  5.01E+04
 
 OM12
+       -3.08E+03 -4.04E+03  1.59E+00  4.63E+01  2.62E+01  5.70E+04  2.19E+05
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.05E+03  3.77E+03 -6.62E+01  5.07E+00  6.88E+01  1.80E+04  7.52E+04  0.00E+00  0.00E+00  1.86E+05
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.29E+01 -5.42E+00  2.05E+02 -1.21E+01  5.13E+01  1.44E+02  2.88E+02  0.00E+00  0.00E+00 -1.07E+02  0.00E+00  0.00E+00
          2.00E+04
 
 OM34
+       -3.81E+01 -5.86E+01 -1.96E+02  3.51E+02  4.58E+01  8.96E+01  1.12E+02  0.00E+00  0.00E+00 -3.27E+02  0.00E+00  0.00E+00
          9.74E+03  4.13E+04
 
 OM44
+        2.51E+00  2.62E+01  1.27E+02  1.28E+02 -8.88E+01 -5.25E+01 -1.20E+02  0.00E+00  0.00E+00 -1.40E+02  0.00E+00  0.00E+00
          1.70E+03  1.03E+04  1.66E+04
 
 SG11
+       -1.50E+03  1.57E+04 -4.61E+03 -3.51E+03  4.88E+03 -4.08E+04  1.33E+04  0.00E+00  0.00E+00  5.24E+04  0.00E+00  0.00E+00
          1.13E+04  4.53E+04  2.85E+04  8.72E+06
 
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
 NO. OF FUNCT. EVALS. ALLOWED:            1368
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
 RAW OUTPUT FILE (FILE): example3.ext
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
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   5
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -9967.56171043020 eff.=    1014. Smpl.=    1000. Fit.= 0.97914
 iteration            1 OBJ=  -9967.16100676108 eff.=     399. Smpl.=    1000. Fit.= 0.89962
 iteration            2 OBJ=  -9968.68367766041 eff.=     399. Smpl.=    1000. Fit.= 0.89940
 iteration            3 OBJ=  -9966.28511290699 eff.=     401. Smpl.=    1000. Fit.= 0.90059
 iteration            4 OBJ=  -9966.86967081711 eff.=     398. Smpl.=    1000. Fit.= 0.89964
 iteration            5 OBJ=  -9967.27886948714 eff.=     401. Smpl.=    1000. Fit.= 0.90034
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:         4.9116E-04 -4.2899E-04 -3.8681E-05  1.1481E-04
 SE:             1.4888E-02  1.1089E-02  4.6952E-04  5.1076E-04
 N:                     200         200         200         200
 
 ETASHRINKSD(%)  1.7505E+00  5.0355E+00  9.7024E+01  9.7014E+01
 ETASHRINKVR(%)  3.4703E+00  9.8174E+00  9.9911E+01  9.9911E+01
 EBVSHRINKSD(%)  2.0154E+00  5.2001E+00  9.3625E+01  1.0000E+02
 EBVSHRINKVR(%)  3.9901E+00  1.0130E+01  9.9594E+01  1.0000E+02
 EPSSHRINKSD(%)  1.1593E+01
 EPSSHRINKVR(%)  2.1842E+01
 

 SUBMODEL    2
 
 ETABAR:         5.4041E-04 -3.2263E-04  2.5490E-03 -4.9796E-03
 SE:             6.6927E-04  5.2598E-04  2.1785E-02  2.4477E-02
 N:                     100         100         100         100
 
 ETASHRINKSD(%)  9.6869E+01  9.6807E+01  2.1087E+00  1.0000E-10
 ETASHRINKVR(%)  9.9902E+01  9.9898E+01  4.1729E+00  1.0000E-10
 EBVSHRINKSD(%)  9.7857E+01  9.4449E+01  1.9070E+00  1.1113E-01
 EBVSHRINKVR(%)  9.9954E+01  9.9692E+01  3.7776E+00  2.2213E-01
 EPSSHRINKSD(%)  1.4584E+01
 EPSSHRINKVR(%)  2.7042E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -9967.27886948714     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5556.37391010471     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1200
  
 #TERE:
 Elapsed estimation  time in seconds:    16.80
 Elapsed covariance  time in seconds:     4.85
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9967.279       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.24E+00 -2.30E+00  4.26E+00 -6.71E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.62E-02
 
 ETA2
+       -8.58E-03  2.74E-02
 
 ETA3
+        0.00E+00  0.00E+00  5.00E-02
 
 ETA4
+        0.00E+00  0.00E+00 -1.06E-02  5.88E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.15E-01
 
 ETA2
+       -2.41E-01  1.66E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.24E-01
 
 ETA4
+        0.00E+00  0.00E+00 -1.96E-01  2.43E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.55E-02  1.23E-02  2.27E-02  2.43E-02  2.73E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.79E-03
 
 ETA2
+        2.79E-03  3.05E-03
 
 ETA3
+        0.00E+00  0.00E+00  7.47E-03
 
 ETA4
+        0.00E+00  0.00E+00  5.61E-03  8.15E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.42E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.11E-02
 
 ETA2
+        7.12E-02  9.22E-03
 
 ETA3
+       ......... .........  1.67E-02
 
 ETA4
+       ......... .........  9.77E-02  1.68E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.69E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        2.41E-04
 
 TH 2
+       -5.01E-05  1.52E-04
 
 TH 3
+       -1.02E-07  3.09E-07  5.16E-04
 
 TH 4
+        3.49E-08  1.73E-07 -1.09E-04  5.92E-04
 
 TH 5
+       -2.76E-07  2.61E-07  5.59E-06 -1.12E-05  7.43E-04
 
 OM11
+       -5.00E-07 -8.84E-09  3.32E-08  9.79E-09 -1.14E-07  2.29E-05
 
 OM12
+        1.62E-07  2.71E-07  3.61E-08 -5.02E-08  7.68E-08 -4.80E-06  7.81E-06
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.39E-07 -1.40E-06 -1.36E-07  3.58E-08 -3.72E-08  9.92E-07 -3.03E-06  0.00E+00  0.00E+00  9.32E-06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.78E-08 -2.07E-08 -2.54E-06  6.48E-07 -8.87E-07 -1.16E-08 -4.06E-08  0.00E+00  0.00E+00  4.34E-08  0.00E+00  0.00E+00
          5.58E-05
 
 OM34
+        7.49E-08 -2.72E-08  2.87E-06 -1.07E-06 -9.58E-07  2.95E-08  1.22E-08  0.00E+00  0.00E+00 -1.94E-08  0.00E+00  0.00E+00
         -1.09E-05  3.14E-05
 
 OM44
+        5.68E-08 -1.14E-07  7.83E-08  3.33E-06  2.71E-06 -4.25E-08 -2.06E-10  0.00E+00  0.00E+00  2.24E-08  0.00E+00  0.00E+00
          2.35E-06 -1.24E-05  6.64E-05
 
 SG11
+        9.17E-08 -3.03E-08  8.91E-08 -6.19E-09 -4.14E-07 -1.80E-08  1.35E-08  0.00E+00  0.00E+00 -2.88E-08  0.00E+00  0.00E+00
         -1.38E-08 -4.37E-10  7.13E-09  1.17E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        1.55E-02
 
 TH 2
+       -2.61E-01  1.23E-02
 
 TH 3
+       -2.90E-04  1.10E-03  2.27E-02
 
 TH 4
+        9.25E-05  5.76E-04 -1.97E-01  2.43E-02
 
 TH 5
+       -6.53E-04  7.75E-04  9.03E-03 -1.70E-02  2.73E-02
 
 OM11
+       -6.73E-03 -1.50E-04  3.06E-04  8.40E-05 -8.76E-04  4.79E-03
 
 OM12
+        3.74E-03  7.86E-03  5.70E-04 -7.39E-04  1.01E-03 -3.59E-01  2.79E-03
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.04E-03 -3.73E-02 -1.96E-03  4.83E-04 -4.47E-04  6.79E-02 -3.55E-01  0.00E+00  0.00E+00  3.05E-03
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.54E-04 -2.25E-04 -1.50E-02  3.57E-03 -4.36E-03 -3.26E-04 -1.94E-03  0.00E+00  0.00E+00  1.90E-03  0.00E+00  0.00E+00
          7.47E-03
 
 OM34
+        8.62E-04 -3.93E-04  2.25E-02 -7.84E-03 -6.27E-03  1.10E-03  7.80E-04  0.00E+00  0.00E+00 -1.13E-03  0.00E+00  0.00E+00
         -2.60E-01  5.61E-03
 
 OM44
+        4.49E-04 -1.13E-03  4.23E-04  1.68E-02  1.22E-02 -1.09E-03 -9.06E-06  0.00E+00  0.00E+00  9.02E-04  0.00E+00  0.00E+00
          3.86E-02 -2.71E-01  8.15E-03
 
 SG11
+        1.73E-02 -7.19E-03  1.15E-02 -7.44E-04 -4.44E-02 -1.10E-02  1.41E-02  0.00E+00  0.00E+00 -2.76E-02  0.00E+00  0.00E+00
         -5.42E-03 -2.28E-04  2.56E-03  3.42E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        4.46E+03
 
 TH 2
+        1.47E+03  7.05E+03
 
 TH 3
+        4.70E-01 -4.50E+00  2.02E+03
 
 TH 4
+       -6.59E-01 -3.10E+00  3.71E+02  1.76E+03
 
 TH 5
+       -5.76E-01 -1.42E+00 -1.04E+01  2.41E+01  1.35E+03
 
 OM11
+        7.93E+01  2.11E+01 -4.63E+00  7.64E-01  7.50E+00  5.03E+04
 
 OM12
+       -6.30E+01  1.54E+02  2.76E+00  1.08E+01 -1.05E+01  3.31E+04  1.68E+05
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.83E+01  1.08E+03  2.34E+01  1.61E+00  1.56E+01  5.41E+03  5.13E+04  0.00E+00  0.00E+00  1.24E+05
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -3.94E+00  2.76E+00  5.54E+01 -1.21E+00  3.06E+01  2.28E+01  8.17E+01  0.00E+00  0.00E+00 -4.40E+01  0.00E+00  0.00E+00
          1.93E+04
 
 OM34
+       -1.21E+01  9.25E+00 -1.74E+02 -9.66E+00  3.33E+01 -3.99E+01 -3.86E+01  0.00E+00  0.00E+00  2.20E+01  0.00E+00  0.00E+00
          6.92E+03  3.68E+04
 
 OM44
+       -2.99E+00  1.22E+01 -5.48E+01 -9.13E+01 -5.16E+01  2.13E+01 -5.35E+00  0.00E+00  0.00E+00 -3.42E+01  0.00E+00  0.00E+00
          6.06E+02  6.62E+03  1.63E+04
 
 SG11
+       -3.08E+03  9.28E+02 -1.54E+03 -9.97E+01  4.80E+03  5.26E+03 -1.62E+03  0.00E+00  0.00E+00  2.56E+04  0.00E+00  0.00E+00
          2.32E+03  8.18E+02 -1.04E+03  8.58E+06
 
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
 NO. OF FUNCT. EVALS. ALLOWED:            1368
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
 RAW OUTPUT FILE (FILE): example3.txt
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
 BURN-IN ITERATIONS (NBURN):                2000
 ITERATIONS (NITER):                        1000
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
 SAMPLES FOR LOCAL SEARCH KERNEL (PSAMPLE_M2):           1
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (PSAMPLE_M3):       1
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS
 SAMPLED OMEGAS:
 SAMPLE ACCEPTANCE RATE (OACCEPT):                       0.500000000000000
 SAMPLES FOR GLOBAL SEARCH KERNEL (OSAMPLE_M1):          -1
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           3
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):3
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
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   5
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3   4
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
   5
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -2000 MCMCOBJ=   -14690.5917203026     
 iteration        -1990 MCMCOBJ=   -14529.6169374154     
 iteration        -1980 MCMCOBJ=   -14763.5332620796     
 iteration        -1970 MCMCOBJ=   -14631.1153653610     
 iteration        -1960 MCMCOBJ=   -14598.4111294625     
 iteration        -1950 MCMCOBJ=   -14604.6682951408     
 iteration        -1940 MCMCOBJ=   -14755.5306239445     
 iteration        -1930 MCMCOBJ=   -14589.7708387182     
 iteration        -1920 MCMCOBJ=   -14637.7564142169     
 iteration        -1910 MCMCOBJ=   -14693.3160444861     
 iteration        -1900 MCMCOBJ=   -14723.6315073664     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -14715.3475086201     
 iteration           10 MCMCOBJ=   -14601.8273973767     
 iteration           20 MCMCOBJ=   -14721.7386762837     
 iteration           30 MCMCOBJ=   -14684.2135937032     
 iteration           40 MCMCOBJ=   -14801.5447158425     
 iteration           50 MCMCOBJ=   -14668.5546374127     
 iteration           60 MCMCOBJ=   -14632.0123647651     
 iteration           70 MCMCOBJ=   -14676.7262390147     
 iteration           80 MCMCOBJ=   -14742.2249930682     
 iteration           90 MCMCOBJ=   -14631.9240575126     
 iteration          100 MCMCOBJ=   -14587.0826163754     
 iteration          110 MCMCOBJ=   -14680.1180828105     
 iteration          120 MCMCOBJ=   -14702.3848361633     
 iteration          130 MCMCOBJ=   -14578.3595505499     
 iteration          140 MCMCOBJ=   -14492.0761156591     
 iteration          150 MCMCOBJ=   -14649.8344962369     
 iteration          160 MCMCOBJ=   -14665.3615177928     
 iteration          170 MCMCOBJ=   -14817.4395670032     
 iteration          180 MCMCOBJ=   -14640.2621261666     
 iteration          190 MCMCOBJ=   -14522.9520770595     
 iteration          200 MCMCOBJ=   -14636.4719376781     
 iteration          210 MCMCOBJ=   -14575.1288388738     
 iteration          220 MCMCOBJ=   -14567.6006016054     
 iteration          230 MCMCOBJ=   -14776.3334692991     
 iteration          240 MCMCOBJ=   -14658.1925718688     
 iteration          250 MCMCOBJ=   -14706.6273340264     
 iteration          260 MCMCOBJ=   -14705.0195606626     
 iteration          270 MCMCOBJ=   -14752.3757510566     
 iteration          280 MCMCOBJ=   -14672.1837531799     
 iteration          290 MCMCOBJ=   -14588.3549330651     
 iteration          300 MCMCOBJ=   -14536.6585671284     
 iteration          310 MCMCOBJ=   -14639.7335528159     
 iteration          320 MCMCOBJ=   -14577.2893096187     
 iteration          330 MCMCOBJ=   -14570.8428278291     
 iteration          340 MCMCOBJ=   -14622.8699481489     
 iteration          350 MCMCOBJ=   -14624.3502737124     
 iteration          360 MCMCOBJ=   -14747.1640759229     
 iteration          370 MCMCOBJ=   -14763.6186408564     
 iteration          380 MCMCOBJ=   -14689.6322096259     
 iteration          390 MCMCOBJ=   -14573.7233238445     
 iteration          400 MCMCOBJ=   -14686.2859302869     
 iteration          410 MCMCOBJ=   -14629.6836341740     
 iteration          420 MCMCOBJ=   -14664.7163330677     
 iteration          430 MCMCOBJ=   -14576.0227051207     
 iteration          440 MCMCOBJ=   -14572.5563348100     
 iteration          450 MCMCOBJ=   -14711.4333455982     
 iteration          460 MCMCOBJ=   -14687.7828487191     
 iteration          470 MCMCOBJ=   -14663.0865995999     
 iteration          480 MCMCOBJ=   -14637.7381090856     
 iteration          490 MCMCOBJ=   -14634.2176662553     
 iteration          500 MCMCOBJ=   -14787.5465533132     
 iteration          510 MCMCOBJ=   -14698.0762598133     
 iteration          520 MCMCOBJ=   -14782.6741310387     
 iteration          530 MCMCOBJ=   -14706.5689719941     
 iteration          540 MCMCOBJ=   -14659.9561944323     
 iteration          550 MCMCOBJ=   -14780.6488633662     
 iteration          560 MCMCOBJ=   -14655.3676259315     
 iteration          570 MCMCOBJ=   -14667.5416565955     
 iteration          580 MCMCOBJ=   -14627.6115339725     
 iteration          590 MCMCOBJ=   -14770.5432023585     
 iteration          600 MCMCOBJ=   -14749.8576717142     
 iteration          610 MCMCOBJ=   -14617.2607991415     
 iteration          620 MCMCOBJ=   -14670.7434922636     
 iteration          630 MCMCOBJ=   -14633.9937533827     
 iteration          640 MCMCOBJ=   -14647.8573317663     
 iteration          650 MCMCOBJ=   -14606.3264399456     
 iteration          660 MCMCOBJ=   -14557.0268891395     
 iteration          670 MCMCOBJ=   -14694.3750349656     
 iteration          680 MCMCOBJ=   -14577.4009647180     
 iteration          690 MCMCOBJ=   -14652.0609150406     
 iteration          700 MCMCOBJ=   -14611.9265465786     
 iteration          710 MCMCOBJ=   -14749.2179253971     
 iteration          720 MCMCOBJ=   -14737.2658357614     
 iteration          730 MCMCOBJ=   -14535.2050183707     
 iteration          740 MCMCOBJ=   -14705.6778235389     
 iteration          750 MCMCOBJ=   -14799.7873165081     
 iteration          760 MCMCOBJ=   -14677.2125958498     
 iteration          770 MCMCOBJ=   -14670.1379564603     
 iteration          780 MCMCOBJ=   -14664.1512274994     
 iteration          790 MCMCOBJ=   -14700.9561715381     
 iteration          800 MCMCOBJ=   -14665.4617605236     
 iteration          810 MCMCOBJ=   -14748.4638502443     
 iteration          820 MCMCOBJ=   -14618.4307947203     
 iteration          830 MCMCOBJ=   -14745.6661896968     
 iteration          840 MCMCOBJ=   -14733.2966707407     
 iteration          850 MCMCOBJ=   -14727.0974695631     
 iteration          860 MCMCOBJ=   -14486.3091551570     
 iteration          870 MCMCOBJ=   -14610.2662106725     
 iteration          880 MCMCOBJ=   -14564.4794448086     
 iteration          890 MCMCOBJ=   -14703.9129743736     
 iteration          900 MCMCOBJ=   -14703.0876475357     
 iteration          910 MCMCOBJ=   -14696.2966323627     
 iteration          920 MCMCOBJ=   -14727.3244743582     
 iteration          930 MCMCOBJ=   -14708.5802611083     
 iteration          940 MCMCOBJ=   -14534.1480300757     
 iteration          950 MCMCOBJ=   -14723.3475928390     
 iteration          960 MCMCOBJ=   -14630.3621287736     
 iteration          970 MCMCOBJ=   -14623.0133690538     
 iteration          980 MCMCOBJ=   -14616.5986125838     
 iteration          990 MCMCOBJ=   -14630.0403136935     
 iteration         1000 MCMCOBJ=   -14677.3458017562     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -14660.0286096253     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -10249.1236502429     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          1200
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2205.45247969121     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -14660.0286096253     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -12454.5761299341     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    28.5447777318297     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -14660.0286096253     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -14631.4838318935     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    99.52
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -14660.029       **************************************************
 #OBJS:********************************************       66.344 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.24E+00 -2.30E+00  4.26E+00 -6.77E-01  6.66E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.72E-02
 
 ETA2
+       -8.75E-03  2.81E-02
 
 ETA3
+        0.00E+00  0.00E+00  5.12E-02
 
 ETA4
+        0.00E+00  0.00E+00 -1.04E-02  6.31E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.17E-01
 
 ETA2
+       -2.39E-01  1.68E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.26E-01
 
 ETA4
+        0.00E+00  0.00E+00 -1.81E-01  2.51E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.56E-02  1.24E-02  2.24E-02  2.39E-02  2.67E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.99E-03
 
 ETA2
+        2.86E-03  3.12E-03
 
 ETA3
+        0.00E+00  0.00E+00  7.00E-03
 
 ETA4
+        0.00E+00  0.00E+00  5.61E-03  8.85E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.36E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.14E-02
 
 ETA2
+        7.00E-02  9.25E-03
 
 ETA3
+        0.00E+00  0.00E+00  1.54E-02
 
 ETA4
+        0.00E+00  0.00E+00  9.05E-02  1.74E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.66E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        2.45E-04
 
 TH 2
+       -3.73E-05  1.53E-04
 
 TH 3
+       -1.05E-05  1.77E-06  5.04E-04
 
 TH 4
+        9.46E-06 -1.46E-06 -3.85E-05  5.71E-04
 
 TH 5
+       -3.82E-06 -2.00E-05  1.12E-05 -1.16E-05  7.13E-04
 
 OM11
+        1.42E-06  5.39E-06 -3.46E-06  1.08E-05 -2.55E-06  2.49E-05
 
 OM12
+        1.39E-06 -8.50E-07  3.22E-07 -6.64E-06 -1.34E-06 -5.75E-06  8.19E-06
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.02E-06 -1.87E-06 -1.51E-08  3.38E-06  4.51E-07  1.78E-06 -3.51E-06  0.00E+00  0.00E+00  9.74E-06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        4.22E-06  2.15E-06  3.41E-06  1.61E-05 -4.03E-06  2.64E-07  1.07E-06  0.00E+00  0.00E+00  1.63E-06  0.00E+00  0.00E+00
          4.90E-05
 
 OM34
+       -2.16E-06 -1.86E-06  8.18E-06 -1.60E-05 -8.00E-06 -9.43E-07 -6.69E-07  0.00E+00  0.00E+00 -2.53E-07  0.00E+00  0.00E+00
         -1.33E-05  3.15E-05
 
 OM44
+       -5.88E-06  5.41E-06 -1.18E-05  1.04E-05  8.81E-06  3.02E-06 -1.71E-06  0.00E+00  0.00E+00  6.45E-07  0.00E+00  0.00E+00
         -7.85E-08 -1.22E-05  7.83E-05
 
 SG11
+        2.41E-07  1.43E-07 -3.11E-08  2.59E-07 -6.53E-08 -1.26E-07  6.69E-08  0.00E+00  0.00E+00 -2.07E-08  0.00E+00  0.00E+00
         -3.64E-09  1.58E-08  7.56E-08  1.13E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        1.56E-02
 
 TH 2
+       -1.92E-01  1.24E-02
 
 TH 3
+       -2.98E-02  6.35E-03  2.24E-02
 
 TH 4
+        2.53E-02 -4.93E-03 -7.18E-02  2.39E-02
 
 TH 5
+       -9.14E-03 -6.06E-02  1.87E-02 -1.82E-02  2.67E-02
 
 OM11
+        1.82E-02  8.72E-02 -3.09E-02  9.08E-02 -1.91E-02  4.99E-03
 
 OM12
+        3.11E-02 -2.40E-02  5.00E-03 -9.70E-02 -1.76E-02 -4.03E-01  2.86E-03
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.10E-02 -4.83E-02 -2.16E-04  4.54E-02  5.41E-03  1.14E-01 -3.93E-01  0.00E+00  0.00E+00  3.12E-03
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.85E-02  2.48E-02  2.17E-02  9.65E-02 -2.16E-02  7.57E-03  5.33E-02  0.00E+00  0.00E+00  7.47E-02  0.00E+00  0.00E+00
          7.00E-03
 
 OM34
+       -2.47E-02 -2.67E-02  6.50E-02 -1.19E-01 -5.34E-02 -3.37E-02 -4.16E-02  0.00E+00  0.00E+00 -1.44E-02  0.00E+00  0.00E+00
         -3.39E-01  5.61E-03
 
 OM44
+       -4.25E-02  4.93E-02 -5.96E-02  4.91E-02  3.73E-02  6.85E-02 -6.74E-02  0.00E+00  0.00E+00  2.33E-02  0.00E+00  0.00E+00
         -1.27E-03 -2.46E-01  8.85E-03
 
 SG11
+        4.58E-02  3.44E-02 -4.12E-03  3.23E-02 -7.28E-03 -7.50E-02  6.96E-02  0.00E+00  0.00E+00 -1.97E-02  0.00E+00  0.00E+00
         -1.55E-03  8.39E-03  2.54E-02  3.36E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        4.29E+03
 
 TH 2
+        1.07E+03  6.92E+03
 
 TH 3
+        8.07E+01 -2.35E+01  2.01E+03
 
 TH 4
+       -4.35E+01  5.64E+01  1.23E+02  1.82E+03
 
 TH 5
+        4.30E+01  1.99E+02 -3.90E+01  3.58E+01  1.42E+03
 
 OM11
+       -7.60E+02 -1.63E+03  2.30E+02 -4.65E+02  2.05E+02  4.92E+04
 
 OM12
+       -1.19E+03  2.84E+02  2.10E+02  1.32E+03  4.31E+02  3.55E+04  1.75E+05
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -5.14E+02  1.65E+03  1.18E+01  2.69E+01  5.82E+01  4.02E+03  5.69E+04  0.00E+00  0.00E+00  1.24E+05
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -2.79E+02 -3.79E+02 -3.35E+02 -4.40E+02  2.10E+02 -5.30E+02 -4.91E+03  0.00E+00  0.00E+00 -5.38E+03  0.00E+00  0.00E+00
          2.38E+04
 
 OM34
+        2.87E+02  2.57E+02 -5.14E+02  7.05E+02  4.66E+02  1.29E+03  4.80E+03  0.00E+00  0.00E+00  1.04E+02  0.00E+00  0.00E+00
          1.04E+04  3.93E+04
 
 OM44
+        3.24E+02 -3.24E+02  2.15E+02 -7.31E+01 -1.07E+02 -8.66E+02  2.46E+03  0.00E+00  0.00E+00 -7.68E+01  0.00E+00  0.00E+00
          1.59E+03  5.97E+03  1.39E+04
 
 SG11
+       -1.09E+04 -1.26E+04  1.60E+02 -5.47E+03  3.82E+02  3.98E+04 -5.67E+04  0.00E+00  0.00E+00 -7.73E+03  0.00E+00  0.00E+00
          1.70E+03 -1.30E+04 -1.27E+04  9.01E+06
 
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
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      12
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     12
 NOPRIOR SETTING (NOPRIOR):                 ON
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example3.ext
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -9961.12013593427        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:        8
 NPARAMETR:  4.2396E+00 -2.3011E+00  4.2586E+00 -6.7658E-01  6.6636E-01  4.7249E-02 -8.7537E-03  2.8148E-02  5.1181E-02 -1.0353E-02
             6.3114E-02  1.0241E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:  -1.1105E+04 -3.1619E+04 -4.7181E+03  3.5219E+04 -1.8079E-01  8.5004E+00  1.7461E+00  1.0375E+01  7.4061E+00  6.6133E+00
             1.1392E+01  4.7712E+00
 
0ITERATION NO.:    1    OBJECTIVE VALUE:  -9961.25523711868        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:       25
 NPARAMETR:  4.2409E+00 -2.2975E+00  4.2592E+00 -6.8060E-01  6.6636E-01  4.7249E-02 -8.7537E-03  2.8148E-02  5.1181E-02 -1.0353E-02
             6.3114E-02  1.0241E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  9.9992E-02  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:  -4.3325E+02 -4.9514E+03 -4.9871E+03  2.8815E+04 -1.8077E-01  8.5561E+00  1.7913E+00  1.1011E+01  7.4109E+00  6.5593E+00
             1.1292E+01  4.2568E+00
 
0ITERATION NO.:    2    OBJECTIVE VALUE:  -9961.25523711868        NO. OF FUNC. EVALS.:  30
 CUMULATIVE NO. OF FUNC. EVALS.:       55
 NPARAMETR:  4.2409E+00 -2.2975E+00  4.2592E+00 -6.8060E-01  6.6636E-01  4.7249E-02 -8.7537E-03  2.8148E-02  5.1181E-02 -1.0353E-02
             6.3114E-02  1.0241E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  9.9992E-02  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01
             1.0000E-01  1.0000E-01
 GRADIENT:  -2.4439E+03 -3.8125E+03 -5.9927E+03  2.0061E+05 -1.8077E-01  8.5561E+00  1.7913E+00  1.1011E+01  7.4109E+00  6.5593E+00
             1.1292E+01  4.2563E+00
 
0ITERATION NO.:    3    OBJECTIVE VALUE:  -9961.69209702056        NO. OF FUNC. EVALS.:  46
 CUMULATIVE NO. OF FUNC. EVALS.:      101             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2419E+00 -2.2964E+00  4.2639E+00 -6.7174E-01  6.6696E-01  4.6419E-02 -8.7522E-03  2.7471E-02  4.9472E-02 -1.1368E-02
             6.0203E-02  1.0213E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0001E-01  1.0271E-01  9.1139E-02 -1.0087E-01  8.6528E-02  8.3020E-02 -1.1169E-01
             7.1084E-02  9.8649E-02
 GRADIENT:   5.7545E+03  3.7036E+03  7.5247E+03  4.5430E+04  1.7859E-01  1.4738E+00 -3.0385E+00  2.1240E+00  3.4794E-01 -2.1600E+00
             2.6664E-01 -7.3255E+00
 
0ITERATION NO.:    4    OBJECTIVE VALUE:  -9961.72258776824        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:      110
 NPARAMETR:  4.2419E+00 -2.2963E+00  4.2637E+00 -6.7646E-01  6.6698E-01  4.6295E-02 -8.7370E-03  2.7363E-02  4.9240E-02 -1.1458E-02
             5.9807E-02  1.0215E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0278E-01  8.9798E-02 -1.0083E-01  8.4459E-02  8.0674E-02 -1.1284E-01
             6.7161E-02  9.8717E-02
 GRADIENT:   5.8670E+03  4.2506E+03  5.2867E+03  3.7170E+04  1.8878E-01  4.4144E-01 -3.4335E+00  7.5687E-01 -6.2069E-01 -3.0872E+00
            -1.2492E+00 -6.9458E+00
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -9961.72258776824        NO. OF FUNC. EVALS.:  24
 CUMULATIVE NO. OF FUNC. EVALS.:      134
 NPARAMETR:  4.2419E+00 -2.2963E+00  4.2637E+00 -6.7646E-01  6.6698E-01  4.6295E-02 -8.7370E-03  2.7363E-02  4.9240E-02 -1.1458E-02
             5.9807E-02  1.0215E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0278E-01  8.9798E-02 -1.0083E-01  8.4459E-02  8.0674E-02 -1.1284E-01
             6.7161E-02  9.8717E-02
 GRADIENT:   3.8512E+03  5.3920E+03  4.2782E+03  2.0938E+05  1.8877E-01  4.4144E-01 -3.4335E+00  7.5687E-01 -6.2069E-01 -3.0872E+00
            -1.2492E+00 -6.9463E+00
 
0ITERATION NO.:    6    OBJECTIVE VALUE:  -9961.74089057695        NO. OF FUNC. EVALS.:  41
 CUMULATIVE NO. OF FUNC. EVALS.:      175             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2415E+00 -2.2965E+00  4.2621E+00 -6.7587E-01  6.6635E-01  4.6246E-02 -8.5826E-03  2.7259E-02  4.9398E-02 -1.0997E-02
             5.9941E-02  1.0257E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  9.9954E-02  8.9268E-02 -9.9103E-02  8.3525E-02  8.2266E-02 -1.0812E-01
             7.0231E-02  1.0080E-01
 GRADIENT:   3.4804E+03  1.9543E+03  2.1479E+03  3.7382E+04 -1.8810E-01  5.5572E-01  1.2393E-01  3.1231E-01  3.8718E-01  3.3797E-01
            -1.4603E-02  7.9898E+00
 
0ITERATION NO.:    7    OBJECTIVE VALUE:  -9961.74089057695        NO. OF FUNC. EVALS.:  26
 CUMULATIVE NO. OF FUNC. EVALS.:      201
 NPARAMETR:  4.2415E+00 -2.2965E+00  4.2621E+00 -6.7587E-01  6.6635E-01  4.6246E-02 -8.5826E-03  2.7259E-02  4.9398E-02 -1.0997E-02
             5.9941E-02  1.0257E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  9.9954E-02  8.9268E-02 -9.9103E-02  8.3525E-02  8.2266E-02 -1.0812E-01
             7.0231E-02  1.0080E-01
 GRADIENT:   1.4727E+03  3.0904E+03  1.1435E+03  2.0890E+05 -1.8810E-01  5.5572E-01  1.2393E-01  3.1231E-01  3.8718E-01  3.3797E-01
            -1.4603E-02  7.9893E+00
 
0ITERATION NO.:    8    OBJECTIVE VALUE:  -9961.74613227386        NO. OF FUNC. EVALS.:  42
 CUMULATIVE NO. OF FUNC. EVALS.:      243             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2416E+00 -2.2964E+00  4.2620E+00 -6.7568E-01  6.6666E-01  4.6221E-02 -8.5831E-03  2.7248E-02  4.9343E-02 -1.1017E-02
             5.9955E-02  1.0233E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0136E-01  8.9002E-02 -9.9135E-02  8.3295E-02  8.1714E-02 -1.0838E-01
             7.0246E-02  9.9606E-02
 GRADIENT:   3.9395E+03  2.4962E+03  2.1973E+03  3.7798E+04 -4.1275E-04  3.1230E-01  1.5267E-02  9.5177E-02  1.4268E-01  1.1295E-01
            -7.8620E-03 -5.2254E-01
 
0ITERATION NO.:    9    OBJECTIVE VALUE:  -9961.74613228182        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      257
 NPARAMETR:  4.2416E+00 -2.2964E+00  4.2620E+00 -6.7568E-01  6.6666E-01  4.6221E-02 -8.5831E-03  2.7248E-02  4.9343E-02 -1.1017E-02
             5.9955E-02  1.0233E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0136E-01  8.9002E-02 -9.9135E-02  8.3295E-02  8.1714E-02 -1.0838E-01
             7.0246E-02  9.9606E-02
 GRADIENT:   3.9395E+03  2.4962E+03  2.1973E+03  3.7798E+04 -3.9480E-04  3.1228E-01  1.5258E-02  9.5160E-02  1.4267E-01  1.1294E-01
            -7.8616E-03 -5.2314E-01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -9961.74613228182        NO. OF FUNC. EVALS.:  25
 CUMULATIVE NO. OF FUNC. EVALS.:      282
 NPARAMETR:  4.2416E+00 -2.2964E+00  4.2620E+00 -6.7568E-01  6.6666E-01  4.6221E-02 -8.5831E-03  2.7248E-02  4.9343E-02 -1.1017E-02
             5.9955E-02  1.0233E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0136E-01  8.9002E-02 -9.9135E-02  8.3295E-02  8.1714E-02 -1.0838E-01
             7.0246E-02  9.9606E-02
 GRADIENT:   1.9272E+03  3.6353E+03  1.1906E+03  2.0972E+05 -3.9947E-04  3.1228E-01  1.5258E-02  9.5160E-02  1.4267E-01  1.1294E-01
            -7.8616E-03 -5.2364E-01
 
0ITERATION NO.:   11    OBJECTIVE VALUE:  -9961.74634963080        NO. OF FUNC. EVALS.:  43
 CUMULATIVE NO. OF FUNC. EVALS.:      325             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2416E+00 -2.2964E+00  4.2619E+00 -6.7549E-01  6.6666E-01  4.6186E-02 -8.5803E-03  2.7238E-02  4.9307E-02 -1.1029E-02
             5.9964E-02  1.0236E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0137E-01  8.8620E-02 -9.9141E-02  8.3092E-02  8.1348E-02 -1.0854E-01
             7.0268E-02  9.9743E-02
 GRADIENT:   4.0417E+03  2.7763E+03  2.0682E+03  3.8086E+04  0.0000E+00  1.8837E-02 -1.1860E-01 -3.0762E-02 -5.3787E-03 -2.5331E-02
             1.7270E-03  4.2688E-01
 
0ITERATION NO.:   12    OBJECTIVE VALUE:  -9961.74634963080        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      342
 NPARAMETR:  4.2416E+00 -2.2964E+00  4.2619E+00 -6.7549E-01  6.6666E-01  4.6186E-02 -8.5803E-03  2.7238E-02  4.9307E-02 -1.1029E-02
             5.9964E-02  1.0236E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0137E-01  8.8620E-02 -9.9141E-02  8.3092E-02  8.1348E-02 -1.0854E-01
             7.0268E-02  9.9743E-02
 GRADIENT:   2.0299E+03  3.9150E+03  1.0618E+03  2.0997E+05 -3.8582E-06  1.8837E-02 -1.1860E-01 -3.0762E-02 -5.3787E-03 -2.5331E-02
             1.7270E-03  4.2638E-01
 
0ITERATION NO.:   13    OBJECTIVE VALUE:  -9961.74636335185        NO. OF FUNC. EVALS.:  39
 CUMULATIVE NO. OF FUNC. EVALS.:      381             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2416E+00 -2.2964E+00  4.2619E+00 -6.7546E-01  6.6666E-01  4.6184E-02 -8.5759E-03  2.7239E-02  4.9308E-02 -1.1026E-02
             5.9962E-02  1.0233E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0137E-01  8.8602E-02 -9.9093E-02  8.3139E-02  8.1362E-02 -1.0850E-01
             7.0264E-02  9.9635E-02
 GRADIENT:   4.0070E+03  2.7648E+03  2.0126E+03  3.8120E+04  0.0000E+00  1.5833E-02 -9.9121E-03 -3.7561E-03  1.1670E-03  1.3494E-03
             2.5085E-04 -3.4026E-01
 
0ITERATION NO.:   14    OBJECTIVE VALUE:  -9961.74636335185        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:      398
 NPARAMETR:  4.2416E+00 -2.2964E+00  4.2619E+00 -6.7546E-01  6.6666E-01  4.6184E-02 -8.5759E-03  2.7239E-02  4.9308E-02 -1.1026E-02
             5.9962E-02  1.0233E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0137E-01  8.8602E-02 -9.9093E-02  8.3139E-02  8.1362E-02 -1.0850E-01
             7.0264E-02  9.9635E-02
 GRADIENT:   1.9948E+03  3.9037E+03  1.0060E+03  2.1004E+05 -3.5890E-07  1.5833E-02 -9.9121E-03 -3.7561E-03  1.1670E-03  1.3494E-03
             2.5085E-04 -3.4075E-01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -9961.74636335185        NO. OF FUNC. EVALS.:  29
 CUMULATIVE NO. OF FUNC. EVALS.:      427
 NPARAMETR:  4.2416E+00 -2.2964E+00  4.2619E+00 -6.7546E-01  6.6666E-01  4.6184E-02 -8.5759E-03  2.7239E-02  4.9308E-02 -1.1026E-02
             5.9962E-02  1.0233E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0000E-01  1.0137E-01  8.8602E-02 -9.9093E-02  8.3139E-02  8.1362E-02 -1.0850E-01
             7.0264E-02  9.9635E-02
 GRADIENT:   9.4668E+00 -8.9682E+00  1.2749E+01 -1.2124E+01 -3.5890E-07  1.5515E-02 -5.1315E-03 -4.5764E-03  1.5797E-03  1.1203E-03
             9.0222E-05 -3.2690E-01
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.
 REGARD THE RESULTS OF THE ESTIMATION STEP CAREFULLY, AND ACCEPT THEM ONLY
 AFTER CHECKING THAT THE COVARIANCE STEP PRODUCES REASONABLE OUTPUT.
 NO. OF FUNCTION EVALUATIONS USED:      427
 NO. OF SIG. DIGITS IN FINAL EST.:  3.0

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -1.1478E-03  2.3836E-03  0.0000E+00  0.0000E+00
 SE:             1.4890E-02  1.1050E-02  0.0000E+00  0.0000E+00
 N:                     200         200           0           0
 
 ETASHRINKSD(%)  1.7695E+00  5.0752E+00  1.0000E+02  1.0000E+02
 ETASHRINKVR(%)  3.5078E+00  9.8928E+00  1.0000E+02  1.0000E+02
 EBVSHRINKSD(%)  1.9971E+00  5.0713E+00  1.0000E+02  1.0000E+02
 EBVSHRINKVR(%)  3.9544E+00  9.8855E+00  1.0000E+02  1.0000E+02
 EPSSHRINKSD(%)  1.1415E+01
 EPSSHRINKVR(%)  2.1527E+01
 

 SUBMODEL    2
 
 ETABAR:         0.0000E+00  0.0000E+00 -2.8904E-04  1.4249E-04
 SE:             0.0000E+00  0.0000E+00  2.1779E-02  2.4454E-02
 N:                       0           0         100         100
 
 ETASHRINKSD(%)  1.0000E+02  1.0000E+02  1.4264E+00  1.0000E-10
 ETASHRINKVR(%)  1.0000E+02  1.0000E+02  2.8324E+00  1.0000E-10
 EBVSHRINKSD(%)  1.0000E+02  1.0000E+02  1.9134E+00  1.0473E-01
 EBVSHRINKVR(%)  1.0000E+02  1.0000E+02  3.7902E+00  2.0935E-01
 EPSSHRINKSD(%)  1.4364E+01
 EPSSHRINKVR(%)  2.6664E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -9961.74636335185     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5550.84140396942     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           600
  
 #TERE:
 Elapsed estimation  time in seconds:    52.26
 Elapsed covariance  time in seconds:    19.32
 Elapsed postprocess time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9961.746       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         4.24E+00 -2.30E+00  4.26E+00 -6.75E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.62E-02
 
 ETA2
+       -8.58E-03  2.72E-02
 
 ETA3
+        0.00E+00  0.00E+00  4.93E-02
 
 ETA4
+        0.00E+00  0.00E+00 -1.10E-02  6.00E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        2.15E-01
 
 ETA2
+       -2.42E-01  1.65E-01
 
 ETA3
+        0.00E+00  0.00E+00  2.22E-01
 
 ETA4
+        0.00E+00  0.00E+00 -2.03E-01  2.45E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5     
 
         1.55E-02  1.23E-02  2.26E-02  2.45E-02  2.72E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        4.81E-03
 
 ETA2
+        2.79E-03  3.02E-03
 
 ETA3
+       ......... .........  7.25E-03
 
 ETA4
+       ......... .........  5.67E-03  8.51E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.43E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4     
 
 ETA1
+        1.12E-02
 
 ETA2
+        7.13E-02  9.15E-03
 
 ETA3
+       ......... .........  1.63E-02
 
 ETA4
+       ......... .........  9.79E-02  1.74E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.69E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        2.41E-04
 
 TH 2
+       -5.01E-05  1.51E-04
 
 TH 3
+        1.10E-07  1.91E-08  5.13E-04
 
 TH 4
+       -2.07E-09  1.87E-08 -1.13E-04  6.01E-04
 
 TH 5
+       -1.01E-09  5.37E-09  2.30E-09  2.39E-08  7.41E-04
 
 OM11
+        6.85E-08 -1.54E-07 -1.95E-08 -1.18E-09 -2.70E-10  2.32E-05
 
 OM12
+       -7.85E-08  3.95E-07  1.37E-08 -1.27E-09 -4.21E-10 -4.83E-06  7.79E-06
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.16E-07 -1.27E-06 -3.08E-08  8.20E-09  2.50E-09  9.95E-07 -3.00E-06 ......... .........  9.12E-06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -2.36E-08 -3.33E-09  1.37E-07 -7.47E-08  4.66E-10  4.06E-09 -2.91E-09 ......... .........  6.92E-09 ......... .........
          5.26E-05
 
 OM34
+        5.18E-09 -1.75E-09 -3.95E-08  1.97E-07 -3.06E-09 -8.42E-10  9.65E-10 ......... ......... -2.37E-09 ......... .........
         -1.16E-05  3.21E-05
 
 OM44
+       -5.20E-10 -1.93E-08  3.39E-07 -3.98E-07 -2.42E-08  1.66E-09  9.37E-10 ......... ......... -7.54E-09 ......... .........
          2.59E-06 -1.36E-05  7.24E-05
 
 SG11
+        1.15E-07  1.79E-08  1.13E-07  1.42E-09 -4.83E-12 -2.04E-08  1.46E-08 ......... ......... -3.28E-08 ......... .........
         -2.43E-08  4.38E-09 -4.07E-09  1.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        1.55E-02
 
 TH 2
+       -2.62E-01  1.23E-02
 
 TH 3
+        3.13E-04  6.84E-05  2.26E-02
 
 TH 4
+       -5.44E-06  6.21E-05 -2.04E-01  2.45E-02
 
 TH 5
+       -2.40E-06  1.60E-05  3.73E-06  3.58E-05  2.72E-02
 
 OM11
+        9.17E-04 -2.59E-03 -1.79E-04 -1.00E-05 -2.06E-06  4.81E-03
 
 OM12
+       -1.81E-03  1.15E-02  2.16E-04 -1.85E-05 -5.54E-06 -3.60E-01  2.79E-03
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.74E-03 -3.42E-02 -4.50E-04  1.11E-04  3.04E-05  6.85E-02 -3.56E-01 ......... .........  3.02E-03
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -2.09E-04 -3.73E-05  8.36E-04 -4.20E-04  2.36E-06  1.16E-04 -1.44E-04 ......... .........  3.16E-04 ......... .........
          7.25E-03
 
 OM34
+        5.89E-05 -2.51E-05 -3.08E-04  1.42E-03 -1.98E-05 -3.09E-05  6.10E-05 ......... ......... -1.38E-04 ......... .........
         -2.83E-01  5.67E-03
 
 OM44
+       -3.94E-06 -1.85E-04  1.76E-03 -1.91E-03 -1.05E-04  4.05E-05  3.95E-05 ......... ......... -2.93E-04 ......... .........
          4.19E-02 -2.83E-01  8.51E-03
 
 SG11
+        2.16E-02  4.24E-03  1.46E-02  1.69E-04 -5.17E-07 -1.23E-02  1.52E-02 ......... ......... -3.17E-02 ......... .........
         -9.75E-03  2.25E-03 -1.40E-03  3.43E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM14      OM22      OM23      OM24  
             OM33      OM34      OM44      SG11  
 
 TH 1
+        4.46E+03
 
 TH 2
+        1.48E+03  7.11E+03
 
 TH 3
+       -3.50E-03 -7.85E-02  2.04E+03
 
 TH 4
+       -2.09E-02 -2.37E-01  3.83E+02  1.74E+03
 
 TH 5
+       -4.73E-03 -5.25E-02 -1.90E-02 -5.70E-02  1.35E+03
 
 OM11
+       -1.24E+01  5.30E+00  2.25E-02  8.23E-02  1.85E-02  4.98E+04
 
 OM12
+       -1.73E+01  2.34E+01 -4.32E-02 -2.95E-01 -6.76E-02  3.30E+04  1.69E+05
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.06E+01  9.38E+02 -5.92E-01 -1.81E+00 -4.01E-01  5.44E+03  5.20E+04 ......... .........  1.26E+05
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.81E-02  1.71E-01 -6.48E+00 -7.33E-01  4.31E-02  1.23E-03 -3.31E-01 ......... .........  1.23E+00 ......... .........
          2.07E+04
 
 OM34
+        5.95E-02  1.50E+00 -5.47E+00 -7.74E+00  3.64E-01 -4.21E-01  7.00E-01 ......... .........  1.13E+01 ......... .........
          7.80E+03  3.68E+04
 
 OM44
+        1.85E-01  2.15E+00 -8.34E+00  6.29E+00  5.18E-01 -7.36E-01  2.58E+00 ......... .........  1.64E+01 ......... .........
          7.30E+02  6.65E+03  1.50E+04
 
 SG11
+       -4.58E+03 -2.26E+03 -1.97E+03 -3.91E+02 -4.34E-18  6.07E+03 -6.79E+02 ......... .........  2.96E+04 ......... .........
          4.01E+03  4.78E+02  4.36E+02  8.52E+06
 
 Elapsed finaloutput time in seconds:     0.43
 #CPUT: Total CPU Time in Seconds,      261.333
Stop Time: 
Sat 04/22/2017 
09:32 AM

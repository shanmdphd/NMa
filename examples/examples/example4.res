Sat 04/22/2017 
09:34 AM
;Model Desc: Population Mixture Problem in 1 Compartment model, 
; with rate constant parameter and its inter-subject variances 
; modeled as coming from two sub-populations
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# example4 (from ad1tr1m2t)
$INPUT C SET ID JID TIME CONC=DV DOSE=AMT RATE EVID MDV CMT VC1 
       K101 VC2 K102 SIGZ PROB
$DATA example4.csv IGNORE=C

$SUBROUTINES ADVAN1 TRANS1

$MIX
P(1)=THETA(4)
P(2)=1.0-THETA(4)
NSPOP=2


$PK
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
V=DEXP(MU_1+ETA(1))
K10M=DEXP(MU_2+ETA(2))
K10F=DEXP(MU_3+ETA(3))
Q=1
IF(MIXNUM.EQ.2) Q=0
K=Q*K10M+(1.0-Q)*K10F
S1=V

$ERROR
Y = F + F*EPS(1)

$THETA
(-1000.0  4.3 1000.0) ;[MU_1]
(-1000.0 -2.9 1000.0) ;[MU_2] 
(-1000.0 -0.67 1000.0) ;[MU_3]
(0.0001 0.667 0.9999)   ;[P(1)]


$OMEGA BLOCK(3)
 .04  ;[p]
 0.01  ;[f]
 .027 ;[p]
 0.01  ;[f]
 0.001 ;[f]
 0.06 ;[p]

$SIGMA 
 0.01 ;[p]

; Prior information setup for OMEGAS only
$PRIOR NWPRI

; Prior OMEGA
$OMEGAP BLOCK(3)
 0.05 FIX
 0.0 0.05
 0.0 0.0  0.05

; Degrees of Freedom defined for Priors.
$OMEGAPD (3 FIX)

$EST METHOD=ITS INTERACTION NITER=30 PRINT=5 NOABORT SIGL=6 
     FILE=example4.ext NOPRIOR=1 CTYPE=3 CITER=10 CALPHA=0.05

$EST METHOD=IMP INTERACTION NITER=20 ISAMPLE=300 PRINT=1 
     NOABORT SIGL=6 NOPRIOR=1

$EST NBURN=500 NITER=500 METHOD=SAEM INTERACTION PRINT=10 SIGL=6 
     ISAMPLE=2 NOPRIOR=1 MAPITER=0

$EST METHOD=IMP INTERACTION EONLY=1 NITER=20 ISAMPLE=3000 PRINT=1 
     NOABORT SIGL=6 NOPRIOR=1

$EST METHOD=BAYES INTERACTION NBURN=2000 NITER=5000 PRINT=10  
     FILE=example4.txt SIGL=6 NOPRIOR=0

$EST MAXEVAL=9999 NSIG=3 SIGL=12 PRINT=1 
     METHOD=CONDITIONAL INTERACTION
     NOABORT FILE=example4.ext NOPRIOR=1

$COV MATRIX=R UNCONDITIONAL SIGL=10
  
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
 RUN# example4 (from ad1tr1m2t)
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
0LENGTH OF THETA:   5
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
 -0.1000E+04     0.4300E+01     0.1000E+04
 -0.1000E+04    -0.2900E+01     0.1000E+04
 -0.1000E+04    -0.6700E+00     0.1000E+04
  0.1000E-03     0.6670E+00     0.9999E+00
  0.3000E+01     0.3000E+01     0.3000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.4000E-01
                  0.1000E-01   0.2700E-01
                  0.1000E-01   0.1000E-02   0.6000E-01
        2                                                                                  YES
                  0.5000E-01
                  0.0000E+00   0.5000E-01
                  0.0000E+00   0.0000E+00   0.5000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E-01
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 GRADIENT METHOD USED:     NOSLOW
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
 NO. OF FUNCT. EVALS. ALLOWED:            840
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
 RAW OUTPUT FILE (FILE): example4.ext
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
 ITERATIONS (NITER):                        30
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
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -7368.93609907050
 iteration            5 OBJ=  -9956.97442036151
 iteration           10 OBJ=  -9960.93067959639
 iteration           15 OBJ=  -9961.00347543110
 iteration           20 OBJ=  -9961.00578701392
 iteration           25 OBJ=  -9961.00593678782
 iteration           30 OBJ=  -9961.00594927628
 
 #TERM:
 OPTIMIZATION WAS NOT COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.8177E-03  1.2649E-03  1.5225E-03
 SE:             1.4906E-02  1.1049E-02  3.3641E-03
 N:                     200         200         200
 
 ETASHRINKSD(%)  3.0067E+00  5.0598E+00  8.0533E+01
 ETASHRINKVR(%)  5.9230E+00  9.8636E+00  9.6210E+01
 EBVSHRINKSD(%)  1.9427E+00  5.0701E+00  8.0324E+01
 EBVSHRINKVR(%)  3.8476E+00  9.8831E+00  9.6129E+01
 EPSSHRINKSD(%)  1.1367E+01
 EPSSHRINKVR(%)  2.1443E+01
 

 SUBMODEL    2
 
 ETABAR:         1.3636E-02 -2.5270E-03 -3.0362E-03
 SE:             2.1747E-02  4.0480E-03  2.4451E-02
 N:                     100         100         100
 
 ETASHRINKSD(%)  1.0000E-10  7.5343E+01  1.0000E-10
 ETASHRINKVR(%)  1.0000E-10  9.3920E+01  1.0000E-10
 EBVSHRINKSD(%)  1.9835E+00  7.5902E+01  1.0446E-01
 EBVSHRINKVR(%)  3.9276E+00  9.4193E+01  2.0880E-01
 EPSSHRINKSD(%)  1.4346E+01
 EPSSHRINKVR(%)  2.6634E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -9961.00594927628     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5550.10098989385     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           900
  
 #TERE:
 Elapsed estimation  time in seconds:     7.35
 Elapsed covariance  time in seconds:     0.59
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9961.006       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.72E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.75E-02
 
 ETA2
+       -8.80E-03  2.72E-02
 
 ETA3
+       -1.06E-02  1.02E-03  6.00E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.45E-01  1.65E-01
 
 ETA3
+       -1.99E-01  2.52E-02  2.45E-01
 


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


         TH 1      TH 2      TH 3      TH 4     
 
         1.30E-02  1.27E-02  2.44E-02  2.73E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.37E-03
 
 ETA2
+        2.65E-03  2.72E-03
 
 ETA3
+        5.36E-03  1.75E+03  8.84E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.49E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.00E-02
 
 ETA2
+        6.61E-02  8.24E-03
 
 ETA3
+        9.39E-02  4.33E+04  1.80E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.72E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.68E-04
 
 TH 2
+       -3.53E-05  1.62E-04
 
 TH 3
+       -3.72E-05  7.76E-06  5.94E-04
 
 TH 4
+        1.51E-05  9.36E-07 -3.96E-06  7.45E-04
 
 OM11
+       -4.24E-06  9.66E-07  2.52E-06  1.88E-06  1.91E-05
 
 OM12
+        1.97E-06  4.53E-06 -8.84E-07 -5.98E-07 -4.95E-06  7.01E-06
 
 OM13
+        1.98E-06 -4.23E-07 -1.13E-05 -2.08E-07 -5.24E-06  1.38E-06  2.87E-05
 
 OM22
+        2.20E-06 -2.63E-06 -3.85E-07  1.58E-06 -1.38E-07 -2.50E-06  1.78E-08  7.39E-06
 
 OM23
+        2.14E-01  4.64E+00  1.98E-01  2.20E+00 -3.90E-01 -1.84E-01  2.72E-02  1.80E+00  3.07E+06
 
 OM33
+       -2.21E-06  4.93E-07 -2.78E-06  3.62E-07  1.12E-06 -2.81E-07 -1.86E-05 -9.22E-08 -1.70E-01  7.82E-05
 
 SG11
+        1.54E-07 -5.08E-08  2.79E-07 -3.09E-07  6.78E-08 -3.18E-08 -1.03E-07  4.48E-08  1.13E-01 -1.63E-07  1.22E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.30E-02
 
 TH 2
+       -2.14E-01  1.27E-02
 
 TH 3
+       -1.18E-01  2.50E-02  2.44E-02
 
 TH 4
+        4.27E-02  2.69E-03 -5.96E-03  2.73E-02
 
 OM11
+       -7.49E-02  1.74E-02  2.37E-02  1.57E-02  4.37E-03
 
 OM12
+        5.75E-02  1.34E-01 -1.37E-02 -8.28E-03 -4.28E-01  2.65E-03
 
 OM13
+        2.85E-02 -6.19E-03 -8.63E-02 -1.42E-03 -2.24E-01  9.69E-02  5.36E-03
 
 OM22
+        6.25E-02 -7.60E-02 -5.81E-03  2.13E-02 -1.16E-02 -3.47E-01  1.22E-03  2.72E-03
 
 OM23
+        9.43E-03  2.08E-01  4.64E-03  4.59E-02 -5.10E-02 -3.96E-02  2.90E-03  3.78E-01  1.75E+03
 
 OM33
+       -1.93E-02  4.38E-03 -1.29E-02  1.50E-03  2.91E-02 -1.20E-02 -3.93E-01 -3.83E-03 -1.10E-02  8.84E-03
 
 SG11
+        3.41E-02 -1.14E-02  3.28E-02 -3.24E-02  4.45E-02 -3.45E-02 -5.52E-02  4.72E-02  1.85E-01 -5.30E-02  3.49E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        6.44E+03
 
 TH 2
+        1.47E+03  7.09E+03
 
 TH 3
+        3.80E+02 -2.31E-01  1.73E+03
 
 TH 4
+       -1.29E+02 -8.81E-02 -6.61E-02  1.35E+03
 
 OM11
+        4.83E+02 -1.69E+03  7.89E+01 -2.02E+02  7.02E+04
 
 OM12
+       -3.19E+03 -5.20E+03 -2.72E-01 -1.10E-01  5.48E+04  2.12E+05
 
 OM13
+        4.42E+01  1.47E+00  8.21E+02  4.27E-01  1.12E+04  4.56E-01  4.41E+04
 
 OM22
+       -2.07E+03  3.62E+03 -1.85E+00 -7.08E-01  1.80E+04  7.61E+04  1.16E+01  1.90E+05
 
 OM23
+       -1.22E-03 -1.39E-02  7.33E-06 -1.16E-03  5.25E-03 -1.75E-02 -3.93E-05 -1.12E-01  4.27E-07
 
 OM33
+        1.58E+02  2.17E+00  2.60E+02  5.71E-01  1.87E+03  2.43E+00  1.05E+04  1.73E+01 -6.55E-05  1.53E+04
 
 SG11
+       -7.71E+03  1.23E+04 -3.44E+03  4.78E+03 -2.63E+04  1.51E+04  4.33E+04  4.79E+04 -3.70E-01  2.77E+04  8.68E+06
 
1
 
 
 #TBLN:      2
 #METH: Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            840
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
 RAW OUTPUT FILE (FILE): example4.ext
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
 ITERATIONS (NITER):                        20
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          300
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 EXPECTATION ONLY (EONLY):                  0
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     0.100000000000000       ,10.0000000000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 LONG TAIL SAMPLE ACCEPT. RATE (IACCEPTL):   0.00000000000000
 T-DIST. PROPOSAL DENSITY (DF):             0
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
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -9965.84409239994 eff.=     303. Smpl.=     300. Fit.= 0.98524
 iteration            1 OBJ=  -9965.18570776384 eff.=     122. Smpl.=     300. Fit.= 0.85779
 iteration            2 OBJ=  -9966.30099230550 eff.=     119. Smpl.=     300. Fit.= 0.85370
 iteration            3 OBJ=  -9967.25772080208 eff.=     120. Smpl.=     300. Fit.= 0.85508
 iteration            4 OBJ=  -9968.39428192526 eff.=     123. Smpl.=     300. Fit.= 0.85833
 iteration            5 OBJ=  -9965.37380761130 eff.=     121. Smpl.=     300. Fit.= 0.85563
 iteration            6 OBJ=  -9965.47260129270 eff.=     117. Smpl.=     300. Fit.= 0.85132
 iteration            7 OBJ=  -9968.11051554352 eff.=     119. Smpl.=     300. Fit.= 0.85297
 iteration            8 OBJ=  -9965.50828658934 eff.=     124. Smpl.=     300. Fit.= 0.85909
 iteration            9 OBJ=  -9967.85584219270 eff.=     118. Smpl.=     300. Fit.= 0.85233
 iteration           10 OBJ=  -9965.53053689377 eff.=     121. Smpl.=     300. Fit.= 0.85645
 iteration           11 OBJ=  -9967.90657675088 eff.=     121. Smpl.=     300. Fit.= 0.85571
 Convergence achieved
 iteration           11 OBJ=  -9967.15427310508 eff.=     121. Smpl.=     300. Fit.= 0.85556
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.1858E-03  9.5595E-04  1.6495E-03
 SE:             1.4909E-02  1.1117E-02  3.6614E-03
 N:                     200         200         200
 
 ETASHRINKSD(%)  2.9019E+00  5.6497E+00  7.8768E+01
 ETASHRINKVR(%)  5.7196E+00  1.0980E+01  9.5492E+01
 EBVSHRINKSD(%)  1.9750E+00  5.1042E+00  7.9101E+01
 EBVSHRINKVR(%)  3.9110E+00  9.9479E+00  9.5632E+01
 EPSSHRINKSD(%)  1.1639E+01
 EPSSHRINKVR(%)  2.1923E+01
 

 SUBMODEL    2
 
 ETABAR:         1.3077E-02 -3.5218E-03 -2.5846E-03
 SE:             2.1757E-02  4.2844E-03  2.4482E-02
 N:                     100         100         100
 
 ETASHRINKSD(%)  1.0000E-10  7.4224E+01  1.0000E-10
 ETASHRINKVR(%)  1.0000E-10  9.3356E+01  1.0000E-10
 EBVSHRINKSD(%)  2.0124E+00  7.5485E+01  1.0929E-01
 EBVSHRINKVR(%)  3.9843E+00  9.3990E+01  2.1845E-01
 EPSSHRINKSD(%)  1.4649E+01
 EPSSHRINKVR(%)  2.7152E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -9967.15427310508     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5556.24931372265     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           900
  
 #TERE:
 Elapsed estimation  time in seconds:    13.98
 Elapsed covariance  time in seconds:     1.74
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9967.154       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.74E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.74E-02
 
 ETA2
+       -8.82E-03  2.79E-02
 
 ETA3
+       -1.08E-02  1.12E-03  5.98E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.43E-01  1.67E-01
 
 ETA3
+       -2.02E-01  2.74E-02  2.44E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.28E-02  1.23E-02  2.42E-02  2.73E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.02E-03
 
 ETA2
+        2.84E-03  3.14E-03
 
 ETA3
+        5.25E-03  2.02E-02  8.40E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.43E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.24E-03
 
 ETA2
+        7.21E-02  9.39E-03
 
 ETA3
+        9.47E-02  4.95E-01  1.72E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.70E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.65E-04
 
 TH 2
+       -3.43E-05  1.52E-04
 
 TH 3
+       -3.68E-05  7.50E-06  5.88E-04
 
 TH 4
+        1.46E-05 -3.97E-06 -5.66E-06  7.45E-04
 
 OM11
+       -5.51E-08 -1.43E-08 -2.57E-08  2.56E-06  1.62E-05
 
 OM12
+        1.56E-07  1.02E-06  5.85E-07 -8.80E-07 -3.29E-06  8.05E-06
 
 OM13
+       -7.81E-08  2.07E-07 -7.99E-06  8.25E-07 -3.50E-06  6.91E-07  2.76E-05
 
 OM22
+        1.69E-07 -1.39E-06 -4.54E-07  9.03E-07  6.80E-07 -3.08E-06 -6.87E-08  9.85E-06
 
 OM23
+        7.19E-07 -4.69E-06  5.69E-05 -1.70E-05  5.05E-07  1.62E-06 -1.45E-05 -2.11E-06  4.08E-04
 
 OM33
+        1.53E-07 -1.57E-07  1.43E-06 -1.96E-08  7.87E-07 -4.31E-08 -1.20E-05 -1.30E-07  1.91E-05  7.05E-05
 
 SG11
+        8.87E-08 -2.83E-08 -2.93E-08 -4.14E-07 -2.43E-08  1.42E-08  1.08E-08 -2.98E-08 -6.78E-08 -8.81E-09  1.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.28E-02
 
 TH 2
+       -2.17E-01  1.23E-02
 
 TH 3
+       -1.18E-01  2.51E-02  2.42E-02
 
 TH 4
+        4.16E-02 -1.18E-02 -8.55E-03  2.73E-02
 
 OM11
+       -1.07E-03 -2.89E-04 -2.64E-04  2.33E-02  4.02E-03
 
 OM12
+        4.27E-03  2.91E-02  8.50E-03 -1.14E-02 -2.88E-01  2.84E-03
 
 OM13
+       -1.16E-03  3.20E-03 -6.28E-02  5.76E-03 -1.66E-01  4.64E-02  5.25E-03
 
 OM22
+        4.20E-03 -3.59E-02 -5.97E-03  1.05E-02  5.38E-02 -3.46E-01 -4.17E-03  3.14E-03
 
 OM23
+        2.77E-03 -1.89E-02  1.16E-01 -3.09E-02  6.21E-03  2.83E-02 -1.37E-01 -3.33E-02  2.02E-02
 
 OM33
+        1.42E-03 -1.52E-03  7.00E-03 -8.56E-05  2.33E-02 -1.81E-03 -2.72E-01 -4.93E-03  1.13E-01  8.40E-03
 
 SG11
+        2.01E-02 -6.71E-03 -3.52E-03 -4.42E-02 -1.76E-02  1.45E-02  6.00E-03 -2.76E-02 -9.78E-03 -3.06E-03  3.43E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          IMPORTANCE SAMPLING (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        6.47E+03
 
 TH 2
+        1.44E+03  6.94E+03
 
 TH 3
+        3.92E+02 -5.18E+00  1.75E+03
 
 TH 4
+       -1.20E+02  1.01E+01 -5.95E-01  1.35E+03
 
 OM11
+       -9.04E+00 -1.67E+02  8.39E+01 -2.11E+02  6.93E+04
 
 OM12
+       -3.48E+02 -6.96E+02 -9.33E+01  1.86E+01  2.93E+04  1.54E+05
 
 OM13
+        1.13E+02 -1.11E+01  4.44E+02 -4.94E+01  8.45E+03 -3.00E+02  4.09E+04
 
 OM22
+       -1.14E+01  7.66E+02 -7.16E+00 -7.40E+01  4.48E+03  4.59E+04 -5.59E+01  1.16E+05
 
 OM23
+       -5.05E+01  8.56E+01 -2.34E+02  5.65E+01  6.97E+01 -4.10E+02  1.07E+03  4.17E+02  2.55E+03
 
 OM33
+        1.35E+01 -9.65E+00  1.02E+02 -2.02E+01  6.69E+02 -8.69E+01  6.56E+03  7.42E+01 -5.04E+02  1.54E+04
 
 SG11
+       -4.85E+03  9.10E+02 -3.97E+00  4.81E+03  1.05E+04 -9.44E+02 -1.03E+03  2.49E+04  1.70E+03  3.71E+02  8.52E+06
 
1
 
 
 #TBLN:      3
 #METH: Stochastic Approximation Expectation-Maximization (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            840
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
 RAW OUTPUT FILE (FILE): example4.ext
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
   1   2   3
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration         -500 SAEMOBJ=  -14089.0924300112
 iteration         -490 SAEMOBJ=  -14306.5134276835
 iteration         -480 SAEMOBJ=  -14278.4233540332
 iteration         -470 SAEMOBJ=  -14277.0929812185
 iteration         -460 SAEMOBJ=  -14274.2167605128
 iteration         -450 SAEMOBJ=  -14350.8058842814
 iteration         -440 SAEMOBJ=  -14284.3854037325
 iteration         -430 SAEMOBJ=  -14232.8046585428
 iteration         -420 SAEMOBJ=  -14355.1003497920
 iteration         -410 SAEMOBJ=  -14280.9797180043
 iteration         -400 SAEMOBJ=  -14335.6663159871
 iteration         -390 SAEMOBJ=  -14295.2067902266
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -14324.0070231894
 iteration           10 SAEMOBJ=  -14423.2577435512
 iteration           20 SAEMOBJ=  -14422.7754685007
 iteration           30 SAEMOBJ=  -14424.7832458010
 iteration           40 SAEMOBJ=  -14424.0934664937
 iteration           50 SAEMOBJ=  -14425.1937273373
 iteration           60 SAEMOBJ=  -14422.7398254002
 iteration           70 SAEMOBJ=  -14422.1496577722
 iteration           80 SAEMOBJ=  -14422.1667717805
 iteration           90 SAEMOBJ=  -14423.1662866147
 iteration          100 SAEMOBJ=  -14422.1002341932
 iteration          110 SAEMOBJ=  -14421.6412140455
 iteration          120 SAEMOBJ=  -14421.2490586904
 iteration          130 SAEMOBJ=  -14421.4099786310
 iteration          140 SAEMOBJ=  -14422.1065548655
 iteration          150 SAEMOBJ=  -14422.2034028811
 iteration          160 SAEMOBJ=  -14421.9433151388
 iteration          170 SAEMOBJ=  -14421.9969096328
 iteration          180 SAEMOBJ=  -14421.0541984473
 iteration          190 SAEMOBJ=  -14420.8169425179
 iteration          200 SAEMOBJ=  -14420.5343202940
 iteration          210 SAEMOBJ=  -14420.3037897165
 iteration          220 SAEMOBJ=  -14419.7367565576
 iteration          230 SAEMOBJ=  -14419.5284521898
 iteration          240 SAEMOBJ=  -14419.3943211613
 iteration          250 SAEMOBJ=  -14419.1319154100
 iteration          260 SAEMOBJ=  -14419.0445694092
 iteration          270 SAEMOBJ=  -14418.9452753641
 iteration          280 SAEMOBJ=  -14418.8458187923
 iteration          290 SAEMOBJ=  -14418.6026231274
 iteration          300 SAEMOBJ=  -14418.8042594671
 iteration          310 SAEMOBJ=  -14418.3728506141
 iteration          320 SAEMOBJ=  -14418.3658270508
 iteration          330 SAEMOBJ=  -14418.2267745987
 iteration          340 SAEMOBJ=  -14418.4927180224
 iteration          350 SAEMOBJ=  -14418.3435110567
 iteration          360 SAEMOBJ=  -14417.9336022440
 iteration          370 SAEMOBJ=  -14417.7504957138
 iteration          380 SAEMOBJ=  -14417.6598252596
 iteration          390 SAEMOBJ=  -14417.5184281534
 iteration          400 SAEMOBJ=  -14417.2312715553
 iteration          410 SAEMOBJ=  -14417.1588992348
 iteration          420 SAEMOBJ=  -14417.4466565344
 iteration          430 SAEMOBJ=  -14417.3999590635
 iteration          440 SAEMOBJ=  -14417.3849655511
 iteration          450 SAEMOBJ=  -14417.3480107797
 iteration          460 SAEMOBJ=  -14417.2481511018
 iteration          470 SAEMOBJ=  -14417.1119236598
 iteration          480 SAEMOBJ=  -14417.1236028060
 iteration          490 SAEMOBJ=  -14416.9295197249
 iteration          500 SAEMOBJ=  -14416.7033318487
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.3786E-03  1.6888E-03  5.1941E-04
 SE:             1.4902E-02  1.1099E-02  5.8680E-03
 N:                     200         200         200
 
 ETASHRINKSD(%)  2.9968E+00  5.2361E+00  6.5852E+01
 ETASHRINKVR(%)  5.9037E+00  1.0198E+01  8.8339E+01
 EBVSHRINKSD(%)  1.9480E+00  5.1180E+00  6.4823E+01
 EBVSHRINKVR(%)  3.8581E+00  9.9741E+00  8.7626E+01
 EPSSHRINKSD(%)  1.1591E+01
 EPSSHRINKVR(%)  2.1839E+01
 

 SUBMODEL    2
 
 ETABAR:         1.2747E-02 -3.3890E-03 -9.3423E-04
 SE:             2.1737E-02  6.2396E-03  2.4478E-02
 N:                     100         100         100
 
 ETASHRINKSD(%)  1.0000E-10  6.2233E+01  1.0000E-10
 ETASHRINKVR(%)  1.0000E-10  8.5737E+01  1.0000E-10
 EBVSHRINKSD(%)  1.9974E+00  6.3265E+01  1.0968E-01
 EBVSHRINKVR(%)  3.9549E+00  8.6505E+01  2.1924E-01
 EPSSHRINKSD(%)  1.4612E+01
 EPSSHRINKVR(%)  2.7089E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -14416.7033318487     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -10005.7983724662     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           900
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    1654.08935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -14416.7033318487     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -12762.6139720802     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:    67.86
 Elapsed covariance  time in seconds:     0.62
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -14416.703       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.75E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.74E-02
 
 ETA2
+       -8.95E-03  2.76E-02
 
 ETA3
+       -1.09E-02 -8.99E-03  5.94E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.47E-01  1.66E-01
 
 ETA3
+       -2.06E-01 -2.22E-01  2.44E-01
 


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


         TH 1      TH 2      TH 3      TH 4     
 
         1.30E-02  1.28E-02  2.44E-02  2.73E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.37E-03
 
 ETA2
+        2.67E-03  2.57E-03
 
 ETA3
+        5.29E-03  6.06E-02  8.64E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.41E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        1.00E-02
 
 ETA2
+        6.60E-02  7.74E-03
 
 ETA3
+        9.28E-02  1.50E+00  1.77E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.69E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.68E-04
 
 TH 2
+       -3.78E-05  1.65E-04
 
 TH 3
+       -4.04E-05  1.82E-05  5.95E-04
 
 TH 4
+        1.33E-05 -1.31E-07  6.56E-06  7.46E-04
 
 OM11
+       -4.55E-06  2.65E-06  4.26E-06  2.62E-06  1.91E-05
 
 OM12
+        2.13E-06  4.64E-06 -1.40E-06 -8.13E-07 -5.08E-06  7.13E-06
 
 OM13
+        2.07E-06 -4.47E-07 -1.02E-05  1.31E-07 -5.39E-06  1.40E-06  2.80E-05
 
 OM22
+        2.15E-06 -6.12E-06 -7.61E-07  2.90E-07  8.56E-08 -2.50E-06 -7.59E-08  6.60E-06
 
 OM23
+       -3.19E-05  1.61E-04  2.70E-04  7.61E-05  2.42E-05 -6.83E-06 -2.01E-06 -9.56E-06  3.67E-03
 
 OM33
+       -2.30E-06  2.24E-06 -2.20E-06  3.77E-06  1.50E-06 -2.63E-07 -1.86E-05 -5.56E-08  2.49E-05  7.47E-05
 
 SG11
+        1.27E-07 -2.52E-07  2.89E-07 -3.89E-07  8.22E-08 -2.51E-08 -9.25E-08 -2.16E-08  5.36E-07 -1.60E-07  1.16E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.30E-02
 
 TH 2
+       -2.26E-01  1.28E-02
 
 TH 3
+       -1.28E-01  5.80E-02  2.44E-02
 
 TH 4
+        3.76E-02 -3.74E-04  9.85E-03  2.73E-02
 
 OM11
+       -8.01E-02  4.72E-02  3.99E-02  2.20E-02  4.37E-03
 
 OM12
+        6.14E-02  1.35E-01 -2.15E-02 -1.11E-02 -4.35E-01  2.67E-03
 
 OM13
+        3.02E-02 -6.57E-03 -7.89E-02  9.04E-04 -2.33E-01  9.90E-02  5.29E-03
 
 OM22
+        6.45E-02 -1.85E-01 -1.21E-02  4.14E-03  7.62E-03 -3.64E-01 -5.58E-03  2.57E-03
 
 OM23
+       -4.06E-02  2.06E-01  1.83E-01  4.60E-02  9.13E-02 -4.22E-02 -6.27E-03 -6.14E-02  6.06E-02
 
 OM33
+       -2.05E-02  2.02E-02 -1.04E-02  1.60E-02  3.97E-02 -1.14E-02 -4.06E-01 -2.51E-03  4.76E-02  8.64E-03
 
 SG11
+        2.86E-02 -5.74E-02  3.47E-02 -4.18E-02  5.51E-02 -2.75E-02 -5.12E-02 -2.46E-02  2.59E-02 -5.42E-02  3.41E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        6.46E+03
 
 TH 2
+        1.48E+03  7.01E+03
 
 TH 3
+        4.08E+02  1.22E+01  1.78E+03
 
 TH 4
+       -1.22E+02  1.12E+01 -1.34E+01  1.35E+03
 
 OM11
+        4.87E+02 -1.72E+03  6.91E+01 -2.07E+02  7.09E+04
 
 OM12
+       -3.25E+03 -5.13E+03 -6.12E+01  2.75E+01  5.52E+04  2.11E+05
 
 OM13
+        2.68E+01 -4.66E+01  8.16E+02 -7.39E+01  1.20E+04  2.93E+02  4.60E+04
 
 OM22
+       -2.01E+03  3.73E+03 -1.27E+02 -2.32E+01  1.80E+04  7.61E+04  4.85E+02  1.85E+05
 
 OM23
+       -5.07E+01 -2.85E+02 -1.30E+02 -2.75E+01 -2.41E+02  4.28E+02 -1.92E+02  3.27E+02  2.99E+02
 
 OM33
+        1.60E+02 -2.86E+01  3.03E+02 -6.73E+01  1.88E+03 -3.05E+02  1.14E+04 -1.88E+01 -1.40E+02  1.63E+04
 
 SG11
+       -6.20E+03  1.56E+04 -3.31E+03  4.82E+03 -2.67E+04  1.11E+04  4.23E+04  4.73E+04 -1.73E+03  2.95E+04  8.76E+06
 
1
 
 
 #TBLN:      4
 #METH: Objective Function Evaluation by Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            840
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
 RAW OUTPUT FILE (FILE): example4.ext
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
 ITERATIONS (NITER):                        20
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
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
   1   2   3
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -9966.51747621679 eff.=    2994. Smpl.=    3000. Fit.= 0.97338
 iteration            1 OBJ=  -9966.63416345030 eff.=    1185. Smpl.=    3000. Fit.= 0.85515
 iteration            2 OBJ=  -9966.00224770727 eff.=    1188. Smpl.=    3000. Fit.= 0.85578
 iteration            3 OBJ=  -9966.09857250084 eff.=    1192. Smpl.=    3000. Fit.= 0.85621
 iteration            4 OBJ=  -9966.46974793633 eff.=    1194. Smpl.=    3000. Fit.= 0.85630
 iteration            5 OBJ=  -9966.75632887216 eff.=    1193. Smpl.=    3000. Fit.= 0.85607
 iteration            6 OBJ=  -9966.45376996498 eff.=    1193. Smpl.=    3000. Fit.= 0.85633
 iteration            7 OBJ=  -9966.67039673248 eff.=    1192. Smpl.=    3000. Fit.= 0.85609
 iteration            8 OBJ=  -9966.07930354096 eff.=    1192. Smpl.=    3000. Fit.= 0.85631
 iteration            9 OBJ=  -9965.91930642774 eff.=    1192. Smpl.=    3000. Fit.= 0.85614
 iteration           10 OBJ=  -9967.10745291247 eff.=    1195. Smpl.=    3000. Fit.= 0.85610
 iteration           11 OBJ=  -9965.77845945176 eff.=    1195. Smpl.=    3000. Fit.= 0.85660
 iteration           12 OBJ=  -9966.43069434020 eff.=    1190. Smpl.=    3000. Fit.= 0.85595
 iteration           13 OBJ=  -9967.22511980936 eff.=    1196. Smpl.=    3000. Fit.= 0.85631
 iteration           14 OBJ=  -9966.24919872409 eff.=    1193. Smpl.=    3000. Fit.= 0.85623
 iteration           15 OBJ=  -9966.86357186548 eff.=    1192. Smpl.=    3000. Fit.= 0.85588
 iteration           16 OBJ=  -9966.02927884473 eff.=    1193. Smpl.=    3000. Fit.= 0.85643
 iteration           17 OBJ=  -9966.77249953207 eff.=    1193. Smpl.=    3000. Fit.= 0.85607
 iteration           18 OBJ=  -9967.57986061592 eff.=    1195. Smpl.=    3000. Fit.= 0.85590
 iteration           19 OBJ=  -9966.95367004442 eff.=    1194. Smpl.=    3000. Fit.= 0.85616
 iteration           20 OBJ=  -9966.30276537542 eff.=    1191. Smpl.=    3000. Fit.= 0.85600
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.5009E-03  1.6700E-03  1.5158E-03
 SE:             1.4908E-02  1.1108E-02  5.8589E-03
 N:                     200         200         200
 
 ETASHRINKSD(%)  2.9617E+00  5.1540E+00  6.5905E+01
 ETASHRINKVR(%)  5.8357E+00  1.0042E+01  8.8375E+01
 EBVSHRINKSD(%)  1.9591E+00  5.1738E+00  6.6224E+01
 EBVSHRINKVR(%)  3.8799E+00  1.0080E+01  8.8592E+01
 EPSSHRINKSD(%)  1.1625E+01
 EPSSHRINKVR(%)  2.1899E+01
 

 SUBMODEL    2
 
 ETABAR:         1.3112E-02 -2.6634E-03 -9.9861E-04
 SE:             2.1741E-02  6.2181E-03  2.4481E-02
 N:                     100         100         100
 
 ETASHRINKSD(%)  1.0000E-10  6.2363E+01  1.0000E-10
 ETASHRINKVR(%)  1.0000E-10  8.5835E+01  1.0000E-10
 EBVSHRINKSD(%)  2.0139E+00  6.2812E+01  1.1016E-01
 EBVSHRINKVR(%)  3.9872E+00  8.6171E+01  2.2020E-01
 EPSSHRINKSD(%)  1.4607E+01
 EPSSHRINKVR(%)  2.7081E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -9966.30276537542     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5555.39780599299     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           900
  
 #TERE:
 Elapsed estimation  time in seconds:   186.51
 Elapsed covariance  time in seconds:    11.58
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9966.303       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.75E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.74E-02
 
 ETA2
+       -8.95E-03  2.76E-02
 
 ETA3
+       -1.09E-02 -8.99E-03  5.94E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.47E-01  1.66E-01
 
 ETA3
+       -2.06E-01 -2.22E-01  2.44E-01
 


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


         TH 1      TH 2      TH 3      TH 4     
 
         1.28E-02  1.23E-02  2.42E-02  2.73E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.03E-03
 
 ETA2
+        2.82E-03  3.07E-03
 
 ETA3
+        5.25E-03  4.50E-02  8.31E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.42E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.25E-03
 
 ETA2
+        7.14E-02  9.25E-03
 
 ETA3
+        9.45E-02  1.11E+00  1.71E-02
 


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
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.65E-04
 
 TH 2
+       -3.48E-05  1.51E-04
 
 TH 3
+       -3.73E-05  9.54E-06  5.86E-04
 
 TH 4
+        1.49E-05 -3.33E-06  7.92E-07  7.44E-04
 
 OM11
+        4.56E-08 -9.42E-08  5.41E-08  2.50E-06  1.62E-05
 
 OM12
+       -1.52E-07  1.36E-06 -1.97E-07 -7.46E-07 -3.43E-06  7.93E-06
 
 OM13
+       -3.85E-07 -1.52E-07 -7.17E-06  1.33E-07 -3.75E-06  8.06E-07  2.76E-05
 
 OM22
+        2.66E-07 -1.98E-06  5.44E-08  1.57E-07  7.19E-07 -3.17E-06 -1.61E-07  9.44E-06
 
 OM23
+       -1.67E-06 -3.26E-05 -8.05E-05 -5.69E-06  1.08E-06  3.51E-06  8.99E-06 -3.01E-06  2.02E-03
 
 OM33
+        3.58E-07  3.36E-07  9.10E-07  1.64E-06  8.65E-07 -2.86E-07 -1.31E-05  6.45E-08 -3.82E-05  6.91E-05
 
 SG11
+        8.91E-08 -3.28E-08 -1.71E-08 -4.09E-07 -2.04E-08  1.43E-08  2.90E-09 -2.83E-08 -6.30E-08  6.72E-09  1.17E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.28E-02
 
 TH 2
+       -2.21E-01  1.23E-02
 
 TH 3
+       -1.20E-01  3.21E-02  2.42E-02
 
 TH 4
+        4.24E-02 -9.95E-03  1.20E-03  2.73E-02
 
 OM11
+        8.81E-04 -1.90E-03  5.55E-04  2.27E-02  4.03E-03
 
 OM12
+       -4.20E-03  3.94E-02 -2.88E-03 -9.71E-03 -3.02E-01  2.82E-03
 
 OM13
+       -5.71E-03 -2.36E-03 -5.64E-02  9.29E-04 -1.77E-01  5.45E-02  5.25E-03
 
 OM22
+        6.74E-03 -5.24E-02  7.31E-04  1.88E-03  5.80E-02 -3.66E-01 -9.95E-03  3.07E-03
 
 OM23
+       -2.88E-03 -5.90E-02 -7.39E-02 -4.63E-03  5.96E-03  2.77E-02  3.80E-02 -2.18E-02  4.50E-02
 
 OM33
+        3.35E-03  3.30E-03  4.52E-03  7.25E-03  2.58E-02 -1.22E-02 -3.00E-01  2.52E-03 -1.02E-01  8.31E-03
 
 SG11
+        2.03E-02 -7.80E-03 -2.06E-03 -4.38E-02 -1.48E-02  1.49E-02  1.61E-03 -2.69E-02 -4.09E-03  2.36E-03  3.42E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        6.47E+03
 
 TH 2
+        1.48E+03  7.03E+03
 
 TH 3
+        3.97E+02 -3.89E+00  1.75E+03
 
 TH 4
+       -1.26E+02  3.00E+00 -9.96E+00  1.35E+03
 
 OM11
+        1.60E+01 -1.93E+02  1.04E+02 -2.06E+02  7.01E+04
 
 OM12
+       -1.21E+02 -8.40E+02  2.07E+01  3.95E+01  3.16E+04  1.60E+05
 
 OM13
+        2.04E+02  3.85E+01  5.01E+02 -5.97E+01  9.13E+03  3.13E+01  4.13E+04
 
 OM22
+        8.84E+01  1.20E+03  6.91E+00  2.52E+01  5.37E+03  5.12E+04 -4.24E+01  1.23E+05
 
 OM23
+        4.42E+01  1.18E+02  6.94E+01  3.10E+00 -1.07E+02 -2.30E+02 -2.26E+01  1.14E+02  5.05E+02
 
 OM33
+        1.99E+01  2.83E+01  1.07E+02 -3.87E+01  9.25E+02  1.03E+02  7.70E+03  7.48E+01  2.73E+02  1.61E+04
 
 SG11
+       -4.83E+03  1.27E+03 -4.75E+01  4.78E+03  8.52E+03 -1.91E+03 -1.81E+02  2.48E+04  3.13E+02 -9.26E+02  8.56E+06
 
1
 
 
 #TBLN:      5
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            840
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
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example4.txt
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
 ITERATIONS (NITER):                        5000
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
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           4
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):4
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
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
   4
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -2000 MCMCOBJ=   -14086.7548142890     
 iteration        -1990 MCMCOBJ=   -14108.3890546704     
 iteration        -1980 MCMCOBJ=   -14069.5423640753     
 iteration        -1970 MCMCOBJ=   -14015.7587298176     
 iteration        -1960 MCMCOBJ=   -13948.6530866162     
 iteration        -1950 MCMCOBJ=   -14109.1262840156     
 iteration        -1940 MCMCOBJ=   -14047.1520548928     
 iteration        -1930 MCMCOBJ=   -14192.1954534859     
 iteration        -1920 MCMCOBJ=   -14137.0509380973     
 iteration        -1910 MCMCOBJ=   -14081.3208013968     
 iteration        -1900 MCMCOBJ=   -14072.6713877882     
 iteration        -1890 MCMCOBJ=   -13995.6069434369     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -14027.6632860721     
 iteration           10 MCMCOBJ=   -14096.2299340523     
 iteration           20 MCMCOBJ=   -13864.5180917246     
 iteration           30 MCMCOBJ=   -14067.7519610003     
 iteration           40 MCMCOBJ=   -13988.1309173033     
 iteration           50 MCMCOBJ=   -14075.3414945293     
 iteration           60 MCMCOBJ=   -14102.2528151099     
 iteration           70 MCMCOBJ=   -14043.2584259992     
 iteration           80 MCMCOBJ=   -14077.1082486417     
 iteration           90 MCMCOBJ=   -14090.9871294226     
 iteration          100 MCMCOBJ=   -14059.2350683256     
 iteration          110 MCMCOBJ=   -14146.8190369761     
 iteration          120 MCMCOBJ=   -14130.1580133636     
 iteration          130 MCMCOBJ=   -14110.8962508501     
 iteration          140 MCMCOBJ=   -14028.8441147264     
 iteration          150 MCMCOBJ=   -14087.2032322249     
 iteration          160 MCMCOBJ=   -14126.2312226169     
 iteration          170 MCMCOBJ=   -14118.4521006254     
 iteration          180 MCMCOBJ=   -14078.2216009096     
 iteration          190 MCMCOBJ=   -14121.9477068067     
 iteration          200 MCMCOBJ=   -14002.8812283935     
 iteration          210 MCMCOBJ=   -14159.5324545818     
 iteration          220 MCMCOBJ=   -14331.9729494780     
 iteration          230 MCMCOBJ=   -14237.0612863852     
 iteration          240 MCMCOBJ=   -14151.8461965387     
 iteration          250 MCMCOBJ=   -14232.1080210464     
 iteration          260 MCMCOBJ=   -14284.3622329763     
 iteration          270 MCMCOBJ=   -14196.4885707060     
 iteration          280 MCMCOBJ=   -14073.1696099921     
 iteration          290 MCMCOBJ=   -14040.2225715383     
 iteration          300 MCMCOBJ=   -14013.9982963231     
 iteration          310 MCMCOBJ=   -14100.9491745503     
 iteration          320 MCMCOBJ=   -14136.8905935212     
 iteration          330 MCMCOBJ=   -14092.5496325530     
 iteration          340 MCMCOBJ=   -14155.8261231222     
 iteration          350 MCMCOBJ=   -14080.1223985710     
 iteration          360 MCMCOBJ=   -14181.0280727027     
 iteration          370 MCMCOBJ=   -14137.1876943829     
 iteration          380 MCMCOBJ=   -14042.7087648085     
 iteration          390 MCMCOBJ=   -14115.8651404794     
 iteration          400 MCMCOBJ=   -14056.5565254733     
 iteration          410 MCMCOBJ=   -14040.0865625230     
 iteration          420 MCMCOBJ=   -14069.8106226938     
 iteration          430 MCMCOBJ=   -14107.9756995726     
 iteration          440 MCMCOBJ=   -14140.2675909841     
 iteration          450 MCMCOBJ=   -14180.3198119207     
 iteration          460 MCMCOBJ=   -13974.7160471014     
 iteration          470 MCMCOBJ=   -14087.8903334395     
 iteration          480 MCMCOBJ=   -14088.7973804362     
 iteration          490 MCMCOBJ=   -14122.9935402893     
 iteration          500 MCMCOBJ=   -14053.6345022641     
 iteration          510 MCMCOBJ=   -14076.3127614433     
 iteration          520 MCMCOBJ=   -14023.7646920929     
 iteration          530 MCMCOBJ=   -14142.1299128819     
 iteration          540 MCMCOBJ=   -14106.6489709730     
 iteration          550 MCMCOBJ=   -14171.2831986652     
 iteration          560 MCMCOBJ=   -14098.2221471363     
 iteration          570 MCMCOBJ=   -14195.0256465643     
 iteration          580 MCMCOBJ=   -14231.6741767672     
 iteration          590 MCMCOBJ=   -14170.7473233551     
 iteration          600 MCMCOBJ=   -14178.3640176251     
 iteration          610 MCMCOBJ=   -14173.4519535631     
 iteration          620 MCMCOBJ=   -14069.7114295654     
 iteration          630 MCMCOBJ=   -14091.3954245935     
 iteration          640 MCMCOBJ=   -13998.3971063436     
 iteration          650 MCMCOBJ=   -14186.2222416205     
 iteration          660 MCMCOBJ=   -14066.6716442103     
 iteration          670 MCMCOBJ=   -14146.8062801002     
 iteration          680 MCMCOBJ=   -14094.8750770481     
 iteration          690 MCMCOBJ=   -14157.0272423849     
 iteration          700 MCMCOBJ=   -14073.0950061856     
 iteration          710 MCMCOBJ=   -14123.1402232741     
 iteration          720 MCMCOBJ=   -14026.6708455650     
 iteration          730 MCMCOBJ=   -14020.8707201108     
 iteration          740 MCMCOBJ=   -14124.1722431238     
 iteration          750 MCMCOBJ=   -14113.9298473061     
 iteration          760 MCMCOBJ=   -14159.7575967167     
 iteration          770 MCMCOBJ=   -14005.3224127120     
 iteration          780 MCMCOBJ=   -14100.2595563105     
 iteration          790 MCMCOBJ=   -14093.0794863553     
 iteration          800 MCMCOBJ=   -14049.9949048979     
 iteration          810 MCMCOBJ=   -14047.0946748213     
 iteration          820 MCMCOBJ=   -14046.5769424883     
 iteration          830 MCMCOBJ=   -14043.5308166624     
 iteration          840 MCMCOBJ=   -13965.8866936725     
 iteration          850 MCMCOBJ=   -14051.5503157497     
 iteration          860 MCMCOBJ=   -14111.1397870961     
 iteration          870 MCMCOBJ=   -14090.3402048398     
 iteration          880 MCMCOBJ=   -14131.8486814856     
 iteration          890 MCMCOBJ=   -14028.2702602621     
 iteration          900 MCMCOBJ=   -14032.3321554137     
 iteration          910 MCMCOBJ=   -14020.2685295904     
 iteration          920 MCMCOBJ=   -14139.1901865663     
 iteration          930 MCMCOBJ=   -14091.3769249177     
 iteration          940 MCMCOBJ=   -14188.2758108400     
 iteration          950 MCMCOBJ=   -14170.7558142876     
 iteration          960 MCMCOBJ=   -14056.5479753685     
 iteration          970 MCMCOBJ=   -14134.1457449486     
 iteration          980 MCMCOBJ=   -14114.2164028110     
 iteration          990 MCMCOBJ=   -14052.1622358500     
 iteration         1000 MCMCOBJ=   -14055.8909746287     
 iteration         1010 MCMCOBJ=   -14097.2740887062     
 iteration         1020 MCMCOBJ=   -14143.0395247356     
 iteration         1030 MCMCOBJ=   -14097.4127023673     
 iteration         1040 MCMCOBJ=   -14099.7052588489     
 iteration         1050 MCMCOBJ=   -14124.3664698603     
 iteration         1060 MCMCOBJ=   -14108.6435343834     
 iteration         1070 MCMCOBJ=   -14168.1185670695     
 iteration         1080 MCMCOBJ=   -14063.4155955865     
 iteration         1090 MCMCOBJ=   -14098.9713860499     
 iteration         1100 MCMCOBJ=   -14029.6717992196     
 iteration         1110 MCMCOBJ=   -14110.4997525150     
 iteration         1120 MCMCOBJ=   -14079.3866539778     
 iteration         1130 MCMCOBJ=   -14015.6346363675     
 iteration         1140 MCMCOBJ=   -14111.2380326017     
 iteration         1150 MCMCOBJ=   -14207.2614841941     
 iteration         1160 MCMCOBJ=   -14076.3966029867     
 iteration         1170 MCMCOBJ=   -13965.3544769081     
 iteration         1180 MCMCOBJ=   -14015.1341982029     
 iteration         1190 MCMCOBJ=   -14126.7955494689     
 iteration         1200 MCMCOBJ=   -14119.1494749983     
 iteration         1210 MCMCOBJ=   -14099.1592627996     
 iteration         1220 MCMCOBJ=   -14038.3291141173     
 iteration         1230 MCMCOBJ=   -14096.0732994806     
 iteration         1240 MCMCOBJ=   -14086.8742779453     
 iteration         1250 MCMCOBJ=   -14051.7775414273     
 iteration         1260 MCMCOBJ=   -14037.6352139462     
 iteration         1270 MCMCOBJ=   -14111.2769087267     
 iteration         1280 MCMCOBJ=   -14031.6475222032     
 iteration         1290 MCMCOBJ=   -13986.4798689912     
 iteration         1300 MCMCOBJ=   -13924.5046566809     
 iteration         1310 MCMCOBJ=   -14078.6997223548     
 iteration         1320 MCMCOBJ=   -14225.7148614357     
 iteration         1330 MCMCOBJ=   -14159.5300722441     
 iteration         1340 MCMCOBJ=   -14096.0196493236     
 iteration         1350 MCMCOBJ=   -14098.1369928926     
 iteration         1360 MCMCOBJ=   -14078.4664169952     
 iteration         1370 MCMCOBJ=   -14055.2882177438     
 iteration         1380 MCMCOBJ=   -13976.3711760084     
 iteration         1390 MCMCOBJ=   -14108.0705181201     
 iteration         1400 MCMCOBJ=   -14172.1729461233     
 iteration         1410 MCMCOBJ=   -14097.3172021827     
 iteration         1420 MCMCOBJ=   -14012.7844666760     
 iteration         1430 MCMCOBJ=   -14096.7163897660     
 iteration         1440 MCMCOBJ=   -14049.1000549386     
 iteration         1450 MCMCOBJ=   -14106.6244973200     
 iteration         1460 MCMCOBJ=   -14058.6798674296     
 iteration         1470 MCMCOBJ=   -14095.8820102165     
 iteration         1480 MCMCOBJ=   -14022.8874976360     
 iteration         1490 MCMCOBJ=   -14067.4489808122     
 iteration         1500 MCMCOBJ=   -14078.2000898193     
 iteration         1510 MCMCOBJ=   -14135.5153990581     
 iteration         1520 MCMCOBJ=   -14123.2247761821     
 iteration         1530 MCMCOBJ=   -14086.6061449550     
 iteration         1540 MCMCOBJ=   -14173.6224333375     
 iteration         1550 MCMCOBJ=   -14049.0167734495     
 iteration         1560 MCMCOBJ=   -14092.2301519712     
 iteration         1570 MCMCOBJ=   -14187.2966915560     
 iteration         1580 MCMCOBJ=   -14045.7814073871     
 iteration         1590 MCMCOBJ=   -14009.0183714154     
 iteration         1600 MCMCOBJ=   -14084.6457437957     
 iteration         1610 MCMCOBJ=   -13957.1328068234     
 iteration         1620 MCMCOBJ=   -14132.2212003735     
 iteration         1630 MCMCOBJ=   -14032.7930638252     
 iteration         1640 MCMCOBJ=   -14118.9846121451     
 iteration         1650 MCMCOBJ=   -14157.8018678569     
 iteration         1660 MCMCOBJ=   -14015.8945694727     
 iteration         1670 MCMCOBJ=   -14075.8905187708     
 iteration         1680 MCMCOBJ=   -14011.2643928051     
 iteration         1690 MCMCOBJ=   -14073.7786219376     
 iteration         1700 MCMCOBJ=   -14087.8352191137     
 iteration         1710 MCMCOBJ=   -13940.3247489849     
 iteration         1720 MCMCOBJ=   -14094.7934662286     
 iteration         1730 MCMCOBJ=   -13993.4355420532     
 iteration         1740 MCMCOBJ=   -14021.7181064231     
 iteration         1750 MCMCOBJ=   -14057.6093494662     
 iteration         1760 MCMCOBJ=   -14128.4975978706     
 iteration         1770 MCMCOBJ=   -14175.6300250252     
 iteration         1780 MCMCOBJ=   -14103.8616091711     
 iteration         1790 MCMCOBJ=   -14141.7064294115     
 iteration         1800 MCMCOBJ=   -13994.4935114317     
 iteration         1810 MCMCOBJ=   -14099.6646966612     
 iteration         1820 MCMCOBJ=   -14140.7174870218     
 iteration         1830 MCMCOBJ=   -14045.2415869880     
 iteration         1840 MCMCOBJ=   -14034.5341912369     
 iteration         1850 MCMCOBJ=   -14094.3368586801     
 iteration         1860 MCMCOBJ=   -14109.6551504575     
 iteration         1870 MCMCOBJ=   -14117.1096775286     
 iteration         1880 MCMCOBJ=   -14107.7988662081     
 iteration         1890 MCMCOBJ=   -14186.6782983158     
 iteration         1900 MCMCOBJ=   -14028.1068975283     
 iteration         1910 MCMCOBJ=   -14039.8335738642     
 iteration         1920 MCMCOBJ=   -14088.2275195188     
 iteration         1930 MCMCOBJ=   -14036.8741235338     
 iteration         1940 MCMCOBJ=   -14026.9399505802     
 iteration         1950 MCMCOBJ=   -14068.5118660579     
 iteration         1960 MCMCOBJ=   -14112.3515544222     
 iteration         1970 MCMCOBJ=   -13943.8031663336     
 iteration         1980 MCMCOBJ=   -14045.9057381892     
 iteration         1990 MCMCOBJ=   -14015.3038470547     
 iteration         2000 MCMCOBJ=   -14143.5818181669     
 iteration         2010 MCMCOBJ=   -14132.3683486411     
 iteration         2020 MCMCOBJ=   -14051.8392859376     
 iteration         2030 MCMCOBJ=   -14105.6361352268     
 iteration         2040 MCMCOBJ=   -14036.5058216233     
 iteration         2050 MCMCOBJ=   -14055.0813322466     
 iteration         2060 MCMCOBJ=   -14092.7009977621     
 iteration         2070 MCMCOBJ=   -14152.9015042259     
 iteration         2080 MCMCOBJ=   -14110.7843495578     
 iteration         2090 MCMCOBJ=   -14024.2196262615     
 iteration         2100 MCMCOBJ=   -14013.1547005628     
 iteration         2110 MCMCOBJ=   -14080.0128846991     
 iteration         2120 MCMCOBJ=   -14105.9299659366     
 iteration         2130 MCMCOBJ=   -14113.3526103620     
 iteration         2140 MCMCOBJ=   -14066.5161164603     
 iteration         2150 MCMCOBJ=   -14111.6710751734     
 iteration         2160 MCMCOBJ=   -14064.6551908621     
 iteration         2170 MCMCOBJ=   -14069.6866037645     
 iteration         2180 MCMCOBJ=   -14086.7738779308     
 iteration         2190 MCMCOBJ=   -14076.5897316977     
 iteration         2200 MCMCOBJ=   -14006.9142277994     
 iteration         2210 MCMCOBJ=   -14010.8209101691     
 iteration         2220 MCMCOBJ=   -13953.3588097175     
 iteration         2230 MCMCOBJ=   -14137.3063967645     
 iteration         2240 MCMCOBJ=   -14245.5745176537     
 iteration         2250 MCMCOBJ=   -14119.0017010299     
 iteration         2260 MCMCOBJ=   -14029.2558706638     
 iteration         2270 MCMCOBJ=   -14009.1958367122     
 iteration         2280 MCMCOBJ=   -14000.1919565357     
 iteration         2290 MCMCOBJ=   -14033.8290563937     
 iteration         2300 MCMCOBJ=   -14066.8493761396     
 iteration         2310 MCMCOBJ=   -14080.9316418166     
 iteration         2320 MCMCOBJ=   -13984.3990080410     
 iteration         2330 MCMCOBJ=   -14075.6029010238     
 iteration         2340 MCMCOBJ=   -14111.5373808160     
 iteration         2350 MCMCOBJ=   -14109.7597698057     
 iteration         2360 MCMCOBJ=   -14121.0018101335     
 iteration         2370 MCMCOBJ=   -14113.2160299209     
 iteration         2380 MCMCOBJ=   -14059.6134657298     
 iteration         2390 MCMCOBJ=   -14080.2848323249     
 iteration         2400 MCMCOBJ=   -14152.2728190287     
 iteration         2410 MCMCOBJ=   -14094.2957202900     
 iteration         2420 MCMCOBJ=   -14094.4233000550     
 iteration         2430 MCMCOBJ=   -13999.7130332035     
 iteration         2440 MCMCOBJ=   -14038.8884699303     
 iteration         2450 MCMCOBJ=   -14095.7197607118     
 iteration         2460 MCMCOBJ=   -14039.8446843903     
 iteration         2470 MCMCOBJ=   -14093.3620040776     
 iteration         2480 MCMCOBJ=   -13952.4843790977     
 iteration         2490 MCMCOBJ=   -14076.5358752066     
 iteration         2500 MCMCOBJ=   -14160.6369854989     
 iteration         2510 MCMCOBJ=   -13951.5502196649     
 iteration         2520 MCMCOBJ=   -14118.5526380786     
 iteration         2530 MCMCOBJ=   -13995.3119413198     
 iteration         2540 MCMCOBJ=   -14092.7126035240     
 iteration         2550 MCMCOBJ=   -14078.0156005369     
 iteration         2560 MCMCOBJ=   -14044.2474052689     
 iteration         2570 MCMCOBJ=   -14179.2941785541     
 iteration         2580 MCMCOBJ=   -14115.9744844445     
 iteration         2590 MCMCOBJ=   -14105.7116333320     
 iteration         2600 MCMCOBJ=   -14179.8451918018     
 iteration         2610 MCMCOBJ=   -14066.7072535661     
 iteration         2620 MCMCOBJ=   -14063.2259089933     
 iteration         2630 MCMCOBJ=   -14134.8492297622     
 iteration         2640 MCMCOBJ=   -14040.9820684633     
 iteration         2650 MCMCOBJ=   -14012.2878718061     
 iteration         2660 MCMCOBJ=   -14018.1789702588     
 iteration         2670 MCMCOBJ=   -13948.4809257810     
 iteration         2680 MCMCOBJ=   -14071.3102192190     
 iteration         2690 MCMCOBJ=   -14061.1034069309     
 iteration         2700 MCMCOBJ=   -14104.6732291881     
 iteration         2710 MCMCOBJ=   -13895.2833827018     
 iteration         2720 MCMCOBJ=   -14006.7835643768     
 iteration         2730 MCMCOBJ=   -14142.1592084851     
 iteration         2740 MCMCOBJ=   -14047.3663297968     
 iteration         2750 MCMCOBJ=   -14060.3832711073     
 iteration         2760 MCMCOBJ=   -14050.7782629997     
 iteration         2770 MCMCOBJ=   -14018.5837718830     
 iteration         2780 MCMCOBJ=   -14057.7891503476     
 iteration         2790 MCMCOBJ=   -14149.5089579266     
 iteration         2800 MCMCOBJ=   -14016.8194918367     
 iteration         2810 MCMCOBJ=   -14080.3045378042     
 iteration         2820 MCMCOBJ=   -14157.0465908099     
 iteration         2830 MCMCOBJ=   -14060.0774166071     
 iteration         2840 MCMCOBJ=   -14094.4081864129     
 iteration         2850 MCMCOBJ=   -14048.2591747588     
 iteration         2860 MCMCOBJ=   -14058.8823048855     
 iteration         2870 MCMCOBJ=   -13969.9063024853     
 iteration         2880 MCMCOBJ=   -14030.9684643753     
 iteration         2890 MCMCOBJ=   -14049.8611003505     
 iteration         2900 MCMCOBJ=   -14078.1063920526     
 iteration         2910 MCMCOBJ=   -14109.9480027933     
 iteration         2920 MCMCOBJ=   -14056.4853643773     
 iteration         2930 MCMCOBJ=   -14037.8244899941     
 iteration         2940 MCMCOBJ=   -14005.3450997656     
 iteration         2950 MCMCOBJ=   -14202.5019860433     
 iteration         2960 MCMCOBJ=   -14056.5352215251     
 iteration         2970 MCMCOBJ=   -14118.6620932092     
 iteration         2980 MCMCOBJ=   -14188.2105986450     
 iteration         2990 MCMCOBJ=   -14253.1673419524     
 iteration         3000 MCMCOBJ=   -14238.0872407602     
 iteration         3010 MCMCOBJ=   -14189.3452081298     
 iteration         3020 MCMCOBJ=   -14224.8842818795     
 iteration         3030 MCMCOBJ=   -14173.7516025671     
 iteration         3040 MCMCOBJ=   -14113.4295846943     
 iteration         3050 MCMCOBJ=   -14100.8358332906     
 iteration         3060 MCMCOBJ=   -14070.8953328420     
 iteration         3070 MCMCOBJ=   -14075.4236514014     
 iteration         3080 MCMCOBJ=   -14139.7622038235     
 iteration         3090 MCMCOBJ=   -14228.6980938261     
 iteration         3100 MCMCOBJ=   -14205.1854148059     
 iteration         3110 MCMCOBJ=   -14093.9900918368     
 iteration         3120 MCMCOBJ=   -14027.0999309320     
 iteration         3130 MCMCOBJ=   -14121.2126237285     
 iteration         3140 MCMCOBJ=   -14094.5636217579     
 iteration         3150 MCMCOBJ=   -13995.2924155833     
 iteration         3160 MCMCOBJ=   -14144.3169439225     
 iteration         3170 MCMCOBJ=   -14180.8710213178     
 iteration         3180 MCMCOBJ=   -14029.4630370813     
 iteration         3190 MCMCOBJ=   -14151.5622724882     
 iteration         3200 MCMCOBJ=   -14092.3909854681     
 iteration         3210 MCMCOBJ=   -14022.9942386487     
 iteration         3220 MCMCOBJ=   -14118.1675185177     
 iteration         3230 MCMCOBJ=   -14251.6597587405     
 iteration         3240 MCMCOBJ=   -14052.1342314659     
 iteration         3250 MCMCOBJ=   -14064.3664878287     
 iteration         3260 MCMCOBJ=   -13970.9953794982     
 iteration         3270 MCMCOBJ=   -14124.4522007306     
 iteration         3280 MCMCOBJ=   -14112.7727422002     
 iteration         3290 MCMCOBJ=   -13983.5708187937     
 iteration         3300 MCMCOBJ=   -13973.2629892599     
 iteration         3310 MCMCOBJ=   -14023.9441780988     
 iteration         3320 MCMCOBJ=   -14099.5416583197     
 iteration         3330 MCMCOBJ=   -14085.6687496565     
 iteration         3340 MCMCOBJ=   -14081.2213415471     
 iteration         3350 MCMCOBJ=   -13999.3464609809     
 iteration         3360 MCMCOBJ=   -13973.0500548829     
 iteration         3370 MCMCOBJ=   -14128.3185944543     
 iteration         3380 MCMCOBJ=   -14093.0730029454     
 iteration         3390 MCMCOBJ=   -13996.2123185967     
 iteration         3400 MCMCOBJ=   -14041.1666335480     
 iteration         3410 MCMCOBJ=   -14126.1037730338     
 iteration         3420 MCMCOBJ=   -14029.3906056484     
 iteration         3430 MCMCOBJ=   -14055.1853484272     
 iteration         3440 MCMCOBJ=   -14109.4566951539     
 iteration         3450 MCMCOBJ=   -13972.4664949496     
 iteration         3460 MCMCOBJ=   -14089.8336881458     
 iteration         3470 MCMCOBJ=   -14119.0378865110     
 iteration         3480 MCMCOBJ=   -14143.2022055607     
 iteration         3490 MCMCOBJ=   -14092.6259309731     
 iteration         3500 MCMCOBJ=   -14127.5397173070     
 iteration         3510 MCMCOBJ=   -14042.6252665761     
 iteration         3520 MCMCOBJ=   -14275.0982731381     
 iteration         3530 MCMCOBJ=   -14121.2668499456     
 iteration         3540 MCMCOBJ=   -14303.2188571011     
 iteration         3550 MCMCOBJ=   -14245.4328219929     
 iteration         3560 MCMCOBJ=   -14102.0912260778     
 iteration         3570 MCMCOBJ=   -14054.3413247428     
 iteration         3580 MCMCOBJ=   -14106.3151967248     
 iteration         3590 MCMCOBJ=   -14063.5341867290     
 iteration         3600 MCMCOBJ=   -14082.1539028452     
 iteration         3610 MCMCOBJ=   -14073.4055397995     
 iteration         3620 MCMCOBJ=   -14085.3940847573     
 iteration         3630 MCMCOBJ=   -14110.7706546040     
 iteration         3640 MCMCOBJ=   -14032.1038359153     
 iteration         3650 MCMCOBJ=   -14011.1328425155     
 iteration         3660 MCMCOBJ=   -14095.4619411318     
 iteration         3670 MCMCOBJ=   -14103.6800943514     
 iteration         3680 MCMCOBJ=   -14221.4662699117     
 iteration         3690 MCMCOBJ=   -14215.5041857264     
 iteration         3700 MCMCOBJ=   -14180.5457404605     
 iteration         3710 MCMCOBJ=   -14080.3376362266     
 iteration         3720 MCMCOBJ=   -14187.2648785062     
 iteration         3730 MCMCOBJ=   -14179.3953707654     
 iteration         3740 MCMCOBJ=   -14062.8976678886     
 iteration         3750 MCMCOBJ=   -14177.9830198908     
 iteration         3760 MCMCOBJ=   -14125.3087989065     
 iteration         3770 MCMCOBJ=   -14155.0424966899     
 iteration         3780 MCMCOBJ=   -14006.2815517617     
 iteration         3790 MCMCOBJ=   -14178.8275458767     
 iteration         3800 MCMCOBJ=   -14188.4772452949     
 iteration         3810 MCMCOBJ=   -14022.1037425830     
 iteration         3820 MCMCOBJ=   -14123.0385957301     
 iteration         3830 MCMCOBJ=   -14115.1200219994     
 iteration         3840 MCMCOBJ=   -14315.5648867298     
 iteration         3850 MCMCOBJ=   -14170.9124718704     
 iteration         3860 MCMCOBJ=   -14147.0185995874     
 iteration         3870 MCMCOBJ=   -14065.0501865809     
 iteration         3880 MCMCOBJ=   -14092.7945228903     
 iteration         3890 MCMCOBJ=   -14100.0486295745     
 iteration         3900 MCMCOBJ=   -14128.1997144461     
 iteration         3910 MCMCOBJ=   -14145.3548709147     
 iteration         3920 MCMCOBJ=   -14112.0132604969     
 iteration         3930 MCMCOBJ=   -14174.0694329210     
 iteration         3940 MCMCOBJ=   -14091.6455010189     
 iteration         3950 MCMCOBJ=   -14037.5350845622     
 iteration         3960 MCMCOBJ=   -14210.9187776079     
 iteration         3970 MCMCOBJ=   -14126.8512345607     
 iteration         3980 MCMCOBJ=   -14118.1784271242     
 iteration         3990 MCMCOBJ=   -14019.4296370063     
 iteration         4000 MCMCOBJ=   -14018.8523850587     
 iteration         4010 MCMCOBJ=   -14040.7127671086     
 iteration         4020 MCMCOBJ=   -13971.7872112027     
 iteration         4030 MCMCOBJ=   -14160.2294531681     
 iteration         4040 MCMCOBJ=   -14033.6024584680     
 iteration         4050 MCMCOBJ=   -14068.2454982171     
 iteration         4060 MCMCOBJ=   -14064.1651398600     
 iteration         4070 MCMCOBJ=   -14054.1108674804     
 iteration         4080 MCMCOBJ=   -14016.0542601481     
 iteration         4090 MCMCOBJ=   -14028.1936070913     
 iteration         4100 MCMCOBJ=   -14011.2996259502     
 iteration         4110 MCMCOBJ=   -14028.9036885817     
 iteration         4120 MCMCOBJ=   -14081.4251479824     
 iteration         4130 MCMCOBJ=   -14007.9618138462     
 iteration         4140 MCMCOBJ=   -14118.8786646732     
 iteration         4150 MCMCOBJ=   -14140.8730704656     
 iteration         4160 MCMCOBJ=   -14040.4092156708     
 iteration         4170 MCMCOBJ=   -14163.1167651506     
 iteration         4180 MCMCOBJ=   -14147.4052649866     
 iteration         4190 MCMCOBJ=   -14092.7685881676     
 iteration         4200 MCMCOBJ=   -14140.9778289191     
 iteration         4210 MCMCOBJ=   -14120.3032158682     
 iteration         4220 MCMCOBJ=   -14086.1868039947     
 iteration         4230 MCMCOBJ=   -14029.8412858672     
 iteration         4240 MCMCOBJ=   -14045.6693226138     
 iteration         4250 MCMCOBJ=   -14184.8316031138     
 iteration         4260 MCMCOBJ=   -14018.3998356224     
 iteration         4270 MCMCOBJ=   -14195.0243975086     
 iteration         4280 MCMCOBJ=   -14136.9831771206     
 iteration         4290 MCMCOBJ=   -14100.6695145412     
 iteration         4300 MCMCOBJ=   -14117.0660647732     
 iteration         4310 MCMCOBJ=   -14049.7534055166     
 iteration         4320 MCMCOBJ=   -14095.2460366525     
 iteration         4330 MCMCOBJ=   -14057.6740964301     
 iteration         4340 MCMCOBJ=   -13993.6604043579     
 iteration         4350 MCMCOBJ=   -14057.4822558949     
 iteration         4360 MCMCOBJ=   -14092.7864116341     
 iteration         4370 MCMCOBJ=   -14059.4498510496     
 iteration         4380 MCMCOBJ=   -14140.7118197137     
 iteration         4390 MCMCOBJ=   -14118.2213736287     
 iteration         4400 MCMCOBJ=   -14088.3252434208     
 iteration         4410 MCMCOBJ=   -14095.1425042607     
 iteration         4420 MCMCOBJ=   -14147.9260485200     
 iteration         4430 MCMCOBJ=   -14063.0345640605     
 iteration         4440 MCMCOBJ=   -14003.7224502241     
 iteration         4450 MCMCOBJ=   -14152.9169118021     
 iteration         4460 MCMCOBJ=   -14172.6411013662     
 iteration         4470 MCMCOBJ=   -14065.4831480403     
 iteration         4480 MCMCOBJ=   -14110.9660693541     
 iteration         4490 MCMCOBJ=   -14163.8901065320     
 iteration         4500 MCMCOBJ=   -14110.1019924786     
 iteration         4510 MCMCOBJ=   -14144.1514251703     
 iteration         4520 MCMCOBJ=   -14137.6592316315     
 iteration         4530 MCMCOBJ=   -14079.5335161368     
 iteration         4540 MCMCOBJ=   -14115.9936479236     
 iteration         4550 MCMCOBJ=   -14052.4106656827     
 iteration         4560 MCMCOBJ=   -14127.8702943262     
 iteration         4570 MCMCOBJ=   -14055.5507199612     
 iteration         4580 MCMCOBJ=   -14109.2111526134     
 iteration         4590 MCMCOBJ=   -14114.9885191818     
 iteration         4600 MCMCOBJ=   -14131.2979869637     
 iteration         4610 MCMCOBJ=   -14119.1011164908     
 iteration         4620 MCMCOBJ=   -13984.4268590328     
 iteration         4630 MCMCOBJ=   -14043.7029333015     
 iteration         4640 MCMCOBJ=   -14168.0132178843     
 iteration         4650 MCMCOBJ=   -14136.1012388867     
 iteration         4660 MCMCOBJ=   -14233.8963882401     
 iteration         4670 MCMCOBJ=   -14163.6250902326     
 iteration         4680 MCMCOBJ=   -14036.9759295793     
 iteration         4690 MCMCOBJ=   -14073.5643160064     
 iteration         4700 MCMCOBJ=   -14146.4865746611     
 iteration         4710 MCMCOBJ=   -14063.4063121411     
 iteration         4720 MCMCOBJ=   -14085.6576997964     
 iteration         4730 MCMCOBJ=   -14088.1776745055     
 iteration         4740 MCMCOBJ=   -14059.3195469951     
 iteration         4750 MCMCOBJ=   -14209.6689007395     
 iteration         4760 MCMCOBJ=   -14179.5406793213     
 iteration         4770 MCMCOBJ=   -14119.7098857146     
 iteration         4780 MCMCOBJ=   -14054.6358509089     
 iteration         4790 MCMCOBJ=   -14055.0799945705     
 iteration         4800 MCMCOBJ=   -14197.7823920777     
 iteration         4810 MCMCOBJ=   -14075.1085981112     
 iteration         4820 MCMCOBJ=   -14048.7554800469     
 iteration         4830 MCMCOBJ=   -14005.5899783767     
 iteration         4840 MCMCOBJ=   -14056.5616523667     
 iteration         4850 MCMCOBJ=   -14063.9958822554     
 iteration         4860 MCMCOBJ=   -14059.0105840295     
 iteration         4870 MCMCOBJ=   -14041.9310426429     
 iteration         4880 MCMCOBJ=   -14109.7452791445     
 iteration         4890 MCMCOBJ=   -14107.8161873579     
 iteration         4900 MCMCOBJ=   -14106.5367345534     
 iteration         4910 MCMCOBJ=   -14118.5531228987     
 iteration         4920 MCMCOBJ=   -14090.4601249481     
 iteration         4930 MCMCOBJ=   -14061.3851196327     
 iteration         4940 MCMCOBJ=   -14155.6818112403     
 iteration         4950 MCMCOBJ=   -14053.2432203043     
 iteration         4960 MCMCOBJ=   -14075.3320745547     
 iteration         4970 MCMCOBJ=   -14078.0746269318     
 iteration         4980 MCMCOBJ=   -14127.1299346395     
 iteration         4990 MCMCOBJ=   -14219.8139676728     
 iteration         5000 MCMCOBJ=   -14081.7640342244     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -14087.8335530506     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -9676.92859366816     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           900
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    1654.08935976841     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -14087.8335530506     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -12433.7441932822     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    27.6497595571396     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -14087.8335530506     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -14060.1837934935     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:   411.35
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -14087.834       **************************************************
 #OBJS:********************************************       65.549 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.75E-01  6.65E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.81E-02
 
 ETA2
+       -8.86E-03  2.87E-02
 
 ETA3
+       -1.04E-02 -2.13E-03  6.26E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        2.19E-01
 
 ETA2
+       -2.38E-01  1.69E-01
 
 ETA3
+       -1.90E-01 -5.03E-02  2.49E-01
 


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


         TH 1      TH 2      TH 3      TH 4     
 
         1.28E-02  1.28E-02  2.52E-02  2.70E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.14E-03
 
 ETA2
+        2.91E-03  3.18E-03
 
 ETA3
+        5.40E-03  1.17E-02  9.26E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.46E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.39E-03
 
 ETA2
+        7.17E-02  9.31E-03
 
 ETA3
+        9.34E-02  2.75E-01  1.83E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.71E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.63E-04
 
 TH 2
+       -3.58E-05  1.63E-04
 
 TH 3
+       -3.69E-05  1.37E-05  6.33E-04
 
 TH 4
+       -2.89E-06  4.66E-06  1.39E-07  7.30E-04
 
 OM11
+        7.43E-07 -1.85E-07  2.57E-06 -1.90E-06  1.71E-05
 
 OM12
+       -1.50E-07  2.11E-06 -3.58E-07  2.13E-06 -3.54E-06  8.49E-06
 
 OM13
+       -4.24E-07 -5.58E-07 -6.56E-06  4.89E-06 -3.80E-06  1.08E-06  2.91E-05
 
 OM22
+        2.79E-08 -9.79E-07  3.45E-06 -2.45E-07  9.07E-07 -3.50E-06 -5.97E-07  1.01E-05
 
 OM23
+       -4.22E-07  3.53E-07 -1.15E-05 -4.93E-06  9.09E-07 -2.15E-06 -3.75E-06 -1.97E-06  1.38E-04
 
 OM33
+        2.65E-08 -1.33E-06 -6.16E-06 -7.38E-06  1.44E-06 -8.39E-07 -1.28E-05  6.35E-07  1.09E-06  8.57E-05
 
 SG11
+        1.65E-07 -2.73E-08  7.41E-08  1.85E-07 -3.25E-08  1.45E-08  4.12E-08 -1.14E-08 -9.01E-08 -1.51E-07  1.20E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.28E-02
 
 TH 2
+       -2.20E-01  1.28E-02
 
 TH 3
+       -1.15E-01  4.26E-02  2.52E-02
 
 TH 4
+       -8.38E-03  1.35E-02  2.04E-04  2.70E-02
 
 OM11
+        1.40E-02 -3.51E-03  2.47E-02 -1.70E-02  4.14E-03
 
 OM12
+       -4.02E-03  5.68E-02 -4.89E-03  2.71E-02 -2.93E-01  2.91E-03
 
 OM13
+       -6.14E-03 -8.11E-03 -4.84E-02  3.36E-02 -1.70E-01  6.88E-02  5.40E-03
 
 OM22
+        6.88E-04 -2.42E-02  4.31E-02 -2.85E-03  6.89E-02 -3.78E-01 -3.48E-02  3.18E-03
 
 OM23
+       -2.81E-03  2.36E-03 -3.89E-02 -1.55E-02  1.87E-02 -6.28E-02 -5.92E-02 -5.28E-02  1.17E-02
 
 OM33
+        2.24E-04 -1.13E-02 -2.65E-02 -2.95E-02  3.77E-02 -3.11E-02 -2.55E-01  2.16E-02  1.00E-02  9.26E-03
 
 SG11
+        3.72E-02 -6.19E-03  8.50E-03  1.98E-02 -2.27E-02  1.44E-02  2.20E-02 -1.04E-02 -2.21E-02 -4.70E-02  3.46E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        6.52E+03
 
 TH 2
+        1.41E+03  6.49E+03
 
 TH 3
+        3.56E+02 -5.55E+01  1.62E+03
 
 TH 4
+        1.88E+01 -3.21E+01  1.60E+00  1.37E+03
 
 OM11
+       -3.78E+02 -3.03E+02 -2.05E+02  4.03E+01  6.57E+04
 
 OM12
+       -4.22E+02 -1.70E+03 -2.40E+02 -3.17E+02  2.84E+04  1.52E+05
 
 OM13
+        2.10E+02  2.08E+02  4.37E+02 -1.71E+02  7.68E+03 -3.68E+02  3.81E+04
 
 OM22
+       -1.09E+02  7.99E+01 -5.84E+02 -9.45E+01  4.49E+03  5.05E+04  1.14E+03  1.17E+05
 
 OM23
+        4.04E+01 -3.68E+01  1.35E+02  3.63E+01  2.73E+02  2.86E+03  9.80E+02  2.42E+03  7.37E+03
 
 OM33
+        6.52E+01  1.13E+02  1.82E+02  8.56E+01  2.86E+02  4.68E+02  5.53E+03 -3.44E+02  7.90E+01  1.25E+04
 
 SG11
+       -8.91E+03 -2.02E+02 -1.40E+03 -1.92E+03  1.32E+04 -2.18E+03 -3.40E+03  7.92E+03  5.05E+03  1.36E+04  8.37E+06
 
1
 
 
 #TBLN:      6
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
 RAW OUTPUT FILE (FILE): example4.ext
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -9960.43312736134        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  4.2467E+00 -2.3031E+00 -6.7506E-01  6.6508E-01  4.8137E-02 -8.8645E-03 -1.0445E-02  2.8685E-02 -2.1288E-03  6.2558E-02
             1.0230E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01 -1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01
             1.0000E-01
 GRADIENT:  -1.2658E+04 -3.3933E+04  3.3443E+04 -9.5231E-01  1.0169E+01  4.7703E+00  1.9403E+00  1.6518E+01 -9.2739E-01  9.1188E+00
             1.1535E+00
 
0ITERATION NO.:    1    OBJECTIVE VALUE:  -9960.58475388540        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:       22
 NPARAMETR:  4.2481E+00 -2.2992E+00 -6.7889E-01  6.6508E-01  4.8137E-02 -8.8645E-03 -1.0445E-02  2.8685E-02 -2.1288E-03  6.2558E-02
             1.0230E-02
 PARAMETER:  1.0000E-01  1.0001E-01  9.9992E-02  1.0000E-01  1.0000E-01 -1.0000E-01 -1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01
             1.0000E-01
 GRADIENT:   5.9973E+02 -5.6096E+03  2.7607E+04 -9.5233E-01  1.0114E+01  4.1244E+00  1.6428E+00  1.7219E+01 -9.1599E-01  9.0067E+00
             5.4006E-01
 
0ITERATION NO.:    2    OBJECTIVE VALUE:  -9960.58475388540        NO. OF FUNC. EVALS.:  28
 CUMULATIVE NO. OF FUNC. EVALS.:       50
 NPARAMETR:  4.2481E+00 -2.2992E+00 -6.7889E-01  6.6508E-01  4.8137E-02 -8.8645E-03 -1.0445E-02  2.8685E-02 -2.1288E-03  6.2558E-02
             1.0230E-02
 PARAMETER:  1.0000E-01  1.0001E-01  9.9992E-02  1.0000E-01  1.0000E-01 -1.0000E-01 -1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01
             1.0000E-01
 GRADIENT:  -2.4198E+03 -4.4699E+03  1.9967E+05 -9.5233E-01  1.0114E+01  4.1244E+00  1.6428E+00  1.7219E+01 -9.1599E-01  9.0067E+00
             5.3956E-01
 
0ITERATION NO.:    3    OBJECTIVE VALUE:  -9961.01453340532        NO. OF FUNC. EVALS.:  43
 CUMULATIVE NO. OF FUNC. EVALS.:       93             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2490E+00 -2.2979E+00 -6.6864E-01  6.6824E-01  4.7189E-02 -8.9482E-03 -1.0574E-02  2.7343E-02 -1.9395E-03  6.0270E-02
             1.0227E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.1425E-01  9.0057E-02 -1.0195E-01 -1.0225E-01  7.3309E-02 -9.9980E-02  7.9557E-02
             9.9860E-02
 GRADIENT:   1.1594E+04  3.0868E+03  4.5012E+04  9.4655E-01 -1.9868E+00 -4.0160E+00  2.2426E-01 -2.3481E-01 -1.4351E-01  1.3549E+00
            -2.6688E+00
 
0ITERATION NO.:    4    OBJECTIVE VALUE:  -9961.03755825623        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      101
 NPARAMETR:  4.2489E+00 -2.2979E+00 -6.7339E-01  6.6826E-01  4.7104E-02 -8.9412E-03 -1.0594E-02  2.7192E-02 -1.9220E-03  5.9983E-02
             1.0229E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.1432E-01  8.9156E-02 -1.0197E-01 -1.0254E-01  7.0362E-02 -9.9977E-02  7.6937E-02
             9.9922E-02
 GRADIENT:   9.2461E+03  3.4376E+03  3.6759E+04  9.5467E-01 -3.1137E+00 -4.4657E+00 -5.0406E-01 -2.1948E+00 -4.0855E-02  3.8369E-01
            -2.3393E+00
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -9961.03755828314        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      115
 NPARAMETR:  4.2489E+00 -2.2979E+00 -6.7339E-01  6.6826E-01  4.7104E-02 -8.9412E-03 -1.0594E-02  2.7192E-02 -1.9220E-03  5.9983E-02
             1.0229E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.1432E-01  8.9156E-02 -1.0197E-01 -1.0254E-01  7.0362E-02 -9.9977E-02  7.6937E-02
             9.9922E-02
 GRADIENT:   9.2460E+03  3.4376E+03  3.6759E+04  9.5466E-01 -3.1136E+00 -4.4657E+00 -5.0405E-01 -2.1948E+00 -4.0854E-02  3.8369E-01
            -2.3393E+00
 
0ITERATION NO.:    6    OBJECTIVE VALUE:  -9961.03755828314        NO. OF FUNC. EVALS.:  21
 CUMULATIVE NO. OF FUNC. EVALS.:      136
 NPARAMETR:  4.2489E+00 -2.2979E+00 -6.7339E-01  6.6826E-01  4.7104E-02 -8.9412E-03 -1.0594E-02  2.7192E-02 -1.9220E-03  5.9983E-02
             1.0229E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.1432E-01  8.9156E-02 -1.0197E-01 -1.0254E-01  7.0362E-02 -9.9977E-02  7.6937E-02
             9.9922E-02
 GRADIENT:   6.2257E+03  4.5758E+03  2.0886E+05  9.5466E-01 -3.1136E+00 -4.4657E+00 -5.0405E-01 -2.1948E+00 -4.0854E-02  3.8369E-01
            -2.3398E+00
 
0ITERATION NO.:    7    OBJECTIVE VALUE:  -9961.05042287937        NO. OF FUNC. EVALS.:  39
 CUMULATIVE NO. OF FUNC. EVALS.:      175             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2484E+00 -2.2979E+00 -6.7257E-01  6.6507E-01  4.7319E-02 -8.7740E-03 -1.0546E-02  2.7334E-02 -1.9583E-03  5.9820E-02
             1.0243E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  9.9961E-02  9.1434E-02 -9.9831E-02 -1.0184E-01  7.4502E-02 -9.9078E-02  7.5879E-02
             1.0063E-01
 GRADIENT:   6.0393E+03  1.9230E+03  3.7964E+04 -9.5839E-01  2.1867E-01  1.0438E+00  2.9371E-01  7.0452E-01  6.5589E-03 -6.2027E-02
             3.0523E+00
 
0ITERATION NO.:    8    OBJECTIVE VALUE:  -9961.05042287937        NO. OF FUNC. EVALS.:  21
 CUMULATIVE NO. OF FUNC. EVALS.:      196
 NPARAMETR:  4.2484E+00 -2.2979E+00 -6.7257E-01  6.6507E-01  4.7319E-02 -8.7740E-03 -1.0546E-02  2.7334E-02 -1.9583E-03  5.9820E-02
             1.0243E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  9.9961E-02  9.1434E-02 -9.9831E-02 -1.0184E-01  7.4502E-02 -9.9078E-02  7.5879E-02
             1.0063E-01
 GRADIENT:   3.0232E+03  3.0598E+03  2.0984E+05 -9.5838E-01  2.1867E-01  1.0438E+00  2.9371E-01  7.0452E-01  6.5589E-03 -6.2027E-02
             3.0518E+00
 
0ITERATION NO.:    9    OBJECTIVE VALUE:  -9961.05113745068        NO. OF FUNC. EVALS.:  41
 CUMULATIVE NO. OF FUNC. EVALS.:      237             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2484E+00 -2.2978E+00 -6.7248E-01  6.6825E-01  4.7307E-02 -8.8131E-03 -1.0583E-02  2.7312E-02 -1.9401E-03  5.9857E-02
             1.0225E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.1430E-01  9.1305E-02 -1.0029E-01 -1.0221E-01  7.3774E-02 -9.9098E-02  7.6056E-02
             9.9761E-02
 GRADIENT:   6.3305E+03  2.8202E+03  3.8168E+04  9.5311E-01 -1.1989E-01 -1.2239E-02  5.2448E-04  1.6379E-01 -8.7221E-04  8.2496E-03
            -3.1535E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -9961.05113745068        NO. OF FUNC. EVALS.:  22
 CUMULATIVE NO. OF FUNC. EVALS.:      259
 NPARAMETR:  4.2484E+00 -2.2978E+00 -6.7248E-01  6.6825E-01  4.7307E-02 -8.8131E-03 -1.0583E-02  2.7312E-02 -1.9401E-03  5.9857E-02
             1.0225E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.1430E-01  9.1305E-02 -1.0029E-01 -1.0221E-01  7.3774E-02 -9.9098E-02  7.6056E-02
             9.9761E-02
 GRADIENT:   3.3094E+03  3.9591E+03  2.1034E+05  9.5311E-01 -1.1989E-01 -1.2239E-02  5.2448E-04  1.6379E-01 -8.7221E-04  8.2496E-03
            -3.1540E+00
 
0ITERATION NO.:   11    OBJECTIVE VALUE:  -9961.05527834665        NO. OF FUNC. EVALS.:  39
 CUMULATIVE NO. OF FUNC. EVALS.:      298             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2484E+00 -2.2978E+00 -6.7248E-01  6.6666E-01  4.7312E-02 -8.8133E-03 -1.0584E-02  2.7306E-02 -1.9394E-03  5.9856E-02
             1.0235E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0714E-01  9.1358E-02 -1.0028E-01 -1.0221E-01  7.3664E-02 -9.9088E-02  7.6046E-02
             1.0021E-01
 GRADIENT:   6.0977E+03  2.7313E+03  3.8129E+04 -7.9797E-04 -4.1473E-02 -1.0001E-02  4.8470E-03  1.1642E-01 -4.5442E-04  4.2984E-03
             4.9101E-02
 
0ITERATION NO.:   12    OBJECTIVE VALUE:  -9961.05527839823        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:      309
 NPARAMETR:  4.2484E+00 -2.2978E+00 -6.7248E-01  6.6666E-01  4.7312E-02 -8.8133E-03 -1.0584E-02  2.7306E-02 -1.9394E-03  5.9856E-02
             1.0235E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0713E-01  9.1358E-02 -1.0028E-01 -1.0221E-01  7.3664E-02 -9.9088E-02  7.6046E-02
             1.0021E-01
 GRADIENT:   6.0973E+03  2.7311E+03  3.8129E+04 -1.9866E-03 -4.1377E-02 -9.9975E-03  4.8397E-03  1.1636E-01 -4.5386E-04  4.2931E-03
             5.3112E-02
 
0ITERATION NO.:   13    OBJECTIVE VALUE:  -9961.05527846462        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      323
 NPARAMETR:  4.2484E+00 -2.2978E+00 -6.7248E-01  6.6666E-01  4.7312E-02 -8.8133E-03 -1.0584E-02  2.7306E-02 -1.9394E-03  5.9856E-02
             1.0235E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0713E-01  9.1358E-02 -1.0028E-01 -1.0221E-01  7.3664E-02 -9.9088E-02  7.6046E-02
             1.0021E-01
 GRADIENT:   6.0973E+03  2.7311E+03  3.8129E+04 -2.0036E-03 -4.1375E-02 -9.9974E-03  4.8391E-03  1.1636E-01 -4.5386E-04  4.2931E-03
             5.3166E-02
 
0ITERATION NO.:   14    OBJECTIVE VALUE:  -9961.05527846462        NO. OF FUNC. EVALS.:  26
 CUMULATIVE NO. OF FUNC. EVALS.:      349
 NPARAMETR:  4.2484E+00 -2.2978E+00 -6.7248E-01  6.6666E-01  4.7312E-02 -8.8133E-03 -1.0584E-02  2.7306E-02 -1.9394E-03  5.9856E-02
             1.0235E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0713E-01  9.1358E-02 -1.0028E-01 -1.0221E-01  7.3664E-02 -9.9088E-02  7.6046E-02
             1.0021E-01
 GRADIENT:   3.0788E+03  3.8689E+03  2.1015E+05 -2.0021E-03 -4.1375E-02 -9.9974E-03  4.8391E-03  1.1636E-01 -4.5386E-04  4.2931E-03
             5.2666E-02
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -9961.05529205431        NO. OF FUNC. EVALS.:  43
 CUMULATIVE NO. OF FUNC. EVALS.:      392             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2483E+00 -2.2978E+00 -6.7247E-01  6.6667E-01  4.7314E-02 -8.8133E-03 -1.0585E-02  2.7297E-02 -1.9379E-03  5.9855E-02
             1.0234E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0715E-01  9.1385E-02 -1.0028E-01 -1.0222E-01  7.3494E-02 -9.9068E-02  7.6034E-02
             1.0020E-01
 GRADIENT:   6.0101E+03  2.7404E+03  3.8144E+04  1.2901E-03 -1.1661E-02 -4.7185E-03  3.1764E-03  3.4696E-03  1.0511E-04 -9.9445E-04
             3.4309E-03
 
0ITERATION NO.:   16    OBJECTIVE VALUE:  -9961.05529205431        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      406
 NPARAMETR:  4.2483E+00 -2.2978E+00 -6.7247E-01  6.6667E-01  4.7314E-02 -8.8133E-03 -1.0585E-02  2.7297E-02 -1.9379E-03  5.9855E-02
             1.0234E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0715E-01  9.1385E-02 -1.0028E-01 -1.0222E-01  7.3494E-02 -9.9068E-02  7.6034E-02
             1.0020E-01
 GRADIENT:   2.9915E+03  3.8782E+03  2.1017E+05  1.2886E-03 -1.1661E-02 -4.7185E-03  3.1764E-03  3.4696E-03  1.0511E-04 -9.9445E-04
             2.9488E-03
 
0ITERATION NO.:   17    OBJECTIVE VALUE:  -9961.05529205431        NO. OF FUNC. EVALS.:  27
 CUMULATIVE NO. OF FUNC. EVALS.:      433
 NPARAMETR:  4.2483E+00 -2.2978E+00 -6.7247E-01  6.6667E-01  4.7314E-02 -8.8133E-03 -1.0585E-02  2.7297E-02 -1.9379E-03  5.9855E-02
             1.0234E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0001E-01  1.0715E-01  9.1385E-02 -1.0028E-01 -1.0222E-01  7.3494E-02 -9.9068E-02  7.6034E-02
             1.0020E-01
 GRADIENT:   1.3317E+01 -3.1498E+01 -6.0448E+00  9.3162E-04 -1.3319E-02 -5.2503E-03  3.2100E-03  2.3835E-03  1.0358E-04 -2.7830E-03
            -2.1838E-04
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.
 REGARD THE RESULTS OF THE ESTIMATION STEP CAREFULLY, AND ACCEPT THEM ONLY
 AFTER CHECKING THAT THE COVARIANCE STEP PRODUCES REASONABLE OUTPUT.
 NO. OF FUNCTION EVALUATIONS USED:      433
 NO. OF SIG. DIGITS IN FINAL EST.:  3.9

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -7.6481E-03  3.5994E-03  0.0000E+00
 SE:             1.4905E-02  1.1055E-02  0.0000E+00
 N:                     200         200           0
 
 ETASHRINKSD(%)  2.8537E+00  5.1393E+00  1.0000E+02
 ETASHRINKVR(%)  5.6260E+00  1.0015E+01  1.0000E+02
 EBVSHRINKSD(%)  1.9511E+00  5.0648E+00  1.0000E+02
 EBVSHRINKVR(%)  3.8641E+00  9.8732E+00  1.0000E+02
 EPSSHRINKSD(%)  1.1414E+01
 EPSSHRINKVR(%)  2.1525E+01
 

 SUBMODEL    2
 
 ETABAR:         1.2713E-02  0.0000E+00 -2.7693E-03
 SE:             2.1743E-02  0.0000E+00  2.4451E-02
 N:                     100           0         100
 
 ETASHRINKSD(%)  1.0000E-10  1.0000E+02  1.0000E-10
 ETASHRINKVR(%)  1.0000E-10  1.0000E+02  1.0000E-10
 EBVSHRINKSD(%)  1.9919E+00  1.0000E+02  1.0485E-01
 EBVSHRINKVR(%)  3.9441E+00  1.0000E+02  2.0959E-01
 EPSSHRINKSD(%)  1.4383E+01
 EPSSHRINKVR(%)  2.6697E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         2400
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    4410.90495938243     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -9961.05529205431     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5550.15033267188     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           600
  
 #TERE:
 Elapsed estimation  time in seconds:    51.91
 Elapsed covariance  time in seconds:    13.57
 Elapsed postprocess time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9961.055       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.72E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.73E-02
 
 ETA2
+       -8.81E-03  2.73E-02
 
 ETA3
+       -1.06E-02 -1.94E-03  5.99E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.45E-01  1.65E-01
 
 ETA3
+       -1.99E-01 -4.79E-02  2.45E-01
 


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


         TH 1      TH 2      TH 3      TH 4     
 
         1.28E-02  1.22E-02  2.42E-02  2.72E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3     
 
 ETA1
+        4.02E-03
 
 ETA2
+        2.79E-03  3.03E-03
 
 ETA3
+        5.30E-03  1.70E+00  8.48E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        3.43E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3     
 
 ETA1
+        9.25E-03
 
 ETA2
+        7.14E-02  9.16E-03
 
 ETA3
+        9.56E-02  4.21E+01  1.73E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        1.70E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.64E-04
 
 TH 2
+       -3.42E-05  1.48E-04
 
 TH 3
+       -3.63E-05  7.59E-06  5.86E-04
 
 TH 4
+       -1.06E-09  8.42E-09  3.55E-08  7.41E-04
 
 OM11
+        3.98E-08 -1.04E-07 -2.55E-08 -2.04E-11  1.62E-05
 
 OM12
+       -4.86E-08  1.36E-06  1.40E-08 -4.91E-10 -3.37E-06  7.81E-06
 
 OM13
+       -1.00E-08  1.94E-08 -7.33E-06 -3.16E-09 -3.58E-06  7.46E-07  2.81E-05
 
 OM22
+        1.92E-07 -1.60E-06 -3.69E-08  2.64E-09  6.94E-07 -3.07E-06 -1.54E-07  9.17E-06
 
 OM23
+       -8.35E-06 -9.33E-07  3.45E-06  1.77E-07  2.09E-06 -2.68E-06 -6.45E-06  2.13E-06  2.90E+00
 
 OM33
+        1.19E-07 -5.81E-08  2.95E-06 -2.57E-08  8.00E-07 -1.65E-07 -1.22E-05  2.52E-08 -4.30E-04  7.19E-05
 
 SG11
+        1.15E-07  1.62E-08  1.05E-09 -5.02E-12 -2.12E-08  1.45E-08  3.35E-09 -3.02E-08 -7.77E-06 -7.80E-10  1.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.28E-02
 
 TH 2
+       -2.19E-01  1.22E-02
 
 TH 3
+       -1.17E-01  2.57E-02  2.42E-02
 
 TH 4
+       -3.04E-06  2.54E-05  5.39E-05  2.72E-02
 
 OM11
+        7.71E-04 -2.12E-03 -2.62E-04 -1.86E-07  4.02E-03
 
 OM12
+       -1.36E-03  4.00E-02  2.06E-04 -6.46E-06 -3.00E-01  2.79E-03
 
 OM13
+       -1.47E-04  3.00E-04 -5.71E-02 -2.19E-05 -1.68E-01  5.04E-02  5.30E-03
 
 OM22
+        4.94E-03 -4.34E-02 -5.04E-04  3.21E-05  5.70E-02 -3.63E-01 -9.61E-03  3.03E-03
 
 OM23
+       -3.82E-04 -4.50E-05  8.36E-05  3.82E-06  3.04E-04 -5.63E-04 -7.15E-04  4.12E-04  1.70E+00
 
 OM33
+        1.10E-03 -5.62E-04  1.44E-02 -1.11E-04  2.35E-02 -6.97E-03 -2.72E-01  9.82E-04 -2.98E-02  8.48E-03
 
 SG11
+        2.61E-02  3.87E-03  1.26E-04 -5.37E-07 -1.53E-02  1.51E-02  1.84E-03 -2.91E-02 -1.33E-02 -2.68E-04  3.43E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        6.48E+03
 
 TH 2
+        1.48E+03  7.10E+03
 
 TH 3
+        3.83E+02 -4.93E-01  1.74E+03
 
 TH 4
+       -2.61E-02 -8.23E-02 -8.05E-02  1.35E+03
 
 OM11
+       -3.90E+01 -1.98E+02  1.05E+02  6.19E-02  7.00E+04
 
 OM12
+       -2.19E+02 -9.57E+02 -3.71E-01 -6.57E-02  3.16E+04  1.62E+05
 
 OM13
+        9.94E+01  2.29E+00  4.69E+02  3.78E-01  8.45E+03  7.77E-01  3.97E+04
 
 OM22
+        3.41E+01  8.97E+02 -2.55E+00 -4.25E-01  5.43E+03  5.18E+04  1.18E+01  1.26E+05
 
 OM23
+        1.12E-15 -3.07E-17  2.53E-14  2.07E-15  1.28E-01  8.70E-02  1.07E+00  2.63E-02  3.45E-01
 
 OM33
+       -8.52E+00  3.37E+00  6.89E+00  5.49E-01  7.27E+02  3.06E+00  6.65E+03  1.75E+01  2.24E+00  1.51E+04
 
 SG11
+       -6.50E+03 -2.10E+03 -3.84E+02 -1.86E-03  9.92E+03 -6.57E+02  4.07E+02  2.68E+04  2.28E+01  2.01E+02  8.51E+06
 
 Elapsed finaloutput time in seconds:     0.34
 #CPUT: Total CPU Time in Seconds,      701.630
Stop Time: 
Sat 04/22/2017 
09:47 AM

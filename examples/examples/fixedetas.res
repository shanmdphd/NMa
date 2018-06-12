Fri 11/18/2016 
02:32 PM
$PROB RUN# Example 1 (from samp5l)
$INPUT ID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT SID CID
$DATA superid2.csv

$SUBROUTINES ADVAN1 TRANS2

$PK
MU_1=THETA(1)
MU_2=THETA(2)
CL=DEXP(MU_1+ETA(1)+ETA(3)+ETA(5))
V=DEXP(MU_2+ETA(2)+ETA(4)+ETA(6))
S1=V
IF(COMACT==1) THEN
PREDCL=CL
PREDV=V
ENDIF

$ERROR
IPRED=F
IRES=DV-F
Y = F + F*EPS(1)

; Initial values of THETA
$THETA 5.0 5.0
;$OMEGA 0.02 0.02 0.0 FIXED 0.0 FIXED 0.0 FIXED 0.0 FIXED
$OMEGA BLOCK(2)
0.03
0.00001 0.03

$OMEGA BLOCK(2)
0.03
0.00001 0.03

$OMEGA BLOCK(2)
0.03
0.00001 0.03

$SIGMA 
0.05

$LEVEL
SID=(3[1],4[2])
CID=(5[3],6[4])

$EST METHOD=ITS INTERACTION PRINT=1 NSIG=3 NITER=10 SIGL=6 FNLETA=0 MCETA=3
$COV MATRIX=R UNCONDITIONAL
$TABLE  ID PREDCL PREDV DOSE RATE TIME CONC IPRED PRED IRES RES WRES CWRES EWRES NPDE NOAPPEND ONEHEADER 
        FILE=FIXEDETAS.tab  NOPRINT ESAMPLE=1000 SEED=1115678
$TABLE  ID PREDCL PREDV DOSE RATE TIME CONC IPRED PRED IRES RES WRES CWRES EWRES NPDE NOAPPEND ONEHEADER FIXEDETAS="3-6" 
        FILE=FIXEDETAS2.tab  NOPRINT ESAMPLE=1000 SEED 1115678
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 12) MU_001: SHOULD NOT BE ASSOCIATED WITH ETA(003)

 (MU_WARNING 11) MU_001: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_001: SHOULD NOT BE ASSOCIATED WITH ETA(005)

 (MU_WARNING 11) MU_001: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_002: SHOULD NOT BE ASSOCIATED WITH ETA(004)

 (MU_WARNING 11) MU_002: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_002: SHOULD NOT BE ASSOCIATED WITH ETA(006)

 (MU_WARNING 11) MU_002: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       18 NOV 2016
Days until program expires :4944
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha14 (nm74a14)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 RUN# Example 1 (from samp5l)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:    20000
 NO. OF DATA ITEMS IN DATA SET:  10
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   2   4   5   0   0   8   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME CONC DOSE RATE EVID MDV CMT SID CID
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 PREDCL PREDV IPRED IRES
0FORMAT FOR DATA:
 (7E10.0/3E10.0)

 TOT. NO. OF OBS RECS:    17500
 TOT. NO. OF INDIVIDUALS:   2500
0LENGTH OF THETA:   2
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  0  0  2
  0  0  2  2
  0  0  0  0  3
  0  0  0  0  3  3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.5000E+01  0.5000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.3000E-01
                  0.1000E-04   0.3000E-01
        2                                                                                   NO
                  0.3000E-01
                  0.1000E-04   0.3000E-01
        3                                                                                   NO
                  0.3000E-01
                  0.1000E-04   0.3000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.5000E-01
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
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           2
 SEED NUMBER (SEED):    1115678
 RANMETHOD:             3U
 MC SAMPLES (ESAMPLE):    1000
 WRES SQUARE ROOT TYPE (WRESCHOL): EIGENVALUE
0-- TABLE   1 --
0RECORDS ONLY:    ALL
 PRINTED:                NO
 HEADERS:               ONE
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
 FIXED_EFFECT_ETAS:
0USER-CHOSEN ITEMS:
 ID PREDCL PREDV DOSE RATE TIME CONC IPRED PRED IRES RES WRES CWRES EWRES NPDE
0-- TABLE   2 --
0RECORDS ONLY:    ALL
 PRINTED:                NO
 HEADERS:               ONE
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
 FIXED_EFFECT_ETAS:     3-6
0USER-CHOSEN ITEMS:
 ID PREDCL PREDV DOSE RATE TIME CONC IPRED PRED IRES RES WRES CWRES EWRES NPDE
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha14 (nm74a14)

 ONE COMPARTMENT MODEL (ADVAN1)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   2
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1

 TRANSLATOR WILL CONVERT PARAMETERS
 CLEARANCE (CL) AND VOLUME (V) TO K (TRANS2)
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
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      6
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     5
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    8

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 
 
 #TBLN:      1
 #METH: Iterative Two Stage
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            528
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    3
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      6
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     6
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          OFF
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): fixedetas.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 NESTED LEVEL MAPS:
  SID=(3[1],4[2])
  CID=(5[3],6[4])
 Level Weighting Type (LEVWT):0
 EM OR BAYESIAN METHOD USED:                ITERATIVE TWO STAGE (ITS)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  0
 ITERATIONS (NITER):                        10
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
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   451071.104559940
 iteration            1 OBJ=  -14995.1141044352
 iteration            2 OBJ=  -35873.5269386687
 iteration            3 OBJ=  -40830.1127519299
 iteration            4 OBJ=  -41154.7946908459
 iteration            5 OBJ=  -41169.9677785737
 iteration            6 OBJ=  -41170.7871393466
 iteration            7 OBJ=  -41170.7243276807
 iteration            8 OBJ=  -41170.6570828825
 iteration            9 OBJ=  -41170.6220873080
 iteration           10 OBJ=  -41170.6057329983
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.2941E-05 -1.2708E-05 -5.8287E-18  1.5174E-17 -7.8255E-16  5.6981E-16
 SE:             1.6652E-03  1.7458E-03  9.8883E-03  1.0710E-02  6.1688E-02  5.5549E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         9.8901E-01  9.9419E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETASHRINKSD(%)  1.0854E+01  1.0624E+01  1.0320E-04  8.8093E-05  7.9201E-05  5.6152E-05
 ETASHRINKVR(%)  2.0529E+01  2.0119E+01  2.0640E-04  1.7619E-04  1.5840E-04  1.1230E-04
 EBVSHRINKSD(%)  1.0845E+01  1.0618E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EBVSHRINKVR(%)  2.0514E+01  2.0109E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSSHRINKSD(%)  1.1907E+01
 EPSSHRINKVR(%)  2.2396E+01
 
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):        17500
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    32162.8486621635     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -41170.6057329983     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -9007.75707083478     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                          4998
  
 #TERE:
 Elapsed estimation  time in seconds:    12.71
 Elapsed covariance  time in seconds:     0.89
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41170.606       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.00E+00  3.63E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6     
 
 ETA1
+        9.69E-03
 
 ETA2
+        3.84E-05  1.06E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.72E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.75E-03  3.19E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.91E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.69E-02  8.04E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        9.96E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6     
 
 ETA1
+        9.85E-02
 
 ETA2
+        3.79E-03  1.03E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.65E-01
 
 ETA4
+        0.00E+00  0.00E+00  5.96E-02  1.78E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.15E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.89E-01  2.83E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        9.98E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.35E-03  2.44E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6     
 
 ETA1
+        3.39E-04
 
 ETA2
+        2.55E-04  3.78E-04
 
 ETA3
+        0.00E+00  0.00E+00  2.27E-03
 
 ETA4
+        0.00E+00  0.00E+00  1.84E-03  2.72E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.17E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.42E-02  2.69E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1     
 
 EPS1
+        1.28E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6     
 
 ETA1
+        1.72E-03
 
 ETA2
+        2.52E-02  1.84E-03
 
 ETA3
+       ......... .........  6.89E-03
 
 ETA4
+       ......... .........  6.28E-02  7.61E-03
 
 ETA5
+       ......... ......... ......... .........  5.04E-02
 
 ETA6
+       ......... ......... ......... .........  2.46E-01  4.74E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1     
 
 EPS1
+        6.39E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        5.53E-06
 
 TH 2
+        3.22E-07  5.95E-06
 
 OM11
+       -5.95E-09  5.71E-08  1.15E-07
 
 OM12
+        6.69E-08  2.46E-08  8.28E-09  6.53E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        4.61E-09  1.53E-08 -3.06E-09 -2.99E-11  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.43E-07
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+        6.27E-08  6.39E-08  1.32E-08  6.40E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.77E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.15E-06
 
 OM34
+       -1.94E-09  2.10E-08 -6.51E-09 -1.96E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.16E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.32E-07  3.39E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        4.01E-08  3.26E-09  1.12E-08  6.10E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.69E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.31E-07  1.10E-07  0.00E+00  0.00E+00  7.38E-06
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+        2.20E-06  3.98E-08  1.52E-07  5.23E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.94E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.28E-06  2.46E-06  0.00E+00  0.00E+00  4.68E-06  0.00E+00  0.00E+00  1.01E-03
 
 OM56
+       -5.57E-07  2.00E-07 -6.02E-08  1.14E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.69E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.23E-06  1.77E-06  0.00E+00  0.00E+00 -1.06E-06  0.00E+00  0.00E+00 -4.97E-04  5.85E-04
 
 OM66
+        1.21E-06  9.35E-07  3.87E-08 -6.72E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.39E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.71E-07 -5.24E-06  0.00E+00  0.00E+00  6.29E-07  0.00E+00  0.00E+00  1.60E-04 -1.78E-04  7.22E-04
 
 SG11
+        1.51E-08  2.89E-09 -1.85E-09 -2.60E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.16E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.00E-09 -5.68E-10  0.00E+00  0.00E+00 -2.36E-09  0.00E+00  0.00E+00 -1.23E-07  2.73E-08 -1.92E-08  1.63E-08
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.35E-03
 
 TH 2
+        5.62E-02  2.44E-03
 
 OM11
+       -7.47E-03  6.91E-02  3.39E-04
 
 OM12
+        1.11E-01  3.95E-02  9.57E-02  2.55E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.19E-03  1.66E-02 -2.39E-02 -3.09E-04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.78E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+        1.17E-02  1.15E-02  1.72E-02  1.10E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.06E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.27E-03
 
 OM34
+       -4.49E-04  4.67E-03 -1.04E-02 -4.17E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.67E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -5.56E-02  1.84E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        6.28E-03  4.92E-04  1.22E-02  8.80E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.65E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.12E-02  2.21E-02  0.00E+00  0.00E+00  2.72E-03
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+        2.96E-02  5.15E-04  1.41E-02  6.45E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.45E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.34E-02  4.22E-02  0.00E+00  0.00E+00  5.44E-02  0.00E+00  0.00E+00  3.17E-02
 
 OM56
+       -9.80E-03  3.39E-03 -7.34E-03  1.85E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.04E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.69E-02  3.97E-02  0.00E+00  0.00E+00 -1.62E-02  0.00E+00  0.00E+00 -6.48E-01  2.42E-02
 
 OM66
+        1.92E-02  1.43E-02  4.25E-03 -9.79E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.32E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.43E-02 -1.06E-01  0.00E+00  0.00E+00  8.61E-03  0.00E+00  0.00E+00  1.88E-01 -2.74E-01  2.69E-02
 
 SG11
+        5.05E-02  9.27E-03 -4.27E-02 -7.97E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.47E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  6.90E-03 -2.42E-03  0.00E+00  0.00E+00 -6.82E-03  0.00E+00  0.00E+00 -3.04E-02  8.83E-03 -5.61E-03  1.28E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        1.85E+05
 
 TH 2
+       -9.26E+03  1.70E+05
 
 OM11
+        2.56E+04 -8.26E+04  8.85E+06
 
 OM12
+       -1.89E+05 -4.45E+04 -1.11E+06  1.57E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -9.20E+03 -2.10E+04  2.05E+05 -1.61E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  7.04E+06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+       -1.76E+03 -2.04E+03 -1.91E+04 -1.09E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.37E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.97E+05
 
 OM34
+       -9.02E+02 -1.89E+03  1.02E+04  9.77E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.99E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.43E+04  3.03E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -6.70E+02  7.19E+01 -1.17E+04 -1.05E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.51E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.89E+03 -3.52E+03  0.00E+00  0.00E+00  1.36E+05
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+       -5.14E+02 -6.85E-01 -9.63E+02 -3.80E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.24E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -6.88E+02 -2.14E+03  0.00E+00  0.00E+00 -8.52E+02  0.00E+00  0.00E+00  1.74E+03
 
 OM56
+       -3.11E+02 -1.56E+02  9.21E+01 -6.41E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.90E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.03E+03 -1.98E+03  0.00E+00  0.00E+00 -4.54E+02  0.00E+00  0.00E+00  1.47E+03  3.10E+03
 
 OM66
+       -2.97E+02 -2.72E+02 -6.44E+01  1.86E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.45E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.64E+02  2.20E+03  0.00E+00  0.00E+00 -5.19E+01  0.00E+00  0.00E+00 -3.81E+01  4.28E+02  1.52E+03
 
 SG11
+       -1.75E+05 -3.42E+04  9.99E+05  2.94E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.90E+05  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -2.63E+04 -6.92E+02  0.00E+00  0.00E+00  1.46E+04  0.00E+00  0.00E+00  1.13E+04  6.87E+03  1.70E+03  6.20E+07
 
 SKIPPING EXECUTION OF SLOW FNLMOD/FNLETA BY USER REQUEST
 Elapsed postprocess time in seconds:    28.47
 Elapsed postprocess time in seconds:    26.98
 #CPUT: Total CPU Time in Seconds,       69.982
Stop Time: 
Fri 11/18/2016 
02:34 PM

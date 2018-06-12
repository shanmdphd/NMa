Sat 09/07/2013 
01:55 PM
;Model Desc: Interoccasion Variability
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB run# example7 (from ad1tr2_occ)
$INPUT C SET ID  TIME  AMT RATE EVID MDV CMT DV
$DATA example7.csv IGNORE=C

$SUBROUTINES ADVAN1 TRANS2

$PRIOR NWPRI NTHETA=2, NETA=5, NTHP=0, NETP=5, NPEXP=1

$PK
MU_1=THETA(1)
MU_2=THETA(2)
V=DEXP(MU_1+ETA(1))
CLB=DEXP(MU_2+ETA(2))
DCL1=DEXP(ETA(3))
DCL2=DEXP(ETA(4))
DCL3=DEXP(ETA(5))
S1=V
DCL=DCL1
IF(TIME.GE.5.0) DCL=DCL2
IF(TIME.GE.10.0) DCL=DCL3
CL=CLB*DCL
VC=V

$ERROR
IPRED=F
Y = F+F*EPS(1)

;Initial Thetas
$THETA
 2.0  ;[MU_1]
 2.0  ;[MU_2]

;Initial omegas
$OMEGA BLOCK(2)
 .3 ;[p]
 -.01  ;[f]
 .3 ;[p]
$OMEGA BLOCK(1)
 .1  ;[p]
$OMEGA BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME

$SIGMA
 0.1 ;[p]

; Degrees of freedom for Prior Omega blocks
$THETA (2.0 FIXED) (1.0 FIXED)
; Prior Omegas
$OMEGA BLOCK(2)
 .14 FIX
 0.0 .125
$OMEGA BLOCK(1) .0164 FIX
$OMEGA BLOCK(1) SAME
$OMEGA  BLOCK(1) SAME

$EST METHOD=ITS INTERACTION FILE=example7.ext   NITER=10000 PRINT=5 NOABORT SIGL=8 CTYPE=3 CITER=10
 NOPRIOR=1 CALPHA=0.05 NSIG=2
$EST METHOD=SAEM INTERACTION NBURN=30000 NITER=500 SIGL=8 ISAMPLE=2 PRINT=10 SEED=1556678 CTYPE=3
 CITER=10 CALPHA=0.05 NOPRIOR=1
$EST METHOD=IMP  INTERACTION EONLY=1 NITER=4 ISAMPLE=3000 PRINT=1 SIGL=10 NOPRIOR=1 MAPITER=0 
$EST METHOD=BAYES INTERACTION FILE=example7.txt NBURN=10000 NITER=10000 PRINT=100 CTYPE=3 CITER=10
CALPHA=0.05 NOPRIOR=0
$EST METHOD=COND INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 PRINT=5 NOABORT NOPRIOR=1
     FILE=example7.ext
$COV MATRIX=R PRINT=E UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        7 SEP 2013
Days until program expires :6110
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3beta7.0(N)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 run# example7 (from ad1tr2_occ)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     4500
 NO. OF DATA ITEMS IN DATA SET:  10
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:  10
 MDV DATA ITEM IS DATA ITEM NO.:  8
0INDICES PASSED TO SUBROUTINE PRED:
   7   4   5   6   0   0   9   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID TIME AMT RATE EVID MDV CMT DV
0FORMAT FOR DATA:
 (E2.0,E3.0,E4.0,E5.0,E4.0,4E2.0,E11.0)

 TOT. NO. OF OBS RECS:     3750
 TOT. NO. OF INDIVIDUALS:    250
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  0  0  2
  0  0  0  2
  0  0  0  0  2
  0  0  0  0  0  3
  0  0  0  0  0  3  3
  0  0  0  0  0  0  0  4
  0  0  0  0  0  0  0  0  4
  0  0  0  0  0  0  0  0  0  4
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.2000E+01     0.1000E+07
 -0.1000E+07     0.2000E+01     0.1000E+07
  0.2000E+01     0.2000E+01     0.2000E+01
  0.1000E+01     0.1000E+01     0.1000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.3000E+00
                 -0.1000E-01   0.3000E+00
        2                                                                                   NO
                  0.1000E+00
        3                                                                                  YES
                  0.1400E+00
                  0.0000E+00   0.1250E+00
        4                                                                                  YES
                  0.1640E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(N)

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
   EVENT ID DATA ITEM IS DATA ITEM NO.:      7
   TIME DATA ITEM IS DATA ITEM NO.:          4
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   5
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     6
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    9

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 
 
 #TBLN:      1
 #METH: Iterative Two Stage (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            1224
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               ON 
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 EM OR BAYESIAN METHOD USED:              ITERATIVE TWO STAGE (ITS)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        5           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      10000       
 
 
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

 iteration            0 OBJ=  -5991.99597488104
 iteration            5 OBJ=  -14742.8326781999
 iteration           10 OBJ=  -17336.5620030450
 iteration           15 OBJ=  -19538.9621039237
 iteration           20 OBJ=  -19599.6299718035
 iteration           25 OBJ=  -19599.6298730771
 iteration           30 OBJ=  -19599.6298724800
 iteration           35 OBJ=  -19599.6298725132
 iteration           40 OBJ=  -19599.6298724886
 iteration           45 OBJ=  -19599.6298725229
 iteration           50 OBJ=  -19599.6298724671
 iteration           55 OBJ=  -19599.6298725072
 Convergence achieved
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.1353E-11 -2.6511E-11 -6.6951E-03  2.6073E-03  4.0878E-03
 SE:             2.3552E-02  2.1839E-02  6.6358E-03  6.6176E-03  6.8553E-03
 N:                     250         250         250         250         250
 
 P VAL.:         1.0000E+00  1.0000E+00  3.1301E-01  6.9359E-01  5.5098E-01
 
 ETAshrink(%):   1.3846E-01  2.1757E+00  1.8793E+01  1.9015E+01  1.6106E+01
 EBVshrink(%):   1.3845E-01  2.1757E+00  1.7839E+01  1.7936E+01  1.7858E+01
 EPSshrink(%):   1.4063E+01
 
 #TERE:
 Elapsed estimation time in seconds:    59.23
 Elapsed covariance time in seconds:     0.17
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -19599.630       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.68E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.40E-01
 
 ETA2
+       -8.07E-02  1.25E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.68E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.68E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.68E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.50E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        3.74E-01
 
 ETA2
+       -6.11E-01  3.54E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.29E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.29E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.29E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.40E-02  2.33E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.26E-02
 
 ETA2
+        1.11E-02  1.31E-02
 
 ETA3
+        0.00E+00  0.00E+00  1.14E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.14E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.14E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.46E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.69E-02
 
 ETA2
+        4.73E-02  1.86E-02
 
 ETA3
+       ......... .........  4.40E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.46E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.75E-04
 
 TH 2
+       -3.35E-04  5.43E-04
 
 OM11
+        2.41E-05 -6.18E-06  1.59E-04
 
 OM12
+       -9.86E-07 -4.25E-06 -1.03E-04  1.23E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -8.12E-07 -8.05E-06  7.46E-05 -1.08E-04  0.00E+00  0.00E+00  0.00E+00  1.72E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        2.87E-06 -4.34E-06  1.41E-06 -6.91E-07  0.00E+00  0.00E+00  0.00E+00  1.24E-06  0.00E+00  0.00E+00  0.00E+00  1.30E-06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -1.60E-08  2.56E-08  1.90E-08 -1.93E-08  0.00E+00  0.00E+00  0.00E+00 -2.56E-08  0.00E+00  0.00E+00  0.00E+00 -3.80E-09
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.17E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.40E-02
 
 TH 2
+       -6.00E-01  2.33E-02
 
 OM11
+        7.96E-02 -2.10E-02  1.26E-02
 
 OM12
+       -3.70E-03 -1.64E-02 -7.37E-01  1.11E-02
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.58E-03 -2.63E-02  4.50E-01 -7.39E-01  0.00E+00  0.00E+00  0.00E+00  1.31E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.05E-01 -1.64E-01  9.80E-02 -5.47E-02  0.00E+00  0.00E+00  0.00E+00  8.28E-02  0.00E+00  0.00E+00  0.00E+00  1.14E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -1.03E-02  1.70E-02  2.33E-02 -2.69E-02  0.00E+00  0.00E+00  0.00E+00 -3.02E-02  0.00E+00  0.00E+00  0.00E+00 -5.16E-02
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.46E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.75E+03
 
 TH 2
+        1.69E+03  2.95E+03
 
 OM11
+       -6.12E+02 -1.28E+02  1.47E+04
 
 OM12
+       -2.64E+02  3.95E+02  1.49E+04  3.32E+04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.93E+02  4.05E+02  3.01E+03  1.44E+04  0.00E+00  0.00E+00  0.00E+00  1.36E+04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -2.25E+01  2.00E+03 -3.28E+03 -3.25E+03  0.00E+00  0.00E+00  0.00E+00 -2.40E+03  0.00E+00  0.00E+00  0.00E+00  8.86E+04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        2.81E+03 -1.24E+03  1.02E+04  1.62E+05  0.00E+00  0.00E+00  0.00E+00  1.28E+05  0.00E+00  0.00E+00  0.00E+00  2.14E+05
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.42E+08
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.54E-01  3.84E-01  5.57E-01  9.14E-01  1.03E+00  1.65E+00  2.31E+00
 
1
 
 
 #TBLN:      2
 #METH: Stochastic Approximation Expectation-Maximization (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            1224
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               ON 
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 EM OR BAYESIAN METHOD USED:              STOCHASTIC APPROXIMATION EXPECTATION MAXIMIZATION (SAEM)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        10          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              30000       
 ITERATIONS (NITER):                      500         
 ANEAL SETTING (CONSTRAIN):               1           
 STARTING SEED FOR MC METHODS (SEED):     1556678     
 MC SAMPLES PER SUBJECT (ISAMPLE):        2           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.00000000        
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2           
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0           
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2           
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2           
 
 
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

 Stochastic/Burn-in Mode
 iteration       -30000 SAEMOBJ=  -29264.6110175287
 iteration       -29990 SAEMOBJ=  -28773.7669593234
 iteration       -29980 SAEMOBJ=  -28732.9438918005
 iteration       -29970 SAEMOBJ=  -28701.3022354221
 iteration       -29960 SAEMOBJ=  -28717.2894306732
 iteration       -29950 SAEMOBJ=  -28635.3219504119
 iteration       -29940 SAEMOBJ=  -28641.8934127419
 iteration       -29930 SAEMOBJ=  -28614.0615871756
 iteration       -29920 SAEMOBJ=  -28610.2417383087
 iteration       -29910 SAEMOBJ=  -28645.5298447517
 iteration       -29900 SAEMOBJ=  -28577.6903931726
 iteration       -29890 SAEMOBJ=  -28566.9565524259
 iteration       -29880 SAEMOBJ=  -28580.5327191145
 iteration       -29870 SAEMOBJ=  -28584.7534557700
 iteration       -29860 SAEMOBJ=  -28560.7913966679
 iteration       -29850 SAEMOBJ=  -28572.7526060277
 iteration       -29840 SAEMOBJ=  -28543.3763467907
 iteration       -29830 SAEMOBJ=  -28591.6719701504
 iteration       -29820 SAEMOBJ=  -28536.3975578827
 iteration       -29810 SAEMOBJ=  -28527.1228684484
 iteration       -29800 SAEMOBJ=  -28488.9366534976
 iteration       -29790 SAEMOBJ=  -28468.8596060907
 iteration       -29780 SAEMOBJ=  -28511.9772517606
 iteration       -29770 SAEMOBJ=  -28543.8533266546
 iteration       -29760 SAEMOBJ=  -28514.4844350363
 iteration       -29750 SAEMOBJ=  -28540.7877848401
 iteration       -29740 SAEMOBJ=  -28503.8514778227
 iteration       -29730 SAEMOBJ=  -28525.1311729791
 iteration       -29720 SAEMOBJ=  -28518.0789550310
 iteration       -29710 SAEMOBJ=  -28548.3330746997
 iteration       -29700 SAEMOBJ=  -28575.3067928927
 iteration       -29690 SAEMOBJ=  -28557.6779368684
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -28569.7901179494
 iteration           10 SAEMOBJ=  -28652.1046186455
 iteration           20 SAEMOBJ=  -28666.9780283389
 iteration           30 SAEMOBJ=  -28674.1962090063
 iteration           40 SAEMOBJ=  -28673.2278136695
 iteration           50 SAEMOBJ=  -28677.4126215343
 iteration           60 SAEMOBJ=  -28681.5024517154
 iteration           70 SAEMOBJ=  -28682.4040600463
 iteration           80 SAEMOBJ=  -28684.2186015053
 iteration           90 SAEMOBJ=  -28687.2128914978
 iteration          100 SAEMOBJ=  -28687.7111886087
 iteration          110 SAEMOBJ=  -28687.3807744445
 iteration          120 SAEMOBJ=  -28685.1494223316
 iteration          130 SAEMOBJ=  -28685.1905081236
 iteration          140 SAEMOBJ=  -28685.8610929533
 iteration          150 SAEMOBJ=  -28685.3871275448
 iteration          160 SAEMOBJ=  -28684.5703199506
 iteration          170 SAEMOBJ=  -28685.5183163065
 iteration          180 SAEMOBJ=  -28686.7542781907
 iteration          190 SAEMOBJ=  -28686.0732792207
 iteration          200 SAEMOBJ=  -28686.4707913196
 iteration          210 SAEMOBJ=  -28686.4487204670
 iteration          220 SAEMOBJ=  -28685.6199353548
 iteration          230 SAEMOBJ=  -28684.9648119129
 iteration          240 SAEMOBJ=  -28684.2859160484
 iteration          250 SAEMOBJ=  -28683.9568488990
 iteration          260 SAEMOBJ=  -28684.4470218327
 iteration          270 SAEMOBJ=  -28684.5580031448
 iteration          280 SAEMOBJ=  -28684.8936698915
 iteration          290 SAEMOBJ=  -28685.3328470807
 iteration          300 SAEMOBJ=  -28685.3106916991
 iteration          310 SAEMOBJ=  -28685.3082763815
 iteration          320 SAEMOBJ=  -28684.8347368002
 iteration          330 SAEMOBJ=  -28683.9308246131
 iteration          340 SAEMOBJ=  -28683.8312950224
 iteration          350 SAEMOBJ=  -28683.6133943109
 iteration          360 SAEMOBJ=  -28683.6154058065
 iteration          370 SAEMOBJ=  -28683.1943130468
 iteration          380 SAEMOBJ=  -28682.8506729419
 iteration          390 SAEMOBJ=  -28682.7438584385
 iteration          400 SAEMOBJ=  -28682.5619044400
 iteration          410 SAEMOBJ=  -28682.2571933884
 iteration          420 SAEMOBJ=  -28682.2313574378
 iteration          430 SAEMOBJ=  -28682.2054627461
 iteration          440 SAEMOBJ=  -28682.2423207919
 iteration          450 SAEMOBJ=  -28681.4819661420
 iteration          460 SAEMOBJ=  -28681.2963764690
 iteration          470 SAEMOBJ=  -28680.5396187904
 iteration          480 SAEMOBJ=  -28680.0593466986
 iteration          490 SAEMOBJ=  -28679.9880485839
 iteration          500 SAEMOBJ=  -28679.8861276746
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.8616E-06 -9.1659E-06 -1.0842E-02 -1.1700E-03  2.2271E-04
 SE:             2.3565E-02  2.2265E-02  6.7456E-03  6.9338E-03  7.1941E-03
 N:                     250         250         250         250         250
 
 P VAL.:         9.9994E-01  9.9967E-01  1.0801E-01  8.6600E-01  9.7530E-01
 
 ETAshrink(%):   1.3531E-01  1.2924E+00  1.4435E+01  1.2048E+01  8.7460E+00
 EBVshrink(%):   1.3517E-01  1.2925E+00  1.1522E+01  1.1641E+01  1.1555E+01
 EPSshrink(%):   1.3987E+01
 
 #TERE:
 Elapsed estimation time in seconds:   528.55
 Elapsed covariance time in seconds:     0.14
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -28679.886       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.69E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.40E-01
 
 ETA2
+       -8.17E-02  1.28E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.56E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.56E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.56E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.50E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        3.74E-01
 
 ETA2
+       -6.12E-01  3.57E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.25E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.25E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.25E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.39E-02  2.31E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.27E-02
 
 ETA2
+        1.11E-02  1.33E-02
 
 ETA3
+        0.00E+00  0.00E+00  9.56E-04
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  9.56E-04
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.56E-04
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.44E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.70E-02
 
 ETA2
+        4.63E-02  1.86E-02
 
 ETA3
+       ......... .........  3.83E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.43E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.71E-04
 
 TH 2
+       -3.32E-04  5.33E-04
 
 OM11
+        2.13E-05 -3.11E-06  1.61E-04
 
 OM12
+        1.95E-06 -4.22E-06 -1.04E-04  1.23E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.17E-06 -5.58E-06  7.57E-05 -1.09E-04  0.00E+00  0.00E+00  0.00E+00  1.77E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        9.21E-07 -2.04E-06  1.44E-06 -8.93E-07  0.00E+00  0.00E+00  0.00E+00  1.27E-06  0.00E+00  0.00E+00  0.00E+00  9.14E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -1.36E-08  2.75E-08  1.84E-08 -1.83E-08  0.00E+00  0.00E+00  0.00E+00 -2.27E-08  0.00E+00  0.00E+00  0.00E+00 -1.74E-09
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.14E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.39E-02
 
 TH 2
+       -6.01E-01  2.31E-02
 
 OM11
+        7.03E-02 -1.06E-02  1.27E-02
 
 OM12
+        7.37E-03 -1.65E-02 -7.38E-01  1.11E-02
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.31E-02 -1.82E-02  4.49E-01 -7.42E-01  0.00E+00  0.00E+00  0.00E+00  1.33E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        4.03E-02 -9.26E-02  1.18E-01 -8.43E-02  0.00E+00  0.00E+00  0.00E+00  1.00E-01  0.00E+00  0.00E+00  0.00E+00  9.56E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -8.85E-03  1.85E-02  2.25E-02 -2.56E-02  0.00E+00  0.00E+00  0.00E+00 -2.65E-02  0.00E+00  0.00E+00  0.00E+00 -2.83E-02
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.44E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.79E+03
 
 TH 2
+        1.73E+03  2.98E+03
 
 OM11
+       -6.98E+02 -2.46E+02  1.47E+04
 
 OM12
+       -4.43E+02  1.85E+02  1.50E+04  3.37E+04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.35E+02  3.17E+02  3.07E+03  1.45E+04  0.00E+00  0.00E+00  0.00E+00  1.34E+04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        5.14E+02  1.67E+03 -4.14E+03 -3.24E+03  0.00E+00  0.00E+00  0.00E+00 -2.85E+03  0.00E+00  0.00E+00  0.00E+00  1.25E+05
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        1.66E+02 -8.34E+03  1.22E+04  1.54E+05  0.00E+00  0.00E+00  0.00E+00  1.18E+05  0.00E+00  0.00E+00  0.00E+00  1.37E+05
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.43E+08
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************           STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION (NO PRIOR)         ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.52E-01  3.83E-01  5.59E-01  9.53E-01  1.02E+00  1.62E+00  2.32E+00
 
1
 
 
 #TBLN:      3
 #METH: Objective Function Evaluation by Importance Sampling (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            1224
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    10          
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   10          
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               ON 
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 EM OR BAYESIAN METHOD USED:              IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      4           
 STARTING SEED FOR MC METHODS (SEED):     1556678     
 MC SAMPLES PER SUBJECT (ISAMPLE):        3000        
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                YES
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   0.100000000000000       ,10.0000000000000        
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 T-DIST. PROPOSAL DENSITY (DF):           0           
 NO. ITERATIONS FOR MAP (MAPITER):        0           
 INTERVAL ITER. FOR MAP (MAPINTER):       0           
 
 
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

 iteration            0 OBJ=  -19581.4444168339 eff.=    4664. Smpl.=    3000. Fit.= 0.94886
 iteration            1 OBJ=  -19600.1945406633 eff.=     980. Smpl.=    3000. Fit.= 0.91637
 iteration            2 OBJ=  -19600.1347678379 eff.=    1071. Smpl.=    3000. Fit.= 0.92279
 iteration            3 OBJ=  -19599.9809090134 eff.=    1199. Smpl.=    3000. Fit.= 0.93031
 iteration            4 OBJ=  -19600.5068599462 eff.=    1203. Smpl.=    3000. Fit.= 0.93047
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.0377E-04 -3.7964E-03 -7.2104E-03  2.4817E-03  3.9228E-03
 SE:             2.3581E-02  2.1973E-02  6.6120E-03  6.6038E-03  6.8469E-03
 N:                     250         250         250         250         250
 
 P VAL.:         9.9649E-01  8.6283E-01  2.7549E-01  7.0706E-01  5.6669E-01
 
 ETAshrink(%):   6.6779E-02  2.5861E+00  1.6129E+01  1.6233E+01  1.3150E+01
 EBVshrink(%):   1.3813E-01  1.9974E+00  1.7998E+01  1.8101E+01  1.8024E+01
 EPSshrink(%):   1.4106E+01
 
 #TERE:
 Elapsed estimation time in seconds:   237.41
 Elapsed covariance time in seconds:    64.83
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -19600.507       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.69E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.40E-01
 
 ETA2
+       -8.17E-02  1.28E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.56E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.56E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.56E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.50E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        3.74E-01
 
 ETA2
+       -6.12E-01  3.57E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.25E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.25E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.25E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.37E-02  2.31E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.26E-02
 
 ETA2
+        1.01E-02  1.20E-02
 
 ETA3
+        0.00E+00  0.00E+00  9.46E-04
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  9.46E-04
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.46E-04
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.75E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.69E-02
 
 ETA2
+        4.13E-02  1.68E-02
 
 ETA3
+       ......... .........  3.79E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.74E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.61E-04
 
 TH 2
+       -3.26E-04  5.33E-04
 
 OM11
+        4.10E-07 -3.34E-07  1.60E-04
 
 OM12
+        2.05E-06 -1.16E-06 -9.31E-05  1.03E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.47E-06  4.38E-06  5.41E-05 -8.79E-05  0.00E+00  0.00E+00  0.00E+00  1.43E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        3.28E-08 -2.10E-08  3.36E-07 -2.82E-07  0.00E+00  0.00E+00  0.00E+00 -1.78E-09  0.00E+00  0.00E+00  0.00E+00  8.94E-07
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        4.58E-09  2.98E-09  9.12E-11 -1.06E-09  0.00E+00  0.00E+00  0.00E+00  5.05E-10  0.00E+00  0.00E+00  0.00E+00 -2.09E-10
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.55E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.37E-02
 
 TH 2
+       -5.97E-01  2.31E-02
 
 OM11
+        1.37E-03 -1.14E-03  1.26E-02
 
 OM12
+        8.53E-03 -4.96E-03 -7.27E-01  1.01E-02
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -8.71E-03  1.58E-02  3.57E-01 -7.25E-01  0.00E+00  0.00E+00  0.00E+00  1.20E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.47E-03 -9.64E-04  2.81E-02 -2.94E-02  0.00E+00  0.00E+00  0.00E+00 -1.57E-04  0.00E+00  0.00E+00  0.00E+00  9.46E-04
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        2.86E-03  1.91E-03  1.07E-04 -1.55E-03  0.00E+00  0.00E+00  0.00E+00  6.25E-04  0.00E+00  0.00E+00  0.00E+00 -3.28E-03
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.75E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.77E+03
 
 TH 2
+        1.70E+03  2.92E+03
 
 OM11
+       -8.14E+01 -4.67E+01  1.52E+04
 
 OM12
+       -1.84E+02 -1.67E+02  1.87E+04  4.36E+04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -8.62E+01 -1.45E+02  5.74E+03  1.97E+04  0.00E+00  0.00E+00  0.00E+00  1.69E+04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -3.05E+01 -1.01E+01  6.66E+01  2.28E+03  0.00E+00  0.00E+00  0.00E+00  1.37E+03  0.00E+00  0.00E+00  0.00E+00  1.26E+05
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -3.93E+03 -3.64E+03  3.55E+03  8.22E+03  0.00E+00  0.00E+00  0.00E+00  2.97E+03  0.00E+00  0.00E+00  0.00E+00  1.78E+04
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.20E+08
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************         OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING (NO PRIOR)        ********************
 ********************                    EIGENVALUES OF COR MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.37E-01  4.03E-01  6.42E-01  9.97E-01  1.00E+00  1.60E+00  2.22E+00
 
1
 
 
 #TBLN:      4
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            1224
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    10          
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   10          
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               OFF
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 EM OR BAYESIAN METHOD USED:              MCMC BAYESIAN (BAYES)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        100         
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              10000       
 ITERATIONS (NITER):                      10000       
 STARTING SEED FOR MC METHODS (SEED):     1556678     
 MC SAMPLES PER SUBJECT (ISAMPLE):        1           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.00000000        
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2           
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0           
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2           
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2           
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
 THETAS THAT ARE GIBBS SAMPLED:
   1   2
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration       -10000 MCMCOBJ=   -29268.8398724508     
 iteration        -9900 MCMCOBJ=   -28418.8832485366     
 iteration        -9800 MCMCOBJ=   -28348.3975164044     
 iteration        -9700 MCMCOBJ=   -28248.7889015873     
 iteration        -9600 MCMCOBJ=   -28318.9694072797     
 iteration        -9500 MCMCOBJ=   -28321.6055265830     
 iteration        -9400 MCMCOBJ=   -28319.9673692575     
 iteration        -9300 MCMCOBJ=   -28445.4895822666     
 iteration        -9200 MCMCOBJ=   -28217.2126749103     
 iteration        -9100 MCMCOBJ=   -28295.5548180671     
 iteration        -9000 MCMCOBJ=   -28282.5051801655     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -28284.1082468941     
 iteration          100 MCMCOBJ=   -28255.3766502811     
 iteration          200 MCMCOBJ=   -28223.9079981327     
 iteration          300 MCMCOBJ=   -28164.3978944046     
 iteration          400 MCMCOBJ=   -28252.6795088944     
 iteration          500 MCMCOBJ=   -28304.5857154756     
 iteration          600 MCMCOBJ=   -28265.8578744294     
 iteration          700 MCMCOBJ=   -28175.7851422749     
 iteration          800 MCMCOBJ=   -28222.4550793718     
 iteration          900 MCMCOBJ=   -28197.9562494887     
 iteration         1000 MCMCOBJ=   -28351.0139701480     
 iteration         1100 MCMCOBJ=   -28277.5161191938     
 iteration         1200 MCMCOBJ=   -28268.0262469663     
 iteration         1300 MCMCOBJ=   -28296.5994314237     
 iteration         1400 MCMCOBJ=   -28247.5733729279     
 iteration         1500 MCMCOBJ=   -28236.4855268075     
 iteration         1600 MCMCOBJ=   -28219.3292089615     
 iteration         1700 MCMCOBJ=   -28244.3933538621     
 iteration         1800 MCMCOBJ=   -28347.5098089049     
 iteration         1900 MCMCOBJ=   -28214.0709378361     
 iteration         2000 MCMCOBJ=   -28163.3526372840     
 iteration         2100 MCMCOBJ=   -28195.4988850024     
 iteration         2200 MCMCOBJ=   -28188.2256225221     
 iteration         2300 MCMCOBJ=   -28257.3461686281     
 iteration         2400 MCMCOBJ=   -28268.4526744336     
 iteration         2500 MCMCOBJ=   -28154.2353712698     
 iteration         2600 MCMCOBJ=   -28303.1367564865     
 iteration         2700 MCMCOBJ=   -28186.5014178192     
 iteration         2800 MCMCOBJ=   -28250.4619304406     
 iteration         2900 MCMCOBJ=   -28259.8539356668     
 iteration         3000 MCMCOBJ=   -28112.6452854179     
 iteration         3100 MCMCOBJ=   -28181.1797999096     
 iteration         3200 MCMCOBJ=   -28216.8822502775     
 iteration         3300 MCMCOBJ=   -28158.1974152559     
 iteration         3400 MCMCOBJ=   -28210.4317609486     
 iteration         3500 MCMCOBJ=   -28279.7894193843     
 iteration         3600 MCMCOBJ=   -28215.5586672753     
 iteration         3700 MCMCOBJ=   -28155.2651758274     
 iteration         3800 MCMCOBJ=   -28211.9469472080     
 iteration         3900 MCMCOBJ=   -28101.4278018716     
 iteration         4000 MCMCOBJ=   -28259.7053968685     
 iteration         4100 MCMCOBJ=   -28162.4760063297     
 iteration         4200 MCMCOBJ=   -28328.9646124579     
 iteration         4300 MCMCOBJ=   -28246.5678988550     
 iteration         4400 MCMCOBJ=   -28135.8765295073     
 iteration         4500 MCMCOBJ=   -28148.2444318093     
 iteration         4600 MCMCOBJ=   -28175.8585624199     
 iteration         4700 MCMCOBJ=   -28267.4428269807     
 iteration         4800 MCMCOBJ=   -28194.1458790149     
 iteration         4900 MCMCOBJ=   -28181.5410703955     
 iteration         5000 MCMCOBJ=   -28272.5793536589     
 iteration         5100 MCMCOBJ=   -28204.1087221423     
 iteration         5200 MCMCOBJ=   -28230.5277212051     
 iteration         5300 MCMCOBJ=   -28311.9515653966     
 iteration         5400 MCMCOBJ=   -28167.3596451905     
 iteration         5500 MCMCOBJ=   -28219.1070713405     
 iteration         5600 MCMCOBJ=   -28246.7629833778     
 iteration         5700 MCMCOBJ=   -28259.0698735920     
 iteration         5800 MCMCOBJ=   -28158.8840165601     
 iteration         5900 MCMCOBJ=   -28242.8625603850     
 iteration         6000 MCMCOBJ=   -28241.1314821819     
 iteration         6100 MCMCOBJ=   -28188.9897784366     
 iteration         6200 MCMCOBJ=   -28212.9176102254     
 iteration         6300 MCMCOBJ=   -28109.7729890833     
 iteration         6400 MCMCOBJ=   -28253.0953506320     
 iteration         6500 MCMCOBJ=   -28247.7253549811     
 iteration         6600 MCMCOBJ=   -28195.7801188675     
 iteration         6700 MCMCOBJ=   -28253.4982547588     
 iteration         6800 MCMCOBJ=   -28205.9384045367     
 iteration         6900 MCMCOBJ=   -28234.0917783397     
 iteration         7000 MCMCOBJ=   -28119.0346813158     
 iteration         7100 MCMCOBJ=   -28102.1104397892     
 iteration         7200 MCMCOBJ=   -28250.6880703852     
 iteration         7300 MCMCOBJ=   -28142.7886876896     
 iteration         7400 MCMCOBJ=   -28288.7205287867     
 iteration         7500 MCMCOBJ=   -28174.7302481519     
 iteration         7600 MCMCOBJ=   -28144.6911602503     
 iteration         7700 MCMCOBJ=   -28210.0795960388     
 iteration         7800 MCMCOBJ=   -28283.0917672663     
 iteration         7900 MCMCOBJ=   -28221.3439808250     
 iteration         8000 MCMCOBJ=   -28304.8104024008     
 iteration         8100 MCMCOBJ=   -28314.3462629746     
 iteration         8200 MCMCOBJ=   -28281.8179816705     
 iteration         8300 MCMCOBJ=   -28266.2012367870     
 iteration         8400 MCMCOBJ=   -28213.2663506859     
 iteration         8500 MCMCOBJ=   -28196.0246030724     
 iteration         8600 MCMCOBJ=   -28144.8396102254     
 iteration         8700 MCMCOBJ=   -28197.5944319301     
 iteration         8800 MCMCOBJ=   -28217.6632417925     
 iteration         8900 MCMCOBJ=   -28190.9739823079     
 iteration         9000 MCMCOBJ=   -28148.5753603581     
 iteration         9100 MCMCOBJ=   -28244.6237685402     
 iteration         9200 MCMCOBJ=   -28290.7334662713     
 iteration         9300 MCMCOBJ=   -28194.0289418538     
 iteration         9400 MCMCOBJ=   -28236.6605534448     
 iteration         9500 MCMCOBJ=   -28327.0957072040     
 iteration         9600 MCMCOBJ=   -28213.5080365079     
 iteration         9700 MCMCOBJ=   -28238.4670134792     
 iteration         9800 MCMCOBJ=   -28146.0728268205     
 iteration         9900 MCMCOBJ=   -28111.5708366939     
 iteration        10000 MCMCOBJ=   -28277.9973939090     
 
 #TERM:
 BURN-IN WAS COMPLETED
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation time in seconds:  3355.38
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -28227.509       **************************************************
 #OBJS:********************************************       56.157 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.68E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.41E-01
 
 ETA2
+       -8.13E-02  1.26E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.72E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.72E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.72E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.51E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        3.76E-01
 
 ETA2
+       -6.08E-01  3.55E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.31E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.31E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.31E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.01E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.39E-02  2.29E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.27E-02
 
 ETA2
+        1.00E-02  1.17E-02
 
 ETA3
+        0.00E+00  0.00E+00  1.04E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.04E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.04E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.74E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.68E-02
 
 ETA2
+        4.13E-02  1.65E-02
 
 ETA3
+        0.00E+00  0.00E+00  3.96E-03
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  3.96E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.96E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.73E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.70E-04
 
 TH 2
+       -3.26E-04  5.23E-04
 
 OM11
+       -2.43E-06 -2.41E-07  1.61E-04
 
 OM12
+        2.12E-06  3.77E-06 -9.19E-05  9.99E-05
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.10E-06 -4.93E-06  5.33E-05 -8.37E-05  0.00E+00  0.00E+00  0.00E+00  1.38E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -3.11E-07  1.22E-07  4.88E-08 -2.61E-07  0.00E+00  0.00E+00  0.00E+00  1.99E-10  0.00E+00  0.00E+00  0.00E+00  1.08E-06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        3.30E-08 -2.97E-08 -1.02E-08 -3.68E-09  0.00E+00  0.00E+00  0.00E+00  2.32E-09  0.00E+00  0.00E+00  0.00E+00  2.98E-10
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.55E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.39E-02
 
 TH 2
+       -5.97E-01  2.29E-02
 
 OM11
+       -8.03E-03 -8.31E-04  1.27E-02
 
 OM12
+        8.89E-03  1.65E-02 -7.25E-01  1.00E-02
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -7.47E-03 -1.84E-02  3.58E-01 -7.13E-01  0.00E+00  0.00E+00  0.00E+00  1.17E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -1.25E-02  5.10E-03  3.70E-03 -2.50E-02  0.00E+00  0.00E+00  0.00E+00  1.63E-05  0.00E+00  0.00E+00  0.00E+00  1.04E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        2.05E-02 -1.92E-02 -1.20E-02 -5.46E-03  0.00E+00  0.00E+00  0.00E+00  2.93E-03  0.00E+00  0.00E+00  0.00E+00  4.24E-03
          0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  6.74E-05
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           EIGENVALUES OF COR MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.46E-01  4.03E-01  6.42E-01  9.95E-01  1.00E+00  1.60E+00  2.21E+00
 
1
 
 
 #TBLN:      5
 #METH: First Order Conditional Estimation with Interaction (No Prior)
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
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
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    10          
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   10          
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               ON 
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        ON 
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS 
       IN SHRINKAGE (ETASTYPE):           NO 
 NON-INFL. ETA CORRECTION (NONINFETA):    OFF
 FORMAT FOR ADDITIONAL FILES (FORMAT):    S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):     TSOL
 ADDITIONAL CONVERGENCE TEST (CTYPE=4)?:  NO
 EM OR BAYESIAN METHOD USED:                NONE
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -19599.3320235605        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.8943E+00  3.6802E+00  1.4147E-01 -8.1332E-02  1.2616E-01  1.7228E-02  2.5083E-03
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   1.2410E+04  1.5849E+04  9.2098E+00  1.7573E+01  8.0008E+00  2.9307E+01  9.6695E+00
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -19599.6170478955        NO. OF FUNC. EVALS.:  12
 CUMULATIVE NO. OF FUNC. EVALS.:       78
 NPARAMETR:  3.8947E+00  3.6814E+00  1.3922E-01 -8.1216E-02  1.2520E-01  1.6665E-02  2.4994E-03
 PARAMETER:  1.0001E-01  1.0003E-01  9.1988E-02 -1.0066E-01  8.9984E-02  8.3383E-02  9.8223E-02
 GRADIENT:  -1.0268E+04  1.2055E+04 -2.6088E+00 -2.2013E+01 -1.5188E+00 -3.1439E+00 -1.0422E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -19599.6305778834        NO. OF FUNC. EVALS.:  25
 CUMULATIVE NO. OF FUNC. EVALS.:      179             RESET HESSIAN, TYPE I
 NPARAMETR:  3.8945E+00  3.6818E+00  1.3943E-01 -8.0665E-02  1.2475E-01  1.6717E-02  2.5085E-03
 PARAMETER:  1.0001E-01  1.0004E-01  9.2757E-02 -9.9900E-02  9.1639E-02  8.4949E-02  1.0005E-01
 GRADIENT:   1.2670E+04  1.6229E+04  4.7470E-01  1.2145E+00  7.6586E-03  3.9983E-02  9.6812E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -19599.6308785734        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:      253
 NPARAMETR:  3.8946E+00  3.6818E+00  1.3933E-01 -8.0670E-02  1.2479E-01  1.6716E-02  2.4999E-03
 PARAMETER:  1.0001E-01  1.0004E-01  9.2383E-02 -9.9944E-02  9.1630E-02  8.4928E-02  9.8317E-02
 GRADIENT:  -1.0243E+04  1.2117E+04 -1.2788E-01 -1.0621E+00 -1.0366E-02 -3.9160E-02 -9.3382E+00
 
0ITERATION NO.:   19    OBJECTIVE VALUE:  -19599.6348876118        NO. OF FUNC. EVALS.:  20
 CUMULATIVE NO. OF FUNC. EVALS.:      329
 NPARAMETR:  3.8946E+00  3.6818E+00  1.3934E-01 -8.0658E-02  1.2477E-01  1.6717E-02  2.5040E-03
 PARAMETER:  1.0001E-01  1.0004E-01  9.2433E-02 -9.9925E-02  9.1635E-02  8.4938E-02  9.9144E-02
 GRADIENT:   1.6095E+01 -1.0542E+01 -6.4417E-03 -3.7501E-01 -1.1355E-03 -1.2805E-03 -2.3957E-01
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.
 REGARD THE RESULTS OF THE ESTIMATION STEP CAREFULLY, AND ACCEPT THEM ONLY
 AFTER CHECKING THAT THE COVARIANCE STEP PRODUCES REASONABLE OUTPUT.
 NO. OF FUNCTION EVALUATIONS USED:      329
 NO. OF SIG. DIGITS IN FINAL EST.:  3.7

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -7.3918E-04  2.0507E-04 -6.7099E-03  2.5910E-03  4.0713E-03
 SE:             2.3551E-02  2.1838E-02  6.6359E-03  6.6176E-03  6.8554E-03
 N:                     250         250         250         250         250
 
 P VAL.:         9.7496E-01  9.9251E-01  3.1194E-01  6.9541E-01  5.5259E-01
 
 ETAshrink(%):   4.1523E-02  2.0498E+00  1.8686E+01  1.8911E+01  1.5997E+01
 EBVshrink(%):   1.3873E-01  2.1758E+00  1.7839E+01  1.7937E+01  1.7858E+01
 EPSshrink(%):   1.4065E+01
 
 #TERE:
 Elapsed estimation time in seconds:   210.87
 Elapsed covariance time in seconds:    31.75
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -19599.635       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         3.89E+00  3.68E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.39E-01
 
 ETA2
+       -8.07E-02  1.25E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.67E-02
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.67E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.67E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.50E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        3.73E-01
 
 ETA2
+       -6.12E-01  3.53E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.29E-01
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.29E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.29E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.00E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.36E-02  2.29E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.25E-02
 
 ETA2
+        9.95E-03  1.17E-02
 
 ETA3
+       ......... .........  1.08E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        6.76E-05
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5   
 
 ETA1
+        1.67E-02
 
 ETA2
+        4.12E-02  1.65E-02
 
 ETA3
+       ......... .........  4.16E-03
 
 ETA4
+       ......... ......... ......... .........
 
 ETA5
+       ......... ......... ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.75E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        5.59E-04
 
 TH 2
+       -3.22E-04  5.22E-04
 
 OM11
+        3.41E-07 -3.51E-07  1.56E-04
 
 OM12
+       -3.31E-07  2.53E-07 -9.00E-05  9.90E-05
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        3.17E-07 -1.46E-07  5.19E-05 -8.41E-05 ......... ......... .........  1.37E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        2.89E-08 -8.97E-09  1.62E-08 -1.33E-08 ......... ......... ......... -3.76E-07 ......... ......... .........  1.16E-06
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        4.39E-09  4.92E-09 -6.25E-10 -3.23E-10 ......... ......... ......... -3.49E-10 ......... ......... ......... -3.24E-10
         ......... ......... ......... ......... .........  4.56E-09
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.36E-02
 
 TH 2
+       -5.96E-01  2.29E-02
 
 OM11
+        1.15E-03 -1.23E-03  1.25E-02
 
 OM12
+       -1.41E-03  1.11E-03 -7.24E-01  9.95E-03
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.15E-03 -5.45E-04  3.55E-01 -7.24E-01 ......... ......... .........  1.17E-02
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+        1.14E-03 -3.65E-04  1.20E-03 -1.24E-03 ......... ......... ......... -2.99E-02 ......... ......... .........  1.08E-03
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+        2.75E-03  3.19E-03 -7.40E-04 -4.81E-04 ......... ......... ......... -4.42E-04 ......... ......... ......... -4.46E-03
         ......... ......... ......... ......... .........  6.76E-05
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM22      OM23      OM24      OM25      OM33  
             OM34      OM35      OM44      OM45      OM55      SG11  
 
 TH 1
+        2.77E+03
 
 TH 2
+        1.71E+03  2.97E+03
 
 OM11
+       -2.88E-02  2.76E+00  1.54E+04
 
 OM12
+        1.66E+00 -2.14E+00  1.90E+04  4.46E+04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -3.76E+00 -3.24E+00  5.85E+03  2.03E+04 ......... ......... .........  1.76E+04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       -5.86E+01 -2.22E+01  1.90E+03  6.85E+03 ......... ......... .........  5.88E+03 ......... ......... .........  8.67E+05
 
 OM34
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... .........
 
 OM44
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... .........
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM55
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 SG11
+       -4.52E+03 -4.85E+03  4.03E+03  7.80E+03 ......... ......... .........  4.01E+03 ......... ......... .........  6.28E+04
         ......... ......... ......... ......... .........  2.19E+08
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************          FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION (NO PRIOR)        ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7
 
         1.38E-01  4.04E-01  6.44E-01  9.96E-01  1.01E+00  1.60E+00  2.22E+00
 
 #CPUT: Total CPU Time in Seconds,     4459.359
Stop Time: 
Sat 09/07/2013 
03:10 PM

Thu 06/18/2015 
07:20 AM
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
include nonmem_reserved_general
; Request extra information for Bayesian analysis.  An extra call will then be made
; for accepted samples
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
; When Bayes_extra=1, then this particular set of individual parameters were "accepted"
; So you may record them if you wish
IF(BAYES_EXTRA==1 .AND. NEWIND/=2) THEN
;IF(BAYES_EXTRA==1 .AND. ITER_REPORT>=0 .AND. TIME==0.0) THEN
" WRITE(50,'(I12,1X,F14.0,9(1X,1PG12.5))') ITER_REPORT,ID,VC,K10,K12,K21,VM,KMC,K03,K30,OBJI(NIREC,1)
ENDIF

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

;$EST METHOD=BAYES INTERACTION NBURN=4000 SIGL=4 NITER=10000 PRINT=25 CTYPE=3 NOABORT NOPRIOR=0
$EST METHOD=ITS INTERACTION SIGL=4 NITER=15 PRINT=1 NOABORT NOPRIOR=1
$EST METHOD=bayes INTERACTION NBURN=2000 NITER=0 PRINT=10 MASSRESET=1 NOPRIOR=0
$EST METHOD=NUTS INTERACTION  NBURN=1000 NITER=2000 PRINT=1 MASSRESET=0 OLKJDF=8.0
$COV MATRIX=R UNCONDITIONAL

  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       18 JUN 2015
Days until program expires :5459
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha6 (nm74a6)
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
 GRADIENT METHOD USED:     NOSL
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha6 (nm74a6)

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
 RAW OUTPUT FILE (FILE): example6hmto.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
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
 iteration            1 OBJ=  -3598.21506830627
 iteration            2 OBJ=  -3711.98700501726
 iteration            3 OBJ=  -3819.33073577038
 iteration            4 OBJ=  -3923.73804275991
 iteration            5 OBJ=  -4026.28952182628
 iteration            6 OBJ=  -4127.36429157082
 iteration            7 OBJ=  -4226.82041883717
 iteration            8 OBJ=  -4324.32301567158
 iteration            9 OBJ=  -4419.00122776895
 iteration           10 OBJ=  -4509.14657828136
 iteration           11 OBJ=  -4591.53257509083
 iteration           12 OBJ=  -4659.21605755075
 iteration           13 OBJ=  -4699.40003638566
 iteration           14 OBJ=  -4708.74095490496
 iteration           15 OBJ=  -4709.83819602782
 
 #TERM:
 OPTIMIZATION WAS NOT TESTED FOR CONVERGENCE


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -8.3154E-04 -3.0221E-03  2.3761E-03  1.4182E-03  1.4589E-03  2.0930E-03  5.5082E-04  1.1400E-03
 SE:             6.9303E-02  5.2691E-02  3.7772E-02  6.5151E-02  5.6769E-02  5.7216E-02  6.4186E-02  6.1426E-02
 N:                      50          50          50          50          50          50          50          50
 
 P VAL.:         9.9043E-01  9.5426E-01  9.4984E-01  9.8263E-01  9.7950E-01  9.7082E-01  9.9315E-01  9.8519E-01
 
 ETAshrink(%):   6.3693E-01  4.2179E+00  8.0369E+00  1.6147E+00  1.4914E+00  5.7569E+00  3.5184E-01  1.5657E+00
 EBVshrink(%):   6.3160E-01  5.4601E+00  9.8632E+00  2.1011E+00  1.5628E+00  6.3046E+00  4.5528E-01  1.7656E+00
 EPSshrink(%):   1.5671E+01  7.2294E+00
 
 #TERE:
 Elapsed estimation  time in seconds:    48.69
 Elapsed covariance  time in seconds:     0.23
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          ITERATIVE TWO STAGE (NO PRIOR)                        ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -4709.838       **************************************************
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
+        2.89E-02 -3.37E-02  3.17E-02 -7.20E-02  2.37E-02  3.39E-03  2.12E-01
 
 ETA8
+        9.78E-02  8.19E-02  3.48E-02  4.44E-02  1.08E-03 -5.09E-02  5.51E-02  1.99E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.27E-03
 
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
 
         3.48E-01  2.25E-01  2.30E-01  3.11E-01  2.16E-01  2.65E-01  1.50E-01  3.84E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        1.50E-01
 
 ETA2
+        2.24E-01  2.80E-01
 
 ETA3
+        1.05E-01  1.23E-01  1.82E-01
 
 ETA4
+        8.68E-02  1.31E-01  1.04E-01  1.68E-01
 
 ETA5
+        1.40E-01  9.08E-02  8.63E-02  1.66E-01  3.09E-01
 
 ETA6
+        1.12E-01  1.62E-01  1.34E-01  1.53E-01  8.28E-02  1.55E-01
 
 ETA7
+        1.61E-01  1.06E-01  1.19E-01  9.44E-02  1.02E-01  1.03E-01  1.45E-01
 
 ETA8
+        2.00E-01  1.48E-01  5.91E-02  9.29E-02  1.42E-01  2.01E-01  1.29E-01  2.64E-01
 


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
+        1.04E+00  3.57E-01
 
 ETA3
+        7.08E-01  9.48E-01  3.09E-01
 
 ETA4
+        3.55E-01  6.30E-01  8.50E-01  1.78E-01
 
 ETA5
+        7.04E-01  5.66E-01  7.14E-01  9.45E-01  3.75E-01
 
 ETA6
+        5.44E-01  9.42E-01  1.03E+00  7.32E-01  8.18E-01  1.79E-01
 
 ETA7
+        6.86E-01  5.38E-01  9.70E-01  4.10E-01  5.98E-01  5.21E-01  1.58E-01
 
 ETA8
+        5.90E-01  7.45E-01  3.39E-01  3.62E-01  7.80E-01  9.14E-01  5.67E-01  2.96E-01
 


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
+        1.21E-01
 
 TH 2
+       -3.28E-02  5.05E-02
 
 TH 3
+        3.02E-02 -1.47E-02  5.28E-02
 
 TH 4
+        8.08E-02 -2.54E-02 -2.73E-03  9.70E-02
 
 TH 5
+       -5.48E-02  2.66E-02 -1.81E-03 -4.99E-02  4.66E-02
 
 TH 6
+       -6.01E-02  2.13E-02 -1.84E-02 -3.83E-02  3.28E-02  7.05E-02
 
 TH 7
+       -1.49E-02 -4.14E-03  9.28E-03 -2.22E-02  1.16E-02  1.07E-02  2.26E-02
 
 TH 8
+        1.05E-01 -4.07E-02  1.89E-02  9.45E-02 -5.98E-02 -5.62E-02 -1.04E-02  1.47E-01
 
 OM11
+       -2.99E-02  5.72E-03 -2.07E-02 -9.54E-03  1.22E-02  1.54E-02  4.46E-04 -1.87E-02  2.25E-02
 
 OM12
+       -2.20E-02  3.10E-02  1.34E-03 -2.86E-02  2.30E-02  1.74E-02 -3.09E-03 -5.55E-02 -3.89E-03  5.01E-02
 
 OM13
+       -1.07E-02 -8.85E-03  2.96E-03 -7.82E-03  3.38E-03  2.96E-03  4.58E-03  1.17E-04  6.76E-03 -1.33E-02  1.11E-02
 
 OM14
+       -8.04E-03 -3.25E-03  3.54E-03 -1.22E-02  3.89E-03 -3.46E-03  6.75E-03 -8.59E-03  5.44E-04 -3.44E-03  3.21E-03  7.54E-03
 
 OM15
+        1.21E-02 -5.83E-03 -3.98E-03  1.66E-02 -6.68E-03  7.76E-03 -6.57E-03  9.56E-03  3.97E-04  1.14E-03 -1.62E-03 -8.40E-03
          1.96E-02
 
 OM16
+       -1.70E-02  1.44E-02 -1.43E-03 -1.77E-02  1.30E-02  5.34E-03  2.03E-03 -1.49E-02  3.06E-03  8.83E-03 -3.24E-04  1.94E-03
         -8.99E-03  1.26E-02
 
 OM17
+        3.60E-03 -2.84E-03 -2.13E-02  2.44E-02 -7.38E-03  6.44E-04 -7.55E-03  2.52E-02  1.40E-02 -1.77E-02  3.18E-03 -4.96E-03
          6.45E-03 -2.90E-03  2.59E-02
 
 OM18
+       -3.04E-02  1.74E-02 -3.52E-02 -8.28E-04  1.26E-02  2.78E-02 -5.68E-03 -1.95E-02  2.17E-02  7.35E-03 -1.99E-03 -5.87E-03
          7.20E-03  1.85E-03  2.14E-02  4.02E-02
 
 OM22
+       -2.69E-02 -2.26E-02 -3.46E-02  2.04E-03 -8.99E-03  1.78E-02  2.81E-03  1.33E-02  2.05E-02 -4.26E-02  1.56E-02  1.78E-03
          4.80E-03 -7.46E-03  2.33E-02  1.75E-02  7.86E-02
 
 OM23
+        6.37E-03  5.77E-03  1.46E-02 -1.04E-02  5.26E-03 -5.75E-03  3.93E-03 -1.20E-02 -9.59E-03  1.47E-02 -6.48E-03  2.71E-03
         -5.76E-03  2.95E-03 -1.33E-02 -1.08E-02 -2.62E-02  1.52E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -1.22E-02 -3.01E-03 -1.34E-02  2.18E-03 -1.17E-03  1.33E-02 -5.37E-03 -6.29E-03  6.03E-03  2.40E-03  2.10E-04 -6.02E-03
          1.18E-02 -3.81E-03  6.29E-03  1.29E-02  1.66E-02 -8.98E-03  1.72E-02
 
 OM25
+        3.08E-03  3.98E-05 -1.10E-03 -1.82E-03 -8.73E-04 -2.39E-03  3.13E-03  1.87E-03  6.82E-04 -4.35E-03  4.61E-04  2.54E-03
         -4.58E-03  1.92E-03 -2.24E-04 -3.83E-03  1.87E-03  2.36E-03 -6.36E-03  8.25E-03
 
 OM26
+       -2.40E-02  2.37E-02 -1.94E-02 -1.38E-02  1.67E-02  2.70E-02 -3.00E-03 -2.84E-02  9.45E-03  1.98E-02 -6.16E-03 -6.13E-03
          4.86E-03  7.23E-03  3.90E-03  2.18E-02 -2.72E-03 -2.92E-06  8.22E-03 -1.77E-03  2.63E-02
 
 OM27
+       -1.38E-02  1.68E-02 -3.58E-03 -1.46E-02  1.36E-02  5.95E-03  1.24E-03 -2.38E-02  3.39E-03  1.51E-02 -3.78E-03  6.84E-04
         -5.01E-03  6.98E-03 -3.33E-03  5.75E-03 -1.47E-02  6.26E-03 -4.29E-03  2.12E-03  9.64E-03  1.12E-02
 
 OM28
+       -3.77E-02  1.66E-02 -1.73E-02 -2.70E-02  1.80E-02  1.95E-02  6.07E-04 -4.73E-02  1.14E-02  1.94E-02 -1.17E-03  2.02E-04
         -6.01E-04  6.34E-03 -2.80E-03  1.37E-02  2.70E-03 -3.85E-04  7.42E-03 -1.07E-05  1.41E-02  8.61E-03  2.18E-02
 
 OM33
+        4.46E-02 -2.58E-02  2.15E-03  3.99E-02 -3.14E-02 -2.47E-02 -6.37E-03  6.19E-02 -6.42E-03 -3.12E-02  2.69E-03 -3.07E-03
          6.93E-03 -1.05E-02  1.28E-02 -8.25E-03  2.02E-02 -8.67E-03  9.93E-04  9.31E-04 -1.44E-02 -1.39E-02 -1.99E-02  3.30E-02
 
 OM34
+        1.34E-02 -1.38E-02  8.78E-03  1.08E-02 -8.66E-03 -1.57E-02  3.90E-03  2.24E-02 -2.64E-03 -1.57E-02  4.33E-03  3.96E-03
         -4.72E-03 -2.28E-03  1.67E-03 -1.05E-02  4.19E-03 -3.44E-04 -6.41E-03  3.22E-03 -1.29E-02 -4.41E-03 -9.32E-03  9.70E-03
         1.08E-02
 
 OM35
+        1.37E-04  5.66E-03 -8.69E-03  3.21E-03 -4.37E-04  9.47E-03 -4.30E-03 -4.66E-03  1.02E-03  4.14E-03 -3.28E-03 -3.48E-03
          6.75E-03 -2.65E-03  2.97E-03  8.64E-03  2.57E-03 -1.62E-03  4.42E-03 -2.12E-03  6.96E-03  6.42E-05  2.10E-03 -2.38E-04
        -6.27E-03  7.45E-03
 
 OM36
+        1.47E-02  2.93E-03 -1.26E-02  1.55E-02 -1.26E-02 -1.55E-02 -1.13E-02  1.37E-02  1.13E-04  1.45E-03 -6.48E-03 -4.33E-03
          2.55E-03  1.71E-03  7.02E-03  4.57E-03 -9.91E-04 -1.11E-03  3.92E-03  3.47E-04  5.09E-03  1.29E-03  1.76E-03  7.30E-03
        -1.69E-03  1.03E-03  1.80E-02
 
 OM37
+       -2.24E-02  1.25E-02 -1.80E-02 -9.99E-03  7.74E-03  1.54E-02 -4.87E-03 -2.29E-02  1.04E-02  9.12E-03 -1.22E-04 -3.41E-03
          4.95E-03  2.92E-03  4.66E-03  1.57E-02  8.27E-03 -6.37E-03  9.60E-03 -3.27E-03  1.30E-02  3.28E-03  1.28E-02 -7.96E-03
        -9.39E-03  5.41E-03  4.20E-03  1.41E-02
 
 OM38
+        9.69E-03 -7.88E-03  1.37E-03  8.22E-03 -6.81E-03 -3.29E-03 -8.46E-04  1.39E-02 -3.51E-04 -9.06E-03  2.60E-03 -7.31E-04
          2.97E-03 -2.99E-03  3.58E-03 -1.62E-03  6.68E-03 -2.97E-03  5.32E-04 -2.81E-06 -3.31E-03 -3.67E-03 -5.00E-03  8.43E-03
         2.01E-03  6.55E-04  1.60E-04 -9.58E-04  3.50E-03
 
 OM44
+       -2.48E-02 -1.01E-02  1.40E-02 -2.49E-02  1.13E-02  3.57E-03  8.76E-03 -1.98E-02  3.43E-03 -2.59E-03  1.11E-02  5.28E-03
         -2.15E-03 -5.09E-04 -7.32E-03 -8.29E-03  6.69E-03 -1.74E-03  3.82E-03 -3.23E-03 -9.18E-03 -3.07E-03  4.26E-03 -6.85E-03
         3.20E-03 -6.67E-03 -9.77E-03 -5.71E-04 -6.58E-04  2.83E-02
 
 OM45
+       -2.25E-02  2.41E-02 -1.27E-02 -1.77E-02  1.75E-02  2.66E-02 -1.94E-03 -3.98E-02  4.73E-03  2.81E-02 -7.13E-03 -3.80E-03
          4.43E-03  5.51E-03 -5.06E-03  1.53E-02 -1.41E-02  3.47E-03  4.85E-03 -1.77E-03  2.19E-02  1.03E-02  1.52E-02 -2.02E-02
        -1.43E-02  7.79E-03  7.07E-05  1.24E-02 -4.10E-03 -8.24E-03  2.75E-02
 
 OM46
+       -1.95E-02 -5.60E-03  8.70E-05 -1.56E-02  8.49E-03  1.27E-02  1.28E-02 -7.74E-04  7.03E-03 -1.74E-02  1.04E-02  5.41E-03
         -8.14E-03  4.29E-03  1.23E-03 -2.91E-03  2.30E-02 -4.94E-03 -3.88E-03  6.00E-03 -3.03E-03 -1.92E-03 -6.71E-04  8.94E-04
         5.79E-03 -4.65E-03 -1.01E-02 -2.72E-03  1.63E-03  7.49E-03 -6.91E-03  2.34E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.09E-02  8.85E-03 -2.42E-03 -6.33E-03  9.52E-03  1.03E-02  5.80E-03 -7.61E-03  4.84E-03  1.75E-03  9.37E-04  1.68E-03
         -4.68E-03  4.56E-03  1.06E-03  5.31E-03 -1.99E-03  8.13E-05 -4.06E-03  1.78E-03  4.48E-03  4.64E-03  2.69E-03 -7.27E-03
        -5.44E-04 -1.97E-04 -4.80E-03  1.05E-03 -1.33E-03 -2.96E-03  5.30E-03  6.04E-03  8.91E-03
 
 OM48
+       -2.08E-02  2.33E-03 -1.29E-02 -7.59E-03  5.76E-03  1.04E-02  1.67E-03 -1.58E-02  8.13E-03 -7.74E-04  2.36E-03  1.27E-03
          1.08E-03 -1.03E-03  4.18E-03  1.05E-02  1.40E-02 -5.45E-03  6.39E-03 -2.01E-03  3.85E-03  5.84E-04  7.59E-03 -4.74E-03
        -2.28E-03  1.70E-03 -1.09E-03  5.89E-03 -1.08E-03  4.63E-03  2.89E-03  1.65E-03  1.50E-03  8.62E-03
 
 OM55
+       -6.47E-02  3.50E-02 -1.49E-02 -6.00E-02  4.15E-02  3.42E-02  7.54E-03 -9.80E-02  1.74E-02  4.92E-02 -3.86E-03  1.33E-03
         -3.29E-03  1.49E-02 -1.53E-02  1.70E-02 -2.27E-02  9.93E-03  6.17E-03  2.76E-03  2.74E-02  2.28E-02  4.01E-02 -4.69E-02
        -1.66E-02 -3.85E-04  2.25E-04  1.88E-02 -1.20E-02  1.15E-02  3.21E-02 -3.60E-03  6.99E-03  8.81E-03  9.52E-02
 
 OM56
+       -8.58E-03  1.11E-02 -1.13E-02 -5.70E-03  3.70E-03  5.96E-03 -4.83E-03 -1.33E-02  3.08E-03  8.29E-03 -3.68E-03 -1.97E-03
          2.03E-04  3.46E-03  1.80E-03  8.44E-03 -6.36E-04 -1.02E-04  2.70E-03  3.23E-04  8.90E-03  4.57E-03  6.90E-03 -5.36E-03
        -5.79E-03  3.30E-03  5.43E-03  6.23E-03 -1.56E-03 -5.29E-03  8.18E-03 -3.99E-03  2.46E-04  1.81E-03  1.11E-02  6.86E-03
 
 OM57
+        5.76E-04 -1.94E-03 -1.42E-02  1.10E-02 -3.35E-03  1.18E-03 -3.38E-03  1.26E-02  7.60E-03 -1.24E-02  2.83E-03 -1.77E-03
          2.46E-03 -1.53E-03  1.28E-02  1.04E-02  1.76E-02 -8.96E-03  2.50E-03  1.28E-03  1.72E-03 -1.51E-03 -1.41E-03  8.12E-03
         9.00E-04  2.07E-03  2.70E-03  3.13E-03  2.76E-03 -4.66E-03 -2.46E-03  3.98E-03  8.09E-04  2.87E-03 -9.79E-03  1.58E-03
          1.03E-02
 
 OM58
+        2.55E-02 -1.31E-02 -8.77E-03  3.11E-02 -1.84E-02 -9.36E-03 -5.30E-03  3.94E-02  2.29E-03 -2.24E-02  2.24E-03 -3.44E-03
          8.14E-03 -7.50E-03  1.57E-02  3.18E-03  1.94E-02 -9.34E-03  1.49E-03  4.00E-03 -5.54E-03 -7.72E-03 -9.95E-03  2.08E-02
         5.80E-03  1.90E-03  5.14E-03 -2.39E-03  5.69E-03 -8.42E-03 -9.79E-03  2.50E-03 -2.12E-03 -1.00E-03 -2.64E-02 -2.24E-03
          9.89E-03  2.03E-02
 
 OM66
+       -3.21E-02  1.45E-02 -8.85E-03 -2.66E-02  1.82E-02  2.73E-02  4.59E-03 -3.39E-02  4.25E-03  1.28E-02 -6.47E-04  1.69E-04
          1.18E-03  3.93E-03 -5.39E-03  1.04E-02  5.17E-03  5.24E-04  6.07E-03 -4.63E-03  1.42E-02  3.44E-03  1.04E-02 -1.34E-02
        -1.04E-02  6.42E-03 -5.45E-03  9.75E-03 -1.93E-03  1.26E-03  1.53E-02  3.35E-03  3.43E-03  5.06E-03  1.45E-02  2.82E-03
         -2.07E-03 -9.54E-03  2.41E-02
 
 OM67
+        1.51E-02 -1.72E-02  6.57E-03  9.99E-03 -1.40E-02 -1.19E-02 -6.49E-04  1.74E-02 -6.14E-03 -9.92E-03  1.64E-03  5.66E-04
          2.69E-03 -6.15E-03 -1.99E-03 -1.04E-02  7.35E-03 -9.53E-04  2.04E-03 -8.89E-04 -9.91E-03 -7.86E-03 -6.64E-03  1.13E-02
         4.69E-03 -1.94E-03  1.80E-03 -4.73E-03  2.37E-03  4.14E-03 -1.00E-02 -1.15E-03 -6.57E-03 -1.62E-03 -1.50E-02 -4.04E-03
         -1.74E-03  3.97E-03 -4.57E-03  1.07E-02
 
 OM68
+        5.90E-02 -1.35E-02  8.91E-03  4.73E-02 -3.10E-02 -3.19E-02 -1.23E-02  6.39E-02 -1.32E-02 -1.22E-02 -7.41E-03 -7.73E-03
          7.17E-03 -4.69E-03  8.36E-03 -9.76E-03 -9.84E-03  5.01E-04 -1.45E-03 -2.91E-04 -6.17E-03 -7.69E-03 -1.92E-02  2.65E-02
         6.38E-03 -4.27E-04  1.54E-02 -9.06E-03  4.74E-03 -1.55E-02 -1.27E-02 -1.12E-02 -7.36E-03 -1.10E-02 -3.65E-02 -2.60E-03
          1.87E-03  1.49E-02 -1.70E-02  8.57E-03  4.05E-02
 
 OM77
+       -8.93E-03  1.05E-02 -5.23E-03 -3.07E-03  5.58E-03  1.64E-03 -8.25E-03 -2.48E-02  1.29E-03  1.98E-02 -6.52E-03 -2.68E-03
          4.20E-03 -1.54E-03 -1.91E-03  8.05E-03 -1.43E-02  4.16E-03  5.20E-03 -3.40E-03  5.90E-03  4.36E-03  9.55E-03 -1.14E-02
        -6.33E-03  2.92E-03  3.96E-03  5.64E-03 -4.30E-03  1.15E-03  8.68E-03 -1.43E-02 -4.08E-03  2.76E-03  2.03E-02  4.31E-03
         -3.76E-03 -6.42E-03  2.04E-03 -2.03E-03 -4.88E-03  2.12E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.38E-02  5.37E-03 -9.25E-03  2.35E-02 -4.99E-03  1.02E-03 -9.52E-03  1.54E-02  3.37E-03  2.61E-03 -5.70E-03 -6.95E-03
          7.98E-03 -2.59E-03  1.32E-02  1.56E-02 -4.94E-03 -2.69E-03  3.81E-03 -2.52E-03  7.42E-03  1.30E-03 -2.40E-03  4.42E-03
        -3.53E-03  5.40E-03  5.85E-03  3.13E-03  1.13E-03 -1.21E-02  4.74E-03 -9.33E-03  1.22E-03 -4.76E-04 -6.91E-03  2.83E-03
          5.01E-03  7.37E-03 -2.22E-03 -4.47E-03  1.03E-02  5.92E-03  1.67E-02
 
 OM88
+       -5.64E-02  3.65E-02 -3.66E-02 -2.67E-02  3.36E-02  4.62E-02 -2.78E-03 -6.37E-02  2.40E-02  3.18E-02 -5.85E-03 -5.40E-03
          5.59E-03  8.58E-03  1.07E-02  4.45E-02 -6.03E-05 -3.24E-03  1.27E-02 -3.85E-03  3.45E-02  1.67E-02  2.87E-02 -3.08E-02
        -1.99E-02  1.13E-02  8.49E-04  2.32E-02 -6.18E-03 -6.88E-03  3.39E-02 -5.61E-03  1.03E-02  1.29E-02  5.10E-02  1.36E-02
          5.04E-03 -1.01E-02  2.20E-02 -1.94E-02 -2.73E-02  1.65E-02  1.43E-02  6.96E-02
 
 SG11
+        9.86E-04 -4.60E-04  1.27E-04  9.28E-04 -5.96E-04 -5.14E-04 -1.73E-04  1.16E-03 -1.73E-04 -4.15E-04 -2.07E-05 -1.01E-04
          1.67E-04 -1.94E-04  1.89E-04 -1.78E-04  8.05E-05 -1.20E-04  3.32E-06  5.67E-06 -2.66E-04 -2.16E-04 -3.62E-04  5.31E-04
         1.68E-04 -1.32E-05  1.37E-04 -1.68E-04  1.27E-04 -1.77E-04 -3.02E-04 -8.22E-05 -1.07E-04 -1.26E-04 -7.47E-04 -1.06E-04
          1.14E-04  3.53E-04 -3.39E-04  1.70E-04  5.44E-04 -1.52E-04  1.47E-04 -5.26E-04  1.20E-05
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.44E-04 -5.89E-04  2.59E-04  3.90E-05 -2.25E-04 -1.95E-04  1.97E-04  6.26E-04 -6.04E-05 -7.34E-04  2.96E-04  1.35E-04
         -6.93E-05 -1.64E-04  6.19E-05 -3.00E-04  6.25E-04 -1.77E-04 -5.20E-05  3.72E-07 -4.57E-04 -2.85E-04 -3.42E-04  4.14E-04
         2.60E-04 -1.08E-04 -2.31E-04 -2.07E-04  1.46E-04  2.80E-04 -5.03E-04  3.41E-04 -5.52E-05  1.03E-05 -8.14E-04 -1.99E-04
          1.02E-04  1.89E-04 -8.95E-05  2.00E-04 -1.81E-05 -3.28E-04 -1.97E-04 -6.59E-04  4.01E-06  0.00E+00  1.84E-05
 
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
+       -4.19E-01  2.25E-01
 
 TH 3
+        3.78E-01 -2.86E-01  2.30E-01
 
 TH 4
+        7.46E-01 -3.63E-01 -3.82E-02  3.11E-01
 
 TH 5
+       -7.30E-01  5.49E-01 -3.65E-02 -7.43E-01  2.16E-01
 
 TH 6
+       -6.50E-01  3.58E-01 -3.02E-01 -4.64E-01  5.73E-01  2.65E-01
 
 TH 7
+       -2.86E-01 -1.22E-01  2.69E-01 -4.74E-01  3.59E-01  2.68E-01  1.50E-01
 
 TH 8
+        7.88E-01 -4.72E-01  2.14E-01  7.91E-01 -7.22E-01 -5.52E-01 -1.81E-01  3.84E-01
 
 OM11
+       -5.73E-01  1.70E-01 -6.01E-01 -2.04E-01  3.76E-01  3.87E-01  1.98E-02 -3.24E-01  1.50E-01
 
 OM12
+       -2.83E-01  6.15E-01  2.60E-02 -4.11E-01  4.76E-01  2.94E-01 -9.19E-02 -6.46E-01 -1.16E-01  2.24E-01
 
 OM13
+       -2.92E-01 -3.73E-01  1.22E-01 -2.38E-01  1.49E-01  1.06E-01  2.89E-01  2.90E-03  4.27E-01 -5.63E-01  1.05E-01
 
 OM14
+       -2.66E-01 -1.66E-01  1.77E-01 -4.52E-01  2.07E-01 -1.50E-01  5.17E-01 -2.58E-01  4.18E-02 -1.77E-01  3.50E-01  8.68E-02
 
 OM15
+        2.48E-01 -1.86E-01 -1.24E-01  3.81E-01 -2.21E-01  2.09E-01 -3.13E-01  1.78E-01  1.89E-02  3.65E-02 -1.10E-01 -6.92E-01
          1.40E-01
 
 OM16
+       -4.36E-01  5.72E-01 -5.55E-02 -5.06E-01  5.36E-01  1.79E-01  1.21E-01 -3.45E-01  1.81E-01  3.51E-01 -2.74E-02  1.99E-01
         -5.73E-01  1.12E-01
 
 OM17
+        6.44E-02 -7.84E-02 -5.76E-01  4.86E-01 -2.13E-01  1.51E-02 -3.12E-01  4.07E-01  5.80E-01 -4.92E-01  1.87E-01 -3.55E-01
          2.87E-01 -1.60E-01  1.61E-01
 
 OM18
+       -4.36E-01  3.85E-01 -7.64E-01 -1.33E-02  2.92E-01  5.23E-01 -1.89E-01 -2.53E-01  7.21E-01  1.64E-01 -9.41E-02 -3.37E-01
          2.57E-01  8.22E-02  6.65E-01  2.00E-01
 
 OM22
+       -2.76E-01 -3.58E-01 -5.37E-01  2.34E-02 -1.49E-01  2.39E-01  6.66E-02  1.24E-01  4.87E-01 -6.79E-01  5.28E-01  7.31E-02
          1.23E-01 -2.37E-01  5.16E-01  3.11E-01  2.80E-01
 
 OM23
+        1.48E-01  2.08E-01  5.17E-01 -2.72E-01  1.97E-01 -1.76E-01  2.12E-01 -2.53E-01 -5.18E-01  5.31E-01 -4.98E-01  2.53E-01
         -3.34E-01  2.13E-01 -6.69E-01 -4.36E-01 -7.57E-01  1.23E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -2.67E-01 -1.02E-01 -4.45E-01  5.33E-02 -4.11E-02  3.82E-01 -2.72E-01 -1.25E-01  3.06E-01  8.16E-02  1.52E-02 -5.28E-01
          6.41E-01 -2.58E-01  2.98E-01  4.89E-01  4.52E-01 -5.54E-01  1.31E-01
 
 OM25
+        9.76E-02  1.95E-03 -5.28E-02 -6.43E-02 -4.45E-02 -9.90E-02  2.30E-01  5.37E-02  5.00E-02 -2.14E-01  4.82E-02  3.22E-01
         -3.61E-01  1.88E-01 -1.54E-02 -2.10E-01  7.35E-02  2.11E-01 -5.34E-01  9.08E-02
 
 OM26
+       -4.25E-01  6.51E-01 -5.20E-01 -2.73E-01  4.76E-01  6.27E-01 -1.23E-01 -4.56E-01  3.88E-01  5.46E-01 -3.61E-01 -4.36E-01
          2.14E-01  3.97E-01  1.49E-01  6.70E-01 -5.98E-02 -1.46E-04  3.86E-01 -1.20E-01  1.62E-01
 
 OM27
+       -3.76E-01  7.07E-01 -1.48E-01 -4.45E-01  5.97E-01  2.12E-01  7.83E-02 -5.87E-01  2.14E-01  6.38E-01 -3.39E-01  7.45E-02
         -3.39E-01  5.89E-01 -1.96E-01  2.72E-01 -4.97E-01  4.80E-01 -3.09E-01  2.21E-01  5.62E-01  1.06E-01
 
 OM28
+       -7.34E-01  4.99E-01 -5.09E-01 -5.88E-01  5.63E-01  4.98E-01  2.73E-02 -8.34E-01  5.15E-01  5.85E-01 -7.54E-02  1.57E-02
         -2.91E-02  3.82E-01 -1.18E-01  4.64E-01  6.53E-02 -2.11E-02  3.83E-01 -7.95E-04  5.88E-01  5.52E-01  1.48E-01
 
 OM33
+        7.07E-01 -6.33E-01  5.14E-02  7.05E-01 -8.00E-01 -5.12E-01 -2.33E-01  8.88E-01 -2.35E-01 -7.67E-01  1.41E-01 -1.95E-01
          2.73E-01 -5.15E-01  4.36E-01 -2.27E-01  3.96E-01 -3.87E-01  4.16E-02  5.65E-02 -4.89E-01 -7.27E-01 -7.41E-01  1.82E-01
 
 OM34
+        3.72E-01 -5.91E-01  3.68E-01  3.35E-01 -3.86E-01 -5.69E-01  2.50E-01  5.61E-01 -1.70E-01 -6.75E-01  3.96E-01  4.39E-01
         -3.25E-01 -1.96E-01  1.00E-01 -5.05E-01  1.44E-01 -2.68E-02 -4.70E-01  3.42E-01 -7.66E-01 -4.02E-01 -6.08E-01  5.14E-01
         1.04E-01
 
 OM35
+        4.57E-03  2.92E-01 -4.38E-01  1.20E-01 -2.35E-02  4.13E-01 -3.32E-01 -1.41E-01  7.87E-02  2.14E-01 -3.61E-01 -4.65E-01
          5.59E-01 -2.74E-01  2.14E-01  4.99E-01  1.06E-01 -1.53E-01  3.90E-01 -2.70E-01  4.98E-01  7.05E-03  1.64E-01 -1.52E-02
        -7.00E-01  8.63E-02
 
 OM36
+        3.16E-01  9.72E-02 -4.07E-01  3.71E-01 -4.37E-01 -4.36E-01 -5.62E-01  2.66E-01  5.63E-03  4.84E-02 -4.58E-01 -3.71E-01
          1.36E-01  1.13E-01  3.25E-01  1.70E-01 -2.64E-02 -6.72E-02  2.22E-01  2.85E-02  2.34E-01  9.08E-02  8.90E-02  3.00E-01
        -1.22E-01  8.86E-02  1.34E-01
 
 OM37
+       -5.42E-01  4.68E-01 -6.58E-01 -2.70E-01  3.02E-01  4.87E-01 -2.72E-01 -5.02E-01  5.81E-01  3.43E-01 -9.77E-03 -3.31E-01
          2.98E-01  2.19E-01  2.44E-01  6.59E-01  2.48E-01 -4.34E-01  6.15E-01 -3.03E-01  6.73E-01  2.61E-01  7.29E-01 -3.69E-01
        -7.60E-01  5.27E-01  2.63E-01  1.19E-01
 
 OM38
+        4.71E-01 -5.93E-01  1.01E-01  4.46E-01 -5.34E-01 -2.10E-01 -9.52E-02  6.12E-01 -3.96E-02 -6.84E-01  4.17E-01 -1.42E-01
          3.60E-01 -4.50E-01  3.76E-01 -1.36E-01  4.03E-01 -4.06E-01  6.85E-02 -5.24E-04 -3.45E-01 -5.87E-01 -5.72E-01  7.85E-01
         3.27E-01  1.28E-01  2.02E-02 -1.36E-01  5.91E-02
 
 OM44
+       -4.23E-01 -2.68E-01  3.61E-01 -4.75E-01  3.12E-01  8.00E-02  3.46E-01 -3.07E-01  1.36E-01 -6.88E-02  6.26E-01  3.61E-01
         -9.14E-02 -2.70E-02 -2.70E-01 -2.46E-01  1.42E-01 -8.39E-02  1.73E-01 -2.11E-01 -3.36E-01 -1.73E-01  1.71E-01 -2.24E-01
         1.83E-01 -4.59E-01 -4.33E-01 -2.85E-02 -6.61E-02  1.68E-01
 
 OM45
+       -3.91E-01  6.47E-01 -3.33E-01 -3.43E-01  4.88E-01  6.03E-01 -7.79E-02 -6.26E-01  1.90E-01  7.55E-01 -4.08E-01 -2.63E-01
          1.91E-01  2.96E-01 -1.89E-01  4.60E-01 -3.04E-01  1.69E-01  2.23E-01 -1.17E-01  8.12E-01  5.86E-01  6.21E-01 -6.69E-01
        -8.32E-01  5.44E-01  3.17E-03  6.29E-01 -4.18E-01 -2.95E-01  1.66E-01
 
 OM46
+       -3.67E-01 -1.63E-01  2.48E-03 -3.27E-01  2.57E-01  3.14E-01  5.56E-01 -1.32E-02  3.06E-01 -5.07E-01  6.47E-01  4.07E-01
         -3.80E-01  2.50E-01  5.00E-02 -9.51E-02  5.36E-01 -2.62E-01 -1.93E-01  4.32E-01 -1.22E-01 -1.19E-01 -2.97E-02  3.22E-02
         3.65E-01 -3.52E-01 -4.94E-01 -1.50E-01  1.80E-01  2.91E-01 -2.72E-01  1.53E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.33E-01  4.17E-01 -1.12E-01 -2.15E-01  4.67E-01  4.09E-01  4.09E-01 -2.10E-01  3.42E-01  8.27E-02  9.42E-02  2.05E-01
         -3.55E-01  4.31E-01  6.97E-02  2.81E-01 -7.52E-02  6.98E-03 -3.27E-01  2.07E-01  2.92E-01  4.65E-01  1.93E-01 -4.24E-01
        -5.55E-02 -2.41E-02 -3.79E-01  9.35E-02 -2.37E-01 -1.87E-01  3.39E-01  4.19E-01  9.44E-02
 
 OM48
+       -6.44E-01  1.12E-01 -6.07E-01 -2.62E-01  2.87E-01  4.23E-01  1.20E-01 -4.44E-01  5.83E-01 -3.72E-02  2.41E-01  1.57E-01
          8.29E-02 -9.92E-02  2.80E-01  5.63E-01  5.36E-01 -4.76E-01  5.24E-01 -2.38E-01  2.56E-01  5.95E-02  5.53E-01 -2.81E-01
        -2.36E-01  2.12E-01 -8.79E-02  5.34E-01 -1.96E-01  2.96E-01  1.88E-01  1.16E-01  1.72E-01  9.29E-02
 
 OM55
+       -6.03E-01  5.05E-01 -2.10E-01 -6.24E-01  6.23E-01  4.17E-01  1.63E-01 -8.28E-01  3.75E-01  7.12E-01 -1.19E-01  4.96E-02
         -7.63E-02  4.29E-01 -3.09E-01  2.74E-01 -2.62E-01  2.61E-01  1.52E-01  9.84E-02  5.48E-01  7.01E-01  8.79E-01 -8.37E-01
        -5.17E-01 -1.44E-02  5.43E-03  5.13E-01 -6.59E-01  2.22E-01  6.27E-01 -7.63E-02  2.40E-01  3.07E-01  3.09E-01
 
 OM56
+       -2.98E-01  5.97E-01 -5.92E-01 -2.21E-01  2.07E-01  2.71E-01 -3.88E-01 -4.18E-01  2.48E-01  4.47E-01 -4.21E-01 -2.73E-01
          1.75E-02  3.72E-01  1.35E-01  5.08E-01 -2.74E-02 -9.94E-03  2.48E-01  4.29E-02  6.63E-01  5.22E-01  5.64E-01 -3.56E-01
        -6.73E-01  4.61E-01  4.89E-01  6.33E-01 -3.18E-01 -3.79E-01  5.95E-01 -3.15E-01  3.15E-02  2.35E-01  4.34E-01  8.28E-02
 
 OM57
+        1.63E-02 -8.51E-02 -6.07E-01  3.47E-01 -1.53E-01  4.36E-02 -2.21E-01  3.24E-01  4.98E-01 -5.45E-01  2.64E-01 -2.01E-01
          1.73E-01 -1.34E-01  7.85E-01  5.09E-01  6.16E-01 -7.15E-01  1.88E-01  1.39E-01  1.04E-01 -1.40E-01 -9.42E-02  4.40E-01
         8.53E-02  2.36E-01  1.98E-01  2.59E-01  4.59E-01 -2.72E-01 -1.46E-01  2.56E-01  8.43E-02  3.04E-01 -3.12E-01  1.87E-01
          1.02E-01
 
 OM58
+        5.15E-01 -4.09E-01 -2.68E-01  7.01E-01 -6.00E-01 -2.48E-01 -2.48E-01  7.21E-01  1.07E-01 -7.04E-01  1.49E-01 -2.78E-01
          4.09E-01 -4.70E-01  6.86E-01  1.11E-01  4.86E-01 -5.32E-01  7.98E-02  3.10E-01 -2.40E-01 -5.13E-01 -4.73E-01  8.04E-01
         3.92E-01  1.54E-01  2.69E-01 -1.41E-01  6.76E-01 -3.51E-01 -4.14E-01  1.15E-01 -1.58E-01 -7.60E-02 -6.02E-01 -1.90E-01
          6.84E-01  1.42E-01
 
 OM66
+       -5.95E-01  4.17E-01 -2.48E-01 -5.50E-01  5.42E-01  6.63E-01  1.97E-01 -5.69E-01  1.82E-01  3.68E-01 -3.95E-02  1.25E-02
          5.42E-02  2.26E-01 -2.16E-01  3.35E-01  1.19E-01  2.74E-02  2.98E-01 -3.29E-01  5.62E-01  2.10E-01  4.52E-01 -4.77E-01
        -6.43E-01  4.79E-01 -2.61E-01  5.28E-01 -2.10E-01  4.81E-02  5.95E-01  1.41E-01  2.34E-01  3.51E-01  3.02E-01  2.19E-01
         -1.31E-01 -4.32E-01  1.55E-01
 
 OM67
+        4.21E-01 -7.40E-01  2.77E-01  3.11E-01 -6.26E-01 -4.33E-01 -4.18E-02  4.38E-01 -3.96E-01 -4.29E-01  1.51E-01  6.31E-02
          1.86E-01 -5.30E-01 -1.20E-01 -5.03E-01  2.54E-01 -7.48E-02  1.50E-01 -9.47E-02 -5.92E-01 -7.20E-01 -4.35E-01  6.04E-01
         4.37E-01 -2.17E-01  1.30E-01 -3.85E-01  3.87E-01  2.38E-01 -5.86E-01 -7.27E-02 -6.74E-01 -1.69E-01 -4.71E-01 -4.72E-01
         -1.66E-01  2.70E-01 -2.85E-01  1.03E-01
 
 OM68
+        8.43E-01 -2.98E-01  1.93E-01  7.55E-01 -7.14E-01 -5.98E-01 -4.06E-01  8.28E-01 -4.38E-01 -2.70E-01 -3.50E-01 -4.42E-01
          2.55E-01 -2.08E-01  2.58E-01 -2.42E-01 -1.75E-01  2.02E-02 -5.47E-02 -1.59E-02 -1.89E-01 -3.62E-01 -6.46E-01  7.25E-01
         3.05E-01 -2.46E-02  5.71E-01 -3.79E-01  3.98E-01 -4.59E-01 -3.81E-01 -3.63E-01 -3.88E-01 -5.86E-01 -5.89E-01 -1.56E-01
          9.16E-02  5.20E-01 -5.46E-01  4.12E-01  2.01E-01
 
 OM77
+       -1.76E-01  3.20E-01 -1.56E-01 -6.78E-02  1.78E-01  4.26E-02 -3.77E-01 -4.44E-01  5.92E-02  6.08E-01 -4.25E-01 -2.13E-01
          2.06E-01 -9.43E-02 -8.18E-02  2.76E-01 -3.51E-01  2.32E-01  2.72E-01 -2.57E-01  2.50E-01  2.84E-01  4.44E-01 -4.32E-01
        -4.19E-01  2.33E-01  2.03E-01  3.26E-01 -5.00E-01  4.69E-02  3.60E-01 -6.45E-01 -2.97E-01  2.04E-01  4.52E-01  3.58E-01
         -2.54E-01 -3.10E-01  9.03E-02 -1.35E-01 -1.67E-01  1.45E-01
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.07E-01  1.85E-01 -3.11E-01  5.83E-01 -1.79E-01  2.96E-02 -4.90E-01  3.10E-01  1.73E-01  9.02E-02 -4.18E-01 -6.19E-01
          4.41E-01 -1.79E-01  6.33E-01  6.01E-01 -1.36E-01 -1.68E-01  2.24E-01 -2.15E-01  3.54E-01  9.53E-02 -1.25E-01  1.88E-01
        -2.62E-01  4.84E-01  3.37E-01  2.04E-01  1.48E-01 -5.53E-01  2.21E-01 -4.71E-01  9.96E-02 -3.96E-02 -1.73E-01  2.64E-01
          3.81E-01  4.00E-01 -1.11E-01 -3.34E-01  3.95E-01  3.15E-01  1.29E-01
 
 OM88
+       -6.15E-01  6.15E-01 -6.04E-01 -3.25E-01  5.90E-01  6.60E-01 -7.01E-02 -6.30E-01  6.06E-01  5.39E-01 -2.10E-01 -2.36E-01
          1.52E-01  2.90E-01  2.53E-01  8.41E-01 -8.16E-04 -9.94E-02  3.66E-01 -1.61E-01  8.07E-01  6.00E-01  7.37E-01 -6.44E-01
        -7.27E-01  4.97E-01  2.40E-02  7.41E-01 -3.96E-01 -1.55E-01  7.74E-01 -1.39E-01  4.15E-01  5.27E-01  6.26E-01  6.23E-01
          1.88E-01 -2.70E-01  5.36E-01 -7.12E-01 -5.14E-01  4.29E-01  4.19E-01  2.64E-01
 
 SG11
+        8.19E-01 -5.91E-01  1.59E-01  8.61E-01 -7.97E-01 -5.59E-01 -3.33E-01  8.73E-01 -3.32E-01 -5.35E-01 -5.69E-02 -3.36E-01
          3.46E-01 -5.00E-01  3.39E-01 -2.57E-01  8.29E-02 -2.81E-01  7.31E-03  1.80E-02 -4.75E-01 -5.90E-01 -7.08E-01  8.45E-01
         4.67E-01 -4.42E-02  2.95E-01 -4.08E-01  6.20E-01 -3.03E-01 -5.25E-01 -1.55E-01 -3.27E-01 -3.92E-01 -7.00E-01 -3.70E-01
          3.24E-01  7.16E-01 -6.30E-01  4.75E-01  7.81E-01 -3.01E-01  3.29E-01 -5.77E-01  3.46E-03
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        9.64E-02 -6.10E-01  2.63E-01  2.92E-02 -2.43E-01 -1.71E-01  3.06E-01  3.80E-01 -9.36E-02 -7.63E-01  6.54E-01  3.62E-01
         -1.15E-01 -3.41E-01  8.96E-02 -3.49E-01  5.19E-01 -3.35E-01 -9.22E-02  9.54E-04 -6.56E-01 -6.29E-01 -5.39E-01  5.31E-01
         5.83E-01 -2.92E-01 -4.01E-01 -4.06E-01  5.75E-01  3.88E-01 -7.05E-01  5.19E-01 -1.36E-01  2.58E-02 -6.14E-01 -5.61E-01
          2.33E-01  3.10E-01 -1.34E-01  4.50E-01 -2.09E-02 -5.26E-01 -3.55E-01 -5.82E-01  2.70E-01  0.00E+00  4.29E-03
 
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
+       -8.10E+01 -1.30E+02 -7.24E+00  2.18E+01  3.64E+02
 
 TH 6
+       -2.04E+01 -7.70E+01 -8.18E+01 -5.55E+01  1.22E+02  2.91E+02
 
 TH 7
+        5.32E+01  1.53E+02 -1.83E+01  1.13E+02 -8.59E+01 -8.23E+01  3.63E+02
 
 TH 8
+       -1.99E+02 -2.87E+02 -1.00E+02 -9.85E+01  1.24E+02  1.50E+02 -2.18E+02  5.56E+02
 
 OM11
+        8.21E+01  3.21E+01 -1.03E+02  1.47E+01  2.39E+01  1.18E+02  1.10E+02 -4.03E+01  9.92E+02
 
 OM12
+        7.76E+01  4.60E+01  4.61E+01  4.30E+02  1.05E+02  5.65E+01  3.51E+02 -3.25E+02  8.72E+02  3.51E+03
 
 OM13
+       -2.32E+02 -3.55E+01 -2.83E+02  5.54E+01 -1.46E+02  1.15E+01  1.85E+02  2.08E+02 -9.17E+02 -8.03E+02  3.59E+03
 
 OM14
+        3.84E+01  4.76E+02  1.09E+02  9.12E+01 -9.65E+01  7.64E+01  1.15E+02 -1.49E+02 -3.23E+02  3.59E+02  3.69E+02  2.64E+03
 
 OM15
+        8.06E+01  1.68E+02 -1.81E+02 -9.10E+01 -4.37E+02 -3.76E+02 -1.36E+01 -1.21E+02 -6.98E+02 -1.77E+03  1.05E+03 -1.95E+02
          2.77E+03
 
 OM16
+        2.45E+02  6.79E+01 -2.85E+01  8.54E+01 -3.49E+02 -3.23E+02  1.27E+02 -2.49E+02  2.02E+02 -4.12E+02 -6.67E+02 -8.33E+02
          8.81E+02  1.90E+03
 
 OM17
+        1.63E+02  1.90E+02  1.90E+02  1.00E+02  1.89E+01  1.68E+02  1.08E+02 -2.12E+02  5.17E+02  1.93E+03 -1.47E+03  1.05E+03
         -1.59E+03 -3.88E+02  2.82E+03
 
 OM18
+       -7.23E+01 -2.69E+02  1.43E+02 -1.09E+02 -6.46E+01 -2.45E+02 -3.18E+02  2.29E+02 -1.11E+03 -2.88E+03  9.84E+02 -8.88E+02
          1.99E+03  6.23E+02 -2.38E+03  4.05E+03
 
 OM22
+        9.38E+01  1.78E+02  2.05E+01  2.66E+02 -4.06E+01 -3.33E+01  2.74E+02 -3.27E+02  3.55E+02  2.13E+03 -2.13E+02  1.57E+02
         -8.02E+02 -2.19E+02  9.82E+02 -1.75E+03  2.01E+03
 
 OM23
+        1.76E+01 -1.29E+02 -2.02E+02 -1.15E+02 -1.69E+02  1.87E+02 -2.24E+02  2.71E+02 -5.56E+02 -1.09E+03  1.76E+03 -4.15E+02
          8.57E+02 -6.62E+01 -8.27E+02  1.53E+03 -3.07E+02  3.76E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.82E+02  6.53E+02 -1.23E+02 -6.40E+01 -8.43E+01  1.20E+02  1.82E+02 -4.81E+02  2.16E+02  3.49E+02 -4.19E+02  1.91E+03
         -8.27E+02 -2.89E+02  1.46E+03 -1.10E+03 -1.46E+02 -3.46E+02  4.51E+03
 
 OM25
+        1.12E+02  8.68E+01 -2.02E+02 -5.20E+01 -2.31E+02 -4.53E+02  1.62E+01 -2.26E+02 -6.32E+02 -1.83E+03  5.86E+02 -8.34E+02
          2.90E+03  1.05E+03 -1.91E+03  2.29E+03 -1.14E+03 -2.79E+02 -6.04E+02  6.19E+03
 
 OM26
+        1.82E+01 -3.28E+01  1.22E+02  1.31E+02 -3.78E+02 -4.57E+02  2.41E+02 -6.04E+01 -3.17E+01 -4.25E+02 -2.99E+02 -2.96E+02
          9.98E+02  1.31E+03 -6.15E+02  7.63E+02 -5.66E+02 -7.03E+02 -1.10E+03  1.96E+03  3.66E+03
 
 OM27
+        2.39E+02  1.52E+02 -2.12E+02  1.55E+02  1.10E+02  3.44E+02  2.23E+02 -1.60E+02  6.11E+02  1.98E+03 -5.99E+02  1.44E+03
         -1.85E+03 -6.34E+02  2.10E+03 -2.33E+03  1.15E+03 -9.46E+02  2.81E+03 -2.43E+03 -1.51E+03  4.43E+03
 
 OM28
+       -3.69E+02 -4.66E+02  1.52E+02 -3.93E+02 -1.27E+02 -7.85E+01 -4.00E+02  8.91E+02 -9.29E+02 -4.28E+03  1.03E+03 -1.04E+03
          2.35E+03  8.81E+02 -2.57E+03  4.18E+03 -3.19E+03  1.42E+03 -2.18E+03  2.67E+03  2.42E+03 -3.55E+03  8.49E+03
 
 OM33
+       -1.15E+02 -5.93E+01  5.13E+01  1.52E+02 -5.56E+00  1.22E+01 -3.67E+00 -1.30E+02  2.93E+01 -8.59E+00  2.22E+02  6.72E+01
          9.19E+01  4.52E+02  1.67E+02 -4.60E+02 -3.18E+02 -1.35E+02  2.25E+02 -1.30E+02  1.51E+02  4.01E+02  1.55E+02  2.90E+03
 
 OM34
+        1.20E+02 -1.10E+02  2.64E+01 -2.72E+02  7.92E+01  1.16E+02 -1.96E+02  1.65E+02  2.26E+01 -4.85E+02 -1.86E+02 -8.37E+02
          1.63E+02  2.51E+02 -3.77E+02  1.85E+02 -3.48E+02  2.29E+02 -1.80E+02  9.38E+02  1.96E+02 -3.42E+02  8.60E+02  6.64E+02
         3.03E+03
 
 OM35
+       -1.75E+02 -1.31E+02  8.81E+01  1.02E+02  2.50E+02  5.87E+01  1.07E+02  2.87E+02  6.24E+02  9.03E+02 -9.29E+02  4.77E+01
         -1.23E+03  3.69E+02  7.67E+02 -1.31E+03 -9.27E+01 -2.01E+03  7.44E+02 -4.93E+02  7.68E+02  1.23E+03 -2.33E+02  9.06E+02
         9.93E+02  4.25E+03
 
 OM36
+       -3.30E+01  1.67E+02  1.21E+02  7.48E+01  1.07E+02  1.88E+01  1.65E+02 -1.17E+02 -2.72E+01  1.51E+02  2.94E+02  1.20E+02
          2.22E+02 -1.75E+02 -3.51E+02 -1.21E+01  1.52E+01 -7.47E+02  7.25E+01  5.16E+02  1.25E+02 -2.78E+02 -1.17E+02 -9.42E+01
        -2.49E+02  1.15E+03  1.91E+03
 
 OM37
+        1.79E+02 -2.79E+02 -4.03E+01 -1.69E+02  1.28E+02  1.89E+02 -2.61E+02  8.47E+01 -7.10E+02 -6.81E+02  9.82E+02 -3.49E+02
          7.22E+02 -4.38E+02 -6.51E+02  1.20E+03 -2.86E+02  2.48E+03 -2.40E+02  1.14E+03 -5.58E+02 -5.04E+02  4.41E+02 -1.25E+02
         1.39E+03 -1.84E+03 -9.19E+02  4.24E+03
 
 OM38
+        1.58E+02  2.72E+02 -1.35E+02  1.39E+02  3.19E+02 -1.40E+02  6.17E+01 -1.67E+02  9.11E+02  1.49E+03 -3.56E+03 -1.22E+02
         -1.33E+03  3.71E+02  1.32E+03 -8.31E+02  7.70E+02 -2.97E+03  6.54E+02 -1.49E+02  4.56E+02  7.13E+02 -1.93E+03 -2.44E+03
        -1.74E+03  8.52E+02  4.45E+02 -3.05E+03  9.41E+03
 
 OM44
+        6.76E+01 -1.17E+01 -1.86E+02  1.40E+02 -1.09E+02 -5.72E+01  1.19E+02 -1.12E+02  2.16E+02  5.02E+02 -3.32E+02 -3.04E+02
          1.85E+01  3.70E+02  9.29E+01 -2.63E+02  4.76E+02 -1.50E+02 -3.69E+02  2.26E+02  5.25E+02  8.99E+01 -3.47E+02  1.49E+02
         1.38E+02  3.04E+02 -6.34E+00 -3.25E+02  3.49E+02  7.49E+02
 
 OM45
+       -1.02E+02 -7.77E+01  9.81E+01 -1.79E+02  1.18E+02  1.34E+02 -2.20E+02  2.56E+02 -1.49E+02 -8.92E+02  1.05E+02 -4.17E+02
          1.87E+01 -7.04E+01 -1.59E+02  6.25E+02 -3.95E+02  7.95E+02 -2.92E+02 -6.19E+02 -1.03E+03 -2.50E+02  1.10E+03  3.16E+02
         5.24E+02  5.48E+01  7.43E+01  1.76E+02 -1.06E+03 -1.52E+02  1.55E+03
 
 OM46
+        6.14E+01  1.16E+02  1.47E+02 -9.72E+01  1.51E+02  2.58E+01 -1.36E+02  8.92E+00 -2.43E+02 -1.74E+02  1.42E+02  5.45E+02
         -1.62E+02 -5.85E+02  1.93E+02  6.76E+01 -3.58E+02  1.19E+02  5.97E+02 -1.17E+03 -1.23E+03  3.82E+02 -3.43E+02  3.46E+01
        -2.83E+02  4.66E+01  4.34E+02  1.58E+02 -4.30E+02 -5.74E+02  6.20E+02  1.72E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.31E+02  2.09E+02 -1.92E+02  2.37E+02 -2.42E+02 -1.59E+02  2.17E+02 -4.48E+02  1.55E+02  1.03E+03 -2.77E+02  5.45E+02
          8.76E+01  2.51E+02  6.01E+02 -6.12E+02  7.97E+02 -3.25E+02  1.07E+03  1.82E+02  4.90E+02  8.68E+02 -1.61E+03  2.43E+02
        -8.25E+02  1.72E+02  2.61E+02 -8.81E+02  1.31E+03  6.88E+02 -8.52E+02 -7.02E+02  2.58E+03
 
 OM48
+       -1.69E+02 -4.96E+02  2.05E+02 -1.94E+02  2.72E+02  6.75E+00 -4.19E+02  3.67E+02 -1.00E+02 -7.69E+02 -1.10E+02 -1.77E+03
          3.71E+02  2.00E+02 -1.03E+03  1.28E+03 -6.20E+02  7.37E+02 -2.33E+03  7.05E+02 -2.89E+02 -2.06E+03  1.56E+03 -6.10E+02
         2.59E+02 -1.08E+03 -3.90E+02  1.07E+03  3.83E+00 -4.69E+02  6.16E+02  3.47E+02 -1.73E+03  3.39E+03
 
 OM55
+       -2.04E+02 -1.30E+02  1.35E+02  5.75E+01  2.29E+02  1.46E+02 -1.67E+02  1.72E+02  1.33E+02  8.05E+02 -3.88E+02  1.07E+02
         -1.18E+03 -6.01E+02  6.89E+02 -9.84E+02  6.22E+02 -3.34E+02 -2.74E+02 -1.77E+03 -1.12E+03  4.94E+02 -1.54E+03 -1.29E+01
        -1.90E+02  1.39E+02 -3.48E+02 -4.50E+02  4.67E+02 -1.34E+02  2.04E+02  2.62E+02 -2.89E+02  2.77E+02  1.22E+03
 
 OM56
+       -3.30E+02 -4.33E+02  9.39E+01  1.45E+02  2.34E+02  1.93E+02 -7.93E+01  3.33E+02  9.62E+01  7.44E+02  5.92E+02  5.04E+00
         -1.26E+03 -1.18E+03  3.62E+02 -5.34E+02  5.57E+02  7.75E+02 -1.03E+03 -2.72E+03 -1.37E+03  3.72E+02 -9.21E+02 -2.03E+02
        -1.67E+02 -1.35E+03 -1.51E+03  3.50E+02 -9.87E+02 -4.87E+01  1.07E+02  1.69E+02 -4.19E+02  6.15E+02  1.36E+03  4.40E+03
 
 OM57
+        1.41E+01  1.60E+02  1.11E+02 -2.17E+02 -3.60E+02 -1.17E+02 -1.13E+02 -2.14E+01 -5.61E+02 -1.71E+03  6.64E+02 -1.50E+02
          1.66E+03  3.72E+02 -1.12E+03  1.67E+03 -9.26E+02  1.31E+03 -1.83E+02  1.97E+03  4.63E+02 -1.97E+03  2.33E+03 -4.09E+02
         2.54E+02 -1.07E+03  1.23E+02  7.35E+02 -1.02E+03 -1.51E+02  2.97E+02 -2.35E+02  2.65E+01  3.95E+02 -7.38E+02 -9.81E+02
          2.68E+03
 
 OM58
+       -7.58E+01 -2.64E+02  3.31E+02  2.38E+02  3.06E+02  3.53E+02  6.72E+01  3.79E+00  8.05E+02  2.49E+03 -9.91E+02  5.82E+02
         -3.50E+03 -9.71E+02  1.99E+03 -2.26E+03  1.32E+03 -1.05E+02  9.87E+02 -5.59E+03 -1.53E+03  2.77E+03 -3.53E+03 -4.15E+02
        -1.26E+03  2.26E+02 -9.21E+02 -8.13E+02  1.03E+03 -6.89E+01 -1.95E+02  4.90E+02  2.53E+02 -5.93E+02  1.70E+03  3.07E+03
         -2.42E+03  6.82E+03
 
 OM66
+       -1.35E+02 -2.10E+02  8.65E+00  2.47E+01  4.77E+01  9.64E+01 -9.56E+01  1.23E+02  2.18E+01  2.29E+02  1.71E+02 -1.85E+02
         -3.74E+02 -5.10E+02  1.50E+02 -1.65E+02  3.19E+02  1.24E+02 -5.66E+02 -3.93E+02 -7.29E+02  2.42E+01 -4.52E+02 -3.40E+02
         2.52E+01 -8.68E+02 -7.18E+02  1.22E+02 -1.07E+02  3.82E+01 -4.08E+01 -3.12E+02 -1.55E+02  4.31E+02  6.27E+02  1.65E+03
         -1.36E+02  7.49E+02  1.17E+03
 
 OM67
+        1.47E+02  3.57E+02  1.72E+02 -1.46E+02 -8.14E+01 -2.16E+02  5.68E+01 -1.91E+02  3.35E+01 -4.85E+02 -5.42E+02  1.37E+02
          3.47E+02  5.45E+02 -4.66E+01  9.66E+01 -5.50E+02 -5.28E+02  2.47E+02  5.40E+02  8.32E+02 -5.30E+02  7.71E+02 -7.54E+01
         2.21E+02  3.32E+02  7.82E+01 -2.75E+02  3.37E+02 -1.76E+02 -1.02E+02  2.40E+02 -1.82E+02  6.74E+01 -3.26E+02 -7.14E+02
          5.19E+02 -6.90E+02 -5.22E+02  1.63E+03
 
 OM68
+       -2.01E+02 -7.99E+01 -1.16E+02  4.75E+00  2.61E+02  2.75E+02 -1.26E+02  3.07E+01 -9.39E+01  6.48E+02  6.46E+02  3.39E+02
         -7.65E+02 -1.41E+03  2.53E+02 -7.06E+02  8.61E+02  3.69E+02 -1.10E+02 -1.28E+03 -2.34E+03  7.67E+02 -1.95E+03 -5.82E+02
        -5.05E+02 -1.15E+03 -4.89E+02  4.20E+02 -2.07E+02 -1.89E+02  3.06E+02  4.24E+02 -1.28E+02  3.46E+02  1.03E+03  1.83E+03
         -4.53E+02  1.34E+03  1.05E+03 -9.55E+02  2.64E+03
 
 OM77
+        4.89E+01  6.88E+01 -1.25E+02  1.03E+02 -5.97E+01  2.67E+01  1.37E+02 -5.06E+01  1.51E+02  5.41E+02 -1.47E+02  4.60E+02
         -3.23E+02  2.88E+01  5.38E+02 -5.04E+02  4.01E+02 -3.83E+02  6.77E+02 -6.74E+02 -8.76E+01  1.05E+03 -9.12E+02  1.03E+02
        -6.45E+02  3.95E+02  1.50E+02 -8.26E+02  9.91E+02  1.79E+02 -1.83E+02  1.88E+00  9.41E+02 -9.16E+02  4.03E+01 -8.71E+01
         -4.83E+02  6.39E+02 -9.50E+01 -3.16E+02  1.19E+02  8.78E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -2.40E+02 -1.04E+02  5.55E+01 -4.04E+02  1.80E+01 -1.92E+02 -1.36E+02  2.72E+02 -6.34E+02 -2.18E+03  1.09E+03 -1.17E+03
          1.76E+03  2.79E+02 -2.23E+03  1.92E+03 -1.19E+03  7.10E+02 -2.42E+03  2.45E+03  8.21E+02 -3.20E+03  3.60E+03 -4.91E+02
         1.20E+03 -8.99E+02  1.20E+02  1.23E+03 -1.88E+03 -3.72E+02  6.80E+02  4.49E+01 -2.04E+03  2.26E+03 -4.92E+02 -3.45E+02
          1.70E+03 -3.14E+03  2.70E+01  9.90E+02 -5.59E+02 -1.53E+03  4.99E+03
 
 OM88
+        1.50E+02  4.08E+02 -5.33E+01  1.47E+02 -6.76E+01  1.42E+01  2.27E+02 -4.08E+02  3.57E+02  1.78E+03  7.67E+01  9.58E+02
         -8.97E+02 -5.08E+02  1.10E+03 -2.54E+03  1.32E+03 -1.04E+03  1.11E+03 -1.33E+03 -9.43E+02  1.61E+03 -3.59E+03  8.26E+02
        -2.07E+02  5.64E+02  3.20E+02 -7.40E+02 -4.67E+02  9.28E+01 -5.39E+02  3.18E+02  6.48E+02 -1.40E+03  6.01E+02  1.27E+02
         -1.21E+03  1.20E+03 -6.08E+01 -1.41E+02  8.59E+02  4.68E+02 -1.86E+03  2.91E+03
 
 SG11
+       -1.72E+03  6.62E+03 -1.95E+03 -1.13E+03 -2.79E+03  1.78E+03  9.19E+03 -2.71E+03  5.62E+03  1.79E+04  1.77E+04  6.88E+03
         -8.70E+03 -5.34E+03  9.82E+03 -1.92E+04  2.32E+04  1.27E+04 -1.95E+04 -1.57E+04  1.88E+04  5.09E+03 -8.59E+03 -1.01E+04
        -3.56E+02 -9.47E+03 -8.72E+03 -1.04E+02 -1.47E+04  1.04E+04 -9.59E+03 -2.05E+04  6.53E+03 -1.31E+04  2.80E+02  2.48E+04
         -1.57E+04  1.70E+04  1.13E+04 -2.91E+03  5.16E+03  6.22E+03 -4.44E+03  8.79E+03  2.40E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.64E+03 -3.22E+03  2.69E+03  4.31E+03  2.07E+03  8.32E+02 -1.43E+03  3.95E+03  6.28E+03  6.38E+03 -6.47E+03 -8.76E+03
         -6.06E+03  5.40E+03  5.72E+02 -2.25E+03 -1.37E+02 -2.81E+03 -1.94E+04 -5.98E+03  1.01E+04 -5.94E+03  1.41E+04  2.35E+03
         2.23E+03  1.61E+04  4.34E+03 -8.13E+03 -5.48E+03  3.02E+03  6.31E+03 -2.78E+03 -6.77E+03  7.30E+03  3.75E+03  7.27E+02
         -9.46E+03  6.92E+03 -2.31E+03  2.74E+02 -2.08E+03 -1.01E+03 -1.14E+03 -4.15E+02  1.80E+05  0.00E+00  8.36E+05
 
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
 RAW OUTPUT FILE (FILE): example6hmto.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
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
 PWR. WT. MASS/IMP/POST MATRIX ACCUM. FOR ETAS (IKAPPA): 1.00000000000000
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
 iteration        -2000 MCMCOBJ=   -6809.70385342651     
 iteration        -1990 MCMCOBJ=   -6671.58925088261     
 iteration        -1980 MCMCOBJ=   -6693.68865708832     
 iteration        -1970 MCMCOBJ=   -6656.10707856915     
 iteration        -1960 MCMCOBJ=   -6668.67260442834     
 iteration        -1950 MCMCOBJ=   -6633.44471320128     
 iteration        -1940 MCMCOBJ=   -6668.60012466310     
 iteration        -1930 MCMCOBJ=   -6643.48583609065     
 iteration        -1920 MCMCOBJ=   -6658.88024231264     
 iteration        -1910 MCMCOBJ=   -6613.53435786108     
 iteration        -1900 MCMCOBJ=   -6576.66235582670     
 iteration        -1890 MCMCOBJ=   -6608.32742263329     
 iteration        -1880 MCMCOBJ=   -6626.79310581548     
 iteration        -1870 MCMCOBJ=   -6587.96255776700     
 iteration        -1860 MCMCOBJ=   -6634.34970506009     
 iteration        -1850 MCMCOBJ=   -6576.02918341586     
 iteration        -1840 MCMCOBJ=   -6586.42530023911     
 iteration        -1830 MCMCOBJ=   -6610.21474377413     
 iteration        -1820 MCMCOBJ=   -6622.15897012801     
 iteration        -1810 MCMCOBJ=   -6556.28190807495     
 iteration        -1800 MCMCOBJ=   -6617.01718499303     
 iteration        -1790 MCMCOBJ=   -6561.90748776235     
 iteration        -1780 MCMCOBJ=   -6608.08878258549     
 iteration        -1770 MCMCOBJ=   -6583.73692237044     
 iteration        -1760 MCMCOBJ=   -6557.17775054879     
 iteration        -1750 MCMCOBJ=   -6581.30680534415     
 iteration        -1740 MCMCOBJ=   -6583.89204637409     
 iteration        -1730 MCMCOBJ=   -6540.36520437899     
 iteration        -1720 MCMCOBJ=   -6554.78834885889     
 iteration        -1710 MCMCOBJ=   -6583.89680953950     
 iteration        -1700 MCMCOBJ=   -6572.49038011571     
 iteration        -1690 MCMCOBJ=   -6603.64945106695     
 iteration        -1680 MCMCOBJ=   -6598.70151938303     
 iteration        -1670 MCMCOBJ=   -6605.40426714973     
 iteration        -1660 MCMCOBJ=   -6566.51477217835     
 iteration        -1650 MCMCOBJ=   -6566.65966481268     
 iteration        -1640 MCMCOBJ=   -6579.59469900560     
 iteration        -1630 MCMCOBJ=   -6560.40888488441     
 iteration        -1620 MCMCOBJ=   -6558.35858887809     
 iteration        -1610 MCMCOBJ=   -6572.14323414123     
 iteration        -1600 MCMCOBJ=   -6564.90071916173     
 iteration        -1590 MCMCOBJ=   -6555.86687768867     
 iteration        -1580 MCMCOBJ=   -6499.93043868507     
 iteration        -1570 MCMCOBJ=   -6572.52889812345     
 iteration        -1560 MCMCOBJ=   -6513.64750152913     
 iteration        -1550 MCMCOBJ=   -6517.00075908733     
 iteration        -1540 MCMCOBJ=   -6529.49092238084     
 iteration        -1530 MCMCOBJ=   -6556.57739046444     
 iteration        -1520 MCMCOBJ=   -6546.26529642948     
 iteration        -1510 MCMCOBJ=   -6520.70952728368     
 iteration        -1500 MCMCOBJ=   -6569.08974528193     
 iteration        -1490 MCMCOBJ=   -6551.83409414343     
 iteration        -1480 MCMCOBJ=   -6531.38817288529     
 iteration        -1470 MCMCOBJ=   -6527.91822754220     
 iteration        -1460 MCMCOBJ=   -6515.45861265219     
 iteration        -1450 MCMCOBJ=   -6536.11972606637     
 iteration        -1440 MCMCOBJ=   -6530.32066313550     
 iteration        -1430 MCMCOBJ=   -6534.32553264542     
 iteration        -1420 MCMCOBJ=   -6575.21888977460     
 iteration        -1410 MCMCOBJ=   -6510.52336579905     
 iteration        -1400 MCMCOBJ=   -6533.38660884476     
 iteration        -1390 MCMCOBJ=   -6545.56625270997     
 iteration        -1380 MCMCOBJ=   -6554.59873068142     
 iteration        -1370 MCMCOBJ=   -6531.47380187795     
 iteration        -1360 MCMCOBJ=   -6503.02593442535     
 iteration        -1350 MCMCOBJ=   -6513.42302762413     
 iteration        -1340 MCMCOBJ=   -6563.01437492388     
 iteration        -1330 MCMCOBJ=   -6616.35571900294     
 iteration        -1320 MCMCOBJ=   -6513.38573087409     
 iteration        -1310 MCMCOBJ=   -6523.00706521197     
 iteration        -1300 MCMCOBJ=   -6481.40558664685     
 iteration        -1290 MCMCOBJ=   -6479.83873841952     
 iteration        -1280 MCMCOBJ=   -6557.66129205225     
 iteration        -1270 MCMCOBJ=   -6518.98538599080     
 iteration        -1260 MCMCOBJ=   -6521.34878965892     
 iteration        -1250 MCMCOBJ=   -6576.79276213968     
 iteration        -1240 MCMCOBJ=   -6452.01701198600     
 iteration        -1230 MCMCOBJ=   -6510.81158747748     
 iteration        -1220 MCMCOBJ=   -6547.35443493749     
 iteration        -1210 MCMCOBJ=   -6530.59908115455     
 iteration        -1200 MCMCOBJ=   -6484.53207088729     
 iteration        -1190 MCMCOBJ=   -6575.80714208248     
 iteration        -1180 MCMCOBJ=   -6495.21062297290     
 iteration        -1170 MCMCOBJ=   -6507.00157411274     
 iteration        -1160 MCMCOBJ=   -6573.77307139690     
 iteration        -1150 MCMCOBJ=   -6520.83830514338     
 iteration        -1140 MCMCOBJ=   -6530.03624716770     
 iteration        -1130 MCMCOBJ=   -6491.01903310229     
 iteration        -1120 MCMCOBJ=   -6537.13805098444     
 iteration        -1110 MCMCOBJ=   -6486.72706966241     
 iteration        -1100 MCMCOBJ=   -6485.94195452145     
 iteration        -1090 MCMCOBJ=   -6560.19331650029     
 iteration        -1080 MCMCOBJ=   -6477.90201096384     
 iteration        -1070 MCMCOBJ=   -6518.25322293746     
 iteration        -1060 MCMCOBJ=   -6483.43650166605     
 iteration        -1050 MCMCOBJ=   -6544.50862540578     
 iteration        -1040 MCMCOBJ=   -6548.96148010529     
 iteration        -1030 MCMCOBJ=   -6476.43851754564     
 iteration        -1020 MCMCOBJ=   -6554.64004396089     
 iteration        -1010 MCMCOBJ=   -6481.38274504141     
 iteration        -1000 MCMCOBJ=   -6516.11566960282     
 iteration         -990 MCMCOBJ=   -6556.29332864208     
 iteration         -980 MCMCOBJ=   -6506.79433677215     
 iteration         -970 MCMCOBJ=   -6546.38986564432     
 iteration         -960 MCMCOBJ=   -6496.39612182027     
 iteration         -950 MCMCOBJ=   -6506.59481711131     
 iteration         -940 MCMCOBJ=   -6471.63446337845     
 iteration         -930 MCMCOBJ=   -6542.71772542401     
 iteration         -920 MCMCOBJ=   -6470.46434398177     
 iteration         -910 MCMCOBJ=   -6490.47524478084     
 iteration         -900 MCMCOBJ=   -6560.77779177385     
 iteration         -890 MCMCOBJ=   -6519.29596732777     
 iteration         -880 MCMCOBJ=   -6545.82934834774     
 iteration         -870 MCMCOBJ=   -6517.23761672358     
 iteration         -860 MCMCOBJ=   -6545.72926065206     
 iteration         -850 MCMCOBJ=   -6485.14770449669     
 iteration         -840 MCMCOBJ=   -6521.16458853379     
 iteration         -830 MCMCOBJ=   -6504.64561065547     
 iteration         -820 MCMCOBJ=   -6485.53798510570     
 iteration         -810 MCMCOBJ=   -6549.28888990244     
 iteration         -800 MCMCOBJ=   -6458.47408217241     
 iteration         -790 MCMCOBJ=   -6490.94316926732     
 iteration         -780 MCMCOBJ=   -6499.02610410545     
 iteration         -770 MCMCOBJ=   -6494.04325317996     
 iteration         -760 MCMCOBJ=   -6522.66610471756     
 iteration         -750 MCMCOBJ=   -6537.73121770392     
 iteration         -740 MCMCOBJ=   -6522.40826591947     
 iteration         -730 MCMCOBJ=   -6576.37105515858     
 iteration         -720 MCMCOBJ=   -6422.68440946180     
 iteration         -710 MCMCOBJ=   -6508.44118012106     
 iteration         -700 MCMCOBJ=   -6462.80406871644     
 iteration         -690 MCMCOBJ=   -6526.91281968367     
 iteration         -680 MCMCOBJ=   -6566.29582194363     
 iteration         -670 MCMCOBJ=   -6529.73708363888     
 iteration         -660 MCMCOBJ=   -6518.24776084963     
 iteration         -650 MCMCOBJ=   -6534.44138685295     
 iteration         -640 MCMCOBJ=   -6486.32181267744     
 iteration         -630 MCMCOBJ=   -6478.88873668630     
 iteration         -620 MCMCOBJ=   -6477.45445122416     
 iteration         -610 MCMCOBJ=   -6529.75532345773     
 iteration         -600 MCMCOBJ=   -6520.87582676927     
 iteration         -590 MCMCOBJ=   -6424.84262614305     
 iteration         -580 MCMCOBJ=   -6512.08941004006     
 iteration         -570 MCMCOBJ=   -6486.69787832089     
 iteration         -560 MCMCOBJ=   -6496.57760699349     
 iteration         -550 MCMCOBJ=   -6528.12635935829     
 iteration         -540 MCMCOBJ=   -6516.45445907514     
 iteration         -530 MCMCOBJ=   -6527.97272636751     
 iteration         -520 MCMCOBJ=   -6506.68338400527     
 iteration         -510 MCMCOBJ=   -6447.96276488971     
 iteration         -500 MCMCOBJ=   -6438.87655851021     
 iteration         -490 MCMCOBJ=   -6516.48731995799     
 iteration         -480 MCMCOBJ=   -6498.32962619246     
 iteration         -470 MCMCOBJ=   -6530.44641217795     
 iteration         -460 MCMCOBJ=   -6479.16947436248     
 iteration         -450 MCMCOBJ=   -6573.70482546160     
 iteration         -440 MCMCOBJ=   -6470.74821383555     
 iteration         -430 MCMCOBJ=   -6552.69297547469     
 iteration         -420 MCMCOBJ=   -6506.99008347458     
 iteration         -410 MCMCOBJ=   -6538.30577906741     
 iteration         -400 MCMCOBJ=   -6537.43898077525     
 iteration         -390 MCMCOBJ=   -6521.73306286143     
 iteration         -380 MCMCOBJ=   -6498.49847324573     
 iteration         -370 MCMCOBJ=   -6511.32745735101     
 iteration         -360 MCMCOBJ=   -6546.74144067785     
 iteration         -350 MCMCOBJ=   -6505.80384499991     
 iteration         -340 MCMCOBJ=   -6500.08879140606     
 iteration         -330 MCMCOBJ=   -6511.24030246249     
 iteration         -320 MCMCOBJ=   -6529.17074936200     
 iteration         -310 MCMCOBJ=   -6526.60357476685     
 iteration         -300 MCMCOBJ=   -6520.93894271352     
 iteration         -290 MCMCOBJ=   -6507.94177270135     
 iteration         -280 MCMCOBJ=   -6480.90049466130     
 iteration         -270 MCMCOBJ=   -6478.35865978908     
 iteration         -260 MCMCOBJ=   -6512.49146946047     
 iteration         -250 MCMCOBJ=   -6500.82444859122     
 iteration         -240 MCMCOBJ=   -6545.47380375247     
 iteration         -230 MCMCOBJ=   -6445.74805015252     
 iteration         -220 MCMCOBJ=   -6504.11178528618     
 iteration         -210 MCMCOBJ=   -6498.20177373228     
 iteration         -200 MCMCOBJ=   -6550.16820937693     
 iteration         -190 MCMCOBJ=   -6554.86386517807     
 iteration         -180 MCMCOBJ=   -6472.35633445782     
 iteration         -170 MCMCOBJ=   -6470.81006296949     
 iteration         -160 MCMCOBJ=   -6418.79588914700     
 iteration         -150 MCMCOBJ=   -6527.86642151506     
 iteration         -140 MCMCOBJ=   -6473.41404842022     
 iteration         -130 MCMCOBJ=   -6475.37321102237     
 iteration         -120 MCMCOBJ=   -6480.51276118408     
 iteration         -110 MCMCOBJ=   -6553.75827452229     
 iteration         -100 MCMCOBJ=   -6487.13157789810     
 iteration          -90 MCMCOBJ=   -6493.56670328140     
 iteration          -80 MCMCOBJ=   -6533.06997927766     
 iteration          -70 MCMCOBJ=   -6453.88276726269     
 iteration          -60 MCMCOBJ=   -6455.06761151818     
 iteration          -50 MCMCOBJ=   -6502.13372730746     
 iteration          -40 MCMCOBJ=   -6530.24995771477     
 iteration          -30 MCMCOBJ=   -6493.81100444928     
 iteration          -20 MCMCOBJ=   -6511.09445760868     
 iteration          -10 MCMCOBJ=   -6487.45291024507     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6480.29978627031     
 
 #TERM:
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 STATISTICAL PORTION WAS NOT PERFORMED
 #TERE:
 Elapsed estimation  time in seconds:   584.16
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6480.300       **************************************************
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
 
         3.91E+00 -2.33E+00  6.29E-01 -2.39E-01  2.32E+00  1.07E-01  3.77E+00 -6.01E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        3.51E-01
 
 ETA2
+       -1.93E-02  2.33E-01
 
 ETA3
+        1.29E-01 -2.83E-02  1.68E-01
 
 ETA4
+        5.25E-02  7.64E-02  7.56E-03  2.71E-01
 
 ETA5
+        1.00E-01  2.62E-02 -1.37E-02  2.77E-02  3.09E-01
 
 ETA6
+       -9.53E-02  3.62E-02 -6.28E-02 -1.27E-02 -1.22E-01  2.59E-01
 
 ETA7
+        1.36E-01 -8.58E-02  9.11E-02 -6.76E-02  6.12E-02 -7.16E-02  3.20E-01
 
 ETA8
+        2.25E-01  7.66E-02  1.14E-01  1.10E-01  1.22E-01 -1.46E-01  1.00E-01  3.31E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        8.13E-03
 
 EPS2
+        0.00E+00  2.47E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.92E-01
 
 ETA2
+       -6.75E-02  4.82E-01
 
 ETA3
+        5.32E-01 -1.43E-01  4.10E-01
 
 ETA4
+        1.71E-01  3.04E-01  3.55E-02  5.20E-01
 
 ETA5
+        3.05E-01  9.76E-02 -6.01E-02  9.60E-02  5.55E-01
 
 ETA6
+       -3.16E-01  1.47E-01 -3.01E-01 -4.81E-02 -4.31E-01  5.09E-01
 
 ETA7
+        4.06E-01 -3.14E-01  3.93E-01 -2.29E-01  1.95E-01 -2.48E-01  5.66E-01
 
 ETA8
+        6.59E-01  2.76E-01  4.83E-01  3.66E-01  3.81E-01 -4.97E-01  3.08E-01  5.75E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.02E-02
 
 EPS2
+        0.00E+00  1.57E-01
 
1
 
 
 #TBLN:      3
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
 RAW OUTPUT FILE (FILE): example6hmto.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
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
 NO U-TURN BAYES SAMPLING TYPE:                          TSOI
 THE FOLLOWING PERTAIN TO THETAS/OMEGAS/SIGMAS/ETAS
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      0
 MASS MATRIX ACCUMULATION ITERATIONS (PMADAPT):          500
 MASS MATRIX BLOCKING TYPE:                              B
 POP. MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (PNUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (PKAPPA):   0.750000000000000
 NUTS SAMPLE ACCEPTANCE RATE (PDELTA):                   0.800000000000000
 NUTS GAMMA SETTING (PGAMMA):                            5.000000000000000E-02
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 8.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 MODEL PARAMETER TRANSFORM. BY PRIORS (NUTS_REPARAM):    2

 
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
 iteration        -1000 MCMCOBJ=   -6537.19291051334     
 iteration         -999 MCMCOBJ=   -6537.19290822059     
 iteration         -998 MCMCOBJ=   -6599.57401303567     
 iteration         -997 MCMCOBJ=   -6407.82720336773     
 iteration         -996 MCMCOBJ=   -6598.34471711010     
 iteration         -995 MCMCOBJ=   -6687.04925606083     
 iteration         -994 MCMCOBJ=   -6701.29491937078     
 iteration         -993 MCMCOBJ=   -6655.00951380839     
 iteration         -992 MCMCOBJ=   -6558.64996555039     
 iteration         -991 MCMCOBJ=   -6486.46299524194     
 iteration         -990 MCMCOBJ=   -6659.32320757434     
 iteration         -989 MCMCOBJ=   -6638.92184875999     
 iteration         -988 MCMCOBJ=   -6683.44385682490     
 iteration         -987 MCMCOBJ=   -6751.43641313032     
 iteration         -986 MCMCOBJ=   -6607.86490821832     
 iteration         -985 MCMCOBJ=   -6624.40917023075     
 iteration         -984 MCMCOBJ=   -6552.69416648830     
 iteration         -983 MCMCOBJ=   -6609.77443931438     
 iteration         -982 MCMCOBJ=   -6579.45041109985     
 iteration         -981 MCMCOBJ=   -6579.45039483166     
 iteration         -980 MCMCOBJ=   -6679.87745372048     
 iteration         -979 MCMCOBJ=   -6591.81550841358     
 iteration         -978 MCMCOBJ=   -6626.89908059044     
 iteration         -977 MCMCOBJ=   -6674.80528916904     
 iteration         -976 MCMCOBJ=   -6700.77772765956     
 iteration         -975 MCMCOBJ=   -6623.89739393250     
 iteration         -974 MCMCOBJ=   -6638.27674520381     
 iteration         -973 MCMCOBJ=   -6692.98391680012     
 iteration         -972 MCMCOBJ=   -6615.28534322842     
 iteration         -971 MCMCOBJ=   -6546.20940125475     
 iteration         -970 MCMCOBJ=   -6643.88436692840     
 iteration         -969 MCMCOBJ=   -6545.02196831880     
 iteration         -968 MCMCOBJ=   -6472.23629452208     
 iteration         -967 MCMCOBJ=   -6444.62507090799     
 iteration         -966 MCMCOBJ=   -6428.53375178846     
 iteration         -965 MCMCOBJ=   -6498.61521843844     
 iteration         -964 MCMCOBJ=   -6572.97435494327     
 iteration         -963 MCMCOBJ=   -6663.10918667570     
 iteration         -962 MCMCOBJ=   -6575.30659574865     
 iteration         -961 MCMCOBJ=   -6611.54529991528     
 iteration         -960 MCMCOBJ=   -6609.14783421999     
 iteration         -959 MCMCOBJ=   -6614.15407652754     
 iteration         -958 MCMCOBJ=   -6560.25166821672     
 iteration         -957 MCMCOBJ=   -6659.38634214843     
 iteration         -956 MCMCOBJ=   -6604.09414640629     
 iteration         -955 MCMCOBJ=   -6620.54452405034     
 iteration         -954 MCMCOBJ=   -6634.62793019372     
 iteration         -953 MCMCOBJ=   -6674.64551126629     
 iteration         -952 MCMCOBJ=   -6600.46807758936     
 iteration         -951 MCMCOBJ=   -6558.63361699441     
 iteration         -950 MCMCOBJ=   -6574.73049370849     
 iteration         -949 MCMCOBJ=   -6534.73732471138     
 iteration         -948 MCMCOBJ=   -6507.67716958371     
 iteration         -947 MCMCOBJ=   -6531.45261244088     
 iteration         -946 MCMCOBJ=   -6612.19976526736     
 iteration         -945 MCMCOBJ=   -6576.39386283587     
 iteration         -944 MCMCOBJ=   -6460.19892498063     
 iteration         -943 MCMCOBJ=   -6402.99309469174     
 iteration         -942 MCMCOBJ=   -6402.99309550574     
 iteration         -941 MCMCOBJ=   -6365.73313006355     
 iteration         -940 MCMCOBJ=   -6346.52436610683     
 iteration         -939 MCMCOBJ=   -6398.57204798221     
 iteration         -938 MCMCOBJ=   -6660.84085285315     
 iteration         -937 MCMCOBJ=   -6601.84873854395     
 iteration         -936 MCMCOBJ=   -6601.84873666826     
 iteration         -935 MCMCOBJ=   -6527.84435561039     
 iteration         -934 MCMCOBJ=   -6625.04569128634     
 iteration         -933 MCMCOBJ=   -6599.64982379277     
 iteration         -932 MCMCOBJ=   -6610.58670626225     
 iteration         -931 MCMCOBJ=   -6658.81758253403     
 iteration         -930 MCMCOBJ=   -6595.16187704858     
 iteration         -929 MCMCOBJ=   -6554.62781274471     
 iteration         -928 MCMCOBJ=   -6545.08497480372     
 iteration         -927 MCMCOBJ=   -6549.63196329749     
 iteration         -926 MCMCOBJ=   -6520.90554222560     
 iteration         -925 MCMCOBJ=   -6574.37966415613     
 iteration         -924 MCMCOBJ=   -6583.70061062084     
 iteration         -923 MCMCOBJ=   -6588.01034586456     
 iteration         -922 MCMCOBJ=   -6652.85007931815     
 iteration         -921 MCMCOBJ=   -6607.04360746814     
 iteration         -920 MCMCOBJ=   -6540.88655447715     
 iteration         -919 MCMCOBJ=   -6551.12065758708     
 iteration         -918 MCMCOBJ=   -6566.87460861035     
 iteration         -917 MCMCOBJ=   -6566.87460652755     
 iteration         -916 MCMCOBJ=   -6497.71890989928     
 iteration         -915 MCMCOBJ=   -6566.58175445416     
 iteration         -914 MCMCOBJ=   -6553.69652412728     
 iteration         -913 MCMCOBJ=   -6600.06806569623     
 iteration         -912 MCMCOBJ=   -6585.61804214143     
 iteration         -911 MCMCOBJ=   -6585.61803698502     
 iteration         -910 MCMCOBJ=   -6622.76727068731     
 iteration         -909 MCMCOBJ=   -6634.04212544989     
 iteration         -908 MCMCOBJ=   -6622.25733869416     
 iteration         -907 MCMCOBJ=   -6555.65376801906     
 iteration         -906 MCMCOBJ=   -6582.10425439457     
 iteration         -905 MCMCOBJ=   -6588.13584557672     
 iteration         -904 MCMCOBJ=   -6581.42606324398     
 iteration         -903 MCMCOBJ=   -6523.49931259542     
 iteration         -902 MCMCOBJ=   -6579.05594473814     
 iteration         -901 MCMCOBJ=   -6538.25720913977     
 iteration         -900 MCMCOBJ=   -6584.04331238857     
 iteration         -899 MCMCOBJ=   -6595.72893596048     
 iteration         -898 MCMCOBJ=   -6576.78515653004     
 iteration         -897 MCMCOBJ=   -6574.70050242271     
 iteration         -896 MCMCOBJ=   -6539.68801738704     
 iteration         -895 MCMCOBJ=   -6619.18931212832     
 iteration         -894 MCMCOBJ=   -6543.00989468095     
 iteration         -893 MCMCOBJ=   -6568.90848955226     
 iteration         -892 MCMCOBJ=   -6579.85730726965     
 iteration         -891 MCMCOBJ=   -6695.94329301313     
 iteration         -890 MCMCOBJ=   -6689.93042608185     
 iteration         -889 MCMCOBJ=   -6708.16344776875     
 iteration         -888 MCMCOBJ=   -6661.74848761896     
 iteration         -887 MCMCOBJ=   -6664.90468271656     
 iteration         -886 MCMCOBJ=   -6617.80549249939     
 iteration         -885 MCMCOBJ=   -6577.66600269024     
 iteration         -884 MCMCOBJ=   -6534.80131570224     
 iteration         -883 MCMCOBJ=   -6578.98957831869     
 iteration         -882 MCMCOBJ=   -6624.34327113032     
 iteration         -881 MCMCOBJ=   -6638.39837154298     
 iteration         -880 MCMCOBJ=   -6654.04716884952     
 iteration         -879 MCMCOBJ=   -6553.75929289733     
 iteration         -878 MCMCOBJ=   -6510.66419528696     
 iteration         -877 MCMCOBJ=   -6586.13768334714     
 iteration         -876 MCMCOBJ=   -6580.75454389567     
 iteration         -875 MCMCOBJ=   -6611.25563444743     
 iteration         -874 MCMCOBJ=   -6632.54851914912     
 iteration         -873 MCMCOBJ=   -6620.36547449141     
 iteration         -872 MCMCOBJ=   -6593.16078182812     
 iteration         -871 MCMCOBJ=   -6524.90080403045     
 iteration         -870 MCMCOBJ=   -6531.52181046741     
 iteration         -869 MCMCOBJ=   -6563.31655654405     
 iteration         -868 MCMCOBJ=   -6629.67613451089     
 iteration         -867 MCMCOBJ=   -6663.27841645078     
 iteration         -866 MCMCOBJ=   -6695.26247561418     
 iteration         -865 MCMCOBJ=   -6695.26247725960     
 iteration         -864 MCMCOBJ=   -6597.54413269261     
 iteration         -863 MCMCOBJ=   -6554.53172334007     
 iteration         -862 MCMCOBJ=   -6573.71503901211     
 iteration         -861 MCMCOBJ=   -6544.23090759666     
 iteration         -860 MCMCOBJ=   -6512.11597863560     
 iteration         -859 MCMCOBJ=   -6581.94812959863     
 iteration         -858 MCMCOBJ=   -6478.81493874267     
 iteration         -857 MCMCOBJ=   -6478.81493876125     
 iteration         -856 MCMCOBJ=   -6472.81196999115     
 iteration         -855 MCMCOBJ=   -6514.87168898707     
 iteration         -854 MCMCOBJ=   -6583.21986403441     
 iteration         -853 MCMCOBJ=   -6605.75228396018     
 iteration         -852 MCMCOBJ=   -6576.70906007143     
 iteration         -851 MCMCOBJ=   -6600.64570913609     
 iteration         -850 MCMCOBJ=   -6686.31782078725     
 iteration         -849 MCMCOBJ=   -6585.77845780227     
 iteration         -848 MCMCOBJ=   -6551.98567696159     
 iteration         -847 MCMCOBJ=   -6650.87568087516     
 iteration         -846 MCMCOBJ=   -6576.43170091393     
 iteration         -845 MCMCOBJ=   -6556.47368060707     
 iteration         -844 MCMCOBJ=   -6614.35089065263     
 iteration         -843 MCMCOBJ=   -6594.54135744003     
 iteration         -842 MCMCOBJ=   -6600.10776017462     
 iteration         -841 MCMCOBJ=   -6519.35428071944     
 iteration         -840 MCMCOBJ=   -6586.92817390609     
 iteration         -839 MCMCOBJ=   -6492.00258195639     
 iteration         -838 MCMCOBJ=   -6509.85918854716     
 iteration         -837 MCMCOBJ=   -6545.77890503547     
 iteration         -836 MCMCOBJ=   -6639.90072000186     
 iteration         -835 MCMCOBJ=   -6652.24663142322     
 iteration         -834 MCMCOBJ=   -6579.91473662700     
 iteration         -833 MCMCOBJ=   -6616.00606727065     
 iteration         -832 MCMCOBJ=   -6649.64064015728     
 iteration         -831 MCMCOBJ=   -6533.57778569196     
 iteration         -830 MCMCOBJ=   -6500.20212931787     
 iteration         -829 MCMCOBJ=   -6489.09357530169     
 iteration         -828 MCMCOBJ=   -6513.52870809315     
 iteration         -827 MCMCOBJ=   -6524.86744609090     
 iteration         -826 MCMCOBJ=   -6482.15678317516     
 iteration         -825 MCMCOBJ=   -6528.18515657837     
 iteration         -824 MCMCOBJ=   -6502.16289954796     
 iteration         -823 MCMCOBJ=   -6516.55056269841     
 iteration         -822 MCMCOBJ=   -6601.52153369688     
 iteration         -821 MCMCOBJ=   -6618.67550006035     
 iteration         -820 MCMCOBJ=   -6640.11916600934     
 iteration         -819 MCMCOBJ=   -6667.16003869292     
 iteration         -818 MCMCOBJ=   -6616.72531935750     
 iteration         -817 MCMCOBJ=   -6607.89141237901     
 iteration         -816 MCMCOBJ=   -6619.19031675987     
 iteration         -815 MCMCOBJ=   -6664.11262286287     
 iteration         -814 MCMCOBJ=   -6636.35224368769     
 iteration         -813 MCMCOBJ=   -6522.24362554019     
 iteration         -812 MCMCOBJ=   -6592.69255563950     
 iteration         -811 MCMCOBJ=   -6505.40843958994     
 iteration         -810 MCMCOBJ=   -6526.05416576368     
 iteration         -809 MCMCOBJ=   -6548.22478259537     
 iteration         -808 MCMCOBJ=   -6585.92403341840     
 iteration         -807 MCMCOBJ=   -6567.79102125723     
 iteration         -806 MCMCOBJ=   -6625.56814737537     
 iteration         -805 MCMCOBJ=   -6587.06623613650     
 iteration         -804 MCMCOBJ=   -6578.22187344916     
 iteration         -803 MCMCOBJ=   -6609.00931208249     
 iteration         -802 MCMCOBJ=   -6619.66978603462     
 iteration         -801 MCMCOBJ=   -6662.13025479151     
 iteration         -800 MCMCOBJ=   -6664.02932480299     
 iteration         -799 MCMCOBJ=   -6674.52830854505     
 iteration         -798 MCMCOBJ=   -6709.47538964608     
 iteration         -797 MCMCOBJ=   -6669.74234303269     
 iteration         -796 MCMCOBJ=   -6671.49349498624     
 iteration         -795 MCMCOBJ=   -6665.60449374022     
 iteration         -794 MCMCOBJ=   -6654.89437194812     
 iteration         -793 MCMCOBJ=   -6677.80124777986     
 iteration         -792 MCMCOBJ=   -6614.70288660265     
 iteration         -791 MCMCOBJ=   -6576.96072650511     
 iteration         -790 MCMCOBJ=   -6547.43140896537     
 iteration         -789 MCMCOBJ=   -6528.21178328971     
 iteration         -788 MCMCOBJ=   -6620.89362013612     
 iteration         -787 MCMCOBJ=   -6626.84412869600     
 iteration         -786 MCMCOBJ=   -6634.30615007130     
 iteration         -785 MCMCOBJ=   -6597.21742055397     
 iteration         -784 MCMCOBJ=   -6564.31640717326     
 iteration         -783 MCMCOBJ=   -6593.46532387934     
 iteration         -782 MCMCOBJ=   -6593.27120893082     
 iteration         -781 MCMCOBJ=   -6645.45880020798     
 iteration         -780 MCMCOBJ=   -6633.85250251418     
 iteration         -779 MCMCOBJ=   -6579.74174116521     
 iteration         -778 MCMCOBJ=   -6619.11321565742     
 iteration         -777 MCMCOBJ=   -6510.86942098903     
 iteration         -776 MCMCOBJ=   -6578.17133100923     
 iteration         -775 MCMCOBJ=   -6641.51987489290     
 iteration         -774 MCMCOBJ=   -6604.48574886078     
 iteration         -773 MCMCOBJ=   -6611.20288138443     
 iteration         -772 MCMCOBJ=   -6550.64636496994     
 iteration         -771 MCMCOBJ=   -6514.28789455257     
 iteration         -770 MCMCOBJ=   -6560.14345470107     
 iteration         -769 MCMCOBJ=   -6597.65464160560     
 iteration         -768 MCMCOBJ=   -6518.10177832076     
 iteration         -767 MCMCOBJ=   -6530.47502544841     
 iteration         -766 MCMCOBJ=   -6523.48340250603     
 iteration         -765 MCMCOBJ=   -6570.62908011606     
 iteration         -764 MCMCOBJ=   -6546.15997284055     
 iteration         -763 MCMCOBJ=   -6527.34091813617     
 iteration         -762 MCMCOBJ=   -6576.41693664562     
 iteration         -761 MCMCOBJ=   -6556.00305069469     
 iteration         -760 MCMCOBJ=   -6548.72597169805     
 iteration         -759 MCMCOBJ=   -6611.61898931712     
 iteration         -758 MCMCOBJ=   -6611.61898831443     
 iteration         -757 MCMCOBJ=   -6578.59298368265     
 iteration         -756 MCMCOBJ=   -6649.28375590413     
 iteration         -755 MCMCOBJ=   -6610.68453191051     
 iteration         -754 MCMCOBJ=   -6604.30159964829     
 iteration         -753 MCMCOBJ=   -6540.04115426341     
 iteration         -752 MCMCOBJ=   -6632.12587757738     
 iteration         -751 MCMCOBJ=   -6666.52139811864     
 iteration         -750 MCMCOBJ=   -6671.41157997610     
 iteration         -749 MCMCOBJ=   -6589.21615006974     
 iteration         -748 MCMCOBJ=   -6579.97158679140     
 iteration         -747 MCMCOBJ=   -6563.21840401314     
 iteration         -746 MCMCOBJ=   -6552.03153309342     
 iteration         -745 MCMCOBJ=   -6525.16519178889     
 iteration         -744 MCMCOBJ=   -6545.65755483647     
 iteration         -743 MCMCOBJ=   -6567.47458414127     
 iteration         -742 MCMCOBJ=   -6545.90648775631     
 iteration         -741 MCMCOBJ=   -6508.06836248137     
 iteration         -740 MCMCOBJ=   -6534.47831354601     
 iteration         -739 MCMCOBJ=   -6509.77151267329     
 iteration         -738 MCMCOBJ=   -6620.40571117651     
 iteration         -737 MCMCOBJ=   -6595.71825559923     
 iteration         -736 MCMCOBJ=   -6554.91044029805     
 iteration         -735 MCMCOBJ=   -6617.25324984661     
 iteration         -734 MCMCOBJ=   -6585.31997130990     
 iteration         -733 MCMCOBJ=   -6631.23112841757     
 iteration         -732 MCMCOBJ=   -6604.93836718246     
 iteration         -731 MCMCOBJ=   -6597.54423294034     
 iteration         -730 MCMCOBJ=   -6611.20508644252     
 iteration         -729 MCMCOBJ=   -6597.49034784369     
 iteration         -728 MCMCOBJ=   -6604.13943064750     
 iteration         -727 MCMCOBJ=   -6614.78980389196     
 iteration         -726 MCMCOBJ=   -6619.34575303321     
 iteration         -725 MCMCOBJ=   -6620.46511563163     
 iteration         -724 MCMCOBJ=   -6641.44281304432     
 iteration         -723 MCMCOBJ=   -6679.61925270953     
 iteration         -722 MCMCOBJ=   -6657.02141489124     
 iteration         -721 MCMCOBJ=   -6662.09811301873     
 iteration         -720 MCMCOBJ=   -6633.01453264404     
 iteration         -719 MCMCOBJ=   -6702.24220851721     
 iteration         -718 MCMCOBJ=   -6623.42478633331     
 iteration         -717 MCMCOBJ=   -6559.92163900336     
 iteration         -716 MCMCOBJ=   -6671.96278105740     
 iteration         -715 MCMCOBJ=   -6663.65340657721     
 iteration         -714 MCMCOBJ=   -6644.87279701469     
 iteration         -713 MCMCOBJ=   -6605.17178557472     
 iteration         -712 MCMCOBJ=   -6637.26005049430     
 iteration         -711 MCMCOBJ=   -6652.33239621967     
 iteration         -710 MCMCOBJ=   -6691.59707676439     
 iteration         -709 MCMCOBJ=   -6684.82518764100     
 iteration         -708 MCMCOBJ=   -6652.79578797460     
 iteration         -707 MCMCOBJ=   -6696.35292986479     
 iteration         -706 MCMCOBJ=   -6707.69222483656     
 iteration         -705 MCMCOBJ=   -6683.43905483755     
 iteration         -704 MCMCOBJ=   -6609.87466890394     
 iteration         -703 MCMCOBJ=   -6593.78129746696     
 iteration         -702 MCMCOBJ=   -6603.55752457699     
 iteration         -701 MCMCOBJ=   -6528.69168805069     
 iteration         -700 MCMCOBJ=   -6624.37735987772     
 iteration         -699 MCMCOBJ=   -6686.87137185859     
 iteration         -698 MCMCOBJ=   -6711.81435498100     
 iteration         -697 MCMCOBJ=   -6673.80875397705     
 iteration         -696 MCMCOBJ=   -6672.07439989206     
 iteration         -695 MCMCOBJ=   -6617.44503313829     
 iteration         -694 MCMCOBJ=   -6607.32375829650     
 iteration         -693 MCMCOBJ=   -6632.82475234648     
 iteration         -692 MCMCOBJ=   -6527.70907055253     
 iteration         -691 MCMCOBJ=   -6660.26649797628     
 iteration         -690 MCMCOBJ=   -6635.49316064881     
 iteration         -689 MCMCOBJ=   -6647.43967022922     
 iteration         -688 MCMCOBJ=   -6593.63453238098     
 iteration         -687 MCMCOBJ=   -6550.58241355626     
 iteration         -686 MCMCOBJ=   -6682.50982919536     
 iteration         -685 MCMCOBJ=   -6664.21999516318     
 iteration         -684 MCMCOBJ=   -6675.99467516768     
 iteration         -683 MCMCOBJ=   -6578.10309234432     
 iteration         -682 MCMCOBJ=   -6578.89906700249     
 iteration         -681 MCMCOBJ=   -6669.38754175957     
 iteration         -680 MCMCOBJ=   -6617.07133808987     
 iteration         -679 MCMCOBJ=   -6604.24467995958     
 iteration         -678 MCMCOBJ=   -6559.05339328402     
 iteration         -677 MCMCOBJ=   -6589.16381680310     
 iteration         -676 MCMCOBJ=   -6629.69714605809     
 iteration         -675 MCMCOBJ=   -6598.08913640817     
 iteration         -674 MCMCOBJ=   -6636.77238713612     
 iteration         -673 MCMCOBJ=   -6669.73857648794     
 iteration         -672 MCMCOBJ=   -6662.71511185407     
 iteration         -671 MCMCOBJ=   -6652.39085954852     
 iteration         -670 MCMCOBJ=   -6680.43640892659     
 iteration         -669 MCMCOBJ=   -6577.63205159466     
 iteration         -668 MCMCOBJ=   -6614.99739348748     
 iteration         -667 MCMCOBJ=   -6598.13770309119     
 iteration         -666 MCMCOBJ=   -6656.92954442266     
 iteration         -665 MCMCOBJ=   -6607.31264564602     
 iteration         -664 MCMCOBJ=   -6589.52479281821     
 iteration         -663 MCMCOBJ=   -6604.53783862498     
 iteration         -662 MCMCOBJ=   -6599.98511710945     
 iteration         -661 MCMCOBJ=   -6583.27712276494     
 iteration         -660 MCMCOBJ=   -6582.81124766244     
 iteration         -659 MCMCOBJ=   -6631.56802024275     
 iteration         -658 MCMCOBJ=   -6625.52468773149     
 iteration         -657 MCMCOBJ=   -6549.71758171495     
 iteration         -656 MCMCOBJ=   -6490.44134133398     
 iteration         -655 MCMCOBJ=   -6557.74017374061     
 iteration         -654 MCMCOBJ=   -6614.21536506996     
 iteration         -653 MCMCOBJ=   -6562.09131484983     
 iteration         -652 MCMCOBJ=   -6613.84333884905     
 iteration         -651 MCMCOBJ=   -6664.82758549484     
 iteration         -650 MCMCOBJ=   -6650.26428810517     
 iteration         -649 MCMCOBJ=   -6643.31938316941     
 iteration         -648 MCMCOBJ=   -6616.27772443971     
 iteration         -647 MCMCOBJ=   -6627.03028148055     
 iteration         -646 MCMCOBJ=   -6664.26239656887     
 iteration         -645 MCMCOBJ=   -6663.97542636751     
 iteration         -644 MCMCOBJ=   -6682.42568750174     
 iteration         -643 MCMCOBJ=   -6660.96880549952     
 iteration         -642 MCMCOBJ=   -6570.86229013283     
 iteration         -641 MCMCOBJ=   -6627.10737211370     
 iteration         -640 MCMCOBJ=   -6633.24688186707     
 iteration         -639 MCMCOBJ=   -6598.48565892322     
 iteration         -638 MCMCOBJ=   -6624.31620005365     
 iteration         -637 MCMCOBJ=   -6664.69588881173     
 iteration         -636 MCMCOBJ=   -6677.62939778066     
 iteration         -635 MCMCOBJ=   -6656.77678979785     
 iteration         -634 MCMCOBJ=   -6592.89086828799     
 iteration         -633 MCMCOBJ=   -6584.63142647385     
 iteration         -632 MCMCOBJ=   -6602.36938161018     
 iteration         -631 MCMCOBJ=   -6648.52505403252     
 iteration         -630 MCMCOBJ=   -6609.92109756063     
 iteration         -629 MCMCOBJ=   -6534.29046934364     
 iteration         -628 MCMCOBJ=   -6585.69272821495     
 iteration         -627 MCMCOBJ=   -6607.89450760821     
 iteration         -626 MCMCOBJ=   -6602.10337461246     
 iteration         -625 MCMCOBJ=   -6571.18665447201     
 iteration         -624 MCMCOBJ=   -6593.00038087306     
 iteration         -623 MCMCOBJ=   -6680.26426820969     
 iteration         -622 MCMCOBJ=   -6590.32585049714     
 iteration         -621 MCMCOBJ=   -6547.90330557424     
 iteration         -620 MCMCOBJ=   -6585.62757993705     
 iteration         -619 MCMCOBJ=   -6527.54605118708     
 iteration         -618 MCMCOBJ=   -6582.32720659044     
 iteration         -617 MCMCOBJ=   -6636.28095837777     
 iteration         -616 MCMCOBJ=   -6636.67795432055     
 iteration         -615 MCMCOBJ=   -6592.93224784885     
 iteration         -614 MCMCOBJ=   -6601.73215093766     
 iteration         -613 MCMCOBJ=   -6604.04074934172     
 iteration         -612 MCMCOBJ=   -6580.27915625482     
 iteration         -611 MCMCOBJ=   -6609.01399399260     
 iteration         -610 MCMCOBJ=   -6594.00121453444     
 iteration         -609 MCMCOBJ=   -6672.29781378348     
 iteration         -608 MCMCOBJ=   -6607.46166095777     
 iteration         -607 MCMCOBJ=   -6665.58354210730     
 iteration         -606 MCMCOBJ=   -6648.95217917647     
 iteration         -605 MCMCOBJ=   -6658.84163791432     
 iteration         -604 MCMCOBJ=   -6643.74982443465     
 iteration         -603 MCMCOBJ=   -6638.68168152697     
 iteration         -602 MCMCOBJ=   -6613.18437468690     
 iteration         -601 MCMCOBJ=   -6591.76095986839     
 iteration         -600 MCMCOBJ=   -6630.32155283290     
 iteration         -599 MCMCOBJ=   -6595.45289851547     
 iteration         -598 MCMCOBJ=   -6569.90553722693     
 iteration         -597 MCMCOBJ=   -6533.17429832643     
 iteration         -596 MCMCOBJ=   -6511.96659537488     
 iteration         -595 MCMCOBJ=   -6610.86567065445     
 iteration         -594 MCMCOBJ=   -6602.67838738167     
 iteration         -593 MCMCOBJ=   -6602.47675057657     
 iteration         -592 MCMCOBJ=   -6470.67181317896     
 iteration         -591 MCMCOBJ=   -6495.64580167806     
 iteration         -590 MCMCOBJ=   -6520.20625238726     
 iteration         -589 MCMCOBJ=   -6644.83516083930     
 iteration         -588 MCMCOBJ=   -6662.75225646600     
 iteration         -587 MCMCOBJ=   -6591.84209597281     
 iteration         -586 MCMCOBJ=   -6558.50262476749     
 iteration         -585 MCMCOBJ=   -6598.81659948872     
 iteration         -584 MCMCOBJ=   -6644.48736215340     
 iteration         -583 MCMCOBJ=   -6647.36688394027     
 iteration         -582 MCMCOBJ=   -6653.57166564387     
 iteration         -581 MCMCOBJ=   -6619.13855786071     
 iteration         -580 MCMCOBJ=   -6621.26987371239     
 iteration         -579 MCMCOBJ=   -6614.39513793036     
 iteration         -578 MCMCOBJ=   -6674.50675808994     
 iteration         -577 MCMCOBJ=   -6617.63013569456     
 iteration         -576 MCMCOBJ=   -6611.30701554779     
 iteration         -575 MCMCOBJ=   -6566.56557207420     
 iteration         -574 MCMCOBJ=   -6604.29758335194     
 iteration         -573 MCMCOBJ=   -6553.83293722438     
 iteration         -572 MCMCOBJ=   -6575.59158155386     
 iteration         -571 MCMCOBJ=   -6577.75754113115     
 iteration         -570 MCMCOBJ=   -6498.30678773817     
 iteration         -569 MCMCOBJ=   -6500.10563231129     
 iteration         -568 MCMCOBJ=   -6503.11549401751     
 iteration         -567 MCMCOBJ=   -6602.26800513389     
 iteration         -566 MCMCOBJ=   -6529.52955208342     
 iteration         -565 MCMCOBJ=   -6515.31061591771     
 iteration         -564 MCMCOBJ=   -6619.24297396588     
 iteration         -563 MCMCOBJ=   -6684.43091679613     
 iteration         -562 MCMCOBJ=   -6623.71946147220     
 iteration         -561 MCMCOBJ=   -6663.17369078282     
 iteration         -560 MCMCOBJ=   -6660.28627908979     
 iteration         -559 MCMCOBJ=   -6606.30663966901     
 iteration         -558 MCMCOBJ=   -6582.35846519057     
 iteration         -557 MCMCOBJ=   -6634.32271020920     
 iteration         -556 MCMCOBJ=   -6620.44950706407     
 iteration         -555 MCMCOBJ=   -6611.38982001560     
 iteration         -554 MCMCOBJ=   -6638.04738130021     
 iteration         -553 MCMCOBJ=   -6637.63323527397     
 iteration         -552 MCMCOBJ=   -6663.35456890490     
 iteration         -551 MCMCOBJ=   -6585.12631627909     
 iteration         -550 MCMCOBJ=   -6631.47276414710     
 iteration         -549 MCMCOBJ=   -6603.99397674647     
 iteration         -548 MCMCOBJ=   -6584.37866543717     
 iteration         -547 MCMCOBJ=   -6550.14854466704     
 iteration         -546 MCMCOBJ=   -6592.29193721079     
 iteration         -545 MCMCOBJ=   -6627.85631807441     
 iteration         -544 MCMCOBJ=   -6590.57430783068     
 iteration         -543 MCMCOBJ=   -6566.94285581314     
 iteration         -542 MCMCOBJ=   -6586.02675976017     
 iteration         -541 MCMCOBJ=   -6579.19556818835     
 iteration         -540 MCMCOBJ=   -6592.74035847239     
 iteration         -539 MCMCOBJ=   -6594.43378104271     
 iteration         -538 MCMCOBJ=   -6528.23190490513     
 iteration         -537 MCMCOBJ=   -6550.55631544233     
 iteration         -536 MCMCOBJ=   -6599.07290829471     
 iteration         -535 MCMCOBJ=   -6522.12033529047     
 iteration         -534 MCMCOBJ=   -6538.55454158130     
 iteration         -533 MCMCOBJ=   -6588.07552597043     
 iteration         -532 MCMCOBJ=   -6518.37394329907     
 iteration         -531 MCMCOBJ=   -6571.16783962568     
 iteration         -530 MCMCOBJ=   -6593.27226593349     
 iteration         -529 MCMCOBJ=   -6602.93468643606     
 iteration         -528 MCMCOBJ=   -6610.96674012472     
 iteration         -527 MCMCOBJ=   -6612.63876599852     
 iteration         -526 MCMCOBJ=   -6553.45363475047     
 iteration         -525 MCMCOBJ=   -6519.24018520016     
 iteration         -524 MCMCOBJ=   -6643.96980938385     
 iteration         -523 MCMCOBJ=   -6562.24197652691     
 iteration         -522 MCMCOBJ=   -6500.80383401562     
 iteration         -521 MCMCOBJ=   -6565.44878156261     
 iteration         -520 MCMCOBJ=   -6566.85935811268     
 iteration         -519 MCMCOBJ=   -6585.13181741673     
 iteration         -518 MCMCOBJ=   -6586.40402440275     
 iteration         -517 MCMCOBJ=   -6555.93525013721     
 iteration         -516 MCMCOBJ=   -6601.53252078597     
 iteration         -515 MCMCOBJ=   -6570.99719902507     
 iteration         -514 MCMCOBJ=   -6596.76083180777     
 iteration         -513 MCMCOBJ=   -6621.44249824611     
 iteration         -512 MCMCOBJ=   -6613.14030237144     
 iteration         -511 MCMCOBJ=   -6632.61486127156     
 iteration         -510 MCMCOBJ=   -6658.67172828448     
 iteration         -509 MCMCOBJ=   -6649.43841517272     
 iteration         -508 MCMCOBJ=   -6664.00587714370     
 iteration         -507 MCMCOBJ=   -6634.86196399829     
 iteration         -506 MCMCOBJ=   -6655.93558429018     
 iteration         -505 MCMCOBJ=   -6537.04279392199     
 iteration         -504 MCMCOBJ=   -6592.56599989086     
 iteration         -503 MCMCOBJ=   -6572.62192333051     
 iteration         -502 MCMCOBJ=   -6587.81603946142     
 iteration         -501 MCMCOBJ=   -6583.68704399023     
 iteration         -500 MCMCOBJ=   -6565.32708230454     
 iteration         -499 MCMCOBJ=   -6611.13009141198     
 iteration         -498 MCMCOBJ=   -6625.21148002484     
 iteration         -497 MCMCOBJ=   -6639.44535036063     
 iteration         -496 MCMCOBJ=   -6602.32389557264     
 iteration         -495 MCMCOBJ=   -6558.28903964703     
 iteration         -494 MCMCOBJ=   -6597.22370492220     
 iteration         -493 MCMCOBJ=   -6606.27837937710     
 iteration         -492 MCMCOBJ=   -6577.85516395239     
 iteration         -491 MCMCOBJ=   -6680.68294211206     
 iteration         -490 MCMCOBJ=   -6652.64991637206     
 iteration         -489 MCMCOBJ=   -6629.74954803572     
 iteration         -488 MCMCOBJ=   -6626.92028015573     
 iteration         -487 MCMCOBJ=   -6586.05767047765     
 iteration         -486 MCMCOBJ=   -6580.27114207789     
 iteration         -485 MCMCOBJ=   -6607.44439493369     
 iteration         -484 MCMCOBJ=   -6585.19335849393     
 iteration         -483 MCMCOBJ=   -6603.51935344713     
 iteration         -482 MCMCOBJ=   -6585.76536694616     
 iteration         -481 MCMCOBJ=   -6681.91161106510     
 iteration         -480 MCMCOBJ=   -6691.53282071367     
 iteration         -479 MCMCOBJ=   -6637.51413025409     
 iteration         -478 MCMCOBJ=   -6615.45621911116     
 iteration         -477 MCMCOBJ=   -6605.16803563317     
 iteration         -476 MCMCOBJ=   -6605.97154620702     
 iteration         -475 MCMCOBJ=   -6606.87957706011     
 iteration         -474 MCMCOBJ=   -6545.05554403863     
 iteration         -473 MCMCOBJ=   -6564.29668226804     
 iteration         -472 MCMCOBJ=   -6596.69445660614     
 iteration         -471 MCMCOBJ=   -6633.14061450287     
 iteration         -470 MCMCOBJ=   -6612.91336819113     
 iteration         -469 MCMCOBJ=   -6622.64764844638     
 iteration         -468 MCMCOBJ=   -6622.64765053826     
 iteration         -467 MCMCOBJ=   -6630.50274863221     
 iteration         -466 MCMCOBJ=   -6610.19947172701     
 iteration         -465 MCMCOBJ=   -6643.49275697917     
 iteration         -464 MCMCOBJ=   -6580.18605447985     
 iteration         -463 MCMCOBJ=   -6567.83106623388     
 iteration         -462 MCMCOBJ=   -6633.76064976212     
 iteration         -461 MCMCOBJ=   -6606.93786882838     
 iteration         -460 MCMCOBJ=   -6626.68726782905     
 iteration         -459 MCMCOBJ=   -6649.02996714161     
 iteration         -458 MCMCOBJ=   -6651.44336127399     
 iteration         -457 MCMCOBJ=   -6660.72879353356     
 iteration         -456 MCMCOBJ=   -6676.23077807506     
 iteration         -455 MCMCOBJ=   -6646.17397000564     
 iteration         -454 MCMCOBJ=   -6690.95710501086     
 iteration         -453 MCMCOBJ=   -6690.95710080845     
 iteration         -452 MCMCOBJ=   -6661.52253182600     
 iteration         -451 MCMCOBJ=   -6652.60960263868     
 iteration         -450 MCMCOBJ=   -6622.96786208968     
 iteration         -449 MCMCOBJ=   -6618.73780530351     
 iteration         -448 MCMCOBJ=   -6629.55512091844     
 iteration         -447 MCMCOBJ=   -6646.98258765981     
 iteration         -446 MCMCOBJ=   -6639.99283979388     
 iteration         -445 MCMCOBJ=   -6643.96634393918     
 iteration         -444 MCMCOBJ=   -6664.87516715157     
 iteration         -443 MCMCOBJ=   -6640.55544055957     
 iteration         -442 MCMCOBJ=   -6700.01802720402     
 iteration         -441 MCMCOBJ=   -6674.08462244951     
 iteration         -440 MCMCOBJ=   -6672.79037107974     
 iteration         -439 MCMCOBJ=   -6665.41660099963     
 iteration         -438 MCMCOBJ=   -6656.86136230755     
 iteration         -437 MCMCOBJ=   -6660.73151826949     
 iteration         -436 MCMCOBJ=   -6675.16468314213     
 iteration         -435 MCMCOBJ=   -6670.10550538744     
 iteration         -434 MCMCOBJ=   -6655.26233119103     
 iteration         -433 MCMCOBJ=   -6659.22819293798     
 iteration         -432 MCMCOBJ=   -6630.10482075773     
 iteration         -431 MCMCOBJ=   -6673.10648536008     
 iteration         -430 MCMCOBJ=   -6699.42649696771     
 iteration         -429 MCMCOBJ=   -6699.42646926784     
 iteration         -428 MCMCOBJ=   -6672.97168883058     
 iteration         -427 MCMCOBJ=   -6699.85333169607     
 iteration         -426 MCMCOBJ=   -6671.02941939538     
 iteration         -425 MCMCOBJ=   -6661.19734156702     
 iteration         -424 MCMCOBJ=   -6654.39938052383     
 iteration         -423 MCMCOBJ=   -6619.01841182833     
 iteration         -422 MCMCOBJ=   -6596.43105743571     
 iteration         -421 MCMCOBJ=   -6614.20973132800     
 iteration         -420 MCMCOBJ=   -6587.17208736288     
 iteration         -419 MCMCOBJ=   -6638.80636025696     
 iteration         -418 MCMCOBJ=   -6571.26013882899     
 iteration         -417 MCMCOBJ=   -6619.47769563951     
 iteration         -416 MCMCOBJ=   -6637.02824258028     
 iteration         -415 MCMCOBJ=   -6627.66709432880     
 iteration         -414 MCMCOBJ=   -6622.22564003487     
 iteration         -413 MCMCOBJ=   -6617.98335193531     
 iteration         -412 MCMCOBJ=   -6623.01652161186     
 iteration         -411 MCMCOBJ=   -6539.55980359030     
 iteration         -410 MCMCOBJ=   -6579.18141400269     
 iteration         -409 MCMCOBJ=   -6563.13428671969     
 iteration         -408 MCMCOBJ=   -6603.96939464315     
 iteration         -407 MCMCOBJ=   -6619.38885980358     
 iteration         -406 MCMCOBJ=   -6629.43900615008     
 iteration         -405 MCMCOBJ=   -6656.64992691522     
 iteration         -404 MCMCOBJ=   -6656.64992075189     
 iteration         -403 MCMCOBJ=   -6621.93117483720     
 iteration         -402 MCMCOBJ=   -6642.27837174735     
 iteration         -401 MCMCOBJ=   -6585.50323771361     
 iteration         -400 MCMCOBJ=   -6616.38212556164     
 iteration         -399 MCMCOBJ=   -6667.42210058986     
 iteration         -398 MCMCOBJ=   -6709.97214165496     
 iteration         -397 MCMCOBJ=   -6697.28505190273     
 iteration         -396 MCMCOBJ=   -6696.91572388732     
 iteration         -395 MCMCOBJ=   -6648.60534700338     
 iteration         -394 MCMCOBJ=   -6613.17810114742     
 iteration         -393 MCMCOBJ=   -6628.13907919424     
 iteration         -392 MCMCOBJ=   -6609.81485742012     
 iteration         -391 MCMCOBJ=   -6623.73012814403     
 iteration         -390 MCMCOBJ=   -6654.30644490698     
 iteration         -389 MCMCOBJ=   -6642.80587674738     
 iteration         -388 MCMCOBJ=   -6682.10225914856     
 iteration         -387 MCMCOBJ=   -6626.93946763809     
 iteration         -386 MCMCOBJ=   -6594.96551368667     
 iteration         -385 MCMCOBJ=   -6587.33914392579     
 iteration         -384 MCMCOBJ=   -6603.37516936143     
 iteration         -383 MCMCOBJ=   -6614.45920379538     
 iteration         -382 MCMCOBJ=   -6629.95460033059     
 iteration         -381 MCMCOBJ=   -6614.52946390497     
 iteration         -380 MCMCOBJ=   -6612.53179176381     
 iteration         -379 MCMCOBJ=   -6587.52949400593     
 iteration         -378 MCMCOBJ=   -6585.82453044034     
 iteration         -377 MCMCOBJ=   -6578.79408287115     
 iteration         -376 MCMCOBJ=   -6609.07276775593     
 iteration         -375 MCMCOBJ=   -6583.77305968265     
 iteration         -374 MCMCOBJ=   -6573.34255771897     
 iteration         -373 MCMCOBJ=   -6624.55777014852     
 iteration         -372 MCMCOBJ=   -6631.51444191370     
 iteration         -371 MCMCOBJ=   -6631.51445074773     
 iteration         -370 MCMCOBJ=   -6604.01019090830     
 iteration         -369 MCMCOBJ=   -6633.90048003016     
 iteration         -368 MCMCOBJ=   -6613.19317260698     
 iteration         -367 MCMCOBJ=   -6598.45182606510     
 iteration         -366 MCMCOBJ=   -6587.16281112968     
 iteration         -365 MCMCOBJ=   -6600.43745276760     
 iteration         -364 MCMCOBJ=   -6574.79221076589     
 iteration         -363 MCMCOBJ=   -6560.89666907256     
 iteration         -362 MCMCOBJ=   -6601.69759530624     
 iteration         -361 MCMCOBJ=   -6610.68652633591     
 iteration         -360 MCMCOBJ=   -6621.42527152343     
 iteration         -359 MCMCOBJ=   -6678.67650055472     
 iteration         -358 MCMCOBJ=   -6648.40709843868     
 iteration         -357 MCMCOBJ=   -6626.23808077847     
 iteration         -356 MCMCOBJ=   -6618.32855477897     
 iteration         -355 MCMCOBJ=   -6604.07833299705     
 iteration         -354 MCMCOBJ=   -6593.30361265007     
 iteration         -353 MCMCOBJ=   -6616.57905773273     
 iteration         -352 MCMCOBJ=   -6616.57905894260     
 iteration         -351 MCMCOBJ=   -6652.45832138631     
 iteration         -350 MCMCOBJ=   -6649.27672408133     
 iteration         -349 MCMCOBJ=   -6618.03239689964     
 iteration         -348 MCMCOBJ=   -6631.49967269773     
 iteration         -347 MCMCOBJ=   -6668.92099111639     
 iteration         -346 MCMCOBJ=   -6618.50637966264     
 iteration         -345 MCMCOBJ=   -6618.01602031420     
 iteration         -344 MCMCOBJ=   -6650.50224381389     
 iteration         -343 MCMCOBJ=   -6609.70670179813     
 iteration         -342 MCMCOBJ=   -6595.80585644988     
 iteration         -341 MCMCOBJ=   -6668.13324355659     
 iteration         -340 MCMCOBJ=   -6660.97817501636     
 iteration         -339 MCMCOBJ=   -6673.19155042006     
 iteration         -338 MCMCOBJ=   -6652.54560732736     
 iteration         -337 MCMCOBJ=   -6652.54560674511     
 iteration         -336 MCMCOBJ=   -6581.10641813166     
 iteration         -335 MCMCOBJ=   -6562.85374415077     
 iteration         -334 MCMCOBJ=   -6614.52859577545     
 iteration         -333 MCMCOBJ=   -6641.07733311408     
 iteration         -332 MCMCOBJ=   -6588.79901957891     
 iteration         -331 MCMCOBJ=   -6584.29854217203     
 iteration         -330 MCMCOBJ=   -6659.21957791002     
 iteration         -329 MCMCOBJ=   -6675.24370043659     
 iteration         -328 MCMCOBJ=   -6665.62075356489     
 iteration         -327 MCMCOBJ=   -6640.88601680670     
 iteration         -326 MCMCOBJ=   -6651.71592372623     
 iteration         -325 MCMCOBJ=   -6623.03837012268     
 iteration         -324 MCMCOBJ=   -6647.00027631469     
 iteration         -323 MCMCOBJ=   -6647.83857484111     
 iteration         -322 MCMCOBJ=   -6657.73526168326     
 iteration         -321 MCMCOBJ=   -6637.99536361250     
 iteration         -320 MCMCOBJ=   -6687.89426626401     
 iteration         -319 MCMCOBJ=   -6665.00151072965     
 iteration         -318 MCMCOBJ=   -6638.50146356711     
 iteration         -317 MCMCOBJ=   -6602.10639615078     
 iteration         -316 MCMCOBJ=   -6630.36860469401     
 iteration         -315 MCMCOBJ=   -6660.49040099857     
 iteration         -314 MCMCOBJ=   -6691.02696527122     
 iteration         -313 MCMCOBJ=   -6672.41490490491     
 iteration         -312 MCMCOBJ=   -6641.40693834926     
 iteration         -311 MCMCOBJ=   -6615.48580497814     
 iteration         -310 MCMCOBJ=   -6655.64592377967     
 iteration         -309 MCMCOBJ=   -6615.43689366494     
 iteration         -308 MCMCOBJ=   -6580.62402894365     
 iteration         -307 MCMCOBJ=   -6601.96431304558     
 iteration         -306 MCMCOBJ=   -6636.75158517618     
 iteration         -305 MCMCOBJ=   -6614.84829471439     
 iteration         -304 MCMCOBJ=   -6628.15897990865     
 iteration         -303 MCMCOBJ=   -6583.93355992349     
 iteration         -302 MCMCOBJ=   -6631.59144386921     
 iteration         -301 MCMCOBJ=   -6634.92450892329     
 iteration         -300 MCMCOBJ=   -6601.73523793233     
 iteration         -299 MCMCOBJ=   -6652.80840385437     
 iteration         -298 MCMCOBJ=   -6607.18130873956     
 iteration         -297 MCMCOBJ=   -6615.03900549050     
 iteration         -296 MCMCOBJ=   -6624.52440563418     
 iteration         -295 MCMCOBJ=   -6624.52440828700     
 iteration         -294 MCMCOBJ=   -6628.23876931614     
 iteration         -293 MCMCOBJ=   -6635.23719483273     
 iteration         -292 MCMCOBJ=   -6636.77030177461     
 iteration         -291 MCMCOBJ=   -6632.42704776854     
 iteration         -290 MCMCOBJ=   -6585.05556323170     
 iteration         -289 MCMCOBJ=   -6571.14543855164     
 iteration         -288 MCMCOBJ=   -6588.47874408095     
 iteration         -287 MCMCOBJ=   -6593.63074631903     
 iteration         -286 MCMCOBJ=   -6577.83948298266     
 iteration         -285 MCMCOBJ=   -6561.15719988605     
 iteration         -284 MCMCOBJ=   -6573.70376589709     
 iteration         -283 MCMCOBJ=   -6601.47933203304     
 iteration         -282 MCMCOBJ=   -6574.43655985036     
 iteration         -281 MCMCOBJ=   -6620.70658249915     
 iteration         -280 MCMCOBJ=   -6633.73980841953     
 iteration         -279 MCMCOBJ=   -6633.73981386242     
 iteration         -278 MCMCOBJ=   -6584.08656868797     
 iteration         -277 MCMCOBJ=   -6572.21048464974     
 iteration         -276 MCMCOBJ=   -6628.14871713252     
 iteration         -275 MCMCOBJ=   -6641.25600623820     
 iteration         -274 MCMCOBJ=   -6657.39897913231     
 iteration         -273 MCMCOBJ=   -6636.82309112095     
 iteration         -272 MCMCOBJ=   -6641.60453512230     
 iteration         -271 MCMCOBJ=   -6626.72924832144     
 iteration         -270 MCMCOBJ=   -6621.66309752979     
 iteration         -269 MCMCOBJ=   -6628.02861778312     
 iteration         -268 MCMCOBJ=   -6621.40678866658     
 iteration         -267 MCMCOBJ=   -6606.69390715380     
 iteration         -266 MCMCOBJ=   -6656.04073318999     
 iteration         -265 MCMCOBJ=   -6608.60301758344     
 iteration         -264 MCMCOBJ=   -6626.65449262275     
 iteration         -263 MCMCOBJ=   -6630.22710636057     
 iteration         -262 MCMCOBJ=   -6574.18080634393     
 iteration         -261 MCMCOBJ=   -6634.49489851659     
 iteration         -260 MCMCOBJ=   -6603.62623393663     
 iteration         -259 MCMCOBJ=   -6682.62129159291     
 iteration         -258 MCMCOBJ=   -6646.86700049379     
 iteration         -257 MCMCOBJ=   -6615.87162144132     
 iteration         -256 MCMCOBJ=   -6661.16641544115     
 iteration         -255 MCMCOBJ=   -6665.65460959597     
 iteration         -254 MCMCOBJ=   -6648.37661389934     
 iteration         -253 MCMCOBJ=   -6643.33382561470     
 iteration         -252 MCMCOBJ=   -6679.53423520707     
 iteration         -251 MCMCOBJ=   -6597.77861991590     
 iteration         -250 MCMCOBJ=   -6620.20519090614     
 iteration         -249 MCMCOBJ=   -6621.48608332717     
 iteration         -248 MCMCOBJ=   -6625.71435759220     
 iteration         -247 MCMCOBJ=   -6579.73366689401     
 iteration         -246 MCMCOBJ=   -6553.04771778026     
 iteration         -245 MCMCOBJ=   -6634.12687921844     
 iteration         -244 MCMCOBJ=   -6646.74758225200     
 iteration         -243 MCMCOBJ=   -6655.27672893276     
 iteration         -242 MCMCOBJ=   -6614.71473463915     
 iteration         -241 MCMCOBJ=   -6589.30291653519     
 iteration         -240 MCMCOBJ=   -6569.36439114051     
 iteration         -239 MCMCOBJ=   -6588.77297698811     
 iteration         -238 MCMCOBJ=   -6668.99230106832     
 iteration         -237 MCMCOBJ=   -6693.51489424211     
 iteration         -236 MCMCOBJ=   -6684.53529052811     
 iteration         -235 MCMCOBJ=   -6639.36991325916     
 iteration         -234 MCMCOBJ=   -6622.06174994544     
 iteration         -233 MCMCOBJ=   -6576.37520731435     
 iteration         -232 MCMCOBJ=   -6564.94884011625     
 iteration         -231 MCMCOBJ=   -6608.93369509969     
 iteration         -230 MCMCOBJ=   -6587.25776597502     
 iteration         -229 MCMCOBJ=   -6591.52917177231     
 iteration         -228 MCMCOBJ=   -6623.87150813461     
 iteration         -227 MCMCOBJ=   -6631.57665189363     
 iteration         -226 MCMCOBJ=   -6595.83457572562     
 iteration         -225 MCMCOBJ=   -6644.41637191319     
 iteration         -224 MCMCOBJ=   -6625.69814063653     
 iteration         -223 MCMCOBJ=   -6654.41772935230     
 iteration         -222 MCMCOBJ=   -6675.70972312833     
 iteration         -221 MCMCOBJ=   -6640.84876067189     
 iteration         -220 MCMCOBJ=   -6626.20748379073     
 iteration         -219 MCMCOBJ=   -6610.79932878327     
 iteration         -218 MCMCOBJ=   -6628.03725541490     
 iteration         -217 MCMCOBJ=   -6643.37350501651     
 iteration         -216 MCMCOBJ=   -6621.72370908679     
 iteration         -215 MCMCOBJ=   -6619.09653020871     
 iteration         -214 MCMCOBJ=   -6649.92460605682     
 iteration         -213 MCMCOBJ=   -6645.21433433468     
 iteration         -212 MCMCOBJ=   -6625.03972736017     
 iteration         -211 MCMCOBJ=   -6559.74375421812     
 iteration         -210 MCMCOBJ=   -6629.26584651870     
 iteration         -209 MCMCOBJ=   -6568.65374436001     
 iteration         -208 MCMCOBJ=   -6627.92127653207     
 iteration         -207 MCMCOBJ=   -6591.28259708378     
 iteration         -206 MCMCOBJ=   -6634.47111545611     
 iteration         -205 MCMCOBJ=   -6664.59919753462     
 iteration         -204 MCMCOBJ=   -6649.38874207173     
 iteration         -203 MCMCOBJ=   -6592.96854977413     
 iteration         -202 MCMCOBJ=   -6578.43627345285     
 iteration         -201 MCMCOBJ=   -6555.21761979315     
 iteration         -200 MCMCOBJ=   -6618.54241964357     
 iteration         -199 MCMCOBJ=   -6648.24413312212     
 iteration         -198 MCMCOBJ=   -6589.59139389631     
 iteration         -197 MCMCOBJ=   -6560.18086111581     
 iteration         -196 MCMCOBJ=   -6607.33752357078     
 iteration         -195 MCMCOBJ=   -6621.96139838576     
 iteration         -194 MCMCOBJ=   -6618.58254822674     
 iteration         -193 MCMCOBJ=   -6611.86315679390     
 iteration         -192 MCMCOBJ=   -6637.11584127165     
 iteration         -191 MCMCOBJ=   -6649.20294149180     
 iteration         -190 MCMCOBJ=   -6632.59833050721     
 iteration         -189 MCMCOBJ=   -6646.77776380712     
 iteration         -188 MCMCOBJ=   -6651.73639991914     
 iteration         -187 MCMCOBJ=   -6608.39625616349     
 iteration         -186 MCMCOBJ=   -6629.34644006164     
 iteration         -185 MCMCOBJ=   -6614.87890255895     
 iteration         -184 MCMCOBJ=   -6579.55615328160     
 iteration         -183 MCMCOBJ=   -6581.80186772786     
 iteration         -182 MCMCOBJ=   -6637.82392215999     
 iteration         -181 MCMCOBJ=   -6615.72594661620     
 iteration         -180 MCMCOBJ=   -6643.37618302948     
 iteration         -179 MCMCOBJ=   -6640.16596036887     
 iteration         -178 MCMCOBJ=   -6665.47819510349     
 iteration         -177 MCMCOBJ=   -6673.76416844484     
 iteration         -176 MCMCOBJ=   -6622.30975534858     
 iteration         -175 MCMCOBJ=   -6581.13667752301     
 iteration         -174 MCMCOBJ=   -6632.59896655690     
 iteration         -173 MCMCOBJ=   -6682.11509074387     
 iteration         -172 MCMCOBJ=   -6658.75906946597     
 iteration         -171 MCMCOBJ=   -6620.81210240244     
 iteration         -170 MCMCOBJ=   -6632.56060289945     
 iteration         -169 MCMCOBJ=   -6628.58329813053     
 iteration         -168 MCMCOBJ=   -6627.96954600920     
 iteration         -167 MCMCOBJ=   -6608.14351780207     
 iteration         -166 MCMCOBJ=   -6624.96195061412     
 iteration         -165 MCMCOBJ=   -6624.54399331316     
 iteration         -164 MCMCOBJ=   -6624.63052362164     
 iteration         -163 MCMCOBJ=   -6623.16275783968     
 iteration         -162 MCMCOBJ=   -6608.50029990707     
 iteration         -161 MCMCOBJ=   -6598.27155909672     
 iteration         -160 MCMCOBJ=   -6649.98568144028     
 iteration         -159 MCMCOBJ=   -6616.09641868418     
 iteration         -158 MCMCOBJ=   -6621.37873747665     
 iteration         -157 MCMCOBJ=   -6635.71668914795     
 iteration         -156 MCMCOBJ=   -6622.73898169462     
 iteration         -155 MCMCOBJ=   -6640.38295392495     
 iteration         -154 MCMCOBJ=   -6624.39979926138     
 iteration         -153 MCMCOBJ=   -6624.39953361640     
 iteration         -152 MCMCOBJ=   -6623.40844437751     
 iteration         -151 MCMCOBJ=   -6611.04311497740     
 iteration         -150 MCMCOBJ=   -6623.57708768244     
 iteration         -149 MCMCOBJ=   -6627.03612611812     
 iteration         -148 MCMCOBJ=   -6641.23629431874     
 iteration         -147 MCMCOBJ=   -6642.73179740096     
 iteration         -146 MCMCOBJ=   -6601.14611221467     
 iteration         -145 MCMCOBJ=   -6591.86813488259     
 iteration         -144 MCMCOBJ=   -6591.86813518393     
 iteration         -143 MCMCOBJ=   -6639.52807136482     
 iteration         -142 MCMCOBJ=   -6570.61435148469     
 iteration         -141 MCMCOBJ=   -6650.68329944143     
 iteration         -140 MCMCOBJ=   -6576.08056928991     
 iteration         -139 MCMCOBJ=   -6507.77021860779     
 iteration         -138 MCMCOBJ=   -6616.75932872387     
 iteration         -137 MCMCOBJ=   -6624.82443343093     
 iteration         -136 MCMCOBJ=   -6697.44212742865     
 iteration         -135 MCMCOBJ=   -6627.59833137710     
 iteration         -134 MCMCOBJ=   -6644.37175083226     
 iteration         -133 MCMCOBJ=   -6610.01150276503     
 iteration         -132 MCMCOBJ=   -6601.31144034114     
 iteration         -131 MCMCOBJ=   -6575.36440228971     
 iteration         -130 MCMCOBJ=   -6591.65173874973     
 iteration         -129 MCMCOBJ=   -6659.71050060916     
 iteration         -128 MCMCOBJ=   -6665.41648410410     
 iteration         -127 MCMCOBJ=   -6667.55184176243     
 iteration         -126 MCMCOBJ=   -6667.55185302204     
 iteration         -125 MCMCOBJ=   -6626.01448387790     
 iteration         -124 MCMCOBJ=   -6590.60352030549     
 iteration         -123 MCMCOBJ=   -6623.96214037348     
 iteration         -122 MCMCOBJ=   -6610.46217272391     
 iteration         -121 MCMCOBJ=   -6647.85039660758     
 iteration         -120 MCMCOBJ=   -6675.20465472413     
 iteration         -119 MCMCOBJ=   -6670.24960285667     
 iteration         -118 MCMCOBJ=   -6663.88264421878     
 iteration         -117 MCMCOBJ=   -6673.96821573425     
 iteration         -116 MCMCOBJ=   -6648.58337205181     
 iteration         -115 MCMCOBJ=   -6616.52000306094     
 iteration         -114 MCMCOBJ=   -6601.80062929980     
 iteration         -113 MCMCOBJ=   -6594.62897777555     
 iteration         -112 MCMCOBJ=   -6598.80044687454     
 iteration         -111 MCMCOBJ=   -6657.65756641243     
 iteration         -110 MCMCOBJ=   -6623.85595526680     
 iteration         -109 MCMCOBJ=   -6635.19686036414     
 iteration         -108 MCMCOBJ=   -6625.06597266270     
 iteration         -107 MCMCOBJ=   -6611.71889693562     
 iteration         -106 MCMCOBJ=   -6631.85352191934     
 iteration         -105 MCMCOBJ=   -6664.42725495123     
 iteration         -104 MCMCOBJ=   -6658.19119427504     
 iteration         -103 MCMCOBJ=   -6662.60287854922     
 iteration         -102 MCMCOBJ=   -6633.50477590514     
 iteration         -101 MCMCOBJ=   -6615.77921623317     
 iteration         -100 MCMCOBJ=   -6584.99540311678     
 iteration          -99 MCMCOBJ=   -6569.85798563937     
 iteration          -98 MCMCOBJ=   -6581.21599127372     
 iteration          -97 MCMCOBJ=   -6552.78247874815     
 iteration          -96 MCMCOBJ=   -6563.77912139288     
 iteration          -95 MCMCOBJ=   -6539.10116428473     
 iteration          -94 MCMCOBJ=   -6548.71667775160     
 iteration          -93 MCMCOBJ=   -6624.45482991154     
 iteration          -92 MCMCOBJ=   -6601.32342263988     
 iteration          -91 MCMCOBJ=   -6606.34941694594     
 iteration          -90 MCMCOBJ=   -6633.39473686576     
 iteration          -89 MCMCOBJ=   -6630.72592532957     
 iteration          -88 MCMCOBJ=   -6625.23373039446     
 iteration          -87 MCMCOBJ=   -6599.46475778835     
 iteration          -86 MCMCOBJ=   -6572.93837627221     
 iteration          -85 MCMCOBJ=   -6575.24387181074     
 iteration          -84 MCMCOBJ=   -6602.53649349646     
 iteration          -83 MCMCOBJ=   -6581.21887723696     
 iteration          -82 MCMCOBJ=   -6650.22863265959     
 iteration          -81 MCMCOBJ=   -6655.86316675648     
 iteration          -80 MCMCOBJ=   -6649.75312077877     
 iteration          -79 MCMCOBJ=   -6657.00217549028     
 iteration          -78 MCMCOBJ=   -6656.30138164350     
 iteration          -77 MCMCOBJ=   -6585.68683706809     
 iteration          -76 MCMCOBJ=   -6593.50191400817     
 iteration          -75 MCMCOBJ=   -6629.59377187819     
 iteration          -74 MCMCOBJ=   -6583.36199632032     
 iteration          -73 MCMCOBJ=   -6605.82983676977     
 iteration          -72 MCMCOBJ=   -6628.83356400222     
 iteration          -71 MCMCOBJ=   -6601.50254797898     
 iteration          -70 MCMCOBJ=   -6636.24587264625     
 iteration          -69 MCMCOBJ=   -6649.82062717903     
 iteration          -68 MCMCOBJ=   -6631.92177466849     
 iteration          -67 MCMCOBJ=   -6626.72002806450     
 iteration          -66 MCMCOBJ=   -6654.30121404129     
 iteration          -65 MCMCOBJ=   -6609.11407755284     
 iteration          -64 MCMCOBJ=   -6645.80934095182     
 iteration          -63 MCMCOBJ=   -6615.43868226116     
 iteration          -62 MCMCOBJ=   -6647.73877622883     
 iteration          -61 MCMCOBJ=   -6625.70672511855     
 iteration          -60 MCMCOBJ=   -6630.04679305516     
 iteration          -59 MCMCOBJ=   -6652.08769456644     
 iteration          -58 MCMCOBJ=   -6656.06665324799     
 iteration          -57 MCMCOBJ=   -6628.50946721570     
 iteration          -56 MCMCOBJ=   -6613.42753312157     
 iteration          -55 MCMCOBJ=   -6611.94627898214     
 iteration          -54 MCMCOBJ=   -6607.07888640185     
 iteration          -53 MCMCOBJ=   -6642.33537048146     
 iteration          -52 MCMCOBJ=   -6672.59949292139     
 iteration          -51 MCMCOBJ=   -6680.79705907506     
 iteration          -50 MCMCOBJ=   -6639.28959477908     
 iteration          -49 MCMCOBJ=   -6582.95697901975     
 iteration          -48 MCMCOBJ=   -6594.87038578251     
 iteration          -47 MCMCOBJ=   -6613.05056488515     
 iteration          -46 MCMCOBJ=   -6590.22706204301     
 iteration          -45 MCMCOBJ=   -6596.73986331418     
 iteration          -44 MCMCOBJ=   -6636.90746233485     
 iteration          -43 MCMCOBJ=   -6636.90746449903     
 iteration          -42 MCMCOBJ=   -6627.47451984157     
 iteration          -41 MCMCOBJ=   -6627.28158758061     
 iteration          -40 MCMCOBJ=   -6626.76365847766     
 iteration          -39 MCMCOBJ=   -6631.91756374934     
 iteration          -38 MCMCOBJ=   -6626.09392103426     
 iteration          -37 MCMCOBJ=   -6615.47745302518     
 iteration          -36 MCMCOBJ=   -6618.64410558937     
 iteration          -35 MCMCOBJ=   -6628.36645921840     
 iteration          -34 MCMCOBJ=   -6582.94367223491     
 iteration          -33 MCMCOBJ=   -6621.79342917997     
 iteration          -32 MCMCOBJ=   -6600.86761529817     
 iteration          -31 MCMCOBJ=   -6613.26561772641     
 iteration          -30 MCMCOBJ=   -6599.19964165469     
 iteration          -29 MCMCOBJ=   -6649.15473475834     
 iteration          -28 MCMCOBJ=   -6637.93755580227     
 iteration          -27 MCMCOBJ=   -6660.41750191012     
 iteration          -26 MCMCOBJ=   -6659.68532785012     
 iteration          -25 MCMCOBJ=   -6626.78346158367     
 iteration          -24 MCMCOBJ=   -6592.59184332636     
 iteration          -23 MCMCOBJ=   -6567.90738864947     
 iteration          -22 MCMCOBJ=   -6617.74858263824     
 iteration          -21 MCMCOBJ=   -6624.58719907440     
 iteration          -20 MCMCOBJ=   -6646.98903126424     
 iteration          -19 MCMCOBJ=   -6654.91215533996     
 iteration          -18 MCMCOBJ=   -6703.33045060015     
 iteration          -17 MCMCOBJ=   -6646.07498023281     
 iteration          -16 MCMCOBJ=   -6644.45359084222     
 iteration          -15 MCMCOBJ=   -6632.38777555546     
 iteration          -14 MCMCOBJ=   -6607.20861355533     
 iteration          -13 MCMCOBJ=   -6630.84446400932     
 iteration          -12 MCMCOBJ=   -6650.75464071339     
 iteration          -11 MCMCOBJ=   -6662.38418720280     
 iteration          -10 MCMCOBJ=   -6612.92936816627     
 iteration           -9 MCMCOBJ=   -6630.54867019475     
 iteration           -8 MCMCOBJ=   -6592.30780502199     
 iteration           -7 MCMCOBJ=   -6583.18113799928     
 iteration           -6 MCMCOBJ=   -6596.33860091399     
 iteration           -5 MCMCOBJ=   -6607.73745834133     
 iteration           -4 MCMCOBJ=   -6641.99092138560     
 iteration           -3 MCMCOBJ=   -6640.88609603894     
 iteration           -2 MCMCOBJ=   -6572.43679786980     
 iteration           -1 MCMCOBJ=   -6563.31672915986     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6574.64104682734     
 iteration            1 MCMCOBJ=   -6571.95680161135     
 iteration            2 MCMCOBJ=   -6610.57342142280     
 iteration            3 MCMCOBJ=   -6585.16623408391     
 iteration            4 MCMCOBJ=   -6612.53841796386     
 iteration            5 MCMCOBJ=   -6616.34164818127     
 iteration            6 MCMCOBJ=   -6661.40147310634     
 iteration            7 MCMCOBJ=   -6629.58564879411     
 iteration            8 MCMCOBJ=   -6649.91632239731     
 iteration            9 MCMCOBJ=   -6653.68044136002     
 iteration           10 MCMCOBJ=   -6627.51836720189     
 iteration           11 MCMCOBJ=   -6602.54728705235     
 iteration           12 MCMCOBJ=   -6569.49281497611     
 iteration           13 MCMCOBJ=   -6614.16052670821     
 iteration           14 MCMCOBJ=   -6618.31800620825     
 iteration           15 MCMCOBJ=   -6668.84472398777     
 iteration           16 MCMCOBJ=   -6686.63513364654     
 iteration           17 MCMCOBJ=   -6680.81733164078     
 iteration           18 MCMCOBJ=   -6618.82081430901     
 iteration           19 MCMCOBJ=   -6616.12548462419     
 iteration           20 MCMCOBJ=   -6576.52696537041     
 iteration           21 MCMCOBJ=   -6603.37757428118     
 iteration           22 MCMCOBJ=   -6598.40908274414     
 iteration           23 MCMCOBJ=   -6620.61738271046     
 iteration           24 MCMCOBJ=   -6592.72299739463     
 iteration           25 MCMCOBJ=   -6620.06220737499     
 iteration           26 MCMCOBJ=   -6617.70507360957     
 iteration           27 MCMCOBJ=   -6655.25919896945     
 iteration           28 MCMCOBJ=   -6629.75984227585     
 iteration           29 MCMCOBJ=   -6613.68406391944     
 iteration           30 MCMCOBJ=   -6604.92844585096     
 iteration           31 MCMCOBJ=   -6596.66868246958     
 iteration           32 MCMCOBJ=   -6590.89667021862     
 iteration           33 MCMCOBJ=   -6613.20026989498     
 iteration           34 MCMCOBJ=   -6644.36977334822     
 iteration           35 MCMCOBJ=   -6629.61221179818     
 iteration           36 MCMCOBJ=   -6673.97711476120     
 iteration           37 MCMCOBJ=   -6662.65407563754     
 iteration           38 MCMCOBJ=   -6642.51644649092     
 iteration           39 MCMCOBJ=   -6635.77525808021     
 iteration           40 MCMCOBJ=   -6566.19005574468     
 iteration           41 MCMCOBJ=   -6578.93692157082     
 iteration           42 MCMCOBJ=   -6559.30996698458     
 iteration           43 MCMCOBJ=   -6627.75644118087     
 iteration           44 MCMCOBJ=   -6619.69238316744     
 iteration           45 MCMCOBJ=   -6623.47021692843     
 iteration           46 MCMCOBJ=   -6661.17777908990     
 iteration           47 MCMCOBJ=   -6661.17777899689     
 iteration           48 MCMCOBJ=   -6648.95805135244     
 iteration           49 MCMCOBJ=   -6649.62086121599     
 iteration           50 MCMCOBJ=   -6663.77898346595     
 iteration           51 MCMCOBJ=   -6652.00256775299     
 iteration           52 MCMCOBJ=   -6600.83289585087     
 iteration           53 MCMCOBJ=   -6633.78126000649     
 iteration           54 MCMCOBJ=   -6609.52033465566     
 iteration           55 MCMCOBJ=   -6618.06931092411     
 iteration           56 MCMCOBJ=   -6580.85674047753     
 iteration           57 MCMCOBJ=   -6560.31183777303     
 iteration           58 MCMCOBJ=   -6558.44652114066     
 iteration           59 MCMCOBJ=   -6585.85493817048     
 iteration           60 MCMCOBJ=   -6552.60328811316     
 iteration           61 MCMCOBJ=   -6589.38908170100     
 iteration           62 MCMCOBJ=   -6580.36344878211     
 iteration           63 MCMCOBJ=   -6555.25460028710     
 iteration           64 MCMCOBJ=   -6620.40729298081     
 iteration           65 MCMCOBJ=   -6624.39547610790     
 iteration           66 MCMCOBJ=   -6631.14229117276     
 iteration           67 MCMCOBJ=   -6633.24266532990     
 iteration           68 MCMCOBJ=   -6683.18958339889     
 iteration           69 MCMCOBJ=   -6673.07473806518     
 iteration           70 MCMCOBJ=   -6619.93908549393     
 iteration           71 MCMCOBJ=   -6664.63449308701     
 iteration           72 MCMCOBJ=   -6664.63449300930     
 iteration           73 MCMCOBJ=   -6674.18658227407     
 iteration           74 MCMCOBJ=   -6650.85737378698     
 iteration           75 MCMCOBJ=   -6650.85737807408     
 iteration           76 MCMCOBJ=   -6666.32646675087     
 iteration           77 MCMCOBJ=   -6660.97949217854     
 iteration           78 MCMCOBJ=   -6582.07031692080     
 iteration           79 MCMCOBJ=   -6610.35454390503     
 iteration           80 MCMCOBJ=   -6609.64552368029     
 iteration           81 MCMCOBJ=   -6629.41340430161     
 iteration           82 MCMCOBJ=   -6653.68693020182     
 iteration           83 MCMCOBJ=   -6674.29222369693     
 iteration           84 MCMCOBJ=   -6637.56170174159     
 iteration           85 MCMCOBJ=   -6606.37205475651     
 iteration           86 MCMCOBJ=   -6615.63065026537     
 iteration           87 MCMCOBJ=   -6625.56431699000     
 iteration           88 MCMCOBJ=   -6644.70726049602     
 iteration           89 MCMCOBJ=   -6658.13287141055     
 iteration           90 MCMCOBJ=   -6668.52344470696     
 iteration           91 MCMCOBJ=   -6638.35992797308     
 iteration           92 MCMCOBJ=   -6606.50651970719     
 iteration           93 MCMCOBJ=   -6630.87368714583     
 iteration           94 MCMCOBJ=   -6654.44071633292     
 iteration           95 MCMCOBJ=   -6669.17150136481     
 iteration           96 MCMCOBJ=   -6668.30857557315     
 iteration           97 MCMCOBJ=   -6603.71433095110     
 iteration           98 MCMCOBJ=   -6603.54764075031     
 iteration           99 MCMCOBJ=   -6624.86004271044     
 iteration          100 MCMCOBJ=   -6633.59455365411     
 iteration          101 MCMCOBJ=   -6621.49574773695     
 iteration          102 MCMCOBJ=   -6608.58977482064     
 iteration          103 MCMCOBJ=   -6615.16215405007     
 iteration          104 MCMCOBJ=   -6624.99961039712     
 iteration          105 MCMCOBJ=   -6615.70642124028     
 iteration          106 MCMCOBJ=   -6583.60484148759     
 iteration          107 MCMCOBJ=   -6580.23159591861     
 iteration          108 MCMCOBJ=   -6586.79377032850     
 iteration          109 MCMCOBJ=   -6598.62161639966     
 iteration          110 MCMCOBJ=   -6646.31312234458     
 iteration          111 MCMCOBJ=   -6646.31311801719     
 iteration          112 MCMCOBJ=   -6641.59500691491     
 iteration          113 MCMCOBJ=   -6655.96089661616     
 iteration          114 MCMCOBJ=   -6675.82755434549     
 iteration          115 MCMCOBJ=   -6676.34126782314     
 iteration          116 MCMCOBJ=   -6639.43634857576     
 iteration          117 MCMCOBJ=   -6648.86940093217     
 iteration          118 MCMCOBJ=   -6627.12295130285     
 iteration          119 MCMCOBJ=   -6645.48084085049     
 iteration          120 MCMCOBJ=   -6621.86563646532     
 iteration          121 MCMCOBJ=   -6648.76901739311     
 iteration          122 MCMCOBJ=   -6615.70301533005     
 iteration          123 MCMCOBJ=   -6661.94249776509     
 iteration          124 MCMCOBJ=   -6655.66197194187     
 iteration          125 MCMCOBJ=   -6614.87688328734     
 iteration          126 MCMCOBJ=   -6609.41482820397     
 iteration          127 MCMCOBJ=   -6622.52457647212     
 iteration          128 MCMCOBJ=   -6602.48093971392     
 iteration          129 MCMCOBJ=   -6600.64640747101     
 iteration          130 MCMCOBJ=   -6640.02918166851     
 iteration          131 MCMCOBJ=   -6628.45575516254     
 iteration          132 MCMCOBJ=   -6663.25360946741     
 iteration          133 MCMCOBJ=   -6664.14475768444     
 iteration          134 MCMCOBJ=   -6634.42304984289     
 iteration          135 MCMCOBJ=   -6670.68813816794     
 iteration          136 MCMCOBJ=   -6638.62135179851     
 iteration          137 MCMCOBJ=   -6598.57463761608     
 iteration          138 MCMCOBJ=   -6627.57195288584     
 iteration          139 MCMCOBJ=   -6603.98065290653     
 iteration          140 MCMCOBJ=   -6580.08963848098     
 iteration          141 MCMCOBJ=   -6564.54644111702     
 iteration          142 MCMCOBJ=   -6617.17901679857     
 iteration          143 MCMCOBJ=   -6652.59604641045     
 iteration          144 MCMCOBJ=   -6652.05678490329     
 iteration          145 MCMCOBJ=   -6656.02509366372     
 iteration          146 MCMCOBJ=   -6690.26130342623     
 iteration          147 MCMCOBJ=   -6672.70075491215     
 iteration          148 MCMCOBJ=   -6653.62249689986     
 iteration          149 MCMCOBJ=   -6623.74790302443     
 iteration          150 MCMCOBJ=   -6633.20829914907     
 iteration          151 MCMCOBJ=   -6628.20776548746     
 iteration          152 MCMCOBJ=   -6675.66142175959     
 iteration          153 MCMCOBJ=   -6623.34339675980     
 iteration          154 MCMCOBJ=   -6642.15506694705     
 iteration          155 MCMCOBJ=   -6622.12993818959     
 iteration          156 MCMCOBJ=   -6610.86568593658     
 iteration          157 MCMCOBJ=   -6612.88955954355     
 iteration          158 MCMCOBJ=   -6598.32135062294     
 iteration          159 MCMCOBJ=   -6630.24492135965     
 iteration          160 MCMCOBJ=   -6642.08885098007     
 iteration          161 MCMCOBJ=   -6615.36683714431     
 iteration          162 MCMCOBJ=   -6616.75851358974     
 iteration          163 MCMCOBJ=   -6574.96619707812     
 iteration          164 MCMCOBJ=   -6550.78908126812     
 iteration          165 MCMCOBJ=   -6585.14322014980     
 iteration          166 MCMCOBJ=   -6606.52909805268     
 iteration          167 MCMCOBJ=   -6622.88609298899     
 iteration          168 MCMCOBJ=   -6614.21474814254     
 iteration          169 MCMCOBJ=   -6607.90981046543     
 iteration          170 MCMCOBJ=   -6645.31110788968     
 iteration          171 MCMCOBJ=   -6616.56636029034     
 iteration          172 MCMCOBJ=   -6577.87807023831     
 iteration          173 MCMCOBJ=   -6590.35355163068     
 iteration          174 MCMCOBJ=   -6607.68024413815     
 iteration          175 MCMCOBJ=   -6571.90462110025     
 iteration          176 MCMCOBJ=   -6592.53426428500     
 iteration          177 MCMCOBJ=   -6577.29055301656     
 iteration          178 MCMCOBJ=   -6575.12391144774     
 iteration          179 MCMCOBJ=   -6572.94266427980     
 iteration          180 MCMCOBJ=   -6585.96878997108     
 iteration          181 MCMCOBJ=   -6590.96177446922     
 iteration          182 MCMCOBJ=   -6593.36085361670     
 iteration          183 MCMCOBJ=   -6576.20688099549     
 iteration          184 MCMCOBJ=   -6543.74698468753     
 iteration          185 MCMCOBJ=   -6553.80466293708     
 iteration          186 MCMCOBJ=   -6573.54169690810     
 iteration          187 MCMCOBJ=   -6616.29652304334     
 iteration          188 MCMCOBJ=   -6643.10560054287     
 iteration          189 MCMCOBJ=   -6634.39091134770     
 iteration          190 MCMCOBJ=   -6633.07790623518     
 iteration          191 MCMCOBJ=   -6639.89786609510     
 iteration          192 MCMCOBJ=   -6568.45153701486     
 iteration          193 MCMCOBJ=   -6576.63209420729     
 iteration          194 MCMCOBJ=   -6627.71064873404     
 iteration          195 MCMCOBJ=   -6620.26599874132     
 iteration          196 MCMCOBJ=   -6606.20141279077     
 iteration          197 MCMCOBJ=   -6616.81738826419     
 iteration          198 MCMCOBJ=   -6618.18680607412     
 iteration          199 MCMCOBJ=   -6646.58530249364     
 iteration          200 MCMCOBJ=   -6606.98610521690     
 iteration          201 MCMCOBJ=   -6599.18698239996     
 iteration          202 MCMCOBJ=   -6603.05786448659     
 iteration          203 MCMCOBJ=   -6652.97213082259     
 iteration          204 MCMCOBJ=   -6649.69465074146     
 iteration          205 MCMCOBJ=   -6651.04824599533     
 iteration          206 MCMCOBJ=   -6643.99873672436     
 iteration          207 MCMCOBJ=   -6670.59478739910     
 iteration          208 MCMCOBJ=   -6634.04454028673     
 iteration          209 MCMCOBJ=   -6659.39451287438     
 iteration          210 MCMCOBJ=   -6631.78292718306     
 iteration          211 MCMCOBJ=   -6589.54080570390     
 iteration          212 MCMCOBJ=   -6606.41226366704     
 iteration          213 MCMCOBJ=   -6657.47569791396     
 iteration          214 MCMCOBJ=   -6696.43295182370     
 iteration          215 MCMCOBJ=   -6709.96684835168     
 iteration          216 MCMCOBJ=   -6680.78678773749     
 iteration          217 MCMCOBJ=   -6658.89274653865     
 iteration          218 MCMCOBJ=   -6639.84016995359     
 iteration          219 MCMCOBJ=   -6631.06604032501     
 iteration          220 MCMCOBJ=   -6672.05108427263     
 iteration          221 MCMCOBJ=   -6679.49803608515     
 iteration          222 MCMCOBJ=   -6666.68444632607     
 iteration          223 MCMCOBJ=   -6604.46828130982     
 iteration          224 MCMCOBJ=   -6630.50691322346     
 iteration          225 MCMCOBJ=   -6646.11587960014     
 iteration          226 MCMCOBJ=   -6612.41167887800     
 iteration          227 MCMCOBJ=   -6638.37296286946     
 iteration          228 MCMCOBJ=   -6630.34244945670     
 iteration          229 MCMCOBJ=   -6672.01591482441     
 iteration          230 MCMCOBJ=   -6661.92052798420     
 iteration          231 MCMCOBJ=   -6650.07668937997     
 iteration          232 MCMCOBJ=   -6585.28595869073     
 iteration          233 MCMCOBJ=   -6625.05014824028     
 iteration          234 MCMCOBJ=   -6623.53054166930     
 iteration          235 MCMCOBJ=   -6630.17210208399     
 iteration          236 MCMCOBJ=   -6641.83302238646     
 iteration          237 MCMCOBJ=   -6669.43317373534     
 iteration          238 MCMCOBJ=   -6648.75471220748     
 iteration          239 MCMCOBJ=   -6633.93488132458     
 iteration          240 MCMCOBJ=   -6584.32375874883     
 iteration          241 MCMCOBJ=   -6615.86450991437     
 iteration          242 MCMCOBJ=   -6606.05970218202     
 iteration          243 MCMCOBJ=   -6619.65793150773     
 iteration          244 MCMCOBJ=   -6570.63086685026     
 iteration          245 MCMCOBJ=   -6584.34775970635     
 iteration          246 MCMCOBJ=   -6590.13373727736     
 iteration          247 MCMCOBJ=   -6602.64799537702     
 iteration          248 MCMCOBJ=   -6625.79899394746     
 iteration          249 MCMCOBJ=   -6658.74406207729     
 iteration          250 MCMCOBJ=   -6703.22918548345     
 iteration          251 MCMCOBJ=   -6673.36474570950     
 iteration          252 MCMCOBJ=   -6633.46427413217     
 iteration          253 MCMCOBJ=   -6627.90136447186     
 iteration          254 MCMCOBJ=   -6619.59158865953     
 iteration          255 MCMCOBJ=   -6598.36195105931     
 iteration          256 MCMCOBJ=   -6615.37292060960     
 iteration          257 MCMCOBJ=   -6593.28046670322     
 iteration          258 MCMCOBJ=   -6563.56608079867     
 iteration          259 MCMCOBJ=   -6558.82316798468     
 iteration          260 MCMCOBJ=   -6551.39584487684     
 iteration          261 MCMCOBJ=   -6554.52528761478     
 iteration          262 MCMCOBJ=   -6644.55245895983     
 iteration          263 MCMCOBJ=   -6627.09536852888     
 iteration          264 MCMCOBJ=   -6622.16735775259     
 iteration          265 MCMCOBJ=   -6660.37356739525     
 iteration          266 MCMCOBJ=   -6675.93370690859     
 iteration          267 MCMCOBJ=   -6649.78091920995     
 iteration          268 MCMCOBJ=   -6688.05606901651     
 iteration          269 MCMCOBJ=   -6645.29273157251     
 iteration          270 MCMCOBJ=   -6647.56813589988     
 iteration          271 MCMCOBJ=   -6653.62314007533     
 iteration          272 MCMCOBJ=   -6630.88346776043     
 iteration          273 MCMCOBJ=   -6640.39666782506     
 iteration          274 MCMCOBJ=   -6633.61619308382     
 iteration          275 MCMCOBJ=   -6664.60102317783     
 iteration          276 MCMCOBJ=   -6653.42985580036     
 iteration          277 MCMCOBJ=   -6637.27842394866     
 iteration          278 MCMCOBJ=   -6671.74671223774     
 iteration          279 MCMCOBJ=   -6632.18579089818     
 iteration          280 MCMCOBJ=   -6632.18578941592     
 iteration          281 MCMCOBJ=   -6607.67810983677     
 iteration          282 MCMCOBJ=   -6600.67480519442     
 iteration          283 MCMCOBJ=   -6600.67480344239     
 iteration          284 MCMCOBJ=   -6590.23755885144     
 iteration          285 MCMCOBJ=   -6580.00728003803     
 iteration          286 MCMCOBJ=   -6619.73015996990     
 iteration          287 MCMCOBJ=   -6623.39166162776     
 iteration          288 MCMCOBJ=   -6636.60440547921     
 iteration          289 MCMCOBJ=   -6608.38121413537     
 iteration          290 MCMCOBJ=   -6619.64531208629     
 iteration          291 MCMCOBJ=   -6647.37391500087     
 iteration          292 MCMCOBJ=   -6623.35910769089     
 iteration          293 MCMCOBJ=   -6593.71969636270     
 iteration          294 MCMCOBJ=   -6573.77837193523     
 iteration          295 MCMCOBJ=   -6614.26153849037     
 iteration          296 MCMCOBJ=   -6611.99289950604     
 iteration          297 MCMCOBJ=   -6619.70531400832     
 iteration          298 MCMCOBJ=   -6631.74902743633     
 iteration          299 MCMCOBJ=   -6640.27038243116     
 iteration          300 MCMCOBJ=   -6653.24365595518     
 iteration          301 MCMCOBJ=   -6666.13990270818     
 iteration          302 MCMCOBJ=   -6640.95398076735     
 iteration          303 MCMCOBJ=   -6636.40254431144     
 iteration          304 MCMCOBJ=   -6618.04039063856     
 iteration          305 MCMCOBJ=   -6677.93332316495     
 iteration          306 MCMCOBJ=   -6606.84623183320     
 iteration          307 MCMCOBJ=   -6621.08009536502     
 iteration          308 MCMCOBJ=   -6597.70043902656     
 iteration          309 MCMCOBJ=   -6624.86425548622     
 iteration          310 MCMCOBJ=   -6538.46229340707     
 iteration          311 MCMCOBJ=   -6620.48386123323     
 iteration          312 MCMCOBJ=   -6604.54221912597     
 iteration          313 MCMCOBJ=   -6567.91847239589     
 iteration          314 MCMCOBJ=   -6595.08604162666     
 iteration          315 MCMCOBJ=   -6599.28552189801     
 iteration          316 MCMCOBJ=   -6582.91688625880     
 iteration          317 MCMCOBJ=   -6608.99398539972     
 iteration          318 MCMCOBJ=   -6603.26305686528     
 iteration          319 MCMCOBJ=   -6603.67189351343     
 iteration          320 MCMCOBJ=   -6588.74387117710     
 iteration          321 MCMCOBJ=   -6587.22263208215     
 iteration          322 MCMCOBJ=   -6630.56777986006     
 iteration          323 MCMCOBJ=   -6628.75368826673     
 iteration          324 MCMCOBJ=   -6625.32155519027     
 iteration          325 MCMCOBJ=   -6629.68003062644     
 iteration          326 MCMCOBJ=   -6631.81637172777     
 iteration          327 MCMCOBJ=   -6625.37836709094     
 iteration          328 MCMCOBJ=   -6616.57356789400     
 iteration          329 MCMCOBJ=   -6664.17818124060     
 iteration          330 MCMCOBJ=   -6642.63943068532     
 iteration          331 MCMCOBJ=   -6568.60396649028     
 iteration          332 MCMCOBJ=   -6576.40553844866     
 iteration          333 MCMCOBJ=   -6577.30308434734     
 iteration          334 MCMCOBJ=   -6590.68979250437     
 iteration          335 MCMCOBJ=   -6608.90904653278     
 iteration          336 MCMCOBJ=   -6622.72134171754     
 iteration          337 MCMCOBJ=   -6658.67182816706     
 iteration          338 MCMCOBJ=   -6635.55417955341     
 iteration          339 MCMCOBJ=   -6657.03267235605     
 iteration          340 MCMCOBJ=   -6668.26336104555     
 iteration          341 MCMCOBJ=   -6661.08857385617     
 iteration          342 MCMCOBJ=   -6658.45588251661     
 iteration          343 MCMCOBJ=   -6658.65134886213     
 iteration          344 MCMCOBJ=   -6689.89642802433     
 iteration          345 MCMCOBJ=   -6673.35571956766     
 iteration          346 MCMCOBJ=   -6700.85625794185     
 iteration          347 MCMCOBJ=   -6684.75956206877     
 iteration          348 MCMCOBJ=   -6653.12948089811     
 iteration          349 MCMCOBJ=   -6645.63097859520     
 iteration          350 MCMCOBJ=   -6596.12463054473     
 iteration          351 MCMCOBJ=   -6600.56272286348     
 iteration          352 MCMCOBJ=   -6622.56554259166     
 iteration          353 MCMCOBJ=   -6559.99064757273     
 iteration          354 MCMCOBJ=   -6632.80307419616     
 iteration          355 MCMCOBJ=   -6634.54109932963     
 iteration          356 MCMCOBJ=   -6622.55797119337     
 iteration          357 MCMCOBJ=   -6633.10478498468     
 iteration          358 MCMCOBJ=   -6637.53674906301     
 iteration          359 MCMCOBJ=   -6646.12205306788     
 iteration          360 MCMCOBJ=   -6599.34181738540     
 iteration          361 MCMCOBJ=   -6586.59171439761     
 iteration          362 MCMCOBJ=   -6572.24527022787     
 iteration          363 MCMCOBJ=   -6572.60446589829     
 iteration          364 MCMCOBJ=   -6586.81158238754     
 iteration          365 MCMCOBJ=   -6578.24306768389     
 iteration          366 MCMCOBJ=   -6604.88210744613     
 iteration          367 MCMCOBJ=   -6634.16854355017     
 iteration          368 MCMCOBJ=   -6683.26403183219     
 iteration          369 MCMCOBJ=   -6661.90923184093     
 iteration          370 MCMCOBJ=   -6640.28926635315     
 iteration          371 MCMCOBJ=   -6600.58202487967     
 iteration          372 MCMCOBJ=   -6611.16731412240     
 iteration          373 MCMCOBJ=   -6631.91639659671     
 iteration          374 MCMCOBJ=   -6656.47904457663     
 iteration          375 MCMCOBJ=   -6667.92424864956     
 iteration          376 MCMCOBJ=   -6634.03702838771     
 iteration          377 MCMCOBJ=   -6675.84431532260     
 iteration          378 MCMCOBJ=   -6614.50268661350     
 iteration          379 MCMCOBJ=   -6645.47867585752     
 iteration          380 MCMCOBJ=   -6646.07539700320     
 iteration          381 MCMCOBJ=   -6622.47796068310     
 iteration          382 MCMCOBJ=   -6584.74800426051     
 iteration          383 MCMCOBJ=   -6604.20696610505     
 iteration          384 MCMCOBJ=   -6598.19129585634     
 iteration          385 MCMCOBJ=   -6647.68118470944     
 iteration          386 MCMCOBJ=   -6623.34327390070     
 iteration          387 MCMCOBJ=   -6623.34329240517     
 iteration          388 MCMCOBJ=   -6647.35658831063     
 iteration          389 MCMCOBJ=   -6631.47122622024     
 iteration          390 MCMCOBJ=   -6658.77856661702     
 iteration          391 MCMCOBJ=   -6667.39534605791     
 iteration          392 MCMCOBJ=   -6670.56139395122     
 iteration          393 MCMCOBJ=   -6654.41312517543     
 iteration          394 MCMCOBJ=   -6637.69015022396     
 iteration          395 MCMCOBJ=   -6641.08993268938     
 iteration          396 MCMCOBJ=   -6614.44148148490     
 iteration          397 MCMCOBJ=   -6610.20955575104     
 iteration          398 MCMCOBJ=   -6614.73495740310     
 iteration          399 MCMCOBJ=   -6650.39548965981     
 iteration          400 MCMCOBJ=   -6669.89884715766     
 iteration          401 MCMCOBJ=   -6552.16977891303     
 iteration          402 MCMCOBJ=   -6627.30694846111     
 iteration          403 MCMCOBJ=   -6627.30695341480     
 iteration          404 MCMCOBJ=   -6587.40600515610     
 iteration          405 MCMCOBJ=   -6600.45371884194     
 iteration          406 MCMCOBJ=   -6627.71749827351     
 iteration          407 MCMCOBJ=   -6598.78827418120     
 iteration          408 MCMCOBJ=   -6613.15781290312     
 iteration          409 MCMCOBJ=   -6618.73726268126     
 iteration          410 MCMCOBJ=   -6667.66867397895     
 iteration          411 MCMCOBJ=   -6622.86789307054     
 iteration          412 MCMCOBJ=   -6632.93642510103     
 iteration          413 MCMCOBJ=   -6605.73857299478     
 iteration          414 MCMCOBJ=   -6621.71189289660     
 iteration          415 MCMCOBJ=   -6619.56031797055     
 iteration          416 MCMCOBJ=   -6630.84158958213     
 iteration          417 MCMCOBJ=   -6591.55389567868     
 iteration          418 MCMCOBJ=   -6599.51963928857     
 iteration          419 MCMCOBJ=   -6579.10564796369     
 iteration          420 MCMCOBJ=   -6646.79610985660     
 iteration          421 MCMCOBJ=   -6642.02096333374     
 iteration          422 MCMCOBJ=   -6614.07907957546     
 iteration          423 MCMCOBJ=   -6552.39191910575     
 iteration          424 MCMCOBJ=   -6574.90101240124     
 iteration          425 MCMCOBJ=   -6590.09657338931     
 iteration          426 MCMCOBJ=   -6603.47072306267     
 iteration          427 MCMCOBJ=   -6574.61807426034     
 iteration          428 MCMCOBJ=   -6651.12041502745     
 iteration          429 MCMCOBJ=   -6639.80888621415     
 iteration          430 MCMCOBJ=   -6627.57595571981     
 iteration          431 MCMCOBJ=   -6585.37930218956     
 iteration          432 MCMCOBJ=   -6581.50322226127     
 iteration          433 MCMCOBJ=   -6625.25084227265     
 iteration          434 MCMCOBJ=   -6625.25084155741     
 iteration          435 MCMCOBJ=   -6641.65624153428     
 iteration          436 MCMCOBJ=   -6627.38656626347     
 iteration          437 MCMCOBJ=   -6670.25470680742     
 iteration          438 MCMCOBJ=   -6670.25470270886     
 iteration          439 MCMCOBJ=   -6671.20667386888     
 iteration          440 MCMCOBJ=   -6666.52271621614     
 iteration          441 MCMCOBJ=   -6642.61190223341     
 iteration          442 MCMCOBJ=   -6610.35398361175     
 iteration          443 MCMCOBJ=   -6646.88154804297     
 iteration          444 MCMCOBJ=   -6572.61545181948     
 iteration          445 MCMCOBJ=   -6631.18048093436     
 iteration          446 MCMCOBJ=   -6636.95313705021     
 iteration          447 MCMCOBJ=   -6662.22413556882     
 iteration          448 MCMCOBJ=   -6663.73413745335     
 iteration          449 MCMCOBJ=   -6669.91670423459     
 iteration          450 MCMCOBJ=   -6642.96425733771     
 iteration          451 MCMCOBJ=   -6639.24949739601     
 iteration          452 MCMCOBJ=   -6612.88273348594     
 iteration          453 MCMCOBJ=   -6584.64926198525     
 iteration          454 MCMCOBJ=   -6612.74657339897     
 iteration          455 MCMCOBJ=   -6626.84640929441     
 iteration          456 MCMCOBJ=   -6642.82461188884     
 iteration          457 MCMCOBJ=   -6587.00741490051     
 iteration          458 MCMCOBJ=   -6631.18327221588     
 iteration          459 MCMCOBJ=   -6644.47815118356     
 iteration          460 MCMCOBJ=   -6621.96583095272     
 iteration          461 MCMCOBJ=   -6666.76345280579     
 iteration          462 MCMCOBJ=   -6668.13958585495     
 iteration          463 MCMCOBJ=   -6672.06258747363     
 iteration          464 MCMCOBJ=   -6654.74525412134     
 iteration          465 MCMCOBJ=   -6611.26602698974     
 iteration          466 MCMCOBJ=   -6630.65982177014     
 iteration          467 MCMCOBJ=   -6628.93917710984     
 iteration          468 MCMCOBJ=   -6616.45906512857     
 iteration          469 MCMCOBJ=   -6628.35301173568     
 iteration          470 MCMCOBJ=   -6669.68904625008     
 iteration          471 MCMCOBJ=   -6633.53265216098     
 iteration          472 MCMCOBJ=   -6659.19667388894     
 iteration          473 MCMCOBJ=   -6643.56706095610     
 iteration          474 MCMCOBJ=   -6643.52720960254     
 iteration          475 MCMCOBJ=   -6625.51640943032     
 iteration          476 MCMCOBJ=   -6605.19975399464     
 iteration          477 MCMCOBJ=   -6613.37021289915     
 iteration          478 MCMCOBJ=   -6651.00154425508     
 iteration          479 MCMCOBJ=   -6676.29627057192     
 iteration          480 MCMCOBJ=   -6674.25491558951     
 iteration          481 MCMCOBJ=   -6638.13420945072     
 iteration          482 MCMCOBJ=   -6612.48237594222     
 iteration          483 MCMCOBJ=   -6634.31247386864     
 iteration          484 MCMCOBJ=   -6683.79716874009     
 iteration          485 MCMCOBJ=   -6638.31242437072     
 iteration          486 MCMCOBJ=   -6692.93198146901     
 iteration          487 MCMCOBJ=   -6717.00833542631     
 iteration          488 MCMCOBJ=   -6669.68277661246     
 iteration          489 MCMCOBJ=   -6698.63964959372     
 iteration          490 MCMCOBJ=   -6675.98960337627     
 iteration          491 MCMCOBJ=   -6688.40574044359     
 iteration          492 MCMCOBJ=   -6684.13939048139     
 iteration          493 MCMCOBJ=   -6676.48212944919     
 iteration          494 MCMCOBJ=   -6660.09361154468     
 iteration          495 MCMCOBJ=   -6633.52231942126     
 iteration          496 MCMCOBJ=   -6614.22543099934     
 iteration          497 MCMCOBJ=   -6618.00367235714     
 iteration          498 MCMCOBJ=   -6606.91824475836     
 iteration          499 MCMCOBJ=   -6593.07042631507     
 iteration          500 MCMCOBJ=   -6655.90737483865     
 iteration          501 MCMCOBJ=   -6627.13926680531     
 iteration          502 MCMCOBJ=   -6616.73369834441     
 iteration          503 MCMCOBJ=   -6620.64843030200     
 iteration          504 MCMCOBJ=   -6631.75403165749     
 iteration          505 MCMCOBJ=   -6623.01392072130     
 iteration          506 MCMCOBJ=   -6643.93491587151     
 iteration          507 MCMCOBJ=   -6617.85164714216     
 iteration          508 MCMCOBJ=   -6588.07786641734     
 iteration          509 MCMCOBJ=   -6639.67670653788     
 iteration          510 MCMCOBJ=   -6586.90077831324     
 iteration          511 MCMCOBJ=   -6658.92585099937     
 iteration          512 MCMCOBJ=   -6639.26080584488     
 iteration          513 MCMCOBJ=   -6680.26336504865     
 iteration          514 MCMCOBJ=   -6671.46058838638     
 iteration          515 MCMCOBJ=   -6633.15339952391     
 iteration          516 MCMCOBJ=   -6639.10986599859     
 iteration          517 MCMCOBJ=   -6681.75185583331     
 iteration          518 MCMCOBJ=   -6623.78485807899     
 iteration          519 MCMCOBJ=   -6646.43041294854     
 iteration          520 MCMCOBJ=   -6651.80180406255     
 iteration          521 MCMCOBJ=   -6657.01820169969     
 iteration          522 MCMCOBJ=   -6619.27632964422     
 iteration          523 MCMCOBJ=   -6596.68217855484     
 iteration          524 MCMCOBJ=   -6634.19865246138     
 iteration          525 MCMCOBJ=   -6667.20648541646     
 iteration          526 MCMCOBJ=   -6623.08267236800     
 iteration          527 MCMCOBJ=   -6636.81173871254     
 iteration          528 MCMCOBJ=   -6617.63378660632     
 iteration          529 MCMCOBJ=   -6633.58871051155     
 iteration          530 MCMCOBJ=   -6652.17166659191     
 iteration          531 MCMCOBJ=   -6672.82930710913     
 iteration          532 MCMCOBJ=   -6674.30228431405     
 iteration          533 MCMCOBJ=   -6648.02814565408     
 iteration          534 MCMCOBJ=   -6608.24673812860     
 iteration          535 MCMCOBJ=   -6620.92095542097     
 iteration          536 MCMCOBJ=   -6593.08690623337     
 iteration          537 MCMCOBJ=   -6577.49096803621     
 iteration          538 MCMCOBJ=   -6571.03232885544     
 iteration          539 MCMCOBJ=   -6625.11324370508     
 iteration          540 MCMCOBJ=   -6588.45251240248     
 iteration          541 MCMCOBJ=   -6611.75910101286     
 iteration          542 MCMCOBJ=   -6584.65739328306     
 iteration          543 MCMCOBJ=   -6655.38451581758     
 iteration          544 MCMCOBJ=   -6635.33324605522     
 iteration          545 MCMCOBJ=   -6640.92303938350     
 iteration          546 MCMCOBJ=   -6640.92303900110     
 iteration          547 MCMCOBJ=   -6649.01636158613     
 iteration          548 MCMCOBJ=   -6618.36667434793     
 iteration          549 MCMCOBJ=   -6649.88612574149     
 iteration          550 MCMCOBJ=   -6617.55699140312     
 iteration          551 MCMCOBJ=   -6605.66192669318     
 iteration          552 MCMCOBJ=   -6623.14087509617     
 iteration          553 MCMCOBJ=   -6588.09412654452     
 iteration          554 MCMCOBJ=   -6579.35607893782     
 iteration          555 MCMCOBJ=   -6651.73258622566     
 iteration          556 MCMCOBJ=   -6602.60723445671     
 iteration          557 MCMCOBJ=   -6664.21430997126     
 iteration          558 MCMCOBJ=   -6626.99215706814     
 iteration          559 MCMCOBJ=   -6658.96825436414     
 iteration          560 MCMCOBJ=   -6665.13530334970     
 iteration          561 MCMCOBJ=   -6644.30026875634     
 iteration          562 MCMCOBJ=   -6628.78493411904     
 iteration          563 MCMCOBJ=   -6565.49979321263     
 iteration          564 MCMCOBJ=   -6587.72120379435     
 iteration          565 MCMCOBJ=   -6586.16684617890     
 iteration          566 MCMCOBJ=   -6618.67284934292     
 iteration          567 MCMCOBJ=   -6636.15141888504     
 iteration          568 MCMCOBJ=   -6602.59110026608     
 iteration          569 MCMCOBJ=   -6599.06682510005     
 iteration          570 MCMCOBJ=   -6611.73666117656     
 iteration          571 MCMCOBJ=   -6627.74100255107     
 iteration          572 MCMCOBJ=   -6615.53225528723     
 iteration          573 MCMCOBJ=   -6610.64586189858     
 iteration          574 MCMCOBJ=   -6623.19381880197     
 iteration          575 MCMCOBJ=   -6641.23372927038     
 iteration          576 MCMCOBJ=   -6645.64940178002     
 iteration          577 MCMCOBJ=   -6632.02217041602     
 iteration          578 MCMCOBJ=   -6611.22042092042     
 iteration          579 MCMCOBJ=   -6633.65460020547     
 iteration          580 MCMCOBJ=   -6650.17078488084     
 iteration          581 MCMCOBJ=   -6675.08629376011     
 iteration          582 MCMCOBJ=   -6698.35108344517     
 iteration          583 MCMCOBJ=   -6698.35109867188     
 iteration          584 MCMCOBJ=   -6668.59831922502     
 iteration          585 MCMCOBJ=   -6636.20830785366     
 iteration          586 MCMCOBJ=   -6634.12165112700     
 iteration          587 MCMCOBJ=   -6660.18568885480     
 iteration          588 MCMCOBJ=   -6692.49578634510     
 iteration          589 MCMCOBJ=   -6589.84147888188     
 iteration          590 MCMCOBJ=   -6669.32275901420     
 iteration          591 MCMCOBJ=   -6661.94857172395     
 iteration          592 MCMCOBJ=   -6685.86251444268     
 iteration          593 MCMCOBJ=   -6690.02223667248     
 iteration          594 MCMCOBJ=   -6681.46460070812     
 iteration          595 MCMCOBJ=   -6632.31492744381     
 iteration          596 MCMCOBJ=   -6608.52939016687     
 iteration          597 MCMCOBJ=   -6690.19186881574     
 iteration          598 MCMCOBJ=   -6658.91847013754     
 iteration          599 MCMCOBJ=   -6671.98816413203     
 iteration          600 MCMCOBJ=   -6678.89899391549     
 iteration          601 MCMCOBJ=   -6652.81942449351     
 iteration          602 MCMCOBJ=   -6620.41781374183     
 iteration          603 MCMCOBJ=   -6595.16172214810     
 iteration          604 MCMCOBJ=   -6625.26202052650     
 iteration          605 MCMCOBJ=   -6638.22060818236     
 iteration          606 MCMCOBJ=   -6678.54262233039     
 iteration          607 MCMCOBJ=   -6635.19087850908     
 iteration          608 MCMCOBJ=   -6592.29745732765     
 iteration          609 MCMCOBJ=   -6603.27751147620     
 iteration          610 MCMCOBJ=   -6583.93898689104     
 iteration          611 MCMCOBJ=   -6652.40943425888     
 iteration          612 MCMCOBJ=   -6650.26969122732     
 iteration          613 MCMCOBJ=   -6610.13561655893     
 iteration          614 MCMCOBJ=   -6663.36438302838     
 iteration          615 MCMCOBJ=   -6663.36438210756     
 iteration          616 MCMCOBJ=   -6649.17750299653     
 iteration          617 MCMCOBJ=   -6650.72166747054     
 iteration          618 MCMCOBJ=   -6642.59638394361     
 iteration          619 MCMCOBJ=   -6636.96090710435     
 iteration          620 MCMCOBJ=   -6577.68739359216     
 iteration          621 MCMCOBJ=   -6549.90827418487     
 iteration          622 MCMCOBJ=   -6570.29147386242     
 iteration          623 MCMCOBJ=   -6620.95431820233     
 iteration          624 MCMCOBJ=   -6623.03802552984     
 iteration          625 MCMCOBJ=   -6648.85165300284     
 iteration          626 MCMCOBJ=   -6642.64719768056     
 iteration          627 MCMCOBJ=   -6615.61657514070     
 iteration          628 MCMCOBJ=   -6650.51477549520     
 iteration          629 MCMCOBJ=   -6655.73189043441     
 iteration          630 MCMCOBJ=   -6639.48709583957     
 iteration          631 MCMCOBJ=   -6612.06306336964     
 iteration          632 MCMCOBJ=   -6623.04420428651     
 iteration          633 MCMCOBJ=   -6621.33311975339     
 iteration          634 MCMCOBJ=   -6640.38621682047     
 iteration          635 MCMCOBJ=   -6594.92128904896     
 iteration          636 MCMCOBJ=   -6612.96616812668     
 iteration          637 MCMCOBJ=   -6601.18262724563     
 iteration          638 MCMCOBJ=   -6576.80854574587     
 iteration          639 MCMCOBJ=   -6595.00954742500     
 iteration          640 MCMCOBJ=   -6601.03239036869     
 iteration          641 MCMCOBJ=   -6565.82684406416     
 iteration          642 MCMCOBJ=   -6555.07861511544     
 iteration          643 MCMCOBJ=   -6539.08265052512     
 iteration          644 MCMCOBJ=   -6544.82985523601     
 iteration          645 MCMCOBJ=   -6600.09555315836     
 iteration          646 MCMCOBJ=   -6631.89722869936     
 iteration          647 MCMCOBJ=   -6655.01304760519     
 iteration          648 MCMCOBJ=   -6636.68578817301     
 iteration          649 MCMCOBJ=   -6662.63946850236     
 iteration          650 MCMCOBJ=   -6622.18027486860     
 iteration          651 MCMCOBJ=   -6595.71378632020     
 iteration          652 MCMCOBJ=   -6621.78373862366     
 iteration          653 MCMCOBJ=   -6620.77523970410     
 iteration          654 MCMCOBJ=   -6618.07031749030     
 iteration          655 MCMCOBJ=   -6601.25218190415     
 iteration          656 MCMCOBJ=   -6627.94794305523     
 iteration          657 MCMCOBJ=   -6614.74901048772     
 iteration          658 MCMCOBJ=   -6674.24578561814     
 iteration          659 MCMCOBJ=   -6621.73204456377     
 iteration          660 MCMCOBJ=   -6617.90391529334     
 iteration          661 MCMCOBJ=   -6590.64284858994     
 iteration          662 MCMCOBJ=   -6597.00749505393     
 iteration          663 MCMCOBJ=   -6617.07306940264     
 iteration          664 MCMCOBJ=   -6615.61980626782     
 iteration          665 MCMCOBJ=   -6564.95966667172     
 iteration          666 MCMCOBJ=   -6612.71607170105     
 iteration          667 MCMCOBJ=   -6654.58972398344     
 iteration          668 MCMCOBJ=   -6599.72832716088     
 iteration          669 MCMCOBJ=   -6581.24042058622     
 iteration          670 MCMCOBJ=   -6610.90982474329     
 iteration          671 MCMCOBJ=   -6645.19147477034     
 iteration          672 MCMCOBJ=   -6617.72213361469     
 iteration          673 MCMCOBJ=   -6595.58861865917     
 iteration          674 MCMCOBJ=   -6637.45659200318     
 iteration          675 MCMCOBJ=   -6580.35317852057     
 iteration          676 MCMCOBJ=   -6606.20843775358     
 iteration          677 MCMCOBJ=   -6620.11243104754     
 iteration          678 MCMCOBJ=   -6607.99925793720     
 iteration          679 MCMCOBJ=   -6631.50381924538     
 iteration          680 MCMCOBJ=   -6617.64611759613     
 iteration          681 MCMCOBJ=   -6643.88427410309     
 iteration          682 MCMCOBJ=   -6622.49248074412     
 iteration          683 MCMCOBJ=   -6589.28277817130     
 iteration          684 MCMCOBJ=   -6628.10807206952     
 iteration          685 MCMCOBJ=   -6636.46128051469     
 iteration          686 MCMCOBJ=   -6636.46128265283     
 iteration          687 MCMCOBJ=   -6649.25918793342     
 iteration          688 MCMCOBJ=   -6684.90822945463     
 iteration          689 MCMCOBJ=   -6694.41278861967     
 iteration          690 MCMCOBJ=   -6655.47131986166     
 iteration          691 MCMCOBJ=   -6580.45664569883     
 iteration          692 MCMCOBJ=   -6631.25103437622     
 iteration          693 MCMCOBJ=   -6580.13737364233     
 iteration          694 MCMCOBJ=   -6588.52063443099     
 iteration          695 MCMCOBJ=   -6618.30494198896     
 iteration          696 MCMCOBJ=   -6565.45044699141     
 iteration          697 MCMCOBJ=   -6609.14920295961     
 iteration          698 MCMCOBJ=   -6585.63704003693     
 iteration          699 MCMCOBJ=   -6607.48755066027     
 iteration          700 MCMCOBJ=   -6617.28391621794     
 iteration          701 MCMCOBJ=   -6663.97528566007     
 iteration          702 MCMCOBJ=   -6644.16467827974     
 iteration          703 MCMCOBJ=   -6628.65443687271     
 iteration          704 MCMCOBJ=   -6618.95908549050     
 iteration          705 MCMCOBJ=   -6659.80066709642     
 iteration          706 MCMCOBJ=   -6636.89975399190     
 iteration          707 MCMCOBJ=   -6641.98283292229     
 iteration          708 MCMCOBJ=   -6617.72207767397     
 iteration          709 MCMCOBJ=   -6617.26255698939     
 iteration          710 MCMCOBJ=   -6536.48980695803     
 iteration          711 MCMCOBJ=   -6594.91958646981     
 iteration          712 MCMCOBJ=   -6560.78559749149     
 iteration          713 MCMCOBJ=   -6634.88180506825     
 iteration          714 MCMCOBJ=   -6654.87098976460     
 iteration          715 MCMCOBJ=   -6652.22607050759     
 iteration          716 MCMCOBJ=   -6601.16696008927     
 iteration          717 MCMCOBJ=   -6621.99316414515     
 iteration          718 MCMCOBJ=   -6584.84146639224     
 iteration          719 MCMCOBJ=   -6579.04849795058     
 iteration          720 MCMCOBJ=   -6593.67157395298     
 iteration          721 MCMCOBJ=   -6643.57958715411     
 iteration          722 MCMCOBJ=   -6693.43538460360     
 iteration          723 MCMCOBJ=   -6694.38539270476     
 iteration          724 MCMCOBJ=   -6604.35121781313     
 iteration          725 MCMCOBJ=   -6648.24247607175     
 iteration          726 MCMCOBJ=   -6602.35939078553     
 iteration          727 MCMCOBJ=   -6539.13228380031     
 iteration          728 MCMCOBJ=   -6555.85571130831     
 iteration          729 MCMCOBJ=   -6612.99335730657     
 iteration          730 MCMCOBJ=   -6602.90937444153     
 iteration          731 MCMCOBJ=   -6629.98390207247     
 iteration          732 MCMCOBJ=   -6658.01434769232     
 iteration          733 MCMCOBJ=   -6654.93129026895     
 iteration          734 MCMCOBJ=   -6612.71125688185     
 iteration          735 MCMCOBJ=   -6586.71963235635     
 iteration          736 MCMCOBJ=   -6607.52636639667     
 iteration          737 MCMCOBJ=   -6558.58324031176     
 iteration          738 MCMCOBJ=   -6599.31550522591     
 iteration          739 MCMCOBJ=   -6616.84032830899     
 iteration          740 MCMCOBJ=   -6637.03049419932     
 iteration          741 MCMCOBJ=   -6656.49543976764     
 iteration          742 MCMCOBJ=   -6688.07585482696     
 iteration          743 MCMCOBJ=   -6625.71469240798     
 iteration          744 MCMCOBJ=   -6600.05699964008     
 iteration          745 MCMCOBJ=   -6649.56771687505     
 iteration          746 MCMCOBJ=   -6608.92105285186     
 iteration          747 MCMCOBJ=   -6587.77647154103     
 iteration          748 MCMCOBJ=   -6605.63706964376     
 iteration          749 MCMCOBJ=   -6585.08703695650     
 iteration          750 MCMCOBJ=   -6606.57680754208     
 iteration          751 MCMCOBJ=   -6583.84411673847     
 iteration          752 MCMCOBJ=   -6566.69528075371     
 iteration          753 MCMCOBJ=   -6592.61908149837     
 iteration          754 MCMCOBJ=   -6630.28531336048     
 iteration          755 MCMCOBJ=   -6611.82052452832     
 iteration          756 MCMCOBJ=   -6611.77376917670     
 iteration          757 MCMCOBJ=   -6650.48591411724     
 iteration          758 MCMCOBJ=   -6661.50665647217     
 iteration          759 MCMCOBJ=   -6638.56633700846     
 iteration          760 MCMCOBJ=   -6640.07927285685     
 iteration          761 MCMCOBJ=   -6643.28732810991     
 iteration          762 MCMCOBJ=   -6623.71364588045     
 iteration          763 MCMCOBJ=   -6662.73813915919     
 iteration          764 MCMCOBJ=   -6690.09781086338     
 iteration          765 MCMCOBJ=   -6679.81839684889     
 iteration          766 MCMCOBJ=   -6638.52917122212     
 iteration          767 MCMCOBJ=   -6652.46031346069     
 iteration          768 MCMCOBJ=   -6652.46031396134     
 iteration          769 MCMCOBJ=   -6635.66278249456     
 iteration          770 MCMCOBJ=   -6619.14843534363     
 iteration          771 MCMCOBJ=   -6625.13701653111     
 iteration          772 MCMCOBJ=   -6590.51832224296     
 iteration          773 MCMCOBJ=   -6577.57005528284     
 iteration          774 MCMCOBJ=   -6595.70971553408     
 iteration          775 MCMCOBJ=   -6604.62433413662     
 iteration          776 MCMCOBJ=   -6573.65880311354     
 iteration          777 MCMCOBJ=   -6658.71274795954     
 iteration          778 MCMCOBJ=   -6641.85910395496     
 iteration          779 MCMCOBJ=   -6612.51748709801     
 iteration          780 MCMCOBJ=   -6609.70173915190     
 iteration          781 MCMCOBJ=   -6656.26453007990     
 iteration          782 MCMCOBJ=   -6619.12088997400     
 iteration          783 MCMCOBJ=   -6623.31042473767     
 iteration          784 MCMCOBJ=   -6656.40022036820     
 iteration          785 MCMCOBJ=   -6691.96659470187     
 iteration          786 MCMCOBJ=   -6682.21008317243     
 iteration          787 MCMCOBJ=   -6643.92952162749     
 iteration          788 MCMCOBJ=   -6636.38569195074     
 iteration          789 MCMCOBJ=   -6623.45256464711     
 iteration          790 MCMCOBJ=   -6674.00355734929     
 iteration          791 MCMCOBJ=   -6661.61054649655     
 iteration          792 MCMCOBJ=   -6655.93526668186     
 iteration          793 MCMCOBJ=   -6664.93172079740     
 iteration          794 MCMCOBJ=   -6684.27192077754     
 iteration          795 MCMCOBJ=   -6651.55454970543     
 iteration          796 MCMCOBJ=   -6670.22418152850     
 iteration          797 MCMCOBJ=   -6671.64047486320     
 iteration          798 MCMCOBJ=   -6674.02735833733     
 iteration          799 MCMCOBJ=   -6657.07457677502     
 iteration          800 MCMCOBJ=   -6641.42688118361     
 iteration          801 MCMCOBJ=   -6668.07841469336     
 iteration          802 MCMCOBJ=   -6635.30436289419     
 iteration          803 MCMCOBJ=   -6602.37239354105     
 iteration          804 MCMCOBJ=   -6598.71039317795     
 iteration          805 MCMCOBJ=   -6607.62981884088     
 iteration          806 MCMCOBJ=   -6609.86409111380     
 iteration          807 MCMCOBJ=   -6621.05245708143     
 iteration          808 MCMCOBJ=   -6620.27180643461     
 iteration          809 MCMCOBJ=   -6648.91492124895     
 iteration          810 MCMCOBJ=   -6681.72013225309     
 iteration          811 MCMCOBJ=   -6681.72013451456     
 iteration          812 MCMCOBJ=   -6668.96635285614     
 iteration          813 MCMCOBJ=   -6655.91853222998     
 iteration          814 MCMCOBJ=   -6636.21264611732     
 iteration          815 MCMCOBJ=   -6570.77393244540     
 iteration          816 MCMCOBJ=   -6597.69398205138     
 iteration          817 MCMCOBJ=   -6626.65146873627     
 iteration          818 MCMCOBJ=   -6599.07980632166     
 iteration          819 MCMCOBJ=   -6622.61599728763     
 iteration          820 MCMCOBJ=   -6622.61602437405     
 iteration          821 MCMCOBJ=   -6602.48480410909     
 iteration          822 MCMCOBJ=   -6597.09782224469     
 iteration          823 MCMCOBJ=   -6592.88300679306     
 iteration          824 MCMCOBJ=   -6615.20900350614     
 iteration          825 MCMCOBJ=   -6602.00454101909     
 iteration          826 MCMCOBJ=   -6535.68959598085     
 iteration          827 MCMCOBJ=   -6573.62176934770     
 iteration          828 MCMCOBJ=   -6575.67694873251     
 iteration          829 MCMCOBJ=   -6628.99374223431     
 iteration          830 MCMCOBJ=   -6574.51175907667     
 iteration          831 MCMCOBJ=   -6600.04876557385     
 iteration          832 MCMCOBJ=   -6558.65834975090     
 iteration          833 MCMCOBJ=   -6590.87866454727     
 iteration          834 MCMCOBJ=   -6553.51018780793     
 iteration          835 MCMCOBJ=   -6599.90660207070     
 iteration          836 MCMCOBJ=   -6620.70137973457     
 iteration          837 MCMCOBJ=   -6644.32688937979     
 iteration          838 MCMCOBJ=   -6640.23220059998     
 iteration          839 MCMCOBJ=   -6669.21300262214     
 iteration          840 MCMCOBJ=   -6686.31934933042     
 iteration          841 MCMCOBJ=   -6700.33926479455     
 iteration          842 MCMCOBJ=   -6694.30971887719     
 iteration          843 MCMCOBJ=   -6683.95303304858     
 iteration          844 MCMCOBJ=   -6631.38528878176     
 iteration          845 MCMCOBJ=   -6652.05717477520     
 iteration          846 MCMCOBJ=   -6611.09404325420     
 iteration          847 MCMCOBJ=   -6681.21969009656     
 iteration          848 MCMCOBJ=   -6631.03771262367     
 iteration          849 MCMCOBJ=   -6632.80650471764     
 iteration          850 MCMCOBJ=   -6646.80117871245     
 iteration          851 MCMCOBJ=   -6674.08496185680     
 iteration          852 MCMCOBJ=   -6657.73599817731     
 iteration          853 MCMCOBJ=   -6647.27319917822     
 iteration          854 MCMCOBJ=   -6624.20979488835     
 iteration          855 MCMCOBJ=   -6633.56957806336     
 iteration          856 MCMCOBJ=   -6602.37729923872     
 iteration          857 MCMCOBJ=   -6594.99635244703     
 iteration          858 MCMCOBJ=   -6621.99332963018     
 iteration          859 MCMCOBJ=   -6643.24384947856     
 iteration          860 MCMCOBJ=   -6624.24911885390     
 iteration          861 MCMCOBJ=   -6670.10608489705     
 iteration          862 MCMCOBJ=   -6658.71080893812     
 iteration          863 MCMCOBJ=   -6672.68887772517     
 iteration          864 MCMCOBJ=   -6641.19238826595     
 iteration          865 MCMCOBJ=   -6679.88844221705     
 iteration          866 MCMCOBJ=   -6653.99005306919     
 iteration          867 MCMCOBJ=   -6657.85193393083     
 iteration          868 MCMCOBJ=   -6624.09172408944     
 iteration          869 MCMCOBJ=   -6610.43781547991     
 iteration          870 MCMCOBJ=   -6626.57090530859     
 iteration          871 MCMCOBJ=   -6678.70055599526     
 iteration          872 MCMCOBJ=   -6618.46347425429     
 iteration          873 MCMCOBJ=   -6597.97600547605     
 iteration          874 MCMCOBJ=   -6653.29457122927     
 iteration          875 MCMCOBJ=   -6664.44123063475     
 iteration          876 MCMCOBJ=   -6635.56652814628     
 iteration          877 MCMCOBJ=   -6668.33666118149     
 iteration          878 MCMCOBJ=   -6668.33666116202     
 iteration          879 MCMCOBJ=   -6679.32464883240     
 iteration          880 MCMCOBJ=   -6638.56195626290     
 iteration          881 MCMCOBJ=   -6635.63238933489     
 iteration          882 MCMCOBJ=   -6681.66261559770     
 iteration          883 MCMCOBJ=   -6688.11696694411     
 iteration          884 MCMCOBJ=   -6719.29438957064     
 iteration          885 MCMCOBJ=   -6709.09477511405     
 iteration          886 MCMCOBJ=   -6709.75209066917     
 iteration          887 MCMCOBJ=   -6707.14443223247     
 iteration          888 MCMCOBJ=   -6713.50494643820     
 iteration          889 MCMCOBJ=   -6684.78922927754     
 iteration          890 MCMCOBJ=   -6702.31393284372     
 iteration          891 MCMCOBJ=   -6747.50671267564     
 iteration          892 MCMCOBJ=   -6649.50045767835     
 iteration          893 MCMCOBJ=   -6702.18823538219     
 iteration          894 MCMCOBJ=   -6727.26222166306     
 iteration          895 MCMCOBJ=   -6690.77053471118     
 iteration          896 MCMCOBJ=   -6675.15640786703     
 iteration          897 MCMCOBJ=   -6659.36738781746     
 iteration          898 MCMCOBJ=   -6697.61778470303     
 iteration          899 MCMCOBJ=   -6651.18569682936     
 iteration          900 MCMCOBJ=   -6635.77210637033     
 iteration          901 MCMCOBJ=   -6655.86366951300     
 iteration          902 MCMCOBJ=   -6661.21167408049     
 iteration          903 MCMCOBJ=   -6647.97159426041     
 iteration          904 MCMCOBJ=   -6629.55513595341     
 iteration          905 MCMCOBJ=   -6621.25871723909     
 iteration          906 MCMCOBJ=   -6596.83905183238     
 iteration          907 MCMCOBJ=   -6593.56730264736     
 iteration          908 MCMCOBJ=   -6650.59496006979     
 iteration          909 MCMCOBJ=   -6647.85405923819     
 iteration          910 MCMCOBJ=   -6659.60105474402     
 iteration          911 MCMCOBJ=   -6657.13745690916     
 iteration          912 MCMCOBJ=   -6648.18579868588     
 iteration          913 MCMCOBJ=   -6609.65520487250     
 iteration          914 MCMCOBJ=   -6613.07441380488     
 iteration          915 MCMCOBJ=   -6634.20418964468     
 iteration          916 MCMCOBJ=   -6626.90277289772     
 iteration          917 MCMCOBJ=   -6593.32356938283     
 iteration          918 MCMCOBJ=   -6646.06597490648     
 iteration          919 MCMCOBJ=   -6625.18750781184     
 iteration          920 MCMCOBJ=   -6634.88429088025     
 iteration          921 MCMCOBJ=   -6628.73089479060     
 iteration          922 MCMCOBJ=   -6582.24342556971     
 iteration          923 MCMCOBJ=   -6587.07642792892     
 iteration          924 MCMCOBJ=   -6524.64092362431     
 iteration          925 MCMCOBJ=   -6588.45735286596     
 iteration          926 MCMCOBJ=   -6535.38873608292     
 iteration          927 MCMCOBJ=   -6571.06271814703     
 iteration          928 MCMCOBJ=   -6571.06271789403     
 iteration          929 MCMCOBJ=   -6602.78864952595     
 iteration          930 MCMCOBJ=   -6604.52411508457     
 iteration          931 MCMCOBJ=   -6575.01460128456     
 iteration          932 MCMCOBJ=   -6589.82128388580     
 iteration          933 MCMCOBJ=   -6623.76213208569     
 iteration          934 MCMCOBJ=   -6570.12888557946     
 iteration          935 MCMCOBJ=   -6567.38741759154     
 iteration          936 MCMCOBJ=   -6690.81262590934     
 iteration          937 MCMCOBJ=   -6625.07421861727     
 iteration          938 MCMCOBJ=   -6576.12305855245     
 iteration          939 MCMCOBJ=   -6572.91148581942     
 iteration          940 MCMCOBJ=   -6599.23664524756     
 iteration          941 MCMCOBJ=   -6660.61241229103     
 iteration          942 MCMCOBJ=   -6660.61241037985     
 iteration          943 MCMCOBJ=   -6681.06927260038     
 iteration          944 MCMCOBJ=   -6679.72725038783     
 iteration          945 MCMCOBJ=   -6679.72716244437     
 iteration          946 MCMCOBJ=   -6652.50278243453     
 iteration          947 MCMCOBJ=   -6665.13073872217     
 iteration          948 MCMCOBJ=   -6602.88673968820     
 iteration          949 MCMCOBJ=   -6548.57267042499     
 iteration          950 MCMCOBJ=   -6569.15987858105     
 iteration          951 MCMCOBJ=   -6579.37388617381     
 iteration          952 MCMCOBJ=   -6651.71256468281     
 iteration          953 MCMCOBJ=   -6651.71257315482     
 iteration          954 MCMCOBJ=   -6671.63107219126     
 iteration          955 MCMCOBJ=   -6631.57766898944     
 iteration          956 MCMCOBJ=   -6684.45895296732     
 iteration          957 MCMCOBJ=   -6684.45895364633     
 iteration          958 MCMCOBJ=   -6684.45895355354     
 iteration          959 MCMCOBJ=   -6674.91074151118     
 iteration          960 MCMCOBJ=   -6644.04000475843     
 iteration          961 MCMCOBJ=   -6648.03731236619     
 iteration          962 MCMCOBJ=   -6648.68810069703     
 iteration          963 MCMCOBJ=   -6630.45279018316     
 iteration          964 MCMCOBJ=   -6560.96727327724     
 iteration          965 MCMCOBJ=   -6588.12017766050     
 iteration          966 MCMCOBJ=   -6566.82464935727     
 iteration          967 MCMCOBJ=   -6606.66139718278     
 iteration          968 MCMCOBJ=   -6611.44112740765     
 iteration          969 MCMCOBJ=   -6634.79859803777     
 iteration          970 MCMCOBJ=   -6633.77565056520     
 iteration          971 MCMCOBJ=   -6654.32860567789     
 iteration          972 MCMCOBJ=   -6622.01094710413     
 iteration          973 MCMCOBJ=   -6630.43257700218     
 iteration          974 MCMCOBJ=   -6648.63060500916     
 iteration          975 MCMCOBJ=   -6628.08946035380     
 iteration          976 MCMCOBJ=   -6581.61343717196     
 iteration          977 MCMCOBJ=   -6586.31850731412     
 iteration          978 MCMCOBJ=   -6602.76174945329     
 iteration          979 MCMCOBJ=   -6647.39527350514     
 iteration          980 MCMCOBJ=   -6621.28006337304     
 iteration          981 MCMCOBJ=   -6671.85773298115     
 iteration          982 MCMCOBJ=   -6674.80674856883     
 iteration          983 MCMCOBJ=   -6602.73540224147     
 iteration          984 MCMCOBJ=   -6618.30656828928     
 iteration          985 MCMCOBJ=   -6650.53919675484     
 iteration          986 MCMCOBJ=   -6661.52459901231     
 iteration          987 MCMCOBJ=   -6677.24160776879     
 iteration          988 MCMCOBJ=   -6697.43810310569     
 iteration          989 MCMCOBJ=   -6658.59993407884     
 iteration          990 MCMCOBJ=   -6650.69734196931     
 iteration          991 MCMCOBJ=   -6679.39716062911     
 iteration          992 MCMCOBJ=   -6651.80507107420     
 iteration          993 MCMCOBJ=   -6670.43057767461     
 iteration          994 MCMCOBJ=   -6635.15793153919     
 iteration          995 MCMCOBJ=   -6664.26423762588     
 iteration          996 MCMCOBJ=   -6643.72739929205     
 iteration          997 MCMCOBJ=   -6612.64410791421     
 iteration          998 MCMCOBJ=   -6640.01617961635     
 iteration          999 MCMCOBJ=   -6610.35984182874     
 iteration         1000 MCMCOBJ=   -6614.33153785869     
 iteration         1001 MCMCOBJ=   -6657.09074907233     
 iteration         1002 MCMCOBJ=   -6629.76021256467     
 iteration         1003 MCMCOBJ=   -6576.84539987838     
 iteration         1004 MCMCOBJ=   -6565.68520298559     
 iteration         1005 MCMCOBJ=   -6589.25867076581     
 iteration         1006 MCMCOBJ=   -6615.44443953736     
 iteration         1007 MCMCOBJ=   -6610.68568197014     
 iteration         1008 MCMCOBJ=   -6615.67780605593     
 iteration         1009 MCMCOBJ=   -6624.87253861421     
 iteration         1010 MCMCOBJ=   -6640.59545921888     
 iteration         1011 MCMCOBJ=   -6583.63770149494     
 iteration         1012 MCMCOBJ=   -6610.39201843559     
 iteration         1013 MCMCOBJ=   -6631.66799424245     
 iteration         1014 MCMCOBJ=   -6656.31862336342     
 iteration         1015 MCMCOBJ=   -6672.54169975496     
 iteration         1016 MCMCOBJ=   -6609.53951242948     
 iteration         1017 MCMCOBJ=   -6630.04579753957     
 iteration         1018 MCMCOBJ=   -6605.46422717237     
 iteration         1019 MCMCOBJ=   -6636.21740162697     
 iteration         1020 MCMCOBJ=   -6602.56109577818     
 iteration         1021 MCMCOBJ=   -6627.93319115233     
 iteration         1022 MCMCOBJ=   -6655.78766483741     
 iteration         1023 MCMCOBJ=   -6589.53657510387     
 iteration         1024 MCMCOBJ=   -6557.21487754756     
 iteration         1025 MCMCOBJ=   -6595.72363390648     
 iteration         1026 MCMCOBJ=   -6620.55246027455     
 iteration         1027 MCMCOBJ=   -6613.00609433165     
 iteration         1028 MCMCOBJ=   -6610.55100515498     
 iteration         1029 MCMCOBJ=   -6648.70140029974     
 iteration         1030 MCMCOBJ=   -6610.37199341482     
 iteration         1031 MCMCOBJ=   -6643.36554735119     
 iteration         1032 MCMCOBJ=   -6587.98734547706     
 iteration         1033 MCMCOBJ=   -6590.42864555907     
 iteration         1034 MCMCOBJ=   -6579.16739912316     
 iteration         1035 MCMCOBJ=   -6589.51588232396     
 iteration         1036 MCMCOBJ=   -6597.95726081592     
 iteration         1037 MCMCOBJ=   -6603.64938970029     
 iteration         1038 MCMCOBJ=   -6667.26505064038     
 iteration         1039 MCMCOBJ=   -6638.30107133489     
 iteration         1040 MCMCOBJ=   -6653.19634785581     
 iteration         1041 MCMCOBJ=   -6610.63857080622     
 iteration         1042 MCMCOBJ=   -6618.71509513542     
 iteration         1043 MCMCOBJ=   -6614.06285721926     
 iteration         1044 MCMCOBJ=   -6666.82060899100     
 iteration         1045 MCMCOBJ=   -6730.20160294923     
 iteration         1046 MCMCOBJ=   -6734.10620344880     
 iteration         1047 MCMCOBJ=   -6728.65223244105     
 iteration         1048 MCMCOBJ=   -6728.65223233798     
 iteration         1049 MCMCOBJ=   -6728.55383312324     
 iteration         1050 MCMCOBJ=   -6681.94383545671     
 iteration         1051 MCMCOBJ=   -6643.78820588866     
 iteration         1052 MCMCOBJ=   -6647.38945857015     
 iteration         1053 MCMCOBJ=   -6622.10349663410     
 iteration         1054 MCMCOBJ=   -6657.49590326794     
 iteration         1055 MCMCOBJ=   -6582.41460655925     
 iteration         1056 MCMCOBJ=   -6588.39097415394     
 iteration         1057 MCMCOBJ=   -6635.08678471598     
 iteration         1058 MCMCOBJ=   -6680.44831028526     
 iteration         1059 MCMCOBJ=   -6657.87328061801     
 iteration         1060 MCMCOBJ=   -6694.37021909916     
 iteration         1061 MCMCOBJ=   -6679.08838546209     
 iteration         1062 MCMCOBJ=   -6692.10421188011     
 iteration         1063 MCMCOBJ=   -6667.24492089341     
 iteration         1064 MCMCOBJ=   -6594.09274366626     
 iteration         1065 MCMCOBJ=   -6590.83214042953     
 iteration         1066 MCMCOBJ=   -6576.22023083808     
 iteration         1067 MCMCOBJ=   -6626.06241121961     
 iteration         1068 MCMCOBJ=   -6639.22415409237     
 iteration         1069 MCMCOBJ=   -6603.86349801369     
 iteration         1070 MCMCOBJ=   -6619.40483930887     
 iteration         1071 MCMCOBJ=   -6629.74964631936     
 iteration         1072 MCMCOBJ=   -6608.95358719527     
 iteration         1073 MCMCOBJ=   -6596.48170038421     
 iteration         1074 MCMCOBJ=   -6619.34821543232     
 iteration         1075 MCMCOBJ=   -6607.52987627874     
 iteration         1076 MCMCOBJ=   -6655.93788377451     
 iteration         1077 MCMCOBJ=   -6685.65243572541     
 iteration         1078 MCMCOBJ=   -6690.96242144252     
 iteration         1079 MCMCOBJ=   -6690.96244419463     
 iteration         1080 MCMCOBJ=   -6629.20550019223     
 iteration         1081 MCMCOBJ=   -6647.00976104455     
 iteration         1082 MCMCOBJ=   -6616.83552440741     
 iteration         1083 MCMCOBJ=   -6635.61658512779     
 iteration         1084 MCMCOBJ=   -6611.19681830700     
 iteration         1085 MCMCOBJ=   -6605.95743002736     
 iteration         1086 MCMCOBJ=   -6594.35490778431     
 iteration         1087 MCMCOBJ=   -6586.71901208677     
 iteration         1088 MCMCOBJ=   -6613.82425699440     
 iteration         1089 MCMCOBJ=   -6600.48384746988     
 iteration         1090 MCMCOBJ=   -6590.53635802594     
 iteration         1091 MCMCOBJ=   -6563.55306930337     
 iteration         1092 MCMCOBJ=   -6533.46625830625     
 iteration         1093 MCMCOBJ=   -6562.65533166011     
 iteration         1094 MCMCOBJ=   -6642.50957316655     
 iteration         1095 MCMCOBJ=   -6625.73762464401     
 iteration         1096 MCMCOBJ=   -6640.29842204101     
 iteration         1097 MCMCOBJ=   -6637.16136263612     
 iteration         1098 MCMCOBJ=   -6574.93922102202     
 iteration         1099 MCMCOBJ=   -6594.19407673538     
 iteration         1100 MCMCOBJ=   -6580.80693462039     
 iteration         1101 MCMCOBJ=   -6565.06879403394     
 iteration         1102 MCMCOBJ=   -6635.52193290332     
 iteration         1103 MCMCOBJ=   -6615.26622806626     
 iteration         1104 MCMCOBJ=   -6592.77411726124     
 iteration         1105 MCMCOBJ=   -6523.71679489569     
 iteration         1106 MCMCOBJ=   -6575.23062395178     
 iteration         1107 MCMCOBJ=   -6605.38773718111     
 iteration         1108 MCMCOBJ=   -6571.21203183244     
 iteration         1109 MCMCOBJ=   -6537.70885070108     
 iteration         1110 MCMCOBJ=   -6582.87432609668     
 iteration         1111 MCMCOBJ=   -6611.48697740696     
 iteration         1112 MCMCOBJ=   -6613.15159889343     
 iteration         1113 MCMCOBJ=   -6652.31349988183     
 iteration         1114 MCMCOBJ=   -6629.03239170584     
 iteration         1115 MCMCOBJ=   -6633.99248506462     
 iteration         1116 MCMCOBJ=   -6649.58268125579     
 iteration         1117 MCMCOBJ=   -6614.05581751330     
 iteration         1118 MCMCOBJ=   -6621.96453157281     
 iteration         1119 MCMCOBJ=   -6643.04756881465     
 iteration         1120 MCMCOBJ=   -6673.39555165501     
 iteration         1121 MCMCOBJ=   -6666.84566484751     
 iteration         1122 MCMCOBJ=   -6649.78033180618     
 iteration         1123 MCMCOBJ=   -6672.37787210006     
 iteration         1124 MCMCOBJ=   -6649.72509727427     
 iteration         1125 MCMCOBJ=   -6652.76684206250     
 iteration         1126 MCMCOBJ=   -6646.18189432142     
 iteration         1127 MCMCOBJ=   -6623.75041818781     
 iteration         1128 MCMCOBJ=   -6573.65197405364     
 iteration         1129 MCMCOBJ=   -6608.90893477492     
 iteration         1130 MCMCOBJ=   -6639.74574582090     
 iteration         1131 MCMCOBJ=   -6634.33497814696     
 iteration         1132 MCMCOBJ=   -6648.90704047031     
 iteration         1133 MCMCOBJ=   -6679.50256732660     
 iteration         1134 MCMCOBJ=   -6660.94437274077     
 iteration         1135 MCMCOBJ=   -6620.81858860706     
 iteration         1136 MCMCOBJ=   -6582.63655842873     
 iteration         1137 MCMCOBJ=   -6569.36400311150     
 iteration         1138 MCMCOBJ=   -6562.40768215883     
 iteration         1139 MCMCOBJ=   -6563.82749117333     
 iteration         1140 MCMCOBJ=   -6604.78997714747     
 iteration         1141 MCMCOBJ=   -6629.00177346274     
 iteration         1142 MCMCOBJ=   -6630.08576224914     
 iteration         1143 MCMCOBJ=   -6628.43922642152     
 iteration         1144 MCMCOBJ=   -6526.00399427641     
 iteration         1145 MCMCOBJ=   -6637.36713901664     
 iteration         1146 MCMCOBJ=   -6628.33718132373     
 iteration         1147 MCMCOBJ=   -6643.86323884556     
 iteration         1148 MCMCOBJ=   -6633.71211650811     
 iteration         1149 MCMCOBJ=   -6644.39468824846     
 iteration         1150 MCMCOBJ=   -6644.39468823255     
 iteration         1151 MCMCOBJ=   -6609.92983598780     
 iteration         1152 MCMCOBJ=   -6675.58191431471     
 iteration         1153 MCMCOBJ=   -6640.47789356136     
 iteration         1154 MCMCOBJ=   -6684.93033741577     
 iteration         1155 MCMCOBJ=   -6627.61841977304     
 iteration         1156 MCMCOBJ=   -6642.67447896485     
 iteration         1157 MCMCOBJ=   -6641.08645510748     
 iteration         1158 MCMCOBJ=   -6645.89909748729     
 iteration         1159 MCMCOBJ=   -6568.02685880156     
 iteration         1160 MCMCOBJ=   -6591.58524782238     
 iteration         1161 MCMCOBJ=   -6661.00548031661     
 iteration         1162 MCMCOBJ=   -6617.41378198125     
 iteration         1163 MCMCOBJ=   -6660.90202679012     
 iteration         1164 MCMCOBJ=   -6646.94569398579     
 iteration         1165 MCMCOBJ=   -6615.52550123990     
 iteration         1166 MCMCOBJ=   -6585.75987028133     
 iteration         1167 MCMCOBJ=   -6550.35716507400     
 iteration         1168 MCMCOBJ=   -6559.86095002282     
 iteration         1169 MCMCOBJ=   -6636.97949145911     
 iteration         1170 MCMCOBJ=   -6623.79355353725     
 iteration         1171 MCMCOBJ=   -6592.71996419692     
 iteration         1172 MCMCOBJ=   -6567.10912441026     
 iteration         1173 MCMCOBJ=   -6483.69545713044     
 iteration         1174 MCMCOBJ=   -6549.35366061059     
 iteration         1175 MCMCOBJ=   -6605.40902067034     
 iteration         1176 MCMCOBJ=   -6601.77888813941     
 iteration         1177 MCMCOBJ=   -6629.63322613474     
 iteration         1178 MCMCOBJ=   -6651.89269624306     
 iteration         1179 MCMCOBJ=   -6626.20050785473     
 iteration         1180 MCMCOBJ=   -6609.88412061381     
 iteration         1181 MCMCOBJ=   -6597.51525145729     
 iteration         1182 MCMCOBJ=   -6625.74781738711     
 iteration         1183 MCMCOBJ=   -6596.47398832198     
 iteration         1184 MCMCOBJ=   -6620.12867338739     
 iteration         1185 MCMCOBJ=   -6611.02978729668     
 iteration         1186 MCMCOBJ=   -6669.12067125556     
 iteration         1187 MCMCOBJ=   -6627.55060452806     
 iteration         1188 MCMCOBJ=   -6639.27191181472     
 iteration         1189 MCMCOBJ=   -6672.45829795107     
 iteration         1190 MCMCOBJ=   -6666.80253083769     
 iteration         1191 MCMCOBJ=   -6607.29947506077     
 iteration         1192 MCMCOBJ=   -6594.01461189049     
 iteration         1193 MCMCOBJ=   -6528.50362052302     
 iteration         1194 MCMCOBJ=   -6581.61442461519     
 iteration         1195 MCMCOBJ=   -6561.43725169485     
 iteration         1196 MCMCOBJ=   -6589.72217263990     
 iteration         1197 MCMCOBJ=   -6580.28779480427     
 iteration         1198 MCMCOBJ=   -6565.66826740770     
 iteration         1199 MCMCOBJ=   -6629.29483931412     
 iteration         1200 MCMCOBJ=   -6608.11595431617     
 iteration         1201 MCMCOBJ=   -6601.61761742902     
 iteration         1202 MCMCOBJ=   -6581.21404911381     
 iteration         1203 MCMCOBJ=   -6573.43692742901     
 iteration         1204 MCMCOBJ=   -6546.77110112310     
 iteration         1205 MCMCOBJ=   -6593.70987295473     
 iteration         1206 MCMCOBJ=   -6618.40675477139     
 iteration         1207 MCMCOBJ=   -6605.05982666614     
 iteration         1208 MCMCOBJ=   -6597.48856857591     
 iteration         1209 MCMCOBJ=   -6631.18923369250     
 iteration         1210 MCMCOBJ=   -6650.60448571583     
 iteration         1211 MCMCOBJ=   -6623.02604310230     
 iteration         1212 MCMCOBJ=   -6623.97470403599     
 iteration         1213 MCMCOBJ=   -6640.27734242641     
 iteration         1214 MCMCOBJ=   -6648.62398664342     
 iteration         1215 MCMCOBJ=   -6646.41992652067     
 iteration         1216 MCMCOBJ=   -6699.18633082817     
 iteration         1217 MCMCOBJ=   -6656.18291187791     
 iteration         1218 MCMCOBJ=   -6657.64573801844     
 iteration         1219 MCMCOBJ=   -6669.17369669535     
 iteration         1220 MCMCOBJ=   -6672.60871857996     
 iteration         1221 MCMCOBJ=   -6621.15431250198     
 iteration         1222 MCMCOBJ=   -6649.44810983627     
 iteration         1223 MCMCOBJ=   -6644.78568048115     
 iteration         1224 MCMCOBJ=   -6662.26230156616     
 iteration         1225 MCMCOBJ=   -6634.32725856793     
 iteration         1226 MCMCOBJ=   -6679.58705568192     
 iteration         1227 MCMCOBJ=   -6662.10556467892     
 iteration         1228 MCMCOBJ=   -6681.96216291592     
 iteration         1229 MCMCOBJ=   -6658.10156955523     
 iteration         1230 MCMCOBJ=   -6640.29529936737     
 iteration         1231 MCMCOBJ=   -6615.98719350843     
 iteration         1232 MCMCOBJ=   -6583.72928975418     
 iteration         1233 MCMCOBJ=   -6617.69968284870     
 iteration         1234 MCMCOBJ=   -6612.69951786584     
 iteration         1235 MCMCOBJ=   -6634.07871893010     
 iteration         1236 MCMCOBJ=   -6613.78886089554     
 iteration         1237 MCMCOBJ=   -6666.06591947487     
 iteration         1238 MCMCOBJ=   -6656.49766395764     
 iteration         1239 MCMCOBJ=   -6589.53431727994     
 iteration         1240 MCMCOBJ=   -6619.49430804162     
 iteration         1241 MCMCOBJ=   -6592.07175538230     
 iteration         1242 MCMCOBJ=   -6610.82982669068     
 iteration         1243 MCMCOBJ=   -6600.69437676093     
 iteration         1244 MCMCOBJ=   -6561.01724675520     
 iteration         1245 MCMCOBJ=   -6616.12022875901     
 iteration         1246 MCMCOBJ=   -6624.99137418641     
 iteration         1247 MCMCOBJ=   -6631.85759024574     
 iteration         1248 MCMCOBJ=   -6623.95315839388     
 iteration         1249 MCMCOBJ=   -6640.38686103726     
 iteration         1250 MCMCOBJ=   -6642.19226464540     
 iteration         1251 MCMCOBJ=   -6634.38733431854     
 iteration         1252 MCMCOBJ=   -6597.31081410177     
 iteration         1253 MCMCOBJ=   -6604.10702020352     
 iteration         1254 MCMCOBJ=   -6636.03730290721     
 iteration         1255 MCMCOBJ=   -6612.43818324168     
 iteration         1256 MCMCOBJ=   -6657.48726395659     
 iteration         1257 MCMCOBJ=   -6668.69780775467     
 iteration         1258 MCMCOBJ=   -6642.89133485742     
 iteration         1259 MCMCOBJ=   -6676.58810345143     
 iteration         1260 MCMCOBJ=   -6682.84593637040     
 iteration         1261 MCMCOBJ=   -6678.73901159050     
 iteration         1262 MCMCOBJ=   -6659.45681472956     
 iteration         1263 MCMCOBJ=   -6652.91540925366     
 iteration         1264 MCMCOBJ=   -6631.79657950407     
 iteration         1265 MCMCOBJ=   -6641.55272398944     
 iteration         1266 MCMCOBJ=   -6628.61595652886     
 iteration         1267 MCMCOBJ=   -6603.70390147073     
 iteration         1268 MCMCOBJ=   -6619.17827445136     
 iteration         1269 MCMCOBJ=   -6588.06162180390     
 iteration         1270 MCMCOBJ=   -6581.48794616619     
 iteration         1271 MCMCOBJ=   -6583.18679925787     
 iteration         1272 MCMCOBJ=   -6579.00186276114     
 iteration         1273 MCMCOBJ=   -6599.66800903192     
 iteration         1274 MCMCOBJ=   -6566.42952848362     
 iteration         1275 MCMCOBJ=   -6610.70165869591     
 iteration         1276 MCMCOBJ=   -6596.98874371213     
 iteration         1277 MCMCOBJ=   -6591.23537582426     
 iteration         1278 MCMCOBJ=   -6546.92763314641     
 iteration         1279 MCMCOBJ=   -6578.16337799550     
 iteration         1280 MCMCOBJ=   -6574.05278189434     
 iteration         1281 MCMCOBJ=   -6574.05278225890     
 iteration         1282 MCMCOBJ=   -6609.78138765042     
 iteration         1283 MCMCOBJ=   -6657.58193256830     
 iteration         1284 MCMCOBJ=   -6579.90956318635     
 iteration         1285 MCMCOBJ=   -6640.64859444511     
 iteration         1286 MCMCOBJ=   -6626.97277927974     
 iteration         1287 MCMCOBJ=   -6651.52114959289     
 iteration         1288 MCMCOBJ=   -6631.38217775945     
 iteration         1289 MCMCOBJ=   -6614.46042477401     
 iteration         1290 MCMCOBJ=   -6607.78037539154     
 iteration         1291 MCMCOBJ=   -6611.01076511048     
 iteration         1292 MCMCOBJ=   -6679.86045105150     
 iteration         1293 MCMCOBJ=   -6645.52056828665     
 iteration         1294 MCMCOBJ=   -6668.81785846475     
 iteration         1295 MCMCOBJ=   -6686.29375007798     
 iteration         1296 MCMCOBJ=   -6622.17968693296     
 iteration         1297 MCMCOBJ=   -6635.96880540530     
 iteration         1298 MCMCOBJ=   -6597.81428022321     
 iteration         1299 MCMCOBJ=   -6569.02426928204     
 iteration         1300 MCMCOBJ=   -6662.32411018151     
 iteration         1301 MCMCOBJ=   -6665.86376252837     
 iteration         1302 MCMCOBJ=   -6644.92195529021     
 iteration         1303 MCMCOBJ=   -6637.28285642221     
 iteration         1304 MCMCOBJ=   -6621.20952810464     
 iteration         1305 MCMCOBJ=   -6622.47926058469     
 iteration         1306 MCMCOBJ=   -6676.83499216267     
 iteration         1307 MCMCOBJ=   -6673.64975338248     
 iteration         1308 MCMCOBJ=   -6623.82364281250     
 iteration         1309 MCMCOBJ=   -6596.06653547066     
 iteration         1310 MCMCOBJ=   -6584.84105693738     
 iteration         1311 MCMCOBJ=   -6652.71768494383     
 iteration         1312 MCMCOBJ=   -6648.23991893001     
 iteration         1313 MCMCOBJ=   -6646.21297071501     
 iteration         1314 MCMCOBJ=   -6613.93266580497     
 iteration         1315 MCMCOBJ=   -6603.28531121197     
 iteration         1316 MCMCOBJ=   -6555.32855012237     
 iteration         1317 MCMCOBJ=   -6552.84233207952     
 iteration         1318 MCMCOBJ=   -6589.33387720340     
 iteration         1319 MCMCOBJ=   -6623.43473897463     
 iteration         1320 MCMCOBJ=   -6547.33839618388     
 iteration         1321 MCMCOBJ=   -6616.49743494492     
 iteration         1322 MCMCOBJ=   -6633.64584948834     
 iteration         1323 MCMCOBJ=   -6653.17942567534     
 iteration         1324 MCMCOBJ=   -6654.61985613233     
 iteration         1325 MCMCOBJ=   -6674.59685729690     
 iteration         1326 MCMCOBJ=   -6644.04663013739     
 iteration         1327 MCMCOBJ=   -6649.15698730913     
 iteration         1328 MCMCOBJ=   -6686.12432540517     
 iteration         1329 MCMCOBJ=   -6642.82792838750     
 iteration         1330 MCMCOBJ=   -6597.54357355612     
 iteration         1331 MCMCOBJ=   -6602.19663495774     
 iteration         1332 MCMCOBJ=   -6618.33896056431     
 iteration         1333 MCMCOBJ=   -6654.84221982373     
 iteration         1334 MCMCOBJ=   -6636.21080833271     
 iteration         1335 MCMCOBJ=   -6596.83786825186     
 iteration         1336 MCMCOBJ=   -6653.62063053561     
 iteration         1337 MCMCOBJ=   -6637.29409520617     
 iteration         1338 MCMCOBJ=   -6622.03486552466     
 iteration         1339 MCMCOBJ=   -6643.31920045865     
 iteration         1340 MCMCOBJ=   -6633.83795789037     
 iteration         1341 MCMCOBJ=   -6586.58745649437     
 iteration         1342 MCMCOBJ=   -6569.32756856180     
 iteration         1343 MCMCOBJ=   -6597.69046652703     
 iteration         1344 MCMCOBJ=   -6584.68622235604     
 iteration         1345 MCMCOBJ=   -6595.91758772137     
 iteration         1346 MCMCOBJ=   -6633.96048943606     
 iteration         1347 MCMCOBJ=   -6605.65719485547     
 iteration         1348 MCMCOBJ=   -6569.34655803944     
 iteration         1349 MCMCOBJ=   -6622.26750824373     
 iteration         1350 MCMCOBJ=   -6618.53749781776     
 iteration         1351 MCMCOBJ=   -6660.23946091962     
 iteration         1352 MCMCOBJ=   -6637.65023292917     
 iteration         1353 MCMCOBJ=   -6630.35804430210     
 iteration         1354 MCMCOBJ=   -6590.15527192668     
 iteration         1355 MCMCOBJ=   -6571.46339064127     
 iteration         1356 MCMCOBJ=   -6568.23718817121     
 iteration         1357 MCMCOBJ=   -6612.85266624934     
 iteration         1358 MCMCOBJ=   -6609.03125814515     
 iteration         1359 MCMCOBJ=   -6582.11056963404     
 iteration         1360 MCMCOBJ=   -6580.05031371807     
 iteration         1361 MCMCOBJ=   -6595.07301829687     
 iteration         1362 MCMCOBJ=   -6576.79902334416     
 iteration         1363 MCMCOBJ=   -6585.97090318007     
 iteration         1364 MCMCOBJ=   -6591.34371323474     
 iteration         1365 MCMCOBJ=   -6606.11004512177     
 iteration         1366 MCMCOBJ=   -6578.59492128871     
 iteration         1367 MCMCOBJ=   -6537.76608888627     
 iteration         1368 MCMCOBJ=   -6558.20488470960     
 iteration         1369 MCMCOBJ=   -6544.74852957902     
 iteration         1370 MCMCOBJ=   -6526.37514774689     
 iteration         1371 MCMCOBJ=   -6590.97278912836     
 iteration         1372 MCMCOBJ=   -6617.30363147362     
 iteration         1373 MCMCOBJ=   -6606.34381239591     
 iteration         1374 MCMCOBJ=   -6610.34398599785     
 iteration         1375 MCMCOBJ=   -6639.20473152157     
 iteration         1376 MCMCOBJ=   -6667.81878725648     
 iteration         1377 MCMCOBJ=   -6656.85203496969     
 iteration         1378 MCMCOBJ=   -6656.74470766571     
 iteration         1379 MCMCOBJ=   -6612.47701848171     
 iteration         1380 MCMCOBJ=   -6624.41938442883     
 iteration         1381 MCMCOBJ=   -6640.90189220304     
 iteration         1382 MCMCOBJ=   -6668.71990865760     
 iteration         1383 MCMCOBJ=   -6639.67657625985     
 iteration         1384 MCMCOBJ=   -6614.24864285258     
 iteration         1385 MCMCOBJ=   -6634.25897400238     
 iteration         1386 MCMCOBJ=   -6626.40213094549     
 iteration         1387 MCMCOBJ=   -6632.00788484323     
 iteration         1388 MCMCOBJ=   -6630.47416189456     
 iteration         1389 MCMCOBJ=   -6634.00489183413     
 iteration         1390 MCMCOBJ=   -6652.73085510312     
 iteration         1391 MCMCOBJ=   -6665.32556205722     
 iteration         1392 MCMCOBJ=   -6577.68509937941     
 iteration         1393 MCMCOBJ=   -6615.60322141601     
 iteration         1394 MCMCOBJ=   -6556.38267220789     
 iteration         1395 MCMCOBJ=   -6544.43915244467     
 iteration         1396 MCMCOBJ=   -6583.43076968607     
 iteration         1397 MCMCOBJ=   -6664.93049263189     
 iteration         1398 MCMCOBJ=   -6664.93049461268     
 iteration         1399 MCMCOBJ=   -6608.00288600210     
 iteration         1400 MCMCOBJ=   -6584.56021742251     
 iteration         1401 MCMCOBJ=   -6615.19998113878     
 iteration         1402 MCMCOBJ=   -6637.38348110226     
 iteration         1403 MCMCOBJ=   -6646.90741242553     
 iteration         1404 MCMCOBJ=   -6617.11979555427     
 iteration         1405 MCMCOBJ=   -6615.82335673771     
 iteration         1406 MCMCOBJ=   -6612.68552148651     
 iteration         1407 MCMCOBJ=   -6565.34528183500     
 iteration         1408 MCMCOBJ=   -6600.18194483940     
 iteration         1409 MCMCOBJ=   -6642.64374444310     
 iteration         1410 MCMCOBJ=   -6647.87525654815     
 iteration         1411 MCMCOBJ=   -6628.96446155421     
 iteration         1412 MCMCOBJ=   -6600.07116110976     
 iteration         1413 MCMCOBJ=   -6602.17612360581     
 iteration         1414 MCMCOBJ=   -6602.21900494149     
 iteration         1415 MCMCOBJ=   -6619.84246045706     
 iteration         1416 MCMCOBJ=   -6623.27643304298     
 iteration         1417 MCMCOBJ=   -6652.51990055689     
 iteration         1418 MCMCOBJ=   -6607.22373540342     
 iteration         1419 MCMCOBJ=   -6611.54943262323     
 iteration         1420 MCMCOBJ=   -6624.92271990074     
 iteration         1421 MCMCOBJ=   -6650.06172348272     
 iteration         1422 MCMCOBJ=   -6644.69952323076     
 iteration         1423 MCMCOBJ=   -6688.21683935671     
 iteration         1424 MCMCOBJ=   -6691.86609271473     
 iteration         1425 MCMCOBJ=   -6722.81885133198     
 iteration         1426 MCMCOBJ=   -6649.74768082078     
 iteration         1427 MCMCOBJ=   -6638.70780389259     
 iteration         1428 MCMCOBJ=   -6628.03555318164     
 iteration         1429 MCMCOBJ=   -6602.30390210906     
 iteration         1430 MCMCOBJ=   -6612.21552062249     
 iteration         1431 MCMCOBJ=   -6623.95762656049     
 iteration         1432 MCMCOBJ=   -6683.59595274597     
 iteration         1433 MCMCOBJ=   -6671.70669225153     
 iteration         1434 MCMCOBJ=   -6610.01819357849     
 iteration         1435 MCMCOBJ=   -6642.38632243394     
 iteration         1436 MCMCOBJ=   -6661.31518201948     
 iteration         1437 MCMCOBJ=   -6670.36855372219     
 iteration         1438 MCMCOBJ=   -6625.17968381413     
 iteration         1439 MCMCOBJ=   -6630.91548621411     
 iteration         1440 MCMCOBJ=   -6639.01842564320     
 iteration         1441 MCMCOBJ=   -6608.09962092958     
 iteration         1442 MCMCOBJ=   -6637.46820829838     
 iteration         1443 MCMCOBJ=   -6665.57538819133     
 iteration         1444 MCMCOBJ=   -6662.40078781810     
 iteration         1445 MCMCOBJ=   -6632.25041648264     
 iteration         1446 MCMCOBJ=   -6626.47577425564     
 iteration         1447 MCMCOBJ=   -6629.31834094371     
 iteration         1448 MCMCOBJ=   -6652.04556392281     
 iteration         1449 MCMCOBJ=   -6673.45286915090     
 iteration         1450 MCMCOBJ=   -6692.28609419318     
 iteration         1451 MCMCOBJ=   -6647.96988478446     
 iteration         1452 MCMCOBJ=   -6638.38138750732     
 iteration         1453 MCMCOBJ=   -6611.27610674006     
 iteration         1454 MCMCOBJ=   -6630.15759456748     
 iteration         1455 MCMCOBJ=   -6630.15782952842     
 iteration         1456 MCMCOBJ=   -6648.08054596045     
 iteration         1457 MCMCOBJ=   -6654.93075040322     
 iteration         1458 MCMCOBJ=   -6627.31219083772     
 iteration         1459 MCMCOBJ=   -6656.31795322219     
 iteration         1460 MCMCOBJ=   -6712.83744379347     
 iteration         1461 MCMCOBJ=   -6664.62301927912     
 iteration         1462 MCMCOBJ=   -6704.67753110699     
 iteration         1463 MCMCOBJ=   -6628.69299161870     
 iteration         1464 MCMCOBJ=   -6585.17986859843     
 iteration         1465 MCMCOBJ=   -6591.51626052992     
 iteration         1466 MCMCOBJ=   -6605.15574095467     
 iteration         1467 MCMCOBJ=   -6564.69248991760     
 iteration         1468 MCMCOBJ=   -6586.91381620577     
 iteration         1469 MCMCOBJ=   -6678.40922318317     
 iteration         1470 MCMCOBJ=   -6658.47846871436     
 iteration         1471 MCMCOBJ=   -6654.99022713242     
 iteration         1472 MCMCOBJ=   -6630.96685042772     
 iteration         1473 MCMCOBJ=   -6629.53707088573     
 iteration         1474 MCMCOBJ=   -6586.94859320742     
 iteration         1475 MCMCOBJ=   -6595.54257299589     
 iteration         1476 MCMCOBJ=   -6678.21400670900     
 iteration         1477 MCMCOBJ=   -6669.22272066232     
 iteration         1478 MCMCOBJ=   -6628.68797667939     
 iteration         1479 MCMCOBJ=   -6632.04166787207     
 iteration         1480 MCMCOBJ=   -6635.92614299702     
 iteration         1481 MCMCOBJ=   -6634.13455458157     
 iteration         1482 MCMCOBJ=   -6675.71564355612     
 iteration         1483 MCMCOBJ=   -6631.40075524418     
 iteration         1484 MCMCOBJ=   -6624.38514092739     
 iteration         1485 MCMCOBJ=   -6637.03849247096     
 iteration         1486 MCMCOBJ=   -6647.30157118553     
 iteration         1487 MCMCOBJ=   -6659.29689230349     
 iteration         1488 MCMCOBJ=   -6577.79310499777     
 iteration         1489 MCMCOBJ=   -6614.69722258048     
 iteration         1490 MCMCOBJ=   -6633.99957719639     
 iteration         1491 MCMCOBJ=   -6633.22651343468     
 iteration         1492 MCMCOBJ=   -6655.59496982956     
 iteration         1493 MCMCOBJ=   -6672.69273673112     
 iteration         1494 MCMCOBJ=   -6680.50862011117     
 iteration         1495 MCMCOBJ=   -6649.03433028063     
 iteration         1496 MCMCOBJ=   -6601.22590909697     
 iteration         1497 MCMCOBJ=   -6562.79881718188     
 iteration         1498 MCMCOBJ=   -6539.57619046438     
 iteration         1499 MCMCOBJ=   -6605.37024175524     
 iteration         1500 MCMCOBJ=   -6592.49229280411     
 iteration         1501 MCMCOBJ=   -6634.89951507632     
 iteration         1502 MCMCOBJ=   -6677.63707724280     
 iteration         1503 MCMCOBJ=   -6663.65523910177     
 iteration         1504 MCMCOBJ=   -6654.39237351429     
 iteration         1505 MCMCOBJ=   -6653.07952170551     
 iteration         1506 MCMCOBJ=   -6645.48209898543     
 iteration         1507 MCMCOBJ=   -6651.19205524767     
 iteration         1508 MCMCOBJ=   -6594.30112070969     
 iteration         1509 MCMCOBJ=   -6590.30742247723     
 iteration         1510 MCMCOBJ=   -6632.09321841270     
 iteration         1511 MCMCOBJ=   -6594.50082635216     
 iteration         1512 MCMCOBJ=   -6610.71847329231     
 iteration         1513 MCMCOBJ=   -6600.11393822991     
 iteration         1514 MCMCOBJ=   -6532.89508127129     
 iteration         1515 MCMCOBJ=   -6588.25900408496     
 iteration         1516 MCMCOBJ=   -6575.41728444255     
 iteration         1517 MCMCOBJ=   -6616.73146430812     
 iteration         1518 MCMCOBJ=   -6608.60392910628     
 iteration         1519 MCMCOBJ=   -6578.95653811236     
 iteration         1520 MCMCOBJ=   -6656.48416294619     
 iteration         1521 MCMCOBJ=   -6626.63828186100     
 iteration         1522 MCMCOBJ=   -6641.79396742161     
 iteration         1523 MCMCOBJ=   -6594.38796597105     
 iteration         1524 MCMCOBJ=   -6650.59539911957     
 iteration         1525 MCMCOBJ=   -6656.14324751299     
 iteration         1526 MCMCOBJ=   -6662.06993835911     
 iteration         1527 MCMCOBJ=   -6648.62161291297     
 iteration         1528 MCMCOBJ=   -6654.94203244410     
 iteration         1529 MCMCOBJ=   -6644.29356299115     
 iteration         1530 MCMCOBJ=   -6648.70204868164     
 iteration         1531 MCMCOBJ=   -6649.17389570937     
 iteration         1532 MCMCOBJ=   -6636.92287690191     
 iteration         1533 MCMCOBJ=   -6678.85686204224     
 iteration         1534 MCMCOBJ=   -6608.64273646142     
 iteration         1535 MCMCOBJ=   -6649.30654902007     
 iteration         1536 MCMCOBJ=   -6710.75753832186     
 iteration         1537 MCMCOBJ=   -6718.74314874995     
 iteration         1538 MCMCOBJ=   -6664.33064451316     
 iteration         1539 MCMCOBJ=   -6656.56267916525     
 iteration         1540 MCMCOBJ=   -6613.66538454590     
 iteration         1541 MCMCOBJ=   -6647.38884958543     
 iteration         1542 MCMCOBJ=   -6643.73504397384     
 iteration         1543 MCMCOBJ=   -6646.28196372696     
 iteration         1544 MCMCOBJ=   -6625.37085418813     
 iteration         1545 MCMCOBJ=   -6624.53664397700     
 iteration         1546 MCMCOBJ=   -6621.56378391376     
 iteration         1547 MCMCOBJ=   -6628.38620185070     
 iteration         1548 MCMCOBJ=   -6655.58738871515     
 iteration         1549 MCMCOBJ=   -6642.56485626243     
 iteration         1550 MCMCOBJ=   -6621.07999807346     
 iteration         1551 MCMCOBJ=   -6685.96720702973     
 iteration         1552 MCMCOBJ=   -6658.51793889406     
 iteration         1553 MCMCOBJ=   -6655.76167977079     
 iteration         1554 MCMCOBJ=   -6615.12538775694     
 iteration         1555 MCMCOBJ=   -6590.18286816862     
 iteration         1556 MCMCOBJ=   -6587.94773579967     
 iteration         1557 MCMCOBJ=   -6593.36906807029     
 iteration         1558 MCMCOBJ=   -6624.59965303488     
 iteration         1559 MCMCOBJ=   -6650.95170501132     
 iteration         1560 MCMCOBJ=   -6622.16693671555     
 iteration         1561 MCMCOBJ=   -6623.26351040949     
 iteration         1562 MCMCOBJ=   -6646.65763263984     
 iteration         1563 MCMCOBJ=   -6674.35066574167     
 iteration         1564 MCMCOBJ=   -6652.49152147270     
 iteration         1565 MCMCOBJ=   -6635.61781066674     
 iteration         1566 MCMCOBJ=   -6655.05228752059     
 iteration         1567 MCMCOBJ=   -6675.37632150115     
 iteration         1568 MCMCOBJ=   -6653.27571581442     
 iteration         1569 MCMCOBJ=   -6629.76990656261     
 iteration         1570 MCMCOBJ=   -6663.91332883818     
 iteration         1571 MCMCOBJ=   -6644.87955888544     
 iteration         1572 MCMCOBJ=   -6674.13667867100     
 iteration         1573 MCMCOBJ=   -6667.16079170780     
 iteration         1574 MCMCOBJ=   -6639.90913135511     
 iteration         1575 MCMCOBJ=   -6673.26017936631     
 iteration         1576 MCMCOBJ=   -6636.44445389667     
 iteration         1577 MCMCOBJ=   -6605.57305933922     
 iteration         1578 MCMCOBJ=   -6603.36484773597     
 iteration         1579 MCMCOBJ=   -6587.61662344085     
 iteration         1580 MCMCOBJ=   -6609.64537910902     
 iteration         1581 MCMCOBJ=   -6578.19860183177     
 iteration         1582 MCMCOBJ=   -6595.85895067377     
 iteration         1583 MCMCOBJ=   -6589.00047565588     
 iteration         1584 MCMCOBJ=   -6620.76349328042     
 iteration         1585 MCMCOBJ=   -6589.83663465611     
 iteration         1586 MCMCOBJ=   -6597.20702682951     
 iteration         1587 MCMCOBJ=   -6565.51384661361     
 iteration         1588 MCMCOBJ=   -6603.08325324763     
 iteration         1589 MCMCOBJ=   -6602.25373639986     
 iteration         1590 MCMCOBJ=   -6609.36807582567     
 iteration         1591 MCMCOBJ=   -6634.39750954554     
 iteration         1592 MCMCOBJ=   -6654.31153885923     
 iteration         1593 MCMCOBJ=   -6673.38112956600     
 iteration         1594 MCMCOBJ=   -6656.15026932786     
 iteration         1595 MCMCOBJ=   -6663.17990980228     
 iteration         1596 MCMCOBJ=   -6572.27508233298     
 iteration         1597 MCMCOBJ=   -6561.17101576694     
 iteration         1598 MCMCOBJ=   -6582.67000541391     
 iteration         1599 MCMCOBJ=   -6601.17370197848     
 iteration         1600 MCMCOBJ=   -6553.34939366823     
 iteration         1601 MCMCOBJ=   -6612.72262058367     
 iteration         1602 MCMCOBJ=   -6540.15580241093     
 iteration         1603 MCMCOBJ=   -6594.60480326922     
 iteration         1604 MCMCOBJ=   -6598.88728601312     
 iteration         1605 MCMCOBJ=   -6615.69482639954     
 iteration         1606 MCMCOBJ=   -6621.90419676968     
 iteration         1607 MCMCOBJ=   -6613.52945859463     
 iteration         1608 MCMCOBJ=   -6604.13388932080     
 iteration         1609 MCMCOBJ=   -6636.15721593882     
 iteration         1610 MCMCOBJ=   -6639.79321829223     
 iteration         1611 MCMCOBJ=   -6660.71831142247     
 iteration         1612 MCMCOBJ=   -6594.12827255848     
 iteration         1613 MCMCOBJ=   -6600.43958109117     
 iteration         1614 MCMCOBJ=   -6639.15986630809     
 iteration         1615 MCMCOBJ=   -6626.13636198662     
 iteration         1616 MCMCOBJ=   -6597.83517052819     
 iteration         1617 MCMCOBJ=   -6547.76431520890     
 iteration         1618 MCMCOBJ=   -6633.28603142404     
 iteration         1619 MCMCOBJ=   -6640.75159095341     
 iteration         1620 MCMCOBJ=   -6634.18037714577     
 iteration         1621 MCMCOBJ=   -6578.28674576459     
 iteration         1622 MCMCOBJ=   -6614.38682591142     
 iteration         1623 MCMCOBJ=   -6601.96725857437     
 iteration         1624 MCMCOBJ=   -6627.27609517358     
 iteration         1625 MCMCOBJ=   -6624.14579199278     
 iteration         1626 MCMCOBJ=   -6617.07229145602     
 iteration         1627 MCMCOBJ=   -6652.71579677322     
 iteration         1628 MCMCOBJ=   -6700.05390292263     
 iteration         1629 MCMCOBJ=   -6706.35882223343     
 iteration         1630 MCMCOBJ=   -6676.75883657366     
 iteration         1631 MCMCOBJ=   -6678.71310958598     
 iteration         1632 MCMCOBJ=   -6655.73224719198     
 iteration         1633 MCMCOBJ=   -6606.74931413043     
 iteration         1634 MCMCOBJ=   -6526.79010718866     
 iteration         1635 MCMCOBJ=   -6633.72839280251     
 iteration         1636 MCMCOBJ=   -6638.03554924197     
 iteration         1637 MCMCOBJ=   -6567.06130642165     
 iteration         1638 MCMCOBJ=   -6556.35166770780     
 iteration         1639 MCMCOBJ=   -6557.61441859513     
 iteration         1640 MCMCOBJ=   -6594.82096447486     
 iteration         1641 MCMCOBJ=   -6596.42929962391     
 iteration         1642 MCMCOBJ=   -6565.19880247730     
 iteration         1643 MCMCOBJ=   -6571.55670292229     
 iteration         1644 MCMCOBJ=   -6545.74152229015     
 iteration         1645 MCMCOBJ=   -6619.82127420105     
 iteration         1646 MCMCOBJ=   -6620.52318195269     
 iteration         1647 MCMCOBJ=   -6609.71751022009     
 iteration         1648 MCMCOBJ=   -6647.27596580830     
 iteration         1649 MCMCOBJ=   -6613.23378364257     
 iteration         1650 MCMCOBJ=   -6580.77198077709     
 iteration         1651 MCMCOBJ=   -6616.21141285487     
 iteration         1652 MCMCOBJ=   -6611.45872541664     
 iteration         1653 MCMCOBJ=   -6585.62369445012     
 iteration         1654 MCMCOBJ=   -6556.84548453822     
 iteration         1655 MCMCOBJ=   -6550.18367507078     
 iteration         1656 MCMCOBJ=   -6595.67278805707     
 iteration         1657 MCMCOBJ=   -6568.21269156520     
 iteration         1658 MCMCOBJ=   -6665.96979015958     
 iteration         1659 MCMCOBJ=   -6664.56163380910     
 iteration         1660 MCMCOBJ=   -6649.10473502037     
 iteration         1661 MCMCOBJ=   -6597.18824912555     
 iteration         1662 MCMCOBJ=   -6619.92264595569     
 iteration         1663 MCMCOBJ=   -6625.29941440547     
 iteration         1664 MCMCOBJ=   -6614.03794823856     
 iteration         1665 MCMCOBJ=   -6666.18747641968     
 iteration         1666 MCMCOBJ=   -6667.48716648453     
 iteration         1667 MCMCOBJ=   -6601.99441264981     
 iteration         1668 MCMCOBJ=   -6589.60373331671     
 iteration         1669 MCMCOBJ=   -6572.93352663698     
 iteration         1670 MCMCOBJ=   -6559.89618057751     
 iteration         1671 MCMCOBJ=   -6565.02626950984     
 iteration         1672 MCMCOBJ=   -6578.09067976032     
 iteration         1673 MCMCOBJ=   -6568.19728543600     
 iteration         1674 MCMCOBJ=   -6591.40663450636     
 iteration         1675 MCMCOBJ=   -6561.22329280200     
 iteration         1676 MCMCOBJ=   -6615.86796013839     
 iteration         1677 MCMCOBJ=   -6614.76141340995     
 iteration         1678 MCMCOBJ=   -6613.63997969856     
 iteration         1679 MCMCOBJ=   -6586.16631070221     
 iteration         1680 MCMCOBJ=   -6626.18453833332     
 iteration         1681 MCMCOBJ=   -6601.93724300236     
 iteration         1682 MCMCOBJ=   -6591.18412617245     
 iteration         1683 MCMCOBJ=   -6581.04811351992     
 iteration         1684 MCMCOBJ=   -6600.66952460783     
 iteration         1685 MCMCOBJ=   -6614.92121322696     
 iteration         1686 MCMCOBJ=   -6599.13334077786     
 iteration         1687 MCMCOBJ=   -6613.89142250900     
 iteration         1688 MCMCOBJ=   -6566.73395995125     
 iteration         1689 MCMCOBJ=   -6613.49155034715     
 iteration         1690 MCMCOBJ=   -6583.70083744483     
 iteration         1691 MCMCOBJ=   -6610.65807993391     
 iteration         1692 MCMCOBJ=   -6576.72802333655     
 iteration         1693 MCMCOBJ=   -6574.42916783237     
 iteration         1694 MCMCOBJ=   -6576.86727865854     
 iteration         1695 MCMCOBJ=   -6628.20245617646     
 iteration         1696 MCMCOBJ=   -6634.19595739731     
 iteration         1697 MCMCOBJ=   -6632.78971777934     
 iteration         1698 MCMCOBJ=   -6588.46468675198     
 iteration         1699 MCMCOBJ=   -6649.90608666719     
 iteration         1700 MCMCOBJ=   -6655.00027819856     
 iteration         1701 MCMCOBJ=   -6622.16187439627     
 iteration         1702 MCMCOBJ=   -6596.32288497151     
 iteration         1703 MCMCOBJ=   -6624.29262260058     
 iteration         1704 MCMCOBJ=   -6558.75715197994     
 iteration         1705 MCMCOBJ=   -6586.80191265399     
 iteration         1706 MCMCOBJ=   -6649.15269335841     
 iteration         1707 MCMCOBJ=   -6696.61255524725     
 iteration         1708 MCMCOBJ=   -6645.63315060045     
 iteration         1709 MCMCOBJ=   -6660.30718688204     
 iteration         1710 MCMCOBJ=   -6659.53544163553     
 iteration         1711 MCMCOBJ=   -6625.37535106917     
 iteration         1712 MCMCOBJ=   -6684.68554474406     
 iteration         1713 MCMCOBJ=   -6684.68554220558     
 iteration         1714 MCMCOBJ=   -6643.31413221836     
 iteration         1715 MCMCOBJ=   -6657.96544057783     
 iteration         1716 MCMCOBJ=   -6654.55893139402     
 iteration         1717 MCMCOBJ=   -6651.95304229098     
 iteration         1718 MCMCOBJ=   -6660.14917316881     
 iteration         1719 MCMCOBJ=   -6687.61654152790     
 iteration         1720 MCMCOBJ=   -6623.83060306945     
 iteration         1721 MCMCOBJ=   -6567.34591188448     
 iteration         1722 MCMCOBJ=   -6600.74297049702     
 iteration         1723 MCMCOBJ=   -6600.06386191722     
 iteration         1724 MCMCOBJ=   -6615.47917174804     
 iteration         1725 MCMCOBJ=   -6604.81341590301     
 iteration         1726 MCMCOBJ=   -6635.10112274113     
 iteration         1727 MCMCOBJ=   -6670.20731105834     
 iteration         1728 MCMCOBJ=   -6593.75273259147     
 iteration         1729 MCMCOBJ=   -6681.98532236271     
 iteration         1730 MCMCOBJ=   -6632.36820157640     
 iteration         1731 MCMCOBJ=   -6609.22117448735     
 iteration         1732 MCMCOBJ=   -6623.32549479581     
 iteration         1733 MCMCOBJ=   -6613.87755447751     
 iteration         1734 MCMCOBJ=   -6641.55085889671     
 iteration         1735 MCMCOBJ=   -6638.78489071941     
 iteration         1736 MCMCOBJ=   -6611.78484387883     
 iteration         1737 MCMCOBJ=   -6630.35189741196     
 iteration         1738 MCMCOBJ=   -6611.95975387316     
 iteration         1739 MCMCOBJ=   -6616.26652230589     
 iteration         1740 MCMCOBJ=   -6614.45271498243     
 iteration         1741 MCMCOBJ=   -6587.21693610921     
 iteration         1742 MCMCOBJ=   -6562.69272654075     
 iteration         1743 MCMCOBJ=   -6580.51522707931     
 iteration         1744 MCMCOBJ=   -6598.86269922014     
 iteration         1745 MCMCOBJ=   -6640.18513329099     
 iteration         1746 MCMCOBJ=   -6637.11874497908     
 iteration         1747 MCMCOBJ=   -6580.87621344822     
 iteration         1748 MCMCOBJ=   -6543.04479001626     
 iteration         1749 MCMCOBJ=   -6573.30679690891     
 iteration         1750 MCMCOBJ=   -6550.41355720352     
 iteration         1751 MCMCOBJ=   -6571.49441966842     
 iteration         1752 MCMCOBJ=   -6587.99398957778     
 iteration         1753 MCMCOBJ=   -6547.91023372737     
 iteration         1754 MCMCOBJ=   -6614.62219036703     
 iteration         1755 MCMCOBJ=   -6622.89379055984     
 iteration         1756 MCMCOBJ=   -6639.84197911093     
 iteration         1757 MCMCOBJ=   -6610.65184037140     
 iteration         1758 MCMCOBJ=   -6611.14520851623     
 iteration         1759 MCMCOBJ=   -6621.04690730321     
 iteration         1760 MCMCOBJ=   -6592.30052031694     
 iteration         1761 MCMCOBJ=   -6613.59286684259     
 iteration         1762 MCMCOBJ=   -6637.83447553757     
 iteration         1763 MCMCOBJ=   -6661.80316149383     
 iteration         1764 MCMCOBJ=   -6662.64873653698     
 iteration         1765 MCMCOBJ=   -6680.20043240212     
 iteration         1766 MCMCOBJ=   -6646.85940086717     
 iteration         1767 MCMCOBJ=   -6649.53754879035     
 iteration         1768 MCMCOBJ=   -6649.52169001864     
 iteration         1769 MCMCOBJ=   -6582.97267240743     
 iteration         1770 MCMCOBJ=   -6584.37152392636     
 iteration         1771 MCMCOBJ=   -6625.87389667348     
 iteration         1772 MCMCOBJ=   -6612.59726340868     
 iteration         1773 MCMCOBJ=   -6671.77662848578     
 iteration         1774 MCMCOBJ=   -6628.22449451331     
 iteration         1775 MCMCOBJ=   -6622.29864373532     
 iteration         1776 MCMCOBJ=   -6621.46846768520     
 iteration         1777 MCMCOBJ=   -6600.98398701136     
 iteration         1778 MCMCOBJ=   -6579.96346877928     
 iteration         1779 MCMCOBJ=   -6599.01144456011     
 iteration         1780 MCMCOBJ=   -6606.57910420362     
 iteration         1781 MCMCOBJ=   -6600.40210673250     
 iteration         1782 MCMCOBJ=   -6659.99505795041     
 iteration         1783 MCMCOBJ=   -6658.75083904920     
 iteration         1784 MCMCOBJ=   -6607.65859193909     
 iteration         1785 MCMCOBJ=   -6604.75881399307     
 iteration         1786 MCMCOBJ=   -6636.84671166345     
 iteration         1787 MCMCOBJ=   -6653.71568643754     
 iteration         1788 MCMCOBJ=   -6624.07208268105     
 iteration         1789 MCMCOBJ=   -6617.36788768752     
 iteration         1790 MCMCOBJ=   -6556.72723624133     
 iteration         1791 MCMCOBJ=   -6593.11858670219     
 iteration         1792 MCMCOBJ=   -6612.03468169002     
 iteration         1793 MCMCOBJ=   -6596.07676389136     
 iteration         1794 MCMCOBJ=   -6594.97979034988     
 iteration         1795 MCMCOBJ=   -6634.00741177512     
 iteration         1796 MCMCOBJ=   -6636.11185917557     
 iteration         1797 MCMCOBJ=   -6597.75826768909     
 iteration         1798 MCMCOBJ=   -6636.81916959071     
 iteration         1799 MCMCOBJ=   -6618.43436528620     
 iteration         1800 MCMCOBJ=   -6625.25951105551     
 iteration         1801 MCMCOBJ=   -6622.51099046396     
 iteration         1802 MCMCOBJ=   -6624.64396278539     
 iteration         1803 MCMCOBJ=   -6624.64396243104     
 iteration         1804 MCMCOBJ=   -6698.88572689102     
 iteration         1805 MCMCOBJ=   -6653.60160985562     
 iteration         1806 MCMCOBJ=   -6611.57824936402     
 iteration         1807 MCMCOBJ=   -6625.09890190685     
 iteration         1808 MCMCOBJ=   -6577.39468898947     
 iteration         1809 MCMCOBJ=   -6611.14362145627     
 iteration         1810 MCMCOBJ=   -6582.63365575906     
 iteration         1811 MCMCOBJ=   -6653.87115797637     
 iteration         1812 MCMCOBJ=   -6676.57353480548     
 iteration         1813 MCMCOBJ=   -6662.91585335600     
 iteration         1814 MCMCOBJ=   -6616.96085267676     
 iteration         1815 MCMCOBJ=   -6618.42673745143     
 iteration         1816 MCMCOBJ=   -6611.04294694380     
 iteration         1817 MCMCOBJ=   -6651.42901743979     
 iteration         1818 MCMCOBJ=   -6621.80719628290     
 iteration         1819 MCMCOBJ=   -6628.91040830514     
 iteration         1820 MCMCOBJ=   -6628.91040798489     
 iteration         1821 MCMCOBJ=   -6610.80723711661     
 iteration         1822 MCMCOBJ=   -6664.02155083969     
 iteration         1823 MCMCOBJ=   -6675.74815466274     
 iteration         1824 MCMCOBJ=   -6659.30389028734     
 iteration         1825 MCMCOBJ=   -6658.37035868669     
 iteration         1826 MCMCOBJ=   -6657.49051113172     
 iteration         1827 MCMCOBJ=   -6651.09779303612     
 iteration         1828 MCMCOBJ=   -6696.98413328998     
 iteration         1829 MCMCOBJ=   -6659.68124685553     
 iteration         1830 MCMCOBJ=   -6622.44483627055     
 iteration         1831 MCMCOBJ=   -6596.53502437439     
 iteration         1832 MCMCOBJ=   -6560.39442751398     
 iteration         1833 MCMCOBJ=   -6570.00396071837     
 iteration         1834 MCMCOBJ=   -6562.77917483444     
 iteration         1835 MCMCOBJ=   -6610.93024173979     
 iteration         1836 MCMCOBJ=   -6600.51683527019     
 iteration         1837 MCMCOBJ=   -6618.87274296636     
 iteration         1838 MCMCOBJ=   -6582.69148176706     
 iteration         1839 MCMCOBJ=   -6608.04331164422     
 iteration         1840 MCMCOBJ=   -6634.21755159422     
 iteration         1841 MCMCOBJ=   -6634.99263294636     
 iteration         1842 MCMCOBJ=   -6618.79321152881     
 iteration         1843 MCMCOBJ=   -6616.56105240253     
 iteration         1844 MCMCOBJ=   -6592.15455978839     
 iteration         1845 MCMCOBJ=   -6529.19632952383     
 iteration         1846 MCMCOBJ=   -6583.06161703203     
 iteration         1847 MCMCOBJ=   -6544.07886286003     
 iteration         1848 MCMCOBJ=   -6508.94698811742     
 iteration         1849 MCMCOBJ=   -6519.17181789754     
 iteration         1850 MCMCOBJ=   -6564.03464645995     
 iteration         1851 MCMCOBJ=   -6606.44952486463     
 iteration         1852 MCMCOBJ=   -6677.38734948182     
 iteration         1853 MCMCOBJ=   -6633.85466505138     
 iteration         1854 MCMCOBJ=   -6630.77463593624     
 iteration         1855 MCMCOBJ=   -6606.19325499022     
 iteration         1856 MCMCOBJ=   -6551.98024824264     
 iteration         1857 MCMCOBJ=   -6580.21776095889     
 iteration         1858 MCMCOBJ=   -6599.84817783763     
 iteration         1859 MCMCOBJ=   -6599.14919097817     
 iteration         1860 MCMCOBJ=   -6646.53349828432     
 iteration         1861 MCMCOBJ=   -6622.66025831857     
 iteration         1862 MCMCOBJ=   -6637.39397001922     
 iteration         1863 MCMCOBJ=   -6620.24848178286     
 iteration         1864 MCMCOBJ=   -6646.71922660666     
 iteration         1865 MCMCOBJ=   -6643.21563397963     
 iteration         1866 MCMCOBJ=   -6629.71279741522     
 iteration         1867 MCMCOBJ=   -6632.84981054864     
 iteration         1868 MCMCOBJ=   -6659.86909495741     
 iteration         1869 MCMCOBJ=   -6629.40240753090     
 iteration         1870 MCMCOBJ=   -6631.99790200847     
 iteration         1871 MCMCOBJ=   -6642.02256197667     
 iteration         1872 MCMCOBJ=   -6626.64349832767     
 iteration         1873 MCMCOBJ=   -6587.49652950159     
 iteration         1874 MCMCOBJ=   -6599.96490495275     
 iteration         1875 MCMCOBJ=   -6585.76319648493     
 iteration         1876 MCMCOBJ=   -6593.61861432719     
 iteration         1877 MCMCOBJ=   -6593.61861522974     
 iteration         1878 MCMCOBJ=   -6634.12131167095     
 iteration         1879 MCMCOBJ=   -6612.62557632862     
 iteration         1880 MCMCOBJ=   -6618.58016633633     
 iteration         1881 MCMCOBJ=   -6626.64494676641     
 iteration         1882 MCMCOBJ=   -6650.95296862456     
 iteration         1883 MCMCOBJ=   -6662.12624491847     
 iteration         1884 MCMCOBJ=   -6644.61211643199     
 iteration         1885 MCMCOBJ=   -6649.48179281595     
 iteration         1886 MCMCOBJ=   -6582.63966964636     
 iteration         1887 MCMCOBJ=   -6594.76607376497     
 iteration         1888 MCMCOBJ=   -6632.77399896320     
 iteration         1889 MCMCOBJ=   -6609.61835521889     
 iteration         1890 MCMCOBJ=   -6638.18535311804     
 iteration         1891 MCMCOBJ=   -6636.71831264467     
 iteration         1892 MCMCOBJ=   -6646.63404073603     
 iteration         1893 MCMCOBJ=   -6640.48091154649     
 iteration         1894 MCMCOBJ=   -6625.50253744416     
 iteration         1895 MCMCOBJ=   -6620.25054190551     
 iteration         1896 MCMCOBJ=   -6641.92524629996     
 iteration         1897 MCMCOBJ=   -6680.82846645944     
 iteration         1898 MCMCOBJ=   -6639.13413804855     
 iteration         1899 MCMCOBJ=   -6672.68259058132     
 iteration         1900 MCMCOBJ=   -6671.47684809666     
 iteration         1901 MCMCOBJ=   -6673.84281553779     
 iteration         1902 MCMCOBJ=   -6609.80787575504     
 iteration         1903 MCMCOBJ=   -6629.13156623900     
 iteration         1904 MCMCOBJ=   -6670.46505667340     
 iteration         1905 MCMCOBJ=   -6702.25389103530     
 iteration         1906 MCMCOBJ=   -6695.22414442088     
 iteration         1907 MCMCOBJ=   -6704.05954126498     
 iteration         1908 MCMCOBJ=   -6635.98777572105     
 iteration         1909 MCMCOBJ=   -6662.40298667406     
 iteration         1910 MCMCOBJ=   -6666.54758990361     
 iteration         1911 MCMCOBJ=   -6680.20597925482     
 iteration         1912 MCMCOBJ=   -6715.09661624587     
 iteration         1913 MCMCOBJ=   -6692.05329450009     
 iteration         1914 MCMCOBJ=   -6669.29431476330     
 iteration         1915 MCMCOBJ=   -6667.23164075536     
 iteration         1916 MCMCOBJ=   -6666.89257084207     
 iteration         1917 MCMCOBJ=   -6670.19491163879     
 iteration         1918 MCMCOBJ=   -6652.64359337991     
 iteration         1919 MCMCOBJ=   -6630.12672328141     
 iteration         1920 MCMCOBJ=   -6610.85248666784     
 iteration         1921 MCMCOBJ=   -6624.49921970071     
 iteration         1922 MCMCOBJ=   -6657.16789782489     
 iteration         1923 MCMCOBJ=   -6735.07574304409     
 iteration         1924 MCMCOBJ=   -6701.88203741355     
 iteration         1925 MCMCOBJ=   -6640.26464479605     
 iteration         1926 MCMCOBJ=   -6628.13649126112     
 iteration         1927 MCMCOBJ=   -6627.72366585609     
 iteration         1928 MCMCOBJ=   -6615.03630283850     
 iteration         1929 MCMCOBJ=   -6624.69942233708     
 iteration         1930 MCMCOBJ=   -6618.64208329296     
 iteration         1931 MCMCOBJ=   -6606.82952866706     
 iteration         1932 MCMCOBJ=   -6612.48004110669     
 iteration         1933 MCMCOBJ=   -6581.79111534179     
 iteration         1934 MCMCOBJ=   -6589.53942849061     
 iteration         1935 MCMCOBJ=   -6612.10754787500     
 iteration         1936 MCMCOBJ=   -6631.69998709117     
 iteration         1937 MCMCOBJ=   -6638.98031501065     
 iteration         1938 MCMCOBJ=   -6628.17311050913     
 iteration         1939 MCMCOBJ=   -6675.09894502888     
 iteration         1940 MCMCOBJ=   -6668.86846206482     
 iteration         1941 MCMCOBJ=   -6662.18619829595     
 iteration         1942 MCMCOBJ=   -6628.48156674002     
 iteration         1943 MCMCOBJ=   -6632.22038195370     
 iteration         1944 MCMCOBJ=   -6631.10612651823     
 iteration         1945 MCMCOBJ=   -6696.74501855999     
 iteration         1946 MCMCOBJ=   -6696.74501901539     
 iteration         1947 MCMCOBJ=   -6676.67727190222     
 iteration         1948 MCMCOBJ=   -6711.30470046594     
 iteration         1949 MCMCOBJ=   -6657.58331845102     
 iteration         1950 MCMCOBJ=   -6589.09524849020     
 iteration         1951 MCMCOBJ=   -6561.70171904322     
 iteration         1952 MCMCOBJ=   -6567.83883465765     
 iteration         1953 MCMCOBJ=   -6578.76150497633     
 iteration         1954 MCMCOBJ=   -6603.04021173495     
 iteration         1955 MCMCOBJ=   -6629.27453053820     
 iteration         1956 MCMCOBJ=   -6666.35135683296     
 iteration         1957 MCMCOBJ=   -6652.00601714650     
 iteration         1958 MCMCOBJ=   -6627.59645165191     
 iteration         1959 MCMCOBJ=   -6629.10719006190     
 iteration         1960 MCMCOBJ=   -6609.78075634677     
 iteration         1961 MCMCOBJ=   -6613.18935506423     
 iteration         1962 MCMCOBJ=   -6631.68543641822     
 iteration         1963 MCMCOBJ=   -6576.81748138457     
 iteration         1964 MCMCOBJ=   -6631.96939719424     
 iteration         1965 MCMCOBJ=   -6630.52589120951     
 iteration         1966 MCMCOBJ=   -6628.28895011274     
 iteration         1967 MCMCOBJ=   -6641.31439608415     
 iteration         1968 MCMCOBJ=   -6672.58529086890     
 iteration         1969 MCMCOBJ=   -6672.58529852406     
 iteration         1970 MCMCOBJ=   -6603.53931750290     
 iteration         1971 MCMCOBJ=   -6624.14073870782     
 iteration         1972 MCMCOBJ=   -6626.01079550646     
 iteration         1973 MCMCOBJ=   -6601.21133426347     
 iteration         1974 MCMCOBJ=   -6593.28122110438     
 iteration         1975 MCMCOBJ=   -6596.57443628481     
 iteration         1976 MCMCOBJ=   -6623.09155896370     
 iteration         1977 MCMCOBJ=   -6601.99250331788     
 iteration         1978 MCMCOBJ=   -6673.53836558338     
 iteration         1979 MCMCOBJ=   -6602.01909688447     
 iteration         1980 MCMCOBJ=   -6621.24417334598     
 iteration         1981 MCMCOBJ=   -6604.46148502243     
 iteration         1982 MCMCOBJ=   -6607.35527668334     
 iteration         1983 MCMCOBJ=   -6529.90228427545     
 iteration         1984 MCMCOBJ=   -6622.14274948980     
 iteration         1985 MCMCOBJ=   -6640.67141800362     
 iteration         1986 MCMCOBJ=   -6637.67039216403     
 iteration         1987 MCMCOBJ=   -6655.83133824219     
 iteration         1988 MCMCOBJ=   -6612.93944519848     
 iteration         1989 MCMCOBJ=   -6563.89630916498     
 iteration         1990 MCMCOBJ=   -6598.76357131455     
 iteration         1991 MCMCOBJ=   -6626.17094858499     
 iteration         1992 MCMCOBJ=   -6620.37147397429     
 iteration         1993 MCMCOBJ=   -6652.37379342668     
 iteration         1994 MCMCOBJ=   -6634.95133800171     
 iteration         1995 MCMCOBJ=   -6674.92595580456     
 iteration         1996 MCMCOBJ=   -6671.01285136734     
 iteration         1997 MCMCOBJ=   -6639.56791767073     
 iteration         1998 MCMCOBJ=   -6639.14143575959     
 iteration         1999 MCMCOBJ=   -6641.59814930066     
 iteration         2000 MCMCOBJ=   -6630.25303286987     
 
 #TERM:
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation  time in seconds:  3652.68
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6625.884       **************************************************
 #OBJS:********************************************       35.845 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.21E+00  5.59E-01 -1.80E-01  2.27E+00  2.34E-01  3.71E+00 -7.03E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.28E-01
 
 ETA2
+       -2.55E-02  1.44E-01
 
 ETA3
+        2.93E-02 -1.27E-02  8.39E-02
 
 ETA4
+        2.21E-02  3.27E-02 -1.38E-02  2.23E-01
 
 ETA5
+        2.08E-02  2.05E-02 -1.83E-03 -2.46E-02  1.72E-01
 
 ETA6
+       -1.59E-02  1.30E-02  1.86E-02  1.23E-02 -6.00E-02  1.96E-01
 
 ETA7
+        1.51E-02 -2.98E-02  2.04E-02 -5.69E-02  1.93E-02  5.75E-03  2.18E-01
 
 ETA8
+        7.33E-02  6.44E-02  2.59E-02  3.31E-02 -7.00E-03 -4.63E-02  5.20E-02  1.88E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.53E-03
 
 EPS2
+        0.00E+00  2.25E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        4.76E-01
 
 ETA2
+       -1.39E-01  3.76E-01
 
 ETA3
+        2.16E-01 -1.22E-01  2.87E-01
 
 ETA4
+        9.86E-02  1.84E-01 -1.04E-01  4.70E-01
 
 ETA5
+        1.05E-01  1.32E-01 -1.57E-02 -1.27E-01  4.12E-01
 
 ETA6
+       -7.64E-02  8.25E-02  1.50E-01  5.93E-02 -3.32E-01  4.39E-01
 
 ETA7
+        6.89E-02 -1.64E-01  1.54E-01 -2.58E-01  9.91E-02  2.75E-02  4.65E-01
 
 ETA8
+        3.53E-01  3.95E-01  2.05E-01  1.61E-01 -3.85E-02 -2.40E-01  2.55E-01  4.32E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.75E-02
 
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
 
         6.87E-02  6.47E-02  4.79E-02  7.08E-02  5.97E-02  7.03E-02  6.56E-02  6.26E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        4.37E-02
 
 ETA2
+        2.45E-02  4.04E-02
 
 ETA3
+        1.79E-02  1.65E-02  2.42E-02
 
 ETA4
+        2.71E-02  2.51E-02  2.00E-02  4.89E-02
 
 ETA5
+        2.40E-02  2.04E-02  1.66E-02  2.52E-02  3.48E-02
 
 ETA6
+        2.68E-02  2.33E-02  1.89E-02  2.93E-02  2.56E-02  5.09E-02
 
 ETA7
+        2.79E-02  2.68E-02  1.81E-02  2.80E-02  2.48E-02  2.91E-02  4.36E-02
 
 ETA8
+        2.68E-02  2.37E-02  1.82E-02  2.59E-02  2.23E-02  2.82E-02  2.71E-02  3.83E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.64E-04
 
 EPS2
+        0.00E+00  1.17E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        4.49E-02
 
 ETA2
+        1.26E-01  5.18E-02
 
 ETA3
+        1.24E-01  1.50E-01  4.08E-02
 
 ETA4
+        1.17E-01  1.31E-01  1.43E-01  5.08E-02
 
 ETA5
+        1.16E-01  1.25E-01  1.36E-01  1.22E-01  4.15E-02
 
 ETA6
+        1.25E-01  1.40E-01  1.44E-01  1.37E-01  1.28E-01  5.62E-02
 
 ETA7
+        1.22E-01  1.36E-01  1.30E-01  1.14E-01  1.22E-01  1.38E-01  4.58E-02
 
 ETA8
+        1.06E-01  1.17E-01  1.31E-01  1.18E-01  1.22E-01  1.28E-01  1.17E-01  4.31E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.39E-03
 
 EPS2
+        0.00E+00  3.90E-03
 
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
+        4.72E-03
 
 TH 2
+       -5.51E-04  4.18E-03
 
 TH 3
+        2.69E-04 -3.94E-05  2.29E-03
 
 TH 4
+        5.15E-04  6.16E-04  1.30E-04  5.01E-03
 
 TH 5
+        4.12E-04  2.40E-04  2.13E-05 -5.90E-04  3.56E-03
 
 TH 6
+       -4.89E-04 -6.77E-05  8.33E-05  4.49E-04 -1.00E-03  4.94E-03
 
 TH 7
+        2.00E-04 -7.10E-04  2.88E-04 -1.15E-03  1.86E-04  6.12E-05  4.31E-03
 
 TH 8
+        1.43E-03  1.19E-03  5.89E-04  8.10E-04 -3.47E-05 -9.42E-04  1.08E-03  3.92E-03
 
 OM11
+       -7.78E-06 -1.47E-04 -6.34E-05  4.28E-05  6.63E-05 -3.83E-05  1.37E-04 -5.41E-05  1.91E-03
 
 OM12
+       -1.70E-06  1.35E-04  2.56E-05  9.20E-05 -8.51E-05  5.02E-05 -4.09E-05 -3.27E-05 -1.98E-04  6.01E-04
 
 OM13
+        5.54E-05  2.57E-05  5.61E-05  5.81E-05  4.38E-05  3.01E-05  2.68E-06 -1.76E-05  1.11E-04 -1.66E-05  3.19E-04
 
 OM14
+       -2.75E-06 -4.59E-05 -6.30E-05  1.56E-05 -6.43E-05  1.68E-05 -3.86E-05 -3.66E-05  9.94E-05  5.06E-05  1.73E-05  7.35E-04
 
 OM15
+        3.50E-05 -4.20E-05 -2.62E-06  2.90E-05 -1.17E-05  6.82E-05  1.26E-05 -2.40E-05  1.72E-04  4.63E-05  1.08E-05 -3.87E-05
          5.76E-04
 
 OM16
+        2.26E-05 -5.58E-05  4.19E-05  7.21E-05 -8.55E-05  6.10E-05  3.74E-05 -1.02E-05 -9.22E-05  1.42E-05  1.77E-05  6.19E-05
         -1.16E-04  7.19E-04
 
 OM17
+       -4.86E-05  3.74E-05 -2.80E-06  8.82E-05  2.68E-05  2.29E-05 -5.58E-05 -4.44E-05  5.62E-05 -1.21E-04  3.17E-05 -1.39E-04
          3.66E-05  5.88E-05  7.79E-04
 
 OM18
+       -2.08E-05 -9.84E-05  1.40E-05  8.19E-05 -1.08E-05  4.30E-05  1.23E-05 -6.43E-06  4.77E-04  8.70E-05  1.09E-04  1.31E-04
         -8.55E-06 -1.08E-04  1.40E-04  7.18E-04
 
 OM22
+       -3.84E-05 -4.62E-04 -5.17E-05  4.52E-05  1.00E-04  4.13E-05 -5.39E-05  1.11E-04  1.24E-04 -3.25E-04 -1.04E-05 -1.01E-06
         -2.09E-05  7.50E-06  8.06E-05 -2.80E-05  1.63E-03
 
 OM23
+       -9.47E-06  1.03E-04  2.15E-05  3.69E-05 -5.20E-05  2.37E-05 -8.79E-06  3.38E-05 -2.69E-05  3.03E-05 -1.44E-05 -1.90E-06
          7.52E-06 -5.96E-06 -1.88E-05 -1.07E-05 -4.84E-05  2.73E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -8.35E-05 -2.70E-04  8.04E-05 -5.18E-05  8.91E-05 -2.07E-05  8.53E-05  7.79E-05  4.69E-06 -2.14E-05  6.61E-06 -8.98E-05
          2.24E-05 -1.35E-05  2.61E-06 -3.28E-06  2.53E-04 -8.14E-06  6.29E-04
 
 OM25
+        8.60E-05 -2.66E-06 -4.15E-07  7.52E-06  8.97E-06 -5.24E-05  6.65E-06  5.74E-05 -2.08E-05  3.01E-05 -5.92E-06  7.32E-06
         -6.10E-05 -7.90E-07 -2.52E-05  3.94E-05  1.12E-04  1.87E-05 -2.22E-05  4.15E-04
 
 OM26
+       -2.91E-05  1.31E-04 -3.64E-05 -1.17E-05  6.59E-06 -2.53E-05  2.99E-05 -3.43E-05 -1.03E-05  2.21E-05 -2.14E-06  8.63E-06
          9.71E-06 -3.78E-05 -1.05E-05 -2.15E-05 -5.12E-05  1.60E-05 -1.73E-05 -7.37E-05  5.44E-04
 
 OM27
+        1.17E-04  3.20E-04 -4.96E-06  6.20E-05 -1.25E-04 -2.17E-05 -1.02E-04 -8.29E-05 -6.26E-05  1.08E-04 -7.09E-06  3.61E-05
         -1.08E-05  1.25E-05 -9.68E-05  1.52E-05 -4.28E-04  6.45E-05 -2.15E-04  5.06E-05  7.41E-05  7.18E-04
 
 OM28
+       -2.36E-05  9.38E-06  4.25E-05 -7.84E-06 -1.03E-05 -2.41E-05 -3.70E-05  4.84E-05 -2.93E-05  7.62E-05 -1.92E-05  9.67E-06
          1.33E-05  2.94E-06 -2.92E-05 -1.43E-05  4.35E-04  6.11E-05  1.00E-04  2.98E-05 -1.25E-04  6.96E-05  5.59E-04
 
 OM33
+        1.03E-05 -3.27E-06 -1.07E-04 -7.49E-06  1.50E-05  1.68E-05  1.02E-05 -1.56E-05  6.54E-05 -8.02E-06  7.72E-05  5.73E-06
         -8.85E-06  4.86E-06 -1.40E-06  2.30E-05 -1.90E-06  9.54E-06 -2.22E-05 -1.95E-05  3.94E-06  6.83E-06 -3.38E-05  5.85E-04
 
 OM34
+       -1.30E-05  1.06E-05  1.77E-04  1.34E-04  2.12E-06 -2.15E-06 -4.05E-06  2.50E-05 -1.85E-05  5.58E-06  5.20E-05  9.84E-06
          1.85E-05  1.19E-05 -1.91E-05 -1.55E-05  4.24E-05  3.67E-05 -3.78E-06  5.30E-06  1.24E-06  1.54E-05  3.31E-05 -2.76E-05
         4.00E-04
 
 OM35
+        2.69E-05 -3.36E-05 -6.44E-05 -3.94E-05  2.78E-05 -8.25E-06  2.99E-05  1.70E-05 -3.38E-05  5.05E-07  1.97E-05 -7.36E-06
          3.88E-05 -2.03E-05 -2.22E-05 -9.67E-06 -1.58E-05  3.68E-05  2.97E-05 -1.62E-06 -1.17E-06  8.81E-06 -4.09E-06 -5.25E-07
        -4.22E-05  2.76E-04
 
 OM36
+       -7.52E-05  3.79E-05  1.41E-05  5.90E-05 -7.68E-05  4.00E-05  2.51E-05  2.04E-05  3.99E-06 -3.74E-06 -4.53E-06  1.28E-05
         -2.28E-05  2.69E-05  1.04E-05 -7.36E-07  3.92E-06  1.06E-05  3.92E-06 -4.89E-07 -2.43E-05  6.23E-06  1.36E-06  3.33E-05
         3.00E-05 -6.56E-05  3.56E-04
 
 OM37
+       -4.02E-06  1.81E-05 -2.66E-05 -8.06E-05  3.94E-05  3.17E-05  1.83E-05  1.41E-06 -1.34E-05 -7.92E-06 -2.78E-06 -1.46E-05
         -1.06E-05 -1.27E-05  3.19E-05  3.68E-06 -3.63E-05 -5.30E-05  1.04E-05 -1.97E-05  7.71E-06 -2.04E-05 -3.39E-05  4.89E-05
        -8.42E-05  3.17E-05  5.08E-06  3.26E-04
 
 OM38
+        1.79E-05  3.22E-05  8.11E-05  6.26E-05  4.50E-05  3.73E-05 -2.00E-05  3.74E-05  2.60E-05  3.05E-06  1.18E-04 -1.64E-06
          5.02E-06  1.39E-06  7.83E-06  4.76E-05  1.79E-05  7.77E-05 -1.11E-05 -1.09E-05  1.31E-06  8.44E-06  1.51E-05  1.61E-04
         7.30E-05 -1.45E-05 -6.60E-05  5.01E-05  3.31E-04
 
 OM44
+       -7.63E-05 -2.87E-05  3.02E-04  4.08E-04  4.10E-05 -1.17E-04 -2.34E-05 -1.21E-05 -1.95E-05 -7.29E-07  4.82E-05  1.44E-04
          3.53E-05  5.88E-05  3.94E-05  1.90E-05  1.10E-04 -2.39E-05  1.72E-04  5.21E-06  3.91E-05 -4.08E-05  4.17E-05 -4.94E-05
         1.10E-06 -2.35E-05 -6.36E-05  2.10E-05  2.62E-05  2.40E-03
 
 OM45
+        4.59E-05 -1.97E-05 -8.94E-05 -2.89E-05  7.54E-05 -1.61E-05 -3.30E-05  7.17E-06  2.30E-05  3.31E-05  2.59E-06  4.58E-05
          5.69E-05 -1.26E-05 -1.23E-05  2.13E-05 -1.93E-05 -9.67E-06  3.05E-05  4.71E-05  1.70E-05  2.45E-05 -3.99E-06 -2.46E-05
        -1.90E-05  2.45E-05 -1.64E-05 -1.49E-05 -3.37E-05 -1.49E-04  6.35E-04
 
 OM46
+       -3.40E-05 -2.05E-05  4.67E-05  1.40E-04 -1.07E-04  2.13E-04 -2.55E-05 -3.31E-05 -5.94E-05  1.87E-05  3.00E-05 -7.30E-06
         -2.51E-05  8.27E-05  2.91E-06 -2.59E-05 -4.42E-05  1.02E-05  2.05E-05 -2.43E-05  7.40E-05  2.40E-05 -1.71E-05  7.71E-05
         1.66E-05  1.10E-06 -2.35E-06 -1.30E-05  3.33E-05  1.52E-04 -1.43E-04  8.60E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -6.10E-06 -7.63E-05 -7.25E-05 -1.34E-04  2.60E-05  8.94E-05  1.25E-05 -9.92E-05  4.69E-05  4.34E-06  8.54E-07  8.70E-06
         -9.33E-06  3.02E-05  4.10E-06  4.96E-05 -7.74E-05  7.02E-07 -1.36E-04  6.54E-06 -1.01E-05  1.06E-04  1.37E-06  1.07E-05
         3.93E-05 -5.19E-06  1.69E-05 -1.36E-05 -7.25E-06 -4.51E-04  8.72E-05 -3.51E-05  7.82E-04
 
 OM48
+       -9.01E-05  3.02E-05 -1.41E-05 -7.01E-05  7.15E-05 -5.53E-06  2.24E-05 -1.73E-05  5.60E-05  2.85E-05  2.01E-05  1.89E-04
          3.39E-06  1.46E-06 -3.00E-05  9.36E-05  3.07E-05  1.11E-05  1.55E-04 -1.12E-05 -2.53E-05  1.73E-05  1.17E-04  6.66E-06
         5.12E-05  1.89E-06 -2.19E-05  1.08E-05  1.65E-05  2.04E-04 -4.04E-05 -1.58E-04  1.44E-04  6.69E-04
 
 OM55
+       -1.55E-05  5.91E-05  2.55E-05  2.67E-05 -1.69E-06 -6.26E-05  5.81E-06  6.89E-05  4.77E-05  2.47E-05  1.19E-05 -3.22E-05
          1.03E-04 -2.54E-05  3.13E-05  5.98E-05  7.30E-05  2.65E-06  2.01E-07  9.81E-05 -2.96E-05 -5.12E-06  3.17E-05 -1.44E-05
        -9.81E-06  2.02E-06 -5.00E-05 -3.02E-05  1.04E-05  6.97E-05 -1.03E-04  2.57E-05 -1.34E-05  4.26E-05  1.21E-03
 
 OM56
+       -3.93E-05 -2.62E-05  3.54E-06  2.41E-05 -4.10E-05 -3.00E-05 -4.30E-07 -2.79E-05  2.13E-05 -1.36E-05  6.00E-06  3.97E-05
         -2.33E-05  8.74E-05  4.45E-05 -1.63E-05  1.47E-05 -1.49E-06  3.27E-06  1.47E-05  6.10E-05  3.44E-05  6.79E-06  4.13E-06
         1.34E-05  1.84E-05  1.42E-05 -4.10E-06  9.09E-06  7.34E-06  5.16E-05 -4.73E-05  7.90E-06  6.19E-06 -2.23E-04  6.56E-04
 
 OM57
+       -1.59E-05 -4.55E-05  4.55E-06  5.91E-05 -1.30E-04 -6.95E-05 -2.93E-05 -1.65E-05 -3.29E-05 -1.49E-05 -2.48E-05 -9.79E-07
          1.37E-05  2.08E-06  8.57E-05  3.07E-05 -4.56E-05  9.37E-06  1.76E-05 -2.64E-05  2.80E-05  4.30E-05 -1.02E-05 -1.11E-05
        -3.67E-05  3.76E-05 -2.10E-05  3.11E-05 -5.39E-06  6.37E-05 -1.46E-04  5.44E-05 -6.23E-05 -6.50E-06  1.43E-04  2.07E-05
          6.13E-04
 
 OM58
+        5.15E-06 -3.46E-05 -1.07E-05  6.52E-06 -3.97E-05 -2.37E-05  7.29E-06 -3.26E-05  5.46E-05  2.88E-05  3.48E-06 -1.15E-05
          1.59E-04 -6.10E-05 -7.90E-06  4.38E-05 -1.86E-05  8.52E-06 -8.67E-06  1.22E-04 -1.10E-05  2.87E-05  1.36E-05 -1.14E-06
        -1.49E-05  6.41E-05 -1.48E-05  9.65E-06 -1.83E-05 -2.25E-05  5.43E-05 -2.23E-06 -2.33E-05 -7.13E-05 -4.12E-05 -9.06E-05
          1.47E-04  4.97E-04
 
 OM66
+        6.73E-06 -8.59E-06 -1.55E-05 -7.40E-06  1.55E-04 -1.89E-04  2.63E-05  1.40E-04 -1.01E-04  7.87E-05 -6.38E-06 -8.45E-05
          3.37E-05 -5.28E-05 -2.41E-05 -2.14E-06 -4.61E-05 -5.59E-05  1.97E-05  6.91E-06 -3.58E-06 -2.09E-05  1.56E-06 -8.85E-05
        -3.64E-05 -1.41E-05  4.64E-05 -1.11E-05 -6.34E-05 -6.69E-05  4.01E-05  2.68E-05 -6.91E-05 -1.00E-04  8.94E-05 -3.55E-04
         -5.12E-05  3.30E-05  2.59E-03
 
 OM67
+       -9.84E-05  2.69E-05 -2.67E-05  6.76E-06  7.81E-06 -1.15E-04 -1.17E-04 -1.22E-05 -2.87E-05 -2.03E-05 -1.57E-05 -1.48E-05
         -3.98E-05  1.13E-05 -3.26E-05 -2.87E-05  3.65E-05 -9.89E-06  2.63E-05  5.92E-06 -1.24E-04 -2.23E-05  3.95E-05 -2.57E-05
         1.53E-05 -3.51E-05  5.45E-05  1.59E-05 -7.18E-06 -3.42E-05  5.13E-05 -1.70E-04  1.96E-05  5.05E-05 -2.77E-05  6.27E-05
         -1.45E-04 -6.75E-05  1.02E-04  8.48E-04
 
 OM68
+       -7.37E-05  4.29E-05 -8.97E-06  9.52E-05 -9.72E-05  9.42E-05 -4.53E-05 -3.29E-05 -5.90E-05  9.96E-06 -8.32E-06  3.06E-06
         -6.31E-05  2.22E-04 -1.10E-05 -7.48E-05 -5.65E-05  1.24E-05  2.36E-05 -7.90E-07  1.68E-04  4.77E-05 -5.61E-05  8.34E-06
         3.60E-06 -2.03E-05  7.76E-05  2.71E-05  8.97E-06  7.05E-05 -2.89E-05  1.40E-04  2.59E-05  1.99E-05  1.88E-05  1.27E-05
         -1.62E-05 -9.78E-05 -5.16E-04  1.61E-04  7.98E-04
 
 OM77
+        3.19E-06  5.97E-05  1.70E-05  2.52E-05 -7.19E-05 -1.67E-04  1.85E-05  1.66E-04 -2.51E-05 -2.59E-05  2.86E-05 -9.87E-06
          4.18E-05 -2.93E-05  7.38E-06 -1.60E-05  1.04E-04 -1.15E-05  8.67E-05 -1.91E-05 -2.47E-05 -2.79E-04 -6.07E-06 -3.08E-05
        -2.90E-05  1.32E-05 -1.50E-05  7.95E-05  1.14E-05  2.06E-04 -7.98E-05  3.18E-06 -3.62E-04 -7.10E-05  7.91E-06 -4.77E-05
          1.32E-04  5.37E-05  4.23E-05  3.38E-05 -2.81E-05  1.90E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        2.13E-05 -1.06E-04 -8.08E-05  4.97E-06 -2.55E-05  7.05E-06 -1.48E-05 -7.73E-05  2.56E-05 -6.58E-06  2.42E-05  1.46E-05
          1.44E-05  3.11E-05  1.83E-04  9.01E-05 -1.05E-04 -2.21E-05 -5.17E-05  2.27E-05  4.16E-05  1.26E-04 -5.82E-05 -4.43E-06
        -1.84E-06  9.17E-06 -8.15E-06  6.26E-05  2.41E-05 -4.11E-05 -4.43E-06  2.33E-05  4.74E-05 -9.63E-05 -1.56E-05  1.23E-05
          2.26E-05  6.28E-05 -2.64E-05 -1.63E-04 -1.59E-05  4.31E-04  7.37E-04
 
 OM88
+        4.09E-05 -4.53E-05 -3.81E-06  5.37E-05  1.23E-04 -2.27E-05 -2.37E-05  7.45E-05  1.89E-04  8.14E-05  5.83E-05  7.97E-05
         -2.04E-05 -1.13E-04  6.53E-05  4.32E-04  7.47E-05  5.41E-05  4.49E-05  2.99E-05 -8.79E-05  9.13E-05  3.28E-04  3.11E-05
         3.01E-05  8.59E-06 -7.51E-05  5.73E-06  1.35E-04 -1.48E-05  3.78E-07 -8.06E-05  6.45E-05  2.37E-04  4.37E-05 -6.97E-06
         -1.75E-05 -5.74E-05  1.14E-04 -7.65E-05 -3.54E-04  1.54E-04  3.68E-04  1.46E-03
 
 SG11
+        3.11E-07 -1.16E-07  1.06E-07 -6.45E-08  8.08E-07  8.96E-07  1.21E-06  2.97E-07  5.30E-07  1.89E-07 -1.84E-09  1.12E-06
         -7.30E-07  2.65E-07 -1.97E-07  2.07E-07 -1.16E-06 -1.46E-07  3.27E-08  4.61E-07 -4.65E-07 -1.15E-07  3.52E-07 -1.22E-06
        -4.97E-07  2.64E-08 -6.01E-07 -1.93E-07 -6.18E-07  4.55E-07  3.02E-07  5.15E-07 -8.41E-07 -1.49E-07 -9.06E-08  9.12E-07
         -1.05E-08 -1.06E-07  1.05E-06  6.08E-07 -2.89E-07 -8.35E-07 -1.02E-06 -1.16E-07  4.41E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -1.13E-07  1.23E-07 -2.62E-06 -5.16E-07 -5.40E-07 -7.45E-07 -1.43E-06 -2.99E-06 -6.38E-07 -5.98E-07  1.13E-07 -2.23E-07
         -8.52E-07 -1.82E-06 -5.09E-07  9.27E-07  2.64E-08  2.65E-07 -1.29E-06 -1.20E-07  4.03E-08  3.43E-07  7.01E-07 -6.32E-07
         2.27E-07 -1.41E-08  2.18E-07  1.34E-07 -4.96E-07 -1.60E-07 -8.77E-07 -1.40E-06  6.69E-07  1.42E-06 -7.30E-07 -2.20E-06
         -3.53E-07  5.73E-07 -3.86E-06  4.79E-07 -5.04E-07  1.54E-06 -9.74E-07  2.09E-06 -3.30E-08  0.00E+00  1.37E-06
 
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
+        6.87E-02
 
 TH 2
+       -1.24E-01  6.47E-02
 
 TH 3
+        8.19E-02 -1.27E-02  4.79E-02
 
 TH 4
+        1.06E-01  1.35E-01  3.85E-02  7.08E-02
 
 TH 5
+        1.00E-01  6.22E-02  7.47E-03 -1.39E-01  5.97E-02
 
 TH 6
+       -1.01E-01 -1.49E-02  2.47E-02  9.02E-02 -2.39E-01  7.03E-02
 
 TH 7
+        4.44E-02 -1.67E-01  9.16E-02 -2.47E-01  4.76E-02  1.33E-02  6.56E-02
 
 TH 8
+        3.33E-01  2.95E-01  1.96E-01  1.83E-01 -9.27E-03 -2.14E-01  2.63E-01  6.26E-02
 
 OM11
+       -2.59E-03 -5.19E-02 -3.03E-02  1.38E-02  2.54E-02 -1.25E-02  4.78E-02 -1.98E-02  4.37E-02
 
 OM12
+       -1.01E-03  8.51E-02  2.19E-02  5.30E-02 -5.82E-02  2.92E-02 -2.54E-02 -2.13E-02 -1.85E-01  2.45E-02
 
 OM13
+        4.52E-02  2.22E-02  6.56E-02  4.59E-02  4.10E-02  2.40E-02  2.28E-03 -1.57E-02  1.42E-01 -3.79E-02  1.79E-02
 
 OM14
+       -1.48E-03 -2.62E-02 -4.85E-02  8.12E-03 -3.97E-02  8.79E-03 -2.17E-02 -2.16E-02  8.38E-02  7.62E-02  3.57E-02  2.71E-02
 
 OM15
+        2.12E-02 -2.71E-02 -2.28E-03  1.71E-02 -8.17E-03  4.05E-02  8.02E-03 -1.60E-02  1.64E-01  7.88E-02  2.51E-02 -5.96E-02
          2.40E-02
 
 OM16
+        1.23E-02 -3.22E-02  3.26E-02  3.79E-02 -5.34E-02  3.23E-02  2.13E-02 -6.08E-03 -7.86E-02  2.16E-02  3.70E-02  8.51E-02
         -1.80E-01  2.68E-02
 
 OM17
+       -2.54E-02  2.07E-02 -2.09E-03  4.46E-02  1.61E-02  1.17E-02 -3.05E-02 -2.54E-02  4.60E-02 -1.77E-01  6.36E-02 -1.83E-01
          5.47E-02  7.86E-02  2.79E-02
 
 OM18
+       -1.13E-02 -5.68E-02  1.09E-02  4.32E-02 -6.76E-03  2.28E-02  6.99E-03 -3.83E-03  4.07E-01  1.32E-01  2.28E-01  1.81E-01
         -1.33E-02 -1.50E-01  1.88E-01  2.68E-02
 
 OM22
+       -1.38E-02 -1.77E-01 -2.67E-02  1.58E-02  4.14E-02  1.45E-02 -2.03E-02  4.40E-02  7.00E-02 -3.28E-01 -1.44E-02 -9.18E-04
         -2.16E-02  6.92E-03  7.15E-02 -2.59E-02  4.04E-02
 
 OM23
+       -8.35E-03  9.66E-02  2.72E-02  3.16E-02 -5.28E-02  2.04E-02 -8.11E-03  3.27E-02 -3.72E-02  7.49E-02 -4.88E-02 -4.24E-03
          1.90E-02 -1.35E-02 -4.07E-02 -2.43E-02 -7.24E-02  1.65E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -4.85E-02 -1.66E-01  6.69E-02 -2.91E-02  5.95E-02 -1.18E-02  5.19E-02  4.96E-02  4.28E-03 -3.48E-02  1.48E-02 -1.32E-01
          3.73E-02 -2.01E-02  3.72E-03 -4.88E-03  2.49E-01 -1.96E-02  2.51E-02
 
 OM25
+        6.15E-02 -2.02E-03 -4.25E-04  5.21E-03  7.38E-03 -3.66E-02  4.97E-03  4.50E-02 -2.34E-02  6.03E-02 -1.63E-02  1.33E-02
         -1.25E-01 -1.45E-03 -4.43E-02  7.23E-02  1.36E-01  5.56E-02 -4.35E-02  2.04E-02
 
 OM26
+       -1.82E-02  8.70E-02 -3.26E-02 -7.08E-03  4.73E-03 -1.54E-02  1.95E-02 -2.34E-02 -1.01E-02  3.87E-02 -5.14E-03  1.36E-02
          1.74E-02 -6.04E-02 -1.62E-02 -3.44E-02 -5.42E-02  4.16E-02 -2.95E-02 -1.55E-01  2.33E-02
 
 OM27
+        6.38E-02  1.85E-01 -3.87E-03  3.27E-02 -7.80E-02 -1.15E-02 -5.78E-02 -4.94E-02 -5.34E-02  1.64E-01 -1.48E-02  4.96E-02
         -1.67E-02  1.74E-02 -1.29E-01  2.11E-02 -3.95E-01  1.46E-01 -3.19E-01  9.27E-02  1.18E-01  2.68E-02
 
 OM28
+       -1.45E-02  6.13E-03  3.75E-02 -4.68E-03 -7.32E-03 -1.45E-02 -2.38E-02  3.27E-02 -2.84E-02  1.31E-01 -4.55E-02  1.51E-02
          2.34E-02  4.63E-03 -4.43E-02 -2.25E-02  4.55E-01  1.56E-01  1.69E-01  6.20E-02 -2.26E-01  1.10E-01  2.37E-02
 
 OM33
+        6.20E-03 -2.09E-03 -9.19E-02 -4.37E-03  1.04E-02  9.89E-03  6.41E-03 -1.03E-02  6.18E-02 -1.35E-02  1.79E-01  8.74E-03
         -1.52E-02  7.50E-03 -2.07E-03  3.55E-02 -1.94E-03  2.39E-02 -3.66E-02 -3.96E-02  6.99E-03  1.05E-02 -5.91E-02  2.42E-02
 
 OM34
+       -9.44E-03  8.18E-03  1.85E-01  9.45E-02  1.78E-03 -1.53E-03 -3.09E-03  2.00E-02 -2.12E-02  1.14E-02  1.46E-01  1.82E-02
          3.87E-02  2.23E-02 -3.43E-02 -2.89E-02  5.24E-02  1.11E-01 -7.54E-03  1.30E-02  2.65E-03  2.87E-02  7.00E-02 -5.71E-02
         2.00E-02
 
 OM35
+        2.35E-02 -3.12E-02 -8.10E-02 -3.34E-02  2.81E-02 -7.06E-03  2.75E-02  1.63E-02 -4.65E-02  1.24E-03  6.65E-02 -1.63E-02
          9.73E-02 -4.54E-02 -4.79E-02 -2.17E-02 -2.35E-02  1.34E-01  7.13E-02 -4.80E-03 -3.02E-03  1.98E-02 -1.04E-02 -1.31E-03
        -1.27E-01  1.66E-02
 
 OM36
+       -5.81E-02  3.11E-02  1.56E-02  4.42E-02 -6.82E-02  3.02E-02  2.03E-02  1.73E-02  4.84E-03 -8.08E-03 -1.35E-02  2.51E-02
         -5.04E-02  5.32E-02  1.98E-02 -1.46E-03  5.14E-03  3.42E-02  8.30E-03 -1.27E-03 -5.52E-02  1.23E-02  3.06E-03  7.31E-02
         7.95E-02 -2.09E-01  1.89E-02
 
 OM37
+       -3.24E-03  1.55E-02 -3.08E-02 -6.30E-02  3.66E-02  2.50E-02  1.55E-02  1.25E-03 -1.70E-02 -1.79E-02 -8.62E-03 -2.98E-02
         -2.44E-02 -2.62E-02  6.33E-02  7.60E-03 -4.97E-02 -1.78E-01  2.29E-02 -5.34E-02  1.83E-02 -4.21E-02 -7.94E-02  1.12E-01
        -2.33E-01  1.06E-01  1.49E-02  1.81E-02
 
 OM38
+        1.43E-02  2.73E-02  9.31E-02  4.85E-02  4.15E-02  2.92E-02 -1.68E-02  3.28E-02  3.27E-02  6.85E-03  3.62E-01 -3.33E-03
          1.15E-02  2.85E-03  1.54E-02  9.77E-02  2.43E-02  2.58E-01 -2.44E-02 -2.95E-02  3.08E-03  1.73E-02  3.50E-02  3.65E-01
         2.01E-01 -4.81E-02 -1.92E-01  1.52E-01  1.82E-02
 
 OM44
+       -2.27E-02 -9.07E-03  1.29E-01  1.18E-01  1.40E-02 -3.39E-02 -7.28E-03 -3.94E-03 -9.09E-03 -6.08E-04  5.52E-02  1.08E-01
          3.00E-02  4.48E-02  2.89E-02  1.45E-02  5.58E-02 -2.95E-02  1.40E-01  5.23E-03  3.43E-02 -3.11E-02  3.60E-02 -4.17E-02
         1.12E-03 -2.89E-02 -6.89E-02  2.37E-02  2.94E-02  4.89E-02
 
 OM45
+        2.65E-02 -1.21E-02 -7.41E-02 -1.62E-02  5.02E-02 -9.09E-03 -2.00E-02  4.55E-03  2.08E-02  5.35E-02  5.75E-03  6.70E-02
          9.42E-02 -1.86E-02 -1.74E-02  3.15E-02 -1.89E-02 -2.32E-02  4.83E-02  9.19E-02  2.89E-02  3.62E-02 -6.70E-03 -4.04E-02
        -3.77E-02  5.84E-02 -3.45E-02 -3.26E-02 -7.36E-02 -1.20E-01  2.52E-02
 
 OM46
+       -1.69E-02 -1.08E-02  3.33E-02  6.75E-02 -6.12E-02  1.03E-01 -1.32E-02 -1.80E-02 -4.63E-02  2.60E-02  5.73E-02 -9.18E-03
         -3.57E-02  1.05E-01  3.55E-03 -3.30E-02 -3.73E-02  2.11E-02  2.79E-02 -4.06E-02  1.08E-01  3.06E-02 -2.47E-02  1.09E-01
         2.83E-02  2.27E-03 -4.26E-03 -2.45E-02  6.24E-02  1.06E-01 -1.94E-01  2.93E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -3.18E-03 -4.22E-02 -5.42E-02 -6.75E-02  1.56E-02  4.55E-02  6.81E-03 -5.66E-02  3.83E-02  6.34E-03  1.71E-03  1.15E-02
         -1.39E-02  4.02E-02  5.25E-03  6.62E-02 -6.85E-02  1.52E-03 -1.94E-01  1.15E-02 -1.55E-02  1.41E-01  2.07E-03  1.59E-02
         7.04E-02 -1.12E-02  3.21E-02 -2.70E-02 -1.43E-02 -3.29E-01  1.24E-01 -4.28E-02  2.80E-02
 
 OM48
+       -5.07E-02  1.81E-02 -1.14E-02 -3.83E-02  4.63E-02 -3.04E-03  1.32E-02 -1.07E-02  4.95E-02  4.49E-02  4.36E-02  2.69E-01
          5.46E-03  2.10E-03 -4.16E-02  1.35E-01  2.94E-02  2.61E-02  2.40E-01 -2.13E-02 -4.19E-02  2.50E-02  1.91E-01  1.06E-02
         9.90E-02  4.39E-03 -4.50E-02  2.32E-02  3.50E-02  1.61E-01 -6.21E-02 -2.09E-01  1.99E-01  2.59E-02
 
 OM55
+       -6.50E-03  2.62E-02  1.53E-02  1.08E-02 -8.14E-04 -2.56E-02  2.54E-03  3.16E-02  3.13E-02  2.90E-02  1.91E-02 -3.41E-02
          1.23E-01 -2.72E-02  3.23E-02  6.41E-02  5.18E-02  4.60E-03  2.30E-04  1.38E-01 -3.64E-02 -5.48E-03  3.85E-02 -1.71E-02
        -1.41E-02  3.48E-03 -7.61E-02 -4.81E-02  1.65E-02  4.09E-02 -1.18E-01  2.52E-02 -1.38E-02  4.73E-02  3.48E-02
 
 OM56
+       -2.23E-02 -1.58E-02  2.89E-03  1.33E-02 -2.68E-02 -1.67E-02 -2.56E-04 -1.74E-02  1.90E-02 -2.16E-02  1.31E-02  5.71E-02
         -3.78E-02  1.27E-01  6.23E-02 -2.38E-02  1.42E-02 -3.51E-03  5.09E-03  2.82E-02  1.02E-01  5.01E-02  1.12E-02  6.67E-03
         2.63E-02  4.32E-02  2.94E-02 -8.87E-03  1.95E-02  5.86E-03  7.99E-02 -6.30E-02  1.10E-02  9.34E-03 -2.50E-01  2.56E-02
 
 OM57
+       -9.34E-03 -2.84E-02  3.83E-03  3.37E-02 -8.82E-02 -4.00E-02 -1.80E-02 -1.07E-02 -3.04E-02 -2.46E-02 -5.60E-02 -1.46E-03
          2.31E-02  3.13E-03  1.24E-01  4.63E-02 -4.56E-02  2.29E-02  2.84E-02 -5.24E-02  4.84E-02  6.48E-02 -1.73E-02 -1.85E-02
        -7.42E-02  9.15E-02 -4.51E-02  6.95E-02 -1.20E-02  5.26E-02 -2.33E-01  7.49E-02 -9.00E-02 -1.02E-02  1.66E-01  3.26E-02
          2.48E-02
 
 OM58
+        3.36E-03 -2.40E-02 -1.01E-02  4.13E-03 -2.98E-02 -1.51E-02  4.98E-03 -2.34E-02  5.60E-02  5.28E-02  8.73E-03 -1.91E-02
          2.97E-01 -1.02E-01 -1.27E-02  7.33E-02 -2.07E-02  2.31E-02 -1.55E-02  2.68E-01 -2.11E-02  4.81E-02  2.58E-02 -2.12E-03
        -3.34E-02  1.73E-01 -3.51E-02  2.40E-02 -4.51E-02 -2.06E-02  9.66E-02 -3.41E-03 -3.73E-02 -1.24E-01 -5.31E-02 -1.59E-01
          2.67E-01  2.23E-02
 
 OM66
+        1.93E-03 -2.61E-03 -6.36E-03 -2.06E-03  5.10E-02 -5.30E-02  7.88E-03  4.40E-02 -4.53E-02  6.31E-02 -7.02E-03 -6.13E-02
          2.76E-02 -3.87E-02 -1.70E-02 -1.57E-03 -2.24E-02 -6.65E-02  1.55E-02  6.67E-03 -3.02E-03 -1.53E-02  1.30E-03 -7.20E-02
        -3.58E-02 -1.66E-02  4.84E-02 -1.21E-02 -6.85E-02 -2.69E-02  3.13E-02  1.80E-02 -4.86E-02 -7.63E-02  5.05E-02 -2.72E-01
         -4.06E-02  2.91E-02  5.09E-02
 
 OM67
+       -4.92E-02  1.43E-02 -1.92E-02  3.28E-03  4.49E-03 -5.64E-02 -6.10E-02 -6.67E-03 -2.26E-02 -2.85E-02 -3.01E-02 -1.88E-02
         -5.70E-02  1.45E-02 -4.01E-02 -3.68E-02  3.10E-02 -2.06E-02  3.60E-02  9.99E-03 -1.82E-01 -2.86E-02  5.73E-02 -3.65E-02
         2.63E-02 -7.26E-02  9.93E-02  3.02E-02 -1.35E-02 -2.40E-02  6.99E-02 -1.99E-01  2.40E-02  6.70E-02 -2.74E-02  8.40E-02
         -2.01E-01 -1.04E-01  6.92E-02  2.91E-02
 
 OM68
+       -3.80E-02  2.35E-02 -6.63E-03  4.76E-02 -5.77E-02  4.74E-02 -2.44E-02 -1.86E-02 -4.77E-02  1.44E-02 -1.65E-02  3.99E-03
         -9.32E-02  2.93E-01 -1.40E-02 -9.89E-02 -4.95E-02  2.67E-02  3.32E-02 -1.37E-03  2.55E-01  6.30E-02 -8.39E-02  1.22E-02
         6.38E-03 -4.33E-02  1.46E-01  5.32E-02  1.75E-02  5.10E-02 -4.05E-02  1.69E-01  3.27E-02  2.73E-02  1.91E-02  1.76E-02
         -2.32E-02 -1.55E-01 -3.59E-01  1.96E-01  2.82E-02
 
 OM77
+        1.06E-03  2.12E-02  8.14E-03  8.16E-03 -2.76E-02 -5.45E-02  6.45E-03  6.08E-02 -1.32E-02 -2.43E-02  3.67E-02 -8.34E-03
          4.00E-02 -2.51E-02  6.06E-03 -1.37E-02  5.87E-02 -1.60E-02  7.93E-02 -2.15E-02 -2.43E-02 -2.38E-01 -5.88E-03 -2.92E-02
        -3.33E-02  1.81E-02 -1.82E-02  1.01E-01  1.44E-02  9.64E-02 -7.27E-02  2.49E-03 -2.97E-01 -6.29E-02  5.21E-03 -4.27E-02
          1.22E-01  5.52E-02  1.91E-02  2.66E-02 -2.28E-02  4.36E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.15E-02 -6.03E-02 -6.22E-02  2.59E-03 -1.57E-02  3.69E-03 -8.33E-03 -4.55E-02  2.16E-02 -9.89E-03  4.99E-02  1.99E-02
          2.21E-02  4.27E-02  2.41E-01  1.24E-01 -9.56E-02 -4.93E-02 -7.60E-02  4.11E-02  6.56E-02  1.73E-01 -9.07E-02 -6.74E-03
        -3.39E-03  2.03E-02 -1.59E-02  1.28E-01  4.89E-02 -3.09E-02 -6.48E-03  2.93E-02  6.24E-02 -1.37E-01 -1.65E-02  1.77E-02
          3.36E-02  1.04E-01 -1.91E-02 -2.07E-01 -2.07E-02  3.64E-01  2.71E-02
 
 OM88
+        1.56E-02 -1.83E-02 -2.08E-03  1.98E-02  5.38E-02 -8.43E-03 -9.43E-03  3.11E-02  1.13E-01  8.68E-02  8.52E-02  7.68E-02
         -2.22E-02 -1.10E-01  6.12E-02  4.21E-01  4.83E-02  8.56E-02  4.68E-02  3.84E-02 -9.85E-02  8.91E-02  3.63E-01  3.36E-02
         3.93E-02  1.35E-02 -1.04E-01  8.29E-03  1.93E-01 -7.90E-03  3.92E-04 -7.18E-02  6.03E-02  2.39E-01  3.28E-02 -7.11E-03
         -1.85E-02 -6.73E-02  5.84E-02 -6.87E-02 -3.28E-01  9.25E-02  3.55E-01  3.83E-02
 
 SG11
+        6.82E-03 -2.70E-03  3.32E-03 -1.37E-03  2.04E-02  1.92E-02  2.78E-02  7.15E-03  1.83E-02  1.16E-02 -1.55E-04  6.25E-02
         -4.58E-02  1.49E-02 -1.07E-02  1.16E-02 -4.31E-02 -1.33E-02  1.96E-03  3.41E-02 -3.00E-02 -6.45E-03  2.24E-02 -7.57E-02
        -3.75E-02  2.39E-03 -4.80E-02 -1.61E-02 -5.12E-02  1.40E-02  1.81E-02  2.65E-02 -4.53E-02 -8.67E-03 -3.92E-03  5.36E-02
         -6.39E-04 -7.15E-03  3.12E-02  3.14E-02 -1.54E-02 -2.88E-02 -5.66E-02 -4.58E-03  6.64E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -1.40E-03  1.62E-03 -4.67E-02 -6.22E-03 -7.72E-03 -9.05E-03 -1.85E-02 -4.07E-02 -1.24E-02 -2.08E-02  5.40E-03 -7.00E-03
         -3.03E-02 -5.80E-02 -1.56E-02  2.95E-02  5.57E-04  1.37E-02 -4.39E-02 -5.03E-03  1.47E-03  1.09E-02  2.53E-02 -2.23E-02
         9.68E-03 -7.25E-04  9.87E-03  6.31E-03 -2.32E-02 -2.79E-03 -2.97E-02 -4.06E-02  2.04E-02  4.70E-02 -1.79E-02 -7.33E-02
         -1.21E-02  2.19E-02 -6.48E-02  1.40E-02 -1.52E-02  3.01E-02 -3.06E-02  4.67E-02 -4.24E-02  0.00E+00  1.17E-03
 
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
+        2.82E+02
 
 TH 2
+        1.08E+02  3.84E+02
 
 TH 3
+        1.24E+00  4.29E+01  5.14E+02
 
 TH 4
+       -1.49E+01 -4.04E+00  1.13E+01  2.52E+02
 
 TH 5
+       -5.22E+01 -6.80E+01 -1.69E+01  2.57E+01  3.34E+02
 
 TH 6
+       -9.23E+00 -4.30E+01 -3.25E+01 -3.45E+01  7.45E+01  2.49E+02
 
 TH 7
+        3.83E+01  1.06E+02  2.61E+00  8.28E+01 -3.07E+01 -4.54E+01  3.17E+02
 
 TH 8
+       -1.49E+02 -2.06E+02 -9.70E+01 -7.86E+01  6.98E+01  1.02E+02 -1.64E+02  4.84E+02
 
 OM11
+       -5.65E+00 -1.53E+01  1.53E+01 -2.12E+01 -4.14E+00  1.86E+01 -2.83E+01  3.07E+01  7.46E+02
 
 OM12
+       -1.72E+01 -3.51E+01  1.38E+01 -6.73E+01  2.06E+01 -1.50E+01  8.30E+00  5.25E+01  4.07E+02  2.75E+03
 
 OM13
+       -9.25E+01 -1.03E+02 -4.61E+01 -2.11E+01 -1.44E+00 -1.32E+00 -2.74E+01  1.19E+02 -5.59E+01  3.16E+02  4.32E+03
 
 OM14
+       -5.11E+00  2.49E+01  2.87E+01  8.68E+00  1.76E+01  1.59E+00  2.71E+01 -1.60E+01 -6.92E+00 -1.28E+01  3.72E+01  1.87E+03
 
 OM15
+       -3.15E+01  4.07E+01  1.74E+01  7.47E+00  7.96E+00 -4.41E+01  2.00E+00 -1.31E+01 -2.89E+02 -4.43E+02 -2.76E+01  4.21E+01
          2.47E+03
 
 OM16
+       -2.09E+01  8.68E+00 -3.10E+01 -4.54E+00  2.97E+01 -1.23E+00 -3.85E+01  1.69E+01 -5.23E+01 -2.79E+02 -2.39E+02 -2.34E+02
          4.36E+02  1.87E+03
 
 OM17
+       -2.03E+01 -9.76E+01 -2.05E+01 -3.22E+01 -2.09E+00  1.11E+01 -6.61E-01  5.53E+01  1.46E+02  5.22E+02 -8.88E+00  5.16E+02
         -3.05E+02 -2.90E+02  1.90E+03
 
 OM18
+        4.07E+01  8.45E+01 -3.54E+01  1.41E+01  2.20E+01 -1.75E+01  2.35E+00 -5.23E+01 -6.28E+02 -9.64E+02 -7.06E+02 -4.57E+02
          4.94E+02  5.19E+02 -7.58E+02  2.97E+03
 
 OM22
+        1.83E+01  1.11E+02  7.80E+01 -4.02E+01 -3.29E+01 -5.69E+01  7.31E+01 -7.54E+01  4.90E+01  1.04E+03  2.23E+02 -4.33E+01
         -1.34E+02 -1.86E+02  1.37E+02 -4.21E+02  1.74E+03
 
 OM23
+       -6.81E+00 -5.73E+01  2.36E+01 -4.52E+00  5.09E+01 -2.43E+01  4.50E-02  6.90E+00 -1.63E+01  1.99E+02  9.69E+02 -6.60E+00
         -6.60E+01 -8.46E+01 -8.37E+01 -3.13E+01  5.20E+02  5.13E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        5.08E+01  1.40E+02 -6.66E+01  1.51E+01 -4.35E+01 -7.79E+00  2.22E+01 -9.01E+01  4.92E+00 -6.23E-01 -9.03E+01  6.03E+02
         -2.53E+01  1.77E+01  2.56E+02 -1.14E+02 -5.88E+01 -5.81E+01  2.50E+03
 
 OM25
+       -5.99E+01 -2.60E+01  4.71E+00  3.73E+01  5.46E+00  2.81E+01 -3.01E+01 -3.73E+00  8.83E+00 -5.67E+02 -6.36E+01  1.98E+01
          8.81E+02  3.04E+02 -9.40E+01  2.07E+02 -8.25E+02 -5.41E+02  4.40E+01  3.75E+03
 
 OM26
+       -2.23E+01 -1.23E+02 -1.95E+00  3.67E+01  1.91E+01  5.54E+01 -7.59E+01  7.80E+01 -4.42E+01 -5.42E+02 -1.58E+02 -1.10E+02
          2.31E+02  5.85E+02 -1.34E+02  4.18E+02 -7.58E+02 -5.61E+02 -1.11E+02  1.05E+03  3.05E+03
 
 OM27
+       -1.07E+02 -1.51E+02 -3.75E+01 -3.62E+01  6.93E+01  2.13E+01  5.93E+00  1.06E+02  8.75E+01  5.92E+02  1.25E+02  2.35E+02
         -1.81E+02 -1.38E+02  6.81E+02 -4.97E+02  1.13E+03 -4.72E+00  7.73E+02 -7.58E+02 -7.63E+02  3.14E+03
 
 OM28
+        7.75E+00 -8.15E+01 -6.81E+01  8.65E+01  5.10E+01  6.68E+01 -5.33E+01  4.30E+01 -1.45E+02 -1.57E+03 -2.63E+02 -1.25E+02
          2.74E+02  3.10E+02 -4.90E+02  1.27E+03 -2.09E+03 -9.41E+02 -4.54E+02  1.16E+03  1.60E+03 -1.94E+03  5.37E+03
 
 OM33
+       -5.66E+00  1.95E+01  1.17E+02  8.50E+00  1.33E+00  1.55E+01 -8.24E+00 -5.01E+00 -7.21E+01 -1.01E+02 -1.20E+02  2.90E+01
          8.05E+01 -1.45E+01 -2.77E+01  1.35E+02 -8.51E+01  2.68E+02  6.30E+01  9.01E+01 -1.54E+00 -1.18E+02  2.52E+02  2.24E+03
 
 OM34
+        2.01E+01  3.97E+00 -1.96E+02 -7.31E+01 -9.79E+00  3.23E+01 -2.07E+01  2.72E+01  3.45E+01 -3.82E+01 -2.83E+02  5.15E+01
         -1.39E+02 -1.26E+01  9.10E+01  1.55E+02 -8.78E+01  7.10E+00  1.12E+02 -6.77E+01 -1.19E+02  2.63E+01 -1.48E+01  3.94E+02
         3.20E+03
 
 OM35
+        1.59E+01  4.82E+01  9.58E+01  6.55E+00 -4.05E+01 -5.50E+00 -1.68E+01 -6.28E+01  1.26E+02 -4.95E+01 -8.16E+02 -3.40E+01
         -8.33E+01  1.54E+02  1.10E+02  1.68E+02 -2.19E+02 -1.25E+03 -2.28E+02  4.23E+02  4.34E+02 -2.57E+02  5.06E+02 -1.21E+02
         2.00E+02  4.60E+03
 
 OM36
+        4.85E+01 -3.25E+01 -3.04E+01 -3.63E+01  3.02E+01 -1.13E+01 -4.46E+01 -1.70E+01  2.43E+01 -2.55E+01 -4.58E+02 -2.03E+02
          4.37E+01  1.36E+02 -6.83E+01 -4.79E+01 -1.48E+02 -8.57E+02 -1.97E+02  2.12E+02  5.16E+02 -1.66E+02  1.76E+02 -5.71E+02
        -4.79E+02  1.09E+03  3.75E+03
 
 OM37
+       -2.13E+01 -4.49E+01 -1.86E+01  4.66E+01 -2.26E+01 -4.34E+01 -2.20E+00 -3.27E+00  3.05E+00  4.38E+01  5.07E+02  1.06E+02
          7.33E+01  6.86E+01 -3.65E+01 -1.72E+01  1.87E+02  1.20E+03  4.36E+01 -5.80E+01 -1.79E+02  2.28E+02 -2.02E+02  8.30E+00
         8.64E+02 -7.48E+02 -5.69E+02  3.98E+03
 
 OM38
+        4.63E+01  2.66E+01 -1.16E+02 -1.14E+01 -5.22E+01 -2.38E+01  2.12E+01 -6.46E+01  5.81E+01 -1.81E+02 -1.82E+03 -4.18E+01
         -5.00E+01  1.28E+02  1.38E+01  1.54E+02 -3.71E+02 -2.11E+03  6.52E+01  3.54E+02  4.41E+02 -1.80E+02  5.96E+02 -1.30E+03
        -9.54E+02  1.12E+03  1.62E+03 -1.43E+03  5.85E+03
 
 OM44
+        1.15E+01  8.06E+00 -6.55E+01 -4.75E+01 -1.09E+01  2.27E+01 -1.31E+01  2.65E+01  1.54E+01  1.44E+01 -5.90E+01 -6.26E+01
         -6.49E+01 -3.60E+01 -2.85E+01 -1.83E+01 -2.31E+01  3.11E+01 -1.76E+01 -5.16E+01 -3.49E+01 -3.46E+01 -6.55E+00  5.23E+01
         5.91E+01  5.90E+01  8.20E+01 -3.39E+01  1.19E+00  5.45E+02
 
 OM45
+        5.31E+00  2.01E+01  6.73E+01 -3.82E+00 -2.96E+01 -1.33E+01  2.90E+01 -3.19E+01  2.42E+01 -4.96E+01 -7.77E+01 -3.06E+02
         -1.57E+02 -1.74E+01 -1.52E+02  2.04E-01  3.59E+01  4.09E+01 -4.17E+02 -1.32E+02 -9.23E+01 -2.24E+02  7.15E+01  1.08E+00
         3.07E+01 -4.66E+01  1.97E+02  7.42E-01  1.74E+02  4.31E+01  2.02E+03
 
 OM46
+        1.57E+01  1.20E+01  1.64E+01 -1.18E+01  8.26E+00 -5.31E+01  1.63E+01 -8.49E+00  2.98E+01  1.67E+01 -9.36E+01 -1.78E+02
          4.30E+01 -3.30E+01 -6.95E+01  2.32E+01  6.95E+01  4.89E+00 -2.92E+02  6.00E+01 -7.29E+00 -1.18E+02 -5.84E+01 -2.28E+02
        -1.46E+02  4.08E+00  1.55E+02  2.99E+01  6.87E+01 -1.18E+02  3.77E+02  1.54E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        8.32E+00  5.10E+01 -2.09E+01  1.28E+01 -1.96E+01 -1.24E+01  5.41E+00 -1.28E+01 -3.05E+00  7.96E+01 -1.51E+01  2.68E+02
         -4.60E+01 -9.95E+01  1.67E+02 -1.67E+02  5.46E+01  4.49E+00  5.98E+02 -4.81E+01 -5.29E+01  2.15E+02 -2.10E+02  2.83E+01
        -4.84E+01 -2.98E+01 -8.43E+01  4.43E+01  3.48E+01  3.37E+02 -3.10E+02 -2.11E+02  1.98E+03
 
 OM48
+        2.35E+01 -4.75E+01  8.09E+01  2.93E+01 -1.54E+01 -1.98E+01 -1.19E+01  1.23E+01  7.08E+00  1.59E+01 -9.63E+01 -7.93E+02
         -9.39E+01  2.19E+01 -2.40E+02  8.83E+01  9.99E+01  1.76E+01 -9.80E+02 -1.16E+01  8.02E+01 -3.64E+02 -9.52E+00 -1.53E+02
        -3.89E+02  3.87E+01  2.99E+02 -2.73E+02  2.75E+02 -2.61E+02  4.59E+02  6.26E+02 -8.35E+02  2.74E+03
 
 OM55
+        1.79E+01 -2.05E+01 -1.30E+01 -2.62E+00  1.13E+00  1.42E+01 -4.86E+00 -5.49E+00 -5.07E+00  2.05E+00 -4.45E+01  5.81E+01
         -3.87E+02 -6.98E+01  3.97E+01 -1.24E+02  1.52E+01  8.07E+01  5.84E+01 -5.43E+02 -1.08E+02  9.89E+01 -1.52E+02 -6.28E+00
         5.82E+01 -9.91E+01  7.57E+01  1.28E+02 -4.26E+01  4.58E-01  7.00E+01 -1.55E+01  2.59E+01 -5.06E+01  1.08E+03
 
 OM56
+        3.95E+01  2.63E+01 -1.09E+01 -2.26E+01  6.44E+00  1.56E+01  5.52E+00 -1.34E+01 -3.37E+01  8.89E+01  3.30E+01 -4.50E-01
         -3.25E+02 -4.25E+02 -1.20E+01 -1.16E+02  1.92E+02  3.11E+02  4.00E+00 -7.34E+02 -7.38E+02  1.30E+02 -4.85E+02  3.32E+01
         3.21E+01 -4.48E+02 -3.14E+02  1.48E+02 -2.85E+02  7.87E+00 -1.61E+02 -3.40E+00  8.09E+01  9.97E-01  5.01E+02  2.33E+03
 
 OM57
+        4.60E+00  6.21E+01  2.86E+01 -1.64E+01  5.24E+01  3.86E+01  2.09E+01 -1.70E+00  6.50E+01  2.17E+01  2.15E+02 -1.59E+02
          3.47E+02  6.50E+01 -4.17E+02  2.98E+01 -7.57E+01 -6.69E+01 -2.55E+02  6.43E+02  1.80E+02 -5.43E+02  3.55E+02  8.70E+01
         5.61E+01  4.21E+00  1.00E+02 -2.10E+02  2.01E+01  5.70E+00  5.42E+02  6.17E+01 -8.98E+01  1.07E+02 -4.29E+02 -5.00E+02
          2.47E+03
 
 OM58
+        3.55E+01 -1.54E+01 -3.46E+01 -3.42E+01  4.98E-01  1.32E+01 -2.78E+00  2.49E+01  1.90E+01  2.94E+02  3.51E+01  1.15E+02
         -1.16E+03 -2.64E+02  3.80E+02 -6.31E+02  4.44E+02  2.22E+02  1.04E+02 -1.69E+03 -6.50E+02  5.19E+02 -1.13E+03 -1.19E+02
         2.91E+01 -7.15E+02 -1.81E+02  5.65E+01 -1.55E+02  3.30E+01 -3.07E+02 -7.73E+01  1.52E+02  7.45E+01  5.81E+02  1.02E+03
         -1.20E+03  3.83E+03
 
 OM66
+        2.10E+01  2.90E+01  1.51E+01 -9.96E+00 -2.09E+01  5.79E+00  1.21E+01 -3.28E+01  1.94E+01 -1.36E+01  5.45E+00  6.97E+01
         -8.43E+01 -1.96E+02  3.18E+01 -6.41E+01  9.59E+01  1.69E+02 -7.72E+00 -1.99E+02 -3.64E+02  4.12E+01 -1.67E+02  1.01E+02
         7.45E+01 -1.00E+02 -2.51E+02  3.45E+01 -1.05E+02  1.41E+01 -4.27E+01 -1.13E+02  5.39E+01 -6.83E+00  3.86E+01  4.53E+02
         -6.26E+01  2.38E+02  6.03E+02
 
 OM67
+        2.08E+01  8.86E+00  3.14E+01  4.72E+00  1.72E+01  3.60E+01  2.61E+01  1.61E+01  1.78E+01 -1.37E+01  1.74E+01 -7.41E+01
          1.70E+02  2.33E+02 -1.64E+02  1.27E+02 -1.40E+02 -5.12E+01 -1.28E+02  3.33E+02  7.13E+02 -3.45E+02  3.50E+02  2.16E+01
        -1.23E+02  2.26E+02  6.71E+01 -1.93E+02  7.99E+01 -8.86E+00  4.47E+01  3.69E+02 -1.64E+02  1.58E+02 -1.06E+02 -4.84E+02
          5.09E+02 -3.73E+02 -2.79E+02  1.78E+03
 
 OM68
+        3.36E+01  4.98E+01  2.91E+01 -4.48E+01 -2.29E+01 -2.82E+01  4.26E+01 -5.53E+01  2.88E+01  2.03E+02  2.15E+02  2.18E+02
         -1.99E+02 -8.21E+02  2.43E+02 -4.91E+02  4.19E+02  2.52E+02 -1.51E+01 -7.21E+02 -1.33E+03  2.98E+02 -9.24E+02  1.41E+02
         1.79E+02 -3.43E+02 -6.77E+02  2.98E+01 -5.73E+02  1.22E+01 -7.98E+01 -4.22E+02  1.02E+02 -3.56E+02  8.66E+01  7.87E+02
         -2.63E+02  9.58E+02  6.62E+02 -8.89E+02  2.92E+03
 
 OM77
+       -1.91E+01 -6.15E+01 -2.63E+01  1.16E+00  2.83E+01  2.16E+01 -8.07E+00  2.12E+00  1.68E+01  7.89E+01 -9.31E+01  1.04E+02
         -1.08E+02 -1.50E+01  3.04E+02 -8.78E+01  1.08E+02 -9.04E+01  1.66E+02 -1.05E+02 -8.62E+01  6.53E+02 -3.37E+02 -1.34E+00
         4.73E+01 -1.79E+01 -4.56E+00 -2.19E+01  2.90E+01  1.40E+00 -6.67E+01 -6.53E+01  4.24E+02 -1.90E+02  6.03E+01  1.12E+02
         -3.23E+02  1.98E+02  2.10E+01 -2.57E+02  1.25E+02  9.03E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        4.10E+01  1.28E+02  1.06E+02  2.50E+01 -2.51E+00 -4.16E+00  1.16E+01 -1.05E+01 -4.17E+01 -2.69E+02 -9.36E+01 -4.21E+02
          1.79E+02  5.67E+01 -9.43E+02  6.19E+02 -3.43E+02  1.61E+02 -4.20E+02  3.12E+02  3.54E+02 -1.34E+03  1.37E+03  1.34E+02
        -2.20E+02  1.68E+02  7.55E+01 -4.38E+02  2.08E+02 -3.10E+01  2.75E+02  2.24E+02 -6.17E+02  8.78E+02 -1.21E+02 -2.99E+02
          6.16E+02 -8.52E+02 -8.83E+01  7.31E+02 -7.13E+02 -9.05E+02  3.34E+03
 
 OM88
+       -1.14E+00  1.34E+01  9.35E+00 -4.02E+01 -4.62E+01 -1.98E+01  2.47E+01 -3.68E+01  8.81E+01  3.79E+02  2.56E+02  2.19E+02
         -1.19E+02 -1.70E+02  3.65E+02 -1.11E+03  5.00E+02  9.20E+01  1.51E+02 -4.29E+02 -5.01E+02  5.42E+02 -1.66E+03 -4.19E+01
         7.65E+01 -2.39E+02 -4.63E+01  1.71E+02 -6.02E+02  4.89E+01 -9.99E+01 -9.90E+01  1.90E+02 -5.62E+02  8.59E+01  2.72E+02
         -1.95E+02  7.88E+02  1.18E+02 -2.83E+02  9.86E+02  1.79E+02 -1.28E+03  1.90E+03
 
 SG11
+       -1.78E+01  3.70E+02  3.48E+02 -2.06E+02 -8.58E+02 -8.82E+02 -5.55E+02 -3.06E+02 -1.00E+03  1.99E+03 -2.38E+03 -4.30E+03
          3.21E+03  3.32E+02 -7.82E+02  1.22E+02  5.17E+03  2.37E+02  6.40E+01 -3.04E+03  6.96E+02  3.02E+03 -6.25E+03  3.97E+03
         1.88E+03  1.41E+03  5.27E+03 -2.38E+02  3.72E+03  3.23E+02 -3.79E+02 -2.07E+03  2.81E+03  1.48E+03 -5.41E+02 -4.15E+03
         -4.84E+01 -5.76E+02 -1.23E+03 -1.42E+03 -2.10E+02  1.01E+03  2.62E+03  1.67E+02  2.37E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -1.47E+02  1.98E+02  9.00E+02 -9.82E+01  1.77E+02  1.94E+02  3.39E+01  5.40E+02  7.10E+02  1.33E+03 -1.08E+03  8.51E+02
          1.36E+03  7.93E+02 -1.89E+02 -8.37E+02  3.79E+02 -2.16E+02  1.82E+03  1.43E+02 -1.70E+03 -6.21E+02 -6.88E+02  1.20E+03
        -6.57E+02 -1.52E+02 -1.17E+03 -8.93E+02  1.61E+03 -1.43E+02  1.12E+03  6.63E+02 -6.31E+02 -9.17E+02  7.10E+02  3.51E+03
          1.14E+03 -1.39E+03  1.99E+03 -4.69E+02  1.27E+03 -1.47E+03  3.24E+03 -1.50E+03  4.71E+04  0.00E+00  7.61E+05
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     4035.886
Stop Time: 
Thu 06/18/2015 
08:31 AM

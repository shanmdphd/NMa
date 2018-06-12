Wed 06/17/2015 
03:13 PM
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
$EST METHOD=NUTS INTERACTION  NBURN=1000 NITER=2000 PRINT=1 MASSRESET=0 ; OLKJDF=4.0
$COV MATRIX=R UNCONDITIONAL

  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       17 JUN 2015
Days until program expires :5460
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
 RAW OUTPUT FILE (FILE): example6hmt.ext
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
 Elapsed estimation  time in seconds:    49.69
 Elapsed covariance  time in seconds:     0.19
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
 RAW OUTPUT FILE (FILE): example6hmt.ext
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
 Elapsed estimation  time in seconds:   571.85
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
 RAW OUTPUT FILE (FILE): example6hmt.ext
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
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 0.00000000000000
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
 iteration        -1000 MCMCOBJ=   -6461.68629031114     
 iteration         -999 MCMCOBJ=   -6461.68629039103     
 iteration         -998 MCMCOBJ=   -6495.30764753339     
 iteration         -997 MCMCOBJ=   -6310.33834290111     
 iteration         -996 MCMCOBJ=   -6489.00837879099     
 iteration         -995 MCMCOBJ=   -6584.99882112482     
 iteration         -994 MCMCOBJ=   -6622.80489431976     
 iteration         -993 MCMCOBJ=   -6679.69616123997     
 iteration         -992 MCMCOBJ=   -6564.96700814206     
 iteration         -991 MCMCOBJ=   -6505.85129269847     
 iteration         -990 MCMCOBJ=   -6515.11685050056     
 iteration         -989 MCMCOBJ=   -6432.08036817553     
 iteration         -988 MCMCOBJ=   -6529.23699233402     
 iteration         -987 MCMCOBJ=   -6443.96926207435     
 iteration         -986 MCMCOBJ=   -6400.55475846922     
 iteration         -985 MCMCOBJ=   -6555.03862811539     
 iteration         -984 MCMCOBJ=   -6513.37171992300     
 iteration         -983 MCMCOBJ=   -6451.04753504249     
 iteration         -982 MCMCOBJ=   -6351.40193690463     
 iteration         -981 MCMCOBJ=   -6411.33672988725     
 iteration         -980 MCMCOBJ=   -6432.79867198428     
 iteration         -979 MCMCOBJ=   -6492.02272976320     
 iteration         -978 MCMCOBJ=   -6449.96257269788     
 iteration         -977 MCMCOBJ=   -6450.70913915329     
 iteration         -976 MCMCOBJ=   -6396.52901916562     
 iteration         -975 MCMCOBJ=   -6450.21173402387     
 iteration         -974 MCMCOBJ=   -6450.47455958441     
 iteration         -973 MCMCOBJ=   -6411.65352686329     
 iteration         -972 MCMCOBJ=   -6374.19217417996     
 iteration         -971 MCMCOBJ=   -6505.95933916055     
 iteration         -970 MCMCOBJ=   -6409.01720913776     
 iteration         -969 MCMCOBJ=   -6409.01720928903     
 iteration         -968 MCMCOBJ=   -6372.97161589763     
 iteration         -967 MCMCOBJ=   -6403.31988779926     
 iteration         -966 MCMCOBJ=   -6380.69741543637     
 iteration         -965 MCMCOBJ=   -6393.37398652204     
 iteration         -964 MCMCOBJ=   -6547.96992782010     
 iteration         -963 MCMCOBJ=   -6498.85246943323     
 iteration         -962 MCMCOBJ=   -6477.93321604056     
 iteration         -961 MCMCOBJ=   -6541.64241945440     
 iteration         -960 MCMCOBJ=   -6562.42992052825     
 iteration         -959 MCMCOBJ=   -6452.68060688298     
 iteration         -958 MCMCOBJ=   -6417.65261269699     
 iteration         -957 MCMCOBJ=   -6400.23897895710     
 iteration         -956 MCMCOBJ=   -6374.42110903562     
 iteration         -955 MCMCOBJ=   -6342.87438686488     
 iteration         -954 MCMCOBJ=   -6439.89685402734     
 iteration         -953 MCMCOBJ=   -6497.31993290535     
 iteration         -952 MCMCOBJ=   -6602.68095716564     
 iteration         -951 MCMCOBJ=   -6576.82726014710     
 iteration         -950 MCMCOBJ=   -6505.51664288198     
 iteration         -949 MCMCOBJ=   -6499.57191709032     
 iteration         -948 MCMCOBJ=   -6421.36660935817     
 iteration         -947 MCMCOBJ=   -6478.15935516613     
 iteration         -946 MCMCOBJ=   -6532.48279598617     
 iteration         -945 MCMCOBJ=   -6528.96285966822     
 iteration         -944 MCMCOBJ=   -6477.58138168855     
 iteration         -943 MCMCOBJ=   -6469.81504677768     
 iteration         -942 MCMCOBJ=   -6533.36640491550     
 iteration         -941 MCMCOBJ=   -6525.59601978129     
 iteration         -940 MCMCOBJ=   -6530.02662391306     
 iteration         -939 MCMCOBJ=   -6471.90393194129     
 iteration         -938 MCMCOBJ=   -6406.97259616764     
 iteration         -937 MCMCOBJ=   -6416.44202103706     
 iteration         -936 MCMCOBJ=   -6477.97108536441     
 iteration         -935 MCMCOBJ=   -6530.86642022121     
 iteration         -934 MCMCOBJ=   -6486.90673034802     
 iteration         -933 MCMCOBJ=   -6428.26755667586     
 iteration         -932 MCMCOBJ=   -6492.79517443612     
 iteration         -931 MCMCOBJ=   -6585.16042013702     
 iteration         -930 MCMCOBJ=   -6557.96192695210     
 iteration         -929 MCMCOBJ=   -6518.78857680496     
 iteration         -928 MCMCOBJ=   -6441.92255595292     
 iteration         -927 MCMCOBJ=   -6519.92436769288     
 iteration         -926 MCMCOBJ=   -6522.29907283359     
 iteration         -925 MCMCOBJ=   -6427.92798251618     
 iteration         -924 MCMCOBJ=   -6532.30514980004     
 iteration         -923 MCMCOBJ=   -6510.51590272227     
 iteration         -922 MCMCOBJ=   -6561.46955847278     
 iteration         -921 MCMCOBJ=   -6497.89407417800     
 iteration         -920 MCMCOBJ=   -6497.89407778775     
 iteration         -919 MCMCOBJ=   -6496.64754240514     
 iteration         -918 MCMCOBJ=   -6554.89642977658     
 iteration         -917 MCMCOBJ=   -6582.86612216582     
 iteration         -916 MCMCOBJ=   -6535.62052670794     
 iteration         -915 MCMCOBJ=   -6513.06065203850     
 iteration         -914 MCMCOBJ=   -6508.36512480228     
 iteration         -913 MCMCOBJ=   -6530.19145640388     
 iteration         -912 MCMCOBJ=   -6530.19145776836     
 iteration         -911 MCMCOBJ=   -6457.85866935910     
 iteration         -910 MCMCOBJ=   -6404.35091278193     
 iteration         -909 MCMCOBJ=   -6486.50745872732     
 iteration         -908 MCMCOBJ=   -6549.17733328698     
 iteration         -907 MCMCOBJ=   -6531.13977156117     
 iteration         -906 MCMCOBJ=   -6512.03339947538     
 iteration         -905 MCMCOBJ=   -6435.34186176743     
 iteration         -904 MCMCOBJ=   -6545.19352202657     
 iteration         -903 MCMCOBJ=   -6545.19352218494     
 iteration         -902 MCMCOBJ=   -6558.37798188483     
 iteration         -901 MCMCOBJ=   -6566.85169201469     
 iteration         -900 MCMCOBJ=   -6468.97404586309     
 iteration         -899 MCMCOBJ=   -6412.72920834727     
 iteration         -898 MCMCOBJ=   -6414.47452144567     
 iteration         -897 MCMCOBJ=   -6504.22714265802     
 iteration         -896 MCMCOBJ=   -6548.06587664636     
 iteration         -895 MCMCOBJ=   -6462.90172430359     
 iteration         -894 MCMCOBJ=   -6518.56639198426     
 iteration         -893 MCMCOBJ=   -6483.89438250857     
 iteration         -892 MCMCOBJ=   -6477.22734002339     
 iteration         -891 MCMCOBJ=   -6472.09408997318     
 iteration         -890 MCMCOBJ=   -6472.09409033668     
 iteration         -889 MCMCOBJ=   -6566.76418468804     
 iteration         -888 MCMCOBJ=   -6517.89827610145     
 iteration         -887 MCMCOBJ=   -6477.17419100009     
 iteration         -886 MCMCOBJ=   -6492.44946691293     
 iteration         -885 MCMCOBJ=   -6450.41770980023     
 iteration         -884 MCMCOBJ=   -6445.36366229352     
 iteration         -883 MCMCOBJ=   -6399.02200821908     
 iteration         -882 MCMCOBJ=   -6437.66794413912     
 iteration         -881 MCMCOBJ=   -6442.05807916235     
 iteration         -880 MCMCOBJ=   -6434.04416066035     
 iteration         -879 MCMCOBJ=   -6414.96083753322     
 iteration         -878 MCMCOBJ=   -6502.34189424037     
 iteration         -877 MCMCOBJ=   -6514.18297458172     
 iteration         -876 MCMCOBJ=   -6469.74823499030     
 iteration         -875 MCMCOBJ=   -6462.87524472586     
 iteration         -874 MCMCOBJ=   -6483.58794848088     
 iteration         -873 MCMCOBJ=   -6483.58794506635     
 iteration         -872 MCMCOBJ=   -6563.60861953918     
 iteration         -871 MCMCOBJ=   -6494.80332617024     
 iteration         -870 MCMCOBJ=   -6437.75966736146     
 iteration         -869 MCMCOBJ=   -6474.80806757723     
 iteration         -868 MCMCOBJ=   -6550.41104684087     
 iteration         -867 MCMCOBJ=   -6590.21557190265     
 iteration         -866 MCMCOBJ=   -6508.96910261542     
 iteration         -865 MCMCOBJ=   -6609.20271727981     
 iteration         -864 MCMCOBJ=   -6563.22385349054     
 iteration         -863 MCMCOBJ=   -6544.94051735901     
 iteration         -862 MCMCOBJ=   -6478.69459065014     
 iteration         -861 MCMCOBJ=   -6484.77021181721     
 iteration         -860 MCMCOBJ=   -6511.06211506375     
 iteration         -859 MCMCOBJ=   -6507.56204071819     
 iteration         -858 MCMCOBJ=   -6439.81944751980     
 iteration         -857 MCMCOBJ=   -6474.08103967642     
 iteration         -856 MCMCOBJ=   -6542.75976321788     
 iteration         -855 MCMCOBJ=   -6570.81241509736     
 iteration         -854 MCMCOBJ=   -6539.03759764066     
 iteration         -853 MCMCOBJ=   -6508.87333176049     
 iteration         -852 MCMCOBJ=   -6534.79703134383     
 iteration         -851 MCMCOBJ=   -6595.13374526066     
 iteration         -850 MCMCOBJ=   -6506.08607743520     
 iteration         -849 MCMCOBJ=   -6516.78493193803     
 iteration         -848 MCMCOBJ=   -6520.17834817909     
 iteration         -847 MCMCOBJ=   -6435.89860216621     
 iteration         -846 MCMCOBJ=   -6415.33821896915     
 iteration         -845 MCMCOBJ=   -6440.03823224295     
 iteration         -844 MCMCOBJ=   -6409.46500354931     
 iteration         -843 MCMCOBJ=   -6364.92942162648     
 iteration         -842 MCMCOBJ=   -6481.54688789818     
 iteration         -841 MCMCOBJ=   -6493.29571165208     
 iteration         -840 MCMCOBJ=   -6512.93727757526     
 iteration         -839 MCMCOBJ=   -6474.29361711890     
 iteration         -838 MCMCOBJ=   -6481.62510609476     
 iteration         -837 MCMCOBJ=   -6481.62510905956     
 iteration         -836 MCMCOBJ=   -6453.37723470618     
 iteration         -835 MCMCOBJ=   -6461.33544827865     
 iteration         -834 MCMCOBJ=   -6492.49985767178     
 iteration         -833 MCMCOBJ=   -6472.11349512888     
 iteration         -832 MCMCOBJ=   -6489.68052199825     
 iteration         -831 MCMCOBJ=   -6452.61799478502     
 iteration         -830 MCMCOBJ=   -6441.28221759689     
 iteration         -829 MCMCOBJ=   -6548.65503288302     
 iteration         -828 MCMCOBJ=   -6455.52647888823     
 iteration         -827 MCMCOBJ=   -6463.41476643227     
 iteration         -826 MCMCOBJ=   -6415.11206205423     
 iteration         -825 MCMCOBJ=   -6436.99555234814     
 iteration         -824 MCMCOBJ=   -6432.26704972437     
 iteration         -823 MCMCOBJ=   -6476.28857966120     
 iteration         -822 MCMCOBJ=   -6441.72324564789     
 iteration         -821 MCMCOBJ=   -6496.26117023811     
 iteration         -820 MCMCOBJ=   -6438.00099647499     
 iteration         -819 MCMCOBJ=   -6481.41869103710     
 iteration         -818 MCMCOBJ=   -6421.82165384459     
 iteration         -817 MCMCOBJ=   -6453.75324420957     
 iteration         -816 MCMCOBJ=   -6505.60403601995     
 iteration         -815 MCMCOBJ=   -6473.42440050408     
 iteration         -814 MCMCOBJ=   -6441.68040196113     
 iteration         -813 MCMCOBJ=   -6488.98893946227     
 iteration         -812 MCMCOBJ=   -6482.47710400064     
 iteration         -811 MCMCOBJ=   -6527.15923279853     
 iteration         -810 MCMCOBJ=   -6537.73641572285     
 iteration         -809 MCMCOBJ=   -6537.44132793176     
 iteration         -808 MCMCOBJ=   -6617.86979055641     
 iteration         -807 MCMCOBJ=   -6594.61367411167     
 iteration         -806 MCMCOBJ=   -6498.80252060822     
 iteration         -805 MCMCOBJ=   -6452.80303542474     
 iteration         -804 MCMCOBJ=   -6486.44762389401     
 iteration         -803 MCMCOBJ=   -6417.32848042486     
 iteration         -802 MCMCOBJ=   -6443.98769243517     
 iteration         -801 MCMCOBJ=   -6500.02357484303     
 iteration         -800 MCMCOBJ=   -6520.27264882271     
 iteration         -799 MCMCOBJ=   -6475.92542446494     
 iteration         -798 MCMCOBJ=   -6392.52341681429     
 iteration         -797 MCMCOBJ=   -6412.16407472526     
 iteration         -796 MCMCOBJ=   -6438.90665108690     
 iteration         -795 MCMCOBJ=   -6480.81867740198     
 iteration         -794 MCMCOBJ=   -6436.36743953879     
 iteration         -793 MCMCOBJ=   -6449.25193992122     
 iteration         -792 MCMCOBJ=   -6449.25194158133     
 iteration         -791 MCMCOBJ=   -6426.53125968453     
 iteration         -790 MCMCOBJ=   -6430.07991474594     
 iteration         -789 MCMCOBJ=   -6510.42910843470     
 iteration         -788 MCMCOBJ=   -6495.98685581028     
 iteration         -787 MCMCOBJ=   -6509.01048783774     
 iteration         -786 MCMCOBJ=   -6516.34987970617     
 iteration         -785 MCMCOBJ=   -6463.04915164869     
 iteration         -784 MCMCOBJ=   -6423.29519225349     
 iteration         -783 MCMCOBJ=   -6504.02711102127     
 iteration         -782 MCMCOBJ=   -6552.35852772263     
 iteration         -781 MCMCOBJ=   -6482.24135421913     
 iteration         -780 MCMCOBJ=   -6474.72093940227     
 iteration         -779 MCMCOBJ=   -6539.13086972201     
 iteration         -778 MCMCOBJ=   -6447.03801088460     
 iteration         -777 MCMCOBJ=   -6427.33635794505     
 iteration         -776 MCMCOBJ=   -6440.26243713373     
 iteration         -775 MCMCOBJ=   -6493.23152427956     
 iteration         -774 MCMCOBJ=   -6507.60686233900     
 iteration         -773 MCMCOBJ=   -6538.32930853662     
 iteration         -772 MCMCOBJ=   -6512.09642283272     
 iteration         -771 MCMCOBJ=   -6446.67075407826     
 iteration         -770 MCMCOBJ=   -6455.95499557768     
 iteration         -769 MCMCOBJ=   -6454.08256577886     
 iteration         -768 MCMCOBJ=   -6523.63101319858     
 iteration         -767 MCMCOBJ=   -6465.71456632588     
 iteration         -766 MCMCOBJ=   -6456.03951403041     
 iteration         -765 MCMCOBJ=   -6460.44267963796     
 iteration         -764 MCMCOBJ=   -6452.82932487445     
 iteration         -763 MCMCOBJ=   -6480.38660469965     
 iteration         -762 MCMCOBJ=   -6529.56920476561     
 iteration         -761 MCMCOBJ=   -6459.84122283574     
 iteration         -760 MCMCOBJ=   -6448.04268544524     
 iteration         -759 MCMCOBJ=   -6523.44941685654     
 iteration         -758 MCMCOBJ=   -6417.18674667817     
 iteration         -757 MCMCOBJ=   -6457.22414887358     
 iteration         -756 MCMCOBJ=   -6555.76399212423     
 iteration         -755 MCMCOBJ=   -6530.85771003370     
 iteration         -754 MCMCOBJ=   -6511.00217814330     
 iteration         -753 MCMCOBJ=   -6448.78230756379     
 iteration         -752 MCMCOBJ=   -6462.75158545455     
 iteration         -751 MCMCOBJ=   -6467.39334047931     
 iteration         -750 MCMCOBJ=   -6369.67137339228     
 iteration         -749 MCMCOBJ=   -6494.39163183999     
 iteration         -748 MCMCOBJ=   -6521.94033736541     
 iteration         -747 MCMCOBJ=   -6445.35082538357     
 iteration         -746 MCMCOBJ=   -6413.18644856376     
 iteration         -745 MCMCOBJ=   -6459.01194593764     
 iteration         -744 MCMCOBJ=   -6465.24178686497     
 iteration         -743 MCMCOBJ=   -6476.44555933480     
 iteration         -742 MCMCOBJ=   -6472.82713391238     
 iteration         -741 MCMCOBJ=   -6568.71416274543     
 iteration         -740 MCMCOBJ=   -6536.21139930596     
 iteration         -739 MCMCOBJ=   -6440.75984842985     
 iteration         -738 MCMCOBJ=   -6425.05687788303     
 iteration         -737 MCMCOBJ=   -6447.20566769301     
 iteration         -736 MCMCOBJ=   -6493.25715624304     
 iteration         -735 MCMCOBJ=   -6474.71272902205     
 iteration         -734 MCMCOBJ=   -6436.90505955947     
 iteration         -733 MCMCOBJ=   -6474.53541515433     
 iteration         -732 MCMCOBJ=   -6466.27744703743     
 iteration         -731 MCMCOBJ=   -6423.68749434474     
 iteration         -730 MCMCOBJ=   -6433.06622185949     
 iteration         -729 MCMCOBJ=   -6402.76537349121     
 iteration         -728 MCMCOBJ=   -6454.95427139852     
 iteration         -727 MCMCOBJ=   -6485.19781941622     
 iteration         -726 MCMCOBJ=   -6445.23010641203     
 iteration         -725 MCMCOBJ=   -6506.38297192147     
 iteration         -724 MCMCOBJ=   -6521.98148822086     
 iteration         -723 MCMCOBJ=   -6524.58645452532     
 iteration         -722 MCMCOBJ=   -6463.18494055979     
 iteration         -721 MCMCOBJ=   -6497.49560251262     
 iteration         -720 MCMCOBJ=   -6465.36691463817     
 iteration         -719 MCMCOBJ=   -6404.07599316065     
 iteration         -718 MCMCOBJ=   -6434.57182883720     
 iteration         -717 MCMCOBJ=   -6519.21978306447     
 iteration         -716 MCMCOBJ=   -6466.08220809900     
 iteration         -715 MCMCOBJ=   -6507.57088178269     
 iteration         -714 MCMCOBJ=   -6472.72782706978     
 iteration         -713 MCMCOBJ=   -6539.71798356291     
 iteration         -712 MCMCOBJ=   -6541.13360699571     
 iteration         -711 MCMCOBJ=   -6531.56789019313     
 iteration         -710 MCMCOBJ=   -6467.53156016332     
 iteration         -709 MCMCOBJ=   -6544.72271776488     
 iteration         -708 MCMCOBJ=   -6519.99248329709     
 iteration         -707 MCMCOBJ=   -6504.55826922879     
 iteration         -706 MCMCOBJ=   -6462.78975732431     
 iteration         -705 MCMCOBJ=   -6532.54669905999     
 iteration         -704 MCMCOBJ=   -6495.29076129859     
 iteration         -703 MCMCOBJ=   -6534.72949970914     
 iteration         -702 MCMCOBJ=   -6457.54237803022     
 iteration         -701 MCMCOBJ=   -6536.56156839283     
 iteration         -700 MCMCOBJ=   -6544.49936140535     
 iteration         -699 MCMCOBJ=   -6549.47539431362     
 iteration         -698 MCMCOBJ=   -6565.07006108362     
 iteration         -697 MCMCOBJ=   -6503.07813198498     
 iteration         -696 MCMCOBJ=   -6502.60241550867     
 iteration         -695 MCMCOBJ=   -6535.30384647317     
 iteration         -694 MCMCOBJ=   -6556.67365140273     
 iteration         -693 MCMCOBJ=   -6587.82036495771     
 iteration         -692 MCMCOBJ=   -6491.24817692236     
 iteration         -691 MCMCOBJ=   -6440.87156655834     
 iteration         -690 MCMCOBJ=   -6433.56716258618     
 iteration         -689 MCMCOBJ=   -6427.36256523795     
 iteration         -688 MCMCOBJ=   -6444.63495132300     
 iteration         -687 MCMCOBJ=   -6397.62684076189     
 iteration         -686 MCMCOBJ=   -6465.28596306611     
 iteration         -685 MCMCOBJ=   -6443.92461358872     
 iteration         -684 MCMCOBJ=   -6470.24313475895     
 iteration         -683 MCMCOBJ=   -6513.94609323302     
 iteration         -682 MCMCOBJ=   -6520.79946140033     
 iteration         -681 MCMCOBJ=   -6539.59368911234     
 iteration         -680 MCMCOBJ=   -6488.36004368610     
 iteration         -679 MCMCOBJ=   -6459.41565560705     
 iteration         -678 MCMCOBJ=   -6435.26118885163     
 iteration         -677 MCMCOBJ=   -6458.37935364544     
 iteration         -676 MCMCOBJ=   -6531.38589523120     
 iteration         -675 MCMCOBJ=   -6520.00531512147     
 iteration         -674 MCMCOBJ=   -6499.38193121998     
 iteration         -673 MCMCOBJ=   -6515.63613732815     
 iteration         -672 MCMCOBJ=   -6538.28346184438     
 iteration         -671 MCMCOBJ=   -6450.97030565318     
 iteration         -670 MCMCOBJ=   -6489.37521403951     
 iteration         -669 MCMCOBJ=   -6529.21077568657     
 iteration         -668 MCMCOBJ=   -6544.87989700281     
 iteration         -667 MCMCOBJ=   -6515.99314794894     
 iteration         -666 MCMCOBJ=   -6531.82441698596     
 iteration         -665 MCMCOBJ=   -6493.93057547756     
 iteration         -664 MCMCOBJ=   -6543.74519637381     
 iteration         -663 MCMCOBJ=   -6479.81384764343     
 iteration         -662 MCMCOBJ=   -6509.34095416552     
 iteration         -661 MCMCOBJ=   -6520.88698774147     
 iteration         -660 MCMCOBJ=   -6466.29665360557     
 iteration         -659 MCMCOBJ=   -6466.47583188311     
 iteration         -658 MCMCOBJ=   -6472.46305386206     
 iteration         -657 MCMCOBJ=   -6420.60221504017     
 iteration         -656 MCMCOBJ=   -6497.45990648863     
 iteration         -655 MCMCOBJ=   -6522.91118264714     
 iteration         -654 MCMCOBJ=   -6519.54269176256     
 iteration         -653 MCMCOBJ=   -6502.75048685104     
 iteration         -652 MCMCOBJ=   -6498.08524377833     
 iteration         -651 MCMCOBJ=   -6485.88404289384     
 iteration         -650 MCMCOBJ=   -6493.05807620922     
 iteration         -649 MCMCOBJ=   -6523.76854427018     
 iteration         -648 MCMCOBJ=   -6454.87189422311     
 iteration         -647 MCMCOBJ=   -6489.11491056144     
 iteration         -646 MCMCOBJ=   -6515.89786303114     
 iteration         -645 MCMCOBJ=   -6468.89331559379     
 iteration         -644 MCMCOBJ=   -6420.47358818639     
 iteration         -643 MCMCOBJ=   -6471.60508109526     
 iteration         -642 MCMCOBJ=   -6474.90880666578     
 iteration         -641 MCMCOBJ=   -6445.26307797328     
 iteration         -640 MCMCOBJ=   -6532.21388395424     
 iteration         -639 MCMCOBJ=   -6552.90859134029     
 iteration         -638 MCMCOBJ=   -6522.53749214411     
 iteration         -637 MCMCOBJ=   -6493.72141284896     
 iteration         -636 MCMCOBJ=   -6465.64608900340     
 iteration         -635 MCMCOBJ=   -6495.45631909423     
 iteration         -634 MCMCOBJ=   -6495.45632230905     
 iteration         -633 MCMCOBJ=   -6510.38753926831     
 iteration         -632 MCMCOBJ=   -6505.13193063953     
 iteration         -631 MCMCOBJ=   -6465.04423146914     
 iteration         -630 MCMCOBJ=   -6531.37481353714     
 iteration         -629 MCMCOBJ=   -6514.92202089472     
 iteration         -628 MCMCOBJ=   -6522.15464315601     
 iteration         -627 MCMCOBJ=   -6526.19354860977     
 iteration         -626 MCMCOBJ=   -6549.41823394415     
 iteration         -625 MCMCOBJ=   -6482.36450799429     
 iteration         -624 MCMCOBJ=   -6435.87780343883     
 iteration         -623 MCMCOBJ=   -6441.94030398599     
 iteration         -622 MCMCOBJ=   -6500.07474817912     
 iteration         -621 MCMCOBJ=   -6495.09550314470     
 iteration         -620 MCMCOBJ=   -6480.19747544262     
 iteration         -619 MCMCOBJ=   -6464.22416226350     
 iteration         -618 MCMCOBJ=   -6450.84601212449     
 iteration         -617 MCMCOBJ=   -6483.07257032449     
 iteration         -616 MCMCOBJ=   -6517.13293637341     
 iteration         -615 MCMCOBJ=   -6508.20345603909     
 iteration         -614 MCMCOBJ=   -6457.18965828817     
 iteration         -613 MCMCOBJ=   -6536.44593066061     
 iteration         -612 MCMCOBJ=   -6499.53221069097     
 iteration         -611 MCMCOBJ=   -6507.38209894148     
 iteration         -610 MCMCOBJ=   -6506.89914306909     
 iteration         -609 MCMCOBJ=   -6518.02830143513     
 iteration         -608 MCMCOBJ=   -6517.43744369543     
 iteration         -607 MCMCOBJ=   -6491.54073629774     
 iteration         -606 MCMCOBJ=   -6463.26846167799     
 iteration         -605 MCMCOBJ=   -6425.67297668508     
 iteration         -604 MCMCOBJ=   -6428.55222154713     
 iteration         -603 MCMCOBJ=   -6491.41047545570     
 iteration         -602 MCMCOBJ=   -6489.65101199116     
 iteration         -601 MCMCOBJ=   -6429.21180712582     
 iteration         -600 MCMCOBJ=   -6467.58332597398     
 iteration         -599 MCMCOBJ=   -6502.95511206757     
 iteration         -598 MCMCOBJ=   -6465.99869752021     
 iteration         -597 MCMCOBJ=   -6487.85720458327     
 iteration         -596 MCMCOBJ=   -6415.50681032258     
 iteration         -595 MCMCOBJ=   -6492.23721054215     
 iteration         -594 MCMCOBJ=   -6457.45557655537     
 iteration         -593 MCMCOBJ=   -6415.31950927741     
 iteration         -592 MCMCOBJ=   -6446.26142103597     
 iteration         -591 MCMCOBJ=   -6520.32696343295     
 iteration         -590 MCMCOBJ=   -6469.21089337907     
 iteration         -589 MCMCOBJ=   -6504.51855259269     
 iteration         -588 MCMCOBJ=   -6491.65121242054     
 iteration         -587 MCMCOBJ=   -6517.74219690017     
 iteration         -586 MCMCOBJ=   -6457.43371658423     
 iteration         -585 MCMCOBJ=   -6439.72021369619     
 iteration         -584 MCMCOBJ=   -6430.02562932989     
 iteration         -583 MCMCOBJ=   -6477.04650968826     
 iteration         -582 MCMCOBJ=   -6469.53685214099     
 iteration         -581 MCMCOBJ=   -6523.00349356926     
 iteration         -580 MCMCOBJ=   -6458.47357304809     
 iteration         -579 MCMCOBJ=   -6433.90358020245     
 iteration         -578 MCMCOBJ=   -6442.95622202409     
 iteration         -577 MCMCOBJ=   -6491.69707697655     
 iteration         -576 MCMCOBJ=   -6499.06039362036     
 iteration         -575 MCMCOBJ=   -6438.80328554598     
 iteration         -574 MCMCOBJ=   -6434.06616856864     
 iteration         -573 MCMCOBJ=   -6434.06616847055     
 iteration         -572 MCMCOBJ=   -6466.81670412842     
 iteration         -571 MCMCOBJ=   -6400.31897805275     
 iteration         -570 MCMCOBJ=   -6444.02696292382     
 iteration         -569 MCMCOBJ=   -6495.70345254751     
 iteration         -568 MCMCOBJ=   -6442.14747014244     
 iteration         -567 MCMCOBJ=   -6432.88833163368     
 iteration         -566 MCMCOBJ=   -6500.67671699594     
 iteration         -565 MCMCOBJ=   -6507.10704219782     
 iteration         -564 MCMCOBJ=   -6517.73311896689     
 iteration         -563 MCMCOBJ=   -6532.84288653524     
 iteration         -562 MCMCOBJ=   -6477.79282097218     
 iteration         -561 MCMCOBJ=   -6390.77984798375     
 iteration         -560 MCMCOBJ=   -6486.86142044069     
 iteration         -559 MCMCOBJ=   -6520.89671708764     
 iteration         -558 MCMCOBJ=   -6548.47994345734     
 iteration         -557 MCMCOBJ=   -6486.60087703574     
 iteration         -556 MCMCOBJ=   -6486.57482704616     
 iteration         -555 MCMCOBJ=   -6454.30423561810     
 iteration         -554 MCMCOBJ=   -6437.62256705604     
 iteration         -553 MCMCOBJ=   -6456.72493266586     
 iteration         -552 MCMCOBJ=   -6479.54915169105     
 iteration         -551 MCMCOBJ=   -6508.15607684373     
 iteration         -550 MCMCOBJ=   -6521.18220579782     
 iteration         -549 MCMCOBJ=   -6496.74596261878     
 iteration         -548 MCMCOBJ=   -6576.11391102962     
 iteration         -547 MCMCOBJ=   -6521.09753146062     
 iteration         -546 MCMCOBJ=   -6419.99751860638     
 iteration         -545 MCMCOBJ=   -6461.48328846399     
 iteration         -544 MCMCOBJ=   -6430.31662247927     
 iteration         -543 MCMCOBJ=   -6447.89736959251     
 iteration         -542 MCMCOBJ=   -6514.15854574222     
 iteration         -541 MCMCOBJ=   -6491.17342380362     
 iteration         -540 MCMCOBJ=   -6427.11215946636     
 iteration         -539 MCMCOBJ=   -6470.79812047917     
 iteration         -538 MCMCOBJ=   -6418.10052451673     
 iteration         -537 MCMCOBJ=   -6389.11533698149     
 iteration         -536 MCMCOBJ=   -6472.42873132166     
 iteration         -535 MCMCOBJ=   -6404.74643332105     
 iteration         -534 MCMCOBJ=   -6391.78715418534     
 iteration         -533 MCMCOBJ=   -6464.63604271856     
 iteration         -532 MCMCOBJ=   -6474.15892990623     
 iteration         -531 MCMCOBJ=   -6541.93784698067     
 iteration         -530 MCMCOBJ=   -6532.22129834554     
 iteration         -529 MCMCOBJ=   -6546.74940686545     
 iteration         -528 MCMCOBJ=   -6537.95943233663     
 iteration         -527 MCMCOBJ=   -6525.61557228871     
 iteration         -526 MCMCOBJ=   -6452.93558946263     
 iteration         -525 MCMCOBJ=   -6428.42412713375     
 iteration         -524 MCMCOBJ=   -6424.11847594613     
 iteration         -523 MCMCOBJ=   -6467.42505193409     
 iteration         -522 MCMCOBJ=   -6433.91477637780     
 iteration         -521 MCMCOBJ=   -6477.35169606749     
 iteration         -520 MCMCOBJ=   -6455.43257016644     
 iteration         -519 MCMCOBJ=   -6453.61372507033     
 iteration         -518 MCMCOBJ=   -6420.47408560486     
 iteration         -517 MCMCOBJ=   -6400.39952907475     
 iteration         -516 MCMCOBJ=   -6475.45059221600     
 iteration         -515 MCMCOBJ=   -6453.46131966094     
 iteration         -514 MCMCOBJ=   -6499.38058752107     
 iteration         -513 MCMCOBJ=   -6510.54448988570     
 iteration         -512 MCMCOBJ=   -6486.24726078625     
 iteration         -511 MCMCOBJ=   -6517.96075089908     
 iteration         -510 MCMCOBJ=   -6528.77503586631     
 iteration         -509 MCMCOBJ=   -6492.70038571739     
 iteration         -508 MCMCOBJ=   -6501.72085850755     
 iteration         -507 MCMCOBJ=   -6516.03496844641     
 iteration         -506 MCMCOBJ=   -6527.22315811719     
 iteration         -505 MCMCOBJ=   -6494.11783465079     
 iteration         -504 MCMCOBJ=   -6450.68955910138     
 iteration         -503 MCMCOBJ=   -6468.74467632321     
 iteration         -502 MCMCOBJ=   -6478.52110620431     
 iteration         -501 MCMCOBJ=   -6479.96294279589     
 iteration         -500 MCMCOBJ=   -6476.49571177951     
 iteration         -499 MCMCOBJ=   -6449.34255631695     
 iteration         -498 MCMCOBJ=   -6488.44581729475     
 iteration         -497 MCMCOBJ=   -6479.36608592567     
 iteration         -496 MCMCOBJ=   -6505.14134601379     
 iteration         -495 MCMCOBJ=   -6458.36390624649     
 iteration         -494 MCMCOBJ=   -6499.18039699893     
 iteration         -493 MCMCOBJ=   -6564.15147455236     
 iteration         -492 MCMCOBJ=   -6477.13390393844     
 iteration         -491 MCMCOBJ=   -6497.96627775570     
 iteration         -490 MCMCOBJ=   -6470.72636675516     
 iteration         -489 MCMCOBJ=   -6487.10612379732     
 iteration         -488 MCMCOBJ=   -6460.10107852384     
 iteration         -487 MCMCOBJ=   -6476.22667439462     
 iteration         -486 MCMCOBJ=   -6502.03028482312     
 iteration         -485 MCMCOBJ=   -6502.96166724816     
 iteration         -484 MCMCOBJ=   -6537.70354967243     
 iteration         -483 MCMCOBJ=   -6496.62543101106     
 iteration         -482 MCMCOBJ=   -6456.42776136351     
 iteration         -481 MCMCOBJ=   -6487.71375303978     
 iteration         -480 MCMCOBJ=   -6475.05970514336     
 iteration         -479 MCMCOBJ=   -6445.20994423360     
 iteration         -478 MCMCOBJ=   -6450.18656854662     
 iteration         -477 MCMCOBJ=   -6456.93301389924     
 iteration         -476 MCMCOBJ=   -6493.89938032497     
 iteration         -475 MCMCOBJ=   -6428.99087694608     
 iteration         -474 MCMCOBJ=   -6449.11825705640     
 iteration         -473 MCMCOBJ=   -6490.91054707704     
 iteration         -472 MCMCOBJ=   -6456.76327513529     
 iteration         -471 MCMCOBJ=   -6503.24167612072     
 iteration         -470 MCMCOBJ=   -6489.34819900581     
 iteration         -469 MCMCOBJ=   -6538.64642091367     
 iteration         -468 MCMCOBJ=   -6545.38563836401     
 iteration         -467 MCMCOBJ=   -6514.31049948380     
 iteration         -466 MCMCOBJ=   -6529.68740374925     
 iteration         -465 MCMCOBJ=   -6561.47719255249     
 iteration         -464 MCMCOBJ=   -6554.29560197260     
 iteration         -463 MCMCOBJ=   -6562.14021958315     
 iteration         -462 MCMCOBJ=   -6500.66980916943     
 iteration         -461 MCMCOBJ=   -6523.49321183622     
 iteration         -460 MCMCOBJ=   -6520.17691962589     
 iteration         -459 MCMCOBJ=   -6486.66569664314     
 iteration         -458 MCMCOBJ=   -6454.79987726803     
 iteration         -457 MCMCOBJ=   -6522.67926165762     
 iteration         -456 MCMCOBJ=   -6517.86610728990     
 iteration         -455 MCMCOBJ=   -6508.47377309819     
 iteration         -454 MCMCOBJ=   -6548.98527136088     
 iteration         -453 MCMCOBJ=   -6499.33543454420     
 iteration         -452 MCMCOBJ=   -6487.29130637647     
 iteration         -451 MCMCOBJ=   -6525.33610669924     
 iteration         -450 MCMCOBJ=   -6443.18223631953     
 iteration         -449 MCMCOBJ=   -6472.66904767828     
 iteration         -448 MCMCOBJ=   -6547.49774947981     
 iteration         -447 MCMCOBJ=   -6547.49774974162     
 iteration         -446 MCMCOBJ=   -6525.88929991378     
 iteration         -445 MCMCOBJ=   -6481.59739280918     
 iteration         -444 MCMCOBJ=   -6482.56490511637     
 iteration         -443 MCMCOBJ=   -6513.29872636214     
 iteration         -442 MCMCOBJ=   -6579.69386742137     
 iteration         -441 MCMCOBJ=   -6578.18219920879     
 iteration         -440 MCMCOBJ=   -6554.56648431382     
 iteration         -439 MCMCOBJ=   -6579.44502962539     
 iteration         -438 MCMCOBJ=   -6566.41889645726     
 iteration         -437 MCMCOBJ=   -6487.24232663711     
 iteration         -436 MCMCOBJ=   -6505.98297976073     
 iteration         -435 MCMCOBJ=   -6499.74213658700     
 iteration         -434 MCMCOBJ=   -6530.43497484001     
 iteration         -433 MCMCOBJ=   -6529.56443530993     
 iteration         -432 MCMCOBJ=   -6518.12470007111     
 iteration         -431 MCMCOBJ=   -6488.45734804842     
 iteration         -430 MCMCOBJ=   -6513.67463898074     
 iteration         -429 MCMCOBJ=   -6567.26745751399     
 iteration         -428 MCMCOBJ=   -6509.95258026632     
 iteration         -427 MCMCOBJ=   -6507.08880713513     
 iteration         -426 MCMCOBJ=   -6459.84919504396     
 iteration         -425 MCMCOBJ=   -6400.55196995000     
 iteration         -424 MCMCOBJ=   -6399.85860635260     
 iteration         -423 MCMCOBJ=   -6459.85418967550     
 iteration         -422 MCMCOBJ=   -6438.19209637644     
 iteration         -421 MCMCOBJ=   -6475.32715667998     
 iteration         -420 MCMCOBJ=   -6491.47561021745     
 iteration         -419 MCMCOBJ=   -6482.81631256260     
 iteration         -418 MCMCOBJ=   -6566.55811820706     
 iteration         -417 MCMCOBJ=   -6544.63316339353     
 iteration         -416 MCMCOBJ=   -6487.06836896411     
 iteration         -415 MCMCOBJ=   -6525.47756781249     
 iteration         -414 MCMCOBJ=   -6490.24074036345     
 iteration         -413 MCMCOBJ=   -6489.29942405218     
 iteration         -412 MCMCOBJ=   -6493.27463703128     
 iteration         -411 MCMCOBJ=   -6510.85481806587     
 iteration         -410 MCMCOBJ=   -6490.13428110549     
 iteration         -409 MCMCOBJ=   -6457.81734770175     
 iteration         -408 MCMCOBJ=   -6505.60772813182     
 iteration         -407 MCMCOBJ=   -6481.37066301651     
 iteration         -406 MCMCOBJ=   -6499.11240534610     
 iteration         -405 MCMCOBJ=   -6499.08853237579     
 iteration         -404 MCMCOBJ=   -6492.90417571032     
 iteration         -403 MCMCOBJ=   -6494.62730172591     
 iteration         -402 MCMCOBJ=   -6530.32729504384     
 iteration         -401 MCMCOBJ=   -6540.58736819715     
 iteration         -400 MCMCOBJ=   -6528.49613731444     
 iteration         -399 MCMCOBJ=   -6472.44454850951     
 iteration         -398 MCMCOBJ=   -6420.62761765510     
 iteration         -397 MCMCOBJ=   -6432.98036527432     
 iteration         -396 MCMCOBJ=   -6450.67574611882     
 iteration         -395 MCMCOBJ=   -6446.14235725831     
 iteration         -394 MCMCOBJ=   -6498.41643471776     
 iteration         -393 MCMCOBJ=   -6480.36620107016     
 iteration         -392 MCMCOBJ=   -6506.99163938416     
 iteration         -391 MCMCOBJ=   -6522.75148434292     
 iteration         -390 MCMCOBJ=   -6473.34643694434     
 iteration         -389 MCMCOBJ=   -6503.81926587849     
 iteration         -388 MCMCOBJ=   -6500.65049666436     
 iteration         -387 MCMCOBJ=   -6520.36271300853     
 iteration         -386 MCMCOBJ=   -6546.71515993101     
 iteration         -385 MCMCOBJ=   -6541.22335985080     
 iteration         -384 MCMCOBJ=   -6487.58196239764     
 iteration         -383 MCMCOBJ=   -6488.98878665817     
 iteration         -382 MCMCOBJ=   -6552.61055414636     
 iteration         -381 MCMCOBJ=   -6565.54395905509     
 iteration         -380 MCMCOBJ=   -6559.48376587758     
 iteration         -379 MCMCOBJ=   -6542.52875471660     
 iteration         -378 MCMCOBJ=   -6542.52872333021     
 iteration         -377 MCMCOBJ=   -6544.05056565608     
 iteration         -376 MCMCOBJ=   -6525.43108304209     
 iteration         -375 MCMCOBJ=   -6517.17958876179     
 iteration         -374 MCMCOBJ=   -6514.42898391548     
 iteration         -373 MCMCOBJ=   -6515.86446790137     
 iteration         -372 MCMCOBJ=   -6506.11271453429     
 iteration         -371 MCMCOBJ=   -6494.05132149961     
 iteration         -370 MCMCOBJ=   -6474.72972508354     
 iteration         -369 MCMCOBJ=   -6461.84109211835     
 iteration         -368 MCMCOBJ=   -6423.59846216655     
 iteration         -367 MCMCOBJ=   -6446.59847707287     
 iteration         -366 MCMCOBJ=   -6489.08881336705     
 iteration         -365 MCMCOBJ=   -6525.68391737296     
 iteration         -364 MCMCOBJ=   -6483.01823936012     
 iteration         -363 MCMCOBJ=   -6532.38488140393     
 iteration         -362 MCMCOBJ=   -6528.09624698798     
 iteration         -361 MCMCOBJ=   -6550.20002101369     
 iteration         -360 MCMCOBJ=   -6522.38871390208     
 iteration         -359 MCMCOBJ=   -6475.79964996057     
 iteration         -358 MCMCOBJ=   -6499.93059645788     
 iteration         -357 MCMCOBJ=   -6470.36910670128     
 iteration         -356 MCMCOBJ=   -6458.99311001907     
 iteration         -355 MCMCOBJ=   -6515.15196714223     
 iteration         -354 MCMCOBJ=   -6481.57185273309     
 iteration         -353 MCMCOBJ=   -6447.40956634392     
 iteration         -352 MCMCOBJ=   -6464.18226376124     
 iteration         -351 MCMCOBJ=   -6442.77707574198     
 iteration         -350 MCMCOBJ=   -6491.51739612023     
 iteration         -349 MCMCOBJ=   -6524.10961796611     
 iteration         -348 MCMCOBJ=   -6473.25103000055     
 iteration         -347 MCMCOBJ=   -6501.05187297985     
 iteration         -346 MCMCOBJ=   -6530.97547182835     
 iteration         -345 MCMCOBJ=   -6579.21549780062     
 iteration         -344 MCMCOBJ=   -6532.76005241633     
 iteration         -343 MCMCOBJ=   -6494.19490139988     
 iteration         -342 MCMCOBJ=   -6495.26123543947     
 iteration         -341 MCMCOBJ=   -6483.23974722589     
 iteration         -340 MCMCOBJ=   -6442.52889085500     
 iteration         -339 MCMCOBJ=   -6457.13908168605     
 iteration         -338 MCMCOBJ=   -6508.84126645950     
 iteration         -337 MCMCOBJ=   -6551.66971122337     
 iteration         -336 MCMCOBJ=   -6545.80535748313     
 iteration         -335 MCMCOBJ=   -6545.86897263066     
 iteration         -334 MCMCOBJ=   -6513.61525666526     
 iteration         -333 MCMCOBJ=   -6549.13150624099     
 iteration         -332 MCMCOBJ=   -6569.51688073022     
 iteration         -331 MCMCOBJ=   -6499.98440433707     
 iteration         -330 MCMCOBJ=   -6502.58427889640     
 iteration         -329 MCMCOBJ=   -6487.69167632869     
 iteration         -328 MCMCOBJ=   -6488.46284287362     
 iteration         -327 MCMCOBJ=   -6508.64580756105     
 iteration         -326 MCMCOBJ=   -6504.38678833845     
 iteration         -325 MCMCOBJ=   -6509.43382569574     
 iteration         -324 MCMCOBJ=   -6537.11576378016     
 iteration         -323 MCMCOBJ=   -6493.57841181048     
 iteration         -322 MCMCOBJ=   -6498.09180433124     
 iteration         -321 MCMCOBJ=   -6492.96504432963     
 iteration         -320 MCMCOBJ=   -6515.19724552361     
 iteration         -319 MCMCOBJ=   -6502.71069647650     
 iteration         -318 MCMCOBJ=   -6521.02527727157     
 iteration         -317 MCMCOBJ=   -6509.44867152991     
 iteration         -316 MCMCOBJ=   -6497.61455177895     
 iteration         -315 MCMCOBJ=   -6477.13165251850     
 iteration         -314 MCMCOBJ=   -6567.68167130621     
 iteration         -313 MCMCOBJ=   -6531.56085799539     
 iteration         -312 MCMCOBJ=   -6509.57352136449     
 iteration         -311 MCMCOBJ=   -6503.77513551020     
 iteration         -310 MCMCOBJ=   -6550.84449701294     
 iteration         -309 MCMCOBJ=   -6540.46635292570     
 iteration         -308 MCMCOBJ=   -6546.36955626165     
 iteration         -307 MCMCOBJ=   -6501.12347739237     
 iteration         -306 MCMCOBJ=   -6537.88482589597     
 iteration         -305 MCMCOBJ=   -6544.01942837753     
 iteration         -304 MCMCOBJ=   -6506.20491371656     
 iteration         -303 MCMCOBJ=   -6488.15763161615     
 iteration         -302 MCMCOBJ=   -6512.42240652432     
 iteration         -301 MCMCOBJ=   -6571.33658426021     
 iteration         -300 MCMCOBJ=   -6576.66293275836     
 iteration         -299 MCMCOBJ=   -6506.11985170640     
 iteration         -298 MCMCOBJ=   -6507.59704715271     
 iteration         -297 MCMCOBJ=   -6492.39503051939     
 iteration         -296 MCMCOBJ=   -6512.03087452369     
 iteration         -295 MCMCOBJ=   -6522.06218370360     
 iteration         -294 MCMCOBJ=   -6523.85328143739     
 iteration         -293 MCMCOBJ=   -6512.08874608926     
 iteration         -292 MCMCOBJ=   -6445.08348248185     
 iteration         -291 MCMCOBJ=   -6521.28889057163     
 iteration         -290 MCMCOBJ=   -6515.98361766562     
 iteration         -289 MCMCOBJ=   -6498.69040496527     
 iteration         -288 MCMCOBJ=   -6463.44763896605     
 iteration         -287 MCMCOBJ=   -6484.69067601031     
 iteration         -286 MCMCOBJ=   -6516.31660669213     
 iteration         -285 MCMCOBJ=   -6510.94343858459     
 iteration         -284 MCMCOBJ=   -6532.69814980304     
 iteration         -283 MCMCOBJ=   -6521.93990120626     
 iteration         -282 MCMCOBJ=   -6467.66182946208     
 iteration         -281 MCMCOBJ=   -6480.63674617659     
 iteration         -280 MCMCOBJ=   -6496.03294315726     
 iteration         -279 MCMCOBJ=   -6526.93399011076     
 iteration         -278 MCMCOBJ=   -6548.12527339826     
 iteration         -277 MCMCOBJ=   -6432.01817616316     
 iteration         -276 MCMCOBJ=   -6452.17578361500     
 iteration         -275 MCMCOBJ=   -6446.34750031473     
 iteration         -274 MCMCOBJ=   -6491.67635556119     
 iteration         -273 MCMCOBJ=   -6549.22123985286     
 iteration         -272 MCMCOBJ=   -6533.92014546000     
 iteration         -271 MCMCOBJ=   -6523.06647353874     
 iteration         -270 MCMCOBJ=   -6543.01427934605     
 iteration         -269 MCMCOBJ=   -6493.69862469301     
 iteration         -268 MCMCOBJ=   -6534.82276954386     
 iteration         -267 MCMCOBJ=   -6495.46273268342     
 iteration         -266 MCMCOBJ=   -6469.71138343350     
 iteration         -265 MCMCOBJ=   -6495.95608610564     
 iteration         -264 MCMCOBJ=   -6458.28502282426     
 iteration         -263 MCMCOBJ=   -6515.63511679615     
 iteration         -262 MCMCOBJ=   -6516.57937320198     
 iteration         -261 MCMCOBJ=   -6495.33091879431     
 iteration         -260 MCMCOBJ=   -6550.58708589372     
 iteration         -259 MCMCOBJ=   -6552.32041649212     
 iteration         -258 MCMCOBJ=   -6536.94851859421     
 iteration         -257 MCMCOBJ=   -6541.16418043327     
 iteration         -256 MCMCOBJ=   -6516.76590186347     
 iteration         -255 MCMCOBJ=   -6532.22094718128     
 iteration         -254 MCMCOBJ=   -6536.66898848293     
 iteration         -253 MCMCOBJ=   -6520.02698060431     
 iteration         -252 MCMCOBJ=   -6503.74389346857     
 iteration         -251 MCMCOBJ=   -6469.18688606959     
 iteration         -250 MCMCOBJ=   -6479.83919094862     
 iteration         -249 MCMCOBJ=   -6475.13154837608     
 iteration         -248 MCMCOBJ=   -6478.23189525557     
 iteration         -247 MCMCOBJ=   -6569.68220418782     
 iteration         -246 MCMCOBJ=   -6533.33556534704     
 iteration         -245 MCMCOBJ=   -6465.83619225628     
 iteration         -244 MCMCOBJ=   -6458.11401264991     
 iteration         -243 MCMCOBJ=   -6446.41814865946     
 iteration         -242 MCMCOBJ=   -6441.87999044817     
 iteration         -241 MCMCOBJ=   -6470.43178403491     
 iteration         -240 MCMCOBJ=   -6422.51722012286     
 iteration         -239 MCMCOBJ=   -6453.18818545245     
 iteration         -238 MCMCOBJ=   -6494.66679045618     
 iteration         -237 MCMCOBJ=   -6470.34792308679     
 iteration         -236 MCMCOBJ=   -6520.86125071062     
 iteration         -235 MCMCOBJ=   -6451.90559658385     
 iteration         -234 MCMCOBJ=   -6449.12949362650     
 iteration         -233 MCMCOBJ=   -6515.97208232930     
 iteration         -232 MCMCOBJ=   -6554.49199169316     
 iteration         -231 MCMCOBJ=   -6547.88346239709     
 iteration         -230 MCMCOBJ=   -6498.53747189771     
 iteration         -229 MCMCOBJ=   -6474.51143288377     
 iteration         -228 MCMCOBJ=   -6465.94275770423     
 iteration         -227 MCMCOBJ=   -6530.19383144356     
 iteration         -226 MCMCOBJ=   -6490.49353761003     
 iteration         -225 MCMCOBJ=   -6484.83869917908     
 iteration         -224 MCMCOBJ=   -6479.58287051226     
 iteration         -223 MCMCOBJ=   -6477.29216704426     
 iteration         -222 MCMCOBJ=   -6504.87837897166     
 iteration         -221 MCMCOBJ=   -6515.41778672146     
 iteration         -220 MCMCOBJ=   -6502.87863284104     
 iteration         -219 MCMCOBJ=   -6475.92135758040     
 iteration         -218 MCMCOBJ=   -6509.91049204734     
 iteration         -217 MCMCOBJ=   -6489.74701877012     
 iteration         -216 MCMCOBJ=   -6508.31703826101     
 iteration         -215 MCMCOBJ=   -6536.59845909319     
 iteration         -214 MCMCOBJ=   -6495.11833542237     
 iteration         -213 MCMCOBJ=   -6489.60815369822     
 iteration         -212 MCMCOBJ=   -6489.60815325310     
 iteration         -211 MCMCOBJ=   -6504.93529687004     
 iteration         -210 MCMCOBJ=   -6521.18343798978     
 iteration         -209 MCMCOBJ=   -6521.13699580967     
 iteration         -208 MCMCOBJ=   -6507.63288330070     
 iteration         -207 MCMCOBJ=   -6470.99263491888     
 iteration         -206 MCMCOBJ=   -6486.67489181565     
 iteration         -205 MCMCOBJ=   -6474.05985508057     
 iteration         -204 MCMCOBJ=   -6482.97495267079     
 iteration         -203 MCMCOBJ=   -6424.99829909726     
 iteration         -202 MCMCOBJ=   -6438.11984237384     
 iteration         -201 MCMCOBJ=   -6470.94878986797     
 iteration         -200 MCMCOBJ=   -6449.15779603782     
 iteration         -199 MCMCOBJ=   -6463.41478177588     
 iteration         -198 MCMCOBJ=   -6454.22531624048     
 iteration         -197 MCMCOBJ=   -6513.51339230571     
 iteration         -196 MCMCOBJ=   -6501.73075980449     
 iteration         -195 MCMCOBJ=   -6515.00424026509     
 iteration         -194 MCMCOBJ=   -6460.24920937340     
 iteration         -193 MCMCOBJ=   -6465.34600413168     
 iteration         -192 MCMCOBJ=   -6522.44489427357     
 iteration         -191 MCMCOBJ=   -6504.87828288657     
 iteration         -190 MCMCOBJ=   -6590.75719120004     
 iteration         -189 MCMCOBJ=   -6510.73081762803     
 iteration         -188 MCMCOBJ=   -6455.22354446099     
 iteration         -187 MCMCOBJ=   -6495.79452242705     
 iteration         -186 MCMCOBJ=   -6503.51704307378     
 iteration         -185 MCMCOBJ=   -6520.20443867707     
 iteration         -184 MCMCOBJ=   -6445.23950177326     
 iteration         -183 MCMCOBJ=   -6470.53801150689     
 iteration         -182 MCMCOBJ=   -6472.64933004934     
 iteration         -181 MCMCOBJ=   -6458.75574892029     
 iteration         -180 MCMCOBJ=   -6449.20785224068     
 iteration         -179 MCMCOBJ=   -6500.97920651826     
 iteration         -178 MCMCOBJ=   -6507.19670856175     
 iteration         -177 MCMCOBJ=   -6522.45683996858     
 iteration         -176 MCMCOBJ=   -6535.17737554631     
 iteration         -175 MCMCOBJ=   -6537.41899396262     
 iteration         -174 MCMCOBJ=   -6531.24317633603     
 iteration         -173 MCMCOBJ=   -6514.57906890025     
 iteration         -172 MCMCOBJ=   -6538.56644495020     
 iteration         -171 MCMCOBJ=   -6556.66230874284     
 iteration         -170 MCMCOBJ=   -6491.87824833106     
 iteration         -169 MCMCOBJ=   -6469.50410043321     
 iteration         -168 MCMCOBJ=   -6494.82096827882     
 iteration         -167 MCMCOBJ=   -6478.05604383841     
 iteration         -166 MCMCOBJ=   -6487.41724004553     
 iteration         -165 MCMCOBJ=   -6488.88247693004     
 iteration         -164 MCMCOBJ=   -6520.19969913191     
 iteration         -163 MCMCOBJ=   -6485.07828736396     
 iteration         -162 MCMCOBJ=   -6470.77357860705     
 iteration         -161 MCMCOBJ=   -6481.43259929480     
 iteration         -160 MCMCOBJ=   -6507.02554317513     
 iteration         -159 MCMCOBJ=   -6518.81726830397     
 iteration         -158 MCMCOBJ=   -6505.58515604448     
 iteration         -157 MCMCOBJ=   -6455.48353141797     
 iteration         -156 MCMCOBJ=   -6480.52289368772     
 iteration         -155 MCMCOBJ=   -6495.38442974681     
 iteration         -154 MCMCOBJ=   -6499.58973078722     
 iteration         -153 MCMCOBJ=   -6556.88115129954     
 iteration         -152 MCMCOBJ=   -6549.81008276348     
 iteration         -151 MCMCOBJ=   -6502.89228453154     
 iteration         -150 MCMCOBJ=   -6469.12506295318     
 iteration         -149 MCMCOBJ=   -6493.27608798935     
 iteration         -148 MCMCOBJ=   -6475.12611922469     
 iteration         -147 MCMCOBJ=   -6428.74129111848     
 iteration         -146 MCMCOBJ=   -6448.51243875456     
 iteration         -145 MCMCOBJ=   -6425.86114700032     
 iteration         -144 MCMCOBJ=   -6424.24297127062     
 iteration         -143 MCMCOBJ=   -6401.85508951829     
 iteration         -142 MCMCOBJ=   -6403.84662144531     
 iteration         -141 MCMCOBJ=   -6453.18915699322     
 iteration         -140 MCMCOBJ=   -6459.03697526795     
 iteration         -139 MCMCOBJ=   -6465.56639505897     
 iteration         -138 MCMCOBJ=   -6516.12481532652     
 iteration         -137 MCMCOBJ=   -6532.55944981117     
 iteration         -136 MCMCOBJ=   -6468.36940105624     
 iteration         -135 MCMCOBJ=   -6462.00023285878     
 iteration         -134 MCMCOBJ=   -6456.75896279249     
 iteration         -133 MCMCOBJ=   -6474.85130518492     
 iteration         -132 MCMCOBJ=   -6473.93570277391     
 iteration         -131 MCMCOBJ=   -6505.25090204600     
 iteration         -130 MCMCOBJ=   -6495.21619901729     
 iteration         -129 MCMCOBJ=   -6495.21619894489     
 iteration         -128 MCMCOBJ=   -6508.65789245130     
 iteration         -127 MCMCOBJ=   -6476.30510095169     
 iteration         -126 MCMCOBJ=   -6505.27193127730     
 iteration         -125 MCMCOBJ=   -6534.50276131066     
 iteration         -124 MCMCOBJ=   -6502.16284948604     
 iteration         -123 MCMCOBJ=   -6493.18071825035     
 iteration         -122 MCMCOBJ=   -6446.88753111718     
 iteration         -121 MCMCOBJ=   -6428.18597930676     
 iteration         -120 MCMCOBJ=   -6440.45653851145     
 iteration         -119 MCMCOBJ=   -6441.18374408098     
 iteration         -118 MCMCOBJ=   -6427.53412006652     
 iteration         -117 MCMCOBJ=   -6419.35425362190     
 iteration         -116 MCMCOBJ=   -6440.11113832155     
 iteration         -115 MCMCOBJ=   -6487.31037713484     
 iteration         -114 MCMCOBJ=   -6481.10872733638     
 iteration         -113 MCMCOBJ=   -6476.24113425252     
 iteration         -112 MCMCOBJ=   -6566.61155694330     
 iteration         -111 MCMCOBJ=   -6562.51104303545     
 iteration         -110 MCMCOBJ=   -6560.75534079930     
 iteration         -109 MCMCOBJ=   -6532.64813112646     
 iteration         -108 MCMCOBJ=   -6508.27251803754     
 iteration         -107 MCMCOBJ=   -6506.67089349224     
 iteration         -106 MCMCOBJ=   -6515.24051144301     
 iteration         -105 MCMCOBJ=   -6484.29098990147     
 iteration         -104 MCMCOBJ=   -6477.49764253505     
 iteration         -103 MCMCOBJ=   -6481.20366907284     
 iteration         -102 MCMCOBJ=   -6497.04606753018     
 iteration         -101 MCMCOBJ=   -6500.07348061193     
 iteration         -100 MCMCOBJ=   -6474.46826862707     
 iteration          -99 MCMCOBJ=   -6456.72469893380     
 iteration          -98 MCMCOBJ=   -6450.14733298321     
 iteration          -97 MCMCOBJ=   -6478.68901002797     
 iteration          -96 MCMCOBJ=   -6522.02582727714     
 iteration          -95 MCMCOBJ=   -6519.86575470218     
 iteration          -94 MCMCOBJ=   -6515.75863153463     
 iteration          -93 MCMCOBJ=   -6517.17917773340     
 iteration          -92 MCMCOBJ=   -6516.27075377548     
 iteration          -91 MCMCOBJ=   -6495.17661063463     
 iteration          -90 MCMCOBJ=   -6444.15090142317     
 iteration          -89 MCMCOBJ=   -6454.66317143549     
 iteration          -88 MCMCOBJ=   -6482.55307019182     
 iteration          -87 MCMCOBJ=   -6480.65895360947     
 iteration          -86 MCMCOBJ=   -6449.44134430830     
 iteration          -85 MCMCOBJ=   -6482.35080949110     
 iteration          -84 MCMCOBJ=   -6532.38113280584     
 iteration          -83 MCMCOBJ=   -6462.28990787461     
 iteration          -82 MCMCOBJ=   -6420.44315058456     
 iteration          -81 MCMCOBJ=   -6376.17419186545     
 iteration          -80 MCMCOBJ=   -6400.62044466456     
 iteration          -79 MCMCOBJ=   -6434.84394708195     
 iteration          -78 MCMCOBJ=   -6429.94411067928     
 iteration          -77 MCMCOBJ=   -6420.68891875555     
 iteration          -76 MCMCOBJ=   -6406.26076073571     
 iteration          -75 MCMCOBJ=   -6453.28624739351     
 iteration          -74 MCMCOBJ=   -6472.48935488572     
 iteration          -73 MCMCOBJ=   -6444.41968697947     
 iteration          -72 MCMCOBJ=   -6405.38519232474     
 iteration          -71 MCMCOBJ=   -6486.43487016593     
 iteration          -70 MCMCOBJ=   -6508.95895219231     
 iteration          -69 MCMCOBJ=   -6486.87001688085     
 iteration          -68 MCMCOBJ=   -6502.17889137447     
 iteration          -67 MCMCOBJ=   -6472.58968792460     
 iteration          -66 MCMCOBJ=   -6492.24975298468     
 iteration          -65 MCMCOBJ=   -6497.77282336526     
 iteration          -64 MCMCOBJ=   -6468.33437202492     
 iteration          -63 MCMCOBJ=   -6515.68132917365     
 iteration          -62 MCMCOBJ=   -6499.95849104370     
 iteration          -61 MCMCOBJ=   -6526.51070813323     
 iteration          -60 MCMCOBJ=   -6529.97557850667     
 iteration          -59 MCMCOBJ=   -6560.68171074584     
 iteration          -58 MCMCOBJ=   -6547.34260366939     
 iteration          -57 MCMCOBJ=   -6521.41302170269     
 iteration          -56 MCMCOBJ=   -6545.28664564634     
 iteration          -55 MCMCOBJ=   -6592.99940116578     
 iteration          -54 MCMCOBJ=   -6563.09315838586     
 iteration          -53 MCMCOBJ=   -6548.14685912281     
 iteration          -52 MCMCOBJ=   -6542.45923146322     
 iteration          -51 MCMCOBJ=   -6525.08282766559     
 iteration          -50 MCMCOBJ=   -6513.83867884189     
 iteration          -49 MCMCOBJ=   -6499.80333278345     
 iteration          -48 MCMCOBJ=   -6508.64325813769     
 iteration          -47 MCMCOBJ=   -6507.30936845905     
 iteration          -46 MCMCOBJ=   -6501.97979981770     
 iteration          -45 MCMCOBJ=   -6489.88318355276     
 iteration          -44 MCMCOBJ=   -6471.27264965295     
 iteration          -43 MCMCOBJ=   -6464.99428536204     
 iteration          -42 MCMCOBJ=   -6436.29442525213     
 iteration          -41 MCMCOBJ=   -6439.31230937037     
 iteration          -40 MCMCOBJ=   -6430.20057932275     
 iteration          -39 MCMCOBJ=   -6467.83963661765     
 iteration          -38 MCMCOBJ=   -6464.18420869153     
 iteration          -37 MCMCOBJ=   -6490.79977443059     
 iteration          -36 MCMCOBJ=   -6552.24991371240     
 iteration          -35 MCMCOBJ=   -6531.93324025403     
 iteration          -34 MCMCOBJ=   -6539.16049217766     
 iteration          -33 MCMCOBJ=   -6516.65457471248     
 iteration          -32 MCMCOBJ=   -6513.18738613127     
 iteration          -31 MCMCOBJ=   -6467.16218938274     
 iteration          -30 MCMCOBJ=   -6507.96560942669     
 iteration          -29 MCMCOBJ=   -6505.12901363046     
 iteration          -28 MCMCOBJ=   -6490.92955600138     
 iteration          -27 MCMCOBJ=   -6503.84091205919     
 iteration          -26 MCMCOBJ=   -6520.28781659575     
 iteration          -25 MCMCOBJ=   -6508.30721091697     
 iteration          -24 MCMCOBJ=   -6472.48285110704     
 iteration          -23 MCMCOBJ=   -6476.09618853023     
 iteration          -22 MCMCOBJ=   -6473.34250910467     
 iteration          -21 MCMCOBJ=   -6503.45390707320     
 iteration          -20 MCMCOBJ=   -6522.19011745928     
 iteration          -19 MCMCOBJ=   -6533.20224921234     
 iteration          -18 MCMCOBJ=   -6523.96936478379     
 iteration          -17 MCMCOBJ=   -6522.78302527405     
 iteration          -16 MCMCOBJ=   -6534.94952027246     
 iteration          -15 MCMCOBJ=   -6531.03157866821     
 iteration          -14 MCMCOBJ=   -6510.93367583729     
 iteration          -13 MCMCOBJ=   -6510.93367603476     
 iteration          -12 MCMCOBJ=   -6511.91865602599     
 iteration          -11 MCMCOBJ=   -6497.00568288864     
 iteration          -10 MCMCOBJ=   -6493.84078785643     
 iteration           -9 MCMCOBJ=   -6526.58878523127     
 iteration           -8 MCMCOBJ=   -6490.39837479284     
 iteration           -7 MCMCOBJ=   -6478.09004755523     
 iteration           -6 MCMCOBJ=   -6466.47238165583     
 iteration           -5 MCMCOBJ=   -6462.46522313305     
 iteration           -4 MCMCOBJ=   -6447.88578222333     
 iteration           -3 MCMCOBJ=   -6456.79689434752     
 iteration           -2 MCMCOBJ=   -6395.37341572089     
 iteration           -1 MCMCOBJ=   -6436.28582662098     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6442.36111753059     
 iteration            1 MCMCOBJ=   -6472.21621255124     
 iteration            2 MCMCOBJ=   -6435.53386878041     
 iteration            3 MCMCOBJ=   -6408.53593767811     
 iteration            4 MCMCOBJ=   -6414.01417125760     
 iteration            5 MCMCOBJ=   -6450.61474095303     
 iteration            6 MCMCOBJ=   -6416.58627578077     
 iteration            7 MCMCOBJ=   -6490.56224274961     
 iteration            8 MCMCOBJ=   -6468.05857277525     
 iteration            9 MCMCOBJ=   -6497.19920993544     
 iteration           10 MCMCOBJ=   -6529.80381878103     
 iteration           11 MCMCOBJ=   -6534.62032182984     
 iteration           12 MCMCOBJ=   -6518.63975075719     
 iteration           13 MCMCOBJ=   -6543.59324505115     
 iteration           14 MCMCOBJ=   -6543.59217619179     
 iteration           15 MCMCOBJ=   -6490.94405640849     
 iteration           16 MCMCOBJ=   -6466.79953708782     
 iteration           17 MCMCOBJ=   -6476.10715129173     
 iteration           18 MCMCOBJ=   -6474.03675837861     
 iteration           19 MCMCOBJ=   -6481.39590472149     
 iteration           20 MCMCOBJ=   -6468.49356677247     
 iteration           21 MCMCOBJ=   -6404.13677994178     
 iteration           22 MCMCOBJ=   -6449.63185366131     
 iteration           23 MCMCOBJ=   -6461.58545036803     
 iteration           24 MCMCOBJ=   -6411.87000799623     
 iteration           25 MCMCOBJ=   -6393.10514960524     
 iteration           26 MCMCOBJ=   -6420.10054238892     
 iteration           27 MCMCOBJ=   -6512.67306973492     
 iteration           28 MCMCOBJ=   -6521.48624158064     
 iteration           29 MCMCOBJ=   -6510.45868617782     
 iteration           30 MCMCOBJ=   -6466.52568099083     
 iteration           31 MCMCOBJ=   -6468.20793959767     
 iteration           32 MCMCOBJ=   -6493.39090228749     
 iteration           33 MCMCOBJ=   -6403.25411759598     
 iteration           34 MCMCOBJ=   -6445.32091551872     
 iteration           35 MCMCOBJ=   -6443.89870352376     
 iteration           36 MCMCOBJ=   -6448.87124572894     
 iteration           37 MCMCOBJ=   -6481.84556973888     
 iteration           38 MCMCOBJ=   -6452.73367096641     
 iteration           39 MCMCOBJ=   -6476.20659625268     
 iteration           40 MCMCOBJ=   -6559.65530860742     
 iteration           41 MCMCOBJ=   -6518.00385364507     
 iteration           42 MCMCOBJ=   -6524.26633159458     
 iteration           43 MCMCOBJ=   -6439.00707407495     
 iteration           44 MCMCOBJ=   -6495.55371412285     
 iteration           45 MCMCOBJ=   -6502.42915586883     
 iteration           46 MCMCOBJ=   -6509.18670723964     
 iteration           47 MCMCOBJ=   -6580.21774454933     
 iteration           48 MCMCOBJ=   -6557.53231327636     
 iteration           49 MCMCOBJ=   -6573.77361537555     
 iteration           50 MCMCOBJ=   -6526.35248159821     
 iteration           51 MCMCOBJ=   -6482.21887617460     
 iteration           52 MCMCOBJ=   -6438.01940260168     
 iteration           53 MCMCOBJ=   -6443.42062142929     
 iteration           54 MCMCOBJ=   -6478.78157070083     
 iteration           55 MCMCOBJ=   -6496.29309067505     
 iteration           56 MCMCOBJ=   -6499.75726230073     
 iteration           57 MCMCOBJ=   -6513.08046289894     
 iteration           58 MCMCOBJ=   -6562.87831577672     
 iteration           59 MCMCOBJ=   -6508.26659364338     
 iteration           60 MCMCOBJ=   -6495.33024787638     
 iteration           61 MCMCOBJ=   -6453.82028367799     
 iteration           62 MCMCOBJ=   -6448.39049014146     
 iteration           63 MCMCOBJ=   -6467.26913259727     
 iteration           64 MCMCOBJ=   -6435.50725955892     
 iteration           65 MCMCOBJ=   -6469.88671162982     
 iteration           66 MCMCOBJ=   -6502.74795337814     
 iteration           67 MCMCOBJ=   -6515.85024623496     
 iteration           68 MCMCOBJ=   -6493.12847131525     
 iteration           69 MCMCOBJ=   -6469.02934055327     
 iteration           70 MCMCOBJ=   -6459.40277225683     
 iteration           71 MCMCOBJ=   -6479.32639122917     
 iteration           72 MCMCOBJ=   -6461.74241110246     
 iteration           73 MCMCOBJ=   -6448.55854547495     
 iteration           74 MCMCOBJ=   -6466.86997854824     
 iteration           75 MCMCOBJ=   -6445.79916162939     
 iteration           76 MCMCOBJ=   -6537.61093723646     
 iteration           77 MCMCOBJ=   -6532.51604478294     
 iteration           78 MCMCOBJ=   -6514.32004076953     
 iteration           79 MCMCOBJ=   -6482.85058348940     
 iteration           80 MCMCOBJ=   -6485.37552078044     
 iteration           81 MCMCOBJ=   -6488.74268058560     
 iteration           82 MCMCOBJ=   -6468.09593265419     
 iteration           83 MCMCOBJ=   -6552.88118690797     
 iteration           84 MCMCOBJ=   -6497.27139770363     
 iteration           85 MCMCOBJ=   -6534.82696557997     
 iteration           86 MCMCOBJ=   -6539.13320849658     
 iteration           87 MCMCOBJ=   -6487.38886072974     
 iteration           88 MCMCOBJ=   -6526.76430155171     
 iteration           89 MCMCOBJ=   -6506.86225170095     
 iteration           90 MCMCOBJ=   -6514.88410025179     
 iteration           91 MCMCOBJ=   -6423.58583363810     
 iteration           92 MCMCOBJ=   -6479.93517663167     
 iteration           93 MCMCOBJ=   -6505.72901381943     
 iteration           94 MCMCOBJ=   -6461.14008440084     
 iteration           95 MCMCOBJ=   -6505.60763351168     
 iteration           96 MCMCOBJ=   -6493.74053939781     
 iteration           97 MCMCOBJ=   -6485.27729893294     
 iteration           98 MCMCOBJ=   -6496.44207719240     
 iteration           99 MCMCOBJ=   -6496.61851689824     
 iteration          100 MCMCOBJ=   -6532.96349105804     
 iteration          101 MCMCOBJ=   -6535.09818766020     
 iteration          102 MCMCOBJ=   -6540.87485195406     
 iteration          103 MCMCOBJ=   -6569.77854513268     
 iteration          104 MCMCOBJ=   -6556.00836051687     
 iteration          105 MCMCOBJ=   -6482.35443089830     
 iteration          106 MCMCOBJ=   -6472.68298737049     
 iteration          107 MCMCOBJ=   -6475.91096631216     
 iteration          108 MCMCOBJ=   -6511.92651948746     
 iteration          109 MCMCOBJ=   -6511.81629487538     
 iteration          110 MCMCOBJ=   -6488.84836563962     
 iteration          111 MCMCOBJ=   -6525.11997822451     
 iteration          112 MCMCOBJ=   -6517.51189477022     
 iteration          113 MCMCOBJ=   -6509.14095289847     
 iteration          114 MCMCOBJ=   -6509.14095298386     
 iteration          115 MCMCOBJ=   -6536.67582778873     
 iteration          116 MCMCOBJ=   -6517.92931817820     
 iteration          117 MCMCOBJ=   -6555.23542923979     
 iteration          118 MCMCOBJ=   -6530.76837998758     
 iteration          119 MCMCOBJ=   -6541.42727089362     
 iteration          120 MCMCOBJ=   -6526.35674505972     
 iteration          121 MCMCOBJ=   -6559.81406246086     
 iteration          122 MCMCOBJ=   -6528.43261201759     
 iteration          123 MCMCOBJ=   -6553.83500449518     
 iteration          124 MCMCOBJ=   -6551.45838159197     
 iteration          125 MCMCOBJ=   -6522.24418888651     
 iteration          126 MCMCOBJ=   -6545.21642358616     
 iteration          127 MCMCOBJ=   -6506.94515959939     
 iteration          128 MCMCOBJ=   -6493.47384951359     
 iteration          129 MCMCOBJ=   -6467.22533241535     
 iteration          130 MCMCOBJ=   -6509.77265345268     
 iteration          131 MCMCOBJ=   -6509.57960034456     
 iteration          132 MCMCOBJ=   -6485.30759251089     
 iteration          133 MCMCOBJ=   -6538.61203652220     
 iteration          134 MCMCOBJ=   -6516.34160614296     
 iteration          135 MCMCOBJ=   -6473.86729273584     
 iteration          136 MCMCOBJ=   -6496.28883394744     
 iteration          137 MCMCOBJ=   -6499.62923377802     
 iteration          138 MCMCOBJ=   -6513.86835918151     
 iteration          139 MCMCOBJ=   -6520.77715970643     
 iteration          140 MCMCOBJ=   -6515.16218644932     
 iteration          141 MCMCOBJ=   -6496.16424707041     
 iteration          142 MCMCOBJ=   -6488.60967143900     
 iteration          143 MCMCOBJ=   -6530.91542384074     
 iteration          144 MCMCOBJ=   -6506.44244241663     
 iteration          145 MCMCOBJ=   -6516.51482994536     
 iteration          146 MCMCOBJ=   -6512.97664614303     
 iteration          147 MCMCOBJ=   -6494.39171073576     
 iteration          148 MCMCOBJ=   -6504.04127679698     
 iteration          149 MCMCOBJ=   -6529.11377853203     
 iteration          150 MCMCOBJ=   -6498.24512423284     
 iteration          151 MCMCOBJ=   -6568.70474208977     
 iteration          152 MCMCOBJ=   -6506.87116071967     
 iteration          153 MCMCOBJ=   -6528.92243424321     
 iteration          154 MCMCOBJ=   -6530.67431758917     
 iteration          155 MCMCOBJ=   -6477.26900758476     
 iteration          156 MCMCOBJ=   -6501.19298172608     
 iteration          157 MCMCOBJ=   -6504.24378706568     
 iteration          158 MCMCOBJ=   -6504.24378303943     
 iteration          159 MCMCOBJ=   -6455.78584081105     
 iteration          160 MCMCOBJ=   -6529.11459355396     
 iteration          161 MCMCOBJ=   -6491.70132008699     
 iteration          162 MCMCOBJ=   -6507.94181243689     
 iteration          163 MCMCOBJ=   -6495.45484697593     
 iteration          164 MCMCOBJ=   -6465.35285929024     
 iteration          165 MCMCOBJ=   -6454.04273752994     
 iteration          166 MCMCOBJ=   -6503.53930012496     
 iteration          167 MCMCOBJ=   -6490.29341351508     
 iteration          168 MCMCOBJ=   -6481.48640443643     
 iteration          169 MCMCOBJ=   -6473.12507415206     
 iteration          170 MCMCOBJ=   -6477.24349058141     
 iteration          171 MCMCOBJ=   -6526.66315004287     
 iteration          172 MCMCOBJ=   -6547.85828804829     
 iteration          173 MCMCOBJ=   -6555.00449528105     
 iteration          174 MCMCOBJ=   -6535.69993325999     
 iteration          175 MCMCOBJ=   -6499.41664456544     
 iteration          176 MCMCOBJ=   -6496.00294362540     
 iteration          177 MCMCOBJ=   -6470.56821019937     
 iteration          178 MCMCOBJ=   -6443.02693658673     
 iteration          179 MCMCOBJ=   -6496.57105777638     
 iteration          180 MCMCOBJ=   -6486.83928579251     
 iteration          181 MCMCOBJ=   -6477.96750870251     
 iteration          182 MCMCOBJ=   -6510.06473149077     
 iteration          183 MCMCOBJ=   -6440.24163988661     
 iteration          184 MCMCOBJ=   -6397.49749376684     
 iteration          185 MCMCOBJ=   -6408.41374949444     
 iteration          186 MCMCOBJ=   -6434.01722374553     
 iteration          187 MCMCOBJ=   -6476.93959436686     
 iteration          188 MCMCOBJ=   -6540.57897263950     
 iteration          189 MCMCOBJ=   -6525.16429556437     
 iteration          190 MCMCOBJ=   -6499.43194184089     
 iteration          191 MCMCOBJ=   -6499.43193356467     
 iteration          192 MCMCOBJ=   -6496.81102639970     
 iteration          193 MCMCOBJ=   -6504.96896725525     
 iteration          194 MCMCOBJ=   -6527.27053325503     
 iteration          195 MCMCOBJ=   -6525.84969754255     
 iteration          196 MCMCOBJ=   -6535.85720337475     
 iteration          197 MCMCOBJ=   -6522.55547535280     
 iteration          198 MCMCOBJ=   -6501.81032252811     
 iteration          199 MCMCOBJ=   -6519.62357688730     
 iteration          200 MCMCOBJ=   -6504.11693697569     
 iteration          201 MCMCOBJ=   -6504.11694365452     
 iteration          202 MCMCOBJ=   -6500.05577894070     
 iteration          203 MCMCOBJ=   -6521.33636281244     
 iteration          204 MCMCOBJ=   -6516.48459582521     
 iteration          205 MCMCOBJ=   -6481.47652952921     
 iteration          206 MCMCOBJ=   -6526.55117833849     
 iteration          207 MCMCOBJ=   -6500.57039852603     
 iteration          208 MCMCOBJ=   -6477.65653274671     
 iteration          209 MCMCOBJ=   -6457.62099687586     
 iteration          210 MCMCOBJ=   -6438.60982286455     
 iteration          211 MCMCOBJ=   -6483.66587062161     
 iteration          212 MCMCOBJ=   -6495.66992236784     
 iteration          213 MCMCOBJ=   -6532.53855465277     
 iteration          214 MCMCOBJ=   -6495.50277399433     
 iteration          215 MCMCOBJ=   -6490.99673048807     
 iteration          216 MCMCOBJ=   -6413.86234297237     
 iteration          217 MCMCOBJ=   -6413.86233196319     
 iteration          218 MCMCOBJ=   -6386.47214678589     
 iteration          219 MCMCOBJ=   -6408.93732321392     
 iteration          220 MCMCOBJ=   -6474.13714064688     
 iteration          221 MCMCOBJ=   -6514.99673972626     
 iteration          222 MCMCOBJ=   -6543.14968960288     
 iteration          223 MCMCOBJ=   -6554.13190608624     
 iteration          224 MCMCOBJ=   -6541.07139669683     
 iteration          225 MCMCOBJ=   -6536.86162130040     
 iteration          226 MCMCOBJ=   -6499.18745084582     
 iteration          227 MCMCOBJ=   -6529.21824238623     
 iteration          228 MCMCOBJ=   -6466.73042436106     
 iteration          229 MCMCOBJ=   -6469.14972996081     
 iteration          230 MCMCOBJ=   -6498.09808429435     
 iteration          231 MCMCOBJ=   -6514.91040968989     
 iteration          232 MCMCOBJ=   -6511.17809085678     
 iteration          233 MCMCOBJ=   -6522.75116589831     
 iteration          234 MCMCOBJ=   -6480.69726588347     
 iteration          235 MCMCOBJ=   -6468.13253615304     
 iteration          236 MCMCOBJ=   -6492.85019823698     
 iteration          237 MCMCOBJ=   -6446.72941551367     
 iteration          238 MCMCOBJ=   -6396.13064863112     
 iteration          239 MCMCOBJ=   -6454.33520155042     
 iteration          240 MCMCOBJ=   -6420.85553597108     
 iteration          241 MCMCOBJ=   -6403.23923673308     
 iteration          242 MCMCOBJ=   -6387.24780500664     
 iteration          243 MCMCOBJ=   -6438.90415386491     
 iteration          244 MCMCOBJ=   -6447.14992615289     
 iteration          245 MCMCOBJ=   -6459.96972313344     
 iteration          246 MCMCOBJ=   -6479.27565245805     
 iteration          247 MCMCOBJ=   -6469.47195350668     
 iteration          248 MCMCOBJ=   -6455.50910984573     
 iteration          249 MCMCOBJ=   -6448.97290688484     
 iteration          250 MCMCOBJ=   -6428.06593935657     
 iteration          251 MCMCOBJ=   -6466.71256542277     
 iteration          252 MCMCOBJ=   -6496.99663273921     
 iteration          253 MCMCOBJ=   -6479.20727011433     
 iteration          254 MCMCOBJ=   -6494.37296918824     
 iteration          255 MCMCOBJ=   -6520.25419404940     
 iteration          256 MCMCOBJ=   -6515.40538348320     
 iteration          257 MCMCOBJ=   -6514.55058507901     
 iteration          258 MCMCOBJ=   -6535.21599626711     
 iteration          259 MCMCOBJ=   -6544.56447447676     
 iteration          260 MCMCOBJ=   -6492.51260365917     
 iteration          261 MCMCOBJ=   -6540.84266124105     
 iteration          262 MCMCOBJ=   -6495.12105273948     
 iteration          263 MCMCOBJ=   -6539.78210702148     
 iteration          264 MCMCOBJ=   -6552.35115258353     
 iteration          265 MCMCOBJ=   -6520.63154607028     
 iteration          266 MCMCOBJ=   -6471.32839663611     
 iteration          267 MCMCOBJ=   -6508.33766339379     
 iteration          268 MCMCOBJ=   -6501.88955555282     
 iteration          269 MCMCOBJ=   -6502.03057689757     
 iteration          270 MCMCOBJ=   -6498.83866183187     
 iteration          271 MCMCOBJ=   -6494.79512576964     
 iteration          272 MCMCOBJ=   -6494.79511073671     
 iteration          273 MCMCOBJ=   -6459.93868426283     
 iteration          274 MCMCOBJ=   -6447.57044920401     
 iteration          275 MCMCOBJ=   -6475.09002135295     
 iteration          276 MCMCOBJ=   -6507.81928040553     
 iteration          277 MCMCOBJ=   -6466.02531231954     
 iteration          278 MCMCOBJ=   -6480.33151371108     
 iteration          279 MCMCOBJ=   -6500.31643073821     
 iteration          280 MCMCOBJ=   -6496.13802511032     
 iteration          281 MCMCOBJ=   -6486.55706782264     
 iteration          282 MCMCOBJ=   -6460.60459870753     
 iteration          283 MCMCOBJ=   -6522.74918035104     
 iteration          284 MCMCOBJ=   -6507.76089670650     
 iteration          285 MCMCOBJ=   -6439.72396813729     
 iteration          286 MCMCOBJ=   -6492.28692251995     
 iteration          287 MCMCOBJ=   -6501.32499883638     
 iteration          288 MCMCOBJ=   -6511.91740166778     
 iteration          289 MCMCOBJ=   -6496.38187807380     
 iteration          290 MCMCOBJ=   -6467.91961300000     
 iteration          291 MCMCOBJ=   -6479.60240392938     
 iteration          292 MCMCOBJ=   -6454.17780477122     
 iteration          293 MCMCOBJ=   -6510.18925131085     
 iteration          294 MCMCOBJ=   -6504.90738353949     
 iteration          295 MCMCOBJ=   -6492.27555633411     
 iteration          296 MCMCOBJ=   -6517.12823994672     
 iteration          297 MCMCOBJ=   -6460.91825244754     
 iteration          298 MCMCOBJ=   -6486.07689693753     
 iteration          299 MCMCOBJ=   -6492.84789028576     
 iteration          300 MCMCOBJ=   -6498.23685092996     
 iteration          301 MCMCOBJ=   -6506.61842910979     
 iteration          302 MCMCOBJ=   -6489.17767459780     
 iteration          303 MCMCOBJ=   -6506.49693129286     
 iteration          304 MCMCOBJ=   -6519.80962581171     
 iteration          305 MCMCOBJ=   -6509.03478001646     
 iteration          306 MCMCOBJ=   -6502.51656461569     
 iteration          307 MCMCOBJ=   -6545.69473251354     
 iteration          308 MCMCOBJ=   -6454.60341636193     
 iteration          309 MCMCOBJ=   -6471.91137834058     
 iteration          310 MCMCOBJ=   -6481.21052954442     
 iteration          311 MCMCOBJ=   -6478.64627749473     
 iteration          312 MCMCOBJ=   -6471.20206591931     
 iteration          313 MCMCOBJ=   -6534.94717758235     
 iteration          314 MCMCOBJ=   -6502.47934025800     
 iteration          315 MCMCOBJ=   -6517.78447507926     
 iteration          316 MCMCOBJ=   -6551.23433479684     
 iteration          317 MCMCOBJ=   -6561.45751930504     
 iteration          318 MCMCOBJ=   -6543.03162610767     
 iteration          319 MCMCOBJ=   -6566.60921855637     
 iteration          320 MCMCOBJ=   -6559.02867293478     
 iteration          321 MCMCOBJ=   -6563.32798481199     
 iteration          322 MCMCOBJ=   -6538.38998993243     
 iteration          323 MCMCOBJ=   -6492.63423892831     
 iteration          324 MCMCOBJ=   -6537.95608230163     
 iteration          325 MCMCOBJ=   -6528.05274607590     
 iteration          326 MCMCOBJ=   -6567.19806493029     
 iteration          327 MCMCOBJ=   -6567.19807145636     
 iteration          328 MCMCOBJ=   -6591.06185584355     
 iteration          329 MCMCOBJ=   -6568.02761662511     
 iteration          330 MCMCOBJ=   -6545.16717247118     
 iteration          331 MCMCOBJ=   -6563.87924423481     
 iteration          332 MCMCOBJ=   -6578.73315184020     
 iteration          333 MCMCOBJ=   -6517.42309324463     
 iteration          334 MCMCOBJ=   -6496.13101820635     
 iteration          335 MCMCOBJ=   -6529.02850084714     
 iteration          336 MCMCOBJ=   -6491.95025587465     
 iteration          337 MCMCOBJ=   -6471.99767553032     
 iteration          338 MCMCOBJ=   -6515.91160658172     
 iteration          339 MCMCOBJ=   -6517.02603437669     
 iteration          340 MCMCOBJ=   -6504.91138259695     
 iteration          341 MCMCOBJ=   -6538.00664651211     
 iteration          342 MCMCOBJ=   -6529.11571483860     
 iteration          343 MCMCOBJ=   -6531.66665096365     
 iteration          344 MCMCOBJ=   -6564.98250150252     
 iteration          345 MCMCOBJ=   -6501.11525583399     
 iteration          346 MCMCOBJ=   -6467.66617458449     
 iteration          347 MCMCOBJ=   -6532.53709293240     
 iteration          348 MCMCOBJ=   -6497.05022974484     
 iteration          349 MCMCOBJ=   -6495.85135073288     
 iteration          350 MCMCOBJ=   -6496.48772125599     
 iteration          351 MCMCOBJ=   -6495.20922324640     
 iteration          352 MCMCOBJ=   -6567.87912786282     
 iteration          353 MCMCOBJ=   -6562.97160883725     
 iteration          354 MCMCOBJ=   -6582.98147067391     
 iteration          355 MCMCOBJ=   -6539.26111591252     
 iteration          356 MCMCOBJ=   -6497.66528633951     
 iteration          357 MCMCOBJ=   -6524.13633314797     
 iteration          358 MCMCOBJ=   -6525.77722124220     
 iteration          359 MCMCOBJ=   -6487.53959026927     
 iteration          360 MCMCOBJ=   -6519.35614835247     
 iteration          361 MCMCOBJ=   -6534.14092717667     
 iteration          362 MCMCOBJ=   -6517.25146613160     
 iteration          363 MCMCOBJ=   -6483.47283239194     
 iteration          364 MCMCOBJ=   -6446.58168074122     
 iteration          365 MCMCOBJ=   -6454.05286656426     
 iteration          366 MCMCOBJ=   -6455.06423291539     
 iteration          367 MCMCOBJ=   -6482.09433286106     
 iteration          368 MCMCOBJ=   -6481.52406328464     
 iteration          369 MCMCOBJ=   -6521.36858744088     
 iteration          370 MCMCOBJ=   -6527.95773550699     
 iteration          371 MCMCOBJ=   -6552.87503900871     
 iteration          372 MCMCOBJ=   -6486.79529360345     
 iteration          373 MCMCOBJ=   -6477.89465036233     
 iteration          374 MCMCOBJ=   -6460.17424296092     
 iteration          375 MCMCOBJ=   -6457.86934050413     
 iteration          376 MCMCOBJ=   -6459.47399837436     
 iteration          377 MCMCOBJ=   -6446.12108324498     
 iteration          378 MCMCOBJ=   -6437.63323636138     
 iteration          379 MCMCOBJ=   -6494.50913766821     
 iteration          380 MCMCOBJ=   -6465.06643391591     
 iteration          381 MCMCOBJ=   -6516.71973413798     
 iteration          382 MCMCOBJ=   -6537.67940962126     
 iteration          383 MCMCOBJ=   -6538.45267425040     
 iteration          384 MCMCOBJ=   -6460.81778741361     
 iteration          385 MCMCOBJ=   -6455.94616029538     
 iteration          386 MCMCOBJ=   -6514.85017996092     
 iteration          387 MCMCOBJ=   -6523.66400349578     
 iteration          388 MCMCOBJ=   -6515.20566220351     
 iteration          389 MCMCOBJ=   -6447.05117448571     
 iteration          390 MCMCOBJ=   -6470.32340948467     
 iteration          391 MCMCOBJ=   -6497.69385889783     
 iteration          392 MCMCOBJ=   -6494.69338857166     
 iteration          393 MCMCOBJ=   -6521.36884295784     
 iteration          394 MCMCOBJ=   -6521.36878846986     
 iteration          395 MCMCOBJ=   -6509.32572956709     
 iteration          396 MCMCOBJ=   -6502.34196693940     
 iteration          397 MCMCOBJ=   -6554.26574863700     
 iteration          398 MCMCOBJ=   -6539.77847484409     
 iteration          399 MCMCOBJ=   -6545.71603164392     
 iteration          400 MCMCOBJ=   -6560.86464811910     
 iteration          401 MCMCOBJ=   -6560.86466690886     
 iteration          402 MCMCOBJ=   -6547.38336780378     
 iteration          403 MCMCOBJ=   -6558.25915638690     
 iteration          404 MCMCOBJ=   -6506.54345329653     
 iteration          405 MCMCOBJ=   -6469.88635774461     
 iteration          406 MCMCOBJ=   -6527.13667156573     
 iteration          407 MCMCOBJ=   -6543.89055331313     
 iteration          408 MCMCOBJ=   -6557.92061667022     
 iteration          409 MCMCOBJ=   -6526.58600657724     
 iteration          410 MCMCOBJ=   -6497.92158551459     
 iteration          411 MCMCOBJ=   -6516.03003758358     
 iteration          412 MCMCOBJ=   -6575.62415225071     
 iteration          413 MCMCOBJ=   -6482.78857261154     
 iteration          414 MCMCOBJ=   -6509.25287125769     
 iteration          415 MCMCOBJ=   -6516.25994197532     
 iteration          416 MCMCOBJ=   -6545.61269728124     
 iteration          417 MCMCOBJ=   -6500.55761282450     
 iteration          418 MCMCOBJ=   -6467.84405673124     
 iteration          419 MCMCOBJ=   -6474.22720061298     
 iteration          420 MCMCOBJ=   -6532.25098116394     
 iteration          421 MCMCOBJ=   -6475.33678849956     
 iteration          422 MCMCOBJ=   -6491.32377746694     
 iteration          423 MCMCOBJ=   -6483.10927044529     
 iteration          424 MCMCOBJ=   -6503.37838090268     
 iteration          425 MCMCOBJ=   -6458.66487863284     
 iteration          426 MCMCOBJ=   -6482.58274836193     
 iteration          427 MCMCOBJ=   -6477.65904582132     
 iteration          428 MCMCOBJ=   -6496.57384641160     
 iteration          429 MCMCOBJ=   -6499.23217594409     
 iteration          430 MCMCOBJ=   -6545.76998297313     
 iteration          431 MCMCOBJ=   -6539.30335074900     
 iteration          432 MCMCOBJ=   -6504.91868655331     
 iteration          433 MCMCOBJ=   -6546.03114739946     
 iteration          434 MCMCOBJ=   -6583.83616562494     
 iteration          435 MCMCOBJ=   -6542.12601351720     
 iteration          436 MCMCOBJ=   -6565.68391653654     
 iteration          437 MCMCOBJ=   -6507.50693831768     
 iteration          438 MCMCOBJ=   -6515.46279399840     
 iteration          439 MCMCOBJ=   -6539.03888956473     
 iteration          440 MCMCOBJ=   -6545.49244978232     
 iteration          441 MCMCOBJ=   -6545.49245043186     
 iteration          442 MCMCOBJ=   -6574.81844319557     
 iteration          443 MCMCOBJ=   -6506.16113206423     
 iteration          444 MCMCOBJ=   -6504.75862521890     
 iteration          445 MCMCOBJ=   -6515.59213497464     
 iteration          446 MCMCOBJ=   -6545.71822858681     
 iteration          447 MCMCOBJ=   -6482.68885555810     
 iteration          448 MCMCOBJ=   -6493.06001765860     
 iteration          449 MCMCOBJ=   -6497.83010971110     
 iteration          450 MCMCOBJ=   -6504.73838887291     
 iteration          451 MCMCOBJ=   -6479.33570346542     
 iteration          452 MCMCOBJ=   -6447.17190006464     
 iteration          453 MCMCOBJ=   -6489.65786247253     
 iteration          454 MCMCOBJ=   -6510.75083328443     
 iteration          455 MCMCOBJ=   -6476.34339300051     
 iteration          456 MCMCOBJ=   -6466.68853206729     
 iteration          457 MCMCOBJ=   -6454.76641844407     
 iteration          458 MCMCOBJ=   -6446.24281309670     
 iteration          459 MCMCOBJ=   -6509.07930875506     
 iteration          460 MCMCOBJ=   -6484.07987015948     
 iteration          461 MCMCOBJ=   -6527.51383544374     
 iteration          462 MCMCOBJ=   -6497.27694597510     
 iteration          463 MCMCOBJ=   -6468.17640793131     
 iteration          464 MCMCOBJ=   -6507.95185563120     
 iteration          465 MCMCOBJ=   -6512.87026298605     
 iteration          466 MCMCOBJ=   -6538.71554581875     
 iteration          467 MCMCOBJ=   -6508.43674885398     
 iteration          468 MCMCOBJ=   -6539.48753962224     
 iteration          469 MCMCOBJ=   -6559.13279949253     
 iteration          470 MCMCOBJ=   -6516.29641855367     
 iteration          471 MCMCOBJ=   -6529.42388972111     
 iteration          472 MCMCOBJ=   -6531.27839919304     
 iteration          473 MCMCOBJ=   -6531.78481842016     
 iteration          474 MCMCOBJ=   -6517.09883524178     
 iteration          475 MCMCOBJ=   -6518.92269166115     
 iteration          476 MCMCOBJ=   -6518.92266152078     
 iteration          477 MCMCOBJ=   -6524.93624960524     
 iteration          478 MCMCOBJ=   -6518.60511518173     
 iteration          479 MCMCOBJ=   -6521.11702263880     
 iteration          480 MCMCOBJ=   -6512.93766120295     
 iteration          481 MCMCOBJ=   -6499.42072186880     
 iteration          482 MCMCOBJ=   -6525.72961138054     
 iteration          483 MCMCOBJ=   -6590.21366415038     
 iteration          484 MCMCOBJ=   -6540.45933226365     
 iteration          485 MCMCOBJ=   -6529.96382667738     
 iteration          486 MCMCOBJ=   -6533.42745770514     
 iteration          487 MCMCOBJ=   -6548.57020425514     
 iteration          488 MCMCOBJ=   -6530.21551550504     
 iteration          489 MCMCOBJ=   -6490.76837683713     
 iteration          490 MCMCOBJ=   -6497.20396140456     
 iteration          491 MCMCOBJ=   -6484.91972661231     
 iteration          492 MCMCOBJ=   -6508.05271878920     
 iteration          493 MCMCOBJ=   -6539.41528374185     
 iteration          494 MCMCOBJ=   -6569.17369443551     
 iteration          495 MCMCOBJ=   -6550.38552802227     
 iteration          496 MCMCOBJ=   -6498.59839096254     
 iteration          497 MCMCOBJ=   -6527.42474377529     
 iteration          498 MCMCOBJ=   -6545.59560644702     
 iteration          499 MCMCOBJ=   -6532.95608334659     
 iteration          500 MCMCOBJ=   -6517.33421691949     
 iteration          501 MCMCOBJ=   -6496.62241782470     
 iteration          502 MCMCOBJ=   -6494.59384770485     
 iteration          503 MCMCOBJ=   -6478.92947641449     
 iteration          504 MCMCOBJ=   -6439.11024594735     
 iteration          505 MCMCOBJ=   -6491.73116331090     
 iteration          506 MCMCOBJ=   -6489.81537774901     
 iteration          507 MCMCOBJ=   -6519.45720656429     
 iteration          508 MCMCOBJ=   -6487.82539184324     
 iteration          509 MCMCOBJ=   -6489.08269989751     
 iteration          510 MCMCOBJ=   -6478.42008925883     
 iteration          511 MCMCOBJ=   -6462.89308607237     
 iteration          512 MCMCOBJ=   -6510.45600833444     
 iteration          513 MCMCOBJ=   -6508.44855740411     
 iteration          514 MCMCOBJ=   -6539.94475017494     
 iteration          515 MCMCOBJ=   -6476.43419008563     
 iteration          516 MCMCOBJ=   -6425.35907436142     
 iteration          517 MCMCOBJ=   -6487.07848473446     
 iteration          518 MCMCOBJ=   -6502.34502203251     
 iteration          519 MCMCOBJ=   -6447.03390496689     
 iteration          520 MCMCOBJ=   -6546.00431802538     
 iteration          521 MCMCOBJ=   -6527.95591822662     
 iteration          522 MCMCOBJ=   -6569.45758008360     
 iteration          523 MCMCOBJ=   -6594.70191623802     
 iteration          524 MCMCOBJ=   -6587.87431928862     
 iteration          525 MCMCOBJ=   -6587.87431874337     
 iteration          526 MCMCOBJ=   -6500.59204044893     
 iteration          527 MCMCOBJ=   -6481.75499510926     
 iteration          528 MCMCOBJ=   -6480.04118435432     
 iteration          529 MCMCOBJ=   -6465.18495176191     
 iteration          530 MCMCOBJ=   -6497.19370186265     
 iteration          531 MCMCOBJ=   -6472.66345422142     
 iteration          532 MCMCOBJ=   -6482.19439697429     
 iteration          533 MCMCOBJ=   -6431.52731714233     
 iteration          534 MCMCOBJ=   -6432.58852935299     
 iteration          535 MCMCOBJ=   -6480.38792290813     
 iteration          536 MCMCOBJ=   -6526.16652605771     
 iteration          537 MCMCOBJ=   -6509.09428445553     
 iteration          538 MCMCOBJ=   -6523.80514663514     
 iteration          539 MCMCOBJ=   -6522.24621248294     
 iteration          540 MCMCOBJ=   -6497.07596556338     
 iteration          541 MCMCOBJ=   -6472.48036599666     
 iteration          542 MCMCOBJ=   -6474.49299813393     
 iteration          543 MCMCOBJ=   -6448.87326973150     
 iteration          544 MCMCOBJ=   -6472.51541511079     
 iteration          545 MCMCOBJ=   -6566.55643467620     
 iteration          546 MCMCOBJ=   -6498.06301207449     
 iteration          547 MCMCOBJ=   -6557.76296954030     
 iteration          548 MCMCOBJ=   -6537.17476314557     
 iteration          549 MCMCOBJ=   -6517.95630741002     
 iteration          550 MCMCOBJ=   -6490.42120588445     
 iteration          551 MCMCOBJ=   -6502.38967347130     
 iteration          552 MCMCOBJ=   -6502.38967351157     
 iteration          553 MCMCOBJ=   -6498.97230376322     
 iteration          554 MCMCOBJ=   -6480.24501019765     
 iteration          555 MCMCOBJ=   -6456.82300504597     
 iteration          556 MCMCOBJ=   -6490.24317124023     
 iteration          557 MCMCOBJ=   -6469.89010529803     
 iteration          558 MCMCOBJ=   -6432.35660537901     
 iteration          559 MCMCOBJ=   -6502.20194376265     
 iteration          560 MCMCOBJ=   -6467.83910343171     
 iteration          561 MCMCOBJ=   -6497.34665452665     
 iteration          562 MCMCOBJ=   -6484.12413025880     
 iteration          563 MCMCOBJ=   -6484.77712423720     
 iteration          564 MCMCOBJ=   -6498.83551598313     
 iteration          565 MCMCOBJ=   -6467.33505175314     
 iteration          566 MCMCOBJ=   -6486.73189476738     
 iteration          567 MCMCOBJ=   -6513.01406158005     
 iteration          568 MCMCOBJ=   -6491.79872363098     
 iteration          569 MCMCOBJ=   -6485.30608098803     
 iteration          570 MCMCOBJ=   -6514.85436433847     
 iteration          571 MCMCOBJ=   -6477.61202879754     
 iteration          572 MCMCOBJ=   -6519.75915998686     
 iteration          573 MCMCOBJ=   -6472.54559280762     
 iteration          574 MCMCOBJ=   -6487.85635785918     
 iteration          575 MCMCOBJ=   -6434.50149071195     
 iteration          576 MCMCOBJ=   -6498.00740244638     
 iteration          577 MCMCOBJ=   -6480.44430505496     
 iteration          578 MCMCOBJ=   -6480.44430610068     
 iteration          579 MCMCOBJ=   -6492.41550448786     
 iteration          580 MCMCOBJ=   -6524.30768702815     
 iteration          581 MCMCOBJ=   -6499.78160459134     
 iteration          582 MCMCOBJ=   -6501.24148555620     
 iteration          583 MCMCOBJ=   -6536.84327115857     
 iteration          584 MCMCOBJ=   -6540.89523248183     
 iteration          585 MCMCOBJ=   -6531.58578386027     
 iteration          586 MCMCOBJ=   -6482.12088413622     
 iteration          587 MCMCOBJ=   -6489.88460772431     
 iteration          588 MCMCOBJ=   -6495.00893459163     
 iteration          589 MCMCOBJ=   -6478.32468568206     
 iteration          590 MCMCOBJ=   -6512.60416075195     
 iteration          591 MCMCOBJ=   -6484.41106715038     
 iteration          592 MCMCOBJ=   -6470.86801191336     
 iteration          593 MCMCOBJ=   -6458.11130194870     
 iteration          594 MCMCOBJ=   -6531.18149729407     
 iteration          595 MCMCOBJ=   -6556.27968305443     
 iteration          596 MCMCOBJ=   -6570.08112747868     
 iteration          597 MCMCOBJ=   -6542.88749977970     
 iteration          598 MCMCOBJ=   -6542.88750176808     
 iteration          599 MCMCOBJ=   -6581.74152696122     
 iteration          600 MCMCOBJ=   -6514.62579710959     
 iteration          601 MCMCOBJ=   -6523.12451657136     
 iteration          602 MCMCOBJ=   -6504.10045381970     
 iteration          603 MCMCOBJ=   -6504.10047132470     
 iteration          604 MCMCOBJ=   -6538.74195995787     
 iteration          605 MCMCOBJ=   -6515.27970557202     
 iteration          606 MCMCOBJ=   -6512.83865427421     
 iteration          607 MCMCOBJ=   -6474.97092016589     
 iteration          608 MCMCOBJ=   -6480.52279319754     
 iteration          609 MCMCOBJ=   -6430.51504027625     
 iteration          610 MCMCOBJ=   -6504.40859573669     
 iteration          611 MCMCOBJ=   -6467.27957455784     
 iteration          612 MCMCOBJ=   -6522.98375037290     
 iteration          613 MCMCOBJ=   -6483.68173501556     
 iteration          614 MCMCOBJ=   -6502.58177113558     
 iteration          615 MCMCOBJ=   -6519.75153757082     
 iteration          616 MCMCOBJ=   -6537.12281667101     
 iteration          617 MCMCOBJ=   -6528.92458203394     
 iteration          618 MCMCOBJ=   -6512.38684396942     
 iteration          619 MCMCOBJ=   -6513.68505481545     
 iteration          620 MCMCOBJ=   -6532.10200180987     
 iteration          621 MCMCOBJ=   -6492.36620926477     
 iteration          622 MCMCOBJ=   -6496.28654539538     
 iteration          623 MCMCOBJ=   -6484.36453894047     
 iteration          624 MCMCOBJ=   -6464.17507447648     
 iteration          625 MCMCOBJ=   -6466.06002866590     
 iteration          626 MCMCOBJ=   -6522.15115351810     
 iteration          627 MCMCOBJ=   -6526.41691180631     
 iteration          628 MCMCOBJ=   -6493.41927767595     
 iteration          629 MCMCOBJ=   -6541.25096343358     
 iteration          630 MCMCOBJ=   -6511.82366991237     
 iteration          631 MCMCOBJ=   -6503.80474776039     
 iteration          632 MCMCOBJ=   -6517.65243502072     
 iteration          633 MCMCOBJ=   -6495.42891866980     
 iteration          634 MCMCOBJ=   -6505.98656194595     
 iteration          635 MCMCOBJ=   -6510.15657813609     
 iteration          636 MCMCOBJ=   -6544.35564869514     
 iteration          637 MCMCOBJ=   -6500.91039279192     
 iteration          638 MCMCOBJ=   -6472.66167786660     
 iteration          639 MCMCOBJ=   -6469.24983878722     
 iteration          640 MCMCOBJ=   -6511.47422507172     
 iteration          641 MCMCOBJ=   -6499.66698933438     
 iteration          642 MCMCOBJ=   -6468.48788468927     
 iteration          643 MCMCOBJ=   -6431.23988658985     
 iteration          644 MCMCOBJ=   -6463.40110963821     
 iteration          645 MCMCOBJ=   -6486.72362072472     
 iteration          646 MCMCOBJ=   -6502.34839067886     
 iteration          647 MCMCOBJ=   -6506.53780468324     
 iteration          648 MCMCOBJ=   -6549.96536601836     
 iteration          649 MCMCOBJ=   -6514.51117097754     
 iteration          650 MCMCOBJ=   -6558.08619027995     
 iteration          651 MCMCOBJ=   -6567.50748907717     
 iteration          652 MCMCOBJ=   -6565.98559843017     
 iteration          653 MCMCOBJ=   -6557.87271559354     
 iteration          654 MCMCOBJ=   -6550.51650489371     
 iteration          655 MCMCOBJ=   -6560.08402589308     
 iteration          656 MCMCOBJ=   -6517.30490599973     
 iteration          657 MCMCOBJ=   -6502.82766640974     
 iteration          658 MCMCOBJ=   -6526.70289104090     
 iteration          659 MCMCOBJ=   -6584.80831250177     
 iteration          660 MCMCOBJ=   -6559.52404922980     
 iteration          661 MCMCOBJ=   -6500.75024086681     
 iteration          662 MCMCOBJ=   -6485.70785903909     
 iteration          663 MCMCOBJ=   -6452.40427373301     
 iteration          664 MCMCOBJ=   -6506.15533367490     
 iteration          665 MCMCOBJ=   -6519.07418270342     
 iteration          666 MCMCOBJ=   -6522.46335968329     
 iteration          667 MCMCOBJ=   -6539.41400821208     
 iteration          668 MCMCOBJ=   -6512.70057915984     
 iteration          669 MCMCOBJ=   -6533.58085317314     
 iteration          670 MCMCOBJ=   -6533.58085396823     
 iteration          671 MCMCOBJ=   -6533.31269318146     
 iteration          672 MCMCOBJ=   -6495.06404799051     
 iteration          673 MCMCOBJ=   -6482.52126943226     
 iteration          674 MCMCOBJ=   -6532.73033984564     
 iteration          675 MCMCOBJ=   -6532.73034119173     
 iteration          676 MCMCOBJ=   -6542.53225329360     
 iteration          677 MCMCOBJ=   -6502.47424284827     
 iteration          678 MCMCOBJ=   -6441.45308700874     
 iteration          679 MCMCOBJ=   -6484.37203926454     
 iteration          680 MCMCOBJ=   -6479.87132522937     
 iteration          681 MCMCOBJ=   -6530.41546286738     
 iteration          682 MCMCOBJ=   -6506.82137466923     
 iteration          683 MCMCOBJ=   -6493.86305218794     
 iteration          684 MCMCOBJ=   -6514.81016441599     
 iteration          685 MCMCOBJ=   -6491.18810973182     
 iteration          686 MCMCOBJ=   -6443.01395341153     
 iteration          687 MCMCOBJ=   -6474.25233012464     
 iteration          688 MCMCOBJ=   -6473.77024899361     
 iteration          689 MCMCOBJ=   -6435.23735395110     
 iteration          690 MCMCOBJ=   -6488.40043795189     
 iteration          691 MCMCOBJ=   -6449.00106186901     
 iteration          692 MCMCOBJ=   -6460.07393347491     
 iteration          693 MCMCOBJ=   -6469.89966590667     
 iteration          694 MCMCOBJ=   -6483.36878928140     
 iteration          695 MCMCOBJ=   -6477.89537152960     
 iteration          696 MCMCOBJ=   -6470.80332921270     
 iteration          697 MCMCOBJ=   -6480.36848176809     
 iteration          698 MCMCOBJ=   -6511.72420502146     
 iteration          699 MCMCOBJ=   -6466.95089409050     
 iteration          700 MCMCOBJ=   -6450.26547241155     
 iteration          701 MCMCOBJ=   -6402.11044476784     
 iteration          702 MCMCOBJ=   -6500.51931818139     
 iteration          703 MCMCOBJ=   -6486.98178024786     
 iteration          704 MCMCOBJ=   -6504.35703362108     
 iteration          705 MCMCOBJ=   -6491.94824947239     
 iteration          706 MCMCOBJ=   -6468.69791402181     
 iteration          707 MCMCOBJ=   -6449.63248383719     
 iteration          708 MCMCOBJ=   -6452.01509606516     
 iteration          709 MCMCOBJ=   -6446.85952883872     
 iteration          710 MCMCOBJ=   -6453.35198486256     
 iteration          711 MCMCOBJ=   -6549.56136434196     
 iteration          712 MCMCOBJ=   -6538.32544725209     
 iteration          713 MCMCOBJ=   -6550.61301985600     
 iteration          714 MCMCOBJ=   -6505.51798093916     
 iteration          715 MCMCOBJ=   -6503.92988921988     
 iteration          716 MCMCOBJ=   -6496.98518931007     
 iteration          717 MCMCOBJ=   -6498.27763477240     
 iteration          718 MCMCOBJ=   -6480.59049735497     
 iteration          719 MCMCOBJ=   -6501.29945095201     
 iteration          720 MCMCOBJ=   -6554.89826037348     
 iteration          721 MCMCOBJ=   -6492.61330855184     
 iteration          722 MCMCOBJ=   -6544.67441629457     
 iteration          723 MCMCOBJ=   -6470.42414225801     
 iteration          724 MCMCOBJ=   -6468.30756563145     
 iteration          725 MCMCOBJ=   -6532.54238868696     
 iteration          726 MCMCOBJ=   -6410.89685092204     
 iteration          727 MCMCOBJ=   -6473.95287753638     
 iteration          728 MCMCOBJ=   -6497.68385803201     
 iteration          729 MCMCOBJ=   -6525.73780304588     
 iteration          730 MCMCOBJ=   -6492.94402332620     
 iteration          731 MCMCOBJ=   -6456.62145212941     
 iteration          732 MCMCOBJ=   -6469.05595120426     
 iteration          733 MCMCOBJ=   -6519.06558278224     
 iteration          734 MCMCOBJ=   -6589.79327729418     
 iteration          735 MCMCOBJ=   -6511.18535134779     
 iteration          736 MCMCOBJ=   -6508.76978991454     
 iteration          737 MCMCOBJ=   -6467.96091045085     
 iteration          738 MCMCOBJ=   -6434.99216262074     
 iteration          739 MCMCOBJ=   -6450.30436237555     
 iteration          740 MCMCOBJ=   -6475.47360748317     
 iteration          741 MCMCOBJ=   -6450.64033980242     
 iteration          742 MCMCOBJ=   -6521.53217405776     
 iteration          743 MCMCOBJ=   -6558.42947625081     
 iteration          744 MCMCOBJ=   -6497.28529187110     
 iteration          745 MCMCOBJ=   -6509.14843988193     
 iteration          746 MCMCOBJ=   -6526.01355497221     
 iteration          747 MCMCOBJ=   -6482.41197982241     
 iteration          748 MCMCOBJ=   -6445.73097054881     
 iteration          749 MCMCOBJ=   -6444.71472436527     
 iteration          750 MCMCOBJ=   -6461.29326343731     
 iteration          751 MCMCOBJ=   -6465.97348194831     
 iteration          752 MCMCOBJ=   -6485.96826750255     
 iteration          753 MCMCOBJ=   -6501.72343270991     
 iteration          754 MCMCOBJ=   -6478.00734509781     
 iteration          755 MCMCOBJ=   -6463.24412147767     
 iteration          756 MCMCOBJ=   -6480.51010715174     
 iteration          757 MCMCOBJ=   -6522.72609942926     
 iteration          758 MCMCOBJ=   -6507.74200880962     
 iteration          759 MCMCOBJ=   -6515.65242041184     
 iteration          760 MCMCOBJ=   -6530.88609589523     
 iteration          761 MCMCOBJ=   -6498.08587005658     
 iteration          762 MCMCOBJ=   -6487.96611639567     
 iteration          763 MCMCOBJ=   -6419.90625811651     
 iteration          764 MCMCOBJ=   -6472.12786220299     
 iteration          765 MCMCOBJ=   -6476.30477765572     
 iteration          766 MCMCOBJ=   -6490.73739779309     
 iteration          767 MCMCOBJ=   -6505.81015178268     
 iteration          768 MCMCOBJ=   -6519.86466653436     
 iteration          769 MCMCOBJ=   -6425.35193420846     
 iteration          770 MCMCOBJ=   -6445.16898036791     
 iteration          771 MCMCOBJ=   -6426.85664168282     
 iteration          772 MCMCOBJ=   -6429.19414387100     
 iteration          773 MCMCOBJ=   -6462.67935459216     
 iteration          774 MCMCOBJ=   -6478.40971483848     
 iteration          775 MCMCOBJ=   -6467.25951585751     
 iteration          776 MCMCOBJ=   -6449.83426407940     
 iteration          777 MCMCOBJ=   -6514.04831232715     
 iteration          778 MCMCOBJ=   -6513.80416742981     
 iteration          779 MCMCOBJ=   -6494.72249897835     
 iteration          780 MCMCOBJ=   -6494.72249889720     
 iteration          781 MCMCOBJ=   -6526.78252181558     
 iteration          782 MCMCOBJ=   -6478.64449267598     
 iteration          783 MCMCOBJ=   -6463.42801425847     
 iteration          784 MCMCOBJ=   -6491.44245875055     
 iteration          785 MCMCOBJ=   -6524.81691618383     
 iteration          786 MCMCOBJ=   -6530.59308406290     
 iteration          787 MCMCOBJ=   -6502.29487733586     
 iteration          788 MCMCOBJ=   -6520.21086270364     
 iteration          789 MCMCOBJ=   -6521.23268238395     
 iteration          790 MCMCOBJ=   -6536.82254887077     
 iteration          791 MCMCOBJ=   -6536.82249761869     
 iteration          792 MCMCOBJ=   -6452.39190129645     
 iteration          793 MCMCOBJ=   -6417.36767759936     
 iteration          794 MCMCOBJ=   -6449.60689380347     
 iteration          795 MCMCOBJ=   -6461.07540961287     
 iteration          796 MCMCOBJ=   -6478.25955857059     
 iteration          797 MCMCOBJ=   -6460.20990752494     
 iteration          798 MCMCOBJ=   -6477.05191030785     
 iteration          799 MCMCOBJ=   -6525.95067890034     
 iteration          800 MCMCOBJ=   -6516.78677597081     
 iteration          801 MCMCOBJ=   -6454.11569551555     
 iteration          802 MCMCOBJ=   -6506.57745478638     
 iteration          803 MCMCOBJ=   -6499.61521519036     
 iteration          804 MCMCOBJ=   -6489.46120991207     
 iteration          805 MCMCOBJ=   -6527.23682004644     
 iteration          806 MCMCOBJ=   -6541.22434085924     
 iteration          807 MCMCOBJ=   -6554.11097003351     
 iteration          808 MCMCOBJ=   -6507.39903521203     
 iteration          809 MCMCOBJ=   -6498.37066730211     
 iteration          810 MCMCOBJ=   -6519.54622073239     
 iteration          811 MCMCOBJ=   -6427.77789941500     
 iteration          812 MCMCOBJ=   -6480.09464152960     
 iteration          813 MCMCOBJ=   -6533.03740635104     
 iteration          814 MCMCOBJ=   -6564.65175037799     
 iteration          815 MCMCOBJ=   -6469.21578577136     
 iteration          816 MCMCOBJ=   -6497.17692090327     
 iteration          817 MCMCOBJ=   -6507.51818938906     
 iteration          818 MCMCOBJ=   -6529.01165999725     
 iteration          819 MCMCOBJ=   -6496.16979626526     
 iteration          820 MCMCOBJ=   -6499.18888432409     
 iteration          821 MCMCOBJ=   -6500.33103445931     
 iteration          822 MCMCOBJ=   -6501.57209529792     
 iteration          823 MCMCOBJ=   -6506.49141773739     
 iteration          824 MCMCOBJ=   -6437.57324902756     
 iteration          825 MCMCOBJ=   -6528.77615292160     
 iteration          826 MCMCOBJ=   -6536.77452089882     
 iteration          827 MCMCOBJ=   -6546.26505574423     
 iteration          828 MCMCOBJ=   -6530.05273923403     
 iteration          829 MCMCOBJ=   -6566.52339946729     
 iteration          830 MCMCOBJ=   -6588.48263349920     
 iteration          831 MCMCOBJ=   -6554.59640584731     
 iteration          832 MCMCOBJ=   -6545.29848668485     
 iteration          833 MCMCOBJ=   -6539.81694379749     
 iteration          834 MCMCOBJ=   -6524.80400578646     
 iteration          835 MCMCOBJ=   -6524.80400581665     
 iteration          836 MCMCOBJ=   -6521.19398483881     
 iteration          837 MCMCOBJ=   -6481.53889501990     
 iteration          838 MCMCOBJ=   -6488.10614543545     
 iteration          839 MCMCOBJ=   -6517.48719663587     
 iteration          840 MCMCOBJ=   -6467.00032516965     
 iteration          841 MCMCOBJ=   -6531.99825347912     
 iteration          842 MCMCOBJ=   -6559.42901414105     
 iteration          843 MCMCOBJ=   -6528.81619202691     
 iteration          844 MCMCOBJ=   -6504.09578438075     
 iteration          845 MCMCOBJ=   -6477.10118604950     
 iteration          846 MCMCOBJ=   -6462.70721950445     
 iteration          847 MCMCOBJ=   -6481.05764362835     
 iteration          848 MCMCOBJ=   -6514.59385079440     
 iteration          849 MCMCOBJ=   -6521.83535723594     
 iteration          850 MCMCOBJ=   -6541.38391244577     
 iteration          851 MCMCOBJ=   -6503.53456255896     
 iteration          852 MCMCOBJ=   -6521.98162415031     
 iteration          853 MCMCOBJ=   -6464.26045579641     
 iteration          854 MCMCOBJ=   -6492.19247808717     
 iteration          855 MCMCOBJ=   -6469.56000671190     
 iteration          856 MCMCOBJ=   -6465.36593490100     
 iteration          857 MCMCOBJ=   -6495.05741540715     
 iteration          858 MCMCOBJ=   -6515.84869927611     
 iteration          859 MCMCOBJ=   -6461.00451811548     
 iteration          860 MCMCOBJ=   -6494.74475930209     
 iteration          861 MCMCOBJ=   -6492.06094250075     
 iteration          862 MCMCOBJ=   -6457.07673605953     
 iteration          863 MCMCOBJ=   -6469.81609195304     
 iteration          864 MCMCOBJ=   -6482.94587053091     
 iteration          865 MCMCOBJ=   -6500.89095101602     
 iteration          866 MCMCOBJ=   -6512.84516113643     
 iteration          867 MCMCOBJ=   -6521.52154134406     
 iteration          868 MCMCOBJ=   -6498.62025955723     
 iteration          869 MCMCOBJ=   -6512.97393739776     
 iteration          870 MCMCOBJ=   -6521.44117059768     
 iteration          871 MCMCOBJ=   -6512.07397594288     
 iteration          872 MCMCOBJ=   -6477.04472474701     
 iteration          873 MCMCOBJ=   -6510.92542779555     
 iteration          874 MCMCOBJ=   -6512.31779793885     
 iteration          875 MCMCOBJ=   -6463.91593505922     
 iteration          876 MCMCOBJ=   -6522.93327324403     
 iteration          877 MCMCOBJ=   -6496.95304482478     
 iteration          878 MCMCOBJ=   -6510.09179699738     
 iteration          879 MCMCOBJ=   -6533.82576490244     
 iteration          880 MCMCOBJ=   -6504.01589418249     
 iteration          881 MCMCOBJ=   -6526.34123078936     
 iteration          882 MCMCOBJ=   -6526.34123146352     
 iteration          883 MCMCOBJ=   -6514.60395620390     
 iteration          884 MCMCOBJ=   -6470.56026672254     
 iteration          885 MCMCOBJ=   -6500.33398972284     
 iteration          886 MCMCOBJ=   -6510.98165688182     
 iteration          887 MCMCOBJ=   -6451.08671368504     
 iteration          888 MCMCOBJ=   -6497.29095977349     
 iteration          889 MCMCOBJ=   -6490.12951613949     
 iteration          890 MCMCOBJ=   -6457.12099940252     
 iteration          891 MCMCOBJ=   -6527.14539105085     
 iteration          892 MCMCOBJ=   -6540.13311466934     
 iteration          893 MCMCOBJ=   -6536.78181128743     
 iteration          894 MCMCOBJ=   -6491.12294618984     
 iteration          895 MCMCOBJ=   -6467.17050652173     
 iteration          896 MCMCOBJ=   -6518.10739524591     
 iteration          897 MCMCOBJ=   -6539.03868404862     
 iteration          898 MCMCOBJ=   -6529.37769643893     
 iteration          899 MCMCOBJ=   -6546.86160323527     
 iteration          900 MCMCOBJ=   -6524.13731881211     
 iteration          901 MCMCOBJ=   -6523.49462425877     
 iteration          902 MCMCOBJ=   -6469.98157423931     
 iteration          903 MCMCOBJ=   -6501.52311942014     
 iteration          904 MCMCOBJ=   -6487.99971778479     
 iteration          905 MCMCOBJ=   -6464.50408332277     
 iteration          906 MCMCOBJ=   -6469.32846531077     
 iteration          907 MCMCOBJ=   -6515.25714207400     
 iteration          908 MCMCOBJ=   -6480.15061462979     
 iteration          909 MCMCOBJ=   -6452.63449332844     
 iteration          910 MCMCOBJ=   -6507.67234311261     
 iteration          911 MCMCOBJ=   -6520.01569590120     
 iteration          912 MCMCOBJ=   -6480.18751035005     
 iteration          913 MCMCOBJ=   -6486.63620715295     
 iteration          914 MCMCOBJ=   -6490.38404846161     
 iteration          915 MCMCOBJ=   -6501.46698575352     
 iteration          916 MCMCOBJ=   -6497.26382771729     
 iteration          917 MCMCOBJ=   -6489.21355276345     
 iteration          918 MCMCOBJ=   -6518.50355725523     
 iteration          919 MCMCOBJ=   -6510.61501473881     
 iteration          920 MCMCOBJ=   -6520.04767758417     
 iteration          921 MCMCOBJ=   -6497.00108607016     
 iteration          922 MCMCOBJ=   -6495.92632429851     
 iteration          923 MCMCOBJ=   -6507.86780149243     
 iteration          924 MCMCOBJ=   -6465.39527018409     
 iteration          925 MCMCOBJ=   -6522.59245591943     
 iteration          926 MCMCOBJ=   -6500.97944063070     
 iteration          927 MCMCOBJ=   -6455.49489461323     
 iteration          928 MCMCOBJ=   -6430.73008278663     
 iteration          929 MCMCOBJ=   -6462.99409263893     
 iteration          930 MCMCOBJ=   -6471.08574622455     
 iteration          931 MCMCOBJ=   -6486.62333169872     
 iteration          932 MCMCOBJ=   -6504.22099130822     
 iteration          933 MCMCOBJ=   -6474.89237325056     
 iteration          934 MCMCOBJ=   -6510.01638921080     
 iteration          935 MCMCOBJ=   -6461.19072236953     
 iteration          936 MCMCOBJ=   -6487.58092239284     
 iteration          937 MCMCOBJ=   -6533.47611953543     
 iteration          938 MCMCOBJ=   -6499.98218656031     
 iteration          939 MCMCOBJ=   -6505.91556777255     
 iteration          940 MCMCOBJ=   -6460.39820979557     
 iteration          941 MCMCOBJ=   -6484.17196771759     
 iteration          942 MCMCOBJ=   -6493.66418512305     
 iteration          943 MCMCOBJ=   -6477.72165826628     
 iteration          944 MCMCOBJ=   -6484.57328784051     
 iteration          945 MCMCOBJ=   -6488.47706551892     
 iteration          946 MCMCOBJ=   -6527.83197876775     
 iteration          947 MCMCOBJ=   -6559.64329275894     
 iteration          948 MCMCOBJ=   -6464.22753596625     
 iteration          949 MCMCOBJ=   -6442.98004652768     
 iteration          950 MCMCOBJ=   -6485.66814471618     
 iteration          951 MCMCOBJ=   -6449.42826595654     
 iteration          952 MCMCOBJ=   -6452.28092969597     
 iteration          953 MCMCOBJ=   -6487.11274108875     
 iteration          954 MCMCOBJ=   -6468.75809077729     
 iteration          955 MCMCOBJ=   -6512.24408377821     
 iteration          956 MCMCOBJ=   -6506.08390174300     
 iteration          957 MCMCOBJ=   -6500.21083751958     
 iteration          958 MCMCOBJ=   -6529.11408664831     
 iteration          959 MCMCOBJ=   -6467.14659514424     
 iteration          960 MCMCOBJ=   -6467.14659539207     
 iteration          961 MCMCOBJ=   -6429.15002536365     
 iteration          962 MCMCOBJ=   -6508.77790810198     
 iteration          963 MCMCOBJ=   -6486.67558142513     
 iteration          964 MCMCOBJ=   -6540.96584903126     
 iteration          965 MCMCOBJ=   -6533.96310752188     
 iteration          966 MCMCOBJ=   -6464.90117804785     
 iteration          967 MCMCOBJ=   -6502.28228234500     
 iteration          968 MCMCOBJ=   -6473.93066560614     
 iteration          969 MCMCOBJ=   -6517.50560486267     
 iteration          970 MCMCOBJ=   -6583.18486411147     
 iteration          971 MCMCOBJ=   -6541.12631990585     
 iteration          972 MCMCOBJ=   -6538.93906987478     
 iteration          973 MCMCOBJ=   -6547.09418971629     
 iteration          974 MCMCOBJ=   -6524.63212362582     
 iteration          975 MCMCOBJ=   -6517.78749137850     
 iteration          976 MCMCOBJ=   -6515.46300478589     
 iteration          977 MCMCOBJ=   -6512.98883460125     
 iteration          978 MCMCOBJ=   -6514.76142296641     
 iteration          979 MCMCOBJ=   -6465.43420875068     
 iteration          980 MCMCOBJ=   -6458.17973284426     
 iteration          981 MCMCOBJ=   -6504.01404519443     
 iteration          982 MCMCOBJ=   -6526.93161560500     
 iteration          983 MCMCOBJ=   -6534.67949406514     
 iteration          984 MCMCOBJ=   -6524.02288024524     
 iteration          985 MCMCOBJ=   -6524.02288139755     
 iteration          986 MCMCOBJ=   -6516.50718776654     
 iteration          987 MCMCOBJ=   -6516.50721562458     
 iteration          988 MCMCOBJ=   -6512.37329376324     
 iteration          989 MCMCOBJ=   -6468.55702421682     
 iteration          990 MCMCOBJ=   -6468.14139614352     
 iteration          991 MCMCOBJ=   -6509.80318599341     
 iteration          992 MCMCOBJ=   -6519.85748774947     
 iteration          993 MCMCOBJ=   -6515.53543904269     
 iteration          994 MCMCOBJ=   -6444.84706622975     
 iteration          995 MCMCOBJ=   -6471.90524517796     
 iteration          996 MCMCOBJ=   -6499.27737143564     
 iteration          997 MCMCOBJ=   -6528.70598235772     
 iteration          998 MCMCOBJ=   -6499.89294673447     
 iteration          999 MCMCOBJ=   -6490.67139041583     
 iteration         1000 MCMCOBJ=   -6469.85545237517     
 iteration         1001 MCMCOBJ=   -6468.64990076031     
 iteration         1002 MCMCOBJ=   -6494.70905770248     
 iteration         1003 MCMCOBJ=   -6536.16945502061     
 iteration         1004 MCMCOBJ=   -6523.67305212552     
 iteration         1005 MCMCOBJ=   -6535.40223962753     
 iteration         1006 MCMCOBJ=   -6485.57264840783     
 iteration         1007 MCMCOBJ=   -6468.59824518432     
 iteration         1008 MCMCOBJ=   -6463.44888935596     
 iteration         1009 MCMCOBJ=   -6479.38120499576     
 iteration         1010 MCMCOBJ=   -6538.92628797668     
 iteration         1011 MCMCOBJ=   -6508.15717218659     
 iteration         1012 MCMCOBJ=   -6502.14188828186     
 iteration         1013 MCMCOBJ=   -6502.15610578577     
 iteration         1014 MCMCOBJ=   -6502.91416031776     
 iteration         1015 MCMCOBJ=   -6516.31823028570     
 iteration         1016 MCMCOBJ=   -6507.58328966454     
 iteration         1017 MCMCOBJ=   -6471.73820686612     
 iteration         1018 MCMCOBJ=   -6459.16015411117     
 iteration         1019 MCMCOBJ=   -6516.19961371319     
 iteration         1020 MCMCOBJ=   -6449.33346502976     
 iteration         1021 MCMCOBJ=   -6471.06226733036     
 iteration         1022 MCMCOBJ=   -6489.80893396389     
 iteration         1023 MCMCOBJ=   -6519.08321034495     
 iteration         1024 MCMCOBJ=   -6545.86201341084     
 iteration         1025 MCMCOBJ=   -6558.39011916210     
 iteration         1026 MCMCOBJ=   -6558.39012045751     
 iteration         1027 MCMCOBJ=   -6528.35671417007     
 iteration         1028 MCMCOBJ=   -6513.15744564808     
 iteration         1029 MCMCOBJ=   -6529.66406947206     
 iteration         1030 MCMCOBJ=   -6475.07799042392     
 iteration         1031 MCMCOBJ=   -6489.54354813412     
 iteration         1032 MCMCOBJ=   -6450.79836146041     
 iteration         1033 MCMCOBJ=   -6471.03426453740     
 iteration         1034 MCMCOBJ=   -6508.03256104318     
 iteration         1035 MCMCOBJ=   -6521.80291693861     
 iteration         1036 MCMCOBJ=   -6508.28191694783     
 iteration         1037 MCMCOBJ=   -6570.03332554484     
 iteration         1038 MCMCOBJ=   -6523.11237504490     
 iteration         1039 MCMCOBJ=   -6489.25287021817     
 iteration         1040 MCMCOBJ=   -6485.44199391676     
 iteration         1041 MCMCOBJ=   -6513.35457578264     
 iteration         1042 MCMCOBJ=   -6500.50901075063     
 iteration         1043 MCMCOBJ=   -6534.66868383077     
 iteration         1044 MCMCOBJ=   -6524.37973700069     
 iteration         1045 MCMCOBJ=   -6524.37972223262     
 iteration         1046 MCMCOBJ=   -6473.66049349503     
 iteration         1047 MCMCOBJ=   -6511.08751282089     
 iteration         1048 MCMCOBJ=   -6519.12603466179     
 iteration         1049 MCMCOBJ=   -6533.34027240438     
 iteration         1050 MCMCOBJ=   -6557.71029501602     
 iteration         1051 MCMCOBJ=   -6552.93373148493     
 iteration         1052 MCMCOBJ=   -6567.26447653573     
 iteration         1053 MCMCOBJ=   -6576.68194919514     
 iteration         1054 MCMCOBJ=   -6537.35449933779     
 iteration         1055 MCMCOBJ=   -6472.61968479703     
 iteration         1056 MCMCOBJ=   -6459.51016369888     
 iteration         1057 MCMCOBJ=   -6486.05421237782     
 iteration         1058 MCMCOBJ=   -6481.18439843973     
 iteration         1059 MCMCOBJ=   -6457.02750160141     
 iteration         1060 MCMCOBJ=   -6478.11759083973     
 iteration         1061 MCMCOBJ=   -6477.37274019075     
 iteration         1062 MCMCOBJ=   -6494.61779981516     
 iteration         1063 MCMCOBJ=   -6489.34984451650     
 iteration         1064 MCMCOBJ=   -6461.96054674022     
 iteration         1065 MCMCOBJ=   -6461.76965512410     
 iteration         1066 MCMCOBJ=   -6499.55180713708     
 iteration         1067 MCMCOBJ=   -6508.71828955949     
 iteration         1068 MCMCOBJ=   -6473.13445843978     
 iteration         1069 MCMCOBJ=   -6483.33494403778     
 iteration         1070 MCMCOBJ=   -6457.93010578206     
 iteration         1071 MCMCOBJ=   -6457.07103789853     
 iteration         1072 MCMCOBJ=   -6499.76695579206     
 iteration         1073 MCMCOBJ=   -6486.48263809481     
 iteration         1074 MCMCOBJ=   -6451.58977278420     
 iteration         1075 MCMCOBJ=   -6489.17437291651     
 iteration         1076 MCMCOBJ=   -6520.01650712467     
 iteration         1077 MCMCOBJ=   -6472.77754142458     
 iteration         1078 MCMCOBJ=   -6527.45443758924     
 iteration         1079 MCMCOBJ=   -6487.51119718467     
 iteration         1080 MCMCOBJ=   -6453.58265510052     
 iteration         1081 MCMCOBJ=   -6490.88854035435     
 iteration         1082 MCMCOBJ=   -6523.39159120017     
 iteration         1083 MCMCOBJ=   -6476.76535924859     
 iteration         1084 MCMCOBJ=   -6472.71183621081     
 iteration         1085 MCMCOBJ=   -6467.08758351562     
 iteration         1086 MCMCOBJ=   -6466.59730478261     
 iteration         1087 MCMCOBJ=   -6453.66508829858     
 iteration         1088 MCMCOBJ=   -6494.22880668082     
 iteration         1089 MCMCOBJ=   -6479.50829570853     
 iteration         1090 MCMCOBJ=   -6493.79021998753     
 iteration         1091 MCMCOBJ=   -6474.56069944973     
 iteration         1092 MCMCOBJ=   -6466.90439047169     
 iteration         1093 MCMCOBJ=   -6480.12376320390     
 iteration         1094 MCMCOBJ=   -6499.43554151916     
 iteration         1095 MCMCOBJ=   -6484.87248120463     
 iteration         1096 MCMCOBJ=   -6561.82121341795     
 iteration         1097 MCMCOBJ=   -6512.38493183120     
 iteration         1098 MCMCOBJ=   -6521.56315908105     
 iteration         1099 MCMCOBJ=   -6486.42502016152     
 iteration         1100 MCMCOBJ=   -6475.93888368522     
 iteration         1101 MCMCOBJ=   -6535.70923786197     
 iteration         1102 MCMCOBJ=   -6523.82158940427     
 iteration         1103 MCMCOBJ=   -6494.98913348297     
 iteration         1104 MCMCOBJ=   -6497.75009300313     
 iteration         1105 MCMCOBJ=   -6521.67084412210     
 iteration         1106 MCMCOBJ=   -6556.05795163933     
 iteration         1107 MCMCOBJ=   -6522.02832477475     
 iteration         1108 MCMCOBJ=   -6492.69735108007     
 iteration         1109 MCMCOBJ=   -6525.38142851783     
 iteration         1110 MCMCOBJ=   -6534.73851390758     
 iteration         1111 MCMCOBJ=   -6536.98102716122     
 iteration         1112 MCMCOBJ=   -6543.63322833977     
 iteration         1113 MCMCOBJ=   -6557.67131394148     
 iteration         1114 MCMCOBJ=   -6545.26107864532     
 iteration         1115 MCMCOBJ=   -6531.18666007050     
 iteration         1116 MCMCOBJ=   -6531.04737954754     
 iteration         1117 MCMCOBJ=   -6465.47374083451     
 iteration         1118 MCMCOBJ=   -6498.36458283932     
 iteration         1119 MCMCOBJ=   -6533.33376472847     
 iteration         1120 MCMCOBJ=   -6506.49646622458     
 iteration         1121 MCMCOBJ=   -6552.42376794169     
 iteration         1122 MCMCOBJ=   -6560.13784053039     
 iteration         1123 MCMCOBJ=   -6560.13784260056     
 iteration         1124 MCMCOBJ=   -6560.31917304878     
 iteration         1125 MCMCOBJ=   -6528.15719446030     
 iteration         1126 MCMCOBJ=   -6501.25255337714     
 iteration         1127 MCMCOBJ=   -6488.36384093187     
 iteration         1128 MCMCOBJ=   -6499.05713383382     
 iteration         1129 MCMCOBJ=   -6539.92336156871     
 iteration         1130 MCMCOBJ=   -6524.78888386625     
 iteration         1131 MCMCOBJ=   -6539.75113167477     
 iteration         1132 MCMCOBJ=   -6483.12841624699     
 iteration         1133 MCMCOBJ=   -6462.43179260144     
 iteration         1134 MCMCOBJ=   -6519.43152644924     
 iteration         1135 MCMCOBJ=   -6514.18287779693     
 iteration         1136 MCMCOBJ=   -6542.21574045304     
 iteration         1137 MCMCOBJ=   -6532.19853787917     
 iteration         1138 MCMCOBJ=   -6563.68956420008     
 iteration         1139 MCMCOBJ=   -6553.05609664523     
 iteration         1140 MCMCOBJ=   -6489.17074627861     
 iteration         1141 MCMCOBJ=   -6496.11479675348     
 iteration         1142 MCMCOBJ=   -6504.96692971652     
 iteration         1143 MCMCOBJ=   -6494.56871399203     
 iteration         1144 MCMCOBJ=   -6491.52284936812     
 iteration         1145 MCMCOBJ=   -6459.62362359810     
 iteration         1146 MCMCOBJ=   -6484.71636351520     
 iteration         1147 MCMCOBJ=   -6479.53233884073     
 iteration         1148 MCMCOBJ=   -6466.56762863974     
 iteration         1149 MCMCOBJ=   -6499.66594677488     
 iteration         1150 MCMCOBJ=   -6457.98411640043     
 iteration         1151 MCMCOBJ=   -6475.09380341900     
 iteration         1152 MCMCOBJ=   -6453.11075017071     
 iteration         1153 MCMCOBJ=   -6455.66562928946     
 iteration         1154 MCMCOBJ=   -6464.87922285392     
 iteration         1155 MCMCOBJ=   -6499.45617084841     
 iteration         1156 MCMCOBJ=   -6462.90342339994     
 iteration         1157 MCMCOBJ=   -6479.67883814786     
 iteration         1158 MCMCOBJ=   -6502.01040912637     
 iteration         1159 MCMCOBJ=   -6545.34507325232     
 iteration         1160 MCMCOBJ=   -6492.85580719080     
 iteration         1161 MCMCOBJ=   -6500.64924992417     
 iteration         1162 MCMCOBJ=   -6513.31919290679     
 iteration         1163 MCMCOBJ=   -6523.22387089032     
 iteration         1164 MCMCOBJ=   -6540.01071864339     
 iteration         1165 MCMCOBJ=   -6517.70552172477     
 iteration         1166 MCMCOBJ=   -6535.98960119197     
 iteration         1167 MCMCOBJ=   -6521.45022757845     
 iteration         1168 MCMCOBJ=   -6498.61568129741     
 iteration         1169 MCMCOBJ=   -6480.25356402983     
 iteration         1170 MCMCOBJ=   -6543.17572510327     
 iteration         1171 MCMCOBJ=   -6470.93039599440     
 iteration         1172 MCMCOBJ=   -6459.16970608410     
 iteration         1173 MCMCOBJ=   -6510.13924904774     
 iteration         1174 MCMCOBJ=   -6508.51589794464     
 iteration         1175 MCMCOBJ=   -6510.97831366908     
 iteration         1176 MCMCOBJ=   -6488.71762140578     
 iteration         1177 MCMCOBJ=   -6551.24210652495     
 iteration         1178 MCMCOBJ=   -6541.56321946765     
 iteration         1179 MCMCOBJ=   -6464.82141981398     
 iteration         1180 MCMCOBJ=   -6450.94097933482     
 iteration         1181 MCMCOBJ=   -6479.01862357610     
 iteration         1182 MCMCOBJ=   -6455.72085312947     
 iteration         1183 MCMCOBJ=   -6491.15900158191     
 iteration         1184 MCMCOBJ=   -6481.47269872608     
 iteration         1185 MCMCOBJ=   -6439.16608034014     
 iteration         1186 MCMCOBJ=   -6424.34132045154     
 iteration         1187 MCMCOBJ=   -6405.25222136242     
 iteration         1188 MCMCOBJ=   -6473.42126887650     
 iteration         1189 MCMCOBJ=   -6469.54383066192     
 iteration         1190 MCMCOBJ=   -6506.39335296839     
 iteration         1191 MCMCOBJ=   -6436.41779740203     
 iteration         1192 MCMCOBJ=   -6427.71993540134     
 iteration         1193 MCMCOBJ=   -6497.60732357795     
 iteration         1194 MCMCOBJ=   -6466.74047263264     
 iteration         1195 MCMCOBJ=   -6444.21574421446     
 iteration         1196 MCMCOBJ=   -6413.45553629602     
 iteration         1197 MCMCOBJ=   -6495.30214831686     
 iteration         1198 MCMCOBJ=   -6519.92010986865     
 iteration         1199 MCMCOBJ=   -6470.38635557961     
 iteration         1200 MCMCOBJ=   -6462.25407776928     
 iteration         1201 MCMCOBJ=   -6493.15419933757     
 iteration         1202 MCMCOBJ=   -6492.23384011329     
 iteration         1203 MCMCOBJ=   -6490.35887623998     
 iteration         1204 MCMCOBJ=   -6494.34378666318     
 iteration         1205 MCMCOBJ=   -6560.62187888288     
 iteration         1206 MCMCOBJ=   -6532.95706556583     
 iteration         1207 MCMCOBJ=   -6530.14531931550     
 iteration         1208 MCMCOBJ=   -6506.29282536907     
 iteration         1209 MCMCOBJ=   -6500.75838290763     
 iteration         1210 MCMCOBJ=   -6535.43107457372     
 iteration         1211 MCMCOBJ=   -6507.95054012673     
 iteration         1212 MCMCOBJ=   -6507.95054000156     
 iteration         1213 MCMCOBJ=   -6508.27127404134     
 iteration         1214 MCMCOBJ=   -6522.43662192933     
 iteration         1215 MCMCOBJ=   -6567.99182924831     
 iteration         1216 MCMCOBJ=   -6537.28955791490     
 iteration         1217 MCMCOBJ=   -6486.73228789797     
 iteration         1218 MCMCOBJ=   -6578.97833260779     
 iteration         1219 MCMCOBJ=   -6556.94438575067     
 iteration         1220 MCMCOBJ=   -6491.04679018973     
 iteration         1221 MCMCOBJ=   -6494.29348952051     
 iteration         1222 MCMCOBJ=   -6498.19783754512     
 iteration         1223 MCMCOBJ=   -6498.05035792121     
 iteration         1224 MCMCOBJ=   -6492.77293096821     
 iteration         1225 MCMCOBJ=   -6539.42120776045     
 iteration         1226 MCMCOBJ=   -6497.54202838094     
 iteration         1227 MCMCOBJ=   -6475.59662361599     
 iteration         1228 MCMCOBJ=   -6461.71546003113     
 iteration         1229 MCMCOBJ=   -6476.00573191201     
 iteration         1230 MCMCOBJ=   -6467.67774212700     
 iteration         1231 MCMCOBJ=   -6447.30713312888     
 iteration         1232 MCMCOBJ=   -6390.56200593948     
 iteration         1233 MCMCOBJ=   -6427.75005008473     
 iteration         1234 MCMCOBJ=   -6446.07917617771     
 iteration         1235 MCMCOBJ=   -6465.17463789292     
 iteration         1236 MCMCOBJ=   -6436.50763122036     
 iteration         1237 MCMCOBJ=   -6476.99414620045     
 iteration         1238 MCMCOBJ=   -6476.99414579970     
 iteration         1239 MCMCOBJ=   -6499.48317948129     
 iteration         1240 MCMCOBJ=   -6492.65216708034     
 iteration         1241 MCMCOBJ=   -6437.63439439974     
 iteration         1242 MCMCOBJ=   -6432.85848227828     
 iteration         1243 MCMCOBJ=   -6489.49200929159     
 iteration         1244 MCMCOBJ=   -6486.95475638864     
 iteration         1245 MCMCOBJ=   -6443.46323248005     
 iteration         1246 MCMCOBJ=   -6441.00425738047     
 iteration         1247 MCMCOBJ=   -6517.51151041352     
 iteration         1248 MCMCOBJ=   -6522.13679290279     
 iteration         1249 MCMCOBJ=   -6491.80490469896     
 iteration         1250 MCMCOBJ=   -6521.61360074498     
 iteration         1251 MCMCOBJ=   -6477.22211133843     
 iteration         1252 MCMCOBJ=   -6445.87668174073     
 iteration         1253 MCMCOBJ=   -6502.84910623727     
 iteration         1254 MCMCOBJ=   -6548.02521066757     
 iteration         1255 MCMCOBJ=   -6522.00126599768     
 iteration         1256 MCMCOBJ=   -6541.79115008444     
 iteration         1257 MCMCOBJ=   -6470.74795567243     
 iteration         1258 MCMCOBJ=   -6476.75977441072     
 iteration         1259 MCMCOBJ=   -6487.86802788359     
 iteration         1260 MCMCOBJ=   -6510.11104255412     
 iteration         1261 MCMCOBJ=   -6508.85682852086     
 iteration         1262 MCMCOBJ=   -6494.61206713613     
 iteration         1263 MCMCOBJ=   -6493.54263814909     
 iteration         1264 MCMCOBJ=   -6528.52316896920     
 iteration         1265 MCMCOBJ=   -6510.10965110714     
 iteration         1266 MCMCOBJ=   -6496.32049557714     
 iteration         1267 MCMCOBJ=   -6502.27080678922     
 iteration         1268 MCMCOBJ=   -6471.96940987019     
 iteration         1269 MCMCOBJ=   -6499.77592397749     
 iteration         1270 MCMCOBJ=   -6558.78222518454     
 iteration         1271 MCMCOBJ=   -6568.16374491693     
 iteration         1272 MCMCOBJ=   -6552.45142517661     
 iteration         1273 MCMCOBJ=   -6554.14110128562     
 iteration         1274 MCMCOBJ=   -6515.04193465966     
 iteration         1275 MCMCOBJ=   -6496.77655187627     
 iteration         1276 MCMCOBJ=   -6558.87737049343     
 iteration         1277 MCMCOBJ=   -6523.64577781747     
 iteration         1278 MCMCOBJ=   -6478.13408230524     
 iteration         1279 MCMCOBJ=   -6542.10295614915     
 iteration         1280 MCMCOBJ=   -6533.20714314921     
 iteration         1281 MCMCOBJ=   -6576.50532175084     
 iteration         1282 MCMCOBJ=   -6556.66547581143     
 iteration         1283 MCMCOBJ=   -6593.63719713844     
 iteration         1284 MCMCOBJ=   -6542.20078552506     
 iteration         1285 MCMCOBJ=   -6510.90493590162     
 iteration         1286 MCMCOBJ=   -6497.52718754951     
 iteration         1287 MCMCOBJ=   -6501.65568373025     
 iteration         1288 MCMCOBJ=   -6539.37345636365     
 iteration         1289 MCMCOBJ=   -6478.66199412683     
 iteration         1290 MCMCOBJ=   -6496.77985500523     
 iteration         1291 MCMCOBJ=   -6435.99657344090     
 iteration         1292 MCMCOBJ=   -6473.58746693846     
 iteration         1293 MCMCOBJ=   -6469.64544584909     
 iteration         1294 MCMCOBJ=   -6485.36810459049     
 iteration         1295 MCMCOBJ=   -6428.06101191019     
 iteration         1296 MCMCOBJ=   -6464.24282928804     
 iteration         1297 MCMCOBJ=   -6498.96188304205     
 iteration         1298 MCMCOBJ=   -6500.12049773731     
 iteration         1299 MCMCOBJ=   -6487.48469931776     
 iteration         1300 MCMCOBJ=   -6459.33118794862     
 iteration         1301 MCMCOBJ=   -6486.82772565748     
 iteration         1302 MCMCOBJ=   -6449.72444776676     
 iteration         1303 MCMCOBJ=   -6461.24973504735     
 iteration         1304 MCMCOBJ=   -6437.91366374716     
 iteration         1305 MCMCOBJ=   -6501.52395178777     
 iteration         1306 MCMCOBJ=   -6492.58274703585     
 iteration         1307 MCMCOBJ=   -6484.86083142850     
 iteration         1308 MCMCOBJ=   -6486.95610294786     
 iteration         1309 MCMCOBJ=   -6482.89040907117     
 iteration         1310 MCMCOBJ=   -6476.38195322144     
 iteration         1311 MCMCOBJ=   -6539.86853806872     
 iteration         1312 MCMCOBJ=   -6513.23624546268     
 iteration         1313 MCMCOBJ=   -6523.66237451722     
 iteration         1314 MCMCOBJ=   -6520.33589657153     
 iteration         1315 MCMCOBJ=   -6513.15480782329     
 iteration         1316 MCMCOBJ=   -6561.33567959054     
 iteration         1317 MCMCOBJ=   -6441.88063552335     
 iteration         1318 MCMCOBJ=   -6429.54404022422     
 iteration         1319 MCMCOBJ=   -6468.10832465296     
 iteration         1320 MCMCOBJ=   -6490.15774297718     
 iteration         1321 MCMCOBJ=   -6487.50155161354     
 iteration         1322 MCMCOBJ=   -6450.27498741360     
 iteration         1323 MCMCOBJ=   -6439.19285647864     
 iteration         1324 MCMCOBJ=   -6461.83156984697     
 iteration         1325 MCMCOBJ=   -6451.43109860647     
 iteration         1326 MCMCOBJ=   -6512.90996552363     
 iteration         1327 MCMCOBJ=   -6465.59669335087     
 iteration         1328 MCMCOBJ=   -6487.59795055542     
 iteration         1329 MCMCOBJ=   -6471.12610512217     
 iteration         1330 MCMCOBJ=   -6467.42983075275     
 iteration         1331 MCMCOBJ=   -6424.47048556181     
 iteration         1332 MCMCOBJ=   -6435.40400089257     
 iteration         1333 MCMCOBJ=   -6458.95978798982     
 iteration         1334 MCMCOBJ=   -6429.47896488865     
 iteration         1335 MCMCOBJ=   -6458.13954200611     
 iteration         1336 MCMCOBJ=   -6470.30679539557     
 iteration         1337 MCMCOBJ=   -6507.30407375043     
 iteration         1338 MCMCOBJ=   -6507.30407037724     
 iteration         1339 MCMCOBJ=   -6466.87960141174     
 iteration         1340 MCMCOBJ=   -6498.95392827387     
 iteration         1341 MCMCOBJ=   -6509.85921232389     
 iteration         1342 MCMCOBJ=   -6517.24701208436     
 iteration         1343 MCMCOBJ=   -6451.53624775616     
 iteration         1344 MCMCOBJ=   -6485.25687564913     
 iteration         1345 MCMCOBJ=   -6423.55175201186     
 iteration         1346 MCMCOBJ=   -6467.76159390721     
 iteration         1347 MCMCOBJ=   -6444.14304359902     
 iteration         1348 MCMCOBJ=   -6486.28076912499     
 iteration         1349 MCMCOBJ=   -6488.69644787316     
 iteration         1350 MCMCOBJ=   -6504.67190681099     
 iteration         1351 MCMCOBJ=   -6502.92574960563     
 iteration         1352 MCMCOBJ=   -6502.92574954785     
 iteration         1353 MCMCOBJ=   -6475.80058164818     
 iteration         1354 MCMCOBJ=   -6469.26360488685     
 iteration         1355 MCMCOBJ=   -6514.75939609162     
 iteration         1356 MCMCOBJ=   -6540.19304377310     
 iteration         1357 MCMCOBJ=   -6540.19297816995     
 iteration         1358 MCMCOBJ=   -6546.76701689020     
 iteration         1359 MCMCOBJ=   -6546.64458027964     
 iteration         1360 MCMCOBJ=   -6514.96795212612     
 iteration         1361 MCMCOBJ=   -6514.40956140765     
 iteration         1362 MCMCOBJ=   -6569.83245656686     
 iteration         1363 MCMCOBJ=   -6572.88187455941     
 iteration         1364 MCMCOBJ=   -6541.27752601480     
 iteration         1365 MCMCOBJ=   -6555.27825364980     
 iteration         1366 MCMCOBJ=   -6514.12983321140     
 iteration         1367 MCMCOBJ=   -6540.42344420052     
 iteration         1368 MCMCOBJ=   -6521.51552091818     
 iteration         1369 MCMCOBJ=   -6513.60262607795     
 iteration         1370 MCMCOBJ=   -6527.39552389660     
 iteration         1371 MCMCOBJ=   -6510.73372176765     
 iteration         1372 MCMCOBJ=   -6469.08616110627     
 iteration         1373 MCMCOBJ=   -6506.96105686756     
 iteration         1374 MCMCOBJ=   -6453.62495281476     
 iteration         1375 MCMCOBJ=   -6478.65317005189     
 iteration         1376 MCMCOBJ=   -6512.75041620974     
 iteration         1377 MCMCOBJ=   -6534.46192248317     
 iteration         1378 MCMCOBJ=   -6555.86678051860     
 iteration         1379 MCMCOBJ=   -6546.15222590254     
 iteration         1380 MCMCOBJ=   -6522.70812091887     
 iteration         1381 MCMCOBJ=   -6488.89505940247     
 iteration         1382 MCMCOBJ=   -6493.69674524766     
 iteration         1383 MCMCOBJ=   -6457.64138878448     
 iteration         1384 MCMCOBJ=   -6458.73444061978     
 iteration         1385 MCMCOBJ=   -6479.24057378473     
 iteration         1386 MCMCOBJ=   -6544.55369905703     
 iteration         1387 MCMCOBJ=   -6523.55979257299     
 iteration         1388 MCMCOBJ=   -6495.41640797048     
 iteration         1389 MCMCOBJ=   -6486.53201924943     
 iteration         1390 MCMCOBJ=   -6501.59751161134     
 iteration         1391 MCMCOBJ=   -6590.40704014077     
 iteration         1392 MCMCOBJ=   -6535.32538180016     
 iteration         1393 MCMCOBJ=   -6516.09746909169     
 iteration         1394 MCMCOBJ=   -6510.35779449501     
 iteration         1395 MCMCOBJ=   -6476.33479559730     
 iteration         1396 MCMCOBJ=   -6480.27886880166     
 iteration         1397 MCMCOBJ=   -6497.57461694992     
 iteration         1398 MCMCOBJ=   -6514.00347934325     
 iteration         1399 MCMCOBJ=   -6526.82018879843     
 iteration         1400 MCMCOBJ=   -6514.26633969037     
 iteration         1401 MCMCOBJ=   -6486.31934999737     
 iteration         1402 MCMCOBJ=   -6464.27312988490     
 iteration         1403 MCMCOBJ=   -6489.77745858156     
 iteration         1404 MCMCOBJ=   -6437.53733472395     
 iteration         1405 MCMCOBJ=   -6492.36766864747     
 iteration         1406 MCMCOBJ=   -6518.05789751587     
 iteration         1407 MCMCOBJ=   -6518.20558390254     
 iteration         1408 MCMCOBJ=   -6477.04055606425     
 iteration         1409 MCMCOBJ=   -6470.60677952897     
 iteration         1410 MCMCOBJ=   -6459.17878187595     
 iteration         1411 MCMCOBJ=   -6475.95992392598     
 iteration         1412 MCMCOBJ=   -6501.96303286678     
 iteration         1413 MCMCOBJ=   -6415.05429819292     
 iteration         1414 MCMCOBJ=   -6429.53156721363     
 iteration         1415 MCMCOBJ=   -6459.11219148235     
 iteration         1416 MCMCOBJ=   -6465.76269573289     
 iteration         1417 MCMCOBJ=   -6444.02361345671     
 iteration         1418 MCMCOBJ=   -6488.89952067709     
 iteration         1419 MCMCOBJ=   -6512.32202757285     
 iteration         1420 MCMCOBJ=   -6476.71361237831     
 iteration         1421 MCMCOBJ=   -6507.33841614778     
 iteration         1422 MCMCOBJ=   -6532.93251335940     
 iteration         1423 MCMCOBJ=   -6512.06415900179     
 iteration         1424 MCMCOBJ=   -6491.82021133846     
 iteration         1425 MCMCOBJ=   -6454.92526323347     
 iteration         1426 MCMCOBJ=   -6426.65434088337     
 iteration         1427 MCMCOBJ=   -6480.63203035697     
 iteration         1428 MCMCOBJ=   -6466.26313462208     
 iteration         1429 MCMCOBJ=   -6451.91401138677     
 iteration         1430 MCMCOBJ=   -6466.44029229792     
 iteration         1431 MCMCOBJ=   -6459.72294163012     
 iteration         1432 MCMCOBJ=   -6415.88723307644     
 iteration         1433 MCMCOBJ=   -6443.40304331141     
 iteration         1434 MCMCOBJ=   -6441.07841541905     
 iteration         1435 MCMCOBJ=   -6425.44204906846     
 iteration         1436 MCMCOBJ=   -6482.42681322801     
 iteration         1437 MCMCOBJ=   -6455.19059960490     
 iteration         1438 MCMCOBJ=   -6485.59518536632     
 iteration         1439 MCMCOBJ=   -6502.34790551144     
 iteration         1440 MCMCOBJ=   -6518.27692431015     
 iteration         1441 MCMCOBJ=   -6471.17342407749     
 iteration         1442 MCMCOBJ=   -6481.97656115459     
 iteration         1443 MCMCOBJ=   -6464.94942509635     
 iteration         1444 MCMCOBJ=   -6459.38385131290     
 iteration         1445 MCMCOBJ=   -6445.71805748726     
 iteration         1446 MCMCOBJ=   -6462.56490478673     
 iteration         1447 MCMCOBJ=   -6451.31554385359     
 iteration         1448 MCMCOBJ=   -6421.47195494694     
 iteration         1449 MCMCOBJ=   -6449.57196847851     
 iteration         1450 MCMCOBJ=   -6450.59429469819     
 iteration         1451 MCMCOBJ=   -6493.38533441112     
 iteration         1452 MCMCOBJ=   -6488.50798506438     
 iteration         1453 MCMCOBJ=   -6471.73138448652     
 iteration         1454 MCMCOBJ=   -6461.58079651301     
 iteration         1455 MCMCOBJ=   -6502.80813380675     
 iteration         1456 MCMCOBJ=   -6547.02394270210     
 iteration         1457 MCMCOBJ=   -6551.76238455959     
 iteration         1458 MCMCOBJ=   -6560.79665698013     
 iteration         1459 MCMCOBJ=   -6512.53883882978     
 iteration         1460 MCMCOBJ=   -6515.03357119630     
 iteration         1461 MCMCOBJ=   -6490.72510068734     
 iteration         1462 MCMCOBJ=   -6518.73712101069     
 iteration         1463 MCMCOBJ=   -6546.54743534085     
 iteration         1464 MCMCOBJ=   -6527.68127569423     
 iteration         1465 MCMCOBJ=   -6512.49507235790     
 iteration         1466 MCMCOBJ=   -6504.75480863131     
 iteration         1467 MCMCOBJ=   -6510.04337650434     
 iteration         1468 MCMCOBJ=   -6462.13676528509     
 iteration         1469 MCMCOBJ=   -6482.72100735625     
 iteration         1470 MCMCOBJ=   -6532.28336868251     
 iteration         1471 MCMCOBJ=   -6509.42183616785     
 iteration         1472 MCMCOBJ=   -6553.42472246154     
 iteration         1473 MCMCOBJ=   -6540.25158968488     
 iteration         1474 MCMCOBJ=   -6526.41903513964     
 iteration         1475 MCMCOBJ=   -6518.29801879237     
 iteration         1476 MCMCOBJ=   -6519.13317760366     
 iteration         1477 MCMCOBJ=   -6531.17422207864     
 iteration         1478 MCMCOBJ=   -6483.48835668174     
 iteration         1479 MCMCOBJ=   -6502.13969514159     
 iteration         1480 MCMCOBJ=   -6480.44331896506     
 iteration         1481 MCMCOBJ=   -6542.72104825940     
 iteration         1482 MCMCOBJ=   -6547.02215268374     
 iteration         1483 MCMCOBJ=   -6544.49025555428     
 iteration         1484 MCMCOBJ=   -6493.94635293248     
 iteration         1485 MCMCOBJ=   -6500.62967670006     
 iteration         1486 MCMCOBJ=   -6511.94276702748     
 iteration         1487 MCMCOBJ=   -6488.38485351004     
 iteration         1488 MCMCOBJ=   -6495.05824730439     
 iteration         1489 MCMCOBJ=   -6488.00448842796     
 iteration         1490 MCMCOBJ=   -6498.12344533068     
 iteration         1491 MCMCOBJ=   -6523.36462547195     
 iteration         1492 MCMCOBJ=   -6485.34390696698     
 iteration         1493 MCMCOBJ=   -6484.03205948843     
 iteration         1494 MCMCOBJ=   -6469.07129964304     
 iteration         1495 MCMCOBJ=   -6514.32325484787     
 iteration         1496 MCMCOBJ=   -6504.59844215033     
 iteration         1497 MCMCOBJ=   -6511.61259845219     
 iteration         1498 MCMCOBJ=   -6497.98007464315     
 iteration         1499 MCMCOBJ=   -6476.65511700668     
 iteration         1500 MCMCOBJ=   -6475.74159973056     
 iteration         1501 MCMCOBJ=   -6540.66685428488     
 iteration         1502 MCMCOBJ=   -6483.69586204258     
 iteration         1503 MCMCOBJ=   -6491.09355293545     
 iteration         1504 MCMCOBJ=   -6506.54480345210     
 iteration         1505 MCMCOBJ=   -6506.54463860778     
 iteration         1506 MCMCOBJ=   -6547.09230535246     
 iteration         1507 MCMCOBJ=   -6456.73030971670     
 iteration         1508 MCMCOBJ=   -6473.30210121801     
 iteration         1509 MCMCOBJ=   -6408.54970634487     
 iteration         1510 MCMCOBJ=   -6431.59072630158     
 iteration         1511 MCMCOBJ=   -6504.42049247212     
 iteration         1512 MCMCOBJ=   -6565.54277205117     
 iteration         1513 MCMCOBJ=   -6467.77933578588     
 iteration         1514 MCMCOBJ=   -6467.77933543433     
 iteration         1515 MCMCOBJ=   -6515.93597345421     
 iteration         1516 MCMCOBJ=   -6550.64497409967     
 iteration         1517 MCMCOBJ=   -6554.50509475929     
 iteration         1518 MCMCOBJ=   -6512.26299533058     
 iteration         1519 MCMCOBJ=   -6543.54614450414     
 iteration         1520 MCMCOBJ=   -6521.20213316784     
 iteration         1521 MCMCOBJ=   -6522.93231627507     
 iteration         1522 MCMCOBJ=   -6536.22554024529     
 iteration         1523 MCMCOBJ=   -6505.78370506835     
 iteration         1524 MCMCOBJ=   -6482.19953298585     
 iteration         1525 MCMCOBJ=   -6478.31486989134     
 iteration         1526 MCMCOBJ=   -6461.38763462875     
 iteration         1527 MCMCOBJ=   -6482.49302756885     
 iteration         1528 MCMCOBJ=   -6526.80980907388     
 iteration         1529 MCMCOBJ=   -6504.84503885365     
 iteration         1530 MCMCOBJ=   -6498.48483701290     
 iteration         1531 MCMCOBJ=   -6543.21211605464     
 iteration         1532 MCMCOBJ=   -6590.28138842335     
 iteration         1533 MCMCOBJ=   -6562.50572682939     
 iteration         1534 MCMCOBJ=   -6535.59989933839     
 iteration         1535 MCMCOBJ=   -6564.19557382729     
 iteration         1536 MCMCOBJ=   -6576.74890114447     
 iteration         1537 MCMCOBJ=   -6590.35710239892     
 iteration         1538 MCMCOBJ=   -6580.45286690807     
 iteration         1539 MCMCOBJ=   -6501.56738665898     
 iteration         1540 MCMCOBJ=   -6440.69332094158     
 iteration         1541 MCMCOBJ=   -6509.19916310883     
 iteration         1542 MCMCOBJ=   -6582.29836379702     
 iteration         1543 MCMCOBJ=   -6527.02874853738     
 iteration         1544 MCMCOBJ=   -6507.15174595939     
 iteration         1545 MCMCOBJ=   -6500.25445998492     
 iteration         1546 MCMCOBJ=   -6504.79087609106     
 iteration         1547 MCMCOBJ=   -6500.02202829968     
 iteration         1548 MCMCOBJ=   -6477.62928638792     
 iteration         1549 MCMCOBJ=   -6477.84053022948     
 iteration         1550 MCMCOBJ=   -6459.39075350238     
 iteration         1551 MCMCOBJ=   -6507.95071101876     
 iteration         1552 MCMCOBJ=   -6459.54733697312     
 iteration         1553 MCMCOBJ=   -6532.15247562913     
 iteration         1554 MCMCOBJ=   -6518.37720835000     
 iteration         1555 MCMCOBJ=   -6499.12272502815     
 iteration         1556 MCMCOBJ=   -6515.61998845265     
 iteration         1557 MCMCOBJ=   -6518.68598022968     
 iteration         1558 MCMCOBJ=   -6522.18234566210     
 iteration         1559 MCMCOBJ=   -6534.02280947375     
 iteration         1560 MCMCOBJ=   -6494.77737484571     
 iteration         1561 MCMCOBJ=   -6508.84063913034     
 iteration         1562 MCMCOBJ=   -6501.87809243501     
 iteration         1563 MCMCOBJ=   -6491.47913283564     
 iteration         1564 MCMCOBJ=   -6513.88496802805     
 iteration         1565 MCMCOBJ=   -6504.13751095171     
 iteration         1566 MCMCOBJ=   -6549.64387466273     
 iteration         1567 MCMCOBJ=   -6502.62230939709     
 iteration         1568 MCMCOBJ=   -6494.84075971804     
 iteration         1569 MCMCOBJ=   -6438.84875653238     
 iteration         1570 MCMCOBJ=   -6469.27725059207     
 iteration         1571 MCMCOBJ=   -6486.41520787531     
 iteration         1572 MCMCOBJ=   -6475.95018968087     
 iteration         1573 MCMCOBJ=   -6472.51830102988     
 iteration         1574 MCMCOBJ=   -6482.91534054496     
 iteration         1575 MCMCOBJ=   -6438.66368021001     
 iteration         1576 MCMCOBJ=   -6465.84765754429     
 iteration         1577 MCMCOBJ=   -6449.87503362173     
 iteration         1578 MCMCOBJ=   -6444.17691829886     
 iteration         1579 MCMCOBJ=   -6488.18746498217     
 iteration         1580 MCMCOBJ=   -6486.67060843243     
 iteration         1581 MCMCOBJ=   -6515.28327280565     
 iteration         1582 MCMCOBJ=   -6423.94015010029     
 iteration         1583 MCMCOBJ=   -6513.78920482302     
 iteration         1584 MCMCOBJ=   -6512.59607206366     
 iteration         1585 MCMCOBJ=   -6535.24271883408     
 iteration         1586 MCMCOBJ=   -6583.19644606725     
 iteration         1587 MCMCOBJ=   -6590.17222405061     
 iteration         1588 MCMCOBJ=   -6572.53147815288     
 iteration         1589 MCMCOBJ=   -6556.35930656160     
 iteration         1590 MCMCOBJ=   -6551.93535668763     
 iteration         1591 MCMCOBJ=   -6522.76130876134     
 iteration         1592 MCMCOBJ=   -6527.16575204389     
 iteration         1593 MCMCOBJ=   -6546.85987877562     
 iteration         1594 MCMCOBJ=   -6547.98311208542     
 iteration         1595 MCMCOBJ=   -6458.59301552073     
 iteration         1596 MCMCOBJ=   -6504.98877145966     
 iteration         1597 MCMCOBJ=   -6537.23832473079     
 iteration         1598 MCMCOBJ=   -6532.28208339432     
 iteration         1599 MCMCOBJ=   -6516.51840689347     
 iteration         1600 MCMCOBJ=   -6523.26088648367     
 iteration         1601 MCMCOBJ=   -6521.84752263016     
 iteration         1602 MCMCOBJ=   -6481.41116345116     
 iteration         1603 MCMCOBJ=   -6443.93958011230     
 iteration         1604 MCMCOBJ=   -6492.66875302010     
 iteration         1605 MCMCOBJ=   -6485.69241838046     
 iteration         1606 MCMCOBJ=   -6488.65556853643     
 iteration         1607 MCMCOBJ=   -6458.09431472365     
 iteration         1608 MCMCOBJ=   -6477.68840101742     
 iteration         1609 MCMCOBJ=   -6519.11874961473     
 iteration         1610 MCMCOBJ=   -6500.36336876130     
 iteration         1611 MCMCOBJ=   -6459.92671513853     
 iteration         1612 MCMCOBJ=   -6460.45280910593     
 iteration         1613 MCMCOBJ=   -6487.75632701800     
 iteration         1614 MCMCOBJ=   -6502.24887932911     
 iteration         1615 MCMCOBJ=   -6504.31932330954     
 iteration         1616 MCMCOBJ=   -6516.92712637908     
 iteration         1617 MCMCOBJ=   -6463.90896913321     
 iteration         1618 MCMCOBJ=   -6546.49275111917     
 iteration         1619 MCMCOBJ=   -6550.75061208203     
 iteration         1620 MCMCOBJ=   -6429.19092094206     
 iteration         1621 MCMCOBJ=   -6509.75711492554     
 iteration         1622 MCMCOBJ=   -6466.24700849789     
 iteration         1623 MCMCOBJ=   -6420.86886836891     
 iteration         1624 MCMCOBJ=   -6374.70973161940     
 iteration         1625 MCMCOBJ=   -6460.20467021044     
 iteration         1626 MCMCOBJ=   -6483.76739871136     
 iteration         1627 MCMCOBJ=   -6455.11299921184     
 iteration         1628 MCMCOBJ=   -6468.29166436030     
 iteration         1629 MCMCOBJ=   -6453.39775660283     
 iteration         1630 MCMCOBJ=   -6473.18979477651     
 iteration         1631 MCMCOBJ=   -6505.94143404290     
 iteration         1632 MCMCOBJ=   -6505.94143349608     
 iteration         1633 MCMCOBJ=   -6529.27655842432     
 iteration         1634 MCMCOBJ=   -6507.98824385525     
 iteration         1635 MCMCOBJ=   -6523.67555870797     
 iteration         1636 MCMCOBJ=   -6507.35626244868     
 iteration         1637 MCMCOBJ=   -6516.12229468141     
 iteration         1638 MCMCOBJ=   -6516.24205855785     
 iteration         1639 MCMCOBJ=   -6514.37753619363     
 iteration         1640 MCMCOBJ=   -6474.16595794868     
 iteration         1641 MCMCOBJ=   -6479.14605302133     
 iteration         1642 MCMCOBJ=   -6465.83013262246     
 iteration         1643 MCMCOBJ=   -6536.58230315823     
 iteration         1644 MCMCOBJ=   -6540.66437796002     
 iteration         1645 MCMCOBJ=   -6540.91518051888     
 iteration         1646 MCMCOBJ=   -6498.99593975381     
 iteration         1647 MCMCOBJ=   -6495.93553813873     
 iteration         1648 MCMCOBJ=   -6495.93551873957     
 iteration         1649 MCMCOBJ=   -6474.98896294076     
 iteration         1650 MCMCOBJ=   -6476.90362991707     
 iteration         1651 MCMCOBJ=   -6512.73779214256     
 iteration         1652 MCMCOBJ=   -6497.58958752398     
 iteration         1653 MCMCOBJ=   -6486.14121509481     
 iteration         1654 MCMCOBJ=   -6504.08050361701     
 iteration         1655 MCMCOBJ=   -6514.94892971636     
 iteration         1656 MCMCOBJ=   -6528.09550507359     
 iteration         1657 MCMCOBJ=   -6502.39143673302     
 iteration         1658 MCMCOBJ=   -6536.27063959054     
 iteration         1659 MCMCOBJ=   -6542.33354916585     
 iteration         1660 MCMCOBJ=   -6561.40921131076     
 iteration         1661 MCMCOBJ=   -6567.12794735785     
 iteration         1662 MCMCOBJ=   -6522.99436639339     
 iteration         1663 MCMCOBJ=   -6491.32640910177     
 iteration         1664 MCMCOBJ=   -6477.41655418049     
 iteration         1665 MCMCOBJ=   -6525.49380127118     
 iteration         1666 MCMCOBJ=   -6517.27395443004     
 iteration         1667 MCMCOBJ=   -6502.50392418196     
 iteration         1668 MCMCOBJ=   -6508.82512195744     
 iteration         1669 MCMCOBJ=   -6435.17242127840     
 iteration         1670 MCMCOBJ=   -6476.53956028359     
 iteration         1671 MCMCOBJ=   -6485.74462818664     
 iteration         1672 MCMCOBJ=   -6448.33818218788     
 iteration         1673 MCMCOBJ=   -6434.30468266334     
 iteration         1674 MCMCOBJ=   -6377.13769280481     
 iteration         1675 MCMCOBJ=   -6415.23924258551     
 iteration         1676 MCMCOBJ=   -6431.86082215632     
 iteration         1677 MCMCOBJ=   -6431.86082223104     
 iteration         1678 MCMCOBJ=   -6456.97936781767     
 iteration         1679 MCMCOBJ=   -6459.12329493942     
 iteration         1680 MCMCOBJ=   -6464.30196946842     
 iteration         1681 MCMCOBJ=   -6476.39158287315     
 iteration         1682 MCMCOBJ=   -6482.48380164921     
 iteration         1683 MCMCOBJ=   -6439.63283726861     
 iteration         1684 MCMCOBJ=   -6435.76510887782     
 iteration         1685 MCMCOBJ=   -6441.65181232188     
 iteration         1686 MCMCOBJ=   -6435.75209001064     
 iteration         1687 MCMCOBJ=   -6432.07767081290     
 iteration         1688 MCMCOBJ=   -6429.94193071707     
 iteration         1689 MCMCOBJ=   -6462.64960350751     
 iteration         1690 MCMCOBJ=   -6462.64960281140     
 iteration         1691 MCMCOBJ=   -6516.49098904654     
 iteration         1692 MCMCOBJ=   -6447.83193141054     
 iteration         1693 MCMCOBJ=   -6448.22180436451     
 iteration         1694 MCMCOBJ=   -6482.16018429083     
 iteration         1695 MCMCOBJ=   -6433.58829862387     
 iteration         1696 MCMCOBJ=   -6505.85095071966     
 iteration         1697 MCMCOBJ=   -6512.19162469301     
 iteration         1698 MCMCOBJ=   -6582.84392394073     
 iteration         1699 MCMCOBJ=   -6481.07828402732     
 iteration         1700 MCMCOBJ=   -6461.87231692695     
 iteration         1701 MCMCOBJ=   -6499.24216282318     
 iteration         1702 MCMCOBJ=   -6556.65249811775     
 iteration         1703 MCMCOBJ=   -6539.86936895356     
 iteration         1704 MCMCOBJ=   -6534.79148586055     
 iteration         1705 MCMCOBJ=   -6564.35384252668     
 iteration         1706 MCMCOBJ=   -6545.78986006698     
 iteration         1707 MCMCOBJ=   -6545.78986012316     
 iteration         1708 MCMCOBJ=   -6550.89873619589     
 iteration         1709 MCMCOBJ=   -6551.72900935778     
 iteration         1710 MCMCOBJ=   -6491.16677925799     
 iteration         1711 MCMCOBJ=   -6513.33553169649     
 iteration         1712 MCMCOBJ=   -6514.66834825650     
 iteration         1713 MCMCOBJ=   -6517.34930716956     
 iteration         1714 MCMCOBJ=   -6497.92396421486     
 iteration         1715 MCMCOBJ=   -6575.46891547526     
 iteration         1716 MCMCOBJ=   -6520.56889474631     
 iteration         1717 MCMCOBJ=   -6525.76406844266     
 iteration         1718 MCMCOBJ=   -6528.86224845248     
 iteration         1719 MCMCOBJ=   -6485.98584577808     
 iteration         1720 MCMCOBJ=   -6495.71579619680     
 iteration         1721 MCMCOBJ=   -6473.93388885175     
 iteration         1722 MCMCOBJ=   -6477.08558806388     
 iteration         1723 MCMCOBJ=   -6497.82443343096     
 iteration         1724 MCMCOBJ=   -6497.84605616298     
 iteration         1725 MCMCOBJ=   -6483.93643529712     
 iteration         1726 MCMCOBJ=   -6481.60688927195     
 iteration         1727 MCMCOBJ=   -6499.62116849188     
 iteration         1728 MCMCOBJ=   -6513.34206219799     
 iteration         1729 MCMCOBJ=   -6497.46066483703     
 iteration         1730 MCMCOBJ=   -6501.06349998449     
 iteration         1731 MCMCOBJ=   -6512.45420278461     
 iteration         1732 MCMCOBJ=   -6471.32381047816     
 iteration         1733 MCMCOBJ=   -6462.39449463801     
 iteration         1734 MCMCOBJ=   -6439.88460714019     
 iteration         1735 MCMCOBJ=   -6496.81615121110     
 iteration         1736 MCMCOBJ=   -6491.12506127419     
 iteration         1737 MCMCOBJ=   -6526.73239679996     
 iteration         1738 MCMCOBJ=   -6499.70570615400     
 iteration         1739 MCMCOBJ=   -6511.55072672277     
 iteration         1740 MCMCOBJ=   -6480.05953316392     
 iteration         1741 MCMCOBJ=   -6462.78800753611     
 iteration         1742 MCMCOBJ=   -6418.26970650213     
 iteration         1743 MCMCOBJ=   -6459.37475938274     
 iteration         1744 MCMCOBJ=   -6506.44089912396     
 iteration         1745 MCMCOBJ=   -6509.02931635615     
 iteration         1746 MCMCOBJ=   -6468.91197613713     
 iteration         1747 MCMCOBJ=   -6496.14927608644     
 iteration         1748 MCMCOBJ=   -6513.57004509789     
 iteration         1749 MCMCOBJ=   -6547.72145139338     
 iteration         1750 MCMCOBJ=   -6589.56499456469     
 iteration         1751 MCMCOBJ=   -6527.20936065819     
 iteration         1752 MCMCOBJ=   -6538.58558137907     
 iteration         1753 MCMCOBJ=   -6537.88835266515     
 iteration         1754 MCMCOBJ=   -6479.10928804855     
 iteration         1755 MCMCOBJ=   -6450.32400670174     
 iteration         1756 MCMCOBJ=   -6423.84116266568     
 iteration         1757 MCMCOBJ=   -6439.54049196126     
 iteration         1758 MCMCOBJ=   -6434.90163831857     
 iteration         1759 MCMCOBJ=   -6499.27475772734     
 iteration         1760 MCMCOBJ=   -6507.96691593240     
 iteration         1761 MCMCOBJ=   -6467.83261294363     
 iteration         1762 MCMCOBJ=   -6489.21501735188     
 iteration         1763 MCMCOBJ=   -6486.53434866836     
 iteration         1764 MCMCOBJ=   -6525.44239350761     
 iteration         1765 MCMCOBJ=   -6433.91621065976     
 iteration         1766 MCMCOBJ=   -6397.24977204754     
 iteration         1767 MCMCOBJ=   -6473.50863899758     
 iteration         1768 MCMCOBJ=   -6493.70116323714     
 iteration         1769 MCMCOBJ=   -6462.58319572590     
 iteration         1770 MCMCOBJ=   -6492.38162553540     
 iteration         1771 MCMCOBJ=   -6493.55220554030     
 iteration         1772 MCMCOBJ=   -6484.70055829692     
 iteration         1773 MCMCOBJ=   -6469.96682238513     
 iteration         1774 MCMCOBJ=   -6480.10968322015     
 iteration         1775 MCMCOBJ=   -6457.12206800477     
 iteration         1776 MCMCOBJ=   -6452.09143708404     
 iteration         1777 MCMCOBJ=   -6451.41959652679     
 iteration         1778 MCMCOBJ=   -6484.39064399161     
 iteration         1779 MCMCOBJ=   -6508.35421150737     
 iteration         1780 MCMCOBJ=   -6474.13017013077     
 iteration         1781 MCMCOBJ=   -6493.23135755440     
 iteration         1782 MCMCOBJ=   -6469.33324349175     
 iteration         1783 MCMCOBJ=   -6457.94473704798     
 iteration         1784 MCMCOBJ=   -6496.75565959152     
 iteration         1785 MCMCOBJ=   -6475.84596147792     
 iteration         1786 MCMCOBJ=   -6475.84595165137     
 iteration         1787 MCMCOBJ=   -6447.51964140596     
 iteration         1788 MCMCOBJ=   -6477.04423011451     
 iteration         1789 MCMCOBJ=   -6465.26587798171     
 iteration         1790 MCMCOBJ=   -6505.13678485961     
 iteration         1791 MCMCOBJ=   -6482.06611759588     
 iteration         1792 MCMCOBJ=   -6455.22102533862     
 iteration         1793 MCMCOBJ=   -6451.81285960026     
 iteration         1794 MCMCOBJ=   -6435.81176081584     
 iteration         1795 MCMCOBJ=   -6466.01414940900     
 iteration         1796 MCMCOBJ=   -6530.13817143347     
 iteration         1797 MCMCOBJ=   -6551.89830829499     
 iteration         1798 MCMCOBJ=   -6536.98126601429     
 iteration         1799 MCMCOBJ=   -6515.47033316200     
 iteration         1800 MCMCOBJ=   -6504.87265159395     
 iteration         1801 MCMCOBJ=   -6498.26239790452     
 iteration         1802 MCMCOBJ=   -6448.46646693244     
 iteration         1803 MCMCOBJ=   -6481.30596989225     
 iteration         1804 MCMCOBJ=   -6511.51190810275     
 iteration         1805 MCMCOBJ=   -6571.38996805064     
 iteration         1806 MCMCOBJ=   -6493.36974790784     
 iteration         1807 MCMCOBJ=   -6468.36459958047     
 iteration         1808 MCMCOBJ=   -6444.83946531442     
 iteration         1809 MCMCOBJ=   -6465.00087320087     
 iteration         1810 MCMCOBJ=   -6503.47844550813     
 iteration         1811 MCMCOBJ=   -6515.37940804561     
 iteration         1812 MCMCOBJ=   -6509.63528760638     
 iteration         1813 MCMCOBJ=   -6508.18459996070     
 iteration         1814 MCMCOBJ=   -6413.95330271327     
 iteration         1815 MCMCOBJ=   -6432.93310273593     
 iteration         1816 MCMCOBJ=   -6452.37161549831     
 iteration         1817 MCMCOBJ=   -6443.26617290058     
 iteration         1818 MCMCOBJ=   -6450.23963033900     
 iteration         1819 MCMCOBJ=   -6450.23963137462     
 iteration         1820 MCMCOBJ=   -6457.12173366142     
 iteration         1821 MCMCOBJ=   -6493.42518951784     
 iteration         1822 MCMCOBJ=   -6485.06568437005     
 iteration         1823 MCMCOBJ=   -6435.36316049621     
 iteration         1824 MCMCOBJ=   -6411.01391556960     
 iteration         1825 MCMCOBJ=   -6446.68401026049     
 iteration         1826 MCMCOBJ=   -6488.92605172749     
 iteration         1827 MCMCOBJ=   -6455.59680406303     
 iteration         1828 MCMCOBJ=   -6468.58436515628     
 iteration         1829 MCMCOBJ=   -6490.60892366129     
 iteration         1830 MCMCOBJ=   -6469.51310289150     
 iteration         1831 MCMCOBJ=   -6533.46461385415     
 iteration         1832 MCMCOBJ=   -6472.75546400260     
 iteration         1833 MCMCOBJ=   -6435.50026919306     
 iteration         1834 MCMCOBJ=   -6431.15142495745     
 iteration         1835 MCMCOBJ=   -6462.81366413356     
 iteration         1836 MCMCOBJ=   -6445.87737593173     
 iteration         1837 MCMCOBJ=   -6434.06558897899     
 iteration         1838 MCMCOBJ=   -6416.87572284684     
 iteration         1839 MCMCOBJ=   -6516.56047975786     
 iteration         1840 MCMCOBJ=   -6507.12247389903     
 iteration         1841 MCMCOBJ=   -6491.16878712743     
 iteration         1842 MCMCOBJ=   -6452.50021184409     
 iteration         1843 MCMCOBJ=   -6475.94920156683     
 iteration         1844 MCMCOBJ=   -6544.71761365843     
 iteration         1845 MCMCOBJ=   -6532.93037159142     
 iteration         1846 MCMCOBJ=   -6517.34704224727     
 iteration         1847 MCMCOBJ=   -6501.52788522326     
 iteration         1848 MCMCOBJ=   -6547.22610385446     
 iteration         1849 MCMCOBJ=   -6488.34160884831     
 iteration         1850 MCMCOBJ=   -6466.04586484059     
 iteration         1851 MCMCOBJ=   -6459.24713051718     
 iteration         1852 MCMCOBJ=   -6454.60003136590     
 iteration         1853 MCMCOBJ=   -6525.69583664466     
 iteration         1854 MCMCOBJ=   -6530.25184135124     
 iteration         1855 MCMCOBJ=   -6532.32607582268     
 iteration         1856 MCMCOBJ=   -6497.89781654807     
 iteration         1857 MCMCOBJ=   -6523.18299201082     
 iteration         1858 MCMCOBJ=   -6478.42148739787     
 iteration         1859 MCMCOBJ=   -6436.24343739846     
 iteration         1860 MCMCOBJ=   -6414.95072126924     
 iteration         1861 MCMCOBJ=   -6473.16679469252     
 iteration         1862 MCMCOBJ=   -6501.77000667340     
 iteration         1863 MCMCOBJ=   -6490.94413929040     
 iteration         1864 MCMCOBJ=   -6479.76645926023     
 iteration         1865 MCMCOBJ=   -6528.96076976793     
 iteration         1866 MCMCOBJ=   -6515.85530293565     
 iteration         1867 MCMCOBJ=   -6462.35081076702     
 iteration         1868 MCMCOBJ=   -6474.77479351324     
 iteration         1869 MCMCOBJ=   -6384.78294830678     
 iteration         1870 MCMCOBJ=   -6458.18240863501     
 iteration         1871 MCMCOBJ=   -6431.23876895159     
 iteration         1872 MCMCOBJ=   -6484.62983915391     
 iteration         1873 MCMCOBJ=   -6498.01042104440     
 iteration         1874 MCMCOBJ=   -6468.08933593043     
 iteration         1875 MCMCOBJ=   -6498.67292256467     
 iteration         1876 MCMCOBJ=   -6508.76088908152     
 iteration         1877 MCMCOBJ=   -6467.25689177950     
 iteration         1878 MCMCOBJ=   -6470.41307507364     
 iteration         1879 MCMCOBJ=   -6455.47540203423     
 iteration         1880 MCMCOBJ=   -6439.15823194380     
 iteration         1881 MCMCOBJ=   -6474.11067831763     
 iteration         1882 MCMCOBJ=   -6548.96927769272     
 iteration         1883 MCMCOBJ=   -6504.22436214468     
 iteration         1884 MCMCOBJ=   -6488.19602148351     
 iteration         1885 MCMCOBJ=   -6486.29572849217     
 iteration         1886 MCMCOBJ=   -6440.97254862668     
 iteration         1887 MCMCOBJ=   -6454.40340474643     
 iteration         1888 MCMCOBJ=   -6437.75558450910     
 iteration         1889 MCMCOBJ=   -6446.70690468100     
 iteration         1890 MCMCOBJ=   -6453.58106359455     
 iteration         1891 MCMCOBJ=   -6459.56046900874     
 iteration         1892 MCMCOBJ=   -6458.00796307747     
 iteration         1893 MCMCOBJ=   -6458.07508308780     
 iteration         1894 MCMCOBJ=   -6427.25638710436     
 iteration         1895 MCMCOBJ=   -6464.51637631980     
 iteration         1896 MCMCOBJ=   -6472.11774776562     
 iteration         1897 MCMCOBJ=   -6471.86930404260     
 iteration         1898 MCMCOBJ=   -6516.01636681896     
 iteration         1899 MCMCOBJ=   -6499.37699544183     
 iteration         1900 MCMCOBJ=   -6490.36129491701     
 iteration         1901 MCMCOBJ=   -6559.30612319839     
 iteration         1902 MCMCOBJ=   -6506.86813542546     
 iteration         1903 MCMCOBJ=   -6496.95090461063     
 iteration         1904 MCMCOBJ=   -6490.15218857011     
 iteration         1905 MCMCOBJ=   -6496.22147404061     
 iteration         1906 MCMCOBJ=   -6483.33261003471     
 iteration         1907 MCMCOBJ=   -6506.35411716841     
 iteration         1908 MCMCOBJ=   -6487.58678643524     
 iteration         1909 MCMCOBJ=   -6493.56677485393     
 iteration         1910 MCMCOBJ=   -6470.88176850215     
 iteration         1911 MCMCOBJ=   -6473.27716021841     
 iteration         1912 MCMCOBJ=   -6464.11233849033     
 iteration         1913 MCMCOBJ=   -6481.28002178745     
 iteration         1914 MCMCOBJ=   -6458.93272056265     
 iteration         1915 MCMCOBJ=   -6447.11211974900     
 iteration         1916 MCMCOBJ=   -6439.25655822578     
 iteration         1917 MCMCOBJ=   -6453.95044000774     
 iteration         1918 MCMCOBJ=   -6510.77818021402     
 iteration         1919 MCMCOBJ=   -6521.56986997336     
 iteration         1920 MCMCOBJ=   -6520.52829513675     
 iteration         1921 MCMCOBJ=   -6520.52827412163     
 iteration         1922 MCMCOBJ=   -6495.21709048854     
 iteration         1923 MCMCOBJ=   -6512.54923891561     
 iteration         1924 MCMCOBJ=   -6513.06771843561     
 iteration         1925 MCMCOBJ=   -6500.81209133871     
 iteration         1926 MCMCOBJ=   -6510.63812155634     
 iteration         1927 MCMCOBJ=   -6460.62606331179     
 iteration         1928 MCMCOBJ=   -6521.11072216234     
 iteration         1929 MCMCOBJ=   -6488.08672379468     
 iteration         1930 MCMCOBJ=   -6464.99913884303     
 iteration         1931 MCMCOBJ=   -6527.69047386349     
 iteration         1932 MCMCOBJ=   -6527.29258180248     
 iteration         1933 MCMCOBJ=   -6518.11501364697     
 iteration         1934 MCMCOBJ=   -6504.53069977421     
 iteration         1935 MCMCOBJ=   -6478.90438211500     
 iteration         1936 MCMCOBJ=   -6478.90438054788     
 iteration         1937 MCMCOBJ=   -6492.26860852113     
 iteration         1938 MCMCOBJ=   -6488.16296683115     
 iteration         1939 MCMCOBJ=   -6435.67714179669     
 iteration         1940 MCMCOBJ=   -6445.62633188415     
 iteration         1941 MCMCOBJ=   -6487.54324662331     
 iteration         1942 MCMCOBJ=   -6506.12673302284     
 iteration         1943 MCMCOBJ=   -6520.44970476228     
 iteration         1944 MCMCOBJ=   -6522.99931331971     
 iteration         1945 MCMCOBJ=   -6500.13826539634     
 iteration         1946 MCMCOBJ=   -6485.74918502150     
 iteration         1947 MCMCOBJ=   -6486.97916540789     
 iteration         1948 MCMCOBJ=   -6477.87374730672     
 iteration         1949 MCMCOBJ=   -6491.51035440675     
 iteration         1950 MCMCOBJ=   -6487.01539574528     
 iteration         1951 MCMCOBJ=   -6482.13634639770     
 iteration         1952 MCMCOBJ=   -6521.51984988792     
 iteration         1953 MCMCOBJ=   -6542.64769792128     
 iteration         1954 MCMCOBJ=   -6523.18351511440     
 iteration         1955 MCMCOBJ=   -6532.50328035572     
 iteration         1956 MCMCOBJ=   -6501.92974526140     
 iteration         1957 MCMCOBJ=   -6516.66843147522     
 iteration         1958 MCMCOBJ=   -6458.34087329937     
 iteration         1959 MCMCOBJ=   -6493.32928282583     
 iteration         1960 MCMCOBJ=   -6460.54844157931     
 iteration         1961 MCMCOBJ=   -6504.68903633654     
 iteration         1962 MCMCOBJ=   -6517.92164668393     
 iteration         1963 MCMCOBJ=   -6494.65040534487     
 iteration         1964 MCMCOBJ=   -6524.50086340858     
 iteration         1965 MCMCOBJ=   -6510.81131116420     
 iteration         1966 MCMCOBJ=   -6510.40363122087     
 iteration         1967 MCMCOBJ=   -6446.97462437752     
 iteration         1968 MCMCOBJ=   -6444.71078187069     
 iteration         1969 MCMCOBJ=   -6394.37418989380     
 iteration         1970 MCMCOBJ=   -6472.67777101160     
 iteration         1971 MCMCOBJ=   -6465.86394094547     
 iteration         1972 MCMCOBJ=   -6465.86394562263     
 iteration         1973 MCMCOBJ=   -6439.99319879295     
 iteration         1974 MCMCOBJ=   -6443.49452551956     
 iteration         1975 MCMCOBJ=   -6455.04959071563     
 iteration         1976 MCMCOBJ=   -6459.14691653662     
 iteration         1977 MCMCOBJ=   -6473.34564297733     
 iteration         1978 MCMCOBJ=   -6476.45547461479     
 iteration         1979 MCMCOBJ=   -6485.17859110174     
 iteration         1980 MCMCOBJ=   -6489.56568041736     
 iteration         1981 MCMCOBJ=   -6511.70506407206     
 iteration         1982 MCMCOBJ=   -6479.88613390285     
 iteration         1983 MCMCOBJ=   -6487.50655947982     
 iteration         1984 MCMCOBJ=   -6506.27165936558     
 iteration         1985 MCMCOBJ=   -6489.20014737519     
 iteration         1986 MCMCOBJ=   -6478.51453980949     
 iteration         1987 MCMCOBJ=   -6507.48587062542     
 iteration         1988 MCMCOBJ=   -6503.29669007182     
 iteration         1989 MCMCOBJ=   -6502.90966137223     
 iteration         1990 MCMCOBJ=   -6506.36664737567     
 iteration         1991 MCMCOBJ=   -6506.36664984995     
 iteration         1992 MCMCOBJ=   -6446.27097256941     
 iteration         1993 MCMCOBJ=   -6469.52955513481     
 iteration         1994 MCMCOBJ=   -6529.20599263426     
 iteration         1995 MCMCOBJ=   -6504.04585261782     
 iteration         1996 MCMCOBJ=   -6479.37651403297     
 iteration         1997 MCMCOBJ=   -6502.73415333619     
 iteration         1998 MCMCOBJ=   -6553.30299838270     
 iteration         1999 MCMCOBJ=   -6532.13769410931     
 iteration         2000 MCMCOBJ=   -6525.35693602593     
 
 #TERM:
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation  time in seconds:  3990.86
 Elapsed covariance  time in seconds:     0.02
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6496.897       **************************************************
 #OBJS:********************************************       36.037 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.23E+00  5.54E-01 -1.82E-01  2.27E+00  2.40E-01  3.71E+00 -7.03E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.93E-01
 
 ETA2
+       -3.74E-02  2.39E-01
 
 ETA3
+        4.73E-02 -9.78E-03  1.60E-01
 
 ETA4
+        3.20E-02  6.35E-02 -1.04E-02  2.95E-01
 
 ETA5
+        2.79E-02  1.71E-02  6.50E-04 -3.47E-02  2.34E-01
 
 ETA6
+       -2.78E-02  4.53E-03  1.75E-02  1.60E-02 -7.86E-02  2.84E-01
 
 ETA7
+        3.27E-02 -5.97E-02  3.64E-02 -8.24E-02  2.93E-02  1.57E-03  2.95E-01
 
 ETA8
+        1.00E-01  7.53E-02  4.68E-02  5.21E-02  5.01E-03 -6.05E-02  7.15E-02  2.78E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.35E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.38E-01
 
 ETA2
+       -1.37E-01  4.85E-01
 
 ETA3
+        2.20E-01 -5.17E-02  3.97E-01
 
 ETA4
+        1.08E-01  2.37E-01 -5.05E-02  5.39E-01
 
 ETA5
+        1.07E-01  7.28E-02  2.29E-03 -1.32E-01  4.80E-01
 
 ETA6
+       -9.61E-02  1.88E-02  8.30E-02  5.69E-02 -3.05E-01  5.29E-01
 
 ETA7
+        1.10E-01 -2.19E-01  1.68E-01 -2.79E-01  1.11E-01  4.17E-03  5.40E-01
 
 ETA8
+        3.48E-01  2.93E-01  2.19E-01  1.79E-01  1.93E-02 -2.14E-01  2.46E-01  5.24E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.66E-02
 
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
 
         7.88E-02  7.86E-02  6.40E-02  8.05E-02  7.27E-02  8.07E-02  7.75E-02  7.71E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        6.23E-02
 
 ETA2
+        4.27E-02  6.46E-02
 
 ETA3
+        3.42E-02  3.41E-02  4.18E-02
 
 ETA4
+        4.32E-02  4.50E-02  3.71E-02  6.74E-02
 
 ETA5
+        3.88E-02  3.69E-02  3.19E-02  4.08E-02  5.39E-02
 
 ETA6
+        4.46E-02  4.43E-02  3.79E-02  4.64E-02  4.27E-02  7.38E-02
 
 ETA7
+        4.47E-02  4.84E-02  3.69E-02  4.56E-02  4.03E-02  4.82E-02  6.71E-02
 
 ETA8
+        4.46E-02  4.35E-02  3.60E-02  4.60E-02  3.90E-02  4.73E-02  4.68E-02  6.24E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.41E-04
 
 EPS2
+        0.00E+00  1.21E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.60E-02
 
 ETA2
+        1.48E-01  6.40E-02
 
 ETA3
+        1.45E-01  1.68E-01  5.06E-02
 
 ETA4
+        1.39E-01  1.50E-01  1.64E-01  6.07E-02
 
 ETA5
+        1.41E-01  1.48E-01  1.58E-01  1.46E-01  5.42E-02
 
 ETA6
+        1.47E-01  1.63E-01  1.67E-01  1.54E-01  1.43E-01  6.76E-02
 
 ETA7
+        1.42E-01  1.59E-01  1.58E-01  1.36E-01  1.44E-01  1.59E-01  6.02E-02
 
 ETA8
+        1.25E-01  1.50E-01  1.52E-01  1.46E-01  1.47E-01  1.53E-01  1.42E-01  5.80E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.30E-03
 
 EPS2
+        0.00E+00  4.04E-03
 
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
+        6.21E-03
 
 TH 2
+       -1.04E-03  6.18E-03
 
 TH 3
+        8.86E-04 -2.38E-06  4.10E-03
 
 TH 4
+        6.92E-04  1.23E-03  2.96E-04  6.47E-03
 
 TH 5
+        6.98E-04  1.82E-04  8.81E-05 -8.18E-04  5.29E-03
 
 TH 6
+       -9.09E-04 -7.82E-05  7.45E-05  4.85E-04 -1.17E-03  6.52E-03
 
 TH 7
+        6.11E-04 -1.26E-03  7.50E-04 -1.80E-03  5.72E-04  6.01E-05  6.00E-03
 
 TH 8
+        2.12E-03  1.32E-03  1.28E-03  1.31E-03  7.72E-05 -1.26E-03  1.48E-03  5.94E-03
 
 OM11
+        6.17E-06  6.38E-05 -1.34E-04  2.14E-04 -1.33E-04 -4.72E-05 -8.99E-08  7.32E-06  3.88E-03
 
 OM12
+       -4.77E-05  3.35E-04  4.28E-05 -4.55E-06 -6.36E-05 -9.75E-06 -9.33E-05  6.66E-05 -6.84E-04  1.82E-03
 
 OM13
+       -2.31E-05  8.05E-05  1.31E-04  8.69E-05 -9.32E-05  5.84E-05  7.41E-05 -2.66E-05  4.65E-04 -9.05E-05  1.17E-03
 
 OM14
+        1.77E-04  3.12E-05  1.16E-04  5.47E-05  7.01E-05 -1.72E-04 -1.89E-05  1.68E-04  4.47E-04  2.55E-04  1.59E-04  1.87E-03
 
 OM15
+       -8.68E-05  4.37E-05  3.00E-05  1.84E-04 -1.06E-05 -6.26E-06  5.36E-05  1.24E-04  2.74E-04  9.37E-05  6.58E-05 -1.41E-04
          1.50E-03
 
 OM16
+       -2.72E-06 -7.88E-05 -8.84E-05 -4.89E-05 -5.03E-06  1.24E-04  1.12E-04 -5.14E-05 -3.72E-04 -1.59E-05  2.98E-05 -1.99E-05
         -3.69E-04  1.99E-03
 
 OM17
+       -2.50E-05  4.39E-05  6.92E-05  1.58E-04 -2.04E-05  2.14E-04  1.37E-04  3.74E-05  3.77E-04 -4.55E-04  2.10E-04 -4.32E-04
          1.79E-04  4.54E-05  2.00E-03
 
 OM18
+        3.33E-05  7.16E-06  1.25E-05  8.73E-05  2.76E-05  6.14E-05  1.30E-04  1.13E-04  1.22E-03  2.32E-04  4.39E-04  4.46E-04
          9.81E-05 -4.48E-04  5.89E-04  1.99E-03
 
 OM22
+        4.76E-05 -1.06E-03 -9.78E-06 -1.30E-04  1.81E-04  8.50E-05  3.54E-04  1.24E-04  1.34E-04 -8.60E-04 -3.90E-05 -1.06E-04
          3.61E-05 -1.61E-05  2.68E-04  3.41E-06  4.18E-03
 
 OM23
+        1.83E-05  1.59E-04  5.53E-05  1.87E-05 -1.23E-05 -1.05E-05 -4.76E-06 -9.39E-06 -1.55E-04  3.08E-04 -1.44E-04  6.90E-05
          4.64E-05 -5.20E-05 -4.99E-05 -3.87E-06 -6.57E-05  1.16E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.15E-05 -4.80E-04  4.96E-05  8.01E-05  3.15E-05  1.05E-04  1.70E-04  1.62E-04 -5.11E-06 -3.05E-05  5.37E-05 -1.91E-04
          7.90E-05 -1.10E-04  3.58E-05  1.40E-05  1.10E-03 -7.18E-05  2.02E-03
 
 OM25
+        1.17E-04  1.15E-04 -1.04E-05 -8.03E-05  2.44E-05  2.35E-05  1.10E-05 -7.25E-05  6.33E-05  7.38E-05 -2.18E-05  2.76E-05
         -1.96E-04  1.76E-05  3.69E-05  7.58E-05  1.79E-04  1.33E-04 -1.79E-04  1.36E-03
 
 OM26
+       -4.72E-05 -8.21E-05  1.29E-04 -4.31E-05 -1.40E-05  1.18E-04 -6.69E-05  1.29E-05 -1.09E-04 -8.29E-05 -1.87E-05  4.86E-05
         -1.08E-05 -2.88E-04  2.88E-05 -1.55E-05 -1.15E-04  5.94E-05  1.35E-04 -3.89E-04  1.96E-03
 
 OM27
+       -9.09E-05  7.75E-04 -8.64E-05 -2.46E-05  7.65E-05 -1.34E-04 -1.52E-04 -1.21E-04 -5.39E-05  4.95E-04 -5.49E-05  1.12E-04
         -2.77E-05 -4.94E-05 -4.02E-04 -7.38E-05 -1.25E-03  3.57E-04 -8.72E-04  2.60E-04 -3.75E-05  2.34E-03
 
 OM28
+       -1.56E-05 -5.71E-06 -3.43E-05 -1.01E-04  8.18E-05 -2.35E-06  6.78E-05 -1.23E-04 -1.64E-04  5.79E-04 -9.33E-05  5.68E-05
          9.37E-05  3.59E-05 -2.31E-04 -7.93E-05  8.08E-04  3.91E-04  3.62E-04  8.87E-05 -4.96E-04  5.27E-04  1.90E-03
 
 OM33
+        9.13E-05  6.62E-05 -1.28E-04 -7.45E-05 -2.67E-05  7.83E-05  1.18E-04 -6.71E-05 -1.65E-05  5.43E-05  3.05E-04 -2.32E-05
         -1.22E-05 -5.10E-05  6.39E-05  4.71E-05  3.80E-05 -3.28E-05  6.77E-05  5.46E-05 -2.58E-05 -1.82E-05  4.82E-05  1.75E-03
 
 OM34
+        6.30E-05  5.08E-05  2.12E-04  1.84E-04  9.36E-05 -1.79E-04 -6.37E-05  8.28E-05  7.04E-05  9.76E-05  1.21E-04  3.82E-04
          5.78E-05 -8.73E-05  8.80E-06  1.50E-04 -4.06E-05  3.14E-04 -4.03E-05  3.32E-05  1.32E-04  9.22E-05  4.40E-05 -7.84E-05
         1.38E-03
 
 OM35
+       -2.73E-05  2.61E-05 -9.86E-05 -6.03E-05 -5.53E-05  9.07E-05  1.03E-04  4.61E-05 -4.20E-05 -2.54E-05  1.38E-04 -2.15E-05
          2.57E-04 -6.59E-05  5.63E-05  2.20E-05  2.06E-05  7.88E-05  3.13E-05 -7.35E-05 -1.05E-05  2.89E-05  4.44E-05  6.50E-05
        -1.58E-04  1.02E-03
 
 OM36
+        1.89E-06 -1.47E-04  3.95E-06  1.16E-05  3.02E-05  9.54E-05 -8.19E-05 -7.19E-05 -3.70E-05  1.84E-05 -1.20E-04  6.98E-05
         -2.57E-05  2.15E-04 -1.01E-04 -8.64E-05  2.68E-06 -3.38E-05 -7.08E-05  4.72E-05  1.03E-05  4.76E-05 -6.87E-05  1.06E-04
         8.08E-05 -3.60E-04  1.44E-03
 
 OM37
+       -1.24E-04 -5.69E-05 -3.98E-05 -8.97E-05 -3.00E-05  1.89E-05  6.32E-05 -7.53E-05  1.06E-05 -9.72E-05  2.07E-04 -4.09E-05
         -1.75E-06  4.52E-05  3.12E-04  1.43E-04  7.33E-07 -3.45E-04 -1.73E-05 -4.42E-05 -4.67E-05 -6.97E-05 -9.56E-05  3.07E-04
        -4.31E-04  1.85E-04 -4.80E-05  1.36E-03
 
 OM38
+       -5.12E-05  1.09E-04  5.79E-05 -1.29E-05  6.28E-05 -7.05E-05  1.78E-04  1.10E-04  1.42E-04  7.70E-05  4.54E-04  7.29E-05
          2.90E-05 -4.79E-05  1.84E-04  3.95E-04 -2.17E-05  3.21E-04 -2.02E-05  7.14E-05 -2.56E-05  1.32E-04  8.69E-05  4.38E-04
         2.16E-04  5.43E-05 -3.21E-04  3.72E-04  1.30E-03
 
 OM44
+        7.21E-05  1.40E-04  2.32E-04  5.16E-04  4.43E-05  3.27E-05 -9.68E-05  3.31E-05  5.79E-06  9.49E-05  1.32E-04  5.16E-04
          6.91E-05 -2.59E-04 -8.46E-05  1.92E-04  6.61E-05 -4.92E-05  6.67E-04 -1.75E-05  3.56E-05 -1.19E-04  6.17E-05 -1.02E-04
         2.27E-04 -1.62E-04 -3.08E-05 -2.52E-05  7.40E-05  4.55E-03
 
 OM45
+        1.12E-04  3.21E-05 -1.90E-05 -5.41E-05  1.32E-04 -1.62E-05  9.35E-05  6.25E-05  1.40E-04 -6.44E-05  9.28E-06  9.67E-05
          1.06E-04  1.13E-06  4.35E-05 -1.76E-06  1.52E-04  8.57E-05  6.17E-05  3.39E-04 -5.94E-05  8.81E-06  5.40E-05  5.39E-05
         7.36E-05 -1.37E-05 -1.58E-05 -2.71E-05  7.78E-05 -4.41E-04  1.67E-03
 
 OM46
+        7.09E-05 -1.46E-04  2.73E-05  6.22E-06 -1.17E-04  3.07E-04  1.81E-05  7.83E-05 -1.55E-04 -5.52E-05 -8.81E-05 -1.64E-04
          3.84E-05  2.11E-04  4.42E-05 -1.92E-04  1.13E-05  3.86E-06 -3.64E-05 -9.15E-05  4.54E-04 -5.55E-05 -6.97E-05 -2.19E-05
         5.65E-05 -7.13E-05  1.12E-04  1.96E-05 -1.17E-04  9.88E-06 -4.06E-04  2.15E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.22E-04  1.13E-04  1.70E-05 -6.37E-05  1.42E-04 -1.42E-04  7.48E-05  5.52E-05 -7.17E-05 -1.08E-05 -2.84E-05  1.43E-04
         -1.09E-04  2.31E-04  1.35E-04  4.79E-05 -2.56E-04  8.35E-05 -5.05E-04  1.25E-04  6.81E-05  5.79E-04 -4.90E-06 -7.01E-05
         2.80E-04 -5.99E-05  9.05E-05 -6.67E-05  3.21E-05 -9.59E-04  2.48E-04  1.88E-04  2.08E-03
 
 OM48
+       -1.15E-05  1.02E-04 -7.95E-05  7.16E-05  5.30E-05  1.77E-05 -3.01E-05 -7.73E-05  1.46E-04  2.13E-04  1.36E-04  7.83E-04
         -7.85E-05 -2.76E-05 -9.14E-05  3.81E-04  1.35E-04  8.09E-05  5.04E-04 -4.41E-05  3.04E-06 -1.66E-06  4.79E-04 -2.90E-05
         4.79E-04 -3.39E-05  1.30E-05 -1.21E-04  4.66E-05  1.01E-03 -5.18E-05 -3.99E-04  2.62E-04  2.11E-03
 
 OM55
+        4.99E-05 -1.21E-04  6.66E-05 -1.27E-04  7.37E-05 -9.10E-05  2.83E-05 -2.93E-05  5.79E-05 -3.54E-06  2.65E-05  9.00E-05
          2.33E-04 -8.15E-05  2.58E-05  7.62E-05  1.89E-04  7.38E-07 -1.34E-05  1.57E-04 -5.61E-05 -8.25E-05  1.02E-06  5.94E-05
         1.03E-04  2.94E-05 -2.93E-06  1.70E-05 -3.14E-05  1.02E-04 -3.50E-04  1.11E-04 -9.57E-05  7.25E-05  2.91E-03
 
 OM56
+        4.49E-05  4.64E-05 -1.03E-04  8.85E-05 -4.51E-05 -1.42E-04 -9.99E-05 -2.30E-05 -2.85E-05  2.21E-05  5.83E-05 -6.62E-05
         -1.01E-04  1.79E-04 -5.19E-05 -3.15E-05 -1.81E-04 -2.82E-05  5.66E-05 -1.56E-04  2.14E-04  6.64E-05 -3.52E-05 -1.82E-05
        -9.61E-05  1.08E-04 -8.71E-05  4.59E-06  4.27E-05 -1.76E-04  1.17E-04 -2.27E-04  3.77E-05 -5.33E-05 -7.76E-04  1.83E-03
 
 OM57
+       -6.20E-05 -7.54E-05 -5.17E-05  1.63E-05 -1.26E-04  6.63E-05 -4.04E-06 -6.81E-05 -9.26E-05 -4.73E-06  9.21E-05 -1.08E-04
          2.34E-04 -1.29E-04  9.71E-05  5.72E-05 -1.01E-04 -7.57E-06  3.42E-05 -4.18E-04  2.06E-04  4.18E-05 -2.73E-06  3.79E-05
        -1.59E-05  2.39E-04 -1.32E-04  4.45E-05  1.81E-05 -2.91E-05 -4.84E-04  1.33E-04 -2.59E-04 -6.95E-05  3.48E-04  7.35E-05
          1.63E-03
 
 OM58
+        3.97E-05  1.07E-04  9.93E-06 -7.40E-05  7.52E-05 -1.24E-04  2.88E-05  9.17E-05  9.90E-05  6.11E-05  2.11E-05 -6.60E-05
          5.34E-04 -1.95E-04  1.21E-04  1.94E-04  2.65E-05  8.20E-05 -5.16E-05  3.43E-04 -8.80E-05  1.44E-04  1.49E-04  5.22E-05
         3.53E-05  2.57E-04 -5.41E-05  8.68E-06  9.53E-05 -3.93E-05  2.76E-04 -5.55E-05 -6.69E-05 -2.40E-04 -4.50E-05 -3.60E-04
          3.50E-04  1.52E-03
 
 OM66
+       -2.65E-04 -1.66E-04  1.71E-04 -1.34E-04  2.04E-04  1.17E-04  1.02E-05 -2.64E-04  1.34E-04 -2.64E-05  1.09E-04  1.25E-05
          5.30E-05 -3.37E-04  1.53E-05  1.41E-04  2.66E-04  1.91E-05  1.35E-04  9.66E-05  9.40E-05  8.76E-08  8.73E-05  2.56E-04
         1.82E-04 -9.40E-05  3.22E-04  2.49E-05  1.15E-04  1.77E-04  1.34E-04  1.43E-04 -2.83E-05  4.39E-05  1.92E-04 -1.04E-03
         -1.07E-04  1.99E-04  5.45E-03
 
 OM67
+        1.75E-04 -1.27E-04 -2.75E-05 -7.65E-05  1.90E-04 -1.78E-04 -5.82E-07  8.91E-05  4.16E-05 -1.21E-05 -7.80E-06  1.77E-04
         -7.89E-05  3.24E-04 -2.74E-04 -1.22E-04  1.92E-04 -7.32E-05 -1.16E-05  8.80E-05 -5.52E-04 -1.09E-04  1.15E-04 -1.11E-04
        -1.01E-04 -7.82E-05  2.48E-04 -5.47E-06 -7.93E-05  1.29E-05  1.44E-04 -5.75E-04 -5.88E-05  1.84E-04 -5.21E-05  9.05E-05
         -5.38E-04 -2.22E-04  9.37E-05  2.33E-03
 
 OM68
+        1.21E-04 -2.16E-04 -8.95E-05  9.22E-05 -1.62E-05  2.35E-04  2.85E-05  7.48E-06 -2.07E-04  2.45E-06 -1.13E-04  5.94E-05
         -1.32E-04  7.83E-04 -9.18E-05 -3.34E-04 -5.75E-06 -8.13E-06  2.36E-05 -9.22E-05  4.94E-04 -5.02E-05 -8.16E-05 -1.48E-04
        -3.80E-05 -7.43E-05  4.52E-04 -1.95E-05 -1.01E-04 -2.97E-05 -4.94E-05  4.41E-04  7.23E-05  2.48E-05 -2.06E-05  1.13E-04
         -1.17E-04 -4.08E-04 -9.09E-04  5.66E-04  2.24E-03
 
 OM77
+        1.75E-04 -1.46E-04  3.71E-05  2.10E-04  3.70E-05 -1.41E-05 -1.20E-04  1.62E-04  2.33E-04 -1.69E-04  2.17E-04 -8.94E-05
          3.95E-05 -4.25E-05  5.78E-04  2.20E-04  3.67E-04 -2.51E-05  3.18E-04 -1.93E-05 -1.96E-05 -1.02E-03 -2.98E-04  1.12E-04
         3.51E-05  4.31E-05 -1.44E-04  2.94E-04  1.19E-04  1.40E-04  7.61E-05 -1.19E-04 -1.03E-03 -1.55E-04  2.14E-04 -7.73E-05
          3.68E-04  2.01E-04  2.12E-05  1.71E-04 -6.88E-05  4.50E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        9.65E-06  5.40E-05  1.76E-04  1.02E-04  6.54E-05  1.55E-04  2.44E-05  1.19E-04  4.20E-05 -1.02E-04  1.53E-04 -1.17E-04
          4.74E-05 -6.54E-05  7.66E-04  3.74E-04 -1.84E-04  1.22E-05 -1.65E-04  1.39E-04  1.66E-04  3.23E-04 -3.55E-04  7.63E-05
         3.65E-05  3.95E-05 -7.69E-05  3.25E-04  3.56E-04 -1.96E-04  1.76E-04  1.02E-04  3.08E-04 -4.02E-04  4.17E-05 -9.20E-05
          4.82E-05  2.69E-04  8.81E-05 -4.80E-04 -1.01E-04  1.12E-03  2.19E-03
 
 OM88
+       -2.35E-05  5.62E-05  1.49E-04  3.07E-05  8.32E-05  2.60E-04  2.55E-04  9.50E-05  3.17E-04  3.14E-04  3.27E-04  2.28E-04
          9.05E-05 -2.58E-04  4.31E-04  1.30E-03  1.93E-04  1.55E-04  1.42E-04  1.18E-04 -1.34E-04  2.79E-04  8.15E-04  2.14E-04
         1.62E-04  6.53E-06 -2.09E-04  1.97E-04  7.61E-04  2.20E-04  1.18E-04 -3.16E-04  1.88E-04  7.07E-04  2.00E-04 -1.35E-04
          2.01E-05  2.12E-04  4.87E-04 -1.64E-04 -7.84E-04  3.06E-04  1.13E-03  3.90E-03
 
 SG11
+        9.11E-07 -1.71E-06  1.01E-06 -2.19E-07 -6.86E-07  1.41E-06 -3.26E-06 -5.19E-07  6.74E-07 -1.92E-07 -3.24E-07  4.86E-07
          8.63E-07  1.03E-06 -1.65E-07  7.59E-07 -4.49E-07 -1.59E-06 -6.57E-07 -8.92E-07 -7.92E-07 -1.70E-06 -1.71E-06 -1.45E-06
        -6.50E-07  4.32E-07 -1.20E-06  1.12E-06  4.19E-07  6.49E-08 -1.51E-06  1.10E-06 -8.19E-07 -1.62E-06  1.55E-06 -6.75E-07
          6.58E-07 -1.39E-07  1.04E-06 -5.75E-07  1.47E-07  7.42E-07  4.47E-07 -1.01E-06  4.10E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        2.79E-06  9.27E-07 -1.82E-07  2.00E-06  2.41E-06 -1.09E-06  2.24E-06  3.52E-06 -1.37E-06 -1.54E-06  4.54E-07 -5.39E-07
          2.09E-06 -1.12E-06 -1.40E-06 -7.83E-08 -9.88E-07 -8.16E-07 -1.05E-06  5.01E-07 -1.18E-06  2.64E-06 -1.16E-06 -2.08E-07
        -1.25E-06 -2.96E-07  1.14E-06  1.07E-06  1.21E-06 -3.82E-07  5.32E-07 -3.70E-06 -3.00E-07  6.64E-07  8.29E-07  1.62E-06
         -4.37E-07  6.70E-09 -4.76E-06 -8.15E-07  4.85E-08 -3.40E-07  2.88E-07  9.01E-07 -1.88E-08  0.00E+00  1.46E-06
 
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
+        7.88E-02
 
 TH 2
+       -1.68E-01  7.86E-02
 
 TH 3
+        1.76E-01 -4.73E-04  6.40E-02
 
 TH 4
+        1.09E-01  1.95E-01  5.74E-02  8.05E-02
 
 TH 5
+        1.22E-01  3.18E-02  1.89E-02 -1.40E-01  7.27E-02
 
 TH 6
+       -1.43E-01 -1.23E-02  1.44E-02  7.46E-02 -1.99E-01  8.07E-02
 
 TH 7
+        1.00E-01 -2.07E-01  1.51E-01 -2.90E-01  1.01E-01  9.62E-03  7.75E-02
 
 TH 8
+        3.49E-01  2.17E-01  2.60E-01  2.11E-01  1.38E-02 -2.02E-01  2.48E-01  7.71E-02
 
 OM11
+        1.26E-03  1.30E-02 -3.36E-02  4.27E-02 -2.93E-02 -9.38E-03 -1.86E-05  1.53E-03  6.23E-02
 
 OM12
+       -1.42E-02  9.99E-02  1.57E-02 -1.32E-03 -2.05E-02 -2.83E-03 -2.82E-02  2.03E-02 -2.57E-01  4.27E-02
 
 OM13
+       -8.56E-03  2.99E-02  5.97E-02  3.16E-02 -3.75E-02  2.11E-02  2.80E-02 -1.01E-02  2.18E-01 -6.20E-02  3.42E-02
 
 OM14
+        5.19E-02  9.18E-03  4.19E-02  1.57E-02  2.23E-02 -4.93E-02 -5.66E-03  5.05E-02  1.66E-01  1.39E-01  1.08E-01  4.32E-02
 
 OM15
+       -2.84E-02  1.43E-02  1.21E-02  5.89E-02 -3.77E-03 -2.00E-03  1.78E-02  4.14E-02  1.14E-01  5.66E-02  4.96E-02 -8.39E-02
          3.88E-02
 
 OM16
+       -7.74E-04 -2.25E-02 -3.10E-02 -1.36E-02 -1.55E-03  3.46E-02  3.24E-02 -1.50E-02 -1.34E-01 -8.38E-03  1.96E-02 -1.04E-02
         -2.14E-01  4.46E-02
 
 OM17
+       -7.11E-03  1.25E-02  2.42E-02  4.40E-02 -6.27E-03  5.93E-02  3.97E-02  1.08E-02  1.35E-01 -2.39E-01  1.38E-01 -2.24E-01
          1.03E-01  2.28E-02  4.47E-02
 
 OM18
+        9.49E-03  2.04E-03  4.37E-03  2.43E-02  8.52E-03  1.71E-02  3.77E-02  3.30E-02  4.38E-01  1.22E-01  2.88E-01  2.32E-01
          5.68E-02 -2.25E-01  2.96E-01  4.46E-02
 
 OM22
+        9.35E-03 -2.09E-01 -2.36E-03 -2.50E-02  3.84E-02  1.63E-02  7.07E-02  2.49E-02  3.32E-02 -3.12E-01 -1.77E-02 -3.81E-02
          1.44E-02 -5.59E-03  9.29E-02  1.18E-03  6.46E-02
 
 OM23
+        6.83E-03  5.92E-02  2.54E-02  6.83E-03 -4.98E-03 -3.81E-03 -1.80E-03 -3.57E-03 -7.30E-02  2.12E-01 -1.24E-01  4.69E-02
          3.51E-02 -3.42E-02 -3.28E-02 -2.55E-03 -2.98E-02  3.41E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        1.17E-02 -1.36E-01  1.72E-02  2.21E-02  9.63E-03  2.88E-02  4.88E-02  4.66E-02 -1.82E-03 -1.59E-02  3.49E-02 -9.85E-02
          4.53E-02 -5.47E-02  1.78E-02  7.00E-03  3.79E-01 -4.68E-02  4.50E-02
 
 OM25
+        4.03E-02  3.97E-02 -4.42E-03 -2.70E-02  9.10E-03  7.89E-03  3.85E-03 -2.55E-02  2.75E-02  4.69E-02 -1.73E-02  1.73E-02
         -1.37E-01  1.07E-02  2.24E-02  4.61E-02  7.50E-02  1.06E-01 -1.08E-01  3.69E-02
 
 OM26
+       -1.35E-02 -2.36E-02  4.56E-02 -1.21E-02 -4.34E-03  3.30E-02 -1.95E-02  3.78E-03 -3.94E-02 -4.39E-02 -1.23E-02  2.54E-02
         -6.29E-03 -1.46E-01  1.46E-02 -7.86E-03 -4.03E-02  3.94E-02  6.76E-02 -2.38E-01  4.43E-02
 
 OM27
+       -2.39E-02  2.04E-01 -2.79E-02 -6.31E-03  2.17E-02 -3.42E-02 -4.06E-02 -3.25E-02 -1.79E-02  2.40E-01 -3.32E-02  5.36E-02
         -1.48E-02 -2.29E-02 -1.86E-01 -3.43E-02 -4.01E-01  2.16E-01 -4.01E-01  1.46E-01 -1.75E-02  4.84E-02
 
 OM28
+       -4.56E-03 -1.67E-03 -1.23E-02 -2.88E-02  2.58E-02 -6.70E-04  2.01E-02 -3.65E-02 -6.04E-02  3.12E-01 -6.27E-02  3.02E-02
          5.55E-02  1.85E-02 -1.19E-01 -4.09E-02  2.87E-01  2.64E-01  1.85E-01  5.52E-02 -2.57E-01  2.50E-01  4.35E-02
 
 OM33
+        2.77E-02  2.01E-02 -4.79E-02 -2.21E-02 -8.76E-03  2.32E-02  3.65E-02 -2.08E-02 -6.32E-03  3.04E-02  2.14E-01 -1.29E-02
         -7.54E-03 -2.73E-02  3.42E-02  2.52E-02  1.41E-02 -2.30E-02  3.60E-02  3.54E-02 -1.39E-02 -8.98E-03  2.64E-02  4.18E-02
 
 OM34
+        2.15E-02  1.74E-02  8.91E-02  6.17E-02  3.47E-02 -5.96E-02 -2.22E-02  2.89E-02  3.05E-02  6.16E-02  9.55E-02  2.38E-01
          4.02E-02 -5.28E-02  5.30E-03  9.10E-02 -1.69E-02  2.48E-01 -2.41E-02  2.43E-02  8.05E-02  5.14E-02  2.73E-02 -5.05E-02
         3.71E-02
 
 OM35
+       -1.09E-02  1.04E-02 -4.83E-02 -2.35E-02 -2.39E-02  3.53E-02  4.18E-02  1.88E-02 -2.12E-02 -1.87E-02  1.26E-01 -1.56E-02
          2.08E-01 -4.64E-02  3.95E-02  1.55E-02  9.99E-03  7.26E-02  2.19E-02 -6.26E-02 -7.45E-03  1.87E-02  3.20E-02  4.87E-02
        -1.34E-01  3.19E-02
 
 OM36
+        6.31E-04 -4.94E-02  1.63E-03  3.82E-03  1.10E-02  3.12E-02 -2.79E-02 -2.46E-02 -1.57E-02  1.14E-02 -9.27E-02  4.26E-02
         -1.75E-02  1.27E-01 -5.94E-02 -5.12E-02  1.09E-03 -2.62E-02 -4.15E-02  3.37E-02  6.15E-03  2.60E-02 -4.16E-02  6.68E-02
         5.74E-02 -2.98E-01  3.79E-02
 
 OM37
+       -4.26E-02 -1.96E-02 -1.69E-02 -3.02E-02 -1.12E-02  6.34E-03  2.21E-02 -2.65E-02  4.63E-03 -6.18E-02  1.64E-01 -2.56E-02
         -1.23E-03  2.74E-02  1.89E-01  8.69E-02  3.07E-04 -2.74E-01 -1.04E-02 -3.24E-02 -2.86E-02 -3.91E-02 -5.95E-02  1.99E-01
        -3.15E-01  1.57E-01 -3.43E-02  3.69E-02
 
 OM38
+       -1.81E-02  3.85E-02  2.51E-02 -4.46E-03  2.40E-02 -2.43E-02  6.38E-02  3.98E-02  6.34E-02  5.01E-02  3.69E-01  4.69E-02
          2.08E-02 -2.98E-02  1.14E-01  2.46E-01 -9.34E-03  2.62E-01 -1.25E-02  5.38E-02 -1.60E-02  7.57E-02  5.54E-02  2.91E-01
         1.61E-01  4.74E-02 -2.35E-01  2.80E-01  3.60E-02
 
 OM44
+        1.36E-02  2.65E-02  5.38E-02  9.52E-02  9.04E-03  6.00E-03 -1.85E-02  6.37E-03  1.38E-03  3.30E-02  5.72E-02  1.77E-01
          2.64E-02 -8.61E-02 -2.81E-02  6.38E-02  1.52E-02 -2.14E-02  2.20E-01 -7.03E-03  1.19E-02 -3.65E-02  2.10E-02 -3.61E-02
         9.08E-02 -7.53E-02 -1.21E-02 -1.01E-02  3.05E-02  6.74E-02
 
 OM45
+        3.47E-02  9.99E-03 -7.28E-03 -1.65E-02  4.43E-02 -4.93E-03  2.96E-02  1.99E-02  5.51E-02 -3.70E-02  6.64E-03  5.48E-02
          6.70E-02  6.21E-04  2.38E-02 -9.66E-04  5.76E-02  6.16E-02  3.36E-02  2.25E-01 -3.28E-02  4.46E-03  3.04E-02  3.15E-02
         4.86E-02 -1.06E-02 -1.02E-02 -1.80E-02  5.29E-02 -1.60E-01  4.08E-02
 
 OM46
+        1.94E-02 -4.00E-02  9.18E-03  1.67E-03 -3.46E-02  8.18E-02  5.04E-03  2.19E-02 -5.36E-02 -2.79E-02 -5.55E-02 -8.20E-02
          2.13E-02  1.02E-01  2.13E-02 -9.29E-02  3.76E-03  2.44E-03 -1.74E-02 -5.35E-02  2.21E-01 -2.47E-02 -3.45E-02 -1.13E-02
         3.28E-02 -4.82E-02  6.37E-02  1.14E-02 -6.98E-02  3.16E-03 -2.14E-01  4.64E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        3.39E-02  3.15E-02  5.82E-03 -1.74E-02  4.28E-02 -3.85E-02  2.12E-02  1.57E-02 -2.52E-02 -5.56E-03 -1.82E-02  7.24E-02
         -6.16E-02  1.14E-01  6.59E-02  2.36E-02 -8.68E-02  5.37E-02 -2.46E-01  7.44E-02  3.37E-02  2.62E-01 -2.47E-03 -3.67E-02
         1.65E-01 -4.12E-02  5.23E-02 -3.96E-02  1.95E-02 -3.12E-01  1.33E-01  8.88E-02  4.56E-02
 
 OM48
+       -3.18E-03  2.83E-02 -2.70E-02  1.94E-02  1.58E-02  4.77E-03 -8.46E-03 -2.18E-02  5.11E-02  1.08E-01  8.65E-02  3.94E-01
         -4.40E-02 -1.34E-02 -4.44E-02  1.86E-01  4.54E-02  5.16E-02  2.44E-01 -2.60E-02  1.49E-03 -7.45E-04  2.39E-01 -1.51E-02
         2.80E-01 -2.32E-02  7.43E-03 -7.13E-02  2.82E-02  3.27E-01 -2.76E-02 -1.87E-01  1.25E-01  4.60E-02
 
 OM55
+        1.18E-02 -2.86E-02  1.93E-02 -2.94E-02  1.88E-02 -2.09E-02  6.78E-03 -7.04E-03  1.72E-02 -1.54E-03  1.43E-02  3.86E-02
          1.12E-01 -3.39E-02  1.07E-02  3.17E-02  5.41E-02  4.01E-04 -5.53E-03  7.89E-02 -2.35E-02 -3.16E-02  4.34E-04  2.63E-02
         5.17E-02  1.71E-02 -1.43E-03  8.56E-03 -1.62E-02  2.79E-02 -1.59E-01  4.42E-02 -3.89E-02  2.92E-02  5.39E-02
 
 OM56
+        1.33E-02  1.38E-02 -3.76E-02  2.57E-02 -1.45E-02 -4.12E-02 -3.02E-02 -6.99E-03 -1.07E-02  1.21E-02  3.99E-02 -3.58E-02
         -6.08E-02  9.38E-02 -2.72E-02 -1.65E-02 -6.54E-02 -1.94E-02  2.94E-02 -9.88E-02  1.13E-01  3.21E-02 -1.89E-02 -1.02E-02
        -6.06E-02  7.93E-02 -5.38E-02  2.91E-03  2.78E-02 -6.11E-02  6.70E-02 -1.14E-01  1.93E-02 -2.71E-02 -3.37E-01  4.27E-02
 
 OM57
+       -1.95E-02 -2.38E-02 -2.00E-02  5.03E-03 -4.30E-02  2.04E-02 -1.29E-03 -2.19E-02 -3.69E-02 -2.75E-03  6.68E-02 -6.21E-02
          1.49E-01 -7.17E-02  5.39E-02  3.18E-02 -3.88E-02 -5.51E-03  1.89E-02 -2.81E-01  1.15E-01  2.15E-02 -1.56E-03  2.24E-02
        -1.07E-02  1.86E-01 -8.61E-02  2.99E-02  1.25E-02 -1.07E-02 -2.94E-01  7.10E-02 -1.41E-01 -3.75E-02  1.60E-01  4.26E-02
          4.03E-02
 
 OM58
+        1.29E-02  3.50E-02  3.98E-03 -2.36E-02  2.65E-02 -3.95E-02  9.54E-03  3.05E-02  4.07E-02  3.67E-02  1.58E-02 -3.92E-02
          3.53E-01 -1.12E-01  6.97E-02  1.12E-01  1.05E-02  6.17E-02 -2.94E-02  2.39E-01 -5.09E-02  7.62E-02  8.76E-02  3.20E-02
         2.44E-02  2.07E-01 -3.65E-02  6.03E-03  6.79E-02 -1.50E-02  1.74E-01 -3.06E-02 -3.76E-02 -1.34E-01 -2.14E-02 -2.16E-01
          2.22E-01  3.90E-02
 
 OM66
+       -4.56E-02 -2.86E-02  3.62E-02 -2.25E-02  3.80E-02  1.96E-02  1.79E-03 -4.63E-02  2.92E-02 -8.39E-03  4.32E-02  3.94E-03
          1.85E-02 -1.02E-01  4.64E-03  4.29E-02  5.58E-02  7.60E-03  4.06E-02  3.55E-02  2.87E-02  2.45E-05  2.72E-02  8.30E-02
         6.65E-02 -4.00E-02  1.15E-01  9.14E-03  4.33E-02  3.55E-02  4.43E-02  4.17E-02 -8.41E-03  1.29E-02  4.82E-02 -3.29E-01
         -3.58E-02  6.93E-02  7.38E-02
 
 OM67
+        4.61E-02 -3.34E-02 -8.92E-03 -1.97E-02  5.42E-02 -4.56E-02 -1.56E-04  2.40E-02  1.39E-02 -5.90E-03 -4.73E-03  8.49E-02
         -4.22E-02  1.51E-01 -1.27E-01 -5.69E-02  6.15E-02 -4.46E-02 -5.36E-03  4.95E-02 -2.58E-01 -4.68E-02  5.47E-02 -5.51E-02
        -5.67E-02 -5.09E-02  1.36E-01 -3.07E-03 -4.57E-02  3.98E-03  7.30E-02 -2.57E-01 -2.67E-02  8.32E-02 -2.00E-02  4.39E-02
         -2.77E-01 -1.18E-01  2.63E-02  4.82E-02
 
 OM68
+        3.25E-02 -5.81E-02 -2.96E-02  2.42E-02 -4.70E-03  6.16E-02  7.77E-03  2.05E-03 -7.02E-02  1.21E-03 -7.02E-02  2.91E-02
         -7.18E-02  3.71E-01 -4.34E-02 -1.59E-01 -1.88E-03 -5.05E-03  1.11E-02 -5.29E-02  2.36E-01 -2.19E-02 -3.96E-02 -7.47E-02
        -2.17E-02 -4.93E-02  2.52E-01 -1.12E-02 -5.94E-02 -9.33E-03 -2.56E-02  2.01E-01  3.35E-02  1.14E-02 -8.08E-03  5.58E-02
         -6.12E-02 -2.21E-01 -2.60E-01  2.48E-01  4.73E-02
 
 OM77
+        3.31E-02 -2.78E-02  8.65E-03  3.90E-02  7.58E-03 -2.60E-03 -2.31E-02  3.13E-02  5.58E-02 -5.92E-02  9.45E-02 -3.08E-02
          1.52E-02 -1.42E-02  1.93E-01  7.38E-02  8.46E-02 -1.10E-02  1.05E-01 -7.82E-03 -6.61E-03 -3.16E-01 -1.02E-01  3.98E-02
         1.41E-02  2.02E-02 -5.65E-02  1.19E-01  4.91E-02  3.10E-02  2.78E-02 -3.82E-02 -3.38E-01 -5.04E-02  5.91E-02 -2.70E-02
          1.36E-01  7.68E-02  4.28E-03  5.28E-02 -2.17E-02  6.71E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        2.62E-03  1.47E-02  5.87E-02  2.70E-02  1.92E-02  4.09E-02  6.74E-03  3.30E-02  1.44E-02 -5.11E-02  9.54E-02 -5.80E-02
          2.61E-02 -3.14E-02  3.66E-01  1.79E-01 -6.10E-02  7.64E-03 -7.84E-02  8.06E-02  8.03E-02  1.43E-01 -1.74E-01  3.90E-02
         2.10E-02  2.65E-02 -4.34E-02  1.88E-01  2.12E-01 -6.22E-02  9.23E-02  4.70E-02  1.44E-01 -1.87E-01  1.65E-02 -4.60E-02
          2.55E-02  1.47E-01  2.55E-02 -2.13E-01 -4.55E-02  3.56E-01  4.68E-02
 
 OM88
+       -4.79E-03  1.15E-02  3.73E-02  6.12E-03  1.83E-02  5.15E-02  5.28E-02  1.97E-02  8.15E-02  1.18E-01  1.53E-01  8.45E-02
          3.74E-02 -9.27E-02  1.54E-01  4.67E-01  4.77E-02  7.30E-02  5.06E-02  5.11E-02 -4.84E-02  9.25E-02  3.00E-01  8.18E-02
         7.00E-02  3.28E-03 -8.81E-02  8.53E-02  3.39E-01  5.23E-02  4.64E-02 -1.09E-01  6.59E-02  2.46E-01  5.94E-02 -5.08E-02
          7.99E-03  8.72E-02  1.06E-01 -5.44E-02 -2.65E-01  7.30E-02  3.86E-01  6.24E-02
 
 SG11
+        1.80E-02 -3.40E-02  2.46E-02 -4.24E-03 -1.47E-02  2.73E-02 -6.57E-02 -1.05E-02  1.69E-02 -7.03E-03 -1.48E-02  1.75E-02
          3.47E-02  3.62E-02 -5.78E-03  2.66E-02 -1.08E-02 -7.31E-02 -2.28E-02 -3.78E-02 -2.79E-02 -5.47E-02 -6.11E-02 -5.40E-02
        -2.73E-02  2.11E-02 -4.96E-02  4.73E-02  1.82E-02  1.50E-03 -5.76E-02  3.69E-02 -2.80E-02 -5.50E-02  4.49E-02 -2.46E-02
          2.55E-02 -5.55E-03  2.20E-02 -1.86E-02  4.86E-03  1.73E-02  1.49E-02 -2.53E-02  6.41E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        2.93E-02  9.75E-03 -2.36E-03  2.05E-02  2.74E-02 -1.12E-02  2.39E-02  3.77E-02 -1.82E-02 -2.98E-02  1.10E-02 -1.03E-02
          4.46E-02 -2.08E-02 -2.60E-02 -1.45E-03 -1.26E-02 -1.98E-02 -1.92E-02  1.12E-02 -2.20E-02  4.51E-02 -2.21E-02 -4.11E-03
        -2.78E-02 -7.69E-03  2.48E-02  2.40E-02  2.79E-02 -4.68E-03  1.08E-02 -6.58E-02 -5.44E-03  1.19E-02  1.27E-02  3.14E-02
         -8.96E-03  1.42E-04 -5.33E-02 -1.40E-02  8.48E-04 -4.20E-03  5.09E-03  1.19E-02 -2.43E-02  0.00E+00  1.21E-03
 
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
+        2.13E+02
 
 TH 2
+        6.50E+01  2.26E+02
 
 TH 3
+       -2.23E+01  2.97E+00  2.81E+02
 
 TH 4
+       -1.91E+01 -1.79E+01 -4.24E+00  2.01E+02
 
 TH 5
+       -3.11E+01 -2.86E+01 -2.66E+00  2.17E+01  2.12E+02
 
 TH 6
+        1.05E+01 -1.17E+01 -1.73E+01 -2.52E+01  3.53E+01  1.81E+02
 
 TH 7
+        1.07E+01  5.66E+01 -2.28E+01  7.18E+01 -2.14E+01 -2.50E+01  2.30E+02
 
 TH 8
+       -8.26E+01 -8.97E+01 -5.05E+01 -5.52E+01  2.55E+01  5.22E+01 -8.96E+01  2.81E+02
 
 OM11
+       -6.84E-01 -8.70E+00  1.19E+01 -6.23E+00  1.51E+01  3.37E+00 -2.29E+00  3.69E+00  4.23E+02
 
 OM12
+        1.17E+01 -1.45E+01  2.14E+00  5.21E+00  1.65E+01 -5.52E+00  7.58E+00 -2.50E+01  2.75E+02  1.09E+03
 
 OM13
+       -8.23E+00 -2.37E+01 -5.28E+01 -1.02E+01  2.87E+01 -5.93E-01 -7.59E+00  4.16E+01 -5.34E+01  7.27E+01  1.25E+03
 
 OM14
+       -6.71E+00  9.28E+00 -1.68E+01 -1.57E+00 -4.94E+00  8.43E+00  3.24E+00 -2.36E+01 -8.03E+01 -7.59E+01 -4.02E+01  8.56E+02
 
 OM15
+        2.14E+01  7.55E+00 -3.53E+00 -2.88E+01 -4.27E+00 -6.02E-01 -1.56E+01 -5.59E+00 -1.11E+02 -1.91E+02 -6.16E+01  8.03E+01
          9.91E+02
 
 OM16
+        1.02E+01  2.76E+00  7.62E+00  4.00E-01 -3.78E+00 -5.92E+00 -1.50E+01  3.35E-01 -2.50E+00 -9.25E+01 -1.34E+02 -4.63E+01
          2.16E+02  7.77E+02
 
 OM17
+       -6.23E+00 -3.08E+01 -3.83E+00 -1.06E+01  3.10E+00 -8.98E+00 -1.49E+01  2.13E+00  3.10E+01  2.55E+02 -9.81E+00  2.62E+02
         -1.32E+02 -1.32E+02  8.73E+02
 
 OM18
+        2.45E-01  2.29E+01  1.72E+01  2.24E+00 -2.09E+01 -5.50E+00 -5.63E+00 -5.77E+00 -3.33E+02 -4.78E+02 -2.11E+02 -1.79E+02
          1.71E+02  2.71E+02 -3.83E+02  1.28E+03
 
 OM22
+        1.99E+01  3.41E+01  1.51E+01 -2.79E+00 -9.76E+00 -4.32E+00  1.71E+00 -3.10E+01  5.86E+01  3.43E+02  4.67E+01 -2.68E+01
         -5.75E+01 -4.12E+01  6.46E+01 -1.59E+02  5.21E+02
 
 OM23
+       -1.44E+01 -1.64E+01 -1.98E+01 -7.59E+00  2.38E+01  1.45E+00 -4.48E+00  3.38E+01  2.94E+01 -6.35E+01  3.30E+02 -3.56E+01
         -3.02E+01 -3.87E+01 -7.76E+01 -4.92E+01  4.34E+01  1.43E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        7.34E+00  2.67E+01 -6.72E-02 -1.11E+01 -1.08E+01 -2.00E+00 -4.15E+00 -3.39E+01 -3.71E+01 -7.78E+01 -4.13E+01  2.17E+02
          2.19E+01  1.21E+01  9.14E+01 -7.35E+00 -9.84E+01  1.51E+01  8.67E+02
 
 OM25
+       -2.25E+01 -1.13E+01 -8.24E+00  6.72E+00  1.38E+01 -1.53E+01 -6.08E+00  2.85E+01 -3.36E+01 -1.65E+02 -4.31E+01  2.33E+01
          3.09E+02  1.16E+02 -9.47E+01  9.79E+01 -1.88E+02 -1.23E+02  4.87E+01  1.19E+03
 
 OM26
+        6.81E+00  6.91E+00 -2.91E+01  2.54E+01 -6.05E+00 -7.52E+00  1.76E+01 -2.27E+00 -1.56E+00 -1.04E+02 -6.03E+01 -5.66E+01
          1.17E+02  2.70E+02 -6.94E+01  1.61E+02 -1.29E+02 -1.73E+02 -9.38E+01  3.17E+02  9.37E+02
 
 OM27
+       -2.91E+00 -5.30E+01  2.89E+01 -4.73E+00 -9.03E+00  9.67E+00 -1.08E+01 -8.84E-01 -3.84E+01  9.94E+01 -5.92E+00  9.77E+01
         -1.63E+01 -2.60E+01  2.57E+02 -9.14E+01  3.15E+02 -8.07E+01  3.28E+02 -2.36E+02 -1.95E+02  1.10E+03
 
 OM28
+       -1.09E+01  6.66E+00 -2.95E+01  1.23E+01 -1.01E+01  5.14E+00  4.79E+00  3.77E+01 -1.08E+02 -5.53E+02 -6.81E+01 -3.20E+01
          1.00E+02  1.47E+02 -2.07E+02  4.85E+02 -4.97E+02 -3.23E+02 -2.08E+02  2.93E+02  5.01E+02 -6.49E+02  1.62E+03
 
 OM33
+       -3.21E+01 -2.03E+01  2.74E+01  8.72E-01  7.32E+00 -7.06E+00 -1.47E+01  1.98E+01  3.50E+00 -4.65E+01 -1.01E+02 -9.66E+00
          2.83E+01  2.80E+01 -2.13E+01  7.24E+01 -5.05E+00  8.16E+01 -2.10E+01 -1.55E+01 -1.25E+01  5.21E+00 -1.26E+01  7.06E+02
 
 OM34
+        9.83E+00  1.47E+01 -4.01E+01 -2.38E+01 -8.54E+00  2.52E+01  7.42E+00  7.47E+00  1.94E+01 -1.40E+01 -8.17E+01 -1.23E+02
         -3.69E+01  3.63E+01 -6.02E+01  2.88E+01 -1.11E+01 -9.08E+01  4.49E+01  2.74E+01 -2.37E+01 -9.78E+00  3.18E+01  6.38E+01
         1.10E+03
 
 OM35
+        1.91E+00  4.45E+00  3.32E+01  1.28E+01 -1.10E-01 -3.01E+01 -1.01E+01 -2.94E+01  5.03E+01  5.55E+01 -2.13E+02 -5.19E+01
         -1.49E+02  3.93E+01  1.78E+01  1.86E+01 -3.04E+01 -3.01E+02 -2.10E+01  8.37E+01  9.98E+01 -4.58E+01  9.75E+01 -5.96E+01
         9.33E+01  1.38E+03
 
 OM36
+        1.24E+01  2.23E+01 -1.93E+00  8.31E+00 -7.96E+00 -1.52E+01  1.55E+01 -1.75E+01  3.21E+00 -1.03E+01 -4.68E+01 -2.10E+01
         -4.83E+01 -7.06E+00  2.72E+01 -6.62E+00 -4.53E+01 -1.76E+02  1.70E+00  3.32E+01  1.20E+02 -6.51E+01  1.57E+02 -1.63E+02
        -1.06E+02  3.95E+02  1.02E+03
 
 OM37
+        1.43E+01  7.58E+00 -1.74E+01 -3.63E+00  1.11E+01  1.91E+01  1.48E+00  2.45E+01  3.91E+01  7.77E+00  6.77E+01 -7.58E+01
          8.16E+00 -1.63E+01 -1.39E+02 -2.98E+01  2.86E+01  4.59E+02  2.37E+01  5.48E+00 -5.29E+01 -1.31E+01 -1.15E+02 -3.84E+01
         3.75E+02 -2.47E+02 -1.69E+02  1.19E+03
 
 OM38
+        2.86E+01  7.76E+00  1.90E+01  1.37E+01 -2.34E+01  8.28E+00 -9.84E+00 -4.57E+01 -1.31E+00 -2.09E+00 -4.62E+02  3.95E+01
          1.55E+01  3.88E+01  5.83E+01 -1.58E+01 -5.06E+01 -6.59E+02 -5.10E+00  3.61E+01  1.45E+02 -3.25E+01  2.05E+02 -2.84E+02
        -2.96E+02  2.76E+02  4.40E+02 -5.57E+02  1.65E+03
 
 OM44
+       -7.19E+00 -6.39E+00 -1.44E+01 -2.22E+01 -5.19E+00  3.91E+00 -7.31E+00  1.31E+01  2.37E+01  1.15E+01 -1.67E+01 -5.72E+01
         -2.07E+01  2.46E+01 -2.20E+01  1.38E+01 -4.63E+00  9.41E+00 -6.75E+01 -8.35E+00  2.45E+01 -5.95E+01  6.44E+01  2.61E+01
        -5.83E+00  6.70E+01  1.79E+01 -5.01E+00 -2.00E+01  3.27E+02
 
 OM45
+       -1.19E+01 -4.54E+00  1.39E+01  6.29E+00 -7.26E+00 -1.72E+01 -6.51E+00 -2.79E+00 -6.07E+00  5.14E+01 -1.62E+01 -9.59E+01
         -9.20E+01  4.63E+00 -3.59E+00  5.85E+01  3.99E+00 -4.76E+01 -9.27E+01 -1.09E+02 -7.76E+00 -3.75E-01  3.21E+00 -2.01E+01
        -4.02E+01  6.66E+01  6.10E+01 -2.48E+01  3.79E+01  7.28E+01  8.42E+02
 
 OM46
+       -1.02E+01  5.57E+00  1.70E+01  1.40E+01  3.95E+00 -2.72E+01  8.13E+00 -2.56E+01  5.37E+00  4.82E+01 -2.89E+00 -7.05E+00
         -7.17E+01 -6.44E+01  3.33E+01 -1.58E+01  2.75E+01 -2.04E+01 -2.16E+01 -4.92E+01 -9.72E+01  5.38E+01 -9.79E+01 -1.26E+01
        -7.61E+01  4.28E+01  2.07E+01 -6.43E+01  8.02E+01 -4.57E+01  1.70E+02  6.64E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.52E+01  1.82E+00 -3.02E+00 -1.31E+01 -1.10E+01  1.86E+01 -9.35E+00 -5.28E-02  2.19E+01  2.15E+01  2.23E-01  2.23E+01
          1.50E+01 -6.95E+01 -3.77E+01 -2.98E+01 -1.79E+01  3.80E+00  1.47E+02 -1.31E+00 -2.62E+01 -4.61E+01  6.26E+00  1.71E+01
        -9.74E+01  2.48E+01 -3.72E+00 -3.20E+00  9.91E+00  1.96E+02 -8.50E+01 -1.28E+02  8.54E+02
 
 OM48
+       -2.22E-01 -2.19E+01  3.69E+01  1.78E+01  6.17E+00 -1.26E+01  7.21E+00  1.26E+01  2.79E+01  5.15E+01 -8.91E+00 -3.03E+02
         -2.37E+01 -1.42E+01 -7.69E+01 -3.55E+01  6.35E+01  2.79E+01 -2.77E+02 -4.97E+01 -3.93E+01 -4.25E+01 -1.18E+02 -6.95E+00
        -2.20E+02 -5.81E+01  9.66E+00 -3.98E+01  1.05E+02 -1.76E+02  6.60E+01  1.97E+02 -2.98E+02  1.01E+03
 
 OM55
+       -4.19E+00  4.41E-02 -3.33E+00  9.10E+00 -5.89E+00  1.43E+01  5.66E+00  3.81E+00 -1.12E+00  1.20E+01  6.65E+00 -1.94E+01
         -1.25E+02 -3.79E+01  2.20E+01 -1.66E+01  1.39E-01  8.75E+00 -7.16E+00 -1.64E+02 -5.05E+01  1.98E+01 -1.59E+01 -2.24E+01
        -3.72E+01 -3.31E+01  1.46E+00 -1.01E+01  2.74E+01  6.15E-01  5.42E+01  1.68E+01 -4.00E+00  2.91E+01  4.56E+02
 
 OM56
+       -8.22E+00 -2.40E+00  6.97E+00 -8.75E+00  8.06E-01  2.53E+01  9.97E+00  1.44E+01  5.87E+00  3.33E+01  1.97E+01  2.69E+01
         -9.02E+01 -1.47E+02  4.08E+01 -1.02E+02  5.94E+01  9.45E+01 -4.85E+01 -1.37E+02 -2.39E+02  2.01E+01 -1.46E+02  2.22E+00
         2.15E+00 -1.61E+02 -7.86E+01  5.45E+01 -1.07E+02 -8.15E+00 -8.99E+01  6.71E+01 -1.52E+01  8.08E+01  2.38E+02  8.87E+02
 
 OM57
+       -4.99E+00  1.66E+01  5.48E+00 -1.75E+00  1.16E+01 -1.50E+01 -5.90E+00  7.78E+00  3.89E+01  4.94E+00 -6.82E+01 -1.65E+01
          4.08E+01  5.96E+01 -6.21E+01  3.43E+01 -4.40E+01 -1.64E+01 -2.63E+01  3.61E+02  8.54E+01 -1.76E+02  1.16E+02  2.63E+00
         6.39E+00 -3.84E+01  4.06E+01  2.13E+00  2.87E+01  5.15E+01  2.53E+02  1.90E+01  4.49E+01 -2.43E+01 -1.57E+02 -1.98E+02
          1.02E+03
 
 OM58
+       -8.87E+00 -1.32E+01  5.63E+00  1.55E+01 -5.74E+00  2.70E+01  1.37E+01 -6.57E+00  4.02E+01  1.14E+02  1.14E+02  4.18E+00
         -4.13E+02 -1.64E+02  9.11E+01 -2.31E+02  1.04E+02  1.34E+02  1.94E-01 -4.85E+02 -2.28E+02  1.02E+02 -3.22E+02  5.11E+00
        -3.67E+01 -2.66E+02 -1.35E+02  7.78E+01 -1.32E+02 -4.43E+01 -1.81E+02  3.36E+01  7.03E+00  1.51E+02  1.85E+02  3.79E+02
         -4.06E+02  1.28E+03
 
 OM66
+        7.27E+00  4.78E+00 -5.45E+00 -5.71E+00 -7.69E+00  1.19E+00 -9.65E-01  1.11E+01 -4.40E+00  3.58E+00  2.72E+00  1.44E+01
         -7.16E+00 -3.35E+01  7.46E+00 -1.45E+01  7.29E+00  3.58E+01 -1.47E+01 -3.46E+01 -1.08E+02  3.81E+00 -4.90E+01 -1.07E+01
        -1.21E+01 -4.82E+01 -1.08E+02  2.06E+01 -6.01E+01 -7.33E+00 -4.33E+01 -3.67E+01  9.44E+00  7.45E+00  3.00E+01  1.76E+02
         -3.43E+01  7.44E+01  2.57E+02
 
 OM67
+       -7.14E+00  1.29E+01 -6.93E+00  1.91E+01 -1.25E+01 -1.51E+00  1.61E+01 -1.83E+01  3.54E+00 -1.03E+01 -4.81E+01 -5.01E+01
         -7.45E+00  4.59E+01  9.97E-01  7.19E+01 -6.07E+01 -2.38E+01 -1.24E+01  9.75E+01  2.68E+02 -1.08E+02  1.62E+02  3.58E+01
         3.47E+01  3.45E+01  3.13E+00 -3.87E+01  4.90E+01  3.16E+00  5.24E+01  1.75E+02 -5.81E+01  3.44E+01 -3.23E+01 -1.14E+02
          2.41E+02 -9.29E+01 -8.58E+01  7.20E+02
 
 OM68
+       -1.07E+01  6.82E+00  1.83E+01 -2.63E+01 -3.11E-01 -1.24E+01 -1.36E+01  8.91E+00  1.09E+01  4.90E+01  1.08E+02  2.81E+01
         -9.21E+01 -3.42E+02  5.66E+01 -1.49E+02  8.03E+01  1.33E+02  2.39E+01 -1.55E+02 -4.41E+02  1.14E+02 -3.15E+02  5.91E+01
         4.55E+01 -1.46E+02 -2.98E+02  9.61E+01 -2.63E+02 -7.45E+00 -7.74E+01 -1.42E+02  6.71E+01 -7.57E+01  3.33E+01  2.04E+02
         -1.24E+02  3.16E+02  1.88E+02 -3.36E+02  1.01E+03
 
 OM77
+       -9.47E+00 -9.29E+00  1.24E+01 -7.63E+00 -6.37E+00  8.46E+00  2.69E+00 -5.01E+00 -2.60E+01 -3.34E+00 -3.66E+01  3.91E+01
          2.41E+01 -2.60E+01  3.00E+01 -1.67E+01  3.68E+01 -6.82E+01  7.48E+01 -4.49E+01 -4.18E+01  2.53E+02 -1.08E+02 -1.43E+01
        -5.50E+01  2.65E+01  2.29E+01 -5.33E+01  5.58E+01  2.79E+01 -4.12E+01 -1.74E+01  2.28E+02 -9.49E+01 -5.61E+00  8.83E+00
         -1.20E+02  1.60E+01  8.97E+00 -1.24E+02  6.21E+01  4.17E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.20E+01  2.77E+01 -2.54E+01  1.00E+01 -8.95E+00 -1.46E+01  2.11E+01 -5.09E+00  1.14E+01 -1.18E+02 -2.02E+01 -1.38E+02
          5.50E+01  1.12E+02 -3.52E+02  2.35E+02 -1.35E+02 -1.45E+01 -1.94E+02  1.03E+02  1.70E+02 -5.06E+02  5.68E+02  3.09E+01
        -1.20E+01  1.06E+01  3.49E+01 -9.82E+01  1.69E+01 -2.53E+01  7.76E+00  2.36E+01 -2.71E+02  3.23E+02 -8.39E+00 -4.04E+01
          1.59E+02 -1.81E+02 -2.64E+01  2.72E+02 -2.64E+02 -3.67E+02  1.26E+03
 
 OM88
+       -3.03E+00 -3.52E+00 -1.35E+00 -1.09E+01  5.84E+00 -1.56E+01 -1.22E+01 -5.18E+00  7.52E+01  1.47E+02  8.09E+01  8.41E+01
         -6.11E+01 -1.26E+02  1.38E+02 -4.39E+02  1.13E+02  1.29E+02  8.43E+01 -7.74E+01 -1.87E+02  1.85E+02 -5.20E+02  6.26E+00
         5.60E+01 -2.26E+01 -7.59E+01  9.53E+01 -2.77E+02  5.15E+00 -3.26E+01  2.42E+00  6.52E+01 -1.96E+02 -2.05E+01  6.56E+01
         -5.37E+01  1.34E+02  1.94E+01 -1.25E+02  2.98E+02  8.55E+01 -4.88E+02  6.90E+02
 
 SG11
+       -6.55E+02  6.46E+02 -8.53E+02  7.93E+02  1.12E+02 -8.07E+02  2.08E+03 -1.99E+02 -3.67E+01 -6.64E+01  2.97E+03 -1.34E+03
         -2.46E+03 -2.34E+03  1.52E+03 -2.38E+03  2.78E+02  3.69E+03  6.42E+02  8.72E+02  1.83E+03  1.53E+03  6.50E+02  2.39E+03
         1.54E+02 -7.47E+02  2.16E+03 -7.36E+02 -3.49E+03 -5.88E+01  1.72E+03 -2.37E+02  2.70E+02  1.62E+03 -1.01E+03  2.69E+00
          1.50E+02  7.00E+02 -1.05E+03  1.24E+03 -4.46E+02  7.04E+01 -7.87E+02  1.21E+03  2.56E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.27E+02  5.68E+01  1.27E+02 -2.09E+02 -2.90E+02 -1.01E+02 -2.03E+02 -3.17E+02  8.01E+02  1.25E+03 -2.44E+02  2.28E+02
         -1.55E+03  4.15E+02  8.12E+02 -2.56E+02 -2.20E+02  4.57E+02 -1.80E+01 -8.53E+01  7.70E+02 -1.61E+03  1.14E+03  5.17E+02
         8.71E+02  6.55E+02 -9.44E+02 -3.04E+02 -1.08E+03  3.10E+02  2.08E+02  1.20E+03  2.82E+02 -6.89E+02 -4.03E+02 -6.13E+02
          7.93E+02  1.85E+01  4.26E+02  1.28E+03 -5.16E+02 -4.55E+02  6.82E+02 -3.02E+02  2.97E+04  0.00E+00  7.05E+05
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     4386.546
Stop Time: 
Wed 06/17/2015 
04:30 PM

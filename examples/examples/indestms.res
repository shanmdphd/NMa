Mon 09/30/2013 
07:09 PM
$PROB  THEOPHYLLINE   POPULATION DATA
$INPUT      ID DOSE=AMT TIME CP=DV WT
$DATA       THEOPP

$SUBROUTINES  ADVAN2

$PK
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
;SCALING PARAMETER=VOLUME/WT SINCE DOSE IS WEIGHT-ADJUSTED
   CALLFL=1
   KA=THETA(1)+ETA(1)
   K=THETA(2)+ETA(2)
   CL=THETA(3)+ETA(3)
   SC=CL/K

$THETA (0.0 FIXED)X3
$OMEGA (1.0E+06 FIXED)X3
$ETAS  3 .08 .04

$ERROR
   IPRED=F
   Y=F+EPS(1)

$SIGMA 0.2

$EST  METHOD=1 INTERACTION LAPLACE MAXEVAL=9999 PRINT=1 NOHABORT FNLETA=0 MCETA=1
$TABLE          ID DOSE TIME DV IPRED NOAPPEND NOPRINT FILE=INDESTMS.TAB
$TABLE          ID KA K CL NOAPPEND FIRSTONLY NOPRINT FILE=INDESTMS.PAR
$COV MATRIX=R
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  87) WITH "LAPLACIAN" AND "INTERACTION", "NUMERICAL" AND "SLOW"
 ARE ALSO REQUIRED ON $ESTIM RECORD, AND "SLOW" IS REQUIRED ON $COV
 RECORD. NM-TRAN HAS SUPPLIED THESE OPTIONS.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       30 SEP 2013
Days until program expires :6087
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3beta7.0(P)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 THEOPHYLLINE   POPULATION DATA
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      144
 NO. OF DATA ITEMS IN DATA SET:   7
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL IPRED
0FORMAT FOR DATA:
 (5E6.0,2F2.0)

 TOT. NO. OF OBS RECS:      132
 TOT. NO. OF INDIVIDUALS:     12
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.0000E+00     0.0000E+00     0.0000E+00
  0.0000E+00     0.0000E+00     0.0000E+00
  0.0000E+00     0.0000E+00     0.0000E+00
0INITIAL ESTIMATE OF OMEGA:
 0.1000E+07
 0.0000E+00   0.1000E+07
 0.0000E+00   0.0000E+00   0.1000E+07
0OMEGA CONSTRAINED TO BE THIS INITIAL ESTIMATE
0INITIAL ESTIMATE OF SIGMA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 SLOW GRADIENT METHOD USED:     YES
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 RELATIVE TOLERANCE (TOL):                  -1
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY (ATOL): -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 RESUME COV ANALYSIS (RESUME):               NO
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           2
 SEED NUMBER (SEED):    11456
 RANMETHOD:
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE:            EIGENVALUE
0-- TABLE   1 --
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE TIME CP IPRED
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(P)

 ONE COMPARTMENT MODEL WITH FIRST-ORDER ABSORPTION (ADVAN2)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   3
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1
   ABSORPTION RATE (KA) IS BASIC PK PARAMETER NO.:  3

0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         DEPOT        OFF        YES        YES        YES        NO
    2         CENTRAL      ON         NO         YES        NO         YES
    3         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            4           *           *           *           *
    3            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      6
   TIME DATA ITEM IS DATA ITEM NO.:          3
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   2

0PK SUBROUTINE CALLED ONCE PER INDIVIDUAL RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 
 
 #TBLN:      1
 #METH: Laplacian Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION WITH UNCONSTRAINED ETAS
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    YES 
 NUMERICAL 2ND DERIVATIVES:               YES 
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
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  1           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    100         
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   100         
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               OFF
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        OFF
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

LOADED    3 PHI/ETA ITEMS FROM CONTROL STREAM
 
0ITERATION NO.:    0    OBJECTIVE VALUE:   22.8833225853200        NO. OF FUNC. EVALS.:   2
 CUMULATIVE NO. OF FUNC. EVALS.:        2
 NPARAMETR:  2.0000E-01
 PARAMETER:  1.0000E-01
 GRADIENT:  -2.0661E+02
 
0ITERATION NO.:    1    OBJECTIVE VALUE:  -4.09444121724228        NO. OF FUNC. EVALS.:   3
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.6442E-01
 PARAMETER:  4.0000E-01
 GRADIENT:   5.8006E+00
 
0ITERATION NO.:    2    OBJECTIVE VALUE:  -4.12368444788626        NO. OF FUNC. EVALS.:   3
 CUMULATIVE NO. OF FUNC. EVALS.:        8
 NPARAMETR:  3.5850E-01
 PARAMETER:  3.9181E-01
 GRADIENT:   1.5330E+00
 
0ITERATION NO.:    3    OBJECTIVE VALUE:  -4.12561474787644        NO. OF FUNC. EVALS.:   3
 CUMULATIVE NO. OF FUNC. EVALS.:       11
 NPARAMETR:  3.5640E-01
 PARAMETER:  3.8886E-01
 GRADIENT:  -1.7159E-02
 
0ITERATION NO.:    4    OBJECTIVE VALUE:  -4.12561474787644        NO. OF FUNC. EVALS.:   2
 CUMULATIVE NO. OF FUNC. EVALS.:       13
 NPARAMETR:  3.5640E-01
 PARAMETER:  3.8886E-01
 GRADIENT:  -1.1987E-01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -4.12561474787644        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:       13
 NPARAMETR:  3.5640E-01
 PARAMETER:  3.8886E-01
 GRADIENT:  -1.1987E-01
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:       13
 NO. OF SIG. DIGITS IN FINAL EST.:  3.2

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         2.1899E+00  8.9247E-02  4.1568E-02
 SE:             6.3050E-01  4.0246E-03  2.7994E-03
 N:                      12          12          12
 
 P VAL.:         5.1422E-04  6.467E-109  7.3805E-50
 
 ETAshrink(%):   9.9772E+01  9.9999E+01  9.9999E+01
 EBVshrink(%):   5.7759E-05  1.0715E-08  1.0188E-09
 EPSshrink(%):   1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:     0.11
 Elapsed covariance time in seconds:     0.01
 SKIPPING EXECUTION OF SLOW FNLMOD/FNLETA BY USER REQUEST
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************       -4.126       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         0.00E+00  0.00E+00  0.00E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3   
 
 ETA1
+        1.00E+06
 
 ETA2
+        0.00E+00  1.00E+06
 
 ETA3
+        0.00E+00  0.00E+00  1.00E+06
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.56E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3   
 
 ETA1
+        1.00E+03
 
 ETA2
+        0.00E+00  1.00E+03
 
 ETA3
+        0.00E+00  0.00E+00  1.00E+03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        5.97E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
        ......... ......... .........
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3   
 
 ETA1
+       .........
 
 ETA2
+       ......... .........
 
 ETA3
+       ......... ......... .........
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        4.39E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3   
 
 ETA1
+       .........
 
 ETA2
+       ......... .........
 
 ETA3
+       ......... ......... .........
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        3.67E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+       .........
 
 TH 2
+       ......... .........
 
 TH 3
+       ......... ......... .........
 
 OM11
+       ......... ......... ......... .........
 
 OM12
+       ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       ......... ......... ......... ......... ......... ......... .........
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... ......... .........  1.92E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+       .........
 
 TH 2
+       ......... .........
 
 TH 3
+       ......... ......... .........
 
 OM11
+       ......... ......... ......... .........
 
 OM12
+       ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       ......... ......... ......... ......... ......... ......... .........
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... ......... .........  4.39E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+       .........
 
 TH 2
+       ......... .........
 
 TH 3
+       ......... ......... .........
 
 OM11
+       ......... ......... ......... .........
 
 OM12
+       ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       ......... ......... ......... ......... ......... ......... .........
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM33
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... ......... .........  5.20E+02
 
 #CPUT: Total CPU Time in Seconds,        0.187
Stop Time: 
Mon 09/30/2013 
07:09 PM

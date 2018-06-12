Fri 09/06/2013 
11:54 PM
; Control problem for sde system.  No SDE matters done.  From Chris Tornoe
$PROBLEM PK ODE HANDS ON ONE

$INPUT ID TIME DV AMT CMT FLAG

$DATA   sde6.csv
        IGNORE=@

$SUBROUTINE ADVAN6 TOL 10 DP

$MODEL 
       COMP = (CENTRAL);

$PK
  IF(NEWIND.NE.2) OT = 0
   
  TVCL  = THETA(1)
  CL    = TVCL*EXP(ETA(1))
  
  TVVD  = THETA(2)
  VD    = TVVD*EXP(ETA(2))


$DES
 DADT(1) = - CL/VD*A(1) ;+SGW1
 
$ERROR 
  
     IPRED = A(1)/VD
     IRES  = DV - IPRED
     W     = THETA(3)
     IWRES = IRES/W
     Y     = IPRED+W*EPS(1)

$THETA (0,10)               ;1 CL
$THETA (0,32)               ;2 VD
$THETA (0, 2)               ;4 SIGMA

$OMEGA 0.1                  ;1 CL
$OMEGA 0.01                 ;2 VD

$SIGMA 1 FIX                ; PK

$EST MAXEVAL=9999 METHOD=1 LAPLACE NUMERICAL SLOW INTER NOABORT SIGDIGITS=3 PRINT=1 MSFO=sde6.msf
$COV MATRIX=R

$TABLE ID TIME FLAG AMT CMT IPRED IRES IWRES
       ONEHEADER NOPRINT FILE=sde6.fit
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        6 SEP 2013
Days until program expires :6111
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3beta7.0(N)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 PK ODE HANDS ON ONE
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      570
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  8
0INDICES PASSED TO SUBROUTINE PRED:
   7   2   4   0   0   0   5   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME DV AMT CMT FLAG EVID MDV
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 IPRED IRES IWRES
0FORMAT FOR DATA:
 (6E10.0,2F2.0)

 TOT. NO. OF OBS RECS:      540
 TOT. NO. OF INDIVIDUALS:     30
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.0000E+00     0.1000E+02     0.1000E+07
  0.0000E+00     0.3200E+02     0.1000E+07
  0.0000E+00     0.2000E+01     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.1000E+00
 0.0000E+00   0.1000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+01
0SIGMA CONSTRAINED TO BE THIS INITIAL ESTIMATE
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
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE:            EIGENVALUE
0-- TABLE   1 --
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID TIME FLAG AMT CMT IPRED IRES IWRES
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(N)

 GENERAL NONLINEAR KINETICS MODEL (ADVAN6)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   2
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         YES        YES        YES        YES
    2         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE FROM SUBROUTINE TOL:  10
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      7
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    5

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
0ERROR SUBROUTINE INDICATES THAT DERIVATIVES OF COMPARTMENT AMOUNTS ARE USED.
0DES SUBROUTINE USES COMPACT STORAGE MODE.
1
 
 
 #TBLN:      1
 #METH: Laplacian Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    YES 
 NUMERICAL 2ND DERIVATIVES:               YES 
 NO. OF FUNCT. EVALS. ALLOWED:            9999
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  YES 
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    100         
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   100         
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   1535.20801632968        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:        6
 NPARAMETR:  1.0000E+01  3.2000E+01  2.0000E+00  1.0000E-01  1.0000E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:  -4.6978E+01 -1.4418E+00  7.6311E+01 -1.7746E+00 -2.9105E+02
 
0ITERATION NO.:    1    OBJECTIVE VALUE:   1399.88230899714        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       13
 NPARAMETR:  1.1017E+01  3.2095E+01  1.7089E+00  1.0073E-01  3.3201E-02
 PARAMETER:  1.9684E-01  1.0297E-01 -5.7314E-02  1.0366E-01  7.0000E-01
 GRADIENT:   1.6870E+01 -8.6154E+00 -6.5240E+01 -7.8207E-01 -1.2884E+02
 
0ITERATION NO.:    2    OBJECTIVE VALUE:   1398.51717332740        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       20
 NPARAMETR:  1.0430E+01  3.2690E+01  2.0295E+00  1.0095E-01  4.6938E-02
 PARAMETER:  1.4212E-01  1.2134E-01  1.1463E-01  1.0471E-01  8.7312E-01
 GRADIENT:  -1.4711E+01  1.3756E+01  2.3530E+02  2.4774E-01 -7.5932E+01
 
0ITERATION NO.:    3    OBJECTIVE VALUE:   1397.05273377130        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       28
 NPARAMETR:  9.7322E+00  3.2776E+01  2.0074E+00  1.0083E-01  4.8726E-02
 PARAMETER:  7.2856E-02  1.2397E-01  1.0369E-01  1.0415E-01  8.9181E-01
 GRADIENT:  -5.4085E+01  1.5529E+01  2.2081E+02 -4.5598E+00 -7.2947E+01
 
0ITERATION NO.:    4    OBJECTIVE VALUE:   1396.08958989020        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       35
 NPARAMETR:  9.5661E+00  3.1621E+01  1.9952E+00  1.3781E-01  5.0334E-02
 PARAMETER:  5.5641E-02  8.8076E-02  9.7611E-02  2.6037E-01  9.0805E-01
 GRADIENT:  -4.7241E+01 -2.4434E+01  2.1332E+02  1.0025E+01 -6.9917E+01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   1395.95378709856        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       42
 NPARAMETR:  9.4598E+00  3.3727E+01  1.9671E+00  1.9296E-01  5.2051E-02
 PARAMETER:  4.4470E-02  1.5255E-01  8.3413E-02  4.2866E-01  9.2482E-01
 GRADIENT:  -3.7037E+01  4.3834E+01  1.9321E+02  2.2658E+01 -6.8942E+01
 
0ITERATION NO.:    6    OBJECTIVE VALUE:   1366.37516125657        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       51
 NPARAMETR:  1.0286E+01  3.2646E+01  1.7786E+00  1.1251E-01  8.2979E-02
 PARAMETER:  1.2818E-01  1.1998E-01 -1.7342E-02  1.5895E-01  1.1580E+00
 GRADIENT:  -1.8596E+01  5.3545E+00  2.8517E+01  4.3486E+00 -2.6688E+01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:   1363.71408580927        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       58
 NPARAMETR:  1.0833E+01  3.2509E+01  1.7403E+00  9.2440E-02  1.0768E-01
 PARAMETER:  1.8006E-01  1.1579E-01 -3.9074E-02  6.0697E-02  1.2883E+00
 GRADIENT:   1.0376E+01  1.8043E+00 -1.2310E+01 -6.1812E+00 -8.3962E+00
 
0ITERATION NO.:    8    OBJECTIVE VALUE:   1363.21946747851        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       65
 NPARAMETR:  1.0624E+01  3.2504E+01  1.7387E+00  1.0540E-01  1.1891E-01
 PARAMETER:  1.6051E-01  1.1563E-01 -3.9987E-02  1.2630E-01  1.3379E+00
 GRADIENT:  -1.4531E+00  1.3101E+00 -1.3244E+01  1.3609E+00 -2.5210E+00
 
0ITERATION NO.:    9    OBJECTIVE VALUE:   1363.13718520572        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       72
 NPARAMETR:  1.0647E+01  3.2454E+01  1.7497E+00  1.0366E-01  1.2256E-01
 PARAMETER:  1.6268E-01  1.1410E-01 -3.3729E-02  1.1798E-01  1.3530E+00
 GRADIENT:  -2.5948E-01  5.2747E-01 -1.1327E+00  4.8996E-01 -7.4244E-01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   1363.13252778364        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       79
 NPARAMETR:  1.0654E+01  3.2432E+01  1.7508E+00  1.0283E-01  1.2411E-01
 PARAMETER:  1.6332E-01  1.1341E-01 -3.3082E-02  1.1393E-01  1.3593E+00
 GRADIENT:   1.0391E-01  1.9135E-01  1.1850E-01  4.9975E-02 -3.3750E-02
 
0ITERATION NO.:   11    OBJECTIVE VALUE:   1363.13246154249        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       86
 NPARAMETR:  1.0651E+01  3.2420E+01  1.7506E+00  1.0265E-01  1.2428E-01
 PARAMETER:  1.6307E-01  1.1302E-01 -3.3170E-02  1.1309E-01  1.3600E+00
 GRADIENT:  -4.0726E-02  1.0768E-02 -4.7063E-02 -4.7309E-02  4.6479E-02
 
0ITERATION NO.:   12    OBJECTIVE VALUE:   1363.13246066851        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       93
 NPARAMETR:  1.0652E+01  3.2421E+01  1.7507E+00  1.0272E-01  1.2420E-01
 PARAMETER:  1.6312E-01  1.1308E-01 -3.3147E-02  1.1344E-01  1.3596E+00
 GRADIENT:  -4.8270E-03  3.9359E-02  4.2941E-03 -6.1293E-03  7.4130E-03
 
0ITERATION NO.:   13    OBJECTIVE VALUE:   1363.13246066851        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      103
 NPARAMETR:  1.0652E+01  3.2421E+01  1.7507E+00  1.0272E-01  1.2420E-01
 PARAMETER:  1.6312E-01  1.1308E-01 -3.3147E-02  1.1344E-01  1.3596E+00
 GRADIENT:  -5.2243E-02  1.0214E-02 -9.7868E-02 -1.3546E-02 -6.8621E-02
 
0ITERATION NO.:   14    OBJECTIVE VALUE:   1363.13246066851        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      103
 NPARAMETR:  1.0652E+01  3.2421E+01  1.7507E+00  1.0272E-01  1.2420E-01
 PARAMETER:  1.6312E-01  1.1308E-01 -3.3147E-02  1.1344E-01  1.3596E+00
 GRADIENT:  -5.2243E-02  1.0214E-02 -9.7868E-02 -1.3546E-02 -6.8621E-02
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      103
 NO. OF SIG. DIGITS IN FINAL EST.:  3.1

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.2962E-04 -3.5148E-03
 SE:             5.7272E-02  6.3132E-02
 N:                      30          30
 
 P VAL.:         9.9680E-01  9.5560E-01
 
 ETAshrink(%):   4.5268E-01  2.0259E-01
 EBVshrink(%):   1.5687E+00  1.5075E+00
 EPSshrink(%):   5.4770E+00
 
 #TERE:
 Elapsed estimation time in seconds:    14.44
 Elapsed covariance time in seconds:     5.62
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1363.132       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.07E+01  3.24E+01  1.75E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        1.03E-01
 
 ETA2
+        0.00E+00  1.24E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        3.21E-01
 
 ETA2
+        0.00E+00  3.52E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.00E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         6.41E-01  2.10E+00  5.65E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        2.64E-02
 
 ETA2
+       .........  3.33E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+       .........
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        4.12E-02
 
 ETA2
+       .........  4.73E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+       .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM22      SG11  
 
 TH 1
+        4.10E-01
 
 TH 2
+        5.83E-02  4.40E+00
 
 TH 3
+        2.10E-04  2.92E-03  3.19E-03
 
 OM11
+        2.31E-03  5.80E-03 -3.89E-05  6.98E-04
 
 OM12
+       ......... ......... ......... ......... .........
 
 OM22
+       -1.76E-04  8.60E-04 -2.39E-05  1.05E-05 .........  1.11E-03
 
 SG11
+       ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.41E-01
 
 TH 2
+        4.34E-02  2.10E+00
 
 TH 3
+        5.80E-03  2.46E-02  5.65E-02
 
 OM11
+        1.37E-01  1.05E-01 -2.61E-02  2.64E-02
 
 OM12
+       ......... ......... ......... ......... .........
 
 OM22
+       -8.25E-03  1.23E-02 -1.27E-02  1.19E-02 .........  3.33E-02
 
 SG11
+       ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11      OM12      OM22      SG11  
 
 TH 1
+        2.49E+00
 
 TH 2
+       -2.22E-02  2.30E-01
 
 TH 3
+       -2.38E-01 -2.33E-01  3.14E+02
 
 OM11
+       -8.08E+00 -1.85E+00  2.01E+01  1.48E+03
 
 OM12
+       ......... ......... ......... ......... .........
 
 OM22
+        4.83E-01 -1.70E-01  6.72E+00 -1.33E+01 .........  9.02E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... .........
 
 #CPUT: Total CPU Time in Seconds,       17.469
Stop Time: 
Fri 09/06/2013 
11:55 PM

Fri 09/06/2013 
11:55 PM
; Based on sde5.ctl, with SDE equations put in, and .csv file modified.  From Chris Tornoe, and can work with NONMEM VI
$PROBLEM PK ODE HANDS ON ONE
$INPUT ID HOUR DV AMT CMT FLAG EVID MDV SDE TIME
$DATA   sde7.csv
        IGNORE=@
$SUBROUTINE ADVAN6 TOL 10 DP
$MODEL 
       COMP = (CENTRAL);
       COMP = (P1)

$THETA (0,10)               ;1 CL
$THETA (0,32)               ;2 VD
$THETA (0, 2)               ;4 SIGMA
$THETA (0,1) ; SGW1

$OMEGA 0.1                  ;1 CL
$OMEGA 0.01                 ;2 VD

$SIGMA 1 FIX                ; PK

$PK
  IF(NEWIND.NE.2) OT = 0
  TVCL  = THETA(1)
  CL    = TVCL*EXP(ETA(1))
  TVVD  = THETA(2)
  VD    = TVVD*EXP(ETA(2))
SGW1 = THETA(4)

IF(NEWIND.NE.2) THEN
  AHT1 = 0
  PHT1 = 0
ENDIF

IF(EVID.NE.3) THEN
  A1 = A(1)
  A2 = A(2)
ELSE
  A1 = A1
  A2 = A2
ENDIF

IF(EVID.EQ.0) OBS = DV

IF(EVID.GT.2.AND.SDE.EQ.2) THEN
  RVAR = A2*(1/VD)**2+ THETA(3)**2
  K1   = A2*(1/VD)/RVAR
  AHT1 = A1 + K1*(OBS -( A1/VD))
  PHT1 = A2 - K1*RVAR*K1
ENDIF

IF(EVID.GT.2.AND.SDE.EQ.3) THEN
  AHT1 = A1
  PHT1 = 0
ENDIF

IF(EVID.GT.2.AND.SDE.EQ.4) THEN
  AHT1 = 0
  PHT1 = A2
ENDIF

IF(A_0FLG.EQ.1) THEN
  A_0(1) = AHT1
  A_0(2) = PHT1
ENDIF

$DES
 DADT(1) = - CL/VD*A(1) ;+0
DADT(2) = (-CL/VD)*(A(2))+(-CL/VD)*(A(2))+SGW1*SGW1

$ERROR (OBS ONLY)
     IPRED = A(1)/VD
     IRES  = DV - IPRED
W=SQRT(A(2)*(1/VD)**2+ THETA(3)**2)
     IWRES = IRES/W
     Y     = IPRED+W*EPS(1)

$EST MAXEVAL=9999 METHOD=1 LAPLACE NUMERICAL SLOW INTER NOABORT SIGDIGITS=3 PRINT=1 MSFO=sde7.msf
$COV MATRIX=R
$TABLE ID TIME FLAG AMT CMT IPRED IRES IWRES EVID
       ONEHEADER NOPRINT FILE=sde7.fit
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  3) THERE MAY BE AN ERROR IN THE ABBREVIATED CODE. THE FOLLOWING
 ONE OR MORE RANDOM VARIABLES ARE DEFINED WITH "IF" STATEMENTS THAT DO NOT
 PROVIDE DEFINITIONS FOR BOTH THE "THEN" AND "ELSE" CASES. IF ALL
 CONDITIONS FAIL, THE VALUES OF THESE VARIABLES WILL BE ZERO.
  
   RVAR K1

  
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
 NO. OF DATA RECS IN DATA SET:     1650
 NO. OF DATA ITEMS IN DATA SET:  10
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  8
0INDICES PASSED TO SUBROUTINE PRED:
   7  10   4   0   0   0   5   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID HOUR DV AMT CMT FLAG EVID MDV SDE TIME
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 IPRED IRES IWRES
0FORMAT FOR DATA:
 (E3.0,E5.0,E9.0,E5.0,5E2.0,E5.0)

 TOT. NO. OF OBS RECS:      540
 TOT. NO. OF INDIVIDUALS:     30
0LENGTH OF THETA:   4
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
  0.0000E+00     0.1000E+01     0.1000E+07
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
 HEADERS:               ONE
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID TIME FLAG AMT CMT IPRED IRES IWRES EVID
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(N)

 GENERAL NONLINEAR KINETICS MODEL (ADVAN6)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   3
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         YES        YES        YES        YES
    2         P1           ON         YES        YES        NO         NO
    3         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE FROM SUBROUTINE TOL:  10
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            *           *           *           *           *
    3            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      7
   TIME DATA ITEM IS DATA ITEM NO.:         10
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    5

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0PK SUBROUTINE INDICATES THAT COMPARTMENT AMOUNTS ARE INITIALIZED.
0PK SUBROUTINE INDICATES THAT DERIVATIVES OF COMPARTMENT AMOUNTS ARE USED.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONLY WITH OBSERVATION EVENTS.
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   1535.08929957713        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  1.0000E+01  3.2000E+01  2.0000E+00  1.0000E+00  1.0000E-01  1.0000E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:  -4.6990E+01 -1.3914E+00  7.6893E+01 -2.3917E-01 -1.7696E+00 -2.9097E+02
 
0ITERATION NO.:    1    OBJECTIVE VALUE:   1399.75206943695        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       15
 NPARAMETR:  1.1017E+01  3.2092E+01  1.7068E+00  1.0005E+00  1.0073E-01  3.3201E-02
 PARAMETER:  1.9689E-01  1.0287E-01 -5.8556E-02  1.0049E-01  1.0365E-01  7.0000E-01
 GRADIENT:   1.6902E+01 -8.7822E+00 -6.6625E+01 -4.3026E-01 -8.0159E-01 -1.2891E+02
 
0ITERATION NO.:    2    OBJECTIVE VALUE:   1398.61088512079        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       23
 NPARAMETR:  1.0432E+01  3.2705E+01  2.0345E+00  1.0014E+00  1.0096E-01  4.7465E-02
 PARAMETER:  1.4226E-01  1.2180E-01  1.1711E-01  1.0136E-01  1.0477E-01  8.7870E-01
 GRADIENT:  -1.4609E+01  1.4095E+01  2.3957E+02 -1.5602E-01  2.7806E-01 -7.4633E+01
 
0ITERATION NO.:    3    OBJECTIVE VALUE:   1397.14266654403        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       32
 NPARAMETR:  9.7321E+00  3.2796E+01  2.0126E+00  1.0021E+00  1.0085E-01  4.9278E-02
 PARAMETER:  7.2848E-02  1.2457E-01  1.0626E-01  1.0211E-01  1.0424E-01  8.9745E-01
 GRADIENT:  -5.4050E+01  1.5975E+01  2.2527E+02 -1.8096E-01 -4.5265E+00 -7.1666E+01
 
0ITERATION NO.:    4    OBJECTIVE VALUE:   1396.17030170116        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       40
 NPARAMETR:  9.5647E+00  3.1618E+01  2.0003E+00  1.0179E+00  1.3788E-01  5.0893E-02
 PARAMETER:  5.5493E-02  8.8006E-02  1.0013E-01  1.1773E-01  2.6060E-01  9.1357E-01
 GRADIENT:  -4.7280E+01 -2.4305E+01  2.1778E+02 -1.7169E-01  1.0041E+01 -6.8669E+01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   1395.95772139998        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       48
 NPARAMETR:  9.4591E+00  3.3713E+01  1.9718E+00  1.0392E+00  1.9326E-01  5.2608E-02
 PARAMETER:  4.4390E-02  1.5214E-01  8.5802E-02  1.3847E-01  4.2944E-01  9.3014E-01
 GRADIENT:  -3.6988E+01  4.2927E+01  1.9762E+02 -1.9504E-01  2.2711E+01 -6.7695E+01
 
0ITERATION NO.:    6    OBJECTIVE VALUE:   1365.96854311155        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:       58
 NPARAMETR:  1.0282E+01  3.2643E+01  1.7783E+00  1.4354E+00  1.1258E-01  8.3376E-02
 PARAMETER:  1.2781E-01  1.1991E-01 -1.7514E-02  4.6147E-01  1.5926E-01  1.1604E+00
 GRADIENT:  -1.8767E+01  5.2913E+00  3.0113E+01 -6.8871E-01  4.3825E+00 -2.6314E+01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:   1349.32486423362        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       67
 NPARAMETR:  1.1442E+01  3.2353E+01  1.6903E+00  9.9729E+00  7.4562E-02  1.4203E-01
 PARAMETER:  2.3469E-01  1.1096E-01 -6.8251E-02  2.3999E+00 -4.6769E-02  1.4267E+00
 GRADIENT:   5.4264E+01 -1.9222E-01  2.3075E+01 -3.1841E+01 -2.2346E+01  7.3210E+00
 
0ITERATION NO.:    8    OBJECTIVE VALUE:   1308.90801675162        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       75
 NPARAMETR:  1.1369E+01  3.2351E+01  1.6811E+00  4.9586E+01  7.6793E-02  1.4397E-01
 PARAMETER:  2.2830E-01  1.1092E-01 -7.3726E-02  4.0037E+00 -3.2029E-02  1.4335E+00
 GRADIENT:   3.3448E+01 -4.0522E+00  3.4303E+02  8.8782E+01 -4.3315E-01  1.0439E+01
 
0ITERATION NO.:    9    OBJECTIVE VALUE:   1289.07956180408        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       83
 NPARAMETR:  1.0924E+01  3.2440E+01  1.6532E+00  3.6736E+01  9.3064E-02  1.4275E-01
 PARAMETER:  1.8839E-01  1.1365E-01 -9.0409E-02  3.7037E+00  6.4058E-02  1.4293E+00
 GRADIENT:   1.1176E+01 -4.1294E-01  2.9805E+02  7.3132E+00  3.1989E+00  8.8294E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   1240.16952983576        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       92
 NPARAMETR:  9.8206E+00  3.2921E+01  1.2667E+00  3.7647E+01  1.4561E-01  1.5509E-01
 PARAMETER:  8.1894E-02  1.2836E-01 -3.5674E-01  3.7282E+00  2.8786E-01  1.4707E+00
 GRADIENT:  -3.1370E+01  5.9481E+00  9.6025E+01 -4.8967E+01  1.7291E+01  1.1880E+01
 
0ITERATION NO.:   11    OBJECTIVE VALUE:   1215.92707830537        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      100
 NPARAMETR:  1.0298E+01  3.2871E+01  1.0476E+00  4.8694E+01  9.7748E-02  1.4877E-01
 PARAMETER:  1.2940E-01  1.2686E-01 -5.4664E-01  3.9856E+00  8.8611E-02  1.4499E+00
 GRADIENT:  -2.2122E+01  4.9663E+00  4.6271E+01  1.2654E+00  7.0899E+00  1.0891E+01
 
0ITERATION NO.:   12    OBJECTIVE VALUE:   1211.89964021530        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      108
 NPARAMETR:  1.1111E+01  3.2624E+01  9.3626E-01  5.1526E+01  6.6539E-02  1.3252E-01
 PARAMETER:  2.0533E-01  1.1932E-01 -6.5901E-01  4.0421E+00 -1.0369E-01  1.3921E+00
 GRADIENT:   1.8802E+01  3.2635E+00  3.7407E+00 -9.2797E+00 -4.8643E+00  5.7353E+00
 
0ITERATION NO.:   13    OBJECTIVE VALUE:   1211.24853317551        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      116
 NPARAMETR:  1.0793E+01  3.2571E+01  9.1358E-01  5.3703E+01  7.3882E-02  1.2542E-01
 PARAMETER:  1.7632E-01  1.1768E-01 -6.8353E-01  4.0835E+00 -5.1349E-02  1.3645E+00
 GRADIENT:  -1.4117E+00  1.9470E+00  6.5790E+00  1.0055E+01 -8.6517E-02  3.1282E+00
 
0ITERATION NO.:   14    OBJECTIVE VALUE:   1211.15929368086        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      124
 NPARAMETR:  1.0772E+01  3.2508E+01  9.1086E-01  5.2878E+01  7.4715E-02  1.2192E-01
 PARAMETER:  1.7436E-01  1.1576E-01 -6.8651E-01  4.0680E+00 -4.5747E-02  1.3504E+00
 GRADIENT:  -2.4118E+00  1.3989E+00 -4.1932E-01 -1.0729E+00 -3.4793E-02  1.5002E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:   1211.14017944413        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      132
 NPARAMETR:  1.0811E+01  3.2453E+01  9.0860E-01  5.3017E+01  7.4253E-02  1.1950E-01
 PARAMETER:  1.7797E-01  1.1406E-01 -6.8899E-01  4.0706E+00 -4.8848E-02  1.3404E+00
 GRADIENT:  -2.4695E-01  6.2470E-01 -6.0158E-01 -4.1727E-01 -1.4114E-01  4.3276E-01
 
0ITERATION NO.:   16    OBJECTIVE VALUE:   1211.13838130946        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      140
 NPARAMETR:  1.0816E+01  3.2425E+01  9.0913E-01  5.3029E+01  7.4430E-02  1.1865E-01
 PARAMETER:  1.7845E-01  1.1320E-01 -6.8842E-01  4.0708E+00 -4.7658E-02  1.3368E+00
 GRADIENT:   6.4135E-02  2.1853E-01 -2.2923E-01 -4.9349E-02 -4.8158E-02  4.0557E-02
 
0ITERATION NO.:   17    OBJECTIVE VALUE:   1211.13838130946        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      155
 NPARAMETR:  1.0816E+01  3.2425E+01  9.0913E-01  5.3029E+01  7.4430E-02  1.1865E-01
 PARAMETER:  1.7845E-01  1.1320E-01 -6.8842E-01  4.0708E+00 -4.7658E-02  1.3368E+00
 GRADIENT:   5.1984E-03  1.8158E-01 -3.9419E-01 -1.3843E+00 -5.2504E-02 -3.4437E-02
 
0ITERATION NO.:   18    OBJECTIVE VALUE:   1211.13682843674        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      169
 NPARAMETR:  1.0816E+01  3.2419E+01  9.0789E-01  5.3223E+01  7.4400E-02  1.1867E-01
 PARAMETER:  1.7847E-01  1.1301E-01 -6.8978E-01  4.0745E+00 -4.7858E-02  1.3369E+00
 GRADIENT:  -4.5844E-02  3.5701E-02  2.9977E-01  5.1069E-01  1.0884E-02  3.3174E-04
 
0ITERATION NO.:   19    OBJECTIVE VALUE:   1211.13682843674        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      169
 NPARAMETR:  1.0816E+01  3.2419E+01  9.0789E-01  5.3223E+01  7.4400E-02  1.1867E-01
 PARAMETER:  1.7847E-01  1.1301E-01 -6.8978E-01  4.0745E+00 -4.7858E-02  1.3369E+00
 GRADIENT:  -4.5844E-02  3.5701E-02  2.9977E-01  5.1069E-01  1.0884E-02  3.3174E-04
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      169
 NO. OF SIG. DIGITS IN FINAL EST.:  3.7

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.4672E-02  1.8002E-03
 SE:             4.4262E-02  6.1653E-02
 N:                      30          30
 
 P VAL.:         7.4028E-01  9.7671E-01
 
 ETAshrink(%):   9.5995E+00  2.9581E-01
 EBVshrink(%):   1.0810E+01  1.7079E+00
 EPSshrink(%):   4.8158E+00
 
 #TERE:
 Elapsed estimation time in seconds:    60.83
 Elapsed covariance time in seconds:    30.03
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1211.137       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.08E+01  3.24E+01  9.08E-01  5.32E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        7.44E-02
 
 ETA2
+        0.00E+00  1.19E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.73E-01
 
 ETA2
+        0.00E+00  3.44E-01
 


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


         TH 1      TH 2      TH 3      TH 4     
 
         6.03E-01  2.01E+00  7.86E-02  3.93E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        2.40E-02
 
 ETA2
+       .........  3.19E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+       .........
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        4.41E-02
 
 ETA2
+       .........  4.62E-02
 


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
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        3.64E-01
 
 TH 2
+       -4.43E-02  4.05E+00
 
 TH 3
+       -2.04E-03  2.64E-03  6.18E-03
 
 TH 4
+        1.08E-01  1.21E-02 -2.04E-01  1.54E+01
 
 OM11
+       -7.36E-04  8.49E-03  1.63E-04 -1.14E-02  5.78E-04
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.10E-04  3.32E-04  5.80E-05 -4.63E-03  9.66E-06 .........  1.02E-03
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.03E-01
 
 TH 2
+       -3.65E-02  2.01E+00
 
 TH 3
+       -4.31E-02  1.67E-02  7.86E-02
 
 TH 4
+        4.58E-02  1.53E-03 -6.60E-01  3.93E+00
 
 OM11
+       -5.07E-02  1.75E-01  8.61E-02 -1.21E-01  2.40E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -5.73E-03  5.18E-03  2.31E-02 -3.70E-02  1.26E-02 .........  3.19E-02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        2.76E+00
 
 TH 2
+        2.41E-02  2.55E-01
 
 TH 3
+        4.54E-01 -1.87E-01  2.87E+02
 
 TH 4
+       -1.13E-02 -5.65E-03  3.79E+00  1.16E-01
 
 OM11
+        2.81E+00 -3.77E+00 -2.95E+00  1.28E+00  1.81E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+        1.88E-01 -6.00E-02  1.02E+00  3.01E-01 -9.71E+00 .........  9.87E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 #CPUT: Total CPU Time in Seconds,       87.781
Stop Time: 
Fri 09/06/2013 
11:56 PM

Fri 09/06/2013 
11:56 PM
$PROBLEM PK ODE HANDS ON ONE
$INPUT ID TIME DV AMT CMT FLAG MDV EVID SDE QA=XVID1 QB=XVID2 QZ=XVID3
$DATA   sde8.csv
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
$EST MAXEVAL=9999 METHOD=1 LAPLACE NUMERICAL SLOW INTER NOABORT SIGDIGITS=3 PRINT=1 MSFO=sde8.msf
$COV MATRIX=R
$TABLE ID TIME FLAG AMT CMT IPRED IRES IWRES EVID
       ONEHEADER NOPRINT FILE=sde8.fit
  
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
 NO. OF DATA RECS IN DATA SET:      570
 NO. OF DATA ITEMS IN DATA SET:  12
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   8   2   4   0   0   0   5   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME DV AMT CMT FLAG MDV EVID SDE QA QB QZ
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 IPRED IRES IWRES
0FORMAT FOR DATA:
 (E3.0,E4.0,E9.0,E5.0,5E2.0,3E3.0)

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
 HEADER:                YES
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
   EVENT ID DATA ITEM IS DATA ITEM NO.:      8
   TIME DATA ITEM IS DATA ITEM NO.:          2
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
 
0ITERATION NO.:    1    OBJECTIVE VALUE:   1399.75206836353        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       15
 NPARAMETR:  1.1017E+01  3.2092E+01  1.7068E+00  1.0005E+00  1.0073E-01  3.3201E-02
 PARAMETER:  1.9689E-01  1.0287E-01 -5.8556E-02  1.0049E-01  1.0365E-01  7.0000E-01
 GRADIENT:   1.6904E+01 -8.7707E+00 -6.6611E+01 -4.1801E-01 -7.8478E-01 -1.2891E+02
 
0ITERATION NO.:    2    OBJECTIVE VALUE:   1398.61167703541        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       23
 NPARAMETR:  1.0431E+01  3.2704E+01  2.0345E+00  1.0013E+00  1.0095E-01  4.7464E-02
 PARAMETER:  1.4224E-01  1.2178E-01  1.1711E-01  1.0133E-01  1.0474E-01  8.7869E-01
 GRADIENT:  -1.4622E+01  1.4064E+01  2.3956E+02 -1.5800E-01  2.7810E-01 -7.4636E+01
 
0ITERATION NO.:    3    OBJECTIVE VALUE:   1397.14368987775        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       32
 NPARAMETR:  9.7321E+00  3.2795E+01  2.0126E+00  1.0021E+00  1.0084E-01  4.9276E-02
 PARAMETER:  7.2840E-02  1.2455E-01  1.0626E-01  1.0206E-01  1.0417E-01  8.9743E-01
 GRADIENT:  -5.4060E+01  1.5967E+01  2.2527E+02 -1.6648E-01 -4.5350E+00 -7.1668E+01
 
0ITERATION NO.:    4    OBJECTIVE VALUE:   1396.19324407692        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       40
 NPARAMETR:  9.5639E+00  3.1598E+01  2.0003E+00  1.0168E+00  1.3783E-01  5.0895E-02
 PARAMETER:  5.5406E-02  8.7349E-02  1.0016E-01  1.1668E-01  2.6042E-01  9.1359E-01
 GRADIENT:  -4.7328E+01 -2.5009E+01  2.1782E+02 -1.7769E-01  1.0012E+01 -6.8686E+01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   1395.91390786611        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       48
 NPARAMETR:  9.4572E+00  3.3655E+01  1.9720E+00  1.0368E+00  1.9332E-01  5.2615E-02
 PARAMETER:  4.4192E-02  1.5043E-01  8.5909E-02  1.3611E-01  4.2958E-01  9.3020E-01
 GRADIENT:  -3.7050E+01  4.1128E+01  1.9778E+02 -2.0376E-01  2.2708E+01 -6.7532E+01
 
0ITERATION NO.:    6    OBJECTIVE VALUE:   1365.97557851898        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:       58
 NPARAMETR:  1.0283E+01  3.2616E+01  1.7784E+00  1.4067E+00  1.1243E-01  8.3374E-02
 PARAMETER:  1.2791E-01  1.1906E-01 -1.7438E-02  4.4128E-01  1.5859E-01  1.1604E+00
 GRADIENT:  -1.8740E+01  4.7046E+00  3.0176E+01 -6.6278E-01  4.3252E+00 -2.6303E+01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:   1327.00635092133        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:       68
 NPARAMETR:  1.2731E+01  3.2080E+01  1.6080E+00  5.7705E+01  5.0030E-02  2.4146E-01
 PARAMETER:  3.4142E-01  1.0249E-01 -1.1817E-01  4.1554E+00 -2.4628E-01  1.6921E+00
 GRADIENT:   1.3134E+02 -4.8238E+00  3.2211E+02  1.2742E+02 -2.2276E+01  2.9562E+01
 
0ITERATION NO.:    8    OBJECTIVE VALUE:   1300.96515986531        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       76
 NPARAMETR:  1.2535E+01  3.2119E+01  1.6196E+00  4.0218E+01  5.3052E-02  2.2383E-01
 PARAMETER:  3.2597E-01  1.0371E-01 -1.1096E-01  3.7943E+00 -2.1695E-01  1.6541E+00
 GRADIENT:   1.3670E+02 -2.2479E+00  2.9621E+02  1.5391E+01 -3.5070E+01  2.6724E+01
 
0ITERATION NO.:    9    OBJECTIVE VALUE:   1272.02867341451        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       84
 NPARAMETR:  1.0694E+01  3.2427E+01  1.5370E+00  3.7752E+01  1.0597E-01  1.9683E-01
 PARAMETER:  1.6705E-01  1.1325E-01 -1.6333E-01  3.7310E+00  1.2901E-01  1.5899E+00
 GRADIENT:  -9.4333E-02 -5.0644E-01  2.4895E+02  1.6451E+00  8.6246E+00  2.1923E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   1232.91802308956        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       92
 NPARAMETR:  1.0170E+01  3.2743E+01  1.2287E+00  3.9125E+01  1.1378E-01  1.5649E-01
 PARAMETER:  1.1682E-01  1.2295E-01 -3.8716E-01  3.7668E+00  1.6456E-01  1.4752E+00
 GRADIENT:  -2.3772E+01  3.9456E+00  8.4654E+01 -4.5788E+01  1.0232E+01  1.2493E+01
 
0ITERATION NO.:   11    OBJECTIVE VALUE:   1214.54838488537        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      100
 NPARAMETR:  1.0106E+01  3.2779E+01  1.0115E+00  5.0992E+01  9.4275E-02  1.2594E-01
 PARAMETER:  1.1056E-01  1.2405E-01 -5.8169E-01  4.0317E+00  7.0522E-02  1.3666E+00
 GRADIENT:  -3.3271E+01  4.0698E+00  4.0902E+01  1.4987E+01  5.2156E+00  2.9130E+00
 
0ITERATION NO.:   12    OBJECTIVE VALUE:   1211.70243504153        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      108
 NPARAMETR:  1.0866E+01  3.2557E+01  8.7280E-01  5.2987E+01  8.0007E-02  1.0993E-01
 PARAMETER:  1.8304E-01  1.1725E-01 -7.2920E-01  4.0701E+00 -1.1525E-02  1.2986E+00
 GRADIENT:   3.1215E+00  3.0019E+00 -1.9513E+01 -1.5839E+01  2.5915E+00 -4.4004E+00
 
0ITERATION NO.:   13    OBJECTIVE VALUE:   1211.19000625037        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      116
 NPARAMETR:  1.0792E+01  3.2492E+01  8.9779E-01  5.3740E+01  7.7916E-02  1.1642E-01
 PARAMETER:  1.7618E-01  1.1527E-01 -7.0097E-01  4.0842E+00 -2.4770E-02  1.3273E+00
 GRADIENT:  -1.2163E+00  1.1359E+00 -1.2465E+00  4.5574E+00  1.8417E+00 -9.3723E-01
 
0ITERATION NO.:   14    OBJECTIVE VALUE:   1211.14658126151        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      124
 NPARAMETR:  1.0786E+01  3.2444E+01  9.1080E-01  5.2953E+01  7.6224E-02  1.1917E-01
 PARAMETER:  1.7563E-01  1.1378E-01 -6.8658E-01  4.0694E+00 -3.5746E-02  1.3390E+00
 GRADIENT:  -1.4759E+00  4.7691E-01  1.0844E-01 -2.3945E-02  7.4758E-01  2.6471E-01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:   1211.13927374052        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      132
 NPARAMETR:  1.0813E+01  3.2417E+01  9.0981E-01  5.2997E+01  7.4943E-02  1.1902E-01
 PARAMETER:  1.7815E-01  1.1294E-01 -6.8767E-01  4.0702E+00 -4.4218E-02  1.3383E+00
 GRADIENT:  -6.2911E-02  9.2483E-02 -9.5107E-02 -7.9855E-02  1.9153E-01  2.0326E-01
 
0ITERATION NO.:   16    OBJECTIVE VALUE:   1211.13859422959        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      140
 NPARAMETR:  1.0818E+01  3.2402E+01  9.0958E-01  5.3025E+01  7.4179E-02  1.1847E-01
 PARAMETER:  1.7866E-01  1.1249E-01 -6.8792E-01  4.0708E+00 -4.9343E-02  1.3360E+00
 GRADIENT:   1.7059E-01 -1.3776E-01 -1.3019E-02  5.1545E-02 -1.5692E-01 -4.3839E-02
 
0ITERATION NO.:   17    OBJECTIVE VALUE:   1211.13854322819        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      148
 NPARAMETR:  1.0817E+01  3.2406E+01  9.0967E-01  5.3018E+01  7.4346E-02  1.1854E-01
 PARAMETER:  1.7850E-01  1.1260E-01 -6.8782E-01  4.0706E+00 -4.8218E-02  1.3363E+00
 GRADIENT:   8.7508E-02 -8.2136E-02 -1.8168E-02  2.5654E-02 -9.0511E-02 -1.2759E-02
 
0ITERATION NO.:   18    OBJECTIVE VALUE:   1211.13854322819        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      163
 NPARAMETR:  1.0817E+01  3.2406E+01  9.0967E-01  5.3018E+01  7.4346E-02  1.1854E-01
 PARAMETER:  1.7850E-01  1.1260E-01 -6.8782E-01  4.0706E+00 -4.8218E-02  1.3363E+00
 GRADIENT:   2.6066E-02 -1.1669E-01 -1.8187E-01 -1.3083E+00 -9.2009E-02 -8.6696E-02
 
0ITERATION NO.:   19    OBJECTIVE VALUE:   1211.13666154070        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      179
 NPARAMETR:  1.0818E+01  3.2413E+01  9.0804E-01  5.3178E+01  7.4465E-02  1.1854E-01
 PARAMETER:  1.7859E-01  1.1283E-01 -6.8962E-01  4.0736E+00 -4.7418E-02  1.3363E+00
 GRADIENT:   4.6097E-02 -3.5891E-02  6.7433E-02  1.4498E-02  2.3742E-02 -6.7158E-02
 
0ITERATION NO.:   20    OBJECTIVE VALUE:   1211.13662786051        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      193
 NPARAMETR:  1.0817E+01  3.2415E+01  9.0781E-01  5.3186E+01  7.4410E-02  1.1871E-01
 PARAMETER:  1.7854E-01  1.1289E-01 -6.8987E-01  4.0738E+00 -4.7790E-02  1.3370E+00
 GRADIENT:   9.4138E-03 -7.1555E-03  3.0321E-03  2.2201E-02  2.1204E-04  1.3684E-02
 
0ITERATION NO.:   21    OBJECTIVE VALUE:   1211.13662786051        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      193
 NPARAMETR:  1.0817E+01  3.2415E+01  9.0781E-01  5.3186E+01  7.4410E-02  1.1871E-01
 PARAMETER:  1.7854E-01  1.1289E-01 -6.8987E-01  4.0738E+00 -4.7790E-02  1.3370E+00
 GRADIENT:   9.4138E-03 -7.1555E-03  3.0321E-03  2.2201E-02  2.1204E-04  1.3684E-02
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      193
 NO. OF SIG. DIGITS IN FINAL EST.:  3.6

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.4594E-02  1.8798E-03
 SE:             4.4276E-02  6.1658E-02
 N:                      30          30
 
 P VAL.:         7.4169E-01  9.7568E-01
 
 ETAshrink(%):   9.5788E+00  3.0543E-01
 EBVshrink(%):   1.0797E+01  1.7059E+00
 EPSshrink(%):   4.7786E+00
 
 #TERE:
 Elapsed estimation time in seconds:    67.57
 Elapsed covariance time in seconds:    28.24
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
+        0.00E+00  3.45E-01
 


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
 
         6.02E-01  2.03E+00  7.88E-02  3.93E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        2.21E-02
 
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
+        4.06E-02
 
 ETA2
+       .........  4.63E-02
 


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
+        3.62E-01
 
 TH 2
+       -5.25E-02  4.13E+00
 
 TH 3
+       -1.75E-03 -9.73E-04  6.20E-03
 
 TH 4
+        8.95E-02  2.67E-01 -2.05E-01  1.55E+01
 
 OM11
+        1.95E-04 -4.94E-03  1.75E-04 -1.15E-02  4.90E-04
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -7.51E-05  8.29E-04  5.27E-05 -4.21E-03 -1.26E-05 .........  1.02E-03
 
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
+        6.02E-01
 
 TH 2
+       -4.29E-02  2.03E+00
 
 TH 3
+       -3.70E-02 -6.08E-03  7.88E-02
 
 TH 4
+        3.78E-02  3.34E-02 -6.62E-01  3.93E+00
 
 OM11
+        1.46E-02 -1.10E-01  1.00E-01 -1.32E-01  2.21E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -3.91E-03  1.28E-02  2.10E-02 -3.36E-02 -1.78E-02 .........  3.19E-02
 
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
+        2.77E+00
 
 TH 2
+        3.47E-02  2.46E-01
 
 TH 3
+        4.32E-01 -1.91E-01  2.87E+02
 
 TH 4
+       -1.17E-02 -5.23E-03  3.80E+00  1.16E-01
 
 OM11
+       -1.18E+00  2.41E+00 -1.51E+01  1.34E+00  2.10E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+        9.06E-02 -1.80E-01  8.47E-01  3.04E-01  3.02E+01 .........  9.85E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1THERE ARE ERROR MESSAGES IN FILE PRDERR                                                                  
 #CPUT: Total CPU Time in Seconds,       91.656
Stop Time: 
Fri 09/06/2013 
11:58 PM

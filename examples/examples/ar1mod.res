Mon 09/30/2013 
02:13 PM
;Model Desc:  ar1est_01mod2.ctl
;Project Name: testprob.08.08.08
;Project ID: None

$PROB  ctlar1mod same as ar1est_01mod2.ctl uses new features
$ABBR declare T(NO)
$ABBR DECLARE DOWHILE J
$ABBR declare integer i
$INPUT CX ID DOSE=AMT TIME CP=DV WT MDV
$DATA       ar1sim01mod.dat IGNORE=@

$SUBROUTINES  ADVAN2

$PK
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
;SCALING PARAMETER=VOLUME/WT SINCE DOSE IS WEIGHT-ADJUSTED
   CALLFL=1
   KA=THETA(1)*DEXP(ETA(1))
   K=THETA(2)*DEXP(ETA(2))
   CL=THETA(3)*WT*DEXP(ETA(3))
   SC=CL/K/WT

$ERROR
 IF(NEWIND.NE.2) I=0
 IF(MDV.EQ.0)THEN
    I=I+1
    T(I)=TIME
    J=1
    DO WHILE (J<=I)
    CORRL2(J,1)=EXP(-THETA(4)*(TIME-T(J)))
    J=J+1
    ENDDO
 ENDIF
   Y=F+F*EPS(1)

$THETA  
(.1,3,5)      ;[KA]
(.008,.08,.5) ;[KE]
(.004,.04,.9) ;[CL]
(0,.5,1)      ;[Ar_parameter]


$OMEGA
 .09      ;[P]
$OMEGA BLOCK(2)
 .09      ;[P]
 .04      ;[F]
 .09      ;[P]

$SIGMA  
 .2       ;[P]

$EST  
  NSIG=3 
  METHOD=1
  INTERACTION
  MAXEVAL=9999
  PRINT=5
  NOTHETABOUNDTEST
  NOOMEGABOUNDTEST
  NOSIGMABOUNDTEST 
  MSFO=ar1est_01mod2.msf

$COV MATRIX=S PRINT=E

$TABLE ID DOSE TIME CP WT 
  NOPRINT
  ONEHEADER
  FILE=tabar1mod

  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  43) THE $PK BLOCK REQUESTS "CALL ONCE PER INDIVIDUAL RECORD", BUT
 DATA ITEMS ARE USED IN THE $PK BLOCK. VALUES OF THESE DATA ITEMS
 SUBSEQUENT TO THOSE FROM THE FIRST EVENT RECORD WILL BE IGNORED.  IF THIS
 IS NOT APPROPRIATE, THE CALL DATA ITEM CAN BE USED TO OBTAIN ADDITIONAL
 CALLS, OR $PK'S CALLING PROTOCOL SHOULD BE CHANGED.
  
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
 ctlar1mod same as ar1est_01mod2.ctl uses new features
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      660
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   2
 DEP VARIABLE IS DATA ITEM NO.:   5
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   8   4   3   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 CX ID DOSE TIME CP WT MDV EVID
0FORMAT FOR DATA:
 (7E10.0,1F2.0)

 TOT. NO. OF OBS RECS:      600
 TOT. NO. OF INDIVIDUALS:     60
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:   YES
0OMEGA HAS BLOCK FORM:
  1
  0  2
  0  2  2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:   YES
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:   YES
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E+00     0.3000E+01     0.5000E+01
  0.8000E-02     0.8000E-01     0.5000E+00
  0.4000E-02     0.4000E-01     0.9000E+00
  0.0000E+00     0.5000E+00     0.1000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.9000E-01
        2                                                                                   NO
                  0.9000E-01
                  0.4000E-01   0.9000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:           NO
 S MATRIX SUBSTITUTED:          YES
 EIGENVLS. PRINTED:             YES
 SPECIAL COMPUTATION:            NO
 COMPRESSED FORMAT:              NO
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
 ID DOSE TIME CP WT
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
   EVENT ID DATA ITEM IS DATA ITEM NO.:      8
   TIME DATA ITEM IS DATA ITEM NO.:          4
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   3

0PK SUBROUTINE CALLED ONCE PER INDIVIDUAL RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 
 
 #TBLN:      1
 #METH: First Order Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            9999
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  YES 
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   862.605393966638        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  3.0000E+00  8.0000E-02  4.0000E-02  5.0000E-01  9.0000E-02  9.0000E-02  4.0000E-02  9.0000E-02  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   1.1468E+01  2.0397E+02 -3.6599E+02  1.3537E+02  1.7345E+01  4.7415E+00  1.2057E+02 -1.1376E+02  8.4439E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -93.1446715897482        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       49
 NPARAMETR:  2.8121E+00  7.5247E-02  5.1924E-02  3.5963E-01  8.3928E-02  9.0300E-02 -3.9183E-02  2.3586E-01  6.6855E-03
 PARAMETER: -5.6755E-02  2.0445E-02  4.0006E-01 -4.7697E-01  6.5075E-02  1.0166E-01 -9.7794E-02  6.5433E-01 -1.5992E+00
 GRADIENT:  -1.1045E+01 -5.4667E+01  8.7669E+01 -2.1637E+01 -4.4715E+00 -2.0881E+01 -1.3034E+02  1.9984E+01 -2.4909E+02
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -146.419401032968        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       91
 NPARAMETR:  3.0114E+00  7.6532E-02  4.3775E-02  2.9449E-01  8.0255E-02  1.0842E-01  3.8480E-02  1.2698E-01  8.8586E-03
 PARAMETER:  1.0967E-01  4.2409E-02  2.0412E-01 -7.7370E-01  4.2700E-02  1.9311E-01  8.7647E-02  3.2525E-01 -1.4585E+00
 GRADIENT:   2.2408E+01 -6.1216E+01  4.3805E+01 -1.9823E+00 -7.3326E+00  1.1231E+01  1.3485E+01  8.4802E+00 -9.5213E+01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -173.301445863789        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:      132
 NPARAMETR:  2.9327E+00  8.1742E-02  4.4968E-02  4.2728E-02  7.9688E-02  7.4421E-02  2.8239E-02  7.7487E-02  4.9211E-02
 PARAMETER:  4.3404E-02  1.2807E-01  2.3507E-01 -3.0092E+00  3.9156E-02  4.9623E-03  7.7637E-02  6.0762E-02 -6.0110E-01
 GRADIENT:   5.7131E+00 -8.3138E+00  2.6914E+00 -2.5050E+01 -2.5978E+00 -7.0836E+00  3.1079E+00  2.8066E-01 -5.4413E+01
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -174.892569942187        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      179
 NPARAMETR:  2.9066E+00  8.1885E-02  4.5959E-02  4.0623E-02  8.1653E-02  7.9968E-02  2.9568E-02  7.5115E-02  5.7301E-02
 PARAMETER:  2.1609E-02  1.3034E-01  2.6012E-01 -3.0620E+00  5.1337E-02  4.0907E-02  7.8419E-02  4.0994E-02 -5.2500E-01
 GRADIENT:   3.1337E-02 -8.6231E-01  1.8928E-02 -8.7853E-01  6.3837E-02 -3.7716E-01 -1.7545E-01 -2.2244E-01 -1.3546E+00
 
0ITERATION NO.:   25    OBJECTIVE VALUE:  -174.896332674156        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      241
 NPARAMETR:  2.9064E+00  8.1944E-02  4.5951E-02  4.1210E-02  8.1580E-02  8.0346E-02  2.9802E-02  7.5768E-02  5.6600E-02
 PARAMETER:  2.1438E-02  1.3129E-01  2.5992E-01 -3.0470E+00  5.0889E-02  4.3263E-02  7.8854E-02  4.5117E-02 -5.3116E-01
 GRADIENT:  -3.9325E-03  1.1057E-02 -1.0500E-03  2.2538E-03 -1.0741E-03  5.9156E-04 -7.9037E-03 -3.4871E-03  6.3172E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      241
 NO. OF SIG. DIGITS IN FINAL EST.:  3.4

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.6803E-03  1.1775E-02  2.5705E-03
 SE:             3.4227E-02  3.2968E-02  3.1097E-02
 N:                      60          60          60
 
 P VAL.:         9.6085E-01  7.2096E-01  9.3412E-01
 
 ETAshrink(%):   6.3933E+00  9.1465E+00  1.1754E+01
 EBVshrink(%):   6.6256E+00  9.2929E+00  1.2478E+01
 EPSshrink(%):   1.0467E+01
 
 #TERE:
 Elapsed estimation time in seconds:    51.72
 Elapsed covariance time in seconds:     7.88
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     -174.896       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.91E+00  8.19E-02  4.60E-02  4.12E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3   
 
 ETA1
+        8.16E-02
 
 ETA2
+        0.00E+00  8.03E-02
 
 ETA3
+        0.00E+00  2.98E-02  7.58E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        5.66E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3   
 
 ETA1
+        2.86E-01
 
 ETA2
+        0.00E+00  2.83E-01
 
 ETA3
+        0.00E+00  3.82E-01  2.75E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        2.38E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.21E-01  3.50E-03  2.42E-03  1.66E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3   
 
 ETA1
+        1.79E-02
 
 ETA2
+       .........  1.92E-02
 
 ETA3
+       .........  1.90E-02  2.34E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        2.25E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3   
 
 ETA1
+        3.13E-02
 
 ETA2
+       .........  3.38E-02
 
 ETA3
+       .........  1.86E-01  4.25E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        4.73E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.47E-02
 
 TH 2
+       -1.15E-05  1.22E-05
 
 TH 3
+        2.37E-05  2.95E-06  5.84E-06
 
 TH 4
+       -2.57E-05 -1.39E-05 -2.32E-05  2.74E-04
 
 OM11
+       -3.89E-04  5.11E-06  6.68E-06 -5.42E-05  3.20E-04
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.06E-04 -7.45E-06 -3.58E-06  7.70E-05  3.96E-06 ......... .........  3.68E-04
 
 OM23
+       -3.79E-04 -5.69E-06 -6.98E-06  1.07E-04 -3.39E-05 ......... .........  2.22E-04  3.60E-04
 
 OM33
+       -4.69E-04 -1.46E-05 -2.09E-05  2.26E-04 -6.83E-05 ......... .........  2.05E-04  2.95E-04  5.47E-04
 
 SG11
+        3.24E-05  1.39E-05  3.35E-05 -3.60E-04  7.09E-05 ......... ......... -6.33E-05 -1.11E-04 -2.64E-04  5.06E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        1.21E-01
 
 TH 2
+       -2.72E-02  3.50E-03
 
 TH 3
+        8.09E-02  3.49E-01  2.42E-03
 
 TH 4
+       -1.28E-02 -2.40E-01 -5.80E-01  1.66E-02
 
 OM11
+       -1.79E-01  8.16E-02  1.54E-01 -1.83E-01  1.79E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.75E-01 -1.11E-01 -7.72E-02  2.42E-01  1.15E-02 ......... .........  1.92E-02
 
 OM23
+       -1.65E-01 -8.57E-02 -1.52E-01  3.42E-01 -9.98E-02 ......... .........  6.11E-01  1.90E-02
 
 OM33
+       -1.65E-01 -1.78E-01 -3.70E-01  5.83E-01 -1.63E-01 ......... .........  4.57E-01  6.65E-01  2.34E-02
 
 SG11
+        1.19E-02  1.77E-01  6.16E-01 -9.66E-01  1.76E-01 ......... ......... -1.47E-01 -2.61E-01 -5.02E-01  2.25E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM13      OM22      OM23      OM33      SG11  
 
 TH 1
+        7.59E+01
 
 TH 2
+        1.46E+02  1.04E+05
 
 TH 3
+       -5.00E+02 -6.37E+04  3.26E+05
 
 TH 4
+       -2.00E+02  2.68E+04 -3.11E+04  8.02E+04
 
 OM11
+        1.02E+02 -2.90E+02 -2.03E+03  2.21E+02  3.43E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        5.37E+01  4.58E+02 -4.24E+02 -4.09E+03 -3.35E+02 ......... .........  4.69E+03
 
 OM23
+        2.36E+01 -1.08E+03 -2.48E+03 -1.89E+02  2.63E+02 ......... ......... -2.42E+03  6.43E+03
 
 OM33
+        7.23E+01 -5.93E+02  5.39E+03 -5.60E+03  3.21E+02 ......... ......... -1.54E+02 -2.68E+03  4.72E+03
 
 SG11
+       -8.26E+01  2.00E+04 -3.94E+04  5.49E+04 -5.33E+00 ......... ......... -2.88E+03 -2.68E+02 -2.52E+03  4.14E+04
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8         9
 
         2.36E-02  2.65E-01  4.26E-01  4.49E-01  7.77E-01  9.41E-01  1.09E+00  1.60E+00  3.42E+00
 
 #CPUT: Total CPU Time in Seconds,       60.154
Stop Time: 
Mon 09/30/2013 
02:14 PM

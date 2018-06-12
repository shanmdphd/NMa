Mon 09/30/2013 
06:27 PM
$PROB  THEOPHYLLINE POPULATION DATA; Analysis of Individuals
; Modification of CONTROL5 control steam
$INPUT      ID DOSE=AMT TIME CP=DV WT
$DATA       THEOPP RECS=ID
;RECS=ID:  Data set will be read until ID changes or end-of-file

$SUBROUTINES  ADVAN2

$PK
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
;SCALING PARAMETER=VOLUME/WT SINCE DOSE IS WEIGHT-ADJUSTED
   CALLFL=1
   KA=THETA(1)
   K=THETA(2)
   CL=THETA(3)
   SC=CL/K

$THETA  (0.001,3) (0.001,.2) (0.001,.1)
$OMEGA .2
;For single subject data OMEGA is residual variance.

$ERROR
   Y=F+ERR(1)
;ERR must be used instead of EPS.

$EST MAXEVAL=450  PRINT=5

$COV SPECIAL MATRIX=R PRINT=E
;SPECIAL is required to obtain the variance-covariance matrix for single-subject data.

$TABLE ID DOSE WT TIME NOPRINT ONEHEADER FILE=indestb.tab NOTITLE

$TABLE ID KA K CL SC NOPRINT FIRSTONLY NOAPPEND FILE=indestb.par NOTITLE ONEHEADER

INCLUDE indestb.txt 11
; INCLUDE: Inserts copies of the file named indestb.txt for each additional individual.
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    2
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    3
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    4
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    5
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    6
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    7
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    8
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    9
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM   10
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM   11
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM   12
             
 (WARNING  1) NM-TRAN INFERS THAT THE DATA ARE SINGLE-SUBJECT.
  NONMEM RUN CANNOT BE PARALLELIZED.
  
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
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                YES, COLUMN LABELS, NO TITLE
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES, COLUMN LABELS, NO TITLE
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      1
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   869.746458449089        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   5.7215E+01 -2.4643E+02  1.0252E+03 -1.7527E+03
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   12.7798450136520        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       36
 NPARAMETR:  1.5020E+00  4.6596E-02  1.5750E-02  9.3937E-01
 PARAMETER: -5.9215E-01 -1.3735E+00 -1.8039E+00  8.7345E-01
 GRADIENT:  -1.7838E+00  9.4367E+01 -1.2718E+02 -4.9121E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   1.67990338152065        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       67
 NPARAMETR:  1.8105E+00  5.6280E-02  2.0889E-02  5.8068E-01
 PARAMETER: -4.0523E-01 -1.1809E+00 -1.5050E+00  6.3294E-01
 GRADIENT:   1.5560E+00 -1.1821E+01  2.2679E+01  6.6917E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  0.649075654801295        NO. OF FUNC. EVALS.:  12
 CUMULATIVE NO. OF FUNC. EVALS.:      104
 NPARAMETR:  1.7815E+00  5.3248E-02  1.9680E-02  3.9389E-01
 PARAMETER: -4.2136E-01 -1.2373E+00 -1.5676E+00  4.3887E-01
 GRADIENT:   1.0585E-01  3.2866E+00 -5.9181E+00  2.0466E-01
 
0ITERATION NO.:   19    OBJECTIVE VALUE:  0.632068622579797        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      134
 NPARAMETR:  1.7774E+00  5.3956E-02  1.9923E-02  3.8955E-01
 PARAMETER: -4.2369E-01 -1.2239E+00 -1.5547E+00  4.3334E-01
 GRADIENT:   4.5476E-03  3.3764E-02 -4.1216E-02 -4.9846E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      134
 NO. OF SIG. DIGITS IN FINAL EST.:  3.6
 #TERE:
 Elapsed estimation time in seconds:     0.04
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************        0.632       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.78E+00  5.40E-02  1.99E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        3.90E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        6.24E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         2.30E-01  7.78E-03  2.17E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        1.66E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.33E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        5.29E-02
 
 TH 2
+       -9.28E-04  6.06E-05
 
 TH 3
+       -1.99E-04  1.63E-05  4.71E-06
 
 OM11
+       -8.08E-07  2.95E-08 -1.45E-08  2.76E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        2.30E-01
 
 TH 2
+       -5.18E-01  7.78E-03
 
 TH 3
+       -3.99E-01  9.62E-01  2.17E-03
 
 OM11
+       -2.12E-05  2.28E-05 -4.01E-05  1.66E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        3.18E+01
 
 TH 2
+        1.71E+03  3.15E+05
 
 TH 3
+       -4.55E+03 -1.01E+06  3.52E+06
 
 OM11
+       -3.29E-03 -8.18E-01  2.80E+00  3.63E+01
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         2.82E-02  6.83E-01  1.00E+00  2.29E+00
 
1
 PROBLEM NO.:         2
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E5.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      2
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   239.006543501048        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   7.5276E+01 -5.6039E+01  4.7305E+02 -4.9137E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   19.2337545010557        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       43
 NPARAMETR:  1.7953E+00  7.6655E-02  4.0062E-02  6.0308E-01
 PARAMETER: -4.1365E-01 -8.6712E-01 -8.2996E-01  6.5187E-01
 GRADIENT:  -3.1427E+01 -9.4280E+01  9.5043E+01 -2.7561E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   8.76316114761650        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       75
 NPARAMETR:  1.9514E+00  9.9607E-02  4.4141E-02  8.6795E-01
 PARAMETER: -3.3023E-01 -6.0216E-01 -7.3065E-01  8.3391E-01
 GRADIENT:  -4.5453E-01 -9.8987E-01  1.7731E-01  1.3751E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:   8.72933573945611        NO. OF FUNC. EVALS.:  12
 CUMULATIVE NO. OF FUNC. EVALS.:      113
 NPARAMETR:  1.9454E+00  1.0149E-01  4.4715E-02  8.1321E-01
 PARAMETER: -3.3331E-01 -5.8328E-01 -7.1742E-01  8.0134E-01
 GRADIENT:   3.4100E-02 -4.9228E-03 -8.7362E-02 -7.4358E-03
 
0ITERATION NO.:   17    OBJECTIVE VALUE:   8.72925828552509        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      123
 NPARAMETR:  1.9432E+00  1.0165E-01  4.4765E-02  8.1342E-01
 PARAMETER: -3.3448E-01 -5.8165E-01 -7.1629E-01  8.0146E-01
 GRADIENT:   8.7751E-03 -1.2232E-02  2.0257E-02 -1.7833E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      123
 NO. OF SIG. DIGITS IN FINAL EST.:  3.2
 #TERE:
 Elapsed estimation time in seconds:     0.04
 Elapsed covariance time in seconds:     0.01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************        8.729       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.94E+00  1.02E-01  4.48E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        8.13E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        9.02E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         4.31E-01  2.08E-02  6.21E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        3.47E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.92E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        1.86E-01
 
 TH 2
+       -5.23E-03  4.32E-04
 
 TH 3
+       -1.01E-03  1.18E-04  3.85E-05
 
 OM11
+        7.45E-05 -1.59E-06 -8.07E-08  1.20E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        4.31E-01
 
 TH 2
+       -5.84E-01  2.08E-02
 
 TH 3
+       -3.78E-01  9.15E-01  6.21E-03
 
 OM11
+        4.98E-04 -2.21E-04 -3.75E-05  3.47E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        1.06E+01
 
 TH 2
+        3.23E+02  2.41E+04
 
 TH 3
+       -7.13E+02 -6.54E+04  2.08E+05
 
 OM11
+       -2.78E-03  7.47E-02 -2.85E-01  8.31E+00
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         5.42E-02  6.66E-01  1.00E+00  2.28E+00
 
1
 PROBLEM NO.:         3
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      3
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   212.997523638581        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   4.0647E+01 -3.2163E+01  4.8407E+02 -4.3936E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   17.9859560937621        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       39
 NPARAMETR:  1.9460E+00  5.6591E-02  3.1859E-02  2.5139E-01
 PARAMETER: -3.3300E-01 -1.1753E+00 -1.0657E+00  2.1434E-01
 GRADIENT:  -8.5255E+01 -2.2188E+02  1.9600E+02 -4.4334E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -14.1111716735391        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       70
 NPARAMETR:  2.6501E+00  8.4038E-02  4.0514E-02  2.0877E-01
 PARAMETER: -2.4073E-02 -7.7401E-01 -8.1847E-01  1.2145E-01
 GRADIENT:   1.9661E+01  4.2143E+01 -3.0525E+01  1.5759E+01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -24.3983459097472        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:      101
 NPARAMETR:  2.4658E+00  8.0284E-02  3.9185E-02  4.0207E-02
 PARAMETER: -9.6171E-02 -8.2027E-01 -8.5266E-01 -7.0214E-01
 GRADIENT:  -5.1583E+00 -1.7123E+01  1.5370E+01  1.1025E-01
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -24.5011681702113        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      145
 NPARAMETR:  2.4533E+00  8.1433E-02  3.9562E-02  3.9633E-02
 PARAMETER: -1.0125E-01 -8.0588E-01 -8.4284E-01 -7.0933E-01
 GRADIENT:  -8.4407E-02 -2.6896E-01  4.5022E-01 -1.5902E-02
 
0ITERATION NO.:   22    OBJECTIVE VALUE:  -24.5011823780657        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      155
 NPARAMETR:  2.4536E+00  8.1425E-02  3.9559E-02  3.9663E-02
 PARAMETER: -1.0115E-01 -8.0598E-01 -8.4293E-01 -7.0895E-01
 GRADIENT:   6.5403E-04  9.9675E-03 -1.4827E-02  8.6605E-04
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      155
 NO. OF SIG. DIGITS IN FINAL EST.:  4.5
 #TERE:
 Elapsed estimation time in seconds:     0.06
 Elapsed covariance time in seconds:     0.01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      -24.501       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         2.45E+00  8.14E-02  3.96E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        3.97E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.99E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.44E-01  3.81E-03  1.31E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        1.69E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        4.25E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        2.07E-02
 
 TH 2
+       -3.09E-04  1.45E-05
 
 TH 3
+       -7.14E-05  4.66E-06  1.73E-06
 
 OM11
+       -1.94E-08 -2.76E-10 -3.81E-10  2.86E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        1.44E-01
 
 TH 2
+       -5.65E-01  3.81E-03
 
 TH 3
+       -3.78E-01  9.31E-01  1.31E-03
 
 OM11
+       -7.99E-06 -4.28E-06 -1.71E-05  1.69E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        9.39E+01
 
 TH 2
+        5.67E+03  8.60E+05
 
 TH 3
+       -1.14E+04 -2.09E+06  5.74E+06
 
 OM11
+       -3.36E-03 -1.56E+00  4.85E+00  3.50E+03
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         4.46E-02  6.75E-01  1.00E+00  2.28E+00
 
1
 PROBLEM NO.:         4
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      4
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   398.050518057523        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   1.8956E+02  2.0775E+02  3.0602E+02 -8.0943E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   8.04581370555692        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       37
 NPARAMETR:  8.4648E-01  1.0540E-01  3.8321E-02  7.9090E-01
 PARAMETER: -1.1661E+00 -5.4507E-01 -8.7556E-01  7.8743E-01
 GRADIENT:  -7.0094E+00  2.7645E+01 -4.5528E+01  7.6434E-01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   3.88479880844534        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       68
 NPARAMETR:  1.1253E+00  9.0169E-02  3.7964E-02  5.1548E-01
 PARAMETER: -8.8115E-01 -7.0277E-01 -8.8518E-01  5.7339E-01
 GRADIENT:  -2.3880E+00  5.9278E-01  5.3184E-01 -3.3527E-01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:   3.83006501957755        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      107
 NPARAMETR:  1.1738E+00  8.7210E-02  3.7325E-02  5.2075E-01
 PARAMETER: -8.3891E-01 -7.3652E-01 -9.0262E-01  5.7847E-01
 GRADIENT:   7.0378E-02  6.1222E-02 -3.0832E-01 -1.4888E-02
 
0ITERATION NO.:   17    OBJECTIVE VALUE:   3.82976720879417        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      117
 NPARAMETR:  1.1714E+00  8.7471E-02  3.7400E-02  5.2101E-01
 PARAMETER: -8.4096E-01 -7.3350E-01 -9.0054E-01  5.7873E-01
 GRADIENT:  -6.1517E-03  2.0364E-03 -5.4064E-03 -3.1414E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      117
 NO. OF SIG. DIGITS IN FINAL EST.:  3.9
 #TERE:
 Elapsed estimation time in seconds:     0.04
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************        3.830       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.17E+00  8.75E-02  3.74E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        5.21E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        7.22E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         2.04E-01  1.61E-02  4.51E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        2.22E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.54E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        4.14E-02
 
 TH 2
+       -2.09E-03  2.58E-04
 
 TH 3
+       -4.23E-04  6.74E-05  2.04E-05
 
 OM11
+       -9.68E-06  3.35E-07  3.71E-08  4.93E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        2.04E-01
 
 TH 2
+       -6.41E-01  1.61E-02
 
 TH 3
+       -4.60E-01  9.30E-01  4.51E-03
 
 OM11
+       -2.14E-04  9.38E-05  3.70E-05  2.22E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        5.36E+01
 
 TH 2
+        1.08E+03  5.05E+04
 
 TH 3
+       -2.45E+03 -1.45E+05  4.78E+05
 
 OM11
+        5.04E-03 -2.26E-02  1.43E-01  2.03E+01
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         4.38E-02  5.82E-01  1.00E+00  2.37E+00
 
1
 PROBLEM NO.:         5
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      5
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   452.329119658522        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   3.2126E+02  4.3879E+02  2.0039E+02 -9.1797E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   24.1655927776362        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       35
 NPARAMETR:  1.2499E+00  6.4535E-02  3.6645E-02  8.5412E-01
 PARAMETER: -7.7602E-01 -1.0417E+00 -9.2150E-01  8.2588E-01
 GRADIENT:  -3.7992E+01 -7.9258E+01  7.1403E+01 -2.9758E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   13.2246102303764        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       66
 NPARAMETR:  1.4772E+00  8.8165E-02  4.3527E-02  1.2051E+00
 PARAMETER: -6.0880E-01 -7.2551E-01 -7.4497E-01  9.9800E-01
 GRADIENT:   1.8735E-01  2.1689E-01  1.5858E-01 -3.2239E-01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:   13.2229335760209        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      103
 NPARAMETR:  1.4717E+00  8.8430E-02  4.3604E-02  1.2240E+00
 PARAMETER: -6.1252E-01 -7.2247E-01 -7.4316E-01  1.0058E+00
 GRADIENT:   5.5729E-03 -1.0520E-02  1.7664E-02  1.2444E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      103
 NO. OF SIG. DIGITS IN FINAL EST.:  3.7
 #TERE:
 Elapsed estimation time in seconds:     0.04
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************       13.223       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.47E+00  8.84E-02  4.36E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        1.22E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.11E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         3.19E-01  2.01E-02  6.71E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        5.22E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        2.36E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        1.02E-01
 
 TH 2
+       -3.83E-03  4.04E-04
 
 TH 3
+       -8.93E-04  1.25E-04  4.50E-05
 
 OM11
+        5.30E-05 -1.24E-06  2.31E-08  2.72E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        3.19E-01
 
 TH 2
+       -5.96E-01  2.01E-02
 
 TH 3
+       -4.17E-01  9.29E-01  6.71E-03
 
 OM11
+        3.18E-04 -1.18E-04  6.59E-06  5.22E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        1.93E+01
 
 TH 2
+        4.69E+02  2.94E+04
 
 TH 3
+       -9.21E+02 -7.26E+04  2.06E+05
 
 OM11
+       -1.55E-03  4.91E-02 -1.69E-01  3.67E+00
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         4.74E-02  6.33E-01  1.00E+00  2.32E+00
 
1
 PROBLEM NO.:         6
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      6
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   165.410046471590        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   1.6665E+02  3.1358E+02 -8.9370E+01 -3.4419E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   17.4667843763934        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       37
 NPARAMETR:  2.3697E-01  2.1877E-01  4.9235E-02  1.9929E+00
 PARAMETER: -2.4423E+00  1.9013E-01 -6.1903E-01  1.2495E+00
 GRADIENT:  -3.5595E+00 -2.7031E+00 -2.1110E+01  2.2620E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   3.63513286110247        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       71
 NPARAMETR:  7.8327E-01  1.7043E-01  7.0958E-02  3.3071E-01
 PARAMETER: -1.2438E+00 -6.0888E-02 -2.4723E-01  3.5146E-01
 GRADIENT:  -8.3716E+00 -6.2770E+00  5.3475E+01 -9.6024E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -5.53103228235256        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:      101
 NPARAMETR:  1.1603E+00  1.0008E-01  5.1404E-02  2.1277E-01
 PARAMETER: -8.5045E-01 -5.9741E-01 -5.7506E-01  1.3095E-01
 GRADIENT:  -3.4364E-01 -1.4072E+00  3.6056E+00 -9.8079E-01
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -5.54426645589903        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:      131
 NPARAMETR:  1.1669E+00  9.9019E-02  5.0943E-02  2.2148E-01
 PARAMETER: -8.4477E-01 -6.0815E-01 -5.8423E-01  1.5101E-01
 GRADIENT:   1.8968E-01  6.4759E-01 -7.0305E-01 -7.1443E-02
 
0ITERATION NO.:   24    OBJECTIVE VALUE:  -5.54577006708872        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      163
 NPARAMETR:  1.1637E+00  9.9528E-02  5.1137E-02  2.2224E-01
 PARAMETER: -8.4755E-01 -6.0297E-01 -5.8036E-01  1.5273E-01
 GRADIENT:  -4.3818E-04  1.2737E-02 -2.0379E-02  3.8706E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      163
 NO. OF SIG. DIGITS IN FINAL EST.:  3.3
 #TERE:
 Elapsed estimation time in seconds:     0.06
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************       -5.546       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.16E+00  9.95E-02  5.11E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        2.22E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        4.71E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.85E-01  1.62E-02  5.26E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        9.48E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        3.43E-02
 
 TH 2
+       -1.99E-03  2.62E-04
 
 TH 3
+       -4.58E-04  7.84E-05  2.77E-05
 
 OM11
+       -1.69E-06  5.51E-08 -1.89E-08  8.98E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        1.85E-01
 
 TH 2
+       -6.64E-01  1.62E-02
 
 TH 3
+       -4.71E-01  9.21E-01  5.26E-03
 
 OM11
+       -9.64E-05  3.59E-05 -3.79E-05  9.48E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        6.82E+01
 
 TH 2
+        1.19E+03  4.60E+04
 
 TH 3
+       -2.24E+03 -1.11E+05  3.13E+05
 
 OM11
+        8.48E-04 -2.91E-01  9.14E-01  1.11E+02
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         4.80E-02  5.64E-01  1.00E+00  2.39E+00
 
1
 PROBLEM NO.:         7
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      7
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   403.505995966353        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   3.4377E+02  8.2568E+02 -5.0803E+02 -8.2034E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   25.7968314486386        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       39
 NPARAMETR:  1.6722E-01  3.9010E-01  6.7521E-02  9.9694E+00
 PARAMETER: -2.7927E+00  7.7054E-01 -2.9760E-01  2.0545E+00
 GRADIENT:  -2.3331E+00 -1.7271E+00  4.1758E+00  2.0998E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   5.15591812855880        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       73
 NPARAMETR:  5.9684E-01  1.0296E-01  5.8959E-02  8.2648E-01
 PARAMETER: -1.5161E+00 -5.6873E-01 -4.3538E-01  8.0943E-01
 GRADIENT:  -1.9382E+01 -4.6871E+01  6.6662E+01  7.5071E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -5.56750079008502        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:      104
 NPARAMETR:  7.2259E-01  7.5086E-02  4.3910E-02  2.7073E-01
 PARAMETER: -1.3246E+00 -8.8808E-01 -7.3603E-01  2.5140E-01
 GRADIENT:  -3.0210E+01 -6.5615E+01  5.3423E+01  4.3934E+00
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -15.3750100121881        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:      135
 NPARAMETR:  6.8902E-01  1.0094E-01  5.1396E-02  8.6400E-02
 PARAMETER: -1.3722E+00 -5.8876E-01 -5.7521E-01 -3.1966E-01
 GRADIENT:   5.1261E-01 -6.3954E+00  1.1372E+01 -1.1154E+00
 
0ITERATION NO.:   25    OBJECTIVE VALUE:  -15.4147807071971        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      178
 NPARAMETR:  6.7963E-01  1.0226E-01  5.1596E-02  9.0555E-02
 PARAMETER: -1.3860E+00 -5.7565E-01 -5.7125E-01 -2.9618E-01
 GRADIENT:  -3.4419E-02  2.3445E-03 -2.6055E-02 -1.0111E-02
 
0ITERATION NO.:   26    OBJECTIVE VALUE:  -15.4147807071971        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      178
 NPARAMETR:  6.7963E-01  1.0226E-01  5.1596E-02  9.0555E-02
 PARAMETER: -1.3860E+00 -5.7565E-01 -5.7125E-01 -2.9618E-01
 GRADIENT:  -3.4419E-02  2.3445E-03 -2.6055E-02 -1.0111E-02
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      178
 NO. OF SIG. DIGITS IN FINAL EST.:  3.1
 #TERE:
 Elapsed estimation time in seconds:     0.06
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      -15.415       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         6.80E-01  1.02E-01  5.16E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        9.06E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        3.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         7.25E-02  1.16E-02  3.20E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        3.86E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        6.41E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        5.26E-03
 
 TH 2
+       -6.67E-04  1.35E-04
 
 TH 3
+       -1.41E-04  3.44E-05  1.03E-05
 
 OM11
+       -1.72E-06  1.58E-07  2.21E-08  1.49E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        7.25E-02
 
 TH 2
+       -7.92E-01  1.16E-02
 
 TH 3
+       -6.09E-01  9.25E-01  3.20E-03
 
 OM11
+       -6.16E-04  3.53E-04  1.79E-04  3.86E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        7.12E+02
 
 TH 2
+        7.07E+03  1.22E+05
 
 TH 3
+       -1.39E+04 -3.11E+05  9.50E+05
 
 OM11
+        2.80E-01 -1.28E-01  2.84E+00  6.71E+02
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         3.70E-02  4.05E-01  1.00E+00  2.56E+00
 
1
 PROBLEM NO.:         8
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      8
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   215.142300195100        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   1.7437E+02  3.0997E+02  3.1163E+01 -4.4365E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   10.5160709152559        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       35
 NPARAMETR:  7.8991E-01  1.7715E-01  6.0130E-02  6.6834E-01
 PARAMETER: -1.2354E+00 -2.1966E-02 -4.1539E-01  7.0324E-01
 GRADIENT:   1.3268E+01  5.4759E+01 -5.6671E+01 -7.8763E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE: -0.742251949670290        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       67
 NPARAMETR:  1.3926E+00  8.9443E-02  4.5866E-02  4.1870E-01
 PARAMETER: -6.6783E-01 -7.1095E-01 -6.9144E-01  4.6942E-01
 GRADIENT:  -1.3433E+00 -5.6798E+00  6.0372E+00  4.3398E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -1.03286334459316        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       99
 NPARAMETR:  1.3783E+00  9.1423E-02  4.6251E-02  3.3593E-01
 PARAMETER: -6.7816E-01 -6.8881E-01 -6.8290E-01  3.5929E-01
 GRADIENT:   2.1414E-02  5.7801E-01 -6.6991E-01  7.4656E-02
 
0ITERATION NO.:   19    OBJECTIVE VALUE:  -1.03479609432618        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      131
 NPARAMETR:  1.3755E+00  9.1958E-02  4.6463E-02  3.3483E-01
 PARAMETER: -6.8018E-01 -6.8291E-01 -6.7823E-01  3.5765E-01
 GRADIENT:   5.7032E-04  2.5242E-03 -2.2373E-03 -1.5765E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      131
 NO. OF SIG. DIGITS IN FINAL EST.:  4.0
 #TERE:
 Elapsed estimation time in seconds:     0.05
 Elapsed covariance time in seconds:     0.01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************       -1.035       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         1.38E+00  9.20E-02  4.65E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        3.35E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        5.79E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         2.32E-01  1.53E-02  5.04E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        1.43E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.23E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        5.36E-02
 
 TH 2
+       -2.27E-03  2.34E-04
 
 TH 3
+       -5.33E-04  7.13E-05  2.54E-05
 
 OM11
+       -4.46E-07  6.19E-08  1.55E-08  2.04E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        2.32E-01
 
 TH 2
+       -6.42E-01  1.53E-02
 
 TH 3
+       -4.57E-01  9.26E-01  5.04E-03
 
 OM11
+       -1.35E-05  2.84E-05  2.15E-05  1.43E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        4.10E+01
 
 TH 2
+        9.53E+02  5.21E+04
 
 TH 3
+       -1.82E+03 -1.26E+05  3.56E+05
 
 OM11
+       -6.20E-04 -4.14E-02  7.35E-02  4.91E+01
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         4.70E-02  5.84E-01  1.00E+00  2.37E+00
 
1
 PROBLEM NO.:         9
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      9
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   378.526612374407        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:  -1.3657E+02 -4.3255E+02  7.6877E+02 -7.7038E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   18.7474232370777        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       36
 NPARAMETR:  6.7698E+00  4.1964E-02  2.1358E-02  3.2470E+00
 PARAMETER:  9.1404E-01 -1.4806E+00 -1.4817E+00  1.4936E+00
 GRADIENT:  -2.7179E+00 -2.5242E+01  2.3587E+01  1.0432E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -5.20504378435242        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       71
 NPARAMETR:  9.8292E+00  8.4624E-02  3.2093E-02  2.4630E-01
 PARAMETER:  1.2870E+00 -7.6698E-01 -1.0581E+00  2.0411E-01
 GRADIENT:   1.3552E+00  4.3417E+00 -5.9780E+00  1.5877E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -5.34679836998290        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      114
 NPARAMETR:  8.8600E+00  8.6636E-02  3.2688E-02  2.2625E-01
 PARAMETER:  1.1832E+00 -7.4320E-01 -1.0392E+00  1.6166E-01
 GRADIENT:  -1.2965E-02 -3.3700E-03 -4.1743E-03 -1.2048E-03
 
0ITERATION NO.:   16    OBJECTIVE VALUE:  -5.34679836998290        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      114
 NPARAMETR:  8.8600E+00  8.6636E-02  3.2688E-02  2.2625E-01
 PARAMETER:  1.1832E+00 -7.4320E-01 -1.0392E+00  1.6166E-01
 GRADIENT:  -1.2965E-02 -3.3700E-03 -4.1743E-03 -1.2048E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      114
 NO. OF SIG. DIGITS IN FINAL EST.:  3.2
 #TERE:
 Elapsed estimation time in seconds:     0.05
 Elapsed covariance time in seconds:     0.02
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************       -5.347       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         8.86E+00  8.66E-02  3.27E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        2.26E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        4.76E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         2.84E+00  9.33E-03  2.78E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        9.65E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        8.07E+00
 
 TH 2
+       -9.29E-03  8.70E-05
 
 TH 3
+       -1.86E-03  2.45E-05  7.72E-06
 
 OM11
+       -2.30E-04  1.43E-07  1.52E-08  9.31E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        2.84E+00
 
 TH 2
+       -3.50E-01  9.33E-03
 
 TH 3
+       -2.36E-01  9.47E-01  2.78E-03
 
 OM11
+       -8.40E-04  1.58E-04  5.68E-05  9.65E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        1.57E-01
 
 TH 2
+        5.86E+01  1.32E+05
 
 TH 3
+       -1.48E+02 -4.06E+05  1.38E+06
 
 OM11
+        3.23E-03  8.70E-02  2.91E-01  1.07E+02
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         4.58E-02  8.51E-01  1.00E+00  2.10E+00
 
1
 PROBLEM NO.:        10
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:     10
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   800.050176178353        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   3.1731E+02  8.7579E+02 -1.0186E+02 -1.6133E+03
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   26.3313421787828        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       35
 NPARAMETR:  6.5373E-01  4.0986E-02  2.7993E-02  5.3779E+00
 PARAMETER: -1.4249E+00 -1.5048E+00 -1.1995E+00  1.7459E+00
 GRADIENT:  -9.6815E+00 -2.9023E+01  3.2058E+01  6.3753E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   13.2770413040620        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       65
 NPARAMETR:  7.3529E-01  4.3158E-02  2.1472E-02  2.5352E+00
 PARAMETER: -1.3071E+00 -1.4519E+00 -1.4761E+00  1.3699E+00
 GRADIENT:  -5.6532E+00 -4.1203E-01 -9.2793E+00  1.5920E+01
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  0.104307865377222        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       98
 NPARAMETR:  5.8223E-01  6.7867E-02  3.0143E-02  5.2355E-01
 PARAMETER: -1.5409E+00 -9.9060E-01 -1.1229E+00  5.8116E-01
 GRADIENT:  -4.1151E+01 -5.0177E+01  3.5316E+01  7.5633E+00
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -12.0303501385296        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:      128
 NPARAMETR:  6.9476E-01  7.3887E-02  3.2320E-02  1.2142E-01
 PARAMETER: -1.3639E+00 -9.0440E-01 -1.0509E+00 -1.4955E-01
 GRADIENT:   3.7932E+00  1.6695E+01 -1.7909E+01 -3.2376E-01
 
0ITERATION NO.:   25    OBJECTIVE VALUE:  -12.0642253443454        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      174
 NPARAMETR:  6.9565E-01  7.3965E-02  3.2443E-02  1.2317E-01
 PARAMETER: -1.3626E+00 -9.0333E-01 -1.0469E+00 -1.4238E-01
 GRADIENT:   1.7056E-01  2.4676E-01 -2.3281E-01  5.6291E-02
 
0ITERATION NO.:   27    OBJECTIVE VALUE:  -12.0642755381864        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      184
 NPARAMETR:  6.9546E-01  7.3964E-02  3.2442E-02  1.2283E-01
 PARAMETER: -1.3629E+00 -9.0333E-01 -1.0470E+00 -1.4374E-01
 GRADIENT:  -5.8465E-02 -8.2532E-02  6.1920E-02 -3.6189E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      184
 NO. OF SIG. DIGITS IN FINAL EST.:  3.3
 #TERE:
 Elapsed estimation time in seconds:     0.08
 Elapsed covariance time in seconds:     0.02
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      -12.064       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         6.95E-01  7.40E-02  3.24E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        1.23E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        3.50E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         5.96E-02  6.92E-03  1.86E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        5.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        7.47E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        3.56E-03
 
 TH 2
+       -3.11E-04  4.79E-05
 
 TH 3
+       -6.47E-05  1.21E-05  3.45E-06
 
 OM11
+       -8.42E-07 -4.70E-08 -1.62E-08  2.74E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        5.96E-02
 
 TH 2
+       -7.53E-01  6.92E-03
 
 TH 3
+       -5.84E-01  9.41E-01  1.86E-03
 
 OM11
+       -2.70E-04 -1.30E-04 -1.66E-04  5.24E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        9.44E+02
 
 TH 2
+        1.45E+04  4.05E+05
 
 TH 3
+       -3.31E+04 -1.15E+06  3.70E+06
 
 OM11
+        3.43E-01  4.60E+00 -8.02E+00  3.65E+02
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         3.04E-02  4.42E-01  1.00E+00  2.53E+00
 
1
 PROBLEM NO.:        11
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:     11
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   48.4568147212456        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   1.8484E+01  7.3254E+01  1.5408E+02 -1.1031E+02
 
0ITERATION NO.:    5    OBJECTIVE VALUE:  -6.58684324370097        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:       36
 NPARAMETR:  3.1482E+00  9.9225E-02  5.9898E-02  3.2069E-01
 PARAMETER:  1.4825E-01 -6.0604E-01 -4.1931E-01  3.3608E-01
 GRADIENT:  -2.8033E+01 -8.0319E+01  9.6497E+01  1.0157E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -23.1466480990983        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       68
 NPARAMETR:  3.8069E+00  1.0097E-01  5.7700E-02  5.3024E-02
 PARAMETER:  3.3828E-01 -5.8840E-01 -4.5734E-01 -5.6379E-01
 GRADIENT:   1.9669E+01  1.2462E+02 -1.3109E+02  3.6894E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -24.7420567072941        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       98
 NPARAMETR:  3.8603E+00  9.7566E-02  5.7005E-02  3.8720E-02
 PARAMETER:  3.5219E-01 -6.2308E-01 -4.6968E-01 -7.2099E-01
 GRADIENT:  -6.4370E-02  4.9106E-01 -8.0425E-01 -3.0993E-02
 
0ITERATION NO.:   19    OBJECTIVE VALUE:  -24.7577413168950        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      128
 NPARAMETR:  3.8490E+00  9.8124E-02  5.7246E-02  3.8739E-02
 PARAMETER:  3.4928E-01 -6.1732E-01 -4.6538E-01 -7.2073E-01
 GRADIENT:  -1.4630E-02 -6.5096E-02  8.6283E-02 -4.4606E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      128
 NO. OF SIG. DIGITS IN FINAL EST.:  3.8
 #TERE:
 Elapsed estimation time in seconds:     0.05
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      -24.758       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         3.85E+00  9.81E-02  5.72E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        3.87E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.97E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         2.48E-01  4.60E-03  1.95E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        1.65E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        4.20E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        6.17E-02
 
 TH 2
+       -5.78E-04  2.12E-05
 
 TH 3
+       -1.51E-04  8.29E-06  3.81E-06
 
 OM11
+       -2.78E-07  2.53E-09  3.02E-09  2.73E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        2.48E-01
 
 TH 2
+       -5.06E-01  4.60E-03
 
 TH 3
+       -3.12E-01  9.23E-01  1.95E-03
 
 OM11
+       -6.77E-05  3.32E-05  9.38E-05  1.65E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        2.79E+01
 
 TH 2
+        2.22E+03  4.96E+05
 
 TH 3
+       -3.72E+03 -9.91E+05  2.27E+06
 
 OM11
+        4.91E-02  8.65E+00 -1.98E+01  3.67E+03
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         5.24E-02  7.44E-01  1.00E+00  2.20E+00
 
1
 PROBLEM NO.:        12
 THEOPHYLLINE POPULATION DATA; Analysis of Individuals
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       12
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   8
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   3   2   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT EVID MDV .ID.
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 KA K CL SC
0FORMAT FOR DATA:
 (5E6.0,3F2.0)

 TOT. NO. OF OBS RECS:       11
 TOT. NO. OF INDIVIDUALS:     11
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.3000E+01     0.1000E+07
  0.1000E-02     0.2000E+00     0.1000E+07
  0.1000E-02     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.2000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:              NO
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
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID DOSE WT TIME
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                 NO
 FILE TO BE FORWARDED:  YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID KA K CL SC
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
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:     12
 #METH: First Order
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           SINGLE-SUBJECT
 SLOW GRADIENT METHOD USED:               YES 
 EPS-ETA INTERACTION:                     NO  
 NO. OF FUNCT. EVALS. ALLOWED:            450
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
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
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 MONITORING OF SEARCH:

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   585.096692744691        NO. OF FUNC. EVALS.:   5
 CUMULATIVE NO. OF FUNC. EVALS.:        5
 NPARAMETR:  3.0000E+00  2.0000E-01  1.0000E-01  2.0000E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   3.3817E+02  5.0459E+02  1.9734E+02 -1.1835E+03
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   17.7264348834753        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       46
 NPARAMETR:  5.4591E-01  1.6188E-01  4.5713E-02  2.9689E-01
 PARAMETER: -1.6054E+00 -1.1266E-01 -6.9485E-01  2.9753E-01
 GRADIENT:   5.6525E+01  1.8416E+02 -2.8832E+02 -4.0151E+01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -3.17455095561370        NO. OF FUNC. EVALS.:   6
 CUMULATIVE NO. OF FUNC. EVALS.:       76
 NPARAMETR:  8.6881E-01  1.0161E-01  4.0453E-02  3.0067E-01
 PARAMETER: -1.1401E+00 -5.8201E-01 -8.2001E-01  3.0385E-01
 GRADIENT:   1.4635E+01  3.8130E+01 -5.7324E+01  1.9169E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:  -3.91841232091213        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:      107
 NPARAMETR:  8.4513E-01  1.0474E-01  4.1728E-02  2.8372E-01
 PARAMETER: -1.1677E+00 -5.5141E-01 -7.8819E-01  2.7484E-01
 GRADIENT:   4.8783E+00  9.0904E+00 -1.0514E+01  2.1276E+00
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -4.01496116203084        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:      150
 NPARAMETR:  8.3295E-01  1.0557E-01  4.1997E-02  2.5541E-01
 PARAMETER: -1.1823E+00 -5.4341E-01 -7.8162E-01  2.2228E-01
 GRADIENT:   7.0329E-03 -1.5074E-02  3.2624E-02  2.5620E-03
 
0ITERATION NO.:   21    OBJECTIVE VALUE:  -4.01496116203084        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      150
 NPARAMETR:  8.3295E-01  1.0557E-01  4.1997E-02  2.5541E-01
 PARAMETER: -1.1823E+00 -5.4341E-01 -7.8162E-01  2.2228E-01
 GRADIENT:   7.0329E-03 -1.5074E-02  3.2624E-02  2.5620E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      150
 NO. OF SIG. DIGITS IN FINAL EST.:  3.6
 #TERE:
 Elapsed estimation time in seconds:     0.06
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************       -4.015       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         8.33E-01  1.06E-01  4.20E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        2.55E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        5.05E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         9.97E-02  1.29E-02  2.92E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1   
 
 ETA1
+        1.09E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1   
 
 ETA1
+        1.08E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        9.94E-03
 
 TH 2
+       -9.66E-04  1.66E-04
 
 TH 3
+       -1.61E-04  3.44E-05  8.50E-06
 
 OM11
+        2.21E-06 -1.10E-07  1.02E-08  1.19E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        9.97E-02
 
 TH 2
+       -7.52E-01  1.29E-02
 
 TH 3
+       -5.54E-01  9.16E-01  2.92E-03
 
 OM11
+        2.03E-04 -7.83E-05  3.23E-05  1.09E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      OM11  
 
 TH 1
+        3.11E+02
 
 TH 2
+        3.64E+03  7.99E+04
 
 TH 3
+       -8.84E+03 -2.54E+05  9.80E+05
 
 OM11
+       -1.65E-02  2.82E-01 -1.56E+00  8.43E+01
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                   FIRST ORDER                                  ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4
 
         4.52E-02  4.64E-01  1.00E+00  2.49E+00
 
 #CPUT: Total CPU Time in Seconds,        0.827
Stop Time: 
Mon 09/30/2013 
06:27 PM

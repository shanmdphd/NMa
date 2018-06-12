Mon 09/30/2013 
06:27 PM
; THIS IS THE FULL CONTROL STREAM FOR HELP FILE infn2.exa
$PROB  THEOPHYLLINE   POPULATION DATA
$INPUT      ID DOSE=AMT TIME CP=DV WT
$DATA       THEOPP
$SUBROUTINES  ADVAN2 

$ABBR REPLACE INTVBL=WT
$ABBR REPLACE NULLVAL=0.0
 INCLUDE 'infnabbr'

$PK
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
;SCALING PARAMETER=VOLUME/WT SINCE DOSE IS WEIGHT-ADJUSTED
   KA=THETA(1)+ETA(1)
   K=THETA(2)+ETA(2)
   CL=THETA(3)*WT+ETA(3)
   SC=CL/K/WT

$THETA  (.1,3,5) (.008,.08,.5) (.004,.04,.9)
$OMEGA BLOCK(3)  6 .005 .0002 .3 .006 .4

$ERROR
   Y=F+EPS(1)

$SIGMA  .4
$TABLE ID DOSE TIME DV WT NOAPPEND FILE=THEOPPexa2
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
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
0FORMAT FOR DATA:
 (5E6.0,2F2.0)

 TOT. NO. OF OBS RECS:      132
 TOT. NO. OF INDIVIDUALS:     12
0LENGTH OF THETA:   3
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E+00     0.3000E+01     0.5000E+01
  0.8000E-02     0.8000E-01     0.5000E+00
  0.4000E-02     0.4000E-01     0.9000E+00
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.6000E+01
                  0.5000E-02   0.2000E-03
                  0.3000E+00   0.6000E-02   0.4000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.4000E+00
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE:            EIGENVALUE
0-- TABLE   1 --
04 COLUMNS APPENDED:     NO
 PRINTED:               YES
 FOR TABLE FILE,
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
   EVENT ID DATA ITEM IS DATA ITEM NO.:      6
   TIME DATA ITEM IS DATA ITEM NO.:          3
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   2

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      1
 #METH: First Order (Evaluation)
 
 ESTIMATION STEP OMITTED:                 YES 
 ANALYSIS TYPE:                           POPULATION
 EPS-ETA INTERACTION:                     NO  
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
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************      110.244       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3     
 
         3.00E+00  8.00E-02  4.00E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3   
 
 ETA1
+        6.00E+00
 
 ETA2
+        5.00E-03  2.00E-04
 
 ETA3
+        3.00E-01  6.00E-03  4.00E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        4.00E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3   
 
 ETA1
+        2.45E+00
 
 ETA2
+        1.44E-01  1.41E-02
 
 ETA3
+        1.94E-01  6.71E-01  6.32E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.32E-01
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                          TABLES OF DATA AND PREDICTIONS                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1TABLE NO.  1



 LINE NO.ID        DOSE      TIME      CP        WT       
 
    1
+        1.00E+00  4.02E+00  0.00E+00  0.00E+00  7.96E+01
 
    2
+        1.00E+00  0.00E+00  0.00E+00  7.40E-01  7.96E+01
 
    3
+        1.00E+00  0.00E+00  2.50E-01  2.84E+00  7.96E+01
 
    4
+        1.00E+00  0.00E+00  5.70E-01  6.57E+00  7.96E+01
 
    5
+        1.00E+00  0.00E+00  1.12E+00  1.05E+01  7.96E+01
 
    6
+        1.00E+00  0.00E+00  2.02E+00  9.66E+00  7.96E+01
 
    7
+        1.00E+00  0.00E+00  3.82E+00  8.58E+00  7.96E+01
 
    8
+        1.00E+00  0.00E+00  5.10E+00  8.36E+00  7.96E+01
 
    9
+        1.00E+00  0.00E+00  7.03E+00  7.47E+00  7.96E+01
 
   10
+        1.00E+00  0.00E+00  9.05E+00  6.89E+00  7.96E+01
 
   11
+        1.00E+00  0.00E+00  1.21E+01  5.94E+00  7.96E+01
 
   12
+        1.00E+00  0.00E+00  2.44E+01  3.28E+00  7.96E+01
 
   13
+        2.00E+00  4.40E+00  0.00E+00  0.00E+00  7.24E+01
 
   14
+        2.00E+00  0.00E+00  0.00E+00  0.00E+00  7.24E+01
 
   15
+        2.00E+00  0.00E+00  2.70E-01  1.72E+00  7.24E+01
 
   16
+        2.00E+00  0.00E+00  5.20E-01  7.91E+00  7.24E+01
 
   17
+        2.00E+00  0.00E+00  1.00E+00  8.31E+00  7.24E+01
 
   18
+        2.00E+00  0.00E+00  1.92E+00  8.33E+00  7.24E+01
 
   19
+        2.00E+00  0.00E+00  3.50E+00  6.85E+00  7.24E+01
 
   20
+        2.00E+00  0.00E+00  5.02E+00  6.08E+00  7.24E+01
 
   21
+        2.00E+00  0.00E+00  7.03E+00  5.40E+00  7.24E+01
 
   22
+        2.00E+00  0.00E+00  9.00E+00  4.55E+00  7.24E+01
 
   23
+        2.00E+00  0.00E+00  1.20E+01  3.01E+00  7.24E+01
 
   24
+        2.00E+00  0.00E+00  2.43E+01  9.00E-01  7.24E+01
 
   25
+        3.00E+00  4.53E+00  0.00E+00  0.00E+00  7.05E+01
 
1

 LINE NO.ID        DOSE      TIME      CP        WT       
 
   26
+        3.00E+00  0.00E+00  0.00E+00  0.00E+00  7.05E+01
 
   27
+        3.00E+00  0.00E+00  2.70E-01  4.40E+00  7.05E+01
 
   28
+        3.00E+00  0.00E+00  5.80E-01  6.90E+00  7.05E+01
 
   29
+        3.00E+00  0.00E+00  1.02E+00  8.20E+00  7.05E+01
 
   30
+        3.00E+00  0.00E+00  2.02E+00  7.80E+00  7.05E+01
 
   31
+        3.00E+00  0.00E+00  3.62E+00  7.50E+00  7.05E+01
 
   32
+        3.00E+00  0.00E+00  5.08E+00  6.20E+00  7.05E+01
 
   33
+        3.00E+00  0.00E+00  7.07E+00  5.30E+00  7.05E+01
 
   34
+        3.00E+00  0.00E+00  9.00E+00  4.90E+00  7.05E+01
 
   35
+        3.00E+00  0.00E+00  1.22E+01  3.70E+00  7.05E+01
 
   36
+        3.00E+00  0.00E+00  2.42E+01  1.05E+00  7.05E+01
 
   37
+        4.00E+00  4.40E+00  0.00E+00  0.00E+00  7.27E+01
 
   38
+        4.00E+00  0.00E+00  0.00E+00  0.00E+00  7.27E+01
 
   39
+        4.00E+00  0.00E+00  3.50E-01  1.89E+00  7.27E+01
 
   40
+        4.00E+00  0.00E+00  6.00E-01  4.60E+00  7.27E+01
 
   41
+        4.00E+00  0.00E+00  1.07E+00  8.60E+00  7.27E+01
 
   42
+        4.00E+00  0.00E+00  2.13E+00  8.38E+00  7.27E+01
 
   43
+        4.00E+00  0.00E+00  3.50E+00  7.54E+00  7.27E+01
 
   44
+        4.00E+00  0.00E+00  5.02E+00  6.88E+00  7.27E+01
 
   45
+        4.00E+00  0.00E+00  7.02E+00  5.78E+00  7.27E+01
 
   46
+        4.00E+00  0.00E+00  9.02E+00  5.33E+00  7.27E+01
 
   47
+        4.00E+00  0.00E+00  1.20E+01  4.19E+00  7.27E+01
 
   48
+        4.00E+00  0.00E+00  2.46E+01  1.15E+00  7.27E+01
 
   49
+        5.00E+00  5.86E+00  0.00E+00  0.00E+00  5.46E+01
 
   50
+        5.00E+00  0.00E+00  0.00E+00  0.00E+00  5.46E+01
 
   51
+        5.00E+00  0.00E+00  3.00E-01  2.02E+00  5.46E+01
 
1

 LINE NO.ID        DOSE      TIME      CP        WT       
 
   52
+        5.00E+00  0.00E+00  5.20E-01  5.63E+00  5.46E+01
 
   53
+        5.00E+00  0.00E+00  1.00E+00  1.14E+01  5.46E+01
 
   54
+        5.00E+00  0.00E+00  2.02E+00  9.33E+00  5.46E+01
 
   55
+        5.00E+00  0.00E+00  3.50E+00  8.74E+00  5.46E+01
 
   56
+        5.00E+00  0.00E+00  5.02E+00  7.56E+00  5.46E+01
 
   57
+        5.00E+00  0.00E+00  7.02E+00  7.09E+00  5.46E+01
 
   58
+        5.00E+00  0.00E+00  9.10E+00  5.90E+00  5.46E+01
 
   59
+        5.00E+00  0.00E+00  1.20E+01  4.37E+00  5.46E+01
 
   60
+        5.00E+00  0.00E+00  2.44E+01  1.57E+00  5.46E+01
 
   61
+        6.00E+00  4.00E+00  0.00E+00  0.00E+00  8.00E+01
 
   62
+        6.00E+00  0.00E+00  0.00E+00  0.00E+00  8.00E+01
 
   63
+        6.00E+00  0.00E+00  2.70E-01  1.29E+00  8.00E+01
 
   64
+        6.00E+00  0.00E+00  5.80E-01  3.08E+00  8.00E+01
 
   65
+        6.00E+00  0.00E+00  1.15E+00  6.44E+00  8.00E+01
 
   66
+        6.00E+00  0.00E+00  2.03E+00  6.32E+00  8.00E+01
 
   67
+        6.00E+00  0.00E+00  3.57E+00  5.53E+00  8.00E+01
 
   68
+        6.00E+00  0.00E+00  5.00E+00  4.94E+00  8.00E+01
 
   69
+        6.00E+00  0.00E+00  7.00E+00  4.02E+00  8.00E+01
 
   70
+        6.00E+00  0.00E+00  9.22E+00  3.46E+00  8.00E+01
 
   71
+        6.00E+00  0.00E+00  1.21E+01  2.78E+00  8.00E+01
 
   72
+        6.00E+00  0.00E+00  2.39E+01  9.20E-01  8.00E+01
 
   73
+        7.00E+00  4.95E+00  0.00E+00  0.00E+00  6.46E+01
 
   74
+        7.00E+00  0.00E+00  0.00E+00  1.50E-01  6.46E+01
 
   75
+        7.00E+00  0.00E+00  2.50E-01  8.50E-01  6.46E+01
 
   76
+        7.00E+00  0.00E+00  5.00E-01  2.35E+00  6.46E+01
 
   77
+        7.00E+00  0.00E+00  1.02E+00  5.02E+00  6.46E+01
 
1

 LINE NO.ID        DOSE      TIME      CP        WT       
 
   78
+        7.00E+00  0.00E+00  2.02E+00  6.58E+00  6.46E+01
 
   79
+        7.00E+00  0.00E+00  3.48E+00  7.09E+00  6.46E+01
 
   80
+        7.00E+00  0.00E+00  5.00E+00  6.66E+00  6.46E+01
 
   81
+        7.00E+00  0.00E+00  6.98E+00  5.25E+00  6.46E+01
 
   82
+        7.00E+00  0.00E+00  9.00E+00  4.39E+00  6.46E+01
 
   83
+        7.00E+00  0.00E+00  1.21E+01  3.53E+00  6.46E+01
 
   84
+        7.00E+00  0.00E+00  2.42E+01  1.15E+00  6.46E+01
 
   85
+        8.00E+00  4.53E+00  0.00E+00  0.00E+00  7.05E+01
 
   86
+        8.00E+00  0.00E+00  0.00E+00  0.00E+00  7.05E+01
 
   87
+        8.00E+00  0.00E+00  2.50E-01  3.05E+00  7.05E+01
 
   88
+        8.00E+00  0.00E+00  5.20E-01  3.05E+00  7.05E+01
 
   89
+        8.00E+00  0.00E+00  9.80E-01  7.31E+00  7.05E+01
 
   90
+        8.00E+00  0.00E+00  2.02E+00  7.56E+00  7.05E+01
 
   91
+        8.00E+00  0.00E+00  3.53E+00  6.59E+00  7.05E+01
 
   92
+        8.00E+00  0.00E+00  5.05E+00  5.88E+00  7.05E+01
 
   93
+        8.00E+00  0.00E+00  7.15E+00  4.73E+00  7.05E+01
 
   94
+        8.00E+00  0.00E+00  9.07E+00  4.57E+00  7.05E+01
 
   95
+        8.00E+00  0.00E+00  1.21E+01  3.00E+00  7.05E+01
 
   96
+        8.00E+00  0.00E+00  2.41E+01  1.25E+00  7.05E+01
 
   97
+        9.00E+00  3.10E+00  0.00E+00  0.00E+00  8.64E+01
 
   98
+        9.00E+00  0.00E+00  0.00E+00  0.00E+00  8.64E+01
 
   99
+        9.00E+00  0.00E+00  3.00E-01  7.37E+00  8.64E+01
 
  100
+        9.00E+00  0.00E+00  6.30E-01  9.03E+00  8.64E+01
 
  101
+        9.00E+00  0.00E+00  1.05E+00  7.14E+00  8.64E+01
 
  102
+        9.00E+00  0.00E+00  2.02E+00  6.33E+00  8.64E+01
 
  103
+        9.00E+00  0.00E+00  3.53E+00  5.66E+00  8.64E+01
 
1

 LINE NO.ID        DOSE      TIME      CP        WT       
 
  104
+        9.00E+00  0.00E+00  5.02E+00  5.67E+00  8.64E+01
 
  105
+        9.00E+00  0.00E+00  7.17E+00  4.24E+00  8.64E+01
 
  106
+        9.00E+00  0.00E+00  8.80E+00  4.11E+00  8.64E+01
 
  107
+        9.00E+00  0.00E+00  1.16E+01  3.16E+00  8.64E+01
 
  108
+        9.00E+00  0.00E+00  2.44E+01  1.12E+00  8.64E+01
 
  109
+        1.00E+01  5.50E+00  0.00E+00  0.00E+00  5.82E+01
 
  110
+        1.00E+01  0.00E+00  0.00E+00  2.40E-01  5.82E+01
 
  111
+        1.00E+01  0.00E+00  3.70E-01  2.89E+00  5.82E+01
 
  112
+        1.00E+01  0.00E+00  7.70E-01  5.22E+00  5.82E+01
 
  113
+        1.00E+01  0.00E+00  1.02E+00  6.41E+00  5.82E+01
 
  114
+        1.00E+01  0.00E+00  2.05E+00  7.83E+00  5.82E+01
 
  115
+        1.00E+01  0.00E+00  3.55E+00  1.02E+01  5.82E+01
 
  116
+        1.00E+01  0.00E+00  5.05E+00  9.18E+00  5.82E+01
 
  117
+        1.00E+01  0.00E+00  7.08E+00  8.02E+00  5.82E+01
 
  118
+        1.00E+01  0.00E+00  9.38E+00  7.14E+00  5.82E+01
 
  119
+        1.00E+01  0.00E+00  1.21E+01  5.68E+00  5.82E+01
 
  120
+        1.00E+01  0.00E+00  2.37E+01  2.42E+00  5.82E+01
 
  121
+        1.10E+01  4.92E+00  0.00E+00  0.00E+00  6.50E+01
 
  122
+        1.10E+01  0.00E+00  0.00E+00  0.00E+00  6.50E+01
 
  123
+        1.10E+01  0.00E+00  2.50E-01  4.86E+00  6.50E+01
 
  124
+        1.10E+01  0.00E+00  5.00E-01  7.24E+00  6.50E+01
 
  125
+        1.10E+01  0.00E+00  9.80E-01  8.00E+00  6.50E+01
 
  126
+        1.10E+01  0.00E+00  1.98E+00  6.81E+00  6.50E+01
 
  127
+        1.10E+01  0.00E+00  3.60E+00  5.87E+00  6.50E+01
 
  128
+        1.10E+01  0.00E+00  5.02E+00  5.22E+00  6.50E+01
 
  129
+        1.10E+01  0.00E+00  7.03E+00  4.45E+00  6.50E+01
 
1

 LINE NO.ID        DOSE      TIME      CP        WT       
 
  130
+        1.10E+01  0.00E+00  9.03E+00  3.62E+00  6.50E+01
 
  131
+        1.10E+01  0.00E+00  1.21E+01  2.69E+00  6.50E+01
 
  132
+        1.10E+01  0.00E+00  2.41E+01  8.60E-01  6.50E+01
 
  133
+        1.20E+01  5.30E+00  0.00E+00  0.00E+00  6.05E+01
 
  134
+        1.20E+01  0.00E+00  0.00E+00  0.00E+00  6.05E+01
 
  135
+        1.20E+01  0.00E+00  2.50E-01  1.25E+00  6.05E+01
 
  136
+        1.20E+01  0.00E+00  5.00E-01  3.96E+00  6.05E+01
 
  137
+        1.20E+01  0.00E+00  1.00E+00  7.82E+00  6.05E+01
 
  138
+        1.20E+01  0.00E+00  2.00E+00  9.72E+00  6.05E+01
 
  139
+        1.20E+01  0.00E+00  3.52E+00  9.75E+00  6.05E+01
 
  140
+        1.20E+01  0.00E+00  5.07E+00  8.57E+00  6.05E+01
 
  141
+        1.20E+01  0.00E+00  7.07E+00  6.59E+00  6.05E+01
 
  142
+        1.20E+01  0.00E+00  9.03E+00  6.11E+00  6.05E+01
 
  143
+        1.20E+01  0.00E+00  1.21E+01  4.57E+00  6.05E+01
 
  144
+        1.20E+01  0.00E+00  2.41E+01  1.17E+00  6.05E+01
 
 #CPUT: Total CPU Time in Seconds,        0.109
Stop Time: 
Mon 09/30/2013 
06:27 PM

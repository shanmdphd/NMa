Mon 09/30/2013 
03:01 PM
; This example is discussed in Repetition_1 example (repeti1.exa) 
; in the online help directory.
; Like repeat1t.ctl, it demonstates that a repeated pass may start with 
; any designated record, not necessarily the first record.
; repeat1s uses the RPT_ data item for the same purpose.
;
 $PROB repeat1s
 $INPT ID TIME AMT DV EVID RPT_
 $DATA repeat1s.dat NULL=.
 $SUB ADVAN6 TOL=5
 $MODEL COMP=(DEPOT DEFDOSE) COMP=(CENTRAL DEFOBS)

 $PK
 KA=THETA(1)*EXP(ETA(1))
 K =THETA(2)*EXP(ETA(2))
 C =THETA(3)
 V =THETA(4)*EXP(ETA(4))
 S2=V
 IF (RPTI.EQ.0) TI=TIME
 IF (NEWIND.EQ.2) RPTO=-1

 $DES
 DADT(1)=-KA*A(1)
 D=EXP(-K*(TI-T)**C)
 DADT(2)=D*KA*A(1)

 $ERROR
 Y=F+EPS(1)

 $THETA 2 1 1 2 
 $OMEGA 1 2 (1 FIX) 1
 $SIGMA 1
 $TABLE TIME TI
  
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
 repeat1s
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       10
 NO. OF DATA ITEMS IN DATA SET:   7
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
 RPT DATA ITEM IS DATA ITEM NO.:  6
0INDICES PASSED TO SUBROUTINE PRED:
   5   2   3   0   0   0   0   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME AMT DV EVID RPT_ MDV
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 TI
0FORMAT FOR DATA:
 (6E5.0,1F2.0)

 TOT. NO. OF OBS RECS:        8
 TOT. NO. OF INDIVIDUALS:      2
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  0  2
  0  0  3
  0  0  0  4
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.2000E+01  0.1000E+01  0.1000E+01  0.2000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.1000E+01
        2                                                                                   NO
                  0.2000E+01
        3                                                                                  YES
                  0.1000E+01
        4                                                                                   NO
                  0.1000E+01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+01
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
0-- TABLE   1 --
 PRINTED:               YES
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 TIME TI
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(P)

 GENERAL NONLINEAR KINETICS MODEL (ADVAN6)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   4
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         DEPOT        ON         YES        YES        YES        NO
    2         CENTRAL      ON         YES        YES        NO         YES
    3         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE FROM SUBROUTINE TOL:   5
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            5           *           *           *           *
    3            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      5
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   3

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
0DES SUBROUTINE USES COMPACT STORAGE MODE.
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
 





 #OBJV:********************************************       12.886       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.00E+00  1.00E+00  1.00E+00  2.00E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.00E+00
 
 ETA2
+        0.00E+00  2.00E+00
 
 ETA3
+        0.00E+00  0.00E+00  1.00E+00
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4   
 
 ETA1
+        1.00E+00
 
 ETA2
+        0.00E+00  1.41E+00
 
 ETA3
+        0.00E+00  0.00E+00  1.00E+00
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.00E+00
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                          TABLES OF DATA AND PREDICTIONS                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1TABLE NO.  1



 LINE NO.TIME      TI        DV        PRED      RES       WRES     
 
    1
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00
 
    2
+        2.50E-01  2.50E-01  2.44E+00  1.72E+00  7.17E-01 -5.80E-01
 
    3
+        5.00E-01  5.00E-01  5.24E+00  2.39E+00  2.85E+00  7.67E-01
 
    4
+        7.50E-01  7.50E-01  5.57E+00  2.49E+00  3.08E+00  5.74E-01
 
    5
+        1.00E+00  1.00E+00  5.85E+00  2.33E+00  3.52E+00  8.66E-01
 
    6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00
 
    7
+        2.50E-01  2.50E-01  2.44E+00  1.72E+00  7.17E-01 -5.80E-01
 
    8
+        5.00E-01  5.00E-01  5.24E+00  2.39E+00  2.85E+00  7.67E-01
 
    9
+        7.50E-01  7.50E-01  5.57E+00  2.49E+00  3.08E+00  5.74E-01
 
   10
+        1.00E+00  1.00E+00  5.85E+00  2.33E+00  3.52E+00  8.66E-01
 
 #CPUT: Total CPU Time in Seconds,        0.078
Stop Time: 
Mon 09/30/2013 
03:01 PM

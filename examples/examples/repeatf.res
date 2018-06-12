Mon 09/30/2013 
03:02 PM
; This example is discussed in Repetition_1 example (repeti1.exa) 
; in the online help directory.
; It demonstrates the use of RPT_ and RPTON
; When data item RPT_=n and PRED sets rpto=-n with the same record,
; repeated calls are made to PRED using this one record.
; This is used to compute the factorial of the value in KRPT.
;
$PROB
$INPT ID TIME DV MDV RPT_ KRPT
$DATA repeatf.dat IGNORE=@ 
$PRED
   F=THETA(1)*EXP(ETA(1)) ; default value

   prdfl=1 ; required with RPT_
   IF (RPT_.EQ.1) RPTO=-1     ; tells NONMEM to repeat this record
   RPTON=KRPT                 ; tells how many times to repeat this record.
   last=0                     ; when last=1, this is the last call with repeated record
   IF (RPTI .EQ. 0) THEN      ; not a repeated call
    factorial=1
    count=0                   ; count calls with repeated records
   ELSE                       ; repeated call
    count=count+1             ; count calls with repeated records
    factorial=factorial*count
    IF (count == rpton) last=1  ; identify the last call with repeated record
   ENDIF
   IF (last==1) F=factorial*EXP(ETA(2))  ; prediction=KRPT! (factorial of KRPT)

  Y=F+EPS(1)

$THETA 2
$OMEGA .1 .1
$SIGMA .1
$TABLE RPT_ KRPT PRED NOAPPEND FILE=repeatf.tab
  
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

0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:        8
 NO. OF DATA ITEMS IN DATA SET:   6
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  4
 RPT DATA ITEM IS DATA ITEM NO.:  5
0LABELS FOR DATA ITEMS:
 ID TIME DV MDV RPT_ KRPT
0FORMAT FOR DATA:
 (6E2.0)

 TOT. NO. OF OBS RECS:        8
 TOT. NO. OF INDIVIDUALS:      2
0LENGTH OF THETA:   1
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.2000E+01
0INITIAL ESTIMATE OF OMEGA:
 0.1000E+00
 0.0000E+00   0.1000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+00
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
 RPT_ KRPT PRED
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
 





 #OBJV:********************************************      284.944       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1     
 
         2.00E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        1.00E-01
 
 ETA2
+        0.00E+00  1.00E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        3.16E-01
 
 ETA2
+        0.00E+00  3.16E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        3.16E-01
 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                          TABLES OF DATA AND PREDICTIONS                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1TABLE NO.  1



 LINE NO.RPT_      KRPT      PRED     
 
    1
+        0.00E+00  0.00E+00  2.00E+00
 
    2
+        1.00E+00  4.00E+00  2.40E+01
 
    3
+        0.00E+00  0.00E+00  2.00E+00
 
    4
+        1.00E+00  3.00E+00  6.00E+00
 
    5
+        0.00E+00  0.00E+00  2.00E+00
 
    6
+        1.00E+00  4.00E+00  2.40E+01
 
    7
+        0.00E+00  0.00E+00  2.00E+00
 
    8
+        1.00E+00  3.00E+00  6.00E+00
 
 #CPUT: Total CPU Time in Seconds,        0.109
Stop Time: 
Mon 09/30/2013 
03:02 PM

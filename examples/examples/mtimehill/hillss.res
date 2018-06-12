Tue 05/19/2015 
04:33 PM
; EHC example using Hill (sigmoid emax model)
;
; The help example for MODEL TIME EXAMPLES contains this fragment of code for EHC:
;     $PK
;     MTIME(1)=THETA(8)
;     MTIME(2)=MTIME(1)+THETA(9)
;      ....
;     $DES
;     FLAG=MPAST(1)-MPAST(2)
;     DADT(1)=-KA*A(1)+K41*A(4)*FLAG
;     DADT(4)=K1G*A(2)-K41*A(4)*FLAG
;      ....
; MTIME variables are ignored during Steady-State calculation, which is intended for 
; a single repeated dose with no changes in the status of the system.
; Instead, this example uses Hill (sigmoid emax model) for FLAG1 and FLAG2.
; The Steady-state dose gives the correct result. 
; This is an example of a smooth step model.
; 
$PROB  EHC using Hill (sigmoid emax model) for the flags
$INPUT      ID DOSE=AMT TIME CP=DV WT SS II ADDL EVID
$DATA       hillss.dat

$SUBROUTINES  ADVAN6 TOL=6
$MODEL COMP=(DEPOT,INITIALOFF,DEFDOSE) COMP=(CENTRAL,DEFOBS,NOOFF)
COMP=(PERIPH) COMP=(GALL )

$PK
   KA=THETA(1)*EXP(ETA(1))
   KE=THETA(2)*EXP(ETA(2))
   CL=THETA(3)*WT*EXP(ETA(3))
   S2=CL/KE/WT
   K41=THETA(4)*EXP(ETA(4))
   K23=THETA(5)*EXP(ETA(5))
   K32=THETA(6)*EXP(ETA(6))
   K1G=THETA(7)*EXP(ETA(7))

$DES
; Save the value of II from the dose record.
   if (ii>0) inter=ii
   mt1=inter*INT(T/inter)+theta(8)
   mt2=mt1+theta(9)
   hill1=exp(-THETA(10)*(t-mt1)) 
   hill2=exp(-THETA(10)*(t-mt2)) 
   flag1=1./(1+hill1)  ; changes from 0 to 1 near t=mt1
   flag2=1./(1+hill2)  ; changes from 0 to 1 near t=mt2
   flag=flag1-flag2

   DADT(1)=-KA*A(1)+K41*A(4)*FLAG
   DADT(2)= KA*A(1)-KE*A(2)-K23*A(2)+K32*A(3)-K1G*A(2)
   DADT(3)= K23*A(2)-K32*A(3)
   DADT(4)= K1G*A(2)-K41*A(4)*FLAG

$ERROR
A1=A(1) ; for display in table
A2=A(2)
A3=A(3)
A4=A(4)
   Y=F+EPS(1)

$THETA 1 1 1
$THETA 10  ; rate of gall bladder emptying is large vs. other k's
$THETA 1 1 1
$THETA 3 5 ; start emptying at T+theta(8) ; lasts till T+theta(9)
$THETA 50 ; the larger the hill exponent, the better the predictions 
          ; coincide with MTIME model.
          ; However, as exponent gets larger, numerical difficulties may occur.
$OMEGA 1 1 1 1 1 1 1 
$SIGMA  .4

$TABLE TIME A1 A2 A3 A4 NOAPPEND FILE=hillss.tab NOPRINT
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  68) THE INT, MOD, MIN, OR MAX FUNCTION IS BEING USED OUTSIDE OF A
 SIMULATION BLOCK. IF THE FUNCTION VALUE AFFECTS THE VALUE OF THE
 OBJECTIVE FUNCTION, THEN AN ERROR WILL PROBABLY OCCUR.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       19 MAY 2015
Days until program expires :5488
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3.0
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 EHC using Hill (sigmoid emax model) for the flags
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       37
 NO. OF DATA ITEMS IN DATA SET:  10
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   3   2   0   6   7   0   0   0   0   8
0LABELS FOR DATA ITEMS:
 ID DOSE TIME CP WT SS II ADDL EVID MDV
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 A1 A2 A3 A4
0FORMAT FOR DATA:
 (9E7.0,1F2.0)

 TOT. NO. OF OBS RECS:        1
 TOT. NO. OF INDIVIDUALS:      1
0LENGTH OF THETA:  10
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   7
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.1000E+01  0.1000E+01  0.1000E+01  0.1000E+02  0.1000E+01  0.1000E+01  0.1000E+01  0.3000E+01  0.5000E+01  0.5000E+02
0INITIAL ESTIMATE OF OMEGA:
 0.1000E+01
 0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+01
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
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 TIME A1 A2 A3 A4
1DOUBLE PRECISION PREDPP VERSION 7.3.0

 GENERAL NONLINEAR KINETICS MODEL (ADVAN6)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   6
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         DEPOT        OFF        YES        YES        YES        NO
    2         CENTRAL      ON         NO         YES        NO         YES
    3         PERIPH       ON         YES        YES        NO         NO
    4         GALL         ON         YES        YES        NO         NO
    5         OUTPUT       OFF        YES        NO         NO         NO
0GENERAL STEADY STATE (SS) ROUTINE IS BEING USED
0NRD VALUE FROM SUBROUTINE TOL:   6
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            7           *           *           *           *
    3            *           *           *           *           *
    4            *           *           *           *           *
    5            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      9
   TIME DATA ITEM IS DATA ITEM NO.:          3
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   2
   STEADY STATE DATA ITEM IS DATA ITEM NO.:  6
   INTERVAL DATA ITEM IS DATA ITEM NO.:      7
   ADDL. DOSES DATA ITEM IS DATA ITEM NO.:   8

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
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
 





 #OBJV:********************************************     3557.040       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      TH 9      TH10     
 
         1.00E+00  1.00E+00  1.00E+00  1.00E+01  1.00E+00  1.00E+00  1.00E+00  3.00E+00  5.00E+00  5.00E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7   
 
 ETA1
+        1.00E+00
 
 ETA2
+        0.00E+00  1.00E+00
 
 ETA3
+        0.00E+00  0.00E+00  1.00E+00
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.00E+00
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.00E+00
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.00E+00
 
 ETA7
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.00E+00
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        4.00E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7   
 
 ETA1
+        1.00E+00
 
 ETA2
+        0.00E+00  1.00E+00
 
 ETA3
+        0.00E+00  0.00E+00  1.00E+00
 
 ETA4
+        0.00E+00  0.00E+00  0.00E+00  1.00E+00
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.00E+00
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.00E+00
 
 ETA7
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.00E+00
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.32E-01
 
 #CPUT: Total CPU Time in Seconds,        0.265
Stop Time: 
Tue 05/19/2015 
04:33 PM

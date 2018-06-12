Fri 10/11/2013 
06:46 PM
$PROBLEM Dose Superposition ctlss
$INPUT ID TIME CMT AMT DV ADDL II SS
$DATA ssaddl_dat IGNORE=@
$SUBROUTINES ADVAN6 TOL=6 TRANS1 
$ABBR  DERIV2=NO
$ABBR DECLARE DOWHILE I
$ABBR DECLARE DOWHILE NDOSE

$MODEL COMP=ABS ;(DEFDOSE)
       COMP=CENTRAL;(DEFOBS)
       COMP=PERI
$abbr declare dosetime(1000),dose(1000),ipt(1000)
$PK 

CALLFL=-2
IF (ss == 1 ) THEN  ; SS dose record
IF (NEWIND < 2) NDOSE=1
 NDOSE=NDOSE+0
 DOWHILE (NDOSE<=10)  ; Insert 10 earlier doses
 dosetime(NDOSE)=TIME-II*(NDOSE-1)
 DOSE(NDOSE)=AMT
 NDOSE=NDOSE+1
 ENDDO
ENDIF

IF (dostim > 0) THEN  ; ADDL dose record
 dosetime(NDOSE)=dostim  ; dostim is the time of the addl dose
 DOSE(NDOSE)=AMT
 NDOSE=NDOSE+1
ENDIF

IF (dostim == 0 .and. amt > 0 .and. ss==0 ) THEN  ; transient non-ss dose
 DOSE(NDOSE)=AMT
 dosetime(NDOSE)=TIME  ; TIME is the time of the transient dose
 NDOSE=NDOSE+1
ENDIF
CL = THETA(1)*EXP(ETA(1))
V2 = THETA(2)*EXP(ETA(2))
Q  = THETA(3)
V3 = THETA(4)
KA = THETA(8)*EXP(ETA(6))

K=CL/V2
S2 =V2
K23=Q/V2
K32=Q/V3

;--------ABSORPTION MODEL-----------
F1=0
MTT  = THETA(5)*EXP(ETA(4))
NN   = THETA(6)*EXP(ETA(5))
BIO  = THETA(7)*EXP(ETA(3))
KTR  = (NN+1)/MTT

NFAC=SQRT(2*3.1416)*NN**(NN+0.5)*(EXP(-NN))*(1+1/(12*NN))
KINPT=BIO*KTR**(NN+1)/NFAC

$DES
INPT=0
I=1
DOWHILE (I<NDOSE)
IPT(i)=0
IF (T>=dosetime(I)) IPT(I)=DOSE(I)*(T-dosetime(I))**NN*EXP(-KTR*(T-dosetime(I)))
INPT=INPT+IPT(I)
I=I+1
ENDDO

 DADT(1)=KINPT*INPT-KTR*A(1)
 DADT(2)=KTR*A(1)-K23*A(2)-K*A(2)+K32*A(3)
 DADT(3)=K23*A(2)-K32*A(3)
$ERROR

Y = F*EXP(EPS(1))
a1=A(1)
a2=A(2)
a3=A(3)

$THETA 
4 ;CL
3 ;V2
1 ;Q
2 ;V3
10 ;MTT
5 ;NN
0.85 ;BIO
1 ; KA

$OMEGA 1 1 1 1 1 1
$SIGMA 1
$TABLE  TIME AMT INPT a1 a2 a3 FILE=ssaddl_tab
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  97) A RANDOM QUANTITY IS RAISED TO A POWER. IF THE RESULT AFFECTS
 THE VALUE OF THE OBJECTIVE FUNCTION, THE USER SHOULD ENSURE THAT THE
 RANDOM QUANTITY IS NEVER 0 WHEN THE POWER IS < 1.
             
 (WARNING  99) A RANDOM QUANTITY IS USED AS A POWER. IF THE RESULT AFFECTS
 THE VALUE OF THE OBJECTIVE FUNCTION, THE USER SHOULD ENSURE THAT THE
 QUANTITY RAISED TO THE POWER IS NOT 0.

 (DATA WARNING   4) RECORD         3, DATA ITEM   5, CONTENTS:  
 THE DV DATA ITEM IS NULL, BUT THE MDV DATA ITEM IS 0 AND NO $SIMULATION
 RECORD IS PRESENT.

 (DATA WARNING   4) RECORD         4, DATA ITEM   5, CONTENTS:  
 THE DV DATA ITEM IS NULL, BUT THE MDV DATA ITEM IS 0 AND NO $SIMULATION
 RECORD IS PRESENT.

 (DATA WARNING   4) RECORD         5, DATA ITEM   5, CONTENTS:  
 THE DV DATA ITEM IS NULL, BUT THE MDV DATA ITEM IS 0 AND NO $SIMULATION
 RECORD IS PRESENT.

 (DATA WARNING   4) RECORD         6, DATA ITEM   5, CONTENTS:  
 THE DV DATA ITEM IS NULL, BUT THE MDV DATA ITEM IS 0 AND NO $SIMULATION
 RECORD IS PRESENT.
             
 (WARNING  48) DES-DEFINED ITEMS ARE COMPUTED ONLY WHEN EVENT TIME
 INCREASES. E.G., DISPLAYED VALUES ASSOCIATED WITH THE FIRST EVENT RECORD
 OF AN INDIVIDUAL RECORD ARE COMPUTED WITH (THE LAST ADVANCE TO) AN EVENT
 TIME OF THE PRIOR INDIVIDUAL RECORD.
  
License Registered to: Temporary license for NONMEM7.3.0 installation
Expiration Date:    22 DEC 2013
Current Date:       11 OCT 2013
Days until program expires :  71
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3beta7.0(P) beta version 11
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 Dose Superposition ctlss
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:        6
 NO. OF DATA ITEMS IN DATA SET:  10
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   5
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   2   4   0   8   7   3   0   0   0   6
0LABELS FOR DATA ITEMS:
 ID TIME CMT AMT DV ADDL II SS EVID MDV
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 INPT a1 a2 a3
0FORMAT FOR DATA:
 (8E5.0,2F2.0)

 TOT. NO. OF OBS RECS:        4
 TOT. NO. OF INDIVIDUALS:      1
0LENGTH OF THETA:   8
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   6
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.4000E+01  0.3000E+01  0.1000E+01  0.2000E+01  0.1000E+02  0.5000E+01  0.8500E+00  0.1000E+01
0INITIAL ESTIMATE OF OMEGA:
 0.1000E+01
 0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+01
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+01
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE:            EIGENVALUE
0-- TABLE   1 --
 PRINTED:               YES
 FOR TABLE FILE,
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 TIME AMT INPT a1 a2 a3
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(P) beta version 11

 GENERAL NONLINEAR KINETICS MODEL (ADVAN6)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   6
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         ABS          ON         YES        YES        YES        NO
    2         CENTRAL      ON         YES        YES        NO         YES
    3         PERI         ON         YES        YES        NO         NO
    4         OUTPUT       OFF        YES        NO         NO         NO
0GENERAL STEADY STATE (SS) ROUTINE IS BEING USED
0NRD VALUE FROM SUBROUTINE TOL:   6
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           8           *           *           *
    2            7           *           *           *           *
    3            *           *           *           *           *
    4            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      9
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   STEADY STATE DATA ITEM IS DATA ITEM NO.:  8
   INTERVAL DATA ITEM IS DATA ITEM NO.:      7
   ADDL. DOSES DATA ITEM IS DATA ITEM NO.:   6
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    3

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE CALLED AT NONEVENT (ADDITIONAL AND LAGGED) DOSE TIMES.
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
 





 #OBJV:********************************************       26.873       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         4.00E+00  3.00E+00  1.00E+00  2.00E+00  1.00E+01  5.00E+00  8.50E-01  1.00E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
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
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
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



 LINE NO.TIME      AMT       INPT      a1        a2        a3        DV        PRED      RES       WRES     
 
    1
+        0.00E+00  1.00E+03  1.90E+05  1.29E+02  6.09E+01  4.07E+01  0.00E+00  2.03E+01  0.00E+00  0.00E+00
 
    2
+        5.00E+00  5.00E+02  2.09E+05  9.32E+01  4.14E+01  3.05E+01  0.00E+00  1.38E+01  0.00E+00  0.00E+00
 
    3
+        2.40E+01  0.00E+00  2.04E+05  1.43E+02  7.04E+01  5.03E+01  0.00E+00  2.35E+01 -2.35E+01 -3.40E-01
 
    4
+        2.50E+01  0.00E+00  1.66E+05  1.24E+02  6.35E+01  4.81E+01  0.00E+00  2.12E+01 -2.12E+01 -2.66E-01
 
    5
+        4.80E+01  0.00E+00  1.90E+05  1.29E+02  6.10E+01  4.07E+01  0.00E+00  2.03E+01 -2.03E+01 -3.77E-01
 
    6
+        5.30E+01  0.00E+00  2.09E+05  9.32E+01  4.14E+01  3.05E+01  0.00E+00  1.38E+01 -1.38E+01 -2.61E-01
 
 #CPUT: Total CPU Time in Seconds,        0.328
Stop Time: 
Fri 10/11/2013 
06:46 PM

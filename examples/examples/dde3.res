Wed 01/18/2017 
10:26 AM
; Pre-Control stream template dde3.dde used by ddexpand program to form functional NMTRAN control stream dde3.ctl
$PROB DDE Problem

; the data file should have only DOSE input records pertaining to base equations.  Also, the CMT must be a data item
; The ddexpand program, using finedata's EXTRADOSE facility, will add doses for additional compartments
; and call the new data file dde3_dde.csv
$INPUT ID TIME    AMT    RATE   CMT   EVID MDV  DV
$DATA dde3_dde.csv IGNORE=C
$SUBROUTINES ADVAN13 TRANS1 TOL=12
$MODEL NCOMPARTMENTS=12 ; number of compartments must be adjusted by user after ddexpand is executed.

$PK
CEVID=EVID
IF(CMT/=1) CEVID=1
K10=THETA(1)+ETA(1)
K12=THETA(2)+ETA(2)
K21=THETA(3)+ETA(3)
V1=THETA(4)+ETA(4)
K1=THETA(5)+ETA(5)
K2=THETA(6)+ETA(6)
K4=THETA(7)+ETA(7)
K5=THETA(8)+ETA(8)
SIG1=THETA(9)+ETA(9)
SIG2=THETA(10)+ETA(10)
SIG3=THETA(11)+ETA(11)
;  TAU1, TAU2, TAU3,etc. are time delays.  This sample has one time delay, TAU1
TAU1=THETA(12)+ETA(12)
I0=THETA(13)+ETA(13)
K3=5.0
AA=1.0
BB=0.5
; Set initial conditions for Base equations
A_0(1)=AA
A_0(2)=I0
A_0(6)=AA
A_0(7)=I0
;Any propagations of initial conditions and ALAG's will be placed here by ddexpand program.


; INITIALIZING EQUATIONS FOR DDE COMPARTMENTS
A_0(9)=AA
A_0(12)=AA
ALAG9=TAU1
ALAG10=TAU1
ALAG11=TAU1
ALAG12=TAU1

$DES
; AD_x_y is the State value of A(x) delayed for time TAUy.  These are used in the differntial equations later on.
; AP_x_y is the State value of A(x) in the past, for time delay TAUy.  That is, when T<Tauy, the AP_x_y defines A(x)
; For every AD_x_y used in the differential equations, there must be an AP_X_Y defined.
; If past is constant, then it can be as simple as AP_x_y=Initial condition constant (same value as A_0(x) is set to).
; Make sure AP_x_y is a function of T: do not use T-TAUy, as this will be done by the ddexpand program.

; DELAY EQUATIONS FOR TAU REPLICATE 0
 AP_1_1=AA*EXP(BB*(T-TAU1))
 AP_6_1=AA*EXP(BB*(T-TAU1))
 DTAU1=0.0
 IF(T>=TAU1) DTAU1=1.0
 AD_1_1=(1.0-DTAU1)*AP_1_1+DTAU1*A(9)
 AD_6_1=(1.0-DTAU1)*AP_6_1+DTAU1*A(12)

; BASE EQUATIONS ENTERED BY USER.  Note use of AD_1_1 and AD_6_1, which warrants an expansion.
 DADT(1)=K3-(K1/K2)*(1.0-EXP(-K2*T))*A(1)
 DADT(2)=K4*A(1)-K4*AD_1_1
 DADT(3)=K4*AD_1_1-K5*A(3)
 CC=A(4)/V1
 EFFECT=CC*(SIG1*EXP(-SIG2*CC)+SIG3)
 DADT(4)=-K10*A(4)-K12*A(4)+K21*A(5)
 DADT(5)=K12*A(4)-K21*A(5)
 DADT(6)=K3-EFFECT*A(6)-K1/K2*(1.0-EXP(-K2*T))*A(6)
 DADT(7)=K4*A(6)-K4*AD_6_1
 DADT(8)=K4*AD_6_1-K5*A(8)
;Any delay equations necessary are placed here by the ddexpand program.


; DELAY EQUATIONS FOR TAU REPLICATE 1

 DADT(9)=DTAU1*(K3-(K1/K2)*(1.0-EXP(-K2*(T-TAU1)))*A(9))
 CC1=DTAU1*(A(10)/V1)
 EFFECT1=DTAU1*(CC1*(SIG1*EXP(-SIG2*CC1)+SIG3))
 DADT(10)=DTAU1*(-K10*A(10)-K12*A(10)+K21*A(11))
 DADT(11)=DTAU1*(K12*A(10)-K21*A(11))
 DADT(12)=DTAU1*(K3-EFFECT1*A(12)-K1/K2*(1.0-EXP(-K2*(T-TAU1)))*A(12))

; FOR FINEDATA $EXTRADOSE: CMT=1:,9,4:,10,5:,11,6:,12

$ERROR
A1=A(1)
A2=A(2)
A3=A(3)
A4=A(4)
A5=A(5)
A6=A(6)
A7=A(7)
A8=A(8)
A9=A(9)
A10=A(10)
A11=A(11)
A12=A(12)

Y1=A(2)+A(3)
Y2=A(7)+A(8)
Y3=A(3)
Y4=A(8)
IF(CMT==1) IPRED=Y1
IF(CMT==2) IPRED=Y2
IF(CMT==3) IPRED=Y3
IF(CMT==4) IPRED=Y4
IF(CMT==1) Y=IPRED*(1.0+EPS(1))
IF(CMT==1) Y=IPRED*(1.0+EPS(2))
IF(CMT==2) Y=IPRED*(1.0+EPS(3))
IF(CMT==4) Y=IPRED*(1.0+EPS(4))

$THETA
0.32544   ; 1: K10
2.6496    ; 2: K12
2.5944    ; 3: K21
0.02645   ; 4: V
0.456     ; 5: K1
0.169     ; 6: K2
0.185     ; 7: K4
0.031     ; 8: K5
0.328     ; 9: SIG1
0.328     ; 10: SIG2
0.025     ; 11: SIG3
10.6      ; 12: TAU1
2.83      ; 13: I0

$OMEGA (0.0 FIXED)X13
$SIGMA (0.04)X4
$SIMULATION (567811 NORMAL) (2933012 UNIFORM) ONLYSIMULATION SUBPROBLEMS=1
$TABLE TIME Y1 Y2 Y3 Y4 EXCLUDE_BY CEVID NOAPPEND NOPRINT FILE=dde3.tab
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  3) THERE MAY BE AN ERROR IN THE ABBREVIATED CODE. THE FOLLOWING
 ONE OR MORE RANDOM VARIABLES ARE DEFINED WITH "IF" STATEMENTS THAT DO NOT
 PROVIDE DEFINITIONS FOR BOTH THE "THEN" AND "ELSE" CASES. IF ALL
 CONDITIONS FAIL, THE VALUES OF THESE VARIABLES WILL BE ZERO.
  
   IPRED Y

  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       18 JAN 2017
Days until program expires :4879
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha15 (nm74a15)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 DDE Problem
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      110
 NO. OF DATA ITEMS IN DATA SET:   8
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   8
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   2   3   4   0   0   5   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME AMT RATE CMT EVID MDV DV
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 CEVID Y1 Y2 Y3 Y4
0FORMAT FOR DATA:
 (8E7.0)

 TOT. NO. OF OBS RECS:      104
 TOT. NO. OF INDIVIDUALS:      1
0LENGTH OF THETA:  13
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:  13
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   4
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.3254E+00  0.2650E+01  0.2594E+01  0.2645E-01  0.4560E+00  0.1690E+00  0.1850E+00  0.3100E-01  0.3280E+00  0.3280E+00  0.2500E-01
   0.1060E+02  0.2830E+01
0INITIAL ESTIMATE OF OMEGA:
 0.0000E+00
 0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00
 0.0000E+00   0.0000E+00   0.0000E+00
0OMEGA CONSTRAINED TO BE THIS INITIAL ESTIMATE
0INITIAL ESTIMATE OF SIGMA:
 0.4000E-01
 0.0000E+00   0.4000E-01
 0.0000E+00   0.0000E+00   0.4000E-01
 0.0000E+00   0.0000E+00   0.0000E+00   0.4000E-01
0SIMULATION STEP OMITTED:    NO
 OBJ FUNC EVALUATED:         NO
 ORIGINAL DATA USED ON EACH NEW SIMULATION:         NO
 SEEDS RESET ON EACH NEW SUPERSET ITERATION:        YES
0SIMULATION RANDOM METHOD SELECTED (RANMETHOD): 4U
SEED   1 RESET TO INITIAL: YES
 SOURCE   1:
   SEED1:        567811   SEED2:             0   PSEUDO-NORMAL
SEED   2 RESET TO INITIAL: YES
 SOURCE   2:
   SEED1:       2933012   SEED2:             0   PSEUDO-UNIFORM
 NUMBER OF SUBPROBLEMS:    1
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:             3U
 MC SAMPLES (ESAMPLE):    300
 WRES SQUARE ROOT TYPE (WRESCHOL): EIGENVALUE
0-- TABLE   1 --
0RECORDS ONLY:    ALL
04 COLUMNS APPENDED:    NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
 FIXED_EFFECT_ETAS:
0USER-CHOSEN ITEMS:
 TIME Y1 Y2 Y3 Y4
0EXCLUDE-BY ITEMS:
 CEVID
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha15 (nm74a15)

 GENERAL NONLINEAR KINETICS MODEL WITH STIFF/NONSTIFF EQUATIONS (ADVAN13)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:  15
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         COMP 1       ON         YES        YES        YES        YES
    2         COMP 2       ON         YES        YES        NO         NO
    3         COMP 3       ON         YES        YES        NO         NO
    4         COMP 4       ON         YES        YES        NO         NO
    5         COMP 5       ON         YES        YES        NO         NO
    6         COMP 6       ON         YES        YES        NO         NO
    7         COMP 7       ON         YES        YES        NO         NO
    8         COMP 8       ON         YES        YES        NO         NO
    9         COMP 9       ON         YES        YES        NO         NO
   10         COMP 10      ON         YES        YES        NO         NO
   11         COMP 11      ON         YES        YES        NO         NO
   12         COMP 12      ON         YES        YES        NO         NO
   13         OUTPUT       OFF        YES        NO         NO         NO
 INITIAL (BASE) TOLERANCE SETTINGS:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:  12
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            *           *           *           *           *
    3            *           *           *           *           *
    4            *           *           *           *           *
    5            *           *           *           *           *
    6            *           *           *           *           *
    7            *           *           *           *           *
    8            *           *           *           *           *
    9            *           *           *           *          16
   10            *           *           *           *          17
   11            *           *           *           *          18
   12            *           *           *           *          19
   13            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      6
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   3
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     4
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    5

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0PK SUBROUTINE INDICATES THAT COMPARTMENT AMOUNTS ARE INITIALIZED.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
0ERROR SUBROUTINE INDICATES THAT DERIVATIVES OF COMPARTMENT AMOUNTS ARE USED.
0DES SUBROUTINE USES COMPACT STORAGE MODE.
 TOLERANCES FOR SIMULATION STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:  12
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 TOLERANCES FOR TABLE/SCATTER STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:  12
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
1
 PROBLEM NO.:           1      SUBPROBLEM NO.:           1

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:    1877106494   SEED2:    1067448803
 SOURCE  2:
    SEED1:       2933012   SEED2:             0
 Elapsed simulation  time in seconds:     0.02
 ESTIMATION STEP OMITTED:                 YES
 Elapsed finaloutput time in seconds:     0.01
 #CPUT: Total CPU Time in Seconds,        0.078
Stop Time: 
Wed 01/18/2017 
10:26 AM

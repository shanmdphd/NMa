Thu Jan 22 08:56:41 PST 2015
$PROBLEM  Parent drug and metabolite, using ADVAN5
$INPUT ID TIME AMT UVOL DV CMT MDV EVID L2
$DATA dextrometab.dat IGNORE=#
$SUBROUTINES ADVAN5 TRANS1 
$MODEL
  COMP=(DEPOT)
  COMP=(PLASMA DEFOBS) ; PARENT IN PLASMA
  COMP=(DEXURIN INITIALOFF NODOSE)  ;PARENT IN URINE
  COMP=METAB    ;METABOLITE IN PLASMA
$PK
  K12=THETA(1)
  MU_1=LOG(THETA(2))
  V2=EXP(MU_1+ETA(1))
  MU_2=LOG(THETA(3))
  CLP=EXP(MU_2+ETA(2)) ; RENAL CL FOR PARENT
  CLB=THETA(4)         ; METABOLIC CL FOR METABOLIC
  CLMR=THETA(5)        ; RENAL CL FOR METABOLITE
  V4=1
  K24=CLB/V2
  K23=CLP/V2
  ; F0=CLP/(CLP+CLB) Omit F0 because parent and metab have different urine compts.
  K45=CLMR/V4
  S2=V2
  S4=V4
  S3=UVOL
  S5=UVOL
$ERROR (EVERY EVENT)
  ACMT=ABS(CMT) ; output compartment may have negative value of CMT
  IF(ACMT.EQ.2) Y=F*(1+EPS(1))
  IF(ACMT.EQ.3) Y=F*(1+EPS(2))
  IF(ACMT.EQ.4) Y=F*(1+EPS(3))
  IF(ACMT.EQ.5) Y=F*(1+EPS(4))
$THETA
  (0.01,0.8,6) ;KA
  (0.01,43,1000);V2
  (0.0001,20,190);CLP
  (0.01,15,90);CLB
  (0.0001,5,90);CLMR
$OMEGA
  0.05  0.05
$SIGMA 
  .01 .01 .01 .01
$SIM (111111) ONLYSIM
$TABLE ID TIME AMT UVOL DV SIMP=PRED CMT MDV EVID L2 NOAPPEND FILE=sim_metab_a5.tab

NM-TRAN MESSAGES
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  3) THERE MAY BE AN ERROR IN THE ABBREVIATED CODE. THE FOLLOWING
 ONE OR MORE RANDOM VARIABLES ARE DEFINED WITH "IF" STATEMENTS THAT DO NOT
 PROVIDE DEFINITIONS FOR BOTH THE "THEN" AND "ELSE" CASES. IF ALL
 CONDITIONS FAIL, THE VALUES OF THESE VARIABLES WILL BE ZERO.
  
   Y


License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       22 JAN 2015
Days until program expires :5605
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha4 (nm74a4)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 Parent drug and metabolite, using ADVAN5
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:       13
 NO. OF DATA ITEMS IN DATA SET:   9
 ID DATA ITEM IS DATA ITEM NO.:   1
 L2 DATA ITEM IS DATA ITEM NO.:   9
 DEP VARIABLE IS DATA ITEM NO.:   5
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   8   2   3   0   0   0   6   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME AMT UVOL DV CMT MDV EVID L2
0LABELS FOR SPECIAL ITEMS:
 SIMP RES WRES
0FORMAT FOR DATA:
 (2E6.0,E10.0,E8.0,E6.0,E7.0,2E6.0,E2.0)

 TOT. NO. OF OBS RECS:        8
 TOT. NO. OF INDIVIDUALS:      1
0LENGTH OF THETA:   5
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   4
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-01     0.8000E+00     0.6000E+01
  0.1000E-01     0.4300E+02     0.1000E+04
  0.1000E-03     0.2000E+02     0.1900E+03
  0.1000E-01     0.1500E+02     0.9000E+02
  0.1000E-03     0.5000E+01     0.9000E+02
0INITIAL ESTIMATE OF OMEGA:
 0.5000E-01
 0.0000E+00   0.5000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E-01
 0.0000E+00   0.1000E-01
 0.0000E+00   0.0000E+00   0.1000E-01
 0.0000E+00   0.0000E+00   0.0000E+00   0.1000E-01
0SIMULATION STEP OMITTED:    NO
 OBJ FUNC EVALUATED:         NO
0SIMULATION RANDOM METHOD SELECTED (RANMETHOD): 4U
 SOURCE   1:
   SEED1:        111111   SEED2:             0   PSEUDO-NORMAL
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:             3U
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE (WRESCHOL): EIGENVALUE
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
 ID TIME AMT UVOL DV SIMP CMT MDV EVID L2
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha4 (nm74a4)

 GENERAL LINEAR KINETICS MODEL (ADVAN5)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   4
0RATE CONSTANT PARAMETERS - ASSIGNMENT OF ROWS IN GG
            TO COMPT.
  FROM      1    2    3    4    5
  COMPT.
    1       *    1    -    -    -
    2       -    *    3    2    -
    3       -    -    *    -    -
    4       -    -    -    *    4
             * LINK FROM A COMPARTMENT TO ITSELF IS NOT POSSIBLE
             - LINK BETWEEN THESE COMPARTMENTS IS NOT DEFINED FOR THIS MODEL
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         DEPOT        ON         YES        YES        YES        NO
    2         PLASMA       ON         YES        YES        NO         YES
    3         DEXURIN      OFF        YES        NO         NO         NO
    4         METAB        ON         YES        YES        NO         NO
    5         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            5           *           *           *           *
    3            7           -           -           -           -
    4            6           *           *           *           *
    5            8           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      8
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   3
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    6

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:    1422409161   SEED2:             0
 Elapsed simulation  time in seconds:     0.01
 ESTIMATION STEP OMITTED:                 YES
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                          TABLES OF DATA AND PREDICTIONS                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1TABLE NO.  1



 LINE NO.ID        TIME      AMT       UVOL      DV        SIMP      CMT       MDV       EVID      L2       
 
    1
+        2.00E+00  0.00E+00  3.00E+04  0.00E+00  0.00E+00  0.00E+00  1.00E+00  1.00E+00  1.00E+00  1.00E+00
 
    2
+        2.00E+00  1.71E-01  0.00E+00  0.00E+00  0.00E+00  8.31E+01  3.00E+00  1.00E+00  2.00E+00  1.00E+00
 
    3
+        2.00E+00  1.71E-01  0.00E+00  0.00E+00  0.00E+00  8.31E+01  5.00E+00  1.00E+00  2.00E+00  1.00E+00
 
    4
+        2.00E+00  2.00E+00  0.00E+00  0.00E+00  2.43E+02  2.22E+02  2.00E+00  0.00E+00  0.00E+00  1.00E+00
 
    5
+        2.00E+00  2.00E+00  0.00E+00  0.00E+00  5.80E+02  7.00E+02  4.00E+00  0.00E+00  0.00E+00  1.00E+00
 
    6
+        2.00E+00  2.00E+00  0.00E+00  9.31E+01  9.61E+01  8.67E+01 -3.00E+00  0.00E+00  0.00E+00  1.00E+00
 
    7
+        2.00E+00  2.00E+00  0.00E+00  9.31E+01  5.63E+01  5.84E+01 -5.00E+00  0.00E+00  0.00E+00  1.00E+00
 
    8
+        2.00E+00  2.00E+00  0.00E+00  0.00E+00  0.00E+00  2.22E+02  3.00E+00  1.00E+00  2.00E+00  2.00E+00
 
    9
+        2.00E+00  2.00E+00  0.00E+00  0.00E+00  0.00E+00  2.22E+02  5.00E+00  1.00E+00  2.00E+00  2.00E+00
 
   10
+        2.00E+00  3.15E+00  0.00E+00  0.00E+00  1.68E+02  1.38E+02  2.00E+00  0.00E+00  0.00E+00  2.00E+00
 
   11
+        2.00E+00  3.15E+00  0.00E+00  0.00E+00  4.12E+02  4.58E+02  4.00E+00  0.00E+00  0.00E+00  2.00E+00
 
   12
+        2.00E+00  3.15E+00  0.00E+00  1.34E+02  2.75E+01  3.10E+01 -3.00E+00  0.00E+00  0.00E+00  2.00E+00
 
   13
+        2.00E+00  3.15E+00  0.00E+00  1.34E+02  2.32E+01  2.51E+01 -5.00E+00  0.00E+00  0.00E+00  2.00E+00
 
 #CPUT: Total CPU Time in Seconds,        0.069
Stop Time:
Thu Jan 22 08:56:45 PST 2015

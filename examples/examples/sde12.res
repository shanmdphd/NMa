Mon 09/30/2013 
02:51 PM
$PROBLEM PK ODE HANDS ON ONE

$INPUT ID TIME DV AMT CMT FLAG MDV SDE

$DATA   sde9.csv
        IGNORE=@

$SUBROUTINE ADVAN6 TOL=6 OTHER=sde.f90
$ABBR DECLARE SGW(3)

$MODEL 
       COMP = (CENTRAL);
       COMP = (DFDX1)
       COMP = (DPDT11)

$PK
  IF(NEWIND.NE.2) OT = 0
   
  MU_1  = THETA(1)
  CL    = EXP(MU_1+ETA(1)) 
  MU_2  = THETA(2)
  VD    = EXP(MU_2+ETA(2))
  SGW1 = THETA(4)


$DES
 FIRSTEM=1
 DADT(1) = - CL/VD*A(1)
; NEXT DERIVATIVES ARE ACUALLY PREDICTIVE VALUES FOR COMPARTMENTS 1 AND 2, RESPECTIVELY
;  Derivatives of these with respect to A() will be calcualted symbolically by DES routine created by NMTRAN
 DADT(2) = A(1)/VD
; DUMMY PLACEMENT FOR DERIVATIVES OF THE STOCHASTIC ERROR SYSTEM.  THESE ARE FILLED OUT BY SDE_DER
 SGW(1)=SGW1
;  the DA() array THEN contains all derivatives of DADT (=DXDT) with respect to A(=X).
; Number of compartments=1, number of base model derivative equations=1
"LAST
"      CALL SDE_DER(DADT,A,DA,IR,SGW,1.0d+00,1.0d+00)
 
$ERROR (OBS ONLY)
  
     IPRED = A(1)/VD
     IRES  = DV - IPRED
     W     = THETA(3)
     IWRES = IRES/W
     WS=1000.0
; CENTRAL COMPARTMENT, PLASMA LEVELS
; EPS(1) = USER MODEL ERROR CONTRIBUTION
; EPS(2) = STOCHASTIC ERROR CONTRIBUTION.  THE WS IS JUST A PLACEHOLDER COEFFICIENT.  SDE_CADD WILL REPLACE THIS
; WITH THE CORRECT VALUE
     Y     = IPRED+W*EPS(1) + WS*EPS(2)
; SDE_CADD WILL EVALUATE THE TRUE COEFFICIENTS (WS) TO THE STOCHASTIC COMPONENTS.
; Number of compartments=1, number of base model derivative equations=1
"LAST
"       CALL SDE_CADD(A,HH,TIME,DV,CMT,1.0D+00,1.0D+00,SDE)



$THETA (0,2.3)               ;1 CL
$THETA (0,3.5)               ;2 VD
$THETA (0, 2)               ;4 SIGMA
$THETA (0,1) ; SGW1

$OMEGA 0.1                  ;1 CL
$OMEGA 0.01                 ;2 VD

$SIGMA (1 FIX) (1 FIX)               ; PK

$EST METHOD=ITS INTERACTION NOABORT PRINT=1 CTYPE=3 OPTMAP=1 ETADER=2 SIGLO=6 SIGL=6 MCETA=1
$EST METHOD=IMP INTERACTION NOABORT PRINT=1 IACCEPT=1.0 CTYPE=3 OPTMAP=0 ETADER=0 SIGLO=6 SIGL=6 MCETA=1 MAPITER=0
$EST MAXEVAL=9999 METHOD=1 INTER NOABORT NSIG=1 PRINT=1 MSFO=sde12.msf OPTMAP=1 ETADER=2 SIGLO=6 SIGL=6 MCETA=1 SLOW
$COV MATRIX=R UNCONDITIONAL TOL=9 SIGL=8 SIGLO=8

$TABLE ID TIME FLAG AMT CMT IPRED IRES IWRES
       ONEHEADER NOPRINT FILE=sde9.fit
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  45) $DES: VALUES HAVE NOT BEEN ASSIGNED TO ALL DADT VARIABLES.
 UNUSED COMPARTMENTS SHOULD BE OFF.
  
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
 PK ODE HANDS ON ONE
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      570
 NO. OF DATA ITEMS IN DATA SET:   9
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   9   2   4   0   0   0   5   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME DV AMT CMT FLAG MDV SDE EVID
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 IPRED IRES IWRES
0FORMAT FOR DATA:
 (8E9.0,1F2.0)

 TOT. NO. OF OBS RECS:      540
 TOT. NO. OF INDIVIDUALS:     30
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.0000E+00     0.2300E+01     0.1000E+07
  0.0000E+00     0.3500E+01     0.1000E+07
  0.0000E+00     0.2000E+01     0.1000E+07
  0.0000E+00     0.1000E+01     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.1000E+00
 0.0000E+00   0.1000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+01
 0.0000E+00   0.1000E+01
0SIGMA CONSTRAINED TO BE THIS INITIAL ESTIMATE
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 SIGDIGITS ETAHAT (SIGLO):                  8
 SIGDIGITS GRADIENTS (SIGL):                8
 RELATIVE TOLERANCE (TOL):                  9
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
 ID TIME FLAG AMT CMT IPRED IRES IWRES
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(P)

 GENERAL NONLINEAR KINETICS MODEL (ADVAN6)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   3
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         YES        YES        YES        YES
    2         DFDX1        ON         YES        YES        NO         NO
    3         DPDT11       ON         YES        YES        NO         NO
    4         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE FROM SUBROUTINE TOL:   6
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            *           *           *           *           *
    3            *           *           *           *           *
    4            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      9
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    5

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONLY WITH OBSERVATION EVENTS.
0ERROR SUBROUTINE INDICATES THAT DERIVATIVES OF COMPARTMENT AMOUNTS ARE USED.
0DES SUBROUTINE USES COMPACT STORAGE MODE.
1
 
 
 #TBLN:      1
 #METH: Iterative Two Stage
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            440
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  YES 
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 1           
 ETA HESSIAN EVALUATION METHOD (ETADER):  2           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  1           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    6           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   6           
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
 EM OR BAYESIAN METHOD USED:              ITERATIVE TWO STAGE (ITS)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      50          
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   1537.77552535606
 iteration            1 OBJ=   1381.56767802497
 iteration            2 OBJ=   1328.01940405227
 iteration            3 OBJ=   1281.38301631570
 iteration            4 OBJ=   1244.21238011065
 iteration            5 OBJ=   1221.24031103944
 iteration            6 OBJ=   1212.84194015235
 iteration            7 OBJ=   1210.98514493465
 iteration            8 OBJ=   1210.80808673641
 iteration            9 OBJ=   1210.77853763257
 iteration           10 OBJ=   1210.77466767154
 iteration           11 OBJ=   1210.77303708254
 iteration           12 OBJ=   1210.77306983328
 iteration           13 OBJ=   1210.77357017795
 iteration           14 OBJ=   1210.77498196748
 iteration           15 OBJ=   1210.77237340531
 iteration           16 OBJ=   1210.77208304950
 iteration           17 OBJ=   1210.77258882101
 iteration           18 OBJ=   1210.77227861535
 iteration           19 OBJ=   1210.77263216461
 iteration           20 OBJ=   1210.77249629889
 iteration           21 OBJ=   1210.77375344663
 Convergence achieved
 iteration           21 OBJ=   1210.77476497493
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -1.0344E-04 -9.0475E-05
 SE:             4.4523E-02  6.1727E-02
 N:                      30          30
 
 P VAL.:         9.9815E-01  9.9883E-01
 
 ETAshrink(%):   1.0512E+01  1.6503E+00
 EBVshrink(%):   1.0562E+01  1.6662E+00
 EPSshrink(%):   4.7877E+00  4.7877E+00
 
 #TERE:
 Elapsed estimation time in seconds:    26.76
 Elapsed covariance time in seconds:     0.83
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1210.775       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.40E+00  3.48E+00  9.10E-01  5.31E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        7.68E-02
 
 ETA2
+        0.00E+00  1.22E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1      EPS2   
 
 EPS1
+        1.00E+00
 
 EPS2
+        0.00E+00  1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.77E-01
 
 ETA2
+        0.00E+00  3.50E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1      EPS2   
 
 EPS1
+        1.00E+00
 
 EPS2
+        0.00E+00  1.00E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         6.47E-02  8.45E-02  8.59E-02  4.28E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        3.55E-02
 
 ETA2
+        0.00E+00  4.11E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1      EPS2   
 
 EPS1
+        0.00E+00
 
 EPS2
+        0.00E+00  0.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        6.41E-02
 
 ETA2
+       .........  5.87E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1      EPS2   
 
 EPS1
+       .........
 
 EPS2
+       ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11      SG12      SG22  
 
 TH 1
+        4.18E-03
 
 TH 2
+        9.00E-04  7.14E-03
 
 TH 3
+       -1.36E-03  1.73E-03  7.39E-03
 
 TH 4
+        8.06E-02 -6.77E-02 -1.95E-01  1.83E+01
 
 OM11
+       -8.09E-04 -8.16E-04  5.74E-04 -2.65E-02  1.26E-03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -7.27E-05 -1.65E-03  5.55E-05  2.55E-02  1.14E-04  0.00E+00  1.69E-03
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11      SG12      SG22  
 
 TH 1
+        6.47E-02
 
 TH 2
+        1.65E-01  8.45E-02
 
 TH 3
+       -2.45E-01  2.38E-01  8.59E-02
 
 TH 4
+        2.91E-01 -1.87E-01 -5.29E-01  4.28E+00
 
 OM11
+       -3.52E-01 -2.72E-01  1.88E-01 -1.75E-01  3.55E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.74E-02 -4.77E-01  1.57E-02  1.45E-01  7.85E-02  0.00E+00  4.11E-02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11      SG12      SG22  
 
 TH 1
+        3.02E+02
 
 TH 2
+       -4.61E+01  2.29E+02
 
 TH 3
+        3.20E+01 -6.86E+01  2.14E+02
 
 TH 4
+       -9.30E-01  2.22E-01  1.91E+00  8.22E-02
 
 OM11
+        1.32E+02  1.35E+02 -7.21E+01  5.13E-01  1.00E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.82E+01  2.13E+02 -9.70E+01 -1.17E+00  6.50E+01  0.00E+00  8.17E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 
 
 #TBLN:      2
 #METH: Importance Sampling
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            440
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  YES 
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  1           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    6           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   6           
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
 EM OR BAYESIAN METHOD USED:              IMPORTANCE SAMPLING (IMP)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      50          
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        300         
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-01   ,10.00000000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        1.000000000000000       
 T-DIST. PROPOSAL DENSITY (DF):           0           
 NO. ITERATIONS FOR MAP (MAPITER):        0           
 INTERVAL ITER. FOR MAP (MAPINTER):       0           
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   1210.69527597229 eff.=     301. Smpl.=     300. Fit.= 0.98185
 iteration            1 OBJ=   1210.83677119450 eff.=     301. Smpl.=     300. Fit.= 0.95992
 iteration            2 OBJ=   1210.70581046700 eff.=     303. Smpl.=     300. Fit.= 0.96112
 iteration            3 OBJ=   1210.84287187062 eff.=     304. Smpl.=     300. Fit.= 0.96114
 iteration            4 OBJ=   1210.60202344702 eff.=     303. Smpl.=     300. Fit.= 0.95941
 iteration            5 OBJ=   1210.57675868065 eff.=     295. Smpl.=     300. Fit.= 0.96104
 iteration            6 OBJ=   1210.70015710088 eff.=     299. Smpl.=     300. Fit.= 0.96158
 iteration            7 OBJ=   1210.76473495658 eff.=     306. Smpl.=     300. Fit.= 0.96085
 iteration            8 OBJ=   1210.84827221976 eff.=     304. Smpl.=     300. Fit.= 0.95895
 iteration            9 OBJ=   1210.69442766879 eff.=     307. Smpl.=     300. Fit.= 0.95671
 iteration           10 OBJ=   1210.79041309812 eff.=     293. Smpl.=     300. Fit.= 0.95994
 iteration           11 OBJ=   1210.76471344992 eff.=     305. Smpl.=     300. Fit.= 0.95778
 iteration           12 OBJ=   1210.81961219836 eff.=     300. Smpl.=     300. Fit.= 0.95773
 iteration           13 OBJ=   1210.75047112470 eff.=     302. Smpl.=     300. Fit.= 0.96075
 iteration           14 OBJ=   1210.53441877700 eff.=     297. Smpl.=     300. Fit.= 0.96066
 Convergence achieved
 iteration           14 OBJ=   1210.65161792300 eff.=     307. Smpl.=     300. Fit.= 0.95906
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         9.2001E-04  1.4583E-03
 SE:             4.5230E-02  6.2053E-02
 N:                      30          30
 
 P VAL.:         9.8377E-01  9.8125E-01
 
 ETAshrink(%):   1.0455E+01  1.4797E+00
 EBVshrink(%):   1.0247E+01  1.6728E+00
 EPSshrink(%):   5.0417E+00  5.0417E+00
 
 #TERE:
 Elapsed estimation time in seconds:    65.96
 Elapsed covariance time in seconds:    32.37
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1210.652       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.38E+00  3.48E+00  9.08E-01  5.31E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        7.92E-02
 
 ETA2
+        0.00E+00  1.23E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1      EPS2   
 
 EPS1
+        1.00E+00
 
 EPS2
+        0.00E+00  1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.81E-01
 
 ETA2
+        0.00E+00  3.51E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1      EPS2   
 
 EPS1
+        1.00E+00
 
 EPS2
+        0.00E+00  1.00E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         5.75E-02  6.52E-02  7.88E-02  3.94E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        2.64E-02
 
 ETA2
+        0.00E+00  3.59E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1      EPS2   
 
 EPS1
+        0.00E+00
 
 EPS2
+        0.00E+00  0.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        4.69E-02
 
 ETA2
+       .........  5.12E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1      EPS2   
 
 EPS1
+       .........
 
 EPS2
+       ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11      SG12      SG22  
 
 TH 1
+        3.30E-03
 
 TH 2
+       -7.22E-05  4.25E-03
 
 TH 3
+       -1.98E-04  1.71E-05  6.21E-03
 
 TH 4
+        1.19E-02  6.05E-03 -2.06E-01  1.55E+01
 
 OM11
+       -1.09E-04 -2.49E-05  1.90E-04 -1.46E-02  6.97E-04
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.28E-05 -5.39E-05  1.34E-04 -8.67E-03  1.02E-04  0.00E+00  1.29E-03
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11      SG12      SG22  
 
 TH 1
+        5.75E-02
 
 TH 2
+       -1.93E-02  6.52E-02
 
 TH 3
+       -4.38E-02  3.33E-03  7.88E-02
 
 TH 4
+        5.25E-02  2.35E-02 -6.62E-01  3.94E+00
 
 OM11
+       -7.17E-02 -1.44E-02  9.14E-02 -1.40E-01  2.64E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.10E-02 -2.30E-02  4.74E-02 -6.13E-02  1.07E-01  0.00E+00  3.59E-02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11      SG12      SG22  
 
 TH 1
+        3.05E+02
 
 TH 2
+        5.65E+00  2.36E+02
 
 TH 3
+        3.44E+00 -6.54E+00  2.87E+02
 
 TH 4
+       -1.49E-01 -1.72E-01  3.80E+00  1.16E-01
 
 OM11
+        4.37E+01  6.16E+00  2.08E+00  1.33E+00  1.48E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+        8.27E-01  8.99E+00 -4.67E+00  2.72E-01 -1.07E+02  0.00E+00  7.86E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 
 
 #TBLN:      3
 #METH: First Order Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            9999
 NO. OF SIG. FIGURES REQUIRED:            1
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  YES 
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 1           
 ETA HESSIAN EVALUATION METHOD (ETADER):  2           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  1           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    6           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   6           
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   1210.80034267136        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  2.3820E+00  3.4768E+00  9.0849E-01  5.3119E+01  7.9180E-02  1.2312E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:  -4.0841E+00 -1.0109E+01  4.5596E-01 -7.4188E-01  3.9418E+00  3.9077E+00
 
0ITERATION NO.:    1    OBJECTIVE VALUE:   1210.78587883612        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:       18
 NPARAMETR:  2.3849E+00  3.4870E+00  9.0837E-01  5.3131E+01  7.9001E-02  1.2284E-01
 PARAMETER:  1.0118E-01  1.0291E-01  9.9869E-02  1.0021E-01  9.8863E-02  9.8873E-02
 GRADIENT:   8.0183E+00  6.1690E+00 -9.6784E-01 -9.9228E-01  2.1616E+00  1.8378E+00
 
0ITERATION NO.:    2    OBJECTIVE VALUE:   1210.78579524419        NO. OF FUNC. EVALS.:  12
 CUMULATIVE NO. OF FUNC. EVALS.:       30
 NPARAMETR:  2.3839E+00  3.4860E+00  9.0841E-01  5.3133E+01  7.8984E-02  1.2282E-01
 PARAMETER:  1.0080E-01  1.0262E-01  9.9914E-02  1.0026E-01  9.8761E-02  9.8786E-02
 GRADIENT:  -5.2516E-01  8.6438E+00  3.8841E+00  2.9570E+00  7.9507E+00  7.0112E+00
 
0ITERATION NO.:    3    OBJECTIVE VALUE:   1210.78579524419        NO. OF FUNC. EVALS.:  20
 CUMULATIVE NO. OF FUNC. EVALS.:       50
 NPARAMETR:  2.3839E+00  3.4860E+00  9.0841E-01  5.3133E+01  7.8984E-02  1.2282E-01
 PARAMETER:  1.0080E-01  1.0262E-01  9.9914E-02  1.0026E-01  9.8761E-02  9.8786E-02
 GRADIENT:  -8.3254E+00  6.8172E+00 -1.1156E+00 -6.1707E-01  1.7936E+00  2.1935E+00
 
0ITERATION NO.:    4    OBJECTIVE VALUE:   1210.77454072738        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:       67
 NPARAMETR:  2.3865E+00  3.4802E+00  9.0949E-01  5.3180E+01  7.9058E-02  1.2288E-01
 PARAMETER:  1.0186E-01  1.0097E-01  1.0110E-01  1.0114E-01  9.9224E-02  9.9019E-02
 GRADIENT:  -4.7074E+00 -2.3733E+00 -1.4392E-01  2.2511E-01  1.8246E+00  2.2225E+00
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   1210.77068174879        NO. OF FUNC. EVALS.:  17
 CUMULATIVE NO. OF FUNC. EVALS.:       84
 NPARAMETR:  2.3890E+00  3.4822E+00  9.1038E-01  5.3214E+01  7.9082E-02  1.2284E-01
 PARAMETER:  1.0290E-01  1.0154E-01  1.0208E-01  1.0179E-01  9.9378E-02  9.8889E-02
 GRADIENT:  -1.5360E+00  8.8394E-01  5.1239E-01  6.8233E-01  1.6109E+00  2.1823E+00
 
0ITERATION NO.:    6    OBJECTIVE VALUE:   1210.76952244207        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      100
 NPARAMETR:  2.3876E+00  3.4823E+00  9.1196E-01  5.3253E+01  7.8980E-02  1.2226E-01
 PARAMETER:  1.0234E-01  1.0156E-01  1.0381E-01  1.0252E-01  9.8736E-02  9.6492E-02
 GRADIENT:  -3.5455E+00  9.8461E-01  1.6599E+00  1.8861E+00  1.6256E+00  1.8837E+00
 
0ITERATION NO.:    7    OBJECTIVE VALUE:   1210.76950004646        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      115
 NPARAMETR:  2.3878E+00  3.4823E+00  9.1335E-01  5.3153E+01  7.9273E-02  1.2177E-01
 PARAMETER:  1.0239E-01  1.0157E-01  1.0534E-01  1.0063E-01  1.0059E-01  9.4512E-02
 GRADIENT:  -2.9225E+00  1.0133E+00  1.4127E+00  9.9778E-01  2.0072E+00  1.7487E+00
 
0ITERATION NO.:    8    OBJECTIVE VALUE:   1210.74824497627        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:      124            RESET HESSIAN, TYPE II
 NPARAMETR:  2.3878E+00  3.4823E+00  9.1315E-01  5.2717E+01  7.6693E-02  1.1831E-01
 PARAMETER:  1.0240E-01  1.0157E-01  1.0512E-01  9.2403E-02  8.4043E-02  8.0101E-02
 GRADIENT:   1.2492E+01  9.7129E+00  5.0004E+00  2.7760E+00  8.9584E+00  7.8607E+00
 
0ITERATION NO.:    9    OBJECTIVE VALUE:   1210.74824497627        NO. OF FUNC. EVALS.:  20
 CUMULATIVE NO. OF FUNC. EVALS.:      144
 NPARAMETR:  2.3878E+00  3.4823E+00  9.1315E-01  5.2717E+01  7.6693E-02  1.1831E-01
 PARAMETER:  1.0240E-01  1.0157E-01  1.0512E-01  9.2403E-02  8.4043E-02  8.0101E-02
 GRADIENT:  -2.9890E+00  1.7917E+00 -1.2954E+00 -4.2212E+00  4.1092E-01  1.1315E-01
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   1210.73397411259        NO. OF FUNC. EVALS.:  16
 CUMULATIVE NO. OF FUNC. EVALS.:      160
 NPARAMETR:  2.3878E+00  3.4823E+00  9.1319E-01  5.3084E+01  7.6590E-02  1.1827E-01
 PARAMETER:  1.0240E-01  1.0157E-01  1.0516E-01  9.9333E-02  8.3368E-02  7.9915E-02
 GRADIENT:  -3.5947E+00  1.0437E+00  1.0393E+00 -2.5834E-01  7.1597E-01  1.6189E-01
 
0ITERATION NO.:   11    OBJECTIVE VALUE:   1210.73077331646        NO. OF FUNC. EVALS.:  15
 CUMULATIVE NO. OF FUNC. EVALS.:      175
 NPARAMETR:  2.3878E+00  3.4823E+00  9.1295E-01  5.3130E+01  7.5261E-02  1.1781E-01
 PARAMETER:  1.0242E-01  1.0156E-01  1.0490E-01  1.0019E-01  7.4618E-02  7.7948E-02
 GRADIENT:  -3.8799E+00  1.1194E+00  1.3223E+00  3.7242E-01  7.7549E-02 -7.2103E-02
 
0ITERATION NO.:   12    OBJECTIVE VALUE:   1210.73060609892        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      189
 NPARAMETR:  2.3880E+00  3.4822E+00  9.1200E-01  5.3164E+01  7.5045E-02  1.1880E-01
 PARAMETER:  1.0249E-01  1.0155E-01  1.0386E-01  1.0084E-01  7.3180E-02  8.2131E-02
 GRADIENT:  -3.8231E+00  7.8264E-01  1.0456E+00  2.3330E-01 -1.4240E-01  3.8208E-01
 
0ITERATION NO.:   13    OBJECTIVE VALUE:   1210.72886544998        NO. OF FUNC. EVALS.:  14
 CUMULATIVE NO. OF FUNC. EVALS.:      203
 NPARAMETR:  2.3886E+00  3.4820E+00  9.0837E-01  5.3282E+01  7.5163E-02  1.1839E-01
 PARAMETER:  1.0276E-01  1.0149E-01  9.9874E-02  1.0305E-01  7.3963E-02  8.0430E-02
 GRADIENT:  -2.8948E+00  6.1957E-01 -9.6237E-03  2.7027E-01  2.1291E-01  3.5373E-01
 
0ITERATION NO.:   14    OBJECTIVE VALUE:   1210.72869159610        NO. OF FUNC. EVALS.:  13
 CUMULATIVE NO. OF FUNC. EVALS.:      216            RESET HESSIAN, TYPE II
 NPARAMETR:  2.3886E+00  3.4820E+00  9.0837E-01  5.3282E+01  7.5097E-02  1.1839E-01
 PARAMETER:  1.0276E-01  1.0149E-01  9.9874E-02  1.0305E-01  7.3524E-02  8.0429E-02
 GRADIENT:   3.6157E+00  1.7713E+00  3.3016E+00 -8.8682E-03  3.1493E+00  5.4643E+00
 
0ITERATION NO.:   15    OBJECTIVE VALUE:   1210.72869159610        NO. OF FUNC. EVALS.:  20
 CUMULATIVE NO. OF FUNC. EVALS.:      236
 NPARAMETR:  2.3886E+00  3.4820E+00  9.0837E-01  5.3282E+01  7.5097E-02  1.1839E-01
 PARAMETER:  1.0276E-01  1.0149E-01  9.9874E-02  1.0305E-01  7.3524E-02  8.0429E-02
 GRADIENT:  -2.7717E+00  7.2242E-01 -4.2106E-02  5.0374E-01  2.0995E-01  3.9089E-01
 
0ITERATION NO.:   16    OBJECTIVE VALUE:   1210.72869159610        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:      243
 NPARAMETR:  2.3886E+00  3.4820E+00  9.0837E-01  5.3281E+01  7.5082E-02  1.1839E-01
 PARAMETER:  1.0276E-01  1.0149E-01  9.9874E-02  1.0305E-01  7.3524E-02  8.0429E-02
 GRADIENT:  -1.5054E+00  7.2242E-01 -4.2106E-02  5.0374E-01  2.0995E-01  3.9089E-01
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.
 REGARD THE RESULTS OF THE ESTIMATION STEP CAREFULLY, AND ACCEPT THEM ONLY
 AFTER CHECKING THAT THE COVARIANCE STEP PRODUCES REASONABLE OUTPUT.
 NO. OF FUNCTION EVALUATIONS USED:      243
 NO. OF SIG. DIGITS IN FINAL EST.:  2.2

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         8.6111E-03 -1.5024E-03
 SE:             4.4295E-02  6.1638E-02
 N:                      30          30
 
 P VAL.:         8.4586E-01  9.8055E-01
 
 ETAshrink(%):   9.9538E+00  2.0403E-01
 EBVshrink(%):   1.0849E+01  1.7218E+00
 EPSshrink(%):   4.8855E+00  4.8855E+00
 
 #TERE:
 Elapsed estimation time in seconds:   123.01
0R MATRIX ALGORITHMICALLY SINGULAR
 AND ALGORITHMICALLY NON-POSITIVE-SEMIDEFINITE
0COVARIANCE STEP ABORTED
 Elapsed covariance time in seconds:    61.12
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1210.729       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.39E+00  3.48E+00  9.08E-01  5.33E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        7.51E-02
 
 ETA2
+        0.00E+00  1.18E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1      EPS2   
 
 EPS1
+        1.00E+00
 
 EPS2
+        0.00E+00  1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.74E-01
 
 ETA2
+        0.00E+00  3.44E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1      EPS2   
 
 EPS1
+        1.00E+00
 
 EPS2
+        0.00E+00  1.00E+00
 
 #CPUT: Total CPU Time in Seconds,      309.600
Stop Time: 
Mon 09/30/2013 
02:56 PM

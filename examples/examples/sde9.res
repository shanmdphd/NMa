Mon 09/30/2013 
07:11 PM
$PROBLEM PK ODE HANDS ON ONE

$INPUT ID TIME DV AMT CMT FLAG MDV SDE

$DATA   sde9.csv
        IGNORE=@

$SUBROUTINE ADVAN6 TOL=9 DP OTHER=sde.f90

; nde=number of base equations, ncmt=number of observation compartments
$ABBR DECLARE SGW(3) ; need at least ncmt of these
$MODEL 
       COMP = (CENTRAL); there are nde base states
       COMP = (DFDX1)  ; need ncmt observation compartments
       COMP = (DPDT11) ; Will need (nde+1)*nde/2 of these

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
; number of base model derivative equations (nde)=1, Number of compartments (ncmt)=1. 
; DA is a reserved array, dimensioned DA(IR,*)
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
;  In general, if you have nmcmt observation compartments, then first ncmt EPS() will pertain to
; measurement error, and the second ncmt set of EPS() will pertain to stochastic errors.
;  This means you cannot have L2 type correlations, and prop+additive should be packaged into a single EPS().
;  For two obervations, you may have:
;  IF(CMT==1) THEN
;  IPRED=A(1)/V
;  W=SQRT(THETA((5)*THETA(5)*IPED*IPRED+THETA(6)*THETA(6))
;  Y=IPRED+W*EPS(1)+WS*EPS(3)
;  ENDIF
;  IF(CMT==2) THEN
;  IPRED=A(2)/V
;  W=SQRT(THETA((7)*THETA(7)*IPED*IPRED+THETA(8)*THETA(8))
;  Y=IPRED+W*EPS(2)+WS*EPS(4)
;  ENDIF

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

$EST METHOD=ITS INTERACTION LAPLACE NUMERICAL SLOW NOABORT PRINT=1 CTYPE=3 SIGL=5
$EST METHOD=IMP INTERACTION NOABORT SIGL=5 PRINT=1 IACCEPT=1.0 CTYPE=3
$EST MAXEVAL=9999 METHOD=1 LAPLACE INTER NOABORT NUMERICAL SLOW NSIG=3 PRINT=1 MSFO=sde9.msf SIGL=9
$COV MATRIX=R UNCONDITIONAL

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
 SLOW GRADIENT METHOD USED:     YES
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
0NRD VALUE FROM SUBROUTINE TOL:   9
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
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    YES 
 NUMERICAL 2ND DERIVATIVES:               YES 
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
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    5           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   5           
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

 iteration            0 OBJ=   1537.57925568805
 iteration            1 OBJ=   1368.97601699317
 iteration            2 OBJ=   1313.69756320881
 iteration            3 OBJ=   1279.02149245435
 iteration            4 OBJ=   1259.35218290874
 iteration            5 OBJ=   1259.37706679917
 iteration            6 OBJ=   1259.55327280150
 iteration            7 OBJ=   1257.11312498978
 iteration            8 OBJ=   1254.18145579194
 iteration            9 OBJ=   1254.38323392730
 iteration           10 OBJ=   1253.33650696259
 iteration           11 OBJ=   1253.07408720591
 iteration           12 OBJ=   1253.18910211656
 iteration           13 OBJ=   1253.34370236509
 iteration           14 OBJ=   1253.06529879402
 iteration           15 OBJ=   1252.97175653348
 iteration           16 OBJ=   1253.00669500077
 iteration           17 OBJ=   1253.00544573336
 iteration           18 OBJ=   1253.04022665553
 iteration           19 OBJ=   1253.01255895823
 iteration           20 OBJ=   1253.03573555595
 iteration           21 OBJ=   1253.01749028802
 iteration           22 OBJ=   1253.20011938130
 iteration           23 OBJ=   1253.05770591910
 Convergence achieved
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.9322E-03  3.9718E-04
 SE:             4.7980E-02  6.1953E-02
 N:                      30          30
 
 P VAL.:         9.5127E-01  9.9488E-01
 
 ETAshrink(%):   3.5824E+00  1.8821E+00
 EBVshrink(%):   4.2562E+00  1.7233E+00
 EPSshrink(%):   1.0000E-10  1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:    20.51
 Elapsed covariance time in seconds:     0.07
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1253.058       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.40E+00  3.47E+00  1.03E+00  5.05E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        7.69E-02
 
 ETA2
+        0.00E+00  1.24E-01
 


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
+        0.00E+00  3.52E-01
 


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
 
         5.85E-02  7.84E-02  7.50E-02  3.28E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        2.72E-02
 
 ETA2
+        0.00E+00  3.90E-02
 


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
+        4.91E-02
 
 ETA2
+       .........  5.54E-02
 


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
+        3.42E-03
 
 TH 2
+        8.56E-04  6.14E-03
 
 TH 3
+       -9.86E-04  1.27E-03  5.62E-03
 
 TH 4
+        4.50E-02 -3.91E-02 -1.65E-01  1.07E+01
 
 OM11
+       -4.65E-04 -3.41E-04  3.99E-04 -8.89E-03  7.41E-04
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.37E-04 -1.19E-03  2.64E-04  6.89E-03  7.64E-05  0.00E+00  1.52E-03
 
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
+        5.85E-02
 
 TH 2
+        1.87E-01  7.84E-02
 
 TH 3
+       -2.25E-01  2.16E-01  7.50E-02
 
 TH 4
+        2.35E-01 -1.52E-01 -6.70E-01  3.28E+00
 
 OM11
+       -2.92E-01 -1.60E-01  1.96E-01 -9.97E-02  2.72E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.04E-01 -3.90E-01  9.04E-02  5.40E-02  7.21E-02  0.00E+00  3.90E-02
 
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
+        3.51E+02
 
 TH 2
+       -5.15E+01  2.26E+02
 
 TH 3
+        2.98E+01 -8.32E+01  3.73E+02
 
 TH 4
+       -1.07E+00 -2.75E-01  5.26E+00  1.78E-01
 
 OM11
+        1.67E+02  9.43E+01 -1.43E+02 -1.29E+00  1.55E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+        5.55E+00  1.80E+02 -1.42E+02 -2.04E+00  5.25E+01  0.00E+00  8.32E+02
 
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
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    YES 
 NUMERICAL 2ND DERIVATIVES:               YES 
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
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    5           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   5           
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
 NO. ITERATIONS FOR MAP (MAPITER):        1           
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

 iteration            0 OBJ=   1213.59954664099 eff.=     451. Smpl.=     300. Fit.= 0.83829
 iteration            1 OBJ=   1210.84663725043 eff.=     244. Smpl.=     300. Fit.= 0.89882
 iteration            2 OBJ=   1210.68817612670 eff.=     292. Smpl.=     300. Fit.= 0.94534
 iteration            3 OBJ=   1210.83517297672 eff.=     280. Smpl.=     300. Fit.= 0.95913
 iteration            4 OBJ=   1210.57553492539 eff.=     313. Smpl.=     300. Fit.= 0.95881
 iteration            5 OBJ=   1210.56021600367 eff.=     291. Smpl.=     300. Fit.= 0.96141
 iteration            6 OBJ=   1210.69787720754 eff.=     302. Smpl.=     300. Fit.= 0.96136
 iteration            7 OBJ=   1210.75235324867 eff.=     304. Smpl.=     300. Fit.= 0.96097
 iteration            8 OBJ=   1210.85738643847 eff.=     305. Smpl.=     300. Fit.= 0.95892
 iteration            9 OBJ=   1210.69341725199 eff.=     306. Smpl.=     300. Fit.= 0.95675
 iteration           10 OBJ=   1210.79558019916 eff.=     293. Smpl.=     300. Fit.= 0.95996
 iteration           11 OBJ=   1210.76678202683 eff.=     304. Smpl.=     300. Fit.= 0.95780
 iteration           12 OBJ=   1210.82251046930 eff.=     300. Smpl.=     300. Fit.= 0.95774
 iteration           13 OBJ=   1210.75407590848 eff.=     302. Smpl.=     300. Fit.= 0.96077
 iteration           14 OBJ=   1210.53582440549 eff.=     298. Smpl.=     300. Fit.= 0.96067
 Convergence achieved
 iteration           14 OBJ=   1210.65295492957 eff.=     306. Smpl.=     300. Fit.= 0.95907
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         9.1845E-04  1.4574E-03
 SE:             4.5260E-02  6.2060E-02
 N:                      30          30
 
 P VAL.:         9.8381E-01  9.8126E-01
 
 ETAshrink(%):   1.0428E+01  1.4769E+00
 EBVshrink(%):   1.0221E+01  1.6700E+00
 EPSshrink(%):   4.9730E+00  4.9730E+00
 
 #TERE:
 Elapsed estimation time in seconds:    90.23
 Elapsed covariance time in seconds:    25.69
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1210.653       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.38E+00  3.48E+00  9.08E-01  5.30E+01
 


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
 
         5.75E-02  6.52E-02  7.87E-02  3.91E+00
 


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
+       -7.23E-05  4.25E-03
 
 TH 3
+       -1.97E-04  1.82E-05  6.19E-03
 
 TH 4
+        1.17E-02  5.85E-03 -2.04E-01  1.53E+01
 
 OM11
+       -1.08E-04 -2.44E-05  1.89E-04 -1.39E-02  6.96E-04
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.24E-05 -5.35E-05  1.33E-04 -8.04E-03  1.01E-04  0.00E+00  1.29E-03
 
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
+       -4.36E-02  3.55E-03  7.87E-02
 
 TH 4
+        5.20E-02  2.29E-02 -6.63E-01  3.91E+00
 
 OM11
+       -7.14E-02 -1.42E-02  9.10E-02 -1.35E-01  2.64E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.09E-02 -2.28E-02  4.72E-02 -5.73E-02  1.07E-01  0.00E+00  3.59E-02
 
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
+        5.63E+00  2.36E+02
 
 TH 3
+        3.44E+00 -6.54E+00  2.89E+02
 
 TH 4
+       -1.49E-01 -1.72E-01  3.85E+00  1.18E-01
 
 OM11
+        4.36E+01  6.16E+00 -3.30E-02  1.26E+00  1.48E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+        8.27E-01  8.98E+00 -6.02E+00  2.31E-01 -1.07E+02  0.00E+00  7.86E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 
 
 #TBLN:      3
 #METH: Laplacian Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 SLOW GRADIENT METHOD USED:               YES 
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    YES 
 NUMERICAL 2ND DERIVATIVES:               YES 
 NO. OF FUNCT. EVALS. ALLOWED:            9999
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  YES 
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    9           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   9           
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   1254.41986329783        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  2.3820E+00  3.4768E+00  9.0837E-01  5.3050E+01  7.9236E-02  1.2314E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   6.0542E+04 -9.2779E+04 -1.1137E+05 -2.8304E+04 -6.0136E+04  1.5216E+04
 
0ITERATION NO.:    1    OBJECTIVE VALUE:   1253.35521168043        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       16
 NPARAMETR:  2.3627E+00  3.5205E+00  9.2210E-01  5.3252E+01  8.0530E-02  1.2264E-01
 PARAMETER:  9.1846E-02  1.1250E-01  1.1500E-01  1.0381E-01  1.0810E-01  9.7951E-02
 GRADIENT:  -4.8928E+05 -3.7706E+04 -2.7512E+05 -1.0434E+05 -7.3749E+04 -5.4819E+04
 
0ITERATION NO.:    2    OBJECTIVE VALUE:   1252.91421654675        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       25
 NPARAMETR:  2.4144E+00  3.5264E+00  9.3341E-01  5.3499E+01  8.1058E-02  1.2323E-01
 PARAMETER:  1.1352E-01  1.1417E-01  1.2719E-01  1.0844E-01  1.1137E-01  1.0038E-01
 GRADIENT:   1.4383E+05  1.0986E+05  1.2813E+05  3.8397E+04  3.9098E+04  1.0462E+05
 
0ITERATION NO.:    3    OBJECTIVE VALUE:   1248.85240342935        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:       35
 NPARAMETR:  2.4306E+00  3.4857E+00  9.3149E-01  5.3524E+01  8.0874E-02  1.2077E-01
 PARAMETER:  1.2018E-01  1.0257E-01  1.2514E-01  1.0891E-01  1.1023E-01  9.0274E-02
 GRADIENT:   1.3045E+06  1.5018E+06  1.2443E+06  1.4398E+06  1.6901E+05  1.2846E+06
 
0ITERATION NO.:    4    OBJECTIVE VALUE:   1248.85240342935        NO. OF FUNC. EVALS.:  25
 CUMULATIVE NO. OF FUNC. EVALS.:       60
 NPARAMETR:  2.4306E+00  3.4857E+00  9.3149E-01  5.3524E+01  8.0874E-02  1.2077E-01
 PARAMETER:  1.2018E-01  1.0257E-01  1.2514E-01  1.0891E-01  1.1023E-01  9.0274E-02
 GRADIENT:   1.5278E+03 -1.4741E+03  2.8538E+02  1.2378E+03 -1.6356E+04 -2.2526E+04
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   1248.85240342935        NO. OF FUNC. EVALS.:  18
 CUMULATIVE NO. OF FUNC. EVALS.:       78
 NPARAMETR:  2.4306E+00  3.4857E+00  9.3149E-01  5.3524E+01  8.0875E-02  1.2077E-01
 PARAMETER:  1.2018E-01  1.0257E-01  1.2514E-01  1.0891E-01  1.1023E-01  9.0274E-02
 GRADIENT:   1.5278E+03 -1.4741E+03  2.8538E+02  1.2378E+03 -1.6356E+04 -2.2526E+04
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.
 REGARD THE RESULTS OF THE ESTIMATION STEP CAREFULLY, AND ACCEPT THEM ONLY
 AFTER CHECKING THAT THE COVARIANCE STEP PRODUCES REASONABLE OUTPUT.
 NO. OF FUNCTION EVALUATIONS USED:       78
 NO. OF SIG. DIGITS IN FINAL EST.:  3.3

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:        -2.2602E-02 -1.2133E-02
 SE:             4.7680E-02  6.1783E-02
 N:                      30          30
 
 P VAL.:         6.3548E-01  8.4431E-01
 
 ETAshrink(%):   6.5983E+00  9.5901E-01
 EBVshrink(%):   5.8915E+00  3.1448E+00
 EPSshrink(%):   1.0000E-10  1.0000E-10
 
 #TERE:
 Elapsed estimation time in seconds:    54.40
0PRED EXIT CODE = 1
0INDIVIDUAL NO.      17   ID= 1.70000000000000E+01   (WITHIN-INDIVIDUAL) DATA REC NO.   2
 THETA=
  2.43E+00   3.48E+00   9.31E-01   5.35E+01
 ETA=
  2.35E+01   4.34E-01
 OCCURS DURING SEARCH FOR ETA AT A NONZERO VALUE OF ETA
 NUMERICAL DIFFICULTIES WITH INTEGRATION ROUTINE.
 NO. OF REQUIRED SIGNIFICANT DIGITS IN SOLUTION VECTOR
 TO DIFFERENTIAL EQUATIONS,   9, MAY BE TOO LARGE.
0PROGRAM TERMINATED BY OBJ
 Elapsed covariance time in seconds:    23.49
 MESSAGE ISSUED FROM COVARIANCE STEP
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1248.852       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.43E+00  3.49E+00  9.31E-01  5.35E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        8.09E-02
 
 ETA2
+        0.00E+00  1.21E-01
 


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
+        2.84E-01
 
 ETA2
+        0.00E+00  3.48E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1      EPS2   
 
 EPS1
+        1.00E+00
 
 EPS2
+        0.00E+00  1.00E+00
 
1THERE ARE ERROR MESSAGES IN FILE PRDERR                                                                  
 #CPUT: Total CPU Time in Seconds,      213.737
Stop Time: 
Mon 09/30/2013 
07:15 PM

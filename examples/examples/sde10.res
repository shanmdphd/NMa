Sat 09/07/2013 
12:03 AM
; Using the SDE in-line equations after the manner of Chris Tornoe, but MU modeling theta(1) and theta(2).  
; Note that IMP method results are similar in sde9 and sde10, since this method did not rely on NMTRAN having 
; exposure to analytical SDE equations.  But ITS and Laplace in sde9 is not correct, relative to sde10.  
; Although their LAPLACE OBJ's are far apart, in fact the parameters are similar.
$PROBLEM PK ODE HANDS ON ONE
$INPUT ID HOUR DV AMT CMT FLAG EVID MDV SDE TIME
$DATA   sde7.csv
        IGNORE=@
$SUBROUTINE ADVAN6 TOL 10 DP
$MODEL 
       COMP = (CENTRAL);
       COMP = (P1)

$THETA (0,2.3)               ;1 CL
$THETA (0,3.5)               ;2 VD
$THETA (0, 2)               ;4 SIGMA
$THETA (0,1) ; SGW1

$OMEGA 0.1                  ;1 CL
$OMEGA 0.01                 ;2 VD

$SIGMA 1 FIX                ; PK

$PK
  IF(NEWIND.NE.2) OT = 0

  MU_1  = THETA(1)
  CL    = EXP(MU_1+ETA(1)) 
  MU_2  = THETA(2)
  VD    = EXP(MU_2+ETA(2))
  SGW1 = THETA(4)

IF(NEWIND.NE.2) THEN
  AHT1 = 0
  PHT1 = 0
ENDIF

IF(EVID.NE.3) THEN
  A1 = A(1)
  A2 = A(2)
ELSE
  A1 = A1
  A2 = A2
ENDIF

IF(EVID.EQ.0) OBS = DV

IF(EVID.GT.2.AND.SDE.EQ.2) THEN
  RVAR = A2*(1/VD)**2+ THETA(3)**2
  K1   = A2*(1/VD)/RVAR
  AHT1 = A1 + K1*(OBS -( A1/VD))
  PHT1 = A2 - K1*RVAR*K1
ENDIF

IF(EVID.GT.2.AND.SDE.EQ.3) THEN
  AHT1 = A1
  PHT1 = 0
ENDIF

IF(EVID.GT.2.AND.SDE.EQ.4) THEN
  AHT1 = 0
  PHT1 = A2
ENDIF

IF(A_0FLG.EQ.1) THEN
  A_0(1) = AHT1
  A_0(2) = PHT1
ENDIF

$DES
 DADT(1) = - CL/VD*A(1) ;+0
DADT(2) = (-CL/VD)*(A(2))+(-CL/VD)*(A(2))+SGW1*SGW1

$ERROR (OBS ONLY)
     IPRED = A(1)/VD
     IRES  = DV - IPRED
W=SQRT(ABS(A(2))*(1/VD)**2+ THETA(3)**2)
     IWRES = IRES/W
     Y     = IPRED+W*EPS(1)

$EST METHOD=ITS INTERACTION LAPLACE NUMERICAL SLOW NOABORT PRINT=1 CTYPE=3
$EST METHOD=IMP INTERACTION NOABORT SIGL=5 PRINT=1 IACCEPT=1.0 CTYPE=3
$EST MAXEVAL=9999 METHOD=1 LAPLACE INTER NOABORT NUMERICAL SLOW NSIG=3 PRINT=1 MSFO=sde10.msf
$COV MATRIX=R
$TABLE ID TIME FLAG AMT CMT IPRED IRES IWRES EVID
       ONEHEADER NOPRINT FILE=sde10.fit
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  3) THERE MAY BE AN ERROR IN THE ABBREVIATED CODE. THE FOLLOWING
 ONE OR MORE RANDOM VARIABLES ARE DEFINED WITH "IF" STATEMENTS THAT DO NOT
 PROVIDE DEFINITIONS FOR BOTH THE "THEN" AND "ELSE" CASES. IF ALL
 CONDITIONS FAIL, THE VALUES OF THESE VARIABLES WILL BE ZERO.
  
   RVAR K1

             
 (WARNING  26) THE DERIVATIVE OF THE ABSOLUTE VALUE OF A RANDOM VARIABLE IS
 BEING COMPUTED. IF THE ABSOLUTE VALUE AFFECTS THE VALUE OF THE OBJECTIVE
 FUNCTION, THE USER SHOULD ENSURE THAT THE RANDOM VARIABLE IS ALWAYS
 POSITIVE OR ALWAYS NEGATIVE.

 (MU_WARNING 24) ABBREVIATED CODE IS TOO COMPLEX. UNABLE TO CHECK USE OF MU_ VARIABLES.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        7 SEP 2013
Days until program expires :6110
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3beta7.0(N)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 PK ODE HANDS ON ONE
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     1650
 NO. OF DATA ITEMS IN DATA SET:  10
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  8
0INDICES PASSED TO SUBROUTINE PRED:
   7  10   4   0   0   0   5   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID HOUR DV AMT CMT FLAG EVID MDV SDE TIME
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 IPRED IRES IWRES
0FORMAT FOR DATA:
 (E3.0,E5.0,E9.0,E5.0,5E2.0,E5.0)

 TOT. NO. OF OBS RECS:      540
 TOT. NO. OF INDIVIDUALS:     30
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
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
 HEADERS:               ONE
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID TIME FLAG AMT CMT IPRED IRES IWRES EVID
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(N)

 GENERAL NONLINEAR KINETICS MODEL (ADVAN6)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   3
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         YES        YES        YES        YES
    2         P1           ON         YES        YES        NO         NO
    3         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE FROM SUBROUTINE TOL:  10
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            *           *           *           *           *
    3            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      7
   TIME DATA ITEM IS DATA ITEM NO.:         10
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    5

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0PK SUBROUTINE INDICATES THAT COMPARTMENT AMOUNTS ARE INITIALIZED.
0PK SUBROUTINE INDICATES THAT DERIVATIVES OF COMPARTMENT AMOUNTS ARE USED.
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
 NO. OF FUNCT. EVALS. ALLOWED:            360
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

 iteration            0 OBJ=   1537.54893528667
 iteration            1 OBJ=   1348.56103759553
 iteration            2 OBJ=   1290.92127165878
 iteration            3 OBJ=   1248.87673693598
 iteration            4 OBJ=   1221.05108267738
 iteration            5 OBJ=   1212.25448872722
 iteration            6 OBJ=   1211.32610576900
 iteration            7 OBJ=   1211.26737590695
 iteration            8 OBJ=   1211.26290959806
 iteration            9 OBJ=   1211.26252625118
 iteration           10 OBJ=   1211.26224089947
 iteration           11 OBJ=   1211.26234579472
 iteration           12 OBJ=   1211.26222681959
 iteration           13 OBJ=   1211.26232548618
 iteration           14 OBJ=   1211.26230691830
 iteration           15 OBJ=   1211.26214770994
 Convergence achieved
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         7.6132E-06  2.5354E-05
 SE:             4.4462E-02  6.1720E-02
 N:                      30          30
 
 P VAL.:         9.9986E-01  9.9967E-01
 
 ETAshrink(%):   1.0460E+01  1.6626E+00
 EBVshrink(%):   1.0456E+01  1.6600E+00
 EPSshrink(%):   4.7930E+00
 
 #TERE:
 Elapsed estimation time in seconds:    18.31
 Elapsed covariance time in seconds:     0.28
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1211.262       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.40E+00  3.48E+00  9.08E-01  5.32E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        7.65E-02
 
 ETA2
+        0.00E+00  1.22E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.77E-01
 
 ETA2
+        0.00E+00  3.50E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.00E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         6.49E-02  8.47E-02  8.63E-02  4.28E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        3.56E-02
 
 ETA2
+        0.00E+00  4.10E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        0.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        6.43E-02
 
 ETA2
+       .........  5.87E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+       .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        4.21E-03
 
 TH 2
+        9.08E-04  7.18E-03
 
 TH 3
+       -1.39E-03  1.79E-03  7.45E-03
 
 TH 4
+        8.06E-02 -7.07E-02 -1.99E-01  1.83E+01
 
 OM11
+       -8.49E-04 -8.06E-04  5.91E-04 -2.85E-02  1.27E-03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -7.91E-05 -1.66E-03  5.22E-05  2.51E-02  1.12E-04  0.00E+00  1.68E-03
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.49E-02
 
 TH 2
+        1.65E-01  8.47E-02
 
 TH 3
+       -2.49E-01  2.44E-01  8.63E-02
 
 TH 4
+        2.90E-01 -1.95E-01 -5.38E-01  4.28E+00
 
 OM11
+       -3.68E-01 -2.67E-01  1.93E-01 -1.87E-01  3.56E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.97E-02 -4.78E-01  1.47E-02  1.43E-01  7.64E-02  0.00E+00  4.10E-02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        3.04E+02
 
 TH 2
+       -4.62E+01  2.29E+02
 
 TH 3
+        3.28E+01 -6.89E+01  2.15E+02
 
 TH 4
+       -9.01E-01  2.58E-01  1.95E+00  8.33E-02
 
 OM11
+        1.41E+02  1.34E+02 -6.99E+01  6.23E-01  1.01E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.82E+01  2.13E+02 -9.75E+01 -1.13E+00  6.50E+01  0.00E+00  8.18E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
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
 NO. OF FUNCT. EVALS. ALLOWED:            360
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
              (ISCALE_MIN, ISCALE_MAX):   0.100000000000000       ,10.0000000000000        
 SAMPLE ACCEPTANCE RATE (IACCEPT):        1.00000000000000        
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

 iteration            0 OBJ=   1210.83697127520 eff.=     302. Smpl.=     300. Fit.= 0.98495
 iteration            1 OBJ=   1210.82483929464 eff.=     299. Smpl.=     300. Fit.= 0.96478
 iteration            2 OBJ=   1210.66465449623 eff.=     302. Smpl.=     300. Fit.= 0.96042
 iteration            3 OBJ=   1210.86546872705 eff.=     301. Smpl.=     300. Fit.= 0.96054
 iteration            4 OBJ=   1210.57360722935 eff.=     307. Smpl.=     300. Fit.= 0.95932
 iteration            5 OBJ=   1210.60599332023 eff.=     292. Smpl.=     300. Fit.= 0.96083
 iteration            6 OBJ=   1210.69618283862 eff.=     301. Smpl.=     300. Fit.= 0.96042
 iteration            7 OBJ=   1210.68320230969 eff.=     304. Smpl.=     300. Fit.= 0.96115
 iteration            8 OBJ=   1210.86010830172 eff.=     305. Smpl.=     300. Fit.= 0.95743
 iteration            9 OBJ=   1210.67589421443 eff.=     307. Smpl.=     300. Fit.= 0.95648
 iteration           10 OBJ=   1210.79466141705 eff.=     293. Smpl.=     300. Fit.= 0.96007
 iteration           11 OBJ=   1210.79254765888 eff.=     305. Smpl.=     300. Fit.= 0.95818
 iteration           12 OBJ=   1210.81391934778 eff.=     300. Smpl.=     300. Fit.= 0.95697
 iteration           13 OBJ=   1210.72374987120 eff.=     301. Smpl.=     300. Fit.= 0.96113
 iteration           14 OBJ=   1210.53151615155 eff.=     299. Smpl.=     300. Fit.= 0.95988
 Convergence achieved
 iteration           14 OBJ=   1210.69788009385 eff.=     306. Smpl.=     300. Fit.= 0.95927
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.4692E-03  1.5818E-03
 SE:             4.5246E-02  6.1911E-02
 N:                      30          30
 
 P VAL.:         9.7410E-01  9.7962E-01
 
 ETAshrink(%):   1.0018E+01  1.8337E+00
 EBVshrink(%):   1.0374E+01  1.6539E+00
 EPSshrink(%):   5.0202E+00
 
 #TERE:
 Elapsed estimation time in seconds:   111.23
 Elapsed covariance time in seconds:    31.70
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1210.698       **************************************************
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
+        7.85E-02
 
 ETA2
+        0.00E+00  1.23E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.80E-01
 
 ETA2
+        0.00E+00  3.51E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.00E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         5.74E-02  6.53E-02  7.84E-02  3.89E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        2.65E-02
 
 ETA2
+        0.00E+00  3.61E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        0.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        4.72E-02
 
 ETA2
+       .........  5.13E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+       .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        3.30E-03
 
 TH 2
+       -6.87E-05  4.26E-03
 
 TH 3
+       -2.11E-04  1.80E-05  6.14E-03
 
 TH 4
+        1.32E-02  5.74E-03 -2.01E-01  1.51E+01
 
 OM11
+       -1.46E-04 -1.96E-05  1.65E-04 -1.28E-02  7.01E-04
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.74E-05 -5.73E-05  9.73E-05 -7.31E-03  1.00E-04  0.00E+00  1.30E-03
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        5.74E-02
 
 TH 2
+       -1.83E-02  6.53E-02
 
 TH 3
+       -4.69E-02  3.52E-03  7.84E-02
 
 TH 4
+        5.91E-02  2.26E-02 -6.60E-01  3.89E+00
 
 OM11
+       -9.62E-02 -1.14E-02  7.96E-02 -1.24E-01  2.65E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -8.41E-03 -2.43E-02  3.44E-02 -5.21E-02  1.05E-01  0.00E+00  3.61E-02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        3.07E+02
 
 TH 2
+        5.43E+00  2.35E+02
 
 TH 3
+        3.12E+00 -6.33E+00  2.89E+02
 
 TH 4
+       -1.78E-01 -1.69E-01  3.84E+00  1.19E-01
 
 OM11
+        6.05E+01  4.76E+00  2.49E+00  1.17E+00  1.48E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.55E+00  9.58E+00 -4.58E-01  2.78E-01 -1.06E+02  0.00E+00  7.78E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:   1211.18434338299        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  2.3820E+00  3.4767E+00  9.0764E-01  5.3144E+01  7.8468E-02  1.2344E-01
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   2.4172E+00 -2.8139E+00 -2.9576E-01  1.7494E-01  1.9074E+00  2.1360E+00
 
0ITERATION NO.:    1    OBJECTIVE VALUE:   1211.18180075466        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:       18
 NPARAMETR:  2.3789E+00  3.4820E+00  9.0779E-01  5.3139E+01  7.8304E-02  1.2315E-01
 PARAMETER:  9.8678E-02  1.0154E-01  1.0016E-01  9.9904E-02  9.8957E-02  9.8832E-02
 GRADIENT:  -2.0047E+00  5.8575E+00 -3.3247E-01  8.5377E-02  1.7911E+00  2.0028E+00
 
0ITERATION NO.:    2    OBJECTIVE VALUE:   1211.17752764856        NO. OF FUNC. EVALS.:  11
 CUMULATIVE NO. OF FUNC. EVALS.:       29
 NPARAMETR:  2.3802E+00  3.4764E+00  9.0787E-01  5.3137E+01  7.8226E-02  1.2302E-01
 PARAMETER:  9.9234E-02  9.9915E-02  1.0025E-01  9.9881E-02  9.8460E-02  9.8277E-02
 GRADIENT:  -2.5051E-01 -3.3440E+00 -2.3195E-01  1.7389E-01  1.7705E+00  1.9429E+00
 
0ITERATION NO.:    3    OBJECTIVE VALUE:   1211.17256034282        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:       39
 NPARAMETR:  2.3886E+00  3.4790E+00  9.0887E-01  5.3106E+01  7.7087E-02  1.2104E-01
 PARAMETER:  1.0276E-01  1.0066E-01  1.0135E-01  9.9289E-02  9.1126E-02  9.0175E-02
 GRADIENT:   1.1911E+01  1.2415E+00  8.4051E-02 -1.7236E-01  1.3295E+00  1.0742E+00
 
0ITERATION NO.:    4    OBJECTIVE VALUE:   1211.14535415489        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       48
 NPARAMETR:  2.3839E+00  3.4783E+00  9.1100E-01  5.3045E+01  7.4496E-02  1.1663E-01
 PARAMETER:  1.0077E-01  1.0046E-01  1.0369E-01  9.8147E-02  7.4027E-02  7.1610E-02
 GRADIENT:   4.4871E+00 -2.3956E-01  7.4216E-01 -4.8521E-01  3.8669E-02 -9.8523E-01
 
0ITERATION NO.:    5    OBJECTIVE VALUE:   1211.14299376158        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       57
 NPARAMETR:  2.3840E+00  3.4783E+00  9.0792E-01  5.3161E+01  7.3886E-02  1.1686E-01
 PARAMETER:  1.0083E-01  1.0046E-01  1.0031E-01  1.0032E-01  6.9919E-02  7.2611E-02
 GRADIENT:   4.4461E+00 -1.6057E-01 -7.3957E-02 -3.8556E-01 -2.1731E-01 -8.5522E-01
 
0ITERATION NO.:    6    OBJECTIVE VALUE:   1211.14294085784        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       66
 NPARAMETR:  2.3840E+00  3.4784E+00  9.0852E-01  5.3221E+01  7.3699E-02  1.1705E-01
 PARAMETER:  1.0084E-01  1.0048E-01  1.0097E-01  1.0145E-01  6.8654E-02  7.3422E-02
 GRADIENT:   4.4807E+00 -1.4708E-01  6.5428E-01  5.8084E-01 -2.8280E-01 -7.5366E-01
 
0ITERATION NO.:    7    OBJECTIVE VALUE:   1211.14259156401        NO. OF FUNC. EVALS.:   9
 CUMULATIVE NO. OF FUNC. EVALS.:       75
 NPARAMETR:  2.3840E+00  3.4783E+00  9.0912E-01  5.3180E+01  7.3457E-02  1.1739E-01
 PARAMETER:  1.0083E-01  1.0048E-01  1.0162E-01  1.0069E-01  6.7007E-02  7.4882E-02
 GRADIENT:   4.3561E+00 -1.7828E-01  6.8152E-01  2.8794E-01 -4.1956E-01 -5.9600E-01
 
0ITERATION NO.:    8    OBJECTIVE VALUE:   1211.13679879599        NO. OF FUNC. EVALS.:  10
 CUMULATIVE NO. OF FUNC. EVALS.:       85
 NPARAMETR:  2.3815E+00  3.4784E+00  9.0802E-01  5.3187E+01  7.4237E-02  1.1840E-01
 PARAMETER:  9.9786E-02  1.0051E-01  1.0041E-01  1.0082E-01  7.2289E-02  7.9160E-02
 GRADIENT:   7.6794E-01 -3.4776E-02  1.4879E-01  1.1992E-01 -7.0494E-02 -1.2298E-01
 
0ITERATION NO.:    9    OBJECTIVE VALUE:   1211.13662612701        NO. OF FUNC. EVALS.:   8
 CUMULATIVE NO. OF FUNC. EVALS.:       93
 NPARAMETR:  2.3810E+00  3.4785E+00  9.0782E-01  5.3181E+01  7.4405E-02  1.1867E-01
 PARAMETER:  9.9564E-02  1.0052E-01  1.0020E-01  1.0070E-01  7.3417E-02  8.0290E-02
 GRADIENT:  -2.8675E-02  2.5198E-02  2.7220E-03 -8.1132E-04  1.4491E-03  1.9703E-03
 
0ITERATION NO.:   10    OBJECTIVE VALUE:   1211.13662612701        NO. OF FUNC. EVALS.:  12
 CUMULATIVE NO. OF FUNC. EVALS.:      105
 NPARAMETR:  2.3810E+00  3.4785E+00  9.0782E-01  5.3181E+01  7.4405E-02  1.1867E-01
 PARAMETER:  9.9564E-02  1.0052E-01  1.0020E-01  1.0070E-01  7.3417E-02  8.0290E-02
 GRADIENT:  -1.9941E-01 -2.9392E-01 -2.3305E-02 -3.4961E-02 -3.2270E-03 -4.5366E-03
 
0ITERATION NO.:   11    OBJECTIVE VALUE:   1211.13662612701        NO. OF FUNC. EVALS.:   0
 CUMULATIVE NO. OF FUNC. EVALS.:      105
 NPARAMETR:  2.3810E+00  3.4785E+00  9.0782E-01  5.3181E+01  7.4405E-02  1.1867E-01
 PARAMETER:  9.9564E-02  1.0052E-01  1.0020E-01  1.0070E-01  7.3417E-02  8.0290E-02
 GRADIENT:  -1.9941E-01 -2.9392E-01 -2.3305E-02 -3.4961E-02 -3.2270E-03 -4.5366E-03
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:      105
 NO. OF SIG. DIGITS IN FINAL EST.:  3.3

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         1.4708E-02  2.0336E-03
 SE:             4.4277E-02  6.1658E-02
 N:                      30          30
 
 P VAL.:         7.3975E-01  9.7369E-01
 
 ETAshrink(%):   9.5720E+00  2.9009E-01
 EBVshrink(%):   1.0796E+01  1.7063E+00
 EPSshrink(%):   4.7749E+00
 
 #TERE:
 Elapsed estimation time in seconds:    39.41
 Elapsed covariance time in seconds:    24.40
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1211.137       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         2.38E+00  3.48E+00  9.08E-01  5.32E+01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        7.44E-02
 
 ETA2
+        0.00E+00  1.19E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.00E+00
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.73E-01
 
 ETA2
+        0.00E+00  3.44E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.00E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         5.63E-02  6.40E-02  7.87E-02  3.93E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        2.50E-02
 
 ETA2
+       .........  3.19E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+       .........
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        4.58E-02
 
 ETA2
+       .........  4.63E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+       .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        3.17E-03
 
 TH 2
+       -7.54E-05  4.10E-03
 
 TH 3
+       -1.75E-04  2.56E-05  6.19E-03
 
 TH 4
+        1.03E-02  5.17E-03 -2.05E-01  1.55E+01
 
 OM11
+       -1.07E-04 -5.86E-06  1.63E-04 -1.28E-02  6.24E-04
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -8.68E-06  2.09E-06  5.63E-05 -4.52E-03  8.09E-06 .........  1.02E-03
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        5.63E-02
 
 TH 2
+       -2.09E-02  6.40E-02
 
 TH 3
+       -3.94E-02  5.09E-03  7.87E-02
 
 TH 4
+        4.64E-02  2.05E-02 -6.61E-01  3.93E+00
 
 OM11
+       -7.59E-02 -3.66E-03  8.29E-02 -1.30E-01  2.50E-02
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+       -4.84E-03  1.03E-03  2.25E-02 -3.60E-02  1.02E-02 .........  3.19E-02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                LAPLACIAN CONDITIONAL ESTIMATION WITH INTERACTION               ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        3.18E+02
 
 TH 2
+        6.05E+00  2.44E+02
 
 TH 3
+        3.44E+00 -6.52E+00  2.87E+02
 
 TH 4
+       -1.25E-01 -1.71E-01  3.80E+00  1.16E-01
 
 OM11
+        5.09E+01  1.55E+00  3.21E+00  1.36E+00  1.64E+03
 
 OM12
+       ......... ......... ......... ......... ......... .........
 
 OM22
+        1.55E+00 -8.62E-01  9.92E-01  2.95E-01 -6.75E+00 .........  9.86E+02
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 #CPUT: Total CPU Time in Seconds,      219.016
Stop Time: 
Sat 09/07/2013 
12:07 AM

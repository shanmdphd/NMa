Mon 09/30/2013 
03:45 PM
;Model Desc: Population Mixture Problem in 1 Compartment model, with rate constant parameter
;            mean modeled for two sub-populations, but its inter-subject variance is the same in both sub-populations
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# example5 (from ad1tr1m4t)
$INPUT C SET ID JID TIME CONC=DV DOSE=AMT RATE EVID MDV CMT VC1 K101 VC2 K102 SIGZ PROB
$DATA example5.csv IGNORE=C

$SUBROUTINES ADVAN1 TRANS1

$MIX
P(1)=THETA(4)
P(2)=1.0-THETA(4)
NSPOP=2


$PK
Q=1
IF(MIXNUM.EQ.2) Q=0
MU_1=THETA(1)
; Note that MU_2 can be modeled as THETA(2) or THETA(3), depending on the MIXNUM value.
; Also, we are avoiding IF/THEN blocks.
MU_2=Q*THETA(2)+(1.0-Q)*THETA(3)
V=DEXP(MU_1+ETA(1))
K=DEXP(MU_2+ETA(2))
S1=V

$ERROR
Y = F + F*EPS(1)

$THETA
(-1000.0  4.3 1000.0)  ;[MU_1]
(-1000.0 -2.9 1000.0)  ;[MU_2-1]
(-1000.0 -0.67 1000.0) ;[MU_2-2]
(0.0001 0.667 0.9999)  ;[P(1)]

$OMEGA BLOCK(2)
0.04 ;[p]
0.01  ;[f]
0.04 ;[p]

$SIGMA 
0.01 ;[p]

$EST METHOD=ITS INTERACTION NITER=100 PRINT=1 NOABORT SIGL=8 FILE=example5.ext CTYPE=3
$EST METHOD=IMPMAP INTERACTION NITER=20 ISAMPLE=300 PRINT=1 NOABORT SIGL=8
$EST METHOD=IMP INTERACTION NITER=20 ISAMPLE=1000 PRINT=1 NOABORT SIGL=6
$EST NBURN=500 NITER=500 METHOD=SAEM INTERACTION PRINT=10 SIGL=6 ISAMPLE=2
$EST METHOD=IMP INTERACTION NITER=5 ISAMPLE=1000 PRINT=1 NOABORT SIGL=6 EONLY=1 MAPITER=0 
$EST METHOD=BAYES INTERACTION NBURN=2000 NITER=5000 PRINT=10  FILE=example5.txt SIGL=8
$EST MAXEVAL=9999 NSIG=2 SIGL=8 PRINT=10 FILE=example5.ext METHOD=CONDITIONAL INTERACTION NOABORT
$COV MATRIX=R

  
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
 RUN# example5 (from ad1tr1m4t)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     2700
 NO. OF DATA ITEMS IN DATA SET:  17
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   5   7   8   0   0  11   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID JID TIME CONC DOSE RATE EVID MDV CMT VC1 K101 VC2 K102 SIGZ PROB
0FORMAT FOR DATA:
 (2E1.0,3E3.0,E10.0,E3.0,4E1.0,E6.0,E8.0,E6.0,E7.0,E3.0,E5.0)

 TOT. NO. OF OBS RECS:     2400
 TOT. NO. OF INDIVIDUALS:    300
0LENGTH OF THETA:   4
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+04     0.4300E+01     0.1000E+04
 -0.1000E+04    -0.2900E+01     0.1000E+04
 -0.1000E+04    -0.6700E+00     0.1000E+04
  0.1000E-03     0.6670E+00     0.9999E+00
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.4000E-01
                  0.1000E-01   0.4000E-01
0INITIAL ESTIMATE OF SIGMA:
 0.1000E-01
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
0
 MIX SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(P)

 ONE COMPARTMENT MODEL (ADVAN1)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   2
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1

0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         NO         YES        YES        YES
    2         OUTPUT       OFF        YES        NO         NO         NO
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            3           *           *           *           *
    2            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0ERROR IN LOG Y IS MODELED
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      9
   TIME DATA ITEM IS DATA ITEM NO.:          5
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   7
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     8
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:   11

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      1
 #METH: Iterative Two Stage
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            360
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
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
 ITERATIONS (NITER):                      100         
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -8044.30293659409
 iteration            1 OBJ=  -9906.52941243044
 iteration            2 OBJ=  -9939.32105992245
 iteration            3 OBJ=  -9939.46678927682
 iteration            4 OBJ=  -9939.46013697652
 iteration            5 OBJ=  -9939.45963133928
 iteration            6 OBJ=  -9939.45959556530
 iteration            7 OBJ=  -9939.45959304076
 iteration            8 OBJ=  -9939.45959287914
 iteration            9 OBJ=  -9939.45959288706
 iteration           10 OBJ=  -9939.45959286014
 iteration           11 OBJ=  -9939.45959278424
 Convergence achieved
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.6693E-03  1.3544E-03
 SE:             1.4908E-02  1.1375E-02
 N:                     200         200
 
 ETAshrink(%):   3.0064E+00  1.8516E+01
 EBVshrink(%):   1.9542E+00  3.6232E+00
 EPSshrink(%):   1.1632E+01
 

 SUBMODEL    2
 
 ETABAR:         1.3339E-02 -2.7101E-03
 SE:             2.1747E-02  2.4428E-02
 N:                     100         100
 
 ETAshrink(%):   1.0000E-10  1.0000E-10
 EBVshrink(%):   1.9814E+00  1.5980E-01
 EPSshrink(%):   1.4289E+01
 
 #TERE:
 Elapsed estimation time in seconds:     3.87
 Elapsed covariance time in seconds:     0.22
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9939.460       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.72E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.75E-02
 
 ETA2
+       -9.65E-03  3.92E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.24E-01  1.98E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.29E-02  1.73E-02  1.58E-02  2.81E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.34E-03
 
 ETA2
+        2.65E-03  3.00E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.41E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        9.96E-03
 
 ETA2
+        5.48E-02  7.58E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.69E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.67E-04
 
 TH 2
+       -4.18E-05  2.98E-04
 
 TH 3
+       -3.21E-05  6.03E-06  2.49E-04
 
 TH 4
+        1.78E-05 -9.39E-06 -7.46E-06  7.91E-04
 
 OM11
+       -3.80E-06  1.93E-06  2.25E-06  4.24E-07  1.89E-05
 
 OM12
+        1.76E-06  4.64E-06 -2.89E-06 -2.59E-06 -5.07E-06  7.04E-06
 
 OM22
+        8.28E-07 -4.42E-06 -8.40E-07  1.99E-05  4.82E-07 -3.08E-06  9.00E-06
 
 SG11
+        1.44E-07 -3.56E-07  1.61E-07 -5.18E-07  8.76E-08 -5.30E-08 -4.76E-08  1.17E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.29E-02
 
 TH 2
+       -1.87E-01  1.73E-02
 
 TH 3
+       -1.57E-01  2.21E-02  1.58E-02
 
 TH 4
+        4.90E-02 -1.93E-02 -1.68E-02  2.81E-02
 
 OM11
+       -6.77E-02  2.57E-02  3.28E-02  3.47E-03  4.34E-03
 
 OM12
+        5.12E-02  1.01E-01 -6.90E-02 -3.48E-02 -4.40E-01  2.65E-03
 
 OM22
+        2.14E-02 -8.53E-02 -1.77E-02  2.36E-01  3.70E-02 -3.87E-01  3.00E-03
 
 SG11
+        3.27E-02 -6.05E-02  2.98E-02 -5.39E-02  5.91E-02 -5.85E-02 -4.65E-02  3.41E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               ITERATIVE TWO STAGE                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.41E+03
 
 TH 2
+        8.83E+02  3.56E+03
 
 TH 3
+        7.78E+02 -3.28E-02  4.14E+03
 
 TH 4
+       -1.26E+02 -9.91E-03 -5.23E-03  1.35E+03
 
 OM11
+        7.18E+02 -1.03E+03  2.13E+02 -2.85E+02  6.81E+04
 
 OM12
+       -1.65E+03 -2.90E+03  2.04E+03 -1.09E+03  5.62E+04  2.18E+05
 
 OM22
+       -4.52E+02  7.80E+02  9.72E+02 -3.31E+03  1.55E+04  7.32E+04  1.44E+05
 
 SG11
+       -8.35E+03  9.56E+03 -5.51E+03  4.52E+03 -2.49E+04  7.24E+04  6.72E+04  8.73E+06
 
1
 
 
 #TBLN:      2
 #METH: Importance Sampling assisted by MAP Estimation
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            360
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
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
 EM OR BAYESIAN METHOD USED:              IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION (IMPMAP)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        1           
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 ITERATIONS (NITER):                      20          
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        300         
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-01   ,10.00000000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
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
   1   2   3
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -9944.69258177654 eff.=     303. Smpl.=     300. Fit.= 0.97891
 iteration            1 OBJ=  -9944.95931189589 eff.=     120. Smpl.=     300. Fit.= 0.78484
 iteration            2 OBJ=  -9944.60358224477 eff.=     120. Smpl.=     300. Fit.= 0.78380
 iteration            3 OBJ=  -9943.69857389434 eff.=     120. Smpl.=     300. Fit.= 0.78481
 iteration            4 OBJ=  -9946.47492774293 eff.=     121. Smpl.=     300. Fit.= 0.78483
 iteration            5 OBJ=  -9945.44777924387 eff.=     120. Smpl.=     300. Fit.= 0.78324
 iteration            6 OBJ=  -9944.87191000967 eff.=     120. Smpl.=     300. Fit.= 0.78354
 iteration            7 OBJ=  -9945.30451181485 eff.=     120. Smpl.=     300. Fit.= 0.78402
 iteration            8 OBJ=  -9946.40392411218 eff.=     120. Smpl.=     300. Fit.= 0.78382
 iteration            9 OBJ=  -9945.58041739084 eff.=     120. Smpl.=     300. Fit.= 0.78348
 iteration           10 OBJ=  -9946.78976647956 eff.=     120. Smpl.=     300. Fit.= 0.78398
 iteration           11 OBJ=  -9942.85188955006 eff.=     119. Smpl.=     300. Fit.= 0.78369
 Convergence achieved
 iteration           11 OBJ=  -9946.84639199909 eff.=     121. Smpl.=     300. Fit.= 0.78541
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.3303E-03  1.6164E-03
 SE:             1.4901E-02  1.1436E-02
 N:                     200         200
 
 ETAshrink(%):   3.0620E+00  1.8549E+01
 EBVshrink(%):   1.9628E+00  3.7106E+00
 EPSshrink(%):   1.1795E+01
 

 SUBMODEL    2
 
 ETABAR:         1.2684E-02 -2.5283E-03
 SE:             2.1724E-02  2.4457E-02
 N:                     100         100
 
 ETAshrink(%):   1.0000E-10  1.0000E-10
 EBVshrink(%):   1.9890E+00  1.6013E-01
 EPSshrink(%):   1.4469E+01
 
 #TERE:
 Elapsed estimation time in seconds:    26.43
 Elapsed covariance time in seconds:     2.45
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9946.846       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.74E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.75E-02
 
 ETA2
+       -9.80E-03  3.96E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.26E-01  1.99E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.29E-02  1.74E-02  1.59E-02  2.84E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.04E-03
 
 ETA2
+        2.74E-03  3.54E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.43E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        9.28E-03
 
 ETA2
+        5.82E-02  8.89E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.70E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.65E-04
 
 TH 2
+       -4.29E-05  3.03E-04
 
 TH 3
+       -3.08E-05  1.91E-06  2.54E-04
 
 TH 4
+        1.63E-05 -1.08E-05 -9.09E-07  8.08E-04
 
 OM11
+       -8.33E-09  1.80E-06 -1.59E-06  2.62E-06  1.63E-05
 
 OM12
+       -3.07E-07  5.95E-06 -4.66E-06 -2.37E-06 -3.55E-06  7.53E-06
 
 OM22
+        9.28E-07 -5.53E-06  2.88E-06  2.76E-05  8.28E-07 -3.22E-06  1.25E-05
 
 SG11
+        1.03E-07 -4.23E-07  2.92E-07 -4.65E-07 -2.50E-08 -4.28E-09 -1.33E-08  1.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.29E-02
 
 TH 2
+       -1.91E-01  1.74E-02
 
 TH 3
+       -1.50E-01  6.89E-03  1.59E-02
 
 TH 4
+        4.46E-02 -2.18E-02 -2.01E-03  2.84E-02
 
 OM11
+       -1.60E-04  2.55E-02 -2.46E-02  2.28E-02  4.04E-03
 
 OM12
+       -8.69E-03  1.25E-01 -1.07E-01 -3.04E-02 -3.20E-01  2.74E-03
 
 OM22
+        2.04E-02 -8.97E-02  5.10E-02  2.74E-01  5.79E-02 -3.31E-01  3.54E-03
 
 SG11
+        2.33E-02 -7.09E-02  5.35E-02 -4.77E-02 -1.80E-02 -4.55E-03 -1.10E-02  3.43E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.44E+03
 
 TH 2
+        8.95E+02  3.52E+03
 
 TH 3
+        7.78E+02 -3.02E-02  4.11E+03
 
 TH 4
+       -1.20E+02  2.47E+00 -7.15E-01  1.35E+03
 
 OM11
+       -9.75E+00 -1.04E+03  1.03E+03 -2.83E+02  6.90E+04
 
 OM12
+       -1.05E+01 -2.88E+03  2.91E+03 -1.09E+03  3.59E+04  1.72E+05
 
 OM22
+       -4.42E+00  8.24E+02 -3.31E+02 -3.21E+03  4.59E+03  4.21E+04  9.78E+04
 
 SG11
+       -4.81E+03  1.17E+04 -1.06E+04  4.99E+03  9.08E+03 -3.24E+03  4.70E+03  8.60E+06
 
1
 
 
 #TBLN:      3
 #METH: Importance Sampling
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            360
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
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
 ITERATIONS (NITER):                      20          
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        1000        
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-01   ,10.00000000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
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
   1   2   3
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -9945.48540034943 eff.=    1012. Smpl.=    1000. Fit.= 0.97899
 iteration            1 OBJ=  -9945.44770809829 eff.=     393. Smpl.=    1000. Fit.= 0.78715
 iteration            2 OBJ=  -9946.53831998960 eff.=     399. Smpl.=    1000. Fit.= 0.78987
 iteration            3 OBJ=  -9945.39101580428 eff.=     401. Smpl.=    1000. Fit.= 0.79093
 iteration            4 OBJ=  -9945.83697171254 eff.=     400. Smpl.=    1000. Fit.= 0.79042
 iteration            5 OBJ=  -9946.97745665386 eff.=     402. Smpl.=    1000. Fit.= 0.79096
 iteration            6 OBJ=  -9944.40624146609 eff.=     399. Smpl.=    1000. Fit.= 0.79041
 iteration            7 OBJ=  -9944.56321893573 eff.=     399. Smpl.=    1000. Fit.= 0.79045
 iteration            8 OBJ=  -9946.20074101187 eff.=     401. Smpl.=    1000. Fit.= 0.79112
 iteration            9 OBJ=  -9946.03841857502 eff.=     401. Smpl.=    1000. Fit.= 0.79146
 iteration           10 OBJ=  -9945.22912579909 eff.=     400. Smpl.=    1000. Fit.= 0.79095
 iteration           11 OBJ=  -9946.56522708733 eff.=     401. Smpl.=    1000. Fit.= 0.79114
 Convergence achieved
 iteration           11 OBJ=  -9943.55259471308 eff.=     400. Smpl.=    1000. Fit.= 0.79125
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.1687E-03  1.0141E-03
 SE:             1.4909E-02  1.1457E-02
 N:                     200         200
 
 ETAshrink(%):   3.0326E+00  1.8340E+01
 EBVshrink(%):   1.9757E+00  3.7594E+00
 EPSshrink(%):   1.1872E+01
 

 SUBMODEL    2
 
 ETABAR:         1.2546E-02 -2.5789E-03
 SE:             2.1724E-02  2.4453E-02
 N:                     100         100
 
 ETAshrink(%):   1.0000E-10  1.0000E-10
 EBVshrink(%):   2.0008E+00  1.6510E-01
 EPSshrink(%):   1.4560E+01
 
 #TERE:
 Elapsed estimation time in seconds:    81.58
 Elapsed covariance time in seconds:     7.46
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9943.553       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.74E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.75E-02
 
 ETA2
+       -9.77E-03  3.96E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.25E-01  1.99E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.29E-02  1.73E-02  1.60E-02  2.84E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.05E-03
 
 ETA2
+        2.73E-03  3.51E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.43E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        9.28E-03
 
 ETA2
+        5.81E-02  8.83E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.70E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.66E-04
 
 TH 2
+       -4.23E-05  3.01E-04
 
 TH 3
+       -3.11E-05  2.12E-06  2.55E-04
 
 TH 4
+        1.60E-05 -9.15E-06 -8.74E-07  8.07E-04
 
 OM11
+       -6.37E-08  1.86E-06 -1.61E-06  2.57E-06  1.64E-05
 
 OM12
+       -2.09E-07  5.72E-06 -4.57E-06 -2.22E-06 -3.52E-06  7.47E-06
 
 OM22
+        8.62E-07 -5.19E-06  2.89E-06  2.71E-05  8.15E-07 -3.16E-06  1.23E-05
 
 SG11
+        1.04E-07 -4.16E-07  2.88E-07 -4.39E-07 -2.49E-08 -4.85E-09 -1.09E-08  1.18E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.29E-02
 
 TH 2
+       -1.90E-01  1.73E-02
 
 TH 3
+       -1.51E-01  7.67E-03  1.60E-02
 
 TH 4
+        4.37E-02 -1.86E-02 -1.93E-03  2.84E-02
 
 OM11
+       -1.22E-03  2.65E-02 -2.50E-02  2.24E-02  4.05E-03
 
 OM12
+       -5.94E-03  1.21E-01 -1.05E-01 -2.86E-02 -3.19E-01  2.73E-03
 
 OM22
+        1.91E-02 -8.52E-02  5.15E-02  2.71E-01  5.73E-02 -3.29E-01  3.51E-03
 
 SG11
+        2.36E-02 -6.99E-02  5.25E-02 -4.50E-02 -1.79E-02 -5.17E-03 -9.03E-03  3.43E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                               IMPORTANCE SAMPLING                              ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.43E+03
 
 TH 2
+        8.90E+02  3.54E+03
 
 TH 3
+        7.81E+02 -2.99E-02  4.09E+03
 
 TH 4
+       -1.19E+02 -2.04E+00 -1.86E-02  1.35E+03
 
 OM11
+       -2.55E+00 -1.03E+03  1.02E+03 -2.80E+02  6.88E+04
 
 OM12
+       -7.25E+01 -2.83E+03  2.85E+03 -1.09E+03  3.57E+04  1.72E+05
 
 OM22
+       -2.01E+01  7.83E+02 -3.61E+02 -3.21E+03  4.55E+03  4.23E+04  9.90E+04
 
 SG11
+       -4.90E+03  1.15E+04 -1.04E+04  4.73E+03  9.26E+03 -2.41E+03  3.55E+03  8.59E+06
 
1
 
 
 #TBLN:      4
 #METH: Stochastic Approximation Expectation-Maximization
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            360
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
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
 EM OR BAYESIAN METHOD USED:              STOCHASTIC APPROXIMATION EXPECTATION MAXIMIZATION (SAEM)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        10          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              500         
 ITERATIONS (NITER):                      500         
 ANEAL SETTING (CONSTRAIN):               1           
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        2           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2           
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0           
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2           
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2           
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 Stochastic/Burn-in Mode
 iteration         -500 SAEMOBJ=  -13828.8992157534
 iteration         -490 SAEMOBJ=  -13578.8545655588
 iteration         -480 SAEMOBJ=  -13540.8091617356
 iteration         -470 SAEMOBJ=  -13552.2607663820
 iteration         -460 SAEMOBJ=  -13561.7325294866
 iteration         -450 SAEMOBJ=  -13600.9120225751
 iteration         -440 SAEMOBJ=  -13554.9646341016
 iteration         -430 SAEMOBJ=  -13568.9767176264
 iteration         -420 SAEMOBJ=  -13487.5547489765
 iteration         -410 SAEMOBJ=  -13543.3070096510
 iteration         -400 SAEMOBJ=  -13536.6397531435
 iteration         -390 SAEMOBJ=  -13573.9288727852
 Convergence achieved
 Reduced Stochastic/Accumulation Mode
 iteration            0 SAEMOBJ=  -13536.9295999924
 iteration           10 SAEMOBJ=  -13609.3483617746
 iteration           20 SAEMOBJ=  -13613.5449225240
 iteration           30 SAEMOBJ=  -13616.0169511707
 iteration           40 SAEMOBJ=  -13617.0048409321
 iteration           50 SAEMOBJ=  -13619.3339805515
 iteration           60 SAEMOBJ=  -13619.6824164675
 iteration           70 SAEMOBJ=  -13619.9099087059
 iteration           80 SAEMOBJ=  -13621.9187968356
 iteration           90 SAEMOBJ=  -13622.1794183761
 iteration          100 SAEMOBJ=  -13622.3314897787
 iteration          110 SAEMOBJ=  -13622.4071758672
 iteration          120 SAEMOBJ=  -13622.5779772042
 iteration          130 SAEMOBJ=  -13622.5603289156
 iteration          140 SAEMOBJ=  -13622.4743751563
 iteration          150 SAEMOBJ=  -13622.5234849542
 iteration          160 SAEMOBJ=  -13623.3330745359
 iteration          170 SAEMOBJ=  -13623.6230456938
 iteration          180 SAEMOBJ=  -13624.1013435557
 iteration          190 SAEMOBJ=  -13623.8927748212
 iteration          200 SAEMOBJ=  -13624.3194131699
 iteration          210 SAEMOBJ=  -13624.9724304159
 iteration          220 SAEMOBJ=  -13624.7933909905
 iteration          230 SAEMOBJ=  -13624.6945478911
 iteration          240 SAEMOBJ=  -13625.1878713369
 iteration          250 SAEMOBJ=  -13625.2196778212
 iteration          260 SAEMOBJ=  -13625.9318279584
 iteration          270 SAEMOBJ=  -13625.7444747888
 iteration          280 SAEMOBJ=  -13625.9629113099
 iteration          290 SAEMOBJ=  -13625.6345059722
 iteration          300 SAEMOBJ=  -13625.6167549780
 iteration          310 SAEMOBJ=  -13625.4296422229
 iteration          320 SAEMOBJ=  -13625.5123846764
 iteration          330 SAEMOBJ=  -13625.4739286228
 iteration          340 SAEMOBJ=  -13625.3182262057
 iteration          350 SAEMOBJ=  -13625.6135083344
 iteration          360 SAEMOBJ=  -13625.7360907136
 iteration          370 SAEMOBJ=  -13625.9237319763
 iteration          380 SAEMOBJ=  -13626.0244090517
 iteration          390 SAEMOBJ=  -13626.0399796789
 iteration          400 SAEMOBJ=  -13625.9430907463
 iteration          410 SAEMOBJ=  -13626.0525232220
 iteration          420 SAEMOBJ=  -13626.0731016746
 iteration          430 SAEMOBJ=  -13625.9076567608
 iteration          440 SAEMOBJ=  -13626.0513980260
 iteration          450 SAEMOBJ=  -13626.0866845235
 iteration          460 SAEMOBJ=  -13625.8906519069
 iteration          470 SAEMOBJ=  -13625.8638561867
 iteration          480 SAEMOBJ=  -13625.8881595087
 iteration          490 SAEMOBJ=  -13626.0128463637
 iteration          500 SAEMOBJ=  -13626.0308080584
 
 #TERM:
 STOCHASTIC PORTION WAS COMPLETED
 REDUCED STOCHASTIC PORTION WAS COMPLETED

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.4775E-03  1.3228E-03
 SE:             1.4927E-02  1.1452E-02
 N:                     200         200
 
 ETAshrink(%):   2.9819E+00  1.8338E+01
 EBVshrink(%):   1.9683E+00  3.7583E+00
 EPSshrink(%):   1.1953E+01
 

 SUBMODEL    2
 
 ETABAR:         1.2960E-02 -2.6545E-03
 SE:             2.1746E-02  2.4453E-02
 N:                     100         100
 
 ETAshrink(%):   1.0000E-10  1.0000E-10
 EBVshrink(%):   2.0225E+00  1.6683E-01
 EPSshrink(%):   1.4575E+01
 
 #TERE:
 Elapsed estimation time in seconds:    87.31
 Elapsed covariance time in seconds:     0.17
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 #OBJT:**************                        FINAL VALUE OF LIKELIHOOD FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -13626.031       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.74E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.76E-02
 
 ETA2
+       -9.75E-03  3.95E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.25E-01  1.99E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                          STANDARD ERROR OF ESTIMATE (S)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.29E-02  1.73E-02  1.59E-02  2.81E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.36E-03
 
 ETA2
+        2.68E-03  3.04E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.42E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        1.00E-02
 
 ETA2
+        5.49E-02  7.64E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.69E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (S)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.68E-04
 
 TH 2
+       -4.21E-05  3.00E-04
 
 TH 3
+       -3.27E-05  6.24E-06  2.53E-04
 
 TH 4
+        1.76E-05 -9.27E-06 -7.37E-06  7.89E-04
 
 OM11
+       -3.99E-06  1.98E-06  2.30E-06  3.56E-07  1.90E-05
 
 OM12
+        1.86E-06  4.76E-06 -2.82E-06 -2.50E-06 -5.19E-06  7.17E-06
 
 OM22
+        8.97E-07 -4.63E-06 -8.90E-07  1.98E-05  5.10E-07 -3.16E-06  9.22E-06
 
 SG11
+        1.32E-07 -4.22E-07  1.57E-07 -4.98E-07  9.05E-08 -5.43E-08 -4.66E-08  1.17E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (S)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.29E-02
 
 TH 2
+       -1.88E-01  1.73E-02
 
 TH 3
+       -1.59E-01  2.26E-02  1.59E-02
 
 TH 4
+        4.84E-02 -1.91E-02 -1.65E-02  2.81E-02
 
 OM11
+       -7.05E-02  2.62E-02  3.31E-02  2.91E-03  4.36E-03
 
 OM12
+        5.35E-02  1.03E-01 -6.61E-02 -3.33E-02 -4.44E-01  2.68E-03
 
 OM22
+        2.28E-02 -8.80E-02 -1.84E-02  2.32E-01  3.85E-02 -3.88E-01  3.04E-03
 
 SG11
+        2.98E-02 -7.11E-02  2.89E-02 -5.17E-02  6.06E-02 -5.93E-02 -4.49E-02  3.42E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                STOCHASTIC APPROXIMATION EXPECTATION-MAXIMIZATION               ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (S)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.40E+03
 
 TH 2
+        8.84E+02  3.54E+03
 
 TH 3
+        7.77E+02 -2.71E-02  4.07E+03
 
 TH 4
+       -1.23E+02 -4.27E-02 -1.83E-02  1.35E+03
 
 OM11
+        7.32E+02 -1.04E+03  1.90E+02 -2.80E+02  6.77E+04
 
 OM12
+       -1.73E+03 -2.89E+03  1.91E+03 -1.08E+03  5.60E+04  2.15E+05
 
 OM22
+       -5.10E+02  8.19E+02  9.34E+02 -3.22E+03  1.53E+04  7.21E+04  1.40E+05
 
 SG11
+       -7.17E+03  1.15E+04 -5.24E+03  4.31E+03 -2.63E+04  6.95E+04  6.59E+04  8.69E+06
 
1
 
 
 #TBLN:      5
 #METH: Objective Function Evaluation by Importance Sampling
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            360
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
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
 ITERATIONS (NITER):                      5           
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        1000        
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                YES
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-01   ,10.00000000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
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
   1   2   3
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=  -9945.08183961886 eff.=     981. Smpl.=    1000. Fit.= 0.95942
 iteration            1 OBJ=  -9945.24841978459 eff.=     404. Smpl.=    1000. Fit.= 0.79274
 iteration            2 OBJ=  -9945.53690939391 eff.=     399. Smpl.=    1000. Fit.= 0.79012
 iteration            3 OBJ=  -9945.89607028318 eff.=     400. Smpl.=    1000. Fit.= 0.79064
 iteration            4 OBJ=  -9945.83662514137 eff.=     401. Smpl.=    1000. Fit.= 0.79123
 iteration            5 OBJ=  -9946.97223651654 eff.=     401. Smpl.=    1000. Fit.= 0.79055
 
 #TERM:
 EXPECTATION ONLY PROCESS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -6.6407E-03  1.5671E-03
 SE:             1.4907E-02  1.1463E-02
 N:                     200         200
 
 ETAshrink(%):   3.1105E+00  1.8260E+01
 EBVshrink(%):   1.9687E+00  3.7492E+00
 EPSshrink(%):   1.1972E+01
 

 SUBMODEL    2
 
 ETABAR:         1.2229E-02 -2.5131E-03
 SE:             2.1731E-02  2.4458E-02
 N:                     100         100
 
 ETAshrink(%):   1.0000E-10  1.0000E-10
 EBVshrink(%):   1.9874E+00  1.6368E-01
 EPSshrink(%):   1.4651E+01
 
 #TERE:
 Elapsed estimation time in seconds:    33.03
 Elapsed covariance time in seconds:     7.54
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9946.972       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.74E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.76E-02
 
 ETA2
+       -9.75E-03  3.95E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.25E-01  1.99E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.29E-02  1.73E-02  1.60E-02  2.84E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.06E-03
 
 ETA2
+        2.73E-03  3.50E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.44E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        9.30E-03
 
 ETA2
+        5.81E-02  8.81E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.70E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.66E-04
 
 TH 2
+       -4.21E-05  3.00E-04
 
 TH 3
+       -3.12E-05  2.14E-06  2.55E-04
 
 TH 4
+        1.61E-05 -1.01E-05 -1.25E-06  8.07E-04
 
 OM11
+        9.08E-08  1.87E-06 -1.69E-06  2.64E-06  1.65E-05
 
 OM12
+       -2.94E-07  5.77E-06 -4.47E-06 -2.22E-06 -3.50E-06  7.47E-06
 
 OM22
+        8.89E-07 -5.26E-06  2.71E-06  2.70E-05  8.07E-07 -3.13E-06  1.23E-05
 
 SG11
+        1.04E-07 -4.13E-07  2.89E-07 -4.60E-07 -3.19E-08 -3.29E-09 -1.67E-08  1.19E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.29E-02
 
 TH 2
+       -1.89E-01  1.73E-02
 
 TH 3
+       -1.52E-01  7.76E-03  1.60E-02
 
 TH 4
+        4.42E-02 -2.06E-02 -2.75E-03  2.84E-02
 
 OM11
+        1.74E-03  2.66E-02 -2.62E-02  2.29E-02  4.06E-03
 
 OM12
+       -8.35E-03  1.22E-01 -1.03E-01 -2.86E-02 -3.16E-01  2.73E-03
 
 OM22
+        1.97E-02 -8.68E-02  4.85E-02  2.71E-01  5.68E-02 -3.27E-01  3.50E-03
 
 SG11
+        2.35E-02 -6.93E-02  5.26E-02 -4.70E-02 -2.29E-02 -3.50E-03 -1.39E-02  3.44E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               OBJECTIVE FUNCTION EVALUATION BY IMPORTANCE SAMPLING             ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.42E+03
 
 TH 2
+        8.84E+02  3.55E+03
 
 TH 3
+        7.82E+02 -2.97E-02  4.09E+03
 
 TH 4
+       -1.19E+02  1.64E+00  4.82E-02  1.35E+03
 
 OM11
+       -5.12E+01 -1.03E+03  1.01E+03 -2.79E+02  6.84E+04
 
 OM12
+       -2.76E+01 -2.85E+03  2.82E+03 -1.08E+03  3.53E+04  1.72E+05
 
 OM22
+       -8.09E+00  8.12E+02 -3.24E+02 -3.21E+03  4.47E+03  4.20E+04  9.95E+04
 
 SG11
+       -4.93E+03  1.13E+04 -1.04E+04  4.78E+03  1.29E+04 -7.55E+02  7.58E+03  8.53E+06
 
1
 
 
 #TBLN:      6
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            360
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
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
 EM OR BAYESIAN METHOD USED:              MCMC BAYESIAN (BAYES)
 MU MODELING PATTERN (MUM):               
 GRADIENT/GIBBS PATTERN (GRD):            
 AUTOMATIC SETTING FEATURE (AUTO):        OFF
 CONVERGENCE TYPE (CTYPE):                3           
 CONVERGENCE INTERVAL (CINTERVAL):        10          
 CONVERGENCE ITERATIONS (CITER):          10          
 CONVERGENCE ALPHA ERROR (CALPHA):        5.000000000000000E-02   
 BURN-IN ITERATIONS (NBURN):              2000        
 ITERATIONS (NITER):                      5000        
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        1           
 RANDOM SAMPLING METHOD (RANMETHOD):      
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   1.000000000000000E-06   ,1000000.000000000       
 SAMPLE ACCEPTANCE RATE (IACCEPT):        0.400000000000000       
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2           
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0           
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2           
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2           
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS 
 SAMPLED THETAS AND SIGMAS: 
 PROPOSAL DENSITY SCALING RANGE 
              (PSCALE_MIN, PSCALE_MAX):   1.000000000000000E-02   ,1000.000000000000       
 SAMPLE ACCEPTANCE RATE (PACCEPT):                       0.500000000000000       
 SAMPLES FOR GLOBAL SEARCH KERNEL (PSAMPLE_M1):          1           
 SAMPLES FOR LOCAL SEARCH KERNEL (PSAMPLE_M2):           -1          
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (PSAMPLE_M3):       1           
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS     
 SAMPLED OMEGAS: 
 SAMPLE ACCEPTANCE RATE (OACCEPT):                       0.500000000000000       
 SAMPLES FOR GLOBAL SEARCH KERNEL (OSAMPLE_M1):          -1          
 SAMPLES FOR LOCAL SEARCH KERNEL (OSAMPLE_M2):           -1          
 
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
 IWRS=IWRESI
 IPRD=IPREDI
 IRS=IRESI
 
 EM/BAYES SETUP:
 THETAS THAT ARE MU MODELED:
   1   2   3
 THETAS THAT MODEL MIXTURE PROPORTIONS:
   4
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
   4
 SIGMAS THAT ARE GIBBS SAMPLED:
   1
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -2000 MCMCOBJ=   -13800.1063296372     
 iteration        -1990 MCMCOBJ=   -13455.5869401161     
 iteration        -1980 MCMCOBJ=   -13386.7779111563     
 iteration        -1970 MCMCOBJ=   -13414.1658120184     
 iteration        -1960 MCMCOBJ=   -13402.3468941082     
 iteration        -1950 MCMCOBJ=   -13409.7756430002     
 iteration        -1940 MCMCOBJ=   -13465.3004134976     
 iteration        -1930 MCMCOBJ=   -13363.9652063147     
 iteration        -1920 MCMCOBJ=   -13409.0658999734     
 iteration        -1910 MCMCOBJ=   -13458.0702658179     
 iteration        -1900 MCMCOBJ=   -13337.1204278745     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -13384.9396627704     
 iteration           10 MCMCOBJ=   -13403.1364531180     
 iteration           20 MCMCOBJ=   -13423.9068800842     
 iteration           30 MCMCOBJ=   -13355.2582518477     
 iteration           40 MCMCOBJ=   -13421.3830735891     
 iteration           50 MCMCOBJ=   -13426.4293889180     
 iteration           60 MCMCOBJ=   -13422.1992339841     
 iteration           70 MCMCOBJ=   -13427.7416342622     
 iteration           80 MCMCOBJ=   -13472.5504342886     
 iteration           90 MCMCOBJ=   -13418.0412191541     
 iteration          100 MCMCOBJ=   -13488.9759533032     
 iteration          110 MCMCOBJ=   -13403.9718072903     
 iteration          120 MCMCOBJ=   -13384.9539633303     
 iteration          130 MCMCOBJ=   -13474.6016011387     
 iteration          140 MCMCOBJ=   -13471.6377747884     
 iteration          150 MCMCOBJ=   -13424.6795552710     
 iteration          160 MCMCOBJ=   -13458.4296303215     
 iteration          170 MCMCOBJ=   -13374.9460206834     
 iteration          180 MCMCOBJ=   -13435.5040939466     
 iteration          190 MCMCOBJ=   -13410.6223692033     
 iteration          200 MCMCOBJ=   -13445.5045031183     
 iteration          210 MCMCOBJ=   -13478.1658206388     
 iteration          220 MCMCOBJ=   -13405.5184524467     
 iteration          230 MCMCOBJ=   -13385.2033579188     
 iteration          240 MCMCOBJ=   -13457.7387752669     
 iteration          250 MCMCOBJ=   -13449.4472610572     
 iteration          260 MCMCOBJ=   -13453.7970601661     
 iteration          270 MCMCOBJ=   -13467.8921037774     
 iteration          280 MCMCOBJ=   -13421.5498614107     
 iteration          290 MCMCOBJ=   -13421.1487095698     
 iteration          300 MCMCOBJ=   -13421.0271743655     
 iteration          310 MCMCOBJ=   -13422.9098598201     
 iteration          320 MCMCOBJ=   -13422.2236207320     
 iteration          330 MCMCOBJ=   -13474.9533148245     
 iteration          340 MCMCOBJ=   -13464.2833195198     
 iteration          350 MCMCOBJ=   -13459.7101854799     
 iteration          360 MCMCOBJ=   -13415.8594616609     
 iteration          370 MCMCOBJ=   -13380.1243122336     
 iteration          380 MCMCOBJ=   -13443.1499541200     
 iteration          390 MCMCOBJ=   -13356.3159590347     
 iteration          400 MCMCOBJ=   -13433.6143704904     
 iteration          410 MCMCOBJ=   -13457.9053959892     
 iteration          420 MCMCOBJ=   -13420.7112367649     
 iteration          430 MCMCOBJ=   -13389.2974300560     
 iteration          440 MCMCOBJ=   -13418.1297543561     
 iteration          450 MCMCOBJ=   -13433.6313445649     
 iteration          460 MCMCOBJ=   -13408.6650181985     
 iteration          470 MCMCOBJ=   -13437.3309602041     
 iteration          480 MCMCOBJ=   -13473.6326538028     
 iteration          490 MCMCOBJ=   -13496.5153755911     
 iteration          500 MCMCOBJ=   -13448.7703783885     
 iteration          510 MCMCOBJ=   -13458.7236352938     
 iteration          520 MCMCOBJ=   -13494.5438086145     
 iteration          530 MCMCOBJ=   -13470.6490758233     
 iteration          540 MCMCOBJ=   -13401.4433688865     
 iteration          550 MCMCOBJ=   -13450.5645859972     
 iteration          560 MCMCOBJ=   -13435.4348359145     
 iteration          570 MCMCOBJ=   -13467.2230391043     
 iteration          580 MCMCOBJ=   -13447.2308068858     
 iteration          590 MCMCOBJ=   -13371.5171036375     
 iteration          600 MCMCOBJ=   -13495.3355861086     
 iteration          610 MCMCOBJ=   -13436.5481806050     
 iteration          620 MCMCOBJ=   -13370.6343155887     
 iteration          630 MCMCOBJ=   -13459.7220116102     
 iteration          640 MCMCOBJ=   -13459.5462410921     
 iteration          650 MCMCOBJ=   -13470.4659690055     
 iteration          660 MCMCOBJ=   -13415.4507793486     
 iteration          670 MCMCOBJ=   -13454.2778254511     
 iteration          680 MCMCOBJ=   -13439.4247529190     
 iteration          690 MCMCOBJ=   -13480.1038930653     
 iteration          700 MCMCOBJ=   -13323.5717822385     
 iteration          710 MCMCOBJ=   -13459.9670439004     
 iteration          720 MCMCOBJ=   -13458.4043787942     
 iteration          730 MCMCOBJ=   -13472.4449543595     
 iteration          740 MCMCOBJ=   -13452.7986127042     
 iteration          750 MCMCOBJ=   -13474.6054907072     
 iteration          760 MCMCOBJ=   -13471.0696598726     
 iteration          770 MCMCOBJ=   -13371.5418879943     
 iteration          780 MCMCOBJ=   -13453.4836420432     
 iteration          790 MCMCOBJ=   -13457.6160246165     
 iteration          800 MCMCOBJ=   -13479.6103017267     
 iteration          810 MCMCOBJ=   -13400.0368639376     
 iteration          820 MCMCOBJ=   -13358.5182868429     
 iteration          830 MCMCOBJ=   -13444.9576014535     
 iteration          840 MCMCOBJ=   -13436.1520586157     
 iteration          850 MCMCOBJ=   -13457.3128534260     
 iteration          860 MCMCOBJ=   -13442.4354303529     
 iteration          870 MCMCOBJ=   -13445.7690171502     
 iteration          880 MCMCOBJ=   -13447.0950572035     
 iteration          890 MCMCOBJ=   -13476.2495482759     
 iteration          900 MCMCOBJ=   -13485.3682898239     
 iteration          910 MCMCOBJ=   -13428.1324979814     
 iteration          920 MCMCOBJ=   -13467.9833284750     
 iteration          930 MCMCOBJ=   -13434.0146289475     
 iteration          940 MCMCOBJ=   -13438.5689337168     
 iteration          950 MCMCOBJ=   -13443.5732082946     
 iteration          960 MCMCOBJ=   -13402.8836486956     
 iteration          970 MCMCOBJ=   -13373.6250631983     
 iteration          980 MCMCOBJ=   -13413.9969923945     
 iteration          990 MCMCOBJ=   -13532.3406564790     
 iteration         1000 MCMCOBJ=   -13433.2363920460     
 iteration         1010 MCMCOBJ=   -13476.6157980663     
 iteration         1020 MCMCOBJ=   -13388.7733277714     
 iteration         1030 MCMCOBJ=   -13429.1064433547     
 iteration         1040 MCMCOBJ=   -13352.6950831860     
 iteration         1050 MCMCOBJ=   -13440.9679094219     
 iteration         1060 MCMCOBJ=   -13396.5324298172     
 iteration         1070 MCMCOBJ=   -13411.1607995275     
 iteration         1080 MCMCOBJ=   -13424.7180485637     
 iteration         1090 MCMCOBJ=   -13411.0635896075     
 iteration         1100 MCMCOBJ=   -13353.1232566288     
 iteration         1110 MCMCOBJ=   -13472.4181113606     
 iteration         1120 MCMCOBJ=   -13422.4702672728     
 iteration         1130 MCMCOBJ=   -13415.6879785550     
 iteration         1140 MCMCOBJ=   -13508.0063738872     
 iteration         1150 MCMCOBJ=   -13370.9658178614     
 iteration         1160 MCMCOBJ=   -13418.2907333741     
 iteration         1170 MCMCOBJ=   -13343.6762837633     
 iteration         1180 MCMCOBJ=   -13338.7414077355     
 iteration         1190 MCMCOBJ=   -13476.7684248263     
 iteration         1200 MCMCOBJ=   -13485.9484563236     
 iteration         1210 MCMCOBJ=   -13506.5463044757     
 iteration         1220 MCMCOBJ=   -13525.2436750578     
 iteration         1230 MCMCOBJ=   -13456.6428634071     
 iteration         1240 MCMCOBJ=   -13447.7339598133     
 iteration         1250 MCMCOBJ=   -13505.0336128473     
 iteration         1260 MCMCOBJ=   -13376.8721133291     
 iteration         1270 MCMCOBJ=   -13434.5721376323     
 iteration         1280 MCMCOBJ=   -13424.3328907427     
 iteration         1290 MCMCOBJ=   -13423.4949934173     
 iteration         1300 MCMCOBJ=   -13430.4674689752     
 iteration         1310 MCMCOBJ=   -13475.4175483384     
 iteration         1320 MCMCOBJ=   -13413.4942734351     
 iteration         1330 MCMCOBJ=   -13441.5085036899     
 iteration         1340 MCMCOBJ=   -13394.8341542101     
 iteration         1350 MCMCOBJ=   -13443.4176197746     
 iteration         1360 MCMCOBJ=   -13380.3485038304     
 iteration         1370 MCMCOBJ=   -13445.5708470341     
 iteration         1380 MCMCOBJ=   -13474.3010926449     
 iteration         1390 MCMCOBJ=   -13348.1806954940     
 iteration         1400 MCMCOBJ=   -13407.5105344227     
 iteration         1410 MCMCOBJ=   -13428.9276481474     
 iteration         1420 MCMCOBJ=   -13408.9427126034     
 iteration         1430 MCMCOBJ=   -13436.7404828489     
 iteration         1440 MCMCOBJ=   -13442.5431855357     
 iteration         1450 MCMCOBJ=   -13472.2452824071     
 iteration         1460 MCMCOBJ=   -13436.1119022191     
 iteration         1470 MCMCOBJ=   -13459.8375298905     
 iteration         1480 MCMCOBJ=   -13436.7134627800     
 iteration         1490 MCMCOBJ=   -13424.6978526605     
 iteration         1500 MCMCOBJ=   -13476.2923396718     
 iteration         1510 MCMCOBJ=   -13346.4662522449     
 iteration         1520 MCMCOBJ=   -13338.7692595361     
 iteration         1530 MCMCOBJ=   -13458.2660961911     
 iteration         1540 MCMCOBJ=   -13416.5943236334     
 iteration         1550 MCMCOBJ=   -13428.4677594808     
 iteration         1560 MCMCOBJ=   -13483.6696623329     
 iteration         1570 MCMCOBJ=   -13459.4890469644     
 iteration         1580 MCMCOBJ=   -13452.0038206464     
 iteration         1590 MCMCOBJ=   -13520.8833684191     
 iteration         1600 MCMCOBJ=   -13393.1439035104     
 iteration         1610 MCMCOBJ=   -13453.6373277428     
 iteration         1620 MCMCOBJ=   -13453.3995919741     
 iteration         1630 MCMCOBJ=   -13523.5699391590     
 iteration         1640 MCMCOBJ=   -13391.7923579586     
 iteration         1650 MCMCOBJ=   -13377.2084213760     
 iteration         1660 MCMCOBJ=   -13529.2672635069     
 iteration         1670 MCMCOBJ=   -13398.3274881534     
 iteration         1680 MCMCOBJ=   -13438.9391576453     
 iteration         1690 MCMCOBJ=   -13401.4312514584     
 iteration         1700 MCMCOBJ=   -13440.0728114303     
 iteration         1710 MCMCOBJ=   -13456.6964358112     
 iteration         1720 MCMCOBJ=   -13475.6059138463     
 iteration         1730 MCMCOBJ=   -13413.8652428159     
 iteration         1740 MCMCOBJ=   -13404.2608678578     
 iteration         1750 MCMCOBJ=   -13461.0485441971     
 iteration         1760 MCMCOBJ=   -13489.2977785296     
 iteration         1770 MCMCOBJ=   -13424.9287398488     
 iteration         1780 MCMCOBJ=   -13466.9087246625     
 iteration         1790 MCMCOBJ=   -13539.5778575069     
 iteration         1800 MCMCOBJ=   -13413.5423476117     
 iteration         1810 MCMCOBJ=   -13488.4385764245     
 iteration         1820 MCMCOBJ=   -13437.5776781271     
 iteration         1830 MCMCOBJ=   -13425.8058850140     
 iteration         1840 MCMCOBJ=   -13456.9253177798     
 iteration         1850 MCMCOBJ=   -13464.4699469310     
 iteration         1860 MCMCOBJ=   -13395.7231449185     
 iteration         1870 MCMCOBJ=   -13470.6759300691     
 iteration         1880 MCMCOBJ=   -13463.6130209067     
 iteration         1890 MCMCOBJ=   -13417.8020977063     
 iteration         1900 MCMCOBJ=   -13482.8452798353     
 iteration         1910 MCMCOBJ=   -13393.0973281134     
 iteration         1920 MCMCOBJ=   -13434.6329939221     
 iteration         1930 MCMCOBJ=   -13406.5408502825     
 iteration         1940 MCMCOBJ=   -13424.7753428968     
 iteration         1950 MCMCOBJ=   -13479.8531872614     
 iteration         1960 MCMCOBJ=   -13412.9955587235     
 iteration         1970 MCMCOBJ=   -13396.8206881426     
 iteration         1980 MCMCOBJ=   -13416.5447081523     
 iteration         1990 MCMCOBJ=   -13399.1353339433     
 iteration         2000 MCMCOBJ=   -13369.9209533126     
 iteration         2010 MCMCOBJ=   -13418.8018717027     
 iteration         2020 MCMCOBJ=   -13397.0203426974     
 iteration         2030 MCMCOBJ=   -13373.8637157460     
 iteration         2040 MCMCOBJ=   -13378.1451328975     
 iteration         2050 MCMCOBJ=   -13419.7352958913     
 iteration         2060 MCMCOBJ=   -13486.1027672169     
 iteration         2070 MCMCOBJ=   -13471.2877726829     
 iteration         2080 MCMCOBJ=   -13508.7623272528     
 iteration         2090 MCMCOBJ=   -13442.5460177860     
 iteration         2100 MCMCOBJ=   -13453.4135022454     
 iteration         2110 MCMCOBJ=   -13361.7055427642     
 iteration         2120 MCMCOBJ=   -13413.0669761015     
 iteration         2130 MCMCOBJ=   -13404.2441371883     
 iteration         2140 MCMCOBJ=   -13460.3282998067     
 iteration         2150 MCMCOBJ=   -13494.9510337660     
 iteration         2160 MCMCOBJ=   -13430.3124490547     
 iteration         2170 MCMCOBJ=   -13436.3452676070     
 iteration         2180 MCMCOBJ=   -13434.3722687518     
 iteration         2190 MCMCOBJ=   -13456.9922601419     
 iteration         2200 MCMCOBJ=   -13427.2327624098     
 iteration         2210 MCMCOBJ=   -13390.5271023594     
 iteration         2220 MCMCOBJ=   -13361.8720065628     
 iteration         2230 MCMCOBJ=   -13435.2186550367     
 iteration         2240 MCMCOBJ=   -13486.8035047365     
 iteration         2250 MCMCOBJ=   -13360.3899038272     
 iteration         2260 MCMCOBJ=   -13508.2732362406     
 iteration         2270 MCMCOBJ=   -13415.5076544753     
 iteration         2280 MCMCOBJ=   -13455.4769017980     
 iteration         2290 MCMCOBJ=   -13389.4731987744     
 iteration         2300 MCMCOBJ=   -13475.3757155337     
 iteration         2310 MCMCOBJ=   -13394.4416242641     
 iteration         2320 MCMCOBJ=   -13404.0344350016     
 iteration         2330 MCMCOBJ=   -13405.2210365220     
 iteration         2340 MCMCOBJ=   -13408.9934475056     
 iteration         2350 MCMCOBJ=   -13460.2636878633     
 iteration         2360 MCMCOBJ=   -13484.6216130502     
 iteration         2370 MCMCOBJ=   -13390.3109599506     
 iteration         2380 MCMCOBJ=   -13406.1997818825     
 iteration         2390 MCMCOBJ=   -13449.9426706642     
 iteration         2400 MCMCOBJ=   -13444.7217110562     
 iteration         2410 MCMCOBJ=   -13451.7225323812     
 iteration         2420 MCMCOBJ=   -13435.7564134071     
 iteration         2430 MCMCOBJ=   -13419.3278558473     
 iteration         2440 MCMCOBJ=   -13325.8879655716     
 iteration         2450 MCMCOBJ=   -13388.0343554777     
 iteration         2460 MCMCOBJ=   -13400.0824041316     
 iteration         2470 MCMCOBJ=   -13407.4208979117     
 iteration         2480 MCMCOBJ=   -13434.4886584453     
 iteration         2490 MCMCOBJ=   -13492.0753539303     
 iteration         2500 MCMCOBJ=   -13468.1842535424     
 iteration         2510 MCMCOBJ=   -13403.4658496279     
 iteration         2520 MCMCOBJ=   -13416.5043011618     
 iteration         2530 MCMCOBJ=   -13443.3874994322     
 iteration         2540 MCMCOBJ=   -13420.0290310015     
 iteration         2550 MCMCOBJ=   -13466.5583625441     
 iteration         2560 MCMCOBJ=   -13444.7029164977     
 iteration         2570 MCMCOBJ=   -13404.0468287509     
 iteration         2580 MCMCOBJ=   -13336.2881804687     
 iteration         2590 MCMCOBJ=   -13424.6445920067     
 iteration         2600 MCMCOBJ=   -13425.1259702265     
 iteration         2610 MCMCOBJ=   -13439.4620323428     
 iteration         2620 MCMCOBJ=   -13389.9140967411     
 iteration         2630 MCMCOBJ=   -13445.3219222954     
 iteration         2640 MCMCOBJ=   -13445.6437376321     
 iteration         2650 MCMCOBJ=   -13386.6240166928     
 iteration         2660 MCMCOBJ=   -13433.7219698231     
 iteration         2670 MCMCOBJ=   -13512.7360763544     
 iteration         2680 MCMCOBJ=   -13431.5341538199     
 iteration         2690 MCMCOBJ=   -13449.6143808354     
 iteration         2700 MCMCOBJ=   -13444.6810566346     
 iteration         2710 MCMCOBJ=   -13483.9736943339     
 iteration         2720 MCMCOBJ=   -13332.8344332807     
 iteration         2730 MCMCOBJ=   -13417.1469051050     
 iteration         2740 MCMCOBJ=   -13316.4177689333     
 iteration         2750 MCMCOBJ=   -13418.7744610001     
 iteration         2760 MCMCOBJ=   -13412.4600217894     
 iteration         2770 MCMCOBJ=   -13442.4785188878     
 iteration         2780 MCMCOBJ=   -13413.2180209048     
 iteration         2790 MCMCOBJ=   -13413.6354070022     
 iteration         2800 MCMCOBJ=   -13394.5874131950     
 iteration         2810 MCMCOBJ=   -13428.0273158418     
 iteration         2820 MCMCOBJ=   -13472.4990799246     
 iteration         2830 MCMCOBJ=   -13469.1667221467     
 iteration         2840 MCMCOBJ=   -13361.6439022173     
 iteration         2850 MCMCOBJ=   -13428.7691464948     
 iteration         2860 MCMCOBJ=   -13476.2800700819     
 iteration         2870 MCMCOBJ=   -13436.1846955885     
 iteration         2880 MCMCOBJ=   -13453.3502422408     
 iteration         2890 MCMCOBJ=   -13418.1692584804     
 iteration         2900 MCMCOBJ=   -13408.3878625901     
 iteration         2910 MCMCOBJ=   -13486.0552629339     
 iteration         2920 MCMCOBJ=   -13379.3785424148     
 iteration         2930 MCMCOBJ=   -13494.2486037311     
 iteration         2940 MCMCOBJ=   -13452.9617467374     
 iteration         2950 MCMCOBJ=   -13379.7707359080     
 iteration         2960 MCMCOBJ=   -13468.4924509945     
 iteration         2970 MCMCOBJ=   -13400.0468087425     
 iteration         2980 MCMCOBJ=   -13403.9320251007     
 iteration         2990 MCMCOBJ=   -13459.9125588863     
 iteration         3000 MCMCOBJ=   -13437.2893384733     
 iteration         3010 MCMCOBJ=   -13413.1903259939     
 iteration         3020 MCMCOBJ=   -13466.3340327883     
 iteration         3030 MCMCOBJ=   -13416.1317976210     
 iteration         3040 MCMCOBJ=   -13399.1705822218     
 iteration         3050 MCMCOBJ=   -13390.4959872011     
 iteration         3060 MCMCOBJ=   -13448.9454748809     
 iteration         3070 MCMCOBJ=   -13424.5405392203     
 iteration         3080 MCMCOBJ=   -13432.5247067089     
 iteration         3090 MCMCOBJ=   -13383.0163317181     
 iteration         3100 MCMCOBJ=   -13447.9434433586     
 iteration         3110 MCMCOBJ=   -13403.1243073791     
 iteration         3120 MCMCOBJ=   -13429.8911963740     
 iteration         3130 MCMCOBJ=   -13445.9757287788     
 iteration         3140 MCMCOBJ=   -13408.8642955515     
 iteration         3150 MCMCOBJ=   -13451.8342337028     
 iteration         3160 MCMCOBJ=   -13478.8561769326     
 iteration         3170 MCMCOBJ=   -13522.8543805282     
 iteration         3180 MCMCOBJ=   -13358.0573107269     
 iteration         3190 MCMCOBJ=   -13431.5936593480     
 iteration         3200 MCMCOBJ=   -13472.6728864274     
 iteration         3210 MCMCOBJ=   -13459.7791282461     
 iteration         3220 MCMCOBJ=   -13454.5265924671     
 iteration         3230 MCMCOBJ=   -13441.2776500587     
 iteration         3240 MCMCOBJ=   -13377.3244297782     
 iteration         3250 MCMCOBJ=   -13469.3592817467     
 iteration         3260 MCMCOBJ=   -13462.3692569790     
 iteration         3270 MCMCOBJ=   -13429.2192148175     
 iteration         3280 MCMCOBJ=   -13387.5687673241     
 iteration         3290 MCMCOBJ=   -13465.2361930783     
 iteration         3300 MCMCOBJ=   -13420.6184388451     
 iteration         3310 MCMCOBJ=   -13449.4171061314     
 iteration         3320 MCMCOBJ=   -13436.8634680476     
 iteration         3330 MCMCOBJ=   -13435.6979061497     
 iteration         3340 MCMCOBJ=   -13409.1637178473     
 iteration         3350 MCMCOBJ=   -13430.2055566122     
 iteration         3360 MCMCOBJ=   -13429.3773383960     
 iteration         3370 MCMCOBJ=   -13458.5022611424     
 iteration         3380 MCMCOBJ=   -13415.6643392629     
 iteration         3390 MCMCOBJ=   -13401.9791904184     
 iteration         3400 MCMCOBJ=   -13498.6554870804     
 iteration         3410 MCMCOBJ=   -13416.3054068095     
 iteration         3420 MCMCOBJ=   -13401.1968933639     
 iteration         3430 MCMCOBJ=   -13486.5441624735     
 iteration         3440 MCMCOBJ=   -13457.2118921562     
 iteration         3450 MCMCOBJ=   -13425.9992826831     
 iteration         3460 MCMCOBJ=   -13436.0567746894     
 iteration         3470 MCMCOBJ=   -13384.0385028203     
 iteration         3480 MCMCOBJ=   -13438.4430142698     
 iteration         3490 MCMCOBJ=   -13534.4550589711     
 iteration         3500 MCMCOBJ=   -13434.5124626891     
 iteration         3510 MCMCOBJ=   -13371.5989491228     
 iteration         3520 MCMCOBJ=   -13420.3615479011     
 iteration         3530 MCMCOBJ=   -13427.8042029501     
 iteration         3540 MCMCOBJ=   -13457.0347231057     
 iteration         3550 MCMCOBJ=   -13417.2801933165     
 iteration         3560 MCMCOBJ=   -13475.8072200994     
 iteration         3570 MCMCOBJ=   -13393.0557398417     
 iteration         3580 MCMCOBJ=   -13442.8197297546     
 iteration         3590 MCMCOBJ=   -13455.8207780808     
 iteration         3600 MCMCOBJ=   -13485.7680904013     
 iteration         3610 MCMCOBJ=   -13435.0749618421     
 iteration         3620 MCMCOBJ=   -13440.3267960147     
 iteration         3630 MCMCOBJ=   -13483.1620741059     
 iteration         3640 MCMCOBJ=   -13429.8894177365     
 iteration         3650 MCMCOBJ=   -13350.6211974368     
 iteration         3660 MCMCOBJ=   -13451.4134835609     
 iteration         3670 MCMCOBJ=   -13471.8754033151     
 iteration         3680 MCMCOBJ=   -13431.4544376115     
 iteration         3690 MCMCOBJ=   -13491.5195098613     
 iteration         3700 MCMCOBJ=   -13436.0505039594     
 iteration         3710 MCMCOBJ=   -13457.7328957296     
 iteration         3720 MCMCOBJ=   -13514.6118701762     
 iteration         3730 MCMCOBJ=   -13408.1311855942     
 iteration         3740 MCMCOBJ=   -13460.1200179087     
 iteration         3750 MCMCOBJ=   -13442.4465893366     
 iteration         3760 MCMCOBJ=   -13484.1975846829     
 iteration         3770 MCMCOBJ=   -13430.7873194681     
 iteration         3780 MCMCOBJ=   -13459.4600496263     
 iteration         3790 MCMCOBJ=   -13462.2488484697     
 iteration         3800 MCMCOBJ=   -13492.4685032167     
 iteration         3810 MCMCOBJ=   -13489.1823338139     
 iteration         3820 MCMCOBJ=   -13382.3113655482     
 iteration         3830 MCMCOBJ=   -13435.3504143419     
 iteration         3840 MCMCOBJ=   -13429.4521095985     
 iteration         3850 MCMCOBJ=   -13450.3402660452     
 iteration         3860 MCMCOBJ=   -13409.3435872171     
 iteration         3870 MCMCOBJ=   -13462.7030962838     
 iteration         3880 MCMCOBJ=   -13412.3007770678     
 iteration         3890 MCMCOBJ=   -13411.6902468330     
 iteration         3900 MCMCOBJ=   -13367.8355791921     
 iteration         3910 MCMCOBJ=   -13382.1339692560     
 iteration         3920 MCMCOBJ=   -13381.5212173639     
 iteration         3930 MCMCOBJ=   -13481.3444760774     
 iteration         3940 MCMCOBJ=   -13412.1656033178     
 iteration         3950 MCMCOBJ=   -13407.5411856487     
 iteration         3960 MCMCOBJ=   -13386.4619486563     
 iteration         3970 MCMCOBJ=   -13417.5817460259     
 iteration         3980 MCMCOBJ=   -13375.7919191572     
 iteration         3990 MCMCOBJ=   -13410.2967200633     
 iteration         4000 MCMCOBJ=   -13349.5927022936     
 iteration         4010 MCMCOBJ=   -13408.1199491493     
 iteration         4020 MCMCOBJ=   -13459.5092713316     
 iteration         4030 MCMCOBJ=   -13439.6366079517     
 iteration         4040 MCMCOBJ=   -13525.5594854441     
 iteration         4050 MCMCOBJ=   -13335.6441953240     
 iteration         4060 MCMCOBJ=   -13467.2724980216     
 iteration         4070 MCMCOBJ=   -13458.5499856786     
 iteration         4080 MCMCOBJ=   -13364.5056853349     
 iteration         4090 MCMCOBJ=   -13407.3037821781     
 iteration         4100 MCMCOBJ=   -13385.8301107442     
 iteration         4110 MCMCOBJ=   -13448.4483642007     
 iteration         4120 MCMCOBJ=   -13403.0468312705     
 iteration         4130 MCMCOBJ=   -13426.0957814439     
 iteration         4140 MCMCOBJ=   -13471.8801736647     
 iteration         4150 MCMCOBJ=   -13407.8618246122     
 iteration         4160 MCMCOBJ=   -13440.2522402820     
 iteration         4170 MCMCOBJ=   -13465.4360767842     
 iteration         4180 MCMCOBJ=   -13399.8559620969     
 iteration         4190 MCMCOBJ=   -13477.6807679909     
 iteration         4200 MCMCOBJ=   -13492.5816516517     
 iteration         4210 MCMCOBJ=   -13450.2125345775     
 iteration         4220 MCMCOBJ=   -13447.1479198657     
 iteration         4230 MCMCOBJ=   -13375.1135899231     
 iteration         4240 MCMCOBJ=   -13481.1491218512     
 iteration         4250 MCMCOBJ=   -13362.5654478523     
 iteration         4260 MCMCOBJ=   -13490.4690523646     
 iteration         4270 MCMCOBJ=   -13419.1856926289     
 iteration         4280 MCMCOBJ=   -13519.6515699644     
 iteration         4290 MCMCOBJ=   -13428.2208521561     
 iteration         4300 MCMCOBJ=   -13348.2431529541     
 iteration         4310 MCMCOBJ=   -13421.5631222369     
 iteration         4320 MCMCOBJ=   -13431.7186874819     
 iteration         4330 MCMCOBJ=   -13418.5574227457     
 iteration         4340 MCMCOBJ=   -13315.2046497805     
 iteration         4350 MCMCOBJ=   -13436.9277583447     
 iteration         4360 MCMCOBJ=   -13396.1747634641     
 iteration         4370 MCMCOBJ=   -13506.0379652751     
 iteration         4380 MCMCOBJ=   -13448.2276013538     
 iteration         4390 MCMCOBJ=   -13424.7765593926     
 iteration         4400 MCMCOBJ=   -13484.6398842631     
 iteration         4410 MCMCOBJ=   -13412.7963593195     
 iteration         4420 MCMCOBJ=   -13391.7466893319     
 iteration         4430 MCMCOBJ=   -13402.8834857250     
 iteration         4440 MCMCOBJ=   -13411.4053316401     
 iteration         4450 MCMCOBJ=   -13428.4135496869     
 iteration         4460 MCMCOBJ=   -13455.0026653649     
 iteration         4470 MCMCOBJ=   -13456.6038255972     
 iteration         4480 MCMCOBJ=   -13425.8299212734     
 iteration         4490 MCMCOBJ=   -13473.7277243982     
 iteration         4500 MCMCOBJ=   -13438.5822500747     
 iteration         4510 MCMCOBJ=   -13469.7910212816     
 iteration         4520 MCMCOBJ=   -13426.6182090075     
 iteration         4530 MCMCOBJ=   -13445.2907222008     
 iteration         4540 MCMCOBJ=   -13412.1763828309     
 iteration         4550 MCMCOBJ=   -13452.6823893783     
 iteration         4560 MCMCOBJ=   -13407.3487494362     
 iteration         4570 MCMCOBJ=   -13419.7678921909     
 iteration         4580 MCMCOBJ=   -13404.7758371948     
 iteration         4590 MCMCOBJ=   -13445.5133116289     
 iteration         4600 MCMCOBJ=   -13422.9746484003     
 iteration         4610 MCMCOBJ=   -13414.4252568833     
 iteration         4620 MCMCOBJ=   -13440.0729976100     
 iteration         4630 MCMCOBJ=   -13434.4581291000     
 iteration         4640 MCMCOBJ=   -13474.1973593618     
 iteration         4650 MCMCOBJ=   -13417.3135248578     
 iteration         4660 MCMCOBJ=   -13443.7455240766     
 iteration         4670 MCMCOBJ=   -13395.6472609336     
 iteration         4680 MCMCOBJ=   -13445.2775038587     
 iteration         4690 MCMCOBJ=   -13423.5443877580     
 iteration         4700 MCMCOBJ=   -13437.2502030487     
 iteration         4710 MCMCOBJ=   -13452.3576622204     
 iteration         4720 MCMCOBJ=   -13411.7695040152     
 iteration         4730 MCMCOBJ=   -13428.5262206086     
 iteration         4740 MCMCOBJ=   -13379.2837560211     
 iteration         4750 MCMCOBJ=   -13462.5467515763     
 iteration         4760 MCMCOBJ=   -13409.0885865605     
 iteration         4770 MCMCOBJ=   -13442.0040198069     
 iteration         4780 MCMCOBJ=   -13453.8588304732     
 iteration         4790 MCMCOBJ=   -13445.6490966036     
 iteration         4800 MCMCOBJ=   -13443.1526366271     
 iteration         4810 MCMCOBJ=   -13465.7356749537     
 iteration         4820 MCMCOBJ=   -13391.5726650747     
 iteration         4830 MCMCOBJ=   -13448.0914244819     
 iteration         4840 MCMCOBJ=   -13377.4028681369     
 iteration         4850 MCMCOBJ=   -13454.5240460299     
 iteration         4860 MCMCOBJ=   -13471.4902870677     
 iteration         4870 MCMCOBJ=   -13381.6428777650     
 iteration         4880 MCMCOBJ=   -13455.1394876797     
 iteration         4890 MCMCOBJ=   -13428.3992016017     
 iteration         4900 MCMCOBJ=   -13446.9683228732     
 iteration         4910 MCMCOBJ=   -13378.0800766814     
 iteration         4920 MCMCOBJ=   -13428.8846104601     
 iteration         4930 MCMCOBJ=   -13412.6297331517     
 iteration         4940 MCMCOBJ=   -13348.8046094663     
 iteration         4950 MCMCOBJ=   -13470.1505235197     
 iteration         4960 MCMCOBJ=   -13404.6914221837     
 iteration         4970 MCMCOBJ=   -13398.2185623237     
 iteration         4980 MCMCOBJ=   -13429.6520829561     
 iteration         4990 MCMCOBJ=   -13424.8868842180     
 iteration         5000 MCMCOBJ=   -13382.9542422471     
 
 #TERM:
 BURN-IN WAS COMPLETED
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation time in seconds:   503.39
 Elapsed covariance time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -13431.932       **************************************************
 #OBJS:********************************************       40.064 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.74E-01  6.66E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.79E-02
 
 ETA2
+       -9.78E-03  4.00E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.19E-01
 
 ETA2
+       -2.23E-01  2.00E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.30E-02  1.46E-02  1.97E-02  2.66E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.10E-03
 
 ETA2
+        2.70E-03  3.37E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.47E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        9.33E-03
 
 ETA2
+        5.69E-02  8.41E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.72E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.69E-04
 
 TH 2
+       -4.03E-05  2.12E-04
 
 TH 3
+       -3.68E-05  5.89E-06  3.89E-04
 
 TH 4
+        2.52E-06  4.65E-06  1.12E-05  7.08E-04
 
 OM11
+        7.12E-07 -1.29E-07  2.70E-07  1.90E-06  1.68E-05
 
 OM12
+       -8.33E-07 -6.73E-07  1.33E-06 -2.99E-07 -3.27E-06  7.29E-06
 
 OM22
+        1.36E-06 -2.36E-06 -1.09E-06 -1.00E-06  5.62E-07 -2.79E-06  1.14E-05
 
 SG11
+        1.70E-08 -3.89E-08 -1.49E-08  3.00E-07 -6.10E-10 -3.05E-09 -1.68E-08  1.20E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.30E-02
 
 TH 2
+       -2.13E-01  1.46E-02
 
 TH 3
+       -1.44E-01  2.05E-02  1.97E-02
 
 TH 4
+        7.29E-03  1.20E-02  2.14E-02  2.66E-02
 
 OM11
+        1.33E-02 -2.16E-03  3.34E-03  1.74E-02  4.10E-03
 
 OM12
+       -2.37E-02 -1.71E-02  2.51E-02 -4.16E-03 -2.95E-01  2.70E-03
 
 OM22
+        3.09E-02 -4.81E-02 -1.64E-02 -1.11E-02  4.07E-02 -3.07E-01  3.37E-03
 
 SG11
+        3.76E-03 -7.71E-03 -2.19E-03  3.24E-02 -4.29E-04 -3.26E-03 -1.44E-02  3.47E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.32E+03
 
 TH 2
+        1.19E+03  4.96E+03
 
 TH 3
+        5.80E+02  3.77E+01  2.63E+03
 
 TH 4
+       -3.92E+01 -3.65E+01 -4.38E+01  1.41E+03
 
 OM11
+       -1.49E+02  1.67E+02 -1.52E+02 -1.55E+02  6.54E+04
 
 OM12
+        5.36E+02  1.10E+03 -4.49E+02  3.66E+01  3.11E+04  1.67E+05
 
 OM22
+       -3.17E+02  1.16E+03  8.53E+01  1.29E+02  4.42E+03  3.95E+04  9.78E+04
 
 SG11
+       -3.70E+02  1.72E+03  3.66E+02 -3.51E+03  2.18E+03  1.00E+04  1.48E+04  8.32E+06
 
1
 
 
 #TBLN:      7
 #METH: First Order Conditional Estimation with Interaction
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            9999
 NO. OF SIG. FIGURES REQUIRED:            2
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 ABORT WITH PRED EXIT CODE 1:             NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  0           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
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

 
0ITERATION NO.:    0    OBJECTIVE VALUE:  -9939.23971653752        NO. OF FUNC. EVALS.:   7
 CUMULATIVE NO. OF FUNC. EVALS.:        7
 NPARAMETR:  4.2466E+00 -2.3043E+00 -6.7354E-01  6.6577E-01  4.7930E-02 -9.7770E-03  3.9956E-02  1.0229E-02
 PARAMETER:  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01  1.0000E-01 -1.0000E-01  1.0000E-01  1.0000E-01
 GRADIENT:   5.8459E+05  2.5429E+05  4.0268E+06 -5.3757E-01  7.0544E+00  8.8272E-01  1.0610E+01  5.5080E+00
 
0ITERATION NO.:   10    OBJECTIVE VALUE:  -9939.49592799384        NO. OF FUNC. EVALS.:  31
 CUMULATIVE NO. OF FUNC. EVALS.:      238             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2485E+00 -2.2992E+00 -6.7266E-01  6.6744E-01  4.7325E-02 -9.6537E-03  3.9178E-02  1.0206E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.0750E-01  9.3645E-02 -9.9369E-02  8.9977E-02  9.8912E-02
 GRADIENT:   6.0442E+05  2.8261E+05  4.0385E+06  4.6217E-01 -4.4768E-02 -2.6414E-03 -1.9357E-02 -3.4743E+00
 
0ITERATION NO.:   20    OBJECTIVE VALUE:  -9939.49702925256        NO. OF FUNC. EVALS.:  31
 CUMULATIVE NO. OF FUNC. EVALS.:      458             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2485E+00 -2.2992E+00 -6.7267E-01  6.6602E-01  4.7327E-02 -9.6538E-03  3.9179E-02  1.0216E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.0112E-01  9.3669E-02 -9.9367E-02  8.9987E-02  9.9404E-02
 GRADIENT:   6.0364E+05  2.8226E+05  4.0346E+06 -3.8797E-01  2.9373E-03  1.9910E-03  9.8345E-04  5.0434E-02
 
0ITERATION NO.:   30    OBJECTIVE VALUE:  -9939.49722659015        NO. OF FUNC. EVALS.:  31
 CUMULATIVE NO. OF FUNC. EVALS.:      686             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2485E+00 -2.2992E+00 -6.7267E-01  6.6719E-01  4.7327E-02 -9.6538E-03  3.9179E-02  1.0216E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.0638E-01  9.3666E-02 -9.9368E-02  8.9986E-02  9.9404E-02
 GRADIENT:   6.0365E+05  2.8226E+05  4.0346E+06  3.1306E-01 -8.1461E-05 -8.1138E-05 -2.4036E-06  4.8201E-02
 
0ITERATION NO.:   40    OBJECTIVE VALUE:  -9939.49738127087        NO. OF FUNC. EVALS.:  31
 CUMULATIVE NO. OF FUNC. EVALS.:      915             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2485E+00 -2.2992E+00 -6.7267E-01  6.6627E-01  4.7327E-02 -9.6538E-03  3.9179E-02  1.0216E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.0224E-01  9.3666E-02 -9.9368E-02  8.9986E-02  9.9404E-02
 GRADIENT:   6.0365E+05  2.8226E+05  4.0346E+06 -2.3847E-01 -3.2384E-05  1.6017E-04 -4.1997E-05  4.8148E-02
 
0ITERATION NO.:   50    OBJECTIVE VALUE:  -9939.49749386432        NO. OF FUNC. EVALS.:  31
 CUMULATIVE NO. OF FUNC. EVALS.:     1140             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2485E+00 -2.2992E+00 -6.7267E-01  6.6694E-01  4.7327E-02 -9.6538E-03  3.9179E-02  1.0216E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.0526E-01  9.3667E-02 -9.9368E-02  8.9987E-02  9.9404E-02
 GRADIENT:   6.0365E+05  2.8226E+05  4.0346E+06  1.6381E-01  1.4406E-04  1.8079E-04  4.2776E-05  4.8148E-02
 
0ITERATION NO.:   60    OBJECTIVE VALUE:  -9939.49756476333        NO. OF FUNC. EVALS.:  31
 CUMULATIVE NO. OF FUNC. EVALS.:     1359             RESET HESSIAN, TYPE I
 NPARAMETR:  4.2485E+00 -2.2992E+00 -6.7267E-01  6.6652E-01  4.7327E-02 -9.6538E-03  3.9179E-02  1.0216E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.0336E-01  9.3666E-02 -9.9368E-02  8.9986E-02  9.9404E-02
 GRADIENT:   6.0365E+05  2.8226E+05  4.0346E+06 -8.9089E-02  6.6213E-05  1.4234E-04 -4.3474E-05  4.8148E-02
 
0ITERATION NO.:   64    OBJECTIVE VALUE:  -9939.49757387109        NO. OF FUNC. EVALS.:  23
 CUMULATIVE NO. OF FUNC. EVALS.:     1435
 NPARAMETR:  4.2485E+00 -2.2992E+00 -6.7267E-01  6.6679E-01  4.7327E-02 -9.6538E-03  3.9179E-02  1.0216E-02
 PARAMETER:  1.0000E-01  1.0001E-01  1.0000E-01  1.0459E-01  9.3666E-02 -9.9368E-02  8.9986E-02  9.9404E-02
 GRADIENT:  -2.2488E-03 -2.6275E-03 -5.8765E-03  6.6708E-02 -1.6455E-04 -2.8964E-05 -4.3545E-05 -1.9820E-05
 
 #TERM:
0MINIMIZATION SUCCESSFUL
 HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION.
 REGARD THE RESULTS OF THE ESTIMATION STEP CAREFULLY, AND ACCEPT THEM ONLY
 AFTER CHECKING THAT THE COVARIANCE STEP PRODUCES REASONABLE OUTPUT.
 NO. OF FUNCTION EVALUATIONS USED:     1435
 NO. OF SIG. DIGITS IN FINAL EST.:  2.0

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND SE IS THE ASSOCIATED STANDARD ERROR.

 SUBMODEL    1
 
 ETABAR:        -7.5518E-03  3.8965E-03
 SE:             1.4906E-02  1.1378E-02
 N:                     200         200
 
 ETAshrink(%):   2.8550E+00  1.8503E+01
 EBVshrink(%):   1.9608E+00  3.6242E+00
 EPSshrink(%):   1.1639E+01
 

 SUBMODEL    2
 
 ETABAR:         1.2391E-02 -2.4592E-03
 SE:             2.1744E-02  2.4428E-02
 N:                     100         100
 
 ETAshrink(%):   1.0000E-10  1.0000E-10
 EBVshrink(%):   1.9880E+00  1.5977E-01
 EPSshrink(%):   1.4291E+01
 
 #TERE:
 Elapsed estimation time in seconds:   172.74
 Elapsed covariance time in seconds:    14.32
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -9939.498       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         4.25E+00 -2.30E+00 -6.73E-01  6.67E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.73E-02
 
 ETA2
+       -9.65E-03  3.92E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.02E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        2.18E-01
 
 ETA2
+       -2.24E-01  1.98E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.01E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4     
 
         1.28E-02  1.44E-02  1.95E-02  2.72E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2   
 
 ETA1
+        4.02E-03
 
 ETA2
+        2.67E-03  3.32E-03
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        3.42E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2   
 
 ETA1
+        9.25E-03
 
 ETA2
+        5.73E-02  8.39E-03
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        1.69E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.64E-04
 
 TH 2
+       -3.71E-05  2.08E-04
 
 TH 3
+       -3.34E-05  7.93E-06  3.81E-04
 
 TH 4
+       -1.05E-08  5.41E-08  2.04E-08  7.41E-04
 
 OM11
+        7.35E-08 -8.79E-08  3.02E-08 -2.55E-11  1.62E-05
 
 OM12
+       -4.51E-08  1.09E-06 -1.65E-06  3.41E-10 -3.51E-06  7.15E-06
 
 OM22
+        1.44E-07 -9.97E-07  6.19E-07  4.04E-10  7.50E-07 -2.86E-06  1.10E-05
 
 SG11
+        1.16E-07 -9.97E-10  3.81E-10 -2.12E-13 -2.08E-08  7.75E-09 -9.46E-09  1.17E-07
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        1.28E-02
 
 TH 2
+       -2.01E-01  1.44E-02
 
 TH 3
+       -1.33E-01  2.82E-02  1.95E-02
 
 TH 4
+       -3.01E-05  1.38E-04  3.84E-05  2.72E-02
 
 OM11
+        1.43E-03 -1.52E-03  3.84E-04 -2.33E-07  4.02E-03
 
 OM12
+       -1.32E-03  2.84E-02 -3.15E-02  4.68E-06 -3.27E-01  2.67E-03
 
 OM22
+        3.39E-03 -2.08E-02  9.54E-03  4.47E-06  5.61E-02 -3.22E-01  3.32E-03
 
 SG11
+        2.65E-02 -2.02E-04  5.70E-05 -2.28E-08 -1.51E-02  8.47E-03 -8.33E-03  3.42E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************               FIRST ORDER CONDITIONAL ESTIMATION WITH INTERACTION              ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      OM11      OM12      OM22      SG11  
 
 TH 1
+        6.45E+03
 
 TH 2
+        1.13E+03  5.02E+03
 
 TH 3
+        5.41E+02 -9.02E+00  2.67E+03
 
 TH 4
+       -6.04E-03 -3.51E-01 -6.55E-02  1.35E+03
 
 OM11
+       -3.76E+01 -1.50E+02  1.42E+02 -1.01E-02  6.94E+04
 
 OM12
+       -2.89E+01 -7.33E+02  6.95E+02 -6.86E-02  3.60E+04  1.75E+05
 
 OM22
+       -2.29E+01  2.59E+02  1.22E+01 -9.46E-02  4.60E+03  4.28E+04  1.01E+05
 
 SG11
+       -6.40E+03 -1.04E+03 -5.65E+02  7.66E-04  1.04E+04 -1.71E+03  6.21E+03  8.56E+06
 
 #CPUT: Total CPU Time in Seconds,      937.940
Stop Time: 
Mon 09/30/2013 
04:01 PM

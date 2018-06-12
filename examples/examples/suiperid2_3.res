Sat 09/07/2013 
07:19 PM
$PROB RUN# Example 1 (from samp5l)
$INPUT ID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT SID CID
$DATA superid2.csv

$SUBROUTINES ADVAN1 TRANS2

$PK
MU_1=THETA(1)
MU_2=THETA(2)
CL=DEXP(MU_1+ETA(1)+ETA(3)+ETA(5))
V=DEXP(MU_2+ETA(2)+ETA(4)+ETA(6))
S1=V

$ERROR
Y = F + F*EPS(1)

; Initial values of THETA
$THETA 5.0 5.0
;$OMEGA 0.02 0.02 0.0 FIXED 0.0 FIXED 0.0 FIXED 0.0 FIXED
$OMEGA BLOCK(2)
0.05
0.00001 0.05

$OMEGA BLOCK(2)
0.1
0.00001 0.1

$OMEGA BLOCK(2)
0.5
0.00001 0.5

$SIGMA 
0.05

$LEVEL
SID=(3[1],4[2])
CID=(5[3],6[4])

;$EST METHOD=ITS INTERACTION PRINT=1 NSIG=3 NITER=0 SIGL=6 FNLETA=0
$EST METHOD=IMPMAP INTERACTION PRINT=1 NSIG=3 NITER=50 CTYPE=3 ISAMPLE=300 SIGL=8 FNLETA=0 MCETA=3
$COV MATRIX=R UNCONDITIONAL
$TABLE  ID TIME CONC DOSE RATE EVID MDV CMT SID CID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
NOAPPEND ONEHEADER FILE=superid2_3.tab  NOPRINT
$TABLE  ID SID CID CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 NOAPPEND ONEHEADER FILE=superid2_3.dat FIRSTONLY NOPRINT FORMAT=,1PE15.8
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 12) MU_001: SHOULD NOT BE ASSOCIATED WITH ETA(003)

 (MU_WARNING 11) MU_001: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_001: SHOULD NOT BE ASSOCIATED WITH ETA(005)

 (MU_WARNING 11) MU_001: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_002: SHOULD NOT BE ASSOCIATED WITH ETA(004)

 (MU_WARNING 11) MU_002: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.

 (MU_WARNING 12) MU_002: SHOULD NOT BE ASSOCIATED WITH ETA(006)

 (MU_WARNING 11) MU_002: SHOULD NOT BE ASSOCIATED WITH MORE THAN ONE ETA.
  
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
 RUN# Example 1 (from samp5l)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:    20000
 NO. OF DATA ITEMS IN DATA SET:  10
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   3
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   6   2   4   5   0   0   8   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME CONC DOSE RATE EVID MDV CMT SID CID
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 CL V
0FORMAT FOR DATA:
 (7E10.0/3E10.0)

 TOT. NO. OF OBS RECS:    17500
 TOT. NO. OF INDIVIDUALS:   2500
0LENGTH OF THETA:   2
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  0  0  2
  0  0  2  2
  0  0  0  0  3
  0  0  0  0  3  3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
   0.5000E+01  0.5000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.5000E-01
                  0.1000E-04   0.5000E-01
        2                                                                                   NO
                  0.1000E+00
                  0.1000E-04   0.1000E+00
        3                                                                                   NO
                  0.5000E+00
                  0.1000E-04   0.5000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.5000E-01
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 RELATIVE TOLERANCE (TOL):                  -1
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY (ATOL): -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 RESUME COV ANALYSIS (RESUME):               NO
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           2
 SEED NUMBER (SEED):    11456
 RANMETHOD:
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE:            EIGENVALUE
0-- TABLE   1 --
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADERS:               ONE
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID TIME CONC DOSE RATE EVID MDV CMT SID CID ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
0-- TABLE   2 --
0FIRST RECORDS ONLY:    YES
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADERS:               ONE
 FILE TO BE FORWARDED:   NO
 FORMAT:                ,1PE15.8
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 ID SID CID CL V ETA1 ETA2 ETA3 ETA4 ETA5 ETA6
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(N)

 ONE COMPARTMENT MODEL (ADVAN1)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   2
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1

 TRANSLATOR WILL CONVERT PARAMETERS
 CLEARANCE (CL) AND VOLUME (V) TO K (TRANS2)
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
   EVENT ID DATA ITEM IS DATA ITEM NO.:      6
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   4
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     5
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    8

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONCE IN THIS PROBLEM.
1
 
 
 #TBLN:      1
 #METH: Importance Sampling assisted by MAP Estimation
 
 ESTIMATION STEP OMITTED:                 NO  
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES 
 CENTERED ETA:                            NO  
 EPS-ETA INTERACTION:                     YES 
 LAPLACIAN OBJ. FUNC.:                    NO  
 NO. OF FUNCT. EVALS. ALLOWED:            528
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES 
 ESTIMATE OUTPUT TO MSF:                  NO  
 IND. OBJ. FUNC. VALUES SORTED:           NO  
 NUMERICAL DERIVATIVE 
       FILE REQUEST (NUMDER):             NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP): 0           
 ETA HESSIAN EVALUATION METHOD (ETADER):  0           
 INITIAL ETA FOR MAP ESTIMATION (MCETA):  3           
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):    8           
 GRADIENT SIGDIGITS OF 
       FIXED EFFECTS PARAMETERS (SIGL):   8           
 EXCLUDE TITLE (NOTITLE):                 NO 
 EXCLUDE COLUMN LABELS (NOLABEL):         NO 
 NOPRIOR SETTING (NOPRIOR):               OFF
 NOCOV SETTING (NOCOV):                   OFF
 DERCONT SETTING (DERCONT):               OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):-100        
 FINAL ETA RE-EVALUATION (FNLETA):        OFF
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
 ITERATIONS (NITER):                      50          
 STARTING SEED FOR MC METHODS (SEED):     11456       
 MC SAMPLES PER SUBJECT (ISAMPLE):        300         
 RANDOM SAMPLING METHOD (RANMETHOD):      
 EXPECTATION ONLY (EONLY):                NO 
 PROPOSAL DENSITY SCALING RANGE 
              (ISCALE_MIN, ISCALE_MAX):   0.100000000000000       ,10.0000000000000        
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
   1   2
 THETAS THAT ARE SIGMA-LIKE:
 
 
 MONITORING OF SEARCH:

 iteration            0 OBJ=   219154.251026392 eff.=     131. Smpl.=     300. Fit.= 0.79900
 iteration            1 OBJ=  -19356.9944327818 eff.=     219. Smpl.=     300. Fit.= 0.91778
 iteration            2 OBJ=  -29001.7520383180 eff.=     138. Smpl.=     300. Fit.= 0.81346
 iteration            3 OBJ=  -36076.4163993221 eff.=     126. Smpl.=     300. Fit.= 0.79475
 iteration            4 OBJ=  -40390.9072864593 eff.=     125. Smpl.=     300. Fit.= 0.79262
 iteration            5 OBJ=  -41124.4848202785 eff.=     123. Smpl.=     300. Fit.= 0.78904
 iteration            6 OBJ=  -41184.8212383150 eff.=     121. Smpl.=     300. Fit.= 0.78634
 iteration            7 OBJ=  -41185.8343450341 eff.=     120. Smpl.=     300. Fit.= 0.78529
 iteration            8 OBJ=  -41192.1918130640 eff.=     120. Smpl.=     300. Fit.= 0.78564
 iteration            9 OBJ=  -41195.9842134756 eff.=     120. Smpl.=     300. Fit.= 0.78543
 iteration           10 OBJ=  -41188.2905922289 eff.=     120. Smpl.=     300. Fit.= 0.78502
 iteration           11 OBJ=  -41200.1734440693 eff.=     121. Smpl.=     300. Fit.= 0.78518
 iteration           12 OBJ=  -41192.2771286523 eff.=     120. Smpl.=     300. Fit.= 0.78459
 iteration           13 OBJ=  -41194.3973047307 eff.=     120. Smpl.=     300. Fit.= 0.78497
 Convergence achieved
 iteration           13 OBJ=  -41196.0204315097 eff.=     120. Smpl.=     300. Fit.= 0.78512
 
 #TERM:
 OPTIMIZATION WAS COMPLETED


 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.
 
 ETABAR:         9.6652E-05 -3.3811E-05  5.6288E-18 -1.2296E-18  8.2774E-16  6.4714E-16
 SE:             1.6656E-03  1.7469E-03  9.9822E-03  1.0722E-02  6.2419E-02  5.5674E-02
 N:                    2500        2500         250         250          25          25
 
 P VAL.:         9.5373E-01  9.8456E-01  1.0000E+00  1.0000E+00  1.0000E+00  1.0000E+00
 
 ETAshrink(%):   1.0977E+01  1.0575E+01  5.8741E-02  4.6584E-02  5.2867E-02  1.0000E-10
 EBVshrink(%):   1.0988E+01  1.0654E+01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
 EPSshrink(%):   1.2066E+01
 
 #TERE:
 Elapsed estimation time in seconds:   886.32
 Elapsed covariance time in seconds:    64.34
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 #OBJT:**************                        FINAL VALUE OF OBJECTIVE FUNCTION                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************   -41196.020       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         1.99E+00  3.63E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.72E-03
 
 ETA2
+       -3.33E-05  1.06E-02
 
 ETA3
+        0.00E+00  0.00E+00  2.77E-02
 
 ETA4
+        0.00E+00  0.00E+00  1.32E-03  3.20E-02
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.02E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.81E-02  8.07E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        9.94E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        9.86E-02
 
 ETA2
+       -3.28E-03  1.03E-01
 
 ETA3
+        0.00E+00  0.00E+00  1.66E-01
 
 ETA4
+        0.00E+00  0.00E+00  4.44E-02  1.79E-01
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.19E-01
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.00E-01  2.84E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        9.97E-02
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                          STANDARD ERROR OF ESTIMATE (R)                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2     
 
         2.23E-03  2.31E-03
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        3.44E-04
 
 ETA2
+        2.58E-04  3.77E-04
 
 ETA3
+        0.00E+00  0.00E+00  2.34E-03
 
 ETA4
+        0.00E+00  0.00E+00  1.86E-03  2.73E-03
 
 ETA5
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.28E-02
 
 ETA6
+        0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.47E-02  2.70E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1   
 
 EPS1
+        1.25E-04
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


            ETA1      ETA2      ETA3      ETA4      ETA5      ETA6   
 
 ETA1
+        1.75E-03
 
 ETA2
+        2.54E-02  1.83E-03
 
 ETA3
+       ......... .........  7.02E-03
 
 ETA4
+       ......... .........  6.28E-02  7.64E-03
 
 ETA5
+       ......... ......... ......... .........  5.14E-02
 
 ETA6
+       ......... ......... ......... .........  2.45E-01  4.75E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


            EPS1   
 
 EPS1
+        6.26E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                        COVARIANCE MATRIX OF ESTIMATE (R)                       ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        4.95E-06
 
 TH 2
+        2.29E-07  5.34E-06
 
 OM11
+       -2.17E-08  1.79E-08  1.19E-07
 
 OM12
+        3.02E-08 -5.83E-09  6.31E-09  6.64E-08
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.83E-08  3.55E-09  1.84E-10  6.02E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.42E-07
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+        6.59E-08  5.20E-08  1.68E-08  5.24E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.35E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.46E-06
 
 OM34
+        7.94E-09  2.38E-08 -7.75E-09 -1.93E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.38E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.38E-07  3.47E-06
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        1.90E-08 -4.80E-11  1.80E-08  5.53E-09  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.02E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.18E-07  1.43E-08  0.00E+00  0.00E+00  7.47E-06
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+        1.94E-06 -1.43E-07  2.26E-07  1.56E-08  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -2.35E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  5.84E-06  2.41E-06  0.00E+00  0.00E+00  5.10E-06  0.00E+00  0.00E+00  1.08E-03
 
 OM56
+       -5.78E-07  2.21E-07 -9.29E-08  1.32E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.71E-08  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -4.64E-06  1.87E-06  0.00E+00  0.00E+00 -1.26E-06  0.00E+00  0.00E+00 -5.35E-04  6.09E-04
 
 OM66
+        1.22E-06  9.08E-07  6.42E-08 -1.08E-07  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -4.06E-07  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -5.32E-07 -5.39E-06  0.00E+00  0.00E+00  8.41E-07  0.00E+00  0.00E+00  1.72E-04 -1.93E-04  7.29E-04
 
 SG11
+        1.00E-08  1.16E-08 -2.95E-09 -9.32E-10  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -3.57E-09  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.32E-09  3.82E-10  0.00E+00  0.00E+00 -2.17E-09  0.00E+00  0.00E+00 -1.14E-07  3.07E-08 -1.14E-08  1.56E-08
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                        CORRELATION MATRIX OF ESTIMATE (R)                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.23E-03
 
 TH 2
+        4.46E-02  2.31E-03
 
 OM11
+       -2.83E-02  2.26E-02  3.44E-04
 
 OM12
+        5.27E-02 -9.80E-03  7.11E-02  2.58E-04
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -2.18E-02  4.07E-03  1.42E-03  6.20E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.77E-04
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+        1.27E-02  9.64E-03  2.08E-02  8.71E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.54E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  2.34E-03
 
 OM34
+        1.91E-03  5.52E-03 -1.21E-02 -4.02E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.33E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -7.77E-02  1.86E-03
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+        3.13E-03 -7.60E-06  1.91E-02  7.86E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -9.94E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -1.84E-02  2.81E-03  0.00E+00  0.00E+00  2.73E-03
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+        2.66E-02 -1.89E-03  2.00E-02  1.85E-03  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.90E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.63E-02  3.94E-02  0.00E+00  0.00E+00  5.69E-02  0.00E+00  0.00E+00  3.28E-02
 
 OM56
+       -1.05E-02  3.87E-03 -1.09E-02  2.08E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.91E-03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.05E-02  4.06E-02  0.00E+00  0.00E+00 -1.86E-02  0.00E+00  0.00E+00 -6.61E-01  2.47E-02
 
 OM66
+        2.03E-02  1.45E-02  6.91E-03 -1.55E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -3.99E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -8.43E-03 -1.07E-01  0.00E+00  0.00E+00  1.14E-02  0.00E+00  0.00E+00  1.95E-01 -2.90E-01  2.70E-02
 
 SG11
+        3.61E-02  4.03E-02 -6.86E-02 -2.90E-02  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -7.59E-02  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.94E-03  1.64E-03  0.00E+00  0.00E+00 -6.37E-03  0.00E+00  0.00E+00 -2.79E-02  9.97E-03 -3.39E-03  1.25E-04
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  IMPORTANCE SAMPLING ASSISTED BY MAP ESTIMATION                ********************
 ********************                    INVERSE COVARIANCE MATRIX OF ESTIMATE (R)                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 TH 1
+        2.04E+05
 
 TH 2
+       -8.67E+03  1.88E+05
 
 OM11
+        4.21E+04 -3.46E+04  8.54E+06
 
 OM12
+       -1.02E+05  2.23E+04 -8.10E+05  1.53E+07
 
 OM13
+       ......... ......... ......... ......... .........
 
 OM14
+       ......... ......... ......... ......... ......... .........
 
 OM15
+       ......... ......... ......... ......... ......... ......... .........
 
 OM16
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        2.59E+04 -1.11E+04  6.06E+04 -6.52E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  7.13E+06
 
 OM23
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM24
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM25
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 OM26
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         .........
 
 OM33
+       -2.15E+03 -1.86E+03 -2.44E+04 -8.98E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.68E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.86E+05
 
 OM34
+       -1.15E+03 -1.70E+03  1.27E+04  9.32E+04  0.00E+00  0.00E+00  0.00E+00  0.00E+00 -1.89E+04  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  1.90E+04  2.96E+05
 
 OM35
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... .........
 
 OM36
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... .........
 
 OM44
+       -2.74E+02  8.46E+00 -1.88E+04 -9.02E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  9.08E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  3.54E+03  4.56E+02  0.00E+00  0.00E+00  1.34E+05
 
 OM45
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... .........
 
1

            TH 1      TH 2      OM11      OM12      OM13      OM14      OM15      OM16      OM22      OM23      OM24      OM25  
             OM26      OM33      OM34      OM35      OM36      OM44      OM45      OM46      OM55      OM56      OM66      SG11  

 
 OM46
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... .........
 
 OM55
+       -4.61E+02  1.79E+00 -1.60E+03 -3.58E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  2.79E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -6.84E+02 -2.08E+03  0.00E+00  0.00E+00 -8.89E+02  0.00E+00  0.00E+00  1.68E+03
 
 OM56
+       -2.88E+02 -1.76E+02 -3.63E+02 -6.46E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  3.72E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  9.93E+02 -1.91E+03  0.00E+00  0.00E+00 -5.01E+02  0.00E+00  0.00E+00  1.47E+03  3.09E+03
 
 OM66
+       -3.13E+02 -2.83E+02 -4.63E+02  1.96E+03  0.00E+00  0.00E+00  0.00E+00  0.00E+00  4.05E+03  0.00E+00  0.00E+00  0.00E+00
          0.00E+00  7.13E+02  2.20E+03  0.00E+00  0.00E+00 -6.56E+01  0.00E+00  0.00E+00 -2.14E+01  4.61E+02  1.52E+03
 
 SG11
+       -1.19E+05 -1.42E+05  1.57E+06  6.47E+05  0.00E+00  0.00E+00  0.00E+00  0.00E+00  1.61E+06  0.00E+00  0.00E+00  0.00E+00
          0.00E+00 -3.27E+04 -1.43E+04  0.00E+00  0.00E+00  1.08E+04  0.00E+00  0.00E+00  9.84E+03  5.54E+03  1.25E+03  6.51E+07
 
 SKIPPING EXECUTION OF SLOW FNLMOD/FNLETA BY USER REQUEST
 #CPUT: Total CPU Time in Seconds,      950.406
Stop Time: 
Sat 09/07/2013 
07:36 PM

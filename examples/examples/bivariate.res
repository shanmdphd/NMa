Thu 03/02/2017 
05:27 PM
$PROB BIVARIATE EXAMPLE
; THESE DECLARATIONS ALLOW ANY FUNCTION TO HAVE ALTERNATIVE DIMENSIONS FOR THEIR ARRAYS
; BUT, USER DEFINED DIMENSIONS ARE PASSED AS THE LAST ARGUMENT TO FUNC, SUCH AS:
; BV=BIVARIATE(VBI(1),FNC001_1(1,1),FNC001_2(1,1,1),5)
$ABBR FUNCTION BIVARIATE(VBI,5)

$INPUT SIM ID DOSE DV TIME
$DATA bivariate.csv IGNORE=@

$SUBROUTINES OTHER=bivariate.f90

$PRED
  B1=THETA(1)
  B2=THETA(2)
  B3=THETA(3)
  K =LOG(2)/EXP(THETA(4))
  ED50=EXP(THETA(5))
  U =(1-EXP(-K*TIME ))

  MU_1=B1+B3*DOSE/(DOSE+ED50)
  MU_2=B2
  MXB=MU_1+ETA(1)
  MXU=MU_2+ETA(2)
  MX =MXB + MXU*U  ;***Current model prediction***;

  PHIMX=PHI(MX)

   IF(NEWIND.NE.2) THEN
   TIMEP=0
   MXP=0
   DVP=0
   PHIMXP=0.5
   ENDIF

  RHOB=(2/(1+EXP(-THETA(8)))-1)
  IF(RHOB>0.0)  RHO=RHOB**(TIME-TIMEP)
  IF(RHOB==0.0) RHO=0.0
  IF(RHOB<0.0)  RHO=-(-RHOB)**(TIME-TIMEP)

  PC =(1-PHIMX) *(1-DV ) + PHIMX*DV
  IF(PC.LE.0.0) EXIT


  V=SQRT(1+OMEGA(1,1)+OMEGA(2,2)*U**2)
  POPP = (B1+B2*U +B3*DOSE/(DOSE+ED50))/V ;***Population mean prediction***;

  IF (TIME.EQ.1) THEN
    JP=PC
    PCP=1.0
  ELSE
  ;***Pass information to bivariate normal***;
   VBI(1)=RHO
   VBI(2)=MX
   VBI(3)=MXP
   VBI(4)=1  ;***0 = Upper tail as in Drezner & Wesolowsky; 1 = Bottom tail***;
   VBI(5)=1  ;***0 = 3 pt approximation; 1 = 5 point approximation***;
   BV=BIVARIATE(VBI)
   JP=((DV-1)*(DVP-1)+(DV-1)*(1-2*DVP)*PHIMXP+(DVP-1)*(1-2*DV)*PHIMX+(1-2*DV)*(1-2*DVP)*BV)
  ENDIF
   IF(JP.LE.0.0) EXIT
   LOGL=LOG(JP/PCP)
   Y = -2*LOGL

  MXP=MX
  PCP=PC
  DVP=DV
  TIMEP=TIME
  PHIMXP=PHIMX

$THETA     
      -1.7   ; 1  B1
       1.2   ; 2  B2
       2.9   ; 3  B3
       1.4   ; 4  LOG(B4)
       1.2   ; 5  LOG(B5)
       (0.0 FIXED)   ; 6  LOG SQRT VAR(ETA1)
       (0.0 FIXED)  ; 7  LOG SQRT VAR(ETA2)
       2.2   ; 8  RHO parameter

$OMEGA DIAGONAL(2)
      0.8       ; V1
      0.8       ; V2

;$EST METHOD=IMP LAPLACE -2LL PRINT=1 NITER=300 ISAMPLE=300 SIGL=6 CTYPE=3 NOHABORT
$EST MAX=0 PRINT=1 METHOD=1 LAPLACE -2LL SIGL=10 NOHABORT
$COV COMPRESS MATRIX=R PRINT=E UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  3) THERE MAY BE AN ERROR IN THE ABBREVIATED CODE. THE FOLLOWING
 ONE OR MORE RANDOM VARIABLES ARE DEFINED WITH "IF" STATEMENTS THAT DO NOT
 PROVIDE DEFINITIONS FOR BOTH THE "THEN" AND "ELSE" CASES. IF ALL
 CONDITIONS FAIL, THE VALUES OF THESE VARIABLES WILL BE ZERO.
  
   VBI(2) BV

             
 (WARNING  13) WITH USER-WRITTEN PRED OR $PRED, NM-TRAN CANNOT APPEND THE
 MDV DATA ITEM.
             
 (WARNING  78) OMEGA IS USED ON THE RIGHT. WITH A SUBSEQUENT RUN, IF AN
 INITIAL ESTIMATE OF A DIAGONAL BLOCK OF OMEGA IS TO BE COMPUTED BY
 NONMEM, THAT BLOCK WILL BE SET TO AN IDENTITY MATRIX DURING THAT
 COMPUTATION. THIS COULD LEAD TO AN ARITHMETIC EXCEPTION.*

 (MU_WARNING 24) ABBREVIATED CODE IS TOO COMPLEX. UNABLE TO CHECK USE OF MU_ VARIABLES.

 * THE MAXIMUM NUMBER OF WARNINGS OF ONE OR MORE TYPES WAS REACHED.
 IT IS POSSIBLE THAT SOME WARNING MESSAGES WERE SUPPRESSED.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        2 MAR 2017
Days until program expires :4835
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 beta 2 (nm74b2)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 BIVARIATE EXAMPLE
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     1960
 NO. OF DATA ITEMS IN DATA SET:   5
 ID DATA ITEM IS DATA ITEM NO.:   2
 DEP VARIABLE IS DATA ITEM NO.:   4
0LABELS FOR DATA ITEMS:
 SIM ID DOSE DV TIME
0FORMAT FOR DATA:
 (5E4.0)

 TOT. NO. OF OBS RECS:     1960
 TOT. NO. OF INDIVIDUALS:    280
0LENGTH OF THETA:   8
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07    -0.1700E+01     0.1000E+07
 -0.1000E+07     0.1200E+01     0.1000E+07
 -0.1000E+07     0.2900E+01     0.1000E+07
 -0.1000E+07     0.1400E+01     0.1000E+07
 -0.1000E+07     0.1200E+01     0.1000E+07
  0.0000E+00     0.0000E+00     0.0000E+00
  0.0000E+00     0.0000E+00     0.0000E+00
 -0.1000E+07     0.2200E+01     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.8000E+00
 0.0000E+00   0.8000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:             YES
 COMPRESSED FORMAT:             YES
 GRADIENT METHOD USED:     NOSLOW
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 TURN OFF Cholesky Transposition of R Matrix (CHOLROFF): NO
 KNUTHSUMOFF:                                -1
 RESUME COV ANALYSIS (RESUME):               NO
 SIR SAMPLE SIZE (SIRSAMPLE):              -1
 NON-LINEARLY TRANSFORM THETAS DURING COV (THBND): 1
 PRECONDTIONING CYCLES (PRECOND):        0
 PRECONDTIONING TYPES (PRECONDS):        TOS
 FORCED PRECONDTIONING CYCLES (PFCOND):0
 PRECONDTIONING TYPE (PRETYPE):        0
 FORCED POS. DEFINITE SETTING: (FPOSDEF):0
1
 
 
 #TBLN:      1
 #METH: Laplacian Conditional Estimation (Evaluation)
 
 ESTIMATION STEP OMITTED:                 YES
 ANALYSIS TYPE:                           POPULATION
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     NO
 LAPLACIAN OBJ. FUNC.:                    YES
 NUMERICAL 2ND DERIVATIVES:               NO
 PRED F SET TO -2 LOG LIKELIHOOD:         YES
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      10
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     10
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): bivariate.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 ADDITIONAL CONVERGENCE TEST (CTYPE=4)?:    NO
 EM OR BAYESIAN METHOD USED:                 NONE

 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=NPRED
 RES=NRES
 WRES=NWRES
 IWRS=NIWRES
 IPRD=NIPRED
 IRS=NIRES
 
 Elapsed evaluation time in seconds:     0.23
 Elapsed covariance  time in seconds:     3.86
 Elapsed postprocess time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  LAPLACIAN CONDITIONAL ESTIMATION (EVALUATION)                 ********************
 #OBJT:**************                       MINIMUM VALUE OF OBJECTIVE FUNCTION                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************     1493.672       **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  LAPLACIAN CONDITIONAL ESTIMATION (EVALUATION)                 ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
        -1.70E+00  1.20E+00  2.90E+00  1.40E+00  1.20E+00  0.00E+00  0.00E+00  2.20E+00
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2     
 
 ETA1
+        8.00E-01
 
 ETA2
+        0.00E+00  8.00E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2     
 
 ETA1
+        8.94E-01
 
 ETA2
+        0.00E+00  8.94E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  LAPLACIAN CONDITIONAL ESTIMATION (EVALUATION)                 ********************
 ********************                            STANDARD ERROR OF ESTIMATE                          ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.40E-01  1.80E-01  4.57E-01  3.35E-01  3.68E-01 ......... .........  5.62E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2     
 
 ETA1
+        6.53E-01
 
 ETA2
+       .........  2.57E-01
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2     
 
 ETA1
+        3.65E-01
 
 ETA2
+       .........  1.44E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  LAPLACIAN CONDITIONAL ESTIMATION (EVALUATION)                 ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

     TH 1 | TH 1      TH 2 | TH 1      TH 2 | TH 2      TH 3 | TH 1      TH 3 | TH 2      TH 3 | TH 3      TH 4 | TH 1  
       1.15E-01        -3.51E-02         3.24E-02        -1.11E-01         2.39E-02         2.09E-01        -6.19E-04

     TH 4 | TH 2      TH 4 | TH 3      TH 4 | TH 4      TH 5 | TH 1      TH 5 | TH 2      TH 5 | TH 3      TH 5 | TH 4  
      -9.04E-03         6.32E-02         1.12E-01         7.89E-02        -8.68E-03        -3.00E-02        -9.19E-03

     TH 5 | TH 5      TH 8 | TH 1      TH 8 | TH 2      TH 8 | TH 3      TH 8 | TH 4      TH 8 | TH 5      TH 8 | TH 8  
       1.36E-01         9.16E-02        -2.81E-02        -1.84E-01        -9.50E-02         4.10E-02         3.16E-01

   OM0101 | TH 1    OM0101 | TH 2    OM0101 | TH 3    OM0101 | TH 4    OM0101 | TH 5    OM0101 | TH 8    OM0101 | OM0101
      -1.03E-01         2.64E-02         2.29E-01         1.43E-01        -4.38E-02        -3.20E-01         4.27E-01

   OM0202 | TH 1    OM0202 | TH 2    OM0202 | TH 3    OM0202 | TH 4    OM0202 | TH 5    OM0202 | TH 8    OM0202 | OM0101
       1.23E-02         2.45E-03        -4.55E-02        -4.34E-02         5.73E-03         6.91E-02        -1.13E-01

   OM0202 | OM0202  
       6.62E-02
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  LAPLACIAN CONDITIONAL ESTIMATION (EVALUATION)                 ********************
 ********************                          CORRELATION MATRIX OF ESTIMATE                        ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

     TH 1 | TH 1      TH 2 | TH 1      TH 2 | TH 2      TH 3 | TH 1      TH 3 | TH 2      TH 3 | TH 3      TH 4 | TH 1  
       3.40E-01        -5.75E-01         1.80E-01        -7.14E-01         2.91E-01         4.57E-01        -5.44E-03

     TH 4 | TH 2      TH 4 | TH 3      TH 4 | TH 4      TH 5 | TH 1      TH 5 | TH 2      TH 5 | TH 3      TH 5 | TH 4  
      -1.50E-01         4.13E-01         3.35E-01         6.30E-01        -1.31E-01        -1.78E-01        -7.44E-02

     TH 5 | TH 5      TH 8 | TH 1      TH 8 | TH 2      TH 8 | TH 3      TH 8 | TH 4      TH 8 | TH 5      TH 8 | TH 8  
       3.68E-01         4.79E-01        -2.78E-01        -7.15E-01        -5.04E-01         1.98E-01         5.62E-01

   OM0101 | TH 1    OM0101 | TH 2    OM0101 | TH 3    OM0101 | TH 4    OM0101 | TH 5    OM0101 | TH 8    OM0101 | OM0101
      -4.63E-01         2.25E-01         7.68E-01         6.55E-01        -1.82E-01        -8.71E-01         6.53E-01

   OM0202 | TH 1    OM0202 | TH 2    OM0202 | TH 3    OM0202 | TH 4    OM0202 | TH 5    OM0202 | TH 8    OM0202 | OM0101
       1.41E-01         5.29E-02        -3.87E-01        -5.03E-01         6.05E-02         4.77E-01        -6.73E-01

   OM0202 | OM0202  
       2.57E-01
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  LAPLACIAN CONDITIONAL ESTIMATION (EVALUATION)                 ********************
 ********************                      INVERSE COVARIANCE MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

     TH 1 | TH 1      TH 2 | TH 1      TH 2 | TH 2      TH 3 | TH 1      TH 3 | TH 2      TH 3 | TH 3      TH 4 | TH 1  
       1.46E+02         8.37E+01         9.00E+01         7.73E+01         4.28E+01         5.39E+01        -3.31E+01

     TH 4 | TH 2      TH 4 | TH 3      TH 4 | TH 4      TH 5 | TH 1      TH 5 | TH 2      TH 5 | TH 3      TH 5 | TH 4  
      -8.36E+00        -1.56E+01         2.67E+01        -6.79E+01        -3.78E+01        -3.56E+01         1.51E+01

     TH 5 | TH 5      TH 8 | TH 1      TH 8 | TH 2      TH 8 | TH 3      TH 8 | TH 4      TH 8 | TH 5      TH 8 | TH 8  
       3.92E+01         3.97E+00         3.07E+00         2.69E+00        -2.93E+00        -2.36E+00         1.49E+01

   OM0101 | TH 1    OM0101 | TH 2    OM0101 | TH 3    OM0101 | TH 4    OM0101 | TH 5    OM0101 | TH 8    OM0101 | OM0101
      -6.54E+00        -1.06E+01        -1.18E+01        -8.64E+00         3.47E+00         1.26E+01         2.50E+01

   OM0202 | TH 1    OM0202 | TH 2    OM0202 | TH 3    OM0202 | TH 4    OM0202 | TH 5    OM0202 | TH 8    OM0202 | OM0101
      -8.29E+00        -1.31E+01        -8.93E+00         2.72E-01         4.48E+00         5.34E+00         1.71E+01

   OM0202 | OM0202  
       3.44E+01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  LAPLACIAN CONDITIONAL ESTIMATION (EVALUATION)                 ********************
 ********************                      EIGENVALUES OF COR MATRIX OF ESTIMATE                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

             1         2         3         4         5         6         7         8
 
         2.98E-02  6.99E-02  3.08E-01  4.57E-01  5.32E-01  9.36E-01  1.78E+00  3.88E+00
 
 Elapsed finaloutput time in seconds:     0.36
 #CPUT: Total CPU Time in Seconds,        4.134
Stop Time: 
Thu 03/02/2017 
05:29 PM

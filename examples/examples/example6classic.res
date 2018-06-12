Wed 06/17/2015 
04:34 PM
;Model Desc: Receptor Mediated Clearance model with Dynamic Change in Receptors
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# example6 (from r2compl)
$INPUT C SET ID JID TIME DV=CONC DOSE=AMT RATE EVID MDV CMT
$DATA example6.csv IGNORE=C

; The new numerical integration solver is used, although ADVAN=9 is also efficient
; for this problem.
$SUBROUTINES ADVAN13 TRANS1 TOL=4
$MODEL NCOMPARTMENTS=3

$PRIOR NWPRI NTHETA=8, NETA=8, NTHP=0, NETP=8, NPEXP=1

$PK
include nonmem_reserved_general
; Request extra information for Bayesian analysis.  An extra call will then be made
; for accepted samples
BAYES_EXTRA_REQUEST=1
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
MU_4=THETA(4)
MU_5=THETA(5)
MU_6=THETA(6)
MU_7=THETA(7)
MU_8=THETA(8)
VC=EXP(MU_1+ETA(1))
K10=EXP(MU_2+ETA(2))
K12=EXP(MU_3+ETA(3))
K21=EXP(MU_4+ETA(4))
VM=EXP(MU_5+ETA(5))
KMC=EXP(MU_6+ETA(6))
K03=EXP(MU_7+ETA(7))
K30=EXP(MU_8+ETA(8))
S3=VC
S1=VC
KM=KMC*S1
F3=K03/K30
; When Bayes_extra=1, then this particular set of individual parameters were "accepted"
; So you may record them if you wish
IF(BAYES_EXTRA==1 .AND. NEWIND/=2) THEN
;IF(BAYES_EXTRA==1 .AND. ITER_REPORT>=0 .AND. TIME==0.0) THEN
" WRITE(50,'(I12,1X,F14.0,9(1X,1PG12.5))') ITER_REPORT,ID,VC,K10,K12,K21,VM,KMC,K03,K30,OBJI(NIREC,1)
ENDIF

$DES
DADT(1) = -(K10+K12)*A(1) + K21*A(2) - VM*A(1)*A(3)/(A(1)+KM)
DADT(2) = K12*A(1) - K21*A(2)
DADT(3) =  -VM*A(1)*A(3)/(A(1)+KM) - K30*A(3) + K03

$ERROR
CALLFL=0
ETYPE=1
IF(CMT.NE.1) ETYPE=0
IPRED=F
Y = F + F*ETYPE*EPS(1) + F*(1.0-ETYPE)*EPS(2)


$THETA 
;Initial Thetas
( 4.0 )  ;[MU_1]
( -2.1 ) ;[MU_2]
( 0.7 )  ;[MU_3]
( -0.17 );[MU_4]      
( 2.2 ) ;[MU_5]
( 0.14 )  ;[MU_6]
( 3.7 )  ;[MU_7]
( -0.7) ;[MU_8]
; degrees of freedom for OMEGA prior
(8 FIXED)           ;[dfo]


;Initial Omegas
$OMEGA BLOCK(8)
0.2 ;[p]
-0.0043  ;[f]
0.2 ;[p]
0.0048   ;[f]    
-0.0023  ;[f]     
0.2 ;[p]
0.0032   ;[f]   
0.0059   ;[f]  
-0.0014  ;[f]   
0.2 ;[p]
0.0029   ;[f]   
0.002703 ;[f]  
-0.00026 ;[f]  
-0.0032  ;[f]    
0.2 ;[p]
-0.0025  ;[f]  
0.00097  ;[f]   
0.0024   ;[f]  
0.00197  ;[f]  
-0.0080  ;[f]   
0.2 ;[p]
0.0031   ;[f]  
-0.00571 ;[f]    
0.0030   ;[f]   
-0.0074  ;[f]    
0.0025   ;[f]   
0.0034   ;[f]  
0.2 ;[p]
0.00973  ;[f]  
0.00862  ;[f]  
0.0041   ;[f]  
0.0046   ;[f]   
0.00061  ;[f] 
-0.0056  ;[f]   
0.0056   ;[f]  
0.2 ;[p]

; Omega prior
$OMEGA BLOCK(8)
0.2 FIX
0.0 0.2
0.0 0.0 0.2
0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.0 0.2
0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.2

$SIGMA  
0.1 ;[p]
0.1 ;[p]

; Starting with a short iterative two stage analysis brings the results closer
; so less time needs to be spent during the burn-in of the BAYES analysis
;$EST METHOD=ITS INTERACTION SIGL=4 NITER=15 PRINT=1 FILE=example6.ext NOABORT NOPRIOR=1
$EST METHOD=BAYES INTERACTION NBURN=4000 SIGL=4 NITER=10000 PRINT=25 CTYPE=3 NOABORT NOPRIOR=0
; By default, ISAMPLE_M* are 2.  Since there are many data points per subject,
; setting these to 1 is enough, and it reduces the time of the analysis
;     ISAMPLE_M1=1 ISAMPLE_M2=1 ISAMPLE_M3=1 IACCEPT=0.4
$COV MATRIX=R UNCONDITIONAL

  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.

 (MU_WARNING 20) MU_001: MU_ VARIABLE SHOULD NOT BE DEFINED AFTER VERBATIM CODE.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       17 JUN 2015
Days until program expires :5460
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha6 (nm74a6)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 RUN# example6 (from r2compl)
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:     1750
 NO. OF DATA ITEMS IN DATA SET:  11
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
   9   5   7   8   0   0  11   0   0   0   0
0LABELS FOR DATA ITEMS:
 C SET ID JID TIME CONC DOSE RATE EVID MDV CMT
0FORMAT FOR DATA:
 (2E2.0,2E3.0,E5.0,E10.0,2E5.0,3E2.0)

 TOT. NO. OF OBS RECS:     1568
 TOT. NO. OF INDIVIDUALS:     50
0LENGTH OF THETA:   9
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
  1  1  1  1
  1  1  1  1  1
  1  1  1  1  1  1
  1  1  1  1  1  1  1
  1  1  1  1  1  1  1  1
  0  0  0  0  0  0  0  0  2
  0  0  0  0  0  0  0  0  2  2
  0  0  0  0  0  0  0  0  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2  2  2  2
  0  0  0  0  0  0  0  0  2  2  2  2  2  2  2  2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
 -0.1000E+07     0.4000E+01     0.1000E+07
 -0.1000E+07    -0.2100E+01     0.1000E+07
 -0.1000E+07     0.7000E+00     0.1000E+07
 -0.1000E+07    -0.1700E+00     0.1000E+07
 -0.1000E+07     0.2200E+01     0.1000E+07
 -0.1000E+07     0.1400E+00     0.1000E+07
 -0.1000E+07     0.3700E+01     0.1000E+07
 -0.1000E+07    -0.7000E+00     0.1000E+07
  0.8000E+01     0.8000E+01     0.8000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO
                  0.2000E+00
                 -0.4300E-02   0.2000E+00
                  0.4800E-02  -0.2300E-02   0.2000E+00
                  0.3200E-02   0.5900E-02  -0.1400E-02   0.2000E+00
                  0.2900E-02   0.2703E-02  -0.2600E-03  -0.3200E-02   0.2000E+00
                 -0.2500E-02   0.9700E-03   0.2400E-02   0.1970E-02  -0.8000E-02   0.2000E+00
                  0.3100E-02  -0.5710E-02   0.3000E-02  -0.7400E-02   0.2500E-02   0.3400E-02   0.2000E+00
                  0.9730E-02   0.8620E-02   0.4100E-02   0.4600E-02   0.6100E-03  -0.5600E-02   0.5600E-02   0.2000E+00
        2                                                                                  YES
                  0.2000E+00
                  0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+00
 0.0000E+00   0.1000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 GRADIENT METHOD USED:     NOSL
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha6 (nm74a6)

 GENERAL NONLINEAR KINETICS MODEL USING LSODA (ADVAN13)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   7
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         COMP 1       ON         YES        YES        YES        YES
    2         COMP 2       ON         YES        YES        NO         NO
    3         COMP 3       ON         YES        YES        NO         NO
    4         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE(S) FROM SUBROUTINE TOL:   4
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            9           *           *           *           *
    2            *           *           *           *           *
    3            8          10           *           *           *
    4            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      9
   TIME DATA ITEM IS DATA ITEM NO.:          5
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   7
   DOSE RATE DATA ITEM IS DATA ITEM NO.:     8
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:   11

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0DURING SIMULATION, ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 OTHERWISE, ERROR SUBROUTINE CALLED ONLY WITH OBSERVATION EVENTS.
0DES SUBROUTINE USES FULL STORAGE MODE.
1
 
 
 #TBLN:      1
 #METH: MCMC Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            3480
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 ABORT WITH PRED EXIT CODE 1:             NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      4
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     4
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6classic.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 EM OR BAYESIAN METHOD USED:                MCMC BAYESIAN (BAYES)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  3
 CONVERGENCE INTERVAL (CINTERVAL):          25
 CONVERGENCE ITERATIONS (CITER):            10
 CONVERGENCE ALPHA ERROR (CALPHA):          5.000000000000000E-02
 BURN-IN ITERATIONS (NBURN):                4000
 ITERATIONS (NITER):                        10000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
 METROPOLIS HASTINGS SAMPLING FOR INDIVIDUAL ETAS:
 SAMPLES FOR GLOBAL SEARCH KERNEL (ISAMPLE_M1):          2
 SAMPLES FOR NEIGHBOR SEARCH KERNEL (ISAMPLE_M1A):       0
 SAMPLES FOR MASS/IMP/POST. MATRIX SEARCH (ISAMPLE_M1B): 2
 SAMPLES FOR LOCAL SEARCH KERNEL (ISAMPLE_M2):           2
 SAMPLES FOR LOCAL UNIVARIATE KERNEL (ISAMPLE_M3):       2
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1
 PWR. WT. MASS/IMP/POST MATRIX ACCUM. FOR ETAS (IKAPPA): 1.00000000000000
 METROPOLIS HASTINGS POPULATION SAMPLING FOR NON-GIBBS
 SAMPLED THETAS AND SIGMAS:
 PROPOSAL DENSITY SCALING RANGE
              (PSCALE_MIN, PSCALE_MAX):   1.000000000000000E-02   ,1000.00000000000
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
   1   2   3   4   5   6   7   8
 THETAS THAT ARE GIBBS SAMPLED:
   1   2   3   4   5   6   7   8
 THETAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 SIGMAS THAT ARE GIBBS SAMPLED:
   1   2
 SIGMAS THAT ARE METROPOLIS-HASTINGS SAMPLED:
 
 OMEGAS ARE GIBBS SAMPLED
 
 MONITORING OF SEARCH:

 Burn-in Mode
 iteration        -4000 MCMCOBJ=    14248.0059234505     
 iteration        -3975 MCMCOBJ=   -4623.80882286459     
 iteration        -3950 MCMCOBJ=   -5351.49139212815     
 iteration        -3925 MCMCOBJ=   -5720.94355165557     
 iteration        -3900 MCMCOBJ=   -5992.93614071414     
 iteration        -3875 MCMCOBJ=   -6151.86920524663     
 iteration        -3850 MCMCOBJ=   -6258.84762440308     
 iteration        -3825 MCMCOBJ=   -6352.41022507796     
 iteration        -3800 MCMCOBJ=   -6387.32252957843     
 iteration        -3775 MCMCOBJ=   -6357.73780614457     
 iteration        -3750 MCMCOBJ=   -6416.02811181529     
 iteration        -3725 MCMCOBJ=   -6447.27622962639     
 iteration        -3700 MCMCOBJ=   -6470.08056081431     
 iteration        -3675 MCMCOBJ=   -6456.12512592517     
 iteration        -3650 MCMCOBJ=   -6470.14942917912     
 iteration        -3625 MCMCOBJ=   -6471.72533094407     
 iteration        -3600 MCMCOBJ=   -6469.56789202118     
 iteration        -3575 MCMCOBJ=   -6480.01337849072     
 iteration        -3550 MCMCOBJ=   -6494.49218253434     
 iteration        -3525 MCMCOBJ=   -6525.91469324398     
 iteration        -3500 MCMCOBJ=   -6497.90088973032     
 iteration        -3475 MCMCOBJ=   -6515.16028993966     
 iteration        -3450 MCMCOBJ=   -6540.56589053129     
 iteration        -3425 MCMCOBJ=   -6518.00399239753     
 iteration        -3400 MCMCOBJ=   -6470.20817651026     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -6500.42905491038     
 iteration           25 MCMCOBJ=   -6529.75080605528     
 iteration           50 MCMCOBJ=   -6533.75550087911     
 iteration           75 MCMCOBJ=   -6537.47381904542     
 iteration          100 MCMCOBJ=   -6534.20097811304     
 iteration          125 MCMCOBJ=   -6523.61967215001     
 iteration          150 MCMCOBJ=   -6489.60940475662     
 iteration          175 MCMCOBJ=   -6543.02059975747     
 iteration          200 MCMCOBJ=   -6450.59905573406     
 iteration          225 MCMCOBJ=   -6492.07880348229     
 iteration          250 MCMCOBJ=   -6525.28212553011     
 iteration          275 MCMCOBJ=   -6547.96853909193     
 iteration          300 MCMCOBJ=   -6503.02057380016     
 iteration          325 MCMCOBJ=   -6425.10228585366     
 iteration          350 MCMCOBJ=   -6484.86697271422     
 iteration          375 MCMCOBJ=   -6484.49615936069     
 iteration          400 MCMCOBJ=   -6524.81229291713     
 iteration          425 MCMCOBJ=   -6492.95812247487     
 iteration          450 MCMCOBJ=   -6588.84652956096     
 iteration          475 MCMCOBJ=   -6529.25900317466     
 iteration          500 MCMCOBJ=   -6509.39023437478     
 iteration          525 MCMCOBJ=   -6534.84778996997     
 iteration          550 MCMCOBJ=   -6484.71533401883     
 iteration          575 MCMCOBJ=   -6510.73261563031     
 iteration          600 MCMCOBJ=   -6541.80980115384     
 iteration          625 MCMCOBJ=   -6508.17875161789     
 iteration          650 MCMCOBJ=   -6488.78344140480     
 iteration          675 MCMCOBJ=   -6482.15558504266     
 iteration          700 MCMCOBJ=   -6479.26548033113     
 iteration          725 MCMCOBJ=   -6564.09124575960     
 iteration          750 MCMCOBJ=   -6520.43840377447     
 iteration          775 MCMCOBJ=   -6501.56115563519     
 iteration          800 MCMCOBJ=   -6514.59828713253     
 iteration          825 MCMCOBJ=   -6496.24527722739     
 iteration          850 MCMCOBJ=   -6549.71166220382     
 iteration          875 MCMCOBJ=   -6531.26810811281     
 iteration          900 MCMCOBJ=   -6560.56216888753     
 iteration          925 MCMCOBJ=   -6549.99683987140     
 iteration          950 MCMCOBJ=   -6500.76332922856     
 iteration          975 MCMCOBJ=   -6470.44092963393     
 iteration         1000 MCMCOBJ=   -6549.54113534603     
 iteration         1025 MCMCOBJ=   -6530.42905385160     
 iteration         1050 MCMCOBJ=   -6474.16492237418     
 iteration         1075 MCMCOBJ=   -6490.94646554461     
 iteration         1100 MCMCOBJ=   -6508.72367329980     
 iteration         1125 MCMCOBJ=   -6503.45651632895     
 iteration         1150 MCMCOBJ=   -6508.53837971400     
 iteration         1175 MCMCOBJ=   -6480.75104554106     
 iteration         1200 MCMCOBJ=   -6538.40675763652     
 iteration         1225 MCMCOBJ=   -6558.08307409805     
 iteration         1250 MCMCOBJ=   -6452.16312988782     
 iteration         1275 MCMCOBJ=   -6426.26966702476     
 iteration         1300 MCMCOBJ=   -6511.35208006922     
 iteration         1325 MCMCOBJ=   -6485.92716951150     
 iteration         1350 MCMCOBJ=   -6512.56569532106     
 iteration         1375 MCMCOBJ=   -6493.22434101207     
 iteration         1400 MCMCOBJ=   -6463.54612661218     
 iteration         1425 MCMCOBJ=   -6507.00124336190     
 iteration         1450 MCMCOBJ=   -6476.38393681243     
 iteration         1475 MCMCOBJ=   -6469.40066975427     
 iteration         1500 MCMCOBJ=   -6483.75140539240     
 iteration         1525 MCMCOBJ=   -6503.62171105664     
 iteration         1550 MCMCOBJ=   -6545.73292400061     
 iteration         1575 MCMCOBJ=   -6549.26898537279     
 iteration         1600 MCMCOBJ=   -6450.57655984608     
 iteration         1625 MCMCOBJ=   -6467.77732903766     
 iteration         1650 MCMCOBJ=   -6531.44683226648     
 iteration         1675 MCMCOBJ=   -6578.33266410011     
 iteration         1700 MCMCOBJ=   -6507.65027808584     
 iteration         1725 MCMCOBJ=   -6557.07026257712     
 iteration         1750 MCMCOBJ=   -6484.09969694594     
 iteration         1775 MCMCOBJ=   -6477.94220447578     
 iteration         1800 MCMCOBJ=   -6540.63060486709     
 iteration         1825 MCMCOBJ=   -6499.83756756124     
 iteration         1850 MCMCOBJ=   -6495.29187111442     
 iteration         1875 MCMCOBJ=   -6487.96384419314     
 iteration         1900 MCMCOBJ=   -6544.10371164304     
 iteration         1925 MCMCOBJ=   -6514.19567107762     
 iteration         1950 MCMCOBJ=   -6433.25313995113     
 iteration         1975 MCMCOBJ=   -6530.06396868017     
 iteration         2000 MCMCOBJ=   -6461.06460015501     
 iteration         2025 MCMCOBJ=   -6493.98167959321     
 iteration         2050 MCMCOBJ=   -6522.34541686110     
 iteration         2075 MCMCOBJ=   -6492.05823605519     
 iteration         2100 MCMCOBJ=   -6507.98612367449     
 iteration         2125 MCMCOBJ=   -6491.99948953385     
 iteration         2150 MCMCOBJ=   -6539.78168299127     
 iteration         2175 MCMCOBJ=   -6463.22809197644     
 iteration         2200 MCMCOBJ=   -6562.56848183950     
 iteration         2225 MCMCOBJ=   -6522.18773462985     
 iteration         2250 MCMCOBJ=   -6473.19518807110     
 iteration         2275 MCMCOBJ=   -6525.99087377317     
 iteration         2300 MCMCOBJ=   -6539.10299804901     
 iteration         2325 MCMCOBJ=   -6524.89543979147     
 iteration         2350 MCMCOBJ=   -6478.17243080826     
 iteration         2375 MCMCOBJ=   -6492.41857909178     
 iteration         2400 MCMCOBJ=   -6496.27378338774     
 iteration         2425 MCMCOBJ=   -6501.49765880130     
 iteration         2450 MCMCOBJ=   -6565.75672311411     
 iteration         2475 MCMCOBJ=   -6531.16585780853     
 iteration         2500 MCMCOBJ=   -6493.48844775309     
 iteration         2525 MCMCOBJ=   -6494.43571281628     
 iteration         2550 MCMCOBJ=   -6550.85772512359     
 iteration         2575 MCMCOBJ=   -6486.64176910314     
 iteration         2600 MCMCOBJ=   -6500.69950185993     
 iteration         2625 MCMCOBJ=   -6523.30058145480     
 iteration         2650 MCMCOBJ=   -6518.91807461982     
 iteration         2675 MCMCOBJ=   -6499.98537050570     
 iteration         2700 MCMCOBJ=   -6500.15930102637     
 iteration         2725 MCMCOBJ=   -6499.47764477861     
 iteration         2750 MCMCOBJ=   -6508.32470851235     
 iteration         2775 MCMCOBJ=   -6463.03224148393     
 iteration         2800 MCMCOBJ=   -6473.52935658525     
 iteration         2825 MCMCOBJ=   -6488.03210626786     
 iteration         2850 MCMCOBJ=   -6538.18350085942     
 iteration         2875 MCMCOBJ=   -6468.44506418521     
 iteration         2900 MCMCOBJ=   -6543.08946925858     
 iteration         2925 MCMCOBJ=   -6533.17635003331     
 iteration         2950 MCMCOBJ=   -6458.52171248821     
 iteration         2975 MCMCOBJ=   -6479.81150706659     
 iteration         3000 MCMCOBJ=   -6516.35060808657     
 iteration         3025 MCMCOBJ=   -6519.95936749659     
 iteration         3050 MCMCOBJ=   -6514.30272594410     
 iteration         3075 MCMCOBJ=   -6505.02522095914     
 iteration         3100 MCMCOBJ=   -6529.95532781393     
 iteration         3125 MCMCOBJ=   -6491.79714221024     
 iteration         3150 MCMCOBJ=   -6520.99205440449     
 iteration         3175 MCMCOBJ=   -6420.85755972934     
 iteration         3200 MCMCOBJ=   -6510.97473196169     
 iteration         3225 MCMCOBJ=   -6540.19610214779     
 iteration         3250 MCMCOBJ=   -6506.77044419335     
 iteration         3275 MCMCOBJ=   -6526.10415152387     
 iteration         3300 MCMCOBJ=   -6481.47021040522     
 iteration         3325 MCMCOBJ=   -6517.78530200344     
 iteration         3350 MCMCOBJ=   -6442.66376635633     
 iteration         3375 MCMCOBJ=   -6470.17899686473     
 iteration         3400 MCMCOBJ=   -6479.15858548168     
 iteration         3425 MCMCOBJ=   -6479.25158929911     
 iteration         3450 MCMCOBJ=   -6536.19371791198     
 iteration         3475 MCMCOBJ=   -6505.88423094338     
 iteration         3500 MCMCOBJ=   -6450.26933569116     
 iteration         3525 MCMCOBJ=   -6446.42542877844     
 iteration         3550 MCMCOBJ=   -6570.51680029952     
 iteration         3575 MCMCOBJ=   -6499.03483629866     
 iteration         3600 MCMCOBJ=   -6463.18552745754     
 iteration         3625 MCMCOBJ=   -6446.81557518691     
 iteration         3650 MCMCOBJ=   -6532.67289355338     
 iteration         3675 MCMCOBJ=   -6451.24655080672     
 iteration         3700 MCMCOBJ=   -6515.54960422973     
 iteration         3725 MCMCOBJ=   -6502.82379947767     
 iteration         3750 MCMCOBJ=   -6561.06483508822     
 iteration         3775 MCMCOBJ=   -6496.39386758080     
 iteration         3800 MCMCOBJ=   -6482.81510017836     
 iteration         3825 MCMCOBJ=   -6477.46582696783     
 iteration         3850 MCMCOBJ=   -6519.62875299435     
 iteration         3875 MCMCOBJ=   -6505.63165154159     
 iteration         3900 MCMCOBJ=   -6522.86659981588     
 iteration         3925 MCMCOBJ=   -6501.41461643211     
 iteration         3950 MCMCOBJ=   -6516.51361453591     
 iteration         3975 MCMCOBJ=   -6509.92018980816     
 iteration         4000 MCMCOBJ=   -6479.11137005574     
 iteration         4025 MCMCOBJ=   -6454.20932018920     
 iteration         4050 MCMCOBJ=   -6484.02321049042     
 iteration         4075 MCMCOBJ=   -6447.22162029728     
 iteration         4100 MCMCOBJ=   -6476.50001418484     
 iteration         4125 MCMCOBJ=   -6520.45886293058     
 iteration         4150 MCMCOBJ=   -6467.13122732178     
 iteration         4175 MCMCOBJ=   -6482.20450954566     
 iteration         4200 MCMCOBJ=   -6522.19405425366     
 iteration         4225 MCMCOBJ=   -6454.06086895382     
 iteration         4250 MCMCOBJ=   -6522.89027871352     
 iteration         4275 MCMCOBJ=   -6479.14624470461     
 iteration         4300 MCMCOBJ=   -6505.93794190533     
 iteration         4325 MCMCOBJ=   -6546.60136761964     
 iteration         4350 MCMCOBJ=   -6481.44262350037     
 iteration         4375 MCMCOBJ=   -6497.28822404527     
 iteration         4400 MCMCOBJ=   -6480.69813752646     
 iteration         4425 MCMCOBJ=   -6527.72738528709     
 iteration         4450 MCMCOBJ=   -6448.38821155635     
 iteration         4475 MCMCOBJ=   -6477.50007923458     
 iteration         4500 MCMCOBJ=   -6469.29758987714     
 iteration         4525 MCMCOBJ=   -6474.91279893278     
 iteration         4550 MCMCOBJ=   -6491.36636766842     
 iteration         4575 MCMCOBJ=   -6529.03407396318     
 iteration         4600 MCMCOBJ=   -6548.22590535282     
 iteration         4625 MCMCOBJ=   -6500.10554638665     
 iteration         4650 MCMCOBJ=   -6523.45278523668     
 iteration         4675 MCMCOBJ=   -6494.77074417546     
 iteration         4700 MCMCOBJ=   -6477.09515919852     
 iteration         4725 MCMCOBJ=   -6484.85498626596     
 iteration         4750 MCMCOBJ=   -6493.89316079645     
 iteration         4775 MCMCOBJ=   -6527.45806074745     
 iteration         4800 MCMCOBJ=   -6522.29854173499     
 iteration         4825 MCMCOBJ=   -6495.97040463150     
 iteration         4850 MCMCOBJ=   -6500.52005431238     
 iteration         4875 MCMCOBJ=   -6504.28253141120     
 iteration         4900 MCMCOBJ=   -6457.49326133708     
 iteration         4925 MCMCOBJ=   -6542.48295695458     
 iteration         4950 MCMCOBJ=   -6520.65326597599     
 iteration         4975 MCMCOBJ=   -6494.71099618612     
 iteration         5000 MCMCOBJ=   -6481.85155697426     
 iteration         5025 MCMCOBJ=   -6445.82791546109     
 iteration         5050 MCMCOBJ=   -6519.96837571801     
 iteration         5075 MCMCOBJ=   -6485.62219827752     
 iteration         5100 MCMCOBJ=   -6483.40437236512     
 iteration         5125 MCMCOBJ=   -6534.62983283138     
 iteration         5150 MCMCOBJ=   -6506.88596270359     
 iteration         5175 MCMCOBJ=   -6495.25747895182     
 iteration         5200 MCMCOBJ=   -6500.81018984783     
 iteration         5225 MCMCOBJ=   -6483.79365477697     
 iteration         5250 MCMCOBJ=   -6475.09050669663     
 iteration         5275 MCMCOBJ=   -6537.73020391038     
 iteration         5300 MCMCOBJ=   -6578.13079210395     
 iteration         5325 MCMCOBJ=   -6431.22801148922     
 iteration         5350 MCMCOBJ=   -6504.13961882847     
 iteration         5375 MCMCOBJ=   -6540.46234983118     
 iteration         5400 MCMCOBJ=   -6522.11004999466     
 iteration         5425 MCMCOBJ=   -6522.82578323490     
 iteration         5450 MCMCOBJ=   -6541.79762290595     
 iteration         5475 MCMCOBJ=   -6576.86803585149     
 iteration         5500 MCMCOBJ=   -6493.39100656692     
 iteration         5525 MCMCOBJ=   -6522.01543782119     
 iteration         5550 MCMCOBJ=   -6466.58346385204     
 iteration         5575 MCMCOBJ=   -6524.86470254450     
 iteration         5600 MCMCOBJ=   -6485.33131224787     
 iteration         5625 MCMCOBJ=   -6476.40498996939     
 iteration         5650 MCMCOBJ=   -6495.19126997582     
 iteration         5675 MCMCOBJ=   -6536.38596873147     
 iteration         5700 MCMCOBJ=   -6474.39253956534     
 iteration         5725 MCMCOBJ=   -6484.49788322355     
 iteration         5750 MCMCOBJ=   -6462.12937937727     
 iteration         5775 MCMCOBJ=   -6462.30293594703     
 iteration         5800 MCMCOBJ=   -6446.93379650324     
 iteration         5825 MCMCOBJ=   -6478.99310431658     
 iteration         5850 MCMCOBJ=   -6514.43205841736     
 iteration         5875 MCMCOBJ=   -6449.22675531174     
 iteration         5900 MCMCOBJ=   -6563.95562843613     
 iteration         5925 MCMCOBJ=   -6503.14649804567     
 iteration         5950 MCMCOBJ=   -6494.57349797699     
 iteration         5975 MCMCOBJ=   -6553.90679305389     
 iteration         6000 MCMCOBJ=   -6497.89390970216     
 iteration         6025 MCMCOBJ=   -6548.07943814780     
 iteration         6050 MCMCOBJ=   -6483.81018856912     
 iteration         6075 MCMCOBJ=   -6471.56292008239     
 iteration         6100 MCMCOBJ=   -6471.50559416411     
 iteration         6125 MCMCOBJ=   -6534.82378440350     
 iteration         6150 MCMCOBJ=   -6562.27554247179     
 iteration         6175 MCMCOBJ=   -6545.67859236640     
 iteration         6200 MCMCOBJ=   -6547.80782149616     
 iteration         6225 MCMCOBJ=   -6539.50402642474     
 iteration         6250 MCMCOBJ=   -6493.49530161667     
 iteration         6275 MCMCOBJ=   -6521.15490968206     
 iteration         6300 MCMCOBJ=   -6476.13691864506     
 iteration         6325 MCMCOBJ=   -6570.26226353964     
 iteration         6350 MCMCOBJ=   -6533.74335812448     
 iteration         6375 MCMCOBJ=   -6576.98465250084     
 iteration         6400 MCMCOBJ=   -6452.96714022971     
 iteration         6425 MCMCOBJ=   -6504.58935070369     
 iteration         6450 MCMCOBJ=   -6470.45665812844     
 iteration         6475 MCMCOBJ=   -6508.34324474260     
 iteration         6500 MCMCOBJ=   -6478.05724653916     
 iteration         6525 MCMCOBJ=   -6498.90009864243     
 iteration         6550 MCMCOBJ=   -6503.95707205645     
 iteration         6575 MCMCOBJ=   -6534.85022315633     
 iteration         6600 MCMCOBJ=   -6523.72649021235     
 iteration         6625 MCMCOBJ=   -6474.18369479007     
 iteration         6650 MCMCOBJ=   -6497.75505622092     
 iteration         6675 MCMCOBJ=   -6520.41897950543     
 iteration         6700 MCMCOBJ=   -6547.28441693182     
 iteration         6725 MCMCOBJ=   -6484.97733950569     
 iteration         6750 MCMCOBJ=   -6452.01311444121     
 iteration         6775 MCMCOBJ=   -6478.92457733724     
 iteration         6800 MCMCOBJ=   -6498.30038082071     
 iteration         6825 MCMCOBJ=   -6490.27900814168     
 iteration         6850 MCMCOBJ=   -6536.65384128849     
 iteration         6875 MCMCOBJ=   -6482.18983147728     
 iteration         6900 MCMCOBJ=   -6541.70272035404     
 iteration         6925 MCMCOBJ=   -6493.78313764800     
 iteration         6950 MCMCOBJ=   -6517.80541138461     
 iteration         6975 MCMCOBJ=   -6478.86494185951     
 iteration         7000 MCMCOBJ=   -6498.08320014508     
 iteration         7025 MCMCOBJ=   -6525.90036084494     
 iteration         7050 MCMCOBJ=   -6504.52807699396     
 iteration         7075 MCMCOBJ=   -6562.41265099847     
 iteration         7100 MCMCOBJ=   -6471.12785533176     
 iteration         7125 MCMCOBJ=   -6529.74732025213     
 iteration         7150 MCMCOBJ=   -6516.01072668030     
 iteration         7175 MCMCOBJ=   -6507.39721608604     
 iteration         7200 MCMCOBJ=   -6481.14076168332     
 iteration         7225 MCMCOBJ=   -6456.84270336586     
 iteration         7250 MCMCOBJ=   -6480.45294293310     
 iteration         7275 MCMCOBJ=   -6501.17247292308     
 iteration         7300 MCMCOBJ=   -6491.15945259466     
 iteration         7325 MCMCOBJ=   -6455.78755908847     
 iteration         7350 MCMCOBJ=   -6489.31266567651     
 iteration         7375 MCMCOBJ=   -6496.75160144141     
 iteration         7400 MCMCOBJ=   -6465.05899537452     
 iteration         7425 MCMCOBJ=   -6557.48166735216     
 iteration         7450 MCMCOBJ=   -6507.47577728914     
 iteration         7475 MCMCOBJ=   -6534.18309500517     
 iteration         7500 MCMCOBJ=   -6492.23781315448     
 iteration         7525 MCMCOBJ=   -6467.65316967112     
 iteration         7550 MCMCOBJ=   -6498.55365543384     
 iteration         7575 MCMCOBJ=   -6472.39553361273     
 iteration         7600 MCMCOBJ=   -6477.44127077562     
 iteration         7625 MCMCOBJ=   -6517.20771420753     
 iteration         7650 MCMCOBJ=   -6547.11496754096     
 iteration         7675 MCMCOBJ=   -6523.41630761639     
 iteration         7700 MCMCOBJ=   -6439.57608435453     
 iteration         7725 MCMCOBJ=   -6562.02213586536     
 iteration         7750 MCMCOBJ=   -6540.38536832854     
 iteration         7775 MCMCOBJ=   -6518.24336669565     
 iteration         7800 MCMCOBJ=   -6505.36746571693     
 iteration         7825 MCMCOBJ=   -6510.75829172021     
 iteration         7850 MCMCOBJ=   -6478.34352942166     
 iteration         7875 MCMCOBJ=   -6488.82269948488     
 iteration         7900 MCMCOBJ=   -6583.92646814563     
 iteration         7925 MCMCOBJ=   -6502.47767845064     
 iteration         7950 MCMCOBJ=   -6504.09258597453     
 iteration         7975 MCMCOBJ=   -6522.50124901693     
 iteration         8000 MCMCOBJ=   -6462.52423061529     
 iteration         8025 MCMCOBJ=   -6411.48780813317     
 iteration         8050 MCMCOBJ=   -6510.66818723232     
 iteration         8075 MCMCOBJ=   -6555.07159811381     
 iteration         8100 MCMCOBJ=   -6520.44159940178     
 iteration         8125 MCMCOBJ=   -6471.70508142767     
 iteration         8150 MCMCOBJ=   -6467.82090271301     
 iteration         8175 MCMCOBJ=   -6479.52337854093     
 iteration         8200 MCMCOBJ=   -6549.02573472886     
 iteration         8225 MCMCOBJ=   -6499.49805182756     
 iteration         8250 MCMCOBJ=   -6462.65346647433     
 iteration         8275 MCMCOBJ=   -6462.27420883841     
 iteration         8300 MCMCOBJ=   -6473.32994752563     
 iteration         8325 MCMCOBJ=   -6501.20122550184     
 iteration         8350 MCMCOBJ=   -6487.52590475908     
 iteration         8375 MCMCOBJ=   -6454.01148465838     
 iteration         8400 MCMCOBJ=   -6443.30285935305     
 iteration         8425 MCMCOBJ=   -6531.01880895545     
 iteration         8450 MCMCOBJ=   -6527.48574709827     
 iteration         8475 MCMCOBJ=   -6521.72296177003     
 iteration         8500 MCMCOBJ=   -6512.61300672144     
 iteration         8525 MCMCOBJ=   -6546.05962033107     
 iteration         8550 MCMCOBJ=   -6488.57954008352     
 iteration         8575 MCMCOBJ=   -6559.71569423330     
 iteration         8600 MCMCOBJ=   -6500.10706657226     
 iteration         8625 MCMCOBJ=   -6497.17875924440     
 iteration         8650 MCMCOBJ=   -6468.85765081656     
 iteration         8675 MCMCOBJ=   -6502.53947842571     
 iteration         8700 MCMCOBJ=   -6476.01881468027     
 iteration         8725 MCMCOBJ=   -6503.66223973713     
 iteration         8750 MCMCOBJ=   -6569.47251369733     
 iteration         8775 MCMCOBJ=   -6450.99942909154     
 iteration         8800 MCMCOBJ=   -6482.53836095060     
 iteration         8825 MCMCOBJ=   -6492.92628168159     
 iteration         8850 MCMCOBJ=   -6531.96298412305     
 iteration         8875 MCMCOBJ=   -6479.23812352400     
 iteration         8900 MCMCOBJ=   -6501.72803462161     
 iteration         8925 MCMCOBJ=   -6486.93499729338     
 iteration         8950 MCMCOBJ=   -6458.92147444655     
 iteration         8975 MCMCOBJ=   -6539.85350909464     
 iteration         9000 MCMCOBJ=   -6452.84008119678     
 iteration         9025 MCMCOBJ=   -6511.81074024082     
 iteration         9050 MCMCOBJ=   -6506.34853902512     
 iteration         9075 MCMCOBJ=   -6401.00482281221     
 iteration         9100 MCMCOBJ=   -6491.59017552810     
 iteration         9125 MCMCOBJ=   -6534.13681374257     
 iteration         9150 MCMCOBJ=   -6538.54298467163     
 iteration         9175 MCMCOBJ=   -6526.86994446513     
 iteration         9200 MCMCOBJ=   -6520.42381090840     
 iteration         9225 MCMCOBJ=   -6517.98794423602     
 iteration         9250 MCMCOBJ=   -6552.11534813087     
 iteration         9275 MCMCOBJ=   -6533.78429773664     
 iteration         9300 MCMCOBJ=   -6482.97439749157     
 iteration         9325 MCMCOBJ=   -6488.70550562330     
 iteration         9350 MCMCOBJ=   -6514.67342667125     
 iteration         9375 MCMCOBJ=   -6454.77959060707     
 iteration         9400 MCMCOBJ=   -6477.61123710793     
 iteration         9425 MCMCOBJ=   -6484.56243993647     
 iteration         9450 MCMCOBJ=   -6504.03183155756     
 iteration         9475 MCMCOBJ=   -6477.10735548707     
 iteration         9500 MCMCOBJ=   -6476.95020336724     
 iteration         9525 MCMCOBJ=   -6459.84520653385     
 iteration         9550 MCMCOBJ=   -6499.04554308771     
 iteration         9575 MCMCOBJ=   -6501.63451390692     
 iteration         9600 MCMCOBJ=   -6540.76007677413     
 iteration         9625 MCMCOBJ=   -6454.58391827048     
 iteration         9650 MCMCOBJ=   -6472.96363373788     
 iteration         9675 MCMCOBJ=   -6486.34704649002     
 iteration         9700 MCMCOBJ=   -6462.09290065209     
 iteration         9725 MCMCOBJ=   -6493.75910073226     
 iteration         9750 MCMCOBJ=   -6522.42162044300     
 iteration         9775 MCMCOBJ=   -6548.52643536294     
 iteration         9800 MCMCOBJ=   -6414.30805414663     
 iteration         9825 MCMCOBJ=   -6490.41024228132     
 iteration         9850 MCMCOBJ=   -6533.40806709774     
 iteration         9875 MCMCOBJ=   -6469.95184555443     
 iteration         9900 MCMCOBJ=   -6477.62051429593     
 iteration         9925 MCMCOBJ=   -6470.57600648751     
 iteration         9950 MCMCOBJ=   -6465.37956159103     
 iteration         9975 MCMCOBJ=   -6479.01260368897     
 iteration        10000 MCMCOBJ=   -6479.16037689312     
 
 #TERM:
 BURN-IN WAS COMPLETED
 STATISTICAL PORTION WAS COMPLETED
 #TERE:
 Elapsed estimation  time in seconds:  2947.71
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6502.980       **************************************************
 #OBJS:********************************************       33.359 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.22E+00  5.55E-01 -1.83E-01  2.27E+00  2.39E-01  3.71E+00 -7.05E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.87E-01
 
 ETA2
+       -3.47E-02  2.21E-01
 
 ETA3
+        4.51E-02 -9.97E-03  1.45E-01
 
 ETA4
+        3.05E-02  5.66E-02 -1.20E-02  2.69E-01
 
 ETA5
+        2.81E-02  1.68E-02 -4.73E-04 -3.26E-02  2.11E-01
 
 ETA6
+       -2.58E-02  5.47E-03  1.43E-02  1.36E-02 -7.47E-02  2.41E-01
 
 ETA7
+        3.00E-02 -4.93E-02  3.27E-02 -7.52E-02  2.42E-02 -1.36E-03  2.51E-01
 
 ETA8
+        9.76E-02  7.34E-02  4.29E-02  4.87E-02  5.37E-03 -5.33E-02  5.85E-02  2.41E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.29E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.33E-01
 
 ETA2
+       -1.34E-01  4.66E-01
 
 ETA3
+        2.20E-01 -5.59E-02  3.78E-01
 
 ETA4
+        1.09E-01  2.30E-01 -6.27E-02  5.15E-01
 
 ETA5
+        1.13E-01  7.86E-02 -3.46E-03 -1.36E-01  4.57E-01
 
 ETA6
+       -9.87E-02  2.56E-02  7.75E-02  5.27E-02 -3.31E-01  4.87E-01
 
 ETA7
+        1.10E-01 -2.05E-01  1.70E-01 -2.87E-01  1.04E-01 -5.74E-03  4.99E-01
 
 ETA8
+        3.68E-01  3.18E-01  2.26E-01  1.89E-01  2.30E-02 -2.20E-01  2.35E-01  4.89E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.63E-02
 
 EPS2
+        0.00E+00  1.49E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         7.60E-02  7.72E-02  6.01E-02  7.52E-02  6.64E-02  7.63E-02  7.06E-02  7.10E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        6.06E-02
 
 ETA2
+        4.07E-02  5.72E-02
 
 ETA3
+        3.31E-02  3.02E-02  3.56E-02
 
 ETA4
+        4.12E-02  4.16E-02  3.32E-02  5.93E-02
 
 ETA5
+        3.73E-02  3.40E-02  2.78E-02  3.55E-02  4.52E-02
 
 ETA6
+        4.09E-02  3.93E-02  3.05E-02  4.15E-02  3.67E-02  5.83E-02
 
 ETA7
+        3.99E-02  4.10E-02  3.05E-02  4.01E-02  3.45E-02  3.84E-02  5.24E-02
 
 ETA8
+        4.15E-02  3.81E-02  3.07E-02  3.92E-02  3.40E-02  3.87E-02  3.79E-02  5.18E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.30E-04
 
 EPS2
+        0.00E+00  1.19E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.52E-02
 
 ETA2
+        1.49E-01  5.92E-02
 
 ETA3
+        1.47E-01  1.62E-01  4.56E-02
 
 ETA4
+        1.40E-01  1.53E-01  1.61E-01  5.61E-02
 
 ETA5
+        1.42E-01  1.51E-01  1.53E-01  1.40E-01  4.81E-02
 
 ETA6
+        1.48E-01  1.64E-01  1.57E-01  1.56E-01  1.39E-01  5.80E-02
 
 ETA7
+        1.40E-01  1.55E-01  1.48E-01  1.31E-01  1.42E-01  1.50E-01  5.11E-02
 
 ETA8
+        1.24E-01  1.42E-01  1.45E-01  1.40E-01  1.44E-01  1.45E-01  1.35E-01  5.14E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.26E-03
 
 EPS2
+        0.00E+00  3.98E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        5.77E-03
 
 TH 2
+       -8.02E-04  5.95E-03
 
 TH 3
+        7.53E-04 -5.88E-05  3.61E-03
 
 TH 4
+        6.36E-04  1.17E-03  1.75E-04  5.66E-03
 
 TH 5
+        4.73E-04  2.28E-04  1.14E-05 -5.67E-04  4.41E-03
 
 TH 6
+       -4.42E-04 -2.09E-04  1.21E-04  2.74E-04 -1.13E-03  5.81E-03
 
 TH 7
+        6.51E-04 -1.27E-03  6.84E-04 -1.48E-03  5.55E-04  7.80E-05  4.98E-03
 
 TH 8
+        1.90E-03  1.15E-03  1.03E-03  1.06E-03  1.89E-04 -1.11E-03  1.22E-03  5.04E-03
 
 OM11
+       -5.51E-05  1.73E-05 -1.02E-04 -2.13E-05  1.75E-05  5.11E-05 -5.63E-06 -2.10E-05  3.67E-03
 
 OM12
+        3.78E-05  2.26E-04  3.23E-05  7.05E-05 -3.17E-05 -8.79E-05 -1.35E-05 -1.85E-05 -5.13E-04  1.65E-03
 
 OM13
+       -1.45E-05  3.96E-05  7.73E-05  1.82E-05  2.03E-05 -1.13E-05 -3.85E-05  3.53E-05  4.89E-04 -8.84E-05  1.09E-03
 
 OM14
+        4.80E-05 -3.44E-05 -7.63E-05  4.23E-06 -2.00E-05 -5.18E-05  7.05E-05  5.46E-05  3.35E-04  3.26E-04  8.75E-06  1.70E-03
 
 OM15
+       -5.12E-05  4.27E-05 -1.02E-05 -2.17E-06  1.22E-05  8.00E-05 -3.24E-06  1.39E-05  3.61E-04  3.67E-05  3.86E-05 -1.88E-04
          1.39E-03
 
 OM16
+        5.45E-05  2.70E-05 -4.31E-05  5.33E-06  6.64E-05  5.94E-05 -2.64E-05  6.30E-06 -2.34E-04 -3.11E-05  1.74E-05  7.13E-05
         -3.72E-04  1.68E-03
 
 OM17
+        2.19E-05 -4.91E-05  2.26E-05  4.09E-06  6.26E-05  2.71E-05 -2.42E-05  3.39E-06  4.32E-04 -4.03E-04  2.36E-04 -4.35E-04
          1.94E-04 -1.80E-05  1.59E-03
 
 OM18
+        2.10E-05 -2.01E-05 -5.49E-06 -3.06E-05  2.63E-06 -2.46E-05 -2.28E-05  8.68E-06  1.22E-03  3.08E-04  3.70E-04  3.59E-04
          1.19E-04 -4.01E-04  4.50E-04  1.73E-03
 
 OM22
+       -1.65E-05 -8.38E-04  7.08E-05 -4.87E-06  2.78E-05  6.95E-05  1.02E-04  1.17E-04  1.62E-04 -6.68E-04  4.09E-05 -1.33E-04
          8.87E-06  3.83E-06  1.37E-04 -8.29E-05  3.27E-03
 
 OM23
+       -4.19E-05  2.19E-04  5.85E-05  8.48E-05 -2.88E-05  1.54E-05 -6.85E-06  1.39E-05 -9.58E-05  2.62E-04 -1.01E-04  6.97E-05
         -6.15E-07 -2.49E-05 -7.87E-05 -3.44E-07 -1.81E-04  9.15E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -7.13E-06 -3.93E-04  1.12E-04  4.35E-05  2.33E-05  2.16E-05  2.33E-05  7.21E-05 -4.43E-05 -1.97E-05  3.96E-05 -2.01E-04
          6.12E-05 -4.72E-05  2.84E-05 -2.01E-05  7.96E-04 -3.98E-05  1.73E-03
 
 OM25
+        2.03E-05  7.20E-06 -1.19E-06  1.77E-05  2.72E-05 -6.99E-06  3.13E-05 -3.91E-06 -4.15E-05  1.48E-04 -2.68E-06  7.51E-05
         -1.85E-04  2.01E-05 -6.90E-05  4.30E-05  1.10E-04  1.06E-05 -2.08E-04  1.15E-03
 
 OM26
+       -3.79E-05  4.38E-06  4.85E-05 -6.63E-06 -7.21E-07 -3.95E-05  2.17E-05 -9.38E-06  5.13E-05 -8.75E-05  1.94E-06 -3.94E-05
          3.65E-05 -2.36E-04  5.34E-05  5.47E-05 -1.22E-05  5.36E-05  6.98E-05 -3.25E-04  1.55E-03
 
 OM27
+       -2.07E-05  6.53E-04 -5.97E-05  2.41E-05 -4.41E-05 -4.03E-05 -3.29E-05 -6.51E-05 -5.21E-05  3.35E-04 -4.42E-05  1.01E-04
         -3.51E-05  4.49E-05 -2.73E-04 -2.56E-05 -8.99E-04  2.43E-04 -6.45E-04  1.71E-04 -1.18E-05  1.68E-03
 
 OM28
+       -4.13E-06  7.92E-05  1.45E-05  6.04E-05 -2.10E-05  3.57E-06  1.17E-05  1.67E-05 -1.52E-04  4.45E-04 -4.64E-05  8.00E-05
          2.34E-05  5.58E-05 -1.81E-04 -8.33E-05  7.27E-04  2.56E-04  3.34E-04  6.14E-05 -3.55E-04  3.32E-04  1.45E-03
 
 OM33
+        6.96E-05  2.30E-05 -1.76E-04 -6.50E-05  8.69E-06  1.36E-05 -1.69E-05  4.27E-05  8.53E-05 -2.47E-05  3.11E-04  4.11E-05
          1.09E-05  3.00E-06  6.75E-05  7.33E-05  7.45E-05 -9.80E-06 -8.11E-06  1.45E-05 -2.20E-05 -2.79E-05  2.08E-05  1.27E-03
 
 OM34
+       -3.34E-05  6.30E-05  2.66E-04  1.06E-04 -7.83E-06 -2.61E-05 -3.40E-05  6.11E-05 -7.00E-06  6.03E-05  1.81E-04  2.05E-04
         -1.89E-05  3.13E-06 -3.46E-05  5.98E-05  2.42E-05  2.19E-04  6.27E-05 -5.51E-06  3.23E-05  9.91E-06  7.39E-05 -8.03E-05
         1.10E-03
 
 OM35
+        6.72E-06 -1.14E-05 -7.94E-05 -2.06E-05  2.34E-06  1.51E-05  1.69E-05  2.10E-05  6.35E-05 -2.28E-05  1.04E-04 -2.23E-05
          1.68E-04 -2.78E-05  4.39E-05  3.00E-05 -1.27E-05  5.05E-05 -6.52E-06 -2.09E-05 -3.77E-05  2.18E-05  4.08E-06  3.46E-05
        -1.31E-04  7.75E-04
 
 OM36
+        6.22E-05 -6.50E-06  4.32E-05  1.30E-05 -8.34E-06  2.40E-05  1.13E-05  2.26E-05 -4.87E-05  2.02E-05 -5.62E-05  9.35E-06
         -3.71E-05  2.07E-04 -8.52E-06 -6.29E-05 -1.02E-05  1.06E-05  8.26E-06  2.17E-06 -2.87E-05  9.32E-06  9.77E-06  3.56E-05
         6.62E-05 -2.45E-04  9.31E-04
 
 OM37
+        5.51E-05 -1.74E-05 -8.86E-05 -3.99E-05  1.10E-05  7.43E-06 -1.19E-05  1.23E-06  6.59E-05 -5.45E-05  9.05E-05 -7.82E-05
          3.01E-05  8.41E-06  2.14E-04  8.56E-05  2.25E-05 -2.22E-04 -2.57E-05  4.76E-07 -4.43E-06 -3.38E-05 -7.18E-05  2.38E-04
        -3.16E-04  1.03E-04 -1.70E-05  9.28E-04
 
 OM38
+       -3.04E-05  5.49E-05  4.84E-05  7.30E-05  2.55E-06  1.75E-05 -4.31E-05  3.88E-05  1.48E-04  3.78E-05  4.26E-04  5.36E-05
          8.19E-06 -4.00E-05  1.12E-04  3.14E-04  4.58E-05  2.69E-04  2.92E-05 -1.49E-07  2.41E-05  2.14E-05  5.81E-05  3.90E-04
         2.14E-04  3.11E-05 -1.87E-04  2.13E-04  9.42E-04
 
 OM44
+       -8.05E-05  1.11E-04  3.29E-04  1.93E-04  7.58E-05  4.08E-06 -8.75E-07  2.84E-05  5.39E-05  8.44E-05  4.62E-05  3.70E-04
         -2.70E-05 -3.01E-05 -7.03E-05  8.91E-05  2.06E-04  2.18E-05  6.77E-04 -4.07E-05  2.90E-05 -1.49E-04  1.49E-04 -4.71E-05
         6.97E-05 -5.99E-05 -1.09E-06 -2.82E-05  3.59E-05  3.52E-03
 
 OM45
+        2.95E-05 -2.77E-05  2.59E-06  2.18E-05 -3.50E-06  3.07E-05 -1.16E-05 -5.92E-06  3.01E-05  5.85E-05 -8.31E-07  1.41E-04
          1.20E-04 -3.21E-05 -2.50E-05  4.91E-05  9.37E-06  1.59E-05  4.35E-05  2.36E-04 -2.88E-05  2.91E-05  1.69E-05  8.13E-06
         7.95E-06  2.67E-06 -1.97E-05  8.27E-06  1.45E-06 -3.57E-04  1.26E-03
 
 OM46
+        2.08E-05  1.78E-05 -6.18E-05 -1.00E-05  6.54E-05  1.65E-04  2.00E-05  1.52E-05 -6.98E-05 -4.71E-05 -7.16E-06 -1.07E-04
         -2.15E-05  1.69E-04  2.59E-05 -7.57E-05  2.79E-05  2.05E-05 -3.55E-05 -1.83E-05  2.56E-04  4.89E-05 -3.41E-05 -3.86E-07
         3.50E-05 -1.38E-05  5.80E-06 -2.78E-05  7.63E-06  1.99E-04 -3.46E-04  1.72E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        3.57E-05 -3.97E-05 -9.94E-05 -8.10E-05 -8.09E-06  4.13E-05  4.14E-05 -5.77E-06  3.22E-05  1.84E-05  1.81E-05  1.18E-04
         -2.13E-05  1.76E-05  1.04E-04  6.88E-05 -1.30E-04  1.80E-05 -4.26E-04  6.03E-05  2.43E-05  3.39E-04 -1.87E-06  1.10E-05
         1.68E-04 -6.27E-06  7.63E-06 -5.00E-05  9.02E-06 -9.31E-04  2.01E-04  1.34E-05  1.61E-03
 
 OM48
+        1.56E-05  5.53E-05  1.59E-05 -4.05E-05  1.41E-05 -1.10E-05  3.36E-05  4.06E-05  1.18E-04  1.67E-04  6.99E-05  6.03E-04
         -5.18E-05  1.47E-05 -1.11E-04  2.58E-04  1.51E-04  7.79E-05  4.78E-04 -3.86E-05 -2.56E-06 -3.42E-05  3.80E-04  3.11E-05
         3.13E-04 -3.13E-05  3.17E-05 -8.80E-05  4.90E-05  6.31E-04  6.80E-06 -2.95E-04  2.62E-04  1.53E-03
 
 OM55
+       -3.28E-05  9.81E-07 -5.49E-05  1.42E-05 -5.33E-05 -1.05E-04  9.72E-06  1.57E-05  1.18E-04 -3.47E-05  4.35E-06 -6.73E-05
          2.45E-04 -9.51E-05  2.86E-05  3.73E-05  9.14E-05  6.38E-06  2.89E-05  1.29E-04 -2.52E-05 -1.74E-05  1.09E-05  4.74E-05
        -2.83E-05 -7.02E-06 -1.38E-05  1.26E-05  5.94E-06  1.05E-04 -2.92E-04  9.61E-05 -7.04E-05 -5.98E-06  2.04E-03
 
 OM56
+       -2.07E-05 -2.98E-05  4.04E-05 -9.35E-06 -9.54E-06 -5.95E-06  5.24E-06 -9.05E-06 -3.02E-05  1.47E-05  6.27E-06  4.81E-05
         -1.57E-04  2.09E-04 -2.66E-05 -2.44E-05  1.22E-05 -1.55E-05  1.41E-07 -1.76E-05  8.53E-05  8.38E-06 -2.53E-06  4.86E-06
         8.33E-06  4.68E-05  1.14E-05  1.15E-05 -1.86E-06 -8.08E-05  1.04E-04 -2.58E-04  6.08E-06  5.13E-05 -5.89E-04  1.35E-03
 
 OM57
+       -1.23E-05  2.55E-05 -1.83E-05  1.60E-05 -2.43E-05 -1.16E-05 -2.70E-05  2.92E-06  9.31E-05 -4.78E-05  1.82E-05 -6.18E-05
          1.78E-04 -4.10E-05  1.76E-04  4.22E-05 -6.63E-06  1.59E-05  3.46E-05 -2.53E-04  5.35E-05  2.01E-05  2.86E-05 -7.31E-06
        -7.92E-06  1.49E-04 -4.65E-05  1.26E-05  5.44E-06  8.42E-05 -3.49E-04  6.97E-05 -2.14E-04 -4.16E-05  2.47E-04 -1.20E-05
          1.19E-03
 
 OM58
+       -1.48E-05  5.88E-06 -1.72E-06 -1.27E-05  3.02E-05  1.75E-05 -5.15E-06  1.10E-05  1.53E-04  5.41E-05  4.93E-05 -4.57E-05
          4.67E-04 -1.64E-04  1.10E-04  1.85E-04 -6.57E-06  3.18E-05 -2.47E-05  3.08E-04 -9.35E-05  5.52E-05  6.59E-05  1.29E-05
        -8.86E-06  2.19E-04 -7.61E-05  1.64E-05  1.95E-05 -5.56E-05  2.10E-04 -1.09E-05 -1.85E-05 -1.47E-04  6.36E-05 -2.76E-04
          3.02E-04  1.15E-03
 
 OM66
+       -2.98E-05  5.14E-05 -6.98E-05  3.74E-05  5.58E-05  9.68E-05 -3.36E-05 -3.14E-05  1.20E-04  1.06E-05  5.82E-05 -4.17E-05
          1.09E-04 -2.01E-04 -8.06E-07  4.50E-05  5.39E-05 -2.87E-05 -2.67E-06  6.09E-06 -7.33E-05  4.21E-06  6.28E-05  9.18E-06
         1.02E-05 -1.35E-05  1.45E-04  1.02E-05  3.23E-06  6.22E-05 -7.06E-05  2.00E-04 -2.82E-05 -6.13E-05  1.34E-04 -7.92E-04
         -1.51E-05  1.32E-04  3.40E-03
 
 OM67
+        8.48E-06 -3.86E-05  4.26E-05 -1.58E-06 -1.28E-05 -5.93E-05  9.77E-06  1.38E-05  4.06E-06  4.37E-05  2.04E-05  2.33E-05
         -3.70E-05  1.85E-04 -1.12E-04 -6.44E-05 -1.62E-05 -3.20E-05  2.33E-05  6.84E-05 -3.64E-04 -4.19E-05  5.27E-05  2.93E-05
        -3.24E-05 -2.42E-05  1.69E-04  6.41E-05 -2.66E-05 -4.48E-05  6.79E-05 -4.66E-04  2.32E-05  8.93E-05 -5.39E-05  1.87E-04
         -3.49E-04 -1.36E-04 -1.09E-05  1.47E-03
 
 OM68
+        2.12E-05 -5.76E-05  2.89E-05 -2.09E-05  2.54E-05  4.36E-05  1.92E-05 -3.83E-06 -1.05E-04 -9.82E-06 -2.73E-05  1.37E-05
         -1.37E-04  5.78E-04 -3.04E-05 -2.23E-04 -5.69E-05 -7.80E-06  1.48E-05 -5.63E-05  4.19E-04  2.16E-05 -7.37E-05  9.73E-06
         4.87E-06 -7.53E-05  2.79E-04  1.78E-05 -1.95E-05  3.30E-05 -4.79E-05  2.87E-04  4.49E-05  3.70E-05 -6.93E-05  1.16E-04
         -1.22E-04 -3.57E-04 -6.65E-04  3.33E-04  1.50E-03
 
 OM77
+        3.92E-05 -9.45E-05  1.08E-05  6.46E-05  2.36E-05  1.04E-05 -3.29E-05  2.84E-05  9.63E-05 -1.10E-04  1.80E-05 -1.05E-04
          7.40E-05 -3.14E-05  3.42E-04  9.44E-05  2.14E-04 -6.59E-05  2.00E-04 -6.71E-05 -1.67E-05 -6.23E-04 -1.23E-04  6.79E-05
        -1.02E-04  2.88E-05 -1.73E-05  3.20E-04  8.88E-05  3.28E-04 -8.08E-05 -3.82E-05 -8.18E-04 -1.84E-04  9.66E-05 -3.53E-05
          3.08E-04  1.09E-04  8.30E-05  2.98E-05 -3.52E-05  2.75E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        7.38E-06 -6.52E-05 -1.62E-05  8.27E-06  3.77E-05 -4.05E-06  1.66E-05  6.10E-05  1.44E-04 -7.78E-05  8.29E-05 -1.26E-04
          5.44E-05 -5.43E-05  5.62E-04  2.91E-04 -1.42E-04 -2.73E-05 -1.60E-04  3.85E-05  8.13E-05  2.37E-04 -2.15E-04  6.03E-05
        -6.08E-05  3.36E-05 -3.71E-05  3.03E-04  2.37E-04 -1.51E-04  3.98E-05  7.13E-05  1.61E-04 -3.37E-04  1.88E-05 -6.05E-05
          7.73E-05  1.68E-04 -7.66E-06 -3.04E-04 -5.47E-05  6.89E-04  1.44E-03
 
 OM88
+       -5.06E-06  8.67E-05  3.42E-05  5.54E-05  5.57E-05  9.25E-06 -1.66E-05  8.67E-05  4.42E-04  2.68E-04  2.22E-04  2.38E-04
          3.69E-05 -2.39E-04  2.44E-04  1.04E-03  2.11E-04  1.44E-04  1.87E-04  2.20E-05 -1.14E-04  1.57E-04  7.34E-04  1.28E-04
         1.15E-04  8.84E-06 -1.15E-04  1.17E-04  5.40E-04  2.11E-04  2.93E-05 -1.20E-04  9.27E-05  5.65E-04  5.19E-05 -1.86E-05
          4.40E-05  1.12E-04  1.63E-04 -1.74E-04 -5.69E-04  2.38E-04  6.54E-04  2.68E-03
 
 SG11
+       -5.17E-07  3.62E-07  5.54E-08  3.22E-07  5.04E-07 -1.27E-07 -1.80E-07  2.25E-07  2.36E-07  6.40E-07 -5.53E-07 -2.07E-07
         -2.34E-07  1.15E-07 -3.78E-07 -6.14E-07 -1.76E-06 -1.53E-08 -2.42E-08 -1.18E-07  1.16E-07  7.79E-07 -1.30E-07 -7.86E-07
        -3.32E-07  2.91E-07  6.78E-08 -2.45E-07 -7.09E-07 -5.59E-07 -3.09E-07 -4.90E-07  7.92E-09 -5.90E-07  2.00E-07  1.88E-07
          4.89E-07 -1.31E-08  9.76E-07  1.18E-07  2.36E-08 -1.16E-07 -9.26E-08 -9.25E-07  3.97E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.40E-06 -1.19E-06  3.31E-07 -4.74E-07 -9.18E-07 -1.14E-07  1.21E-06 -1.08E-07 -8.13E-07 -3.05E-07  2.03E-07 -2.73E-08
          3.44E-07 -7.05E-07 -2.29E-07 -1.41E-07  5.00E-07  4.15E-07  8.82E-08 -4.95E-07 -6.66E-09  6.88E-08 -1.58E-07 -9.40E-07
        -2.73E-07 -9.03E-08  4.33E-07 -4.48E-07 -2.54E-07  4.74E-07 -1.63E-07  6.79E-07  5.07E-09  8.38E-07  4.36E-07 -2.59E-07
         -1.83E-07 -3.37E-07 -2.82E-06 -6.84E-07  3.43E-07  3.22E-07 -1.77E-07 -2.90E-07 -3.55E-08  0.00E+00  1.42E-06
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        7.60E-02
 
 TH 2
+       -1.37E-01  7.72E-02
 
 TH 3
+        1.65E-01 -1.27E-02  6.01E-02
 
 TH 4
+        1.11E-01  2.01E-01  3.87E-02  7.52E-02
 
 TH 5
+        9.37E-02  4.45E-02  2.86E-03 -1.13E-01  6.64E-02
 
 TH 6
+       -7.64E-02 -3.55E-02  2.64E-02  4.77E-02 -2.23E-01  7.63E-02
 
 TH 7
+        1.21E-01 -2.33E-01  1.61E-01 -2.78E-01  1.18E-01  1.45E-02  7.06E-02
 
 TH 8
+        3.53E-01  2.10E-01  2.42E-01  1.98E-01  4.00E-02 -2.05E-01  2.44E-01  7.10E-02
 
 OM11
+       -1.20E-02  3.69E-03 -2.80E-02 -4.67E-03  4.34E-03  1.11E-02 -1.32E-03 -4.89E-03  6.06E-02
 
 OM12
+        1.22E-02  7.22E-02  1.32E-02  2.31E-02 -1.18E-02 -2.83E-02 -4.70E-03 -6.42E-03 -2.08E-01  4.07E-02
 
 OM13
+       -5.77E-03  1.55E-02  3.89E-02  7.30E-03  9.26E-03 -4.47E-03 -1.65E-02  1.50E-02  2.44E-01 -6.57E-02  3.31E-02
 
 OM14
+        1.54E-02 -1.08E-02 -3.08E-02  1.36E-03 -7.33E-03 -1.65E-02  2.43E-02  1.87E-02  1.34E-01  1.95E-01  6.43E-03  4.12E-02
 
 OM15
+       -1.81E-02  1.48E-02 -4.55E-03 -7.75E-04  4.94E-03  2.81E-02 -1.23E-03  5.23E-03  1.60E-01  2.42E-02  3.13E-02 -1.23E-01
          3.73E-02
 
 OM16
+        1.75E-02  8.54E-03 -1.75E-02  1.73E-03  2.44E-02  1.90E-02 -9.13E-03  2.17E-03 -9.44E-02 -1.87E-02  1.28E-02  4.23E-02
         -2.44E-01  4.09E-02
 
 OM17
+        7.22E-03 -1.59E-02  9.41E-03  1.36E-03  2.36E-02  8.89E-03 -8.58E-03  1.19E-03  1.78E-01 -2.48E-01  1.79E-01 -2.64E-01
          1.31E-01 -1.10E-02  3.99E-02
 
 OM18
+        6.65E-03 -6.27E-03 -2.20E-03 -9.79E-03  9.53E-04 -7.76E-03 -7.78E-03  2.94E-03  4.83E-01  1.82E-01  2.69E-01  2.10E-01
          7.68E-02 -2.36E-01  2.71E-01  4.15E-02
 
 OM22
+       -3.79E-03 -1.90E-01  2.06E-02 -1.13E-03  7.31E-03  1.59E-02  2.52E-02  2.88E-02  4.66E-02 -2.87E-01  2.16E-02 -5.64E-02
          4.16E-03  1.64E-03  5.98E-02 -3.49E-02  5.72E-02
 
 OM23
+       -1.83E-02  9.37E-02  3.22E-02  3.73E-02 -1.43E-02  6.67E-03 -3.21E-03  6.48E-03 -5.23E-02  2.13E-01 -1.01E-01  5.59E-02
         -5.46E-04 -2.01E-02 -6.51E-02 -2.74E-04 -1.04E-01  3.02E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -2.26E-03 -1.22E-01  4.48E-02  1.39E-02  8.44E-03  6.81E-03  7.93E-03  2.44E-02 -1.76E-02 -1.16E-02  2.88E-02 -1.18E-01
          3.95E-02 -2.77E-02  1.71E-02 -1.16E-02  3.35E-01 -3.17E-02  4.16E-02
 
 OM25
+        7.86E-03  2.75E-03 -5.84E-04  6.92E-03  1.21E-02 -2.70E-03  1.30E-02 -1.62E-03 -2.02E-02  1.07E-01 -2.39E-03  5.37E-02
         -1.46E-01  1.44E-02 -5.09E-02  3.04E-02  5.67E-02  1.03E-02 -1.47E-01  3.40E-02
 
 OM26
+       -1.27E-02  1.44E-03  2.05E-02 -2.24E-03 -2.76E-04 -1.32E-02  7.80E-03 -3.36E-03  2.15E-02 -5.47E-02  1.49E-03 -2.43E-02
          2.49E-02 -1.46E-01  3.40E-02  3.34E-02 -5.41E-03  4.51E-02  4.27E-02 -2.43E-01  3.93E-02
 
 OM27
+       -6.65E-03  2.06E-01 -2.42E-02  7.82E-03 -1.62E-02 -1.29E-02 -1.14E-02 -2.24E-02 -2.10E-02  2.01E-01 -3.26E-02  5.97E-02
         -2.30E-02  2.67E-02 -1.67E-01 -1.50E-02 -3.83E-01  1.96E-01 -3.78E-01  1.23E-01 -7.33E-03  4.10E-02
 
 OM28
+       -1.43E-03  2.70E-02  6.33E-03  2.11E-02 -8.31E-03  1.23E-03  4.36E-03  6.18E-03 -6.58E-02  2.87E-01 -3.69E-02  5.11E-02
          1.65E-02  3.58E-02 -1.19E-01 -5.26E-02  3.34E-01  2.22E-01  2.11E-01  4.74E-02 -2.37E-01  2.12E-01  3.81E-02
 
 OM33
+        2.57E-02  8.38E-03 -8.24E-02 -2.42E-02  3.67E-03  5.00E-03 -6.73E-03  1.69E-02  3.95E-02 -1.70E-02  2.64E-01  2.80E-02
          8.23E-03  2.05E-03  4.74E-02  4.95E-02  3.65E-02 -9.09E-03 -5.47E-03  1.20E-02 -1.57E-02 -1.91E-02  1.53E-02  3.56E-02
 
 OM34
+       -1.32E-02  2.46E-02  1.33E-01  4.24E-02 -3.55E-03 -1.03E-02 -1.45E-02  2.59E-02 -3.48E-03  4.47E-02  1.65E-01  1.50E-01
         -1.53E-02  2.31E-03 -2.61E-02  4.34E-02  1.28E-02  2.18E-01  4.54E-02 -4.89E-03  2.47E-02  7.29E-03  5.85E-02 -6.79E-02
         3.32E-02
 
 OM35
+        3.18E-03 -5.31E-03 -4.74E-02 -9.81E-03  1.27E-03  7.09E-03  8.61E-03  1.06E-02  3.77E-02 -2.01E-02  1.13E-01 -1.95E-02
          1.62E-01 -2.44E-02  3.95E-02  2.59E-02 -7.96E-03  6.00E-02 -5.63E-03 -2.21E-02 -3.45E-02  1.91E-02  3.85E-03  3.49E-02
        -1.42E-01  2.78E-02
 
 OM36
+        2.68E-02 -2.76E-03  2.35E-02  5.68E-03 -4.12E-03  1.03E-02  5.24E-03  1.04E-02 -2.63E-02  1.63E-02 -5.56E-02  7.44E-03
         -3.26E-02  1.66E-01 -6.99E-03 -4.96E-02 -5.84E-03  1.15E-02  6.50E-03  2.09E-03 -2.39E-02  7.45E-03  8.40E-03  3.27E-02
         6.54E-02 -2.89E-01  3.05E-02
 
 OM37
+        2.38E-02 -7.41E-03 -4.84E-02 -1.74E-02  5.43E-03  3.20E-03 -5.54E-03  5.71E-04  3.57E-02 -4.40E-02  8.99E-02 -6.24E-02
          2.65E-02  6.74E-03  1.76E-01  6.76E-02  1.29E-02 -2.41E-01 -2.03E-02  4.60E-04 -3.70E-03 -2.71E-02 -6.19E-02  2.19E-01
        -3.13E-01  1.21E-01 -1.83E-02  3.05E-02
 
 OM38
+       -1.30E-02  2.32E-02  2.63E-02  3.16E-02  1.25E-03  7.47E-03 -1.99E-02  1.78E-02  7.94E-02  3.03E-02  4.19E-01  4.24E-02
          7.16E-03 -3.19E-02  9.10E-02  2.46E-01  2.61E-02  2.90E-01  2.29E-02 -1.43E-04  1.99E-02  1.70E-02  4.97E-02  3.57E-01
         2.10E-01  3.64E-02 -2.00E-01  2.28E-01  3.07E-02
 
 OM44
+       -1.79E-02  2.43E-02  9.22E-02  4.33E-02  1.92E-02  9.02E-04 -2.09E-04  6.75E-03  1.50E-02  3.50E-02  2.35E-02  1.51E-01
         -1.22E-02 -1.24E-02 -2.97E-02  3.62E-02  6.08E-02  1.21E-02  2.75E-01 -2.02E-02  1.24E-02 -6.13E-02  6.60E-02 -2.23E-02
         3.54E-02 -3.63E-02 -6.03E-04 -1.56E-02  1.97E-02  5.93E-02
 
 OM45
+        1.09E-02 -1.01E-02  1.21E-03  8.15E-03 -1.48E-03  1.13E-02 -4.64E-03 -2.35E-03  1.40E-02  4.05E-02 -7.08E-04  9.62E-02
          9.10E-02 -2.21E-02 -1.76E-02  3.33E-02  4.61E-03  1.48E-02  2.95E-02  1.96E-01 -2.06E-02  2.00E-02  1.25E-02  6.42E-03
         6.75E-03  2.70E-03 -1.82E-02  7.64E-03  1.33E-03 -1.69E-01  3.55E-02
 
 OM46
+        6.60E-03  5.56E-03 -2.48E-02 -3.21E-03  2.37E-02  5.21E-02  6.82E-03  5.17E-03 -2.78E-02 -2.79E-02 -5.22E-03 -6.26E-02
         -1.39E-02  9.96E-02  1.56E-02 -4.39E-02  1.17E-02  1.64E-02 -2.06E-02 -1.30E-02  1.57E-01  2.87E-02 -2.16E-02 -2.61E-04
         2.55E-02 -1.19E-02  4.58E-03 -2.20E-02  5.99E-03  8.10E-02 -2.35E-01  4.15E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.17E-02 -1.28E-02 -4.12E-02 -2.69E-02 -3.04E-03  1.35E-02  1.46E-02 -2.03E-03  1.32E-02  1.13E-02  1.37E-02  7.12E-02
         -1.42E-02  1.07E-02  6.51E-02  4.13E-02 -5.67E-02  1.49E-02 -2.56E-01  4.43E-02  1.54E-02  2.06E-01 -1.22E-03  7.68E-03
         1.26E-01 -5.62E-03  6.23E-03 -4.09E-02  7.33E-03 -3.91E-01  1.41E-01  8.04E-03  4.01E-02
 
 OM48
+        5.26E-03  1.83E-02  6.74E-03 -1.38E-02  5.42E-03 -3.68E-03  1.21E-02  1.46E-02  4.97E-02  1.05E-01  5.40E-02  3.74E-01
         -3.55E-02  9.17E-03 -7.11E-02  1.59E-01  6.72E-02  6.57E-02  2.93E-01 -2.90E-02 -1.66E-03 -2.13E-02  2.55E-01  2.23E-02
         2.41E-01 -2.87E-02  2.65E-02 -7.37E-02  4.08E-02  2.71E-01  4.89E-03 -1.82E-01  1.67E-01  3.92E-02
 
 OM55
+       -9.55E-03  2.82E-04 -2.02E-02  4.19E-03 -1.78E-02 -3.06E-02  3.05E-03  4.90E-03  4.31E-02 -1.89E-02  2.91E-03 -3.62E-02
          1.46E-01 -5.14E-02  1.59E-02  1.99E-02  3.54E-02  4.67E-03  1.54E-02  8.42E-02 -1.42E-02 -9.41E-03  6.35E-03  2.95E-02
        -1.89E-02 -5.58E-03 -1.00E-02  9.17E-03  4.28E-03  3.93E-02 -1.82E-01  5.13E-02 -3.89E-02 -3.38E-03  4.52E-02
 
 OM56
+       -7.40E-03 -1.05E-02  1.83E-02 -3.38E-03 -3.91E-03 -2.12E-03  2.02E-03 -3.47E-03 -1.36E-02  9.88E-03  5.16E-03  3.18E-02
         -1.15E-01  1.39E-01 -1.82E-02 -1.60E-02  5.82E-03 -1.39E-02  9.22E-05 -1.41E-02  5.90E-02  5.57E-03 -1.81E-03  3.72E-03
         6.84E-03  4.58E-02  1.02E-02  1.03E-02 -1.65E-03 -3.71E-02  7.99E-02 -1.69E-01  4.13E-03  3.57E-02 -3.55E-01  3.67E-02
 
 OM57
+       -4.67E-03  9.57E-03 -8.82E-03  6.16E-03 -1.06E-02 -4.40E-03 -1.11E-02  1.19E-03  4.45E-02 -3.41E-02  1.59E-02 -4.35E-02
          1.39E-01 -2.90E-02  1.28E-01  2.94E-02 -3.36E-03  1.52E-02  2.41E-02 -2.16E-01  3.94E-02  1.42E-02  2.18E-02 -5.95E-03
        -6.92E-03  1.56E-01 -4.41E-02  1.20E-02  5.13E-03  4.11E-02 -2.85E-01  4.86E-02 -1.55E-01 -3.08E-02  1.59E-01 -9.45E-03
          3.45E-02
 
 OM58
+       -5.72E-03  2.24E-03 -8.43E-04 -4.98E-03  1.34E-02  6.75E-03 -2.15E-03  4.57E-03  7.43E-02  3.91E-02  4.39E-02 -3.26E-02
          3.69E-01 -1.18E-01  8.12E-02  1.31E-01 -3.38E-03  3.10E-02 -1.75E-02  2.67E-01 -6.99E-02  3.96E-02  5.09E-02  1.07E-02
        -7.86E-03  2.32E-01 -7.34E-02  1.58E-02  1.87E-02 -2.76E-02  1.74E-01 -7.73E-03 -1.36E-02 -1.11E-01  4.15E-02 -2.21E-01
          2.58E-01  3.40E-02
 
 OM66
+       -6.72E-03  1.14E-02 -1.99E-02  8.52E-03  1.44E-02  2.18E-02 -8.16E-03 -7.58E-03  3.40E-02  4.48E-03  3.02E-02 -1.74E-02
          5.01E-02 -8.43E-02 -3.46E-04  1.86E-02  1.62E-02 -1.63E-02 -1.10E-03  3.08E-03 -3.19E-02  1.76E-03  2.83E-02  4.42E-03
         5.26E-03 -8.34E-03  8.14E-02  5.77E-03  1.80E-03  1.80E-02 -3.41E-02  8.29E-02 -1.21E-02 -2.69E-02  5.11E-02 -3.70E-01
         -7.52E-03  6.65E-02  5.83E-02
 
 OM67
+        2.91E-03 -1.30E-02  1.85E-02 -5.46E-04 -5.01E-03 -2.03E-02  3.61E-03  5.05E-03  1.74E-03  2.80E-02  1.61E-02  1.47E-02
         -2.59E-02  1.18E-01 -7.31E-02 -4.04E-02 -7.38E-03 -2.76E-02  1.46E-02  5.24E-02 -2.41E-01 -2.66E-02  3.60E-02  2.15E-02
        -2.54E-02 -2.27E-02  1.44E-01  5.48E-02 -2.25E-02 -1.97E-02  4.98E-02 -2.93E-01  1.51E-02  5.94E-02 -3.11E-02  1.33E-01
         -2.63E-01 -1.04E-01 -4.88E-03  3.84E-02
 
 OM68
+        7.22E-03 -1.93E-02  1.24E-02 -7.18E-03  9.88E-03  1.48E-02  7.01E-03 -1.39E-03 -4.49E-02 -6.24E-03 -2.13E-02  8.60E-03
         -9.50E-02  3.64E-01 -1.96E-02 -1.38E-01 -2.57E-02 -6.66E-03  9.18E-03 -4.28E-02  2.75E-01  1.36E-02 -5.00E-02  7.05E-03
         3.79E-03 -6.98E-02  2.36E-01  1.51E-02 -1.64E-02  1.44E-02 -3.48E-02  1.79E-01  2.89E-02  2.44E-02 -3.96E-02  8.14E-02
         -9.09E-02 -2.71E-01 -2.94E-01  2.24E-01  3.87E-02
 
 OM77
+        9.84E-03 -2.34E-02  3.42E-03  1.64E-02  6.77E-03  2.61E-03 -8.89E-03  7.62E-03  3.03E-02 -5.15E-02  1.04E-02 -4.88E-02
          3.78E-02 -1.46E-02  1.63E-01  4.33E-02  7.13E-02 -4.15E-02  9.16E-02 -3.77E-02 -8.08E-03 -2.90E-01 -6.17E-02  3.63E-02
        -5.87E-02  1.97E-02 -1.08E-02  2.01E-01  5.52E-02  1.05E-01 -4.34E-02 -1.76E-02 -3.89E-01 -8.97E-02  4.08E-02 -1.84E-02
          1.70E-01  6.12E-02  2.72E-02  1.48E-02 -1.73E-02  5.24E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        2.56E-03 -2.23E-02 -7.13E-03  2.90E-03  1.50E-02 -1.40E-03  6.19E-03  2.27E-02  6.27E-02 -5.05E-02  6.61E-02 -8.06E-02
          3.85E-02 -3.50E-02  3.71E-01  1.84E-01 -6.55E-02 -2.38E-02 -1.02E-01  2.98E-02  5.45E-02  1.53E-01 -1.49E-01  4.46E-02
        -4.83E-02  3.18E-02 -3.21E-02  2.62E-01  2.03E-01 -6.70E-02  2.96E-02  4.53E-02  1.06E-01 -2.27E-01  1.10E-02 -4.35E-02
          5.90E-02  1.30E-01 -3.47E-03 -2.09E-01 -3.72E-02  3.47E-01  3.79E-02
 
 OM88
+       -1.29E-03  2.17E-02  1.10E-02  1.42E-02  1.62E-02  2.34E-03 -4.54E-03  2.36E-02  1.41E-01  1.27E-01  1.30E-01  1.12E-01
          1.91E-02 -1.13E-01  1.18E-01  4.84E-01  7.13E-02  9.18E-02  8.68E-02  1.25E-02 -5.61E-02  7.41E-02  3.72E-01  6.93E-02
         6.69E-02  6.14E-03 -7.28E-02  7.43E-02  3.40E-01  6.88E-02  1.59E-02 -5.59E-02  4.46E-02  2.79E-01  2.22E-02 -9.78E-03
          2.46E-02  6.37E-02  5.41E-02 -8.75E-02 -2.84E-01  8.75E-02  3.33E-01  5.18E-02
 
 SG11
+       -1.08E-02  7.45E-03  1.46E-03  6.78E-03  1.20E-02 -2.65E-03 -4.04E-03  5.03E-03  6.19E-03  2.50E-02 -2.65E-02 -7.98E-03
         -9.97E-03  4.46E-03 -1.50E-02 -2.34E-02 -4.89E-02 -8.05E-04 -9.22E-04 -5.53E-03  4.68E-03  3.02E-02 -5.43E-03 -3.50E-02
        -1.59E-02  1.66E-02  3.52E-03 -1.28E-02 -3.66E-02 -1.50E-02 -1.38E-02 -1.87E-02  3.13E-04 -2.39E-02  7.04E-03  8.13E-03
          2.25E-02 -6.10E-04  2.66E-02  4.89E-03  9.68E-04 -3.51E-03 -3.87E-03 -2.84E-02  6.30E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.55E-02 -1.29E-02  4.62E-03 -5.29E-03 -1.16E-02 -1.25E-03  1.44E-02 -1.28E-03 -1.13E-02 -6.30E-03  5.15E-03 -5.56E-04
          7.75E-03 -1.45E-02 -4.81E-03 -2.84E-03  7.34E-03  1.15E-02  1.78E-03 -1.22E-02 -1.42E-04  1.41E-03 -3.49E-03 -2.21E-02
        -6.91E-03 -2.72E-03  1.19E-02 -1.24E-02 -6.96E-03  6.71E-03 -3.86E-03  1.37E-02  1.06E-04  1.79E-02  8.10E-03 -5.93E-03
         -4.45E-03 -8.32E-03 -4.06E-02 -1.50E-02  7.42E-03  5.15E-03 -3.93E-03 -4.70E-03 -4.73E-02  0.00E+00  1.19E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        2.18E+02
 
 TH 2
+        5.74E+01  2.31E+02
 
 TH 3
+       -2.31E+01  5.53E+00  3.17E+02
 
 TH 4
+       -2.05E+01 -2.08E+01 -1.84E-01  2.18E+02
 
 TH 5
+       -2.71E+01 -2.93E+01  2.31E+00  1.90E+01  2.51E+02
 
 TH 6
+       -2.33E+00 -1.14E+01 -1.90E+01 -2.08E+01  5.10E+01  1.97E+02
 
 TH 7
+        7.64E+00  6.81E+01 -2.70E+01  7.38E+01 -3.29E+01 -2.96E+01  2.72E+02
 
 TH 8
+       -8.88E+01 -9.17E+01 -5.48E+01 -5.61E+01  2.28E+01  6.07E+01 -1.00E+02  3.14E+02
 
 OM11
+        9.93E-01 -7.78E+00  6.01E+00 -4.09E+00 -1.41E+00 -1.26E+00 -5.22E+00  4.85E+00  4.54E+02
 
 OM12
+       -4.05E+00 -8.20E+00 -1.26E+01 -1.12E+01  8.97E-01  1.14E+01 -5.41E+00  1.37E+01  2.84E+02  1.23E+03
 
 OM13
+        6.64E+00 -8.08E+00 -2.84E+01  1.17E+00 -2.73E+00  4.02E+00  4.09E+00  5.67E-01 -1.05E+02  2.81E+01  1.42E+03
 
 OM14
+        8.43E-01  2.22E+01  2.55E+01 -6.95E+00  1.87E+00  2.43E+00 -7.91E+00 -1.50E+01 -7.55E+01 -8.52E+01  6.55E+01  9.41E+02
 
 OM15
+        5.22E+00 -2.81E+00  6.82E+00  1.21E+00 -7.73E+00 -1.65E+01 -3.25E-01 -9.80E+00 -1.40E+02 -2.16E+02  2.31E+01  1.09E+02
          1.10E+03
 
 OM16
+       -7.31E+00 -5.79E+00  1.23E+01  2.11E+00 -8.39E+00 -6.41E+00  4.72E+00 -1.64E+00 -5.01E+01 -1.39E+02 -9.37E+01 -7.13E+01
          2.80E+02  9.29E+02
 
 OM17
+       -8.92E+00 -1.96E+01 -6.32E+00 -7.89E+00 -5.98E+00  2.88E+00 -6.32E+00  1.71E+01  5.34E+01  3.58E+02 -1.00E+02  3.05E+02
         -1.51E+02 -1.54E+02  1.11E+03
 
 OM18
+       -7.48E+00  1.37E+01  8.12E+00  1.51E+01  1.01E+01  2.15E+00  1.12E+01 -4.41E+00 -4.25E+02 -6.69E+02 -1.81E+02 -2.01E+02
          2.11E+02  3.62E+02 -5.01E+02  1.66E+03
 
 OM22
+        1.45E+01  3.96E+01 -4.78E+00 -3.64E+00 -8.13E+00 -5.47E+00  9.24E+00 -2.18E+01  4.66E+01  4.20E+02  5.55E+01 -5.56E-01
         -8.66E+01 -9.51E+01  1.37E+02 -2.39E+02  6.86E+02
 
 OM23
+        4.04E+00 -2.28E+01 -7.41E+00 -2.08E+00  8.45E-01 -7.46E+00 -7.65E+00  2.09E+00 -4.27E+01 -1.16E+02  4.62E+02  1.86E+01
          4.17E+01 -2.00E+01 -9.08E+01  1.87E+01  1.30E+02  1.77E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        7.64E+00  3.43E+01  1.34E+00 -7.46E+00 -5.16E+00 -4.25E+00  7.91E+00 -1.71E+01 -1.81E+01 -5.90E+01 -2.56E-01  2.90E+02
          5.21E+01  8.46E+00  9.84E+01 -1.94E+01 -7.25E+01  5.74E+01  1.06E+03
 
 OM25
+       -2.12E+00  2.76E+00  2.33E+00 -9.28E+00 -4.08E+00  3.65E-01 -1.03E+01  3.27E+00 -5.06E+01 -2.84E+02 -3.45E+01  3.24E+01
          4.29E+02  1.97E+02 -1.16E+02  1.89E+02 -2.67E+02 -4.26E+01  1.61E+02  1.47E+03
 
 OM26
+       -4.64E-01 -8.49E+00 -7.41E+00 -5.76E+00  9.09E+00  1.82E+01 -1.18E+01  1.12E+01 -3.09E+01 -1.82E+02 -7.87E+01 -1.06E+01
          1.39E+02  3.50E+02 -1.04E+02  2.06E+02 -2.33E+02 -2.19E+02 -8.19E+01  4.52E+02  1.18E+03
 
 OM27
+       -2.03E+01 -7.44E+01  1.74E-01  1.71E+00  1.21E+01  6.71E+00 -1.89E+01  3.83E+01  1.20E+01  2.36E+02 -4.72E+00  1.24E+02
         -7.21E+01 -1.10E+02  3.75E+02 -2.36E+02  4.48E+02 -6.61E+01  4.00E+02 -2.73E+02 -2.82E+02  1.43E+03
 
 OM28
+       -5.57E+00 -6.40E+00  7.93E+00 -2.80E+00  1.65E+01  1.17E+01 -1.06E+01  9.82E+00 -1.39E+02 -7.79E+02 -1.26E+02 -5.75E+01
          1.56E+02  2.43E+02 -3.50E+02  7.99E+02 -7.55E+02 -4.03E+02 -2.77E+02  3.96E+02  6.77E+02 -9.10E+02  2.31E+03
 
 OM33
+       -1.61E+01 -5.59E+00  4.83E+01  1.85E+01  7.05E-01 -6.61E+00  4.14E+00 -1.44E+01 -5.76E+00 -2.10E+01 -1.56E+02 -3.76E+01
          1.41E+01  3.46E+01 -2.65E+01  8.30E+01 -1.34E+01  8.37E+01  2.33E+01  9.15E+00  1.73E+00  1.21E+00 -1.80E+01  1.01E+03
 
 OM34
+        9.99E+00 -2.96E+00 -7.26E+01 -1.13E+01  4.42E-01  1.09E+01  1.28E+01  3.57E+00  1.92E+01 -7.85E+00 -1.76E+02 -8.16E+01
         -1.05E+01  1.97E+01 -2.45E+01  5.96E+01 -1.25E+01 -8.89E+01  7.02E+00  1.42E+01 -4.20E+00  1.64E+01  8.50E+00  1.63E+02
         1.31E+03
 
 OM35
+       -4.91E+00  8.75E+00  2.28E+01 -1.75E+00  3.24E+00 -2.58E+00 -4.93E+00 -1.24E+01  2.51E+01  2.47E+01 -2.73E+02 -3.30E+01
         -9.46E+01  1.06E+01  2.16E+01  3.39E+01 -4.11E+01 -3.41E+02 -2.61E+01  1.11E+02  1.44E+02 -3.75E+01  1.32E+02 -2.05E+01
         1.60E+02  1.69E+03
 
 OM36
+       -1.10E+01  3.20E-01 -5.71E+00 -5.44E+00  7.96E+00 -1.66E+00 -4.08E+00 -2.70E-01  2.38E+01  2.25E-01 -1.17E+02 -5.66E+00
         -3.12E+01 -5.03E+01  7.07E+00 -1.97E+01 -4.42E+01 -2.92E+02 -3.27E+01  5.92E+01  1.57E+02 -2.99E+01  1.39E+02 -1.71E+02
        -1.45E+02  5.04E+02  1.48E+03
 
 OM37
+       -1.22E+01 -1.38E+01 -3.68E+00  7.84E+00  2.29E+00 -1.03E+00  3.68E+00  6.39E+00 -1.15E+01 -4.49E+01  1.57E+02  1.63E+01
          6.78E+00 -1.55E+01 -1.08E+02  1.28E+01  2.83E+01  5.54E+02  5.61E+01 -2.31E+01 -9.89E+01  3.78E+00 -1.36E+02 -7.85E+01
         4.48E+02 -2.38E+02 -2.00E+02  1.65E+03
 
 OM38
+        9.00E+00  5.64E+00 -6.17E+00 -1.92E+01  2.40E+00 -1.08E+00 -1.64E-01  2.81E+00  7.83E+01  4.57E+01 -7.06E+02 -2.39E+01
         -3.17E+01  2.61E+01  1.12E+02 -7.66E+01 -1.01E+02 -9.53E+02 -7.18E+01  4.74E+01  1.70E+02 -3.30E+00  3.16E+02 -4.71E+02
        -4.17E+02  2.95E+02  6.10E+02 -6.94E+02  2.44E+03
 
 OM44
+        6.94E+00 -5.84E+00 -3.49E+01 -1.42E+01 -6.21E+00  1.13E+00 -2.26E+00  1.12E+01 -4.00E+00 -2.56E+01 -1.44E+01 -8.04E+01
          1.21E+00  2.78E+01 -4.54E+01  4.36E+01 -2.83E+01 -2.68E-01 -8.01E+01  2.53E+00  2.35E+01 -6.43E+01  6.85E+01  1.88E+01
         2.58E+01  3.21E+01  1.52E+01  8.69E+00 -3.80E+00  4.32E+02
 
 OM45
+       -5.57E+00 -3.89E+00 -4.33E+00 -5.40E+00  1.12E+00 -4.55E+00  3.12E+00  4.58E+00  1.23E+01  1.85E+01 -1.52E+01 -1.62E+02
         -1.25E+02 -1.51E+01 -3.95E+01  2.04E+01  2.55E-01 -4.93E+01 -1.85E+02 -1.38E+02 -5.61E+00 -7.56E+01  4.87E+01 -7.97E+00
         5.24E-01  5.26E+01  4.82E+01 -3.74E+01  5.22E+01  8.79E+01  1.12E+03
 
 OM46
+       -4.80E+00 -1.30E+00  2.32E+01  7.12E+00 -9.96E+00 -2.12E+01 -3.78E-01 -1.27E+01  1.77E+01  2.46E+01 -8.31E+00 -3.60E+01
         -3.20E+01 -6.46E+01 -8.01E+00 -2.20E+01 -2.96E+00 -3.45E+01 -8.62E+01 -4.69E+01 -1.68E+01 -5.12E+01  1.34E+01 -2.32E+01
        -7.88E+01  4.13E+00  5.39E+01 -4.10E+01  6.45E+01 -8.18E+01  2.39E+02  8.39E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -7.84E-01  1.73E+01  4.59E+00 -4.74E+00 -3.48E+00 -9.28E+00  6.27E-01 -2.12E+00 -1.44E+01 -4.54E+01  1.79E+01  4.39E+01
          3.60E+01  2.84E+01 -6.70E+01  3.53E+01 -4.93E+01  4.00E+01  2.94E+02  7.40E+01 -1.39E+01  3.28E+01 -2.53E+01 -6.28E+00
        -8.47E+01 -9.92E+00  1.96E+01  2.98E+01  1.49E+01  2.90E+02 -1.14E+02 -1.52E+02  1.20E+03
 
 OM48
+       -8.90E+00 -2.23E+01  2.72E+01  2.45E+01  1.22E+00 -4.90E+00 -4.74E+00 -3.83E+00  4.10E+01  7.26E+01 -3.38E+01 -4.07E+02
         -7.19E+01 -2.27E+01 -1.06E+02 -2.18E+01  5.30E+01 -5.66E+01 -4.60E+02 -9.56E+01 -8.49E+00 -1.30E+02 -6.99E+00 -5.59E+01
        -2.54E+02 -2.53E+01  3.50E+01 -1.35E+02  2.06E+02 -2.22E+02  1.28E+02  3.06E+02 -5.18E+02  1.46E+03
 
 OM55
+        4.21E+00  5.62E-01  3.66E+00 -2.02E+00  9.88E+00  1.27E+01 -3.25E+00  1.39E+00  2.05E+00  4.77E+01  7.01E+00  3.04E+00
         -1.84E+02 -7.49E+01  4.70E+01 -5.10E+01  1.91E+01 -6.06E+00 -2.89E+01 -2.51E+02 -1.04E+02  3.28E+01 -5.53E+01 -3.07E+01
         3.08E+00 -8.70E+00 -1.26E+01 -3.76E-01  9.14E+00 -3.96E+00  1.10E+02  1.55E+01 -1.85E+01  1.91E+01  6.60E+02
 
 OM56
+        1.15E+01  6.50E+00 -6.08E+00  1.41E+00 -4.09E+00 -4.22E+00  2.49E+00 -2.30E+00  2.80E+00  4.51E+01  3.41E+01  1.07E+01
         -1.17E+02 -2.25E+02  5.15E+01 -1.04E+02  4.80E+01  8.62E+01 -7.11E-01 -2.78E+02 -2.98E+02  5.82E+01 -1.73E+02 -1.68E+01
        -3.14E+01 -1.86E+02 -1.15E+02  3.01E+01 -5.96E+01 -3.34E+00 -6.51E+01  7.21E+01  3.04E+00  3.39E+01  3.64E+02  1.25E+03
 
 OM57
+       -5.55E-01  8.69E+00  2.04E+00 -4.69E+00  6.73E+00  2.93E+00  2.48E+00 -3.25E+00 -2.54E+01 -8.39E+01  6.01E+00 -5.72E+01
          9.56E+01  7.00E+01 -1.97E+02  1.21E+02 -8.44E+01  2.46E+00 -3.14E+01  4.47E+02  1.77E+02 -2.40E+02  1.73E+02  2.02E+01
        -1.15E+01 -7.74E+01 -1.90E+00 -7.65E-01 -2.27E+00  4.66E+01  3.54E+02  8.53E+01  7.60E+01 -3.04E+00 -1.98E+02 -2.67E+02
          1.37E+03
 
 OM58
+        8.42E+00  4.25E+00 -1.07E+01  9.69E+00 -1.21E+01 -3.79E+00  9.09E+00 -4.61E+00  6.66E+01  2.07E+02  4.28E+01  1.09E+01
         -5.69E+02 -2.51E+02  1.61E+02 -3.55E+02  1.87E+02  6.85E+01 -2.96E+01 -7.57E+02 -3.50E+02  2.13E+02 -4.71E+02 -2.68E+01
        -4.96E+01 -3.45E+02 -1.30E+02  7.06E+01 -5.66E+01 -3.90E+01 -2.41E+02 -3.93E+01 -3.39E+01  1.22E+02  2.32E+02  5.22E+02
         -6.09E+02  1.82E+03
 
 OM66
+        4.04E+00  2.41E+00  4.03E+00 -5.59E-01 -8.50E+00 -8.50E+00  2.98E+00 -1.03E+00 -1.13E+01  8.74E+00  6.65E+00  8.31E+00
         -4.35E+01 -7.41E+01  1.77E+01 -2.23E+01  2.43E+01  6.99E+01  1.16E+01 -7.89E+01 -1.48E+02  2.31E+01 -1.01E+02  1.37E+01
         4.31E+00 -7.16E+01 -1.60E+02  2.82E+01 -8.11E+01 -5.20E+00 -1.49E+01 -7.39E+01  1.01E-01 -8.50E+00  7.45E+01  3.09E+02
         -5.06E+01  1.54E+02  4.31E+02
 
 OM67
+        5.02E+00  9.96E+00 -5.36E+00 -2.28E+00  5.16E+00  1.17E+01 -2.46E+00 -7.29E+00 -1.58E+01 -6.51E+01 -4.71E+01 -2.13E+01
          3.01E+01  9.15E+01 -7.45E+01  8.26E+01 -6.99E+01 -6.81E+01 -8.69E+01  1.55E+02  4.14E+02 -1.87E+02  2.59E+02  5.56E+00
        -6.00E+00  2.36E+01 -1.78E+01 -1.35E+02  5.85E+01 -8.20E+00  1.28E+02  3.25E+02 -1.22E+02  1.34E+02 -8.46E+01 -2.44E+02
          3.90E+02 -2.32E+02 -1.38E+02  1.15E+03
 
 OM68
+        9.08E+00  1.39E+01 -1.12E+01  3.14E+00 -1.62E+01 -1.52E+01  1.04E+01 -6.25E+00  1.87E+01  1.41E+02  1.21E+02  5.01E+01
         -1.81E+02 -5.03E+02  1.24E+02 -2.78E+02  1.88E+02  2.19E+02  6.54E+01 -3.28E+02 -7.07E+02  2.18E+02 -5.91E+02  3.37E+01
         6.22E+01 -1.78E+02 -4.08E+02  1.38E+02 -3.67E+02 -1.02E+01 -8.60E+01 -2.74E+02  4.09E+01 -1.90E+02  1.26E+02  3.67E+02
         -2.01E+02  6.21E+02  3.51E+02 -5.50E+02  1.67E+03
 
 OM77
+       -1.05E+01 -1.79E+01  2.89E+00 -3.07E+00  1.44E+00 -2.62E+00 -2.86E+00  1.30E+01 -4.95E+00  3.17E+01  7.61E+00  3.29E+01
         -8.56E+00 -1.46E+01  6.97E+01 -3.27E+01  5.58E+01 -4.21E+01  1.30E+02 -4.04E+01 -5.78E+01  3.68E+02 -1.83E+02 -1.77E+01
        -3.30E+01  1.68E+01  2.76E+01 -8.91E+01  6.05E+01  2.49E+01 -5.67E+01 -4.51E+01  3.69E+02 -1.46E+02  2.46E+00  1.95E+01
         -1.67E+02  5.50E+01 -6.98E+00 -1.64E+02  6.37E+01  6.64E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.99E+01  4.90E+01  1.16E+01  3.63E+00 -1.18E+00  4.32E+00  4.45E+00 -3.83E+01 -2.91E+01 -2.27E+02  8.07E+00 -1.66E+02
          9.34E+01  1.28E+02 -5.45E+02  3.56E+02 -2.18E+02 -3.88E+01 -2.74E+02  1.54E+02  2.69E+02 -7.38E+02  8.52E+02  4.15E+01
        -4.69E+01  3.81E+01  1.21E+01 -2.65E+02 -1.44E+01 -4.09E+01  8.97E+01  1.53E+02 -4.30E+02  5.79E+02 -3.53E+01 -7.94E+01
          2.42E+02 -3.22E+02 -4.25E+01  4.57E+02 -4.40E+02 -5.35E+02  1.91E+03
 
 OM88
+        3.84E+00 -7.49E+00 -1.17E+01 -4.15E+00 -1.64E+01 -1.00E+01  4.31E+00 -3.60E+00  9.27E+01  2.78E+02  1.37E+02  1.07E+02
         -7.22E+01 -1.78E+02  2.33E+02 -6.97E+02  2.23E+02  1.89E+02  1.23E+02 -1.40E+02 -2.84E+02  3.13E+02 -9.31E+02  3.72E+01
         7.75E+01 -6.42E+01 -1.23E+02  1.51E+02 -4.56E+02 -8.63E-01 -4.66E+01 -7.77E+01  9.38E+01 -3.63E+02  2.03E+01  8.40E+01
         -8.73E+01  2.65E+02  6.71E+01 -1.76E+02  5.66E+02  9.99E+01 -7.33E+02  1.15E+03
 
 SG11
+        3.94E+02  2.47E+02 -9.99E+01 -1.17E+02 -4.37E+02 -1.21E+02  2.05E+02 -4.14E+02 -9.61E+02 -1.05E+03  6.95E+02 -1.80E+02
          9.49E+02  1.57E+02 -1.86E+02  1.20E+03  1.38E+03  5.96E+02 -1.45E+03 -7.11E+01 -3.12E+02 -9.29E+02 -3.01E+02  1.13E+03
         3.84E+02 -1.13E+03 -1.60E+02  5.24E+02  3.68E+02  2.75E+02  8.03E+02  1.24E+03 -6.42E+02  1.31E+03 -5.34E+02 -9.08E+02
         -7.12E+02 -1.36E+02 -1.04E+03  2.32E+02 -4.44E+02 -3.18E+02  3.02E+02  3.25E+02  2.55E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -1.92E+02  6.15E+01 -1.04E+01  7.33E+00  1.80E+02  7.02E+01 -1.66E+02  1.25E+02  2.12E+02  5.00E+00 -6.81E+02  1.11E+02
         -1.33E+02  5.02E+02  6.17E+01  1.20E+02 -3.75E+02 -4.89E+02  1.78E+02  5.45E+02  4.53E+02 -4.71E+02  6.01E+02  7.60E+02
         7.31E+02 -1.75E+01 -6.98E+02  3.21E+02 -1.07E+02  5.35E+01 -6.42E+01 -4.60E+02  5.00E+01 -8.18E+02 -1.37E+02  2.87E+02
          3.89E+02  7.17E+00  7.27E+02  3.85E+02  4.34E+01 -3.34E+02  1.76E+02  4.37E+01  5.95E+04  0.00E+00  7.11E+05
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     2909.840
Stop Time: 
Wed 06/17/2015 
05:24 PM

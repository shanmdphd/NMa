Thu 11/03/2016 
12:41 AM
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
(0.3);[p]

$EST METHOD=NUTS NBURN=1000 NITER=2000 PRINT=5 OLKJDF=8.0 NUTS_REG=1.0
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  121) INTERACTION IS IMPLIED WITH EM/BAYES ESTIMATION METHODS
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        3 NOV 2016
Days until program expires :4959
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha12 (nm74a12)
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
 0.0000E+00   0.3000E+00
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha12 (nm74a12)

 GENERAL NONLINEAR KINETICS MODEL WITH STIFF/NONSTIFF EQUATIONS (ADVAN13)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   7
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         COMP 1       ON         YES        YES        YES        YES
    2         COMP 2       ON         YES        YES        NO         NO
    3         COMP 3       ON         YES        YES        NO         NO
    4         OUTPUT       OFF        YES        NO         NO         NO
 INITIAL (BASE) TOLERANCE SETTINGS:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   4
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
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
0DES SUBROUTINE USES COMPACT STORAGE MODE.
1
 
 
 #TBLN:      1
 #METH: NUTS Bayesian Analysis
 
 ESTIMATION STEP OMITTED:                 NO
 ANALYSIS TYPE:                           POPULATION
 NUMBER OF SADDLE POINT RESET ITERATIONS:      0
 GRADIENT METHOD USED:               NOSLOW
 CONDITIONAL ESTIMATES USED:              YES
 CENTERED ETA:                            NO
 EPS-ETA INTERACTION:                     YES
 LAPLACIAN OBJ. FUNC.:                    NO
 NO. OF FUNCT. EVALS. ALLOWED:            3480
 NO. OF SIG. FIGURES REQUIRED:            3
 INTERMEDIATE PRINTOUT:                   YES
 ESTIMATE OUTPUT TO MSF:                  NO
 IND. OBJ. FUNC. VALUES SORTED:           NO
 NUMERICAL DERIVATIVE
       FILE REQUEST (NUMDER):               NONE
 MAP (ETAHAT) ESTIMATION METHOD (OPTMAP):   0
 ETA HESSIAN EVALUATION METHOD (ETADER):    0
 INITIAL ETA FOR MAP ESTIMATION (MCETA):    0
 SIGDIGITS FOR MAP ESTIMATION (SIGLO):      100
 GRADIENT SIGDIGITS OF
       FIXED EFFECTS PARAMETERS (SIGL):     100
 NOPRIOR SETTING (NOPRIOR):                 OFF
 NOCOV SETTING (NOCOV):                     OFF
 DERCONT SETTING (DERCONT):                 OFF
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmto26.ext
 EXCLUDE TITLE (NOTITLE):                   NO
 EXCLUDE COLUMN LABELS (NOLABEL):           NO
 FORMAT FOR ADDITIONAL FILES (FORMAT):      S1PE12.5
 PARAMETER ORDER FOR OUTPUTS (ORDER):       TSOL
 WISHART PRIOR DF INTERPRETATION (WISHTYPE):0
 KNUTHSUMOFF:                               0
 INCLUDE LNTWOPI:                           NO
 INCLUDE CONSTANT TERM TO PRIOR (PRIORC):   NO
 INCLUDE CONSTANT TERM TO OMEGA (ETA) (OLNTWOPI):NO
 EM OR BAYESIAN METHOD USED:                MCMC BAYESIAN (BAYES)
 MU MODELING PATTERN (MUM):
 GRADIENT/GIBBS PATTERN (GRD):
 AUTOMATIC SETTING FEATURE (AUTO):          OFF
 CONVERGENCE TYPE (CTYPE):                  0
 KEEP ITERATIONS (THIN):            1
 BURN-IN ITERATIONS (NBURN):                1000
 ITERATIONS (NITER):                        2000
 ANEAL SETTING (CONSTRAIN):                 1
 STARTING SEED FOR MC METHODS (SEED):       11456
 MC SAMPLES PER SUBJECT (ISAMPLE):          1
 RANDOM SAMPLING METHOD (RANMETHOD):        3U
 PROPOSAL DENSITY SCALING RANGE
              (ISCALE_MIN, ISCALE_MAX):     1.000000000000000E-06   ,1000000.00000000
 SAMPLE ACCEPTANCE RATE (IACCEPT):          0.400000000000000
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
 SAMPLES FOR LOCAL UNIVARIATE SEARCH KERNEL (OSAMPLE_M3):0
 MASS/IMP./POST. MATRIX REFRESH SETTING (MASSREST):      -1
 MASS MATRIX ACCUMULATION ITERATIONS (MADAPT):          -1
 MASS MATRIX BLOCKING TYPE (NUTS_MASS):                 B
 MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (NUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (KAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (NUTS_DELTA):                   0.800000000000000
 NUTS GAMMA SETTING (NUTS_GAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 8.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000
 NUTS WARMUP METHOD (NUTS_TEST):       0
 NUTS MAXIMAL DEPTH SEARCH (NUTS_MAXDEPTH):       10
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       7.500000000000000E-02
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): 2.500000000000000E-02
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 5.000000000000000E-02
 INITIAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPITER): 1
 INTERVAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPINTER):0
 ETA PARAMETERIZATION (NUTS_EPARAM):0
 OMEGA PARAMETERIZATION (NUTS_OPARAM):1
 SIGMA PARAMETERIZATION (NUTS_SPARAM):1
 NUTS REGULARIZING METHOD (NUTS_REG): 1.00000000000000

 TOLERANCES FOR ESTIMATION/EVALUATION STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   4
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 TOLERANCES FOR COVARIANCE STEP:
 NRD (RELATIVE) VALUE(S) OF TOLERANCE:   4
 ANRD (ABSOLUTE) VALUE(S) OF TOLERANCE:  12
 
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
 iteration        -1000 MCMCOBJ=    181560.900376746     
 iteration         -995 MCMCOBJ=    9599.10315913452     
 iteration         -990 MCMCOBJ=   -686.558013363094     
 iteration         -985 MCMCOBJ=   -5763.94187699271     
 iteration         -980 MCMCOBJ=   -6595.51500108330     
 iteration         -975 MCMCOBJ=   -6582.51895621072     
 iteration         -970 MCMCOBJ=   -6547.67496716881     
 iteration         -965 MCMCOBJ=   -6589.32034415059     
 iteration         -960 MCMCOBJ=   -6553.96716498778     
 iteration         -955 MCMCOBJ=   -6626.84591280598     
 iteration         -950 MCMCOBJ=   -6619.53125218791     
 iteration         -945 MCMCOBJ=   -6625.24869530610     
 iteration         -940 MCMCOBJ=   -6592.13987288588     
 iteration         -935 MCMCOBJ=   -6584.80651110230     
 iteration         -930 MCMCOBJ=   -6584.56047071240     
 iteration         -925 MCMCOBJ=   -6541.45203601778     
 iteration         -920 MCMCOBJ=   -6607.77928263631     
 iteration         -915 MCMCOBJ=   -6629.67164462530     
 iteration         -910 MCMCOBJ=   -6623.77653500615     
 iteration         -905 MCMCOBJ=   -6570.34130599157     
 iteration         -900 MCMCOBJ=   -6603.41652640356     
 iteration         -895 MCMCOBJ=   -6564.81411877531     
 iteration         -890 MCMCOBJ=   -6593.52763754797     
 iteration         -885 MCMCOBJ=   -6624.34845892261     
 iteration         -880 MCMCOBJ=   -6590.43419245254     
 iteration         -875 MCMCOBJ=   -6596.00432575972     
 iteration         -870 MCMCOBJ=   -6578.53331441338     
 iteration         -865 MCMCOBJ=   -6570.59106823439     
 iteration         -860 MCMCOBJ=   -6595.31300455721     
 iteration         -855 MCMCOBJ=   -6580.46229975662     
 iteration         -850 MCMCOBJ=   -6624.03045348370     
 iteration         -845 MCMCOBJ=   -6528.81595024451     
 iteration         -840 MCMCOBJ=   -6586.85715114059     
 iteration         -835 MCMCOBJ=   -6658.39234323775     
 iteration         -830 MCMCOBJ=   -6576.53717375818     
 iteration         -825 MCMCOBJ=   -6639.55094254172     
 iteration         -820 MCMCOBJ=   -6619.67222661265     
 iteration         -815 MCMCOBJ=   -6653.06197451860     
 iteration         -810 MCMCOBJ=   -6579.62053196806     
 iteration         -805 MCMCOBJ=   -6610.67554027699     
 iteration         -800 MCMCOBJ=   -6609.34262181800     
 iteration         -795 MCMCOBJ=   -6603.65592118757     
 iteration         -790 MCMCOBJ=   -6669.60904344448     
 iteration         -785 MCMCOBJ=   -6689.40850503763     
 iteration         -780 MCMCOBJ=   -6594.83669953293     
 iteration         -775 MCMCOBJ=   -6562.28170961016     
 iteration         -770 MCMCOBJ=   -6656.40025581245     
 iteration         -765 MCMCOBJ=   -6601.85564848789     
 iteration         -760 MCMCOBJ=   -6586.95275973173     
 iteration         -755 MCMCOBJ=   -6611.61487242668     
 iteration         -750 MCMCOBJ=   -6536.61834022761     
 iteration         -745 MCMCOBJ=   -6610.72286201389     
 iteration         -740 MCMCOBJ=   -6669.67406302808     
 iteration         -735 MCMCOBJ=   -6567.86373220930     
 iteration         -730 MCMCOBJ=   -6597.63998369539     
 iteration         -725 MCMCOBJ=   -6580.59293089980     
 iteration         -720 MCMCOBJ=   -6596.23371005652     
 iteration         -715 MCMCOBJ=   -6635.52061024509     
 iteration         -710 MCMCOBJ=   -6566.91536178188     
 iteration         -705 MCMCOBJ=   -6556.01828695221     
 iteration         -700 MCMCOBJ=   -6593.74335244815     
 iteration         -695 MCMCOBJ=   -6571.62837608684     
 iteration         -690 MCMCOBJ=   -6594.65628138866     
 iteration         -685 MCMCOBJ=   -6560.22712821009     
 iteration         -680 MCMCOBJ=   -6579.87356334231     
 iteration         -675 MCMCOBJ=   -6627.83736793723     
 iteration         -670 MCMCOBJ=   -6579.87976989399     
 iteration         -665 MCMCOBJ=   -6613.81165625039     
 iteration         -660 MCMCOBJ=   -6650.25401815967     
 iteration         -655 MCMCOBJ=   -6513.65405990661     
 iteration         -650 MCMCOBJ=   -6601.27507350644     
 iteration         -645 MCMCOBJ=   -6571.50955624057     
 iteration         -640 MCMCOBJ=   -6566.79158618132     
 iteration         -635 MCMCOBJ=   -6606.84228070894     
 iteration         -630 MCMCOBJ=   -6549.45384952154     
 iteration         -625 MCMCOBJ=   -6602.06644255567     
 iteration         -620 MCMCOBJ=   -6593.93802719174     
 iteration         -615 MCMCOBJ=   -6632.10972734428     
 iteration         -610 MCMCOBJ=   -6600.47020794988     
 iteration         -605 MCMCOBJ=   -6585.91938901452     
 iteration         -600 MCMCOBJ=   -6661.35054844605     
 iteration         -595 MCMCOBJ=   -6529.97830158194     
 iteration         -590 MCMCOBJ=   -6560.60635963690     
 iteration         -585 MCMCOBJ=   -6578.11827822852     
 iteration         -580 MCMCOBJ=   -6565.59656444532     
 iteration         -575 MCMCOBJ=   -6590.12524940216     
 iteration         -570 MCMCOBJ=   -6646.22857935881     
 iteration         -565 MCMCOBJ=   -6597.19644706890     
 iteration         -560 MCMCOBJ=   -6637.76604422753     
 iteration         -555 MCMCOBJ=   -6593.33675473009     
 iteration         -550 MCMCOBJ=   -6577.02426311876     
 iteration         -545 MCMCOBJ=   -6507.74023400341     
 iteration         -540 MCMCOBJ=   -6574.40738240345     
 iteration         -535 MCMCOBJ=   -6562.75022715975     
 iteration         -530 MCMCOBJ=   -6593.44868620965     
 iteration         -525 MCMCOBJ=   -6634.98476649646     
 iteration         -520 MCMCOBJ=   -6626.71484539478     
 iteration         -515 MCMCOBJ=   -6616.93521524623     
 iteration         -510 MCMCOBJ=   -6598.86691238445     
 iteration         -505 MCMCOBJ=   -6631.71391979079     
 iteration         -500 MCMCOBJ=   -6578.61623064029     
 iteration         -495 MCMCOBJ=   -6599.14286868771     
 iteration         -490 MCMCOBJ=   -6642.73441290658     
 iteration         -485 MCMCOBJ=   -6619.80867201789     
 iteration         -480 MCMCOBJ=   -6588.99636786499     
 iteration         -475 MCMCOBJ=   -6564.13115877800     
 iteration         -470 MCMCOBJ=   -6628.50800139083     
 iteration         -465 MCMCOBJ=   -6527.80744774223     
 iteration         -460 MCMCOBJ=   -6557.57740392947     
 iteration         -455 MCMCOBJ=   -6589.41116407505     
 iteration         -450 MCMCOBJ=   -6619.24133298313     
 iteration         -445 MCMCOBJ=   -6611.79500399134     
 iteration         -440 MCMCOBJ=   -6605.49394759981     
 iteration         -435 MCMCOBJ=   -6609.59633552994     
 iteration         -430 MCMCOBJ=   -6611.05624330217     
 iteration         -425 MCMCOBJ=   -6613.55110784973     
 iteration         -420 MCMCOBJ=   -6681.73691635703     
 iteration         -415 MCMCOBJ=   -6650.15685351006     
 iteration         -410 MCMCOBJ=   -6605.52363797466     
 iteration         -405 MCMCOBJ=   -6631.60167635883     
 iteration         -400 MCMCOBJ=   -6592.44850736252     
 iteration         -395 MCMCOBJ=   -6616.22222539529     
 iteration         -390 MCMCOBJ=   -6598.82851740300     
 iteration         -385 MCMCOBJ=   -6608.32168084328     
 iteration         -380 MCMCOBJ=   -6596.20271889537     
 iteration         -375 MCMCOBJ=   -6597.71602661414     
 iteration         -370 MCMCOBJ=   -6582.16321227765     
 iteration         -365 MCMCOBJ=   -6571.02529263841     
 iteration         -360 MCMCOBJ=   -6573.53781095929     
 iteration         -355 MCMCOBJ=   -6579.61595748482     
 iteration         -350 MCMCOBJ=   -6488.62405497189     
 iteration         -345 MCMCOBJ=   -6578.05316816069     
 iteration         -340 MCMCOBJ=   -6618.08887583384     
 iteration         -335 MCMCOBJ=   -6609.97356460747     
 iteration         -330 MCMCOBJ=   -6618.69744036974     
 iteration         -325 MCMCOBJ=   -6589.40036349539     
 iteration         -320 MCMCOBJ=   -6536.95279410843     
 iteration         -315 MCMCOBJ=   -6599.50845810702     
 iteration         -310 MCMCOBJ=   -6612.09963096162     
 iteration         -305 MCMCOBJ=   -6682.75375906172     
 iteration         -300 MCMCOBJ=   -6638.18643866567     
 iteration         -295 MCMCOBJ=   -6640.01488859848     
 iteration         -290 MCMCOBJ=   -6663.25014705955     
 iteration         -285 MCMCOBJ=   -6603.53526191192     
 iteration         -280 MCMCOBJ=   -6625.49520793929     
 iteration         -275 MCMCOBJ=   -6582.50401739907     
 iteration         -270 MCMCOBJ=   -6575.09166813709     
 iteration         -265 MCMCOBJ=   -6594.08900399504     
 iteration         -260 MCMCOBJ=   -6549.12611716488     
 iteration         -255 MCMCOBJ=   -6615.58759316579     
 iteration         -250 MCMCOBJ=   -6604.26484745664     
 iteration         -245 MCMCOBJ=   -6657.74103523958     
 iteration         -240 MCMCOBJ=   -6587.10125032770     
 iteration         -235 MCMCOBJ=   -6607.16082389022     
 iteration         -230 MCMCOBJ=   -6585.22206338487     
 iteration         -225 MCMCOBJ=   -6523.69812853399     
 iteration         -220 MCMCOBJ=   -6629.54160236235     
 iteration         -215 MCMCOBJ=   -6650.74945700997     
 iteration         -210 MCMCOBJ=   -6567.13286479832     
 iteration         -205 MCMCOBJ=   -6523.41211561029     
 iteration         -200 MCMCOBJ=   -6596.34129490296     
 iteration         -195 MCMCOBJ=   -6589.94609258953     
 iteration         -190 MCMCOBJ=   -6559.84087302551     
 iteration         -185 MCMCOBJ=   -6653.12635744619     
 iteration         -180 MCMCOBJ=   -6650.47089535049     
 iteration         -175 MCMCOBJ=   -6611.12068926390     
 iteration         -170 MCMCOBJ=   -6612.66776054193     
 iteration         -165 MCMCOBJ=   -6518.74307675933     
 iteration         -160 MCMCOBJ=   -6626.89848566389     
 iteration         -155 MCMCOBJ=   -6587.70544415319     
 iteration         -150 MCMCOBJ=   -6637.87055689361     
 iteration         -145 MCMCOBJ=   -6572.13738746688     
 iteration         -140 MCMCOBJ=   -6634.24737030940     
 iteration         -135 MCMCOBJ=   -6585.11105627318     
 iteration         -130 MCMCOBJ=   -6629.64800502681     
 iteration         -125 MCMCOBJ=   -6581.50113388186     
 iteration         -120 MCMCOBJ=   -6593.71677662510     
 iteration         -115 MCMCOBJ=   -6613.00474774094     
 iteration         -110 MCMCOBJ=   -6558.93341831173     
 iteration         -105 MCMCOBJ=   -6579.95611026253     
 iteration         -100 MCMCOBJ=   -6603.99902450165     
 iteration          -95 MCMCOBJ=   -6616.46787402186     
 iteration          -90 MCMCOBJ=   -6605.96066196404     
 iteration          -85 MCMCOBJ=   -6638.97881316825     
 iteration          -80 MCMCOBJ=   -6623.58573648715     
 iteration          -75 MCMCOBJ=   -6591.10483585302     
 iteration          -70 MCMCOBJ=   -6630.43082576172     
 iteration          -65 MCMCOBJ=   -6579.38199420611     
 iteration          -60 MCMCOBJ=   -6645.37098616565     
 iteration          -55 MCMCOBJ=   -6507.11978599850     
 iteration          -50 MCMCOBJ=   -6599.11108772303     
 iteration          -45 MCMCOBJ=   -6623.76480270446     
 iteration          -40 MCMCOBJ=   -6591.48344261073     
 iteration          -35 MCMCOBJ=   -6609.31370824444     
 iteration          -30 MCMCOBJ=   -6567.69551540887     
 iteration          -25 MCMCOBJ=   -6649.23351334152     
 iteration          -20 MCMCOBJ=   -6638.09678493471     
 iteration          -15 MCMCOBJ=   -6627.64327073585     
 iteration          -10 MCMCOBJ=   -6629.81687337285     
 iteration           -5 MCMCOBJ=   -6635.37710647554     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6575.56897979036     
 iteration            5 MCMCOBJ=   -6573.58494625474     
 iteration           10 MCMCOBJ=   -6625.42037368381     
 iteration           15 MCMCOBJ=   -6609.76820880818     
 iteration           20 MCMCOBJ=   -6585.68030326543     
 iteration           25 MCMCOBJ=   -6577.93618568119     
 iteration           30 MCMCOBJ=   -6618.65192764825     
 iteration           35 MCMCOBJ=   -6587.59083241886     
 iteration           40 MCMCOBJ=   -6513.43231698204     
 iteration           45 MCMCOBJ=   -6579.11684774846     
 iteration           50 MCMCOBJ=   -6608.75732467728     
 iteration           55 MCMCOBJ=   -6605.34194163727     
 iteration           60 MCMCOBJ=   -6617.71858295681     
 iteration           65 MCMCOBJ=   -6581.07504394214     
 iteration           70 MCMCOBJ=   -6611.83855118004     
 iteration           75 MCMCOBJ=   -6570.17921258686     
 iteration           80 MCMCOBJ=   -6554.37668709776     
 iteration           85 MCMCOBJ=   -6690.64028435427     
 iteration           90 MCMCOBJ=   -6602.65037439934     
 iteration           95 MCMCOBJ=   -6575.65130040043     
 iteration          100 MCMCOBJ=   -6588.47559125942     
 iteration          105 MCMCOBJ=   -6580.02820740311     
 iteration          110 MCMCOBJ=   -6571.26164800328     
 iteration          115 MCMCOBJ=   -6570.17459751088     
 iteration          120 MCMCOBJ=   -6504.20397717126     
 iteration          125 MCMCOBJ=   -6578.64834152854     
 iteration          130 MCMCOBJ=   -6628.36755422855     
 iteration          135 MCMCOBJ=   -6582.40148164292     
 iteration          140 MCMCOBJ=   -6645.99855066449     
 iteration          145 MCMCOBJ=   -6619.06845739416     
 iteration          150 MCMCOBJ=   -6563.99033366908     
 iteration          155 MCMCOBJ=   -6524.92966568592     
 iteration          160 MCMCOBJ=   -6595.24933400706     
 iteration          165 MCMCOBJ=   -6618.81316932797     
 iteration          170 MCMCOBJ=   -6561.55510101021     
 iteration          175 MCMCOBJ=   -6587.84693725673     
 iteration          180 MCMCOBJ=   -6577.55459872721     
 iteration          185 MCMCOBJ=   -6629.03397802026     
 iteration          190 MCMCOBJ=   -6608.81629510803     
 iteration          195 MCMCOBJ=   -6637.42663334759     
 iteration          200 MCMCOBJ=   -6574.10593653740     
 iteration          205 MCMCOBJ=   -6566.62967217150     
 iteration          210 MCMCOBJ=   -6565.70757643515     
 iteration          215 MCMCOBJ=   -6552.94194548427     
 iteration          220 MCMCOBJ=   -6602.14434383602     
 iteration          225 MCMCOBJ=   -6628.89420870745     
 iteration          230 MCMCOBJ=   -6585.56695737637     
 iteration          235 MCMCOBJ=   -6543.98830503738     
 iteration          240 MCMCOBJ=   -6596.93759257002     
 iteration          245 MCMCOBJ=   -6519.87777854142     
 iteration          250 MCMCOBJ=   -6621.96309347339     
 iteration          255 MCMCOBJ=   -6571.13257388225     
 iteration          260 MCMCOBJ=   -6560.14330636184     
 iteration          265 MCMCOBJ=   -6658.34354639155     
 iteration          270 MCMCOBJ=   -6599.86425868925     
 iteration          275 MCMCOBJ=   -6629.31674107249     
 iteration          280 MCMCOBJ=   -6512.34391657068     
 iteration          285 MCMCOBJ=   -6566.52303610402     
 iteration          290 MCMCOBJ=   -6657.60102901046     
 iteration          295 MCMCOBJ=   -6621.02053288018     
 iteration          300 MCMCOBJ=   -6648.56380997178     
 iteration          305 MCMCOBJ=   -6588.89304768454     
 iteration          310 MCMCOBJ=   -6615.85280126281     
 iteration          315 MCMCOBJ=   -6554.59182489115     
 iteration          320 MCMCOBJ=   -6594.84648484139     
 iteration          325 MCMCOBJ=   -6565.37975578017     
 iteration          330 MCMCOBJ=   -6598.94041415064     
 iteration          335 MCMCOBJ=   -6638.84775157469     
 iteration          340 MCMCOBJ=   -6582.94838116856     
 iteration          345 MCMCOBJ=   -6511.20957689723     
 iteration          350 MCMCOBJ=   -6568.80299309329     
 iteration          355 MCMCOBJ=   -6545.11268198001     
 iteration          360 MCMCOBJ=   -6559.22658865283     
 iteration          365 MCMCOBJ=   -6596.60161540588     
 iteration          370 MCMCOBJ=   -6616.89606871420     
 iteration          375 MCMCOBJ=   -6639.98950728868     
 iteration          380 MCMCOBJ=   -6575.99470752626     
 iteration          385 MCMCOBJ=   -6619.99566042515     
 iteration          390 MCMCOBJ=   -6574.49423667987     
 iteration          395 MCMCOBJ=   -6582.22719526410     
 iteration          400 MCMCOBJ=   -6615.60227981090     
 iteration          405 MCMCOBJ=   -6655.57772496199     
 iteration          410 MCMCOBJ=   -6628.45154607389     
 iteration          415 MCMCOBJ=   -6590.76073967388     
 iteration          420 MCMCOBJ=   -6638.68545086344     
 iteration          425 MCMCOBJ=   -6615.01138706558     
 iteration          430 MCMCOBJ=   -6612.46548185868     
 iteration          435 MCMCOBJ=   -6618.62822363896     
 iteration          440 MCMCOBJ=   -6592.68427007245     
 iteration          445 MCMCOBJ=   -6630.22951686444     
 iteration          450 MCMCOBJ=   -6622.60820925977     
 iteration          455 MCMCOBJ=   -6623.67377926598     
 iteration          460 MCMCOBJ=   -6579.31894797236     
 iteration          465 MCMCOBJ=   -6631.55960551794     
 iteration          470 MCMCOBJ=   -6628.92992648660     
 iteration          475 MCMCOBJ=   -6578.64208982713     
 iteration          480 MCMCOBJ=   -6611.18942126954     
 iteration          485 MCMCOBJ=   -6602.31975361984     
 iteration          490 MCMCOBJ=   -6545.95266601251     
 iteration          495 MCMCOBJ=   -6626.02821523586     
 iteration          500 MCMCOBJ=   -6626.54072842818     
 iteration          505 MCMCOBJ=   -6543.82284130413     
 iteration          510 MCMCOBJ=   -6569.92242576418     
 iteration          515 MCMCOBJ=   -6501.25237532999     
 iteration          520 MCMCOBJ=   -6571.45332358555     
 iteration          525 MCMCOBJ=   -6522.80008326522     
 iteration          530 MCMCOBJ=   -6569.45151864691     
 iteration          535 MCMCOBJ=   -6580.57852549896     
 iteration          540 MCMCOBJ=   -6545.38831106056     
 iteration          545 MCMCOBJ=   -6573.35543014145     
 iteration          550 MCMCOBJ=   -6580.52674561281     
 iteration          555 MCMCOBJ=   -6591.16165603271     
 iteration          560 MCMCOBJ=   -6539.44367689762     
 iteration          565 MCMCOBJ=   -6614.86948368087     
 iteration          570 MCMCOBJ=   -6591.29763781454     
 iteration          575 MCMCOBJ=   -6613.70738692032     
 iteration          580 MCMCOBJ=   -6585.50743920024     
 iteration          585 MCMCOBJ=   -6529.80832893699     
 iteration          590 MCMCOBJ=   -6631.05767179645     
 iteration          595 MCMCOBJ=   -6659.99239114372     
 iteration          600 MCMCOBJ=   -6593.61961775912     
 iteration          605 MCMCOBJ=   -6615.69972156809     
 iteration          610 MCMCOBJ=   -6613.79045135110     
 iteration          615 MCMCOBJ=   -6599.77715441332     
 iteration          620 MCMCOBJ=   -6598.07942800386     
 iteration          625 MCMCOBJ=   -6614.04506443788     
 iteration          630 MCMCOBJ=   -6654.57952121778     
 iteration          635 MCMCOBJ=   -6630.63876368262     
 iteration          640 MCMCOBJ=   -6559.38844999643     
 iteration          645 MCMCOBJ=   -6615.71566815242     
 iteration          650 MCMCOBJ=   -6594.21109267977     
 iteration          655 MCMCOBJ=   -6630.82530008792     
 iteration          660 MCMCOBJ=   -6615.17809180234     
 iteration          665 MCMCOBJ=   -6591.00435850580     
 iteration          670 MCMCOBJ=   -6572.88373513414     
 iteration          675 MCMCOBJ=   -6557.16383299617     
 iteration          680 MCMCOBJ=   -6587.89628931262     
 iteration          685 MCMCOBJ=   -6632.31382125050     
 iteration          690 MCMCOBJ=   -6570.23652065900     
 iteration          695 MCMCOBJ=   -6561.90743051888     
 iteration          700 MCMCOBJ=   -6620.17771011783     
 iteration          705 MCMCOBJ=   -6608.98750280369     
 iteration          710 MCMCOBJ=   -6635.74035756844     
 iteration          715 MCMCOBJ=   -6599.55478038791     
 iteration          720 MCMCOBJ=   -6643.27783724017     
 iteration          725 MCMCOBJ=   -6563.39496420976     
 iteration          730 MCMCOBJ=   -6661.38441576535     
 iteration          735 MCMCOBJ=   -6614.70685562842     
 iteration          740 MCMCOBJ=   -6521.43232503650     
 iteration          745 MCMCOBJ=   -6626.35942001139     
 iteration          750 MCMCOBJ=   -6598.44452245921     
 iteration          755 MCMCOBJ=   -6589.21734348041     
 iteration          760 MCMCOBJ=   -6603.02310711748     
 iteration          765 MCMCOBJ=   -6544.87628961557     
 iteration          770 MCMCOBJ=   -6612.20009758511     
 iteration          775 MCMCOBJ=   -6627.83562648705     
 iteration          780 MCMCOBJ=   -6594.89493903005     
 iteration          785 MCMCOBJ=   -6642.05999026127     
 iteration          790 MCMCOBJ=   -6626.54706464152     
 iteration          795 MCMCOBJ=   -6526.36517326708     
 iteration          800 MCMCOBJ=   -6580.90749782434     
 iteration          805 MCMCOBJ=   -6585.58075031975     
 iteration          810 MCMCOBJ=   -6504.23625376632     
 iteration          815 MCMCOBJ=   -6618.63294042748     
 iteration          820 MCMCOBJ=   -6650.63394461462     
 iteration          825 MCMCOBJ=   -6675.67601684822     
 iteration          830 MCMCOBJ=   -6625.57860215776     
 iteration          835 MCMCOBJ=   -6608.83843950334     
 iteration          840 MCMCOBJ=   -6559.90507596878     
 iteration          845 MCMCOBJ=   -6613.74683256743     
 iteration          850 MCMCOBJ=   -6606.56936703368     
 iteration          855 MCMCOBJ=   -6632.77428919640     
 iteration          860 MCMCOBJ=   -6595.20763857091     
 iteration          865 MCMCOBJ=   -6634.36643463195     
 iteration          870 MCMCOBJ=   -6605.33494458492     
 iteration          875 MCMCOBJ=   -6524.43438677961     
 iteration          880 MCMCOBJ=   -6573.71207549940     
 iteration          885 MCMCOBJ=   -6530.31081764162     
 iteration          890 MCMCOBJ=   -6687.67244811224     
 iteration          895 MCMCOBJ=   -6594.77942416704     
 iteration          900 MCMCOBJ=   -6541.22565490549     
 iteration          905 MCMCOBJ=   -6558.49082197794     
 iteration          910 MCMCOBJ=   -6592.14516527740     
 iteration          915 MCMCOBJ=   -6599.58567859709     
 iteration          920 MCMCOBJ=   -6521.27375137571     
 iteration          925 MCMCOBJ=   -6588.67829217046     
 iteration          930 MCMCOBJ=   -6602.88158283131     
 iteration          935 MCMCOBJ=   -6650.71482855223     
 iteration          940 MCMCOBJ=   -6607.51748634561     
 iteration          945 MCMCOBJ=   -6544.33732070530     
 iteration          950 MCMCOBJ=   -6578.36814369457     
 iteration          955 MCMCOBJ=   -6627.65569104550     
 iteration          960 MCMCOBJ=   -6605.01279662347     
 iteration          965 MCMCOBJ=   -6608.69163240282     
 iteration          970 MCMCOBJ=   -6627.90929770751     
 iteration          975 MCMCOBJ=   -6568.56196765460     
 iteration          980 MCMCOBJ=   -6599.04375812263     
 iteration          985 MCMCOBJ=   -6643.83262477482     
 iteration          990 MCMCOBJ=   -6601.24529858912     
 iteration          995 MCMCOBJ=   -6574.44466601909     
 iteration         1000 MCMCOBJ=   -6560.28530262009     
 iteration         1005 MCMCOBJ=   -6599.45676502902     
 iteration         1010 MCMCOBJ=   -6575.56205341335     
 iteration         1015 MCMCOBJ=   -6644.70627417473     
 iteration         1020 MCMCOBJ=   -6649.75246768919     
 iteration         1025 MCMCOBJ=   -6657.43129789402     
 iteration         1030 MCMCOBJ=   -6619.88670886068     
 iteration         1035 MCMCOBJ=   -6523.36305661095     
 iteration         1040 MCMCOBJ=   -6576.36227309707     
 iteration         1045 MCMCOBJ=   -6590.40765711408     
 iteration         1050 MCMCOBJ=   -6583.08425942306     
 iteration         1055 MCMCOBJ=   -6670.61222164364     
 iteration         1060 MCMCOBJ=   -6552.86201354351     
 iteration         1065 MCMCOBJ=   -6617.98177180969     
 iteration         1070 MCMCOBJ=   -6637.99299368711     
 iteration         1075 MCMCOBJ=   -6582.23961141444     
 iteration         1080 MCMCOBJ=   -6566.53940918461     
 iteration         1085 MCMCOBJ=   -6572.36257887505     
 iteration         1090 MCMCOBJ=   -6534.88664912217     
 iteration         1095 MCMCOBJ=   -6619.92068662654     
 iteration         1100 MCMCOBJ=   -6604.54063002153     
 iteration         1105 MCMCOBJ=   -6559.26353548078     
 iteration         1110 MCMCOBJ=   -6588.75487973129     
 iteration         1115 MCMCOBJ=   -6600.72667897206     
 iteration         1120 MCMCOBJ=   -6624.65351327666     
 iteration         1125 MCMCOBJ=   -6588.06760728943     
 iteration         1130 MCMCOBJ=   -6628.07223302360     
 iteration         1135 MCMCOBJ=   -6577.88010394109     
 iteration         1140 MCMCOBJ=   -6613.19022091805     
 iteration         1145 MCMCOBJ=   -6535.10589854219     
 iteration         1150 MCMCOBJ=   -6593.29163943036     
 iteration         1155 MCMCOBJ=   -6620.27363524949     
 iteration         1160 MCMCOBJ=   -6570.40991260972     
 iteration         1165 MCMCOBJ=   -6564.65867253564     
 iteration         1170 MCMCOBJ=   -6548.70162984869     
 iteration         1175 MCMCOBJ=   -6602.40301765353     
 iteration         1180 MCMCOBJ=   -6583.12049319161     
 iteration         1185 MCMCOBJ=   -6644.55928905136     
 iteration         1190 MCMCOBJ=   -6604.03053202950     
 iteration         1195 MCMCOBJ=   -6547.74862532713     
 iteration         1200 MCMCOBJ=   -6632.07294775466     
 iteration         1205 MCMCOBJ=   -6595.55462791401     
 iteration         1210 MCMCOBJ=   -6623.85524242539     
 iteration         1215 MCMCOBJ=   -6555.09247757595     
 iteration         1220 MCMCOBJ=   -6598.00945648483     
 iteration         1225 MCMCOBJ=   -6549.22027046979     
 iteration         1230 MCMCOBJ=   -6614.73857210398     
 iteration         1235 MCMCOBJ=   -6612.70224905661     
 iteration         1240 MCMCOBJ=   -6653.74844274453     
 iteration         1245 MCMCOBJ=   -6618.00683100249     
 iteration         1250 MCMCOBJ=   -6613.04092228511     
 iteration         1255 MCMCOBJ=   -6621.98368391117     
 iteration         1260 MCMCOBJ=   -6633.29205739483     
 iteration         1265 MCMCOBJ=   -6667.21684828735     
 iteration         1270 MCMCOBJ=   -6541.66896955210     
 iteration         1275 MCMCOBJ=   -6515.71651015157     
 iteration         1280 MCMCOBJ=   -6579.61773106405     
 iteration         1285 MCMCOBJ=   -6566.27536502911     
 iteration         1290 MCMCOBJ=   -6652.44496294105     
 iteration         1295 MCMCOBJ=   -6653.86156267352     
 iteration         1300 MCMCOBJ=   -6673.88670457753     
 iteration         1305 MCMCOBJ=   -6652.95172028677     
 iteration         1310 MCMCOBJ=   -6598.96285758263     
 iteration         1315 MCMCOBJ=   -6630.70224893388     
 iteration         1320 MCMCOBJ=   -6601.58121791892     
 iteration         1325 MCMCOBJ=   -6488.90707449407     
 iteration         1330 MCMCOBJ=   -6665.05363264083     
 iteration         1335 MCMCOBJ=   -6627.79898144403     
 iteration         1340 MCMCOBJ=   -6536.56916246284     
 iteration         1345 MCMCOBJ=   -6641.43009602637     
 iteration         1350 MCMCOBJ=   -6623.81080626570     
 iteration         1355 MCMCOBJ=   -6546.14112998638     
 iteration         1360 MCMCOBJ=   -6590.49876355080     
 iteration         1365 MCMCOBJ=   -6604.27734964901     
 iteration         1370 MCMCOBJ=   -6649.75437506341     
 iteration         1375 MCMCOBJ=   -6630.06391814653     
 iteration         1380 MCMCOBJ=   -6557.49234363169     
 iteration         1385 MCMCOBJ=   -6574.01369705329     
 iteration         1390 MCMCOBJ=   -6575.16702073014     
 iteration         1395 MCMCOBJ=   -6622.65129216402     
 iteration         1400 MCMCOBJ=   -6622.12386036725     
 iteration         1405 MCMCOBJ=   -6570.58637816814     
 iteration         1410 MCMCOBJ=   -6614.01885540857     
 iteration         1415 MCMCOBJ=   -6578.55288682399     
 iteration         1420 MCMCOBJ=   -6617.14222091237     
 iteration         1425 MCMCOBJ=   -6607.56103229385     
 iteration         1430 MCMCOBJ=   -6563.43554793489     
 iteration         1435 MCMCOBJ=   -6603.65064460489     
 iteration         1440 MCMCOBJ=   -6539.27235930367     
 iteration         1445 MCMCOBJ=   -6561.92013857845     
 iteration         1450 MCMCOBJ=   -6631.25355886860     
 iteration         1455 MCMCOBJ=   -6574.20407376139     
 iteration         1460 MCMCOBJ=   -6548.60360106048     
 iteration         1465 MCMCOBJ=   -6647.98828905900     
 iteration         1470 MCMCOBJ=   -6588.96025578108     
 iteration         1475 MCMCOBJ=   -6598.29248128363     
 iteration         1480 MCMCOBJ=   -6624.95588823947     
 iteration         1485 MCMCOBJ=   -6636.14696164580     
 iteration         1490 MCMCOBJ=   -6607.36998249844     
 iteration         1495 MCMCOBJ=   -6628.99330776769     
 iteration         1500 MCMCOBJ=   -6542.10686428137     
 iteration         1505 MCMCOBJ=   -6624.00346311622     
 iteration         1510 MCMCOBJ=   -6590.18037463267     
 iteration         1515 MCMCOBJ=   -6551.37485874906     
 iteration         1520 MCMCOBJ=   -6596.30197855413     
 iteration         1525 MCMCOBJ=   -6516.34430686754     
 iteration         1530 MCMCOBJ=   -6554.44292580881     
 iteration         1535 MCMCOBJ=   -6610.04536138685     
 iteration         1540 MCMCOBJ=   -6526.47155588517     
 iteration         1545 MCMCOBJ=   -6599.84923329279     
 iteration         1550 MCMCOBJ=   -6639.69327520824     
 iteration         1555 MCMCOBJ=   -6665.67678060531     
 iteration         1560 MCMCOBJ=   -6590.56252904830     
 iteration         1565 MCMCOBJ=   -6580.17503498416     
 iteration         1570 MCMCOBJ=   -6571.97105426148     
 iteration         1575 MCMCOBJ=   -6593.64163626064     
 iteration         1580 MCMCOBJ=   -6553.91323409446     
 iteration         1585 MCMCOBJ=   -6631.98795433569     
 iteration         1590 MCMCOBJ=   -6609.44532780582     
 iteration         1595 MCMCOBJ=   -6597.80411686479     
 iteration         1600 MCMCOBJ=   -6600.22831625622     
 iteration         1605 MCMCOBJ=   -6555.09263978788     
 iteration         1610 MCMCOBJ=   -6547.05731550717     
 iteration         1615 MCMCOBJ=   -6607.78149099912     
 iteration         1620 MCMCOBJ=   -6575.56971169688     
 iteration         1625 MCMCOBJ=   -6600.49297601414     
 iteration         1630 MCMCOBJ=   -6624.47995630803     
 iteration         1635 MCMCOBJ=   -6583.07120511287     
 iteration         1640 MCMCOBJ=   -6569.15312458652     
 iteration         1645 MCMCOBJ=   -6603.33279920666     
 iteration         1650 MCMCOBJ=   -6557.41768209520     
 iteration         1655 MCMCOBJ=   -6596.11083097286     
 iteration         1660 MCMCOBJ=   -6589.06439022775     
 iteration         1665 MCMCOBJ=   -6575.45742681290     
 iteration         1670 MCMCOBJ=   -6549.13621251838     
 iteration         1675 MCMCOBJ=   -6607.66229649280     
 iteration         1680 MCMCOBJ=   -6622.13911776402     
 iteration         1685 MCMCOBJ=   -6608.02044022105     
 iteration         1690 MCMCOBJ=   -6564.39165984866     
 iteration         1695 MCMCOBJ=   -6567.58789192575     
 iteration         1700 MCMCOBJ=   -6553.23897211028     
 iteration         1705 MCMCOBJ=   -6571.74697453316     
 iteration         1710 MCMCOBJ=   -6553.90747194431     
 iteration         1715 MCMCOBJ=   -6610.39204459699     
 iteration         1720 MCMCOBJ=   -6610.81114135523     
 iteration         1725 MCMCOBJ=   -6617.50250169273     
 iteration         1730 MCMCOBJ=   -6623.21287570477     
 iteration         1735 MCMCOBJ=   -6595.00027160618     
 iteration         1740 MCMCOBJ=   -6607.19861052384     
 iteration         1745 MCMCOBJ=   -6561.91198395195     
 iteration         1750 MCMCOBJ=   -6656.32754364676     
 iteration         1755 MCMCOBJ=   -6598.94117096682     
 iteration         1760 MCMCOBJ=   -6608.42472121195     
 iteration         1765 MCMCOBJ=   -6550.71102018197     
 iteration         1770 MCMCOBJ=   -6598.73831062118     
 iteration         1775 MCMCOBJ=   -6620.59977286329     
 iteration         1780 MCMCOBJ=   -6561.09898593020     
 iteration         1785 MCMCOBJ=   -6567.22800233575     
 iteration         1790 MCMCOBJ=   -6632.47732131583     
 iteration         1795 MCMCOBJ=   -6590.45102230517     
 iteration         1800 MCMCOBJ=   -6575.08214199230     
 iteration         1805 MCMCOBJ=   -6592.00422654415     
 iteration         1810 MCMCOBJ=   -6574.41833832296     
 iteration         1815 MCMCOBJ=   -6584.61269184616     
 iteration         1820 MCMCOBJ=   -6606.03110288407     
 iteration         1825 MCMCOBJ=   -6607.50442365204     
 iteration         1830 MCMCOBJ=   -6556.14674791019     
 iteration         1835 MCMCOBJ=   -6541.13780668222     
 iteration         1840 MCMCOBJ=   -6627.85950490258     
 iteration         1845 MCMCOBJ=   -6589.37388622224     
 iteration         1850 MCMCOBJ=   -6611.14892805330     
 iteration         1855 MCMCOBJ=   -6666.42912339101     
 iteration         1860 MCMCOBJ=   -6628.95328770130     
 iteration         1865 MCMCOBJ=   -6631.85386167166     
 iteration         1870 MCMCOBJ=   -6643.45411688351     
 iteration         1875 MCMCOBJ=   -6626.18374400215     
 iteration         1880 MCMCOBJ=   -6628.19328315347     
 iteration         1885 MCMCOBJ=   -6600.99349073522     
 iteration         1890 MCMCOBJ=   -6624.73590123623     
 iteration         1895 MCMCOBJ=   -6590.37903306274     
 iteration         1900 MCMCOBJ=   -6596.30724876076     
 iteration         1905 MCMCOBJ=   -6500.14842286811     
 iteration         1910 MCMCOBJ=   -6553.69157730441     
 iteration         1915 MCMCOBJ=   -6631.31144273484     
 iteration         1920 MCMCOBJ=   -6582.46536806226     
 iteration         1925 MCMCOBJ=   -6620.75645889131     
 iteration         1930 MCMCOBJ=   -6570.84313217409     
 iteration         1935 MCMCOBJ=   -6661.82004166766     
 iteration         1940 MCMCOBJ=   -6600.56272733329     
 iteration         1945 MCMCOBJ=   -6610.58623385044     
 iteration         1950 MCMCOBJ=   -6613.55018444930     
 iteration         1955 MCMCOBJ=   -6617.75896438454     
 iteration         1960 MCMCOBJ=   -6576.28817016768     
 iteration         1965 MCMCOBJ=   -6544.43488097104     
 iteration         1970 MCMCOBJ=   -6588.53206677532     
 iteration         1975 MCMCOBJ=   -6657.24052974596     
 iteration         1980 MCMCOBJ=   -6618.20263132497     
 iteration         1985 MCMCOBJ=   -6570.33337450223     
 iteration         1990 MCMCOBJ=   -6565.13247221928     
 iteration         1995 MCMCOBJ=   -6589.37557235596     
 iteration         2000 MCMCOBJ=   -6591.52174151171     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6596.90043184543     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3715.10919171558     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6596.90043184543     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5861.74960528169     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -16.9020929079541     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6596.90043184543     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6613.80252475338     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  7905.64
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6596.900       **************************************************
 #OBJS:********************************************       35.973 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.21E+00  5.57E-01 -1.79E-01  2.27E+00  2.37E-01  3.71E+00 -7.06E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.64E-01
 
 ETA2
+       -3.06E-02  1.77E-01
 
 ETA3
+        2.84E-02 -1.04E-02  1.05E-01
 
 ETA4
+        2.26E-02  3.31E-02 -1.14E-02  2.48E-01
 
 ETA5
+        2.07E-02  1.90E-02 -1.14E-03 -2.33E-02  1.89E-01
 
 ETA6
+       -1.19E-02  1.09E-02  1.67E-02  1.10E-02 -5.58E-02  2.12E-01
 
 ETA7
+        1.25E-02 -3.58E-02  1.87E-02 -5.49E-02  1.82E-02  7.66E-03  2.25E-01
 
 ETA8
+        6.99E-02  6.24E-02  2.61E-02  3.22E-02 -5.50E-03 -4.16E-02  4.67E-02  1.92E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.42E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.11E-01
 
 ETA2
+       -1.40E-01  4.17E-01
 
 ETA3
+        1.74E-01 -7.76E-02  3.21E-01
 
 ETA4
+        8.90E-02  1.59E-01 -7.42E-02  4.94E-01
 
 ETA5
+        9.39E-02  1.05E-01 -9.79E-03 -1.09E-01  4.32E-01
 
 ETA6
+       -5.44E-02  5.80E-02  1.16E-01  4.81E-02 -2.83E-01  4.56E-01
 
 ETA7
+        5.18E-02 -1.76E-01  1.23E-01 -2.34E-01  8.86E-02  3.50E-02  4.72E-01
 
 ETA8
+        3.12E-01  3.43E-01  1.83E-01  1.49E-01 -2.85E-02 -2.07E-01  2.24E-01  4.36E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.70E-02
 
 EPS2
+        0.00E+00  1.50E-01
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                STANDARD ERROR OF ESTIMATE (From Sample Variance)               ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         7.57E-02  7.24E-02  5.46E-02  7.49E-02  6.56E-02  7.18E-02  6.73E-02  6.34E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.53E-02
 
 ETA2
+        2.94E-02  4.96E-02
 
 ETA3
+        2.19E-02  1.88E-02  2.95E-02
 
 ETA4
+        3.13E-02  2.90E-02  2.26E-02  5.81E-02
 
 ETA5
+        2.75E-02  2.45E-02  1.83E-02  2.61E-02  4.01E-02
 
 ETA6
+        3.05E-02  2.66E-02  2.13E-02  2.94E-02  2.74E-02  5.73E-02
 
 ETA7
+        2.91E-02  2.84E-02  2.00E-02  2.98E-02  2.50E-02  2.68E-02  4.45E-02
 
 ETA8
+        2.82E-02  2.43E-02  1.91E-02  2.67E-02  2.28E-02  2.65E-02  2.52E-02  3.94E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.56E-04
 
 EPS2
+        0.00E+00  1.19E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.28E-02
 
 ETA2
+        1.27E-01  5.73E-02
 
 ETA3
+        1.27E-01  1.36E-01  4.45E-02
 
 ETA4
+        1.18E-01  1.30E-01  1.37E-01  5.68E-02
 
 ETA5
+        1.20E-01  1.28E-01  1.29E-01  1.17E-01  4.51E-02
 
 ETA6
+        1.27E-01  1.34E-01  1.39E-01  1.25E-01  1.24E-01  6.07E-02
 
 ETA7
+        1.17E-01  1.29E-01  1.26E-01  1.15E-01  1.18E-01  1.20E-01  4.62E-02
 
 ETA8
+        1.10E-01  1.15E-01  1.22E-01  1.17E-01  1.16E-01  1.20E-01  1.10E-01  4.38E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.37E-03
 
 EPS2
+        0.00E+00  3.97E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************               COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        5.73E-03
 
 TH 2
+       -7.56E-04  5.25E-03
 
 TH 3
+        4.37E-04 -7.98E-05  2.98E-03
 
 TH 4
+        4.13E-04  5.60E-04  1.42E-04  5.61E-03
 
 TH 5
+        3.93E-04  1.27E-04 -2.04E-05 -3.62E-04  4.31E-03
 
 TH 6
+       -5.05E-05 -3.35E-05  3.35E-04  3.47E-04 -6.23E-04  5.15E-03
 
 TH 7
+        2.33E-04 -1.10E-03  5.49E-04 -1.02E-03  3.95E-04  3.42E-04  4.53E-03
 
 TH 8
+        1.21E-03  9.72E-04  7.20E-04  6.15E-04 -8.66E-05 -6.38E-04  9.81E-04  4.02E-03
 
 OM11
+       -2.51E-04  2.10E-04  2.35E-06 -1.61E-04  1.03E-04 -7.37E-05  1.84E-04  1.52E-04  3.06E-03
 
 OM12
+       -1.58E-04  2.15E-04 -6.63E-05 -6.60E-06  8.17E-07 -9.06E-05 -5.30E-05 -3.65E-05 -2.31E-04  8.64E-04
 
 OM13
+       -2.00E-05  5.57E-06  8.68E-05 -1.41E-05 -6.09E-06 -8.96E-05  3.20E-05  4.91E-05  1.06E-04 -7.34E-06  4.79E-04
 
 OM14
+        7.57E-06  1.22E-04  8.87E-06 -1.92E-05  8.67E-06 -5.65E-05  3.25E-05  1.79E-05  1.97E-04  5.59E-05  1.39E-05  9.78E-04
 
 OM15
+       -8.87E-05  3.41E-05 -4.75E-05 -2.02E-05  3.90E-06  3.32E-05  1.21E-05 -1.46E-05  1.26E-04  2.44E-05  6.03E-06 -4.60E-05
          7.58E-04
 
 OM16
+       -4.51E-05  3.62E-05 -2.28E-05  4.22E-05  7.62E-05  5.50E-05  1.55E-05  1.42E-05  8.08E-05  3.70E-05  3.16E-05  3.90E-05
         -8.55E-05  9.32E-04
 
 OM17
+       -7.20E-06 -4.26E-05  6.58E-05  1.53E-05  5.59E-05 -2.81E-05 -2.26E-06  2.49E-05  9.66E-05 -9.87E-05  2.37E-05 -1.38E-04
          1.11E-04  4.27E-05  8.48E-04
 
 OM18
+       -5.93E-05  6.78E-05  5.53E-05  8.41E-05  3.34E-05 -3.17E-05  4.48E-05  8.29E-05  5.09E-04  1.48E-04  8.62E-05  1.19E-04
          1.52E-05 -9.50E-05  1.79E-04  7.97E-04
 
 OM22
+       -8.36E-06 -9.32E-04  6.38E-05  5.77E-05  7.43E-05  4.56E-05  2.09E-05  1.37E-05  8.55E-05 -4.56E-04  2.88E-05 -8.76E-05
         -6.37E-05 -6.86E-05  1.09E-04 -3.45E-05  2.46E-03
 
 OM23
+        1.29E-05  9.78E-05  1.78E-05  3.32E-05 -3.99E-05  2.84E-05  8.80E-06  2.29E-05 -2.58E-05  6.26E-05 -2.82E-05  7.92E-06
          2.63E-06 -1.07E-05 -2.67E-06 -1.63E-05 -1.19E-04  3.53E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -1.06E-04 -3.56E-04  3.60E-05 -9.86E-06 -3.07E-05  5.07E-05  1.26E-06 -6.08E-05 -5.35E-07 -2.11E-05  1.95E-05 -8.15E-05
         -3.82E-05 -5.28E-06  1.54E-05  1.28E-05  3.37E-04 -1.69E-05  8.39E-04
 
 OM25
+        9.21E-05 -1.11E-04  2.62E-05 -1.16E-04  3.63E-05 -9.23E-05  7.75E-06  1.20E-05 -1.11E-05  5.41E-05  3.07E-06 -6.02E-06
         -8.14E-05  3.40E-05  6.77E-06  8.55E-06  1.19E-04 -5.56E-06  2.65E-05  6.01E-04
 
 OM26
+        3.23E-05  3.53E-05  3.49E-05 -2.68E-05 -4.52E-05  1.19E-05 -6.52E-07 -1.04E-04 -1.66E-05 -4.03E-05 -4.35E-06 -1.44E-05
          3.30E-06 -9.66E-05 -7.94E-06 -3.21E-05  6.59E-05  1.75E-05  4.43E-06 -1.23E-04  7.08E-04
 
 OM27
+        5.97E-05  4.31E-04 -8.14E-05 -1.62E-05 -2.04E-05 -5.63E-05 -4.19E-05 -7.77E-05 -5.40E-06  6.56E-05 -1.38E-05  6.43E-05
          2.22E-05  1.07E-05 -1.12E-04  8.03E-07 -5.60E-04  5.08E-05 -2.22E-04  9.82E-07  2.77E-05  8.07E-04
 
 OM28
+       -3.31E-05 -1.19E-04 -8.14E-06  2.35E-05 -2.06E-05 -3.89E-05 -3.69E-05 -3.68E-05 -6.21E-05  9.65E-05  1.39E-05 -2.25E-05
         -1.35E-05  4.64E-07 -1.27E-05 -9.26E-06  4.20E-04  3.29E-05  1.18E-04  2.85E-05 -1.23E-04  4.62E-05  5.91E-04
 
 OM33
+        3.92E-05  6.46E-05 -1.61E-04 -5.18E-05  1.03E-05  5.27E-05 -3.50E-05 -4.60E-05  1.26E-05  5.41E-06  9.09E-05  1.91E-05
          1.33E-05 -1.11E-05  1.05E-05  5.29E-06 -1.60E-06 -2.97E-05 -2.00E-05 -3.20E-05  4.37E-05  9.81E-06 -2.76E-05  8.72E-04
 
 OM34
+       -1.68E-04  1.18E-04  1.76E-04  7.89E-05  6.80E-06  2.13E-05  1.67E-05 -2.28E-05 -5.18E-05  4.49E-05  5.33E-05  4.30E-05
          8.72E-07  1.30E-05  1.14E-05  1.03E-05  1.93E-05  4.96E-05 -1.81E-05  8.70E-06  6.07E-06  1.05E-05  7.70E-06 -3.92E-05
         5.11E-04
 
 OM35
+        6.63E-05  2.86E-05 -4.56E-06  8.18E-06  7.36E-05  7.85E-06 -2.45E-05  7.92E-06 -2.33E-05  4.24E-06  3.07E-06 -2.35E-05
          1.88E-05 -5.28E-06  1.19E-05  6.72E-06 -8.70E-06  4.12E-05  1.10E-05 -7.76E-06 -5.99E-06 -1.00E-05  8.77E-06  2.24E-05
        -3.24E-05  3.34E-04
 
 OM36
+        3.94E-05 -1.84E-05 -3.63E-05  4.23E-05 -1.58E-05 -7.72E-06 -8.15E-06 -1.13E-05 -3.91E-05 -2.03E-05  2.11E-05 -2.69E-06
          7.32E-06  5.66E-05 -3.11E-05 -1.81E-05  1.55E-05 -6.04E-06 -3.62E-05  1.29E-05 -2.11E-05 -1.19E-05 -1.75E-05  4.43E-05
         1.01E-05 -7.26E-05  4.55E-04
 
 OM37
+        2.71E-06  1.53E-06 -7.45E-05 -1.25E-04  8.01E-05  3.38E-05  4.95E-05  1.68E-05  1.34E-05 -1.31E-05 -9.46E-06  2.80E-06
          1.77E-05 -1.30E-05  1.76E-05  1.12E-05 -2.99E-06 -6.29E-05  9.20E-06  8.59E-06 -1.58E-05 -8.87E-06 -8.22E-06  8.79E-05
        -9.67E-05  6.30E-06  2.04E-05  4.01E-04
 
 OM38
+       -3.83E-05  1.69E-05  1.46E-05  1.71E-05  2.09E-05  1.08E-05  5.45E-06  2.92E-05  3.07E-05  2.94E-05  1.14E-04  2.99E-05
         -1.23E-05 -4.30E-06  2.18E-05  9.05E-05  8.33E-06  6.18E-05  2.67E-05 -6.92E-07  5.10E-06 -1.14E-05  1.36E-05  1.48E-04
         5.92E-05 -1.51E-05 -5.60E-05  7.57E-05  3.65E-04
 
 OM44
+        2.68E-04 -8.43E-05  4.08E-04  3.38E-04  2.40E-05  1.76E-04 -2.20E-05 -3.57E-05 -1.01E-05  2.52E-05  3.30E-06  1.58E-04
         -1.44E-05  7.94E-05 -1.04E-05 -1.42E-05  2.10E-04  1.75E-05  2.80E-04  1.56E-05  3.50E-05 -7.41E-05  6.65E-05 -1.45E-04
         8.93E-05 -2.48E-05 -1.56E-05  1.85E-06  1.37E-05  3.37E-03
 
 OM45
+       -6.82E-05  4.86E-05  9.60E-06  2.20E-05  5.05E-05  1.09E-07  1.23E-04  5.56E-05  2.85E-05  2.66E-05 -1.46E-05  5.11E-05
          2.67E-05  1.10E-05 -1.27E-05  3.75E-05 -5.78E-05  3.29E-05  3.85E-05  6.56E-05  2.63E-06  2.14E-05  1.42E-07 -1.64E-05
        -6.84E-06 -5.75E-06 -1.46E-06  4.06E-06  1.17E-05 -1.26E-04  6.81E-04
 
 OM46
+        2.23E-05  1.28E-05 -1.11E-04 -1.52E-06 -2.03E-05  1.08E-04 -7.23E-05 -2.51E-05  3.87E-06  1.82E-05 -6.12E-06  7.69E-06
         -8.87E-06  8.38E-05  1.67E-05 -4.46E-06  2.64E-05 -2.93E-05  4.74E-06  2.73E-05  3.34E-05 -3.11E-05 -2.98E-05 -1.12E-06
        -4.40E-06 -1.33E-05 -4.26E-07  1.02E-05 -7.40E-06  1.58E-04 -1.21E-04  8.66E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.07E-04 -9.80E-06 -1.81E-05 -8.33E-05 -1.12E-05  8.38E-05 -8.13E-05  2.08E-05  2.42E-05 -4.30E-05 -7.41E-06 -1.64E-06
          1.12E-05 -2.33E-06  4.27E-05  3.99E-05 -1.20E-04 -9.88E-06 -1.71E-04  2.89E-05 -1.23E-05  1.01E-04 -3.91E-05  1.03E-05
        -7.07E-06  2.54E-05 -3.01E-05 -3.15E-06 -1.97E-05 -4.85E-04  8.03E-05  2.06E-05  8.85E-04
 
 OM48
+       -6.98E-05  1.51E-04  6.45E-05  5.32E-05 -1.87E-08  1.12E-06 -3.12E-05  1.06E-05  9.30E-05  3.79E-05  3.23E-05  2.15E-04
          1.92E-06  2.06E-05  1.62E-05  1.22E-04  3.16E-05  1.61E-05  1.83E-04  1.79E-05 -8.80E-06  3.11E-05  7.34E-05  4.16E-05
         8.13E-05  9.67E-07 -1.87E-05 -1.29E-05  2.74E-05  1.53E-04  1.99E-05 -1.41E-04  8.62E-05  7.12E-04
 
 OM55
+       -3.52E-05 -4.08E-05  8.05E-05 -6.76E-05 -5.43E-05  2.19E-05 -2.16E-05  5.87E-05  6.56E-05 -1.04E-05  3.20E-05  3.65E-05
          8.79E-05 -3.30E-05  8.42E-05  3.07E-05  2.61E-05  1.01E-05 -5.78E-05  7.37E-05  4.99E-05 -4.96E-05 -5.09E-05  2.64E-07
         3.00E-05  2.86E-05  1.73E-06 -2.25E-05  1.86E-08 -1.23E-04 -9.58E-05  3.86E-05  7.64E-06 -4.98E-06  1.61E-03
 
 OM56
+        6.48E-05  6.24E-05  1.14E-05 -1.05E-05  7.48E-05 -1.37E-04 -1.02E-04 -2.40E-05  2.91E-05  1.05E-05 -1.46E-05  3.76E-05
          1.24E-05  2.13E-06 -1.77E-05  2.21E-05  1.41E-05 -1.71E-05  1.27E-05  4.67E-05 -1.87E-05 -2.80E-06 -1.22E-05 -3.50E-06
         2.57E-06  1.68E-05  1.74E-05 -6.02E-06 -1.52E-05  5.68E-06  2.50E-05 -8.03E-05 -4.84E-06  1.54E-05 -2.12E-04  7.53E-04
 
 OM57
+        2.75E-05  6.09E-07  7.16E-05 -4.65E-06  3.96E-05  8.37E-05 -2.81E-05 -6.04E-06  3.35E-05 -2.78E-05 -6.58E-06  1.70E-05
          9.16E-06 -2.71E-05  3.13E-05  3.20E-05  2.15E-05  1.05E-05 -1.82E-05 -8.09E-05  1.73E-05 -1.66E-06  1.89E-05  7.04E-06
         2.32E-05  2.83E-05 -8.95E-06 -2.77E-06  7.29E-06  3.49E-05 -1.30E-04 -1.78E-05 -5.88E-05 -8.14E-06  9.59E-05  4.77E-05
          6.25E-04
 
 OM58
+        7.05E-05  3.97E-05  2.79E-05 -4.20E-05  3.16E-06  2.53E-05 -1.17E-05 -1.75E-05  4.40E-05  3.58E-05  1.06E-06 -2.33E-05
          1.43E-04 -1.50E-05  3.80E-05  3.73E-05 -5.35E-05 -4.45E-06  1.91E-05  8.65E-05 -1.41E-05  1.46E-05  2.19E-05  9.95E-06
        -1.17E-05  4.72E-05 -1.89E-05  2.12E-05  2.20E-06  3.10E-05  5.14E-05 -2.11E-05  1.44E-05  3.46E-07 -8.46E-05 -6.94E-05
          9.69E-05  5.22E-04
 
 OM66
+       -1.39E-04 -2.21E-04  7.84E-05 -4.03E-06 -2.55E-05 -1.19E-05 -3.16E-05 -1.62E-04  7.40E-05 -5.12E-05  3.36E-05 -7.36E-05
         -5.07E-05  1.45E-04 -3.74E-07  1.89E-05  7.08E-05 -4.91E-05 -9.60E-06 -4.18E-07  5.29E-05  1.62E-05  3.26E-05  9.80E-06
         6.39E-06 -1.59E-05  1.07E-04 -1.01E-05 -1.13E-05  1.28E-04 -6.19E-05  1.01E-04  7.93E-06 -6.32E-05 -4.86E-05 -4.59E-04
         -7.86E-05  2.40E-05  3.28E-03
 
 OM67
+       -4.58E-05 -1.42E-05 -6.00E-05  1.23E-04 -2.44E-05 -1.24E-04 -5.10E-05  8.98E-06  1.64E-05 -1.55E-06  2.62E-05  2.69E-05
         -3.87E-06  3.09E-05 -1.85E-05  4.11E-06 -2.33E-05 -3.36E-05  7.60E-07  1.41E-05 -1.19E-04  2.17E-05  3.94E-05  2.49E-05
        -2.19E-05 -6.06E-06  4.61E-05  2.83E-05 -6.15E-06 -4.98E-05  2.21E-05 -1.15E-04 -1.08E-05  4.45E-05 -3.21E-05  5.77E-05
         -1.16E-04 -5.03E-05  7.26E-05  7.17E-04
 
 OM68
+        5.90E-05 -4.62E-05  3.40E-05 -2.06E-05 -3.03E-05  6.03E-05  3.51E-05  2.16E-05  1.78E-05 -2.01E-05  6.03E-06  3.85E-05
         -3.81E-05  1.77E-04 -3.86E-06 -3.96E-05 -7.49E-05  8.49E-06  2.46E-06 -2.04E-05  1.41E-04 -2.84E-05 -5.57E-05  8.16E-06
         1.42E-05 -1.89E-05  8.05E-05  1.35E-05  2.33E-06 -6.16E-05  4.38E-06  1.01E-04 -7.56E-06  1.08E-05  3.40E-05  3.20E-05
         -2.74E-05 -1.08E-04 -4.22E-04  1.07E-04  7.04E-04
 
 OM77
+       -1.70E-04  4.92E-05  1.03E-04  2.91E-05  1.76E-05 -1.78E-07  1.62E-05  5.38E-05  1.98E-05  1.53E-05  2.63E-06 -9.14E-06
         -9.15E-06 -8.19E-06  1.35E-05  3.40E-08  2.29E-04 -2.88E-05  7.29E-05 -2.64E-05 -2.29E-05 -3.11E-04  2.58E-05  1.93E-05
         8.93E-06  2.57E-05  2.01E-05  7.50E-05  3.22E-05  9.43E-05 -4.23E-05 -4.39E-07 -3.13E-04 -5.72E-05  4.48E-05 -6.92E-05
          1.28E-04  5.18E-05  1.12E-04  6.36E-05 -1.53E-05  1.98E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.92E-05 -1.33E-04  1.16E-05 -4.82E-05 -7.54E-06  1.29E-05 -1.60E-05  4.57E-05  2.26E-05 -4.96E-05  5.80E-06 -3.22E-06
          3.16E-05 -1.76E-05  1.35E-04  3.06E-05  1.45E-05 -2.54E-06  1.94E-05 -9.69E-06  1.21E-05  9.16E-05 -3.00E-05  4.48E-05
        -3.68E-05  8.99E-06 -2.36E-05  6.56E-05  3.72E-05 -7.77E-05 -9.42E-06 -4.24E-06  3.85E-05 -7.57E-05 -2.14E-05 -2.73E-05
          1.07E-05  5.10E-05  8.15E-06 -9.26E-05 -1.22E-05  3.28E-04  6.37E-04
 
 OM88
+       -7.50E-05 -7.62E-06  8.00E-05  1.55E-04  4.95E-05  4.71E-05  8.73E-05  7.21E-05  1.55E-04  1.08E-04  8.74E-05  5.59E-05
          4.83E-06 -4.19E-05  5.19E-05  3.90E-04  7.51E-05  4.63E-05  8.84E-05 -1.88E-05 -8.34E-05  9.16E-05  3.30E-04  4.81E-05
         1.19E-06  2.72E-05 -5.29E-05  2.89E-05  1.92E-04  7.96E-05  1.23E-05 -2.83E-05 -6.12E-06  2.00E-04 -4.41E-05 -4.65E-06
          3.81E-05 -2.04E-05  1.37E-04 -1.86E-05 -2.66E-04  5.65E-05  2.85E-04  1.55E-03
 
 SG11
+        1.01E-06 -1.53E-06  1.58E-08 -1.41E-06 -2.10E-06  2.17E-06  2.27E-06 -2.45E-07 -1.19E-06 -4.72E-07 -2.46E-07  2.04E-07
         -6.74E-08 -2.03E-07 -7.61E-08  3.13E-09 -3.74E-07  2.01E-07  6.21E-07 -2.07E-07  3.26E-07 -8.70E-07 -1.43E-07 -8.73E-07
        -7.46E-08  3.57E-07  4.34E-08  3.57E-07  2.43E-07  8.35E-08 -3.33E-07  6.56E-08  1.30E-07  1.99E-07  9.56E-07 -4.91E-07
          1.09E-07  5.02E-07  3.20E-07  1.27E-07  1.07E-06  2.25E-06 -3.38E-08 -6.46E-07  4.31E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.41E-06 -2.53E-06 -2.21E-06 -3.02E-06 -4.33E-06  1.24E-06 -1.19E-06 -2.46E-06  1.57E-07 -1.37E-06  1.09E-06  1.73E-07
         -6.29E-07 -5.47E-07 -4.73E-07 -2.05E-06  7.51E-07 -2.70E-07  5.37E-09  9.76E-07 -7.58E-08  4.68E-07  2.74E-07  1.20E-06
        -1.46E-06  2.02E-07  7.31E-07  5.01E-07 -7.28E-08  7.84E-07 -1.27E-07  6.37E-07 -7.11E-07  1.44E-08  1.92E-06 -4.56E-08
          1.56E-07  1.01E-06 -6.58E-06 -3.05E-07  6.18E-07 -5.85E-07 -4.46E-07 -6.10E-07 -4.32E-08  0.00E+00  1.42E-06
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************              CORRELATION MATRIX OF ESTIMATE (From Sample Variance)             ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        7.57E-02
 
 TH 2
+       -1.38E-01  7.24E-02
 
 TH 3
+        1.06E-01 -2.02E-02  5.46E-02
 
 TH 4
+        7.29E-02  1.03E-01  3.47E-02  7.49E-02
 
 TH 5
+        7.92E-02  2.67E-02 -5.69E-03 -7.37E-02  6.56E-02
 
 TH 6
+       -9.29E-03 -6.44E-03  8.55E-02  6.45E-02 -1.32E-01  7.18E-02
 
 TH 7
+        4.58E-02 -2.26E-01  1.50E-01 -2.02E-01  8.94E-02  7.08E-02  6.73E-02
 
 TH 8
+        2.52E-01  2.12E-01  2.08E-01  1.30E-01 -2.08E-02 -1.40E-01  2.30E-01  6.34E-02
 
 OM11
+       -6.00E-02  5.25E-02  7.77E-04 -3.88E-02  2.82E-02 -1.86E-02  4.94E-02  4.32E-02  5.53E-02
 
 OM12
+       -7.11E-02  1.01E-01 -4.13E-02 -3.00E-03  4.23E-04 -4.30E-02 -2.68E-02 -1.96E-02 -1.42E-01  2.94E-02
 
 OM13
+       -1.21E-02  3.51E-03  7.26E-02 -8.62E-03 -4.23E-03 -5.70E-02  2.17E-02  3.53E-02  8.75E-02 -1.14E-02  2.19E-02
 
 OM14
+        3.20E-03  5.37E-02  5.20E-03 -8.20E-03  4.22E-03 -2.52E-02  1.54E-02  9.00E-03  1.14E-01  6.09E-02  2.03E-02  3.13E-02
 
 OM15
+       -4.25E-02  1.71E-02 -3.16E-02 -9.80E-03  2.16E-03  1.68E-02  6.55E-03 -8.35E-03  8.24E-02  3.01E-02  1.00E-02 -5.34E-02
          2.75E-02
 
 OM16
+       -1.95E-02  1.64E-02 -1.37E-02  1.85E-02  3.80E-02  2.51E-02  7.54E-03  7.32E-03  4.78E-02  4.13E-02  4.72E-02  4.08E-02
         -1.02E-01  3.05E-02
 
 OM17
+       -3.27E-03 -2.02E-02  4.14E-02  7.00E-03  2.92E-02 -1.35E-02 -1.15E-03  1.35E-02  6.00E-02 -1.15E-01  3.71E-02 -1.52E-01
          1.39E-01  4.81E-02  2.91E-02
 
 OM18
+       -2.77E-02  3.31E-02  3.59E-02  3.98E-02  1.80E-02 -1.56E-02  2.36E-02  4.63E-02  3.26E-01  1.78E-01  1.39E-01  1.35E-01
          1.96E-02 -1.10E-01  2.18E-01  2.82E-02
 
 OM22
+       -2.23E-03 -2.59E-01  2.36E-02  1.55E-02  2.28E-02  1.28E-02  6.26E-03  4.35E-03  3.11E-02 -3.13E-01  2.65E-02 -5.65E-02
         -4.67E-02 -4.53E-02  7.53E-02 -2.46E-02  4.96E-02
 
 OM23
+        9.08E-03  7.19E-02  1.73E-02  2.36E-02 -3.23E-02  2.11E-02  6.97E-03  1.92E-02 -2.48E-02  1.14E-01 -6.85E-02  1.35E-02
          5.09E-03 -1.87E-02 -4.87E-03 -3.08E-02 -1.28E-01  1.88E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -4.85E-02 -1.70E-01  2.28E-02 -4.55E-03 -1.61E-02  2.44E-02  6.46E-04 -3.31E-02 -3.34E-04 -2.48E-02  3.07E-02 -9.00E-02
         -4.79E-02 -5.98E-03  1.83E-02  1.57E-02  2.34E-01 -3.11E-02  2.90E-02
 
 OM25
+        4.96E-02 -6.24E-02  1.96E-02 -6.33E-02  2.26E-02 -5.24E-02  4.70E-03  7.74E-03 -8.19E-03  7.51E-02  5.71E-03 -7.86E-03
         -1.21E-01  4.54E-02  9.48E-03  1.23E-02  9.78E-02 -1.21E-02  3.73E-02  2.45E-02
 
 OM26
+        1.60E-02  1.83E-02  2.40E-02 -1.34E-02 -2.59E-02  6.20E-03 -3.64E-04 -6.18E-02 -1.12E-02 -5.16E-02 -7.47E-03 -1.73E-02
          4.51E-03 -1.19E-01 -1.02E-02 -4.27E-02  4.99E-02  3.50E-02  5.75E-03 -1.88E-01  2.66E-02
 
 OM27
+        2.78E-02  2.09E-01 -5.25E-02 -7.60E-03 -1.09E-02 -2.76E-02 -2.19E-02 -4.32E-02 -3.44E-03  7.86E-02 -2.23E-02  7.24E-02
          2.83E-02  1.23E-02 -1.35E-01  1.00E-03 -3.98E-01  9.53E-02 -2.70E-01  1.41E-03  3.66E-02  2.84E-02
 
 OM28
+       -1.80E-02 -6.73E-02 -6.13E-03  1.29E-02 -1.29E-02 -2.23E-02 -2.26E-02 -2.38E-02 -4.61E-02  1.35E-01  2.61E-02 -2.96E-02
         -2.01E-02  6.24E-04 -1.80E-02 -1.35E-02  3.48E-01  7.21E-02  1.67E-01  4.79E-02 -1.89E-01  6.69E-02  2.43E-02
 
 OM33
+        1.75E-02  3.02E-02 -9.98E-02 -2.34E-02  5.33E-03  2.48E-02 -1.76E-02 -2.45E-02  7.74E-03  6.23E-03  1.41E-01  2.07E-02
          1.63E-02 -1.23E-02  1.22E-02  6.35E-03 -1.09E-03 -5.36E-02 -2.34E-02 -4.42E-02  5.56E-02  1.17E-02 -3.84E-02  2.95E-02
 
 OM34
+       -9.84E-02  7.23E-02  1.42E-01  4.66E-02  4.58E-03  1.31E-02  1.10E-02 -1.59E-02 -4.14E-02  6.76E-02  1.08E-01  6.08E-02
          1.40E-03  1.88E-02  1.73E-02  1.61E-02  1.72E-02  1.17E-01 -2.76E-02  1.57E-02  1.01E-02  1.64E-02  1.40E-02 -5.87E-02
         2.26E-02
 
 OM35
+        4.80E-02  2.16E-02 -4.57E-03  5.97E-03  6.13E-02  5.98E-03 -1.99E-02  6.83E-03 -2.30E-02  7.90E-03  7.68E-03 -4.12E-02
          3.73E-02 -9.47E-03  2.23E-02  1.30E-02 -9.59E-03  1.20E-01  2.08E-02 -1.73E-02 -1.23E-02 -1.93E-02  1.97E-02  4.15E-02
        -7.85E-02  1.83E-02
 
 OM36
+        2.44E-02 -1.19E-02 -3.12E-02  2.65E-02 -1.13E-02 -5.05E-03 -5.68E-03 -8.39E-03 -3.31E-02 -3.24E-02  4.52E-02 -4.04E-03
          1.25E-02  8.70E-02 -5.01E-02 -3.01E-02  1.46E-02 -1.51E-02 -5.86E-02  2.46E-02 -3.72E-02 -1.96E-02 -3.37E-02  7.04E-02
         2.10E-02 -1.86E-01  2.13E-02
 
 OM37
+        1.79E-03  1.06E-03 -6.82E-02 -8.37E-02  6.10E-02  2.35E-02  3.68E-02  1.32E-02  1.21E-02 -2.23E-02 -2.16E-02  4.47E-03
          3.20E-02 -2.13E-02  3.02E-02  1.99E-02 -3.01E-03 -1.67E-01  1.59E-02  1.75E-02 -2.97E-02 -1.56E-02 -1.69E-02  1.49E-01
        -2.14E-01  1.72E-02  4.78E-02  2.00E-02
 
 OM38
+       -2.65E-02  1.23E-02  1.40E-02  1.20E-02  1.67E-02  7.90E-03  4.24E-03  2.41E-02  2.90E-02  5.24E-02  2.73E-01  5.01E-02
         -2.34E-02 -7.37E-03  3.91E-02  1.68E-01  8.80E-03  1.72E-01  4.83E-02 -1.48E-03  1.00E-02 -2.10E-02  2.93E-02  2.63E-01
         1.37E-01 -4.32E-02 -1.38E-01  1.98E-01  1.91E-02
 
 OM44
+        6.09E-02 -2.00E-02  1.29E-01  7.78E-02  6.29E-03  4.23E-02 -5.63E-03 -9.68E-03 -3.15E-03  1.48E-02  2.60E-03  8.72E-02
         -9.00E-03  4.48E-02 -6.17E-03 -8.63E-03  7.30E-02  1.60E-02  1.67E-01  1.09E-02  2.26E-02 -4.49E-02  4.71E-02 -8.46E-02
         6.80E-02 -2.33E-02 -1.26E-02  1.59E-03  1.23E-02  5.81E-02
 
 OM45
+       -3.45E-02  2.57E-02  6.74E-03  1.13E-02  2.95E-02  5.82E-05  7.00E-02  3.36E-02  1.98E-02  3.47E-02 -2.56E-02  6.26E-02
          3.72E-02  1.38E-02 -1.67E-02  5.09E-02 -4.46E-02  6.71E-02  5.10E-02  1.02E-01  3.79E-03  2.88E-02  2.24E-04 -2.13E-02
        -1.16E-02 -1.21E-02 -2.62E-03  7.77E-03  2.35E-02 -8.31E-02  2.61E-02
 
 OM46
+        1.00E-02  6.01E-03 -6.93E-02 -6.90E-04 -1.05E-02  5.12E-02 -3.65E-02 -1.34E-02  2.38E-03  2.11E-02 -9.49E-03  8.36E-03
         -1.09E-02  9.33E-02  1.94E-02 -5.36E-03  1.81E-02 -5.30E-02  5.56E-03  3.78E-02  4.27E-02 -3.72E-02 -4.16E-02 -1.29E-03
        -6.61E-03 -2.48E-02 -6.80E-04  1.73E-02 -1.32E-02  9.22E-02 -1.57E-01  2.94E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        4.75E-02 -4.55E-03 -1.11E-02 -3.74E-02 -5.71E-03  3.93E-02 -4.06E-02  1.10E-02  1.47E-02 -4.92E-02 -1.14E-02 -1.76E-03
          1.37E-02 -2.56E-03  4.92E-02  4.74E-02 -8.15E-02 -1.77E-02 -1.98E-01  3.97E-02 -1.55E-02  1.19E-01 -5.41E-02  1.17E-02
        -1.05E-02  4.67E-02 -4.75E-02 -5.29E-03 -3.47E-02 -2.81E-01  1.03E-01  2.35E-02  2.98E-02
 
 OM48
+       -3.46E-02  7.80E-02  4.43E-02  2.66E-02 -1.07E-05  5.84E-04 -1.74E-02  6.24E-03  6.30E-02  4.83E-02  5.52E-02  2.58E-01
          2.61E-03  2.52E-02  2.08E-02  1.62E-01  2.38E-02  3.21E-02  2.37E-01  2.73E-02 -1.24E-02  4.11E-02  1.13E-01  5.28E-02
         1.35E-01  1.98E-03 -3.30E-02 -2.42E-02  5.39E-02  9.85E-02  2.86E-02 -1.80E-01  1.09E-01  2.67E-02
 
 OM55
+       -1.16E-02 -1.40E-02  3.68E-02 -2.25E-02 -2.06E-02  7.61E-03 -8.01E-03  2.31E-02  2.96E-02 -8.85E-03  3.64E-02  2.91E-02
          7.96E-02 -2.70E-02  7.21E-02  2.71E-02  1.31E-02  1.34E-02 -4.97E-02  7.49E-02  4.68E-02 -4.36E-02 -5.22E-02  2.23E-04
         3.30E-02  3.90E-02  2.03E-03 -2.81E-02  2.43E-05 -5.26E-02 -9.15E-02  3.27E-02  6.40E-03 -4.65E-03  4.01E-02
 
 OM56
+        3.12E-02  3.14E-02  7.61E-03 -5.11E-03  4.15E-02 -6.97E-02 -5.55E-02 -1.38E-02  1.91E-02  1.31E-02 -2.44E-02  4.38E-02
          1.65E-02  2.55E-03 -2.22E-02  2.85E-02  1.03E-02 -3.32E-02  1.60E-02  6.94E-02 -2.57E-02 -3.60E-03 -1.83E-02 -4.32E-03
         4.15E-03  3.34E-02  2.97E-02 -1.10E-02 -2.90E-02  3.56E-03  3.49E-02 -9.95E-02 -5.93E-03  2.10E-02 -1.93E-01  2.74E-02
 
 OM57
+        1.45E-02  3.36E-04  5.24E-02 -2.48E-03  2.41E-02  4.66E-02 -1.67E-02 -3.81E-03  2.42E-02 -3.78E-02 -1.20E-02  2.18E-02
          1.33E-02 -3.55E-02  4.30E-02  4.54E-02  1.74E-02  2.24E-02 -2.52E-02 -1.32E-01  2.60E-02 -2.34E-03  3.11E-02  9.53E-03
         4.10E-02  6.19E-02 -1.68E-02 -5.54E-03  1.53E-02  2.40E-02 -1.99E-01 -2.42E-02 -7.90E-02 -1.22E-02  9.56E-02  6.96E-02
          2.50E-02
 
 OM58
+        4.08E-02  2.40E-02  2.24E-02 -2.45E-02  2.10E-03  1.54E-02 -7.62E-03 -1.20E-02  3.48E-02  5.33E-02  2.13E-03 -3.26E-02
          2.28E-01 -2.15E-02  5.71E-02  5.78E-02 -4.72E-02 -1.04E-02  2.88E-02  1.54E-01 -2.31E-02  2.25E-02  3.94E-02  1.47E-02
        -2.26E-02  1.13E-01 -3.88E-02  4.63E-02  5.04E-03  2.33E-02  8.63E-02 -3.14E-02  2.12E-02  5.67E-04 -9.23E-02 -1.11E-01
          1.70E-01  2.28E-02
 
 OM66
+       -3.21E-02 -5.32E-02  2.51E-02 -9.40E-04 -6.78E-03 -2.90E-03 -8.21E-03 -4.46E-02  2.34E-02 -3.04E-02  2.68E-02 -4.11E-02
         -3.21E-02  8.27E-02 -2.24E-04  1.17E-02  2.49E-02 -4.57E-02 -5.79E-03 -2.98E-04  3.47E-02  9.97E-03  2.34E-02  5.79E-03
         4.93E-03 -1.52E-02  8.80E-02 -8.82E-03 -1.03E-02  3.84E-02 -4.14E-02  6.01E-02  4.66E-03 -4.14E-02 -2.12E-02 -2.92E-01
         -5.49E-02  1.84E-02  5.73E-02
 
 OM67
+       -2.26E-02 -7.34E-03 -4.10E-02  6.11E-02 -1.39E-02 -6.46E-02 -2.83E-02  5.29E-03  1.11E-02 -1.96E-03  4.47E-02  3.21E-02
         -5.25E-03  3.78E-02 -2.38E-02  5.44E-03 -1.75E-02 -6.67E-02  9.80E-04  2.15E-02 -1.66E-01  2.86E-02  6.05E-02  3.15E-02
        -3.61E-02 -1.24E-02  8.07E-02  5.27E-02 -1.20E-02 -3.20E-02  3.16E-02 -1.46E-01 -1.36E-02  6.23E-02 -2.98E-02  7.84E-02
         -1.74E-01 -8.21E-02  4.73E-02  2.68E-02
 
 OM68
+        2.94E-02 -2.40E-02  2.34E-02 -1.04E-02 -1.74E-02  3.17E-02  1.97E-02  1.28E-02  1.21E-02 -2.58E-02  1.04E-02  4.64E-02
         -5.22E-02  2.18E-01 -4.99E-03 -5.28E-02 -5.69E-02  1.70E-02  3.21E-03 -3.14E-02  2.00E-01 -3.77E-02 -8.64E-02  1.04E-02
         2.36E-02 -3.89E-02  1.42E-01  2.55E-02  4.61E-03 -3.99E-02  6.32E-03  1.29E-01 -9.58E-03  1.52E-02  3.19E-02  4.39E-02
         -4.13E-02 -1.77E-01 -2.78E-01  1.51E-01  2.65E-02
 
 OM77
+       -5.05E-02  1.53E-02  4.23E-02  8.72E-03  6.02E-03 -5.57E-05  5.39E-03  1.91E-02  8.04E-03  1.17E-02  2.70E-03 -6.56E-03
         -7.46E-03 -6.02E-03  1.04E-02  2.70E-05  1.04E-01 -3.44E-02  5.65E-02 -2.42E-02 -1.93E-02 -2.46E-01  2.38E-02  1.47E-02
         8.87E-03  3.16E-02  2.11E-02  8.41E-02  3.79E-02  3.64E-02 -3.64E-02 -3.35E-04 -2.36E-01 -4.82E-02  2.51E-02 -5.66E-02
          1.15E-01  5.09E-02  4.40E-02  5.33E-02 -1.29E-02  4.45E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        2.05E-02 -7.27E-02  8.39E-03 -2.55E-02 -4.55E-03  7.14E-03 -9.40E-03  2.86E-02  1.62E-02 -6.69E-02  1.05E-02 -4.09E-03
          4.55E-02 -2.29E-02  1.83E-01  4.29E-02  1.16E-02 -5.36E-03  2.65E-02 -1.57E-02  1.81E-02  1.28E-01 -4.89E-02  6.01E-02
        -6.45E-02  1.95E-02 -4.39E-02  1.30E-01  7.73E-02 -5.30E-02 -1.43E-02 -5.71E-03  5.13E-02 -1.12E-01 -2.11E-02 -3.94E-02
          1.70E-02  8.84E-02  5.64E-03 -1.37E-01 -1.82E-02  2.92E-01  2.52E-02
 
 OM88
+       -2.52E-02 -2.67E-03  3.72E-02  5.26E-02  1.91E-02  1.67E-02  3.30E-02  2.89E-02  7.11E-02  9.34E-02  1.01E-01  4.54E-02
          4.46E-03 -3.49E-02  4.52E-02  3.50E-01  3.85E-02  6.26E-02  7.75E-02 -1.95E-02 -7.96E-02  8.19E-02  3.45E-01  4.14E-02
         1.34E-03  3.77E-02 -6.30E-02  3.66E-02  2.55E-01  3.48E-02  1.19E-02 -2.44E-02 -5.22E-03  1.90E-01 -2.79E-02 -4.30E-03
          3.87E-02 -2.27E-02  6.09E-02 -1.76E-02 -2.55E-01  3.22E-02  2.87E-01  3.94E-02
 
 SG11
+        2.03E-02 -3.22E-02  4.41E-04 -2.88E-02 -4.87E-02  4.61E-02  5.14E-02 -5.88E-03 -3.27E-02 -2.45E-02 -1.71E-02  9.93E-03
         -3.73E-03 -1.01E-02 -3.98E-03  1.69E-04 -1.15E-02  1.63E-02  3.27E-02 -1.29E-02  1.87E-02 -4.67E-02 -8.97E-03 -4.51E-02
        -5.03E-03  2.98E-02  3.10E-03  2.72E-02  1.94E-02  2.19E-03 -1.94E-02  3.40E-03  6.66E-03  1.14E-02  3.63E-02 -2.73E-02
          6.66E-03  3.35E-02  8.52E-03  7.24E-03  6.14E-02  7.69E-02 -2.04E-03 -2.50E-02  6.56E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.57E-02 -2.93E-02 -3.40E-02 -3.38E-02 -5.55E-02  1.45E-02 -1.49E-02 -3.26E-02  2.38E-03 -3.91E-02  4.19E-02  4.66E-03
         -1.92E-02 -1.50E-02 -1.37E-02 -6.09E-02  1.27E-02 -1.21E-02  1.56E-04  3.35E-02 -2.39E-03  1.39E-02  9.47E-03  3.42E-02
        -5.44E-02  9.31E-03  2.88E-02  2.10E-02 -3.20E-03  1.13E-02 -4.07E-03  1.82E-02 -2.01E-02  4.53E-04  4.02E-02 -1.39E-03
          5.26E-03  3.71E-02 -9.65E-02 -9.56E-03  1.96E-02 -1.10E-02 -1.48E-02 -1.30E-02 -5.53E-02  0.00E+00  1.19E-03
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************           INVERSE COVARIANCE MATRIX OF ESTIMATE (From Sample Variance)         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 TH 1
+        2.12E+02
 
 TH 2
+        5.68E+01  2.80E+02
 
 TH 3
+       -1.41E+01  1.00E+01  3.92E+02
 
 TH 4
+       -1.04E+01 -2.46E+00 -1.15E+00  2.04E+02
 
 TH 5
+       -2.86E+01 -2.68E+01 -1.81E-01  9.35E+00  2.52E+02
 
 TH 6
+       -1.09E+01 -2.53E+01 -3.23E+01 -2.15E+01  3.68E+01  2.20E+02
 
 TH 7
+        2.06E+01  9.27E+01 -2.65E+01  5.62E+01 -3.43E+01 -3.89E+01  2.95E+02
 
 TH 8
+       -8.28E+01 -1.15E+02 -6.59E+01 -4.34E+01  3.56E+01  6.48E+01 -1.11E+02  3.65E+02
 
 OM11
+        1.76E+01 -1.91E+01  6.74E+00  1.24E+01 -6.28E+00  4.44E+00 -1.36E+01 -7.55E+00  4.08E+02
 
 OM12
+        3.97E+01 -3.44E+00  2.62E+01  7.72E+00 -1.18E+01  1.55E+01  6.24E+00 -5.65E+00  1.87E+02  1.70E+03
 
 OM13
+        3.33E-01 -1.11E+01 -6.85E+01  1.58E+01  1.58E+01  4.69E+01 -1.61E+01 -2.84E+00 -4.38E+01  7.94E+01  2.45E+03
 
 OM14
+       -1.02E+01 -3.63E+00  3.15E+00  9.88E+00  4.69E+00  1.63E+01 -1.21E+01  1.44E+01 -4.80E+01 -1.33E+01  2.19E+01  1.25E+03
 
 OM15
+        2.56E+01  2.33E+01  3.22E+01  1.98E+00 -3.95E+00 -1.59E+01 -2.01E+00 -1.37E+01 -8.33E+01 -1.32E+02 -5.03E+01  6.88E+01
          1.57E+03
 
 OM16
+        1.47E+01  2.68E-01  2.78E+01 -1.31E+01 -2.23E+01 -1.80E+01 -3.99E+00 -1.19E+01 -7.96E+01 -1.70E+02 -1.06E+02 -5.20E+01
          1.87E+02  1.31E+03
 
 OM17
+       -2.71E+00 -2.96E+01 -2.53E+01 -2.97E+00 -1.19E+01  2.30E+01 -1.25E+01  2.75E+01  5.19E+01  2.85E+02  2.61E+01  2.73E+02
         -2.27E+02 -2.00E+02  1.53E+03
 
 OM18
+       -2.08E+00  1.54E+01  6.80E-01 -2.50E+01  6.61E+00 -2.30E+00  4.12E+00 -1.50E+01 -3.18E+02 -5.70E+02 -1.89E+02 -1.78E+02
          1.72E+02  3.65E+02 -5.36E+02  2.10E+03
 
 OM22
+        2.27E+01  7.43E+01  1.17E+01 -8.79E+00 -2.45E+01 -1.57E+01  1.75E+01 -3.62E+01  5.79E+00  4.79E+02  4.26E+01  9.04E+00
         -3.02E+01 -3.96E+01  7.36E+01 -1.66E+02  8.32E+02
 
 OM23
+       -1.76E+01 -2.88E+01  1.03E+01  4.01E+00  3.06E+01 -1.71E+01 -1.82E+01 -1.08E+01 -4.71E+01 -1.27E+02  3.99E+02 -2.50E+00
         -1.44E+01  7.12E+01 -8.29E+01  2.05E+02  1.90E+02  3.47E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        3.30E+01  7.85E+01  4.21E-01  1.17E+01  4.17E+00 -1.67E+01  2.25E+01 -6.47E+00 -4.06E+00  4.55E+01 -2.82E+01  2.61E+02
          8.44E+01  7.12E-01  1.17E+02 -4.43E+01 -3.52E+00  5.72E+01  1.67E+03
 
 OM25
+       -2.85E+01  2.98E+01 -1.31E+01  3.81E+01 -2.12E+00  2.50E+01  7.98E+00  1.64E+00 -1.40E+01 -2.50E+02 -2.17E+01  5.32E+01
          3.64E+02  4.27E+01 -7.84E+01  1.02E+02 -2.41E+02 -5.73E+01 -1.55E+01  2.09E+03
 
 OM26
+       -2.59E+01 -4.08E+01 -1.23E+01 -2.25E-01  2.51E+01  3.26E+01 -1.62E+01  7.01E+01 -3.71E+00 -1.78E+02 -4.81E+01  3.08E+01
          8.61E+01  3.19E+02 -7.07E+01  2.06E+02 -3.14E+02 -1.34E+02 -8.07E+01  4.52E+02  1.92E+03
 
 OM27
+       -3.33E+01 -1.21E+02  1.71E+01 -3.84E+00  6.20E+00  2.09E+01 -4.95E+01  8.55E+01  1.49E+01  2.97E+02  5.58E+01  4.79E+01
         -8.93E+01 -1.15E+02  3.92E+02 -2.17E+02  5.69E+02 -3.01E+01  4.63E+02 -2.33E+02 -3.60E+02  2.18E+03
 
 OM28
+       -1.72E+01  6.34E+00  7.13E+00  1.42E+01  3.44E+01  3.10E+01  3.12E+01  1.98E+01 -1.73E+01 -7.13E+02 -1.33E+02 -2.08E+00
          9.79E+01  1.57E+02 -2.36E+02  5.90E+02 -8.50E+02 -3.30E+02 -3.21E+02  2.46E+02  7.06E+02 -8.95E+02  3.20E+03
 
 OM33
+       -2.25E+01 -2.07E+01  5.76E+01  9.37E+00  5.59E+00 -2.19E+01 -6.63E+00  1.64E+01 -1.09E+01 -6.08E+01 -1.30E+02 -3.88E+00
          4.67E+00  2.78E+01 -2.36E+01  1.01E+02 -2.19E+01  1.97E+02  5.48E+01  6.21E+01 -8.57E+01 -3.39E+00  4.96E+01  1.35E+03
 
 OM34
+        6.30E+01 -4.58E+01 -1.13E+02 -2.20E+01 -1.61E+01 -1.67E+00 -3.16E+01  3.89E+01  4.75E+01 -1.20E+02 -1.62E+02 -3.20E+01
         -2.36E+01  2.62E+01 -6.29E+01  6.93E+01 -8.99E+01 -1.37E+02  1.09E+02 -2.34E+01  4.57E+01 -7.79E+01  6.56E+01  1.32E+02
         2.34E+03
 
 OM35
+       -2.99E+01 -2.08E+01  1.12E+01 -1.35E+01 -5.90E+01 -2.84E+00  1.30E+01  6.35E+00  5.79E+01  1.90E+01 -1.83E+02  5.76E+01
         -1.22E+01 -2.78E+01  2.23E+01 -4.68E+01 -3.79E+01 -6.03E+02 -6.12E+01  1.01E+02  9.79E+01  3.69E+01  8.61E+01 -1.75E+02
         1.61E+02  3.39E+03
 
 OM36
+       -2.41E+01 -3.80E+00  3.79E+01 -2.49E+01  1.71E+00  7.33E+00  3.91E+00  1.09E+01  6.68E+01  5.81E+01 -2.43E+02  2.85E+01
         -7.13E+01 -7.04E+01  1.07E+02 -7.18E+01 -7.39E+01 -2.99E+02  8.64E+01 -2.15E+01  1.97E+02  1.02E+01  1.63E+02 -2.32E+02
        -1.24E+02  6.54E+02  2.63E+03
 
 OM37
+        1.44E+01 -3.13E+01  3.76E+01  5.69E+01 -4.61E+01 -3.08E+01 -3.30E+01 -1.90E+01  8.49E-01  1.53E+01  2.78E+02 -9.48E+00
         -6.05E+01  7.01E+01 -3.10E+01  2.90E+01  5.25E+01  6.68E+02  1.73E+01 -6.78E+01  2.32E+01  3.41E+01 -9.01E+01 -9.27E+01
         5.63E+02 -1.78E+02 -3.05E+02  3.07E+03
 
 OM38
+        1.03E+01  2.44E+01  1.77E+01 -1.51E+01 -1.67E+01  2.34E-01  4.35E+01 -2.68E+01  4.69E+01 -1.81E+01 -8.23E+02 -6.29E+01
          7.57E+01 -7.00E+00 -3.43E+00 -1.83E+02 -8.62E+01 -9.89E+02 -1.05E+02  1.94E+00  8.84E+01  3.93E+00  3.12E+02 -6.19E+02
        -5.11E+02  5.26E+02  7.48E+02 -9.51E+02  4.12E+03
 
 OM44
+       -2.11E+01  2.68E+00 -4.52E+01 -1.59E+01 -2.47E+00 -9.51E+00  5.40E+00  1.18E+01 -1.97E+00 -1.02E+01  2.71E+00 -5.55E+01
         -1.24E+01 -3.08E+01 -1.47E+01  2.08E+01 -1.57E+01 -2.03E+01 -6.78E+01 -9.07E+00 -2.58E+01 -1.67E+01  1.28E+01  5.54E+01
        -3.58E+01  1.05E+01  8.69E+00 -4.42E+01 -1.41E+00  3.59E+02
 
 OM45
+        2.85E+01 -1.34E+01 -3.96E-01 -2.24E+01 -2.36E+01 -1.06E+01 -4.18E+01 -1.82E+01  9.12E+00  2.10E+01  6.35E+01 -1.46E+02
         -6.97E+01 -4.38E+01 -3.05E+00 -7.77E+01  2.56E+01 -1.54E+02 -1.83E+02 -1.59E+02 -7.34E+01 -4.17E+01  2.03E+00  2.03E+01
         1.87E+01  5.86E+01 -8.82E+00 -2.27E+01 -2.25E+01  4.67E+01  1.71E+03
 
 OM46
+       -3.05E+00 -2.20E+01  6.75E+01 -2.12E+00  1.75E+00 -1.96E+01  1.76E+01 -1.09E+01  1.45E+00 -6.93E+01  3.34E+00 -1.16E+02
         -3.92E+01 -5.50E+01 -6.71E+01  3.23E+00 -5.21E+01  7.70E+01 -1.43E+02 -9.82E+01  3.90E+01 -6.31E+01  1.33E+02 -3.10E+01
        -3.60E+01  5.39E+01  7.89E+01 -3.95E+01  8.94E+01 -8.01E+01  2.68E+02  1.41E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.80E+01  3.96E+01 -2.22E+01  2.19E+01 -1.94E+00 -3.79E+01  4.92E+01 -1.98E+01  3.20E+00  1.03E+02  1.19E+01  8.60E+01
          2.02E+01 -1.75E+01  1.34E+01 -8.74E+01  5.72E+01  6.55E+01  3.56E+02 -3.79E+01 -1.51E+01  6.72E+01 -7.99E+01  2.72E+01
        -1.83E+01 -1.17E+02  7.25E+01 -1.07E+01  2.64E+01  1.91E+02 -1.74E+02 -1.72E+02  1.49E+03
 
 OM48
+        4.33E+00 -6.03E+01 -8.79E-01 -4.46E+00  2.73E+00  5.96E+00  9.42E+00 -1.21E-01  1.51E+00 -2.99E+01 -2.07E+01 -4.65E+02
         -6.09E+01 -3.15E+01 -1.57E+02 -6.58E+01 -6.42E+01 -5.65E+01 -6.19E+02 -6.30E+01  2.16E+01 -2.48E+02  1.08E+02 -1.51E+02
        -2.59E+02  5.25E+01  7.47E+01 -4.66E+01  2.10E+02 -7.74E+01  1.51E+02  4.20E+02 -4.03E+02  2.07E+03
 
 OM55
+        4.54E+00  7.38E+00 -2.29E+01  8.91E+00  4.77E+00 -5.83E-01  1.83E+01 -1.24E+01 -1.10E+01 -1.42E+00 -2.38E+01 -5.11E+01
         -1.52E+02 -4.91E+00 -5.08E+01 -2.79E+01  1.23E+01  1.01E+01  2.42E+01 -2.19E+02 -9.19E+01  4.36E+01  2.28E+01  1.48E+00
        -2.40E+01 -1.05E+02 -3.24E+01  3.46E+01  2.04E+00  2.96E+01  7.35E+01  7.32E-01  2.38E+00  1.21E+01  7.28E+02
 
 OM56
+       -2.35E+01 -2.40E+01 -3.87E+01  1.51E+01 -7.99E+00  3.77E+01  3.62E+01  3.11E+01 -1.90E+01 -3.77E+00  6.75E+01 -5.15E+01
         -1.61E+02 -9.19E+01  3.87E+01 -8.42E+01  6.89E+00  1.36E+02 -4.42E+01 -2.88E+02 -1.11E+02  6.86E+01  2.06E+01 -4.47E+00
        -3.47E+01 -1.80E+02 -1.49E+02  4.69E+01  6.58E+00 -3.63E+00 -3.10E+01  9.94E+01 -2.09E+00  6.00E+01  2.88E+02  1.71E+03
 
 OM57
+        8.23E+00  3.57E+01 -1.66E+01  3.18E+00 -2.43E+01 -3.18E+01  2.40E+01 -2.23E+01 -3.34E+00  7.29E+01  3.99E+01 -6.95E+01
          1.33E+02  4.20E+01 -9.18E+01 -2.68E+01 -2.31E+01 -6.17E+01  1.61E+01  3.51E+02  6.74E+01 -1.55E+02 -1.14E+01 -1.89E+01
        -6.96E+01 -4.06E+01 -1.81E+00 -2.60E+01  2.07E+01  9.52E+00  3.68E+02  1.03E+02  5.73E+01  6.90E+01 -1.68E+02 -2.78E+02
          1.98E+03
 
 OM58
+       -4.23E+01 -3.71E+01 -4.27E+01  8.28E+00  1.35E+01  8.86E-01  1.73E+00  3.54E+01 -1.45E+01  3.43E+01  3.36E+01  3.80E+01
         -5.55E+02 -1.44E+02  6.64E+01 -2.41E+02  1.78E+02  1.57E+02 -4.38E+01 -6.00E+02 -2.31E+02  1.79E+02 -3.73E+02 -1.62E+01
         6.92E+00 -3.49E+02 -7.43E+01 -1.60E+01 -1.18E+02 -1.67E+01 -1.96E+02 -2.51E+00 -1.17E+01 -4.24E+01  2.71E+02  4.44E+02
         -5.53E+02  2.63E+03
 
 OM66
+        5.90E+00  1.94E+01 -2.64E+01  6.19E+00  7.71E-01  3.65E+00  1.08E+01  8.85E+00 -1.26E+01  4.61E+01  8.85E+00  1.44E+01
          8.25E-01 -1.31E+02  2.07E+01 -4.91E+01  3.55E+01  5.35E+01  6.79E+00 -4.91E+01 -1.58E+02  2.05E+01 -6.84E+01  1.87E-01
        -1.64E+01 -4.76E+01 -1.62E+02  1.90E+01 -2.96E+01 -5.52E+00  2.14E+01 -6.40E+01 -6.58E+00  1.42E+01  4.92E+01  2.65E+02
          4.82E+00  8.85E+01  4.16E+02
 
 OM67
+        1.48E+01  2.89E+01  3.53E+01 -3.86E+01  1.08E+01  2.70E+01  2.09E+01 -1.38E+01 -1.80E+00  1.30E+00 -8.77E+01 -6.09E+01
         -3.15E-01  9.64E+01 -9.30E+01  6.85E+01 -4.02E+01  1.01E+02 -5.19E+01  6.85E+01  3.94E+02 -2.49E+02  7.32E+01 -5.68E+01
         6.34E+01  1.10E+01  1.32E+00 -1.21E+02  8.52E+01  6.94E+00  4.98E+01  2.90E+02 -3.71E+01  4.43E+01 -2.10E+01 -1.88E+02
          3.56E+02 -5.55E+01 -1.28E+02  1.77E+03
 
 OM68
+       -1.02E+01  3.61E+01 -6.58E+01  1.19E+01 -2.44E-02 -2.69E+01 -3.37E+00 -8.43E+00 -1.32E+01  1.66E+02  5.45E+01  1.34E+01
         -3.58E+01 -5.07E+02  1.25E+02 -2.68E+02  2.47E+02  4.85E+00  5.31E+01 -1.24E+02 -7.06E+02  3.10E+02 -4.93E+02  4.85E+01
        -5.48E+01 -1.11E+02 -4.42E+02 -1.94E+01 -2.81E+02  5.64E+01 -5.05E+01 -3.53E+02  1.05E+02 -2.21E+02  4.52E+01  1.85E+02
         -9.83E+01  5.51E+02  3.67E+02 -5.29E+02  2.38E+03
 
 OM77
+        6.40E+00 -4.68E+01 -2.38E+01 -2.74E+00 -8.50E-01  4.78E+00 -1.50E+01  1.89E+01 -5.33E+00 -1.08E+01  3.23E+01  3.72E+01
          6.51E+00 -2.50E+01  1.27E+02 -5.46E+01  3.48E+01  4.26E+01  1.09E+02 -1.78E+01 -4.53E+01  4.05E+02 -1.73E+02  1.41E+01
        -3.76E+01 -6.65E+01 -3.87E+01 -3.88E+01 -4.70E+01  8.42E+00 -4.04E+01 -5.34E+01  2.50E+02 -9.37E+01 -4.83E+00  6.63E+01
         -1.65E+02  3.39E+01 -5.46E+00 -1.83E+02  1.09E+02  7.17E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        8.38E+00  1.10E+02  1.77E+01  1.79E+01  1.12E+01 -3.01E+00  7.65E+01 -7.54E+01 -8.69E+00 -6.61E+01 -5.70E+01 -2.01E+02
          2.13E+01  1.26E+02 -5.37E+02  3.63E+02 -2.27E+02 -7.98E+01 -3.37E+02  1.08E+02  2.14E+02 -8.52E+02  7.97E+02 -9.09E+01
         5.14E+01  9.87E+01  1.32E+02 -2.76E+02  2.07E+02  3.09E+01  1.27E+02  2.15E+02 -2.98E+02  5.44E+02  2.51E+01 -3.35E+01
          2.37E+02 -3.63E+02 -3.03E+01  4.79E+02 -4.80E+02 -5.80E+02  2.72E+03
 
 OM88
+        8.56E+00 -6.30E+00 -2.81E+01 -2.12E+01 -1.55E+01 -1.74E+01 -3.70E+01  4.16E+00  2.71E+01  1.34E+02  2.75E+01  8.22E+01
         -5.34E+01 -1.26E+02  1.97E+02 -5.90E+02  1.87E+02  2.13E+01  9.94E+01 -3.91E+01 -1.80E+02  2.22E+02 -8.71E+02  3.42E+01
         6.66E+01 -1.19E+02 -1.01E+02  8.10E+01 -4.95E+02 -3.92E+00 -3.50E+01 -1.36E+02  1.03E+02 -3.41E+02  1.91E+01  1.99E+01
         -9.24E+01  3.20E+02  3.53E+01 -1.38E+02  5.75E+02  1.28E+02 -7.77E+02  1.26E+03
 
 SG11
+       -5.50E+02  1.49E+02  7.83E+02  5.04E+02  1.28E+03 -6.94E+02 -1.29E+03  3.85E+02  1.29E+03  2.58E+03  1.02E+03 -9.24E+02
          6.57E+02  1.07E+03  3.17E+02 -1.01E+03  1.46E+03 -8.64E+02 -1.71E+03  1.06E+03 -2.34E+02  1.51E+03 -9.57E+02  3.21E+03
         4.83E+02 -2.38E+03 -5.90E+02 -1.54E+03 -3.24E+03 -7.75E+01  1.80E+03  5.06E+02 -1.77E+03 -5.07E+02 -1.70E+03 -3.12E+02
          1.28E+03 -3.97E+03 -6.91E+02  4.27E+02 -4.44E+03 -3.16E+03  2.04E+03  3.79E+02  2.42E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -1.39E+02  3.96E+02  3.91E+02  3.76E+02  7.83E+02 -1.57E+02  2.05E+02  2.80E+02 -3.64E+02  7.88E+02 -2.17E+03 -2.25E+02
          1.35E+03  7.31E+02 -4.46E+02  2.60E+03 -1.09E+02  3.68E+02  3.48E+02 -5.59E+02  1.86E+02 -1.05E+03  2.32E+02 -4.56E+02
         1.96E+03 -6.95E+02 -1.62E+03 -4.08E+02  2.14E+01 -2.42E+02 -2.11E+02 -7.03E+02  6.61E+02 -4.88E+02 -9.80E+02  4.18E+02
          3.52E+02 -2.24E+03  1.67E+03  3.54E+02 -8.81E+01 -2.09E+02  1.42E+03 -7.40E+02  8.11E+04  0.00E+00  7.37E+05
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     7855.025
Stop Time: 
Thu 11/03/2016 
02:53 AM

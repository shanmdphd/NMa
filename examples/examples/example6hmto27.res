Thu 11/03/2016 
03:47 PM
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

$EST METHOD=NUTS AUTO=1 PRINT=5 OLKJDF=8.0 NUTS_REG=1.0
  
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
 RAW OUTPUT FILE (FILE): example6hmto27.ext
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
 AUTOMATIC SETTING FEATURE (AUTO):          ON
 CONVERGENCE TYPE (CTYPE):                  0
 KEEP ITERATIONS (THIN):            1
 BURN-IN ITERATIONS (NBURN):                10000
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
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       75.0000000000000
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): -3.00000000000000
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 50.0000000000000
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
 iteration         -517 MCMCOBJ=    181560.900376746     
 iteration         -515 MCMCOBJ=    67581.2988953633     
 iteration         -510 MCMCOBJ=    5962.79487405312     
 iteration         -505 MCMCOBJ=   -3747.21554545445     
 iteration         -500 MCMCOBJ=   -6280.59419252772     
 iteration         -495 MCMCOBJ=   -6585.00937490002     
 iteration         -490 MCMCOBJ=   -6564.64400311897     
 iteration         -485 MCMCOBJ=   -6531.70666447053     
 iteration         -480 MCMCOBJ=   -6551.79470151691     
 iteration         -475 MCMCOBJ=   -6601.29845774028     
 iteration         -470 MCMCOBJ=   -6545.14698850820     
 iteration         -465 MCMCOBJ=   -6586.95169143315     
 iteration         -460 MCMCOBJ=   -6621.43638967553     
 iteration         -455 MCMCOBJ=   -6584.17658144657     
 iteration         -450 MCMCOBJ=   -6568.66932464071     
 iteration         -445 MCMCOBJ=   -6613.24893046720     
 iteration         -440 MCMCOBJ=   -6611.98567230773     
 iteration         -435 MCMCOBJ=   -6570.37868767375     
 iteration         -430 MCMCOBJ=   -6648.91438136308     
 iteration         -425 MCMCOBJ=   -6585.39087107343     
 iteration         -420 MCMCOBJ=   -6603.87949354053     
 iteration         -415 MCMCOBJ=   -6609.43001547722     
 iteration         -410 MCMCOBJ=   -6599.81843190029     
 iteration         -405 MCMCOBJ=   -6569.52335823057     
 iteration         -400 MCMCOBJ=   -6571.74469227119     
 iteration         -395 MCMCOBJ=   -6606.11218614068     
 iteration         -390 MCMCOBJ=   -6607.82264768311     
 iteration         -385 MCMCOBJ=   -6642.49270831034     
 iteration         -380 MCMCOBJ=   -6625.50470133392     
 iteration         -375 MCMCOBJ=   -6581.21841540362     
 iteration         -370 MCMCOBJ=   -6585.08770770094     
 iteration         -365 MCMCOBJ=   -6540.94076345243     
 iteration         -360 MCMCOBJ=   -6604.10352456308     
 iteration         -355 MCMCOBJ=   -6654.00273753914     
 iteration         -350 MCMCOBJ=   -6630.86135859099     
 iteration         -345 MCMCOBJ=   -6619.59629285547     
 iteration         -340 MCMCOBJ=   -6637.06027928000     
 iteration         -335 MCMCOBJ=   -6590.32027039883     
 iteration         -330 MCMCOBJ=   -6568.61085507633     
 iteration         -325 MCMCOBJ=   -6591.86732495654     
 iteration         -320 MCMCOBJ=   -6605.67893325028     
 iteration         -315 MCMCOBJ=   -6557.55275461210     
 iteration         -310 MCMCOBJ=   -6601.71382369869     
 iteration         -305 MCMCOBJ=   -6643.65767678812     
 iteration         -300 MCMCOBJ=   -6571.25611059216     
 iteration         -295 MCMCOBJ=   -6624.20331257147     
 iteration         -290 MCMCOBJ=   -6573.76429647648     
 iteration         -285 MCMCOBJ=   -6596.74993515486     
 iteration         -280 MCMCOBJ=   -6581.97214480417     
 iteration         -275 MCMCOBJ=   -6595.85979766543     
 iteration         -270 MCMCOBJ=   -6686.83925084763     
 iteration         -265 MCMCOBJ=   -6590.01468772229     
 iteration         -260 MCMCOBJ=   -6541.12174441876     
 iteration         -255 MCMCOBJ=   -6622.80905838581     
 iteration         -250 MCMCOBJ=   -6575.83916342650     
 iteration         -245 MCMCOBJ=   -6641.29579252298     
 iteration         -240 MCMCOBJ=   -6604.97509509613     
 iteration         -235 MCMCOBJ=   -6535.17536203548     
 iteration         -230 MCMCOBJ=   -6611.82704692082     
 iteration         -225 MCMCOBJ=   -6594.87163281946     
 iteration         -220 MCMCOBJ=   -6563.34512925595     
 iteration         -215 MCMCOBJ=   -6588.71001656419     
 iteration         -210 MCMCOBJ=   -6616.06503152660     
 iteration         -205 MCMCOBJ=   -6619.27819038980     
 iteration         -200 MCMCOBJ=   -6578.06617506431     
 iteration         -195 MCMCOBJ=   -6644.22638258956     
 iteration         -190 MCMCOBJ=   -6698.05659846142     
 iteration         -185 MCMCOBJ=   -6608.07671693955     
 iteration         -180 MCMCOBJ=   -6562.08686884801     
 iteration         -175 MCMCOBJ=   -6635.59485865215     
 iteration         -170 MCMCOBJ=   -6623.76426996030     
 iteration         -165 MCMCOBJ=   -6604.34347231240     
 iteration         -160 MCMCOBJ=   -6622.32445383806     
 iteration         -155 MCMCOBJ=   -6547.25237926490     
 iteration         -150 MCMCOBJ=   -6648.65313583777     
 iteration         -145 MCMCOBJ=   -6607.12690174045     
 iteration         -140 MCMCOBJ=   -6562.06857995300     
 iteration         -135 MCMCOBJ=   -6575.24124373323     
 iteration         -130 MCMCOBJ=   -6671.51368424422     
 iteration         -125 MCMCOBJ=   -6687.04145586139     
 iteration         -120 MCMCOBJ=   -6663.53821666960     
 iteration         -115 MCMCOBJ=   -6581.90864444193     
 iteration         -110 MCMCOBJ=   -6594.97257845881     
 iteration         -105 MCMCOBJ=   -6544.98871775729     
 iteration         -100 MCMCOBJ=   -6616.75613584417     
 iteration          -95 MCMCOBJ=   -6589.35092747018     
 iteration          -90 MCMCOBJ=   -6599.20787776871     
 iteration          -85 MCMCOBJ=   -6586.83075220336     
 iteration          -80 MCMCOBJ=   -6669.36545997794     
 iteration          -75 MCMCOBJ=   -6614.06088699159     
 iteration          -70 MCMCOBJ=   -6614.91047295991     
 iteration          -65 MCMCOBJ=   -6595.98249678495     
 iteration          -60 MCMCOBJ=   -6614.92700584418     
 iteration          -55 MCMCOBJ=   -6559.60402863044     
 iteration          -50 MCMCOBJ=   -6630.22231474241     
 iteration          -45 MCMCOBJ=   -6638.07000263073     
 iteration          -40 MCMCOBJ=   -6637.76928617000     
 iteration          -35 MCMCOBJ=   -6563.40096055686     
 iteration          -30 MCMCOBJ=   -6639.49202800949     
 iteration          -25 MCMCOBJ=   -6563.96250851762     
 iteration          -20 MCMCOBJ=   -6608.79453940324     
 iteration          -15 MCMCOBJ=   -6624.97987854263     
 iteration          -10 MCMCOBJ=   -6590.52238504361     
 iteration           -5 MCMCOBJ=   -6564.78506956511     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6529.97505502808     
 iteration            5 MCMCOBJ=   -6548.82363347029     
 iteration           10 MCMCOBJ=   -6623.15611208684     
 iteration           15 MCMCOBJ=   -6657.35406859929     
 iteration           20 MCMCOBJ=   -6568.83479610995     
 iteration           25 MCMCOBJ=   -6551.56388865700     
 iteration           30 MCMCOBJ=   -6572.62302149754     
 iteration           35 MCMCOBJ=   -6541.43689279898     
 iteration           40 MCMCOBJ=   -6606.08852111332     
 iteration           45 MCMCOBJ=   -6596.63723922211     
 iteration           50 MCMCOBJ=   -6637.51274209020     
 iteration           55 MCMCOBJ=   -6630.36366063359     
 iteration           60 MCMCOBJ=   -6598.53585877377     
 iteration           65 MCMCOBJ=   -6588.92079568584     
 iteration           70 MCMCOBJ=   -6672.74427847351     
 iteration           75 MCMCOBJ=   -6640.77293179128     
 iteration           80 MCMCOBJ=   -6599.10798961760     
 iteration           85 MCMCOBJ=   -6557.54041900944     
 iteration           90 MCMCOBJ=   -6624.33812135286     
 iteration           95 MCMCOBJ=   -6567.17653730248     
 iteration          100 MCMCOBJ=   -6621.74969617097     
 iteration          105 MCMCOBJ=   -6612.83759670987     
 iteration          110 MCMCOBJ=   -6667.83786410297     
 iteration          115 MCMCOBJ=   -6541.52753245299     
 iteration          120 MCMCOBJ=   -6614.19303792475     
 iteration          125 MCMCOBJ=   -6569.73058199863     
 iteration          130 MCMCOBJ=   -6556.96949997682     
 iteration          135 MCMCOBJ=   -6604.40482183147     
 iteration          140 MCMCOBJ=   -6620.42974171273     
 iteration          145 MCMCOBJ=   -6597.50548008995     
 iteration          150 MCMCOBJ=   -6608.42533275362     
 iteration          155 MCMCOBJ=   -6589.09999204027     
 iteration          160 MCMCOBJ=   -6620.75372120627     
 iteration          165 MCMCOBJ=   -6549.24468015020     
 iteration          170 MCMCOBJ=   -6558.82994319592     
 iteration          175 MCMCOBJ=   -6633.06786161820     
 iteration          180 MCMCOBJ=   -6588.89118134631     
 iteration          185 MCMCOBJ=   -6561.91647373877     
 iteration          190 MCMCOBJ=   -6638.43501945144     
 iteration          195 MCMCOBJ=   -6661.88969407337     
 iteration          200 MCMCOBJ=   -6613.24008024797     
 iteration          205 MCMCOBJ=   -6606.22528702017     
 iteration          210 MCMCOBJ=   -6630.06745434338     
 iteration          215 MCMCOBJ=   -6631.53431784487     
 iteration          220 MCMCOBJ=   -6558.27812040324     
 iteration          225 MCMCOBJ=   -6597.05290554026     
 iteration          230 MCMCOBJ=   -6611.48221220004     
 iteration          235 MCMCOBJ=   -6579.13318632003     
 iteration          240 MCMCOBJ=   -6623.36120251014     
 iteration          245 MCMCOBJ=   -6610.39481726411     
 iteration          250 MCMCOBJ=   -6608.28161105371     
 iteration          255 MCMCOBJ=   -6623.74749269405     
 iteration          260 MCMCOBJ=   -6557.93761610789     
 iteration          265 MCMCOBJ=   -6567.33608324211     
 iteration          270 MCMCOBJ=   -6573.85166377311     
 iteration          275 MCMCOBJ=   -6572.12993085681     
 iteration          280 MCMCOBJ=   -6628.57244518076     
 iteration          285 MCMCOBJ=   -6624.08901995755     
 iteration          290 MCMCOBJ=   -6636.70499124764     
 iteration          295 MCMCOBJ=   -6595.10255642056     
 iteration          300 MCMCOBJ=   -6615.01071096205     
 iteration          305 MCMCOBJ=   -6568.75570040393     
 iteration          310 MCMCOBJ=   -6585.38135302319     
 iteration          315 MCMCOBJ=   -6647.13577650784     
 iteration          320 MCMCOBJ=   -6570.73322034528     
 iteration          325 MCMCOBJ=   -6631.94949370749     
 iteration          330 MCMCOBJ=   -6541.29213792816     
 iteration          335 MCMCOBJ=   -6578.99938394722     
 iteration          340 MCMCOBJ=   -6627.53485177021     
 iteration          345 MCMCOBJ=   -6554.66488401199     
 iteration          350 MCMCOBJ=   -6638.34633892219     
 iteration          355 MCMCOBJ=   -6598.89487999485     
 iteration          360 MCMCOBJ=   -6579.51228974514     
 iteration          365 MCMCOBJ=   -6597.44272699713     
 iteration          370 MCMCOBJ=   -6520.90623962131     
 iteration          375 MCMCOBJ=   -6644.45922627100     
 iteration          380 MCMCOBJ=   -6539.61397044204     
 iteration          385 MCMCOBJ=   -6575.14420749528     
 iteration          390 MCMCOBJ=   -6546.02955358884     
 iteration          395 MCMCOBJ=   -6676.06393991575     
 iteration          400 MCMCOBJ=   -6604.68363578879     
 iteration          405 MCMCOBJ=   -6616.05998850732     
 iteration          410 MCMCOBJ=   -6624.47125917424     
 iteration          415 MCMCOBJ=   -6581.08580036369     
 iteration          420 MCMCOBJ=   -6583.67840101170     
 iteration          425 MCMCOBJ=   -6604.13690695144     
 iteration          430 MCMCOBJ=   -6603.14026972181     
 iteration          435 MCMCOBJ=   -6570.35897204913     
 iteration          440 MCMCOBJ=   -6580.12992421364     
 iteration          445 MCMCOBJ=   -6565.04100714838     
 iteration          450 MCMCOBJ=   -6553.54085759993     
 iteration          455 MCMCOBJ=   -6636.93219567564     
 iteration          460 MCMCOBJ=   -6671.57077972256     
 iteration          465 MCMCOBJ=   -6633.00109221785     
 iteration          470 MCMCOBJ=   -6603.84318886041     
 iteration          475 MCMCOBJ=   -6565.32041646159     
 iteration          480 MCMCOBJ=   -6603.50862666279     
 iteration          485 MCMCOBJ=   -6612.40035816029     
 iteration          490 MCMCOBJ=   -6618.58550757841     
 iteration          495 MCMCOBJ=   -6606.53733428366     
 iteration          500 MCMCOBJ=   -6579.38605163752     
 iteration          505 MCMCOBJ=   -6622.97894627758     
 iteration          510 MCMCOBJ=   -6558.47131351968     
 iteration          515 MCMCOBJ=   -6572.86029030105     
 iteration          520 MCMCOBJ=   -6629.45450800508     
 iteration          525 MCMCOBJ=   -6630.38006026417     
 iteration          530 MCMCOBJ=   -6523.37490110555     
 iteration          535 MCMCOBJ=   -6648.83401469097     
 iteration          540 MCMCOBJ=   -6629.14819055840     
 iteration          545 MCMCOBJ=   -6610.11911895880     
 iteration          550 MCMCOBJ=   -6595.61949390273     
 iteration          555 MCMCOBJ=   -6639.03464198690     
 iteration          560 MCMCOBJ=   -6538.79990841186     
 iteration          565 MCMCOBJ=   -6602.19237905421     
 iteration          570 MCMCOBJ=   -6598.06410202038     
 iteration          575 MCMCOBJ=   -6609.18756862833     
 iteration          580 MCMCOBJ=   -6584.83489469081     
 iteration          585 MCMCOBJ=   -6554.18707436297     
 iteration          590 MCMCOBJ=   -6653.07954326983     
 iteration          595 MCMCOBJ=   -6578.11455732730     
 iteration          600 MCMCOBJ=   -6643.94804510745     
 iteration          605 MCMCOBJ=   -6608.09251299409     
 iteration          610 MCMCOBJ=   -6626.81174136572     
 iteration          615 MCMCOBJ=   -6571.87913805340     
 iteration          620 MCMCOBJ=   -6611.58238760079     
 iteration          625 MCMCOBJ=   -6594.74076955440     
 iteration          630 MCMCOBJ=   -6597.39337569827     
 iteration          635 MCMCOBJ=   -6692.49977471539     
 iteration          640 MCMCOBJ=   -6565.80848027980     
 iteration          645 MCMCOBJ=   -6598.11626721894     
 iteration          650 MCMCOBJ=   -6554.34899033321     
 iteration          655 MCMCOBJ=   -6611.96283672422     
 iteration          660 MCMCOBJ=   -6620.28828875889     
 iteration          665 MCMCOBJ=   -6629.51377129064     
 iteration          670 MCMCOBJ=   -6517.30611177841     
 iteration          675 MCMCOBJ=   -6588.20605555097     
 iteration          680 MCMCOBJ=   -6604.37534876142     
 iteration          685 MCMCOBJ=   -6617.64114321907     
 iteration          690 MCMCOBJ=   -6575.09386171400     
 iteration          695 MCMCOBJ=   -6626.50240966879     
 iteration          700 MCMCOBJ=   -6640.96083192291     
 iteration          705 MCMCOBJ=   -6596.57651304763     
 iteration          710 MCMCOBJ=   -6647.43221953546     
 iteration          715 MCMCOBJ=   -6583.26814952506     
 iteration          720 MCMCOBJ=   -6645.04633665842     
 iteration          725 MCMCOBJ=   -6612.08281838103     
 iteration          730 MCMCOBJ=   -6585.30930126184     
 iteration          735 MCMCOBJ=   -6605.94138938049     
 iteration          740 MCMCOBJ=   -6597.84183090827     
 iteration          745 MCMCOBJ=   -6636.35386726744     
 iteration          750 MCMCOBJ=   -6600.17665266526     
 iteration          755 MCMCOBJ=   -6585.09361690924     
 iteration          760 MCMCOBJ=   -6639.55665551588     
 iteration          765 MCMCOBJ=   -6598.39087516139     
 iteration          770 MCMCOBJ=   -6543.81779784763     
 iteration          775 MCMCOBJ=   -6632.32760696909     
 iteration          780 MCMCOBJ=   -6624.18433229116     
 iteration          785 MCMCOBJ=   -6527.16780976684     
 iteration          790 MCMCOBJ=   -6500.64763947762     
 iteration          795 MCMCOBJ=   -6565.09687364612     
 iteration          800 MCMCOBJ=   -6588.14198599833     
 iteration          805 MCMCOBJ=   -6609.44463937392     
 iteration          810 MCMCOBJ=   -6574.86905680525     
 iteration          815 MCMCOBJ=   -6570.97493162916     
 iteration          820 MCMCOBJ=   -6621.73686118508     
 iteration          825 MCMCOBJ=   -6603.52102590390     
 iteration          830 MCMCOBJ=   -6576.03132238386     
 iteration          835 MCMCOBJ=   -6574.68043388468     
 iteration          840 MCMCOBJ=   -6605.97006047818     
 iteration          845 MCMCOBJ=   -6567.01750540092     
 iteration          850 MCMCOBJ=   -6618.26395767755     
 iteration          855 MCMCOBJ=   -6560.16850113998     
 iteration          860 MCMCOBJ=   -6572.22410805891     
 iteration          865 MCMCOBJ=   -6655.47743975893     
 iteration          870 MCMCOBJ=   -6588.03809604604     
 iteration          875 MCMCOBJ=   -6603.15696688361     
 iteration          880 MCMCOBJ=   -6623.89894710177     
 iteration          885 MCMCOBJ=   -6607.91333600132     
 iteration          890 MCMCOBJ=   -6631.41913029078     
 iteration          895 MCMCOBJ=   -6600.30147312509     
 iteration          900 MCMCOBJ=   -6601.55630218883     
 iteration          905 MCMCOBJ=   -6626.90720873901     
 iteration          910 MCMCOBJ=   -6642.75481828892     
 iteration          915 MCMCOBJ=   -6657.41120618835     
 iteration          920 MCMCOBJ=   -6621.79541870050     
 iteration          925 MCMCOBJ=   -6574.22225657635     
 iteration          930 MCMCOBJ=   -6623.85366138197     
 iteration          935 MCMCOBJ=   -6594.97305288937     
 iteration          940 MCMCOBJ=   -6651.37889565008     
 iteration          945 MCMCOBJ=   -6537.69857664414     
 iteration          950 MCMCOBJ=   -6606.04787037225     
 iteration          955 MCMCOBJ=   -6613.39723361315     
 iteration          960 MCMCOBJ=   -6624.01061849890     
 iteration          965 MCMCOBJ=   -6591.93494754500     
 iteration          970 MCMCOBJ=   -6640.29515971970     
 iteration          975 MCMCOBJ=   -6562.30145525255     
 iteration          980 MCMCOBJ=   -6629.68609621549     
 iteration          985 MCMCOBJ=   -6510.38846923921     
 iteration          990 MCMCOBJ=   -6600.70149359324     
 iteration          995 MCMCOBJ=   -6516.44103623492     
 iteration         1000 MCMCOBJ=   -6617.49413929052     
 iteration         1005 MCMCOBJ=   -6585.58758880650     
 iteration         1010 MCMCOBJ=   -6664.19105935370     
 iteration         1015 MCMCOBJ=   -6639.31585752256     
 iteration         1020 MCMCOBJ=   -6604.39589342060     
 iteration         1025 MCMCOBJ=   -6592.16865844449     
 iteration         1030 MCMCOBJ=   -6622.25233210835     
 iteration         1035 MCMCOBJ=   -6594.15210224160     
 iteration         1040 MCMCOBJ=   -6596.41391579602     
 iteration         1045 MCMCOBJ=   -6676.63867085101     
 iteration         1050 MCMCOBJ=   -6568.79371981372     
 iteration         1055 MCMCOBJ=   -6541.52795876683     
 iteration         1060 MCMCOBJ=   -6645.52601105671     
 iteration         1065 MCMCOBJ=   -6584.58753947436     
 iteration         1070 MCMCOBJ=   -6559.14440344446     
 iteration         1075 MCMCOBJ=   -6607.90017566742     
 iteration         1080 MCMCOBJ=   -6626.95661022054     
 iteration         1085 MCMCOBJ=   -6595.80363688617     
 iteration         1090 MCMCOBJ=   -6591.16388280052     
 iteration         1095 MCMCOBJ=   -6593.84597021654     
 iteration         1100 MCMCOBJ=   -6623.95810935619     
 iteration         1105 MCMCOBJ=   -6618.32717155166     
 iteration         1110 MCMCOBJ=   -6545.06037058611     
 iteration         1115 MCMCOBJ=   -6608.01333981739     
 iteration         1120 MCMCOBJ=   -6622.46625413965     
 iteration         1125 MCMCOBJ=   -6645.96479636166     
 iteration         1130 MCMCOBJ=   -6580.32283845551     
 iteration         1135 MCMCOBJ=   -6601.48770673307     
 iteration         1140 MCMCOBJ=   -6578.90737197175     
 iteration         1145 MCMCOBJ=   -6572.69670895631     
 iteration         1150 MCMCOBJ=   -6542.99460798664     
 iteration         1155 MCMCOBJ=   -6601.73867129787     
 iteration         1160 MCMCOBJ=   -6596.70094259517     
 iteration         1165 MCMCOBJ=   -6647.04522955626     
 iteration         1170 MCMCOBJ=   -6606.52702500693     
 iteration         1175 MCMCOBJ=   -6642.43737626335     
 iteration         1180 MCMCOBJ=   -6535.65631087843     
 iteration         1185 MCMCOBJ=   -6588.52133372085     
 iteration         1190 MCMCOBJ=   -6614.99041497597     
 iteration         1195 MCMCOBJ=   -6610.84052402422     
 iteration         1200 MCMCOBJ=   -6603.71832438573     
 iteration         1205 MCMCOBJ=   -6629.35345158955     
 iteration         1210 MCMCOBJ=   -6621.61795354281     
 iteration         1215 MCMCOBJ=   -6634.59477908960     
 iteration         1220 MCMCOBJ=   -6569.58001389750     
 iteration         1225 MCMCOBJ=   -6601.71286621062     
 iteration         1230 MCMCOBJ=   -6566.10431747563     
 iteration         1235 MCMCOBJ=   -6612.92626398748     
 iteration         1240 MCMCOBJ=   -6610.96257192299     
 iteration         1245 MCMCOBJ=   -6598.10691478944     
 iteration         1250 MCMCOBJ=   -6594.66898507942     
 iteration         1255 MCMCOBJ=   -6605.04727199704     
 iteration         1260 MCMCOBJ=   -6549.49655013897     
 iteration         1265 MCMCOBJ=   -6619.17618786499     
 iteration         1270 MCMCOBJ=   -6640.81164978487     
 iteration         1275 MCMCOBJ=   -6573.32597057584     
 iteration         1280 MCMCOBJ=   -6599.74913885827     
 iteration         1285 MCMCOBJ=   -6603.68429278405     
 iteration         1290 MCMCOBJ=   -6607.42128377117     
 iteration         1295 MCMCOBJ=   -6616.82392759068     
 iteration         1300 MCMCOBJ=   -6647.32209336220     
 iteration         1305 MCMCOBJ=   -6585.80459702865     
 iteration         1310 MCMCOBJ=   -6596.61137835127     
 iteration         1315 MCMCOBJ=   -6655.89130144920     
 iteration         1320 MCMCOBJ=   -6615.18811661851     
 iteration         1325 MCMCOBJ=   -6622.97328927470     
 iteration         1330 MCMCOBJ=   -6646.71065354529     
 iteration         1335 MCMCOBJ=   -6601.25287385682     
 iteration         1340 MCMCOBJ=   -6561.69019751795     
 iteration         1345 MCMCOBJ=   -6553.77163024851     
 iteration         1350 MCMCOBJ=   -6572.16884294644     
 iteration         1355 MCMCOBJ=   -6615.54850243223     
 iteration         1360 MCMCOBJ=   -6558.66276019726     
 iteration         1365 MCMCOBJ=   -6617.77503597956     
 iteration         1370 MCMCOBJ=   -6630.43461508600     
 iteration         1375 MCMCOBJ=   -6618.84732673271     
 iteration         1380 MCMCOBJ=   -6582.47233051049     
 iteration         1385 MCMCOBJ=   -6618.95316329130     
 iteration         1390 MCMCOBJ=   -6589.94292235207     
 iteration         1395 MCMCOBJ=   -6540.97194739151     
 iteration         1400 MCMCOBJ=   -6541.37039286483     
 iteration         1405 MCMCOBJ=   -6573.34599494260     
 iteration         1410 MCMCOBJ=   -6596.40527823934     
 iteration         1415 MCMCOBJ=   -6561.38195764558     
 iteration         1420 MCMCOBJ=   -6586.73812388058     
 iteration         1425 MCMCOBJ=   -6656.61563819989     
 iteration         1430 MCMCOBJ=   -6584.15799896648     
 iteration         1435 MCMCOBJ=   -6546.90812468507     
 iteration         1440 MCMCOBJ=   -6654.91614379942     
 iteration         1445 MCMCOBJ=   -6549.42230126017     
 iteration         1450 MCMCOBJ=   -6553.47530647900     
 iteration         1455 MCMCOBJ=   -6541.48993805630     
 iteration         1460 MCMCOBJ=   -6569.32644477883     
 iteration         1465 MCMCOBJ=   -6585.09604565934     
 iteration         1470 MCMCOBJ=   -6608.31794653136     
 iteration         1475 MCMCOBJ=   -6574.92056906622     
 iteration         1480 MCMCOBJ=   -6593.15848355657     
 iteration         1485 MCMCOBJ=   -6571.81743319919     
 iteration         1490 MCMCOBJ=   -6605.40770363677     
 iteration         1495 MCMCOBJ=   -6621.97109443964     
 iteration         1500 MCMCOBJ=   -6576.94769789861     
 iteration         1505 MCMCOBJ=   -6585.43645777117     
 iteration         1510 MCMCOBJ=   -6532.87669355089     
 iteration         1515 MCMCOBJ=   -6613.45725401618     
 iteration         1520 MCMCOBJ=   -6598.21288781884     
 iteration         1525 MCMCOBJ=   -6605.36978189399     
 iteration         1530 MCMCOBJ=   -6642.15928879973     
 iteration         1535 MCMCOBJ=   -6671.32727128420     
 iteration         1540 MCMCOBJ=   -6660.56865864833     
 iteration         1545 MCMCOBJ=   -6578.35311929095     
 iteration         1550 MCMCOBJ=   -6581.11086488212     
 iteration         1555 MCMCOBJ=   -6656.10078426961     
 iteration         1560 MCMCOBJ=   -6609.42680588381     
 iteration         1565 MCMCOBJ=   -6684.50791888295     
 iteration         1570 MCMCOBJ=   -6551.95493422883     
 iteration         1575 MCMCOBJ=   -6597.74447161796     
 iteration         1580 MCMCOBJ=   -6624.27716331303     
 iteration         1585 MCMCOBJ=   -6581.80717568177     
 iteration         1590 MCMCOBJ=   -6618.97322266263     
 iteration         1595 MCMCOBJ=   -6584.19629577108     
 iteration         1600 MCMCOBJ=   -6650.01313516388     
 iteration         1605 MCMCOBJ=   -6601.80348086478     
 iteration         1610 MCMCOBJ=   -6685.44546393699     
 iteration         1615 MCMCOBJ=   -6626.14865506019     
 iteration         1620 MCMCOBJ=   -6566.31980406047     
 iteration         1625 MCMCOBJ=   -6618.92510750017     
 iteration         1630 MCMCOBJ=   -6611.74461628765     
 iteration         1635 MCMCOBJ=   -6579.74749583294     
 iteration         1640 MCMCOBJ=   -6566.03971027242     
 iteration         1645 MCMCOBJ=   -6557.41044033108     
 iteration         1650 MCMCOBJ=   -6510.60130815231     
 iteration         1655 MCMCOBJ=   -6592.57340175036     
 iteration         1660 MCMCOBJ=   -6587.35392160195     
 iteration         1665 MCMCOBJ=   -6539.66383016542     
 iteration         1670 MCMCOBJ=   -6581.92559213964     
 iteration         1675 MCMCOBJ=   -6622.37387903316     
 iteration         1680 MCMCOBJ=   -6626.08009812443     
 iteration         1685 MCMCOBJ=   -6626.24376048699     
 iteration         1690 MCMCOBJ=   -6620.07516771760     
 iteration         1695 MCMCOBJ=   -6595.24640653283     
 iteration         1700 MCMCOBJ=   -6583.74516858630     
 iteration         1705 MCMCOBJ=   -6607.55211973404     
 iteration         1710 MCMCOBJ=   -6597.64577069399     
 iteration         1715 MCMCOBJ=   -6547.40939755878     
 iteration         1720 MCMCOBJ=   -6621.60930355959     
 iteration         1725 MCMCOBJ=   -6588.81309913805     
 iteration         1730 MCMCOBJ=   -6657.43121310517     
 iteration         1735 MCMCOBJ=   -6609.29032303895     
 iteration         1740 MCMCOBJ=   -6584.03967495582     
 iteration         1745 MCMCOBJ=   -6561.88332678010     
 iteration         1750 MCMCOBJ=   -6657.00754536993     
 iteration         1755 MCMCOBJ=   -6602.69858232073     
 iteration         1760 MCMCOBJ=   -6569.20431580750     
 iteration         1765 MCMCOBJ=   -6605.11160600871     
 iteration         1770 MCMCOBJ=   -6664.79722040657     
 iteration         1775 MCMCOBJ=   -6625.78531851764     
 iteration         1780 MCMCOBJ=   -6650.77634252606     
 iteration         1785 MCMCOBJ=   -6613.17823212061     
 iteration         1790 MCMCOBJ=   -6601.53988957934     
 iteration         1795 MCMCOBJ=   -6655.09447390305     
 iteration         1800 MCMCOBJ=   -6635.56322212210     
 iteration         1805 MCMCOBJ=   -6658.20096156140     
 iteration         1810 MCMCOBJ=   -6640.99156480050     
 iteration         1815 MCMCOBJ=   -6557.57155443090     
 iteration         1820 MCMCOBJ=   -6572.80756597436     
 iteration         1825 MCMCOBJ=   -6528.57886002063     
 iteration         1830 MCMCOBJ=   -6597.36924411478     
 iteration         1835 MCMCOBJ=   -6589.03343441450     
 iteration         1840 MCMCOBJ=   -6583.97369400484     
 iteration         1845 MCMCOBJ=   -6656.43537782262     
 iteration         1850 MCMCOBJ=   -6581.38920484488     
 iteration         1855 MCMCOBJ=   -6599.64945880467     
 iteration         1860 MCMCOBJ=   -6613.11758953436     
 iteration         1865 MCMCOBJ=   -6619.37435913168     
 iteration         1870 MCMCOBJ=   -6596.59415393704     
 iteration         1875 MCMCOBJ=   -6567.06931831224     
 iteration         1880 MCMCOBJ=   -6545.98260334958     
 iteration         1885 MCMCOBJ=   -6621.93823351527     
 iteration         1890 MCMCOBJ=   -6606.52949089171     
 iteration         1895 MCMCOBJ=   -6654.09334844626     
 iteration         1900 MCMCOBJ=   -6589.51453167077     
 iteration         1905 MCMCOBJ=   -6654.74783740660     
 iteration         1910 MCMCOBJ=   -6633.06296282302     
 iteration         1915 MCMCOBJ=   -6599.64456561018     
 iteration         1920 MCMCOBJ=   -6528.17562907547     
 iteration         1925 MCMCOBJ=   -6583.32040826743     
 iteration         1930 MCMCOBJ=   -6574.93919002087     
 iteration         1935 MCMCOBJ=   -6606.23783997783     
 iteration         1940 MCMCOBJ=   -6601.36276082582     
 iteration         1945 MCMCOBJ=   -6642.78985993466     
 iteration         1950 MCMCOBJ=   -6595.79338805156     
 iteration         1955 MCMCOBJ=   -6519.09776143206     
 iteration         1960 MCMCOBJ=   -6644.47476388465     
 iteration         1965 MCMCOBJ=   -6623.27588917574     
 iteration         1970 MCMCOBJ=   -6562.26307119199     
 iteration         1975 MCMCOBJ=   -6561.14811874861     
 iteration         1980 MCMCOBJ=   -6600.95793703230     
 iteration         1985 MCMCOBJ=   -6616.34260764998     
 iteration         1990 MCMCOBJ=   -6622.94088534715     
 iteration         1995 MCMCOBJ=   -6588.98994989326     
 iteration         2000 MCMCOBJ=   -6518.88954406485     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6597.99243307986     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3716.20119295001     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6597.99243307986     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5862.84160651612     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -16.9020929079541     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6597.99243307986     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6614.89452598781     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  7400.66
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6597.992       **************************************************
 #OBJS:********************************************       35.259 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.21E+00  5.58E-01 -1.77E-01  2.27E+00  2.36E-01  3.71E+00 -7.06E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.67E-01
 
 ETA2
+       -2.99E-02  1.75E-01
 
 ETA3
+        2.88E-02 -1.07E-02  1.05E-01
 
 ETA4
+        2.36E-02  3.24E-02 -1.15E-02  2.50E-01
 
 ETA5
+        2.16E-02  1.87E-02 -1.48E-03 -2.27E-02  1.88E-01
 
 ETA6
+       -1.32E-02  1.05E-02  1.56E-02  1.13E-02 -5.56E-02  2.12E-01
 
 ETA7
+        1.19E-02 -3.35E-02  1.85E-02 -5.54E-02  1.77E-02  7.20E-03  2.25E-01
 
 ETA8
+        6.98E-02  6.26E-02  2.62E-02  3.42E-02 -6.16E-03 -4.14E-02  4.71E-02  1.92E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.41E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.13E-01
 
 ETA2
+       -1.37E-01  4.14E-01
 
 ETA3
+        1.76E-01 -8.14E-02  3.20E-01
 
 ETA4
+        9.10E-02  1.58E-01 -7.43E-02  4.97E-01
 
 ETA5
+        9.67E-02  1.05E-01 -1.06E-02 -1.06E-01  4.32E-01
 
 ETA6
+       -5.79E-02  5.74E-02  1.08E-01  5.01E-02 -2.82E-01  4.56E-01
 
 ETA7
+        4.88E-02 -1.66E-01  1.24E-01 -2.34E-01  8.61E-02  3.23E-02  4.72E-01
 
 ETA8
+        3.10E-01  3.43E-01  1.85E-01  1.56E-01 -3.25E-02 -2.06E-01  2.25E-01  4.36E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.69E-02
 
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
 
         7.54E-02  7.15E-02  5.37E-02  6.96E-02  6.37E-02  7.08E-02  6.90E-02  6.48E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.91E-02
 
 ETA2
+        2.96E-02  4.99E-02
 
 ETA3
+        2.15E-02  1.89E-02  3.13E-02
 
 ETA4
+        3.24E-02  2.80E-02  2.22E-02  5.84E-02
 
 ETA5
+        2.85E-02  2.33E-02  1.92E-02  2.75E-02  4.05E-02
 
 ETA6
+        3.04E-02  2.70E-02  2.14E-02  2.93E-02  2.71E-02  5.55E-02
 
 ETA7
+        2.88E-02  2.92E-02  2.01E-02  3.02E-02  2.53E-02  2.87E-02  4.66E-02
 
 ETA8
+        2.89E-02  2.64E-02  1.89E-02  2.79E-02  2.28E-02  2.67E-02  2.51E-02  3.96E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.40E-04
 
 EPS2
+        0.00E+00  1.21E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.57E-02
 
 ETA2
+        1.29E-01  5.77E-02
 
 ETA3
+        1.24E-01  1.36E-01  4.67E-02
 
 ETA4
+        1.18E-01  1.29E-01  1.36E-01  5.67E-02
 
 ETA5
+        1.22E-01  1.25E-01  1.34E-01  1.23E-01  4.59E-02
 
 ETA6
+        1.25E-01  1.37E-01  1.40E-01  1.24E-01  1.25E-01  5.92E-02
 
 ETA7
+        1.14E-01  1.32E-01  1.27E-01  1.15E-01  1.18E-01  1.27E-01  4.83E-02
 
 ETA8
+        1.10E-01  1.19E-01  1.21E-01  1.20E-01  1.17E-01  1.22E-01  1.08E-01  4.44E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.29E-03
 
 EPS2
+        0.00E+00  4.03E-03
 
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
+        5.68E-03
 
 TH 2
+       -6.43E-04  5.12E-03
 
 TH 3
+        4.47E-04  2.29E-05  2.88E-03
 
 TH 4
+        3.71E-04  6.24E-04  2.03E-04  4.85E-03
 
 TH 5
+        5.40E-04  1.46E-04 -1.95E-06 -4.83E-04  4.06E-03
 
 TH 6
+       -4.39E-04 -6.19E-05  2.45E-04  2.60E-04 -8.94E-04  5.01E-03
 
 TH 7
+        9.77E-05 -9.37E-04  4.58E-04 -9.88E-04  5.16E-04  1.12E-04  4.76E-03
 
 TH 8
+        1.36E-03  1.20E-03  7.87E-04  8.54E-04 -4.87E-05 -8.66E-04  1.00E-03  4.20E-03
 
 OM11
+        6.13E-05  1.59E-05  2.44E-04 -4.07E-05  1.40E-05 -2.63E-05  2.76E-04  9.73E-05  3.50E-03
 
 OM12
+       -7.20E-06  2.14E-04  1.19E-05  5.27E-05 -5.43E-05  1.26E-05 -2.11E-05  5.55E-05 -3.47E-04  8.74E-04
 
 OM13
+       -1.16E-05  2.17E-05  8.42E-06 -1.13E-05  1.52E-05 -7.76E-06 -2.37E-05 -7.94E-05  1.17E-04 -6.06E-06  4.64E-04
 
 OM14
+        6.10E-06  8.02E-06  3.68E-05  5.23E-05  3.28E-05  2.14E-05  7.79E-05 -6.54E-05  2.52E-04  2.08E-05  2.53E-05  1.05E-03
 
 OM15
+        4.32E-05 -2.63E-05  8.44E-06  4.71E-05 -8.94E-05 -4.11E-05  2.70E-05  9.60E-05  1.60E-04  3.80E-05  8.69E-06 -5.53E-05
          8.13E-04
 
 OM16
+        5.45E-05 -4.28E-05  3.18E-05 -3.97E-05  7.42E-05 -2.79E-05 -1.79E-05 -1.83E-05 -8.97E-06  1.78E-05  3.84E-05  7.69E-05
         -7.65E-05  9.26E-04
 
 OM17
+        4.00E-05  7.96E-05  1.80E-06  7.34E-06  6.46E-05 -7.82E-05 -9.10E-05  1.21E-05  1.32E-04 -1.34E-04  3.79E-05 -1.49E-04
          6.13E-05  1.83E-05  8.32E-04
 
 OM18
+        6.36E-06 -7.00E-06 -1.21E-05  6.40E-05  1.41E-05 -1.92E-05  5.55E-05  3.31E-05  6.43E-04  1.08E-04  9.98E-05  8.68E-05
         -2.49E-05 -1.37E-04  2.01E-04  8.37E-04
 
 OM22
+       -7.03E-05 -8.70E-04  2.21E-05 -9.17E-05 -3.74E-06  1.43E-04  1.27E-04 -1.81E-05  6.22E-05 -4.04E-04 -3.38E-05  5.50E-05
         -4.88E-05  9.78E-06  6.41E-05 -1.81E-05  2.49E-03
 
 OM23
+       -9.77E-06  1.15E-04  5.64E-05  2.13E-05 -3.86E-05  2.23E-05 -2.02E-05  3.69E-05  2.86E-06  4.90E-05 -1.71E-05  1.10E-05
          8.19E-06  9.22E-06  9.61E-07  5.46E-06 -1.25E-04  3.56E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.33E-05 -3.50E-04  1.43E-04 -3.74E-05  5.02E-05  4.81E-06  1.04E-04  6.11E-05 -5.84E-05 -5.85E-05 -2.12E-05 -8.83E-05
          1.52E-05  1.09E-05  2.15E-05 -2.56E-05  2.78E-04 -2.61E-05  7.82E-04
 
 OM25
+       -3.74E-05  2.21E-05  2.62E-05  3.47E-05 -2.86E-05  2.97E-05 -1.22E-05  5.16E-05 -1.65E-06  4.16E-05  8.91E-06 -1.78E-05
         -9.35E-05 -2.44E-06 -7.04E-06  3.91E-05  9.06E-05  6.82E-06 -4.16E-05  5.45E-04
 
 OM26
+       -6.03E-05  9.00E-05 -1.51E-05 -3.65E-05 -5.21E-05  4.02E-05 -4.26E-06 -5.45E-05  2.29E-05 -4.28E-06  1.85E-05  1.68E-05
          4.06E-05 -9.54E-05 -2.04E-05  1.75E-05 -8.77E-07 -1.03E-05  6.46E-06 -1.15E-04  7.30E-04
 
 OM27
+        2.12E-05  4.98E-04  1.56E-05  2.24E-05 -2.41E-05  5.90E-05 -6.40E-06  2.90E-05  7.09E-05  1.15E-04  8.56E-06 -2.01E-05
         -5.61E-07 -2.60E-05 -9.27E-05 -3.71E-06 -5.47E-04  6.22E-05 -2.35E-04  4.79E-05 -1.49E-05  8.50E-04
 
 OM28
+       -5.78E-05 -1.39E-04  8.29E-05 -1.31E-04  2.48E-05  2.86E-05  6.97E-05  6.44E-06 -6.90E-05  1.05E-04 -2.29E-05  1.69E-05
          9.62E-06  1.50E-05 -2.41E-05 -1.60E-05  5.34E-04  5.12E-05  1.20E-04  2.63E-05 -1.35E-04  9.98E-05  6.99E-04
 
 OM33
+        3.69E-05  9.32E-05 -2.32E-04 -1.53E-04 -1.61E-05  4.65E-05 -7.65E-05 -4.10E-05 -2.36E-05  3.35E-05  1.15E-04  1.20E-05
          1.34E-05 -1.61E-05  1.54E-05  4.81E-05  4.28E-05 -1.20E-05 -2.48E-05 -1.19E-05  2.16E-05  2.89E-06  1.49E-05  9.80E-04
 
 OM34
+        3.48E-05 -8.37E-05  1.75E-04  5.57E-05 -2.67E-06 -1.78E-05  2.20E-05  1.02E-05 -7.76E-06  1.57E-06  3.31E-05  6.83E-05
          3.53E-06  5.54E-05 -3.30E-05 -3.91E-05  6.36E-06  4.29E-05 -7.54E-06  3.93E-06  1.58E-06  1.14E-05  8.99E-06 -4.26E-05
         4.93E-04
 
 OM35
+        1.15E-05  2.46E-05 -2.22E-05  4.93E-05 -4.47E-05 -2.99E-06  2.22E-05  3.09E-05  6.91E-07  1.25E-06  3.07E-05  3.32E-06
          2.98E-05 -7.04E-06  3.82E-05  1.33E-05  3.40E-05  3.02E-05  9.97E-06 -6.25E-07 -1.61E-05  6.88E-06  8.18E-08 -1.00E-05
        -3.56E-05  3.69E-04
 
 OM36
+       -4.06E-05 -2.70E-05  2.17E-05 -1.38E-06 -2.50E-05 -2.13E-05  3.50E-05 -6.45E-07 -1.94E-05 -7.51E-06  9.26E-06  2.15E-05
          2.05E-05  4.40E-05 -1.58E-05 -3.77E-06  2.01E-05  1.01E-05 -1.72E-06  1.27E-05 -6.11E-06  9.22E-06  2.11E-05  5.97E-05
         3.15E-05 -5.75E-05  4.58E-04
 
 OM37
+       -2.73E-06 -4.42E-06 -8.18E-05  5.68E-06  2.60E-05 -7.34E-05  2.61E-06  1.75E-05  1.79E-05 -5.02E-06  1.22E-05 -2.06E-05
         -1.51E-05 -1.52E-05  4.96E-05  1.98E-05  3.62E-05 -5.91E-05  1.38E-05 -2.05E-05  1.79E-05  6.30E-06 -7.00E-06  4.69E-05
        -8.04E-05  3.83E-05  1.53E-05  4.06E-04
 
 OM38
+        7.87E-06 -7.66E-06 -2.23E-05 -2.70E-05  1.17E-05  3.27E-05 -4.74E-06 -3.49E-06  3.21E-05  1.85E-05  1.13E-04  1.32E-05
         -1.81E-05  2.86E-05  2.11E-05  8.15E-05  1.71E-05  8.46E-05 -2.13E-06 -6.37E-06  2.42E-05  2.13E-06  3.58E-05  1.70E-04
         4.83E-05 -2.14E-05 -4.33E-05  5.83E-05  3.56E-04
 
 OM44
+       -1.86E-04 -1.49E-04  3.95E-04  4.57E-04 -1.46E-04  4.76E-05  1.47E-04  9.01E-05  2.63E-04  3.95E-05  3.88E-05  3.28E-04
         -5.81E-05  3.04E-05 -1.08E-04  7.92E-05  8.17E-05  2.96E-05  1.84E-04 -2.71E-05 -1.89E-05 -3.52E-05  9.57E-05 -1.38E-04
         8.25E-05 -4.44E-05  1.45E-05 -3.57E-06  4.14E-05  3.41E-03
 
 OM45
+       -9.24E-05  9.13E-05  7.28E-05  3.52E-05 -1.01E-05 -2.95E-06  1.50E-05  1.24E-04  4.17E-05 -1.43E-05 -1.77E-05  5.35E-05
          5.20E-05  5.67E-06  2.30E-05  1.17E-05  3.16E-05  7.20E-06  3.58E-05  5.27E-05  1.45E-05 -1.58E-05 -1.75E-05 -2.15E-05
         4.46E-06 -1.74E-05 -5.29E-06  1.50E-05 -2.88E-06 -1.14E-04  7.58E-04
 
 OM46
+        1.02E-04  4.79E-05  1.43E-06 -5.05E-05  3.47E-05  1.29E-04  5.52E-05  3.74E-05  4.16E-06  5.25E-06  8.86E-06 -1.68E-05
         -1.64E-05  2.07E-05  1.99E-05  3.72E-05 -5.98E-06 -1.78E-05  2.51E-06  1.55E-05  3.83E-05  3.30E-07 -2.29E-05  4.20E-05
        -1.62E-05  1.69E-05 -2.12E-06  7.97E-08 -1.71E-05  1.27E-04 -9.20E-05  8.56E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        2.99E-05  1.62E-05 -2.51E-05  6.22E-06 -5.07E-06 -3.92E-05 -6.94E-06  2.13E-05  1.01E-05 -3.21E-05 -1.35E-05 -8.34E-07
          2.72E-05 -3.22E-05  4.54E-05 -9.16E-06 -9.96E-05  9.76E-06 -1.65E-04  2.44E-05  1.76E-05  1.11E-04 -5.77E-05  4.18E-05
         2.51E-05  9.31E-06  1.60E-05 -9.27E-06 -1.30E-05 -5.04E-04  4.84E-05  4.23E-05  9.13E-04
 
 OM48
+        1.27E-05  1.00E-06  2.11E-05  4.92E-06  5.33E-06  5.81E-06  6.36E-05  9.83E-06  2.41E-06 -1.05E-05  1.21E-05  2.41E-04
          7.33E-06  1.17E-05 -5.35E-06  7.59E-05  4.69E-05  2.49E-05  1.67E-04 -1.24E-05 -1.40E-05  9.92E-06  1.15E-04  2.17E-05
         8.48E-05  3.88E-06  2.36E-05 -2.10E-05  1.46E-05  3.26E-04 -4.15E-05 -6.72E-05  1.35E-04  7.77E-04
 
 OM55
+       -1.04E-04  9.41E-05 -5.32E-05 -4.90E-05 -1.07E-04 -6.54E-05  5.84E-05 -3.51E-06  1.41E-04 -6.10E-05 -5.82E-06 -2.89E-05
          1.38E-04 -4.49E-06  3.87E-05 -1.12E-05 -3.52E-05 -1.87E-05  2.77E-05  6.66E-05 -2.22E-05  2.06E-05 -5.65E-05  1.62E-05
        -1.21E-05 -2.18E-05 -1.34E-05 -5.96E-05 -2.75E-05  6.65E-05 -7.93E-05  2.71E-05 -4.51E-06  3.10E-05  1.64E-03
 
 OM56
+       -5.92E-05  8.21E-05  6.60E-06  3.53E-05 -8.06E-05 -5.43E-05 -7.47E-05 -1.84E-05 -2.19E-05  1.09E-06  8.23E-06 -4.41E-05
         -6.00E-05  3.83E-05  1.39E-05  4.01E-05 -8.05E-06 -1.59E-06  1.29E-05  1.48E-05  5.39E-05  1.03E-05  9.05E-06 -5.58E-06
        -5.50E-06  6.74E-06  1.81E-06  1.40E-05  1.61E-05 -5.15E-05  4.66E-05 -2.34E-05  6.03E-06 -2.44E-05 -2.35E-04  7.34E-04
 
 OM57
+        4.25E-05 -1.01E-04 -3.80E-05 -4.77E-05  7.28E-05 -1.35E-04 -6.61E-05 -4.77E-05  2.99E-05  1.74E-06  4.70E-06 -2.18E-05
          1.11E-05  5.78E-06  7.22E-05  1.52E-05 -1.95E-05 -1.68E-05  1.63E-05 -3.42E-05  3.59E-05  6.39E-06 -1.84E-05 -5.48E-07
        -2.40E-05  2.33E-05  5.91E-06  2.08E-05  5.81E-06  4.09E-05 -1.24E-04  2.84E-05 -5.95E-05 -4.35E-05  1.20E-04  4.41E-05
          6.40E-04
 
 OM58
+       -4.84E-05 -3.32E-05  2.11E-05 -1.40E-05 -1.08E-06  2.11E-05  1.56E-05  2.89E-05  3.75E-05  3.35E-05  1.97E-05 -1.88E-05
          1.42E-04 -2.90E-05  3.39E-05  3.43E-05  3.94E-05 -2.37E-06 -1.51E-05  1.12E-04 -9.74E-06  9.18E-06  5.17E-05  6.74E-06
        -1.28E-05  5.73E-05  1.15E-05  1.38E-05  3.45E-06 -4.92E-05  5.37E-05  6.04E-06  1.26E-05 -7.30E-06 -5.57E-05 -7.19E-05
          1.20E-04  5.20E-04
 
 OM66
+        6.31E-05 -2.30E-04  8.64E-05 -8.01E-05  3.00E-05  8.65E-05  1.02E-04 -1.55E-05  4.92E-05  4.15E-05 -4.62E-05 -2.97E-05
          4.60E-05 -1.61E-05 -1.91E-05  1.93E-05  1.26E-05 -5.31E-05 -5.54E-07 -3.36E-05  4.85E-06  7.53E-06  4.66E-05  3.47E-07
         3.30E-05 -7.17E-06  7.12E-05 -3.86E-05 -3.83E-05  1.04E-04  2.63E-05  3.28E-05  2.40E-05  4.16E-05 -5.05E-05 -4.02E-04
          1.42E-05  6.24E-05  3.08E-03
 
 OM67
+        7.69E-05 -6.09E-05 -7.51E-05  4.57E-05 -5.40E-05 -1.37E-04 -2.31E-05  9.24E-06 -6.98E-05 -1.89E-05 -9.62E-06 -3.45E-05
          8.97E-06  3.25E-06 -2.00E-05 -9.55E-06 -1.61E-05 -8.92E-06  3.63E-05  9.66E-06 -1.26E-04  2.45E-06 -1.69E-05  1.31E-06
        -4.53E-05 -2.19E-06  4.77E-05  4.80E-06 -1.90E-05 -1.13E-05  2.27E-05 -1.48E-04  1.33E-05  4.25E-05 -9.93E-06  3.55E-05
         -8.78E-05 -2.06E-05  1.06E-04  8.24E-04
 
 OM68
+        3.93E-06  4.89E-05 -3.01E-05  7.29E-05  5.94E-05 -1.98E-05  1.10E-05  5.73E-05 -1.13E-04 -4.40E-06 -6.46E-06  4.97E-05
         -1.49E-05  1.46E-04 -3.86E-05 -8.16E-05 -1.54E-05  1.23E-05  8.89E-06 -2.27E-05  1.87E-04 -2.51E-05 -6.09E-05  2.74E-05
         1.24E-06 -1.23E-05  8.62E-05  1.58E-05  7.10E-07  5.93E-05  8.66E-06  9.06E-05  8.94E-06 -9.44E-06 -1.21E-05  3.54E-05
         -1.49E-05 -7.23E-05 -4.13E-04  9.13E-05  7.12E-04
 
 OM77
+       -1.03E-04 -3.40E-05 -1.67E-04 -1.15E-05 -2.62E-05 -1.67E-04 -8.33E-05 -1.07E-04  1.05E-05  2.27E-05 -1.43E-05 -5.00E-06
         -4.97E-05  4.14E-06  3.34E-05  3.67E-05  9.18E-05  2.94E-05  6.42E-05 -5.07E-05 -4.45E-05 -3.07E-04  2.26E-05  4.69E-05
        -8.80E-05 -1.43E-05 -1.90E-05  6.10E-05  1.56E-05  1.40E-04 -2.21E-05 -2.64E-05 -4.50E-04 -1.22E-04  2.59E-05 -3.49E-05
          1.05E-04  9.11E-06  3.77E-05  8.99E-05  3.37E-06  2.17E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -5.89E-05 -2.71E-05 -3.35E-05  2.15E-05 -3.63E-05  7.31E-05 -2.50E-06  2.54E-05  1.12E-05  1.10E-05 -5.22E-06 -6.90E-05
          5.37E-06 -1.79E-05  1.35E-04  3.49E-05 -4.06E-05  1.09E-05 -3.08E-05  1.42E-05  1.24E-05  1.04E-04  8.86E-06  1.92E-05
        -1.67E-05 -1.94E-06 -1.76E-05  7.35E-05  5.04E-05 -5.72E-05  5.28E-09 -1.41E-05  1.54E-05 -1.08E-04  1.23E-06  7.46E-06
          7.01E-06  4.33E-05 -4.08E-05 -8.25E-05 -3.08E-05  3.75E-04  6.29E-04
 
 OM88
+       -5.50E-05  2.63E-05  1.86E-05 -6.39E-05  2.98E-05  3.52E-05  5.04E-05  3.85E-05  1.69E-04  1.10E-04  5.73E-05  1.10E-05
         -1.89E-05 -1.72E-05  9.14E-05  3.72E-04  1.46E-04  5.54E-05  1.02E-04  3.42E-05 -8.52E-05  7.25E-05  4.13E-04  6.14E-05
        -1.97E-05 -2.02E-05 -2.12E-05  3.06E-05  1.86E-04  1.68E-04 -4.11E-05 -5.30E-05 -2.95E-05  2.04E-04 -1.35E-05  5.94E-05
         -1.90E-05 -1.61E-05  4.02E-05 -5.07E-05 -2.59E-04  9.01E-05  3.27E-04  1.57E-03
 
 SG11
+       -2.46E-06  1.69E-06 -5.33E-07 -1.17E-06  6.75E-07 -1.27E-06  7.90E-07 -7.16E-07 -2.62E-07 -1.03E-06 -2.83E-07 -1.32E-06
         -3.11E-07 -1.06E-06  5.50E-07  1.43E-07 -5.53E-07 -2.24E-07  4.35E-08  3.20E-07  1.39E-07  5.25E-07  5.22E-09 -9.41E-07
        -1.45E-07  4.93E-07 -2.21E-07 -1.59E-07 -3.80E-07 -7.60E-07 -9.02E-07  4.14E-07  1.23E-08 -3.38E-07  5.49E-08  6.79E-07
          5.05E-07  2.56E-07  5.11E-08  2.02E-07  2.62E-07 -8.25E-07  2.86E-07  2.33E-07  4.10E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        3.31E-06 -6.55E-07  9.20E-07 -5.19E-06  1.17E-06 -3.56E-07  4.82E-07  9.74E-07 -2.77E-06  6.34E-08 -3.25E-07 -2.10E-07
          7.70E-07 -1.62E-07 -8.26E-07 -1.11E-06 -9.97E-07  4.65E-07 -1.02E-06  9.30E-07  5.51E-07  5.72E-07 -2.70E-08  3.37E-07
         5.27E-07 -8.59E-07  2.82E-06 -1.18E-07 -1.09E-06 -1.01E-08 -7.68E-07  9.08E-07  1.09E-06 -4.33E-07 -1.01E-06 -1.54E-06
         -7.65E-07  9.82E-07 -3.02E-07 -4.82E-07  2.79E-06 -8.47E-07 -9.09E-07 -4.11E-07 -1.84E-09  0.00E+00  1.46E-06
 
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
+        7.54E-02
 
 TH 2
+       -1.19E-01  7.15E-02
 
 TH 3
+        1.10E-01  5.97E-03  5.37E-02
 
 TH 4
+        7.06E-02  1.25E-01  5.42E-02  6.96E-02
 
 TH 5
+        1.12E-01  3.21E-02 -5.69E-04 -1.09E-01  6.37E-02
 
 TH 6
+       -8.23E-02 -1.22E-02  6.45E-02  5.27E-02 -1.98E-01  7.08E-02
 
 TH 7
+        1.88E-02 -1.90E-01  1.24E-01 -2.06E-01  1.17E-01  2.30E-02  6.90E-02
 
 TH 8
+        2.79E-01  2.59E-01  2.26E-01  1.89E-01 -1.18E-02 -1.89E-01  2.24E-01  6.48E-02
 
 OM11
+        1.38E-02  3.77E-03  7.70E-02 -9.90E-03  3.72E-03 -6.29E-03  6.77E-02  2.54E-02  5.91E-02
 
 OM12
+       -3.23E-03  1.01E-01  7.51E-03  2.56E-02 -2.88E-02  6.00E-03 -1.03E-02  2.90E-02 -1.99E-01  2.96E-02
 
 OM13
+       -7.18E-03  1.41E-02  7.28E-03 -7.56E-03  1.10E-02 -5.09E-03 -1.59E-02 -5.70E-02  9.16E-02 -9.51E-03  2.15E-02
 
 OM14
+        2.50E-03  3.47E-03  2.11E-02  2.32E-02  1.59E-02  9.32E-03  3.49E-02 -3.12E-02  1.32E-01  2.18E-02  3.62E-02  3.24E-02
 
 OM15
+        2.01E-02 -1.29E-02  5.51E-03  2.37E-02 -4.92E-02 -2.04E-02  1.37E-02  5.20E-02  9.46E-02  4.50E-02  1.42E-02 -5.99E-02
          2.85E-02
 
 OM16
+        2.38E-02 -1.96E-02  1.95E-02 -1.87E-02  3.82E-02 -1.30E-02 -8.52E-03 -9.27E-03 -4.99E-03  1.97E-02  5.86E-02  7.81E-02
         -8.82E-02  3.04E-02
 
 OM17
+        1.84E-02  3.86E-02  1.16E-03  3.66E-03  3.51E-02 -3.83E-02 -4.57E-02  6.50E-03  7.74E-02 -1.57E-01  6.10E-02 -1.59E-01
          7.46E-02  2.09E-02  2.88E-02
 
 OM18
+        2.91E-03 -3.38E-03 -7.79E-03  3.18E-02  7.67E-03 -9.37E-03  2.78E-02  1.77E-02  3.76E-01  1.26E-01  1.60E-01  9.27E-02
         -3.02E-02 -1.56E-01  2.40E-01  2.89E-02
 
 OM22
+       -1.87E-02 -2.44E-01  8.25E-03 -2.64E-02 -1.18E-03  4.05E-02  3.69E-02 -5.60E-03  2.11E-02 -2.73E-01 -3.15E-02  3.40E-02
         -3.43E-02  6.43E-03  4.45E-02 -1.25E-02  4.99E-02
 
 OM23
+       -6.87E-03  8.55E-02  5.57E-02  1.62E-02 -3.21E-02  1.67E-02 -1.55E-02  3.02E-02  2.56E-03  8.79E-02 -4.22E-02  1.79E-02
          1.52E-02  1.61E-02  1.77E-03  1.00E-02 -1.33E-01  1.89E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        2.05E-02 -1.75E-01  9.52E-02 -1.92E-02  2.82E-02  2.43E-03  5.41E-02  3.37E-02 -3.53E-02 -7.08E-02 -3.52E-02 -9.76E-02
          1.91E-02  1.28E-02  2.67E-02 -3.17E-02  1.99E-01 -4.96E-02  2.80E-02
 
 OM25
+       -2.12E-02  1.32E-02  2.09E-02  2.14E-02 -1.92E-02  1.80E-02 -7.59E-03  3.41E-02 -1.20E-03  6.03E-02  1.77E-02 -2.35E-02
         -1.41E-01 -3.44E-03 -1.05E-02  5.79E-02  7.78E-02  1.55E-02 -6.38E-02  2.33E-02
 
 OM26
+       -2.96E-02  4.66E-02 -1.04E-02 -1.94E-02 -3.02E-02  2.10E-02 -2.28E-03 -3.11E-02  1.44E-02 -5.35E-03  3.18E-02  1.92E-02
          5.28E-02 -1.16E-01 -2.62E-02  2.24E-02 -6.50E-04 -2.02E-02  8.55E-03 -1.83E-01  2.70E-02
 
 OM27
+        9.63E-03  2.39E-01  9.95E-03  1.10E-02 -1.30E-02  2.86E-02 -3.18E-03  1.54E-02  4.11E-02  1.33E-01  1.36E-02 -2.13E-02
         -6.75E-04 -2.93E-02 -1.10E-01 -4.40E-03 -3.76E-01  1.13E-01 -2.89E-01  7.04E-02 -1.90E-02  2.92E-02
 
 OM28
+       -2.90E-02 -7.34E-02  5.84E-02 -7.10E-02  1.47E-02  1.53E-02  3.82E-02  3.76E-03 -4.41E-02  1.34E-01 -4.03E-02  1.98E-02
          1.28E-02  1.87E-02 -3.16E-02 -2.09E-02  4.05E-01  1.03E-01  1.62E-01  4.26E-02 -1.90E-01  1.29E-01  2.64E-02
 
 OM33
+        1.57E-02  4.16E-02 -1.38E-01 -7.02E-02 -8.07E-03  2.10E-02 -3.54E-02 -2.02E-02 -1.28E-02  3.62E-02  1.70E-01  1.19E-02
          1.50E-02 -1.69E-02  1.71E-02  5.31E-02  2.74E-02 -2.03E-02 -2.83E-02 -1.63E-02  2.56E-02  3.17E-03  1.80E-02  3.13E-02
 
 OM34
+        2.08E-02 -5.27E-02  1.47E-01  3.60E-02 -1.89E-03 -1.13E-02  1.44E-02  7.06E-03 -5.91E-03  2.38E-03  6.92E-02  9.50E-02
          5.58E-03  8.20E-02 -5.15E-02 -6.08E-02  5.74E-03  1.02E-01 -1.21E-02  7.58E-03  2.64E-03  1.77E-02  1.53E-02 -6.12E-02
         2.22E-02
 
 OM35
+        7.92E-03  1.79E-02 -2.15E-02  3.68E-02 -3.65E-02 -2.20E-03  1.68E-02  2.48E-02  6.08E-04  2.20E-03  7.43E-02  5.33E-03
          5.44E-02 -1.20E-02  6.88E-02  2.39E-02  3.55E-02  8.32E-02  1.86E-02 -1.39E-03 -3.10E-02  1.23E-02  1.61E-04 -1.67E-02
        -8.34E-02  1.92E-02
 
 OM36
+       -2.52E-02 -1.77E-02  1.89E-02 -9.26E-04 -1.84E-02 -1.41E-02  2.37E-02 -4.66E-04 -1.53E-02 -1.19E-02  2.01E-02  3.11E-02
          3.36E-02  6.76E-02 -2.56E-02 -6.09E-03  1.88E-02  2.51E-02 -2.87E-03  2.54E-02 -1.06E-02  1.48E-02  3.74E-02  8.92E-02
         6.63E-02 -1.40E-01  2.14E-02
 
 OM37
+       -1.80E-03 -3.07E-03 -7.57E-02  4.05E-03  2.02E-02 -5.15E-02  1.88E-03  1.34E-02  1.50E-02 -8.44E-03  2.81E-02 -3.16E-02
         -2.63E-02 -2.47E-02  8.54E-02  3.40E-02  3.60E-02 -1.56E-01  2.45E-02 -4.35E-02  3.29E-02  1.07E-02 -1.31E-02  7.45E-02
        -1.80E-01  9.91E-02  3.56E-02  2.01E-02
 
 OM38
+        5.53E-03 -5.67E-03 -2.20E-02 -2.06E-02  9.70E-03  2.45E-02 -3.64E-03 -2.86E-03  2.87E-02  3.31E-02  2.79E-01  2.17E-02
         -3.36E-02  4.98E-02  3.87E-02  1.49E-01  1.82E-02  2.38E-01 -4.04E-03 -1.45E-02  4.75E-02  3.88E-03  7.17E-02  2.88E-01
         1.15E-01 -5.91E-02 -1.07E-01  1.53E-01  1.89E-02
 
 OM44
+       -4.23E-02 -3.56E-02  1.26E-01  1.12E-01 -3.93E-02  1.15E-02  3.65E-02  2.38E-02  7.63E-02  2.29E-02  3.09E-02  1.74E-01
         -3.49E-02  1.71E-02 -6.41E-02  4.69E-02  2.80E-02  2.69E-02  1.12E-01 -1.99E-02 -1.20E-02 -2.06E-02  6.19E-02 -7.53E-02
         6.36E-02 -3.95E-02  1.16E-02 -3.04E-03  3.76E-02  5.84E-02
 
 OM45
+       -4.45E-02  4.64E-02  4.92E-02  1.84E-02 -5.74E-03 -1.52E-03  7.89E-03  6.98E-02  2.56E-02 -1.76E-02 -2.99E-02  6.00E-02
          6.62E-02  6.76E-03  2.89E-02  1.47E-02  2.30E-02  1.39E-02  4.65E-02  8.19E-02  1.95E-02 -1.97E-02 -2.40E-02 -2.50E-02
         7.29E-03 -3.29E-02 -8.99E-03  2.71E-02 -5.55E-03 -7.09E-02  2.75E-02
 
 OM46
+        4.61E-02  2.29E-02  9.09E-04 -2.48E-02  1.86E-02  6.24E-02  2.73E-02  1.97E-02  2.40E-03  6.07E-03  1.41E-02 -1.78E-02
         -1.97E-02  2.32E-02  2.35E-02  4.39E-02 -4.09E-03 -3.23E-02  3.07E-03  2.26E-02  4.84E-02  3.87E-04 -2.96E-02  4.58E-02
        -2.49E-02  3.01E-02 -3.39E-03  1.35E-04 -3.09E-02  7.45E-02 -1.14E-01  2.93E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.31E-02  7.50E-03 -1.55E-02  2.96E-03 -2.64E-03 -1.83E-02 -3.33E-03  1.09E-02  5.64E-03 -3.60E-02 -2.07E-02 -8.53E-04
          3.16E-02 -3.51E-02  5.21E-02 -1.05E-02 -6.60E-02  1.71E-02 -1.96E-01  3.46E-02  2.16E-02  1.26E-01 -7.22E-02  4.42E-02
         3.74E-02  1.60E-02  2.47E-02 -1.52E-02 -2.28E-02 -2.85E-01  5.82E-02  4.78E-02  3.02E-02
 
 OM48
+        6.06E-03  5.02E-04  1.41E-02  2.54E-03  3.00E-03  2.94E-03  3.30E-02  5.44E-03  1.46E-03 -1.27E-02  2.02E-02  2.68E-01
          9.23E-03  1.38E-02 -6.66E-03  9.42E-02  3.37E-02  4.73E-02  2.14E-01 -1.91E-02 -1.86E-02  1.22E-02  1.56E-01  2.48E-02
         1.37E-01  7.24E-03  3.95E-02 -3.74E-02  2.77E-02  2.00E-01 -5.41E-02 -8.24E-02  1.60E-01  2.79E-02
 
 OM55
+       -3.41E-02  3.24E-02 -2.44E-02 -1.74E-02 -4.14E-02 -2.28E-02  2.09E-02 -1.34E-03  5.89E-02 -5.09E-02 -6.66E-03 -2.20E-02
          1.20E-01 -3.64E-03  3.31E-02 -9.55E-03 -1.74E-02 -2.45E-02  2.44E-02  7.04E-02 -2.03E-02  1.74E-02 -5.27E-02  1.28E-02
        -1.34E-02 -2.79E-02 -1.55E-02 -7.30E-02 -3.59E-02  2.81E-02 -7.11E-02  2.28E-02 -3.68E-03  2.74E-02  4.05E-02
 
 OM56
+       -2.90E-02  4.24E-02  4.54E-03  1.87E-02 -4.67E-02 -2.83E-02 -4.00E-02 -1.05E-02 -1.37E-02  1.36E-03  1.41E-02 -5.02E-02
         -7.77E-02  4.64E-02  1.78E-02  5.12E-02 -5.95E-03 -3.11E-03  1.70E-02  2.33E-02  7.37E-02  1.31E-02  1.26E-02 -6.58E-03
        -9.14E-03  1.29E-02  3.13E-03  2.57E-02  3.15E-02 -3.26E-02  6.25E-02 -2.95E-02  7.37E-03 -3.24E-02 -2.14E-01  2.71E-02
 
 OM57
+        2.23E-02 -5.56E-02 -2.80E-02 -2.71E-02  4.52E-02 -7.55E-02 -3.79E-02 -2.91E-02  2.00E-02  2.33E-03  8.63E-03 -2.66E-02
          1.53E-02  7.51E-03  9.90E-02  2.07E-02 -1.55E-02 -3.52E-02  2.31E-02 -5.79E-02  5.25E-02  8.66E-03 -2.74E-02 -6.93E-04
        -4.28E-02  4.79E-02  1.09E-02  4.09E-02  1.22E-02  2.77E-02 -1.79E-01  3.84E-02 -7.79E-02 -6.17E-02  1.17E-01  6.43E-02
          2.53E-02
 
 OM58
+       -2.82E-02 -2.04E-02  1.73E-02 -8.83E-03 -7.40E-04  1.31E-02  9.91E-03  1.96E-02  2.78E-02  4.97E-02  4.01E-02 -2.55E-02
          2.19E-01 -4.18E-02  5.16E-02  5.20E-02  3.46E-02 -5.51E-03 -2.37E-02  2.11E-01 -1.58E-02  1.38E-02  8.57E-02  9.44E-03
        -2.53E-02  1.31E-01  2.36E-02  3.01E-02  8.01E-03 -3.69E-02  8.56E-02  9.06E-03  1.83E-02 -1.15E-02 -6.03E-02 -1.16E-01
          2.08E-01  2.28E-02
 
 OM66
+        1.51E-02 -5.79E-02  2.89E-02 -2.07E-02  8.46E-03  2.20E-02  2.66E-02 -4.31E-03  1.50E-02  2.53E-02 -3.86E-02 -1.65E-02
          2.90E-02 -9.54E-03 -1.19E-02  1.20E-02  4.54E-03 -5.07E-02 -3.57E-04 -2.59E-02  3.23E-03  4.65E-03  3.18E-02  2.00E-04
         2.68E-02 -6.72E-03  5.99E-02 -3.45E-02 -3.66E-02  3.19E-02  1.72E-02  2.02E-02  1.43E-02  2.69E-02 -2.24E-02 -2.67E-01
          1.01E-02  4.93E-02  5.55E-02
 
 OM67
+        3.55E-02 -2.97E-02 -4.87E-02  2.29E-02 -2.95E-02 -6.76E-02 -1.17E-02  4.97E-03 -4.11E-02 -2.23E-02 -1.56E-02 -3.71E-02
          1.10E-02  3.72E-03 -2.42E-02 -1.15E-02 -1.12E-02 -1.65E-02  4.53E-02  1.44E-02 -1.62E-01  2.93E-03 -2.23E-02  1.46E-03
        -7.10E-02 -3.97E-03  7.77E-02  8.30E-03 -3.51E-02 -6.76E-03  2.88E-02 -1.76E-01  1.54E-02  5.32E-02 -8.53E-03  4.57E-02
         -1.21E-01 -3.15E-02  6.66E-02  2.87E-02
 
 OM68
+        1.95E-03  2.56E-02 -2.10E-02  3.93E-02  3.49E-02 -1.05E-02  5.99E-03  3.32E-02 -7.16E-02 -5.58E-03 -1.12E-02  5.75E-02
         -1.95E-02  1.79E-01 -5.02E-02 -1.06E-01 -1.16E-02  2.45E-02  1.19E-02 -3.65E-02  2.59E-01 -3.22E-02 -8.63E-02  3.28E-02
         2.09E-03 -2.41E-02  1.51E-01  2.93E-02  1.41E-03  3.80E-02  1.18E-02  1.16E-01  1.11E-02 -1.27E-02 -1.12E-02  4.90E-02
         -2.21E-02 -1.19E-01 -2.78E-01  1.19E-01  2.67E-02
 
 OM77
+       -2.93E-02 -1.02E-02 -6.68E-02 -3.55E-03 -8.83E-03 -5.07E-02 -2.59E-02 -3.54E-02  3.82E-03  1.64E-02 -1.43E-02 -3.31E-03
         -3.74E-02  2.92E-03  2.48E-02  2.72E-02  3.94E-02  3.35E-02  4.93E-02 -4.66E-02 -3.53E-02 -2.26E-01  1.83E-02  3.22E-02
        -8.50E-02 -1.59E-02 -1.91E-02  6.50E-02  1.78E-02  5.13E-02 -1.72E-02 -1.94E-02 -3.20E-01 -9.38E-02  1.37E-02 -2.76E-02
          8.93E-02  8.58E-03  1.45E-02  6.72E-02  2.71E-03  4.66E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -3.12E-02 -1.51E-02 -2.49E-02  1.23E-02 -2.27E-02  4.12E-02 -1.44E-03  1.57E-02  7.57E-03  1.48E-02 -9.68E-03 -8.50E-02
          7.51E-03 -2.35E-02  1.87E-01  4.81E-02 -3.24E-02  2.30E-02 -4.39E-02  2.43E-02  1.83E-02  1.42E-01  1.34E-02  2.45E-02
        -2.99E-02 -4.03E-03 -3.29E-02  1.46E-01  1.06E-01 -3.90E-02  7.66E-06 -1.92E-02  2.03E-02 -1.54E-01  1.21E-03  1.10E-02
          1.10E-02  7.58E-02 -2.93E-02 -1.15E-01 -4.61E-02  3.21E-01  2.51E-02
 
 OM88
+       -1.84E-02  9.27E-03  8.74E-03 -2.32E-02  1.18E-02  1.26E-02  1.84E-02  1.50E-02  7.20E-02  9.37E-02  6.72E-02  8.58E-03
         -1.68E-02 -1.42E-02  7.99E-02  3.24E-01  7.38E-02  7.41E-02  9.17E-02  3.70E-02 -7.96E-02  6.28E-02  3.94E-01  4.95E-02
        -2.24E-02 -2.65E-02 -2.50E-02  3.84E-02  2.48E-01  7.27E-02 -3.76E-02 -4.57E-02 -2.46E-02  1.85E-01 -8.39E-03  5.53E-02
         -1.89E-02 -1.79E-02  1.83E-02 -4.46E-02 -2.45E-01  4.88E-02  3.29E-01  3.96E-02
 
 SG11
+       -5.10E-02  3.69E-02 -1.55E-02 -2.63E-02  1.65E-02 -2.79E-02  1.79E-02 -1.73E-02 -6.92E-03 -5.42E-02 -2.05E-02 -6.37E-02
         -1.70E-02 -5.42E-02  2.98E-02  7.75E-03 -1.73E-02 -1.85E-02  2.43E-03  2.14E-02  8.01E-03  2.81E-02  3.09E-04 -4.70E-02
        -1.02E-02  4.01E-02 -1.62E-02 -1.24E-02 -3.14E-02 -2.03E-02 -5.11E-02  2.21E-02  6.35E-04 -1.89E-02  2.11E-03  3.91E-02
          3.12E-02  1.75E-02  1.44E-03  1.10E-02  1.53E-02 -2.76E-02  1.78E-02  9.16E-03  6.40E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        3.63E-02 -7.58E-03  1.42E-02 -6.18E-02  1.52E-02 -4.17E-03  5.78E-03  1.25E-02 -3.89E-02  1.77E-03 -1.25E-02 -5.37E-03
          2.24E-02 -4.42E-03 -2.37E-02 -3.18E-02 -1.65E-02  2.04E-02 -3.02E-02  3.30E-02  1.69E-02  1.62E-02 -8.46E-04  8.91E-03
         1.96E-02 -3.70E-02  1.09E-01 -4.85E-03 -4.79E-02 -1.43E-04 -2.31E-02  2.57E-02  2.99E-02 -1.29E-02 -2.05E-02 -4.70E-02
         -2.51E-02  3.57E-02 -4.51E-03 -1.39E-02  8.64E-02 -1.50E-02 -3.00E-02 -8.59E-03 -2.38E-03  0.00E+00  1.21E-03
 
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
+        2.15E+02
 
 TH 2
+        6.37E+01  2.91E+02
 
 TH 3
+       -1.81E+01  3.55E+00  4.06E+02
 
 TH 4
+       -7.28E+00 -6.33E+00 -6.27E-02  2.45E+02
 
 TH 5
+       -3.56E+01 -3.90E+01 -3.93E+00  1.34E+01  2.79E+02
 
 TH 6
+       -2.56E+00 -2.24E+01 -3.35E+01 -2.46E+01  5.52E+01  2.34E+02
 
 TH 7
+        2.96E+01  8.10E+01 -1.69E+01  6.32E+01 -4.44E+01 -3.18E+01  2.73E+02
 
 TH 8
+       -9.26E+01 -1.30E+02 -7.11E+01 -6.34E+01  4.60E+01  7.69E+01 -1.14E+02  3.82E+02
 
 OM11
+       -4.97E+00 -1.39E+01 -2.80E+01  9.56E+00  4.01E+00  8.10E-01 -1.54E+01  3.57E+00  3.94E+02
 
 OM12
+       -7.33E+00 -4.69E+01 -1.35E+01 -8.19E+00  2.00E+01 -3.98E+00 -1.23E+01  3.63E+00  2.33E+02  1.64E+03
 
 OM13
+       -2.50E-02 -1.89E+01 -4.07E+01 -3.44E+00 -4.80E+00  1.82E+01  6.20E-01  6.23E+01 -2.58E+01  7.08E+01  2.56E+03
 
 OM14
+       -1.51E+01 -1.92E+01 -9.38E+00 -1.23E+01 -6.83E+00  5.52E+00 -1.98E+01  4.10E+01 -7.27E+01 -1.63E+01 -1.93E+00  1.20E+03
 
 OM15
+       -6.99E+00  3.21E+01  1.70E+01 -1.82E+01  2.43E+01  1.16E+01  3.36E+00 -3.01E+01 -1.16E+02 -1.76E+02 -5.29E+01  6.92E+01
          1.49E+03
 
 OM16
+       -7.86E+00  1.69E+01  1.40E+00  1.37E+01 -9.37E+00  4.38E+00  1.15E+01  5.80E-01 -7.17E+01 -1.47E+02 -1.28E+02 -9.47E+01
          1.50E+02  1.28E+03
 
 OM17
+       -2.40E+01 -7.97E+01 -2.82E+01  1.49E+00 -1.05E+01  1.88E+01  9.77E+00  3.98E+01  5.24E+01  3.42E+02 -4.64E+01  2.48E+02
         -1.48E+02 -1.58E+02  1.58E+03
 
 OM18
+        1.40E+01  7.26E+01  2.90E+01 -2.90E+01 -1.11E+01  3.15E+00  2.54E-01 -3.02E+01 -3.70E+02 -5.44E+02 -2.17E+02 -1.22E+02
          2.25E+02  3.70E+02 -5.47E+02  2.07E+03
 
 OM22
+        9.59E+00  4.78E+01  1.28E+01 -1.14E+01 -2.44E+00 -2.61E+01  7.00E+00 -2.87E+01  1.08E+01  4.05E+02  6.32E+01 -2.09E+01
          1.07E+01 -3.57E+01  6.24E+01 -1.22E+02  8.06E+02
 
 OM23
+        2.58E+00 -3.69E+01 -3.62E+01  1.47E+00  2.20E+01 -4.98E+00  1.39E+01 -5.58E+00 -2.96E+01 -2.30E+01  4.16E+02  1.97E+01
         -2.55E+01  1.90E+01 -7.85E+01  3.89E+01  2.45E+02  3.53E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.75E+00  7.85E+01 -7.56E+01 -4.25E+00 -2.73E+01 -8.86E+00  3.15E+00 -3.15E+01 -8.93E+00  6.76E+01  4.16E+01  2.78E+02
          4.08E+00 -6.49E+00  5.58E+01  6.41E+01  3.94E+01  7.26E+01  1.77E+03
 
 OM25
+        8.74E+00  1.64E+01 -6.37E+00 -5.45E+00  8.00E+00  7.21E-01  1.78E+01 -2.57E+01 -1.66E+01 -2.17E+02 -7.64E+01  5.00E+01
          3.95E+02  8.13E+01 -1.56E+01  7.89E+00 -2.19E+02 -8.28E+01  6.48E+01  2.27E+03
 
 OM26
+       -2.97E+00 -4.34E+01 -7.95E+00  2.71E+01  3.95E+01  1.08E+01 -1.01E+01  4.44E+01 -1.34E+01 -1.24E+02 -8.11E+01 -1.83E+01
         -2.87E+01  2.89E+02  3.13E+01  2.54E+01 -1.96E+02 -4.23E+01 -9.91E+01  4.12E+02  1.84E+03
 
 OM27
+       -4.42E+01 -1.48E+02 -3.18E+00 -1.41E+01  1.33E+01 -9.78E+00 -4.15E+01  6.25E+01 -5.10E+01  1.91E+02 -2.45E+01  1.30E+02
          1.54E+01 -2.08E+01  3.48E+02 -1.30E+02  5.78E+02 -5.21E+01  5.11E+02 -2.28E+02 -1.59E+02  2.15E+03
 
 OM28
+        2.98E+01  5.62E+01 -4.54E+01  5.62E+01 -8.01E+00  1.86E+01  1.31E+01 -9.66E+00 -2.60E+01 -6.57E+02  3.14E+01 -5.22E+01
          7.43E+00  1.34E+02 -2.05E+02  5.24E+02 -8.95E+02 -4.08E+02 -3.31E+02  2.93E+02  5.76E+02 -1.06E+03  3.12E+03
 
 OM33
+       -1.83E+01 -2.68E+01  7.69E+01  3.75E+01  9.01E+00 -1.41E+01  1.71E+01 -8.83E+00  1.59E+00 -6.87E+01 -1.62E+02 -1.80E+01
         -1.59E+01  6.08E+01 -1.37E+01  2.24E+01 -4.20E+01  1.77E+02  3.75E+00  3.53E+01  2.32E+01 -2.90E+01  1.51E+01  1.22E+03
 
 OM34
+       -1.32E+00  4.27E+01 -1.14E+02 -3.04E+01  1.41E+00  3.65E+01  3.52E+00  1.29E+01 -5.45E+00 -4.30E+01 -1.31E+02 -6.16E+01
         -2.08E+01 -7.40E+01  2.46E+01  1.36E+02 -3.79E+01 -9.67E+01  7.32E+01 -2.15E+00  9.97E+00 -2.30E+01  4.55E+01  1.46E+02
         2.36E+03
 
 OM35
+       -4.96E+00 -1.45E+01  2.05E+01 -3.05E+01  4.40E+01  6.97E+00 -3.58E+01 -3.76E+00  2.66E+01 -4.06E+01 -3.47E+02 -8.05E+01
         -6.53E+01 -5.04E+00 -1.02E+02 -1.59E+01 -1.36E+02 -5.61E+02 -9.90E+01  8.93E+01  1.38E+02 -1.13E+02  1.65E+02 -1.67E+01
         1.33E+02  3.08E+03
 
 OM36
+        2.27E+01  3.96E-01 -2.19E+01 -1.03E+01  2.83E+01  1.20E+01 -2.73E+01  1.03E+01  3.37E+01  8.44E+01 -1.66E+02 -3.09E+01
         -8.35E+01 -8.49E+01  4.65E+01 -1.06E+02 -2.15E+01 -3.10E+02 -3.14E+00 -4.38E+01  9.46E+01 -1.28E+01 -4.69E+01 -2.37E+02
        -1.98E+02  4.91E+02  2.53E+03
 
 OM37
+        2.93E+00  1.32E+00  3.99E+01 -1.17E+01 -6.28E+00  4.62E+01 -4.25E+00 -6.15E+00 -3.93E+01 -7.48E+01  1.20E+02  2.51E+01
          8.24E+01  6.34E+01 -1.26E+02  8.40E+01 -4.13E+01  6.81E+02 -4.65E+01  1.05E+02 -1.85E+01 -1.12E+02  5.09E+01  3.29E+01
         4.57E+02 -4.04E+02 -2.99E+02  2.94E+03
 
 OM38
+       -2.81E+00  2.67E+01  2.28E+01  1.27E+01 -8.30E+00 -3.37E+01 -4.49E+00 -2.61E+01  5.06E+01  6.32E+01 -8.33E+02  2.48E+00
          5.34E+01 -1.28E+02  5.97E+01 -2.02E+02 -5.12E+01 -1.22E+03  1.34E+01  5.89E+01 -7.86E+01  7.23E+01  6.74E+01 -6.48E+02
        -4.81E+02  5.26E+02  6.63E+02 -8.02E+02  4.26E+03
 
 OM44
+        2.22E+01  1.73E+01 -4.23E+01 -3.81E+01  1.20E+01  9.80E+00 -6.54E+00 -1.22E+00 -2.93E+01 -2.54E+01 -2.36E+01 -6.84E+01
          2.35E+01  2.26E+01  1.48E+01  3.02E+01 -6.62E+00 -2.25E+01 -1.14E+00  1.52E+01  2.33E+01 -1.24E+01  2.10E+01  4.31E+01
        -3.60E+00  4.79E+01  6.57E+00 -1.83E+01 -3.55E+01  3.77E+02
 
 OM45
+        3.08E+01 -1.64E+01 -1.85E+01  7.11E+00 -5.15E+00 -2.37E+00  5.72E+00 -4.28E+01  6.54E+00  2.61E+01  4.92E+01 -1.59E+02
         -9.89E+01 -1.58E+01 -8.23E+01 -3.57E+01 -2.71E+01 -5.74E+01 -1.92E+02 -1.19E+02 -1.28E+01 -6.56E+01  9.08E+01  1.98E+01
        -2.07E+01  1.28E+02  6.10E+01 -9.14E+01  2.31E+01  2.71E+01  1.52E+03
 
 OM46
+       -3.16E+01 -2.39E+01  1.67E+01  2.74E+01 -3.58E+00 -3.77E+01 -9.80E+00 -8.47E+00  2.78E+01  2.28E+01 -1.38E+01  1.23E+00
          1.98E+00 -2.14E+01 -2.95E+01 -1.22E+02 -4.67E+00  3.71E+01 -1.05E+02 -4.82E+01  3.73E+01 -4.32E+01  2.70E+01 -6.26E+01
         7.45E+00 -3.77E+01  4.45E+01 -1.22E+01  1.13E+02 -9.02E+01  1.79E+02  1.33E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.48E+01  3.78E+01 -2.43E+01 -2.68E+01  3.87E+00  3.30E+01  1.94E+00  6.04E-01 -3.65E+01  1.32E+01  4.22E+01  4.74E+01
          1.90E+01  5.85E+01 -3.16E+01  7.40E+01  1.57E+01 -6.66E+01  3.71E+02  2.04E+00 -4.42E+00  5.55E+01  5.15E+01 -4.91E+01
         1.62E+01 -8.76E+00 -1.14E+01  9.95E+00  3.00E+01  2.42E+02 -1.12E+02 -1.82E+02  1.57E+03
 
 OM48
+       -1.25E+01 -3.18E+01  5.16E+01  2.04E+01  5.28E+00 -1.87E+01 -1.02E+01 -5.15E+00  8.17E+01  1.12E+02  1.01E+01 -4.13E+02
         -4.11E+01 -1.22E+01 -1.05E+02 -1.31E+02  3.63E+01 -5.35E+01 -5.29E+02  4.26E+00 -1.67E+01 -1.57E+02 -9.15E+01 -5.58E+01
        -2.80E+02 -1.18E+01 -1.82E+00 -4.60E+01  1.36E+02 -1.82E+02  2.02E+02  2.21E+02 -5.05E+02  1.92E+03
 
 OM55
+        1.27E+01 -2.75E+01  6.61E+00  5.09E+00  2.81E+01  1.36E+01 -1.88E+01  1.18E+01 -1.57E+01  5.04E+01  1.84E+01  2.62E+01
         -1.62E+02 -3.18E+01 -5.78E+00 -3.52E+00  3.27E+00  5.14E+01 -4.18E+01 -1.90E+02 -1.71E+01 -1.81E+01  3.38E+01 -2.16E+01
         1.69E+01  2.51E+01  1.81E+01  1.02E+02  1.56E+01 -1.07E+01  2.95E+01 -1.63E+01 -1.08E+01 -3.20E+01  7.16E+02
 
 OM56
+        1.16E+01 -3.30E+01 -1.83E+01 -8.12E+00  4.22E+01  1.97E+01 -2.23E+00  2.21E+01  1.48E+01  5.74E+01  3.56E+01  9.45E+01
         -2.67E+00 -1.34E+02  4.44E+01 -1.29E+02  5.01E+01  9.54E+01 -9.96E+00 -1.81E+02 -2.33E+02  5.69E+01 -1.51E+02 -3.58E+00
        -1.68E+01 -8.68E+01 -6.72E+01  2.99E+01 -3.10E+01  3.74E+00 -1.48E+02 -1.16E+01 -2.03E+01  2.15E+01  2.88E+02  1.72E+03
 
 OM57
+       -8.20E+00  6.85E+01  1.49E+01  1.52E+01 -3.27E+01  3.60E+01  4.46E+01 -1.81E+01 -9.85E+00 -7.26E+01  4.00E+01 -5.62E+01
          1.07E+02  2.07E+01 -2.20E+02  7.89E+01 -3.79E+01  5.00E+01 -1.06E+02  2.42E+02 -2.05E+00 -2.30E+02  2.03E+02  2.89E+01
         5.09E+01 -2.39E+01 -6.98E+01 -3.87E+01 -6.86E+01 -2.13E+01  3.40E+02  3.28E+01  2.26E+01  1.44E+02 -2.06E+02 -3.06E+02
          1.96E+03
 
 OM58
+        2.87E+01 -1.14E+01 -2.05E+01  4.50E-01 -9.15E+00 -1.84E+01 -6.61E+00 -7.12E+00  1.37E+01  4.71E+01 -3.81E+01  5.13E+01
         -4.99E+02 -7.10E+01  8.73E+01 -1.87E+02  9.42E+01  1.16E+02  1.05E+02 -6.88E+02 -1.86E+02  1.99E+02 -4.46E+02 -3.18E+00
         5.23E+01 -3.56E+02 -1.06E+02  2.42E+00 -1.22E+02  1.40E+01 -2.30E+02 -5.88E+01  7.18E+00 -1.09E+02  2.31E+02  3.96E+02
         -6.47E+02  2.63E+03
 
 OM66
+        2.84E+00  1.27E+01 -7.63E+00 -3.54E-01 -7.22E+00 -8.28E+00 -1.50E-01 -6.43E+00 -6.76E-01 -9.05E+00  5.86E+01  2.88E+01
         -9.20E+00 -6.46E+01  1.26E+01 -2.08E+01  2.21E+01  8.20E+01  2.13E+01 -1.02E+01 -1.40E+02  1.25E+01 -7.03E+01 -1.03E+01
        -2.25E+01 -3.70E+01 -1.12E+02  4.19E+01 -1.93E+01 -1.82E+01 -4.89E+01 -5.62E+01 -2.18E+01 -1.54E+01  5.24E+01  2.54E+02
         -5.53E+01  6.18E+01  4.15E+02
 
 OM67
+       -2.56E+01  2.09E+01  2.79E+01  1.11E+00  2.93E+01  3.08E+01  3.29E+00  5.85E+00  2.65E+01  5.89E+00 -1.21E+01  4.14E+01
         -2.18E+01  7.81E+01 -3.25E+01 -4.18E+00 -5.34E+01  2.48E+01 -1.15E+02  4.94E+01  3.78E+02 -2.00E+02  2.31E+02  1.53E+01
         1.32E+02  2.13E+01 -7.68E+01 -1.08E+01 -1.98E+00 -9.88E+00  3.94E+01  2.86E+02 -9.31E+01 -2.70E+00 -2.94E+01 -1.79E+02
          2.59E+02 -1.18E+02 -1.26E+02  1.50E+03
 
 OM68
+        1.61E+01  2.53E-01  1.57E+01 -3.37E+01 -4.99E+01 -1.30E+01 -4.07E+00 -3.83E+01  4.27E+01  3.28E+01  8.80E+01 -3.96E+01
         -4.36E+01 -3.81E+02  7.83E+01 -6.84E+01  8.08E+01 -6.20E+00  1.42E+01 -1.29E+02 -6.82E+02  1.38E+02 -2.97E+02 -3.27E+01
         4.60E+01 -7.16E+01 -3.69E+02 -1.12E+01 -1.65E+02 -4.67E+01 -6.76E+01 -2.50E+02 -2.20E+01 -5.28E+01  4.54E+01  1.78E+02
         -7.70E+01  3.75E+02  3.24E+02 -4.13E+02  2.16E+03
 
 OM77
+       -4.15E+00 -3.89E+01  1.36E+01 -7.43E+00  1.17E+01  3.09E+01 -8.49E+00  3.47E+01 -1.43E+01  2.02E+01 -5.24E+00  9.69E+00
          3.91E+01 -4.85E+00  1.07E+02 -6.76E+01  6.84E+01 -1.17E+02  9.40E+01  2.38E+01  1.51E+01  4.03E+02 -1.98E+02 -4.05E+01
         6.56E+01  2.31E+01  3.15E+01 -2.81E+01  6.28E+01  1.82E+01 -3.57E+01 -3.95E+01  3.36E+02 -7.42E+01 -4.21E+00  4.35E+01
         -1.41E+02  5.94E+01 -1.07E+01 -1.52E+02  2.32E+01  7.01E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.40E+01  1.15E+02  1.78E+01  1.83E+00  1.79E+00 -5.56E+01  1.96E+01 -7.78E+01  8.25E+00 -1.07E+02  8.81E+01 -5.07E+01
          2.03E+01  8.39E+01 -4.99E+02  3.36E+02 -1.46E+02  2.14E+01 -1.48E+02  4.13E+01  3.03E+01 -7.65E+02  6.42E+02  1.40E+01
        -1.16E+02  7.21E+01  2.20E+01 -2.76E+02 -2.62E+01 -1.58E+01  9.20E+01  1.25E+02 -3.32E+02  4.90E+02 -2.36E+01 -6.03E+01
          2.60E+02 -3.61E+02 -7.06E-02  3.45E+02 -2.24E+02 -6.03E+02  2.74E+03
 
 OM88
+       -4.95E+00 -4.58E+01  4.21E+00 -1.48E+00 -1.02E+01  3.03E+00 -6.11E+00  7.97E+00  3.06E+01  9.52E+01  1.25E+01  5.85E+01
         -4.22E+01 -1.20E+02  1.47E+02 -5.32E+02  1.47E+02  5.98E+01  1.13E+00 -9.55E+01 -1.03E+02  2.55E+02 -8.38E+02  2.36E+01
         1.06E+02 -6.50E+00 -4.01E+01  5.67E+01 -4.02E+02 -2.13E+01  6.08E+00 -5.64E+00  4.77E+01 -2.63E+02  1.18E+00 -2.42E+00
         -6.16E+01  2.72E+02  3.67E+01 -8.28E+01  4.21E+02  1.24E+02 -7.68E+02  1.23E+03
 
 SG11
+        8.56E+02 -1.22E+03  2.24E+02  3.75E+02 -1.19E+02  8.00E+02 -7.77E+02  7.68E+02  4.14E+02  4.09E+03  1.35E+03  2.24E+03
          9.10E+02  2.89E+03 -4.08E+02 -1.19E+03  1.27E+03  1.99E+03 -4.28E+02 -1.57E+03  1.76E+02  1.08E+02 -1.75E+03  2.15E+03
        -7.30E+02 -3.23E+03  1.03E+03  1.68E+03  6.75E+02  4.24E+02  3.03E+03 -9.18E+02  9.54E+02  4.71E+02  4.82E+01 -2.35E+03
         -1.25E+03 -1.41E+03 -8.13E+02 -8.34E+02 -2.85E+03  1.51E+03 -1.86E+03 -4.16E+02  2.52E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -4.27E+02  1.79E+02 -1.40E+02  9.73E+02 -6.87E+01 -8.63E+01  2.57E+02 -2.77E+02  5.74E+02  4.79E+02 -7.28E+01  4.73E+01
         -6.90E+02  6.72E+02 -1.31E+01  4.79E+02  1.09E+02 -1.33E+03  4.25E+02 -1.15E+03  6.40E+01 -7.30E+02  9.62E+02 -2.48E+02
        -8.08E+02  1.36E+03 -3.34E+03 -7.51E+02  2.82E+03 -3.45E+02  1.11E+03  1.31E+02 -1.08E+03  1.30E+03  5.49E+02  1.27E+03
          1.32E+03 -1.96E+03 -5.14E+01  1.26E+03 -3.31E+03 -5.22E+02  2.41E+03 -1.67E+03  3.20E+03  0.00E+00  7.17E+05
 
 Elapsed postprocess time in seconds:     0.00
 #CPUT: Total CPU Time in Seconds,     7363.045
Stop Time: 
Thu 11/03/2016 
05:50 PM

Tue 04/19/2016 
02:18 PM
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

$EST METHOD=NUTS NBURN=1000 NITER=2000 PRINT=1 OLKJDF=8.0
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  121) INTERACTION IS IMPLIED WITH EM/BAYES ESTIMATION METHODS
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       19 APR 2016
Days until program expires :5153
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.4.0 alpha9 (nm74a9)
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
1DOUBLE PRECISION PREDPP VERSION 7.4.0 alpha9 (nm74a9)

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
 #METH: NUTS Bayesian Analysis
 
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
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY(ATOL):  -100
 FINAL ETA RE-EVALUATION (FNLETA):          ON
 EXCLUDE NON-INFLUENTIAL (NON-INFL.) ETAS
       IN SHRINKAGE (ETASTYPE):             NO
 NON-INFL. ETA CORRECTION (NONINFETA):      OFF
 RAW OUTPUT FILE (FILE): example6hmto5.ext
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
 MASS MATRIX BLOCKING TYPE:                              B
 MODEL PARAMETERS TRASNFORMED BY MASS MATRIX (NUTS_TRANSFORM=0)
 POWER TERM WEIGHTING FOR MASS MATRIX ACCUM. (KAPPA):   1.00000000000000
 NUTS SAMPLE ACCEPTANCE RATE (NUTS_DELTA):                   0.800000000000000
 NUTS GAMMA SETTING (NUTS_GAMMA):                            5.000000000000000E-02
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 8.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000
 NUTS WARMUP METHOD (NUTS_TEST):       NO
 NUTS MAXIMAL DEPTH SEARCH (NUTS_MAXDEPTH):                 
 NUTS STAGE I WARMUP ITERATIONS (NUTS_INIT):       7.500000000000000E-02
 NUTS STAGE II base WARMUP ITERATIONS (NUTS_BASE): 2.500000000000000E-02
 NUTS STAGE III FINAL ITERATIONS (NUTS_TERM): 5.000000000000000E-02
 INITIAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPITER): 1
 INTERVAL ITERATIONS FOR STEP NUTS SIZE ASSESSMENT (NUTS_STEPINTER):0
 ETA PARAMETERIZATION (NUTS_EPARAM):0
 OMEGA PARAMETERIZATION (NUTS_OPARAM):1
 SIGMA PARAMETERIZATION (NUTS_SPARAM):1
 NUTS REGULARIZING METHOD (NUTS_REG): 0.00000000000000

 
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
 iteration        -1000 MCMCOBJ=    318797.919935581     
 iteration         -999 MCMCOBJ=    318797.919935581     
 iteration         -998 MCMCOBJ=    26696.4792106134     
 iteration         -997 MCMCOBJ=    22445.1652668738     
 iteration         -996 MCMCOBJ=    22004.4361794481     
 iteration         -995 MCMCOBJ=    21015.3180262427     
 iteration         -994 MCMCOBJ=    10465.6725854369     
 iteration         -993 MCMCOBJ=    7776.73791498297     
 iteration         -992 MCMCOBJ=    5146.30916203652     
 iteration         -991 MCMCOBJ=    1087.14664620185     
 iteration         -990 MCMCOBJ=    76.8259398296835     
 iteration         -989 MCMCOBJ=   -602.216472650526     
 iteration         -988 MCMCOBJ=   -841.609937496315     
 iteration         -987 MCMCOBJ=   -1393.27818126944     
 iteration         -986 MCMCOBJ=   -1942.39627402554     
 iteration         -985 MCMCOBJ=   -2111.08942002015     
 iteration         -984 MCMCOBJ=   -2529.48581785904     
 iteration         -983 MCMCOBJ=   -2677.52102212757     
 iteration         -982 MCMCOBJ=   -3171.75861446130     
 iteration         -981 MCMCOBJ=   -4101.71077127439     
 iteration         -980 MCMCOBJ=   -4611.21909134582     
 iteration         -979 MCMCOBJ=   -4840.37348971304     
 iteration         -978 MCMCOBJ=   -4851.34228729168     
 iteration         -977 MCMCOBJ=   -5356.64816545213     
 iteration         -976 MCMCOBJ=   -5399.63254684767     
 iteration         -975 MCMCOBJ=   -5498.20147008450     
 iteration         -974 MCMCOBJ=   -6121.96897417487     
 iteration         -973 MCMCOBJ=   -6225.28203591423     
 iteration         -972 MCMCOBJ=   -6453.82446862880     
 iteration         -971 MCMCOBJ=   -6531.32259675142     
 iteration         -970 MCMCOBJ=   -6540.87789624372     
 iteration         -969 MCMCOBJ=   -6578.78178637305     
 iteration         -968 MCMCOBJ=   -6543.96336820499     
 iteration         -967 MCMCOBJ=   -6570.43326480626     
 iteration         -966 MCMCOBJ=   -6609.44465761856     
 iteration         -965 MCMCOBJ=   -6629.71490029530     
 iteration         -964 MCMCOBJ=   -6633.92706824514     
 iteration         -963 MCMCOBJ=   -6631.92408304434     
 iteration         -962 MCMCOBJ=   -6645.95365155964     
 iteration         -961 MCMCOBJ=   -6628.89249921099     
 iteration         -960 MCMCOBJ=   -6625.99229870168     
 iteration         -959 MCMCOBJ=   -6600.15879262027     
 iteration         -958 MCMCOBJ=   -6596.08277474980     
 iteration         -957 MCMCOBJ=   -6581.13263181803     
 iteration         -956 MCMCOBJ=   -6606.45516431302     
 iteration         -955 MCMCOBJ=   -6602.74299272396     
 iteration         -954 MCMCOBJ=   -6565.26812062369     
 iteration         -953 MCMCOBJ=   -6618.42825569196     
 iteration         -952 MCMCOBJ=   -6619.14618459418     
 iteration         -951 MCMCOBJ=   -6622.09411320955     
 iteration         -950 MCMCOBJ=   -6648.42763382011     
 iteration         -949 MCMCOBJ=   -6586.41720975245     
 iteration         -948 MCMCOBJ=   -6563.96722612363     
 iteration         -947 MCMCOBJ=   -6632.21684639220     
 iteration         -946 MCMCOBJ=   -6582.84002111346     
 iteration         -945 MCMCOBJ=   -6611.61110610602     
 iteration         -944 MCMCOBJ=   -6593.47249080378     
 iteration         -943 MCMCOBJ=   -6597.92962498368     
 iteration         -942 MCMCOBJ=   -6604.50643583809     
 iteration         -941 MCMCOBJ=   -6600.49020409117     
 iteration         -940 MCMCOBJ=   -6609.56159691851     
 iteration         -939 MCMCOBJ=   -6622.45071405402     
 iteration         -938 MCMCOBJ=   -6613.42798509424     
 iteration         -937 MCMCOBJ=   -6612.51943331382     
 iteration         -936 MCMCOBJ=   -6570.75037396438     
 iteration         -935 MCMCOBJ=   -6605.42536256468     
 iteration         -934 MCMCOBJ=   -6621.73412412042     
 iteration         -933 MCMCOBJ=   -6641.43234811984     
 iteration         -932 MCMCOBJ=   -6653.07475933214     
 iteration         -931 MCMCOBJ=   -6643.89781149554     
 iteration         -930 MCMCOBJ=   -6587.88733534005     
 iteration         -929 MCMCOBJ=   -6626.41806233522     
 iteration         -928 MCMCOBJ=   -6618.99687251799     
 iteration         -927 MCMCOBJ=   -6600.55800403208     
 iteration         -926 MCMCOBJ=   -6575.62167116438     
 iteration         -925 MCMCOBJ=   -6575.62167116438     
 iteration         -924 MCMCOBJ=   -6575.62167116438     
 iteration         -923 MCMCOBJ=   -6635.42264362690     
 iteration         -922 MCMCOBJ=   -6657.93276078995     
 iteration         -921 MCMCOBJ=   -6605.08544076811     
 iteration         -920 MCMCOBJ=   -6666.53873553204     
 iteration         -919 MCMCOBJ=   -6674.78767798306     
 iteration         -918 MCMCOBJ=   -6663.31932746543     
 iteration         -917 MCMCOBJ=   -6647.70371198932     
 iteration         -916 MCMCOBJ=   -6670.76374676672     
 iteration         -915 MCMCOBJ=   -6662.08792279870     
 iteration         -914 MCMCOBJ=   -6684.20097272396     
 iteration         -913 MCMCOBJ=   -6607.57969766023     
 iteration         -912 MCMCOBJ=   -6610.11303559947     
 iteration         -911 MCMCOBJ=   -6647.40401820575     
 iteration         -910 MCMCOBJ=   -6628.83817033568     
 iteration         -909 MCMCOBJ=   -6617.86880412567     
 iteration         -908 MCMCOBJ=   -6608.24718020885     
 iteration         -907 MCMCOBJ=   -6572.44115756605     
 iteration         -906 MCMCOBJ=   -6569.44122201815     
 iteration         -905 MCMCOBJ=   -6557.87521896366     
 iteration         -904 MCMCOBJ=   -6534.01316940450     
 iteration         -903 MCMCOBJ=   -6525.17052184886     
 iteration         -902 MCMCOBJ=   -6523.16802660315     
 iteration         -901 MCMCOBJ=   -6544.57519423494     
 iteration         -900 MCMCOBJ=   -6544.57519964417     
 iteration         -899 MCMCOBJ=   -6544.57520030132     
 iteration         -898 MCMCOBJ=   -6609.33908271236     
 iteration         -897 MCMCOBJ=   -6590.05909783279     
 iteration         -896 MCMCOBJ=   -6579.29921792866     
 iteration         -895 MCMCOBJ=   -6604.35370943410     
 iteration         -894 MCMCOBJ=   -6607.51362925802     
 iteration         -893 MCMCOBJ=   -6578.03964908586     
 iteration         -892 MCMCOBJ=   -6599.88412083569     
 iteration         -891 MCMCOBJ=   -6599.94267275125     
 iteration         -890 MCMCOBJ=   -6633.92480591380     
 iteration         -889 MCMCOBJ=   -6676.22129043629     
 iteration         -888 MCMCOBJ=   -6689.17207716901     
 iteration         -887 MCMCOBJ=   -6618.34824403887     
 iteration         -886 MCMCOBJ=   -6625.34870143941     
 iteration         -885 MCMCOBJ=   -6569.51883387363     
 iteration         -884 MCMCOBJ=   -6606.62478716956     
 iteration         -883 MCMCOBJ=   -6617.68403627398     
 iteration         -882 MCMCOBJ=   -6608.26649282519     
 iteration         -881 MCMCOBJ=   -6624.46288199001     
 iteration         -880 MCMCOBJ=   -6590.88618614811     
 iteration         -879 MCMCOBJ=   -6626.92521471546     
 iteration         -878 MCMCOBJ=   -6575.26755823085     
 iteration         -877 MCMCOBJ=   -6571.05069324685     
 iteration         -876 MCMCOBJ=   -6570.92748790408     
 iteration         -875 MCMCOBJ=   -6635.89852022241     
 iteration         -874 MCMCOBJ=   -6605.58778102885     
 iteration         -873 MCMCOBJ=   -6588.95513463071     
 iteration         -872 MCMCOBJ=   -6626.30794958941     
 iteration         -871 MCMCOBJ=   -6624.30491093856     
 iteration         -870 MCMCOBJ=   -6620.42058682032     
 iteration         -869 MCMCOBJ=   -6629.14337876913     
 iteration         -868 MCMCOBJ=   -6562.96392570027     
 iteration         -867 MCMCOBJ=   -6587.84513951468     
 iteration         -866 MCMCOBJ=   -6580.32264380219     
 iteration         -865 MCMCOBJ=   -6617.61786003468     
 iteration         -864 MCMCOBJ=   -6663.85289042742     
 iteration         -863 MCMCOBJ=   -6620.06168618316     
 iteration         -862 MCMCOBJ=   -6610.61190683203     
 iteration         -861 MCMCOBJ=   -6611.90272013165     
 iteration         -860 MCMCOBJ=   -6627.44032480156     
 iteration         -859 MCMCOBJ=   -6667.67175168723     
 iteration         -858 MCMCOBJ=   -6634.92628114443     
 iteration         -857 MCMCOBJ=   -6640.37801233569     
 iteration         -856 MCMCOBJ=   -6641.38291565526     
 iteration         -855 MCMCOBJ=   -6647.93088770130     
 iteration         -854 MCMCOBJ=   -6634.44682284813     
 iteration         -853 MCMCOBJ=   -6590.06014343121     
 iteration         -852 MCMCOBJ=   -6602.38485626455     
 iteration         -851 MCMCOBJ=   -6554.97590824468     
 iteration         -850 MCMCOBJ=   -6554.97590814961     
 iteration         -849 MCMCOBJ=   -6554.97590818952     
 iteration         -848 MCMCOBJ=   -6621.05672147185     
 iteration         -847 MCMCOBJ=   -6571.93278268967     
 iteration         -846 MCMCOBJ=   -6618.96900846651     
 iteration         -845 MCMCOBJ=   -6626.08700961453     
 iteration         -844 MCMCOBJ=   -6575.98798141652     
 iteration         -843 MCMCOBJ=   -6552.42193019273     
 iteration         -842 MCMCOBJ=   -6602.06070897444     
 iteration         -841 MCMCOBJ=   -6590.80972487690     
 iteration         -840 MCMCOBJ=   -6629.01103609719     
 iteration         -839 MCMCOBJ=   -6651.85519732095     
 iteration         -838 MCMCOBJ=   -6632.60399231705     
 iteration         -837 MCMCOBJ=   -6597.02077549603     
 iteration         -836 MCMCOBJ=   -6578.35149172973     
 iteration         -835 MCMCOBJ=   -6607.20587729595     
 iteration         -834 MCMCOBJ=   -6596.82724803756     
 iteration         -833 MCMCOBJ=   -6625.97708761993     
 iteration         -832 MCMCOBJ=   -6648.33057301458     
 iteration         -831 MCMCOBJ=   -6616.10630458453     
 iteration         -830 MCMCOBJ=   -6603.15105998839     
 iteration         -829 MCMCOBJ=   -6614.81353551363     
 iteration         -828 MCMCOBJ=   -6643.64991424677     
 iteration         -827 MCMCOBJ=   -6644.29436402027     
 iteration         -826 MCMCOBJ=   -6612.95242621007     
 iteration         -825 MCMCOBJ=   -6603.45154172389     
 iteration         -824 MCMCOBJ=   -6625.35523156899     
 iteration         -823 MCMCOBJ=   -6661.85383346283     
 iteration         -822 MCMCOBJ=   -6569.51577741132     
 iteration         -821 MCMCOBJ=   -6531.40183983214     
 iteration         -820 MCMCOBJ=   -6586.90972231500     
 iteration         -819 MCMCOBJ=   -6582.65250744821     
 iteration         -818 MCMCOBJ=   -6602.14130259759     
 iteration         -817 MCMCOBJ=   -6620.19221096357     
 iteration         -816 MCMCOBJ=   -6671.94831027584     
 iteration         -815 MCMCOBJ=   -6632.82502342074     
 iteration         -814 MCMCOBJ=   -6620.15427768794     
 iteration         -813 MCMCOBJ=   -6613.98622784977     
 iteration         -812 MCMCOBJ=   -6581.63699712027     
 iteration         -811 MCMCOBJ=   -6578.82147150787     
 iteration         -810 MCMCOBJ=   -6582.27817553456     
 iteration         -809 MCMCOBJ=   -6612.21085112710     
 iteration         -808 MCMCOBJ=   -6633.66766034917     
 iteration         -807 MCMCOBJ=   -6612.49494626547     
 iteration         -806 MCMCOBJ=   -6628.26744547475     
 iteration         -805 MCMCOBJ=   -6587.48972713839     
 iteration         -804 MCMCOBJ=   -6601.56277916984     
 iteration         -803 MCMCOBJ=   -6577.93670333237     
 iteration         -802 MCMCOBJ=   -6607.56019952977     
 iteration         -801 MCMCOBJ=   -6543.95900511608     
 iteration         -800 MCMCOBJ=   -6508.74759036559     
 iteration         -799 MCMCOBJ=   -6541.56354625574     
 iteration         -798 MCMCOBJ=   -6586.88742119815     
 iteration         -797 MCMCOBJ=   -6584.79566237303     
 iteration         -796 MCMCOBJ=   -6593.97228058865     
 iteration         -795 MCMCOBJ=   -6604.69869511005     
 iteration         -794 MCMCOBJ=   -6559.39009876104     
 iteration         -793 MCMCOBJ=   -6562.34546012670     
 iteration         -792 MCMCOBJ=   -6580.52216810506     
 iteration         -791 MCMCOBJ=   -6616.88665586702     
 iteration         -790 MCMCOBJ=   -6616.88665276979     
 iteration         -789 MCMCOBJ=   -6602.15393651631     
 iteration         -788 MCMCOBJ=   -6597.22629862337     
 iteration         -787 MCMCOBJ=   -6572.02142910336     
 iteration         -786 MCMCOBJ=   -6579.49431156898     
 iteration         -785 MCMCOBJ=   -6529.86795859166     
 iteration         -784 MCMCOBJ=   -6569.89517480468     
 iteration         -783 MCMCOBJ=   -6613.26651624020     
 iteration         -782 MCMCOBJ=   -6613.26651709636     
 iteration         -781 MCMCOBJ=   -6594.77218638891     
 iteration         -780 MCMCOBJ=   -6608.17764359072     
 iteration         -779 MCMCOBJ=   -6662.19592922837     
 iteration         -778 MCMCOBJ=   -6625.40329213061     
 iteration         -777 MCMCOBJ=   -6644.40388868207     
 iteration         -776 MCMCOBJ=   -6650.00738103190     
 iteration         -775 MCMCOBJ=   -6628.84135187521     
 iteration         -774 MCMCOBJ=   -6648.69242613985     
 iteration         -773 MCMCOBJ=   -6603.98624075216     
 iteration         -772 MCMCOBJ=   -6592.91637198876     
 iteration         -771 MCMCOBJ=   -6605.46652667737     
 iteration         -770 MCMCOBJ=   -6601.67341480219     
 iteration         -769 MCMCOBJ=   -6619.00640656756     
 iteration         -768 MCMCOBJ=   -6639.54186251509     
 iteration         -767 MCMCOBJ=   -6641.30224442677     
 iteration         -766 MCMCOBJ=   -6662.32328605378     
 iteration         -765 MCMCOBJ=   -6652.97349544609     
 iteration         -764 MCMCOBJ=   -6550.58416232753     
 iteration         -763 MCMCOBJ=   -6588.70569595157     
 iteration         -762 MCMCOBJ=   -6585.79797203001     
 iteration         -761 MCMCOBJ=   -6619.18693547781     
 iteration         -760 MCMCOBJ=   -6617.61994697522     
 iteration         -759 MCMCOBJ=   -6640.88579842343     
 iteration         -758 MCMCOBJ=   -6666.08959272214     
 iteration         -757 MCMCOBJ=   -6634.07442201867     
 iteration         -756 MCMCOBJ=   -6644.80306986348     
 iteration         -755 MCMCOBJ=   -6607.51799401186     
 iteration         -754 MCMCOBJ=   -6619.57989709280     
 iteration         -753 MCMCOBJ=   -6633.85127146696     
 iteration         -752 MCMCOBJ=   -6609.23858807824     
 iteration         -751 MCMCOBJ=   -6644.36479522383     
 iteration         -750 MCMCOBJ=   -6644.36479019739     
 iteration         -749 MCMCOBJ=   -6644.36479021796     
 iteration         -748 MCMCOBJ=   -6624.78231468079     
 iteration         -747 MCMCOBJ=   -6597.57098840773     
 iteration         -746 MCMCOBJ=   -6619.55484794902     
 iteration         -745 MCMCOBJ=   -6587.18682174226     
 iteration         -744 MCMCOBJ=   -6605.55045042588     
 iteration         -743 MCMCOBJ=   -6577.32537443641     
 iteration         -742 MCMCOBJ=   -6573.49035644323     
 iteration         -741 MCMCOBJ=   -6595.43772390010     
 iteration         -740 MCMCOBJ=   -6601.46992687965     
 iteration         -739 MCMCOBJ=   -6606.50781861659     
 iteration         -738 MCMCOBJ=   -6636.24525470209     
 iteration         -737 MCMCOBJ=   -6642.35642319402     
 iteration         -736 MCMCOBJ=   -6615.68294990488     
 iteration         -735 MCMCOBJ=   -6625.87412917849     
 iteration         -734 MCMCOBJ=   -6653.50883925557     
 iteration         -733 MCMCOBJ=   -6601.56616281505     
 iteration         -732 MCMCOBJ=   -6588.09769014867     
 iteration         -731 MCMCOBJ=   -6610.33540546745     
 iteration         -730 MCMCOBJ=   -6624.82519312503     
 iteration         -729 MCMCOBJ=   -6645.69135802295     
 iteration         -728 MCMCOBJ=   -6601.79717682385     
 iteration         -727 MCMCOBJ=   -6626.81504433457     
 iteration         -726 MCMCOBJ=   -6619.91764420342     
 iteration         -725 MCMCOBJ=   -6616.40827691512     
 iteration         -724 MCMCOBJ=   -6616.76935499808     
 iteration         -723 MCMCOBJ=   -6625.54499202958     
 iteration         -722 MCMCOBJ=   -6627.22672002826     
 iteration         -721 MCMCOBJ=   -6621.09122309058     
 iteration         -720 MCMCOBJ=   -6621.09122471944     
 iteration         -719 MCMCOBJ=   -6539.29959365377     
 iteration         -718 MCMCOBJ=   -6563.41608445251     
 iteration         -717 MCMCOBJ=   -6596.20483727826     
 iteration         -716 MCMCOBJ=   -6594.72321507048     
 iteration         -715 MCMCOBJ=   -6576.84557378694     
 iteration         -714 MCMCOBJ=   -6523.84444238077     
 iteration         -713 MCMCOBJ=   -6562.68154919747     
 iteration         -712 MCMCOBJ=   -6537.34640253582     
 iteration         -711 MCMCOBJ=   -6549.64343051178     
 iteration         -710 MCMCOBJ=   -6583.92865616594     
 iteration         -709 MCMCOBJ=   -6538.74255343495     
 iteration         -708 MCMCOBJ=   -6517.76027645488     
 iteration         -707 MCMCOBJ=   -6479.40862389222     
 iteration         -706 MCMCOBJ=   -6554.09618662468     
 iteration         -705 MCMCOBJ=   -6556.66982718054     
 iteration         -704 MCMCOBJ=   -6572.98004789979     
 iteration         -703 MCMCOBJ=   -6588.80721931674     
 iteration         -702 MCMCOBJ=   -6583.23333682172     
 iteration         -701 MCMCOBJ=   -6551.03287441323     
 iteration         -700 MCMCOBJ=   -6567.32698441648     
 iteration         -699 MCMCOBJ=   -6586.24565553599     
 iteration         -698 MCMCOBJ=   -6616.72168384182     
 iteration         -697 MCMCOBJ=   -6599.67540995937     
 iteration         -696 MCMCOBJ=   -6548.55138227351     
 iteration         -695 MCMCOBJ=   -6544.99510052324     
 iteration         -694 MCMCOBJ=   -6535.88657161123     
 iteration         -693 MCMCOBJ=   -6559.69356303179     
 iteration         -692 MCMCOBJ=   -6612.87927554259     
 iteration         -691 MCMCOBJ=   -6642.18894987984     
 iteration         -690 MCMCOBJ=   -6626.62062566799     
 iteration         -689 MCMCOBJ=   -6626.62062519069     
 iteration         -688 MCMCOBJ=   -6624.09434522284     
 iteration         -687 MCMCOBJ=   -6609.68868094198     
 iteration         -686 MCMCOBJ=   -6575.77512115538     
 iteration         -685 MCMCOBJ=   -6603.38073810677     
 iteration         -684 MCMCOBJ=   -6611.13226016430     
 iteration         -683 MCMCOBJ=   -6607.18765067019     
 iteration         -682 MCMCOBJ=   -6644.41649141766     
 iteration         -681 MCMCOBJ=   -6579.52691759908     
 iteration         -680 MCMCOBJ=   -6583.79424459123     
 iteration         -679 MCMCOBJ=   -6611.90387749010     
 iteration         -678 MCMCOBJ=   -6651.53491077594     
 iteration         -677 MCMCOBJ=   -6647.71245895845     
 iteration         -676 MCMCOBJ=   -6637.08760447374     
 iteration         -675 MCMCOBJ=   -6616.57002358595     
 iteration         -674 MCMCOBJ=   -6558.04506626161     
 iteration         -673 MCMCOBJ=   -6595.53717869072     
 iteration         -672 MCMCOBJ=   -6598.29026985248     
 iteration         -671 MCMCOBJ=   -6598.29030753521     
 iteration         -670 MCMCOBJ=   -6599.39476036227     
 iteration         -669 MCMCOBJ=   -6577.58611346825     
 iteration         -668 MCMCOBJ=   -6569.58364159323     
 iteration         -667 MCMCOBJ=   -6559.34734539733     
 iteration         -666 MCMCOBJ=   -6574.48467889365     
 iteration         -665 MCMCOBJ=   -6579.19295579651     
 iteration         -664 MCMCOBJ=   -6594.42037985860     
 iteration         -663 MCMCOBJ=   -6599.37341956971     
 iteration         -662 MCMCOBJ=   -6584.13169778437     
 iteration         -661 MCMCOBJ=   -6584.13170739019     
 iteration         -660 MCMCOBJ=   -6628.54990492333     
 iteration         -659 MCMCOBJ=   -6637.05664222944     
 iteration         -658 MCMCOBJ=   -6610.93569837242     
 iteration         -657 MCMCOBJ=   -6590.51317882224     
 iteration         -656 MCMCOBJ=   -6576.05278210260     
 iteration         -655 MCMCOBJ=   -6585.05380445750     
 iteration         -654 MCMCOBJ=   -6658.96798062182     
 iteration         -653 MCMCOBJ=   -6646.34665526332     
 iteration         -652 MCMCOBJ=   -6614.72511604500     
 iteration         -651 MCMCOBJ=   -6647.52653179234     
 iteration         -650 MCMCOBJ=   -6656.57844926060     
 iteration         -649 MCMCOBJ=   -6638.48889350692     
 iteration         -648 MCMCOBJ=   -6518.14010211421     
 iteration         -647 MCMCOBJ=   -6519.04115458965     
 iteration         -646 MCMCOBJ=   -6601.72823835489     
 iteration         -645 MCMCOBJ=   -6601.72823812061     
 iteration         -644 MCMCOBJ=   -6585.17947340114     
 iteration         -643 MCMCOBJ=   -6500.94487926495     
 iteration         -642 MCMCOBJ=   -6462.73184102673     
 iteration         -641 MCMCOBJ=   -6544.80657461001     
 iteration         -640 MCMCOBJ=   -6564.25386429433     
 iteration         -639 MCMCOBJ=   -6614.07382654817     
 iteration         -638 MCMCOBJ=   -6579.88522451528     
 iteration         -637 MCMCOBJ=   -6673.77599713379     
 iteration         -636 MCMCOBJ=   -6572.66242940579     
 iteration         -635 MCMCOBJ=   -6564.92168115158     
 iteration         -634 MCMCOBJ=   -6597.37991883804     
 iteration         -633 MCMCOBJ=   -6606.84154531841     
 iteration         -632 MCMCOBJ=   -6632.68690151530     
 iteration         -631 MCMCOBJ=   -6649.56985167001     
 iteration         -630 MCMCOBJ=   -6649.35557734755     
 iteration         -629 MCMCOBJ=   -6608.07108714614     
 iteration         -628 MCMCOBJ=   -6664.68412626047     
 iteration         -627 MCMCOBJ=   -6585.68991003746     
 iteration         -626 MCMCOBJ=   -6536.92862967416     
 iteration         -625 MCMCOBJ=   -6620.95222626482     
 iteration         -624 MCMCOBJ=   -6620.91331736117     
 iteration         -623 MCMCOBJ=   -6552.51450610839     
 iteration         -622 MCMCOBJ=   -6572.40659568472     
 iteration         -621 MCMCOBJ=   -6565.51363460974     
 iteration         -620 MCMCOBJ=   -6603.13341895551     
 iteration         -619 MCMCOBJ=   -6549.24608798482     
 iteration         -618 MCMCOBJ=   -6591.25495651962     
 iteration         -617 MCMCOBJ=   -6583.87025847880     
 iteration         -616 MCMCOBJ=   -6588.72117522282     
 iteration         -615 MCMCOBJ=   -6595.81012019523     
 iteration         -614 MCMCOBJ=   -6594.09432259775     
 iteration         -613 MCMCOBJ=   -6588.18802682316     
 iteration         -612 MCMCOBJ=   -6591.62765715789     
 iteration         -611 MCMCOBJ=   -6599.29320786911     
 iteration         -610 MCMCOBJ=   -6619.36041992971     
 iteration         -609 MCMCOBJ=   -6590.22314896045     
 iteration         -608 MCMCOBJ=   -6580.74122822792     
 iteration         -607 MCMCOBJ=   -6604.68447209263     
 iteration         -606 MCMCOBJ=   -6615.46792954683     
 iteration         -605 MCMCOBJ=   -6616.48895454270     
 iteration         -604 MCMCOBJ=   -6621.73621376625     
 iteration         -603 MCMCOBJ=   -6626.91890058493     
 iteration         -602 MCMCOBJ=   -6552.01291281304     
 iteration         -601 MCMCOBJ=   -6545.50533899671     
 iteration         -600 MCMCOBJ=   -6518.62222781147     
 iteration         -599 MCMCOBJ=   -6561.48422689441     
 iteration         -598 MCMCOBJ=   -6548.77333634794     
 iteration         -597 MCMCOBJ=   -6595.24910178936     
 iteration         -596 MCMCOBJ=   -6606.62020811434     
 iteration         -595 MCMCOBJ=   -6625.61971064944     
 iteration         -594 MCMCOBJ=   -6596.01976180083     
 iteration         -593 MCMCOBJ=   -6610.07369369605     
 iteration         -592 MCMCOBJ=   -6634.09132170570     
 iteration         -591 MCMCOBJ=   -6585.77975972550     
 iteration         -590 MCMCOBJ=   -6602.87267825547     
 iteration         -589 MCMCOBJ=   -6598.47723419654     
 iteration         -588 MCMCOBJ=   -6651.70624064300     
 iteration         -587 MCMCOBJ=   -6676.84657365709     
 iteration         -586 MCMCOBJ=   -6679.75899300858     
 iteration         -585 MCMCOBJ=   -6678.72764844450     
 iteration         -584 MCMCOBJ=   -6670.67504050106     
 iteration         -583 MCMCOBJ=   -6638.79962521161     
 iteration         -582 MCMCOBJ=   -6582.65886905771     
 iteration         -581 MCMCOBJ=   -6574.28930022064     
 iteration         -580 MCMCOBJ=   -6550.19847052659     
 iteration         -579 MCMCOBJ=   -6596.20033374762     
 iteration         -578 MCMCOBJ=   -6614.12315437425     
 iteration         -577 MCMCOBJ=   -6581.08941208096     
 iteration         -576 MCMCOBJ=   -6577.55697687417     
 iteration         -575 MCMCOBJ=   -6587.19063176577     
 iteration         -574 MCMCOBJ=   -6636.05559376451     
 iteration         -573 MCMCOBJ=   -6667.85128600985     
 iteration         -572 MCMCOBJ=   -6647.37268296030     
 iteration         -571 MCMCOBJ=   -6644.75816553248     
 iteration         -570 MCMCOBJ=   -6616.31141936068     
 iteration         -569 MCMCOBJ=   -6599.86051424986     
 iteration         -568 MCMCOBJ=   -6597.91809014978     
 iteration         -567 MCMCOBJ=   -6613.69119981516     
 iteration         -566 MCMCOBJ=   -6638.05568940350     
 iteration         -565 MCMCOBJ=   -6646.13482375857     
 iteration         -564 MCMCOBJ=   -6647.43396265992     
 iteration         -563 MCMCOBJ=   -6645.47367556974     
 iteration         -562 MCMCOBJ=   -6638.54949399507     
 iteration         -561 MCMCOBJ=   -6603.46015524583     
 iteration         -560 MCMCOBJ=   -6587.88358226248     
 iteration         -559 MCMCOBJ=   -6618.84131682924     
 iteration         -558 MCMCOBJ=   -6611.15030847367     
 iteration         -557 MCMCOBJ=   -6627.00860965709     
 iteration         -556 MCMCOBJ=   -6562.47427448294     
 iteration         -555 MCMCOBJ=   -6568.82095087081     
 iteration         -554 MCMCOBJ=   -6620.23018930168     
 iteration         -553 MCMCOBJ=   -6691.88850757737     
 iteration         -552 MCMCOBJ=   -6688.77012313043     
 iteration         -551 MCMCOBJ=   -6656.62224947284     
 iteration         -550 MCMCOBJ=   -6651.33179715940     
 iteration         -549 MCMCOBJ=   -6651.33179946488     
 iteration         -548 MCMCOBJ=   -6658.77928134305     
 iteration         -547 MCMCOBJ=   -6640.61188839560     
 iteration         -546 MCMCOBJ=   -6640.12309078987     
 iteration         -545 MCMCOBJ=   -6562.60472280253     
 iteration         -544 MCMCOBJ=   -6557.50563557924     
 iteration         -543 MCMCOBJ=   -6619.99054677133     
 iteration         -542 MCMCOBJ=   -6619.99054818899     
 iteration         -541 MCMCOBJ=   -6570.95377207228     
 iteration         -540 MCMCOBJ=   -6585.33015690939     
 iteration         -539 MCMCOBJ=   -6522.44643738126     
 iteration         -538 MCMCOBJ=   -6576.86305933756     
 iteration         -537 MCMCOBJ=   -6559.34515938493     
 iteration         -536 MCMCOBJ=   -6558.03176167381     
 iteration         -535 MCMCOBJ=   -6567.82841126148     
 iteration         -534 MCMCOBJ=   -6573.53729120718     
 iteration         -533 MCMCOBJ=   -6563.68037615007     
 iteration         -532 MCMCOBJ=   -6524.78787962530     
 iteration         -531 MCMCOBJ=   -6578.74224479837     
 iteration         -530 MCMCOBJ=   -6567.93320704639     
 iteration         -529 MCMCOBJ=   -6552.86194751629     
 iteration         -528 MCMCOBJ=   -6542.05659439591     
 iteration         -527 MCMCOBJ=   -6578.46200352292     
 iteration         -526 MCMCOBJ=   -6578.46201279057     
 iteration         -525 MCMCOBJ=   -6588.22488980944     
 iteration         -524 MCMCOBJ=   -6615.06861856832     
 iteration         -523 MCMCOBJ=   -6611.25975269459     
 iteration         -522 MCMCOBJ=   -6578.91306784974     
 iteration         -521 MCMCOBJ=   -6550.07323852799     
 iteration         -520 MCMCOBJ=   -6553.78535533290     
 iteration         -519 MCMCOBJ=   -6597.93119756482     
 iteration         -518 MCMCOBJ=   -6616.35274212388     
 iteration         -517 MCMCOBJ=   -6598.85046539414     
 iteration         -516 MCMCOBJ=   -6592.61736790557     
 iteration         -515 MCMCOBJ=   -6575.73451379346     
 iteration         -514 MCMCOBJ=   -6589.67026674319     
 iteration         -513 MCMCOBJ=   -6557.80557362029     
 iteration         -512 MCMCOBJ=   -6582.16289330678     
 iteration         -511 MCMCOBJ=   -6584.13674516985     
 iteration         -510 MCMCOBJ=   -6595.19602842308     
 iteration         -509 MCMCOBJ=   -6574.17177572659     
 iteration         -508 MCMCOBJ=   -6613.71215620111     
 iteration         -507 MCMCOBJ=   -6600.14250979052     
 iteration         -506 MCMCOBJ=   -6623.02455117302     
 iteration         -505 MCMCOBJ=   -6566.32251172137     
 iteration         -504 MCMCOBJ=   -6575.12108353939     
 iteration         -503 MCMCOBJ=   -6558.43918700650     
 iteration         -502 MCMCOBJ=   -6544.09679713546     
 iteration         -501 MCMCOBJ=   -6600.06566426153     
 iteration         -500 MCMCOBJ=   -6569.55866341387     
 iteration         -499 MCMCOBJ=   -6601.21282934514     
 iteration         -498 MCMCOBJ=   -6577.19701726792     
 iteration         -497 MCMCOBJ=   -6592.98058729981     
 iteration         -496 MCMCOBJ=   -6602.61710401287     
 iteration         -495 MCMCOBJ=   -6595.06004397627     
 iteration         -494 MCMCOBJ=   -6556.67456379464     
 iteration         -493 MCMCOBJ=   -6555.59393302413     
 iteration         -492 MCMCOBJ=   -6513.02969719964     
 iteration         -491 MCMCOBJ=   -6583.62697355439     
 iteration         -490 MCMCOBJ=   -6605.59906271477     
 iteration         -489 MCMCOBJ=   -6607.52374143237     
 iteration         -488 MCMCOBJ=   -6588.91548129101     
 iteration         -487 MCMCOBJ=   -6614.10288934465     
 iteration         -486 MCMCOBJ=   -6626.96633837981     
 iteration         -485 MCMCOBJ=   -6626.96633791435     
 iteration         -484 MCMCOBJ=   -6624.82692027631     
 iteration         -483 MCMCOBJ=   -6606.14887179578     
 iteration         -482 MCMCOBJ=   -6587.27275583168     
 iteration         -481 MCMCOBJ=   -6619.01110568859     
 iteration         -480 MCMCOBJ=   -6528.75661250053     
 iteration         -479 MCMCOBJ=   -6619.02765634108     
 iteration         -478 MCMCOBJ=   -6581.19731717700     
 iteration         -477 MCMCOBJ=   -6599.03008095424     
 iteration         -476 MCMCOBJ=   -6586.02483324952     
 iteration         -475 MCMCOBJ=   -6589.78519238894     
 iteration         -474 MCMCOBJ=   -6582.50216639921     
 iteration         -473 MCMCOBJ=   -6554.48990361735     
 iteration         -472 MCMCOBJ=   -6536.20536238963     
 iteration         -471 MCMCOBJ=   -6579.83444411469     
 iteration         -470 MCMCOBJ=   -6588.10141766260     
 iteration         -469 MCMCOBJ=   -6560.57430060964     
 iteration         -468 MCMCOBJ=   -6545.00587823651     
 iteration         -467 MCMCOBJ=   -6569.44255137452     
 iteration         -466 MCMCOBJ=   -6561.76779806692     
 iteration         -465 MCMCOBJ=   -6596.77317214777     
 iteration         -464 MCMCOBJ=   -6640.89857449174     
 iteration         -463 MCMCOBJ=   -6628.75430611284     
 iteration         -462 MCMCOBJ=   -6634.21367617183     
 iteration         -461 MCMCOBJ=   -6581.26848848129     
 iteration         -460 MCMCOBJ=   -6643.74408088699     
 iteration         -459 MCMCOBJ=   -6610.61919319121     
 iteration         -458 MCMCOBJ=   -6614.10598326301     
 iteration         -457 MCMCOBJ=   -6609.12015383587     
 iteration         -456 MCMCOBJ=   -6637.86087571766     
 iteration         -455 MCMCOBJ=   -6648.67821804428     
 iteration         -454 MCMCOBJ=   -6667.97840770063     
 iteration         -453 MCMCOBJ=   -6664.32824270277     
 iteration         -452 MCMCOBJ=   -6678.83894171299     
 iteration         -451 MCMCOBJ=   -6631.80730882428     
 iteration         -450 MCMCOBJ=   -6622.96608825941     
 iteration         -449 MCMCOBJ=   -6577.82407664910     
 iteration         -448 MCMCOBJ=   -6576.61721327770     
 iteration         -447 MCMCOBJ=   -6609.02731450690     
 iteration         -446 MCMCOBJ=   -6599.17076572577     
 iteration         -445 MCMCOBJ=   -6633.63665572441     
 iteration         -444 MCMCOBJ=   -6644.05448544963     
 iteration         -443 MCMCOBJ=   -6612.93217866138     
 iteration         -442 MCMCOBJ=   -6621.85142848450     
 iteration         -441 MCMCOBJ=   -6612.88876448891     
 iteration         -440 MCMCOBJ=   -6633.43134489319     
 iteration         -439 MCMCOBJ=   -6636.11499637288     
 iteration         -438 MCMCOBJ=   -6612.82723640506     
 iteration         -437 MCMCOBJ=   -6575.85421505401     
 iteration         -436 MCMCOBJ=   -6585.57475366808     
 iteration         -435 MCMCOBJ=   -6644.67996420671     
 iteration         -434 MCMCOBJ=   -6649.78375922484     
 iteration         -433 MCMCOBJ=   -6649.78375939053     
 iteration         -432 MCMCOBJ=   -6611.95532882499     
 iteration         -431 MCMCOBJ=   -6618.56712895865     
 iteration         -430 MCMCOBJ=   -6641.91865097205     
 iteration         -429 MCMCOBJ=   -6625.62297628830     
 iteration         -428 MCMCOBJ=   -6602.55667488910     
 iteration         -427 MCMCOBJ=   -6624.79433301258     
 iteration         -426 MCMCOBJ=   -6593.57859616694     
 iteration         -425 MCMCOBJ=   -6640.88173756387     
 iteration         -424 MCMCOBJ=   -6626.85642256862     
 iteration         -423 MCMCOBJ=   -6615.11044838670     
 iteration         -422 MCMCOBJ=   -6577.90556243407     
 iteration         -421 MCMCOBJ=   -6571.23207953835     
 iteration         -420 MCMCOBJ=   -6536.18243902560     
 iteration         -419 MCMCOBJ=   -6581.71008817789     
 iteration         -418 MCMCOBJ=   -6596.97573701952     
 iteration         -417 MCMCOBJ=   -6587.36488290671     
 iteration         -416 MCMCOBJ=   -6535.72953057055     
 iteration         -415 MCMCOBJ=   -6585.77872716001     
 iteration         -414 MCMCOBJ=   -6588.32551711902     
 iteration         -413 MCMCOBJ=   -6598.47803832823     
 iteration         -412 MCMCOBJ=   -6576.94283525491     
 iteration         -411 MCMCOBJ=   -6598.75154925360     
 iteration         -410 MCMCOBJ=   -6598.75154861152     
 iteration         -409 MCMCOBJ=   -6612.57179129355     
 iteration         -408 MCMCOBJ=   -6598.90970466511     
 iteration         -407 MCMCOBJ=   -6607.99168456325     
 iteration         -406 MCMCOBJ=   -6603.91828905289     
 iteration         -405 MCMCOBJ=   -6584.01986708619     
 iteration         -404 MCMCOBJ=   -6580.70943582918     
 iteration         -403 MCMCOBJ=   -6603.68660716469     
 iteration         -402 MCMCOBJ=   -6645.72614382801     
 iteration         -401 MCMCOBJ=   -6643.52222978426     
 iteration         -400 MCMCOBJ=   -6593.67737336229     
 iteration         -399 MCMCOBJ=   -6550.34094750047     
 iteration         -398 MCMCOBJ=   -6608.07557226369     
 iteration         -397 MCMCOBJ=   -6588.74704546401     
 iteration         -396 MCMCOBJ=   -6576.91323976119     
 iteration         -395 MCMCOBJ=   -6573.70227094959     
 iteration         -394 MCMCOBJ=   -6511.89386910766     
 iteration         -393 MCMCOBJ=   -6553.68982090760     
 iteration         -392 MCMCOBJ=   -6546.59815707153     
 iteration         -391 MCMCOBJ=   -6595.62605531011     
 iteration         -390 MCMCOBJ=   -6588.56896877343     
 iteration         -389 MCMCOBJ=   -6613.94661602955     
 iteration         -388 MCMCOBJ=   -6636.33988368747     
 iteration         -387 MCMCOBJ=   -6648.41673645346     
 iteration         -386 MCMCOBJ=   -6646.36707516360     
 iteration         -385 MCMCOBJ=   -6593.65496221306     
 iteration         -384 MCMCOBJ=   -6635.52592611464     
 iteration         -383 MCMCOBJ=   -6599.91197327314     
 iteration         -382 MCMCOBJ=   -6579.37289237162     
 iteration         -381 MCMCOBJ=   -6579.37288798331     
 iteration         -380 MCMCOBJ=   -6589.77692926964     
 iteration         -379 MCMCOBJ=   -6585.67388468563     
 iteration         -378 MCMCOBJ=   -6581.74075084632     
 iteration         -377 MCMCOBJ=   -6568.40832105461     
 iteration         -376 MCMCOBJ=   -6509.85381471601     
 iteration         -375 MCMCOBJ=   -6541.65416710328     
 iteration         -374 MCMCOBJ=   -6525.69287407754     
 iteration         -373 MCMCOBJ=   -6534.88680766973     
 iteration         -372 MCMCOBJ=   -6529.61233890828     
 iteration         -371 MCMCOBJ=   -6486.10116874903     
 iteration         -370 MCMCOBJ=   -6510.33757602407     
 iteration         -369 MCMCOBJ=   -6526.41547433012     
 iteration         -368 MCMCOBJ=   -6580.66677741553     
 iteration         -367 MCMCOBJ=   -6586.51233580083     
 iteration         -366 MCMCOBJ=   -6567.19373614748     
 iteration         -365 MCMCOBJ=   -6579.01142834856     
 iteration         -364 MCMCOBJ=   -6543.87260985785     
 iteration         -363 MCMCOBJ=   -6541.64837776705     
 iteration         -362 MCMCOBJ=   -6543.86600894358     
 iteration         -361 MCMCOBJ=   -6591.92014482691     
 iteration         -360 MCMCOBJ=   -6570.69572923176     
 iteration         -359 MCMCOBJ=   -6597.07301617629     
 iteration         -358 MCMCOBJ=   -6612.85766583594     
 iteration         -357 MCMCOBJ=   -6588.45606942848     
 iteration         -356 MCMCOBJ=   -6590.23629548738     
 iteration         -355 MCMCOBJ=   -6580.47692131756     
 iteration         -354 MCMCOBJ=   -6579.11452914103     
 iteration         -353 MCMCOBJ=   -6600.22366697925     
 iteration         -352 MCMCOBJ=   -6583.46121694293     
 iteration         -351 MCMCOBJ=   -6549.78082706300     
 iteration         -350 MCMCOBJ=   -6597.26989583054     
 iteration         -349 MCMCOBJ=   -6598.59224423918     
 iteration         -348 MCMCOBJ=   -6605.90577281326     
 iteration         -347 MCMCOBJ=   -6589.13714761161     
 iteration         -346 MCMCOBJ=   -6611.93658758147     
 iteration         -345 MCMCOBJ=   -6650.83127663090     
 iteration         -344 MCMCOBJ=   -6653.76025678555     
 iteration         -343 MCMCOBJ=   -6651.05930048814     
 iteration         -342 MCMCOBJ=   -6623.37061398120     
 iteration         -341 MCMCOBJ=   -6639.15598830128     
 iteration         -340 MCMCOBJ=   -6640.28254393553     
 iteration         -339 MCMCOBJ=   -6640.28253229065     
 iteration         -338 MCMCOBJ=   -6617.81010037855     
 iteration         -337 MCMCOBJ=   -6572.21927275767     
 iteration         -336 MCMCOBJ=   -6628.40745924814     
 iteration         -335 MCMCOBJ=   -6601.11900114868     
 iteration         -334 MCMCOBJ=   -6582.43241308086     
 iteration         -333 MCMCOBJ=   -6579.69857145850     
 iteration         -332 MCMCOBJ=   -6595.59879132596     
 iteration         -331 MCMCOBJ=   -6614.16847638771     
 iteration         -330 MCMCOBJ=   -6597.15592805053     
 iteration         -329 MCMCOBJ=   -6666.45251792970     
 iteration         -328 MCMCOBJ=   -6655.45543587119     
 iteration         -327 MCMCOBJ=   -6597.68778350969     
 iteration         -326 MCMCOBJ=   -6563.12214804317     
 iteration         -325 MCMCOBJ=   -6615.86066637213     
 iteration         -324 MCMCOBJ=   -6696.30663297781     
 iteration         -323 MCMCOBJ=   -6678.14749504173     
 iteration         -322 MCMCOBJ=   -6628.95257415112     
 iteration         -321 MCMCOBJ=   -6594.31063245057     
 iteration         -320 MCMCOBJ=   -6605.24146135982     
 iteration         -319 MCMCOBJ=   -6585.69674578539     
 iteration         -318 MCMCOBJ=   -6566.74872291190     
 iteration         -317 MCMCOBJ=   -6614.86120369325     
 iteration         -316 MCMCOBJ=   -6655.60298598405     
 iteration         -315 MCMCOBJ=   -6648.79523508858     
 iteration         -314 MCMCOBJ=   -6621.82850470930     
 iteration         -313 MCMCOBJ=   -6609.50591246996     
 iteration         -312 MCMCOBJ=   -6592.11554498537     
 iteration         -311 MCMCOBJ=   -6602.18178700385     
 iteration         -310 MCMCOBJ=   -6584.24862443517     
 iteration         -309 MCMCOBJ=   -6588.02875857295     
 iteration         -308 MCMCOBJ=   -6555.98859343687     
 iteration         -307 MCMCOBJ=   -6597.36192449913     
 iteration         -306 MCMCOBJ=   -6594.99045075309     
 iteration         -305 MCMCOBJ=   -6607.54314547490     
 iteration         -304 MCMCOBJ=   -6654.43954665571     
 iteration         -303 MCMCOBJ=   -6654.43954620677     
 iteration         -302 MCMCOBJ=   -6632.78909978614     
 iteration         -301 MCMCOBJ=   -6596.34586548062     
 iteration         -300 MCMCOBJ=   -6613.21098920295     
 iteration         -299 MCMCOBJ=   -6607.83036077696     
 iteration         -298 MCMCOBJ=   -6634.29867501502     
 iteration         -297 MCMCOBJ=   -6620.05741094145     
 iteration         -296 MCMCOBJ=   -6618.07029069248     
 iteration         -295 MCMCOBJ=   -6618.82917414333     
 iteration         -294 MCMCOBJ=   -6590.30049764585     
 iteration         -293 MCMCOBJ=   -6559.44603098938     
 iteration         -292 MCMCOBJ=   -6577.59939525659     
 iteration         -291 MCMCOBJ=   -6586.15704080912     
 iteration         -290 MCMCOBJ=   -6576.09431643892     
 iteration         -289 MCMCOBJ=   -6559.35573451862     
 iteration         -288 MCMCOBJ=   -6553.20408882387     
 iteration         -287 MCMCOBJ=   -6561.90317349346     
 iteration         -286 MCMCOBJ=   -6619.58007738721     
 iteration         -285 MCMCOBJ=   -6631.96460248371     
 iteration         -284 MCMCOBJ=   -6620.64973462868     
 iteration         -283 MCMCOBJ=   -6596.64193332000     
 iteration         -282 MCMCOBJ=   -6629.77920390341     
 iteration         -281 MCMCOBJ=   -6595.24960926695     
 iteration         -280 MCMCOBJ=   -6571.03866179897     
 iteration         -279 MCMCOBJ=   -6619.65416104714     
 iteration         -278 MCMCOBJ=   -6595.26981993345     
 iteration         -277 MCMCOBJ=   -6605.65762662056     
 iteration         -276 MCMCOBJ=   -6565.61640458106     
 iteration         -275 MCMCOBJ=   -6612.41805211360     
 iteration         -274 MCMCOBJ=   -6582.73206749419     
 iteration         -273 MCMCOBJ=   -6609.96902691326     
 iteration         -272 MCMCOBJ=   -6542.33110793569     
 iteration         -271 MCMCOBJ=   -6537.25539023722     
 iteration         -270 MCMCOBJ=   -6563.34212449100     
 iteration         -269 MCMCOBJ=   -6576.10393736521     
 iteration         -268 MCMCOBJ=   -6623.44734401240     
 iteration         -267 MCMCOBJ=   -6552.40944410919     
 iteration         -266 MCMCOBJ=   -6624.00750139165     
 iteration         -265 MCMCOBJ=   -6591.46003821777     
 iteration         -264 MCMCOBJ=   -6548.58813135051     
 iteration         -263 MCMCOBJ=   -6522.31923943659     
 iteration         -262 MCMCOBJ=   -6572.24548213580     
 iteration         -261 MCMCOBJ=   -6589.00900957687     
 iteration         -260 MCMCOBJ=   -6586.82028157203     
 iteration         -259 MCMCOBJ=   -6587.98195748336     
 iteration         -258 MCMCOBJ=   -6649.42887491617     
 iteration         -257 MCMCOBJ=   -6606.61003158288     
 iteration         -256 MCMCOBJ=   -6566.08421588915     
 iteration         -255 MCMCOBJ=   -6583.14111716620     
 iteration         -254 MCMCOBJ=   -6584.68345085775     
 iteration         -253 MCMCOBJ=   -6615.35337173363     
 iteration         -252 MCMCOBJ=   -6613.11391648905     
 iteration         -251 MCMCOBJ=   -6603.48990707277     
 iteration         -250 MCMCOBJ=   -6612.27051195844     
 iteration         -249 MCMCOBJ=   -6574.82118430610     
 iteration         -248 MCMCOBJ=   -6580.55977987910     
 iteration         -247 MCMCOBJ=   -6575.00161340724     
 iteration         -246 MCMCOBJ=   -6555.95939495613     
 iteration         -245 MCMCOBJ=   -6596.45725042731     
 iteration         -244 MCMCOBJ=   -6588.35716413842     
 iteration         -243 MCMCOBJ=   -6623.80752513002     
 iteration         -242 MCMCOBJ=   -6635.41223121861     
 iteration         -241 MCMCOBJ=   -6630.53138690898     
 iteration         -240 MCMCOBJ=   -6642.89439099466     
 iteration         -239 MCMCOBJ=   -6633.85437147115     
 iteration         -238 MCMCOBJ=   -6618.20743932365     
 iteration         -237 MCMCOBJ=   -6666.26064515583     
 iteration         -236 MCMCOBJ=   -6628.34014186338     
 iteration         -235 MCMCOBJ=   -6606.89065958035     
 iteration         -234 MCMCOBJ=   -6563.44358927641     
 iteration         -233 MCMCOBJ=   -6560.24341535953     
 iteration         -232 MCMCOBJ=   -6557.92835522553     
 iteration         -231 MCMCOBJ=   -6512.08069317860     
 iteration         -230 MCMCOBJ=   -6592.82479400173     
 iteration         -229 MCMCOBJ=   -6560.45068345832     
 iteration         -228 MCMCOBJ=   -6537.47035423335     
 iteration         -227 MCMCOBJ=   -6584.33022939494     
 iteration         -226 MCMCOBJ=   -6600.61654355363     
 iteration         -225 MCMCOBJ=   -6611.03359994166     
 iteration         -224 MCMCOBJ=   -6596.59018059323     
 iteration         -223 MCMCOBJ=   -6591.77974077374     
 iteration         -222 MCMCOBJ=   -6623.70216676952     
 iteration         -221 MCMCOBJ=   -6625.61503969737     
 iteration         -220 MCMCOBJ=   -6577.64924394835     
 iteration         -219 MCMCOBJ=   -6578.71016136707     
 iteration         -218 MCMCOBJ=   -6578.90584021488     
 iteration         -217 MCMCOBJ=   -6622.30180346034     
 iteration         -216 MCMCOBJ=   -6547.62958890692     
 iteration         -215 MCMCOBJ=   -6573.17178793819     
 iteration         -214 MCMCOBJ=   -6593.62458840028     
 iteration         -213 MCMCOBJ=   -6543.43867657957     
 iteration         -212 MCMCOBJ=   -6581.32701066435     
 iteration         -211 MCMCOBJ=   -6598.70507505507     
 iteration         -210 MCMCOBJ=   -6647.52607698369     
 iteration         -209 MCMCOBJ=   -6672.13106413818     
 iteration         -208 MCMCOBJ=   -6684.14151099751     
 iteration         -207 MCMCOBJ=   -6634.03320505229     
 iteration         -206 MCMCOBJ=   -6648.27383439923     
 iteration         -205 MCMCOBJ=   -6633.32128123191     
 iteration         -204 MCMCOBJ=   -6627.86707682984     
 iteration         -203 MCMCOBJ=   -6623.99873094111     
 iteration         -202 MCMCOBJ=   -6599.71433448817     
 iteration         -201 MCMCOBJ=   -6634.79913967133     
 iteration         -200 MCMCOBJ=   -6548.22665177886     
 iteration         -199 MCMCOBJ=   -6531.61849270984     
 iteration         -198 MCMCOBJ=   -6582.44191016628     
 iteration         -197 MCMCOBJ=   -6569.64928521702     
 iteration         -196 MCMCOBJ=   -6571.97910487625     
 iteration         -195 MCMCOBJ=   -6588.23936988679     
 iteration         -194 MCMCOBJ=   -6545.33432751867     
 iteration         -193 MCMCOBJ=   -6534.63551022774     
 iteration         -192 MCMCOBJ=   -6524.48677597303     
 iteration         -191 MCMCOBJ=   -6588.30914706228     
 iteration         -190 MCMCOBJ=   -6609.10556187990     
 iteration         -189 MCMCOBJ=   -6577.23411915658     
 iteration         -188 MCMCOBJ=   -6609.13337189826     
 iteration         -187 MCMCOBJ=   -6574.44081553040     
 iteration         -186 MCMCOBJ=   -6543.13265497240     
 iteration         -185 MCMCOBJ=   -6590.09645431998     
 iteration         -184 MCMCOBJ=   -6604.45377887541     
 iteration         -183 MCMCOBJ=   -6591.95197146676     
 iteration         -182 MCMCOBJ=   -6624.24011840959     
 iteration         -181 MCMCOBJ=   -6541.20415299986     
 iteration         -180 MCMCOBJ=   -6530.23301514430     
 iteration         -179 MCMCOBJ=   -6536.51166370392     
 iteration         -178 MCMCOBJ=   -6618.53345131681     
 iteration         -177 MCMCOBJ=   -6618.53345093860     
 iteration         -176 MCMCOBJ=   -6598.35099741562     
 iteration         -175 MCMCOBJ=   -6590.32668251198     
 iteration         -174 MCMCOBJ=   -6590.32668115570     
 iteration         -173 MCMCOBJ=   -6510.02168205976     
 iteration         -172 MCMCOBJ=   -6525.50515208607     
 iteration         -171 MCMCOBJ=   -6475.93238134504     
 iteration         -170 MCMCOBJ=   -6509.96025014448     
 iteration         -169 MCMCOBJ=   -6483.43500761690     
 iteration         -168 MCMCOBJ=   -6520.34338258163     
 iteration         -167 MCMCOBJ=   -6525.02147283715     
 iteration         -166 MCMCOBJ=   -6626.12594094909     
 iteration         -165 MCMCOBJ=   -6617.41737901663     
 iteration         -164 MCMCOBJ=   -6649.39663333435     
 iteration         -163 MCMCOBJ=   -6672.99380068405     
 iteration         -162 MCMCOBJ=   -6642.53636152694     
 iteration         -161 MCMCOBJ=   -6585.19957109297     
 iteration         -160 MCMCOBJ=   -6585.19957164514     
 iteration         -159 MCMCOBJ=   -6614.84638042888     
 iteration         -158 MCMCOBJ=   -6583.61896037435     
 iteration         -157 MCMCOBJ=   -6586.87837199415     
 iteration         -156 MCMCOBJ=   -6629.68530129171     
 iteration         -155 MCMCOBJ=   -6653.71737923880     
 iteration         -154 MCMCOBJ=   -6643.82708319217     
 iteration         -153 MCMCOBJ=   -6625.79181444563     
 iteration         -152 MCMCOBJ=   -6589.45259884678     
 iteration         -151 MCMCOBJ=   -6604.45322106622     
 iteration         -150 MCMCOBJ=   -6596.98318717275     
 iteration         -149 MCMCOBJ=   -6596.98318586926     
 iteration         -148 MCMCOBJ=   -6613.32693894271     
 iteration         -147 MCMCOBJ=   -6628.47424976500     
 iteration         -146 MCMCOBJ=   -6632.14303681652     
 iteration         -145 MCMCOBJ=   -6635.33948264685     
 iteration         -144 MCMCOBJ=   -6610.02170007182     
 iteration         -143 MCMCOBJ=   -6591.86584620554     
 iteration         -142 MCMCOBJ=   -6531.93632259896     
 iteration         -141 MCMCOBJ=   -6555.65369324445     
 iteration         -140 MCMCOBJ=   -6569.53382814265     
 iteration         -139 MCMCOBJ=   -6617.27849074536     
 iteration         -138 MCMCOBJ=   -6636.90498661623     
 iteration         -137 MCMCOBJ=   -6663.01364216679     
 iteration         -136 MCMCOBJ=   -6573.38453178519     
 iteration         -135 MCMCOBJ=   -6578.80608230794     
 iteration         -134 MCMCOBJ=   -6490.72129932392     
 iteration         -133 MCMCOBJ=   -6507.80831586774     
 iteration         -132 MCMCOBJ=   -6512.96235324162     
 iteration         -131 MCMCOBJ=   -6569.06287896151     
 iteration         -130 MCMCOBJ=   -6583.53758533321     
 iteration         -129 MCMCOBJ=   -6601.82804488505     
 iteration         -128 MCMCOBJ=   -6623.95853639550     
 iteration         -127 MCMCOBJ=   -6600.45474684534     
 iteration         -126 MCMCOBJ=   -6609.31581442260     
 iteration         -125 MCMCOBJ=   -6603.92770785501     
 iteration         -124 MCMCOBJ=   -6613.04954589410     
 iteration         -123 MCMCOBJ=   -6670.74465970744     
 iteration         -122 MCMCOBJ=   -6598.17954824251     
 iteration         -121 MCMCOBJ=   -6572.01071780787     
 iteration         -120 MCMCOBJ=   -6592.61440105346     
 iteration         -119 MCMCOBJ=   -6605.04640773647     
 iteration         -118 MCMCOBJ=   -6619.05414475957     
 iteration         -117 MCMCOBJ=   -6631.17835054125     
 iteration         -116 MCMCOBJ=   -6673.88194000168     
 iteration         -115 MCMCOBJ=   -6609.53532282362     
 iteration         -114 MCMCOBJ=   -6602.82544711532     
 iteration         -113 MCMCOBJ=   -6649.10783946199     
 iteration         -112 MCMCOBJ=   -6667.16019785432     
 iteration         -111 MCMCOBJ=   -6649.91528388421     
 iteration         -110 MCMCOBJ=   -6611.86046083342     
 iteration         -109 MCMCOBJ=   -6613.02476257329     
 iteration         -108 MCMCOBJ=   -6630.53489652048     
 iteration         -107 MCMCOBJ=   -6619.03648273888     
 iteration         -106 MCMCOBJ=   -6636.42504298751     
 iteration         -105 MCMCOBJ=   -6635.72149226892     
 iteration         -104 MCMCOBJ=   -6619.49796802091     
 iteration         -103 MCMCOBJ=   -6573.35163179524     
 iteration         -102 MCMCOBJ=   -6550.94895557439     
 iteration         -101 MCMCOBJ=   -6626.45849466179     
 iteration         -100 MCMCOBJ=   -6630.32365955463     
 iteration          -99 MCMCOBJ=   -6638.20433267600     
 iteration          -98 MCMCOBJ=   -6604.54270244418     
 iteration          -97 MCMCOBJ=   -6600.66316335717     
 iteration          -96 MCMCOBJ=   -6600.66315278143     
 iteration          -95 MCMCOBJ=   -6564.17930971796     
 iteration          -94 MCMCOBJ=   -6518.29603574699     
 iteration          -93 MCMCOBJ=   -6567.27099474941     
 iteration          -92 MCMCOBJ=   -6544.60078711151     
 iteration          -91 MCMCOBJ=   -6556.31145903176     
 iteration          -90 MCMCOBJ=   -6556.31145960586     
 iteration          -89 MCMCOBJ=   -6566.25337224590     
 iteration          -88 MCMCOBJ=   -6551.04068817975     
 iteration          -87 MCMCOBJ=   -6611.82925033023     
 iteration          -86 MCMCOBJ=   -6567.15357211991     
 iteration          -85 MCMCOBJ=   -6609.58585024604     
 iteration          -84 MCMCOBJ=   -6658.28287323387     
 iteration          -83 MCMCOBJ=   -6671.14859487378     
 iteration          -82 MCMCOBJ=   -6662.25754648096     
 iteration          -81 MCMCOBJ=   -6632.38265954727     
 iteration          -80 MCMCOBJ=   -6590.56184024625     
 iteration          -79 MCMCOBJ=   -6590.56183943466     
 iteration          -78 MCMCOBJ=   -6593.50294235147     
 iteration          -77 MCMCOBJ=   -6650.70985098135     
 iteration          -76 MCMCOBJ=   -6643.67054469519     
 iteration          -75 MCMCOBJ=   -6638.78183984355     
 iteration          -74 MCMCOBJ=   -6645.73035284744     
 iteration          -73 MCMCOBJ=   -6647.13622399497     
 iteration          -72 MCMCOBJ=   -6639.71086841144     
 iteration          -71 MCMCOBJ=   -6656.09383252577     
 iteration          -70 MCMCOBJ=   -6621.32632036847     
 iteration          -69 MCMCOBJ=   -6577.97443310373     
 iteration          -68 MCMCOBJ=   -6639.23756984592     
 iteration          -67 MCMCOBJ=   -6641.34506123600     
 iteration          -66 MCMCOBJ=   -6605.53410301932     
 iteration          -65 MCMCOBJ=   -6609.33785765882     
 iteration          -64 MCMCOBJ=   -6634.56742763625     
 iteration          -63 MCMCOBJ=   -6604.30614243269     
 iteration          -62 MCMCOBJ=   -6634.50543876336     
 iteration          -61 MCMCOBJ=   -6635.02725002365     
 iteration          -60 MCMCOBJ=   -6571.57920511276     
 iteration          -59 MCMCOBJ=   -6563.18396937669     
 iteration          -58 MCMCOBJ=   -6567.45680279690     
 iteration          -57 MCMCOBJ=   -6593.88621346619     
 iteration          -56 MCMCOBJ=   -6579.65057498755     
 iteration          -55 MCMCOBJ=   -6556.79077608237     
 iteration          -54 MCMCOBJ=   -6583.44030329539     
 iteration          -53 MCMCOBJ=   -6605.76494585215     
 iteration          -52 MCMCOBJ=   -6604.92521636532     
 iteration          -51 MCMCOBJ=   -6592.78520432036     
 iteration          -50 MCMCOBJ=   -6600.51048240014     
 iteration          -49 MCMCOBJ=   -6620.79030354567     
 iteration          -48 MCMCOBJ=   -6630.99355457991     
 iteration          -47 MCMCOBJ=   -6620.79937338982     
 iteration          -46 MCMCOBJ=   -6599.58574577278     
 iteration          -45 MCMCOBJ=   -6595.74591847804     
 iteration          -44 MCMCOBJ=   -6577.30315721842     
 iteration          -43 MCMCOBJ=   -6603.62676748506     
 iteration          -42 MCMCOBJ=   -6545.38871984500     
 iteration          -41 MCMCOBJ=   -6537.51330406969     
 iteration          -40 MCMCOBJ=   -6533.89126025573     
 iteration          -39 MCMCOBJ=   -6541.70027765950     
 iteration          -38 MCMCOBJ=   -6584.43424335942     
 iteration          -37 MCMCOBJ=   -6575.43107085177     
 iteration          -36 MCMCOBJ=   -6633.96823078805     
 iteration          -35 MCMCOBJ=   -6565.68181611064     
 iteration          -34 MCMCOBJ=   -6656.43508642022     
 iteration          -33 MCMCOBJ=   -6647.20801444042     
 iteration          -32 MCMCOBJ=   -6646.72468185733     
 iteration          -31 MCMCOBJ=   -6626.42361964667     
 iteration          -30 MCMCOBJ=   -6641.07311959875     
 iteration          -29 MCMCOBJ=   -6631.13063106567     
 iteration          -28 MCMCOBJ=   -6578.94245493954     
 iteration          -27 MCMCOBJ=   -6594.14958551808     
 iteration          -26 MCMCOBJ=   -6600.36153546745     
 iteration          -25 MCMCOBJ=   -6511.85273521221     
 iteration          -24 MCMCOBJ=   -6519.16161311339     
 iteration          -23 MCMCOBJ=   -6600.03373543004     
 iteration          -22 MCMCOBJ=   -6582.86269891056     
 iteration          -21 MCMCOBJ=   -6587.77288037652     
 iteration          -20 MCMCOBJ=   -6579.13940352252     
 iteration          -19 MCMCOBJ=   -6565.89844895143     
 iteration          -18 MCMCOBJ=   -6547.25737505371     
 iteration          -17 MCMCOBJ=   -6568.86078564874     
 iteration          -16 MCMCOBJ=   -6571.41326546244     
 iteration          -15 MCMCOBJ=   -6576.22308490463     
 iteration          -14 MCMCOBJ=   -6610.26102919209     
 iteration          -13 MCMCOBJ=   -6566.55899957136     
 iteration          -12 MCMCOBJ=   -6560.20878705488     
 iteration          -11 MCMCOBJ=   -6554.22078751500     
 iteration          -10 MCMCOBJ=   -6552.99616422849     
 iteration           -9 MCMCOBJ=   -6597.84214359648     
 iteration           -8 MCMCOBJ=   -6628.81977008425     
 iteration           -7 MCMCOBJ=   -6636.73045646755     
 iteration           -6 MCMCOBJ=   -6576.48018531398     
 iteration           -5 MCMCOBJ=   -6566.56894982106     
 iteration           -4 MCMCOBJ=   -6589.48251460577     
 iteration           -3 MCMCOBJ=   -6592.75302895681     
 iteration           -2 MCMCOBJ=   -6630.24934753116     
 iteration           -1 MCMCOBJ=   -6640.20819899979     
 Sampling Mode
 iteration            0 MCMCOBJ=   -6660.18606168106     
 iteration            1 MCMCOBJ=   -6606.43895263954     
 iteration            2 MCMCOBJ=   -6594.40848000132     
 iteration            3 MCMCOBJ=   -6587.80883260959     
 iteration            4 MCMCOBJ=   -6571.44200922746     
 iteration            5 MCMCOBJ=   -6642.25362040205     
 iteration            6 MCMCOBJ=   -6627.41596777519     
 iteration            7 MCMCOBJ=   -6639.02814893226     
 iteration            8 MCMCOBJ=   -6616.88432410269     
 iteration            9 MCMCOBJ=   -6617.22530528960     
 iteration           10 MCMCOBJ=   -6630.65065517407     
 iteration           11 MCMCOBJ=   -6614.34130555306     
 iteration           12 MCMCOBJ=   -6614.34130575668     
 iteration           13 MCMCOBJ=   -6603.07506902832     
 iteration           14 MCMCOBJ=   -6622.99527122838     
 iteration           15 MCMCOBJ=   -6542.69075951935     
 iteration           16 MCMCOBJ=   -6563.11387768668     
 iteration           17 MCMCOBJ=   -6628.27001028671     
 iteration           18 MCMCOBJ=   -6626.90694215171     
 iteration           19 MCMCOBJ=   -6670.82614129893     
 iteration           20 MCMCOBJ=   -6636.28054044506     
 iteration           21 MCMCOBJ=   -6601.01933875999     
 iteration           22 MCMCOBJ=   -6630.84790040088     
 iteration           23 MCMCOBJ=   -6641.91683137608     
 iteration           24 MCMCOBJ=   -6608.43684746347     
 iteration           25 MCMCOBJ=   -6625.62223669192     
 iteration           26 MCMCOBJ=   -6614.69398299873     
 iteration           27 MCMCOBJ=   -6629.88905551885     
 iteration           28 MCMCOBJ=   -6643.42485393274     
 iteration           29 MCMCOBJ=   -6593.81212941916     
 iteration           30 MCMCOBJ=   -6591.32852153648     
 iteration           31 MCMCOBJ=   -6560.05466035513     
 iteration           32 MCMCOBJ=   -6588.13301513127     
 iteration           33 MCMCOBJ=   -6628.45228303214     
 iteration           34 MCMCOBJ=   -6603.61111229606     
 iteration           35 MCMCOBJ=   -6583.52576208119     
 iteration           36 MCMCOBJ=   -6596.49537093868     
 iteration           37 MCMCOBJ=   -6638.95783001325     
 iteration           38 MCMCOBJ=   -6624.36446743747     
 iteration           39 MCMCOBJ=   -6637.35628073933     
 iteration           40 MCMCOBJ=   -6645.44698482965     
 iteration           41 MCMCOBJ=   -6606.08995231935     
 iteration           42 MCMCOBJ=   -6626.09151467060     
 iteration           43 MCMCOBJ=   -6629.39138946202     
 iteration           44 MCMCOBJ=   -6599.07241483923     
 iteration           45 MCMCOBJ=   -6567.26469435918     
 iteration           46 MCMCOBJ=   -6559.99308397219     
 iteration           47 MCMCOBJ=   -6550.99936673329     
 iteration           48 MCMCOBJ=   -6595.43282012267     
 iteration           49 MCMCOBJ=   -6595.68895564838     
 iteration           50 MCMCOBJ=   -6542.90546739180     
 iteration           51 MCMCOBJ=   -6517.06239999401     
 iteration           52 MCMCOBJ=   -6554.45702631106     
 iteration           53 MCMCOBJ=   -6513.18031977594     
 iteration           54 MCMCOBJ=   -6595.88185675780     
 iteration           55 MCMCOBJ=   -6584.02795199115     
 iteration           56 MCMCOBJ=   -6541.41505918035     
 iteration           57 MCMCOBJ=   -6517.89113494931     
 iteration           58 MCMCOBJ=   -6603.05632967468     
 iteration           59 MCMCOBJ=   -6603.05632544868     
 iteration           60 MCMCOBJ=   -6606.36809993738     
 iteration           61 MCMCOBJ=   -6620.02238928167     
 iteration           62 MCMCOBJ=   -6604.33082010773     
 iteration           63 MCMCOBJ=   -6571.28183368369     
 iteration           64 MCMCOBJ=   -6631.26383820106     
 iteration           65 MCMCOBJ=   -6577.22148589246     
 iteration           66 MCMCOBJ=   -6558.84292542810     
 iteration           67 MCMCOBJ=   -6572.15350567843     
 iteration           68 MCMCOBJ=   -6579.89085657911     
 iteration           69 MCMCOBJ=   -6572.47863862347     
 iteration           70 MCMCOBJ=   -6577.50232533910     
 iteration           71 MCMCOBJ=   -6568.58429897695     
 iteration           72 MCMCOBJ=   -6576.80054707496     
 iteration           73 MCMCOBJ=   -6570.96195926239     
 iteration           74 MCMCOBJ=   -6597.06369119030     
 iteration           75 MCMCOBJ=   -6608.24406225024     
 iteration           76 MCMCOBJ=   -6608.02020162845     
 iteration           77 MCMCOBJ=   -6613.32836703669     
 iteration           78 MCMCOBJ=   -6571.49313995822     
 iteration           79 MCMCOBJ=   -6589.29805549076     
 iteration           80 MCMCOBJ=   -6587.13254066165     
 iteration           81 MCMCOBJ=   -6588.72921512230     
 iteration           82 MCMCOBJ=   -6641.19600854426     
 iteration           83 MCMCOBJ=   -6602.49688443703     
 iteration           84 MCMCOBJ=   -6598.04347240471     
 iteration           85 MCMCOBJ=   -6546.88034877483     
 iteration           86 MCMCOBJ=   -6629.45323323378     
 iteration           87 MCMCOBJ=   -6593.70439951494     
 iteration           88 MCMCOBJ=   -6631.24384650289     
 iteration           89 MCMCOBJ=   -6608.31292018446     
 iteration           90 MCMCOBJ=   -6617.05112233657     
 iteration           91 MCMCOBJ=   -6606.57065396858     
 iteration           92 MCMCOBJ=   -6587.74151662924     
 iteration           93 MCMCOBJ=   -6602.11488607538     
 iteration           94 MCMCOBJ=   -6609.57899489009     
 iteration           95 MCMCOBJ=   -6656.95217812961     
 iteration           96 MCMCOBJ=   -6610.82286636997     
 iteration           97 MCMCOBJ=   -6574.57097141033     
 iteration           98 MCMCOBJ=   -6599.08431923023     
 iteration           99 MCMCOBJ=   -6634.88989007255     
 iteration          100 MCMCOBJ=   -6651.74300556069     
 iteration          101 MCMCOBJ=   -6634.48500546497     
 iteration          102 MCMCOBJ=   -6664.83719416422     
 iteration          103 MCMCOBJ=   -6565.79570648774     
 iteration          104 MCMCOBJ=   -6567.27734163395     
 iteration          105 MCMCOBJ=   -6626.21570631926     
 iteration          106 MCMCOBJ=   -6638.14860870188     
 iteration          107 MCMCOBJ=   -6565.14229220313     
 iteration          108 MCMCOBJ=   -6590.47610167691     
 iteration          109 MCMCOBJ=   -6583.80441870356     
 iteration          110 MCMCOBJ=   -6531.91863519082     
 iteration          111 MCMCOBJ=   -6556.82661963638     
 iteration          112 MCMCOBJ=   -6532.99051540247     
 iteration          113 MCMCOBJ=   -6554.27625034611     
 iteration          114 MCMCOBJ=   -6491.27697619459     
 iteration          115 MCMCOBJ=   -6538.97387964415     
 iteration          116 MCMCOBJ=   -6551.35473711266     
 iteration          117 MCMCOBJ=   -6601.56064994547     
 iteration          118 MCMCOBJ=   -6599.05248201092     
 iteration          119 MCMCOBJ=   -6601.64406709894     
 iteration          120 MCMCOBJ=   -6608.30958295356     
 iteration          121 MCMCOBJ=   -6609.15413063752     
 iteration          122 MCMCOBJ=   -6562.43610780037     
 iteration          123 MCMCOBJ=   -6593.44013109006     
 iteration          124 MCMCOBJ=   -6648.41065172889     
 iteration          125 MCMCOBJ=   -6620.05860527140     
 iteration          126 MCMCOBJ=   -6653.18639434509     
 iteration          127 MCMCOBJ=   -6591.19161699976     
 iteration          128 MCMCOBJ=   -6562.71657246253     
 iteration          129 MCMCOBJ=   -6580.27124900083     
 iteration          130 MCMCOBJ=   -6574.60245785267     
 iteration          131 MCMCOBJ=   -6590.60056441440     
 iteration          132 MCMCOBJ=   -6629.26202221779     
 iteration          133 MCMCOBJ=   -6578.51209807759     
 iteration          134 MCMCOBJ=   -6602.76941146522     
 iteration          135 MCMCOBJ=   -6627.79893035470     
 iteration          136 MCMCOBJ=   -6603.19365010526     
 iteration          137 MCMCOBJ=   -6581.72071256477     
 iteration          138 MCMCOBJ=   -6581.63712573685     
 iteration          139 MCMCOBJ=   -6589.84992766838     
 iteration          140 MCMCOBJ=   -6603.24225030584     
 iteration          141 MCMCOBJ=   -6583.07063164027     
 iteration          142 MCMCOBJ=   -6544.73575403017     
 iteration          143 MCMCOBJ=   -6545.77672677656     
 iteration          144 MCMCOBJ=   -6554.09461669281     
 iteration          145 MCMCOBJ=   -6591.72774724142     
 iteration          146 MCMCOBJ=   -6622.03464031887     
 iteration          147 MCMCOBJ=   -6604.08664111064     
 iteration          148 MCMCOBJ=   -6616.67670442223     
 iteration          149 MCMCOBJ=   -6604.13928815577     
 iteration          150 MCMCOBJ=   -6572.38071178141     
 iteration          151 MCMCOBJ=   -6593.86584828030     
 iteration          152 MCMCOBJ=   -6620.08910819010     
 iteration          153 MCMCOBJ=   -6618.29632926998     
 iteration          154 MCMCOBJ=   -6624.48224853160     
 iteration          155 MCMCOBJ=   -6656.92918490393     
 iteration          156 MCMCOBJ=   -6629.28453334371     
 iteration          157 MCMCOBJ=   -6640.23202506884     
 iteration          158 MCMCOBJ=   -6670.65589105208     
 iteration          159 MCMCOBJ=   -6552.43235910007     
 iteration          160 MCMCOBJ=   -6597.73621182219     
 iteration          161 MCMCOBJ=   -6557.45210188505     
 iteration          162 MCMCOBJ=   -6615.38749492019     
 iteration          163 MCMCOBJ=   -6604.68516694300     
 iteration          164 MCMCOBJ=   -6617.23772134889     
 iteration          165 MCMCOBJ=   -6654.47535580839     
 iteration          166 MCMCOBJ=   -6639.66789961655     
 iteration          167 MCMCOBJ=   -6613.00213210890     
 iteration          168 MCMCOBJ=   -6588.10025008943     
 iteration          169 MCMCOBJ=   -6565.05953247807     
 iteration          170 MCMCOBJ=   -6590.55873135812     
 iteration          171 MCMCOBJ=   -6609.65908200833     
 iteration          172 MCMCOBJ=   -6651.66983723360     
 iteration          173 MCMCOBJ=   -6638.80075939286     
 iteration          174 MCMCOBJ=   -6588.39544636912     
 iteration          175 MCMCOBJ=   -6584.66043789492     
 iteration          176 MCMCOBJ=   -6558.44105734340     
 iteration          177 MCMCOBJ=   -6587.11545470746     
 iteration          178 MCMCOBJ=   -6598.51663171438     
 iteration          179 MCMCOBJ=   -6619.49136990324     
 iteration          180 MCMCOBJ=   -6578.60830855125     
 iteration          181 MCMCOBJ=   -6581.58948022095     
 iteration          182 MCMCOBJ=   -6558.25220962828     
 iteration          183 MCMCOBJ=   -6623.40066968484     
 iteration          184 MCMCOBJ=   -6624.67684083360     
 iteration          185 MCMCOBJ=   -6515.55117030603     
 iteration          186 MCMCOBJ=   -6508.31521647717     
 iteration          187 MCMCOBJ=   -6501.17808564365     
 iteration          188 MCMCOBJ=   -6517.99556857725     
 iteration          189 MCMCOBJ=   -6568.87995503844     
 iteration          190 MCMCOBJ=   -6562.69201416100     
 iteration          191 MCMCOBJ=   -6560.78767931454     
 iteration          192 MCMCOBJ=   -6606.32775428222     
 iteration          193 MCMCOBJ=   -6591.75773554633     
 iteration          194 MCMCOBJ=   -6634.05646744999     
 iteration          195 MCMCOBJ=   -6663.23976811406     
 iteration          196 MCMCOBJ=   -6601.38984020161     
 iteration          197 MCMCOBJ=   -6600.16859729188     
 iteration          198 MCMCOBJ=   -6573.87385104332     
 iteration          199 MCMCOBJ=   -6583.31223535841     
 iteration          200 MCMCOBJ=   -6589.84374369408     
 iteration          201 MCMCOBJ=   -6586.11761984243     
 iteration          202 MCMCOBJ=   -6586.33730027638     
 iteration          203 MCMCOBJ=   -6593.09395190862     
 iteration          204 MCMCOBJ=   -6592.09838189254     
 iteration          205 MCMCOBJ=   -6615.23162774717     
 iteration          206 MCMCOBJ=   -6591.38299138810     
 iteration          207 MCMCOBJ=   -6555.24535391877     
 iteration          208 MCMCOBJ=   -6579.55237174876     
 iteration          209 MCMCOBJ=   -6614.45820255163     
 iteration          210 MCMCOBJ=   -6628.13145094848     
 iteration          211 MCMCOBJ=   -6579.20143402839     
 iteration          212 MCMCOBJ=   -6567.40310451420     
 iteration          213 MCMCOBJ=   -6555.57302488158     
 iteration          214 MCMCOBJ=   -6610.50780992171     
 iteration          215 MCMCOBJ=   -6548.39127114053     
 iteration          216 MCMCOBJ=   -6545.24016046449     
 iteration          217 MCMCOBJ=   -6594.36702258895     
 iteration          218 MCMCOBJ=   -6589.06014047342     
 iteration          219 MCMCOBJ=   -6621.06686995274     
 iteration          220 MCMCOBJ=   -6562.52244881031     
 iteration          221 MCMCOBJ=   -6600.70740006260     
 iteration          222 MCMCOBJ=   -6557.71406155992     
 iteration          223 MCMCOBJ=   -6572.06504595526     
 iteration          224 MCMCOBJ=   -6563.63434392272     
 iteration          225 MCMCOBJ=   -6537.66798840086     
 iteration          226 MCMCOBJ=   -6553.10703526214     
 iteration          227 MCMCOBJ=   -6574.31812033409     
 iteration          228 MCMCOBJ=   -6575.47203257277     
 iteration          229 MCMCOBJ=   -6571.63182856118     
 iteration          230 MCMCOBJ=   -6584.43135593122     
 iteration          231 MCMCOBJ=   -6609.54641281472     
 iteration          232 MCMCOBJ=   -6587.07892161236     
 iteration          233 MCMCOBJ=   -6591.59981674892     
 iteration          234 MCMCOBJ=   -6565.73303387039     
 iteration          235 MCMCOBJ=   -6550.51539001788     
 iteration          236 MCMCOBJ=   -6534.63372572610     
 iteration          237 MCMCOBJ=   -6551.80319774462     
 iteration          238 MCMCOBJ=   -6591.07738831897     
 iteration          239 MCMCOBJ=   -6570.55939718605     
 iteration          240 MCMCOBJ=   -6579.90592823443     
 iteration          241 MCMCOBJ=   -6566.82455945216     
 iteration          242 MCMCOBJ=   -6587.97750604885     
 iteration          243 MCMCOBJ=   -6572.74320567093     
 iteration          244 MCMCOBJ=   -6611.80158780457     
 iteration          245 MCMCOBJ=   -6594.11523411554     
 iteration          246 MCMCOBJ=   -6615.45835130714     
 iteration          247 MCMCOBJ=   -6623.80069025009     
 iteration          248 MCMCOBJ=   -6588.38779890592     
 iteration          249 MCMCOBJ=   -6604.87338823367     
 iteration          250 MCMCOBJ=   -6551.14696259784     
 iteration          251 MCMCOBJ=   -6609.91851350960     
 iteration          252 MCMCOBJ=   -6574.13436552680     
 iteration          253 MCMCOBJ=   -6596.46325136327     
 iteration          254 MCMCOBJ=   -6578.35016898805     
 iteration          255 MCMCOBJ=   -6559.92134794133     
 iteration          256 MCMCOBJ=   -6553.60588863307     
 iteration          257 MCMCOBJ=   -6510.04749086984     
 iteration          258 MCMCOBJ=   -6523.80628415286     
 iteration          259 MCMCOBJ=   -6536.38948066381     
 iteration          260 MCMCOBJ=   -6574.92116675872     
 iteration          261 MCMCOBJ=   -6554.72392272712     
 iteration          262 MCMCOBJ=   -6521.36179773065     
 iteration          263 MCMCOBJ=   -6553.01374568569     
 iteration          264 MCMCOBJ=   -6569.82085269981     
 iteration          265 MCMCOBJ=   -6601.67740406567     
 iteration          266 MCMCOBJ=   -6541.29961366950     
 iteration          267 MCMCOBJ=   -6573.24774112085     
 iteration          268 MCMCOBJ=   -6649.17278281138     
 iteration          269 MCMCOBJ=   -6627.99269560559     
 iteration          270 MCMCOBJ=   -6608.84343019759     
 iteration          271 MCMCOBJ=   -6554.34551580773     
 iteration          272 MCMCOBJ=   -6595.36046631006     
 iteration          273 MCMCOBJ=   -6629.29337287880     
 iteration          274 MCMCOBJ=   -6620.38298250738     
 iteration          275 MCMCOBJ=   -6594.08931706354     
 iteration          276 MCMCOBJ=   -6614.58297474115     
 iteration          277 MCMCOBJ=   -6603.44142276065     
 iteration          278 MCMCOBJ=   -6607.92632000350     
 iteration          279 MCMCOBJ=   -6622.40449570010     
 iteration          280 MCMCOBJ=   -6584.52175558736     
 iteration          281 MCMCOBJ=   -6577.79810010057     
 iteration          282 MCMCOBJ=   -6582.93823389696     
 iteration          283 MCMCOBJ=   -6567.04079781521     
 iteration          284 MCMCOBJ=   -6540.01567730077     
 iteration          285 MCMCOBJ=   -6545.14016935181     
 iteration          286 MCMCOBJ=   -6546.38548107140     
 iteration          287 MCMCOBJ=   -6542.06927025027     
 iteration          288 MCMCOBJ=   -6496.04414771795     
 iteration          289 MCMCOBJ=   -6562.21310735335     
 iteration          290 MCMCOBJ=   -6586.80662892231     
 iteration          291 MCMCOBJ=   -6582.08530566788     
 iteration          292 MCMCOBJ=   -6536.32099494210     
 iteration          293 MCMCOBJ=   -6535.81167102532     
 iteration          294 MCMCOBJ=   -6545.44997632853     
 iteration          295 MCMCOBJ=   -6535.08383298779     
 iteration          296 MCMCOBJ=   -6587.07477101532     
 iteration          297 MCMCOBJ=   -6630.63510029525     
 iteration          298 MCMCOBJ=   -6636.80311029221     
 iteration          299 MCMCOBJ=   -6675.55488740773     
 iteration          300 MCMCOBJ=   -6656.84815950542     
 iteration          301 MCMCOBJ=   -6632.21074436142     
 iteration          302 MCMCOBJ=   -6595.73330017613     
 iteration          303 MCMCOBJ=   -6648.85557286065     
 iteration          304 MCMCOBJ=   -6616.69634401747     
 iteration          305 MCMCOBJ=   -6587.22866740150     
 iteration          306 MCMCOBJ=   -6562.94982735739     
 iteration          307 MCMCOBJ=   -6606.24621456194     
 iteration          308 MCMCOBJ=   -6567.11228778088     
 iteration          309 MCMCOBJ=   -6624.45197960157     
 iteration          310 MCMCOBJ=   -6572.08956589753     
 iteration          311 MCMCOBJ=   -6569.71645156044     
 iteration          312 MCMCOBJ=   -6576.51595819645     
 iteration          313 MCMCOBJ=   -6586.56622794915     
 iteration          314 MCMCOBJ=   -6543.49934882024     
 iteration          315 MCMCOBJ=   -6483.72121974317     
 iteration          316 MCMCOBJ=   -6507.29242963333     
 iteration          317 MCMCOBJ=   -6534.58692716526     
 iteration          318 MCMCOBJ=   -6525.94036671325     
 iteration          319 MCMCOBJ=   -6526.80619766674     
 iteration          320 MCMCOBJ=   -6507.87798014165     
 iteration          321 MCMCOBJ=   -6555.23792770142     
 iteration          322 MCMCOBJ=   -6609.48389647661     
 iteration          323 MCMCOBJ=   -6578.17388847384     
 iteration          324 MCMCOBJ=   -6597.32799488855     
 iteration          325 MCMCOBJ=   -6616.44204055745     
 iteration          326 MCMCOBJ=   -6639.34044685160     
 iteration          327 MCMCOBJ=   -6639.34044711251     
 iteration          328 MCMCOBJ=   -6615.50729871560     
 iteration          329 MCMCOBJ=   -6606.30486715848     
 iteration          330 MCMCOBJ=   -6639.95355529437     
 iteration          331 MCMCOBJ=   -6558.79383368980     
 iteration          332 MCMCOBJ=   -6574.43071583375     
 iteration          333 MCMCOBJ=   -6552.95788850546     
 iteration          334 MCMCOBJ=   -6601.48873236203     
 iteration          335 MCMCOBJ=   -6587.03405427628     
 iteration          336 MCMCOBJ=   -6624.47488630449     
 iteration          337 MCMCOBJ=   -6589.30086461027     
 iteration          338 MCMCOBJ=   -6574.82247530788     
 iteration          339 MCMCOBJ=   -6613.74922281286     
 iteration          340 MCMCOBJ=   -6568.28454637229     
 iteration          341 MCMCOBJ=   -6554.02394204609     
 iteration          342 MCMCOBJ=   -6572.59356479194     
 iteration          343 MCMCOBJ=   -6601.55121476452     
 iteration          344 MCMCOBJ=   -6595.15025833210     
 iteration          345 MCMCOBJ=   -6563.12016424756     
 iteration          346 MCMCOBJ=   -6600.24119000570     
 iteration          347 MCMCOBJ=   -6583.01419188487     
 iteration          348 MCMCOBJ=   -6622.90938328217     
 iteration          349 MCMCOBJ=   -6586.71259278295     
 iteration          350 MCMCOBJ=   -6581.32403960688     
 iteration          351 MCMCOBJ=   -6602.75268117671     
 iteration          352 MCMCOBJ=   -6594.10701459793     
 iteration          353 MCMCOBJ=   -6634.49652041223     
 iteration          354 MCMCOBJ=   -6584.54554997188     
 iteration          355 MCMCOBJ=   -6581.07151399882     
 iteration          356 MCMCOBJ=   -6616.38799349005     
 iteration          357 MCMCOBJ=   -6615.35479255350     
 iteration          358 MCMCOBJ=   -6578.10896096023     
 iteration          359 MCMCOBJ=   -6558.58236715752     
 iteration          360 MCMCOBJ=   -6634.37051758193     
 iteration          361 MCMCOBJ=   -6641.70388846179     
 iteration          362 MCMCOBJ=   -6606.16539891366     
 iteration          363 MCMCOBJ=   -6654.10215461739     
 iteration          364 MCMCOBJ=   -6627.94607387234     
 iteration          365 MCMCOBJ=   -6645.84133735972     
 iteration          366 MCMCOBJ=   -6653.61205124088     
 iteration          367 MCMCOBJ=   -6639.03852771525     
 iteration          368 MCMCOBJ=   -6607.35475768745     
 iteration          369 MCMCOBJ=   -6597.04730374518     
 iteration          370 MCMCOBJ=   -6647.71930579083     
 iteration          371 MCMCOBJ=   -6575.03376577438     
 iteration          372 MCMCOBJ=   -6576.02853124615     
 iteration          373 MCMCOBJ=   -6582.06774364807     
 iteration          374 MCMCOBJ=   -6589.09747309607     
 iteration          375 MCMCOBJ=   -6645.88418469867     
 iteration          376 MCMCOBJ=   -6657.25418621571     
 iteration          377 MCMCOBJ=   -6630.76106456523     
 iteration          378 MCMCOBJ=   -6627.38963611324     
 iteration          379 MCMCOBJ=   -6594.04237277762     
 iteration          380 MCMCOBJ=   -6610.29516539059     
 iteration          381 MCMCOBJ=   -6621.35942647060     
 iteration          382 MCMCOBJ=   -6601.94057857448     
 iteration          383 MCMCOBJ=   -6645.43679734464     
 iteration          384 MCMCOBJ=   -6633.43140185953     
 iteration          385 MCMCOBJ=   -6640.81899704087     
 iteration          386 MCMCOBJ=   -6637.46220366809     
 iteration          387 MCMCOBJ=   -6634.38449254680     
 iteration          388 MCMCOBJ=   -6571.84393003069     
 iteration          389 MCMCOBJ=   -6605.07718764579     
 iteration          390 MCMCOBJ=   -6650.50932918947     
 iteration          391 MCMCOBJ=   -6674.85193204920     
 iteration          392 MCMCOBJ=   -6594.84175823092     
 iteration          393 MCMCOBJ=   -6634.67373298490     
 iteration          394 MCMCOBJ=   -6588.23367282731     
 iteration          395 MCMCOBJ=   -6594.09187449096     
 iteration          396 MCMCOBJ=   -6565.43845860231     
 iteration          397 MCMCOBJ=   -6595.16920875063     
 iteration          398 MCMCOBJ=   -6596.09778804269     
 iteration          399 MCMCOBJ=   -6624.31694425482     
 iteration          400 MCMCOBJ=   -6607.66622849430     
 iteration          401 MCMCOBJ=   -6616.12748038933     
 iteration          402 MCMCOBJ=   -6585.87824533212     
 iteration          403 MCMCOBJ=   -6532.17209069749     
 iteration          404 MCMCOBJ=   -6530.69329499382     
 iteration          405 MCMCOBJ=   -6567.56275961547     
 iteration          406 MCMCOBJ=   -6554.92662351754     
 iteration          407 MCMCOBJ=   -6583.63110759771     
 iteration          408 MCMCOBJ=   -6536.46725042528     
 iteration          409 MCMCOBJ=   -6540.29594328970     
 iteration          410 MCMCOBJ=   -6565.97667599847     
 iteration          411 MCMCOBJ=   -6562.64503921868     
 iteration          412 MCMCOBJ=   -6477.41933364052     
 iteration          413 MCMCOBJ=   -6521.48278811743     
 iteration          414 MCMCOBJ=   -6502.08279411997     
 iteration          415 MCMCOBJ=   -6476.84935245643     
 iteration          416 MCMCOBJ=   -6572.12554330354     
 iteration          417 MCMCOBJ=   -6641.42713626510     
 iteration          418 MCMCOBJ=   -6651.33026189354     
 iteration          419 MCMCOBJ=   -6629.36821211878     
 iteration          420 MCMCOBJ=   -6614.36848347844     
 iteration          421 MCMCOBJ=   -6616.09710447700     
 iteration          422 MCMCOBJ=   -6653.52282528700     
 iteration          423 MCMCOBJ=   -6659.15519948846     
 iteration          424 MCMCOBJ=   -6664.98341526490     
 iteration          425 MCMCOBJ=   -6619.39557586073     
 iteration          426 MCMCOBJ=   -6544.59081666036     
 iteration          427 MCMCOBJ=   -6545.31032294682     
 iteration          428 MCMCOBJ=   -6629.12695266273     
 iteration          429 MCMCOBJ=   -6589.92665520519     
 iteration          430 MCMCOBJ=   -6580.47699218748     
 iteration          431 MCMCOBJ=   -6572.38757892558     
 iteration          432 MCMCOBJ=   -6610.13852680943     
 iteration          433 MCMCOBJ=   -6662.83709380960     
 iteration          434 MCMCOBJ=   -6634.00876123411     
 iteration          435 MCMCOBJ=   -6640.90142279426     
 iteration          436 MCMCOBJ=   -6638.39197070538     
 iteration          437 MCMCOBJ=   -6647.64819559098     
 iteration          438 MCMCOBJ=   -6616.77698071190     
 iteration          439 MCMCOBJ=   -6623.53199693336     
 iteration          440 MCMCOBJ=   -6630.52936694513     
 iteration          441 MCMCOBJ=   -6619.64815589815     
 iteration          442 MCMCOBJ=   -6613.55855451685     
 iteration          443 MCMCOBJ=   -6585.63682696154     
 iteration          444 MCMCOBJ=   -6547.78816726251     
 iteration          445 MCMCOBJ=   -6581.92055630083     
 iteration          446 MCMCOBJ=   -6627.42004558350     
 iteration          447 MCMCOBJ=   -6613.49774952841     
 iteration          448 MCMCOBJ=   -6612.56875338415     
 iteration          449 MCMCOBJ=   -6574.18928187196     
 iteration          450 MCMCOBJ=   -6564.51727503459     
 iteration          451 MCMCOBJ=   -6586.27466287950     
 iteration          452 MCMCOBJ=   -6625.51100624888     
 iteration          453 MCMCOBJ=   -6626.77349705912     
 iteration          454 MCMCOBJ=   -6602.13181576200     
 iteration          455 MCMCOBJ=   -6619.76828020680     
 iteration          456 MCMCOBJ=   -6610.57625787207     
 iteration          457 MCMCOBJ=   -6607.09725023616     
 iteration          458 MCMCOBJ=   -6597.61293169280     
 iteration          459 MCMCOBJ=   -6612.16926623024     
 iteration          460 MCMCOBJ=   -6611.14889037945     
 iteration          461 MCMCOBJ=   -6606.36042329454     
 iteration          462 MCMCOBJ=   -6613.16570761283     
 iteration          463 MCMCOBJ=   -6584.15972620585     
 iteration          464 MCMCOBJ=   -6584.03015640638     
 iteration          465 MCMCOBJ=   -6636.12813655994     
 iteration          466 MCMCOBJ=   -6657.35394926638     
 iteration          467 MCMCOBJ=   -6593.94055733070     
 iteration          468 MCMCOBJ=   -6570.47183529282     
 iteration          469 MCMCOBJ=   -6541.31837272819     
 iteration          470 MCMCOBJ=   -6533.92470147201     
 iteration          471 MCMCOBJ=   -6540.89477255671     
 iteration          472 MCMCOBJ=   -6584.66025736814     
 iteration          473 MCMCOBJ=   -6629.12567634741     
 iteration          474 MCMCOBJ=   -6617.21241324666     
 iteration          475 MCMCOBJ=   -6650.92747411544     
 iteration          476 MCMCOBJ=   -6634.89671868914     
 iteration          477 MCMCOBJ=   -6631.63287752108     
 iteration          478 MCMCOBJ=   -6584.44080133187     
 iteration          479 MCMCOBJ=   -6543.71296734715     
 iteration          480 MCMCOBJ=   -6497.02074540737     
 iteration          481 MCMCOBJ=   -6469.72908891389     
 iteration          482 MCMCOBJ=   -6520.88654963439     
 iteration          483 MCMCOBJ=   -6530.12543308157     
 iteration          484 MCMCOBJ=   -6549.62449746022     
 iteration          485 MCMCOBJ=   -6568.61431677337     
 iteration          486 MCMCOBJ=   -6557.50393976374     
 iteration          487 MCMCOBJ=   -6575.48991181940     
 iteration          488 MCMCOBJ=   -6533.36226681383     
 iteration          489 MCMCOBJ=   -6604.89738627352     
 iteration          490 MCMCOBJ=   -6628.46422836945     
 iteration          491 MCMCOBJ=   -6602.45001681547     
 iteration          492 MCMCOBJ=   -6553.17582938972     
 iteration          493 MCMCOBJ=   -6601.64621075042     
 iteration          494 MCMCOBJ=   -6617.64151064629     
 iteration          495 MCMCOBJ=   -6630.61719176862     
 iteration          496 MCMCOBJ=   -6619.42726202594     
 iteration          497 MCMCOBJ=   -6597.37483227202     
 iteration          498 MCMCOBJ=   -6543.08788732107     
 iteration          499 MCMCOBJ=   -6549.90658917284     
 iteration          500 MCMCOBJ=   -6568.31802812892     
 iteration          501 MCMCOBJ=   -6561.62852455942     
 iteration          502 MCMCOBJ=   -6545.54826462646     
 iteration          503 MCMCOBJ=   -6604.66690917959     
 iteration          504 MCMCOBJ=   -6624.82158644299     
 iteration          505 MCMCOBJ=   -6646.80691465187     
 iteration          506 MCMCOBJ=   -6606.41981265338     
 iteration          507 MCMCOBJ=   -6620.94743099431     
 iteration          508 MCMCOBJ=   -6598.17368547181     
 iteration          509 MCMCOBJ=   -6644.99825938290     
 iteration          510 MCMCOBJ=   -6638.18419307602     
 iteration          511 MCMCOBJ=   -6649.34208158967     
 iteration          512 MCMCOBJ=   -6646.81333247774     
 iteration          513 MCMCOBJ=   -6551.40565969599     
 iteration          514 MCMCOBJ=   -6657.74664385650     
 iteration          515 MCMCOBJ=   -6598.49031567761     
 iteration          516 MCMCOBJ=   -6619.71290705623     
 iteration          517 MCMCOBJ=   -6627.79166079249     
 iteration          518 MCMCOBJ=   -6643.65475017708     
 iteration          519 MCMCOBJ=   -6601.77287039713     
 iteration          520 MCMCOBJ=   -6687.39779472112     
 iteration          521 MCMCOBJ=   -6627.85486393172     
 iteration          522 MCMCOBJ=   -6603.11517949436     
 iteration          523 MCMCOBJ=   -6606.66010337116     
 iteration          524 MCMCOBJ=   -6581.70498816031     
 iteration          525 MCMCOBJ=   -6592.77422267211     
 iteration          526 MCMCOBJ=   -6519.93949673898     
 iteration          527 MCMCOBJ=   -6558.78411738008     
 iteration          528 MCMCOBJ=   -6610.02520017273     
 iteration          529 MCMCOBJ=   -6598.66348044383     
 iteration          530 MCMCOBJ=   -6562.83201164920     
 iteration          531 MCMCOBJ=   -6572.03096019941     
 iteration          532 MCMCOBJ=   -6547.00024980342     
 iteration          533 MCMCOBJ=   -6543.49476689941     
 iteration          534 MCMCOBJ=   -6559.68960100414     
 iteration          535 MCMCOBJ=   -6551.88587980621     
 iteration          536 MCMCOBJ=   -6557.84923079394     
 iteration          537 MCMCOBJ=   -6609.04751644056     
 iteration          538 MCMCOBJ=   -6591.40417378207     
 iteration          539 MCMCOBJ=   -6575.99706673762     
 iteration          540 MCMCOBJ=   -6533.91194625382     
 iteration          541 MCMCOBJ=   -6563.16671066703     
 iteration          542 MCMCOBJ=   -6626.14648337828     
 iteration          543 MCMCOBJ=   -6561.87289437792     
 iteration          544 MCMCOBJ=   -6583.77865137310     
 iteration          545 MCMCOBJ=   -6625.37740430512     
 iteration          546 MCMCOBJ=   -6675.76696288635     
 iteration          547 MCMCOBJ=   -6679.33724712511     
 iteration          548 MCMCOBJ=   -6611.63532024631     
 iteration          549 MCMCOBJ=   -6653.51104611233     
 iteration          550 MCMCOBJ=   -6635.84125179846     
 iteration          551 MCMCOBJ=   -6677.54574183107     
 iteration          552 MCMCOBJ=   -6677.58156808585     
 iteration          553 MCMCOBJ=   -6660.89921945847     
 iteration          554 MCMCOBJ=   -6660.89921958801     
 iteration          555 MCMCOBJ=   -6678.50510429032     
 iteration          556 MCMCOBJ=   -6681.97147911018     
 iteration          557 MCMCOBJ=   -6683.92080309125     
 iteration          558 MCMCOBJ=   -6571.96850897221     
 iteration          559 MCMCOBJ=   -6617.17478220029     
 iteration          560 MCMCOBJ=   -6633.88220114808     
 iteration          561 MCMCOBJ=   -6533.92879086784     
 iteration          562 MCMCOBJ=   -6479.53293835365     
 iteration          563 MCMCOBJ=   -6497.12278659328     
 iteration          564 MCMCOBJ=   -6536.86718152252     
 iteration          565 MCMCOBJ=   -6603.33578988884     
 iteration          566 MCMCOBJ=   -6629.12996133295     
 iteration          567 MCMCOBJ=   -6564.15187547605     
 iteration          568 MCMCOBJ=   -6571.92129499139     
 iteration          569 MCMCOBJ=   -6640.18225296179     
 iteration          570 MCMCOBJ=   -6589.67156008184     
 iteration          571 MCMCOBJ=   -6608.77932978846     
 iteration          572 MCMCOBJ=   -6587.50906295505     
 iteration          573 MCMCOBJ=   -6581.00120294573     
 iteration          574 MCMCOBJ=   -6581.10739170666     
 iteration          575 MCMCOBJ=   -6531.66251140600     
 iteration          576 MCMCOBJ=   -6542.68314215041     
 iteration          577 MCMCOBJ=   -6618.63132632899     
 iteration          578 MCMCOBJ=   -6621.78829458951     
 iteration          579 MCMCOBJ=   -6583.01581109334     
 iteration          580 MCMCOBJ=   -6651.04708459906     
 iteration          581 MCMCOBJ=   -6648.65683329294     
 iteration          582 MCMCOBJ=   -6672.31963045904     
 iteration          583 MCMCOBJ=   -6638.74106160046     
 iteration          584 MCMCOBJ=   -6627.23582116227     
 iteration          585 MCMCOBJ=   -6563.92784321744     
 iteration          586 MCMCOBJ=   -6630.54432783557     
 iteration          587 MCMCOBJ=   -6615.80012637509     
 iteration          588 MCMCOBJ=   -6620.97092991664     
 iteration          589 MCMCOBJ=   -6599.07942860663     
 iteration          590 MCMCOBJ=   -6613.15432075322     
 iteration          591 MCMCOBJ=   -6568.83087867677     
 iteration          592 MCMCOBJ=   -6556.96703826928     
 iteration          593 MCMCOBJ=   -6549.22567466632     
 iteration          594 MCMCOBJ=   -6575.95503218786     
 iteration          595 MCMCOBJ=   -6557.90215446609     
 iteration          596 MCMCOBJ=   -6561.78082013782     
 iteration          597 MCMCOBJ=   -6623.99936363745     
 iteration          598 MCMCOBJ=   -6596.80891979208     
 iteration          599 MCMCOBJ=   -6595.28713090837     
 iteration          600 MCMCOBJ=   -6619.26160328630     
 iteration          601 MCMCOBJ=   -6650.42108194909     
 iteration          602 MCMCOBJ=   -6614.10842703143     
 iteration          603 MCMCOBJ=   -6617.86492086347     
 iteration          604 MCMCOBJ=   -6611.59301855811     
 iteration          605 MCMCOBJ=   -6600.56136360454     
 iteration          606 MCMCOBJ=   -6626.03510784981     
 iteration          607 MCMCOBJ=   -6635.75404241044     
 iteration          608 MCMCOBJ=   -6556.45133146340     
 iteration          609 MCMCOBJ=   -6614.12026443897     
 iteration          610 MCMCOBJ=   -6561.47410501778     
 iteration          611 MCMCOBJ=   -6580.87680018664     
 iteration          612 MCMCOBJ=   -6618.54425795325     
 iteration          613 MCMCOBJ=   -6584.22613962625     
 iteration          614 MCMCOBJ=   -6615.15832013496     
 iteration          615 MCMCOBJ=   -6603.52864938784     
 iteration          616 MCMCOBJ=   -6598.34990332687     
 iteration          617 MCMCOBJ=   -6612.74826976473     
 iteration          618 MCMCOBJ=   -6625.83138118609     
 iteration          619 MCMCOBJ=   -6615.23116049996     
 iteration          620 MCMCOBJ=   -6589.75954057865     
 iteration          621 MCMCOBJ=   -6535.05887256835     
 iteration          622 MCMCOBJ=   -6621.58743555942     
 iteration          623 MCMCOBJ=   -6598.51556442713     
 iteration          624 MCMCOBJ=   -6600.33814271695     
 iteration          625 MCMCOBJ=   -6621.88838402663     
 iteration          626 MCMCOBJ=   -6606.76538751249     
 iteration          627 MCMCOBJ=   -6654.85701740916     
 iteration          628 MCMCOBJ=   -6679.91101476048     
 iteration          629 MCMCOBJ=   -6624.38288666001     
 iteration          630 MCMCOBJ=   -6581.57068397222     
 iteration          631 MCMCOBJ=   -6642.49279941392     
 iteration          632 MCMCOBJ=   -6579.86103566830     
 iteration          633 MCMCOBJ=   -6598.22841665787     
 iteration          634 MCMCOBJ=   -6604.83584100854     
 iteration          635 MCMCOBJ=   -6615.48155740238     
 iteration          636 MCMCOBJ=   -6641.70473237239     
 iteration          637 MCMCOBJ=   -6561.74382423347     
 iteration          638 MCMCOBJ=   -6580.82697475852     
 iteration          639 MCMCOBJ=   -6583.57832013003     
 iteration          640 MCMCOBJ=   -6638.77306171098     
 iteration          641 MCMCOBJ=   -6625.98612170654     
 iteration          642 MCMCOBJ=   -6584.95792391459     
 iteration          643 MCMCOBJ=   -6581.63758071428     
 iteration          644 MCMCOBJ=   -6593.83428750344     
 iteration          645 MCMCOBJ=   -6579.49247786858     
 iteration          646 MCMCOBJ=   -6618.04732277781     
 iteration          647 MCMCOBJ=   -6563.33514658843     
 iteration          648 MCMCOBJ=   -6624.79130164135     
 iteration          649 MCMCOBJ=   -6610.65518260769     
 iteration          650 MCMCOBJ=   -6643.19204893112     
 iteration          651 MCMCOBJ=   -6592.64097961531     
 iteration          652 MCMCOBJ=   -6601.05277976708     
 iteration          653 MCMCOBJ=   -6631.11072097680     
 iteration          654 MCMCOBJ=   -6521.71804600758     
 iteration          655 MCMCOBJ=   -6590.04272393602     
 iteration          656 MCMCOBJ=   -6577.63326695831     
 iteration          657 MCMCOBJ=   -6593.79712259294     
 iteration          658 MCMCOBJ=   -6597.80021485281     
 iteration          659 MCMCOBJ=   -6548.38278720642     
 iteration          660 MCMCOBJ=   -6651.93201244551     
 iteration          661 MCMCOBJ=   -6631.68323479597     
 iteration          662 MCMCOBJ=   -6662.83940015460     
 iteration          663 MCMCOBJ=   -6683.29000369187     
 iteration          664 MCMCOBJ=   -6642.05859244075     
 iteration          665 MCMCOBJ=   -6639.81516036916     
 iteration          666 MCMCOBJ=   -6688.62620463294     
 iteration          667 MCMCOBJ=   -6650.11076252771     
 iteration          668 MCMCOBJ=   -6629.94618191990     
 iteration          669 MCMCOBJ=   -6646.62571656474     
 iteration          670 MCMCOBJ=   -6622.05475588326     
 iteration          671 MCMCOBJ=   -6585.89793701722     
 iteration          672 MCMCOBJ=   -6649.91201093536     
 iteration          673 MCMCOBJ=   -6615.94512917797     
 iteration          674 MCMCOBJ=   -6608.55476678690     
 iteration          675 MCMCOBJ=   -6630.38575149698     
 iteration          676 MCMCOBJ=   -6615.93488931989     
 iteration          677 MCMCOBJ=   -6608.38403066771     
 iteration          678 MCMCOBJ=   -6547.40648796203     
 iteration          679 MCMCOBJ=   -6570.80473801327     
 iteration          680 MCMCOBJ=   -6591.77256155968     
 iteration          681 MCMCOBJ=   -6601.95702989573     
 iteration          682 MCMCOBJ=   -6594.83887986119     
 iteration          683 MCMCOBJ=   -6645.97286636030     
 iteration          684 MCMCOBJ=   -6625.71937649064     
 iteration          685 MCMCOBJ=   -6596.01458709690     
 iteration          686 MCMCOBJ=   -6578.24083102490     
 iteration          687 MCMCOBJ=   -6595.77835668785     
 iteration          688 MCMCOBJ=   -6571.34415681205     
 iteration          689 MCMCOBJ=   -6564.29059203726     
 iteration          690 MCMCOBJ=   -6578.41827190160     
 iteration          691 MCMCOBJ=   -6592.38271876548     
 iteration          692 MCMCOBJ=   -6614.30521711576     
 iteration          693 MCMCOBJ=   -6623.83974125215     
 iteration          694 MCMCOBJ=   -6634.36250235680     
 iteration          695 MCMCOBJ=   -6611.63322156645     
 iteration          696 MCMCOBJ=   -6575.02737320233     
 iteration          697 MCMCOBJ=   -6580.45855425485     
 iteration          698 MCMCOBJ=   -6558.83010304011     
 iteration          699 MCMCOBJ=   -6624.93929001169     
 iteration          700 MCMCOBJ=   -6604.87371475126     
 iteration          701 MCMCOBJ=   -6607.86974672862     
 iteration          702 MCMCOBJ=   -6646.26404476350     
 iteration          703 MCMCOBJ=   -6647.25629549294     
 iteration          704 MCMCOBJ=   -6622.53923796703     
 iteration          705 MCMCOBJ=   -6620.83203508997     
 iteration          706 MCMCOBJ=   -6621.59673875869     
 iteration          707 MCMCOBJ=   -6574.09950595400     
 iteration          708 MCMCOBJ=   -6615.71907416008     
 iteration          709 MCMCOBJ=   -6600.83454786590     
 iteration          710 MCMCOBJ=   -6625.37159324445     
 iteration          711 MCMCOBJ=   -6624.47495565939     
 iteration          712 MCMCOBJ=   -6623.57597542144     
 iteration          713 MCMCOBJ=   -6607.31006268443     
 iteration          714 MCMCOBJ=   -6623.00822381506     
 iteration          715 MCMCOBJ=   -6625.26439012752     
 iteration          716 MCMCOBJ=   -6646.26519966036     
 iteration          717 MCMCOBJ=   -6689.44268145551     
 iteration          718 MCMCOBJ=   -6645.35404448103     
 iteration          719 MCMCOBJ=   -6567.99228792692     
 iteration          720 MCMCOBJ=   -6666.60343177315     
 iteration          721 MCMCOBJ=   -6655.30967169592     
 iteration          722 MCMCOBJ=   -6629.30425119465     
 iteration          723 MCMCOBJ=   -6604.91518723487     
 iteration          724 MCMCOBJ=   -6614.76594583670     
 iteration          725 MCMCOBJ=   -6640.54666189586     
 iteration          726 MCMCOBJ=   -6586.04220208975     
 iteration          727 MCMCOBJ=   -6587.35423873152     
 iteration          728 MCMCOBJ=   -6620.32264511639     
 iteration          729 MCMCOBJ=   -6590.44583973598     
 iteration          730 MCMCOBJ=   -6596.17965272536     
 iteration          731 MCMCOBJ=   -6558.23050454778     
 iteration          732 MCMCOBJ=   -6564.33349344330     
 iteration          733 MCMCOBJ=   -6541.59397086629     
 iteration          734 MCMCOBJ=   -6585.64502306068     
 iteration          735 MCMCOBJ=   -6623.26593155343     
 iteration          736 MCMCOBJ=   -6659.97853054285     
 iteration          737 MCMCOBJ=   -6584.66080736576     
 iteration          738 MCMCOBJ=   -6607.90537604719     
 iteration          739 MCMCOBJ=   -6620.79142818838     
 iteration          740 MCMCOBJ=   -6603.03142907581     
 iteration          741 MCMCOBJ=   -6628.07680752067     
 iteration          742 MCMCOBJ=   -6602.70484159221     
 iteration          743 MCMCOBJ=   -6576.19362277335     
 iteration          744 MCMCOBJ=   -6667.61279717640     
 iteration          745 MCMCOBJ=   -6635.69074626404     
 iteration          746 MCMCOBJ=   -6600.70586268729     
 iteration          747 MCMCOBJ=   -6591.27723998172     
 iteration          748 MCMCOBJ=   -6578.16276644701     
 iteration          749 MCMCOBJ=   -6620.58192441563     
 iteration          750 MCMCOBJ=   -6687.05700260882     
 iteration          751 MCMCOBJ=   -6657.60372707736     
 iteration          752 MCMCOBJ=   -6596.42902018960     
 iteration          753 MCMCOBJ=   -6586.33639261989     
 iteration          754 MCMCOBJ=   -6614.06759890528     
 iteration          755 MCMCOBJ=   -6571.79149530118     
 iteration          756 MCMCOBJ=   -6634.70886221592     
 iteration          757 MCMCOBJ=   -6657.62198556930     
 iteration          758 MCMCOBJ=   -6647.91760681951     
 iteration          759 MCMCOBJ=   -6596.25301272069     
 iteration          760 MCMCOBJ=   -6633.60160155815     
 iteration          761 MCMCOBJ=   -6641.37037485552     
 iteration          762 MCMCOBJ=   -6624.23029101369     
 iteration          763 MCMCOBJ=   -6613.56572309365     
 iteration          764 MCMCOBJ=   -6622.96044563506     
 iteration          765 MCMCOBJ=   -6597.15712009279     
 iteration          766 MCMCOBJ=   -6609.62832689326     
 iteration          767 MCMCOBJ=   -6593.47052647523     
 iteration          768 MCMCOBJ=   -6562.82093401206     
 iteration          769 MCMCOBJ=   -6572.68274188001     
 iteration          770 MCMCOBJ=   -6599.40566237594     
 iteration          771 MCMCOBJ=   -6645.94183838147     
 iteration          772 MCMCOBJ=   -6574.29505479588     
 iteration          773 MCMCOBJ=   -6587.15413064077     
 iteration          774 MCMCOBJ=   -6569.01519262747     
 iteration          775 MCMCOBJ=   -6567.27796545123     
 iteration          776 MCMCOBJ=   -6589.19701902012     
 iteration          777 MCMCOBJ=   -6601.97690796161     
 iteration          778 MCMCOBJ=   -6576.91476504594     
 iteration          779 MCMCOBJ=   -6591.02707542896     
 iteration          780 MCMCOBJ=   -6607.25663858284     
 iteration          781 MCMCOBJ=   -6578.37885020477     
 iteration          782 MCMCOBJ=   -6619.10719718860     
 iteration          783 MCMCOBJ=   -6612.05527501968     
 iteration          784 MCMCOBJ=   -6680.83585686751     
 iteration          785 MCMCOBJ=   -6663.32583907095     
 iteration          786 MCMCOBJ=   -6564.41745209858     
 iteration          787 MCMCOBJ=   -6548.32838132821     
 iteration          788 MCMCOBJ=   -6529.71226295677     
 iteration          789 MCMCOBJ=   -6577.86440477942     
 iteration          790 MCMCOBJ=   -6583.65058423152     
 iteration          791 MCMCOBJ=   -6574.83623083954     
 iteration          792 MCMCOBJ=   -6576.08003283583     
 iteration          793 MCMCOBJ=   -6576.75736605040     
 iteration          794 MCMCOBJ=   -6549.74101297250     
 iteration          795 MCMCOBJ=   -6595.26414421257     
 iteration          796 MCMCOBJ=   -6606.56845410209     
 iteration          797 MCMCOBJ=   -6613.61007982803     
 iteration          798 MCMCOBJ=   -6605.74300599762     
 iteration          799 MCMCOBJ=   -6601.16618057035     
 iteration          800 MCMCOBJ=   -6590.43104319402     
 iteration          801 MCMCOBJ=   -6642.92468647076     
 iteration          802 MCMCOBJ=   -6639.81092219133     
 iteration          803 MCMCOBJ=   -6684.17016053705     
 iteration          804 MCMCOBJ=   -6643.42175016483     
 iteration          805 MCMCOBJ=   -6637.27644738998     
 iteration          806 MCMCOBJ=   -6640.40332523557     
 iteration          807 MCMCOBJ=   -6612.19528969100     
 iteration          808 MCMCOBJ=   -6596.84354366581     
 iteration          809 MCMCOBJ=   -6593.30768974390     
 iteration          810 MCMCOBJ=   -6593.30769285709     
 iteration          811 MCMCOBJ=   -6609.46690199089     
 iteration          812 MCMCOBJ=   -6622.00392956183     
 iteration          813 MCMCOBJ=   -6515.47413772380     
 iteration          814 MCMCOBJ=   -6558.79141745830     
 iteration          815 MCMCOBJ=   -6531.62829350561     
 iteration          816 MCMCOBJ=   -6562.84638629469     
 iteration          817 MCMCOBJ=   -6574.71297372262     
 iteration          818 MCMCOBJ=   -6584.74427023921     
 iteration          819 MCMCOBJ=   -6585.06337394460     
 iteration          820 MCMCOBJ=   -6599.85967663218     
 iteration          821 MCMCOBJ=   -6628.33801105140     
 iteration          822 MCMCOBJ=   -6637.04993331864     
 iteration          823 MCMCOBJ=   -6671.48252450265     
 iteration          824 MCMCOBJ=   -6680.80419913109     
 iteration          825 MCMCOBJ=   -6644.17807550445     
 iteration          826 MCMCOBJ=   -6632.48072517722     
 iteration          827 MCMCOBJ=   -6591.33106550352     
 iteration          828 MCMCOBJ=   -6595.11356048275     
 iteration          829 MCMCOBJ=   -6555.15743698245     
 iteration          830 MCMCOBJ=   -6550.31605975365     
 iteration          831 MCMCOBJ=   -6538.05813981793     
 iteration          832 MCMCOBJ=   -6537.43447047383     
 iteration          833 MCMCOBJ=   -6623.57815284671     
 iteration          834 MCMCOBJ=   -6609.16471151276     
 iteration          835 MCMCOBJ=   -6649.98618543765     
 iteration          836 MCMCOBJ=   -6632.69975148070     
 iteration          837 MCMCOBJ=   -6586.88745621314     
 iteration          838 MCMCOBJ=   -6556.29112051051     
 iteration          839 MCMCOBJ=   -6560.62554045229     
 iteration          840 MCMCOBJ=   -6581.36570899515     
 iteration          841 MCMCOBJ=   -6585.27852293186     
 iteration          842 MCMCOBJ=   -6636.47499948952     
 iteration          843 MCMCOBJ=   -6592.76805890151     
 iteration          844 MCMCOBJ=   -6614.33877564556     
 iteration          845 MCMCOBJ=   -6552.74913475694     
 iteration          846 MCMCOBJ=   -6596.28104805678     
 iteration          847 MCMCOBJ=   -6591.86924170456     
 iteration          848 MCMCOBJ=   -6601.32354370814     
 iteration          849 MCMCOBJ=   -6610.90294224752     
 iteration          850 MCMCOBJ=   -6661.72655514769     
 iteration          851 MCMCOBJ=   -6653.70652150288     
 iteration          852 MCMCOBJ=   -6552.34246324596     
 iteration          853 MCMCOBJ=   -6518.70915871607     
 iteration          854 MCMCOBJ=   -6519.88502947670     
 iteration          855 MCMCOBJ=   -6529.30815696595     
 iteration          856 MCMCOBJ=   -6530.36090709473     
 iteration          857 MCMCOBJ=   -6601.06123556853     
 iteration          858 MCMCOBJ=   -6614.93794607080     
 iteration          859 MCMCOBJ=   -6600.58958285593     
 iteration          860 MCMCOBJ=   -6615.91882985717     
 iteration          861 MCMCOBJ=   -6575.64391486911     
 iteration          862 MCMCOBJ=   -6565.07562024358     
 iteration          863 MCMCOBJ=   -6552.37315041767     
 iteration          864 MCMCOBJ=   -6575.26306632230     
 iteration          865 MCMCOBJ=   -6617.78952096620     
 iteration          866 MCMCOBJ=   -6548.28227779370     
 iteration          867 MCMCOBJ=   -6567.49965323527     
 iteration          868 MCMCOBJ=   -6572.73245109192     
 iteration          869 MCMCOBJ=   -6591.68727979222     
 iteration          870 MCMCOBJ=   -6591.63117142349     
 iteration          871 MCMCOBJ=   -6571.92702165875     
 iteration          872 MCMCOBJ=   -6549.41819677555     
 iteration          873 MCMCOBJ=   -6568.08396411904     
 iteration          874 MCMCOBJ=   -6592.93967296355     
 iteration          875 MCMCOBJ=   -6590.41973443263     
 iteration          876 MCMCOBJ=   -6565.63897907461     
 iteration          877 MCMCOBJ=   -6604.46270857824     
 iteration          878 MCMCOBJ=   -6599.64827308521     
 iteration          879 MCMCOBJ=   -6575.37854462736     
 iteration          880 MCMCOBJ=   -6549.13261123547     
 iteration          881 MCMCOBJ=   -6628.35297762507     
 iteration          882 MCMCOBJ=   -6584.36279981969     
 iteration          883 MCMCOBJ=   -6620.54996818611     
 iteration          884 MCMCOBJ=   -6587.37692777048     
 iteration          885 MCMCOBJ=   -6591.67448625023     
 iteration          886 MCMCOBJ=   -6599.03042175221     
 iteration          887 MCMCOBJ=   -6643.29755956732     
 iteration          888 MCMCOBJ=   -6592.26004419450     
 iteration          889 MCMCOBJ=   -6550.30673049695     
 iteration          890 MCMCOBJ=   -6612.70613748055     
 iteration          891 MCMCOBJ=   -6582.51123862005     
 iteration          892 MCMCOBJ=   -6603.60529799995     
 iteration          893 MCMCOBJ=   -6608.00989905598     
 iteration          894 MCMCOBJ=   -6555.34870953428     
 iteration          895 MCMCOBJ=   -6572.36399136957     
 iteration          896 MCMCOBJ=   -6593.08795675058     
 iteration          897 MCMCOBJ=   -6563.86192007923     
 iteration          898 MCMCOBJ=   -6575.19210298731     
 iteration          899 MCMCOBJ=   -6579.15094340561     
 iteration          900 MCMCOBJ=   -6575.31706331957     
 iteration          901 MCMCOBJ=   -6596.63634513804     
 iteration          902 MCMCOBJ=   -6617.77319372012     
 iteration          903 MCMCOBJ=   -6618.92170846208     
 iteration          904 MCMCOBJ=   -6612.18194635516     
 iteration          905 MCMCOBJ=   -6609.53405494900     
 iteration          906 MCMCOBJ=   -6581.06117640182     
 iteration          907 MCMCOBJ=   -6567.75553659821     
 iteration          908 MCMCOBJ=   -6589.39188346010     
 iteration          909 MCMCOBJ=   -6561.90191077548     
 iteration          910 MCMCOBJ=   -6598.14028391725     
 iteration          911 MCMCOBJ=   -6633.93355447106     
 iteration          912 MCMCOBJ=   -6597.22218783855     
 iteration          913 MCMCOBJ=   -6569.12229906627     
 iteration          914 MCMCOBJ=   -6524.92144980975     
 iteration          915 MCMCOBJ=   -6562.80616629878     
 iteration          916 MCMCOBJ=   -6562.65821458583     
 iteration          917 MCMCOBJ=   -6534.50883596003     
 iteration          918 MCMCOBJ=   -6550.14779678372     
 iteration          919 MCMCOBJ=   -6508.06622551355     
 iteration          920 MCMCOBJ=   -6457.74247268059     
 iteration          921 MCMCOBJ=   -6554.46038395330     
 iteration          922 MCMCOBJ=   -6611.55469182058     
 iteration          923 MCMCOBJ=   -6593.03809251394     
 iteration          924 MCMCOBJ=   -6600.77777357733     
 iteration          925 MCMCOBJ=   -6620.71444158425     
 iteration          926 MCMCOBJ=   -6617.05608363765     
 iteration          927 MCMCOBJ=   -6668.76271714006     
 iteration          928 MCMCOBJ=   -6685.80339578961     
 iteration          929 MCMCOBJ=   -6614.13233459898     
 iteration          930 MCMCOBJ=   -6562.78959047651     
 iteration          931 MCMCOBJ=   -6584.19404237576     
 iteration          932 MCMCOBJ=   -6613.33939528478     
 iteration          933 MCMCOBJ=   -6633.35967771168     
 iteration          934 MCMCOBJ=   -6612.25097328574     
 iteration          935 MCMCOBJ=   -6592.67652818997     
 iteration          936 MCMCOBJ=   -6618.06811925026     
 iteration          937 MCMCOBJ=   -6617.08347239827     
 iteration          938 MCMCOBJ=   -6612.20300817110     
 iteration          939 MCMCOBJ=   -6604.68154098046     
 iteration          940 MCMCOBJ=   -6593.87301026282     
 iteration          941 MCMCOBJ=   -6609.59894037614     
 iteration          942 MCMCOBJ=   -6616.76618171533     
 iteration          943 MCMCOBJ=   -6527.21326798955     
 iteration          944 MCMCOBJ=   -6575.11223537563     
 iteration          945 MCMCOBJ=   -6605.21625005787     
 iteration          946 MCMCOBJ=   -6569.05769905833     
 iteration          947 MCMCOBJ=   -6586.88851559051     
 iteration          948 MCMCOBJ=   -6571.00349090854     
 iteration          949 MCMCOBJ=   -6612.24518229588     
 iteration          950 MCMCOBJ=   -6550.57503536075     
 iteration          951 MCMCOBJ=   -6568.85648537911     
 iteration          952 MCMCOBJ=   -6548.00121704483     
 iteration          953 MCMCOBJ=   -6606.45757151336     
 iteration          954 MCMCOBJ=   -6654.24487037815     
 iteration          955 MCMCOBJ=   -6614.35223193813     
 iteration          956 MCMCOBJ=   -6600.67471342455     
 iteration          957 MCMCOBJ=   -6595.54148140401     
 iteration          958 MCMCOBJ=   -6582.75249258679     
 iteration          959 MCMCOBJ=   -6617.78078710849     
 iteration          960 MCMCOBJ=   -6616.97412127759     
 iteration          961 MCMCOBJ=   -6580.52727836346     
 iteration          962 MCMCOBJ=   -6608.67256953671     
 iteration          963 MCMCOBJ=   -6646.95085317101     
 iteration          964 MCMCOBJ=   -6665.74249505998     
 iteration          965 MCMCOBJ=   -6665.74249930971     
 iteration          966 MCMCOBJ=   -6689.26160128010     
 iteration          967 MCMCOBJ=   -6700.54119453681     
 iteration          968 MCMCOBJ=   -6658.61866765560     
 iteration          969 MCMCOBJ=   -6627.82865998623     
 iteration          970 MCMCOBJ=   -6670.76696034396     
 iteration          971 MCMCOBJ=   -6655.66011940475     
 iteration          972 MCMCOBJ=   -6621.48124700077     
 iteration          973 MCMCOBJ=   -6576.90404285870     
 iteration          974 MCMCOBJ=   -6548.23894838055     
 iteration          975 MCMCOBJ=   -6603.01363678651     
 iteration          976 MCMCOBJ=   -6597.05269508823     
 iteration          977 MCMCOBJ=   -6585.68446589643     
 iteration          978 MCMCOBJ=   -6643.05683301079     
 iteration          979 MCMCOBJ=   -6621.82539882724     
 iteration          980 MCMCOBJ=   -6627.51254979701     
 iteration          981 MCMCOBJ=   -6577.22091362692     
 iteration          982 MCMCOBJ=   -6606.05719194232     
 iteration          983 MCMCOBJ=   -6613.60676489389     
 iteration          984 MCMCOBJ=   -6566.54879422727     
 iteration          985 MCMCOBJ=   -6596.00891808623     
 iteration          986 MCMCOBJ=   -6569.03269692207     
 iteration          987 MCMCOBJ=   -6593.94460130829     
 iteration          988 MCMCOBJ=   -6586.20585276241     
 iteration          989 MCMCOBJ=   -6598.67258782788     
 iteration          990 MCMCOBJ=   -6592.11501564048     
 iteration          991 MCMCOBJ=   -6571.56868977276     
 iteration          992 MCMCOBJ=   -6569.02080042309     
 iteration          993 MCMCOBJ=   -6573.17804313900     
 iteration          994 MCMCOBJ=   -6635.20564807604     
 iteration          995 MCMCOBJ=   -6587.56468623091     
 iteration          996 MCMCOBJ=   -6569.34911674078     
 iteration          997 MCMCOBJ=   -6566.78860332560     
 iteration          998 MCMCOBJ=   -6562.84389561740     
 iteration          999 MCMCOBJ=   -6571.21537642066     
 iteration         1000 MCMCOBJ=   -6531.16005315311     
 iteration         1001 MCMCOBJ=   -6538.81622031398     
 iteration         1002 MCMCOBJ=   -6621.85718535464     
 iteration         1003 MCMCOBJ=   -6544.53624332415     
 iteration         1004 MCMCOBJ=   -6526.59058974510     
 iteration         1005 MCMCOBJ=   -6522.85722680411     
 iteration         1006 MCMCOBJ=   -6632.86170513875     
 iteration         1007 MCMCOBJ=   -6603.26807361198     
 iteration         1008 MCMCOBJ=   -6570.54967323375     
 iteration         1009 MCMCOBJ=   -6578.68231844823     
 iteration         1010 MCMCOBJ=   -6578.45981368697     
 iteration         1011 MCMCOBJ=   -6606.78781760065     
 iteration         1012 MCMCOBJ=   -6623.12158686673     
 iteration         1013 MCMCOBJ=   -6689.31931823807     
 iteration         1014 MCMCOBJ=   -6688.15704126959     
 iteration         1015 MCMCOBJ=   -6635.88830406000     
 iteration         1016 MCMCOBJ=   -6545.69166614913     
 iteration         1017 MCMCOBJ=   -6580.60096417303     
 iteration         1018 MCMCOBJ=   -6641.59298841379     
 iteration         1019 MCMCOBJ=   -6586.33937207746     
 iteration         1020 MCMCOBJ=   -6548.91925643395     
 iteration         1021 MCMCOBJ=   -6533.71687481252     
 iteration         1022 MCMCOBJ=   -6538.22718187934     
 iteration         1023 MCMCOBJ=   -6555.90102434448     
 iteration         1024 MCMCOBJ=   -6531.65926455635     
 iteration         1025 MCMCOBJ=   -6501.56281480357     
 iteration         1026 MCMCOBJ=   -6580.63509396530     
 iteration         1027 MCMCOBJ=   -6630.91695734904     
 iteration         1028 MCMCOBJ=   -6596.35271613176     
 iteration         1029 MCMCOBJ=   -6558.82230710147     
 iteration         1030 MCMCOBJ=   -6583.88580614086     
 iteration         1031 MCMCOBJ=   -6537.00933943696     
 iteration         1032 MCMCOBJ=   -6548.90267097793     
 iteration         1033 MCMCOBJ=   -6543.89467669590     
 iteration         1034 MCMCOBJ=   -6549.56509945398     
 iteration         1035 MCMCOBJ=   -6543.47921595327     
 iteration         1036 MCMCOBJ=   -6577.03654429755     
 iteration         1037 MCMCOBJ=   -6592.39288846517     
 iteration         1038 MCMCOBJ=   -6563.99899750100     
 iteration         1039 MCMCOBJ=   -6620.14015006697     
 iteration         1040 MCMCOBJ=   -6617.60869015416     
 iteration         1041 MCMCOBJ=   -6615.61811170636     
 iteration         1042 MCMCOBJ=   -6632.31329133895     
 iteration         1043 MCMCOBJ=   -6610.38424371545     
 iteration         1044 MCMCOBJ=   -6583.73941766705     
 iteration         1045 MCMCOBJ=   -6558.18977324324     
 iteration         1046 MCMCOBJ=   -6532.17372672657     
 iteration         1047 MCMCOBJ=   -6542.32336021701     
 iteration         1048 MCMCOBJ=   -6599.11697780532     
 iteration         1049 MCMCOBJ=   -6564.18814173466     
 iteration         1050 MCMCOBJ=   -6640.92983321839     
 iteration         1051 MCMCOBJ=   -6594.69103441689     
 iteration         1052 MCMCOBJ=   -6621.51286200651     
 iteration         1053 MCMCOBJ=   -6627.33662422243     
 iteration         1054 MCMCOBJ=   -6568.92033776731     
 iteration         1055 MCMCOBJ=   -6525.42599026881     
 iteration         1056 MCMCOBJ=   -6514.63796497622     
 iteration         1057 MCMCOBJ=   -6533.62484206278     
 iteration         1058 MCMCOBJ=   -6492.54457694897     
 iteration         1059 MCMCOBJ=   -6535.80623807831     
 iteration         1060 MCMCOBJ=   -6547.28517705639     
 iteration         1061 MCMCOBJ=   -6552.27725035035     
 iteration         1062 MCMCOBJ=   -6596.00819454829     
 iteration         1063 MCMCOBJ=   -6593.09076816305     
 iteration         1064 MCMCOBJ=   -6632.09724787659     
 iteration         1065 MCMCOBJ=   -6612.46265559606     
 iteration         1066 MCMCOBJ=   -6579.04668469706     
 iteration         1067 MCMCOBJ=   -6572.57089163110     
 iteration         1068 MCMCOBJ=   -6584.63755999283     
 iteration         1069 MCMCOBJ=   -6635.93836416752     
 iteration         1070 MCMCOBJ=   -6625.51754352471     
 iteration         1071 MCMCOBJ=   -6616.48281022817     
 iteration         1072 MCMCOBJ=   -6609.98003036595     
 iteration         1073 MCMCOBJ=   -6630.26666235521     
 iteration         1074 MCMCOBJ=   -6588.95066579850     
 iteration         1075 MCMCOBJ=   -6622.61273852173     
 iteration         1076 MCMCOBJ=   -6656.20546204413     
 iteration         1077 MCMCOBJ=   -6637.91270757484     
 iteration         1078 MCMCOBJ=   -6651.55857141479     
 iteration         1079 MCMCOBJ=   -6676.19558047750     
 iteration         1080 MCMCOBJ=   -6667.97842383325     
 iteration         1081 MCMCOBJ=   -6673.71347327951     
 iteration         1082 MCMCOBJ=   -6636.33374187569     
 iteration         1083 MCMCOBJ=   -6630.60356276980     
 iteration         1084 MCMCOBJ=   -6603.68649078405     
 iteration         1085 MCMCOBJ=   -6619.72901073011     
 iteration         1086 MCMCOBJ=   -6569.55527508950     
 iteration         1087 MCMCOBJ=   -6571.32392304665     
 iteration         1088 MCMCOBJ=   -6622.68227336118     
 iteration         1089 MCMCOBJ=   -6589.74379420714     
 iteration         1090 MCMCOBJ=   -6611.98295594322     
 iteration         1091 MCMCOBJ=   -6595.30052535388     
 iteration         1092 MCMCOBJ=   -6558.18969100899     
 iteration         1093 MCMCOBJ=   -6605.32321124062     
 iteration         1094 MCMCOBJ=   -6606.75100675848     
 iteration         1095 MCMCOBJ=   -6546.06109829509     
 iteration         1096 MCMCOBJ=   -6530.58373875311     
 iteration         1097 MCMCOBJ=   -6554.09401948801     
 iteration         1098 MCMCOBJ=   -6553.65734950090     
 iteration         1099 MCMCOBJ=   -6560.17331693769     
 iteration         1100 MCMCOBJ=   -6558.76585474745     
 iteration         1101 MCMCOBJ=   -6564.08480199183     
 iteration         1102 MCMCOBJ=   -6584.06553482684     
 iteration         1103 MCMCOBJ=   -6619.57591495465     
 iteration         1104 MCMCOBJ=   -6604.61897454900     
 iteration         1105 MCMCOBJ=   -6569.53942818663     
 iteration         1106 MCMCOBJ=   -6588.81291421065     
 iteration         1107 MCMCOBJ=   -6617.03028000449     
 iteration         1108 MCMCOBJ=   -6645.95293261762     
 iteration         1109 MCMCOBJ=   -6614.64120760345     
 iteration         1110 MCMCOBJ=   -6579.70924922672     
 iteration         1111 MCMCOBJ=   -6577.12378178481     
 iteration         1112 MCMCOBJ=   -6595.97116476016     
 iteration         1113 MCMCOBJ=   -6575.90396489139     
 iteration         1114 MCMCOBJ=   -6583.38434498033     
 iteration         1115 MCMCOBJ=   -6615.52805320747     
 iteration         1116 MCMCOBJ=   -6596.02157651733     
 iteration         1117 MCMCOBJ=   -6631.70216160802     
 iteration         1118 MCMCOBJ=   -6621.87432753612     
 iteration         1119 MCMCOBJ=   -6592.21446527901     
 iteration         1120 MCMCOBJ=   -6622.21037851507     
 iteration         1121 MCMCOBJ=   -6608.29067742145     
 iteration         1122 MCMCOBJ=   -6549.99085087491     
 iteration         1123 MCMCOBJ=   -6535.99868288013     
 iteration         1124 MCMCOBJ=   -6572.22454604109     
 iteration         1125 MCMCOBJ=   -6562.45510590120     
 iteration         1126 MCMCOBJ=   -6598.48594248048     
 iteration         1127 MCMCOBJ=   -6613.92712829007     
 iteration         1128 MCMCOBJ=   -6638.82180458817     
 iteration         1129 MCMCOBJ=   -6642.80836925758     
 iteration         1130 MCMCOBJ=   -6629.17509383036     
 iteration         1131 MCMCOBJ=   -6666.12888522372     
 iteration         1132 MCMCOBJ=   -6686.09348220662     
 iteration         1133 MCMCOBJ=   -6662.20173991568     
 iteration         1134 MCMCOBJ=   -6659.21389848711     
 iteration         1135 MCMCOBJ=   -6600.91624154550     
 iteration         1136 MCMCOBJ=   -6547.53467776906     
 iteration         1137 MCMCOBJ=   -6569.76958699143     
 iteration         1138 MCMCOBJ=   -6593.44678929041     
 iteration         1139 MCMCOBJ=   -6602.92189365341     
 iteration         1140 MCMCOBJ=   -6541.54021960446     
 iteration         1141 MCMCOBJ=   -6563.07687965520     
 iteration         1142 MCMCOBJ=   -6572.02175776293     
 iteration         1143 MCMCOBJ=   -6573.75473955255     
 iteration         1144 MCMCOBJ=   -6599.86172033210     
 iteration         1145 MCMCOBJ=   -6564.58187768175     
 iteration         1146 MCMCOBJ=   -6625.89691758751     
 iteration         1147 MCMCOBJ=   -6623.82054810929     
 iteration         1148 MCMCOBJ=   -6609.89989506227     
 iteration         1149 MCMCOBJ=   -6597.18083666337     
 iteration         1150 MCMCOBJ=   -6628.87225441306     
 iteration         1151 MCMCOBJ=   -6648.88950214725     
 iteration         1152 MCMCOBJ=   -6630.93085356239     
 iteration         1153 MCMCOBJ=   -6631.78245434868     
 iteration         1154 MCMCOBJ=   -6600.29988331451     
 iteration         1155 MCMCOBJ=   -6622.33810761705     
 iteration         1156 MCMCOBJ=   -6606.08584832292     
 iteration         1157 MCMCOBJ=   -6618.78686441010     
 iteration         1158 MCMCOBJ=   -6634.03697809417     
 iteration         1159 MCMCOBJ=   -6597.65624415619     
 iteration         1160 MCMCOBJ=   -6615.58575787407     
 iteration         1161 MCMCOBJ=   -6618.76630698858     
 iteration         1162 MCMCOBJ=   -6575.59078513073     
 iteration         1163 MCMCOBJ=   -6619.80144789677     
 iteration         1164 MCMCOBJ=   -6599.80491621078     
 iteration         1165 MCMCOBJ=   -6611.35175857569     
 iteration         1166 MCMCOBJ=   -6592.86465767274     
 iteration         1167 MCMCOBJ=   -6625.15753938628     
 iteration         1168 MCMCOBJ=   -6578.02785362202     
 iteration         1169 MCMCOBJ=   -6580.11630885842     
 iteration         1170 MCMCOBJ=   -6565.65580656983     
 iteration         1171 MCMCOBJ=   -6570.49111585910     
 iteration         1172 MCMCOBJ=   -6603.17033883399     
 iteration         1173 MCMCOBJ=   -6594.28071530112     
 iteration         1174 MCMCOBJ=   -6592.19480250126     
 iteration         1175 MCMCOBJ=   -6603.31077687009     
 iteration         1176 MCMCOBJ=   -6576.67041906065     
 iteration         1177 MCMCOBJ=   -6616.42609311454     
 iteration         1178 MCMCOBJ=   -6610.45795973239     
 iteration         1179 MCMCOBJ=   -6545.52269928330     
 iteration         1180 MCMCOBJ=   -6572.64750399055     
 iteration         1181 MCMCOBJ=   -6559.71270215899     
 iteration         1182 MCMCOBJ=   -6565.85790903554     
 iteration         1183 MCMCOBJ=   -6630.28379043070     
 iteration         1184 MCMCOBJ=   -6627.30718532555     
 iteration         1185 MCMCOBJ=   -6609.38291842807     
 iteration         1186 MCMCOBJ=   -6623.46338376817     
 iteration         1187 MCMCOBJ=   -6546.17189204622     
 iteration         1188 MCMCOBJ=   -6534.70953245417     
 iteration         1189 MCMCOBJ=   -6514.22915784331     
 iteration         1190 MCMCOBJ=   -6541.33426296084     
 iteration         1191 MCMCOBJ=   -6622.59378432702     
 iteration         1192 MCMCOBJ=   -6583.16097219698     
 iteration         1193 MCMCOBJ=   -6594.37137039428     
 iteration         1194 MCMCOBJ=   -6594.37137035087     
 iteration         1195 MCMCOBJ=   -6620.31975182048     
 iteration         1196 MCMCOBJ=   -6615.59064540230     
 iteration         1197 MCMCOBJ=   -6626.91941023166     
 iteration         1198 MCMCOBJ=   -6638.08218794413     
 iteration         1199 MCMCOBJ=   -6626.95797486874     
 iteration         1200 MCMCOBJ=   -6589.76509155378     
 iteration         1201 MCMCOBJ=   -6599.15527071054     
 iteration         1202 MCMCOBJ=   -6618.17949629115     
 iteration         1203 MCMCOBJ=   -6647.43769785273     
 iteration         1204 MCMCOBJ=   -6668.66014808315     
 iteration         1205 MCMCOBJ=   -6606.42707951974     
 iteration         1206 MCMCOBJ=   -6569.20692134609     
 iteration         1207 MCMCOBJ=   -6577.88860074652     
 iteration         1208 MCMCOBJ=   -6634.04116449761     
 iteration         1209 MCMCOBJ=   -6608.38660286056     
 iteration         1210 MCMCOBJ=   -6565.81313384442     
 iteration         1211 MCMCOBJ=   -6558.12846166820     
 iteration         1212 MCMCOBJ=   -6566.52013242160     
 iteration         1213 MCMCOBJ=   -6543.89344664959     
 iteration         1214 MCMCOBJ=   -6548.86165044658     
 iteration         1215 MCMCOBJ=   -6552.91724573507     
 iteration         1216 MCMCOBJ=   -6589.90101136958     
 iteration         1217 MCMCOBJ=   -6649.91220199332     
 iteration         1218 MCMCOBJ=   -6550.08698732765     
 iteration         1219 MCMCOBJ=   -6536.77270805034     
 iteration         1220 MCMCOBJ=   -6640.61067379803     
 iteration         1221 MCMCOBJ=   -6658.50720448895     
 iteration         1222 MCMCOBJ=   -6651.02955871071     
 iteration         1223 MCMCOBJ=   -6590.39843410307     
 iteration         1224 MCMCOBJ=   -6609.53718423162     
 iteration         1225 MCMCOBJ=   -6599.60803744673     
 iteration         1226 MCMCOBJ=   -6569.86886951799     
 iteration         1227 MCMCOBJ=   -6630.18983630902     
 iteration         1228 MCMCOBJ=   -6614.86988359519     
 iteration         1229 MCMCOBJ=   -6585.59645491462     
 iteration         1230 MCMCOBJ=   -6558.01046689894     
 iteration         1231 MCMCOBJ=   -6565.57546079394     
 iteration         1232 MCMCOBJ=   -6602.67610737839     
 iteration         1233 MCMCOBJ=   -6571.82180227754     
 iteration         1234 MCMCOBJ=   -6595.32765101426     
 iteration         1235 MCMCOBJ=   -6614.35811883040     
 iteration         1236 MCMCOBJ=   -6631.54342013480     
 iteration         1237 MCMCOBJ=   -6613.53658131119     
 iteration         1238 MCMCOBJ=   -6617.59847959304     
 iteration         1239 MCMCOBJ=   -6635.32232837254     
 iteration         1240 MCMCOBJ=   -6636.94872308720     
 iteration         1241 MCMCOBJ=   -6606.58410089603     
 iteration         1242 MCMCOBJ=   -6614.47774432074     
 iteration         1243 MCMCOBJ=   -6581.20553462650     
 iteration         1244 MCMCOBJ=   -6625.63585821038     
 iteration         1245 MCMCOBJ=   -6596.54155080956     
 iteration         1246 MCMCOBJ=   -6576.07178268121     
 iteration         1247 MCMCOBJ=   -6638.44053817260     
 iteration         1248 MCMCOBJ=   -6638.44053743758     
 iteration         1249 MCMCOBJ=   -6635.27174211646     
 iteration         1250 MCMCOBJ=   -6595.28398703066     
 iteration         1251 MCMCOBJ=   -6615.04989747709     
 iteration         1252 MCMCOBJ=   -6612.92212584181     
 iteration         1253 MCMCOBJ=   -6612.38512780583     
 iteration         1254 MCMCOBJ=   -6640.08409635959     
 iteration         1255 MCMCOBJ=   -6638.52510906894     
 iteration         1256 MCMCOBJ=   -6609.39011042037     
 iteration         1257 MCMCOBJ=   -6596.65850020181     
 iteration         1258 MCMCOBJ=   -6587.40044257121     
 iteration         1259 MCMCOBJ=   -6564.61333062306     
 iteration         1260 MCMCOBJ=   -6576.02019973579     
 iteration         1261 MCMCOBJ=   -6555.49094144289     
 iteration         1262 MCMCOBJ=   -6555.49415064998     
 iteration         1263 MCMCOBJ=   -6574.43424940154     
 iteration         1264 MCMCOBJ=   -6580.77179078999     
 iteration         1265 MCMCOBJ=   -6591.66999028219     
 iteration         1266 MCMCOBJ=   -6583.89408648599     
 iteration         1267 MCMCOBJ=   -6550.84448116904     
 iteration         1268 MCMCOBJ=   -6573.34326147657     
 iteration         1269 MCMCOBJ=   -6569.09307655928     
 iteration         1270 MCMCOBJ=   -6577.86499266050     
 iteration         1271 MCMCOBJ=   -6636.24466920737     
 iteration         1272 MCMCOBJ=   -6696.89816846970     
 iteration         1273 MCMCOBJ=   -6624.01686699817     
 iteration         1274 MCMCOBJ=   -6619.40854304565     
 iteration         1275 MCMCOBJ=   -6636.96881146611     
 iteration         1276 MCMCOBJ=   -6631.59020199061     
 iteration         1277 MCMCOBJ=   -6621.99681152559     
 iteration         1278 MCMCOBJ=   -6617.40989719412     
 iteration         1279 MCMCOBJ=   -6588.47602728326     
 iteration         1280 MCMCOBJ=   -6595.77979910365     
 iteration         1281 MCMCOBJ=   -6522.76387399845     
 iteration         1282 MCMCOBJ=   -6555.54639788699     
 iteration         1283 MCMCOBJ=   -6613.10761074875     
 iteration         1284 MCMCOBJ=   -6540.85330522842     
 iteration         1285 MCMCOBJ=   -6585.05274274617     
 iteration         1286 MCMCOBJ=   -6564.82305420938     
 iteration         1287 MCMCOBJ=   -6599.21162647584     
 iteration         1288 MCMCOBJ=   -6626.72909586115     
 iteration         1289 MCMCOBJ=   -6610.22725992224     
 iteration         1290 MCMCOBJ=   -6682.78327986289     
 iteration         1291 MCMCOBJ=   -6609.59672772325     
 iteration         1292 MCMCOBJ=   -6589.74132215121     
 iteration         1293 MCMCOBJ=   -6607.83746975442     
 iteration         1294 MCMCOBJ=   -6608.69513266941     
 iteration         1295 MCMCOBJ=   -6613.04666464743     
 iteration         1296 MCMCOBJ=   -6612.90406397930     
 iteration         1297 MCMCOBJ=   -6540.32052579342     
 iteration         1298 MCMCOBJ=   -6563.88816426674     
 iteration         1299 MCMCOBJ=   -6628.29395113097     
 iteration         1300 MCMCOBJ=   -6624.60114390844     
 iteration         1301 MCMCOBJ=   -6613.10820701940     
 iteration         1302 MCMCOBJ=   -6599.64493598569     
 iteration         1303 MCMCOBJ=   -6589.25306456718     
 iteration         1304 MCMCOBJ=   -6627.11735029783     
 iteration         1305 MCMCOBJ=   -6608.98262192759     
 iteration         1306 MCMCOBJ=   -6634.09970456257     
 iteration         1307 MCMCOBJ=   -6566.75390676381     
 iteration         1308 MCMCOBJ=   -6526.94665222336     
 iteration         1309 MCMCOBJ=   -6546.02378457481     
 iteration         1310 MCMCOBJ=   -6586.72444805059     
 iteration         1311 MCMCOBJ=   -6589.33693855778     
 iteration         1312 MCMCOBJ=   -6617.62721918074     
 iteration         1313 MCMCOBJ=   -6553.43666922249     
 iteration         1314 MCMCOBJ=   -6625.15340545333     
 iteration         1315 MCMCOBJ=   -6650.61727038175     
 iteration         1316 MCMCOBJ=   -6629.35970234365     
 iteration         1317 MCMCOBJ=   -6640.80372550852     
 iteration         1318 MCMCOBJ=   -6617.35023836538     
 iteration         1319 MCMCOBJ=   -6594.90612532641     
 iteration         1320 MCMCOBJ=   -6653.45252076759     
 iteration         1321 MCMCOBJ=   -6590.28444083440     
 iteration         1322 MCMCOBJ=   -6597.52598984890     
 iteration         1323 MCMCOBJ=   -6572.47683572805     
 iteration         1324 MCMCOBJ=   -6572.07480852010     
 iteration         1325 MCMCOBJ=   -6554.11959797670     
 iteration         1326 MCMCOBJ=   -6557.03291808719     
 iteration         1327 MCMCOBJ=   -6581.19098069512     
 iteration         1328 MCMCOBJ=   -6650.83233314181     
 iteration         1329 MCMCOBJ=   -6637.66248429029     
 iteration         1330 MCMCOBJ=   -6629.18140806950     
 iteration         1331 MCMCOBJ=   -6597.09235554081     
 iteration         1332 MCMCOBJ=   -6591.84085805288     
 iteration         1333 MCMCOBJ=   -6572.70327303610     
 iteration         1334 MCMCOBJ=   -6578.66408725211     
 iteration         1335 MCMCOBJ=   -6599.39493219220     
 iteration         1336 MCMCOBJ=   -6626.64056574388     
 iteration         1337 MCMCOBJ=   -6618.42339410957     
 iteration         1338 MCMCOBJ=   -6673.61298040173     
 iteration         1339 MCMCOBJ=   -6644.23636986719     
 iteration         1340 MCMCOBJ=   -6666.33934946222     
 iteration         1341 MCMCOBJ=   -6635.22831956329     
 iteration         1342 MCMCOBJ=   -6594.02846489416     
 iteration         1343 MCMCOBJ=   -6606.04551315822     
 iteration         1344 MCMCOBJ=   -6622.36382676470     
 iteration         1345 MCMCOBJ=   -6610.41671219269     
 iteration         1346 MCMCOBJ=   -6609.25041038479     
 iteration         1347 MCMCOBJ=   -6555.06427625644     
 iteration         1348 MCMCOBJ=   -6606.99845789115     
 iteration         1349 MCMCOBJ=   -6543.03316176451     
 iteration         1350 MCMCOBJ=   -6576.85058525025     
 iteration         1351 MCMCOBJ=   -6533.03645635626     
 iteration         1352 MCMCOBJ=   -6556.10267533250     
 iteration         1353 MCMCOBJ=   -6592.40014266663     
 iteration         1354 MCMCOBJ=   -6565.56916653342     
 iteration         1355 MCMCOBJ=   -6559.04567350789     
 iteration         1356 MCMCOBJ=   -6546.50628814707     
 iteration         1357 MCMCOBJ=   -6553.31899598514     
 iteration         1358 MCMCOBJ=   -6553.14667741756     
 iteration         1359 MCMCOBJ=   -6576.22206465704     
 iteration         1360 MCMCOBJ=   -6564.73961544021     
 iteration         1361 MCMCOBJ=   -6617.01907966577     
 iteration         1362 MCMCOBJ=   -6602.61414130924     
 iteration         1363 MCMCOBJ=   -6605.47799128382     
 iteration         1364 MCMCOBJ=   -6558.65170076292     
 iteration         1365 MCMCOBJ=   -6587.58179808586     
 iteration         1366 MCMCOBJ=   -6578.76657792515     
 iteration         1367 MCMCOBJ=   -6659.28368843402     
 iteration         1368 MCMCOBJ=   -6650.19077054116     
 iteration         1369 MCMCOBJ=   -6641.42614486325     
 iteration         1370 MCMCOBJ=   -6680.25323940570     
 iteration         1371 MCMCOBJ=   -6604.13580449514     
 iteration         1372 MCMCOBJ=   -6611.76818791522     
 iteration         1373 MCMCOBJ=   -6602.72376425815     
 iteration         1374 MCMCOBJ=   -6546.41822775902     
 iteration         1375 MCMCOBJ=   -6565.47890120932     
 iteration         1376 MCMCOBJ=   -6534.34549713030     
 iteration         1377 MCMCOBJ=   -6561.38287291527     
 iteration         1378 MCMCOBJ=   -6602.35819936624     
 iteration         1379 MCMCOBJ=   -6663.28940665038     
 iteration         1380 MCMCOBJ=   -6644.32816376327     
 iteration         1381 MCMCOBJ=   -6595.97715068849     
 iteration         1382 MCMCOBJ=   -6561.36589230605     
 iteration         1383 MCMCOBJ=   -6568.14008079566     
 iteration         1384 MCMCOBJ=   -6576.73750590329     
 iteration         1385 MCMCOBJ=   -6535.59669657677     
 iteration         1386 MCMCOBJ=   -6514.59862756123     
 iteration         1387 MCMCOBJ=   -6523.88839734799     
 iteration         1388 MCMCOBJ=   -6530.83094402171     
 iteration         1389 MCMCOBJ=   -6536.79168256912     
 iteration         1390 MCMCOBJ=   -6581.90510855512     
 iteration         1391 MCMCOBJ=   -6592.20163639993     
 iteration         1392 MCMCOBJ=   -6574.03212086437     
 iteration         1393 MCMCOBJ=   -6622.15814595343     
 iteration         1394 MCMCOBJ=   -6595.96937589816     
 iteration         1395 MCMCOBJ=   -6635.74921724086     
 iteration         1396 MCMCOBJ=   -6612.74685287701     
 iteration         1397 MCMCOBJ=   -6616.30004169457     
 iteration         1398 MCMCOBJ=   -6564.65518688734     
 iteration         1399 MCMCOBJ=   -6600.54824726060     
 iteration         1400 MCMCOBJ=   -6586.97057825398     
 iteration         1401 MCMCOBJ=   -6590.87419204073     
 iteration         1402 MCMCOBJ=   -6653.22643815818     
 iteration         1403 MCMCOBJ=   -6541.00360975230     
 iteration         1404 MCMCOBJ=   -6549.71437163147     
 iteration         1405 MCMCOBJ=   -6569.45514131354     
 iteration         1406 MCMCOBJ=   -6584.83423997484     
 iteration         1407 MCMCOBJ=   -6586.17852433780     
 iteration         1408 MCMCOBJ=   -6582.91231691451     
 iteration         1409 MCMCOBJ=   -6579.57922008024     
 iteration         1410 MCMCOBJ=   -6576.21018857742     
 iteration         1411 MCMCOBJ=   -6596.00947722766     
 iteration         1412 MCMCOBJ=   -6629.26562846359     
 iteration         1413 MCMCOBJ=   -6582.27569396252     
 iteration         1414 MCMCOBJ=   -6603.37527172460     
 iteration         1415 MCMCOBJ=   -6669.51236639864     
 iteration         1416 MCMCOBJ=   -6627.70072375637     
 iteration         1417 MCMCOBJ=   -6590.96559650671     
 iteration         1418 MCMCOBJ=   -6619.71712889251     
 iteration         1419 MCMCOBJ=   -6584.03348906980     
 iteration         1420 MCMCOBJ=   -6555.40494235372     
 iteration         1421 MCMCOBJ=   -6567.61818479567     
 iteration         1422 MCMCOBJ=   -6590.58356445076     
 iteration         1423 MCMCOBJ=   -6552.77188875008     
 iteration         1424 MCMCOBJ=   -6620.68929376154     
 iteration         1425 MCMCOBJ=   -6568.85827438297     
 iteration         1426 MCMCOBJ=   -6523.49732695563     
 iteration         1427 MCMCOBJ=   -6571.63661310777     
 iteration         1428 MCMCOBJ=   -6576.83729477702     
 iteration         1429 MCMCOBJ=   -6622.98140566618     
 iteration         1430 MCMCOBJ=   -6583.16489129976     
 iteration         1431 MCMCOBJ=   -6605.45709856917     
 iteration         1432 MCMCOBJ=   -6640.03274224378     
 iteration         1433 MCMCOBJ=   -6579.52221428874     
 iteration         1434 MCMCOBJ=   -6563.47747415387     
 iteration         1435 MCMCOBJ=   -6586.67543940342     
 iteration         1436 MCMCOBJ=   -6612.30800657255     
 iteration         1437 MCMCOBJ=   -6587.35684857040     
 iteration         1438 MCMCOBJ=   -6596.77753187021     
 iteration         1439 MCMCOBJ=   -6630.69904141921     
 iteration         1440 MCMCOBJ=   -6661.30690935770     
 iteration         1441 MCMCOBJ=   -6581.19745208740     
 iteration         1442 MCMCOBJ=   -6615.13637814910     
 iteration         1443 MCMCOBJ=   -6572.83435661870     
 iteration         1444 MCMCOBJ=   -6542.00405050803     
 iteration         1445 MCMCOBJ=   -6546.64959406555     
 iteration         1446 MCMCOBJ=   -6570.16035500209     
 iteration         1447 MCMCOBJ=   -6565.99328513609     
 iteration         1448 MCMCOBJ=   -6619.68468142809     
 iteration         1449 MCMCOBJ=   -6583.35317579493     
 iteration         1450 MCMCOBJ=   -6602.00728968907     
 iteration         1451 MCMCOBJ=   -6610.57990163627     
 iteration         1452 MCMCOBJ=   -6566.87779157661     
 iteration         1453 MCMCOBJ=   -6545.47616149628     
 iteration         1454 MCMCOBJ=   -6513.60492375968     
 iteration         1455 MCMCOBJ=   -6546.81789133734     
 iteration         1456 MCMCOBJ=   -6535.41216843918     
 iteration         1457 MCMCOBJ=   -6532.04903765859     
 iteration         1458 MCMCOBJ=   -6548.67023915903     
 iteration         1459 MCMCOBJ=   -6580.11466799788     
 iteration         1460 MCMCOBJ=   -6586.29811118093     
 iteration         1461 MCMCOBJ=   -6597.39877184827     
 iteration         1462 MCMCOBJ=   -6600.84714582995     
 iteration         1463 MCMCOBJ=   -6614.99939549890     
 iteration         1464 MCMCOBJ=   -6602.66948369286     
 iteration         1465 MCMCOBJ=   -6614.57355294424     
 iteration         1466 MCMCOBJ=   -6614.23911905735     
 iteration         1467 MCMCOBJ=   -6576.45704414609     
 iteration         1468 MCMCOBJ=   -6613.95511400453     
 iteration         1469 MCMCOBJ=   -6648.97362702499     
 iteration         1470 MCMCOBJ=   -6588.61050251008     
 iteration         1471 MCMCOBJ=   -6595.05202125088     
 iteration         1472 MCMCOBJ=   -6671.79841926396     
 iteration         1473 MCMCOBJ=   -6652.72270756499     
 iteration         1474 MCMCOBJ=   -6673.83392803589     
 iteration         1475 MCMCOBJ=   -6619.78729103708     
 iteration         1476 MCMCOBJ=   -6611.37903718955     
 iteration         1477 MCMCOBJ=   -6621.64848450954     
 iteration         1478 MCMCOBJ=   -6601.17208353862     
 iteration         1479 MCMCOBJ=   -6586.47039818485     
 iteration         1480 MCMCOBJ=   -6605.12124838397     
 iteration         1481 MCMCOBJ=   -6611.76425818622     
 iteration         1482 MCMCOBJ=   -6616.57944501190     
 iteration         1483 MCMCOBJ=   -6617.97276623305     
 iteration         1484 MCMCOBJ=   -6604.92542046916     
 iteration         1485 MCMCOBJ=   -6624.86462405068     
 iteration         1486 MCMCOBJ=   -6632.37216980338     
 iteration         1487 MCMCOBJ=   -6590.07387624257     
 iteration         1488 MCMCOBJ=   -6612.18550981539     
 iteration         1489 MCMCOBJ=   -6569.70058333924     
 iteration         1490 MCMCOBJ=   -6620.83438078952     
 iteration         1491 MCMCOBJ=   -6636.44642263174     
 iteration         1492 MCMCOBJ=   -6637.81241777618     
 iteration         1493 MCMCOBJ=   -6660.16412125884     
 iteration         1494 MCMCOBJ=   -6621.54063377179     
 iteration         1495 MCMCOBJ=   -6547.98623317557     
 iteration         1496 MCMCOBJ=   -6561.16601732122     
 iteration         1497 MCMCOBJ=   -6590.55280659149     
 iteration         1498 MCMCOBJ=   -6616.19596124378     
 iteration         1499 MCMCOBJ=   -6617.70539836722     
 iteration         1500 MCMCOBJ=   -6572.77028229131     
 iteration         1501 MCMCOBJ=   -6591.32881626518     
 iteration         1502 MCMCOBJ=   -6541.64726922437     
 iteration         1503 MCMCOBJ=   -6530.52239169232     
 iteration         1504 MCMCOBJ=   -6597.64105998840     
 iteration         1505 MCMCOBJ=   -6547.94640633994     
 iteration         1506 MCMCOBJ=   -6588.84253437632     
 iteration         1507 MCMCOBJ=   -6615.75340308424     
 iteration         1508 MCMCOBJ=   -6583.51930223284     
 iteration         1509 MCMCOBJ=   -6532.27937280976     
 iteration         1510 MCMCOBJ=   -6517.28316514057     
 iteration         1511 MCMCOBJ=   -6521.44224040647     
 iteration         1512 MCMCOBJ=   -6530.55408569588     
 iteration         1513 MCMCOBJ=   -6532.77480300836     
 iteration         1514 MCMCOBJ=   -6574.02544990575     
 iteration         1515 MCMCOBJ=   -6596.29107174470     
 iteration         1516 MCMCOBJ=   -6615.00817010790     
 iteration         1517 MCMCOBJ=   -6599.72910796983     
 iteration         1518 MCMCOBJ=   -6587.95563953810     
 iteration         1519 MCMCOBJ=   -6568.69540373187     
 iteration         1520 MCMCOBJ=   -6574.25154574807     
 iteration         1521 MCMCOBJ=   -6651.03394972537     
 iteration         1522 MCMCOBJ=   -6590.22977513610     
 iteration         1523 MCMCOBJ=   -6621.60035280740     
 iteration         1524 MCMCOBJ=   -6596.33721201227     
 iteration         1525 MCMCOBJ=   -6620.15626348176     
 iteration         1526 MCMCOBJ=   -6606.29112032622     
 iteration         1527 MCMCOBJ=   -6625.11385291982     
 iteration         1528 MCMCOBJ=   -6608.40308617225     
 iteration         1529 MCMCOBJ=   -6591.38800730991     
 iteration         1530 MCMCOBJ=   -6597.12635908188     
 iteration         1531 MCMCOBJ=   -6612.19749369179     
 iteration         1532 MCMCOBJ=   -6599.25208776912     
 iteration         1533 MCMCOBJ=   -6601.04886318652     
 iteration         1534 MCMCOBJ=   -6607.36733110097     
 iteration         1535 MCMCOBJ=   -6558.58917856718     
 iteration         1536 MCMCOBJ=   -6611.42753711208     
 iteration         1537 MCMCOBJ=   -6621.00312455495     
 iteration         1538 MCMCOBJ=   -6642.97597932451     
 iteration         1539 MCMCOBJ=   -6656.89369199575     
 iteration         1540 MCMCOBJ=   -6622.45780963675     
 iteration         1541 MCMCOBJ=   -6569.23811952824     
 iteration         1542 MCMCOBJ=   -6605.91835700303     
 iteration         1543 MCMCOBJ=   -6547.48973447109     
 iteration         1544 MCMCOBJ=   -6597.40939205000     
 iteration         1545 MCMCOBJ=   -6596.97431687057     
 iteration         1546 MCMCOBJ=   -6620.52533338852     
 iteration         1547 MCMCOBJ=   -6589.10581164876     
 iteration         1548 MCMCOBJ=   -6590.13505344587     
 iteration         1549 MCMCOBJ=   -6604.79704642852     
 iteration         1550 MCMCOBJ=   -6579.26696170953     
 iteration         1551 MCMCOBJ=   -6556.48428666262     
 iteration         1552 MCMCOBJ=   -6565.28887876372     
 iteration         1553 MCMCOBJ=   -6620.90020795855     
 iteration         1554 MCMCOBJ=   -6591.79038390590     
 iteration         1555 MCMCOBJ=   -6575.40829542120     
 iteration         1556 MCMCOBJ=   -6548.68521714414     
 iteration         1557 MCMCOBJ=   -6568.10607237108     
 iteration         1558 MCMCOBJ=   -6593.61901073534     
 iteration         1559 MCMCOBJ=   -6590.99331802312     
 iteration         1560 MCMCOBJ=   -6591.09168056066     
 iteration         1561 MCMCOBJ=   -6587.92234295580     
 iteration         1562 MCMCOBJ=   -6585.17027100276     
 iteration         1563 MCMCOBJ=   -6568.25580020944     
 iteration         1564 MCMCOBJ=   -6562.33327648564     
 iteration         1565 MCMCOBJ=   -6539.97350705382     
 iteration         1566 MCMCOBJ=   -6537.87528215635     
 iteration         1567 MCMCOBJ=   -6572.61031432178     
 iteration         1568 MCMCOBJ=   -6576.85037091702     
 iteration         1569 MCMCOBJ=   -6539.76751921053     
 iteration         1570 MCMCOBJ=   -6557.21530639556     
 iteration         1571 MCMCOBJ=   -6589.54141484826     
 iteration         1572 MCMCOBJ=   -6528.24987083316     
 iteration         1573 MCMCOBJ=   -6646.39074736884     
 iteration         1574 MCMCOBJ=   -6640.43142962915     
 iteration         1575 MCMCOBJ=   -6589.31798533911     
 iteration         1576 MCMCOBJ=   -6561.93202124186     
 iteration         1577 MCMCOBJ=   -6637.11864794211     
 iteration         1578 MCMCOBJ=   -6634.39539717004     
 iteration         1579 MCMCOBJ=   -6654.36381883494     
 iteration         1580 MCMCOBJ=   -6626.72736701767     
 iteration         1581 MCMCOBJ=   -6567.98161376647     
 iteration         1582 MCMCOBJ=   -6577.62501644854     
 iteration         1583 MCMCOBJ=   -6527.68302754025     
 iteration         1584 MCMCOBJ=   -6571.59068980977     
 iteration         1585 MCMCOBJ=   -6595.87137953980     
 iteration         1586 MCMCOBJ=   -6578.63797886671     
 iteration         1587 MCMCOBJ=   -6568.21893425533     
 iteration         1588 MCMCOBJ=   -6544.36250604959     
 iteration         1589 MCMCOBJ=   -6627.82072518291     
 iteration         1590 MCMCOBJ=   -6587.50941003967     
 iteration         1591 MCMCOBJ=   -6557.71080294897     
 iteration         1592 MCMCOBJ=   -6543.71712113783     
 iteration         1593 MCMCOBJ=   -6559.40821861620     
 iteration         1594 MCMCOBJ=   -6554.25862389182     
 iteration         1595 MCMCOBJ=   -6518.28413899414     
 iteration         1596 MCMCOBJ=   -6529.21954683203     
 iteration         1597 MCMCOBJ=   -6597.91727592193     
 iteration         1598 MCMCOBJ=   -6614.83533813043     
 iteration         1599 MCMCOBJ=   -6549.42464570371     
 iteration         1600 MCMCOBJ=   -6576.78125051713     
 iteration         1601 MCMCOBJ=   -6647.98930671779     
 iteration         1602 MCMCOBJ=   -6640.76826012817     
 iteration         1603 MCMCOBJ=   -6611.58981050887     
 iteration         1604 MCMCOBJ=   -6586.63407743800     
 iteration         1605 MCMCOBJ=   -6616.83405261913     
 iteration         1606 MCMCOBJ=   -6627.20648618634     
 iteration         1607 MCMCOBJ=   -6563.52320652227     
 iteration         1608 MCMCOBJ=   -6572.54840824980     
 iteration         1609 MCMCOBJ=   -6580.29073347199     
 iteration         1610 MCMCOBJ=   -6569.20950151108     
 iteration         1611 MCMCOBJ=   -6610.15032452632     
 iteration         1612 MCMCOBJ=   -6597.08284165832     
 iteration         1613 MCMCOBJ=   -6545.34927100262     
 iteration         1614 MCMCOBJ=   -6540.75499401098     
 iteration         1615 MCMCOBJ=   -6587.13301298386     
 iteration         1616 MCMCOBJ=   -6559.18062265613     
 iteration         1617 MCMCOBJ=   -6603.82289955778     
 iteration         1618 MCMCOBJ=   -6620.20682918811     
 iteration         1619 MCMCOBJ=   -6627.41640566436     
 iteration         1620 MCMCOBJ=   -6631.25113614339     
 iteration         1621 MCMCOBJ=   -6623.05563451573     
 iteration         1622 MCMCOBJ=   -6609.06826381082     
 iteration         1623 MCMCOBJ=   -6572.91662701652     
 iteration         1624 MCMCOBJ=   -6618.61207017670     
 iteration         1625 MCMCOBJ=   -6590.66314309859     
 iteration         1626 MCMCOBJ=   -6598.15858777796     
 iteration         1627 MCMCOBJ=   -6573.90930441180     
 iteration         1628 MCMCOBJ=   -6568.77176382211     
 iteration         1629 MCMCOBJ=   -6654.00868527933     
 iteration         1630 MCMCOBJ=   -6585.90112776362     
 iteration         1631 MCMCOBJ=   -6553.36234920417     
 iteration         1632 MCMCOBJ=   -6604.02635934107     
 iteration         1633 MCMCOBJ=   -6642.12218652127     
 iteration         1634 MCMCOBJ=   -6655.82118957544     
 iteration         1635 MCMCOBJ=   -6576.98584646101     
 iteration         1636 MCMCOBJ=   -6579.35822258227     
 iteration         1637 MCMCOBJ=   -6592.90772114985     
 iteration         1638 MCMCOBJ=   -6663.96323458733     
 iteration         1639 MCMCOBJ=   -6666.56718560543     
 iteration         1640 MCMCOBJ=   -6658.73264020041     
 iteration         1641 MCMCOBJ=   -6613.11073502176     
 iteration         1642 MCMCOBJ=   -6599.54500178152     
 iteration         1643 MCMCOBJ=   -6556.62041555407     
 iteration         1644 MCMCOBJ=   -6561.03038623573     
 iteration         1645 MCMCOBJ=   -6516.73795630249     
 iteration         1646 MCMCOBJ=   -6516.11839973187     
 iteration         1647 MCMCOBJ=   -6543.13317733811     
 iteration         1648 MCMCOBJ=   -6584.38964347426     
 iteration         1649 MCMCOBJ=   -6600.99024597688     
 iteration         1650 MCMCOBJ=   -6636.03292011969     
 iteration         1651 MCMCOBJ=   -6592.08112623856     
 iteration         1652 MCMCOBJ=   -6526.00829013131     
 iteration         1653 MCMCOBJ=   -6582.11249694679     
 iteration         1654 MCMCOBJ=   -6511.60235428717     
 iteration         1655 MCMCOBJ=   -6571.17547325512     
 iteration         1656 MCMCOBJ=   -6580.68434111726     
 iteration         1657 MCMCOBJ=   -6637.93647810861     
 iteration         1658 MCMCOBJ=   -6579.32664950374     
 iteration         1659 MCMCOBJ=   -6551.83025015213     
 iteration         1660 MCMCOBJ=   -6587.06309692110     
 iteration         1661 MCMCOBJ=   -6578.71035966067     
 iteration         1662 MCMCOBJ=   -6521.05197165708     
 iteration         1663 MCMCOBJ=   -6543.04102902616     
 iteration         1664 MCMCOBJ=   -6577.01712861799     
 iteration         1665 MCMCOBJ=   -6588.96444597280     
 iteration         1666 MCMCOBJ=   -6597.47425135396     
 iteration         1667 MCMCOBJ=   -6568.52614480133     
 iteration         1668 MCMCOBJ=   -6630.17964730543     
 iteration         1669 MCMCOBJ=   -6653.47682913304     
 iteration         1670 MCMCOBJ=   -6635.98149326170     
 iteration         1671 MCMCOBJ=   -6617.03739052568     
 iteration         1672 MCMCOBJ=   -6623.13938220953     
 iteration         1673 MCMCOBJ=   -6625.03776702263     
 iteration         1674 MCMCOBJ=   -6615.75013892472     
 iteration         1675 MCMCOBJ=   -6645.34227709052     
 iteration         1676 MCMCOBJ=   -6550.97359443551     
 iteration         1677 MCMCOBJ=   -6562.79809317350     
 iteration         1678 MCMCOBJ=   -6566.92154569090     
 iteration         1679 MCMCOBJ=   -6559.62859718240     
 iteration         1680 MCMCOBJ=   -6558.62553023910     
 iteration         1681 MCMCOBJ=   -6589.22002172819     
 iteration         1682 MCMCOBJ=   -6577.84682303710     
 iteration         1683 MCMCOBJ=   -6599.71907477236     
 iteration         1684 MCMCOBJ=   -6585.43247034442     
 iteration         1685 MCMCOBJ=   -6593.96881152040     
 iteration         1686 MCMCOBJ=   -6553.09156938480     
 iteration         1687 MCMCOBJ=   -6586.86385532997     
 iteration         1688 MCMCOBJ=   -6632.88601882027     
 iteration         1689 MCMCOBJ=   -6648.43590036281     
 iteration         1690 MCMCOBJ=   -6628.63060486928     
 iteration         1691 MCMCOBJ=   -6599.86782901331     
 iteration         1692 MCMCOBJ=   -6601.70049813992     
 iteration         1693 MCMCOBJ=   -6566.21951440696     
 iteration         1694 MCMCOBJ=   -6635.04473587766     
 iteration         1695 MCMCOBJ=   -6650.18530342055     
 iteration         1696 MCMCOBJ=   -6678.06026936953     
 iteration         1697 MCMCOBJ=   -6631.54260772997     
 iteration         1698 MCMCOBJ=   -6637.68947549292     
 iteration         1699 MCMCOBJ=   -6648.03416786716     
 iteration         1700 MCMCOBJ=   -6658.71867627461     
 iteration         1701 MCMCOBJ=   -6638.12197521195     
 iteration         1702 MCMCOBJ=   -6654.83128927515     
 iteration         1703 MCMCOBJ=   -6623.02845942530     
 iteration         1704 MCMCOBJ=   -6667.30489319951     
 iteration         1705 MCMCOBJ=   -6655.42785015610     
 iteration         1706 MCMCOBJ=   -6595.79847898743     
 iteration         1707 MCMCOBJ=   -6628.36530849088     
 iteration         1708 MCMCOBJ=   -6601.87972731984     
 iteration         1709 MCMCOBJ=   -6636.86093477579     
 iteration         1710 MCMCOBJ=   -6629.49715809695     
 iteration         1711 MCMCOBJ=   -6645.49129538140     
 iteration         1712 MCMCOBJ=   -6604.74288125812     
 iteration         1713 MCMCOBJ=   -6608.36370558768     
 iteration         1714 MCMCOBJ=   -6617.06752920214     
 iteration         1715 MCMCOBJ=   -6611.18621156514     
 iteration         1716 MCMCOBJ=   -6566.53860398691     
 iteration         1717 MCMCOBJ=   -6583.22403743051     
 iteration         1718 MCMCOBJ=   -6593.31267135311     
 iteration         1719 MCMCOBJ=   -6600.53154196372     
 iteration         1720 MCMCOBJ=   -6602.49357924558     
 iteration         1721 MCMCOBJ=   -6605.35291475389     
 iteration         1722 MCMCOBJ=   -6569.30687330002     
 iteration         1723 MCMCOBJ=   -6561.79336879019     
 iteration         1724 MCMCOBJ=   -6614.94253494041     
 iteration         1725 MCMCOBJ=   -6604.94846700998     
 iteration         1726 MCMCOBJ=   -6623.07131940714     
 iteration         1727 MCMCOBJ=   -6614.22845886594     
 iteration         1728 MCMCOBJ=   -6644.38899743063     
 iteration         1729 MCMCOBJ=   -6655.22074956519     
 iteration         1730 MCMCOBJ=   -6655.22074873664     
 iteration         1731 MCMCOBJ=   -6631.81569582872     
 iteration         1732 MCMCOBJ=   -6631.81569530056     
 iteration         1733 MCMCOBJ=   -6619.41038829372     
 iteration         1734 MCMCOBJ=   -6586.39163340577     
 iteration         1735 MCMCOBJ=   -6569.14261583061     
 iteration         1736 MCMCOBJ=   -6566.34818253778     
 iteration         1737 MCMCOBJ=   -6570.31257000591     
 iteration         1738 MCMCOBJ=   -6552.97599161602     
 iteration         1739 MCMCOBJ=   -6566.67532808358     
 iteration         1740 MCMCOBJ=   -6553.62141249473     
 iteration         1741 MCMCOBJ=   -6550.42159968256     
 iteration         1742 MCMCOBJ=   -6540.13242991473     
 iteration         1743 MCMCOBJ=   -6548.00215961129     
 iteration         1744 MCMCOBJ=   -6548.00215303833     
 iteration         1745 MCMCOBJ=   -6522.34311260502     
 iteration         1746 MCMCOBJ=   -6544.76439681165     
 iteration         1747 MCMCOBJ=   -6544.67440157716     
 iteration         1748 MCMCOBJ=   -6543.81346035070     
 iteration         1749 MCMCOBJ=   -6579.95392847374     
 iteration         1750 MCMCOBJ=   -6603.10343848992     
 iteration         1751 MCMCOBJ=   -6521.85434095612     
 iteration         1752 MCMCOBJ=   -6562.05666151007     
 iteration         1753 MCMCOBJ=   -6590.51928541286     
 iteration         1754 MCMCOBJ=   -6584.30911638038     
 iteration         1755 MCMCOBJ=   -6618.84636579431     
 iteration         1756 MCMCOBJ=   -6612.42389500730     
 iteration         1757 MCMCOBJ=   -6611.78107329360     
 iteration         1758 MCMCOBJ=   -6578.46795981485     
 iteration         1759 MCMCOBJ=   -6526.66594561736     
 iteration         1760 MCMCOBJ=   -6582.39277349773     
 iteration         1761 MCMCOBJ=   -6549.16544319522     
 iteration         1762 MCMCOBJ=   -6612.91931353992     
 iteration         1763 MCMCOBJ=   -6637.85733724210     
 iteration         1764 MCMCOBJ=   -6630.46746000088     
 iteration         1765 MCMCOBJ=   -6613.77312382721     
 iteration         1766 MCMCOBJ=   -6613.66265645380     
 iteration         1767 MCMCOBJ=   -6563.02678934164     
 iteration         1768 MCMCOBJ=   -6591.28680180403     
 iteration         1769 MCMCOBJ=   -6610.00916408992     
 iteration         1770 MCMCOBJ=   -6659.02538955604     
 iteration         1771 MCMCOBJ=   -6646.25817477127     
 iteration         1772 MCMCOBJ=   -6636.75760027673     
 iteration         1773 MCMCOBJ=   -6657.26288200322     
 iteration         1774 MCMCOBJ=   -6635.09893922419     
 iteration         1775 MCMCOBJ=   -6592.33556293811     
 iteration         1776 MCMCOBJ=   -6638.45339848811     
 iteration         1777 MCMCOBJ=   -6597.95215897504     
 iteration         1778 MCMCOBJ=   -6629.86193367590     
 iteration         1779 MCMCOBJ=   -6632.80244037305     
 iteration         1780 MCMCOBJ=   -6631.96120900409     
 iteration         1781 MCMCOBJ=   -6618.34672602728     
 iteration         1782 MCMCOBJ=   -6621.31469068958     
 iteration         1783 MCMCOBJ=   -6595.92491534419     
 iteration         1784 MCMCOBJ=   -6590.33357636066     
 iteration         1785 MCMCOBJ=   -6598.21986015771     
 iteration         1786 MCMCOBJ=   -6569.66298330213     
 iteration         1787 MCMCOBJ=   -6596.11253719378     
 iteration         1788 MCMCOBJ=   -6561.91334255615     
 iteration         1789 MCMCOBJ=   -6614.91534474773     
 iteration         1790 MCMCOBJ=   -6594.40729474941     
 iteration         1791 MCMCOBJ=   -6590.56629423640     
 iteration         1792 MCMCOBJ=   -6610.00983703130     
 iteration         1793 MCMCOBJ=   -6605.46694276323     
 iteration         1794 MCMCOBJ=   -6652.46660184303     
 iteration         1795 MCMCOBJ=   -6622.19799586642     
 iteration         1796 MCMCOBJ=   -6580.10758866102     
 iteration         1797 MCMCOBJ=   -6655.61257867250     
 iteration         1798 MCMCOBJ=   -6635.59724343363     
 iteration         1799 MCMCOBJ=   -6578.19219995424     
 iteration         1800 MCMCOBJ=   -6599.97971369308     
 iteration         1801 MCMCOBJ=   -6594.59123423260     
 iteration         1802 MCMCOBJ=   -6621.80175023892     
 iteration         1803 MCMCOBJ=   -6597.54120675512     
 iteration         1804 MCMCOBJ=   -6605.06765814752     
 iteration         1805 MCMCOBJ=   -6625.49639334273     
 iteration         1806 MCMCOBJ=   -6582.37836112742     
 iteration         1807 MCMCOBJ=   -6601.22763678713     
 iteration         1808 MCMCOBJ=   -6590.10128248092     
 iteration         1809 MCMCOBJ=   -6591.07124776051     
 iteration         1810 MCMCOBJ=   -6620.15748470897     
 iteration         1811 MCMCOBJ=   -6597.36530660855     
 iteration         1812 MCMCOBJ=   -6654.46152829655     
 iteration         1813 MCMCOBJ=   -6655.94376744726     
 iteration         1814 MCMCOBJ=   -6648.60700592830     
 iteration         1815 MCMCOBJ=   -6610.83265517995     
 iteration         1816 MCMCOBJ=   -6598.56851685837     
 iteration         1817 MCMCOBJ=   -6635.31870815934     
 iteration         1818 MCMCOBJ=   -6596.64267656915     
 iteration         1819 MCMCOBJ=   -6610.39449370754     
 iteration         1820 MCMCOBJ=   -6580.60815031586     
 iteration         1821 MCMCOBJ=   -6641.83726449065     
 iteration         1822 MCMCOBJ=   -6644.29549113633     
 iteration         1823 MCMCOBJ=   -6644.29560447559     
 iteration         1824 MCMCOBJ=   -6621.93627661614     
 iteration         1825 MCMCOBJ=   -6628.65525586136     
 iteration         1826 MCMCOBJ=   -6623.12105233941     
 iteration         1827 MCMCOBJ=   -6611.04673132986     
 iteration         1828 MCMCOBJ=   -6608.06801353503     
 iteration         1829 MCMCOBJ=   -6576.27785171359     
 iteration         1830 MCMCOBJ=   -6518.60864721099     
 iteration         1831 MCMCOBJ=   -6525.07464190904     
 iteration         1832 MCMCOBJ=   -6540.43687602087     
 iteration         1833 MCMCOBJ=   -6593.00185437079     
 iteration         1834 MCMCOBJ=   -6583.59763835470     
 iteration         1835 MCMCOBJ=   -6670.83672909389     
 iteration         1836 MCMCOBJ=   -6672.70691015667     
 iteration         1837 MCMCOBJ=   -6608.10531495785     
 iteration         1838 MCMCOBJ=   -6582.95751395810     
 iteration         1839 MCMCOBJ=   -6592.82788246117     
 iteration         1840 MCMCOBJ=   -6610.96957710005     
 iteration         1841 MCMCOBJ=   -6589.74188011147     
 iteration         1842 MCMCOBJ=   -6612.81827510715     
 iteration         1843 MCMCOBJ=   -6579.58581099746     
 iteration         1844 MCMCOBJ=   -6603.44133453105     
 iteration         1845 MCMCOBJ=   -6612.49570066313     
 iteration         1846 MCMCOBJ=   -6626.96368618848     
 iteration         1847 MCMCOBJ=   -6606.21187999508     
 iteration         1848 MCMCOBJ=   -6647.45221662014     
 iteration         1849 MCMCOBJ=   -6634.61232736688     
 iteration         1850 MCMCOBJ=   -6628.03366021569     
 iteration         1851 MCMCOBJ=   -6604.95603242870     
 iteration         1852 MCMCOBJ=   -6635.13593419579     
 iteration         1853 MCMCOBJ=   -6612.95264731110     
 iteration         1854 MCMCOBJ=   -6593.00612534446     
 iteration         1855 MCMCOBJ=   -6561.85492392133     
 iteration         1856 MCMCOBJ=   -6556.66897053105     
 iteration         1857 MCMCOBJ=   -6605.27690730434     
 iteration         1858 MCMCOBJ=   -6649.79361629465     
 iteration         1859 MCMCOBJ=   -6587.67111136172     
 iteration         1860 MCMCOBJ=   -6626.36154181698     
 iteration         1861 MCMCOBJ=   -6625.43050445302     
 iteration         1862 MCMCOBJ=   -6575.64364128151     
 iteration         1863 MCMCOBJ=   -6555.08758706077     
 iteration         1864 MCMCOBJ=   -6585.64376437453     
 iteration         1865 MCMCOBJ=   -6577.63101007830     
 iteration         1866 MCMCOBJ=   -6586.88886374674     
 iteration         1867 MCMCOBJ=   -6528.15860339197     
 iteration         1868 MCMCOBJ=   -6543.55500749152     
 iteration         1869 MCMCOBJ=   -6602.33655585180     
 iteration         1870 MCMCOBJ=   -6574.43613149909     
 iteration         1871 MCMCOBJ=   -6590.10062430746     
 iteration         1872 MCMCOBJ=   -6619.58755149995     
 iteration         1873 MCMCOBJ=   -6640.85112392978     
 iteration         1874 MCMCOBJ=   -6627.74017453955     
 iteration         1875 MCMCOBJ=   -6581.05968043109     
 iteration         1876 MCMCOBJ=   -6568.78990923290     
 iteration         1877 MCMCOBJ=   -6600.72333036536     
 iteration         1878 MCMCOBJ=   -6640.24096309520     
 iteration         1879 MCMCOBJ=   -6633.09290102475     
 iteration         1880 MCMCOBJ=   -6626.19265637823     
 iteration         1881 MCMCOBJ=   -6647.83008254410     
 iteration         1882 MCMCOBJ=   -6566.99684262177     
 iteration         1883 MCMCOBJ=   -6596.49098319608     
 iteration         1884 MCMCOBJ=   -6536.32555800415     
 iteration         1885 MCMCOBJ=   -6540.04515630373     
 iteration         1886 MCMCOBJ=   -6469.40918754308     
 iteration         1887 MCMCOBJ=   -6527.22891217758     
 iteration         1888 MCMCOBJ=   -6558.45081402229     
 iteration         1889 MCMCOBJ=   -6570.07972443875     
 iteration         1890 MCMCOBJ=   -6633.84215767648     
 iteration         1891 MCMCOBJ=   -6677.04441098941     
 iteration         1892 MCMCOBJ=   -6656.40275813893     
 iteration         1893 MCMCOBJ=   -6629.61257949740     
 iteration         1894 MCMCOBJ=   -6632.14981023165     
 iteration         1895 MCMCOBJ=   -6613.15438862116     
 iteration         1896 MCMCOBJ=   -6638.41627305631     
 iteration         1897 MCMCOBJ=   -6573.88476321310     
 iteration         1898 MCMCOBJ=   -6574.46930747354     
 iteration         1899 MCMCOBJ=   -6585.85600522128     
 iteration         1900 MCMCOBJ=   -6549.95589650615     
 iteration         1901 MCMCOBJ=   -6561.38106716685     
 iteration         1902 MCMCOBJ=   -6551.77326439301     
 iteration         1903 MCMCOBJ=   -6599.78710641348     
 iteration         1904 MCMCOBJ=   -6621.65338598343     
 iteration         1905 MCMCOBJ=   -6629.04952294059     
 iteration         1906 MCMCOBJ=   -6650.76158027173     
 iteration         1907 MCMCOBJ=   -6646.43822953019     
 iteration         1908 MCMCOBJ=   -6639.38660431577     
 iteration         1909 MCMCOBJ=   -6628.08200448240     
 iteration         1910 MCMCOBJ=   -6681.01635922088     
 iteration         1911 MCMCOBJ=   -6668.62595915530     
 iteration         1912 MCMCOBJ=   -6612.94898482519     
 iteration         1913 MCMCOBJ=   -6514.09038945026     
 iteration         1914 MCMCOBJ=   -6557.78210054696     
 iteration         1915 MCMCOBJ=   -6561.77074008324     
 iteration         1916 MCMCOBJ=   -6559.49922860126     
 iteration         1917 MCMCOBJ=   -6621.28762510030     
 iteration         1918 MCMCOBJ=   -6595.49613930723     
 iteration         1919 MCMCOBJ=   -6611.28727304993     
 iteration         1920 MCMCOBJ=   -6627.41701549117     
 iteration         1921 MCMCOBJ=   -6623.16417392776     
 iteration         1922 MCMCOBJ=   -6593.73544186771     
 iteration         1923 MCMCOBJ=   -6582.29970308511     
 iteration         1924 MCMCOBJ=   -6600.02841478884     
 iteration         1925 MCMCOBJ=   -6642.60455580933     
 iteration         1926 MCMCOBJ=   -6644.41606529310     
 iteration         1927 MCMCOBJ=   -6652.86974757068     
 iteration         1928 MCMCOBJ=   -6610.37569725511     
 iteration         1929 MCMCOBJ=   -6618.29830267923     
 iteration         1930 MCMCOBJ=   -6591.93553010520     
 iteration         1931 MCMCOBJ=   -6614.10374152691     
 iteration         1932 MCMCOBJ=   -6635.97400257565     
 iteration         1933 MCMCOBJ=   -6631.41429334880     
 iteration         1934 MCMCOBJ=   -6591.52112498323     
 iteration         1935 MCMCOBJ=   -6632.85735468965     
 iteration         1936 MCMCOBJ=   -6631.93163916878     
 iteration         1937 MCMCOBJ=   -6628.46655540601     
 iteration         1938 MCMCOBJ=   -6629.97791439516     
 iteration         1939 MCMCOBJ=   -6643.47400940716     
 iteration         1940 MCMCOBJ=   -6591.79523414070     
 iteration         1941 MCMCOBJ=   -6619.85295401990     
 iteration         1942 MCMCOBJ=   -6562.36414734411     
 iteration         1943 MCMCOBJ=   -6623.51727034472     
 iteration         1944 MCMCOBJ=   -6611.22319222494     
 iteration         1945 MCMCOBJ=   -6584.49431108051     
 iteration         1946 MCMCOBJ=   -6569.66167319924     
 iteration         1947 MCMCOBJ=   -6554.96238708852     
 iteration         1948 MCMCOBJ=   -6574.01534852471     
 iteration         1949 MCMCOBJ=   -6639.75256825546     
 iteration         1950 MCMCOBJ=   -6624.81974855140     
 iteration         1951 MCMCOBJ=   -6674.24796629518     
 iteration         1952 MCMCOBJ=   -6666.76272350369     
 iteration         1953 MCMCOBJ=   -6621.80799213349     
 iteration         1954 MCMCOBJ=   -6607.76587456197     
 iteration         1955 MCMCOBJ=   -6590.28871300683     
 iteration         1956 MCMCOBJ=   -6605.22213504111     
 iteration         1957 MCMCOBJ=   -6615.00759736202     
 iteration         1958 MCMCOBJ=   -6604.36633782584     
 iteration         1959 MCMCOBJ=   -6617.85816709099     
 iteration         1960 MCMCOBJ=   -6623.70853389254     
 iteration         1961 MCMCOBJ=   -6595.04845205540     
 iteration         1962 MCMCOBJ=   -6606.49491283284     
 iteration         1963 MCMCOBJ=   -6597.00350061115     
 iteration         1964 MCMCOBJ=   -6581.76315979124     
 iteration         1965 MCMCOBJ=   -6615.35284778237     
 iteration         1966 MCMCOBJ=   -6592.80677878294     
 iteration         1967 MCMCOBJ=   -6618.07715702661     
 iteration         1968 MCMCOBJ=   -6623.24721181931     
 iteration         1969 MCMCOBJ=   -6583.55008940897     
 iteration         1970 MCMCOBJ=   -6596.86861756282     
 iteration         1971 MCMCOBJ=   -6578.95997763575     
 iteration         1972 MCMCOBJ=   -6570.13476070908     
 iteration         1973 MCMCOBJ=   -6617.88788310189     
 iteration         1974 MCMCOBJ=   -6597.71138047632     
 iteration         1975 MCMCOBJ=   -6601.99371494821     
 iteration         1976 MCMCOBJ=   -6607.12705279848     
 iteration         1977 MCMCOBJ=   -6602.02538443598     
 iteration         1978 MCMCOBJ=   -6588.25094082454     
 iteration         1979 MCMCOBJ=   -6604.42925934346     
 iteration         1980 MCMCOBJ=   -6552.63450336545     
 iteration         1981 MCMCOBJ=   -6569.51973381894     
 iteration         1982 MCMCOBJ=   -6568.14664480009     
 iteration         1983 MCMCOBJ=   -6567.67246699825     
 iteration         1984 MCMCOBJ=   -6560.41499787254     
 iteration         1985 MCMCOBJ=   -6631.02736361696     
 iteration         1986 MCMCOBJ=   -6599.43445227820     
 iteration         1987 MCMCOBJ=   -6588.27660546013     
 iteration         1988 MCMCOBJ=   -6570.45429869477     
 iteration         1989 MCMCOBJ=   -6541.32346605007     
 iteration         1990 MCMCOBJ=   -6552.08452295196     
 iteration         1991 MCMCOBJ=   -6572.67381774217     
 iteration         1992 MCMCOBJ=   -6628.50085640302     
 iteration         1993 MCMCOBJ=   -6626.16765704870     
 iteration         1994 MCMCOBJ=   -6615.34338776665     
 iteration         1995 MCMCOBJ=   -6635.56629209536     
 iteration         1996 MCMCOBJ=   -6647.42856085642     
 iteration         1997 MCMCOBJ=   -6610.66133906517     
 iteration         1998 MCMCOBJ=   -6586.25951897907     
 iteration         1999 MCMCOBJ=   -6591.97398872417     
 iteration         2000 MCMCOBJ=   -6557.66994522910     
 BURN-IN WAS NOT TESTED FOR CONVERGENCE
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6596.66557179355     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3714.87433166370     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6596.66557179355     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5861.51474522981     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:   -16.9020929079542     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6596.66557179355     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6613.56766470151     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds: 10419.91
 Elapsed covariance  time in seconds:     0.00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6596.666       **************************************************
 #OBJS:********************************************       36.749 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              NUTS BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.21E+00  5.56E-01 -1.79E-01  2.27E+00  2.35E-01  3.71E+00 -7.05E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.65E-01
 
 ETA2
+       -3.03E-02  1.77E-01
 
 ETA3
+        2.87E-02 -1.04E-02  1.05E-01
 
 ETA4
+        2.19E-02  3.27E-02 -1.24E-02  2.47E-01
 
 ETA5
+        2.11E-02  1.86E-02 -1.41E-03 -2.37E-02  1.89E-01
 
 ETA6
+       -1.21E-02  9.87E-03  1.70E-02  1.33E-02 -5.48E-02  2.16E-01
 
 ETA7
+        1.21E-02 -3.57E-02  1.84E-02 -5.47E-02  1.83E-02  6.97E-03  2.25E-01
 
 ETA8
+        7.01E-02  6.28E-02  2.56E-02  3.19E-02 -6.90E-03 -4.31E-02  4.65E-02  1.92E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.36E-03
 
 EPS2
+        0.00E+00  2.24E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.12E-01
 
 ETA2
+       -1.38E-01  4.16E-01
 
 ETA3
+        1.76E-01 -7.91E-02  3.21E-01
 
 ETA4
+        8.60E-02  1.58E-01 -7.95E-02  4.94E-01
 
 ETA5
+        9.43E-02  1.03E-01 -1.17E-02 -1.10E-01  4.32E-01
 
 ETA6
+       -5.40E-02  5.24E-02  1.15E-01  5.72E-02 -2.76E-01  4.61E-01
 
 ETA7
+        5.15E-02 -1.76E-01  1.22E-01 -2.33E-01  8.86E-02  3.08E-02  4.72E-01
 
 ETA8
+        3.11E-01  3.44E-01  1.81E-01  1.47E-01 -3.60E-02 -2.12E-01  2.23E-01  4.36E-01
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        9.67E-02
 
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
 
         7.38E-02  7.15E-02  5.46E-02  7.32E-02  6.31E-02  7.38E-02  7.00E-02  6.15E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.72E-02
 
 ETA2
+        3.05E-02  5.03E-02
 
 ETA3
+        2.16E-02  1.92E-02  3.02E-02
 
 ETA4
+        3.14E-02  2.78E-02  2.35E-02  5.68E-02
 
 ETA5
+        2.82E-02  2.42E-02  1.92E-02  2.78E-02  4.27E-02
 
 ETA6
+        3.14E-02  2.80E-02  2.12E-02  3.08E-02  2.80E-02  5.65E-02
 
 ETA7
+        2.96E-02  2.91E-02  2.01E-02  3.05E-02  2.53E-02  2.90E-02  4.77E-02
 
 ETA8
+        2.90E-02  2.49E-02  1.93E-02  2.77E-02  2.24E-02  2.69E-02  2.57E-02  3.92E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.42E-04
 
 EPS2
+        0.00E+00  1.22E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.42E-02
 
 ETA2
+        1.31E-01  5.78E-02
 
 ETA3
+        1.24E-01  1.37E-01  4.52E-02
 
 ETA4
+        1.19E-01  1.26E-01  1.42E-01  5.57E-02
 
 ETA5
+        1.20E-01  1.26E-01  1.33E-01  1.23E-01  4.80E-02
 
 ETA6
+        1.28E-01  1.40E-01  1.35E-01  1.30E-01  1.31E-01  5.92E-02
 
 ETA7
+        1.18E-01  1.32E-01  1.26E-01  1.19E-01  1.18E-01  1.27E-01  4.92E-02
 
 ETA8
+        1.11E-01  1.15E-01  1.25E-01  1.22E-01  1.15E-01  1.20E-01  1.10E-01  4.39E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.31E-03
 
 EPS2
+        0.00E+00  4.06E-03
 
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
+        5.44E-03
 
 TH 2
+       -8.88E-04  5.11E-03
 
 TH 3
+        4.58E-04 -7.44E-05  2.98E-03
 
 TH 4
+        4.03E-05  8.15E-04  1.51E-04  5.35E-03
 
 TH 5
+        4.60E-04  2.22E-04  2.73E-05 -2.87E-04  3.98E-03
 
 TH 6
+       -1.51E-04  2.47E-05  3.44E-04  2.25E-04 -8.57E-04  5.45E-03
 
 TH 7
+        1.79E-04 -9.73E-04  5.41E-04 -1.29E-03  6.53E-04  4.45E-04  4.91E-03
 
 TH 8
+        1.24E-03  9.16E-04  6.34E-04  4.51E-04  6.61E-05 -8.86E-04  9.92E-04  3.78E-03
 
 OM11
+       -3.24E-05  2.68E-05 -7.73E-05 -2.15E-04 -1.49E-05 -1.20E-04 -6.70E-05 -9.76E-05  3.27E-03
 
 OM12
+       -1.20E-05  1.83E-04 -5.55E-05 -4.49E-05  6.98E-05 -9.52E-05 -8.05E-05 -5.58E-05 -3.64E-04  9.33E-04
 
 OM13
+        3.07E-05  7.66E-06  9.24E-05  2.21E-05 -2.27E-05  4.64E-06  3.05E-05 -3.34E-05  1.33E-04 -2.72E-05  4.68E-04
 
 OM14
+        3.89E-05 -4.09E-05  2.57E-05  2.63E-05  5.66E-05 -6.76E-05  6.81E-05 -1.77E-05  2.07E-04  3.73E-05 -8.00E-06  9.88E-04
 
 OM15
+        4.14E-05 -6.58E-05  2.37E-05 -7.64E-05  7.25E-05 -1.52E-04 -7.86E-05  6.74E-05  1.51E-04  3.68E-05  1.34E-05 -8.46E-05
          7.93E-04
 
 OM16
+       -8.77E-05  4.13E-05 -9.94E-06 -7.77E-05 -5.31E-05  6.06E-06 -2.38E-05 -1.21E-05  2.51E-05  1.24E-05  4.29E-05  8.24E-05
         -9.66E-05  9.87E-04
 
 OM17
+        2.91E-05  3.46E-05  1.26E-05  2.41E-05 -3.97E-05  2.61E-05  7.23E-05  5.14E-05 -3.46E-05 -1.51E-04  1.54E-05 -1.05E-04
          2.78E-05  3.83E-05  8.73E-04
 
 OM18
+       -2.90E-06 -3.17E-05 -1.83E-05 -1.50E-04 -2.92E-05  2.88E-06  3.18E-05 -5.72E-05  6.03E-04  8.95E-05  8.51E-05  1.26E-04
         -2.43E-05 -1.05E-04  1.56E-04  8.40E-04
 
 OM22
+       -2.65E-05 -8.15E-04  1.30E-04  1.33E-05 -3.83E-05  1.53E-04  1.95E-04  1.51E-04  6.88E-05 -4.56E-04  4.48E-05 -1.27E-05
         -4.72E-05 -2.25E-05  1.43E-05 -2.64E-05  2.53E-03
 
 OM23
+       -3.26E-05  1.20E-04  3.84E-05  5.81E-05 -9.30E-06  3.13E-05 -4.16E-05 -3.94E-06 -4.01E-05  5.69E-05 -2.24E-05 -6.07E-06
         -1.78E-05 -1.62E-05 -1.59E-05 -1.20E-05 -9.89E-05  3.68E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        4.66E-05 -3.06E-04 -3.65E-05 -2.89E-05 -3.94E-05  8.41E-05 -4.63E-05  2.59E-05 -6.95E-05 -4.91E-05  7.72E-06 -7.55E-05
         -9.31E-06 -2.17E-05 -7.20E-07  3.73E-05  3.38E-04 -1.01E-05  7.72E-04
 
 OM25
+       -1.51E-05 -3.49E-05 -7.12E-06 -3.47E-05  1.93E-05  7.30E-05  9.69E-05  3.91E-05 -3.60E-05  2.87E-05 -2.67E-05  2.76E-05
         -1.19E-04  2.68E-05  1.48E-05  1.32E-05  1.66E-04 -1.86E-05 -1.54E-05  5.85E-04
 
 OM26
+       -4.30E-05  9.62E-05  1.95E-05  5.66E-05  2.60E-05 -2.67E-06 -9.09E-05 -2.80E-05  1.22E-05  2.28E-05 -2.33E-05 -2.72E-05
         -1.09E-06 -1.09E-04 -8.04E-06  1.72E-05  2.19E-05  3.04E-05 -2.12E-06 -6.74E-05  7.86E-04
 
 OM27
+        4.72E-05  4.41E-04  3.22E-05  5.04E-05  1.05E-04 -1.29E-04 -3.42E-06 -5.76E-05  2.53E-05  9.31E-05 -3.80E-05  6.27E-06
          1.97E-05  9.16E-06 -8.30E-05 -4.68E-05 -5.51E-04  6.72E-05 -2.33E-04  4.49E-05  7.65E-05  8.48E-04
 
 OM28
+        1.11E-05 -1.80E-05  6.06E-05 -6.46E-05 -3.50E-05  6.15E-06  2.93E-05  1.01E-05 -6.35E-05  9.34E-05  1.44E-06 -1.01E-05
          2.21E-05 -7.62E-06 -4.45E-05 -2.45E-05  4.57E-04  3.80E-05  9.16E-05 -1.10E-05 -1.23E-04  5.91E-05  6.18E-04
 
 OM33
+       -4.18E-05 -3.39E-05 -2.23E-04 -1.75E-04 -5.13E-05  1.20E-04  4.95E-05 -3.77E-05 -4.43E-06 -1.48E-05  7.71E-05  1.32E-05
         -5.36E-05  3.87E-05  1.20E-05  6.55E-06  3.40E-05 -4.59E-07 -1.06E-05  2.16E-05 -3.56E-05 -3.08E-05 -1.04E-05  9.13E-04
 
 OM34
+       -1.87E-05  2.88E-05  1.59E-04  1.97E-04 -2.61E-05 -8.07E-05 -3.51E-05  6.29E-05 -5.80E-05 -8.75E-06  1.86E-05  7.35E-05
          7.60E-06 -1.51E-05 -1.08E-05 -8.53E-06  5.99E-05  3.00E-05  3.85E-06 -2.09E-05 -1.03E-05 -1.12E-05  2.90E-05 -7.23E-05
         5.50E-04
 
 OM35
+       -3.65E-05 -1.03E-05 -3.00E-06 -8.89E-06 -6.71E-05  7.30E-06  1.44E-05  2.33E-05  7.08E-06 -1.17E-05  1.76E-05 -2.15E-07
         -1.78E-06  2.13E-05  1.77E-06 -1.50E-05  6.62E-06  1.84E-05 -2.26E-05  6.33E-06  4.14E-05  3.28E-05  3.64E-06  2.78E-05
        -5.17E-05  3.69E-04
 
 OM36
+       -7.28E-05 -2.81E-05 -6.16E-05 -6.00E-05 -1.82E-05 -2.10E-05 -2.58E-05 -3.43E-05 -4.43E-05  1.81E-05 -6.98E-06 -2.24E-06
         -1.32E-05  7.04E-05  7.03E-06 -7.13E-06  3.48E-05  1.25E-05  4.74E-05  3.95E-06 -1.12E-05 -2.16E-05  5.91E-07  6.36E-05
         3.57E-05 -4.65E-05  4.50E-04
 
 OM37
+        2.22E-05  3.26E-05 -1.02E-04 -8.18E-05 -1.21E-04 -4.44E-06 -6.43E-05 -2.13E-05  5.73E-05  3.20E-06 -1.33E-05  1.63E-05
          9.21E-06 -1.53E-06  7.51E-05  1.71E-05 -3.45E-05 -7.32E-05 -1.33E-05  9.19E-06  4.98E-06  6.51E-06 -1.81E-05  6.73E-05
        -7.61E-05  4.66E-05  6.17E-06  4.05E-04
 
 OM38
+       -2.42E-05 -4.65E-05  6.10E-05  7.00E-05 -2.35E-05 -3.53E-05  4.24E-06  2.87E-05  9.09E-05 -2.16E-05  1.07E-04  3.25E-05
         -3.81E-06  8.95E-06  8.69E-06  5.17E-05  5.76E-05  8.29E-05 -6.42E-06 -2.45E-06 -2.04E-05 -8.57E-06  2.26E-05  1.57E-04
         4.59E-05 -1.77E-05 -5.83E-05  6.28E-05  3.72E-04
 
 OM44
+       -3.00E-05  8.48E-05  4.80E-04  4.11E-04 -4.53E-05  1.42E-04 -4.95E-06  1.23E-04 -5.34E-05  9.95E-07 -1.34E-05  1.37E-04
          2.44E-05 -7.80E-06  8.08E-05 -2.77E-05  7.28E-05  3.77E-05  1.82E-04 -4.13E-05  1.12E-04 -3.87E-05  3.25E-05 -1.13E-04
         8.42E-05 -4.20E-05  8.61E-07 -2.00E-05 -3.61E-05  3.22E-03
 
 OM45
+       -7.54E-05  6.09E-05  1.76E-05 -1.79E-04  8.45E-05  1.13E-04  1.16E-04 -7.17E-06  3.12E-06  1.91E-05  1.24E-05  4.80E-05
          2.51E-05  5.75E-05 -1.83E-05  2.17E-05 -5.22E-05 -1.09E-05  3.30E-05  4.37E-05 -4.27E-05  3.29E-05  1.11E-05  1.42E-05
        -1.92E-05 -6.31E-06  9.29E-06 -2.12E-05  2.72E-05 -2.21E-04  7.73E-04
 
 OM46
+        4.80E-05 -4.13E-05  9.22E-05  3.22E-06 -3.05E-07  2.14E-04  8.45E-05  1.70E-05 -6.45E-05 -4.10E-06  2.72E-05  1.05E-06
         -3.99E-05  8.80E-05  1.97E-05 -1.55E-05  1.55E-05  1.15E-05  2.44E-06  2.44E-06  6.36E-05 -2.14E-05 -1.31E-05  3.47E-06
        -3.02E-05  2.98E-05  3.59E-06  1.09E-05 -1.36E-05  9.21E-05 -6.79E-05  9.46E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.70E-06  8.42E-06 -1.65E-05 -2.21E-05 -4.95E-05  4.12E-05  1.55E-04 -5.34E-06  3.24E-05 -3.17E-05  1.81E-06 -3.08E-05
          2.94E-05  4.41E-05  8.14E-05  2.46E-06 -2.16E-05 -2.57E-05 -1.51E-04  1.25E-05 -4.88E-06  8.89E-05 -1.13E-05 -2.44E-06
         1.16E-05  1.82E-05 -3.04E-05  2.72E-06 -1.36E-05 -3.90E-04  6.50E-05  1.95E-05  9.33E-04
 
 OM48
+        7.18E-06 -2.32E-05 -3.04E-05  6.56E-05 -8.74E-05  7.06E-05 -2.88E-05  1.02E-05  5.12E-05  6.34E-06 -8.47E-06  1.83E-04
         -1.46E-05  1.05E-05  2.70E-05  8.08E-05  7.90E-05 -1.07E-05  1.78E-04  1.95E-05  3.55E-06 -3.04E-05  8.02E-05 -1.62E-05
         7.75E-05 -1.34E-05 -5.53E-06 -3.94E-05 -1.86E-05  3.33E-04 -4.27E-05 -1.06E-04  1.09E-04  7.69E-04
 
 OM55
+        1.15E-04 -1.48E-05  3.66E-05  7.06E-05 -1.10E-04  1.51E-05 -9.90E-05  5.78E-05  2.27E-05  1.97E-05  2.92E-05 -3.91E-05
          1.86E-04 -1.01E-04  2.88E-05  1.99E-05  5.68E-05 -4.01E-05  2.20E-05  1.12E-04  1.35E-05 -4.48E-05  9.80E-06  7.71E-06
         1.11E-05  4.85E-06 -1.09E-05  1.83E-06  1.33E-09  9.88E-08 -1.36E-04 -1.96E-05  2.72E-05  3.23E-05  1.82E-03
 
 OM56
+       -3.25E-05 -4.46E-05 -3.50E-05 -2.71E-05 -1.26E-04 -3.91E-05  1.10E-05 -2.51E-05 -3.05E-05  5.28E-06 -7.55E-06  3.47E-05
         -5.41E-05  4.73E-05  2.10E-05  3.20E-05 -3.54E-05  1.39E-05 -4.60E-05 -2.04E-06  4.45E-05  3.88E-05  5.45E-07  1.75E-05
         3.07E-05  3.75E-05 -1.15E-05  1.11E-07  7.92E-06  3.02E-05  6.20E-05 -7.18E-05  2.04E-05  4.88E-06 -1.91E-04  7.81E-04
 
 OM57
+       -1.15E-05 -4.77E-05 -6.11E-05  2.53E-05  4.35E-05 -1.27E-05  9.63E-06 -2.47E-05  3.56E-05 -1.48E-05 -5.34E-06  7.66E-06
         -3.78E-06 -2.93E-05  7.80E-05  4.73E-06 -2.07E-05  1.90E-07  1.76E-05 -3.66E-05  2.57E-05  2.69E-06  2.92E-06 -2.43E-05
        -1.44E-06  2.83E-05 -9.81E-06  2.79E-05  1.24E-05  5.23E-05 -9.93E-05 -1.31E-05 -1.13E-04 -1.62E-05  1.22E-04  2.42E-07
          6.40E-04
 
 OM58
+       -4.55E-05  2.66E-06 -6.93E-06 -6.66E-05  4.31E-06 -2.83E-05  6.82E-05  4.71E-05  3.52E-05 -5.79E-06  5.83E-06 -1.07E-05
          1.23E-04  3.53E-06  1.58E-05  1.74E-05  1.50E-05 -8.70E-06  3.15E-06  1.12E-04 -9.81E-06  2.97E-05  1.17E-05 -1.47E-05
        -2.01E-05  5.00E-05  3.86E-06  1.67E-05 -1.61E-05  4.72E-05  4.60E-05  2.04E-05 -1.93E-05  4.66E-06 -4.39E-05 -1.06E-04
          1.33E-04  5.04E-04
 
 OM66
+       -5.60E-06  6.12E-05  3.55E-05  9.55E-05  1.14E-04  6.97E-05  1.18E-04  9.35E-05  7.86E-05  3.69E-05  2.93E-05 -6.69E-05
         -3.21E-05  1.55E-04  3.61E-05  2.47E-05  1.28E-04 -4.60E-06  8.18E-06 -2.52E-05  5.45E-05 -1.45E-05  6.13E-05  8.36E-05
        -2.94E-05  4.02E-05  1.20E-04  2.47E-05  4.34E-05  4.92E-05 -2.53E-05  1.87E-04 -5.55E-05 -8.65E-05  1.39E-05 -3.57E-04
         -4.41E-05 -8.28E-06  3.20E-03
 
 OM67
+        6.89E-05 -1.19E-05 -4.06E-05 -2.71E-05 -1.55E-04 -6.05E-06 -1.42E-05  1.87E-05  2.74E-05  4.57E-06 -2.23E-06 -1.60E-05
          2.04E-06  1.51E-05  2.75E-05  1.58E-05 -6.28E-05  1.77E-05  2.86E-06 -1.05E-05 -1.06E-04 -3.32E-06  2.41E-06  2.96E-05
         1.27E-05 -1.48E-05  6.86E-06  2.97E-05  9.10E-06 -3.62E-05 -1.59E-06 -1.02E-04  4.31E-05  4.29E-05  6.23E-05  5.69E-05
         -9.78E-05 -1.80E-05  1.08E-04  8.38E-04
 
 OM68
+        1.25E-07 -5.05E-05 -4.49E-06 -3.79E-05  1.44E-05  5.85E-06 -4.70E-05  1.65E-05  1.93E-05  3.79E-06 -2.07E-05  6.83E-06
          8.89E-06  1.50E-04  2.42E-05 -1.75E-05 -3.71E-05 -9.73E-06  9.40E-06  1.10E-05  1.78E-04 -1.48E-05 -8.87E-05  2.09E-06
        -1.34E-05 -9.38E-06  5.21E-05  1.93E-05 -2.29E-05  6.30E-05 -2.68E-05  6.91E-05  3.34E-05  4.95E-05 -2.18E-05  3.71E-07
         -1.95E-05 -3.23E-05 -4.15E-04  1.37E-04  7.22E-04
 
 OM77
+        2.66E-05 -1.20E-04 -6.65E-05 -7.76E-05  2.93E-05  6.85E-05 -4.59E-05 -1.03E-04  5.38E-05  3.01E-05  5.76E-05  4.63E-05
         -2.82E-05 -3.97E-06 -3.97E-05  6.74E-05  5.94E-05 -3.03E-07  6.89E-05 -3.17E-05 -7.31E-05 -4.10E-04 -4.14E-05  6.06E-05
        -4.24E-06 -1.62E-05  8.27E-05  9.30E-05  6.37E-05  7.97E-05 -3.82E-05  1.85E-06 -4.02E-04 -7.33E-05  6.47E-07 -1.21E-05
          1.72E-04  9.38E-06  1.37E-04  1.09E-04 -5.71E-06  2.28E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -1.07E-05 -8.45E-05  3.23E-05 -3.28E-05  4.66E-05  8.61E-06  8.58E-05 -3.06E-05 -1.04E-05 -1.24E-05  1.57E-05 -1.04E-05
         -1.53E-05 -1.04E-05  1.59E-04  5.26E-05 -9.22E-05 -1.49E-05 -6.26E-05  3.95E-05 -2.88E-06  7.16E-05 -7.08E-05  2.48E-05
        -1.98E-05  9.97E-06  4.17E-08  9.64E-05  4.42E-05  1.74E-05 -1.75E-07 -6.49E-06  2.85E-05 -1.07E-04 -1.53E-05  1.24E-05
          1.03E-05  1.57E-05  6.34E-06 -8.83E-05 -4.24E-06  3.83E-04  6.58E-04
 
 OM88
+        2.20E-05 -5.89E-06  3.49E-05 -5.58E-05  4.74E-05  2.38E-05  1.33E-06 -5.80E-05  2.23E-04  9.67E-05  2.89E-05  5.72E-05
         -4.36E-05 -8.09E-05  5.51E-05  4.11E-04  9.99E-05  3.04E-05  5.56E-05 -3.29E-05 -7.16E-05  7.85E-05  3.61E-04  2.41E-05
         1.66E-06 -4.86E-06 -4.79E-05  1.62E-05  1.40E-04  3.85E-05  4.50E-05 -5.96E-05 -3.99E-06  1.47E-04  2.39E-05  3.63E-05
          2.10E-05 -6.32E-05  1.70E-04 -5.39E-05 -2.76E-04  1.57E-04  3.07E-04  1.54E-03
 
 SG11
+        5.41E-07 -1.80E-06 -2.98E-07 -1.27E-06 -1.36E-06 -1.37E-06 -7.30E-07 -1.40E-06 -1.49E-07  1.01E-06  1.45E-07  4.14E-07
          6.52E-07  5.31E-08 -1.02E-06 -1.92E-07 -1.18E-06 -2.85E-07  3.08E-07  2.58E-07 -7.75E-07  3.31E-07 -2.78E-07 -9.74E-07
        -1.21E-07 -5.38E-08 -4.50E-07  7.88E-07 -2.19E-07 -6.13E-07 -4.85E-07  1.49E-07  5.39E-07  2.90E-08  1.08E-06 -2.08E-07
         -1.53E-07  1.09E-07 -2.91E-07  1.02E-07  5.99E-07 -2.46E-07  1.76E-07 -4.26E-07  4.12E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        6.26E-06  8.31E-07 -1.77E-06 -4.08E-06 -3.22E-07 -3.01E-07 -3.02E-06 -1.20E-06  5.77E-06 -3.11E-07  1.06E-06 -6.12E-07
          1.25E-06 -5.07E-07 -1.09E-06  9.52E-07 -2.10E-06  1.20E-07 -8.56E-07 -9.77E-07 -2.42E-06  1.04E-06  3.48E-07  3.49E-07
        -5.44E-07 -2.09E-07  2.59E-07 -6.39E-07 -6.63E-07 -1.09E-06  1.39E-06 -1.66E-06 -1.64E-06  1.69E-07 -1.88E-07  8.08E-07
         -2.74E-07 -2.09E-07 -5.62E-06 -4.38E-08  2.47E-07  9.62E-07 -5.63E-07  8.82E-07  1.61E-08  0.00E+00  1.48E-06
 
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
+        7.38E-02
 
 TH 2
+       -1.68E-01  7.15E-02
 
 TH 3
+        1.14E-01 -1.91E-02  5.46E-02
 
 TH 4
+        7.47E-03  1.56E-01  3.77E-02  7.32E-02
 
 TH 5
+        9.89E-02  4.92E-02  7.93E-03 -6.22E-02  6.31E-02
 
 TH 6
+       -2.77E-02  4.67E-03  8.53E-02  4.16E-02 -1.84E-01  7.38E-02
 
 TH 7
+        3.46E-02 -1.94E-01  1.42E-01 -2.51E-01  1.48E-01  8.60E-02  7.00E-02
 
 TH 8
+        2.72E-01  2.08E-01  1.89E-01  1.00E-01  1.70E-02 -1.95E-01  2.30E-01  6.15E-02
 
 OM11
+       -7.68E-03  6.56E-03 -2.48E-02 -5.15E-02 -4.13E-03 -2.85E-02 -1.67E-02 -2.78E-02  5.72E-02
 
 OM12
+       -5.33E-03  8.36E-02 -3.33E-02 -2.01E-02  3.62E-02 -4.22E-02 -3.76E-02 -2.97E-02 -2.09E-01  3.05E-02
 
 OM13
+        1.92E-02  4.95E-03  7.82E-02  1.40E-02 -1.66E-02  2.91E-03  2.01E-02 -2.51E-02  1.07E-01 -4.11E-02  2.16E-02
 
 OM14
+        1.68E-02 -1.82E-02  1.49E-02  1.14E-02  2.86E-02 -2.91E-02  3.09E-02 -9.18E-03  1.15E-01  3.89E-02 -1.18E-02  3.14E-02
 
 OM15
+        1.99E-02 -3.27E-02  1.54E-02 -3.71E-02  4.08E-02 -7.30E-02 -3.99E-02  3.89E-02  9.39E-02  4.28E-02  2.20E-02 -9.56E-02
          2.82E-02
 
 OM16
+       -3.78E-02  1.84E-02 -5.80E-03 -3.38E-02 -2.68E-02  2.61E-03 -1.08E-02 -6.25E-03  1.40E-02  1.29E-02  6.31E-02  8.34E-02
         -1.09E-01  3.14E-02
 
 OM17
+        1.33E-02  1.64E-02  7.79E-03  1.11E-02 -2.13E-02  1.20E-02  3.49E-02  2.83E-02 -2.05E-02 -1.67E-01  2.42E-02 -1.13E-01
          3.34E-02  4.12E-02  2.96E-02
 
 OM18
+       -1.36E-03 -1.53E-02 -1.16E-02 -7.05E-02 -1.60E-02  1.35E-03  1.57E-02 -3.21E-02  3.64E-01  1.01E-01  1.36E-01  1.38E-01
         -2.98E-02 -1.15E-01  1.83E-01  2.90E-02
 
 OM22
+       -7.13E-03 -2.27E-01  4.74E-02  3.61E-03 -1.21E-02  4.12E-02  5.53E-02  4.89E-02  2.39E-02 -2.97E-01  4.12E-02 -8.00E-03
         -3.33E-02 -1.42E-02  9.59E-03 -1.81E-02  5.03E-02
 
 OM23
+       -2.31E-02  8.72E-02  3.67E-02  4.14E-02 -7.68E-03  2.21E-02 -3.09E-02 -3.34E-03 -3.65E-02  9.72E-02 -5.39E-02 -1.01E-02
         -3.29E-02 -2.69E-02 -2.80E-02 -2.16E-02 -1.02E-01  1.92E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        2.27E-02 -1.54E-01 -2.40E-02 -1.42E-02 -2.25E-02  4.10E-02 -2.38E-02  1.51E-02 -4.37E-02 -5.78E-02  1.28E-02 -8.65E-02
         -1.19E-02 -2.48E-02 -8.77E-04  4.63E-02  2.41E-01 -1.90E-02  2.78E-02
 
 OM25
+       -8.49E-03 -2.02E-02 -5.39E-03 -1.96E-02  1.27E-02  4.09E-02  5.72E-02  2.63E-02 -2.60E-02  3.89E-02 -5.10E-02  3.64E-02
         -1.74E-01  3.53E-02  2.07E-02  1.88E-02  1.37E-01 -4.02E-02 -2.29E-02  2.42E-02
 
 OM26
+       -2.08E-02  4.80E-02  1.27E-02  2.76E-02  1.47E-02 -1.29E-03 -4.63E-02 -1.62E-02  7.60E-03  2.66E-02 -3.84E-02 -3.09E-02
         -1.38E-03 -1.23E-01 -9.70E-03  2.12E-02  1.55E-02  5.65E-02 -2.72E-03 -9.95E-02  2.80E-02
 
 OM27
+        2.20E-02  2.12E-01  2.02E-02  2.37E-02  5.70E-02 -5.98E-02 -1.68E-03 -3.22E-02  1.52E-02  1.05E-01 -6.04E-02  6.85E-03
          2.40E-02  1.00E-02 -9.65E-02 -5.55E-02 -3.76E-01  1.20E-01 -2.88E-01  6.38E-02  9.37E-02  2.91E-02
 
 OM28
+        6.06E-03 -1.01E-02  4.47E-02 -3.55E-02 -2.23E-02  3.35E-03  1.68E-02  6.62E-03 -4.47E-02  1.23E-01  2.68E-03 -1.29E-02
          3.15E-02 -9.76E-03 -6.06E-02 -3.40E-02  3.66E-01  7.97E-02  1.33E-01 -1.83E-02 -1.76E-01  8.17E-02  2.49E-02
 
 OM33
+       -1.87E-02 -1.57E-02 -1.35E-01 -7.93E-02 -2.69E-02  5.39E-02  2.34E-02 -2.03E-02 -2.57E-03 -1.60E-02  1.18E-01  1.39E-02
         -6.29E-02  4.07E-02  1.35E-02  7.48E-03  2.24E-02 -7.92E-04 -1.26E-02  2.96E-02 -4.21E-02 -3.50E-02 -1.39E-02  3.02E-02
 
 OM34
+       -1.08E-02  1.72E-02  1.24E-01  1.15E-01 -1.76E-02 -4.66E-02 -2.14E-02  4.36E-02 -4.33E-02 -1.22E-02  3.66E-02  9.97E-02
          1.15E-02 -2.05E-02 -1.55E-02 -1.26E-02  5.08E-02  6.68E-02  5.91E-03 -3.69E-02 -1.57E-02 -1.63E-02  4.98E-02 -1.02E-01
         2.35E-02
 
 OM35
+       -2.58E-02 -7.52E-03 -2.85E-03 -6.32E-03 -5.54E-02  5.14E-03  1.07E-02  1.97E-02  6.45E-03 -1.99E-02  4.23E-02 -3.55E-04
         -3.29E-03  3.53E-02  3.12E-03 -2.69E-02  6.84E-03  4.98E-02 -4.22E-02  1.36E-02  7.69E-02  5.86E-02  7.62E-03  4.79E-02
        -1.15E-01  1.92E-02
 
 OM36
+       -4.65E-02 -1.85E-02 -5.32E-02 -3.87E-02 -1.36E-02 -1.34E-02 -1.74E-02 -2.63E-02 -3.65E-02  2.80E-02 -1.52E-02 -3.36E-03
         -2.21E-02  1.06E-01  1.12E-02 -1.16E-02  3.26E-02  3.08E-02  8.04E-02  7.70E-03 -1.89E-02 -3.50E-02  1.12E-03  9.92E-02
         7.18E-02 -1.14E-01  2.12E-02
 
 OM37
+        1.49E-02  2.26E-02 -9.32E-02 -5.56E-02 -9.53E-02 -2.99E-03 -4.57E-02 -1.72E-02  4.98E-02  5.21E-03 -3.05E-02  2.58E-02
          1.63E-02 -2.42E-03  1.26E-01  2.93E-02 -3.41E-02 -1.90E-01 -2.38E-02  1.89E-02  8.83E-03  1.11E-02 -3.61E-02  1.11E-01
        -1.61E-01  1.20E-01  1.45E-02  2.01E-02
 
 OM38
+       -1.70E-02 -3.37E-02  5.79E-02  4.96E-02 -1.93E-02 -2.48E-02  3.14E-03  2.42E-02  8.24E-02 -3.67E-02  2.56E-01  5.35E-02
         -7.01E-03  1.48E-02  1.52E-02  9.25E-02  5.94E-02  2.24E-01 -1.20E-02 -5.25E-03 -3.78E-02 -1.53E-02  4.71E-02  2.69E-01
         1.01E-01 -4.76E-02 -1.42E-01  1.62E-01  1.93E-02
 
 OM44
+       -7.16E-03  2.09E-02  1.55E-01  9.90E-02 -1.27E-02  3.38E-02 -1.25E-03  3.51E-02 -1.65E-02  5.74E-04 -1.09E-02  7.68E-02
          1.53E-02 -4.38E-03  4.82E-02 -1.68E-02  2.55E-02  3.47E-02  1.15E-01 -3.01E-02  7.02E-02 -2.34E-02  2.30E-02 -6.61E-02
         6.33E-02 -3.85E-02  7.15E-04 -1.76E-02 -3.29E-02  5.68E-02
 
 OM45
+       -3.67E-02  3.07E-02  1.16E-02 -8.82E-02  4.82E-02  5.50E-02  5.98E-02 -4.19E-03  1.96E-03  2.24E-02  2.06E-02  5.49E-02
          3.20E-02  6.58E-02 -2.22E-02  2.69E-02 -3.73E-02 -2.05E-02  4.27E-02  6.50E-02 -5.48E-02  4.06E-02  1.61E-02  1.68E-02
        -2.94E-02 -1.18E-02  1.58E-02 -3.79E-02  5.06E-02 -1.40E-01  2.78E-02
 
 OM46
+        2.12E-02 -1.88E-02  5.49E-02  1.43E-03 -1.57E-04  9.42E-02  3.92E-02  8.99E-03 -3.67E-02 -4.37E-03  4.08E-02  1.09E-03
         -4.61E-02  9.11E-02  2.17E-02 -1.74E-02  1.00E-02  1.94E-02  2.85E-03  3.28E-03  7.37E-02 -2.39E-02 -1.71E-02  3.74E-03
        -4.18E-02  5.05E-02  5.50E-03  1.77E-02 -2.30E-02  5.28E-02 -7.94E-02  3.08E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -7.56E-04  3.86E-03 -9.92E-03 -9.88E-03 -2.57E-02  1.83E-02  7.26E-02 -2.84E-03  1.85E-02 -3.40E-02  2.73E-03 -3.21E-02
          3.42E-02  4.59E-02  9.02E-02  2.77E-03 -1.40E-02 -4.38E-02 -1.78E-01  1.69E-02 -5.70E-03  1.00E-01 -1.49E-02 -2.65E-03
         1.62E-02  3.09E-02 -4.69E-02  4.43E-03 -2.31E-02 -2.25E-01  7.66E-02  2.07E-02  3.05E-02
 
 OM48
+        3.51E-03 -1.17E-02 -2.00E-02  3.23E-02 -5.00E-02  3.45E-02 -1.48E-02  5.99E-03  3.23E-02  7.49E-03 -1.41E-02  2.09E-01
         -1.87E-02  1.21E-02  3.29E-02  1.01E-01  5.67E-02 -2.01E-02  2.31E-01  2.91E-02  4.57E-03 -3.77E-02  1.16E-01 -1.93E-02
         1.19E-01 -2.52E-02 -9.40E-03 -7.07E-02 -3.47E-02  2.12E-01 -5.54E-02 -1.24E-01  1.29E-01  2.77E-02
 
 OM55
+        3.65E-02 -4.84E-03  1.57E-02  2.26E-02 -4.10E-02  4.78E-03 -3.31E-02  2.20E-02  9.29E-03  1.51E-02  3.16E-02 -2.92E-02
          1.54E-01 -7.50E-02  2.28E-02  1.61E-02  2.64E-02 -4.89E-02  1.86E-02  1.09E-01  1.13E-02 -3.61E-02  9.24E-03  5.98E-03
         1.11E-02  5.91E-03 -1.21E-02  2.13E-03  1.62E-06  4.08E-05 -1.15E-01 -1.49E-02  2.08E-02  2.73E-02  4.27E-02
 
 OM56
+       -1.57E-02 -2.23E-02 -2.29E-02 -1.32E-02 -7.15E-02 -1.90E-02  5.64E-03 -1.46E-02 -1.91E-02  6.19E-03 -1.25E-02  3.95E-02
         -6.87E-02  5.38E-02  2.54E-02  3.95E-02 -2.52E-02  2.59E-02 -5.93E-02 -3.02E-03  5.68E-02  4.77E-02  7.85E-04  2.07E-02
         4.69E-02  6.99E-02 -1.94E-02  1.97E-04  1.47E-02  1.90E-02  7.98E-02 -8.35E-02  2.39E-02  6.29E-03 -1.60E-01  2.80E-02
 
 OM57
+       -6.14E-03 -2.64E-02 -4.43E-02  1.36E-02  2.72E-02 -6.79E-03  5.43E-03 -1.59E-02  2.46E-02 -1.91E-02 -9.76E-03  9.63E-03
         -5.31E-03 -3.69E-02  1.04E-01  6.45E-03 -1.63E-02  3.91E-04  2.50E-02 -5.98E-02  3.62E-02  3.64E-03  4.65E-03 -3.18E-02
        -2.42E-03  5.81E-02 -1.83E-02  5.49E-02  2.53E-02  3.64E-02 -1.41E-01 -1.69E-02 -1.46E-01 -2.31E-02  1.13E-01  3.42E-04
          2.53E-02
 
 OM58
+       -2.75E-02  1.66E-03 -5.66E-03 -4.06E-02  3.04E-03 -1.71E-02  4.34E-02  3.41E-02  2.75E-02 -8.44E-03  1.20E-02 -1.51E-02
          1.94E-01  5.00E-03  2.38E-02  2.68E-02  1.32E-02 -2.02E-02  5.05E-03  2.07E-01 -1.56E-02  4.54E-02  2.10E-02 -2.17E-02
        -3.81E-02  1.16E-01  8.10E-03  3.70E-02 -3.72E-02  3.71E-02  7.38E-02  2.95E-02 -2.81E-02  7.48E-03 -4.58E-02 -1.70E-01
          2.34E-01  2.24E-02
 
 OM66
+       -1.34E-03  1.51E-02  1.15E-02  2.31E-02  3.21E-02  1.67E-02  2.99E-02  2.69E-02  2.43E-02  2.14E-02  2.40E-02 -3.77E-02
         -2.01E-02  8.75E-02  2.16E-02  1.51E-02  4.51E-02 -4.24E-03  5.21E-03 -1.85E-02  3.44E-02 -8.78E-03  4.37E-02  4.90E-02
        -2.22E-02  3.70E-02  1.00E-01  2.17E-02  3.98E-02  1.53E-02 -1.61E-02  1.08E-01 -3.21E-02 -5.52E-02  5.77E-03 -2.26E-01
         -3.09E-02 -6.53E-03  5.65E-02
 
 OM67
+        3.23E-02 -5.74E-03 -2.57E-02 -1.28E-02 -8.48E-02 -2.83E-03 -7.00E-03  1.05E-02  1.66E-02  5.16E-03 -3.57E-03 -1.76E-02
          2.50E-03  1.66E-02  3.21E-02  1.89E-02 -4.31E-02  3.19E-02  3.55E-03 -1.50E-02 -1.31E-01 -3.94E-03  3.34E-03  3.39E-02
         1.88E-02 -2.67E-02  1.12E-02  5.10E-02  1.63E-02 -2.21E-02 -1.98E-03 -1.15E-01  4.87E-02  5.34E-02  5.04E-02  7.02E-02
         -1.33E-01 -2.78E-02  6.58E-02  2.90E-02
 
 OM68
+        6.28E-05 -2.63E-02 -3.06E-03 -1.93E-02  8.53E-03  2.95E-03 -2.50E-02  9.99E-03  1.26E-02  4.62E-03 -3.57E-02  8.08E-03
          1.18E-02  1.78E-01  3.05E-02 -2.24E-02 -2.75E-02 -1.89E-02  1.26E-02  1.69E-02  2.36E-01 -1.89E-02 -1.33E-01  2.57E-03
        -2.12E-02 -1.82E-02  9.15E-02  3.57E-02 -4.42E-02  4.13E-02 -3.59E-02  8.36E-02  4.06E-02  6.65E-02 -1.90E-02  4.94E-04
         -2.87E-02 -5.36E-02 -2.73E-01  1.76E-01  2.69E-02
 
 OM77
+        7.56E-03 -3.53E-02 -2.55E-02 -2.22E-02  9.73E-03  1.94E-02 -1.37E-02 -3.50E-02  1.97E-02  2.07E-02  5.58E-02  3.09E-02
         -2.10E-02 -2.65E-03 -2.81E-02  4.87E-02  2.47E-02 -3.30E-04  5.19E-02 -2.75E-02 -5.46E-02 -2.95E-01 -3.49E-02  4.20E-02
        -3.79E-03 -1.77E-02  8.17E-02  9.68E-02  6.91E-02  2.94E-02 -2.87E-02  1.26E-03 -2.76E-01 -5.54E-02  3.18E-04 -9.06E-03
          1.42E-01  8.75E-03  5.06E-02  7.87E-02 -4.45E-03  4.77E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+       -5.65E-03 -4.61E-02  2.31E-02 -1.75E-02  2.88E-02  4.55E-03  4.77E-02 -1.94E-02 -7.12E-03 -1.58E-02  2.82E-02 -1.29E-02
         -2.12E-02 -1.29E-02  2.09E-01  7.07E-02 -7.14E-02 -3.02E-02 -8.78E-02  6.36E-02 -4.01E-03  9.59E-02 -1.11E-01  3.20E-02
        -3.29E-02  2.02E-02  7.66E-05  1.87E-01  8.94E-02  1.19E-02 -2.45E-04 -8.23E-03  3.64E-02 -1.50E-01 -1.40E-02  1.74E-02
          1.59E-02  2.73E-02  4.37E-03 -1.19E-01 -6.15E-03  3.13E-01  2.57E-02
 
 OM88
+        7.62E-03 -2.10E-03  1.63E-02 -1.95E-02  1.92E-02  8.22E-03  4.84E-04 -2.41E-02  9.97E-02  8.08E-02  3.41E-02  4.64E-02
         -3.95E-02 -6.57E-02  4.76E-02  3.62E-01  5.07E-02  4.04E-02  5.10E-02 -3.48E-02 -6.52E-02  6.88E-02  3.71E-01  2.04E-02
         1.80E-03 -6.46E-03 -5.76E-02  2.06E-02  1.85E-01  1.73E-02  4.13E-02 -4.94E-02 -3.33E-03  1.35E-01  1.43E-02  3.32E-02
          2.12E-02 -7.19E-02  7.68E-02 -4.75E-02 -2.62E-01  8.39E-02  3.05E-01  3.92E-02
 
 SG11
+        1.14E-02 -3.92E-02 -8.49E-03 -2.71E-02 -3.35E-02 -2.90E-02 -1.62E-02 -3.54E-02 -4.06E-03  5.14E-02  1.04E-02  2.05E-02
          3.60E-02  2.63E-03 -5.39E-02 -1.03E-02 -3.66E-02 -2.31E-02  1.72E-02  1.66E-02 -4.30E-02  1.77E-02 -1.74E-02 -5.02E-02
        -8.01E-03 -4.36E-03 -3.30E-02  6.10E-02 -1.77E-02 -1.68E-02 -2.72E-02  7.56E-03  2.74E-02  1.63E-03  3.95E-02 -1.16E-02
         -9.44E-03  7.57E-03 -8.01E-03  5.51E-03  3.47E-02 -8.01E-03  1.07E-02 -1.69E-02  6.42E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        6.98E-02  9.56E-03 -2.66E-02 -4.58E-02 -4.20E-03 -3.35E-03 -3.55E-02 -1.60E-02  8.31E-02 -8.38E-03  4.02E-02 -1.60E-02
          3.65E-02 -1.33E-02 -3.02E-02  2.70E-02 -3.44E-02  5.13E-03 -2.53E-02 -3.32E-02 -7.10E-02  2.95E-02  1.15E-02  9.50E-03
        -1.91E-02 -8.95E-03  1.01E-02 -2.61E-02 -2.83E-02 -1.58E-02  4.10E-02 -4.44E-02 -4.42E-02  5.00E-03 -3.62E-03  2.38E-02
         -8.89E-03 -7.66E-03 -8.18E-02 -1.24E-03  7.55E-03  1.66E-02 -1.81E-02  1.85E-02  2.06E-02  0.00E+00  1.22E-03
 
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
+        2.29E+02
 
 TH 2
+        7.66E+01  2.85E+02
 
 TH 3
+       -1.74E+01  1.37E+01  3.93E+02
 
 TH 4
+        3.25E+00 -1.37E+01  1.61E+00  2.22E+02
 
 TH 5
+       -3.96E+01 -4.36E+01 -1.01E+00  3.33E+00  2.94E+02
 
 TH 6
+       -2.10E+01 -3.68E+01 -3.61E+01 -2.29E+01  5.81E+01  2.23E+02
 
 TH 7
+        3.63E+01  8.00E+01 -2.20E+01  6.70E+01 -5.77E+01 -5.17E+01  2.82E+02
 
 TH 8
+       -1.08E+02 -1.30E+02 -6.10E+01 -4.37E+01  4.83E+01  9.03E+01 -1.22E+02  4.07E+02
 
 OM11
+        2.93E-01 -9.69E+00  8.64E+00  7.26E+00  2.60E-01  7.74E+00  3.84E+00  7.74E+00  4.17E+02
 
 OM12
+       -2.63E+00 -9.96E+00  1.60E+01 -6.62E+00 -2.61E+01  2.07E+01  4.13E+00  1.22E+01  2.40E+02  1.55E+03
 
 OM13
+       -4.41E+01 -6.46E+01 -7.16E+01 -2.04E+01  1.89E+01  2.00E+01 -3.71E+01  8.52E+01 -4.12E+01  2.67E+01  2.52E+03
 
 OM14
+       -1.74E+01  9.26E+00 -6.98E+00 -1.71E+01 -1.99E+01  2.24E+01 -2.20E+01  1.72E+01 -4.43E+01 -1.44E+01  9.26E+01  1.21E+03
 
 OM15
+        1.64E+01  5.82E+01 -1.03E+00  2.98E+01 -3.97E+01  5.69E+00  5.13E+01 -5.48E+01 -1.18E+02 -1.82E+02 -3.12E+01  9.41E+01
          1.59E+03
 
 OM16
+        1.44E+01 -8.34E+00 -7.99E-01  2.81E+01  2.11E+01  2.75E+00  2.23E+01 -2.84E+00 -6.20E+01 -1.16E+02 -1.35E+02 -1.19E+02
          1.84E+02  1.24E+03
 
 OM17
+       -2.52E+01 -4.74E+01 -4.28E+00 -2.11E+01 -5.43E-02  1.79E+01 -2.74E+01  2.05E+01  1.24E+02  3.70E+02  2.15E+01  2.05E+02
         -1.56E+02 -1.52E+02  1.52E+03
 
 OM18
+        6.67E+00  5.55E+00 -3.80E+00  4.30E+01  2.84E+01 -5.95E+00 -5.08E+00  5.66E+00 -3.55E+02 -4.59E+02 -2.76E+02 -2.07E+02
          1.87E+02  3.05E+02 -4.92E+02  2.01E+03
 
 OM22
+        1.95E+01  6.97E+01 -5.26E+00 -1.87E+01 -2.83E+01 -1.12E+01  5.34E+00 -3.61E+01  1.59E+01  4.15E+02 -2.15E+00 -1.27E+01
         -2.82E+01 -3.47E+01  1.02E+02 -9.20E+01  7.90E+02
 
 OM23
+       -8.47E+00 -6.27E+01  9.82E-01  9.66E+00  1.61E+01 -3.49E+01  1.42E+01  1.56E+01 -3.29E+00 -1.29E+02  4.12E+02  6.41E+00
          6.88E+01  6.52E+01 -8.66E+01  3.96E+01  1.56E+02  3.42E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -8.43E+00  6.84E+01  3.36E+01  2.84E+00 -1.45E+01 -1.35E+01  3.01E+01 -3.36E+01  4.81E+01  8.57E+01 -1.60E+01  2.42E+02
          1.70E+01 -1.67E+01  1.01E+02 -1.40E+02 -7.18E+01 -5.57E+01  1.73E+03
 
 OM25
+        1.08E+01  1.33E+01  9.97E+00  1.27E+01 -1.97E+01 -3.11E+01 -1.53E+00 -2.84E+01 -2.35E+01 -2.86E+02  1.32E+02 -1.15E+01
          5.06E+02  6.12E+01 -1.31E+02  5.11E+01 -3.33E+02  3.16E+01  4.06E+01  2.25E+03
 
 OM26
+        1.05E+00 -3.36E+01 -9.87E+00  1.41E+01  1.38E+01  4.47E+00  1.77E+01  2.42E+01 -2.06E+01 -2.01E+02  2.49E+00  2.84E+01
          1.06E+02  3.18E+02 -4.85E+01  8.29E+01 -2.58E+02 -1.87E+02 -6.66E+01  3.58E+02  1.75E+03
 
 OM27
+       -6.23E+01 -1.14E+02 -2.55E+01 -3.41E+01 -2.60E+01  4.79E+01 -4.94E+01  9.74E+01 -1.19E+01  2.36E+02  8.26E+01  4.64E+01
         -1.15E+02 -8.67E+01  3.53E+02 -3.96E+01  5.29E+02 -1.35E+02  3.90E+02 -3.85E+02 -3.74E+02  2.11E+03
 
 OM28
+       -7.13E+00 -4.76E+01 -3.51E+01  5.25E+01  6.08E+01  1.00E+01 -1.72E+01  2.89E+01 -2.93E+01 -6.41E+02 -8.42E+01  8.75E+00
          1.83E+01  1.26E+02 -1.92E+02  5.59E+02 -8.14E+02 -3.06E+02 -2.10E+02  3.92E+02  6.54E+02 -8.00E+02  3.16E+03
 
 OM33
+       -2.08E+00 -5.72E+00  1.01E+02  4.42E+01  1.72E+00 -3.98E+01 -3.66E+00 -1.38E+01  1.98E+01 -3.51E+00 -1.01E+02 -2.16E+01
          7.40E+01  1.70E+01 -1.30E+01  4.23E+01 -3.29E+00  1.28E+02  3.39E+01 -1.77E+00  4.23E+01  1.34E+01  2.48E+01  1.31E+03
 
 OM34
+        9.03E+00 -1.86E+01 -8.17E+01 -4.74E+01  2.00E+01  3.10E+01  7.27E+00 -2.90E+00  4.46E+01  2.38E+01 -1.59E+01 -1.36E+02
         -3.50E+01  4.20E+01  5.76E+00 -1.02E-01 -2.66E+01  1.50E+01  4.05E+01  5.74E+01  1.47E+00  1.97E+01 -7.49E+01  1.86E+02
         2.08E+03
 
 OM35
+        4.11E+01  4.41E+01 -2.35E+01 -1.07E+01  3.27E+01  6.30E+00  5.34E+00 -4.56E+01  7.07E-01  3.65E+01 -2.44E+02 -3.34E+01
          4.58E+01 -6.44E+01  2.21E+01  7.70E+01 -4.48E+01 -4.05E+02  2.89E+01  5.22E+01 -1.07E+02 -1.05E+02  2.08E+01 -1.36E+02
         1.76E+02  3.05E+03
 
 OM36
+        3.85E+01  2.83E+01  1.80E+01  1.38E+01  1.70E+01  2.17E+01  1.14E+01 -2.80E+00  2.10E+01 -4.74E+01 -9.96E+01 -1.34E+01
          3.27E+01 -1.16E+02 -3.61E+01 -1.50E+01 -7.41E+01 -3.51E+02 -1.57E+02  4.31E+01  1.16E+02 -1.13E+02  8.08E+01 -2.76E+02
        -2.69E+02  4.19E+02  2.59E+03
 
 OM37
+       -4.69E+01 -7.33E+01  7.45E+01  5.32E+01  9.53E+01 -2.64E+00  3.28E+01  2.21E+01 -4.98E+01 -9.49E+01  3.43E+02 -1.15E+02
         -2.99E+01  7.64E+01 -2.30E+02  4.61E+01  4.47E+01  9.06E+02 -5.60E+01 -1.34E+01 -6.48E+01 -2.78E+01 -1.18E+02 -1.59E+01
         3.81E+02 -4.61E+02 -2.52E+02  3.19E+03
 
 OM38
+        5.64E+01  8.78E+01 -8.73E+01 -6.71E+01  7.70E-01  4.66E+01  5.77E+00 -5.06E+01 -3.97E+01  7.01E+01 -8.44E+02 -6.21E+01
         -5.04E+01 -5.96E+01  5.23E+01  1.30E+01 -1.19E+02 -1.24E+03  1.92E+01 -3.82E+01  1.04E+02 -4.03E+01  1.98E+02 -6.63E+02
        -4.24E+02  5.10E+02  7.51E+02 -9.59E+02  4.09E+03
 
 OM44
+        7.28E+00 -8.07E+00 -5.80E+01 -2.19E+01  8.34E-01 -3.78E+00 -9.29E+00  3.98E+00 -5.20E+00 -1.26E+01 -2.78E+00 -3.21E+01
         -1.87E+01  1.83E+00 -4.20E+01  4.52E+01 -6.09E+00 -3.71E+01 -3.49E+01  3.35E+01 -2.59E+01  5.52E-01  9.13E+00  1.58E+01
        -8.51E+00  5.64E+01  1.62E+01 -2.24E+01  5.06E+01  3.81E+02
 
 OM45
+        2.13E+01 -2.20E+01 -1.69E+01  3.64E+01 -3.13E+01 -4.88E+01 -1.30E+01 -8.37E+00  1.23E+01 -6.97E+00 -5.75E+00 -1.35E+02
         -6.82E+01 -5.90E+01 -3.20E+01  1.94E+01  5.21E+01  1.18E+02 -2.06E+02 -8.47E+01  2.21E+01 -5.62E+01  3.68E+00  3.11E+01
         5.33E+01  1.98E+01 -4.09E+01  1.19E+02 -1.77E+02  8.35E+01  1.50E+03
 
 OM46
+       -6.30E+00  1.53E+01 -1.41E+01  3.45E+00 -1.07E+00 -4.62E+01 -6.53E+00 -1.16E+01  2.55E+01  1.34E+01 -8.81E+01 -6.03E+01
          6.11E+01 -6.73E+01 -4.54E+01 -5.70E+00  2.05E+01 -6.92E+01 -6.20E+01  7.44E+00 -3.08E+01 -5.08E+00 -1.51E+01 -2.09E+00
         1.69E+01 -4.51E+01  5.25E+01 -4.66E+01  7.70E+01 -4.46E+01  1.29E+02  1.19E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -9.97E+00 -8.39E+00 -2.37E-01 -1.56E+01  1.18E+01 -1.14E+00 -4.72E+01  2.45E+01 -1.59E+01  1.59E+01 -5.73E+00  1.04E+02
         -4.90E+01 -5.21E+01 -5.38E+01 -3.18E+00 -2.99E+01  2.92E+01  3.15E+02  5.30E+01 -7.52E+00  5.66E+01 -1.43E+01  1.13E+01
        -3.59E+01 -3.41E+01  1.96E+01 -3.00E+01  3.80E+01  1.84E+02 -1.07E+02 -1.03E+02  1.42E+03
 
 OM48
+        4.42E+00  4.12E+00  4.73E+01  1.43E+00  2.55E+01 -2.88E+01  7.68E+00 -1.51E+01 -1.67E+00 -7.91E+00 -1.40E+01 -3.56E+02
          2.83E+01  7.84E+00 -1.46E+02  2.06E+01  3.50E+01  7.85E+01 -4.68E+02 -8.97E+01 -4.85E+00 -8.36E+01 -2.39E+01 -2.46E+01
        -1.77E+02 -6.63E+00  7.95E+01  1.11E+02  1.13E+02 -1.84E+02  1.70E+02  2.50E+02 -4.00E+02  1.83E+03
 
 OM55
+       -1.11E+01 -9.30E+00 -9.11E+00 -3.09E+00  2.22E+01  3.28E-02  6.18E+00 -1.82E+00  1.04E+01  1.10E+01 -3.56E+01  7.07E+00
         -2.20E+02  6.86E+00  2.03E+01 -3.52E+01  2.24E+01  7.24E+01 -1.57E+00 -2.58E+02 -8.28E+01  6.90E+01 -4.68E+01 -2.27E+01
        -8.68E+00 -5.28E+01 -2.56E+01  4.37E+01 -5.31E+00 -2.38E+00  7.69E+01 -1.00E+00 -3.01E+01 -1.04E+01  6.47E+02
 
 OM56
+        2.90E+00  2.29E+01  2.18E+01  3.81E-01  4.21E+01  1.56E+01 -8.94E+00 -8.97E+00  3.16E+01  3.46E+01  3.20E+01  5.77E+00
         -5.28E+01 -1.67E+02 -1.83E+00 -1.40E+02  2.62E+01  2.46E+01  1.02E+02 -1.59E+02 -2.47E+02  4.05E+00 -1.10E+02 -4.12E+01
        -8.42E+01 -2.27E+02 -5.05E+01  3.21E+01 -1.25E+01 -5.09E+01 -1.37E+02  4.58E+01 -1.11E+01  1.34E+01  2.00E+02  1.59E+03
 
 OM57
+        1.40E+01  3.98E+01  4.28E+01 -3.96E+00 -3.09E+01 -2.18E+01 -7.19E+00 -7.90E+00 -4.26E+01 -5.56E+01  3.60E+01 -6.15E+01
          2.30E+02  7.36E+01 -3.03E+02  1.53E+02 -1.02E+01  2.71E+01 -7.86E+01  3.35E+02  5.65E+01 -2.12E+02  9.37E+01  7.51E+01
        -2.71E+01 -4.27E+01  5.90E+01 -7.10E+01 -1.07E+02  1.89E+01  2.57E+02  9.34E+01  1.24E+02  9.69E+01 -2.07E+02 -1.77E+02
          2.00E+03
 
 OM58
+        1.28E+01 -1.24E+01  8.66E+00  8.07E+00  2.57E+01  2.28E+01 -2.22E+01 -1.82E+01  3.58E+01  1.36E+02 -2.98E+01  6.12E+01
         -5.79E+02 -1.39E+02  1.39E+02 -2.82E+02  7.14E+01  2.50E+01  5.44E+01 -7.34E+02 -1.78E+02  7.35E+01 -2.89E+02 -5.29E+00
         3.17E+01 -3.45E+02 -1.02E+02 -3.71E+00  6.29E+01 -5.96E+01 -2.19E+02 -1.01E+02  3.25E+01 -1.13E+02  2.57E+02  5.06E+02
         -7.32E+02  2.75E+03
 
 OM66
+        1.58E+00  3.34E+00  1.87E+00 -9.30E+00 -1.52E+01 -4.79E+00 -6.63E+00 -1.25E+01 -1.36E+01 -2.01E+01  2.67E+01  3.47E+01
         -1.37E+01 -1.30E+02 -1.38E+01 -3.28E+01 -6.44E+00  5.55E+01  2.37E+01 -1.95E+01 -1.59E+02  6.59E+00 -4.32E+01 -2.01E+01
         1.48E+01 -7.44E+01 -1.46E+02  1.23E+01 -6.49E+01 -1.44E+01 -4.06E+00 -8.27E+01  1.72E+01  8.79E+00  3.02E+01  2.30E+02
         -4.33E+00  1.03E+02  4.18E+02
 
 OM67
+       -1.31E+01  1.28E+01  1.08E+01  7.79E+00  4.89E+01  1.38E+00 -5.59E+00 -1.04E+00 -9.48E+00 -2.53E+01 -2.15E+01  7.84E+00
          9.10E+01  1.21E+02 -1.37E+02  5.37E+01 -9.84E+00 -1.40E+02 -5.17E+01  1.35E+02  3.68E+02 -1.83E+02  1.32E+02 -1.54E+01
        -4.95E+01  7.69E+01  1.21E+02 -1.59E+02  5.65E+01  7.51E+00  4.69E+01  2.01E+02 -1.02E+02  3.56E+01 -1.12E+02 -2.33E+02
          3.28E+02 -2.08E+02 -1.60E+02  1.52E+03
 
 OM68
+        1.02E+01  3.11E+01 -5.80E+00  9.71E-01 -4.94E+01 -1.28E+01  2.27E+01 -4.05E+01 -9.29E+00  2.73E+01  1.30E+02  5.93E+01
         -1.28E+02 -4.32E+02  5.51E+01 -2.30E+02  7.33E+01  1.17E+02  4.31E+01 -1.73E+02 -6.67E+02  1.39E+02 -2.38E+02 -1.44E+01
         7.40E+01 -2.14E+01 -2.90E+02  1.15E+01 -1.46E+02 -2.69E+01  5.30E+00 -1.96E+02  5.24E+00 -2.08E+02  9.15E+01  2.90E+02
         -1.21E+02  3.74E+02  3.60E+02 -5.39E+02  2.18E+03
 
 OM77
+       -2.05E+01 -3.21E+01  3.48E+00 -1.92E+00 -8.10E+00  2.57E+00 -8.40E+00  3.12E+01 -4.35E+00  2.03E+01 -2.58E+01  8.77E+00
         -3.45E+01 -2.59E+01  1.80E+02 -5.14E+01  6.15E+01 -3.74E+01  7.91E+01 -2.76E+01 -2.93E+01  4.48E+02 -1.28E+02  9.45E-01
         1.10E+00 -9.71E+00 -1.07E+02 -4.54E+01 -3.80E+01  1.89E+01 -2.58E+01 -3.89E+01  2.65E+02 -7.88E+01  2.16E+01  1.12E+01
         -2.06E+02  6.39E+01 -9.08E+00 -1.87E+02  5.29E+01  6.79E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        3.74E+01  8.45E+01 -1.82E+01  8.24E+00 -2.21E+00 -1.45E+01 -9.33E+00 -2.92E+01  1.30E+01 -8.99E+01 -1.09E+02 -6.81E+01
          9.27E+01  1.13E+02 -5.34E+02  3.22E+02 -1.05E+02 -8.56E-01 -6.93E+01 -3.15E+01  1.93E+02 -6.06E+02  7.62E+02 -3.75E+00
        -6.20E+01  3.80E+01  7.46E+01 -3.80E+02  8.34E+01 -5.55E+01  9.00E+01  1.35E+02 -2.89E+02  4.66E+02 -2.78E+01 -7.14E+01
          2.98E+02 -2.67E+02 -2.39E+01  4.34E+02 -3.59E+02 -5.50E+02  2.64E+03
 
 OM88
+       -6.77E+00 -3.73E+00  3.23E+00 -5.29E+00 -2.98E+01 -5.80E+00  1.73E+01 -6.71E-01  1.87E+01  8.36E+01  1.45E+02  5.96E+01
          4.75E+00 -7.28E+01  1.44E+02 -6.30E+02  1.18E+02  8.42E+01  3.35E+01  1.29E+01 -1.41E+02  9.35E+01 -8.81E+02  1.60E+01
         7.84E+01 -4.33E+01 -9.99E+00  1.17E+02 -3.59E+02 -2.74E+00 -6.54E+01 -2.79E+01  4.62E+01 -2.69E+02  4.61E+00  4.11E+01
         -1.07E+02  2.70E+02  1.73E+01 -9.08E+01  4.58E+02  6.20E+01 -7.54E+02  1.27E+03
 
 SG11
+       -1.82E+02  5.93E+02 -8.60E+01  4.80E+02  1.03E+03  7.63E+02  8.36E+01  9.51E+02  1.83E+02 -2.36E+03 -2.21E+03 -1.37E+03
         -1.39E+03  6.27E+02  2.93E+03  1.04E+03  2.71E+02 -6.18E+02 -2.79E+03 -7.18E+02  4.19E+03 -1.35E+03  2.56E+03  2.76E+03
        -2.58E+02  1.46E+03  3.55E+03 -5.87E+03  2.18E+03  4.04E+02  1.87E+03 -2.00E+02 -1.77E+03  5.37E+02 -1.41E+03 -7.65E+02
          7.93E+02 -1.03E+03 -9.52E+02  1.37E+03 -3.94E+03  1.53E+02 -2.10E+02 -4.39E+02  2.51E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -9.67E+02 -2.06E+02  3.63E+02  5.06E+02  1.85E+02 -6.59E+01  3.82E+02  2.24E+02 -1.21E+03  8.11E+01 -1.69E+03  1.01E+03
         -4.56E+02  5.38E+02  3.61E+02  1.51E+02 -9.15E+01 -7.04E+02  1.23E+03  1.31E+03  2.55E+03 -1.07E+03  3.76E+02 -4.26E+02
         3.61E+02 -5.39E+01 -5.84E+02  9.45E+02  1.97E+03  6.65E+01 -1.37E+03  6.23E+02  1.68E+03 -6.59E+02 -8.99E+01 -4.12E+02
          4.53E+02  1.13E+02  1.03E+03  7.17E+02 -7.97E+02 -3.68E+02  7.87E+02 -5.85E+02 -2.51E+04  0.00E+00  7.08E+05
 
 Elapsed postprocess time in seconds:     0.03
 #CPUT: Total CPU Time in Seconds,    10220.047
Stop Time: 
Tue 04/19/2016 
05:12 PM

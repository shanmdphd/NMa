Tue 04/19/2016 
07:27 PM
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
0.1 ;[p]

$EST METHOD=BAYES INTERACTION NBURN=4000 SIGL=4 NITER=10000 PRINT=25 CTYPE=3 NOABORT NOPRIOR=0 IKAPPA=0.75
$COV MATRIX=R UNCONDITIONAL
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
  
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
 0.0000E+00   0.1000E+00
0COVARIANCE STEP OMITTED:        NO
 R MATRIX SUBSTITUTED:          YES
 S MATRIX SUBSTITUTED:           NO
 EIGENVLS. PRINTED:              NO
 COMPRESSED FORMAT:              NO
 GRADIENT METHOD USED:     NOSLOW
 SIGDIGITS ETAHAT (SIGLO):                  -1
 SIGDIGITS GRADIENTS (SIGL):                -1
 RELATIVE TOLERANCE (TOL):                  -1
 ABSOLUTE TOLERANCE-ADVAN 9,13 ONLY (ATOL): -1
 EXCLUDE COV FOR FOCE (NOFCOV):              NO
 TURN OFF Cholesky Transposition of R Matrix (CHOLROFF): NO
 KNUTHSUMOFF:                                -1
 RESUME COV ANALYSIS (RESUME):               NO
 SIR SAMPLE SIZE (SIRSAMPLE):              -1
 LINEARLY TRANSFORM THETAS DURING COV (NOTHBND): -1
 PRECONDTIONING CYCLES (PRECOND):        0
 PRECONDTIONING TYPES (PRECONDS):        TOS
 FORCED PRECONDTIONING CYCLES (PFCOND):0
 PRECONDTIONING TYPE (PRETYPE):        0
 FORCED POS. DEFINITE SETTING: (FPOSDEF):0
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
 RAW OUTPUT FILE (FILE): example6classic2.ext
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
 DEG. FR. FOR T DIST.  PRIOR FOR THETAS (TTDF):        0.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR OMEGAS (OLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR OMEGAS (OVARF): 1.00000000000000
 DEG. FR. FOR LKJ CORRELATION PRIOR FOR SIGMAS (SLKJDF): 0.00000000000000
 WEIGHT FACTOR FOR STD PRIOR FOR SIGMAS (SVARF): 1.00000000000000

 
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
 iteration        -4000 MCMCOBJ=    14257.2160359759     
 iteration        -3975 MCMCOBJ=   -4619.40357650958     
 iteration        -3950 MCMCOBJ=   -5353.41417301393     
 iteration        -3925 MCMCOBJ=   -5701.06390458655     
 iteration        -3900 MCMCOBJ=   -5989.69104459212     
 iteration        -3875 MCMCOBJ=   -6249.17104518207     
 iteration        -3850 MCMCOBJ=   -6380.50830908914     
 iteration        -3825 MCMCOBJ=   -6399.84582998834     
 iteration        -3800 MCMCOBJ=   -6428.41961624486     
 iteration        -3775 MCMCOBJ=   -6442.89026086778     
 iteration        -3750 MCMCOBJ=   -6462.27598308933     
 iteration        -3725 MCMCOBJ=   -6539.87122893944     
 iteration        -3700 MCMCOBJ=   -6482.79394743926     
 iteration        -3675 MCMCOBJ=   -6434.00778635167     
 iteration        -3650 MCMCOBJ=   -6555.53279998940     
 iteration        -3625 MCMCOBJ=   -6597.58757595046     
 iteration        -3600 MCMCOBJ=   -6496.87616919915     
 iteration        -3575 MCMCOBJ=   -6476.29188163417     
 Convergence achieved
 Sampling Mode
 iteration            0 MCMCOBJ=   -6444.40442065998     
 iteration           25 MCMCOBJ=   -6426.50675752334     
 iteration           50 MCMCOBJ=   -6576.35468040096     
 iteration           75 MCMCOBJ=   -6494.54534006728     
 iteration          100 MCMCOBJ=   -6561.54455337999     
 iteration          125 MCMCOBJ=   -6574.11984756458     
 iteration          150 MCMCOBJ=   -6537.11055431394     
 iteration          175 MCMCOBJ=   -6494.89785249209     
 iteration          200 MCMCOBJ=   -6522.65960515382     
 iteration          225 MCMCOBJ=   -6513.42792051420     
 iteration          250 MCMCOBJ=   -6517.68718166633     
 iteration          275 MCMCOBJ=   -6548.52948797883     
 iteration          300 MCMCOBJ=   -6454.83176487648     
 iteration          325 MCMCOBJ=   -6531.22424375046     
 iteration          350 MCMCOBJ=   -6520.63882318555     
 iteration          375 MCMCOBJ=   -6489.41949389739     
 iteration          400 MCMCOBJ=   -6535.57542119759     
 iteration          425 MCMCOBJ=   -6500.71937515475     
 iteration          450 MCMCOBJ=   -6505.50372898223     
 iteration          475 MCMCOBJ=   -6511.90291498414     
 iteration          500 MCMCOBJ=   -6459.72762467906     
 iteration          525 MCMCOBJ=   -6532.45743414806     
 iteration          550 MCMCOBJ=   -6537.68501291513     
 iteration          575 MCMCOBJ=   -6536.54584163320     
 iteration          600 MCMCOBJ=   -6515.04278264012     
 iteration          625 MCMCOBJ=   -6457.42741158990     
 iteration          650 MCMCOBJ=   -6479.61644505801     
 iteration          675 MCMCOBJ=   -6494.32283273683     
 iteration          700 MCMCOBJ=   -6495.23523449257     
 iteration          725 MCMCOBJ=   -6475.32670535101     
 iteration          750 MCMCOBJ=   -6512.75855792204     
 iteration          775 MCMCOBJ=   -6544.34244784764     
 iteration          800 MCMCOBJ=   -6492.24381626842     
 iteration          825 MCMCOBJ=   -6529.05841983972     
 iteration          850 MCMCOBJ=   -6498.84624124817     
 iteration          875 MCMCOBJ=   -6487.48894661190     
 iteration          900 MCMCOBJ=   -6540.09237565912     
 iteration          925 MCMCOBJ=   -6490.97504233819     
 iteration          950 MCMCOBJ=   -6510.31098396488     
 iteration          975 MCMCOBJ=   -6496.79632480335     
 iteration         1000 MCMCOBJ=   -6522.58484050635     
 iteration         1025 MCMCOBJ=   -6583.04919214155     
 iteration         1050 MCMCOBJ=   -6463.43169077978     
 iteration         1075 MCMCOBJ=   -6518.05976409410     
 iteration         1100 MCMCOBJ=   -6524.61109529084     
 iteration         1125 MCMCOBJ=   -6448.65863191739     
 iteration         1150 MCMCOBJ=   -6464.05087939596     
 iteration         1175 MCMCOBJ=   -6496.35690698909     
 iteration         1200 MCMCOBJ=   -6572.38648474935     
 iteration         1225 MCMCOBJ=   -6443.88134324870     
 iteration         1250 MCMCOBJ=   -6495.52711878050     
 iteration         1275 MCMCOBJ=   -6463.42795513726     
 iteration         1300 MCMCOBJ=   -6535.84917454571     
 iteration         1325 MCMCOBJ=   -6538.78489127999     
 iteration         1350 MCMCOBJ=   -6483.97311952410     
 iteration         1375 MCMCOBJ=   -6496.75314757577     
 iteration         1400 MCMCOBJ=   -6558.81997075681     
 iteration         1425 MCMCOBJ=   -6478.46473151399     
 iteration         1450 MCMCOBJ=   -6480.60631392815     
 iteration         1475 MCMCOBJ=   -6505.76745822649     
 iteration         1500 MCMCOBJ=   -6483.99530731440     
 iteration         1525 MCMCOBJ=   -6512.04255866754     
 iteration         1550 MCMCOBJ=   -6514.36687529939     
 iteration         1575 MCMCOBJ=   -6448.67859508828     
 iteration         1600 MCMCOBJ=   -6542.74862116112     
 iteration         1625 MCMCOBJ=   -6525.45304186344     
 iteration         1650 MCMCOBJ=   -6458.44618346641     
 iteration         1675 MCMCOBJ=   -6549.89128159857     
 iteration         1700 MCMCOBJ=   -6505.85981149381     
 iteration         1725 MCMCOBJ=   -6529.40619959155     
 iteration         1750 MCMCOBJ=   -6494.47179195445     
 iteration         1775 MCMCOBJ=   -6450.93575901258     
 iteration         1800 MCMCOBJ=   -6434.52773755736     
 iteration         1825 MCMCOBJ=   -6494.98402172260     
 iteration         1850 MCMCOBJ=   -6539.29352820224     
 iteration         1875 MCMCOBJ=   -6547.47008662244     
 iteration         1900 MCMCOBJ=   -6551.96723610407     
 iteration         1925 MCMCOBJ=   -6526.80206744338     
 iteration         1950 MCMCOBJ=   -6481.03307814925     
 iteration         1975 MCMCOBJ=   -6516.01567145790     
 iteration         2000 MCMCOBJ=   -6457.90980103132     
 iteration         2025 MCMCOBJ=   -6425.45589919201     
 iteration         2050 MCMCOBJ=   -6440.37002772541     
 iteration         2075 MCMCOBJ=   -6517.56912364869     
 iteration         2100 MCMCOBJ=   -6525.33862487039     
 iteration         2125 MCMCOBJ=   -6467.27799180448     
 iteration         2150 MCMCOBJ=   -6434.33421362193     
 iteration         2175 MCMCOBJ=   -6481.93692244237     
 iteration         2200 MCMCOBJ=   -6487.42967912894     
 iteration         2225 MCMCOBJ=   -6523.01281393367     
 iteration         2250 MCMCOBJ=   -6447.00153996780     
 iteration         2275 MCMCOBJ=   -6493.11863414957     
 iteration         2300 MCMCOBJ=   -6487.62265395612     
 iteration         2325 MCMCOBJ=   -6541.46020147744     
 iteration         2350 MCMCOBJ=   -6481.72035645740     
 iteration         2375 MCMCOBJ=   -6531.27222739122     
 iteration         2400 MCMCOBJ=   -6544.96736542150     
 iteration         2425 MCMCOBJ=   -6463.32782144147     
 iteration         2450 MCMCOBJ=   -6519.65514762130     
 iteration         2475 MCMCOBJ=   -6497.15453045680     
 iteration         2500 MCMCOBJ=   -6532.29088104361     
 iteration         2525 MCMCOBJ=   -6443.01444758604     
 iteration         2550 MCMCOBJ=   -6470.96741419306     
 iteration         2575 MCMCOBJ=   -6552.96636650844     
 iteration         2600 MCMCOBJ=   -6501.85001126426     
 iteration         2625 MCMCOBJ=   -6496.27384739423     
 iteration         2650 MCMCOBJ=   -6536.18727918088     
 iteration         2675 MCMCOBJ=   -6464.18478084024     
 iteration         2700 MCMCOBJ=   -6442.60479563355     
 iteration         2725 MCMCOBJ=   -6509.78679520065     
 iteration         2750 MCMCOBJ=   -6458.28617416494     
 iteration         2775 MCMCOBJ=   -6486.55674005585     
 iteration         2800 MCMCOBJ=   -6464.75151080604     
 iteration         2825 MCMCOBJ=   -6499.23142763740     
 iteration         2850 MCMCOBJ=   -6486.66720589986     
 iteration         2875 MCMCOBJ=   -6503.63316905347     
 iteration         2900 MCMCOBJ=   -6457.65297060052     
 iteration         2925 MCMCOBJ=   -6475.32435007223     
 iteration         2950 MCMCOBJ=   -6453.23496623923     
 iteration         2975 MCMCOBJ=   -6518.98732764370     
 iteration         3000 MCMCOBJ=   -6485.07898374415     
 iteration         3025 MCMCOBJ=   -6532.05411039617     
 iteration         3050 MCMCOBJ=   -6460.26939462216     
 iteration         3075 MCMCOBJ=   -6451.98933590597     
 iteration         3100 MCMCOBJ=   -6509.34485640121     
 iteration         3125 MCMCOBJ=   -6487.42580078599     
 iteration         3150 MCMCOBJ=   -6493.42597635033     
 iteration         3175 MCMCOBJ=   -6544.66732048058     
 iteration         3200 MCMCOBJ=   -6479.69350847020     
 iteration         3225 MCMCOBJ=   -6449.48505408464     
 iteration         3250 MCMCOBJ=   -6530.92693183995     
 iteration         3275 MCMCOBJ=   -6468.97628893228     
 iteration         3300 MCMCOBJ=   -6489.24911424776     
 iteration         3325 MCMCOBJ=   -6463.92521383541     
 iteration         3350 MCMCOBJ=   -6468.70366098035     
 iteration         3375 MCMCOBJ=   -6501.53497377317     
 iteration         3400 MCMCOBJ=   -6503.90782346971     
 iteration         3425 MCMCOBJ=   -6481.37800456889     
 iteration         3450 MCMCOBJ=   -6487.27972562594     
 iteration         3475 MCMCOBJ=   -6539.63377127228     
 iteration         3500 MCMCOBJ=   -6536.29216439598     
 iteration         3525 MCMCOBJ=   -6479.78521936823     
 iteration         3550 MCMCOBJ=   -6529.96219057999     
 iteration         3575 MCMCOBJ=   -6501.18416478424     
 iteration         3600 MCMCOBJ=   -6487.62462849524     
 iteration         3625 MCMCOBJ=   -6505.60819411090     
 iteration         3650 MCMCOBJ=   -6557.94268374048     
 iteration         3675 MCMCOBJ=   -6491.95883050637     
 iteration         3700 MCMCOBJ=   -6525.06300067834     
 iteration         3725 MCMCOBJ=   -6505.28176983849     
 iteration         3750 MCMCOBJ=   -6490.09930058342     
 iteration         3775 MCMCOBJ=   -6508.52200951235     
 iteration         3800 MCMCOBJ=   -6485.18116440082     
 iteration         3825 MCMCOBJ=   -6531.05014318330     
 iteration         3850 MCMCOBJ=   -6469.72067678339     
 iteration         3875 MCMCOBJ=   -6478.69547874138     
 iteration         3900 MCMCOBJ=   -6457.89267931123     
 iteration         3925 MCMCOBJ=   -6496.62280308732     
 iteration         3950 MCMCOBJ=   -6504.58846756378     
 iteration         3975 MCMCOBJ=   -6505.48787144311     
 iteration         4000 MCMCOBJ=   -6418.05463893834     
 iteration         4025 MCMCOBJ=   -6464.15225236468     
 iteration         4050 MCMCOBJ=   -6511.28418370604     
 iteration         4075 MCMCOBJ=   -6529.93593567870     
 iteration         4100 MCMCOBJ=   -6478.39491350415     
 iteration         4125 MCMCOBJ=   -6539.91989204188     
 iteration         4150 MCMCOBJ=   -6482.99158137225     
 iteration         4175 MCMCOBJ=   -6489.27445360930     
 iteration         4200 MCMCOBJ=   -6509.99393680672     
 iteration         4225 MCMCOBJ=   -6391.39395303244     
 iteration         4250 MCMCOBJ=   -6412.74887239842     
 iteration         4275 MCMCOBJ=   -6492.87279273850     
 iteration         4300 MCMCOBJ=   -6511.19182431593     
 iteration         4325 MCMCOBJ=   -6516.77675763778     
 iteration         4350 MCMCOBJ=   -6476.51731976855     
 iteration         4375 MCMCOBJ=   -6461.09703556792     
 iteration         4400 MCMCOBJ=   -6466.24636903486     
 iteration         4425 MCMCOBJ=   -6528.07669761662     
 iteration         4450 MCMCOBJ=   -6510.46926897005     
 iteration         4475 MCMCOBJ=   -6547.90786112270     
 iteration         4500 MCMCOBJ=   -6479.41487006594     
 iteration         4525 MCMCOBJ=   -6484.29318406058     
 iteration         4550 MCMCOBJ=   -6463.67261808399     
 iteration         4575 MCMCOBJ=   -6497.30212652437     
 iteration         4600 MCMCOBJ=   -6504.55226270244     
 iteration         4625 MCMCOBJ=   -6429.67931632274     
 iteration         4650 MCMCOBJ=   -6471.84364281247     
 iteration         4675 MCMCOBJ=   -6461.45748422401     
 iteration         4700 MCMCOBJ=   -6532.93975655773     
 iteration         4725 MCMCOBJ=   -6533.21798737451     
 iteration         4750 MCMCOBJ=   -6507.28712933380     
 iteration         4775 MCMCOBJ=   -6523.77369216865     
 iteration         4800 MCMCOBJ=   -6490.18232052576     
 iteration         4825 MCMCOBJ=   -6523.13669542177     
 iteration         4850 MCMCOBJ=   -6518.95363436245     
 iteration         4875 MCMCOBJ=   -6486.94493387249     
 iteration         4900 MCMCOBJ=   -6530.17858167262     
 iteration         4925 MCMCOBJ=   -6484.98651702676     
 iteration         4950 MCMCOBJ=   -6448.52397514739     
 iteration         4975 MCMCOBJ=   -6441.34396893195     
 iteration         5000 MCMCOBJ=   -6447.11743983511     
 iteration         5025 MCMCOBJ=   -6401.01543707838     
 iteration         5050 MCMCOBJ=   -6480.11788337003     
 iteration         5075 MCMCOBJ=   -6414.84690797375     
 iteration         5100 MCMCOBJ=   -6537.59387643536     
 iteration         5125 MCMCOBJ=   -6503.85867923966     
 iteration         5150 MCMCOBJ=   -6468.74411918259     
 iteration         5175 MCMCOBJ=   -6470.20796494519     
 iteration         5200 MCMCOBJ=   -6476.34903187889     
 iteration         5225 MCMCOBJ=   -6475.55460984243     
 iteration         5250 MCMCOBJ=   -6493.52881171405     
 iteration         5275 MCMCOBJ=   -6501.85906248120     
 iteration         5300 MCMCOBJ=   -6494.93729506333     
 iteration         5325 MCMCOBJ=   -6456.26078680226     
 iteration         5350 MCMCOBJ=   -6510.85335513585     
 iteration         5375 MCMCOBJ=   -6501.11964468169     
 iteration         5400 MCMCOBJ=   -6495.16920488973     
 iteration         5425 MCMCOBJ=   -6447.32075976865     
 iteration         5450 MCMCOBJ=   -6499.00999952469     
 iteration         5475 MCMCOBJ=   -6492.97808270876     
 iteration         5500 MCMCOBJ=   -6446.91496160917     
 iteration         5525 MCMCOBJ=   -6432.56056254048     
 iteration         5550 MCMCOBJ=   -6469.24110828088     
 iteration         5575 MCMCOBJ=   -6504.98968808867     
 iteration         5600 MCMCOBJ=   -6480.15584396738     
 iteration         5625 MCMCOBJ=   -6502.37924478431     
 iteration         5650 MCMCOBJ=   -6519.34962326623     
 iteration         5675 MCMCOBJ=   -6474.52219247234     
 iteration         5700 MCMCOBJ=   -6539.88885970734     
 iteration         5725 MCMCOBJ=   -6474.68095327167     
 iteration         5750 MCMCOBJ=   -6509.79497697702     
 iteration         5775 MCMCOBJ=   -6506.27138487230     
 iteration         5800 MCMCOBJ=   -6484.94861013846     
 iteration         5825 MCMCOBJ=   -6461.13583420207     
 iteration         5850 MCMCOBJ=   -6496.97431375031     
 iteration         5875 MCMCOBJ=   -6507.19569230752     
 iteration         5900 MCMCOBJ=   -6536.86325169183     
 iteration         5925 MCMCOBJ=   -6512.94807074066     
 iteration         5950 MCMCOBJ=   -6465.16405738294     
 iteration         5975 MCMCOBJ=   -6549.03593770634     
 iteration         6000 MCMCOBJ=   -6488.27126185864     
 iteration         6025 MCMCOBJ=   -6544.52343121096     
 iteration         6050 MCMCOBJ=   -6509.61863892307     
 iteration         6075 MCMCOBJ=   -6557.97974924764     
 iteration         6100 MCMCOBJ=   -6489.76472727463     
 iteration         6125 MCMCOBJ=   -6508.15704485337     
 iteration         6150 MCMCOBJ=   -6514.31228200520     
 iteration         6175 MCMCOBJ=   -6532.34526743122     
 iteration         6200 MCMCOBJ=   -6535.94922770700     
 iteration         6225 MCMCOBJ=   -6477.50978462597     
 iteration         6250 MCMCOBJ=   -6511.98830465560     
 iteration         6275 MCMCOBJ=   -6467.17889640476     
 iteration         6300 MCMCOBJ=   -6506.62310103590     
 iteration         6325 MCMCOBJ=   -6520.38701387499     
 iteration         6350 MCMCOBJ=   -6503.52262542729     
 iteration         6375 MCMCOBJ=   -6468.53798846293     
 iteration         6400 MCMCOBJ=   -6512.00832022861     
 iteration         6425 MCMCOBJ=   -6426.43819524005     
 iteration         6450 MCMCOBJ=   -6402.69807873216     
 iteration         6475 MCMCOBJ=   -6488.50471250335     
 iteration         6500 MCMCOBJ=   -6476.17164885963     
 iteration         6525 MCMCOBJ=   -6510.62686897948     
 iteration         6550 MCMCOBJ=   -6469.54592852634     
 iteration         6575 MCMCOBJ=   -6519.63516083922     
 iteration         6600 MCMCOBJ=   -6434.66957603446     
 iteration         6625 MCMCOBJ=   -6441.97545807940     
 iteration         6650 MCMCOBJ=   -6532.44352705797     
 iteration         6675 MCMCOBJ=   -6501.91012375257     
 iteration         6700 MCMCOBJ=   -6499.11982429384     
 iteration         6725 MCMCOBJ=   -6490.03119014132     
 iteration         6750 MCMCOBJ=   -6487.59600660545     
 iteration         6775 MCMCOBJ=   -6539.99251026481     
 iteration         6800 MCMCOBJ=   -6422.98193113460     
 iteration         6825 MCMCOBJ=   -6470.98349495034     
 iteration         6850 MCMCOBJ=   -6478.79900678643     
 iteration         6875 MCMCOBJ=   -6541.57442318106     
 iteration         6900 MCMCOBJ=   -6418.03056390147     
 iteration         6925 MCMCOBJ=   -6500.80306145414     
 iteration         6950 MCMCOBJ=   -6475.96285932967     
 iteration         6975 MCMCOBJ=   -6512.26542985202     
 iteration         7000 MCMCOBJ=   -6544.29926685353     
 iteration         7025 MCMCOBJ=   -6509.75224375108     
 iteration         7050 MCMCOBJ=   -6442.17398074954     
 iteration         7075 MCMCOBJ=   -6536.60176178477     
 iteration         7100 MCMCOBJ=   -6452.09013596214     
 iteration         7125 MCMCOBJ=   -6484.80682063066     
 iteration         7150 MCMCOBJ=   -6535.00599938024     
 iteration         7175 MCMCOBJ=   -6474.66429162875     
 iteration         7200 MCMCOBJ=   -6478.40269687222     
 iteration         7225 MCMCOBJ=   -6508.98150520559     
 iteration         7250 MCMCOBJ=   -6500.33147456053     
 iteration         7275 MCMCOBJ=   -6581.99192004018     
 iteration         7300 MCMCOBJ=   -6489.53614811958     
 iteration         7325 MCMCOBJ=   -6455.53927524624     
 iteration         7350 MCMCOBJ=   -6514.41416094111     
 iteration         7375 MCMCOBJ=   -6512.36141629975     
 iteration         7400 MCMCOBJ=   -6448.29313108976     
 iteration         7425 MCMCOBJ=   -6489.28536230419     
 iteration         7450 MCMCOBJ=   -6467.43299775281     
 iteration         7475 MCMCOBJ=   -6549.07524915455     
 iteration         7500 MCMCOBJ=   -6506.52338656695     
 iteration         7525 MCMCOBJ=   -6474.42309928560     
 iteration         7550 MCMCOBJ=   -6526.68747530517     
 iteration         7575 MCMCOBJ=   -6437.92430226735     
 iteration         7600 MCMCOBJ=   -6522.64836506758     
 iteration         7625 MCMCOBJ=   -6506.63475057617     
 iteration         7650 MCMCOBJ=   -6486.28242155208     
 iteration         7675 MCMCOBJ=   -6477.57443122440     
 iteration         7700 MCMCOBJ=   -6479.50707008105     
 iteration         7725 MCMCOBJ=   -6506.80171505460     
 iteration         7750 MCMCOBJ=   -6432.04201495432     
 iteration         7775 MCMCOBJ=   -6522.20270692095     
 iteration         7800 MCMCOBJ=   -6509.14772961934     
 iteration         7825 MCMCOBJ=   -6536.34309879784     
 iteration         7850 MCMCOBJ=   -6524.16556528970     
 iteration         7875 MCMCOBJ=   -6519.64968654119     
 iteration         7900 MCMCOBJ=   -6524.48234195869     
 iteration         7925 MCMCOBJ=   -6504.28185045766     
 iteration         7950 MCMCOBJ=   -6478.97370020346     
 iteration         7975 MCMCOBJ=   -6503.96236672592     
 iteration         8000 MCMCOBJ=   -6500.63708799889     
 iteration         8025 MCMCOBJ=   -6548.55800003786     
 iteration         8050 MCMCOBJ=   -6499.32304299700     
 iteration         8075 MCMCOBJ=   -6532.05138378852     
 iteration         8100 MCMCOBJ=   -6507.20156972283     
 iteration         8125 MCMCOBJ=   -6459.56148595844     
 iteration         8150 MCMCOBJ=   -6531.65919500044     
 iteration         8175 MCMCOBJ=   -6461.33983865900     
 iteration         8200 MCMCOBJ=   -6414.90183063848     
 iteration         8225 MCMCOBJ=   -6498.74633328131     
 iteration         8250 MCMCOBJ=   -6525.71856973658     
 iteration         8275 MCMCOBJ=   -6524.24740931972     
 iteration         8300 MCMCOBJ=   -6514.41560613113     
 iteration         8325 MCMCOBJ=   -6463.14806010515     
 iteration         8350 MCMCOBJ=   -6533.77924441943     
 iteration         8375 MCMCOBJ=   -6453.92291099925     
 iteration         8400 MCMCOBJ=   -6527.53946274037     
 iteration         8425 MCMCOBJ=   -6503.88730941306     
 iteration         8450 MCMCOBJ=   -6536.13163678314     
 iteration         8475 MCMCOBJ=   -6441.34200538650     
 iteration         8500 MCMCOBJ=   -6493.74313305212     
 iteration         8525 MCMCOBJ=   -6482.97929743789     
 iteration         8550 MCMCOBJ=   -6480.05448701063     
 iteration         8575 MCMCOBJ=   -6463.01827694415     
 iteration         8600 MCMCOBJ=   -6504.11577791928     
 iteration         8625 MCMCOBJ=   -6485.44783223980     
 iteration         8650 MCMCOBJ=   -6497.34723743066     
 iteration         8675 MCMCOBJ=   -6504.59181422711     
 iteration         8700 MCMCOBJ=   -6509.04371367113     
 iteration         8725 MCMCOBJ=   -6513.27311594845     
 iteration         8750 MCMCOBJ=   -6526.84733897157     
 iteration         8775 MCMCOBJ=   -6499.27947165441     
 iteration         8800 MCMCOBJ=   -6454.41351953828     
 iteration         8825 MCMCOBJ=   -6511.10514520417     
 iteration         8850 MCMCOBJ=   -6524.18791693458     
 iteration         8875 MCMCOBJ=   -6485.25585543618     
 iteration         8900 MCMCOBJ=   -6484.06673921294     
 iteration         8925 MCMCOBJ=   -6468.40131449876     
 iteration         8950 MCMCOBJ=   -6469.38710034323     
 iteration         8975 MCMCOBJ=   -6494.79168250185     
 iteration         9000 MCMCOBJ=   -6482.82450132212     
 iteration         9025 MCMCOBJ=   -6528.32526708446     
 iteration         9050 MCMCOBJ=   -6468.36362492015     
 iteration         9075 MCMCOBJ=   -6486.86278247415     
 iteration         9100 MCMCOBJ=   -6527.12567753247     
 iteration         9125 MCMCOBJ=   -6476.51048638737     
 iteration         9150 MCMCOBJ=   -6479.38253610456     
 iteration         9175 MCMCOBJ=   -6507.07501104546     
 iteration         9200 MCMCOBJ=   -6502.65553788753     
 iteration         9225 MCMCOBJ=   -6455.05182377037     
 iteration         9250 MCMCOBJ=   -6457.52171556301     
 iteration         9275 MCMCOBJ=   -6465.73218462326     
 iteration         9300 MCMCOBJ=   -6441.92195288107     
 iteration         9325 MCMCOBJ=   -6517.70567287894     
 iteration         9350 MCMCOBJ=   -6508.21672116768     
 iteration         9375 MCMCOBJ=   -6441.79545606651     
 iteration         9400 MCMCOBJ=   -6522.03977890490     
 iteration         9425 MCMCOBJ=   -6514.27842625632     
 iteration         9450 MCMCOBJ=   -6502.47721457377     
 iteration         9475 MCMCOBJ=   -6466.42024319199     
 iteration         9500 MCMCOBJ=   -6493.57974350501     
 iteration         9525 MCMCOBJ=   -6490.01258531018     
 iteration         9550 MCMCOBJ=   -6506.15231810040     
 iteration         9575 MCMCOBJ=   -6502.44508305069     
 iteration         9600 MCMCOBJ=   -6532.03810328544     
 iteration         9625 MCMCOBJ=   -6482.53012391583     
 iteration         9650 MCMCOBJ=   -6458.80253442945     
 iteration         9675 MCMCOBJ=   -6515.35270541719     
 iteration         9700 MCMCOBJ=   -6423.42879739824     
 iteration         9725 MCMCOBJ=   -6512.05385824087     
 iteration         9750 MCMCOBJ=   -6488.77144986107     
 iteration         9775 MCMCOBJ=   -6452.93003277272     
 iteration         9800 MCMCOBJ=   -6455.32818093719     
 iteration         9825 MCMCOBJ=   -6492.78522177623     
 iteration         9850 MCMCOBJ=   -6475.87227773249     
 iteration         9875 MCMCOBJ=   -6526.67667964665     
 iteration         9900 MCMCOBJ=   -6518.79307450335     
 iteration         9925 MCMCOBJ=   -6482.52370380856     
 iteration         9950 MCMCOBJ=   -6490.93344090881     
 iteration         9975 MCMCOBJ=   -6505.34142251763     
 iteration        10000 MCMCOBJ=   -6507.19799932887     
 BURN-IN WAS COMPLETED
 
 #TERM:
 STATISTICAL PORTION WAS COMPLETED
  
 TOTAL DATA POINTS NORMALLY DISTRIBUTED (N):         1568
 N*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    2881.79124012985     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6492.94132407657     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -3611.15008394672     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 TOTAL EFFECTIVE ETAS (NIND*NETA):                           400
 NIND*NETA*LOG(2PI) CONSTANT TO OBJECTIVE FUNCTION:    735.150826563738     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6492.94132407657     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -5757.79049751283     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 PRIOR CONSTANT TO OBJECTIVE FUNCTION:    55.1779157436876     
 OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT:   -6492.94132407657     
 OBJECTIVE FUNCTION VALUE WITH CONSTANT:      -6437.76340833288     
 REPORTED OBJECTIVE FUNCTION DOES NOT CONTAIN CONSTANT
  
 #TERE:
 Elapsed estimation  time in seconds:  2917.78
 Elapsed covariance  time in seconds:     0.01
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 #OBJT:**************                       AVERAGE VALUE OF LIKELIHOOD FUNCTION                     ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 #OBJV:********************************************    -6492.941       **************************************************
 #OBJS:********************************************       32.980 (STD) **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                              MCMC BAYESIAN ANALYSIS                            ********************
 ********************                             FINAL PARAMETER ESTIMATE                           ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


         TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8     
 
         3.91E+00 -2.22E+00  5.54E-01 -1.85E-01  2.27E+00  2.40E-01  3.71E+00 -7.04E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        2.81E-01
 
 ETA2
+       -3.30E-02  2.12E-01
 
 ETA3
+        4.48E-02 -1.09E-02  1.41E-01
 
 ETA4
+        3.01E-02  5.61E-02 -1.40E-02  2.61E-01
 
 ETA5
+        2.75E-02  1.67E-02 -7.54E-04 -3.27E-02  2.07E-01
 
 ETA6
+       -2.64E-02  4.06E-03  1.59E-02  1.43E-02 -7.39E-02  2.34E-01
 
 ETA7
+        2.89E-02 -4.69E-02  3.27E-02 -7.36E-02  2.40E-02 -1.12E-03  2.46E-01
 
 ETA8
+        9.57E-02  7.37E-02  4.09E-02  4.64E-02  5.44E-03 -5.12E-02  5.73E-02  2.36E-01
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        9.29E-03
 
 EPS2
+        0.00E+00  2.23E-02
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.28E-01
 
 ETA2
+       -1.32E-01  4.57E-01
 
 ETA3
+        2.24E-01 -6.40E-02  3.73E-01
 
 ETA4
+        1.11E-01  2.36E-01 -7.45E-02  5.07E-01
 
 ETA5
+        1.13E-01  8.03E-02 -5.19E-03 -1.40E-01  4.52E-01
 
 ETA6
+       -1.03E-01  1.94E-02  8.81E-02  5.71E-02 -3.35E-01  4.80E-01
 
 ETA7
+        1.09E-01 -2.01E-01  1.75E-01 -2.88E-01  1.05E-01 -4.61E-03  4.93E-01
 
 ETA8
+        3.69E-01  3.29E-01  2.21E-01  1.85E-01  2.39E-02 -2.16E-01  2.35E-01  4.83E-01
 


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
 
         7.53E-02  7.42E-02  5.90E-02  7.38E-02  6.60E-02  7.52E-02  6.97E-02  7.04E-02
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.91E-02
 
 ETA2
+        3.96E-02  5.34E-02
 
 ETA3
+        3.21E-02  2.85E-02  3.45E-02
 
 ETA4
+        4.00E-02  3.93E-02  3.17E-02  5.73E-02
 
 ETA5
+        3.66E-02  3.27E-02  2.75E-02  3.49E-02  4.44E-02
 
 ETA6
+        3.98E-02  3.75E-02  2.93E-02  3.95E-02  3.60E-02  5.65E-02
 
 ETA7
+        3.92E-02  3.87E-02  2.96E-02  3.89E-02  3.36E-02  3.72E-02  5.12E-02
 
 ETA8
+        4.07E-02  3.66E-02  2.98E-02  3.81E-02  3.35E-02  3.75E-02  3.69E-02  5.08E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


         EPS1      EPS2     
 
 EPS1
+        6.24E-04
 
 EPS2
+        0.00E+00  1.18E-03
 
1


 OMEGA - CORR MATRIX FOR RANDOM EFFECTS - ETAS  *******


         ETA1      ETA2      ETA3      ETA4      ETA5      ETA6      ETA7      ETA8     
 
 ETA1
+        5.45E-02
 
 ETA2
+        1.49E-01  5.64E-02
 
 ETA3
+        1.45E-01  1.58E-01  4.46E-02
 
 ETA4
+        1.40E-01  1.48E-01  1.59E-01  5.51E-02
 
 ETA5
+        1.42E-01  1.49E-01  1.54E-01  1.40E-01  4.77E-02
 
 ETA6
+        1.47E-01  1.62E-01  1.54E-01  1.53E-01  1.39E-01  5.68E-02
 
 ETA7
+        1.40E-01  1.52E-01  1.47E-01  1.30E-01  1.40E-01  1.49E-01  5.04E-02
 
 ETA8
+        1.24E-01  1.38E-01  1.45E-01  1.40E-01  1.45E-01  1.44E-01  1.35E-01  5.10E-02
 


 SIGMA - CORR MATRIX FOR RANDOM EFFECTS - EPSILONS  ***


         EPS1      EPS2     
 
 EPS1
+        3.23E-03
 
 EPS2
+        0.00E+00  3.95E-03
 
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
+        5.67E-03
 
 TH 2
+       -7.44E-04  5.50E-03
 
 TH 3
+        7.38E-04 -7.28E-05  3.48E-03
 
 TH 4
+        6.26E-04  1.14E-03  1.15E-04  5.44E-03
 
 TH 5
+        4.68E-04  2.62E-04  2.22E-05 -5.59E-04  4.35E-03
 
 TH 6
+       -4.47E-04 -2.13E-04  1.57E-04  3.27E-04 -1.20E-03  5.66E-03
 
 TH 7
+        6.33E-04 -1.15E-03  6.67E-04 -1.41E-03  5.68E-04  8.13E-05  4.86E-03
 
 TH 8
+        1.85E-03  1.24E-03  9.98E-04  1.00E-03  2.20E-04 -1.08E-03  1.20E-03  4.95E-03
 
 OM11
+       -6.30E-05  1.43E-06 -6.24E-05 -2.22E-05  1.62E-05  2.64E-05 -1.08E-06 -1.42E-06  3.50E-03
 
 OM12
+        3.04E-05  1.67E-04 -7.29E-06  3.53E-05 -1.38E-05 -4.77E-05  3.98E-06 -9.93E-06 -4.71E-04  1.57E-03
 
 OM13
+        2.73E-06  3.49E-05  7.31E-05  2.78E-05  2.93E-05 -2.41E-05 -2.73E-05  5.24E-05  4.85E-04 -9.29E-05  1.03E-03
 
 OM14
+        6.18E-05 -5.04E-05 -6.35E-05 -4.64E-06 -5.88E-06 -5.29E-05  8.83E-05  6.35E-05  3.11E-04  3.24E-04 -6.11E-06  1.60E-03
 
 OM15
+       -4.43E-05  5.06E-05 -1.83E-05 -5.62E-06  1.60E-05  7.41E-05 -1.51E-06  2.62E-05  3.36E-04  4.22E-05  3.29E-05 -1.94E-04
          1.34E-03
 
 OM16
+        3.89E-05  1.06E-05 -1.24E-05  1.23E-05  3.56E-05  3.38E-05 -1.19E-05 -9.34E-06 -2.61E-04 -4.61E-05  2.60E-05  6.45E-05
         -3.66E-04  1.58E-03
 
 OM17
+        1.98E-05 -3.79E-05  1.23E-05 -6.79E-07  4.52E-05  6.59E-06 -2.60E-05  4.34E-06  4.10E-04 -4.04E-04  2.26E-04 -4.17E-04
          1.93E-04 -5.21E-06  1.53E-03
 
 OM18
+        3.93E-05 -3.27E-05 -3.58E-05 -3.98E-05  4.76E-06  7.72E-06 -2.20E-05 -6.00E-06  1.17E-03  3.17E-04  3.61E-04  3.30E-04
          1.26E-04 -3.80E-04  4.38E-04  1.66E-03
 
 OM22
+       -5.15E-05 -6.70E-04  4.36E-05  6.54E-06  2.44E-05  1.22E-04  8.17E-05  6.80E-05  1.85E-04 -5.95E-04  3.35E-05 -1.11E-04
         -6.80E-06  3.19E-05  1.31E-04 -7.05E-05  2.85E-03
 
 OM23
+       -3.15E-05  1.62E-04  5.42E-05  5.70E-05 -1.48E-05 -2.05E-05 -1.06E-05 -1.06E-05 -8.70E-05  2.31E-04 -1.03E-04  6.51E-05
         -8.51E-06 -9.41E-06 -7.43E-05  8.28E-07 -1.32E-04  8.11E-04
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -2.17E-06 -3.46E-04  5.99E-05  1.69E-05  2.57E-05  3.70E-05  1.01E-05  2.81E-05 -2.39E-05 -1.47E-05  2.64E-05 -1.81E-04
          5.10E-05 -2.14E-05  3.17E-05  4.74E-06  7.78E-04 -6.66E-05  1.55E-03
 
 OM25
+       -1.83E-06  1.55E-05 -3.47E-05  1.41E-05  3.26E-05  1.62E-05  2.03E-05 -1.28E-05 -3.20E-05  1.27E-04 -6.17E-06  7.81E-05
         -1.70E-04  2.65E-05 -6.54E-05  3.66E-05  1.06E-04  2.89E-05 -1.83E-04  1.07E-03
 
 OM26
+       -4.34E-05 -6.90E-06  2.48E-06 -9.92E-06 -1.38E-05  1.09E-05 -1.61E-05 -8.20E-06  8.03E-05 -1.11E-04 -1.21E-06 -1.81E-05
          4.40E-05 -2.09E-04  4.67E-05  4.76E-05 -1.09E-06  5.23E-05  6.26E-05 -3.22E-04  1.41E-03
 
 OM27
+       -2.05E-05  5.06E-04 -8.39E-05  2.04E-06 -3.69E-05 -4.14E-05 -3.17E-06 -4.77E-05 -7.98E-05  3.06E-04 -4.97E-05  1.03E-04
         -2.90E-05  2.35E-05 -2.52E-04 -3.86E-05 -7.75E-04  2.18E-04 -5.99E-04  1.62E-04 -1.17E-05  1.50E-03
 
 OM28
+       -1.54E-05  4.61E-05  1.47E-05  4.48E-05  6.70E-06  1.17E-06  3.64E-05  1.22E-06 -1.46E-04  4.22E-04 -5.44E-05  9.27E-05
          1.31E-05  5.11E-05 -1.79E-04 -7.20E-05  7.08E-04  2.25E-04  2.91E-04  7.16E-05 -3.19E-04  2.88E-04  1.34E-03
 
 OM33
+        7.78E-05  3.13E-05 -1.32E-04 -7.18E-05 -9.03E-06 -1.36E-05  2.33E-06  3.29E-05  7.59E-05 -5.50E-06  3.07E-04  3.46E-05
          2.05E-05  1.43E-05  6.75E-05  8.86E-05  7.59E-06 -1.45E-05 -3.45E-06  1.75E-05  5.45E-06  1.31E-06  7.56E-06  1.19E-03
 
 OM34
+       -4.08E-05  4.00E-05  2.07E-04  7.64E-05  1.05E-05 -7.37E-06 -2.19E-06  5.17E-05  3.20E-05  3.84E-05  1.68E-04  1.98E-04
         -1.96E-05  7.14E-06 -2.35E-05  6.12E-05  5.30E-06  2.10E-04  9.05E-06 -5.54E-06 -7.76E-06  8.73E-06  6.06E-05 -7.53E-05
         1.01E-03
 
 OM35
+       -1.32E-05 -2.11E-06 -5.70E-05 -3.12E-05  5.69E-06  5.97E-06  9.05E-06  8.59E-06  4.07E-05 -3.20E-05  9.50E-05 -3.94E-05
          1.67E-04 -2.38E-05  4.84E-05  2.59E-05  1.46E-05  4.96E-05  8.31E-06 -2.11E-05 -1.86E-05  7.99E-06  8.35E-06  2.71E-05
        -1.32E-04  7.56E-04
 
 OM36
+        6.37E-05  1.37E-05  4.14E-05  9.77E-06 -2.76E-06 -1.62E-05 -4.33E-06  2.59E-05 -2.21E-05  5.81E-06 -6.09E-05  1.09E-05
         -3.26E-05  2.13E-04 -3.97E-06 -5.31E-05 -2.15E-05  6.39E-06 -1.18E-06  1.35E-05 -2.24E-05  2.22E-05  1.56E-06  6.73E-05
         6.53E-05 -2.29E-04  8.57E-04
 
 OM37
+        4.35E-05 -2.98E-05 -5.48E-05 -3.58E-05  3.22E-06 -1.15E-05 -2.37E-05  8.16E-06  5.15E-05 -5.71E-05  9.14E-05 -7.60E-05
          2.65E-05  6.17E-06  2.12E-04  8.11E-05  1.38E-05 -2.01E-04 -6.51E-07 -3.89E-06  7.66E-06 -4.62E-05 -6.51E-05  2.22E-04
        -2.95E-04  9.49E-05 -1.18E-05  8.76E-04
 
 OM38
+       -1.93E-05  3.04E-05  6.22E-05  6.61E-05 -2.48E-06  1.06E-06 -2.99E-05  3.58E-05  1.46E-04  2.79E-05  4.01E-04  3.53E-05
          2.99E-06 -3.27E-05  1.10E-04  2.95E-04  3.18E-05  2.58E-04  1.58E-05 -1.01E-06  1.76E-05  2.48E-05  5.20E-05  3.49E-04
         1.91E-04  2.45E-05 -1.64E-04  2.11E-04  8.90E-04
 
 OM44
+       -7.94E-05  6.78E-05  2.64E-04  1.51E-04  8.12E-05  4.34E-05 -6.69E-06  1.22E-05  8.47E-05  4.19E-05  1.61E-05  3.28E-04
         -3.75E-05  2.80E-06 -5.28E-05  7.27E-05  1.90E-04  1.20E-05  6.40E-04 -6.56E-05  3.59E-05 -1.76E-04  1.25E-04 -4.38E-05
         8.76E-06 -3.59E-05 -7.33E-06 -1.02E-05  2.27E-05  3.28E-03
 
 OM45
+        3.06E-05 -2.50E-05 -1.40E-05  2.77E-05  1.03E-05  5.79E-05 -1.66E-05 -1.30E-05  2.23E-05  3.45E-05 -6.85E-06  1.25E-04
          1.31E-04 -3.52E-05 -1.58E-05  4.34E-05  2.77E-05  7.79E-06  4.13E-05  2.34E-04 -1.62E-05  2.21E-05  1.12E-05  8.80E-06
         6.87E-06 -1.31E-05  2.77E-07  3.15E-06  2.70E-06 -3.78E-04  1.22E-03
 
 OM46
+        1.49E-05  3.56E-05 -2.07E-05  7.36E-06  4.89E-06  7.48E-05  2.68E-05  2.34E-05 -6.44E-05 -1.91E-05 -1.93E-06 -1.23E-04
         -1.10E-05  1.46E-04  1.86E-05 -6.36E-05 -1.06E-05  2.35E-05 -2.40E-05 -3.89E-05  2.77E-04  4.69E-05 -4.89E-05 -2.31E-05
         6.73E-05 -3.10E-05 -1.35E-05 -4.44E-05  8.85E-06  1.84E-04 -3.50E-04  1.56E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        3.08E-05 -2.77E-05 -8.87E-05 -8.52E-05 -1.61E-05  1.19E-07  4.51E-05  8.40E-06  2.44E-05  2.43E-05  2.37E-05  1.05E-04
         -1.24E-05  2.18E-06  1.07E-04  6.72E-05 -1.60E-04  2.83E-05 -3.95E-04  6.42E-05  3.76E-05  3.62E-04  1.86E-05  9.96E-06
         1.72E-04 -2.07E-05  1.40E-05 -6.73E-05  1.33E-05 -8.81E-04  1.96E-04  1.79E-05  1.52E-03
 
 OM48
+        2.22E-05 -1.36E-06 -1.03E-06 -3.47E-05  2.54E-05  1.83E-05  3.55E-05  3.23E-05  1.22E-04  1.43E-04  5.39E-05  5.61E-04
         -5.51E-05  2.84E-05 -9.70E-05  2.49E-04  1.45E-04  5.63E-05  4.58E-04 -3.61E-05 -2.39E-05 -3.86E-05  3.73E-04  2.99E-05
         2.79E-04 -3.20E-05  2.33E-05 -8.57E-05  2.63E-05  5.65E-04  8.13E-06 -2.64E-04  2.55E-04  1.45E-03
 
 OM55
+       -4.80E-05  2.33E-05 -6.16E-05  1.14E-05 -3.37E-05 -8.98E-05 -2.08E-05  1.21E-05  1.20E-04 -2.98E-05  1.09E-05 -6.56E-05
          2.45E-04 -7.31E-05  2.27E-05  3.98E-05  6.00E-05 -9.59E-06  2.69E-06  1.27E-04 -4.63E-05 -1.21E-05  9.37E-06  4.44E-05
        -5.43E-05  6.09E-06 -1.47E-05  2.41E-05  1.18E-05  9.09E-05 -2.95E-04  1.01E-04 -6.26E-05 -2.20E-05  1.97E-03
 
 OM56
+       -1.54E-05 -3.13E-05  5.77E-05  5.70E-06  9.65E-06  2.45E-05  4.10E-06  3.54E-06 -2.24E-05  1.01E-05  1.51E-05  5.44E-05
         -1.51E-04  2.00E-04 -1.28E-05 -3.07E-05  4.12E-05 -1.29E-05  1.13E-05 -2.33E-05  8.97E-05 -8.86E-06  2.14E-06  1.33E-05
         2.37E-05  5.07E-05  5.88E-06  1.14E-05  7.01E-06 -4.45E-05  1.08E-04 -2.44E-04  1.90E-05  5.24E-05 -5.87E-04  1.29E-03
 
 OM57
+       -4.15E-06 -1.32E-05 -1.57E-05  1.58E-05 -2.95E-05  2.94E-06 -1.58E-05 -4.44E-07  7.60E-05 -4.23E-05  2.01E-05 -5.08E-05
          1.62E-04 -3.37E-05  1.67E-04  4.37E-05  8.63E-06  4.79E-06  2.89E-05 -2.30E-04  3.73E-05  1.54E-05  2.88E-05 -1.52E-05
        -1.40E-05  1.52E-04 -4.63E-05  4.83E-06 -1.68E-06  1.17E-04 -3.35E-04  6.11E-05 -2.12E-04 -3.66E-05  2.42E-04 -2.19E-05
          1.13E-03
 
 OM58
+        7.93E-06 -1.43E-05 -3.59E-05 -2.50E-05  3.82E-05  2.95E-05 -1.77E-06 -1.08E-05  1.37E-04  4.78E-05  4.45E-05 -3.58E-05
          4.55E-04 -1.66E-04  1.09E-04  1.82E-04  1.89E-05  2.61E-05 -2.81E-05  3.12E-04 -8.47E-05  5.02E-05  6.26E-05  1.96E-05
        -2.36E-05  2.12E-04 -6.56E-05  1.93E-05  1.15E-05 -5.94E-05  2.14E-04 -7.85E-06 -1.09E-05 -1.47E-04  7.01E-05 -2.68E-04
          2.89E-04  1.12E-03
 
 OM66
+        2.32E-05  5.52E-05 -8.79E-06  3.07E-05  7.91E-05  1.67E-05 -3.52E-06 -4.27E-06  7.71E-05  2.37E-06  1.35E-05 -5.69E-05
          8.87E-05 -2.17E-04  4.43E-06  3.55E-05  1.48E-05 -1.87E-06  1.55E-05  2.50E-06 -4.07E-05 -8.72E-06  5.63E-05  7.61E-06
        -1.16E-06 -9.31E-06  1.40E-04 -1.14E-06 -4.53E-06  1.10E-04 -6.61E-05  2.19E-04 -3.37E-05 -5.06E-05  1.70E-04 -7.78E-04
          7.46E-06  1.45E-04  3.20E-03
 
 OM67
+        1.23E-05 -4.33E-05  5.33E-05 -3.15E-06  5.04E-06 -4.50E-05  3.26E-05  2.10E-05  1.41E-05  3.23E-05  1.11E-05  2.10E-05
         -3.34E-05  1.67E-04 -1.14E-04 -5.69E-05 -1.96E-05 -1.78E-05  1.13E-05  6.24E-05 -3.53E-04 -3.88E-05  5.27E-05  4.24E-05
        -2.76E-05 -1.57E-05  1.46E-04  7.42E-05 -1.82E-05 -3.15E-05  7.25E-05 -4.24E-04  2.43E-05  8.92E-05 -4.72E-05  1.68E-04
         -3.27E-04 -1.30E-04 -4.21E-05  1.38E-03
 
 OM68
+        6.26E-06 -6.22E-05 -2.57E-06 -2.04E-05 -1.31E-06  2.88E-05  1.58E-05 -5.71E-06 -7.77E-05 -3.35E-05 -2.61E-05  1.90E-05
         -1.25E-04  5.63E-04 -2.34E-05 -2.07E-04 -2.18E-05  1.41E-05  1.47E-05 -7.53E-05  4.06E-04  1.72E-05 -7.62E-05  3.08E-05
         1.74E-06 -6.66E-05  2.56E-04  2.01E-05 -6.62E-06  1.91E-05 -3.83E-05  2.31E-04  4.86E-05  4.79E-05 -5.89E-05  1.14E-04
         -1.14E-04 -3.43E-04 -6.25E-04  3.24E-04  1.41E-03
 
 OM77
+        4.07E-05 -5.96E-05  4.12E-05  8.95E-05  1.75E-05  2.57E-05 -6.67E-05  9.10E-06  9.31E-05 -9.47E-05  3.42E-05 -9.01E-05
          6.94E-05 -2.91E-05  3.07E-04  8.80E-05  2.09E-04 -6.04E-05  1.83E-04 -5.74E-05 -1.97E-05 -5.65E-04 -9.58E-05  7.38E-05
        -8.19E-05  3.12E-05 -1.95E-05  3.08E-04  9.05E-05  3.27E-04 -7.66E-05 -3.93E-05 -7.97E-04 -1.73E-04  8.74E-05 -3.29E-05
          2.90E-04  9.50E-05  1.02E-04  2.26E-05 -5.00E-05  2.62E-03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        2.98E-06 -3.86E-05 -2.51E-05  1.20E-05  2.82E-05 -2.16E-05 -6.29E-06  4.60E-05  1.39E-04 -8.05E-05  9.33E-05 -1.22E-04
          5.85E-05 -5.57E-05  5.30E-04  2.74E-04 -1.03E-04 -1.29E-05 -1.61E-04  3.94E-05  8.53E-05  2.52E-04 -1.79E-04  6.50E-05
        -5.54E-05  3.22E-05 -2.47E-05  2.82E-04  2.35E-04 -1.40E-04  4.62E-05  6.30E-05  1.40E-04 -3.25E-04  2.43E-05 -4.71E-05
          6.36E-05  1.51E-04  7.11E-06 -2.90E-04 -6.08E-05  6.48E-04  1.36E-03
 
 OM88
+        2.22E-05  3.13E-05  3.08E-05  7.27E-05  4.95E-05  2.07E-05 -1.08E-05  8.04E-05  4.38E-04  2.66E-04  2.09E-04  2.19E-04
          5.09E-05 -2.30E-04  2.23E-04  9.90E-04  2.61E-04  1.28E-04  1.72E-04  2.99E-05 -1.21E-04  1.57E-04  7.37E-04  1.20E-04
         8.53E-05  1.91E-05 -1.07E-04  1.26E-04  5.10E-04  2.05E-04  2.76E-05 -1.13E-04  9.70E-05  5.28E-04  7.24E-05 -1.74E-05
          3.87E-05  9.32E-05  1.52E-04 -1.69E-04 -5.41E-04  2.26E-04  6.32E-04  2.58E-03
 
 SG11
+        1.81E-07 -6.39E-07 -2.37E-07 -1.02E-06 -4.59E-07  4.02E-07  9.92E-07 -4.64E-07  2.47E-07  3.87E-07  1.91E-07 -1.70E-07
          7.70E-08  1.40E-07 -3.95E-07  2.35E-07 -1.95E-07 -6.55E-07  5.19E-08  3.05E-07 -1.88E-07 -2.68E-07  1.50E-07  7.75E-08
        -5.18E-07  1.35E-07 -4.73E-07  6.89E-07 -7.77E-08 -7.36E-07  3.94E-08  3.48E-07 -5.27E-07 -4.05E-07  1.20E-08 -4.55E-08
          1.53E-07  3.07E-07  4.16E-07  3.99E-08  1.16E-08  3.65E-07 -3.15E-07  5.24E-08  3.89E-07
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.23E-06  1.05E-06 -1.87E-06  2.41E-07  3.35E-07 -1.75E-06 -1.59E-06 -3.16E-07  2.32E-07  3.45E-07  4.82E-08 -2.79E-07
         -4.08E-07 -6.32E-07  2.24E-07  5.46E-07 -3.27E-07  4.01E-08 -1.02E-06  2.38E-07 -2.26E-07  1.28E-06  5.87E-08 -4.39E-08
         7.64E-08 -1.20E-06  4.12E-07 -5.61E-07 -8.16E-07 -5.77E-07  6.83E-08 -5.58E-07  4.09E-07 -4.23E-07 -6.68E-07 -1.51E-07
         -4.73E-07 -1.52E-07 -2.61E-06  7.33E-08  8.05E-07 -1.13E-06  9.68E-08 -5.14E-07 -2.19E-08  0.00E+00  1.40E-06
 
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
+        7.53E-02
 
 TH 2
+       -1.33E-01  7.42E-02
 
 TH 3
+        1.66E-01 -1.67E-02  5.90E-02
 
 TH 4
+        1.13E-01  2.08E-01  2.65E-02  7.38E-02
 
 TH 5
+        9.41E-02  5.36E-02  5.70E-03 -1.15E-01  6.60E-02
 
 TH 6
+       -7.88E-02 -3.82E-02  3.54E-02  5.88E-02 -2.41E-01  7.52E-02
 
 TH 7
+        1.20E-01 -2.22E-01  1.62E-01 -2.74E-01  1.23E-01  1.55E-02  6.97E-02
 
 TH 8
+        3.49E-01  2.37E-01  2.40E-01  1.93E-01  4.73E-02 -2.05E-01  2.45E-01  7.04E-02
 
 OM11
+       -1.41E-02  3.25E-04 -1.79E-02 -5.09E-03  4.16E-03  5.92E-03 -2.61E-04 -3.42E-04  5.91E-02
 
 OM12
+        1.02E-02  5.67E-02 -3.12E-03  1.21E-02 -5.28E-03 -1.60E-02  1.44E-03 -3.56E-03 -2.01E-01  3.96E-02
 
 OM13
+        1.13E-03  1.47E-02  3.86E-02  1.17E-02  1.38E-02 -9.99E-03 -1.22E-02  2.32E-02  2.55E-01 -7.29E-02  3.21E-02
 
 OM14
+        2.05E-02 -1.70E-02 -2.69E-02 -1.57E-03 -2.23E-03 -1.76E-02  3.17E-02  2.26E-02  1.31E-01  2.05E-01 -4.76E-03  4.00E-02
 
 OM15
+       -1.61E-02  1.87E-02 -8.47E-03 -2.08E-03  6.62E-03  2.69E-02 -5.92E-04  1.02E-02  1.55E-01  2.91E-02  2.80E-02 -1.33E-01
          3.66E-02
 
 OM16
+        1.30E-02  3.59E-03 -5.29E-03  4.21E-03  1.36E-02  1.13E-02 -4.29E-03 -3.34E-03 -1.11E-01 -2.92E-02  2.03E-02  4.06E-02
         -2.51E-01  3.98E-02
 
 OM17
+        6.71E-03 -1.31E-02  5.31E-03 -2.35E-04  1.75E-02  2.24E-03 -9.52E-03  1.57E-03  1.77E-01 -2.60E-01  1.79E-01 -2.67E-01
          1.35E-01 -3.35E-03  3.92E-02
 
 OM18
+        1.28E-02 -1.08E-02 -1.49E-02 -1.33E-02  1.77E-03  2.52E-03 -7.76E-03 -2.09E-03  4.87E-01  1.96E-01  2.76E-01  2.03E-01
          8.46E-02 -2.35E-01  2.74E-01  4.07E-02
 
 OM22
+       -1.28E-02 -1.69E-01  1.39E-02  1.66E-03  6.93E-03  3.05E-02  2.20E-02  1.81E-02  5.85E-02 -2.81E-01  1.95E-02 -5.21E-02
         -3.48E-03  1.50E-02  6.28E-02 -3.24E-02  5.34E-02
 
 OM23
+       -1.47E-02  7.67E-02  3.23E-02  2.71E-02 -7.88E-03 -9.57E-03 -5.33E-03 -5.26E-03 -5.17E-02  2.04E-01 -1.12E-01  5.72E-02
         -8.17E-03 -8.31E-03 -6.66E-02  7.14E-04 -8.69E-02  2.85E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+       -7.33E-04 -1.19E-01  2.58E-02  5.82E-03  9.91E-03  1.25E-02  3.67E-03  1.02E-02 -1.03E-02 -9.46E-03  2.09E-02 -1.15E-01
          3.55E-02 -1.37E-02  2.06E-02  2.96E-03  3.70E-01 -5.95E-02  3.93E-02
 
 OM25
+       -7.42E-04  6.39E-03 -1.80E-02  5.84E-03  1.51E-02  6.58E-03  8.91E-03 -5.58E-03 -1.66E-02  9.83E-02 -5.87E-03  5.98E-02
         -1.42E-01  2.04E-02 -5.11E-02  2.75E-02  6.08E-02  3.10E-02 -1.43E-01  3.27E-02
 
 OM26
+       -1.53E-02 -2.48E-03  1.12E-03 -3.58E-03 -5.56E-03  3.85E-03 -6.16E-03 -3.10E-03  3.62E-02 -7.45E-02 -1.00E-03 -1.21E-02
          3.21E-02 -1.40E-01  3.18E-02  3.12E-02 -5.43E-04  4.89E-02  4.24E-02 -2.62E-01  3.75E-02
 
 OM27
+       -7.02E-03  1.76E-01 -3.67E-02  7.16E-04 -1.44E-02 -1.42E-02 -1.17E-03 -1.75E-02 -3.48E-02  2.00E-01 -4.00E-02  6.67E-02
         -2.05E-02  1.52E-02 -1.66E-01 -2.45E-02 -3.75E-01  1.97E-01 -3.94E-01  1.28E-01 -8.06E-03  3.87E-02
 
 OM28
+       -5.58E-03  1.70E-02  6.83E-03  1.66E-02  2.78E-03  4.26E-04  1.43E-02  4.73E-04 -6.77E-02  2.91E-01 -4.63E-02  6.34E-02
          9.77E-03  3.51E-02 -1.25E-01 -4.84E-02  3.62E-01  2.16E-01  2.02E-01  5.99E-02 -2.33E-01  2.03E-01  3.66E-02
 
 OM33
+        3.00E-02  1.23E-02 -6.49E-02 -2.82E-02 -3.97E-03 -5.24E-03  9.71E-04  1.36E-02  3.73E-02 -4.03E-03  2.77E-01  2.52E-02
          1.63E-02  1.04E-02  5.00E-02  6.31E-02  4.13E-03 -1.48E-02 -2.55E-03  1.55E-02  4.22E-03  9.84E-04  6.00E-03  3.45E-02
 
 OM34
+       -1.71E-02  1.70E-02  1.11E-01  3.26E-02  4.99E-03 -3.08E-03 -9.89E-04  2.31E-02  1.71E-02  3.05E-02  1.65E-01  1.56E-01
         -1.69E-02  5.66E-03 -1.89E-02  4.74E-02  3.13E-03  2.32E-01  7.25E-03 -5.33E-03 -6.51E-03  7.10E-03  5.22E-02 -6.88E-02
         3.17E-02
 
 OM35
+       -6.40E-03 -1.03E-03 -3.52E-02 -1.54E-02  3.13E-03  2.89E-03  4.72E-03  4.44E-03  2.50E-02 -2.94E-02  1.08E-01 -3.59E-02
          1.66E-01 -2.18E-02  4.50E-02  2.32E-02  9.95E-03  6.34E-02  7.69E-03 -2.35E-02 -1.80E-02  7.51E-03  8.31E-03  2.86E-02
        -1.52E-01  2.75E-02
 
 OM36
+        2.89E-02  6.32E-03  2.40E-02  4.53E-03 -1.43E-03 -7.37E-03 -2.12E-03  1.26E-02 -1.27E-02  5.00E-03 -6.48E-02  9.31E-03
         -3.04E-02  1.83E-01 -3.46E-03 -4.45E-02 -1.38E-02  7.67E-03 -1.03E-03  1.41E-02 -2.04E-02  1.96E-02  1.46E-03  6.67E-02
         7.03E-02 -2.84E-01  2.93E-02
 
 OM37
+        1.95E-02 -1.36E-02 -3.14E-02 -1.64E-02  1.65E-03 -5.14E-03 -1.15E-02  3.92E-03  2.94E-02 -4.87E-02  9.62E-02 -6.43E-02
          2.45E-02  5.24E-03  1.83E-01  6.73E-02  8.70E-03 -2.38E-01 -5.60E-04 -4.02E-03  6.89E-03 -4.03E-02 -6.01E-02  2.18E-01
        -3.13E-01  1.17E-01 -1.36E-02  2.96E-02
 
 OM38
+       -8.60E-03  1.37E-02  3.54E-02  3.00E-02 -1.26E-03  4.72E-04 -1.44E-02  1.71E-02  8.25E-02  2.36E-02  4.19E-01  2.96E-02
          2.74E-03 -2.75E-02  9.44E-02  2.43E-01  1.99E-02  3.04E-01  1.34E-02 -1.04E-03  1.57E-02  2.14E-02  4.77E-02  3.40E-01
         2.02E-01  2.99E-02 -1.88E-01  2.39E-01  2.98E-02
 
 OM44
+       -1.84E-02  1.59E-02  7.82E-02  3.57E-02  2.15E-02  1.01E-02 -1.67E-03  3.02E-03  2.50E-02  1.84E-02  8.73E-03  1.43E-01
         -1.79E-02  1.23E-03 -2.35E-02  3.11E-02  6.22E-02  7.37E-03  2.84E-01 -3.50E-02  1.67E-02 -7.91E-02  5.98E-02 -2.22E-02
         4.81E-03 -2.28E-02 -4.37E-03 -6.03E-03  1.33E-02  5.73E-02
 
 OM45
+        1.17E-02 -9.69E-03 -6.80E-03  1.08E-02  4.47E-03  2.21E-02 -6.84E-03 -5.31E-03  1.08E-02  2.50E-02 -6.11E-03  8.99E-02
          1.03E-01 -2.54E-02 -1.16E-02  3.06E-02  1.49E-02  7.85E-03  3.01E-02  2.05E-01 -1.24E-02  1.64E-02  8.78E-03  7.33E-03
         6.21E-03 -1.36E-02  2.71E-04  3.06E-03  2.60E-03 -1.89E-01  3.49E-02
 
 OM46
+        5.01E-03  1.21E-02 -8.89E-03  2.53E-03  1.87E-03  2.52E-02  9.71E-03  8.40E-03 -2.76E-02 -1.22E-02 -1.52E-03 -7.77E-02
         -7.61E-03  9.29E-02  1.21E-02 -3.95E-02 -5.05E-03  2.09E-02 -1.55E-02 -3.01E-02  1.87E-01  3.07E-02 -3.39E-02 -1.69E-02
         5.37E-02 -2.85E-02 -1.17E-02 -3.80E-02  7.51E-03  8.12E-02 -2.54E-01  3.95E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+        1.05E-02 -9.60E-03 -3.86E-02 -2.97E-02 -6.26E-03  4.06E-05  1.66E-02  3.06E-03  1.06E-02  1.58E-02  1.89E-02  6.75E-02
         -8.71E-03  1.41E-03  7.01E-02  4.24E-02 -7.67E-02  2.56E-02 -2.58E-01  5.04E-02  2.57E-02  2.40E-01  1.30E-02  7.43E-03
         1.39E-01 -1.93E-02  1.23E-02 -5.84E-02  1.15E-02 -3.95E-01  1.45E-01  1.16E-02  3.89E-02
 
 OM48
+        7.72E-03 -4.83E-04 -4.58E-04 -1.24E-02  1.01E-02  6.40E-03  1.34E-02  1.21E-02  5.40E-02  9.50E-02  4.40E-02  3.68E-01
         -3.95E-02  1.88E-02 -6.50E-02  1.61E-01  7.12E-02  5.19E-02  3.06E-01 -2.90E-02 -1.67E-02 -2.62E-02  2.68E-01  2.28E-02
         2.30E-01 -3.05E-02  2.09E-02 -7.60E-02  2.31E-02  2.59E-01  6.13E-03 -1.75E-01  1.72E-01  3.81E-02
 
 OM55
+       -1.43E-02  7.07E-03 -2.35E-02  3.48E-03 -1.15E-02 -2.69E-02 -6.73E-03  3.88E-03  4.56E-02 -1.69E-02  7.63E-03 -3.70E-02
          1.51E-01 -4.14E-02  1.30E-02  2.20E-02  2.53E-02 -7.58E-03  1.54E-03  8.72E-02 -2.78E-02 -7.04E-03  5.77E-03  2.90E-02
        -3.85E-02  4.99E-03 -1.13E-02  1.83E-02  8.91E-03  3.57E-02 -1.90E-01  5.73E-02 -3.62E-02 -1.30E-02  4.44E-02
 
 OM56
+       -5.67E-03 -1.17E-02  2.72E-02  2.15E-03  4.07E-03  9.05E-03  1.64E-03  1.40E-03 -1.05E-02  7.07E-03  1.31E-02  3.79E-02
         -1.15E-01  1.40E-01 -9.07E-03 -2.10E-02  2.14E-02 -1.26E-02  7.98E-03 -1.98E-02  6.65E-02 -6.37E-03  1.63E-03  1.07E-02
         2.07E-02  5.13E-02  5.59E-03  1.08E-02  6.53E-03 -2.16E-02  8.62E-02 -1.71E-01  1.36E-02  3.83E-02 -3.68E-01  3.60E-02
 
 OM57
+       -1.64E-03 -5.31E-03 -7.92E-03  6.36E-03 -1.33E-02  1.16E-03 -6.74E-03 -1.88E-04  3.83E-02 -3.18E-02  1.87E-02 -3.79E-02
          1.32E-01 -2.52E-02  1.27E-01  3.19E-02  4.81E-03  5.01E-03  2.19E-02 -2.09E-01  2.96E-02  1.19E-02  2.34E-02 -1.31E-02
        -1.31E-02  1.64E-01 -4.71E-02  4.86E-03 -1.68E-03  6.09E-02 -2.86E-01  4.61E-02 -1.62E-01 -2.86E-02  1.62E-01 -1.81E-02
          3.36E-02
 
 OM58
+        3.15E-03 -5.75E-03 -1.82E-02 -1.01E-02  1.73E-02  1.17E-02 -7.61E-04 -4.57E-03  6.93E-02  3.60E-02  4.14E-02 -2.68E-02
          3.72E-01 -1.25E-01  8.36E-02  1.33E-01  1.06E-02  2.74E-02 -2.14E-02  2.85E-01 -6.74E-02  3.87E-02  5.12E-02  1.70E-02
        -2.23E-02  2.30E-01 -6.70E-02  1.95E-02  1.15E-02 -3.10E-02  1.83E-01 -5.94E-03 -8.37E-03 -1.15E-01  4.72E-02 -2.23E-01
          2.57E-01  3.35E-02
 
 OM66
+        5.45E-03  1.32E-02 -2.64E-03  7.36E-03  2.12E-02  3.93E-03 -8.94E-04 -1.07E-03  2.31E-02  1.06E-03  7.41E-03 -2.52E-02
          4.29E-02 -9.66E-02  2.00E-03  1.54E-02  4.89E-03 -1.16E-03  6.96E-03  1.36E-03 -1.92E-02 -3.98E-03  2.72E-02  3.91E-03
        -6.48E-04 -5.99E-03  8.46E-02 -6.81E-04 -2.69E-03  3.41E-02 -3.35E-02  9.82E-02 -1.53E-02 -2.35E-02  6.76E-02 -3.83E-01
          3.93E-03  7.69E-02  5.65E-02
 
 OM67
+        4.37E-03 -1.57E-02  2.43E-02 -1.15E-03  2.05E-03 -1.61E-02  1.26E-02  8.02E-03  6.41E-03  2.19E-02  9.28E-03  1.41E-02
         -2.45E-02  1.13E-01 -7.81E-02 -3.75E-02 -9.86E-03 -1.68E-02  7.76E-03  5.13E-02 -2.53E-01 -2.69E-02  3.88E-02  3.31E-02
        -2.34E-02 -1.54E-02  1.34E-01  6.74E-02 -1.64E-02 -1.48E-02  5.59E-02 -2.88E-01  1.68E-02  6.30E-02 -2.86E-02  1.26E-01
         -2.62E-01 -1.04E-01 -2.00E-02  3.72E-02
 
 OM68
+        2.21E-03 -2.23E-02 -1.16E-03 -7.36E-03 -5.30E-04  1.02E-02  6.05E-03 -2.16E-03 -3.50E-02 -2.25E-02 -2.16E-02  1.27E-02
         -9.11E-02  3.77E-01 -1.59E-02 -1.35E-01 -1.09E-02  1.32E-02  9.97E-03 -6.14E-02  2.88E-01  1.18E-02 -5.55E-02  2.38E-02
         1.46E-03 -6.46E-02  2.33E-01  1.81E-02 -5.91E-03  8.87E-03 -2.93E-02  1.56E-01  3.32E-02  3.35E-02 -3.53E-02  8.47E-02
         -9.04E-02 -2.73E-01 -2.95E-01  2.32E-01  3.75E-02
 
 OM77
+        1.06E-02 -1.57E-02  1.36E-02  2.37E-02  5.18E-03  6.68E-03 -1.87E-02  2.53E-03  3.08E-02 -4.67E-02  2.08E-02 -4.41E-02
          3.71E-02 -1.43E-02  1.53E-01  4.22E-02  7.65E-02 -4.14E-02  9.10E-02 -3.43E-02 -1.03E-02 -2.85E-01 -5.12E-02  4.18E-02
        -5.04E-02  2.22E-02 -1.30E-02  2.03E-01  5.93E-02  1.12E-01 -4.29E-02 -1.94E-02 -4.00E-01 -8.86E-02  3.85E-02 -1.79E-02
          1.69E-01  5.55E-02  3.51E-02  1.19E-02 -2.60E-02  5.12E-02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        1.07E-03 -1.41E-02 -1.15E-02  4.41E-03  1.16E-02 -7.76E-03 -2.44E-03  1.77E-02  6.37E-02 -5.50E-02  7.87E-02 -8.24E-02
          4.33E-02 -3.79E-02  3.67E-01  1.83E-01 -5.23E-02 -1.22E-02 -1.11E-01  3.26E-02  6.15E-02  1.76E-01 -1.32E-01  5.11E-02
        -4.72E-02  3.17E-02 -2.28E-02  2.58E-01  2.14E-01 -6.64E-02  3.59E-02  4.32E-02  9.73E-02 -2.31E-01  1.48E-02 -3.55E-02
          5.13E-02  1.22E-01  3.41E-03 -2.11E-01 -4.38E-02  3.43E-01  3.69E-02
 
 OM88
+        5.81E-03  8.30E-03  1.03E-02  1.94E-02  1.48E-02  5.40E-03 -3.04E-03  2.25E-02  1.46E-01  1.32E-01  1.28E-01  1.08E-01
          2.74E-02 -1.14E-01  1.12E-01  4.78E-01  9.60E-02  8.86E-02  8.60E-02  1.80E-02 -6.34E-02  7.96E-02  3.96E-01  6.85E-02
         5.29E-02  1.37E-02 -7.22E-02  8.39E-02  3.36E-01  7.02E-02  1.56E-02 -5.61E-02  4.90E-02  2.73E-01  3.21E-02 -9.55E-03
          2.27E-02  5.48E-02  5.29E-02 -8.92E-02 -2.83E-01  8.68E-02  3.37E-01  5.08E-02
 
 SG11
+        3.85E-03 -1.38E-02 -6.46E-03 -2.22E-02 -1.11E-02  8.57E-03  2.28E-02 -1.06E-02  6.69E-03  1.57E-02  9.54E-03 -6.80E-03
          3.38E-03  5.66E-03 -1.62E-02  9.24E-03 -5.87E-03 -3.69E-02  2.12E-03  1.50E-02 -8.02E-03 -1.11E-02  6.56E-03  3.61E-03
        -2.61E-02  7.86E-03 -2.59E-02  3.73E-02 -4.17E-03 -2.06E-02  1.81E-03  1.41E-02 -2.17E-02 -1.71E-02  4.35E-04 -2.03E-03
          7.32E-03  1.47E-02  1.18E-02  1.72E-03  4.94E-04  1.14E-02 -1.37E-02  1.65E-03  6.24E-04
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+        1.38E-02  1.20E-02 -2.69E-02  2.77E-03  4.30E-03 -1.96E-02 -1.93E-02 -3.80E-03  3.32E-03  7.37E-03  1.27E-03 -5.90E-03
         -9.44E-03 -1.34E-02  4.84E-03  1.13E-02 -5.18E-03  1.19E-03 -2.20E-02  6.18E-03 -5.10E-03  2.81E-02  1.36E-03 -1.08E-03
         2.04E-03 -3.69E-02  1.19E-02 -1.60E-02 -2.32E-02 -8.53E-03  1.66E-03 -1.20E-02  8.90E-03 -9.40E-03 -1.27E-02 -3.56E-03
         -1.19E-02 -3.85E-03 -3.91E-02  1.67E-03  1.82E-02 -1.88E-02  2.22E-03 -8.56E-03 -2.97E-02  0.00E+00  1.18E-03
 
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
+        2.22E+02
 
 TH 2
+        6.16E+01  2.50E+02
 
 TH 3
+       -2.36E+01  8.62E+00  3.26E+02
 
 TH 4
+       -2.29E+01 -2.41E+01  2.78E+00  2.26E+02
 
 TH 5
+       -2.78E+01 -3.33E+01  1.98E+00  2.00E+01  2.57E+02
 
 TH 6
+       -2.01E+00 -1.35E+01 -2.10E+01 -2.32E+01  5.57E+01  2.04E+02
 
 TH 7
+        7.59E+00  7.06E+01 -2.55E+01  7.41E+01 -3.56E+01 -3.21E+01  2.77E+02
 
 TH 8
+       -9.09E+01 -1.03E+02 -5.77E+01 -5.52E+01  2.42E+01  6.32E+01 -1.04E+02  3.24E+02
 
 OM11
+        5.54E+00 -4.56E+00  3.19E+00 -3.29E+00 -1.26E+00 -3.25E-01 -3.54E+00  1.87E-01  4.77E+02
 
 OM12
+        2.79E+00  7.56E-01 -3.79E+00 -8.51E+00 -1.83E+00  1.38E+00 -2.32E+00 -2.20E+00  3.01E+02  1.33E+03
 
 OM13
+        3.28E+00 -7.26E+00 -2.71E+01 -3.89E+00 -7.11E+00  6.72E+00  5.25E+00 -2.57E+00 -1.22E+02  2.77E+01  1.54E+03
 
 OM14
+       -2.33E+00  2.14E+01  2.59E+01 -5.16E+00  3.48E+00  7.45E+00 -1.11E+01 -1.62E+01 -8.48E+01 -1.23E+02  8.51E+01  1.00E+03
 
 OM15
+        7.25E+00 -5.29E+00  6.96E+00  1.33E+00 -6.31E+00 -1.60E+01 -1.40E+00 -1.14E+01 -1.44E+02 -2.38E+02  2.66E+01  1.32E+02
          1.17E+03
 
 OM16
+       -5.18E+00 -8.69E+00  6.98E+00  1.57E+00 -4.88E+00 -6.18E+00  2.97E+00  5.36E+00 -3.31E+01 -1.31E+02 -1.26E+02 -7.72E+01
          3.02E+02  1.01E+03
 
 OM17
+       -7.61E+00 -1.71E+01 -8.47E-01 -6.07E+00 -2.57E+00  4.26E+00 -8.53E+00  1.11E+01  6.08E+01  3.89E+02 -8.69E+01  3.11E+02
         -1.64E+02 -1.63E+02  1.17E+03
 
 OM18
+       -1.39E+01  2.10E+00  1.13E+01  1.76E+01  9.29E+00 -1.16E+00  7.69E+00  1.10E+01 -4.45E+02 -7.37E+02 -2.08E+02 -1.93E+02
          2.25E+02  3.69E+02 -5.39E+02  1.76E+03
 
 OM22
+        1.59E+01  4.42E+01  4.84E+00 -3.43E+00 -8.25E+00 -1.45E+01  8.69E+00 -2.82E+01  4.70E+01  4.86E+02  5.61E+01 -2.96E+01
         -9.43E+01 -1.01E+02  1.45E+02 -2.63E+02  8.19E+02
 
 OM23
+       -5.00E+00 -2.25E+01 -1.06E+01  4.50E-02  3.57E+00  6.55E+00 -4.51E+00  1.44E+01 -4.91E+01 -1.36E+02  5.54E+02  2.85E+01
          4.34E+01 -4.58E+01 -1.02E+02  8.71E+00  1.34E+02  2.06E+03
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM24
+        2.65E+00  3.28E+01  1.04E+01 -2.68E+00 -1.53E+00  7.26E-01  6.93E+00 -1.33E+01 -2.22E+01 -1.28E+02  1.56E+01  3.25E+02
          6.25E+01  5.99E+00  8.53E+01 -6.00E+00 -1.54E+02  9.00E+01  1.22E+03
 
 OM25
+        5.99E+00  2.04E+00  7.61E+00 -8.24E+00 -5.98E+00 -4.23E+00 -6.87E+00 -9.22E-01 -5.28E+01 -2.91E+02 -4.29E+01  3.75E+01
          4.73E+02  2.05E+02 -1.18E+02  1.98E+02 -3.08E+02 -9.06E+01  1.88E+02  1.63E+03
 
 OM26
+        7.01E+00 -7.63E+00 -7.09E+00 -3.46E+00  5.37E+00  8.62E+00 -3.21E+00  5.71E+00 -2.88E+01 -1.57E+02 -1.01E+02 -2.70E+01
          1.55E+02  4.00E+02 -8.71E+01  2.06E+02 -2.53E+02 -2.84E+02 -8.11E+01  5.30E+02  1.35E+03
 
 OM27
+       -2.10E+01 -6.92E+01  1.47E+01  4.43E+00  1.53E+01  1.47E+00 -2.10E+01  3.34E+01  1.13E+01  2.31E+02  3.23E+00  1.10E+02
         -8.54E+01 -1.08E+02  3.92E+02 -2.25E+02  5.01E+02 -6.83E+01  4.13E+02 -3.14E+02 -3.01E+02  1.58E+03
 
 OM28
+       -1.91E+00 -2.00E+01 -7.77E+00 -3.68E+00  9.85E+00  1.81E+01 -1.44E+01  2.41E+01 -1.46E+02 -8.63E+02 -1.50E+02 -3.41E+01
          1.84E+02  2.61E+02 -3.58E+02  8.80E+02 -8.84E+02 -4.58E+02 -2.20E+02  4.44E+02  7.25E+02 -9.77E+02  2.58E+03
 
 OM33
+       -1.92E+01 -9.41E+00  4.53E+01  2.11E+01  6.99E+00 -3.51E+00 -2.41E+00 -6.52E+00  5.23E+00 -1.90E+01 -1.94E+02 -4.44E+01
         -3.18E+00  2.65E+01 -2.84E+01  6.66E+01  2.62E+00  9.59E+01  1.19E+01 -4.70E+00 -2.20E+01 -1.35E+01 -2.33E+01  1.07E+03
 
 OM34
+        1.73E+01  6.13E-01 -6.34E+01 -9.57E+00 -5.05E+00  4.81E+00  7.78E+00 -2.19E-01  1.67E+01  4.18E+00 -2.16E+02 -1.02E+02
         -2.17E+01  3.59E+01 -3.37E+01  6.19E+01 -1.17E+01 -1.52E+02  3.99E+01  2.65E+01  3.37E+01  3.81E+01  1.32E+01  1.79E+02
         1.45E+03
 
 OM35
+        2.14E+00  6.29E-01  1.33E+01  3.54E+00  3.19E+00  2.21E-01 -2.03E+00 -9.19E+00  3.61E+01  5.68E+01 -2.86E+02 -2.50E+01
         -1.17E+02 -6.78E+00  3.58E+01  1.74E+01 -3.96E+01 -3.98E+02 -3.71E+01  1.06E+02  1.39E+02 -2.90E+01  1.26E+02 -2.66E+01
         1.88E+02  1.75E+03
 
 OM36
+       -9.67E+00 -5.11E+00 -1.54E+01 -2.80E+00  8.90E+00  6.52E+00  2.19E+00  2.18E-01  1.80E+01  2.78E+01 -8.48E+01  1.85E+00
         -4.94E+01 -9.10E+01  2.22E+01 -4.16E+01 -2.92E+01 -2.96E+02 -5.06E+01  3.43E+01  1.55E+02 -3.80E+01  1.27E+02 -2.19E+02
        -1.74E+02  5.24E+02  1.61E+03
 
 OM37
+       -8.29E+00 -4.75E+00 -5.42E+00  1.03E+01  9.04E-01  2.12E+00  1.00E+01  1.49E+00 -1.27E+01 -4.55E+01  1.77E+02 -3.04E+00
          1.44E+01 -1.50E+01 -1.41E+02  2.98E+01  4.32E+01  6.19E+02  6.06E+01 -3.14E+01 -1.24E+02  2.74E+01 -1.54E+02 -7.72E+01
         4.77E+02 -2.46E+02 -2.14E+02  1.77E+03
 
 OM38
+        1.29E+01  6.02E+00 -1.62E+01 -1.97E+01  4.50E+00 -1.55E-01  2.80E-01  4.54E-01  8.44E+01  6.83E+01 -7.70E+02 -1.76E+01
         -1.65E+01  4.95E+01  1.25E+02 -7.10E+01 -9.78E+01 -1.10E+03 -9.62E+01  7.56E+01  2.24E+02 -9.28E+00  3.51E+02 -4.71E+02
        -4.22E+02  3.35E+02  6.42E+02 -7.70E+02  2.61E+03
 
 OM44
+        7.86E+00 -5.50E+00 -3.33E+01 -1.15E+01 -7.44E+00 -1.21E+00 -3.79E-01  9.51E+00 -7.55E+00 -1.47E+01 -8.94E+00 -9.08E+01
          1.55E+00  2.55E+01 -5.41E+01  5.45E+01 -1.73E+01 -5.38E+00 -9.83E+01 -9.87E-01  1.38E+01 -6.57E+01  6.94E+01  2.27E+01
         4.49E+01  3.07E+01  9.38E+00  1.51E+01 -1.32E+01  4.62E+02
 
 OM45
+       -4.93E+00 -3.93E+00 -5.48E+00 -9.14E+00 -8.25E-01 -8.69E+00  3.20E+00  5.01E+00  1.89E+01  4.54E+01 -7.15E+00 -1.62E+02
         -1.54E+02 -2.41E+01 -3.01E+01  1.10E+01  1.63E+01 -3.02E+01 -2.03E+02 -1.68E+02 -2.65E+01 -6.41E+01  3.24E+01  5.06E+00
         2.07E+00  7.60E+01  4.39E+01 -1.53E+01  2.95E+01  1.07E+02  1.19E+03
 
 OM46
+       -5.55E+00 -3.18E+00  1.39E+01  2.04E+00 -1.09E-01 -1.13E+01 -5.56E+00 -8.43E+00  1.74E+01  2.24E+01 -6.10E+00 -1.05E+01
         -4.68E+01 -8.74E+01  8.49E+00 -3.96E+01  1.43E+01 -1.60E+01 -9.78E+01 -5.50E+01 -5.63E+01 -4.51E+01 -5.97E+00 -1.47E+01
        -1.25E+02  3.16E+01  8.85E+01 -3.77E+01  6.66E+01 -8.40E+01  2.77E+02  9.23E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM47
+       -1.47E+00  1.88E+01  4.09E+00  6.79E-01  3.60E-01 -4.17E+00  3.51E+00 -4.24E+00 -1.73E+01 -6.11E+01  2.06E+01  4.17E+01
          4.35E+01  3.65E+01 -1.02E+02  5.90E+01 -5.31E+01  6.06E+01  3.14E+02  7.61E+01 -3.24E+01 -4.71E+00 -1.61E+01 -6.55E+00
        -7.80E+01 -9.45E+00  8.49E+00  4.77E+01 -8.41E+00  3.06E+02 -1.10E+02 -1.63E+02  1.28E+03
 
 OM48
+       -6.86E+00 -1.56E+01  2.29E+01  1.78E+01 -1.44E+00 -8.94E+00 -1.91E+00 -4.80E+00  5.52E+01  1.35E+02 -3.87E+01 -4.23E+02
         -8.61E+01 -3.13E+01 -8.80E+01 -7.56E+01  1.09E+02 -5.15E+01 -5.31E+02 -1.14E+02 -2.01E+00 -1.17E+02 -8.77E+01 -5.98E+01
        -2.93E+02 -1.32E+01  6.74E+01 -1.31E+02  2.37E+02 -2.22E+02  1.24E+02  3.14E+02 -5.45E+02  1.55E+03
 
 OM55
+        3.96E+00 -9.27E-01  2.16E+00 -1.55E+00  4.71E+00  1.07E+01  1.33E+00  1.91E-01 -5.56E-01  4.78E+01  9.64E+00 -1.87E+00
         -2.02E+02 -8.46E+01  5.08E+01 -4.51E+01  2.46E+01  1.15E+01 -2.50E+01 -2.76E+02 -1.14E+02  4.98E+01 -6.31E+01 -2.67E+01
         1.56E+01 -1.21E+01 -8.36E+00  1.72E+00 -9.17E+00 -6.41E+00  1.27E+02  2.28E+01 -3.03E+01  2.97E+01  6.93E+02
 
 OM56
+        5.97E+00  5.38E+00 -1.02E+01 -2.97E-01 -1.15E+01 -5.57E+00  4.48E+00 -2.99E+00 -5.37E+00  2.91E+01  4.95E+01  6.77E+00
         -1.21E+02 -2.45E+02  3.96E+01 -8.97E+01  4.91E+01  1.10E+02  5.68E-01 -3.03E+02 -3.55E+02  8.42E+01 -1.96E+02 -2.12E+01
        -4.72E+01 -2.06E+02 -1.15E+02  3.88E+01 -8.72E+01 -2.34E+01 -6.89E+01  8.75E+01 -2.52E+01  5.04E+01  3.89E+02  1.33E+03
 
 OM57
+        3.56E+00  1.72E+01  2.71E+00 -7.43E+00  7.37E+00 -1.00E+00  1.26E+00 -7.78E+00 -2.16E+01 -7.88E+01  2.89E+00 -5.18E+01
          1.13E+02  7.54E+01 -2.06E+02  1.15E+02 -9.24E+01  5.86E+00 -2.02E+01  4.96E+02  2.12E+02 -2.74E+02  1.84E+02  3.02E+01
        -8.91E+00 -9.03E+01 -5.93E+00  1.64E+01 -6.55E-01  4.14E+01  3.66E+02  9.84E+01  9.55E+01 -1.40E+01 -2.13E+02 -2.84E+02
          1.46E+03
 
 OM58
+       -3.65E+00  8.48E+00 -5.60E-01  9.48E+00 -1.46E+01 -4.83E+00  8.23E+00 -1.37E+00  7.13E+01  2.18E+02  5.00E+01 -1.56E+01
         -6.09E+02 -2.63E+02  1.55E+02 -3.77E+02  1.96E+02  7.46E+01 -4.36E+01 -8.37E+02 -3.98E+02  2.34E+02 -5.18E+02 -2.88E+01
        -4.21E+01 -3.48E+02 -1.19E+02  5.76E+01 -7.20E+01 -4.73E+01 -2.55E+02 -4.01E+01 -5.81E+01  1.55E+02  2.45E+02  5.54E+02
         -6.57E+02  1.92E+03
 
 OM66
+       -1.30E-01  1.73E+00  1.06E+00 -1.22E+00 -1.12E+01 -5.23E+00  1.62E+00  3.18E-01 -1.09E+01  1.31E+01  2.48E+01  1.14E+01
         -3.33E+01 -7.62E+01  1.33E+01 -2.33E+01  3.11E+01  6.54E+01  1.33E+01 -8.13E+01 -1.75E+02  3.95E+01 -1.19E+02  9.89E+00
         1.14E+01 -8.30E+01 -1.77E+02  3.77E+01 -9.54E+01 -1.52E+01 -2.45E+01 -8.01E+01 -8.13E+00 -1.29E+01  7.14E+01  3.35E+02
         -6.10E+01  1.57E+02  4.62E+02
 
 OM67
+        8.73E+00  1.28E+01 -9.93E+00 -5.19E+00  3.78E+00  9.01E+00 -5.66E+00 -8.51E+00 -1.47E+01 -4.41E+01 -5.14E+01 -5.03E+00
          3.28E+01  1.11E+02 -4.84E+01  5.83E+01 -6.54E+01 -9.37E+01 -7.43E+01  1.82E+02  4.79E+02 -1.94E+02  2.55E+02 -1.05E+00
        -1.51E+01  2.37E+01 -9.83E-01 -1.65E+02  8.64E+01 -1.58E+01  1.19E+02  3.26E+02 -1.25E+02  1.30E+02 -8.92E+01 -2.63E+02
          4.18E+02 -2.46E+02 -1.49E+02  1.23E+03
 
 OM68
+        2.30E+00  1.90E+01  3.21E+00  2.14E+00 -1.48E+01 -1.32E+01  7.38E+00 -8.78E+00  1.05E+01  1.30E+02  1.49E+02  4.51E+01
         -1.96E+02 -5.61E+02  1.08E+02 -2.78E+02  1.75E+02  2.17E+02  7.22E+01 -3.44E+02 -8.03E+02  2.25E+02 -6.13E+02  3.49E+01
         7.39E+01 -1.85E+02 -4.25E+02  1.46E+02 -4.13E+02 -1.00E+01 -8.85E+01 -2.58E+02  4.03E+01 -2.00E+02  1.23E+02  4.06E+02
         -2.26E+02  6.62E+02  3.79E+02 -6.02E+02  1.80E+03
 
 OM77
+       -1.17E+01 -1.78E+01  4.57E-01 -3.09E+00  2.75E+00 -3.12E+00 -1.94E-01  1.37E+01 -6.33E+00  2.24E+01  5.54E+00  2.77E+01
         -1.49E+01 -1.47E+01  6.94E+01 -2.41E+01  6.24E+01 -3.61E+01  1.33E+02 -5.45E+01 -6.91E+01  3.90E+02 -1.97E+02 -2.89E+01
        -4.03E+01  2.17E+01  2.78E+01 -9.00E+01  6.13E+01  2.81E+01 -5.27E+01 -4.61E+01  3.86E+02 -1.50E+02  7.36E+00  2.46E+01
         -1.80E+02  6.54E+01 -4.94E+00 -1.72E+02  7.66E+01  6.97E+02
 
1

            TH 1      TH 2      TH 3      TH 4      TH 5      TH 6      TH 7      TH 8      OM11      OM12      OM13      OM14  
             OM15      OM16      OM17      OM18      OM22      OM23      OM24      OM25      OM26      OM27      OM28      OM33  
            OM34      OM35      OM36      OM37      OM38      OM44      OM45      OM46      OM47      OM48      OM55      OM56  
             OM57      OM58      OM66      OM67      OM68      OM77      OM78      OM88      SG11      SG12      SG22  
 
 OM78
+        2.14E+01  4.20E+01  7.49E+00  1.95E+00 -5.84E+00  6.06E+00  3.85E+00 -3.08E+01 -2.49E+01 -2.18E+02 -1.06E+01 -1.54E+02
          1.02E+02  1.33E+02 -5.53E+02  3.49E+02 -2.47E+02 -5.02E+01 -2.62E+02  1.74E+02  2.82E+02 -8.05E+02  8.92E+02  4.75E+01
        -5.29E+01  2.81E+01  6.30E+00 -2.68E+02 -7.86E+00 -3.61E+01  6.89E+01  1.40E+02 -4.13E+02  5.75E+02 -3.97E+01 -9.31E+01
          2.63E+02 -3.25E+02 -5.32E+01  4.70E+02 -4.50E+02 -5.63E+02  1.99E+03
 
 OM88
+        1.54E+00  2.26E+00 -6.54E+00 -6.98E+00 -1.25E+01 -1.01E+01  5.56E+00 -1.13E+01  9.04E+01  2.91E+02  1.59E+02  1.02E+02
         -9.25E+01 -1.91E+02  2.41E+02 -7.30E+02  2.34E+02  2.11E+02  1.23E+02 -1.54E+02 -3.06E+02  3.22E+02 -1.01E+03  4.05E+01
         8.82E+01 -7.45E+01 -1.25E+02  1.47E+02 -4.92E+02 -1.15E+01 -4.14E+01 -6.31E+01  8.15E+01 -3.46E+02  1.50E+01  9.54E+01
         -9.11E+01  2.98E+02  7.83E+01 -1.71E+02  5.97E+02  1.06E+02 -7.55E+02  1.20E+03
 
 SG11
+       -2.01E+02 -3.37E+01  1.24E+02  3.18E+02  3.95E+02 -3.58E+01 -5.66E+02  3.25E+02 -1.69E+02 -2.73E+02 -5.33E+02  3.11E+02
         -1.42E+02 -3.73E+02  1.00E+03 -6.10E+02  6.83E+02  1.47E+03 -1.73E+02 -8.91E+02  1.66E+02  6.42E+02 -8.64E+02  9.43E+01
         7.32E+01  5.96E+02  1.94E+03 -2.15E+03  8.29E+02  8.88E+02 -4.57E+01 -5.99E+02  9.66E+02  4.86E+02  1.16E+02 -3.78E+02
         -4.39E+02 -5.14E+02 -5.95E+02  1.59E+02 -9.30E+02 -7.21E+01  9.07E+02 -3.53E+02  2.60E+06
 
 SG12
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
        ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
         ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
 SG22
+       -2.53E+02 -1.08E+02  3.92E+02  2.55E+01 -6.19E+00  1.84E+02  1.70E+02  7.18E+01  3.15E+01 -2.12E+02 -5.75E+02  2.52E+02
          2.65E+02  5.79E+02 -1.95E+02 -2.90E+02 -4.05E+02 -4.38E+02  3.00E+02  2.62E+02  4.71E+02 -6.71E+02  2.94E+02 -1.95E+02
        -1.47E+02  1.25E+03  1.76E+02  3.41E+01  1.27E+03 -4.12E+01  1.47E+02  3.75E+02  1.38E+02  2.40E+02  3.02E+02  4.18E+02
          2.29E+02 -3.22E+02  5.72E+02  1.94E+02 -7.32E+02  1.72E+02  5.53E+01 -1.04E+02  3.85E+04  0.00E+00  7.24E+05
 
 Elapsed postprocess time in seconds:     0.01
 #CPUT: Total CPU Time in Seconds,     2880.668
Stop Time: 
Tue 04/19/2016 
08:15 PM

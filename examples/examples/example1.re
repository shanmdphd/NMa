Tue 08/05/2014 
07:20 PM
;Model Desc: Two compartment Model, Using ADVAN3, TRANS4
;Project Name: nm7examples
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# Example 1 (from samp5l)
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
$DATA example1.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4

;NTHETA=number of Thetas to be estimated
;NETA=number of Etas to be estimated (and to be described by NETAxBETA OMEGA matrix)
;NTHP=number of thetas which have a prior
;NETP=number of Omegas with prior
;Prior information is important for MCMC Bayesian analysis, not necessary for maximization
; methods
$PRIOR NWPRI NTHETA=4, NETA=4, NTHP=4, NETP=4

$PK
; The thetas are MU modeled.  Best that there is a linear relationship between THETAs and Mus
;  The linear MU modeling of THETAS allows them to be efficiently Gibbs sampled.
MU_1=THETA(1)
MU_2=THETA(2)
MU_3=THETA(3)
MU_4=THETA(4)
CL=DEXP(MU_1+ETA(1))
V1=DEXP(MU_2+ETA(2))
Q=DEXP(MU_3+ETA(3))
V2=DEXP(MU_4+ETA(4))
S1=V1

$ERROR
Y = F + F*EPS(1)

; Initial values of THETA
$THETA 
(0.001, 2.0) ;[LN(CL)]
(0.001, 2.0) ;[LN(V1)]
(0.001, 2.0) ;[LN(Q)]
(0.001, 2.0) ;[LN(V2)]
;INITIAL values of OMEGA
$OMEGA BLOCK(4)
0.15   ;[P]
0.01  ;[F]
0.15   ;[P]
0.01  ;[F]
0.01  ;[F]
0.15   ;[P]
0.01  ;[F]
0.01  ;[F]
0.01  ;[F]
0.15   ;[P]
;Initial value of SIGMA
$SIGMA 
(0.6 )   ;[P]

; Prior information of THETAS
$THETA (2.0 FIX) (2.0 FIX) (2.0 FIX) (2.0 FIX)

; Variance to prior information of THETAS.  Because variances are very large, this
; means that the prior information to the THETAS is highly uninformative.
$OMEGA BLOCK(4)
10000 FIX 
0.00 10000
0.00  0.00 10000
0.00  0.00 0.0 10000

; Prior information to the OMEGAS.
$OMEGA BLOCK(4)
0.2 FIX 
0.0  0.2 
0.0  0.0 0.2
0.0  0.0 0.0 0.2
;Degrees of freedom to prior OMEGA matrix.  Because degrees of freedom is very low, equal to the
; the dimension of the prior OMEGA, this means that the prior information to the OMEGAS is
; highly uninformative
$THETA (4 FIX)

; The first analysis is iterative two-stage, maximum of 500 iterations (NITER), iteration results
; are printed every 5 iterations, gradient precision (SIGL) is 4. Termination is tested on all of
; the population parameters (CTYPE=3), and for less then 2 significant digits change (NSIG).
; Prior information is not necessary for ITS, so NOPRIOR=1.  The intermediate and final results
; of the ITS method will be recoded in row/column format in example1.ext
$EST METHOD=ITS INTERACTION FILE=example1.ext NITER=500 PRINT=5 NOABORT SIGL=4 CTYPE=3 CITER=10   
     CALPHA=0.05 NOPRIOR=1 NSIG=2
; The results of ITS are used as the initial values for the SAEM method.  A maximum of 3000
; stochastic iterations (NBURN) is requested, but may end early if statistical test determines
; that variations in all parameters is stationary (note that any settings from the previous $EST
; carries over to the next $EST statement, within a $PROB).  The SAEM is a Monte Carlo process,
; so setting the SEED assures repeatability of results.  Each iteration obtains only 2 Monte
; Carlo samples ISAMPLE), so they are very fast.  But many iterations are needed, so PRINT only
; every 100th iteration.  After the stochastic phase, 500 accumulation iterations will be
; Performed (NITER), to obtain good parameters estimates with little stochastic noise.
; As a new FILE has not been given, the SAEM results will append to example1.ext.
$EST METHOD=SAEM INTERACTION NBURN=3000 NITER=500 PRINT=100 SEED=1556678 ISAMPLE=2
; After the SAEM method, obtain good estimates of the marginal density (objective function),
; along with good estimates of the standard errors.  This is best done with importance sampling
; (IMP), performing the expectation step only (EONLY=1), so that final population parameters
; remain at the final SAEM result.  Five iterations (NITER) should allow the importance sampling
; proposal density to become stationary.  This is observed by the objective function settling 
; to a particular value (with some stochastic noise).  By using 3000 Monte Carlo samples
; (ISAMPLE), this assures a precise assessment of standard errors.
$EST METHOD=IMP  INTERACTION EONLY=1 NITER=5 ISAMPLE=3000 PRINT=1 SIGL=8 NOPRIOR=1
; The Bayesian analysis is performed.  While 10000 burn-in
; iterations are requested as a maximum, because the termination test is on (CTYPE<>0, set at the
; first $EST statement), and because the initial parameters are at the SAEM result, which is the
; maximum likelihood position, the analysis should settle down to a stationary distribution in
; several hundred iterations.  Prior information is also used to facilitate Bayesian analysis.
; The individual Bayesian iteration results are important, and may be need for post-processing
; analysis. So specify a separate FILE for the Bayesian analysis. 
$EST METHOD=BAYES INTERACTION FILE=example1.txt NBURN=10000 NITER=10000 PRINT=100 NOPRIOR=0
; Just for old-times sake, let?s see what the traditional FOCE method will give us.  
; And, remember to introduce a new FILE, so its results won?t append to our Bayesian FILE. 
; Appending to example1.ext with the EM methods is fine.
$EST METHOD=COND INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 PRINT=5 NOABORT NOPRIOR=1
     FILE=example1.ext
; Time for the standard error results.  You may request a more precise gradient precision (SIGL)
; that differed from that used during estimation.
$COV MATRIX=R PRINT=E UNCONDITIONAL SIGL=12
; Print out results in tables. Include some of the new weighted residual types
$TABLE ID TIME PRED RES WRES CPRED CWRES EPRED ERES EWRES NOAPPEND ONEHEADER 
 FILE=example1.TAB NOPRINT
$TABLE ID CL V1 Q V2 FIRSTONLY NOAPPEND NOPRINT FILE=example1.PAR
$TABLE ID ETA1 ETA2 ETA3 ETA4 FIRSTONLY NOAPPEND NOPRINT FILE=example1.ETA

  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
 CREATING MUMODEL ROUTINE...
FSUBS
FSUBS_MU.F90
        1 file(s) copied.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:        5 AUG 2014
Days until program expires :5777
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.1.2
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.
 
 PROBLEM NO.:         1
 RUN# Example 1 (from samp5l)                                            
0DATA CHECKOUT RUN:              NO 
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO 
 NO. OF DATA RECS IN DATA SET:      600
 NO. OF DATA ITEMS IN DATA SET:  17
 ID DATA ITEM IS DATA ITEM NO.:   3
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.: 10
0INDICES PASSED TO SUBROUTINE PRED:
  9  5  7  8  0  0 11  0  0
  0  0
0LABELS FOR DATA ITEMS:
 C SET ID JID TIME CONC DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 CL      V1      Q       V2  
0FORMAT FOR DATA:
 (2E2.0,3E4.0,E11.0,E4.0,4E2.0,2E7.0,E8.0,E7.0,E2.0,E5.0)                        
 
 TOT. NO. OF OBS RECS:      500
 TOT. NO. OF INDIVIDUALS:    100
0LENGTH OF THETA:  9
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO 
0OMEGA HAS BLOCK FORM:
  1
  1  1
  1  1  1
  1  1  1  1
  0  0  0  0  2
  0  0  0  0  2  2
  0  0  0  0  2  2  2
  0  0  0  0  2  2  2  2
  0  0  0  0  0  0  0  0  3
  0  0  0  0  0  0  0  0  3  3
  0  0  0  0  0  0  0  0  3  3  3
  0  0  0  0  0  0  0  0  3  3  3  3
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO 
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:  1
0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO 
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
  0.1000E-02     0.2000E+01     0.1000E+07
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.2000E+01     0.2000E+01     0.2000E+01
  0.4000E+01     0.4000E+01     0.4000E+01
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO 
                  0.1500E+00
                  0.1000E-01   0.1500E+00
                  0.1000E-01   0.1000E-01   0.1500E+00
                  0.1000E-01   0.1000E-01   0.1000E-01   0.1500E+00
        2                                                                                  YES 
                  0.1000E+05
                  0.0000E+00   0.1000E+05
                  0.0000E+00   0.0000E+00   0.1000E+05
                  0.0000E+00   0.0000E+00   0.0000E+00   0.1000E+05
        3                                                                                  YES 
                  0.2000E+00
                  0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.2000E+00
                  0.0000E+00   0.0000E+00   0.0000E+00   0.2000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.6000E+00
0ESTIMATION STEP OMITTED:           NO 
 CONDITIONAL ESTIMATES USED:       YES 
 CENTERED ETA:                      NO 
 EPS-ETA INTERACTION:              YES 
 LAPLACIAN OBJ. FUNC.:              NO 
 NO. OF FUNCT. EVALS. ALLOWED:       2400
 NO. OF SIG. FIGURES REQUIRED:       2
 INTERMEDIATE PRINTOUT:            YES 
 ESTIMATE OUTPUT TO MSF:            NO 
 ABORT WITH PRED EXIT CODE 1:       NO 
 IND. OBJ. FUNC. VALUES SORTED:     NO 
0ESTIMATION STEP OMITTED:           NO 
 CONDITIONAL ESTIMATES USED:       YES 
 CENTERED ETA:                      NO 
 EPS-ETA INTERACTION:              YES 
 LAPLACIAN OBJ. FUNC.:              NO 
 NO. OF FUNCT. EVALS. ALLOWED:       2400
 NO. OF SIG. FIGURES REQUIRED:       2
 INTERMEDIATE PRINTOUT:            YES 
 ESTIMATE OUTPUT TO MSF:            NO 
 ABORT WITH PRED EXIT CODE 1:       NO 
 IND. OBJ. FUNC. VALUES SORTED:     NO 
0ESTIMATION STEP OMITTED:           NO 
 CONDITIONAL ESTIMATES USED:       YES 
 CENTERED ETA:                      NO 
 EPS-ETA INTERACTION:              YES 
 LAPLACIAN OBJ. FUNC.:              NO 
 NO. OF FUNCT. EVALS. ALLOWED:       2400
 NO. OF SIG. FIGURES REQUIRED:       2
 INTERMEDIATE PRINTOUT:            YES 
 ESTIMATE OUTPUT TO MSF:            NO 
 ABORT WITH PRED EXIT CODE 1:       NO 
 IND. OBJ. FUNC. VALUES SORTED:     NO 
0ESTIMATION STEP OMITTED:           NO 
 CONDITIONAL ESTIMATES USED:       YES 
 CENTERED ETA:                      NO 
 EPS-ETA INTERACTION:              YES 
 LAPLACIAN OBJ. FUNC.:              NO 
 NO. OF FUNCT. EVALS. ALLOWED:       2400
 NO. OF SIG. FIGURES REQUIRED:       2
 INTERMEDIATE PRINTOUT:            YES 
 ESTIMATE OUTPUT TO MSF:            NO 
 ABORT WITH PRED EXIT CODE 1:       NO 
 IND. OBJ. FUNC. VALUES SORTED:     NO 
0ESTIMATION STEP OMITTED:           NO 
 CONDITIONAL ESTIMATES USED:       YES 
 CENTERED ETA:                      NO 
 EPS-ETA INTERACTION:              YES 
 LAPLACIAN OBJ. FUNC.:              NO 
 NO. OF FUNCT. EVALS. ALLOWED:       9999
 NO. OF SIG. FIGURES REQUIRED:       3
 INTERMEDIATE PRINTOUT:            YES 
 ESTIMATE OUTPUT TO MSF:            NO 
 ABORT WITH PRED EXIT CODE 1:       NO 
 IND. OBJ. FUNC. VALUES SORTED:     NO 
0COVARIANCE STEP OMITTED:    NO 
 R MATRIX SUBSTITUTED:      YES 
 S MATRIX SUBSTITUTED:       NO 
 EIGENVLS. PRINTED:         YES 
 COMPRESSED FORMAT:          NO 
0TABLES STEP OMITTED:    NO 
 NO. OF TABLES:           3
0-- TABLE  1 --
04 COLUMNS APPENDED:     NO 
 PRINTED:                NO 
 HEADER:                YES 
 FILE TO BE FORWARDED:   NO 
0USER-CHOSEN ITEMS:
 ID TIME PRED RES WRES CPRED CWRES EPRED ERES EWRES
0-- TABLE  2 --
0FIRST RECORDS ONLY:    YES 
04 COLUMNS APPENDED:     NO 
 PRINTED:                NO 
 HEADER:                YES 
 FILE TO BE FORWARDED:   NO 
0USER-CHOSEN ITEMS:
 ID CL V1 Q V2
0-- TABLE  3 --
0FIRST RECORDS ONLY:    YES 
04 COLUMNS APPENDED:     NO 
 PRINTED:                NO 
 HEADER:                YES 
 FILE TO BE FORWARDED:   NO 
0USER-CHOSEN ITEMS:
 ID ETA1 ETA2 ETA3 ETA4
 
 THE FOLLOWING LABELS ARE EQUIVALENT
 PRED=PREDI
 RES=RESI
 WRES=WRESI
0
 PRIOR SUBROUTINE USER-SUPPLIED
1DOUBLE PRECISION PREDPP VERSION 7.1.2
 
 TWO COMPARTMENT MODEL (ADVAN3)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   4
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   BASIC PK PARAMETER NO.  1: ELIMINATION RATE (K)
   BASIC PK PARAMETER NO.  2: CENTRAL-TO-PERIPH. RATE (K12)
   BASIC PK PARAMETER NO.  3: PERIPH.-TO-CENTRAL RATE (K21)
 TRANSLATOR WILL CONVERT PARAMETERS 
 CL, V1, Q, V2 TO K, K12, K21 (TRANS4)
0COMPARTMENT ATTRIBUTES 
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         CENTRAL      ON         NO         YES        YES        YES
    2         PERIPH.      ON         NO         YES        NO         NO 
    3         OUTPUT       OFF        YES        NO         NO         NO 
Stop Time: 
Tue 08/05/2014 
07:20 PM

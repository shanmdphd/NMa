; Used for comparing single versus parallel computing for FOCE method.
;$SIZES LVR=30
;$SIZES LTH=15
;$SIZES LIM1=100
$PROB RUN# Example 1 (from samp5l)
$INPUT C SET ID JID TIME  DV=CONC AMT=DOSE RATE EVID MDV CMT CLX V1X QX V2X SDIX SDSX
$DATA example1.csv IGNORE=C

$SUBROUTINES ADVAN3 TRANS4

$THETAI
THETA=DLOG(THETAI)

$THETAR
THETAR=DEXP(THETA)


$PK
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
$THETA 7.389 7.389 7.389 7.389
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

$PRIOR NWPRI NTHETA=4, NETA=4, NTHP=4, NETP=4
; Prior information of THETAS
$THETA (7.389 FIX) (7.389  FIX) (7.389  FIX) (7.389  FIX)

; Variance to prior information of THETAS.  Because variances are very large, this
; means that the prior information to the THETAS is highly uninformative.
$OMEGA BLOCK(4)
545973 FIX 
0.00 545973
0.00  0.00 545973
0.00  0.00 0.0 545973

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


$EST METHOD=ITS INTERACTION NOABORT CTYPE=3 PRINT=5 NOPRIOR=1
$EST METHOD=BAYES INTERACTION NOABORT NBURN=200 NITER=500 CTYPE=3 PRINT=50 NOPRIOR=0
$EST METHOD=1 INTERACTION NSIG=3 SIGL=10 PRINT=1 NOABORT MAXEVAL=9999 NOPRIOR=1
$COV MATRIX=R PRINT=E UNCONDITIONAL

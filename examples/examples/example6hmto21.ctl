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


$PK
include nonmem_reserved_general
MUFIRSTREC=1
OBJQUICK=2
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


;Initial Thetas
$THETA
( 4.0 )  ;[MU_1]
( -2 ) ;[MU_2]
( 1.0 )  ;[MU_3]
( -0.2 );[MU_4]      
( 2.2 ) ;[MU_5]
( 0.5 )  ;[MU_6]
( 4.0 )  ;[MU_7]
( -0.8) ;[MU_8]

;Initial Omegas
$OMEGA BLOCK(8) VALUES(0.8,0.001)

$SIGMA  
0.1 ;[p]
0.1 ;[p]

$PRIOR NWPRI
$OMEGAP BLOCK(8) FIXED VALUES(0.2,0.0)
$OMEGAPD 8.0 FIXED

$EST METHOD=ITS INTERACTION SIGL=4 NITER=15 PRINT=1 NOABORT NOPRIOR=1 file=example6hmto21_its.ext
$EST METHOD=bayes INTERACTION NBURN=2000 NITER=0 PRINT=10 MASSRESET=1 NOPRIOR=0 file=example6hmto21_bayes.ext
$EST METHOD=NUTS INTERACTION  NBURN=250 NITER=2000 PRINT=5 MASSRESET=0 PMADAPT=200  file=example6hmto21.ext
     OLKJDF=1.0
$COV MATRIX=R UNCONDITIONAL

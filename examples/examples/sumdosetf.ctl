; From Appendix 4 of 
; Jun Shen, Alison Boeckmann & Andrew Vick
; Journal of Pharmacokinetics and Pharmacodynamics
; ISSN 1567-567X Volume 39 Number 3
; J Pharmacokinet Pharmacodyn (2012) 39:251-262 DOI 10.1007/s10928-012-9247-3

$PROBLEM Dose Superposition
$INPUT ID TIME CMT AMT DV ADDL II RATE
$DATA sumdosetf.csv IGNORE=@
$SUBROUTINES ADVAN6 TOL=10 TRANS1  OTHER=sumdoset.f90 ; use sumdoset2.f90 for all methods, including LAPLACE

$MODEL COMP=ABS ;(DEFDOSE)
       COMP=CENTRAL;(DEFOBS)
       COMP=PERI
$PK 
CALLFL=-2
IF (NEWIND .LT. 2) THEN
VECTRA(1)=0. ;set FLAG for initialization
X=FUNCA(VECTRA) ;call subroutine for initialization
ENDIF

CL = THETA(1)*EXP(ETA(1))
V2 = THETA(2)*EXP(ETA(2))
Q  = THETA(3)
V3 = THETA(4)

K=CL/V2
S2 =V2
K23=Q/V2
K32=Q/V3

;--------ABSORPTION MODEL-----------
F1=0
MTT  = THETA(5)*EXP(ETA(4))
NN   = THETA(6)*EXP(ETA(5))
BIO  = THETA(7)*EXP(ETA(3))
KTR  = (NN+1)/MTT

NFAC=SQRT(2*3.1416)*NN**(NN+0.5)*(EXP(-NN))*(1+1/(12*NN))
KINPT=BIO*KTR**(NN+1)/NFAC

IF (DOSTIM .EQ. 0) THEN
   VECTRA(2)=TIME ;assign event dose time
   VECTRA(3)=AMT ;assign event dose amount
ELSE
   VECTRA(2)=DOSTIM ;assign non-event dose time
   VECTRA(3)=AMT ;assign non-event dose amount
ENDIF
IF (AMT > 0 .AND. RATE == 0 .AND. CMT == 1) THEN
  VECTRA(1)=1.  ;set FLAG for PK dose recording
  X=FUNCA(VECTRA) ;call subroutine for dose recording
ENDIF

$DES
 VECTRB(1)=2
 VECTRB(2)=T
 VECTRB(4)=NN
 VECTRB(5)=KTR
 INPT=FUNCA(VECTRB) ;call subroutine to compute drug input

 DADT(1)=KINPT*INPT-KTR*A(1)
 DADT(2)=KTR*A(1)-K23*A(2)-K*A(2)+K32*A(3)
 DADT(3)=K23*A(2)-K32*A(3)

$ERROR

Y = F*EXP(EPS(1))

$THETA 
4 ;CL
3 ;V2
1 ;Q
2 ;V3
10 ;MTT
5 ;NN
0.85 ;BIO

$OMEGA 0 FIX 0 FIX 0 FIX 0 FIX 0 FIX 
$SIGMA 0 FIX
$SIMULATION (123456) ONLY
;$ESTIMATION METHOD=0
$TABLE  ID TIME CMT CL V2 Q V3 MTT NN INPT ONEHEADER NOPRINT FORMAT=s1PE18.11 FILE=functable.txt

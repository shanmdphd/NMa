$PROBLEM Dose Superposition ctlss
$INPUT ID TIME CMT AMT DV ADDL II SS
$DATA ssaddl_dat IGNORE=@
$SUBROUTINES ADVAN6 TOL=6 TRANS1 
$ABBR  DERIV2=NO
$ABBR DECLARE DOWHILE I
$ABBR DECLARE DOWHILE NDOSE

$MODEL COMP=ABS ;(DEFDOSE)
       COMP=CENTRAL;(DEFOBS)
       COMP=PERI
$abbr declare dosetime(1000),dose(1000),ipt(1000)
$PK 

CALLFL=-2
IF (ss == 1 ) THEN  ; SS dose record
IF (NEWIND < 2) NDOSE=1
 NDOSE=NDOSE+0
 DOWHILE (NDOSE<=10)  ; Insert 10 earlier doses
 dosetime(NDOSE)=TIME-II*(NDOSE-1)
 DOSE(NDOSE)=AMT
 NDOSE=NDOSE+1
 ENDDO
ENDIF

IF (dostim > 0) THEN  ; ADDL dose record
 dosetime(NDOSE)=dostim  ; dostim is the time of the addl dose
 DOSE(NDOSE)=AMT
 NDOSE=NDOSE+1
ENDIF

IF (dostim == 0 .and. amt > 0 .and. ss==0 ) THEN  ; transient non-ss dose
 DOSE(NDOSE)=AMT
 dosetime(NDOSE)=TIME  ; TIME is the time of the transient dose
 NDOSE=NDOSE+1
ENDIF
CL = THETA(1)*EXP(ETA(1))
V2 = THETA(2)*EXP(ETA(2))
Q  = THETA(3)
V3 = THETA(4)
KA = THETA(8)*EXP(ETA(6))

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

$DES
INPT=0
I=1
DOWHILE (I<NDOSE)
IPT(i)=0
IF (T>=dosetime(I)) IPT(I)=DOSE(I)*(T-dosetime(I))**NN*EXP(-KTR*(T-dosetime(I)))
INPT=INPT+IPT(I)
I=I+1
ENDDO

 DADT(1)=KINPT*INPT-KTR*A(1)
 DADT(2)=KTR*A(1)-K23*A(2)-K*A(2)+K32*A(3)
 DADT(3)=K23*A(2)-K32*A(3)
$ERROR

Y = F*EXP(EPS(1))
a1=A(1)
a2=A(2)
a3=A(3)

$THETA 
4 ;CL
3 ;V2
1 ;Q
2 ;V3
10 ;MTT
5 ;NN
0.85 ;BIO
1 ; KA

$OMEGA 1 1 1 1 1 1
$SIGMA 1
$TABLE  TIME AMT INPT a1 a2 a3 FILE=ssaddl_tab

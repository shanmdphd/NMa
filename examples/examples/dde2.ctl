; Pre-Control stream template dde2.dde used by ddexpand program to form functional NMTRAN control stream dde2.ctl
$PROB DDE Problem

; the data file should have only DOSE input records pertaining to base equations.  Also, the CMT must be a data item
; The ddexpand program, using finedata's EXTRADOSE facility, will add doses for additional compartments
; and call the new data file dde2_dde.csv
$INPUT ID TIME    AMT    RATE   CMT   EVID MDV  DV
$DATA dde2_dde.csv IGNORE=C

$SUBROUTINES ADVAN13 TRANS1 TOL=12
$MODEL NCOMPARTMENTS=12 ; number of compartments must be adjusted by user after ddexpand is executed.

$PK
CEVID=EVID
IF(CMT/=1) CEVID=1
K10=THETA(1)+ETA(1)
K12=THETA(2)+ETA(2)
K21=THETA(3)+ETA(3)
V1=THETA(4)+ETA(4)
K1=THETA(5)+ETA(5)
K2=THETA(6)+ETA(6)
K4=THETA(7)+ETA(7)
K5=THETA(8)+ETA(8)
SIG1=THETA(9)+ETA(9)
SIG2=THETA(10)+ETA(10)
SIG3=THETA(11)+ETA(11)
;  TAU1, TAU2, TAU3,etc. are time delays.  This sample has one time delay, TAU1
TAU1=THETA(12)+ETA(12)
I0=THETA(13)+ETA(13)
K3=5.0
AA=1.0
BB=0.5
; Set initial conditions for Base equations
A_0(1)=AA
A_0(2)=I0
A_0(6)=AA
A_0(7)=I0
;Any propagations of initial conditions and ALAG's will be placed here by ddexpand program.

; INITIALIZING EQUATIONS FOR DDE COMPARTMENTS
A_0(9)=AA
A_0(15)=AA
A_0(21)=AA
A_0(27)=AA
A_0(10)=I0
A_0(16)=I0
A_0(22)=I0
A_0(28)=I0
A_0(13)=AA
A_0(19)=AA
A_0(25)=AA
A_0(31)=AA
A_0(14)=I0
A_0(20)=I0
A_0(26)=I0
A_0(32)=I0
ALAG9=TAU1
ALAG10=TAU1
ALAG11=TAU1
ALAG12=TAU1
ALAG13=TAU1
ALAG14=TAU1
ALAG15=TAU1*2
ALAG16=TAU1*2
ALAG17=TAU1*2
ALAG18=TAU1*2
ALAG19=TAU1*2
ALAG20=TAU1*2
ALAG21=TAU1*3
ALAG22=TAU1*3
ALAG23=TAU1*3
ALAG24=TAU1*3
ALAG25=TAU1*3
ALAG26=TAU1*3
ALAG27=TAU1*4
ALAG28=TAU1*4
ALAG29=TAU1*4
ALAG30=TAU1*4
ALAG31=TAU1*4
ALAG32=TAU1*4

$DES
; AD_x_y is the State value of A(x) delayed for time TAUy.  These are used in the differntial equations later on.
; AP_x_y is the State value of A(x) in the past, for time delay TAUy.  That is, when T<Tauy, the AP_x_y defines A(x)
; For every AD_x_y used in the differential equations, there must be an AP_X_Y defined.
; If past is constant, then it can be as simple as AP_x_y=Initial condition constant (same value as A_0(x) is set to).
; Make sure AP_x_y is a function of T: do not use T-TAUy, as this will be done by the ddexpand program.

; DELAY EQUATIONS FOR TAU REPLICATE 0
 AP_1_1=AA*EXP(BB*(T-TAU1))
 AP_6_1=AA*EXP(BB*(T-TAU1))
 DTAU1=0.0
 IF(T>=TAU1) DTAU1=1.0
 AD_1_1=(1.0-DTAU1)*AP_1_1+DTAU1*A(9)
 AD_6_1=(1.0-DTAU1)*AP_6_1+DTAU1*A(13)

; BASE EQUATIONS ENTERED BY USER.  Note use of AD_1_1 and AD_6_1, which warrants an expansion.
 DADT(1)=K3-(K1/K2)*(1.0-EXP(-K2*T))*A(1)+K4*A(2)
 DADT(2)=K4*A(1)-K4*AD_1_1
 DADT(3)=K4*AD_1_1-K5*A(3)
 CC=A(4)/V1
 EFFECT=CC*(SIG1*EXP(-SIG2*CC)+SIG3)
 DADT(4)=-K10*A(4)-K12*A(4)+K21*A(5)
 DADT(5)=K12*A(4)-K21*A(5)
 DADT(6)=K3-EFFECT*A(6)-K1/K2*(1.0-EXP(-K2*T))*A(6)+K4*A(7)
 DADT(7)=K4*A(6)-K4*AD_6_1
 DADT(8)=K4*AD_6_1-K5*A(8)
;Any delay equations necessary are placed here by the ddexpand program.

; DELAY EQUATIONS FOR TAU REPLICATE 1
 AP_1_1_2=AA*EXP(BB*(T-TAU1*2))
 AP_6_1_2=AA*EXP(BB*(T-TAU1*2))
 DTAU1_2=0.0
 IF(T>=TAU1*2) DTAU1_2=1.0
 AD_1_1_2=(1.0-DTAU1_2)*AP_1_1_2+DTAU1_2*A(15)
 AD_6_1_2=(1.0-DTAU1_2)*AP_6_1_2+DTAU1_2*A(19)

 DADT(9)=DTAU1*(K3-(K1/K2)*(1.0-EXP(-K2*(T-TAU1)))*A(9)+K4*A(10))
 DADT(10)=DTAU1*(K4*A(9)-K4*AD_1_1_2)
 CC1=DTAU1*(A(11)/V1)
 EFFECT1=DTAU1*(CC1*(SIG1*EXP(-SIG2*CC1)+SIG3))
 DADT(11)=DTAU1*(-K10*A(11)-K12*A(11)+K21*A(12))
 DADT(12)=DTAU1*(K12*A(11)-K21*A(12))
 DADT(13)=DTAU1*(K3-EFFECT1*A(13)-K1/K2*(1.0-EXP(-K2*(T-TAU1)))*A(13)+K4*A(14))
 DADT(14)=DTAU1*(K4*A(13)-K4*AD_6_1_2)

; DELAY EQUATIONS FOR TAU REPLICATE 2
 AP_1_1_3=AA*EXP(BB*(T-TAU1*3))
 AP_6_1_3=AA*EXP(BB*(T-TAU1*3))
 DTAU1_3=0.0
 IF(T>=TAU1*3) DTAU1_3=1.0
 AD_1_1_3=(1.0-DTAU1_3)*AP_1_1_3+DTAU1_3*A(21)
 AD_6_1_3=(1.0-DTAU1_3)*AP_6_1_3+DTAU1_3*A(25)

 DADT(15)=DTAU1_2*(K3-(K1/K2)*(1.0-EXP(-K2*(T-TAU1_2)))*A(15)+K4*A(16))
 DADT(16)=DTAU1_2*(K4*A(15)-K4*AD_1_1_3)
 CC1_2=DTAU1_2*(A(17)/V1)
 EFFECT1_2=DTAU1_2*(CC1_2*(SIG1*EXP(-SIG2*CC1_2)+SIG3))
 DADT(17)=DTAU1_2*(-K10*A(17)-K12*A(17)+K21*A(18))
 DADT(18)=DTAU1_2*(K12*A(17)-K21*A(18))
 DADT(19)=DTAU1_2*(K3-EFFECT1_2*A(19)-K1/K2*(1.0-EXP(-K2*(T-TAU1_2))) &
  *A(19)+K4*A(20))
 DADT(20)=DTAU1_2*(K4*A(19)-K4*AD_6_1_3)

; DELAY EQUATIONS FOR TAU REPLICATE 3
 AP_1_1_4=AA*EXP(BB*(T-TAU1*4))
 AP_6_1_4=AA*EXP(BB*(T-TAU1*4))
 DTAU1_4=0.0
 IF(T>=TAU1*4) DTAU1_4=1.0
 AD_1_1_4=(1.0-DTAU1_4)*AP_1_1_4+DTAU1_4*A(27)
 AD_6_1_4=(1.0-DTAU1_4)*AP_6_1_4+DTAU1_4*A(31)

 DADT(21)=DTAU1_3*(K3-(K1/K2)*(1.0-EXP(-K2*(T-TAU1_3)))*A(21)+K4*A(22))
 DADT(22)=DTAU1_3*(K4*A(21)-K4*AD_1_1_4)
 CC1_3=DTAU1_3*(A(23)/V1)
 EFFECT1_3=DTAU1_3*(CC1_3*(SIG1*EXP(-SIG2*CC1_3)+SIG3))
 DADT(23)=DTAU1_3*(-K10*A(23)-K12*A(23)+K21*A(24))
 DADT(24)=DTAU1_3*(K12*A(23)-K21*A(24))
 DADT(25)=DTAU1_3*(K3-EFFECT1_3*A(25)-K1/K2*(1.0-EXP(-K2*(T-TAU1_3))) &
  *A(25)+K4*A(26))
 DADT(26)=DTAU1_3*(K4*A(25)-K4*AD_6_1_4)

; DELAY EQUATIONS FOR TAU REPLICATE 4
 AP_1_1_5=AA*EXP(BB*(T-TAU1*5))
 AP_6_1_5=AA*EXP(BB*(T-TAU1*5))
 AD_1_1_5=AP_1_1_5
 AD_6_1_5=AP_6_1_5

 DADT(27)=DTAU1_4*(K3-(K1/K2)*(1.0-EXP(-K2*(T-TAU1_4)))*A(27)+K4*A(28))
 DADT(28)=DTAU1_4*(K4*A(27)-K4*AD_1_1_5)
 CC1_4=DTAU1_4*(A(29)/V1)
 EFFECT1_4=DTAU1_4*(CC1_4*(SIG1*EXP(-SIG2*CC1_4)+SIG3))
 DADT(29)=DTAU1_4*(-K10*A(29)-K12*A(29)+K21*A(30))
 DADT(30)=DTAU1_4*(K12*A(29)-K21*A(30))
 DADT(31)=DTAU1_4*(K3-EFFECT1_4*A(31)-K1/K2*(1.0-EXP(-K2*(T-TAU1_4))) &
  *A(31)+K4*A(32))
 DADT(32)=DTAU1_4*(K4*A(31)-K4*AD_6_1_5)

; FOR FINEDATA $EXTRADOSE: CMT=1:,9,15,21,27,2:,10,16,22,28,4:,11,17,23,29,5:,12,18,24,30,6:,13,19,25,31,7:,14,20,26,32

$ERROR

A1=A(1)
A2=A(2)
A3=A(3)
A4=A(4)
A5=A(5)
A6=A(6)
A7=A(7)
A8=A(8)
A9=A(9)
A10=A(10)
A11=A(11)
A12=A(12)

Y1=A(2)+A(3)
Y2=A(7)+A(8)
Y3=A(3)
Y4=A(8)
IF(CMT==1) IPRED=Y1
IF(CMT==2) IPRED=Y2
IF(CMT==3) IPRED=Y3
IF(CMT==4) IPRED=Y4
IF(CMT==1) Y=IPRED*(1.0+EPS(1))
IF(CMT==1) Y=IPRED*(1.0+EPS(2))
IF(CMT==2) Y=IPRED*(1.0+EPS(3))
IF(CMT==4) Y=IPRED*(1.0+EPS(4))


$THETA
0.32544   ; 1: K10
2.6496    ; 2: K12
2.5944    ; 3: K21
0.02645   ; 4: V
0.456     ; 5: K1
0.169     ; 6: K2
0.185     ; 7: K4
0.031     ; 8: K5
0.328     ; 9: SIG1
0.328     ; 10: SIG2
0.025     ; 11: SIG3
10.6      ; 12: TAU1
2.83      ; 13: I0

$OMEGA (0.0 FIXED)X13

$SIGMA (0.04)X4

$SIMULATION (567811 NORMAL) (2933012 UNIFORM) ONLYSIMULATION SUBPROBLEMS=1

$TABLE TIME Y1 Y2 Y3 Y4 EXCLUDE_BY CEVID NOAPPEND NOPRINT FILE=dde2.tab

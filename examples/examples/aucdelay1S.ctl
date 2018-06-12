; AUC WITH DELAY EXAMPLE (MOVING AVERAGE OF AUC)
; Example aucdelay1S.ctl
; Compare with aucdelay1.ctl,
; the simple absorption model example developed by Bob
; Bauer and Alison Boeckmann for illustration purposes.
; This version is used for the special case that TDY is less than
; the time between events.

$PROB TEST AUC DELAY USING TWO SETS OF COMPARTMENTS, TDY < TIME BETWEEN EVENTS
$INPUT ID TIME AMT CMT DV
$DATA DELAYDATA IGNORE=@
$WARNINGS DATA=NONE ; supress warnings about missing DV values
$SUBR ADVAN6 TOL=5
$MODEL
COMP=(DEPOT) COMP=(CENTRAL) COMP=(AUC)
COMP=(D_DEPOT) COMP=(D_CENTR) COMP=(D_AUC)
COMP=(AUCDIFF)
$PK
TDY=THETA(1)*EXP(ETA(1))
ALAG4=TDY
IF (TDY>=1) EXIT 1 1

KA=THETA(2)*EXP(ETA(2))
KE=THETA(3)*EXP(ETA(3))
$DES
DADT(1)=-KA*A(1)
DADT(2)= KA*A(1)-KE*A(2) ; C(T)
DADT(3)= A(2)  ; AUC(T)
DADT(4)=-KA*A(4)
DADT(5)= KA*A(4)-KE*A(5) ; C(T-TDY)
DADT(6)= A(5)  ; AUC(T-TDY)
DADT(7)= A(2)-A(5) ; AUC(T)-AUC(T-TDY)

$ERROR
A1=A(1)
A2=A(2)
A3=A(3)
A4=A(4)
A5=A(5)
A6=A(6)
A7=A(7)
DAUC=A(3)-A(6) ; AUC(T)-AUC(T-TDY)
Y=F+DAUC+EPS(1)
$THETA (0,.5,1) ; DELAY
$THETA 1 2
$OMEGA 1 1 1
$SIGMA 1
$TABLE ID TIME DAUC G11 G21 G31 NOPRINT FILE=aucdelay1S.tbl 
FORMAT=sF8.3

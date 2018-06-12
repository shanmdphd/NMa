; AUC WITH DELAY EXAMPLE (MOVING AVERAGE OF AUC)
; Example aucdelay1.ctl
;In the following simple absorption model example developed by Bob
;Bauer and Alison Boeckmann for illustration purposes, compartments
;1, 2, and 3 are the "real time" depot, central, auc, and  compartments
;4,5,6 are the "delayed time" depot, central, auc.
;So, the base model (non-time delay) system (compartments 1,2,3) is replicated
;(compartments 4,5,6) for the time delay portion.  
;
;In addition, the data set duplicates the dose information of
;compartment 1 into compartment 4, and setting ALAG4 to a non-zero
;value in the control stream file provides a lag time to any doses
;inputted into compartment 4 (so this would take care of multiple
;dose problems as well).  
;
;This allows for assessment and availability of AUC(t) and
;AUCT(t-time-delay) at any time t.  
;The comments explain the meaning of each compartment.

$PROB TEST AUC DELAY USING TWO SETS OF COMPARTMENTS
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
Y=F+DAUC+EPS(1) ; changed to show that DAUC can be used in model for Y
$THETA 3 ; DELAY
$THETA 1 2
$OMEGA 1 1 1
$SIGMA 1
$TABLE ID TIME A1 A2 A3 A4 A5 A6 A7 DAUC NOAPPEND NOPRINT FILE=aucdelay.tbl FORMAT=sF8.3

; new table for comparison with aucdelay3.tbl and aucdelay2.tbl
$TABLE ID TIME A1 A2 A3 DAUC NOPRINT FILE=aucdelay1.tbl 
FORMAT=sF8.3

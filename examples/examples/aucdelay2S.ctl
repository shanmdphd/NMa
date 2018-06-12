; AUC WITH DELAY EXAMPLE (MOVING AVERAGE OF AUC)
; Example aucdelay2S.ctl
; Compare with aucdelay1S.ctl, 
; the simple absorption model example developed by Bob
; Bauer and Alison Boeckmann for illustration purposes.
; This version is used for the special case that TDY is less than
; the time between events.
; Compartments 1, 2, and 3 are the "real time" depot, central, auc.
; The duplicate dose into CMT=4 is ignored.
; Time delay TDY is used to model MTIME(1) rather than ALAG.
; SAVEA saves the value of A(3) for $ERROR in the next event record.

$PROB TEST AUC DELAY USING ONE SET OF COMPARTMENTS, TDY < TIME BETWEEN EVENTS
; NEXTT is listed in $INPUT. It will be created by the $INFN code.
$INPUT ID TIME AMT CMT DV NEXTT
$DATA DELAYDATA IGNORE=@ IGNORE=(CMT==4)
$WARNINGS DATA=NONE ; supress warnings about missing DV values
$SUBR ADVAN6 TOL=5
$ABBR DECLARE REAL TIMES(1000)
$ABBR DECLARE INTEGER K
$MODEL
COMP=(DEPOT) COMP=(CENTRAL) COMP=(AUC)

; For each event record, the value of NEXTT gives the next value of TIME. 
; This example shows how NEXTT may be created ("transgenerated") during the run 

$INFN
IF (ICALL.EQ.1) THEN
; FIRST PASS THROUGH THE DATA SET
K=0
DOWHILE(DATA) ; SAVE ALL VALUES OF TIME
K=K+1
TIMES(K)=TIME
ENDDO

; SECOND PASS THROUGH THE DATA SET
K=0
DOWHILE(DATA) ; CREATE THE NEXTT DATA ITEM
K=K+1
NEXTT=TIMES(K+1)
ENDDO
ENDIF

$OMEGA 1 1 1 ; MUST PRECEDE $PK WHEN A(I) USED IN $PK

$PK
MTDIFF=1
TDY=THETA(1)*EXP(ETA(1))
IF (TDY>=1) EXIT 1 1
MTIME(1)=NEXTT-TDY ;  tell PREDPP that MTIME values will change.

; TSTATE is the "State Time",
; the time at which the state-vector A was last computed.
IF (TSTATE==TIME-TDY) SAVEA=A(3)

KA=THETA(2)*EXP(ETA(2))
KE=THETA(3)*EXP(ETA(3))

$DES
DADT(1)=-KA*A(1)
DADT(2)= KA*A(1)-KE*A(2) ; C(T)
DADT(3)= A(2)  ; AUC(T)

$ERROR
DAUC=A(3)-SAVEA
Y=F+DAUC+EPS(1)
A1=A(1)
A2=A(2)
A3=A(3)
$THETA (0,.5,1) ; DELAY
$THETA 1 2
$SIGMA 1
$TABLE ID TIME DAUC G11 G21 G31 NOPRINT FILE=aucdelay2S.tbl
FORMAT=sF8.3

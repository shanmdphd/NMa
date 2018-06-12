; AUC WITH DELAY EXAMPLE (MOVING AVERAGE OF AUC)
; Example aucdelay3.ctl
; Compare with aucdelay1.ctl, 
; the simple absorption model example developed by Bob
; Bauer and Alison Boeckmann for illustration purposes. compartments
; Compartments 1, 2, and 3 are the "real time" depot, central, auc.
; The duplicate dose into CMT=4 is ignored.
; Time delay TDY is used to model MTIME(i) rather than ALAG.
; UNIX commands to run aucdelay3.ctl are:
; nmtemplate aucdelay3.ctl temp1 maxrecs=16
; doexpand  temp1  temp2
; nmfe74 temp2 aucdelay3.res
;

$PROB TEST AUC DELAY USING ONE SET OF COMPARTMENTS AND DOE REPETITION
$INPUT ID TIME AMT CMT DV 
$DATA DELAYDATA IGNORE=@ IGNORE=(CMT==4)
$WARNINGS DATA=NONE ; supress warnings about missing DV values
$ABBR DECLARE REAL TIMES(1000)
$ABBR DECLARE INTEGER K, INTEGER KREC
$SUBR ADVAN6 TOL=5
$MODEL
COMP=(DEPOT) COMP=(CENTRAL) COMP=(AUC)

$INFN
IF (ICALL.EQ.1) THEN
K=0
DOWHILE(DATA) ; SAVE ALL VALUES OF TIME
K=K+1
TIMES(K)=TIME
ENDDO
ENDIF

$OMEGA 1 1 1 ; MUST PRECEDE $PK WHEN A(I) USED IN $PK

$PK
"FIRST
" USE CMNM2_INT ,ONLY : IRECIDX 
; IRECIDX gives the position in TIMES of the start of the current individual record.
; LIREC gives the number of records in the current individual record
; and is needed if different subjects have different amounts of data,
; in which case maxrecs will be too large for some individuals.
 
MTDIFF=1 ;  tell PREDPP that MTIME values will change.
TDY=THETA(1)*EXP(ETA(1))

DOE (I=1,<maxrecs>)
KREC=IRECIDX+[I]
IF (NEWIND.LE.1.AND.[I]<=LIREC) THEN
MTIME([I])=TIMES(KREC)-TDY  ; SET NEW VALUE OF MTIME (TDY MAY DEPEND ON ETA)
SAVEA[I]=0                  ; ERASE OLD VALUE FROM PREVIOUS PASS
ELSE
MTIME([I])=MTIME([I])
SAVEA[I]=SAVEA[I]
ENDIF
ENDDOE

DOE (I=1,<maxrecs>)
IF ([I]<=LIREC.AND.MTIME([I])==TSTATE) THEN
MTIME([I])=0 ; MAKES SURE THAT ONLY THE FIRST A(3) VALUE AT TSTATE IS SAVED
SAVEA[I]=A(3)
ELSE
MTIME([I])=MTIME([I])
SAVEA[I]=SAVEA[I]
ENDIF
ENDDOE


KA=THETA(2)*EXP(ETA(2))
KE=THETA(3)*EXP(ETA(3))

$DES
DADT(1)=-KA*A(1)
DADT(2)= KA*A(1)-KE*A(2) ; C(T)
DADT(3)= A(2)  ; AUC(T)

$ERROR
"FIRST
" USE CMNM2_INT ,ONLY : IRECIDX
DOE (I=1,<maxrecs>)
KREC=IRECIDX+[I]
IF ([I]<=LIREC.AND.TIME==TIMES(KREC)) SA=SAVEA[I]
ENDDOE

A1=A(1)
A2=A(2)
A3=A(3)

DAUC=A(3)-SA

Y=F+DAUC+EPS(1)
$THETA 3
$THETA 1 2
$SIGMA 1
$TABLE ID TIME A1 A2 A3 DAUC NOPRINT FILE=aucdelay3.tbl 
FORMAT=SF8.3

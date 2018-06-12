; This example is discussed in Repetition_1 example (repeti1.exa) 
; in the online help directory.
; It demonstrates the use of RPT_ and RPTON
; When data item RPT_=n and PRED sets rpto=-n with the same record,
; repeated calls are made to PRED using this one record.
; This is used to compute the factorial of the value in KRPT.
;
$PROB
$INPT ID TIME DV MDV RPT_ KRPT
$DATA repeatf.dat IGNORE=@ 
$PRED
   F=THETA(1)*EXP(ETA(1)) ; default value

   prdfl=1 ; required with RPT_
   IF (RPT_.EQ.1) RPTO=-1     ; tells NONMEM to repeat this record
   RPTON=KRPT                 ; tells how many times to repeat this record.
   last=0                     ; when last=1, this is the last call with repeated record
   IF (RPTI .EQ. 0) THEN      ; not a repeated call
    factorial=1
    count=0                   ; count calls with repeated records
   ELSE                       ; repeated call
    count=count+1             ; count calls with repeated records
    factorial=factorial*count
    IF (count == rpton) last=1  ; identify the last call with repeated record
   ENDIF
   IF (last==1) F=factorial*EXP(ETA(2))  ; prediction=KRPT! (factorial of KRPT)

  Y=F+EPS(1)

$THETA 2
$OMEGA .1 .1
$SIGMA .1
$TABLE RPT_ KRPT PRED NOAPPEND FILE=repeatf.tab

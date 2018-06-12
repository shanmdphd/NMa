; This example is discussed in Repetition_1 example (repeti1.exa) 
; in the online help directory.
; Like repeat1t.ctl, it demonstates that a repeated pass may start with 
; any designated record, not necessarily the first record.
; repeat1s uses the RPT_ data item for the same purpose.
;
 $PROB repeat1s
 $INPT ID TIME AMT DV EVID RPT_
 $DATA repeat1s.dat NULL=.
 $SUB ADVAN6 TOL=5
 $MODEL COMP=(DEPOT DEFDOSE) COMP=(CENTRAL DEFOBS)

 $PK
 KA=THETA(1)*EXP(ETA(1))
 K =THETA(2)*EXP(ETA(2))
 C =THETA(3)
 V =THETA(4)*EXP(ETA(4))
 S2=V
 IF (RPTI.EQ.0) TI=TIME
 IF (NEWIND.EQ.2) RPTO=-1

 $DES
 DADT(1)=-KA*A(1)
 D=EXP(-K*(TI-T)**C)
 DADT(2)=D*KA*A(1)

 $ERROR
 Y=F+EPS(1)

 $THETA 2 1 1 2 
 $OMEGA 1 2 (1 FIX) 1
 $SIGMA 1
 $TABLE TIME TI

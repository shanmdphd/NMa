; EHC example using Hill (sigmoid emax model)
;
; The help example for MODEL TIME EXAMPLES contains this fragment of code for EHC:
;     $PK
;     MTIME(1)=THETA(8)
;     MTIME(2)=MTIME(1)+THETA(9)
;      ....
;     $DES
;     FLAG=MPAST(1)-MPAST(2)
;     DADT(1)=-KA*A(1)+K41*A(4)*FLAG
;     DADT(4)=K1G*A(2)-K41*A(4)*FLAG
;      ....
; MTIME variables are ignored during Steady-State calculation, which is intended for 
; a single repeated dose with no changes in the status of the system.
; Instead, this example uses Hill (sigmoid emax model) for FLAG1 and FLAG2.
; The Steady-state dose gives the correct result. 
; This is an example of a smooth step model.
; 
$PROB  EHC using Hill (sigmoid emax model) for the flags
$INPUT      ID DOSE=AMT TIME CP=DV WT SS II ADDL EVID
$DATA       hillss.dat

$SUBROUTINES  ADVAN6 TOL=6
$MODEL COMP=(DEPOT,INITIALOFF,DEFDOSE) COMP=(CENTRAL,DEFOBS,NOOFF)
COMP=(PERIPH) COMP=(GALL )

$PK
   KA=THETA(1)*EXP(ETA(1))
   KE=THETA(2)*EXP(ETA(2))
   CL=THETA(3)*WT*EXP(ETA(3))
   S2=CL/KE/WT
   K41=THETA(4)*EXP(ETA(4))
   K23=THETA(5)*EXP(ETA(5))
   K32=THETA(6)*EXP(ETA(6))
   K1G=THETA(7)*EXP(ETA(7))

$DES
; Save the value of II from the dose record.
   if (ii>0) inter=ii
   mt1=inter*INT(T/inter)+theta(8)
   mt2=mt1+theta(9)
   hill1=exp(-THETA(10)*(t-mt1)) 
   hill2=exp(-THETA(10)*(t-mt2)) 
   flag1=1./(1+hill1)  ; changes from 0 to 1 near t=mt1
   flag2=1./(1+hill2)  ; changes from 0 to 1 near t=mt2
   flag=flag1-flag2

   DADT(1)=-KA*A(1)+K41*A(4)*FLAG
   DADT(2)= KA*A(1)-KE*A(2)-K23*A(2)+K32*A(3)-K1G*A(2)
   DADT(3)= K23*A(2)-K32*A(3)
   DADT(4)= K1G*A(2)-K41*A(4)*FLAG

$ERROR
A1=A(1) ; for display in table
A2=A(2)
A3=A(3)
A4=A(4)
   Y=F+EPS(1)

$THETA 1 1 1
$THETA 10  ; rate of gall bladder emptying is large vs. other k's
$THETA 1 1 1
$THETA 3 5 ; start emptying at T+theta(8) ; lasts till T+theta(9)
$THETA 50 ; the larger the hill exponent, the better the predictions 
          ; coincide with MTIME model.
          ; However, as exponent gets larger, numerical difficulties may occur.
$OMEGA 1 1 1 1 1 1 1 
$SIGMA  .4

$TABLE TIME A1 A2 A3 A4 NOAPPEND FILE=hillss.tab NOPRINT

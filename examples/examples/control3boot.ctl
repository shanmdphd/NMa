$PROBLEM  THEOPHYLLINE   SINGLE SUBJECT DATA
$INPUT  DOSE=AMT TIME CP=DV CAT
$DATA  DATA3B
$SUBROUTINES  ADVAN2

$PK
CALLFL=1
KA=THETA(1)
K=THETA(2)
SC=THETA(3)

$ERROR
IPRED=F
W=1.0
; first observation after dose is part of "first subject".  So, put in dummy record, CAT=3,
; and give it a residual variance that is very large, so it does not influence the fit.
IF(CAT==3.0) W=1.0E+10
Y=F+W*ERR(1)

$THETA  (0,1.7)  (0,.102)  (0,29)
$OMEGA 0.2

$SIML (567666 NORMAL) (33012 UNIFORM) BOOTSTRAP=-1 STRAT=CAT SUBP=100

$ESTIMATION MAXEVAL=9999  PRINT=2
$COVR
$TABLE TIME CAT AMT CP IPRED W NOAPPEND NOPRINT file=control3boot.tab

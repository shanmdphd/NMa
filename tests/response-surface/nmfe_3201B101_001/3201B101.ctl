$PROBLEM INDOBUFEN PKPD Simulation (R and S form PK and Platelet Aggregation)
; FLAG=1, S form
; FLAG=2, R form
; FLAG=3, MPA *%)
; unit, mg, ug/ml
; No interconversion between S- and R-form, There is a reference for this.
; 2 comp, 1st and 0 order oral absorption

$INPUT ID TIME AMT RATE DV MPA FLAG EVID MDV CMT BASE TRAC

$DATA Indobufen_PKPD_nm.csv IGNORE=@
$SUBROUTINE ADVAN13 TOL=6

$MODEL
   COMP=(GUT,DEFDOSE)
   COMP=(CENTRAL,DEFOBS)
   COMP=(PERI)
   COMP=(GUT2)
   COMP=(CENTRAL2)
   COMP=(PERI2)
   
$PK

; R Enantiomer  

   TVV2   = THETA(1)
   V2     = TVV2 * EXP(ETA(1))
   
   TVCLR  = THETA(2)
   CLR    = TVCLR * EXP(ETA(2))
   
   TVV3   = THETA(10)
   V3     = TVV3 * EXP(ETA(3))
   
   TVQR   = THETA(11)
   QR     = TVQR
   
   TVKAR  = THETA(3)
   KAR    = TVKAR * EXP(ETA(4))
   
   TVGAMR = THETA(4)
   GAMR   = TVGAMR
                
   D2     = THETA(5)
   
   ALAG2  = THETA(6) * EXP(ETA(6))
   
   F1     = THETA(7) * EXP(ETA(5))
   IF (F1.GT.1) F1 = 0.9999 
   F2     = 1 - F1
   

   S2    = V2
   K     = CLR / V2
   K23   = QR  / V2
   K32   = QR  / V3
   
; S Enantiomer   

   TVV5   = THETA(12)
   V5     = TVV2 * EXP(ETA(7))
   
   TVCLS  = THETA(13)
   CLS    = TVCLS * EXP(ETA(8))
   
   TVV6   = THETA(21)
   V6     = TVV6 * EXP(ETA(9))
   
   TVQS   = THETA(22)
   QS     = TVQS
   
   TVKAS  = THETA(14)
   KAS    = TVKAS
   
   TVGAMS = THETA(15)
   GAMS   = TVGAMS * EXP(ETA(12))
                
   D5     = THETA(16)
   
   ALAG5  = THETA(17) * EXP(ETA(10))
   
   F4     = THETA(18) * EXP(ETA(11))
   IF (F4.GT.1) F4 = 0.9999 
   F5     = 1 - F4

   S5     = V5
   K50    = CLS / V5
   K56    = QS  / V5
   K65    = QS  / V6
   
   
;#################################
;#### Response Surface Model #####
;#################################
  C50R    = THETA(23) * EXP(ETA(13))
  C50S    = THETA(24) * EXP(ETA(16))
  ISR     = THETA(25)               ; Interaction term
  GAM     = THETA(26) * EXP(ETA(15))
  IMAX    = THETA(27) * EXP(ETA(14))
;#################################
;#################################


$DES

   WBR      = 1-EXP((-(KAR*TIME)**GAMR))        ; Weibull Function

   DADT(1)  = -A(1)*WBR
   DADT(2)  =  A(1)*WBR + A(3)*K32 - A(2)*(K + K23)
   DADT(3)  = -A(3)*K32 + A(2)*K23
   
   WBS      = 1-EXP((-(KAS*TIME)**GAMS))        ; Weibull Function

   DADT(4)  = -A(4)*WBS
   DADT(5)  =  A(4)*WBS + A(6)*K65 - A(5)*(K50 + K56)
   DADT(6)  = -A(6)*K65 + A(5)*K56
      
      
$ERROR

;#################################
;#### Response Surface Model #####
;#################################

   DEL    = 0.000001

   RC     = (A(2)+DEL)/S2     ; Concentration of R-indobufen
   SC     = (A(5)+DEL)/S5     ; Concentration of S-indobufen
   

;Equations of Minto model
; Normalize to potency
   UR     = RC/C50R
   US     = SC/C50S
   
; Compute "theta" (Q) for Minto interaction model
   IF(UR.EQ.0.AND.US.EQ.0) THEN
     Q    = 0
   ELSE
     Q    = UR/(UR+US)
   ENDIF

; Compute C50 Function In Combined Domain
   U50    = 1 - ISR*Q + ISR*Q**2
 
; Compute "concentration" normalized to potency and corrected for interaction
   UT = (UR + US)/U50
   
;   A    = 0
;   B    = 0
;   C    = 0
;   D    = 0
   
;   IF(TRAC.EQ.1) THEN
;   A = 1
;   ENDIF
;   IF(TRAC.EQ.2) THEN
;   B = 1
;   ENDIF
;   IF(TRAC.EQ.3) THEN
;   C = 1
;   ENDIF
;   IF(TRAC.EQ.4) THEN
;   D = 1
;   ENDIF
   
;   BASE   = BAS1*A + BAS2*B + BAS3*C + BAS4*D
   
   IPRED = F
   W1    = SQRT(THETA(8)**2 + THETA(9)**2 * F**2)
   W2    = SQRT(THETA(19)**2 + THETA(20)**2 * F**2)
   
   Q1    = 0
   IF (FLAG.EQ.1) Q1 = 1
   Y1    = IPRED + W1 * EPS(1)
   
   Q2    = 0
   IF (FLAG.EQ.2) Q2 = 1
   Y2    = IPRED + W2 * EPS(2)
   

   PDIPRED = BASE - (BASE - IMAX)*((UT+DEL)**GAM)/(1 + (UT+DEL)**GAM)
   WPD     = SQRT(THETA(28)**2 + THETA(29)**2 * PDIPRED**2)
   PD      = PDIPRED + WPD * EPS(3)

   Q3    = 0
   IF (FLAG.EQ.3) Q3 = 1
   Y3    = PD
   
   Y     = Q1*Y1 + Q2*Y2 + Q3*Y3


$THETA

; R Enantiomer
 
  2.54 FIX      ;[V2]
  0.198 FIX     ;[CLR]

  1.08 FIX      ;[KAR]
  7.37 FIX      ;[GAMR]
  0.401 FIX     ;[D2]
      
  0.225 FIX     ;[ALAG2]
  0.789 FIX     ;[F1]
  
  0.29 FIX      ;[ADD ERROR]
  0.0951 FIX    ;[PRO ERROR]

  8.44 FIX      ;[V3]
  0.332 FIX     ;[QR] 

; S Enantiomer

  2.01 FIX      ;[V5]
  0.494 FIX     ;[CLS]

  0.381 FIX     ;[KAS]
  1.44 FIX      ;[GAMS]
  0.409 FIX     ;[D5]
      
  0.289 FIX     ;[ALAG5]
  0.726 FIX     ;[F4]
  
  0.6 FIX       ;[ADD ERROR]
  0.0589 FIX    ;[PRO ERROR]

  2.59 FIX      ;[V6]
  0.439 FIX     ;[QS]
  
; Maximal Platelet Aggregation (%)

  (0, 6.75)
  (0, 48.7)
  (-0.1)
  (0, 2.54)
  (0, 6.38)
  (0, 12.1)
  0 FIX

; R Enantiomer

$OMEGA BLOCK(2)
 0.13 FIX
 0.0857
 0.237
 
$OMEGA 0.0525 FIX
$OMEGA 0.134 FIX
$OMEGA 0.0903 FIX
$OMEGA 0.302 FIX

; S Enantiomer

$OMEGA BLOCK(2)
 0.176 FIX
 0.135 
 0.154
 
$OMEGA 0.0195 FIX
$OMEGA 0.454 FIX
$OMEGA 0.0277 FIX
$OMEGA 0.412 FIX

; Maximal Platelet Aggregation (%)

$OMEGA BLOCK(2)
  1.61
  -0.736 0.677

$OMEGA 0.188
$OMEGA 0.00709


$SIGMA
  1 FIX
  1 FIX
  1 FIX

$ESTIMATION SORT MAXEVAL=9999 PRINT=2 METHOD=COND INTER SLOW MSFO=3201B101.MSF
$COVARIANCE PRINT=E ; MATRIX=S
$TABLE ID TIME MDV CWRES NPDE
       FILE=3201B101.FIT NOPRINT ONEHEADER
$TABLE ID BASE
       ETA(13) ETA(14) ETA(15) ETA(16)
       FILE=3201B101.PAR NOPRINT ONEHEADER FIRSTONLY NOAPPEND
$TABLE ID C50R C50S ISR GAM IMAX
       FILE=3201B101.IPK NOPRINT ONEHEADER FIRSTONLY NOAPPEND


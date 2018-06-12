$PROBLEM idr_circadian
;; -----------------------------------------------------------------------------
;; PURPOSE:
;; 1- Illustrate an indirect response model describing a baseline response with
;;    circadian rhythm. The model implements a time-varying kin(t)=dR/dt+Kout*R(t)
;;    where R(t) is a modified sine function. Every 24h, this function increases
;;    from a minimum value, Rmin, at a certain time, SHIFT, reaches a maximum,
;;    Rmin+Amp, then decreases to return to its minimum at time SHIFT+DUR
;;
;;            ^
;;  Rmin+Amp -|     ---            ---            ---
;;            |    |   |          |   |          |   |
;;            |   |     |        |     |        |     |
;;            |  |       |      |       |      |       |
;;  Rmin     -|---       --------       --------       ------ Response R
;;            |
;;            ---------------------------------------------------> TIME
;;               |   |              |              |
;;        SHIFT-24   0              24             48
;;                   <-  SHIFT -><- DUR ->
;;
;;    Response must oscillate between 0.5 and 4.5
;;
;; 2- Illustrate how to create a 24h clock time variable T24 in $DES by
;;    transforming the T variable ranging from 0 to infinity into repeating
;;    intervals of continuous time between 0 and 24.
;;
;; -----------------------------------------------------------------------------

$INPUT ID TIME AMT CMT EVID DV MDV

$DATA circadian.csv IGNORE=@

$THETA
 (0.0001,0.5,0.9999)   ;-- typical scaled-down shift
 (0.0001,0.5,0.9999)   ;-- typical scaled-down duration
 (0,4)                 ;-- typical amplitude of response
 (0,0.5)               ;-- typical minimum response
 (0,0.1)               ;-- typical elimination rate

$OMEGA
  3                    ;-- IIV in shift (%CV=100*sqrt(1-th1)*eta1)
  3                    ;-- IIV in duration (%CV=100*sqrt(1-th2)*eta2)

$SUBROUTINE ADVAN13 TOL=9

$MODEL NCOMP=1
  COMP=(RESP)

$PK

  ;------------------------
  ; Indirect response model
  ;------------------------
  PI = 3.14159263

  ; Define shift and duration parameters

  TVSHIFT = 24*THETA(1)                   ; typical shift on 24h scale; (0,24)
                                          ; where THETA(1) represents this typical shift as a fraction of 24h; (0,1)
  LGSHI = LOG(THETA(1)/(1-THETA(1)))      ; logit transformation of the typical fraction of 24h; (-INF,+INF)
  ILGSHI = LGSHI+ETA(1)                   ; addition of IIV; (-INF,+INF)
  SHIFT = 24*EXP(ILGSHI)/(1+EXP(ILGSHI))  ; back-transformation and scale-up of individual shift to 24h scale; (0,24)

  TVDUR = 24*THETA(2)                     ; typical duration on 24h scale; (0,24)
                                          ; where THETA(1) represents this typical duration as a fraction of 24h; (0,1)
  LGDUR = LOG(THETA(2)/(1-THETA(2)))      ; logit transformation of the typical fraction of 24h; (-INF,+INF)
  ILGDUR = LGDUR+ETA(2)                   ; addition of IIV; (-INF,+INF)
  DUR = 24*EXP(ILGDUR)/(1+EXP(ILGDUR))    ; back-transformation and scale-up of individual duration to 24h scale; (0,24)

  AMP = THETA(3)
  RMIN = THETA(4)
  KOUT = THETA(5)
  RMAX = RMIN + AMP

  ; Circadian rhythm
  ; Determine initial value of the response and time offset based upon whether
  ; SHIFT and DUR are such that the response is flat or within the sinusoid
  ; phase at time 0
  IF ((SHIFT+DUR).GT.24) THEN
    OFFSET = 24
    INIT1 = RMIN+AMP*SIN(PI*(OFFSET-SHIFT)/DUR)
  ELSE
    OFFSET = 0
    INIT1 = RMIN
  END IF
  TX = SHIFT-OFFSET

  IF (NEWIND <= 1) THEN
    MTIME(1) = TX
    MTIME(2) = TX+DUR
  ELSE
    MTIME(1) = MTIME(1)
    MTIME(2) = MTIME(2)
  ENDIF

  ; Defines of mtime(1) to use for integration during this interval
  INTMTIME = MTIME(1)

  ; Update mtime variables after mtime(2) is reached
  IF (MNOW == 2) THEN
    MTIME(1) = MTIME(1)+24
    MTIME(2) = MTIME(2)+24
    MTDIFF=1
  ENDIF

  ; Initialize the response compartment
  IF (A_0FLG == 1) THEN
    A_0(1) = INIT1
  ENDIF

  ;---------------
  ; 24h clock time
  ;---------------

  ; Use mtime(3) to scale the T variable in $DES from 0 to infinity to repeating
  ; 0-24h intervals
  IF (NEWIND<=1) THEN
    MTIME(3)=TIME+24
  ELSE
    MTIME(3)=MTIME(3)
  ENDIF

  INTMTIME3=MTIME(3)

  IF (MNOW==3) THEN
    MTIME(3)=MTIME(3)+24
    MTDIFF=1
  ENDIF

$DES
  ; Define FLAG variable:
  ; FLAG=1 when T is in [SHIFT+n*24,SHIFT+DUR+n*24], where n=0,1,2,3,...
  ; FLAG=0 otherwise
  FLAG = MPAST(1)-MPAST(2)

  ; Define time-varying kin:
  ; * kin is constant (KOUT*RMIN) when FLAG=0
  ; * kin is time-varying when FLAG=1, and function of TS, a transformation
  ;   function of T scaling time from 0 to infinity to repeating intervals [0,1]
  TS = (T-INTMTIME)/DUR
  KIN = KOUT*RMIN + FLAG*((AMP*PI/DUR)*COS(PI*TS) + KOUT*AMP*SIN(PI*TS))

  ; Response
  DADT(1) = KIN-KOUT*A(1)

  ; 24h clock time
  T24 = T-(INTMTIME3-24)

$ERROR
  REPID = (IREP-1)*10 + ID
  RESPONSE = A(1)
  IF (ICALL == 4) THEN
    DV = RESPONSE
  ENDIF

$SIMULATION(12345) NSUB=10 ONLYSIM NOPREDICTION

$TABLE REPID TIME RESPONSE SHIFT DUR ONEHEADER NOAPPEND NOPRINT 
  FILE=idr_circexa.tab

$SCAT FLAG VS TIME BY REPID
$SCAT RESPONSE VS TIME BY REPID

$PROBLEM step_circadian
;; -----------------------------------------------------------------------------
;; PURPOSE:
;; 1- Illustrate how to turn a step function on at a certain time of the day,
;;    SHIFT, every 24h and back off after a certain duration, DUR, using model
;;    event (MTIME) variables.
;;
;;            ^
;;         1 -|  ---------      ---------      ---------
;;            |  |       |      |       |      |       |
;;            |  |       |      |       |      |       |
;;         0 -|---       --------       --------       -------- FLAG
;;            |
;;            ---------------------------------------------------> TIME
;;               |   |              |              |
;;        SHIFT-24   0              24             48
;;                   <-  SHIFT -><- DUR ->
;;
;;    FLAG is used as the rate of the differential equation dA(1)/dt integrated
;;    over TIME. A(1) increases by the amount DUR every day.
;;
;; 2- Illustrate how NONMEM updates MTIME, MPAST, and MNOW variables over TIME.
;;    $TABLE will only report the value of MTIME, MPAST, MNOW, and any variable
;;    dependent on them at the TIME of event records. Therefore, WRITE
;;    statements are implemented to document the update of these variables at
;;    and in between event records.
;;
;; -----------------------------------------------------------------------------

$INPUT ID TIME AMT CMT EVID DV MDV

$DATA circadian.csv IGNORE=@

$THETA
 (0.0001,0.5,0.9999)   ;-- typical shift as a fraction of 24h
 (0.0001,0.5,0.9999)   ;-- typical duration as a fraction of 24h

$OMEGA
  3                    ;-- IIV in shift (%CV=100*sqrt(1-th1)*eta1)
  3                    ;-- IIV in duration (%CV=100*sqrt(1-th2)*eta2)

$SUBROUTINE ADVAN13 TOL=9

$MODEL NCOMP=1
  COMP=(FLAG)

$PK
  REPID=(IREP-1)*10 + ID

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

  ; Define offset for the start of the circadian rhythm parameter (in case the
  ; step function "starts" before TIME=0
  IF ((SHIFT+DUR) > 24) THEN
    OFFSET = 24
  ELSE
    OFFSET = 0
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

  ; Save mtime(1), mtime(2), and tstate for the TABLE statement
  MT1=MTIME(1)
  MT2=MTIME(2)
  MP1=MPAST(1)
  MP2=MPAST(2)
  STS=TSTATE

  ; Save mnow as a real variable for the WRITE statement
  RMNOW=MNOW

  ; Initialize the file used to output variables at and in-between observation
  ; records
  IF (ICALL == 1) THEN
    "OPEN (FILE='step_circexa.txt',UNIT=96)
    "WRITE (96,'(10A14)') 'REPID','RMNOW','TIME','MT1','MT2','MP1','MP2','TSTATE','INTMTIME'
  ENDIF

  ; Output variables in-between observation records using WRITE statement
  WRITE (96,991) REPID,RMNOW,TIME,MT1,MT2,MP1,MP2,TSTATE,INTMTIME

$DES
  ; Define FLAG variable:
  ; FLAG=1 when T is in [SHIFT+n*24, SHIFT+DUR+n*24], where n=0,1,2,3,...
  ; FLAG=0 otherwise
  FLAG = MPAST(1)-MPAST(2)

  ; Monitor FLAG:
  DADT(1) = FLAG

$ERROR
  ; Output variables at observation records using WRITE statement and flagging
  ; the output row with MNOW=9.
  ; Uncomment the following 3 lines of verbatim code when using nm7.1 or nm7.2,
  ; as TSTATE is not a reserved variable in $ERROR prior to nm7.3
    "FIRST
    "USE PROCM_REAL, ONLY: TSTATE
    "WRITE (96,991) REPID,9.0,TIME,MT1,MT2,MP1,MP2,TSTATE,INTMTIME
  ;
  ; Uncomment the following line of code when using nm7.3
  ;  WRITE (96,991) REPID,9.0,TIME,MT1,MT2,MP1,MP2,TSTATE,INTMTIME

  ; Define ouput for TABLE statement
  FLAGCMT = A(1)
  IF (ICALL == 4) THEN
    DV = FLAGCMT
  ENDIF

$SIMULATION(12345) NSUB=10 ONLYSIM NOPREDICTION

$TABLE REPID TIME FLAG FLAGCMT NOAPPEND NOPRINT ONEHEADER FILE=step_circexa.tab

$TABLE REPID RMNOW TIME MT1 MT2 MP1 MP2 STS INTMTIME SHIFT DUR
  NOAPPEND NOPRINT ONEHEADER FILE=step_circexa_debug.tab
  ; Uncomment the following line of code when using nm7.1 or nm7.2
  FORMAT=s34F13.4
  ; Uncomment the following line of code when using nm7.3
  ;RFORMAT="(10F10.4)"  LFORMAT="(3x,10A10)"

$SCAT FLAG VS TIME BY REPID OBSONLY


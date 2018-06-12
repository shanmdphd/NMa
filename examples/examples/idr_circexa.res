Mon 09/30/2013 
06:26 PM
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
  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  102) $ERROR: Y IS NOT SET AT ICALL=2. VALUES OF PRED DISPLAYED BY
 $TABLE OR $SCATTER MAY BE INCORRECT.
             
 (WARNING  48) DES-DEFINED ITEMS ARE COMPUTED ONLY WHEN EVENT TIME
 INCREASES. E.G., DISPLAYED VALUES ASSOCIATED WITH THE FIRST EVENT RECORD
 OF AN INDIVIDUAL RECORD ARE COMPUTED WITH (THE LAST ADVANCE TO) AN EVENT
 TIME OF THE PRIOR INDIVIDUAL RECORD.
  
License Registered to: IDS NONMEM 7 TEAM
Expiration Date:     2 JUN 2030
Current Date:       30 SEP 2013
Days until program expires :6087
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM) VERSION 7.3beta7.0(P)
 ORIGINALLY DEVELOPED BY STUART BEAL, LEWIS SHEINER, AND ALISON BOECKMANN
 CURRENT DEVELOPERS ARE ROBERT BAUER, ICON DEVELOPMENT SOLUTIONS,
 AND ALISON BOECKMANN. IMPLEMENTATION, EFFICIENCY, AND STANDARDIZATION
 PERFORMED BY NOUS INFOSYSTEMS.

 PROBLEM NO.:         1
 idr_circadian
0DATA CHECKOUT RUN:              NO
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO
 NO. OF DATA RECS IN DATA SET:      420
 NO. OF DATA ITEMS IN DATA SET:   7
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   6
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
   5   2   3   0   0   0   4   0   0   0   0
0LABELS FOR DATA ITEMS:
 ID TIME AMT CMT EVID DV MDV
0(NONBLANK) LABELS FOR PRED-DEFINED ITEMS:
 SHIFT DUR FLAG REPID RESPONSE
0FORMAT FOR DATA:
 (7E5.0)

 TOT. NO. OF OBS RECS:      412
 TOT. NO. OF INDIVIDUALS:      8
0LENGTH OF THETA:   5
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-03     0.5000E+00     0.9999E+00
  0.1000E-03     0.5000E+00     0.9999E+00
  0.0000E+00     0.4000E+01     0.1000E+07
  0.0000E+00     0.5000E+00     0.1000E+07
  0.0000E+00     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 0.3000E+01
 0.0000E+00   0.3000E+01
0SIMULATION STEP OMITTED:    NO
 OBJ FUNC EVALUATED:         NO
0SIMULATION RANDOM METHOD SELECTED (RANMETHOD): DEFAULT
 SOURCE   1:
   SEED1:         12345   SEED2:             0   PSEUDO-NORMAL
 NUMBER OF SUBPROBLEMS:   10
0TABLES STEP OMITTED:    NO
 NO. OF TABLES:           1
 SEED NUMBER (SEED):    11456
 RANMETHOD:
 MC SAMPLES (ESEED):    300
 WRES SQUARE ROOT TYPE:            EIGENVALUE
0-- TABLE   1 --
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                S1PE11.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 REPID TIME RESPONSE SHIFT DUR
0SCATTERPLOT STEP OMITTED:    NO
 FAMILIES OF SCATTERPLOTS:     2
0-- SCATTERPLOT   1 --
 UNIT SLOPE LINE:             NO
0ITEMS TO BE SCATTERED:    TIME    FLAG
     FOR FIXED VALUES OF ITEMS:    REPID
0-- SCATTERPLOT   2 --
 UNIT SLOPE LINE:             NO
0ITEMS TO BE SCATTERED:    TIME    RESPONSE
     FOR FIXED VALUES OF ITEMS:    REPID
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(P)

 GENERAL NONLINEAR KINETICS MODEL USING LSODA (ADVAN13)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   7
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         RESP         ON         YES        YES        YES        YES
    2         OUTPUT       OFF        YES        NO         NO         NO
0NRD VALUE(S) FROM SUBROUTINE TOL:   9
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG
    1            *           *           *           *           *
    2            *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0FIRST MODEL TIME PARAMETER ASSIGNED TO ROW NO.:  8
 LAST  MODEL TIME PARAMETER ASSIGNED TO ROW NO.: 10
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      5
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   3
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    4

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE CALLED AT NONEVENT (ADDITIONAL AND LAGGED) DOSE TIMES AND AT MODEL TIMES.

0PK SUBROUTINE INDICATES THAT COMPARTMENT AMOUNTS ARE INITIALIZED.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
0DES SUBROUTINE USES FULL STORAGE MODE.
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      1

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:     319731802   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.       *                                                                                   *       ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .*                                                                                                  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *  *                                                                                              .
  9.00E+00.                *          *                                                                       ..
          .                                      *         *                                                  .
          .                                                          *        *                               .
          .                                                                           *      *                .
          .                                                                                       *    *      .
          .                                                                                               **  .
          .                                                                                               * * .
          .                                                                                         *   *     .
          .                                                                             *      *              .
          .                                                             *        *                            .
  1.90E+01.                                          *         *                                              ..
          .                    *          *                                                                   .
          . *      *                                                                                          .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *  *                                                                                              .
          .                *          *                                                                       .
          .                                      *         *                                                  .
          .                                                          *        *                               .
          .                                                                           *      *                .
          .                                                                                       *    *      .
          .                                                                                               **  .
  3.90E+01.                                                                                               * * ..
          .                                                                                         *   *     .
          .                                                                             *      *              .
          .                                                             *        *                            .
          .                                          *         *                                              .
          .                    *          *                                                                   .
          . *      *                                                                                          .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                   *                                                               .
          .                                            *      *                                               .
          .                                                           *     *                                 .
          .                                                                        *    *                     .
          .                                                                                  *    *           .
          .                                                                                          *  *     .
          .                                                                                               **  .
          .                                                                                                ** .
          .                                                                                             * *   .
  9.00E+00.                                                                                      *   *        ..
          .                                                                             *    *                .
          .                                                                 *     *                           .
          .                                                   *      *                                        .
          .                                   *       *                                                       .
          .                  *        *                                                                       .
          . *       *                                                                                         .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .          *       *                                                                                .
TIME      .                           *       *                                                               .
          .                                            *      *                                               .
          .                                                           *     *                                 .
          .                                                                        *    *                     .
          .                                                                                  *    *           .
  2.90E+01.                                                                                          *  *     ..
          .                                                                                               **  .
          .                                                                                                ** .
          .                                                                                             * *   .
          .                                                                                      *   *        .
          .                                                                             *    *                .
          .                                                                 *     *                           .
          .                                                   *      *                                        .
          .                                   *       *                                                       .
          .                  *        *                                                                       .
  3.90E+01. *       *                                                                                         ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .          *       *                                                                                .
          .                           *       *                                                               .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          .  *        *                                                                                       .
          .                    *        *                                                                     .
          .                                     *        *                                                    .
          .                                                      *      *                                     .
          .                                                                    *     *                        .
          .                                                                                *    *             .
          .                                                                                         *  *      .
  9.00E+00.                                                                                              * *  ..
          .                                                                                                ** .
          .                                                                                             * *   .
          .                                                                                       *   *       .
          .                                                                             *    *                .
          .                                                                 *     *                           .
          .                                                  *       *                                        .
          .                                 *        *                                                        .
          .                *        *                                                                         .
          . *    *                                                                                            .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          .  *        *                                                                                       .
          .                    *        *                                                                     .
          .                                     *        *                                                    .
  2.90E+01.                                                      *      *                                     ..
          .                                                                    *     *                        .
          .                                                                                *    *             .
          .                                                                                         *  *      .
          .                                                                                              * *  .
          .                                                                                                ** .
          .                                                                                             * *   .
          .                                                                                       *   *       .
          .                                                                             *    *                .
          .                                                                 *     *                           .
  3.90E+01.                                                  *       *                                        ..
          .                                 *        *                                                        .
          .                *        *                                                                         .
          . *    *                                                                                            .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.00E+00
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                         *                                         .
          .                                     *         *                                                   .
          .               *          *                                                                        .
          . *  *                                                                                              .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *           *                                                                                     .
          .                        *          *                                                               .
          .                                             *         *                                           .
          .                                                                *       *                          .
          .                                                                               *     *             .
  1.90E+01.                                                                                          *   *    ..
          .                                                                                                ** .
          .                                                                                              * *  .
          .                                                                                      *    *       .
          .                                                                          *      *                 .
TIME      .                                                         *        *                                .
          .                                     *         *                                                   .
          .               *          *                                                                        .
          . *  *                                                                                              .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *           *                                                                                     .
  3.90E+01.                        *          *                                                               ..
          .                                             *         *                                           .
          .                                                                *       *                          .
          .                                                                               *     *             .
          .                                                                                          *   *    .
          .                                                                                                ** .
          .                                                                                              * *  .
          .                                                                                      *    *       .
          .                                                                          *      *                 .
          .                                                         *        *                                .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      2

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:     718977347   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   1.73E+00            1.73E+00            1.73E+00 RESPONSE   1.73E+00            1.73E+00            1.73E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                       *                                                           .
          .                    *         *                                                                    .
          . *         *                                                                                       .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          .  *         *                                                                                      .
          .                     *         *                                                                   .
          .                                        *       *                                                  .
          .                                                        *       *                                  .
          .                                                                       *     *                     .
          .                                                                                   *   *           .
          .                                                                                           *  *    .
          .                                                                                                ** .
  1.90E+01.                                                                                               **  ..
          .                                                                                           * *     .
          .                                                                                  *    *           .
          .                                                                      *     *                      .
          .                                                       *       *                                   .
TIME      .                                       *       *                                                   .
          .                    *         *                                                                    .
          . *         *                                                                                       .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .  *         *                                                                                      .
          .                     *         *                                                                   .
          .                                        *       *                                                  .
          .                                                        *       *                                  .
  3.90E+01.                                                                       *     *                     ..
          .                                                                                   *   *           .
          .                                                                                           *  *    .
          .                                                                                                ** .
          .                                                                                               **  .
          .                                                                                           * *     .
          .                                                                                  *    *           .
          .                                                                      *     *                      .
          .                                                       *       *                                   .
          .                                       *       *                                                   .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.22E+00            2.04E+00 RESPONSE   2.86E+00            3.68E+00            4.50E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .                                            *                                  *                   .
          .                                                                                               *  *.
          .                                 *                                     *                           .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .                                            *                                  *                   .
  2.90E+01.                                                                                               *  *..
          .                                 *                                     *                           .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                         *                                         .
          .                                                                *      *                           .
          .                                                                             *     *               .
          .                                                                                       *   *       .
          .                                                                                              * *  .
          .                                                                                                ** .
          .                                                                                              * *  .
          .                                                                                       *   *       .
          .                                                                             *     *               .
  9.00E+00.                                                                *      *                           ..
          .                                                 *       *                                         .
          .                               *        *                                                          .
          .             *        *                                                                            .
          . * *                                                                                               .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . *  *                                                                                              .
          .             *         *                                                                           .
          .                                *        *                                                         .
TIME      .                                                 *       *                                         .
          .                                                                *      *                           .
          .                                                                             *     *               .
          .                                                                                       *   *       .
          .                                                                                              * *  .
  2.90E+01.                                                                                                ** ..
          .                                                                                              * *  .
          .                                                                                       *   *       .
          .                                                                             *     *               .
          .                                                                *      *                           .
          .                                                 *       *                                         .
          .                               *        *                                                          .
          .             *        *                                                                            .
          . * *                                                                                               .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *  *                                                                                              .
          .             *         *                                                                           .
          .                                *        *                                                         .
          .                                                 *       *                                         .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  1.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                             *                                                     .
          .                *              *                                                                   .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *       *                                                                                         .
          .                        *              *                                                           .
          .                                                    *            *                                 .
  1.90E+01.                                                                           *        *              ..
          .                                                                                           *   *   .
          .                                                                                                ** .
          .                                                                                       *     *     .
          .                                                                      *         *                  .
TIME      .                                             *            *                                        .
          .                *              *                                                                   .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . *       *                                                                                         .
          .                        *              *                                                           .
          .                                                    *            *                                 .
          .                                                                           *        *              .
          .                                                                                           *   *   .
          .                                                                                                ** .
          .                                                                                       *     *     .
          .                                                                      *         *                  .
          .                                             *            *                                        .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      3

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:    1523644508   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   1.61E+00            1.61E+00            1.61E+00 RESPONSE   1.61E+00            1.61E+00            1.61E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .   *                                                                                               .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                      *            .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                             *     .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                              *    .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                              *    .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                              *    .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                              *    .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                              *    .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   8.60E-01            8.60E-01            8.60E-01 RESPONSE   8.60E-01            8.60E-01            8.60E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .     *                                                                                             .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                       *           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                              *    .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                               *   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                               *   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                              *    .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                              *    .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                              *    .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *             *                                                                                   .
  9.00E+00.                                          *                       *                                ..
          .                                                                                    *         *    .
          .                                                                                          *     *  .
          .                                                      *                     *                      .
          . *                           *                                                                     .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *             *                                                                                   .
          .                                          *                       *                                .
          .                                                                                    *         *    .
          .                                                                                          *     *  .
          .                                                      *                     *                      .
          . *                           *                                                                     .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                           *                                                                       .
          .                                   *      *                                                        .
          .                                                 *      *                                          .
          .                                                              *     *                              .
          .                                                                         *    *                    .
          .                                                                                   *   *           .
          .                                                                                          *  *     .
          .                                                                                               **  .
          .                                                                                                 2 .
  9.00E+00.                                                                                               **  ..
          .                                                                                          *  *     .
          .                                                                                   *   *           .
          .                                                                          *    *                   .
          .                                                              *     *                              .
          .                                                 *      *                                          .
          .                                   *      *                                                        .
          .                   *       *                                                                       .
          .    *       *                                                                                      .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .   *       *                                                                                       .
TIME      .                   *       *                                                                       .
          .                                   *      *                                                        .
          .                                                 *      *                                          .
          .                                                              *     *                              .
          .                                                                         *    *                    .
  2.90E+01.                                                                                   *   *           ..
          .                                                                                          *  *     .
          .                                                                                               **  .
          .                                                                                                 2 .
          .                                                                                               **  .
          .                                                                                          *  *     .
          .                                                                                   *   *           .
          .                                                                          *    *                   .
          .                                                              *     *                              .
          .                                                 *      *                                          .
  3.90E+01.                                   *      *                                                        ..
          .                   *       *                                                                       .
          .    *       *                                                                                      .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .   *       *                                                                                       .
          .                   *       *                                                                       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .              *                                                                                    .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *     *                                                                                           .
          .                    *           *                                                                  .
          .                                            *          *                                           .
          .                                                                 *        *                        .
          .                                                                                 *      *          .
          .                                                                                            *  *   .
  1.90E+01.                                                                                                ** ..
          .                                                                                          *   *    .
          .                                                                              *      *             .
          .                                                            *         *                            .
          .                                      *           *                                                .
TIME      .              *           *                                                                        .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *     *                                                                                           .
          .                    *           *                                                                  .
  3.90E+01.                                            *          *                                           ..
          .                                                                 *        *                        .
          .                                                                                 *      *          .
          .                                                                                            *  *   .
          .                                                                                                ** .
          .                                                                                          *   *    .
          .                                                                              *      *             .
          .                                                            *         *                            .
          .                                      *           *                                                .
          .              *           *                                                                        .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  2.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .             *               *                                                                     .
          .                                            *             *                                        .
          .                                                                      *         *                  .
          .                                                                                        *     *    .
          .                                                                                                2  .
  9.00E+00.                                                                                       *     *     ..
          .                                                                     *         *                   .
          .                                          *             *                                          .
          .          *               *                                                                        .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .             *               *                                                                     .
  2.90E+01.                                            *             *                                        ..
          .                                                                      *         *                  .
          .                                                                                        *     *    .
          .                                                                                                2  .
          .                                                                                       *     *     .
          .                                                                     *         *                   .
          .                                          *             *                                          .
          .          *               *                                                                        .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      4

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:     991404394   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .    *                                                                                              .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                               *   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .  *                                                                                                .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .   *                                                                                               .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *                                                                                               * .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *                                                                                               * .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          .           *                                   *                                                   .
          .                                                                            *                *     .
          .                                                                                    *           *  .
          .                          *                                 *                                      .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          .           *                                   *                                                   .
          .                                                                            *                *     .
          .                                                                                    *           *  .
  2.90E+01.                          *                                 *                                      ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                    *                                                              .
          .       *              *                                                                            .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .                *             *                                                                    .
          .                                            *           *                                          .
          .                                                                   *         *                     .
  1.90E+01.                                                                                     *     *       ..
          .                                                                                               * * .
          .                                                                                             *  *  .
          .                                                                                 *      *          .
          .                                                             *          *                          .
TIME      .                                    *            *                                                 .
          .       *              *                                                                            .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          .                *             *                                                                    .
          .                                            *           *                                          .
          .                                                                   *         *                     .
          .                                                                                     *     *       .
          .                                                                                               * * .
          .                                                                                             *  *  .
          .                                                                                 *      *          .
          .                                                             *          *                          .
          .                                    *            *                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  3.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .           *                                   *                                                   .
          .                                                                            *                *     .
          .                                                                                   *            *  .
          .                        *                                 *                                        .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01.           *                                   *                                                   ..
          .                                                                            *                *     .
          .                                                                                   *            *  .
          .                        *                                 *                                        .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      5

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:     376088129   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   1.20E+00            1.20E+00            1.20E+00 RESPONSE   1.20E+00            1.20E+00            1.20E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .*                                                                                                  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   1.57E+00            1.57E+00            1.57E+00 RESPONSE   1.57E+00            1.57E+00            1.57E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .  *                                                                                                .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                        *          .
          .                                                                                           * *     .
          .                                                                                               **  .
          .                                                                                                 2 .
          .                                                                                               **  .
          .                                                                                           * *     .
          .                                                                                     *  *          .
          .                                                                             *   *                 .
          .                                                                  *     *                          .
  9.00E+00.                                                       *     *                                     ..
          .                                          *     *                                                  .
          .                            *      *                                                               .
          .             *       *                                                                             .
          . *    *                                                                                            .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *   *                                                                                             .
          .            *       *                                                                              .
  1.90E+01.                           *      *                                                                ..
          .                                         *      *                                                  .
          .                                                      *     *                                      .
          .                                                                  *    *                           .
          .                                                                            *   *                  .
TIME      .                                                                                    *   *          .
          .                                                                                           * *     .
          .                                                                                               **  .
          .                                                                                                 2 .
          .                                                                                               **  .
  2.90E+01.                                                                                           * *     ..
          .                                                                                     *  *          .
          .                                                                             *   *                 .
          .                                                                  *     *                          .
          .                                                       *     *                                     .
          .                                          *     *                                                  .
          .                            *      *                                                               .
          .             *       *                                                                             .
          . *    *                                                                                            .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . *   *                                                                                             .
          .            *       *                                                                              .
          .                           *      *                                                                .
          .                                         *      *                                                  .
          .                                                      *     *                                      .
          .                                                                  *    *                           .
          .                                                                            *   *                  .
          .                                                                                    *   *          .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00.                          *                        *                                               ..
          .                                                                       *              *            .
          .                                                                                               **  .
          .                                                                             *            *        .
          .                                   *                       *                                       .
          . *       *                                                                                         .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .                          *                        *                                               .
          .                                                                       *              *            .
          .                                                                                               **  .
          .                                                                             *            *        .
          .                                   *                       *                                       .
          . *       *                                                                                         .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                     *                             .
          .                                                                               *        *          .
          .                                                                                              * *  .
          .                                                                                             *  *  .
          .                                                                              *        *           .
          .                                                      *            *                               .
          .                       *               *                                                           .
          . *    *                                                                                            .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *      *                                                                                          .
          .                         *               *                                                         .
TIME      .                                                        *            *                             .
          .                                                                               *        *          .
          .                                                                                              * *  .
          .                                                                                             *  *  .
          .                                                                              *        *           .
  2.90E+01.                                                      *            *                               ..
          .                       *               *                                                           .
          . *    *                                                                                            .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *      *                                                                                          .
          .                         *               *                                                         .
          .                                                        *            *                             .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  4.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                *  .
          .                                                                                          *  *     .
          .                                                                                *     *            .
          .                                                                   *      *                        .
          .                                                  *        *                                       .
          .                               *         *                                                         .
          .          *          *                                                                             .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .        *         *                                                                                .
          .                            *         *                                                            .
  1.90E+01.                                                *        *                                         ..
          .                                                                 *      *                          .
          .                                                                               *    *              .
          .                                                                                         *   *     .
          .                                                                                               **  .
TIME      .                                                                                                ** .
          .                                                                                          *  *     .
          .                                                                                *     *            .
          .                                                                   *      *                        .
          .                                                  *        *                                       .
  2.90E+01.                               *         *                                                         ..
          .          *          *                                                                             .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          .        *         *                                                                                .
          .                            *         *                                                            .
          .                                                *        *                                         .
          .                                                                 *      *                          .
          .                                                                               *    *              .
          .                                                                                         *   *     .
          .                                                                                               **  .
          .                                                                                                ** .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      6

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:    1379456649   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       *                                                                                   *       ..
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .    *                                                                                              .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .  *                                                                                                .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. *                                                             *                                   ..
          .                                                                             *                  *  .
          . *               *                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *                                                             *                                   .
          .                                                                             *                  *  .
          . *               *                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          .                     *                       *                                                     .
          .                                                                  *                *               .
          .                                                                                             *   * .
          .                                                                                   *         *     .
          .                                               *                   *                               .
          . *                     *                                                                           .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .                     *                       *                                                     .
          .                                                                  *                *               .
          .                                                                                             *   * .
          .                                                                                   *         *     .
  3.90E+01.                                               *                   *                               ..
          . *                     *                                                                           .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                            *                                      .
          . *                   *                                                                             .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          .                 *                                       *                                         .
          .                                                                                      *          * .
TIME      .                                                            *                          *           .
          . *                   *                                                                             .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .                 *                                       *                                         .
          .                                                                                      *          * .
          .                                                            *                          *           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  5.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *                              *                                                                  .
          .                                                                  *                      *         .
          .                                                                                        *        * .
          .                             *                                  *                                  .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *                              *                                                                  .
          .                                                                  *                      *         .
  2.90E+01.                                                                                        *        * ..
          .                             *                                  *                                  .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      7

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:    1444774356   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       *                                                                                   *       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       *                                                                                   *       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.       *                                                                                   *       ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .*                                                                                                  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                             *     .
          .                                                                                               **  .
          .                                                                                                 2 .
          .                                                                                               **  .
          .                                                                                          *  *     .
          .                                                                                   *   *           .
          .                                                                          *    *                   .
          .                                                               *     *                             .
          .                                                  *      *                                         .
  9.00E+00.                                    *       *                                                      ..
          .                     *       *                                                                     .
          .      *       *                                                                                    .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .      *       *                                                                                    .
          .                      *      *                                                                     .
  1.90E+01.                                     *      *                                                      ..
          .                                                   *     *                                         .
          .                                                               *     *                             .
          .                                                                          *    *                   .
          .                                                                                   *   *           .
TIME      .                                                                                          *  *     .
          .                                                                                               **  .
          .                                                                                                 2 .
          .                                                                                               **  .
          .                                                                                          *  *     .
  2.90E+01.                                                                                   *   *           ..
          .                                                                          *    *                   .
          .                                                               *     *                             .
          .                                                  *      *                                         .
          .                                    *       *                                                      .
          .                     *       *                                                                     .
          .      *       *                                                                                    .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          .      *       *                                                                                    .
          .                      *      *                                                                     .
          .                                     *      *                                                      .
          .                                                   *     *                                         .
          .                                                               *     *                             .
          .                                                                          *    *                   .
          .                                                                                   *   *           .
          .                                                                                          *  *     .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                 * .
          .                                                                                                ** .
          .                                                                                             * *   .
          .                                                                                       *  *        .
          .                                                                               *   *               .
          .                                                                     *    *                        .
          .                                                         *     *                                   .
          .                                            *      *                                               .
          .                              *      *                                                             .
  9.00E+00.               *      *                                                                            ..
          . *     *                                                                                           .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *      *                                                                                          .
          .               *       *                                                                           .
          .                              *       *                                                            .
          .                                             *      *                                              .
  1.90E+01.                                                          *     *                                  ..
          .                                                                      *    *                       .
          .                                                                                *   *              .
          .                                                                                       *  *        .
          .                                                                                             * *   .
TIME      .                                                                                                ** .
          .                                                                                                ** .
          .                                                                                             * *   .
          .                                                                                       *  *        .
          .                                                                               *   *               .
  2.90E+01.                                                                     *    *                        ..
          .                                                         *     *                                   .
          .                                            *      *                                               .
          .                              *      *                                                             .
          .               *      *                                                                            .
          . *     *                                                                                           .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. *      *                                                                                          ..
          .               *       *                                                                           .
          .                              *       *                                                            .
          .                                             *      *                                              .
          .                                                          *     *                                  .
          .                                                                      *    *                       .
          .                                                                                *   *              .
          .                                                                                       *  *        .
          .                                                                                             * *   .
          .                                                                                                ** .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *            *                                                                                    .
          .                           *           *                                                           .
          .                                                   *         *                                     .
  9.00E+00.                                                                       *       *                   ..
          .                                                                                      *    *       .
          .                                                                                               **  .
          .                                                                                               **  .
          .                                                                                      *    *       .
          .                                                                      *        *                   .
          .                                                  *          *                                     .
          .                          *           *                                                            .
          . *           *                                                                                     .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . *            *                                                                                    .
          .                           *           *                                                           .
          .                                                   *         *                                     .
          .                                                                       *       *                   .
          .                                                                                      *    *       .
          .                                                                                               **  .
          .                                                                                               **  .
          .                                                                                      *    *       .
          .                                                                      *        *                   .
  3.90E+01.                                                  *          *                                     ..
          .                          *           *                                                            .
          . *           *                                                                                     .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  6.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. *            *                                                                                    ..
          .                             *            *                                                        .
          .                                                       *           *                               .
          .                                                                             *       *             .
          .                                                                                           *   *   .
          .                                                                                                ** .
          .                                                                                        *    *     .
          .                                                                        *        *                 .
          .                                                 *            *                                    .
          .                      *             *                                                              .
  1.90E+01. *      *                                                                                          ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *            *                                                                                    .
          .                             *            *                                                        .
          .                                                       *           *                               .
          .                                                                             *       *             .
          .                                                                                           *   *   .
          .                                                                                                ** .
  3.90E+01.                                                                                        *    *     ..
          .                                                                        *        *                 .
          .                                                 *            *                                    .
          .                      *             *                                                              .
          . *      *                                                                                          .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      8

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:    1500844083   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   2.14E+00            2.14E+00            2.14E+00 RESPONSE   2.14E+00            2.14E+00            2.14E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .*                                                                                                  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .*                                                                                                  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   1.38E+00            1.38E+00            1.38E+00 RESPONSE   1.38E+00            1.38E+00            1.38E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . *        *                                                                                        .
          .                   *        *                                                                      .
          .                                     *        *                                                    .
          .                                                      *      *                                     .
          .                                                                    *      *                       .
          .                                                                                *    *             .
          .                                                                                         *  *      .
  9.00E+00.                                                                                               **  ..
          .                                                                                                ** .
          .                                                                                             * *   .
          .                                                                                      *   *        .
          .                                                                            *    *                 .
          .                                                               *      *                            .
          .                                               *       *                                           .
          .                              *        *                                                           .
          .            *        *                                                                             .
          . * *                                                                                               .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . *        *                                                                                        .
          .                   *        *                                                                      .
          .                                     *        *                                                    .
  2.90E+01.                                                      *      *                                     ..
          .                                                                    *      *                       .
          .                                                                                *    *             .
          .                                                                                         *  *      .
          .                                                                                               **  .
          .                                                                                                ** .
          .                                                                                             * *   .
          .                                                                                      *   *        .
          .                                                                            *    *                 .
          .                                                               *      *                            .
  3.90E+01.                                               *       *                                           ..
          .                              *        *                                                           .
          .            *        *                                                                             .
          . * *                                                                                               .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.04E+00            1.68E+00 RESPONSE   2.32E+00            2.96E+00            3.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .  *                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
  9.00E+00.  2                                                                                                ..
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
  1.90E+01.  2                                                                                                ..
          .  *                                                                                               *.
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
TIME      .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
  2.90E+01.  2                                                                                                ..
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
  3.90E+01.  2                                                                                                ..
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  *                                                                                               *.
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
          .  2                                                                                                .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *         *                                                                                       .
          .                         *            *                                                            .
          .                                                  *           *                                    .
  9.00E+00.                                                                        *       *                  ..
          .                                                                                       *    *      .
          .                                                                                               * * .
          .                                                                                             *  *  .
          .                                                                                  *     *          .
          .                                                               *         *                         .
          .                                        *           *                                              .
          .             *             *                                                                       .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . *         *                                                                                       .
          .                         *            *                                                            .
          .                                                  *           *                                    .
          .                                                                        *       *                  .
          .                                                                                       *    *      .
          .                                                                                               * * .
          .                                                                                             *  *  .
          .                                                                                  *     *          .
          .                                                               *         *                         .
  3.90E+01.                                        *           *                                              ..
          .             *             *                                                                       .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  7.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . **                                                                                                .
          .                      *                  *                                                         .
          .                                                          *              *                         .
          .                                                                                     *      *      .
          .                                                                                                2  .
          .                                                                                   *       *       .
          .                                                       *              *                            .
  9.00E+00.                  *                  *                                                             ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . **                                                                                                .
          .                      *                  *                                                         .
          .                                                          *              *                         .
  2.90E+01.                                                                                     *      *      ..
          .                                                                                                2  .
          .                                                                                   *       *       .
          .                                                       *              *                            .
          .                  *                  *                                                             .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:      9

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:    1999381949   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.       *                                                                                   *       ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .*                                                                                                  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                              *                                    .
          .                                                                            *         *            .
          .                                                                                             *  *  .
          .                                                                                           *    *  .
          .                                                                       *           *               .
          .                                        *                *                                         .
          .  *                  *                                                                             .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .         *                  *                                                                      .
TIME      .                                              *               *                                    .
          .                                                                            *         *            .
          .                                                                                             *  *  .
          .                                                                                           *    *  .
          .                                                                       *           *               .
  2.90E+01.                                        *                *                                         ..
          .  *                  *                                                                             .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .         *                  *                                                                      .
          .                                              *               *                                    .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .     *                  *                                                                          .
  9.00E+00.                                          *                *                                       ..
          .                                                                         *          *              .
          .                                                                                            *   *  .
          .                                                                                             *  *  .
          .                                                                          *          *             .
          .                                            *                *                                     .
          .       *                  *                                                                        .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          .     *                  *                                                                          .
          .                                          *                *                                       .
          .                                                                         *          *              .
          .                                                                                            *   *  .
          .                                                                                             *  *  .
          .                                                                          *          *             .
          .                                            *                *                                     .
  3.90E+01.       *                  *                                                                        ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          .         *                   *                                                                     .
          .                                                *                *                                 .
          .                                                                              *         *          .
          .                                                                                               * * .
          .                                                                                       *      *    .
          .                                                               *             *                     .
          .                           *                  *                                                    .
  1.90E+01. *     *                                                                                           ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .         *                   *                                                                     .
          .                                                *                *                                 .
          .                                                                              *         *          .
  3.90E+01.                                                                                               * * ..
          .                                                                                       *      *    .
          .                                                               *             *                     .
          .                           *                  *                                                    .
          . *     *                                                                                           .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  8.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                      *                                            .
          .                                        *      *                                                   .
          .                          *      *                                                                 .
          .           *       *                                                                               .
          . *  *                                                                                              .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . **                                                                                                .
  9.00E+00.          *       *                                                                                ..
          .                         *      *                                                                  .
          .                                       *      *                                                    .
          .                                                     *     *                                       .
          .                                                                 *    *                            .
          .                                                                           *    *                  .
          .                                                                                    *  *           .
          .                                                                                          *  *     .
          .                                                                                               **  .
          .                                                                                                 2 .
  1.90E+01.                                                                                               **  ..
          .                                                                                           * *     .
          .                                                                                    *   *          .
          .                                                                            *   *                  .
          .                                                                  *    *                           .
TIME      .                                                      *     *                                      .
          .                                        *      *                                                   .
          .                          *      *                                                                 .
          .           *       *                                                                               .
          . *  *                                                                                              .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . **                                                                                                .
          .          *       *                                                                                .
          .                         *      *                                                                  .
          .                                       *      *                                                    .
          .                                                     *     *                                       .
          .                                                                 *    *                            .
          .                                                                           *    *                  .
  3.90E+01.                                                                                    *  *           ..
          .                                                                                          *  *     .
          .                                                                                               **  .
          .                                                                                                 2 .
          .                                                                                               **  .
          .                                                                                           * *     .
          .                                                                                    *   *          .
          .                                                                            *   *                  .
          .                                                                  *    *                           .
          .                                                      *     *                                      .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
  
1
 PROBLEM NO.:         1     SUBPROBLEM NO.:     10

 SIMULATION STEP PERFORMED
 SOURCE  1:
    SEED1:    2010332759   SEED2:             0
 ESTIMATION STEP OMITTED:                 YES 
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             FIRST ORDER (EVALUATION)                           ********************
 ********************                                     SCATTERS                                   ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                           *       .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .       *                                                                                           .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
0RANGE FOR FLAG IS ZERO
 SCATTERPLOT OMITTED
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  9.00E+00.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
  2.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       *                                                                                   *       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                           *       .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  1.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
TIME      .       2                                                                                           .
          .       2                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  3.90E+01.       2                                                                                           ..
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
          .       2                                                                                           .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       FLAG VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -1.00E-01            1.40E-01            3.80E-01 FLAG       6.20E-01            8.60E-01            1.10E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .       *                                                                                           .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  9.00E+00.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  1.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
TIME      .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .       *                                                                                   *       .
  2.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  3.90E+01.                                                                                           2       ..
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
          .                                                                                           2       .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.10E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
  -2.15E+00           -2.15E+00           -2.15E+00 RESPONSE  -2.15E+00           -2.15E+00           -2.15E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.20E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          . *                                                                                                 .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                        *          .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                *  .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.30E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .  *                                                                                                .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.40E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   5.00E-01            5.00E-01            5.00E-01 RESPONSE   5.00E-01            5.00E-01            5.00E-01
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                                   .
          .                                                                                                   .
          .  *                                                                                                .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                         *         .
  2.80E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                 * .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  6.60E+01.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
TIME      .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
  1.04E+02.                                                                                                   ..
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
  1.42E+02.                                                                                                   ..
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                   .
          .                                                                                                  *.
          .                                                                                                   .
          .                                                                                                   .
  1.80E+02. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.50E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                                  *                .
          .                                                                       *     *                     .
          .                                                          *      *                                 .
          .                                            *      *                                               .
          .                            *       *                                                              .
          .           *        *                                                                              .
          . * *                                                                                               .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .        *        *                                                                                 .
          .                         *       *                                                                 .
          .                                         *       *                                                 .
          .                                                        *      *                                   .
          .                                                                     *     *                       .
          .                                                                                *   *              .
  1.90E+01.                                                                                        *  *       ..
          .                                                                                              * *  .
          .                                                                                                ** .
          .                                                                                               **  .
          .                                                                                          * *      .
TIME      .                                                                                  *   *            .
          .                                                                       *     *                     .
          .                                                          *      *                                 .
          .                                            *      *                                               .
          .                            *       *                                                              .
  2.90E+01.           *        *                                                                              ..
          . * *                                                                                               .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          .        *        *                                                                                 .
          .                         *       *                                                                 .
  3.90E+01.                                         *       *                                                 ..
          .                                                        *      *                                   .
          .                                                                     *     *                       .
          .                                                                                *   *              .
          .                                                                                        *  *       .
          .                                                                                              * *  .
          .                                                                                                ** .
          .                                                                                               **  .
          .                                                                                          * *      .
          .                                                                                  *   *            .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.60E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                                                          *                        .
          .                                                                                            *   *  .
          .                                                              *                       *            .
          . *                            *                                                                    .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  9.00E+00. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *      *                                                                                          .
TIME      .                                            *                             *                        .
          .                                                                                            *   *  .
          .                                                              *                       *            .
          . *                            *                                                                    .
          . 2                                                                                                 .
  2.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . *      *                                                                                          .
          .                                            *                             *                        .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.70E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          . *                                                                                                 .
          . 2                                                                                                 .
          .      *              *                                                                             .
          .                                    *             *                                                .
          .                                                              *           *                        .
          .                                                                                   *      *        .
          .                                                                                              * *  .
          .                                                                                             *  *  .
          .                                                                                *       *          .
  9.00E+00.                                                           *           *                           ..
          .                                *             *                                                    .
          . *               *                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  1.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
TIME      . 2                                                                                                 .
          . 2                                                                                                 .
          .      *              *                                                                             .
          .                                    *             *                                                .
          .                                                              *           *                        .
  2.90E+01.                                                                                   *      *        ..
          .                                                                                              * *  .
          .                                                                                             *  *  .
          .                                                                                *       *          .
          .                                                           *           *                           .
          .                                *             *                                                    .
          . *               *                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  3.90E+01. 2                                                                                                 ..
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
          . 2                                                                                                 .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
1       RESPONSE VS. TIME
+                               POINTS ARE ONLY FOR
+                                                   - REPID =  9.80E+01
+                                                                                                   DATA RECS.       1 THROUGH     420
   4.00E-01            1.24E+00            2.08E+00 RESPONSE   2.92E+00            3.76E+00            4.60E+00
          .                   .                   .                   .                   .                   .
          .         .         .         .         .         .         .         .         .         .         .
 -1.00E+00. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .                                              *                                                    .
          .                                  *     *                                                          .
          .                      *     *                                                                      .
          .         *     *                                                                                   .
          . * *                                                                                               .
          .     *      *                                                                                      .
          .                  *     *                                                                          .
          .                              *     *                                                              .
          .                                          *     *                                                  .
  9.00E+00.                                                     *     *                                       ..
          .                                                                *   *                              .
          .                                                                         *   *                     .
          .                                                                                 *  *              .
          .                                                                                       *  *        .
          .                                                                                            * *    .
          .                                                                                               **  .
          .                                                                                                ** .
          .                                                                                                2  .
          .                                                                                             **    .
  1.90E+01.                                                                                        *  *       ..
          .                                                                                   *  *            .
          .                                                                           *   *                   .
          .                                                                  *    *                           .
          .                                                        *     *                                    .
TIME      .                                              *    *                                               .
          .                                  *     *                                                          .
          .                      *     *                                                                      .
          .         *     *                                                                                   .
          . * *                                                                                               .
  2.90E+01.     *      *                                                                                      ..
          .                  *     *                                                                          .
          .                              *     *                                                              .
          .                                          *     *                                                  .
          .                                                     *     *                                       .
          .                                                                *   *                              .
          .                                                                         *   *                     .
          .                                                                                 *  *              .
          .                                                                                       *  *        .
          .                                                                                            * *    .
  3.90E+01.                                                                                               **  ..
          .                                                                                                ** .
          .                                                                                                2  .
          .                                                                                             **    .
          .                                                                                        *  *       .
          .                                                                                   *  *            .
          .                                                                           *   *                   .
          .                                                                  *    *                           .
          .                                                        *     *                                    .
          .                                              *    *                                               .
  4.90E+01. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
          .         .         .         .         .         .         .         .         .         .         .
 #CPUT: Total CPU Time in Seconds,        0.764
Stop Time: 
Mon 09/30/2013 
06:27 PM

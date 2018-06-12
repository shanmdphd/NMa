Mon 09/30/2013 
06:27 PM
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

  
NM-TRAN MESSAGES 
  
 WARNINGS AND ERRORS (IF ANY) FOR PROBLEM    1
             
 (WARNING  2) NM-TRAN INFERS THAT THE DATA ARE POPULATION.
             
 (WARNING  42) A WRITE OR PRINT STATEMENT WILL BE EXECUTED AT ICALL=2. THIS
 MAY PRODUCE EXCESSIVE AMOUNTS OF OUTPUT.
             
 (WARNING  102) $ERROR: Y IS NOT SET AT ICALL=2. VALUES OF PRED DISPLAYED BY
 $TABLE OR $SCATTER MAY BE INCORRECT.
             
 (WARNING  62) AN MTIME VARIABLE IS USED IN THE $ERROR BLOCK. IF THIS
 VARIABLE IS A RANDOM VARIABLE, Y SHOULD NOT DEPEND ON IT.
             
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
 step_circadian
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
 REPID SHIFT DUR INTMTIME MT1 MT2 MP1 MP2 STS RMNOW FLAG FLAGCMT
0FORMAT FOR DATA:
 (7E5.0)

 TOT. NO. OF OBS RECS:      412
 TOT. NO. OF INDIVIDUALS:      8
0LENGTH OF THETA:   2
0DEFAULT THETA BOUNDARY TEST OMITTED:    NO
0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2
0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E-03     0.5000E+00     0.9999E+00
  0.1000E-03     0.5000E+00     0.9999E+00
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
 NO. OF TABLES:           2
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
 REPID TIME FLAG FLAGCMT
0-- TABLE   2 --
04 COLUMNS APPENDED:     NO
 PRINTED:                NO
 HEADER:                YES
 FILE TO BE FORWARDED:   NO
 FORMAT:                s34F13.4
 LFORMAT:
 RFORMAT:
0USER-CHOSEN ITEMS:
 REPID RMNOW TIME MT1 MT2 MP1 MP2 STS INTMTIME SHIFT DUR
0SCATTERPLOT STEP OMITTED:    NO
 FAMILIES OF SCATTERPLOTS:     1
0-- SCATTERPLOT   1 --
0OBSERVATION RECORDS ONLY:   YES
 UNIT SLOPE LINE:             NO
0ITEMS TO BE SCATTERED:    TIME    FLAG
     FOR FIXED VALUES OF ITEMS:    REPID
1DOUBLE PRECISION PREDPP VERSION 7.3beta7.0(P)

 GENERAL NONLINEAR KINETICS MODEL USING LSODA (ADVAN13)
0MODEL SUBROUTINE USER-SUPPLIED - ID NO. 9999
0MAXIMUM NO. OF BASIC PK PARAMETERS:   0
0COMPARTMENT ATTRIBUTES
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         FLAG         ON         YES        YES        YES        YES
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
0FIRST MODEL TIME PARAMETER ASSIGNED TO ROW NO.:  1
 LAST  MODEL TIME PARAMETER ASSIGNED TO ROW NO.:  2
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      5
   TIME DATA ITEM IS DATA ITEM NO.:          2
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   3
   COMPT. NO. DATA ITEM IS DATA ITEM NO.:    4

0PK SUBROUTINE CALLED WITH EVERY EVENT RECORD.
 PK SUBROUTINE CALLED AT NONEVENT (ADDITIONAL AND LAGGED) DOSE TIMES AND AT MODEL TIMES.

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
 #CPUT: Total CPU Time in Seconds,        0.998
Stop Time: 
Mon 09/30/2013 
06:27 PM

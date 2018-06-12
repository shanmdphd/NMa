$PROB Time delay problem
$INPUT ID TIME  DV AMT RATE CMT EVID MDV
$DATA delayed_pre.csv IGNORE=C

$EXTRADOSE  AXIS=TIME(LIN) CMT=3
          file=delayed.csv 

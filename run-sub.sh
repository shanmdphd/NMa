mkdir $1.R74
cp ./$1.ctl /c/nm74g64/run/mpiwini8.pnm $1.R74
nmfe74.bat $1.ctl $1.OUT -parafile=mpiwini8.pnm [NODES]=7 -rundir=$1.R74


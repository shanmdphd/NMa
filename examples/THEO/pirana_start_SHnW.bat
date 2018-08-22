echo Starting NONMEM runs
C:
cd "C:\nma\examples\THEO"
cd nmfe_C01-1_001
CALL "C:\nm74g64\util\nmfe74.bat" C01-1.ctl C01-1.lst   
copy C01-1.lst ..
copy C01-1.ext ..
copy C01-1.phi ..
copy *tabC01-1 .. 
cd ..
echo Pirana: All runs finished / submitted.
echo Done

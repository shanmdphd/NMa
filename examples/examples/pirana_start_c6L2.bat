echo Starting NONMEM runs
C:
cd "C:\nma\examples\examples"
cd nmfe_example8_001
CALL "C:\nm74g64\util\nmfe74.bat" example8.ctl example8.lst   
copy example8.lst ..
copy example8.ext ..
copy example8.phi ..
copy *tabexample8 .. 
cd ..
echo Pirana: All runs finished / submitted.
echo Done
pause


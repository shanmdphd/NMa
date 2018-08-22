echo Starting NONMEM runs
C:
cd "C:\nma\examples\THEO-pirana"
cd nmfe_C01-1_001
CALL "C:\nm74g64\util\nmfe74.bat" C01-1.ctl C01-1.OUT   
copy C01-1.OUT ..
copy C01-1.ext ..
copy C01-1.phi ..
copy *tabC01-1 .. 
cd ..
cd nmfe_C01-2_001
CALL "C:\nm74g64\util\nmfe74.bat" C01-2.ctl C01-2.OUT   
copy C01-2.OUT ..
copy C01-2.ext ..
copy C01-2.phi ..
copy *tabC01-2 .. 
cd ..
cd nmfe_C02-1_001
CALL "C:\nm74g64\util\nmfe74.bat" C02-1.ctl C02-1.OUT   
copy C02-1.OUT ..
copy C02-1.ext ..
copy C02-1.phi ..
copy *tabC02-1 .. 
cd ..
cd nmfe_C02-2_001
CALL "C:\nm74g64\util\nmfe74.bat" C02-2.ctl C02-2.OUT   
copy C02-2.OUT ..
copy C02-2.ext ..
copy C02-2.phi ..
copy *tabC02-2 .. 
cd ..
cd nmfe_C03-1_001
CALL "C:\nm74g64\util\nmfe74.bat" C03-1.ctl C03-1.OUT   
copy C03-1.OUT ..
copy C03-1.ext ..
copy C03-1.phi ..
copy *tabC03-1 .. 
cd ..
cd nmfe_C07-1_001
CALL "C:\nm74g64\util\nmfe74.bat" C07-1.ctl C07-1.OUT   
copy C07-1.OUT ..
copy C07-1.ext ..
copy C07-1.phi ..
copy *tabC07-1 .. 
cd ..
cd nmfe_C07-2_001
CALL "C:\nm74g64\util\nmfe74.bat" C07-2.ctl C07-2.OUT   
copy C07-2.OUT ..
copy C07-2.ext ..
copy C07-2.phi ..
copy *tabC07-2 .. 
cd ..
cd nmfe_C07-3_001
CALL "C:\nm74g64\util\nmfe74.bat" C07-3.ctl C07-3.OUT   
copy C07-3.OUT ..
copy C07-3.ext ..
copy C07-3.phi ..
copy *tabC07-3 .. 
cd ..
cd nmfe_C07-4_001
CALL "C:\nm74g64\util\nmfe74.bat" C07-4.ctl C07-4.OUT   
copy C07-4.OUT ..
copy C07-4.ext ..
copy C07-4.phi ..
copy *tabC07-4 .. 
cd ..
echo Pirana: All runs finished / submitted.
echo Done
pause


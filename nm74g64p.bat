@echo off
REM =================================================== KSBAE MODIFICATION BEGIN
set Editor="C:\Program Files (x86)\AcroSoft\AcroEdit\AcroEdit.exe"
set Rcmd="C:\Program Files\R\R-3.4.2\bin\x64\Rcmd.exe"
set NMaDir=C:\NMa
set ModelNo=%2
REM =================================================== KSBAE MODIFICATION END

set dir=c:\nm74g64
set f=gfortran
set op=-O3 -finit-integer=0 -ffast-math -m64 -mpc64


set lfile=%dir%\license\nonmem.lic
set ndir=%dir%\nm
set mpilib=fmpich2

set exitcode=0
if "%f%"=="df" set mpilib=fmpich2s
set compile_type=ifort
if "%f%"=="gfortran"  set compile_type=gf
if "%f%"=="g95" set compile_type=gf
if "%compile_type%"=="gf" goto liskip1
set obj=obj
set comp=/c
set n=%ndir%\nonmem.%obj% %ndir%\nonmem.lib
goto lidone1
:liskip1
set obj=o
set comp=-c
set n=%ndir%\nonmem.%obj%
:lidone1
rem added 4/2008
set u=
rem added 4/2008
if (%1) == () goto checko
if (%2) == () goto checko
goto checki
:checko
echo Usage:
echo Usage:
echo nmfe74 infile outfile [-background] [-PARAFILE=file.pnm] [-licfile=filename]
echo [-prsame] [-prdefault] [-prcompile] [-tprdefault] [-trskip] [-nobuild]
echo [-maxlim=0, 1, 2, or 3] [-locfile=file.bat]
echo [-rundir=dirname]
echo [-runpdir=dirname] [-nmexec=filename] [-xmloff]
echo [-parafprint=0-99999] [-simparon] [-flushtime>=0.0)
echo "Default:"
echo nmfe74 infile outfile
echo   -licfile=%lfile%
echo   -prsame \(if possible\; else -prcompile\)
echo   -rundir=%cd%
echo   -runpdir=temp_dir -nmexec=nonmem 
set exitcode=100
goto exit
:checki
rem compare the infile and outfile name/extension. If same then exit
rem /i option converts infile and outfile to same case and then compare
if /i '%1' == '%2' set exitcode=101
if /i '%1' == '%2' goto exite1
%dir%\util\OPTION_CHECK %*
if not %errorlevel%==0 set exitcode=102
if not %errorlevel%==0 goto exit
for /F "delims=" %%n in (rundir.set) do set rundir=%%n
if "%rundir%"=="0" set rundir=%cd%
for /F "delims=" %%n in (runpdir.set) do set runpdir=%%n
if "%runpdir%"=="0" set runpdir=temp_dir
set runpdir="%runpdir%"
for /F "delims=" %%n in (nmexec.set) do set nmexec=%%n
if "%nmexec%"=="0" set nmexec=nonmem.exe
for /F "delims=" %%n in (trskip.set) do set trskip=%%n
for /F "delims=" %%n in (prsame.set) do set prsame=%%n
for /F "delims=" %%n in (prdefault.set) do set prdefault=%%n
for /F "delims=" %%n in (maxlim.set) do set maxlim=%%n
for /F "delims=" %%n in (tprdefault.set) do set tprdefault=%%n
for /F "delims=" %%n in (prcompile.set) do set prcompile=%%n
for /F "delims=" %%n in (nobuild.set) do set nobuild=%%n
for /F "delims=" %%n in (licfile.set) do set licfile=%%n
if not "%licfile%" == "0" set lfile=%licfile%
for /F "delims=" %%n in (locfile.set) do set locfile=%%n
if "%locfile%" == "0" set locfile=nmloc.bat

if "%rundir%"=="%cd%" goto lbegin

REM =================================================== KSBAE MODIFICATION BEGIN
MD "%rundir%"
COPY "%1" "%rundir%"
IF EXIST "%rundir%\PRINT.OUT" DEL "%rundir%\PRINT.OUT"
REM =================================================== KSBAE MODIFICATION END

if exist "%rundir%" goto lrundir
echo Directory %rundir% does not exist.  Make sure directory exists with all necessary input files.
set exitcode=103
goto exit
:lrundir
echo Changing directory to %rundir%
del background.set  2>trash.tmp
del licfile.set  2>trash.tmp
del nmexec.set  2>trash.tmp
del parafile.set  2>trash.tmp
del prcompile.set  2>trash.tmp
del nobuild.set  2>trash.tmp
del prdefault.set  2>trash.tmp
del locfile.set  2>trash.tmp
del maxlim.set  2>trash.tmp
del parafprint.set  2>trash.tmp
del flushtime.set  2>trash.tmp
del simparon.set  2>trash.tmp
del tprdefault.set  2>trash.tmp
del prsame.set  2>trash.tmp
del rundir.set  2>trash.tmp
del runpdir.set  2>trash.tmp
del trskip.set  2>trash.tmp
del worker.set  2>trash.tmp
del xmloff.set  2>trash.tmp
pushd "%rundir%"
if not %errorlevel%==0 set exitcode=104
if not %errorlevel%==0 goto exit
rem rerun OPTION_CHECK, make sure appropriate files exist in this new run directory.
%dir%\util\OPTION_CHECK %* -norundir
if not %errorlevel%==0 set exitcode=105
if not %errorlevel%==0 goto exit


:lbegin
echo.  > newline
date /t >%2
time /t >>%2
if exist %1 goto tran
echo %1 does not exist.
copy %2 +newline %2 >trash.tmp
echo %1 does not exist. >>%2
set exitcode=106
goto exite

:tran
if "%trskip%"=="1" goto predpp
if exist FSUBS* del FSUBS*
if exist FLIB del FLIB
if exist FREPORT del FREPORT 
if exist gfcompile.bat del gfcompile.bat
if exist LINK.LNK del LINK.LNK
if exist %f%.txt del %f%.txt
if "%nobuild%"=="1" goto ldelnonmemskip
if exist %nmexec% del %nmexec%
:ldelnonmemskip
if exist FMSG del FMSG
echo Starting NMTRAN
if "%tprdefault%"=="1" set prdefault=1
rem %dir%\tr\nmtran.exe %prdefault% %tprdefault% %maxlim% < %1 >FMSG
rem %dir%\util\nmtran_presort < %1 | %dir%\tr\nmtran.exe %prdefault% %tprdefault% %maxlim% %1 >FMSG
%dir%\util\nmtran_presort < %1 >tempzzzz
%dir%\tr\nmtran.exe <tempzzzz %prdefault% %tprdefault% %maxlim% %1 >FMSG
del tempzzzz
if %errorlevel%==2 set prdefault=0
if "%prdefault%"=="0" goto nmtranc
set prcompile=0
set prsame=0
:nmtranc
copy fsubs fsubs.f90 >trash.tmp
type FMSG

:predpp
set s=
copy %2 +%1 +newline %2 >trash.tmp
@echo NM-TRAN MESSAGES >>%2
copy %2 +FMSG /b %2 /b  >trash.tmp
if exist FREPORT goto predpp2
if not "%trskip%"=="1" set exitcode=107
if not "%trskip%"=="1" goto exit
echo MESSAGES FROM PREVIOUS NMTRAN RUN:
type FMSG
set exitcode=108
goto exit

:predpp2
if "%prdefault%"=="1" goto predpp3
set tempdir=%runpdir%
set pdir=%tempdir%
set rdir=%tempdir%
goto predpp4
:predpp3
set tempdir=%dir%\resource
set pdir=%dir%\pr
set rdir=%dir%\resource

:predpp4
if "%nobuild%"=="1" goto compile
rem the modified NMLINK7.exe can optionally accept arguments from the command line, rather than to
rem take settings from path.for.  This makes NMLINK7 more flexible.
%dir%\util\NMLINK7.exe none \  .%obj%
copy link.lnk linkc.lnk
%dir%\util\NMLINK7.exe %pdir% \  .%obj% DQUOTE
if not exist FSUBS.f90 goto compile
copy FSUBS FSUBS.f90 >trash.tmp
set s=FSUBS.f90

:compile
set compilerpath=
set mpibinpath=
set mpilibpath=
set mpilibname=
if not exist %locfile% set locfile=%dir%\nmloc.bat
if exist %locfile% call %locfile%

if not defined compilerpath goto lcompilerpath
if defined old_path set path=%old_path%
set old_path=%path%
set path=%compilerpath%;%path%
echo Pathlist information for compiler and MPI systems are located in nmpathlist.txt
echo The pathlist %compilerpath% obtained from file %locfile% is placed at the head of PATH >nmpathlist.txt
echo The %f% compiler at one of those locations will be used if it exists. >>nmpathlist.txt
echo If this is not what you want, then modify the file %locfile% accordingly. >>nmpathlist.txt
echo.

:lcompilerpath
if not defined library_path_old set library_path_old=%library_path%
if not defined ld_library_path_old set ld_library_path_old=%ld_library_path%
set library_path=
set ld_library_path=

rem compile FSUBS.f90, and link into libraries, etc., to make nonmem.exe
rem Note that the /I switch is used, so that mod files do not need to be copied from resource directory.

if "%nobuild%"=="1" goto liskip2
rem New section to statically allocate predpp on-the-fly
if '%prdefault%'=='1' goto lpredskip
if '%prsame%'=='1' goto lpredskip
del nmprd4p.mod 2>trash.tmp
del compile.lnk 2>trash.tmp
%dir%\util\COMPILE7.exe %dir%\pr  \  .%obj%

rem goto lpreddo
if '%prcompile%'=='1' goto lpreddo
if not exist %tempdir% goto lpreddo
if not exist %tempdir%\PRSIZES.%obj% goto lpreddo
if not exist %tempdir%\PRGLOBALP.%obj% goto lpreddo
if not exist %tempdir%\prpnm.%obj% goto lpreddo
if not exist %tempdir%\MUMODEL.%obj% goto lpreddo
fc PRSIZES.f90 %tempdir%\PRSIZES.f90 >trash.tmp
if not %errorlevel%==0 goto lpreddo
fc compile.lnk %tempdir%\compile.lnk >trash.tmp
if not %errorlevel%==0 goto lpreddo
fc link.lnk %tempdir%\link.lnk >trash.tmp
if not %errorlevel%==0 goto lpreddo
goto lpredskip

:lpreddo
echo Recompiling certain components.
if exist %tempdir% rd %tempdir% /s /q
mkdir %tempdir%
copy link.lnk %tempdir%\ >trash.tmp
copy linkc.lnk %tempdir%\ >trash.tmp
copy compile.lnk %tempdir%\ >trash.tmp
copy PRSIZES.f90 %tempdir%\ >trash.tmp
set nmcdir=%cd%
cd %tempdir%
copy %dir%\resource\PRGLOBALP.f90  >trash.tmp
copy %dir%\resource\*.mod  >trash.tmp
copy %dir%\resource\total.inc  >trash.tmp
copy %dir%\pr\MUMODEL.f90  >trash.tmp
copy %dir%\pr\prpnm.f90  >trash.tmp
%f% %comp% %op% PRSIZES.f90
%f% %comp% %op% PRGLOBALP.f90
del *.szz  2>trash.tmp
echo hello >MUMODEL.szz
start /B cmd.exe /C %dir%\util\startc.bat %f% MUMODEL.f90 %comp% %op% 
echo hello >prpnm.szz
start /B cmd.exe /C %dir%\util\startc.bat %f% prpnm.f90 %comp% %op% 
for /F "delims=" %%n in (linkc.lnk) do copy %dir%\pr\%%n  >trash.tmp
for /F "delims=" %%n in (compile.lnk) do copy %%n.f90  >trash.tmp
for /F "delims=" %%I in (compile.lnk) do echo hello >%%~nI.szz
for /F "delims=" %%I in (compile.lnk) do start /B cmd.exe /C %dir%\util\startc.bat %f% %%I.f90 %comp% %op%
echo starting wait
:waitloop
if exist *.szz goto waitloop
echo ending wait
rem copy *.%obj% ..\ > trash.tmp
cd %nmcdir%
rem End new section to statically allocate predpp on-the-fly
echo Exiting lpreddo

:lpredskip
if not "%compile_type%"=="gf" goto liskip2
echo Compiling FSUBS
%f% %comp% %op% -I%tempdir% -I%dir%\resource FSUBS.f90
:liskip2
del nmmpi.bat 2>trash.tmp
rem NONMEM_MPI.exe is called just to check what transfer type is to be used, and make sure .pnm file is syntactiaclly okay.
del PARALLEL.pnm 2>trash.tmp
%dir%\util\NONMEM_MPI.exe %1 %3 -licfile=%lfile% -check %* -nmexec=%nmexec%
if %errorlevel%==1 goto lmpi
if not %errorlevel%==0 set exitcode=109
if not %errorlevel%==0 goto exite

:lcompile
if "%nobuild%"=="1" goto lidone3
Echo Building NONMEM Executable
if "%compile_type%"=="gf" goto liskip3
%f% %op% /Fe%nmexec%  %s% %ndir%\pnm_mpid.%obj% /include:%tempdir% /include:%dir%\resource @link.lnk  %rdir%\PRGLOBALP.%obj% %pdir%\MUMODEL.%obj% %dir%\util\PARALLEL.%obj% %pdir%\prpnm.%obj% %n% %u% %dir%\resource\resource.lib > %f%.txt
goto lidone3
:liskip3
%dir%\util\MAKEGFBAT.exe
rem link the files to get nonmem.exe.  gfcompile.bat accepts pertinent arguments
call gfcompile.bat %f% %dir% o %tempdir% %rdir% %pdir% %dir%\util %nmexec%
:lidone3
if not exist %nmexec% set exitcode=110
if not exist %nmexec% goto exite
rem rd %tempdir% /s /q
rem del *.%obj%
rem nonmem is invoked with the control stream file name as an argument.  This allows raw output files
rem to have by default the control stream root name.
rem Console directed output can be redirected to an output file by adding a pipe to the command: >myconsole.txt
echo Starting nonmem execution ...
:lexec
%nmexec% %1 %3 -licfile=%lfile% %* -nmexec=%nmexec%
if exist output goto error0
echo Failure of NONMEM execution. Trying again.
%dir%\util\sleep.exe 1000
goto lcompile
:error0
if not %errorlevel%==0 set exitcode=114
goto lgoon

:lmpi
%dir%\util\getmpi >>nmpathlist.txt
call mpiloc.bat
echo mpi library file to be used is %mpilib% >>nmpathlist.txt
if defined old_path2 set path=%old_path2%
set old_path2=%path%
set path=%mpibinpath%;%path%

:mpicompile
if "%nobuild%"=="1" goto lidone4
Echo Building NONMEM Executable
if "%compile_type%"=="gf" goto liskip4
%f% %op% /Fe%nmexec%  %s% %dir%\mpi\mpi_wini\pnm_mpi.%obj% /include:%tempdir% /include:%dir%\resource @link.lnk %rdir%\PRGLOBALP.%obj% %pdir%\MUMODEL.%obj%  %dir%\util\PARALLEL.%obj% %pdir%\prpnm.%obj% %n% %mpilib% %u% %dir%\resource\resource.lib > %f%.txt
goto lidone4
:liskip4
%dir%\util\MAKEGFBAT.exe -mpi %mpilib%
rem link the files to get nonmem.exe.  gfcompile.bat accepts pertinent arguments
call gfcompile.bat %f% %dir% o %tempdir% %rdir% %pdir% %dir%\util %nmexec%
:lidone4
if not exist %nmexec% set exitcode=111
if not exist %nmexec% goto exite
del nmmpi.bat 2>trash.tmp
rem Now NONMEM_MPI.exe is called again, to create nmmpi.bat file for mpi transfer type.
rem NONMEM_MPI.exe has to be called again for mpi methods, beause it copies nonmem.exe to worker directories.
rem This could not be done until nonmem.exe was created.
%dir%\util\NONMEM_MPI.exe %1 %3 -licfile=%lfile% "-nmexec=%nmexec%" %*
if not %errorlevel%==1 set exitcode=112
if not %errorlevel%==1 goto exite
if not exist nmmpi.bat set exitcode=113
if not exist nmmpi.bat goto exite
rem rd %tempdir% /s /q
rem del *.%obj%
echo Starting MPI version of nonmem execution ...
if exist nmmpi.bat call nmmpi.bat %1 %3 -licfile=%lfile% %* -nmexec=%nmexec%
if not exist nmmpi.bat %nmexec% %1 %3 -licfile=%lfile% %* -nmexec=%nmexec%
if %errorlevel%==0 goto lgoon
if %errorlevel%==1 goto lgoon
if exist output goto mpierror0
echo Failure of NONMEM execution. Trying again.
%dir%\util\sleep.exe 1000
goto mpicompile
:mpierror0
set exitcode=115

:lgoon
REM =================================================== KSBAE MODIFICATION BEGIN
IF EXIST OUTPUT %NMaDir%\PRINT4.EXE < OUTPUT
IF EXIST PRINTOUT REN PRINTOUT PRINT.OUT
%Rcmd% BATCH %NMaDir%\PP.R
IF EXIST ??tab ren ??tab ??tab%ModelNo%
IF EXIST ??tab*.OUT ren ??tab*.OUT ??tab*.
IF EXIST ??tab*. copy ??tab* ..
IF EXIST nonmem.exe DEL nonmem.exe
REM =================================================== KSBAE MODIFICATION END

echo Done with nonmem execution
rem changed 4/2008
rem copy %2 +%1 +output %2
copy %2 +newline +output %2 >trash.tmp
echo Stop Time: >>%2
date /t >>%2
time /t >>%2
del output
rem if exist LINK.LNK del LINK.LNK
rem changed 4/2008
rem if exist IFORT.txt del IFORT.txt
goto exit

:exite1
echo output file name/extension should not be same as input file name/extension

:exite
echo No nonmem execution.
if exist %f%.txt echo There may be error messages in file %f%.txt.

:exit
del trash.tmp
if defined old_path2 set path=%old_path2%
if defined old_path set path=%old_path%
set old_path=
set old_path2=
if defined library_path_old set library_path=%library_path_old%
if defined ld_library_path_old set ld_library_path=%ld_library_path_old%
set dir=
set ndir=
set n=
set f=
set u=
popd
verify > NUL

REM =================================================== KSBAE MODIFICATION BEGIN
ECHO 
PAUSE
%Editor% "%rundir%\PRINT.OUT"
REM =================================================== KSBAE MODIFICATION END

exit /b %exitcode%

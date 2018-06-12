@echo off 
set dir="C:\NONMEM7.3beta7.0P"
set f=ifort
rem set op=/nologo /nbs /4Yportlib /Gs /Ob1ti /Qprec-div /traceback
rem set op=/Gs /nologo /nbs /w /Ob1ti /O1 /Qprec-div /4Yportlib /traceback /check:bounds
set op=/nologo /nbs /4Yportlib /Gs /Ob1ti /Qprec-div

call %dir%\util\nmfe73original.bat %*

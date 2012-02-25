@ECHO OFF

REM Configure our environment variables.  We assume that TASM is
REM installed in ~\bin and that your MFORTH source is located in
REM ~\Dev\main\MFORTH.
SETLOCAL

SET TASMDIR=%USERPROFILE%\bin\tasm3.2
SET TASM=%TASMDIR%\tasm.exe
SET TASMTABS=%TASMDIR%

SET MFORTHDIR=%USERPROFILE%\Dev\main\MFORTH
SET BINDIR=%MFORTHDIR%\bin
SET SRCDIR=%MFORTHDIR%\src\MFORTH

REM Enable the line below to generate a build with the MFORTH profiler.
REM SET PROFILER=-dPROFILER


REM Get the Perforce change number for this build if -v was specified.
IF "%1"=="-v" (GOTO :getchange) ELSE (GOTO :nochange)

:getchange
FOR /F "Tokens=2" %%I IN ('p4 changes -i -m 1 //depot/main/MFORTH/src/...#have') DO SET MFORTH_CHANGE=%%I
ECHO Change number is %MFORTH_CHANGE%.
GOTO assemble

:nochange
SET MFORTH_CHANGE=0
GOTO assemble


:assemble
REM TASM can only process relative includes if they are relative to the
REM current directory.  That means that we need to change into the
REM source directory before we try to assemble MFORTH.  We store the
REM current directory so that we can return to that directory after
REM assembling.
SET PREVDIR=%CD%
CD %SRCDIR%

REM Assemble MFORTH using the standard, linked-list dictionary.
%TASM% -85 -s -b -f00 ^
	%PROFILER% -dMFORTH_CHANGE=%MFORTH_CHANGE% ^
	main.asm ^
	%BINDIR%\MFORTH.BX ^
	%BINDIR%\MFORTH.LST ^
	%BINDIR%\MFORTH.EXP ^
	%BINDIR%\MFORTH.SYM

REM Use PhashGen to generate a perfect hash table for the dictionary.
%MFORTHDIR%\tools\PhashGen.exe ^
	%BINDIR%\MFORTH.BX ^
	%BINDIR%\MFORTH.SYM ^
	%SRCDIR%\phash.asm

REM Assemble MFORTH again, this time using the perfect hash table.
%TASM% -85 -s -b -f00 ^
	%PROFILER% -dPHASH -dMFORTH_CHANGE=%MFORTH_CHANGE% ^
	main.asm ^
	%BINDIR%\MFORTH.BX ^
	%BINDIR%\MFORTH.LST ^
	%BINDIR%\MFORTH.EXP ^
	%BINDIR%\MFORTH.SYM

REM Restore the previous directory.
CD %PREVDIR%

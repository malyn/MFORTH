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


REM Get the Git commit hash for this build if -v was specified.
IF "%1"=="-v" (GOTO :getcommit) ELSE (GOTO :nocommit)

:getcommit
FOR /F "Tokens=1" %%I IN ('git log -1 --oneline HEAD') DO SET MFORTH_COMMIT="%%I"
ECHO Commit hash is %MFORTH_COMMIT%.
GOTO assemble

:nocommit
SET MFORTH_COMMIT="0000000"
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
	%PROFILER% -dMFORTH_COMMIT="\"%MFORTH_COMMIT%\"" ^
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
	%PROFILER% -dPHASH -dMFORTH_COMMIT="\"%MFORTH_COMMIT%\"" ^
	main.asm ^
	%BINDIR%\MFORTH.BX ^
	%BINDIR%\MFORTH.LST ^
	%BINDIR%\MFORTH.EXP ^
	%BINDIR%\MFORTH.SYM

REM Restore the previous directory.
CD %PREVDIR%

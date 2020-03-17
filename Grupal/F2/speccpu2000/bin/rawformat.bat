@rem = '--*-Perl-*--';
@rem = '
@echo off
rem
rem rawformat.bat
rem
rem Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
rem  All Rights Reserved
rem
rem $Id: rawformat.bat 1391 2005-05-11 00:09:35Z cloyce $

rem Find top of SPEC heirarchy
rem Add hack to set the spec environment variable automatically jh/18/mar/98

if not "%SPEC%."=="." goto SPEC_env_defined
    echo The environment variable SPEC should point to the source of the
    echo SPEC distribution (as an absolute path).  I will now try to set
    echo the variable for you...
    if exist %temp%\set_spec_loc.bat del %temp%\set_spec_loc.bat
    echo >%temp%\set_spec_loc.bat set SPEC=^^
    cd >>%temp%\set_spec_loc.bat
    call %temp%\set_spec_loc.bat
    echo.
    echo SPEC is set to %SPEC%
    echo If this is NOT what you want, press control-C
    echo set SPEC= >%temp%\setspecloc.bat
    pause
:SPEC_env_defined
    if exist %SPEC%\bin\runspec goto SPEC_env_ok
    echo It appears that the environment variable SPEC (defined as %SPEC%)
    echo is incorrect.  Please check the value and try again.
    goto done
:SPEC_env_ok

:set_path
echo %Path%>%temp%\specpath.txt
findstr -l "%SPEC%\bin" %temp%\specpath.txt >nul 2>&1
if not errorlevel 1 goto path_ok
    set Path=%SPEC%\bin;%Path%
:path_ok
del %temp%\specpath.txt

set list=%SPEC%\bin\rawformat %1
:getnext
shift
if "%1"=="" goto endgetnext
set list=%list% %1
goto getnext
:endgetnext
specperl %list%

:done









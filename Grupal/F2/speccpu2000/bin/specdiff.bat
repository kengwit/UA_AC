@rem = '--*-Perl-*--';
@rem = '
@echo off
rem
rem specdiff.bat
rem
rem Copyright (C) 1999-2000 Standard Performance Evaluation Corporation
rem  All Rights Reserved
rem
rem $Id: specdiff.bat 1391 2005-05-11 00:09:35Z cloyce $

set list=%SPEC%\bin\specdiff %1
:getnext
shift
if "%1"=="" goto endgetnext
set list=%list% %1
goto getnext
:endgetnext
specperl %list%

:done









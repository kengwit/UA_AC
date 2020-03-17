@echo off
rem echo on

rem
rem packagetools.bat
rem
rem Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
rem  All Rights Reserved
rem
rem $Id: packagetools.bat 1427 2005-06-17 15:59:40Z cloyce $

rem Attempt to do same thing on NT that packagetools does on Unix
rem j.henning 7 jul 99
rem Note that the NT command interpreter uses fewer features here
rem than the Unix version, so could be fooled if (for example) more
rem spec_mumble executable files were to be added to bin.  If in doubt,
rem check the Unix version.

set SUITE=cpu2000

if not "%SPEC%."=="." goto SPEC_env_defined
    echo Please run shrc.bat before attempting packagetools
    goto end

:SPEC_env_defined
if not "%1%."=="." goto arch_defined
    echo Usage: packagetools arch-name
    goto end

:arch_defined
cd %SPEC%
echo Setting up the tools\bin\%1% directory
mkdir tools\bin\%1%
copy bin\specbzip2.exe tools\bin\%1%
copy bin\spectar.exe  tools\bin\%1%
copy bin\specmd5sum.exe  tools\bin\%1%
rem Make a marker for install.bat
echo yow > tools\bin\%1%\unbundled

if exist %temp%\set_spec_v.bat del %temp%\set_spec_v.bat 
echo >%temp%\set_spec_v.bat set SPECv=^^
type >>%temp%\set_spec_v.bat bin\version
call %temp%\set_spec_v.bat 
del %temp%\set_spec_v.bat 

rem tar in one step and bzip2 in the next because otherwise there
rem seem to be random instances of 'broken pipe'

rem Remove all old instances of tools tarballs
echo Cleaning up old builds (if any)
if exist tools\bin\%1\*tools-%1.tar del tools\bin\%1\*tools-%1.tar 
if exist tools\bin\%1\*tools-%1.tar.gz del tools\bin\%1\*tools-%1.tar.gz 
if exist tools\bin\%1\*tools-%1.tar.bz2 del tools\bin\%1\*tools-%1.tar.bz2 

rem Generate sums for the stuff that'll be inside the tarball
echo Generating checksums for installed tools
bin\specmd5sum -b -e bin/perl58.dll bin/specbzip2.exe bin/specinvoke.exe bin/specinvoke_pm.exe bin/specmake.exe bin/specmd5sum.exe bin/spectar.exe bin/specperl.exe bin/lib > SUMS.tools

rem Generate the tarball
echo Making the big tarball of everything
bin\spectar -cf tools/bin/%1/%SUITE%tools-%1.tar SUMS.tools bin/perl58.dll bin/specbzip2.exe bin/specinvoke.exe bin/specinvoke_pm.exe bin/specmake.exe bin/specmd5sum.exe bin/spectar.exe bin/specperl.exe bin/lib
rem ...and compress it.
echo Compressing it...
bin\specbzip2 -9v tools/bin/%1/%SUITE%tools-%1.tar 

rem Now make the tarball that will be sent in

rem Remove any existing old stuff
echo Removing old tools builds (if any)
if exist %1-%SPECv%.tar del %1-%SPECv%.tar 
if exist tools\bin\%1\specgzip.exe del tools\bin\%1\specgzip.exe
if exist %1-%SPECv%.tar.gz del %1-%SPECv%.tar.gz 
if exist %1-%SPECv%.tar.bz2 del %1-%SPECv%.tar.bz2

rem Tar up the tools\bin\arch directory
echo Making the tarball for submission
bin\spectar --exclude=.svn -cvf %1-%SPECv%.tar tools/bin/%1
rem ...and compress it.
echo Compressing it...
bin\specbzip2 -9v %1-%SPECv%.tar 

echo .
echo *******************************************************************
echo .
echo The tarball to submit for inclusion in the distribution is
echo .
echo %1-%SPECv%.tar.bz2
echo .
echo ABSOLUTELY DO NOT submit the tarball in tools\bin\%1
echo as it is not complete.
echo .
echo *******************************************************************
echo .

:end

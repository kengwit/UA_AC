@echo off
if not "%INSTALL_DEBUG%."=="." @echo on
rem
rem
rem  install.bat - installs SPEC CPU2006 or just tool binaries
rem  Copyright (c) 1995-2005 Standard Performance Evaluation Corporation
rem
rem  Authors:  Bill Carr (CPU95 version)
rem            John Henning
rem            Cloyce D. Spradling
rem            Rahul Rahatekar
rem  $Id: install.bat 1596 2005-09-01 00:36:41Z cloyce $
rem

set SUITE=cpu2000

rem Currently the only platform that runs Windows is x86
if not "%CPU%."=="." goto cpu_set
set CPU=i386
rem echo The instance of Windows that you're running doesn't seem to set the
rem echo CPU environment variable.  Please set it appropriately by hand.  For
rem echo valid values, look in the \tools\bin directory of the distribution CD-ROM.
rem echo Toolsets for Windows have names like 'windows-i386'.
rem echo For example, if you have an Intel x86-based machine, you would
rem echo .
rem echo   set CPU=i386
rem echo .
rem echo Whereas if you have an Alpha NT system, you would
rem echo .
rem echo   set CPU=alpha
rem echo .
rem echo If there is no toolset for your processor, you will need to build the
rem echo tools yourself.  In that case, set CPU to a reasonable value.
rem echo .
rem echo After setting the CPU environment variable, please re-run install.bat.
rem goto really_done
:cpu_set

if not "%temp%."=="." goto temp_set
echo This install process will write several small temporary files.  Currently,
echo the TEMP environment variable is not set.  Please set it to the full
echo pathname of a directory where it's okay to write those files.  After
echo that variable is set, please re-run install.bat.
goto really_done
:temp_set

rem This is a guess, but it should be a good one
set SPEC_VALID_ARCH=windows-%cpu%

rem Find top of SPEC heirarchy

if not "%SPEC%."=="." goto SPEC_env_defined
    echo The environment variable SPEC should point to the source of the
    echo SPEC distribution (as an absolute path).  I will now try to set
    echo the variable for you...
    if exist %temp%\set_spec_loc.bat del %temp%\set_spec_loc.bat
    echo >%temp%\set_spec_loc.bat set SPEC=^^
    cd >>%temp%\set_spec_loc.bat
    call %temp%\set_spec_loc.bat
    set SPEC=%SPEC%
    echo.
    echo SPEC is set to %SPEC%
    echo If this is NOT what you want, press control-C 
    echo set SPEC= >%temp%\setspecloc.bat
    pause
:SPEC_env_defined
    echo Installing from %SPEC%
    if exist %SPEC%\install.bat goto SPEC_env_ok
    echo It appears that the environment variable SPEC (defined as %SPEC%)
    echo is incorrect.  Please check the value and try again.
    goto really_done
:SPEC_env_ok


set SPECNEWDEV=%1
if "%SPECNEWDEV%." == "." goto dev_ok
    echo %SPECNEWDEV%>%temp%\specdev.txt
    findstr -r "^[a-zA-Z]:$" %temp%\specdev.txt >nul 2>&1
    if errorlevel 1 goto dev_not_ok
rem     The specperl test a few lines down will fail if the
rem     current directory is a CD.  So if the user has specified
rem     a valid destination, let's go there!
        %SPECNEWDEV%
        goto dev_ok
:dev_not_ok
	echo First parameter must be in the form c:
	goto really_done
:dev_ok
set SPECNEWPATH=%2
if "%SPECNEWPATH%." == "." goto path_ok
    echo %SPECNEWPATH%>%temp%\specdev.txt
    findstr -r "^\\\\" %temp%\specdev.txt >nul 2>&1
    if not errorlevel 1 goto path_ok
	echo Second parameter must be a rooted directory e.g. \%SUITE%
	goto really_done
:path_ok
set SPECNEW=%1%2

rem Check to see if we are writable
if not "%SPECNEW%." == "." goto specnew_set
set SPECNEW=%SPEC%
goto specnew_exists
:specnew_set
if exist %SPECNEW% goto specnew_exists
	mkdir %SPECNEW%
	if errorlevel 1 goto :specnew_error
	goto specnew_exists
:specnew_error
	echo There was an error creating the %SPECNEW% directory.
	goto really_done
:specnew_exists
set SPECNEWTMPFILE=%SPECNEW%\__SPEC__.TMP
echo hello >%SPECNEWTMPFILE% 2>&1
dir %SPECNEWTMPFILE% >nul 2>&1
if errorlevel 1 goto write_error
	del %SPECNEWTMPFILE%
	goto no_write_error
:write_error
	echo You seem to be installing from a CD-ROM.  Please re-run
	echo this command with the first parameter being the destination
	echo drive letter and the second parameter being the path of the 
	echo directory into which you wish to install the benchmark suite
	echo For example:   install c: \%SUITE%
	goto really_done
:no_write_error

set SPECARCH=none

set TAR_VERBOSE=
set EXCLUDE_OPTS=
set EXCLUDE_PAT=
if "%VERBOSE%."=="." goto be_quiet
set TAR_VERBOSE=-v
:be_quiet

if not exist %SPEC%\tools\bin\windows-%cpu%\specbzip2.exe goto pre_copy
set toolsbindir=%SPEC%\tools\bin\windows-%cpu%
set SPEC_VALID_ARCH=windows-%cpu%
set INSTALL_DONT_COPY_TOOLS=TRUE
set EXCLUDE_OPTS=%EXCLUDE_OPTS% --exclude=tools/*
set EXCLUDE_PAT=%EXCLUDE_PAT% tools/

set SPECARCH=%SPEC_VALID_ARCH%

:pre_copy

if "%INSTALL_DONT_COPY_BENCHSPEC%." == "." goto copy_benchspec
set EXCLUDE_OPTS=%EXCLUDE_OPTS% --exclude=benchspec/*
set EXCLUDE_PAT=%EXCLUDE_PAT% benchspec/
:copy_benchspec

if not %SPECNEW%.==%SPEC%. goto copy_doc_etc
set EXCLUDE_OPTS=%EXCLUDE_OPTS% --exclude=bin/* --exclude=config/* --exclude=docs/* --exclude=result/*
set EXCLUDE_PAT=%EXCLUDE_PAT% bin/ config/ docs/ result/
:copy_doc_etc

rem Here's a part where we try to unpack a tarball that lives in original.src
rem on the installation media.  If it's not found, it's not necessarily fatal;
rem this situation could arise when running install.bat to install tools
rem binaries in a benchmark tree that has already been unpacked.

echo Unpacking benchmark files
cd /d %SPECNEW%
if "%SPECARCH%."=="none." goto dont_have_exes
    %toolsbindir%\specbzip2 -dc %SPEC%\original.src\%SUITE%.tbz 2>NUL: | %toolsbindir%\spectar %EXCLUDE_OPTS% %TAR_VERBOSE% -xf -
    echo %SPECARCH% > bin\packagename
    if not "%SPECNOCHECK%." == "." goto md5_ok
    echo Checking the integrity of your source tree...
    type %SPEC%\MANIFEST > MANIFEST.tmp
    CALL :cull_manifest MANIFEST.tmp original.src/
rem Also remove things from the manifest that have been excluded from the tar
    for %%I in (%EXCLUDE_PAT%) DO CALL :cull_manifest MANIFEST.tmp %%I
    %toolsbindir%\specmd5sum -e -c MANIFEST.tmp > manifest.check.out
    if errorlevel 1 goto md5_bad
    goto md5_ok
:md5_bad
    findstr /V /R /C:": OK$" manifest.check.out
    del /F /Q MANIFEST.tmp
    echo Package integrity check failed!
    goto bad_end
:md5_ok
    del /F /Q MANIFEST.tmp
    del manifest.check.out
    echo Unpacking tools binaries
    %toolsbindir%\specbzip2 -dc %toolsbindir%\%SUITE%tools-%SPEC_VALID_ARCH%.tar.bz2 2>NUL: | %toolsbindir%\spectar %TAR_VERBOSE% -xf - 
    
    echo Setting SPEC environment variable to %SPECNEW%
    set SPEC=%SPECNEW%
rem There's no NT relocate, or it'd be run here
    if not "%SPECNOCHECK%." == "." goto end_build
    echo Checking the integrity of your binary tools...
    %toolsbindir%\specmd5sum -e -c SUMS.tools > toolcheck.out
    if errorlevel 1 goto tools_bad
    goto end_build
:tools_bad
    findstr /V /R /C:": OK$" toolcheck.out
    echo Binary tools integrity check failed!
    del toolcheck.out
    goto bad_end
:dont_have_exes

rem So we don't have a pre-built executable. 
if not "%INSTALL_DEBUG%."=="." @echo on
rem Re-home ourselves
echo Setting SPEC environment variable to %SPECNEW%
set SPEC=%SPECNEW%
%SPECNEWDEV%
cd %SPEC%

rem Ask the question about compiling here so the person can go away
rem (hopefully) and let the thing install on it's own.
if not "%SPECARCH%."=="none." goto dont_ask_build
    echo We do not appear to have vendor supplied binaries for your
    echo architecture.  You will have to compile specmake and specperl
    echo by yourself.  Please read \docs\tools_build.txt and
    echo \tools\src\buildtools.bat.
    echo --------------------------------------------------------------
    echo If you wish I can attempt to build the tools myself.
    echo I'm not very intelligent so if anything goes wrong I'm just going
    echo to stop.
    echo If you do not hit CTRL-C *NOW*, I will attempt to execute the build.
    pause
:dont_ask_build

%SPECNEWDEV%
rem Let buildtools worry about whether or not their build environment
rem works.
if exist %SPEC%\tools\bin goto tools_bin_dir_exists
    echo Creating directory %SPEC%\tools\bin
    mkdir %SPEC%\tools\bin
:tools_bin_dir_exists
if exist %SPEC%\tools\bin\%SPEC_VALID_ARCH% goto arch_dir_exists
    echo Creating directory %SPEC%\tools\bin\%SPEC_VALID_ARCH%
    mkdir %SPEC%\tools\bin\%SPEC_VALID_ARCH%
:arch_dir_exists

echo Running %SPECNEW%\tools\src\buildtools.bat
cd %SPECNEW%\tools\src
buildtools.bat

:end_build
del /F /Q toolcheck.out

echo %Path%>%temp%\specpath.txt
findstr -l "%SPECNEW%\bin" %temp%\specpath.txt >nul 2>&1
if not errorlevel 1 goto path_ok
    set Path=%SPECNEW%\bin;%Path%
:path_ok
del %temp%\specpath.txt

:done
cd %SPEC%

rem Fix up the MANIFEST
rem set SPECPERLLIB=%SPEC%\bin;%SPEC%\bin\lib;%SPEC%\bin\lib\site
rem bin\specperl bin\scripts.misc\convert_manifest

:really_done
set SPECARCH=
set SPECNEW=
set SPECNEWDEV=
set SPECNEWPATH=
set SPECNEWTMPFILE=
set SPEC_MAKE_LEVEL=
set SPEC_PERL_LEVEL=
set SPEC_VALID_ARCH=
goto end_it_all

:bad_end

rem Check for WinZip munging
if not exist %SPEC%\test\WinZip.guard goto end_it_all
  %toolsbindir%\specmd5sum -e %SPEC%\test\WinZip.guard > %temp%\winzip.test
  findstr /C:"1401a09c7fed3b499c79d987f1bf11e7" %temp%\winzip.test >nul 2>&1
  if errorlevel 1 goto no_winzip_munge
  del %temp%\winzip.test
  echo .
  echo It looks like WinZip has helpfully performed CR/LF conversion on files
  echo it's extracted from the tarball.  Unfortunately, this has corrupted
  echo most of the files in the distribution.
  echo Please DISABLE the "Automatic CR/LF conversion for TAR files" in the
  echo WinZip preferences before unpacking the distribution tarball, or
  echo preferably, use specbzip2 and spectar (located in %toolsbindir%)
  echo to unpack the distribution.
  echo .
:no_winzip_munge

goto end_it_all
:cull_manifest
rem This is how to get multiple commands into a FOR loop.  Geez.
rem The name of the file to cull is in %1, and the strings that should
rem be removed are in %2.
findstr /V /C:" %2" %1 > cpu2000.cull.filetemp
del /F /Q %1
rename cpu2000.cull.filetemp %1
goto end_it_all

:end_it_all

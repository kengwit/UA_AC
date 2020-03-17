echo off
rem
rem shrc.bat
rem
rem Copyright (C) 1999-2005 Standard Performance Evaluation Corporation
rem  All Rights Reserved
rem
rem $Id: shrc.bat 2818 2005-06-03 02:43:52Z cloyce $
rem
rem    This file sets up your path for SPEC CPU2000.  It is intended 
rem    to be customized as you may wish.  You will need to add a path
rem    to your compiler(s).  Look for lines that contain 'BEGIN EDIT HERE'.
rem
rem    Usage: cd <specroot>
rem           shrc
rem
rem    Authors: J.Henning, Cloyce Spradling
rem    Last updated: 9 December 1999
rem    ---------------------------------------------------------------

 set SHRC_COMPILER_PATH_SET=yes
rem
rem The SPEC CPU2000 benchmarks are supplied in source code form
rem (C, C++, Fortran).  In order to run the benchmarks, you need to
rem compile the benchmarks, or someone else will need to compile them
rem for you.  If you are compiling them, set your path appropriately,
rem using the examples below as a guideline.  Then, uncomment
rem the line that follows (just remove the word 'rem').
rem set SHRC_COMPILER_PATH_SET=yes
rem
rem If someone else has compiled the benchmarks for you, then the
rem only change you have to make to this file is to uncomment the
rem line that follows (just remove the word 'rem')
rem set SHRC_PRECOMPILED=yes
rem
set SHRC_COMPILER_PATH_SET=yes

if "%SHRC_COMPILER_PATH_SET%x"=="yesx" goto SHRC_compiler_path_set
if "%SHRC_PRECOMPILED%x"=="yesx"       goto SHRC_compiler_path_set
echo Please read and edit shrc.bat before attempting to execute it!
goto end

:SHRC_compiler_path_set

rem    First, set the path to a minimal level.  This is important because 
rem    it was observed during testing of CPU2000 that the presence of 
rem    certain other tools (such as the MKS toolkit) can create 
rem    substantial confusion for the SPEC tools.

set PATH=^
%SystemRoot%\system32;^
%SystemRoot%;

rem    Now, add some compilers.  
rem    If the path to your compiler includes space characters, you
rem    may find that some utilities behave better if you define both
rem    the long name and the shorter, "8dot3" name.  You can discover
rem    the short name with the "dir /x" command.
rem
rem    Many compilers include a batch file which sets the path,  
rem    typically called XXXvars.bat.  The example lines below call the 
rem    batch files for Visual C++ version 6 and Digital Fortran version 6, 
rem    provided that both of these are installed on drive C.  You should 
rem    adjust for your compilers.

set SHRC_COMPILER_PATH_SET=yes
rem One way to set your path is to use the utilities provided by your
rem compiler.  **This is the preferred way to set your compiler path**

 call "F:\VST\VC\Auxiliary\Build\vcvars32.bat"
 call "F:\VST\VC\Auxiliary\Build\vcvars32.bat"

rem
rem -OR- (*ONLY* if your vendor does not provide you with a handy batch file)
rem

rem If you know where the executable is on your system, you can also
rem add the path using the set command

rem set PATH="%PATH%;c:\program files\microsoft visual studio\vc98\bin"
rem set PATH="%PATH%;c:\program files\microsoft visual studio\df98\bin"

rem The paths above are only examples; be sure to replace the paths with
rem correct ones for your compiler.
set SHRC_COMPILER_PATH_SET=yes

rem set SPEC environment variable
rem

rem if the SPEC variable is set, and it points to something that looks
rem reasonable, then use that, otherwise fall through
if not "%SPEC%."=="." goto SPEC_env_not_defined
if exist %SPEC%\bin\runspec goto SPEC_env_defined
:SPEC_env_not_defined

rem we don't search the directory path, so you have to be in the top level
rem of the SPEC tree for this to work
if exist bin\runspec goto SPEC_get_dir
    echo "You are not in the top level of the SPEC directory!"
    goto end
:SPEC_get_dir

rem go ahead and fetch the path, thanks to JH.  
rem Is there any easier way to get this info?
if exist %temp%\set_spec_loc.bat del %temp%\set_spec_loc.bat
echo >%temp%\set_spec_loc.bat set SPEC=^^
cd >>%temp%\set_spec_loc.bat
call %temp%\set_spec_loc.bat
del %temp%\set_spec_loc.bat

rem at this point SPEC should point to the correct stuff, so set up the
rem rest of the environment.
rem Is there anyway to detect that we are already in the path?
:SPEC_env_defined
set SPECPERLLIB=%SPEC%\bin;%SPEC%\bin\lib;%SPEC%\bin\lib\site
set PATH=%SPEC%\bin;%PATH%

rem    Finally, let's print all this in a way that makes sense.  
rem    While we're at it, this is a good little test of whether 
rem    specperl is working for you!

specperl bin\printpath.pl

rem this is the end

:end


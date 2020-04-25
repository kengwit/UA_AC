###############################################################################
# SPECtools2000
#   Version 0.3
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file.  Or by using the "runspec" program to 
#   build the Makefile.spec file for them.
###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark

NUMBER         = 253
NAME           = perlbmk
EXEBASE        = perlbmk
BENCHLANG      = C
COMMON_SOURCES = av.c byterun.c deb.c doio.c doop.c dump.c ebcdic.c \
                 fake_dyna.c globals.c gv.c hv.c mg.c op.c perl.c perlio.c \
                 perly.c pp.c pp_ctl.c pp_hot.c pp_sys.c regcomp.c regexec.c \
                 run.c scope.c sv.c taint.c toke.c universal.c util.c MD5.c \
                 md5c.c
NT_SOURCES     = nt_perlmain.c win32.c win32sck.c perllib.c
UNIX_SOURCES   = unix_perlmain.c
#
ifeq ($(OS),Windows_NT)
OS_CFLAGS      = -Iwin32 -I. -DWIN32 -D_CONSOLE
SOURCES        = $(COMMON_SOURCES) $(NT_SOURCES)
OS_LIBS        = advapi32.lib wsock32.lib
else
SOURCES        = $(COMMON_SOURCES) $(UNIX_SOURCES)
endif
NEED_MATH      = yes

# Include standard template for this suite
-include ../../../../Makefile.defaults
-include ../../../Makefile.defaults
-include ../../Makefile.defaults
-include ../Makefile.defaults
-include Makefile.defaults

nomakefiles:
	@echo "*** Whoops!  I didn't find a Makefile.defaults file."
	@echo "*** Your distribution is probably corrupt."
	@echo

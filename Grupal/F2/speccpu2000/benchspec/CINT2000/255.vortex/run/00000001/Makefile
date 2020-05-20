###############################################################################
# SPECtools2000
#   Version 0.3
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file.  Or by using the "runspec" program to 
#   build the Makefile.spec file for them.
###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark

NUMBER    = 255
NAME      = vortex
EXEBASE   = vortex
BENCHLANG = C
SOURCES   = bitvec.c bmt.c bmt0.c bmt01.c bmt10.c bmtlib.c bmtobj.c        \
	    core01.c dba.c dbm0.c dbm1.c dbm2.c domain.c draw07.c draw7.c  \
	    drawlib.c drawobj.c emplib.c empobj.c env0.c env01.c env1.c    \
	    fm.c gdbm.c grp0.c grp1.c grp2.c hm.c iam.c ifm.c im.c km.c    \
	    list01.c mem00.c mem01.c mem10.c oa0.c oa1.c oadmp.c obj01.c   \
	    odbm.c ogrp.c om.c point.c primal.c pstub.c query.c rect.c     \
	    rects.c sa.c shell.c sm.c testobj.c tm.c trans00.c trans01.c   \
	    trans10.c trans20.c tree0.c tree00.c tree01.c ut.c vchunk.c    \
	    vdbm.c voa.c vom.c
NEED_MATH = yes

# Include standard template for this suite
-include ../../../../Makefile.defaults
-include ../../../Makefile.defaults
-include ../../Makefile.defaults
-include ../Makefile.defaults

nomakefiles:
	@echo "*** Whoops!  I didn't find a Makefile.defaults file."
	@echo "*** Your distribution is probably corrupt."
	@echo

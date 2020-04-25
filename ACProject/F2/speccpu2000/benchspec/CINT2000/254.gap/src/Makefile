###############################################################################
# SPECtools2000
#   Version 0.3
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file.  Or by using the "runspec" program to 
#   build the Makefile.spec file for them.
###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark

NUMBER    = 254
NAME      = gap
EXEBASE   = gap
BENCHLANG = C
SOURCES   = agcollec.c aggroup.c blister.c coding.c costab.c cyclotom.c    \
	    eval.c finfield.c function.c gap.c gasman.c idents.c integer.c \
	    list.c pcpresen.c permutat.c plist.c polynom.c range.c         \
	    rational.c read.c record.c scanner.c set.c statemen.c string.c \
	    system.c tietze.c unknown.c vecffe.c vector.c word.c
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

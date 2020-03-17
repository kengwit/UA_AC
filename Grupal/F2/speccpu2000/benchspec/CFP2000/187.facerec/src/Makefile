###############################################################################
# SPECtools2000
#   Version 0.3
#   This makefile should NOT be changed other than to customize for a 
#   particular benchmark.  Users may overide variables by placing them
#   in the Makefile.spec file.  Or by using the "runspec" program to 
#   build the Makefile.spec file for them.
###############################################################################
#	BENCHMARK	-- standard definitions for this benchmark

NUMBER      = 187
NAME        = facerec
EXEBASE     = facerec
BENCHLANG   = F
SOURCES     = FaceRecTypes.f90 \
              parameterRoutines.f90 \
              cfftb.f90 cfftf.f90 cffti.f90 fft2d.f90 \
	      gaborRoutines.f90 imageRoutines.f90 graphRoutines.f90 \
              FaceRec.f90

# Include standard template for this suite
-include ../../../../Makefile.defaults
-include ../../../Makefile.defaults
-include ../../Makefile.defaults
-include ../Makefile.defaults

nomakefiles:
	@echo "*** Whoops!  I didn't find a Makefile.defaults file."
	@echo "*** Your distribution is probably corrupt."
	@echo

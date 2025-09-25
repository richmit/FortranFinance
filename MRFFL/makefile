#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set Global Variables.  
include ../setup.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use FCOMP to set variables for fortran development.  
include $(PROJECT_ROOT)/setup_fortran_gmake/setup_fortran.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# If you need to tweak the fortran variables set above, then this is the place.  Set AR, FC, FFLAGS, & FSHFLG here.

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create rules and targets for the MRFFL library
include $(PROJECT_ROOT)/MRFFL/src/include.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

.PHONY: clean
clean :
	make -C functional_tests clean
	make -C src clean
	make -C unit_tests clean
	rm -f *~
	rm -rf build/* build/.???*

all : 
	make -C functional_tests all
	make -C unit_tests all

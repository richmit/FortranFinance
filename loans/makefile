# -*- Mode:makefile; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      makefile
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-02
# @brief     Build monte loans examples.@EOL
# @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
# @std       GNUMake
# @see       https://github.com/richmit/FortranFinance/
# @copyright
#  @parblock
#  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#
#  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
#
#  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
#  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#  DAMAGE.
#  @endparblock
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set Global Variables.  
include ../setup.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use FCOMP to set variables for fortran development.  
include $(FF_PROJ_ROOT)/setup_fortran_gmake/setup_fortran.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# If you need to tweak the fortran variables set above, then this is the place.  Set AR, FC, FFLAGS, & FSHFLG here.

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create rules and targets for the MRFFL library
include $(FF_PROJ_ROOT)/MRFFL/src/include.mk

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

EXE_TARGETS := loan_level_payments loan_geometric_payments loan_up_down_payments

# Set RUNEM to something to run examples after being built.  I normally set it on the command line like this:
#     make RUNEM=T test_tvm_cashflows
# The value need not be "T" -- any non-whitespace should work.
RUNEM :=

loan_level_payments : loan_level_payments.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

loan_geometric_payments : loan_geometric_payments.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

loan_up_down_payments : loan_up_down_payments.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

.PHONY: clean
clean : clean_mrffl
	rm -f *~ *.tmp.o
	rm -f $(addsuffix $(EXE_SUFFIX), $(EXE_TARGETS)) $(addsuffix $(OBJ_SUFFIX), $(EXE_TARGETS))

.PHONY: all
all : $(EXE_TARGETS)

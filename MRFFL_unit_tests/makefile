# -*- Mode:makefile; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      makefile
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-02
# @brief     Build unit tests.@EOL
# @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
# @std       GNUmake
# @see       https://github.com/richmit/FortranFinance
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

################################################################################################################################################################

MRFFL_PATH = ../MRFFL

include $(MRFFL_PATH)/tools_gfortran.mk
# include $(MRFFL_PATH)/tools_flang.mk
# include $(MRFFL_PATH)/tools_ifx.mk
# include $(MRFFL_PATH)/tools_lfortran.mk
# include $(MRFFL_PATH)/tools_nvfortran.mk

include $(MRFFL_PATH)/include.mk

################################################################################################################################################################

NDIF := ~/world/my_prog/utils/float_diff.rb

EXE_TARGETS := utest_percentages utest_tvm_lump_sum_solve utest_inflation utest_tvm_delayed_geometric_annuity_solve utest_tvm_delayed_lump_sum_solve utest_tvm_cashflows utest_tvm_delayed_arithmetic_annuity_solve utest_taxes utest_tvm12 utest_tvm_delayed_level_annuity_solve utest_life_table utest_life_table_print utest_stats_probit

# Set RUNEM to something to run the binaries after being built.  I normally set it on the command line like this: 
#     make RUNEM=T utest_percentages
# The value need not be "T" -- any non-whitespace should work.
RUNEM := 

# Set TESTEM to something to run tests after being built.  I normally set it on the command line like this: 
#     make TESTEM=T utest_percentages
# The value need not be "T" -- any non-whitespace should work.
TESTEM :=

# Set GOLDEM to something to create golden regression files.  I normally set it on the command line like this: 
#     make GOLDEM=T utest_percentages
# The value need not be "T" -- any non-whitespace should work.
GOLDEM := 

utest_stats_probit : utest_stats_probit.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif


utest_life_table_print : utest_life_table_print.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_life_table : utest_life_table.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_inflation : utest_inflation.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_percentages : utest_percentages.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_tvm_lump_sum_solve : utest_tvm_lump_sum_solve.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_tvm12 : utest_tvm12.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_tvm_delayed_level_annuity_solve : utest_tvm_delayed_level_annuity_solve.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_tvm_delayed_geometric_annuity_solve : utest_tvm_delayed_geometric_annuity_solve.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_tvm_delayed_lump_sum_solve : utest_tvm_delayed_lump_sum_solve.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_tvm_cashflows : utest_tvm_cashflows.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_taxes : utest_taxes.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_markets : utest_markets.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

utest_tvm_delayed_arithmetic_annuity_solve : utest_tvm_delayed_arithmetic_annuity_solve.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@
ifdef RUNEM
	./$@$(EXE_SUFFIX)
endif
ifdef GOLDEM
	./$@$(EXE_SUFFIX) > $@.good
endif
ifdef TESTEM
	./$@$(EXE_SUFFIX) > $@.test
	$(NDIF) -Dba -e 1e-5 $@.good $@.test
endif

################################################################################################################################################################

.PHONY: clean
clean : clean_multi_mrffl
	rm -f *~ *.tmp.o
	rm -f rm -f $(addsuffix .test, $(EXE_TARGETS))
	rm -f $(addsuffix $(EXE_SUFFIX), $(EXE_TARGETS)) 
	rm -f $(addsuffix $(OBJ_SUFFIX), $(EXE_TARGETS))
	rm -f $(addsuffix .exe, $(EXE_TARGETS))
	rm -f $(EXE_TARGETS)

.PHONY: all
all : $(EXE_TARGETS) 

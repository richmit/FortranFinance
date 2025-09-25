# -*- Mode:makefile; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      include.mk
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-02
# @brief     GNU Make include file to help use MRFFL modules.@EOL
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


#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set MRFFL_SRC_DIR based on location of this file.
ifndef MRFFL_SRC_DIR
	MRFFL_SRC_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
endif

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
MRFFL_MODS := mrffl_config mrffl_stats mrffl_bitset mrffl_prt_sets mrffl_var_sets mrffl_percentages mrffl_us_inflation mrffl_solver mrffl_solver_ne mrffl_tvm mrffl_us_taxes mrffl_cashflows mrffl_tvm12 mrffl_us_markets mrffl_life_table

MRFFL_MOD_FILES := $(addsuffix .mod,$(MRFFL_MODS))
MRFFL_OBJ_FILES := $(addsuffix $(OBJ_SUFFIX),$(MRFFL_MODS))

MRFFL_STATIC_LIB_FILE  := libmrffl.a
MRFFL_SHARED_LIB_FILE  := libmrffl$(SLB_SUFFIX)
MRFFL_LIB_FILES        := $(MRFFL_STATIC_LIB_FILE) $(MRFFL_SHARED_LIB_FILE)

mrffl_config$(OBJ_SUFFIX) mrffl_config.mod &: $(MRFFL_SRC_DIR)/mrffl_config.f90
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_stats$(OBJ_SUFFIX) mrffl_stats.mod &: $(MRFFL_SRC_DIR)/mrffl_stats.f90 mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_bitset$(OBJ_SUFFIX) mrffl_bitset.mod &: $(MRFFL_SRC_DIR)/mrffl_bitset.f90 mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_prt_sets$(OBJ_SUFFIX) mrffl_prt_sets.mod &: $(MRFFL_SRC_DIR)/mrffl_prt_sets.f90 mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_var_sets$(OBJ_SUFFIX) mrffl_var_sets.mod &: $(MRFFL_SRC_DIR)/mrffl_var_sets.f90 mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_solver$(OBJ_SUFFIX) mrffl_solver.mod &: $(MRFFL_SRC_DIR)/mrffl_solver.f90 mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_solver_ne$(OBJ_SUFFIX) mrffl_solver_ne.mod &: $(MRFFL_SRC_DIR)/mrffl_solver_ne.f90 mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_tvm12$(OBJ_SUFFIX) mrffl_tvm12.mod &: $(MRFFL_SRC_DIR)/mrffl_tvm12.f90 mrffl_percentages.mod mrffl_solver.mod mrffl_tvm.mod mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_percentages$(OBJ_SUFFIX) mrffl_percentages.mod &: $(MRFFL_SRC_DIR)/mrffl_percentages.f90 mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_us_markets$(OBJ_SUFFIX) mrffl_us_markets.mod &: $(MRFFL_SRC_DIR)/mrffl_us_markets.f90 mrffl_stats.mod mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_life_table$(OBJ_SUFFIX) mrffl_life_table.mod &: $(MRFFL_SRC_DIR)/mrffl_life_table.f90 mrffl_bitset.mod mrffl_stats.mod mrffl_prt_sets.mod mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_us_inflation$(OBJ_SUFFIX) mrffl_us_inflation.mod &: $(MRFFL_SRC_DIR)/mrffl_us_inflation.f90 mrffl_percentages.mod mrffl_stats.mod mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_tvm$(OBJ_SUFFIX) mrffl_tvm.mod &: $(MRFFL_SRC_DIR)/mrffl_tvm.f90 mrffl_solver.mod mrffl_percentages.mod mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_cashflows$(OBJ_SUFFIX) mrffl_cashflows.mod &: $(MRFFL_SRC_DIR)/mrffl_cashflows.f90 mrffl_tvm.mod  mrffl_config.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

mrffl_us_taxes$(OBJ_SUFFIX) mrffl_us_taxes.mod &: $(MRFFL_SRC_DIR)/mrffl_us_taxes.f90 mrffl_config.mod mrffl_tvm.mod
	$(FC) $(FFLAGS) -c $< -o $(basename $@)$(OBJ_SUFFIX)

$(MRFFL_STATIC_LIB_FILE) : $(MRFFL_MOD_FILES) $(MRFFL_OBJ_FILES)
	$(AR) rcs $(MRFFL_STATIC_LIB_FILE) $(MRFFL_OBJ_FILES)

$(MRFFL_SHARED_LIB_FILE) : $(MRFFL_MOD_FILES) $(MRFFL_OBJ_FILES)
	$(FC) $(FSHFLG)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
.PHONY: all_mrffl_lib
all_mrffl_lib : $(MRFFL_LIB_FILES)

.PHONY: all_mrffl_mod
all_mrffl_mod : $(MRFFL_MOD_FILES)

.PHONY: all_mrffl_obj
all_mrffl_obj : $(MRFFL_OBJ_FILES)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
.PHONY: clean_mrffl_mod
clean_mrffl_mod :
	rm -f $(MRFFL_MOD_FILES)

.PHONY: clean_mrffl_obj
clean_mrffl_obj :
	rm -f $(MRFFL_OBJ_FILES)

.PHONY: clean_mrffl_lib
clean_mrffl_lib :
	rm -f $(MRFFL_LIB_FILES)

.PHONY: clean_mrffl
clean_mrffl : clean_mrffl_obj clean_mrffl_mod clean_mrffl_lib

.PHONY: clean_multi_mrffl
clean_multi_mrffl :
	rm -f $(addsuffix .mod,$(MRFFL_MODS))
	rm -f $(addsuffix .obj,$(MRFFL_MODS))
	rm -f $(addsuffix .o,$(MRFFL_MODS))
	rm -f libmrffl.so
	rm -f libmrffl.dll
	rm -f libmrffl.a

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

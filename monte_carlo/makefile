# -*- Mode:makefile; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      makefile
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-02
# @version   VERSION
# @brief     Build monte carlo examples.@EOL
# @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
# @std       R
# @see       https://github.com/richmit/FortranFinance/monte_carlo
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

EXE_TARGETS := inflation stocks
TXT_TARGETS := inflation.txt stocks.txt 
PNG_TARGETS := inflation.png stocks_paths.png stocks_ranges.png

inflation.png : inflation.R inflation.txt
	R -q -f inflation.R

inflation.txt : inflation
	rm -f inflation.txt
	./inflation$(EXE_SUFFIX) > inflation.txt

inflation : inflation.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@

stocks_ranges.png stocks_paths.png &: stocks.R stocks.txt
	R -q -f stocks.R

stocks.txt : stocks
	rm -f stocks.txt
	./stocks$(EXE_SUFFIX) > stocks.txt

stocks : stocks.f90 $(MRFFL_OBJ_FILES)
	$(FC) $(FFLAGS) $^ -o $@

################################################################################################################################################################

.PHONY: clean
clean : clean_mrffl 
	rm -f *~ $(addsuffix $(EXE_SUFFIX), $(EXE_TARGETS)) $(addsuffix $(OBJ_SUFFIX), $(EXE_TARGETS)) $(TXT_TARGETS) $(PNG_TARGETS)

.PHONY: all
all : $(PNG_TARGETS)


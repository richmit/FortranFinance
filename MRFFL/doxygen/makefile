# -*- Mode:makefile; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      makefile
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-19
# @brief     Generate MRFFL documentation.@EOL
# @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
# @std       GNUMake
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
LOC_DIR=autodocs/html
PUB_DIR=~/MJR/WWW/site/SS/FortranFinance/MRFFL_API_Docs

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
$(LOC_DIR)/index.html: ../src/*.f90
	doxygen

.PHONY: docs
docs: $(LOC_DIR)/index.html

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
$(PUB_DIR)/index.html: $(LOC_DIR)/index.html 
	rsync -rlt --log-format=%f --stats --delete --delete-excluded --modify-window=2 $(LOC_DIR)/ $(PUB_DIR)

.PHONY: pub
pub: $(PUB_DIR)/index.html

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
.PHONY: all
all: pub

.PHONY: read
read: $(LOC_DIR)/index.html
	edge $(LOC_DIR)/index.html

.PHONY: clean
clean: 
	rm -f *~

.PHONY: extra_clean
extra_clean: clean
	rm -fr autodocs

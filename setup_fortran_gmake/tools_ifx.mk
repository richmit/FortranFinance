# -*- Mode:makefile; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      tools_ifx.mk
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-02
# @brief     Include file for Intel Fortran ifx compiler.@EOL
# @keywords
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
ifeq ($(OS),Windows_NT)
	FC := ifx
    FFLAGS := -stand:f$(subst 20,,$(FCOMP_STD)) $(if $(FCOMP_OPT_XWARN),-check:all) $(if $(FCOMP_OPT_WARN),-warn:all) $(if $(FCOMP_OPT_OPT),-fp:precise       -O3) -Qdiag-disable:7712 -Qdiag-disable:5268 $(if $(FCOMP_OPT_OMP),-Qopenmp)
	FSHFLG = -dll -Fe:$(MRKISS_SHARED_LIB_FILE) $(MRKISS_OBJ_FILES)
else
	FC := ifx
	FFLAGS := -stand f$(subst 20,,$(FCOMP_STD)) $(if $(FCOMP_OPT_XWARN),-check all) $(if $(FCOMP_OPT_WARN),-warn all) $(if $(FCOMP_OPT_OPT),-fp-model=precise -O3) -diag-disable=7712  -diag-disable=5268  $(if $(FCOMP_OPT_OMP),-qopenmp) $(if $(FCOMP_OPT_XSTACK),-Xlinker -z -Xlinker execstack) -fPIC
	FSHFLG = -fPIC -shared  -Fe:$(MRKISS_SHARED_LIB_FILE) $(MRKISS_OBJ_FILES)
endif



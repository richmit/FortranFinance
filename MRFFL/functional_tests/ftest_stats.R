#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      ftest_life_table_print.R
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-11
# @brief     Analyze data from ftest_life_table_print.f90.@EOL
# @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
# @std       GNU-R
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
# @todo      @EOL@EOL
# @warning   @EOL@EOL
# @bug       @EOL@EOL
# @filedetails
#
#  File details go here.  Multiple paragraphs are fine....
#
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
daDat <- fread('ftest_stats_rand_norm_std.txt')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot() + 
  geom_line(data=data.table(x=seq(min(daDat$z_box), max(daDat$z_box), length=1000)) %>% mutate(y=dnorm(x, mean=0, sd=1)), aes(x=x, y=y), col='black', linewidth=15) +
  geom_density(data=daDat, aes(x=z_box), col='red', fill=NA, alpha=0.5, linewidth=1) + 
  geom_density(data=daDat, aes(x=z_probit), col='green', fill=NA, alpha=0.5, linewidth=1) + 
  xlim(c(-3,3)) +
  ylim(c(0,0.4))
print(gp)

#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      ftest_random_walk.R
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-20
# @brief     Check the results from ftest_random_walk.f90.@EOL
# @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
# @std       F2023
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
mu <- 0.04
sigma <- 0.18

daDat <- fread('ftest_random_walk.txt')

fiDat <- daDat %>% filter(step == max(step))

pdfDat <- 
  data.table(x=seq(min(daDat$val1, daDat$val2), max(daDat$val1, daDat$val2), length=500)) %>% 
  mutate(d =dnorm( x, mean=(mu+1.0 - 0.5 * sigma^2), sd=sigma), 
         dl=dlnorm(x, mean=(mu - 0.5 * sigma^2),     sd=sigma))

gp <- ggplot() + 
  geom_line(data=pdfDat, aes(x=x, y=d),  col='salmon',  linewidth=20, alpha=0.3) +
  geom_line(data=pdfDat, aes(x=x, y=dl), col='darkred', linewidth=20, alpha=0.3) +
  geom_density(data=fiDat, aes(val1), col='green', fill=NA, linewidth=3) +
  geom_density(data=fiDat, aes(val2), col='blue',  fill=NA, linewidth=3)
print(gp)

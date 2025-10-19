#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      blend_risk_cvuc.R
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-19
# @brief     Difference between correlated and uncorrelated MC.@EOL
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
# @filedetails
#
#  I ran blend_risk twice -- once correlated and once uncorrelated.  I put the results in blend_risk_cor.txt & blend_risk_uncor.txt.  Then I stuck them
#  together like this:
#
#    echo '     trial        hp             balance  Method' > blend_risk.txt
#
#    grep -v trial blend_risk_cor.txt | sed 's/$/   Correlated/'   >> blend_risk.txt
#
#    grep -v trial blend_risk_uncor.txt | sed 's/$/  Uncorrelated/' >> blend_risk.txt
#
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load required packages

suppressPackageStartupMessages(library('dplyr'))
suppressPackageStartupMessages(library('ggplot2'))
suppressPackageStartupMessages(library('scales'))
suppressPackageStartupMessages(library('data.table'))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set this to your favorite image viewer, or TRUE to attempt to find one automatically, or FALSE to not load images
imageV <- TRUE

# Try and find an image viewer
if (imageV == TRUE) {
  if (.Platform$OS == "windows") {
    imageV <- "explorer"
  } else {
    for(piv in c("/usr/bin/display", "/usr/bin/eog", "/usr/bin/pqiv", "/usr/bin/nomacs"))
      if(file.exists(piv))
        imageV <- piv
  }
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
a<-fread('blend_risk.txt')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
successDat <- a %>%
  group_by(hp, Method) %>%
  summarize(success=100-100*sum(balance<=0)/length(balance), .groups='drop') %>%
  mutate(hp=as.integer(hp))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(data=successDat) +
  geom_line(aes(x=hp, y=success, col=Method), linewidth=1) +
  labs(title='Correlated vs Uncorrelated Monte Carlo',
       subtitle='Chances of making it 50 years on 4M withdrawing 100K annually adjusted by inflation on a mix of bonds and S&P',
       x='Percentage of portfolio in S&P vs. 10 Year US Treasury Bonds',
       y='Chance Of Success')
fname <- "blend_risk_cvuc.png"
ggsave(fname, width=12, height=10)
if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

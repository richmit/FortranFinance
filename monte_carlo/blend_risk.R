#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      blend_risk.R
# @author    Mitch Richling http://www.mitchr.me/
# @date      2024-12-19
# @brief     Draw probability of success vs blend percentage curve from blend_risk.f90 output.@EOL
# @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
# @std       R
# @see       https://github.com/richmit/FortranFinance/monte_carlo
# @copyright 
#  @parblock
#  Copyright (c) 2024, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
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
# Set this to your favorite image viewer, or TRUE to attempt to find one automatically, or FALSE to not load images
imageV <- TRUE

# Try and find an image viewer
if (imageV == TRUE) {
  if (.Platform$OS == "windows") {
    imageV <- "explorer"
  } else {
    for(piv in c("/usr/bin/display", "/usr/bin/pqiv", "/usr/bin/nomacs"))
      if(file.exists(piv))
        imageV <- piv
  }
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
daDat <- fread("blend_risk.txt") %>% mutate(hp=factor(hp))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
## gp <- ggplot(data=daDat %>% filter(hp==75 & balance < 70e6)) +
##   geom_histogram(aes(x=balance), col='red', fill='pink', breaks=seq(0, 40e6, by=5e6)) +
##   scale_x_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale())) + 
##   labs(title='Ending balance of making it 50 years on 4M withdrawing 100K annually adjusted by inflation', x='Balance', y='')
## print(gp)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
successDat <- daDat %>% group_by(hp) %>% summarize(success=100-100*sum(balance<=0)/length(balance), .group='drop') %>% mutate(hp=as.integer(hp))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(data=successDat) +
  geom_smooth(aes(x=hp, y=success), method="loess", formula='y~x', span=0.2, level=.9999, se=TRUE, linewidth=2) +
  geom_text(data=successDat %>% filter(success == max(success)), aes(x=hp, y=success, label=paste('Best Chance at ', hp, '% S&P', sep='')), vjust='bottom', hjust='centered', size=6) +
  labs(title='Chances of making it 50 years on 4M withdrawing 100K annually adjusted by inflation on a mix of bonds and S&P', x='Percentage of portfolio in S&P vs. 10 Year US Treasury Bonds', y='Chance Of Success')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
fname <- "blend_risk.png"
ggsave(fname, width=12, height=10)
if (is.character(imageV)) system(paste(imageV, fname, sep=' '))


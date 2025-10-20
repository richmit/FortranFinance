#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      stocks.R
# @author    Mitch Richling http://www.mitchr.me/
# @date      2024-12-19
# @brief     Draw nice time series graphs with the output from stocks.f90.@EOL
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
source('../setup.R')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
daDat <- fread("stocks.txt") %>% mutate(trial=factor(trial))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(data=daDat) +
  geom_line(aes(x=year, y=value+1, group=trial), alpha=0.03, linewidth=2, col='black', show.legend=FALSE) +
  scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') +
  labs(title='Value of $100 after 20 years', x='Year', y='Value')

fname <- "stocks_paths.png"
ggsave(fname, width=12, height=10)
if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
bands <- c(90, 80, 50)

sumDat <- group_by(daDat, year) %>%
  summarize(band_90_0 = pmax(0, quantile(value,  (50-bands[1]/2)/100)),
            band_90_1 = pmax(0, quantile(value,  (50+bands[1]/2)/100)),
            band_80_0 = pmax(0, quantile(value,  (50-bands[2]/2)/100)),
            band_80_1 = pmax(0, quantile(value,  (50+bands[2]/2)/100)),
            band_50_0 = pmax(0, quantile(value,  (50-bands[3]/2)/100)),
            band_50_1 = pmax(0, quantile(value,  (50+bands[3]/2)/100)),
            .groups='keep')

gp <- ggplot(sumDat) +
  geom_ribbon(aes(x=year, ymin=band_90_0, ymax=band_90_1), fill='darkred',   alpha=0.7) +
  geom_ribbon(aes(x=year, ymin=band_80_0, ymax=band_80_1), fill='goldenrod', alpha=0.7) +
  geom_ribbon(aes(x=year, ymin=band_50_0, ymax=band_50_1), fill='darkgreen', alpha=0.7) +
  annotate("text", x=17, y=mean(c(sumDat$band_90_0[17], sumDat$band_80_0[17])), label="90%", col='white', size=10, vjust=0.5) +
  annotate("text", x=17, y=mean(c(sumDat$band_80_0[17], sumDat$band_50_0[17])), label="80%", col='white', size=10, vjust=0.5) +
  annotate("text", x=17, y=mean(c(sumDat$band_50_0[17], sumDat$band_50_1[17])), label="50%", col='white', size=10, vjust=0.5) +
  scale_y_continuous(labels = scales::label_dollar(scale_cut = cut_short_scale()), trans='log10') +
  labs(title='Value Balance Ranges') +
  ylab('Value') +
  xlab('Year')
fname <- 'stocks_ranges.png'
ggsave(fname, width=12, height=10, dpi=100, units='in', plot=gp);
if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

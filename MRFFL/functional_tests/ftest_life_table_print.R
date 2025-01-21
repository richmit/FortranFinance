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
cdc_lx    <- fread('ftest_life_table_print_cdc_lx.txt')
cdc_qx    <- fread('ftest_life_table_print_cdc_qx.txt')
cdc_wf_lx <- fread('ftest_life_table_print_cdc_wf_lx.txt')
cdc_wm_lx <- fread('ftest_life_table_print_cdc_wm_lx.txt')
cdc_w_lx  <- fread('ftest_life_table_print_cdc_w_lx.txt')
usss_f_lx <- fread('ftest_life_table_print_usss_f_lx.txt')
usss_f_qx <- fread('ftest_life_table_print_usss_f_qx.txt')
usss_m_lx <- fread('ftest_life_table_print_usss_m_lx.txt')
usss_m_qx <- fread('ftest_life_table_print_usss_m_qx.txt')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
cdc_qxlx_lx <- fread('ftest_life_table_print_cdc_qxlx_dat.txt')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
cdc_src    <- fread('../../../data/life/cdc_us_population_2021.csv')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# qx vs px for usss_f
ggplot() + 
  geom_line(data=usss_f_lx, aes(x=age, y=qx, col="lx"), linewidth=4) + 
  geom_line(data=usss_f_qx, aes(x=age, y=qx, col="qx"), linewidth=2) +
  scale_colour_manual(values = c("lx"="black", "qx"="red"))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# qx vs px for usss_m
ggplot() + 
  geom_line(data=usss_m_lx, aes(x=age, y=qx, col="lx"), linewidth=4) + 
  geom_line(data=usss_m_qx, aes(x=age, y=qx, col="qx"), linewidth=2) +
  scale_colour_manual(values = c("lx"="black", "qx"="red"))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# cdc white people vs all people 
ggplot() + 
  geom_line(data=cdc_lx, aes(x=age, y=qx, col="all people"), linewidth=4) + 
  geom_line(data=cdc_w_lx,  aes(x=age, y=qx, col="white people"), linewidth=2) +
  scale_colour_manual(values = c("white people"="black", "all people"="red"))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# cdc white people vs white females, vs white males
ggplot() + 
  geom_line(data=cdc_w_lx,  aes(x=age, y=qx, col="white people"), linewidth=2) +
  geom_line(data=cdc_wf_lx, aes(x=age, y=qx, col="white females"),   linewidth=2) +
  geom_line(data=cdc_wm_lx, aes(x=age, y=qx, col="white males"), linewidth=2) +
  scale_colour_manual(values = c("white people"="black", "white females"="red", "white males"="blue"))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
perr <- function(tru,est) 100*(abs(tru)-abs(est))/abs(tru)
pdif <- function(v1,v2) 100*(abs(v1)-abs(v2))/pmin(abs(v1), abs(v2))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
inner_join(cdc_lx %>% select(-mx, -px), cdc_src, by='age', suffix=c('_src', '_lx')) %>% 
  mutate(qxErr=perr(qx_src,qx_lx), lxErr=perr(lx_src,lx_lx), dxErr=perr(dx_src,dx_lx), LxErr=perr(Lx_src,Lx_lx), TxErr=perr(Tx_src,Tx_lx), exErr=perr(ex_src,ex_lx)) %>%
  mutate(maxERR=pmax(abs(qxErr), abs(lxErr), abs(dxErr), abs(LxErr), abs(TxErr), abs(exErr))) %>% 
  filter(maxERR > 1)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
inner_join(cdc_qx %>% select(-mx, -px), cdc_src, by='age', suffix=c('_src', '_qx')) %>% 
  mutate(qxErr=perr(qx_src,qx_qx), lxErr=perr(lx_src,lx_qx), dxErr=perr(dx_src,dx_qx), LxErr=perr(Lx_src,Lx_qx), TxErr=perr(Tx_src,Tx_qx), exErr=perr(ex_src,ex_qx)) %>%
  mutate(maxERR=pmax(abs(qxErr), abs(lxErr), abs(dxErr), abs(LxErr), abs(TxErr), abs(exErr))) %>% 
  filter(maxERR > 1)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# These diffrences should be very tiny...
lx_vs_qxlx <- 
  inner_join(cdc_lx, cdc_qxlx_lx, by='age', suffix=c('_lx', '_qx')) %>% 
  mutate(qxDif=pdif(qx_lx,qx_qx), lxDif=pdif(lx_lx,lx_qx), dxDif=pdif(dx_lx,dx_qx), LxDif=pdif(Lx_lx,Lx_qx), TxDif=pdif(Tx_lx,Tx_qx), exDif=pdif(ex_lx,ex_qx)) %>%
  mutate(maxDIF=pmax(abs(qxDif), abs(lxDif), abs(dxDif), abs(LxDif), abs(TxDif), abs(exDif)))
head(lx_vs_qxlx)
lx_vs_qxlx %>% filter(maxDIF > quantile(lx_vs_qxlx$maxDIF, .9))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# This is a PDF of expected death for someone at age 0
ggplot() + 
  geom_line(data=usss_m_lx, aes(x=age, y=dx/100000), stat='identity', col='blue') + 
  geom_line(data=usss_f_lx, aes(x=age, y=dx/100000), stat='identity', col='red')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# This is a PDF of expected death for someone at age 'cage'
cage <- 50
ggplot() + 
  geom_line(data=usss_m_lx %>% filter(age>=cage), aes(x=age, y=dx/max(lx)), stat='identity', col='blue') + 
  geom_line(data=usss_f_lx %>% filter(age>=cage), aes(x=age, y=dx/max(lx)), stat='identity', col='red')

#!/usr/bin/env -S Rscript
# -*- Mode:ess-r; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      inflation.R
# @author    Mitch Richling http://www.mitchr.me/
# @date      2024-12-19
# @brief     Draw a nice histogram with the output from inflation.f90.@EOL
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
daDat <- fread("inflation.txt")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
gp <- ggplot(data=daDat) + 
  geom_histogram(aes(x=value), bins=30, col='black', fill='pink') +
  labs(title='Value of $100 after 20 years', x='Value', y='Count')

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
fname <- "inflation.png"
ggsave(fname, width=12, height=10)
if (is.character(imageV)) system(paste(imageV, fname, sep=' '))

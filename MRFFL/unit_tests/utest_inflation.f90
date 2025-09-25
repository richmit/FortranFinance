! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      utest_inflation.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: mrffl_us_inflation.@EOL
!! @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
!! @std       F2023
!! @see       https://github.com/richmit/FortranFinance
!! @copyright
!!  @parblock
!!  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
!!
!!  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
!!  conditions are met:
!!
!!  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following
!!     disclaimer.
!!
!!  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following
!!     disclaimer in the documentation and/or other materials provided with the distribution.
!!
!!  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products
!!     derived from this software without specific prior written permission.
!!
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
!!  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
!!  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
!!  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!!  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
!!  OF THE POSSIBILITY OF SUCH DAMAGE.
!!  @endparblock
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program utest_inflation
  use :: mrffl_config,       only: rk
  use :: mrffl_us_inflation, only: inf_adj, inf_aggregate, inf_dat

  implicit none (type, external)

  print "(a50,f20.5,f20.5)", "inf_adj(1950, 1920, 1.0_rk)",        inf_adj(1950, 1920, 1.0_rk),   0.82908487540483555_rk

  print "(a50,f20.5,f20.5)", "inf_adj(1920, 1950, 1.0_rk)",        inf_adj(1920, 1950, 1.0_rk),   1.20614912859399090_rk

  print "(a50,f20.5,f20.5)", "inf_adj(1950, 1950, 1.0_rk)",        inf_adj(1950, 1950, 1.0_rk),   1.00000000000000000_rk

  print "(a50,f20.5,f20.5)", "inf_aggregate(1950, 1920, 1.0_rk)",  inf_aggregate(1950, 1920),   -17.09151245951644100_rk

  print "(a50,f20.5,f20.5)", "inf_aggregate(1920, 1950, 1.0_rk)",  inf_aggregate(1920, 1950),    20.61491285939909100_rk

  print "(a50,f20.5,f20.5)", "inf_aggregate(1950, 1950, 1.0_rk)",  inf_aggregate(1950, 1950),     0.00000000000000000_rk

  print "(a50,f20.5,f20.5)", "inf_dat(1921)",                         inf_dat(1921),                   -10.5000000000000000_rk

  print "(a50,f20.5,f20.5)", "inf_dat(1914)",                         inf_dat(1914),                     1.0000000000000000_rk

  print "(a50,f20.5,f20.5)", "inf_dat(2023)",                         inf_dat(2023),                     4.1000000000000000_rk

end program utest_inflation

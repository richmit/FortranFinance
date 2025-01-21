! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      blend_risk.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     Predict the value of $100 after 20 years using monte carlo simulation.@EOL
!! @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
!! @std       F2023
!! @see       https://github.com/richmit/FortranFinance
!! @copyright
!!  @parblock
!!  Copyright (c) 2024, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
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
!! @filedetails
!!
!!  Scenario:
!!
!!  We start with 4M in the bank.  Over the next 50 years we wish to withdrawal 100K at the end of each year -- and grow that
!!  value over time with inflation.  We invest the money in a blend of S&P 500 and US 10 year treasury bonds.
!!
!!  Approach:
!!
!!  For each possible integer percentage mix of S&P & bonds (i.e. percentages of S&P that range from 0% up to 100%) we run 100000
!!  (`trials`) simulations.  The simulations use historical data.  The technique is called "coupled resampling" where we pick a
!!  random year, and use the measured values for that year for all three variables (S&P 500 return, 10 year US Treasury bond
!!  return, and US inflation).  This technique attempts to capture the inherent correlation between the variables; however, when
!!  used with low resolution data it can create bias in the results.  Note we can switch to uncoupled via the variable
!!  `coupled_mc`.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program blend_risk
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_us_markets, only: snp_dat, dgs10_dat
  use mrffl_stats, only: rand_int
  use mrffl_us_inflation, only: inf_dat
  use mrffl_percentages, only: p_add=>add_percentage, p_of=>percentage_of

  integer,          parameter :: years              = 50       ! Number of years to project out our stocks adjusted value
  integer,          parameter :: trials             = 100000   ! Number of trials to run
  real(kind=rk),    parameter :: initial_balance    = 4000000  ! This is the balance we will stocks adjusted over the years
  real(kind=rk),    parameter :: initial_withdrawal = 100000   ! Annual withdrawal
  logical,          parameter :: coupled_mc         = .TRUE.   ! Use coupled resampling

  real(kind=rk)               :: balance, withdrawal, c_snp, c_dgs, c_inf
  integer                     :: year, trial, hp
  integer(kind=ik)            :: rand_year

  ! Run monte carlo simulations and dump the results to STDOUT
  print '(a10,a10,a20)', "trial", "hp", "balance"
  do hp=0,100
     do trial=1,trials
        balance = initial_balance
        withdrawal = initial_withdrawal
        do year=1,years
           if (coupled_mc) then
              rand_year = rand_int(2023, 2000)
              c_snp = snp_dat(rand_year)
              c_dgs = dgs10_dat(rand_year)
              c_inf = inf_dat(rand_year)
           else
              c_snp = snp_dat(rand_int(2023, 2000))
              c_dgs = dgs10_dat(rand_int(2023, 2000))
              c_inf = inf_dat(rand_int(2023, 2000))
           end if
           balance = p_add(p_of(balance, real(hp, rk)), c_snp) + p_add(p_of(balance, real(100-hp, rk)), c_dgs)
           balance = balance - min(withdrawal, balance)
           withdrawal = p_add(withdrawal, max(0.0_rk, c_inf))
        end do
        print '(i10,i10,f20.5)', trial, hp, balance
     end do
  end do

end program blend_risk

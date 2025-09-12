! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      stocks.f90
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
!!  This program runs 2000 (`trials`) stocks simulations on $100 (`initial_value`).  Each simulation is over 20 (`years`) years
!!  using the last 30 (`mc_history_years`) years of historical US stocks data.  This program prints the resulting value of each
!!  simulation to STDOUT.  If placed in a file, this data may be consumed by `stocks.R` to produce a nice histogram showing the
!!  probability of the value after 100 years.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program stocks
  use mrffl_config,      only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_us_markets,  only: rut_resample
  use mrffl_percentages, only: add_percentage

  implicit none (type, external)

  integer,          parameter :: years            = 20      ! Number of years to project out our stocks adjusted value
  integer(kind=ik), parameter :: mc_history_years = 30      ! Number of years of US stocks data for our random stocks values
  integer,          parameter :: trials           = 2000    ! Number of trials to run
  real(kind=rk),    parameter :: initial_value    = 100     ! This is the value we will stocks adjusted over the years

  real(kind=rk)               :: value
  real(kind=rk)               :: i
  integer                     :: year, trial

  ! Run monte carlo simulations and dump the results to STDOUT
  print *, "trial year value"
  do trial=1,trials
     value = initial_value
     print *, trial, 1, value
     do year=2,years
        i =  rut_resample(mc_history_years)
        value = add_percentage(value, i)
        print *, trial, year, value
     end do
  end do

end program stocks

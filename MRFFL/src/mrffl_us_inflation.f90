! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_us_inflation.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     US inflation data and monte carlo.@EOL
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> US inflation data and monte carlo.
!!
module mrffl_us_inflation
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_stats, only: resample_tail
  use mrffl_percentages, only: percentage_of, percentage_change, add_percentage
  implicit none (type, external)
  private
  real(kind=rk), parameter, public :: inf_dat(1914:2023)  = [ &
       &                                       1.0_rk,  1.0_rk,  7.9_rk, 17.4_rk, 18.0_rk, 14.6_rk, & ! 1914-1919
       & 15.6_rk, -10.5_rk, -6.1_rk,  1.8_rk,  0.0_rk,  2.3_rk,  1.1_rk, -1.7_rk, -1.7_rk,  0.0_rk, & ! 1920-1929
       & -2.3_rk,  -9.0_rk, -9.9_rk, -5.1_rk,  3.1_rk,  2.2_rk,  1.5_rk,  3.6_rk, -2.1_rk, -1.4_rk, & ! 1930-1939
       &  0.7_rk,   5.0_rk, 10.9_rk,  6.1_rk,  1.7_rk,  2.3_rk,  8.3_rk, 14.4_rk,  8.1_rk, -1.2_rk, & ! 1940-1949
       &  1.3_rk,   7.9_rk,  1.9_rk,  0.8_rk,  0.7_rk, -0.4_rk,  1.5_rk,  3.3_rk,  2.8_rk,  0.7_rk, & ! 1950-1959
       &  1.7_rk,   1.0_rk,  1.0_rk,  1.3_rk,  1.3_rk,  1.6_rk,  2.9_rk,  3.1_rk,  4.2_rk,  5.5_rk, & ! 1960-1969
       &  5.7_rk,   4.4_rk,  3.2_rk,  6.2_rk, 11.0_rk,  9.1_rk,  5.8_rk,  6.5_rk,  7.6_rk, 11.3_rk, & ! 1970-1979
       & 13.5_rk,  10.3_rk,  6.2_rk,  3.2_rk,  4.3_rk,  3.6_rk,  1.9_rk,  3.6_rk,  4.1_rk,  4.8_rk, & ! 1980-1989
       &  5.4_rk,   4.2_rk,  3.0_rk,  3.0_rk,  2.6_rk,  2.8_rk,  3.0_rk,  2.3_rk,  1.6_rk,  2.2_rk, & ! 1990-1999
       &  3.4_rk,   2.8_rk,  1.6_rk,  2.3_rk,  2.7_rk,  3.4_rk,  3.2_rk,  2.8_rk,  3.8_rk, -0.4_rk, & ! 2000-2009
       &  1.6_rk,   3.2_rk,  2.1_rk,  1.5_rk,  1.6_rk,  0.1_rk,  1.3_rk,  2.1_rk,  2.4_rk,  1.8_rk, & ! 2010-2019
       &  1.2_rk,   4.7_rk,  8.0_rk,  4.1_rk]                                                         ! 2020-2023

  public  :: inf_resample, inf_aggregate, inf_adj

contains

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Aggregate inflation rate between two years.
  !! Out of range years cause an ERROR STOP.
  !!
  real(kind=rk) pure function inf_aggregate(from_year, to_year)
    implicit none (type, external)
    integer(kind=ik), intent(in) :: from_year, to_year
    integer(kind=ik)             :: year
    if (from_year < lbound(inf_dat, 1)) error stop "ERROR(inf_aggregate): from_year too small"
    if (from_year > ubound(inf_dat, 1)) error stop "ERROR(inf_aggregate): from_year too large"
    if (to_year   < lbound(inf_dat, 1)) error stop "ERROR(inf_aggregate): to_year too small"
    if (to_year   > ubound(inf_dat, 1)) error stop "ERROR(inf_aggregate): to_year too large"
    inf_aggregate = 1.0
    do year=(min(from_year, to_year)+1_ik),max(from_year, to_year)
       inf_aggregate = inf_aggregate + percentage_of(inf_aggregate, inf_dat(year))
    end do
    if (from_year > to_year) then
       inf_aggregate = percentage_change(inf_aggregate, 1.0_rk)
    else
       inf_aggregate = percentage_change(1.0_rk, inf_aggregate)
    end if
  end function inf_aggregate

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Adjust value for inflation from_year to to_year using US inflation data.
  !! Out of range years cause an ERROR STOP.
  !!
  real(kind=rk) pure function inf_adj(from_year, to_year, v)
    implicit none (type, external)
    integer(kind=ik), intent(in) :: from_year, to_year
    real(kind=rk),    intent(in) :: v
    inf_adj = add_percentage(v, inf_aggregate(from_year, to_year))
  end function inf_adj

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return a random inflation value from the last history_years of US inflation data.
  !!
  real(kind=rk) function inf_resample(history_years)
    implicit none (type, external)
    integer(kind=ik), intent(in) :: history_years
    inf_resample = resample_tail(inf_dat, history_years)
  end function inf_resample

end module mrffl_us_inflation

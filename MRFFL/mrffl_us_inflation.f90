! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_us_inflation.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     US inflation data and monte carlo.@EOL
!! @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
!! @std       F2023
!! @see       https://github.com/richmit/FortranFinance/MRFFL
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
  implicit none  
  private                  
  real(kind=rk), parameter, public :: inf_dat(1914:2023)  = [ &
                                 1.0,  1.0,  7.9, 17.4, 18.0, 14.6, & ! 1914-1919
       15.6, -10.5, -6.1,  1.8,  0.0,  2.3,  1.1, -1.7, -1.7,  0.0, & ! 1920-1929
       -2.3,  -9.0, -9.9, -5.1,  3.1,  2.2,  1.5,  3.6, -2.1, -1.4, & ! 1930-1939
        0.7,   5.0, 10.9,  6.1,  1.7,  2.3,  8.3, 14.4,  8.1, -1.2, & ! 1940-1949
        1.3,   7.9,  1.9,  0.8,  0.7, -0.4,  1.5,  3.3,  2.8,  0.7, & ! 1950-1959
        1.7,   1.0,  1.0,  1.3,  1.3,  1.6,  2.9,  3.1,  4.2,  5.5, & ! 1960-1969
        5.7,   4.4,  3.2,  6.2, 11.0,  9.1,  5.8,  6.5,  7.6, 11.3, & ! 1970-1979
       13.5,  10.3,  6.2,  3.2,  4.3,  3.6,  1.9,  3.6,  4.1,  4.8, & ! 1980-1989
        5.4,   4.2,  3.0,  3.0,  2.6,  2.8,  3.0,  2.3,  1.6,  2.2, & ! 1990-1999
        3.4,   2.8,  1.6,  2.3,  2.7,  3.4,  3.2,  2.8,  3.8, -0.4, & ! 2000-2009
        1.6,   3.2,  2.1,  1.5,  1.6,  0.1,  1.3,  2.1,  2.4,  1.8, & ! 2010-2019
        1.2,   4.7,  8.0,  4.1]                                       ! 2020-2023

  public  :: inf_resample, inf_aggregate, inf_adj

contains

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Aggregate inflation rate between two years.
  !! Out of range years cause an ERROR STOP.
  !!
  real(kind=rk) pure function inf_aggregate(from_year, to_year)
    implicit none
    integer(kind=ik), intent(in) :: from_year, to_year
    integer(kind=ik)             :: year
    if (from_year < lbound(inf_dat, 1)) error stop "ERROR(inf_aggregate): from_year too small"
    if (from_year > ubound(inf_dat, 1)) error stop "ERROR(inf_aggregate): from_year too large"
    if (to_year   < lbound(inf_dat, 1)) error stop "ERROR(inf_aggregate): to_year too small"
    if (to_year   > ubound(inf_dat, 1)) error stop "ERROR(inf_aggregate): to_year too large"
    inf_aggregate = 1.0
    do year=(min(from_year, to_year)+1),max(from_year, to_year)
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
    implicit none
    integer(kind=ik), intent(in) :: from_year, to_year
    real(kind=rk),    intent(in) :: v
    inf_adj = add_percentage(v, inf_aggregate(from_year, to_year))
  end function inf_adj

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return a random inflation value from the last history_years of US inflation data.
  !!
  real(kind=rk) function inf_resample(history_years)
    implicit none
    integer(kind=ik), intent(in) :: history_years
    inf_resample = resample_tail(inf_dat, history_years)
  end function inf_resample

end module mrffl_us_inflation

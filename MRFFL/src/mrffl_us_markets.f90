! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_us_markets.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     Annual us stock market return data and monte carlo.@EOL
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
!> Annual us stock market return data and monte carlo
!!
module mrffl_us_markets
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_stats, only: resample_tail
  implicit none
  private

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Annual S&P 500 data
  !!
  real(kind=rk), parameter, public :: snp_dat(1970:2023)  = [ &
    10.79,  15.63, -17.37, -29.72,  31.55, 19.15, -11.50,   1.06,  12.31,  25.77,  & ! 1970-1979
    -9.73,  14.76,  17.27,   1.40,  26.33, 14.62,   2.03,  12.40,  27.25,  -6.56,  & ! 1980-1989
    26.31,   4.46,   7.06,  -1.54,  34.11, 20.26,  31.01,  26.67,  19.53, -10.14,  & ! 1990-1999
   -13.04, -23.37,  26.38,   8.99,   3.00, 13.62,   3.53, -38.49,  23.45,  12.78,  & ! 2000-2009
     0.00,  13.41,  29.60,  11.39,  -0.73,  9.54,  19.42,  -6.24,  28.88,  16.26,  & ! 2010-2019
    26.89, -19.44,  24.23,  24.34]                                                   ! 2020-2023

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Annual Russel 2000 data
  !!
  real(kind=rk), parameter, public :: rut_dat(1988:2023)  = [ &
                                                                   22.38,  14.21, & ! 1980-1989
   -21.46,  43.68,  16.36,  17.00,  -3.18, 26.21,  14.76,  20.52,  -3.45,  19.62, & ! 1990-1999
    -4.20,   1.03, -21.58,  45.37,  17.00,  3.32,  17.00,  -2.75, -34.80,  25.22, & ! 2000-2009
    25.31,  -5.45,  14.63,  37.00,   3.53, -5.71,  19.48,  13.14, -12.18,  23.72, & ! 2010-2019
    18.36,  13.70, -21.56,  15.09]                                                  ! 2020-2023

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Nasdaq data
  !!
  real(kind=rk), parameter, public :: nas_dat(1974:2023)  = [ &
                                   -35.11, 29.76,  26.10,   7.33,  12.31,  28.11, & ! 1974-1979
    33.88,  -3.21,  18.67,  19.87, -11.22, 31.35,   7.36,  -5.25,  15.40,  19.24, & ! 1980-1989
   -17.81,  56.85,  15.47,  14.74,  -3.19, 39.91,  22.71,  21.64,  39.62,  85.60, & ! 1990-1999
   -39.29, -21.05, -31.53,  50.01,   8.59,  1.37,   9.52,   9.81, -40.54,  43.89, & ! 2000-2009
    16.91,  -1.80,  15.91,  38.32,  13.40,  5.73,   7.50,  28.24,  -3.88,  35.23, & ! 2010-2019
    43.64,  21.39, -33.10,  43.42]                                                  ! 2020-2023

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Dow Jones data
  !!
  real(kind=rk), parameter, public :: dow_dat(1970:2023)  = [ &
     4.82,   6.11,  14.58, -16.58, -27.57, 38.32,  17.86, -17.27,  -3.15,   4.19, & ! 1974-1979
    14.93,  -9.23,  19.61,  20.27,  -3.74, 27.66,  22.59,   2.26,  11.85,  26.96, & ! 1980-1989
    -4.34,  20.32,   4.17,  13.72,   2.14, 33.45,  26.01,  22.64,  16.35,  24.95, & ! 1990-1999
    -6.17,  -7.11, -16.76,  25.32,   3.15, -0.61,  16.29,   6.43, -33.84,  18.82, & ! 2000-2009
    11.02,   5.53,   7.26,  26.50,   7.52, -2.23,  13.42,  25.08,  -5.63,  22.34, & ! 2010-2019
     7.25,  18.73,  -8.78,  13.70]                                                  ! 2020-2023

  !--------------------------------------------------------------------------------------------------------------------------------
  !> DGS10 US treasury 10 year (mean by year)
  !!
  real(kind=rk), parameter, public :: dgs10_dat(1962:2025)  = [ &
                  3.946, 4.001, 4.187, 4.283, 4.925, 5.072, 5.644, 6.672, & ! 1962-1969
    7.354, 6.160, 6.206, 6.848, 7.562, 7.993, 7.612, 7.417, 8.408, 9.432, & ! 1970-1979
    11.43, 13.92, 13.00, 11.10, 12.45, 10.61, 7.672, 8.392, 8.848, 8.493, & ! 1980-1989
    8.552, 7.862, 7.008, 5.866, 7.085, 6.573, 6.443, 6.353, 5.262, 5.646, & ! 1990-1999
    6.030, 5.020, 4.613, 4.013, 4.271, 4.288, 4.795, 4.634, 3.664, 3.264, & ! 2000-2009
    3.215, 2.781, 1.803, 2.350, 2.539, 2.138, 1.837, 2.329, 2.911, 2.141, & ! 2010-2019
    0.889, 1.446, 2.950, 3.959, 4.207, 4.585]                               ! 2020-2025

  public  :: snp_resample, rut_resample, nas_resample, dow_resample, dgs10_resample

contains

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of S&P Data.
  !!
  real(kind=rk) function snp_resample(history_years)
    implicit none
    integer(kind=ik), intent(in) :: history_years
    snp_resample = resample_tail(snp_dat, history_years)
  end function snp_resample

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of Russel 2000 Data.
  !!
  real(kind=rk) function rut_resample(history_years)
    implicit none
    integer(kind=ik), intent(in) :: history_years
    rut_resample = resample_tail(rut_dat, history_years)
  end function rut_resample

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of Nasdaq Data.
  !!
  real(kind=rk) function nas_resample(history_years)
    implicit none
    integer(kind=ik), intent(in) :: history_years
    nas_resample = resample_tail(nas_dat, history_years)
  end function nas_resample

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of Dow Data.
  !!
  real(kind=rk) function dow_resample(history_years)
    implicit none
    integer(kind=ik), intent(in) :: history_years
    dow_resample = resample_tail(dow_dat, history_years)
  end function dow_resample

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of US treasury 10 year data.
  !!
  real(kind=rk) function dgs10_resample(history_years)
    implicit none
    integer(kind=ik), intent(in) :: history_years
    dgs10_resample = resample_tail(dgs10_dat, history_years)
  end function dgs10_resample

end module mrffl_us_markets

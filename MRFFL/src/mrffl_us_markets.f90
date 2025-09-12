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
  implicit none (type, external)
  private

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Annual S&P 500 data
  !!
  real(kind=rk), parameter, public :: snp_dat(1970:2023)  = [ &
       &  10.79_rk,  15.63_rk, -17.37_rk, -29.72_rk,  31.55_rk, 19.15_rk, -11.50_rk,   1.06_rk,  12.31_rk,  25.77_rk,  & ! 1970-1979
       &  -9.73_rk,  14.76_rk,  17.27_rk,   1.40_rk,  26.33_rk, 14.62_rk,   2.03_rk,  12.40_rk,  27.25_rk,  -6.56_rk,  & ! 1980-1989
       &  26.31_rk,   4.46_rk,   7.06_rk,  -1.54_rk,  34.11_rk, 20.26_rk,  31.01_rk,  26.67_rk,  19.53_rk, -10.14_rk,  & ! 1990-1999
       & -13.04_rk, -23.37_rk,  26.38_rk,   8.99_rk,   3.00_rk, 13.62_rk,   3.53_rk, -38.49_rk,  23.45_rk,  12.78_rk,  & ! 2000-2009
       &   0.00_rk,  13.41_rk,  29.60_rk,  11.39_rk,  -0.73_rk,  9.54_rk,  19.42_rk,  -6.24_rk,  28.88_rk,  16.26_rk,  & ! 2010-2019
       &  26.89_rk, -19.44_rk,  24.23_rk,  24.34_rk]                                                                     ! 2020-2023

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Annual Russel 2000 data
  !!
  real(kind=rk), parameter, public :: rut_dat(1988:2023)  = [ &
       &                                                                                         22.38_rk,  14.21_rk, & ! 1980-1989
       & -21.46_rk,  43.68_rk,  16.36_rk,  17.00_rk,  -3.18_rk, 26.21_rk,  14.76_rk,  20.52_rk,  -3.45_rk,  19.62_rk, & ! 1990-1999
       &  -4.20_rk,   1.03_rk, -21.58_rk,  45.37_rk,  17.00_rk,  3.32_rk,  17.00_rk,  -2.75_rk, -34.80_rk,  25.22_rk, & ! 2000-2009
       &  25.31_rk,  -5.45_rk,  14.63_rk,  37.00_rk,   3.53_rk, -5.71_rk,  19.48_rk,  13.14_rk, -12.18_rk,  23.72_rk, & ! 2010-2019
       &  18.36_rk,  13.70_rk, -21.56_rk,  15.09_rk]                                                                    ! 2020-2023

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Nasdaq data
  !!
  real(kind=rk), parameter, public :: nas_dat(1974:2023)  = [ &
       &                                             -35.11_rk, 29.76_rk,  26.10_rk,   7.33_rk,  12.31_rk,  28.11_rk, & ! 1974-1979
       &  33.88_rk,  -3.21_rk,  18.67_rk,  19.87_rk, -11.22_rk, 31.35_rk,   7.36_rk,  -5.25_rk,  15.40_rk,  19.24_rk, & ! 1980-1989
       & -17.81_rk,  56.85_rk,  15.47_rk,  14.74_rk,  -3.19_rk, 39.91_rk,  22.71_rk,  21.64_rk,  39.62_rk,  85.60_rk, & ! 1990-1999
       & -39.29_rk, -21.05_rk, -31.53_rk,  50.01_rk,   8.59_rk,  1.37_rk,   9.52_rk,   9.81_rk, -40.54_rk,  43.89_rk, & ! 2000-2009
       &  16.91_rk,  -1.80_rk,  15.91_rk,  38.32_rk,  13.40_rk,  5.73_rk,   7.50_rk,  28.24_rk,  -3.88_rk,  35.23_rk, & ! 2010-2019
       &  43.64_rk,  21.39_rk, -33.10_rk,  43.42_rk]                                                                    ! 2020-2023

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Dow Jones data
  !!
  real(kind=rk), parameter, public :: dow_dat(1970:2023)  = [ &
       &   4.82_rk,   6.11_rk,  14.58_rk, -16.58_rk, -27.57_rk, 38.32_rk,  17.86_rk, -17.27_rk,  -3.15_rk,   4.19_rk, & ! 1974-1979
       &  14.93_rk,  -9.23_rk,  19.61_rk,  20.27_rk,  -3.74_rk, 27.66_rk,  22.59_rk,   2.26_rk,  11.85_rk,  26.96_rk, & ! 1980-1989
       &  -4.34_rk,  20.32_rk,   4.17_rk,  13.72_rk,   2.14_rk, 33.45_rk,  26.01_rk,  22.64_rk,  16.35_rk,  24.95_rk, & ! 1990-1999
       &  -6.17_rk,  -7.11_rk, -16.76_rk,  25.32_rk,   3.15_rk, -0.61_rk,  16.29_rk,   6.43_rk, -33.84_rk,  18.82_rk, & ! 2000-2009
       &  11.02_rk,   5.53_rk,   7.26_rk,  26.50_rk,   7.52_rk, -2.23_rk,  13.42_rk,  25.08_rk,  -5.63_rk,  22.34_rk, & ! 2010-2019
       &   7.25_rk,  18.73_rk,  -8.78_rk,  13.70_rk]                                                                    ! 2020-2023

  !--------------------------------------------------------------------------------------------------------------------------------
  !> DGS10 US treasury 10 year (mean by year)
  !!
  real(kind=rk), parameter, public :: dgs10_dat(1962:2025)  = [ &
       &                        3.946_rk,  4.001_rk,  4.187_rk,  4.283_rk, 4.925_rk, 5.072_rk, 5.644_rk, 6.672_rk, & ! 1962-1969
       &  7.354_rk,  6.160_rk,  6.206_rk,  6.848_rk,  7.562_rk,  7.993_rk, 7.612_rk, 7.417_rk, 8.408_rk, 9.432_rk, & ! 1970-1979
       & 11.430_rk, 13.920_rk, 13.000_rk, 11.100_rk, 12.450_rk, 10.610_rk, 7.672_rk, 8.392_rk, 8.848_rk, 8.493_rk, & ! 1980-1989
       &  8.552_rk,  7.862_rk,  7.008_rk,  5.866_rk,  7.085_rk,  6.573_rk, 6.443_rk, 6.353_rk, 5.262_rk, 5.646_rk, & ! 1990-1999
       &  6.030_rk,  5.020_rk,  4.613_rk,  4.013_rk,  4.271_rk,  4.288_rk, 4.795_rk, 4.634_rk, 3.664_rk, 3.264_rk, & ! 2000-2009
       &  3.215_rk,  2.781_rk,  1.803_rk,  2.350_rk,  2.539_rk,  2.138_rk, 1.837_rk, 2.329_rk, 2.911_rk, 2.141_rk, & ! 2010-2019
       &  0.889_rk,  1.446_rk,  2.950_rk,  3.959_rk,  4.207_rk,  4.585_rk]                                           ! 2020-2025

  public  :: snp_resample, rut_resample, nas_resample, dow_resample, dgs10_resample

contains

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of S&P Data.
  !!
  real(kind=rk) function snp_resample(history_years)
    implicit none (type, external)
    integer(kind=ik), intent(in) :: history_years
    snp_resample = resample_tail(snp_dat, history_years)
  end function snp_resample

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of Russel 2000 Data.
  !!
  real(kind=rk) function rut_resample(history_years)
    implicit none (type, external)
    integer(kind=ik), intent(in) :: history_years
    rut_resample = resample_tail(rut_dat, history_years)
  end function rut_resample

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of Nasdaq Data.
  !!
  real(kind=rk) function nas_resample(history_years)
    implicit none (type, external)
    integer(kind=ik), intent(in) :: history_years
    nas_resample = resample_tail(nas_dat, history_years)
  end function nas_resample

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of Dow Data.
  !!
  real(kind=rk) function dow_resample(history_years)
    implicit none (type, external)
    integer(kind=ik), intent(in) :: history_years
    dow_resample = resample_tail(dow_dat, history_years)
  end function dow_resample

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Random return from the last history_years of US treasury 10 year data.
  !!
  real(kind=rk) function dgs10_resample(history_years)
    implicit none (type, external)
    integer(kind=ik), intent(in) :: history_years
    dgs10_resample = resample_tail(dgs10_dat, history_years)
  end function dgs10_resample

end module mrffl_us_markets

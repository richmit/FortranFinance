! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_config.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     Configuratoin for MRFFL.@EOL
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Configuration for MRFFL (MR Fortran Finance Library).
!!
module mrffl_config
  use, intrinsic :: ieee_arithmetic, only: ieee_selected_real_kind
 !use, intrinsic :: iso_c_binding,   only: c_double
  implicit none (type, external)
  public

  integer,          parameter :: rk                 = ieee_selected_real_kind(p=15, r=307) !< Real Kind

  real(kind=rk),    parameter :: zero_epsilon       = 1.0e-8_rk                            !< Test for zero
  real(kind=rk),    parameter :: consistent_epsilon = 1.0e-3_rk                            !< Check equation consistency
                                                                                       
  character(len=5), parameter :: fvfmt_ai           = "f15.4"                              !< Fmt for cash values
  character(len=5), parameter :: ftfmt_ai           = "a15"                                !< Fmt for cash titles (width > 12)

        !
        !                  Typical settings for fvfmt_ai & ftfmt_ai:
        !
        ! 000000000011111111112222222222
        ! 123456789012345678901234567890
        ! -000000000000000.0000    => 999 Trillion   f21.4 a21  Nation state level economic models
        ! -000000000000000.00      => 999 Trillion   f19.2 a19
        ! -000000000000000.        => 999 Trillion   f17.0 a17
        ! -000000000000.0000       => 999 Billion    f18.4 a18  Corprate finance
        ! -000000000000.00         => 999 Billion    f16.2 a16
        ! -000000000000.           => 999 Billion    f14.0 a14
        ! -000000000.0000          => 999 Million    f15.4 a15  Retirement planning
        ! -000000000.00            => 999 Million    f13.2 a13
        ! -000000000.              => 999 Million    f11.0 a11
        ! -000000.0000             => 999 Thousand   f12.4 a12  Small real estate & car loans
        ! -000000.00               => 999 Thousand   f10.4 a10
        ! -000000.                 => 999 Thousand   f8.4  a8
        !

end module mrffl_config

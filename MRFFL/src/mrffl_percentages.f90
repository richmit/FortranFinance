! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_percentages.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-13
!! @brief     Simple functions for working with percentages.@EOL
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

!##################################################################################################################################
!------------------------------------------------------------------------------------------------------------------------------
!> Simple functions for working with percentages.
!!
module mrffl_percentages
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik, zero_epsilon
  implicit none (type, external)
  private

  public :: percentage_to_fraction, fraction_to_percentage
  public :: percentage_of, percentage_change, percentage_of_total, add_percentage

contains

  !------------------------------------------------------------------------------------------------------------------------------
  !> Convert a percentage to a fraction.
  !!
  elemental real(kind=rk) function percentage_to_fraction(p)
    real(kind=rk), intent(in) :: p
    percentage_to_fraction = (p/100)
  end function percentage_to_fraction

  !------------------------------------------------------------------------------------------------------------------------------
  !> Convert a fraction to a percentage.
  !!
  elemental real(kind=rk) function fraction_to_percentage(f)
    real(kind=rk), intent(in) :: f
    fraction_to_percentage = (f*100)
  end function fraction_to_percentage

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute the percentage of a value.
  !!
  elemental real(kind=rk) function percentage_of(v, p)
    real(kind=rk), intent(in) :: v
    real(kind=rk), intent(in) :: p
    percentage_of = v * percentage_to_fraction(p)
  end function percentage_of

  !------------------------------------------------------------------------------------------------------------------------------
  !> Add a percentage of a value to that value.
  !!
  elemental real(kind=rk) function add_percentage(v, p)
    real(kind=rk), intent(in) :: v
    real(kind=rk), intent(in) :: p
    add_percentage = v * (1 + percentage_to_fraction(p))
  end function add_percentage

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute the percentage change.
  !! WARNING: We do not check to make sure that (v_to-v_from) is non-zero
  !!
  elemental real(kind=rk) function percentage_change(v_from, v_to)
    real(kind=rk), intent(in) :: v_from
    real(kind=rk), intent(in) :: v_to
    percentage_change = fraction_to_percentage((v_to-v_from)/v_from)
  end function percentage_change

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute the percentage of a total.
  !! WARNING: We do not check to make sure that (v_total) is non-zero
  !!
  elemental real(kind=rk) function percentage_of_total(v_total, v_part)
    real(kind=rk), intent(in) :: v_total
    real(kind=rk), intent(in) :: v_part
    if ((abs(v_part) < zero_epsilon) .and. (abs(v_total) < zero_epsilon)) then
       percentage_of_total = 0
    else
       percentage_of_total = fraction_to_percentage(v_part / v_total)
    end if
  end function percentage_of_total

end module mrffl_percentages

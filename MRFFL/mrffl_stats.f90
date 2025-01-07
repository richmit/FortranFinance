! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_stats.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     Statstical tools supporting MRFFL.@EOL
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
!> Some statstical utilities supporting other MRFFL modules.
!!
module mrffl_stats
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  implicit none  
  private                  

  public  :: resample_tail, resample_head
  public  :: mean_and_variance
  public  :: rand_int, rand_real
  public  :: rand_norm_std, rand_norm, rand_log_norm

contains

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random integer in U([0,bound))
  !!
  integer(kind=ik) function rand_int(bound)
    implicit none
    integer(kind=ik), intent(in) :: bound
   real(kind=rk)                 :: r  ! Random Number in [0, 1)
   call random_number(r)
   rand_int = int(r * bound)
  end function rand_int

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random real value on U([0,bound)).
  !!
  real(kind=rk) function rand_real(bound)
    implicit none
    real(kind=rk), intent(in) :: bound
    real(kind=rk)             :: r  ! Random Number in [0, 1)
   call random_number(r)
   rand_real = r * bound
  end function rand_real

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from among the last tail_length elements of data.
  !!
  real(kind=rk) function resample_tail(data, tail_length)
    implicit none
    real(kind=rk), intent(in)    :: data(:)
    integer(kind=ik), intent(in) :: tail_length
    integer(kind=ik)             :: i
    i = ubound(data, 1) - rand_int(min(tail_length, size(data)))
    i = max(i, lbound(data, 1))
    i = min(i, ubound(data, 1))
    resample_tail = data(i)
  end function resample_tail

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from among the first head_length elements of data.
  !!
  real(kind=rk) function resample_head(data, head_length)
    implicit none
    real(kind=rk), intent(in)    :: data(:)
    integer(kind=ik), intent(in) :: head_length
    integer(kind=ik)             :: i
    i = lbound(data, 1) + rand_int(min(tail_length, size(data)))
    i = max(i, lbound(data, 1))
    i = min(i, ubound(data, 1))
    resample_head = data(i)
  end function resample_head

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return mean of elements in an array
  !!
  subroutine mean_and_variance(mean, variance, data)
    implicit none
    real(kind=rk), intent(in)  :: data(:)
    real(kind=rk), intent(out) :: mean, variance
    mean     = sum(data) / size(data)
    variance = sum((data - mean) ** 2)
  end subroutine mean_and_variance

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from the standard normal distribution.
  !!
  real(kind=rk) function rand_norm_std()
    implicit none
    real(kind=rk), parameter :: pi = 4.0_rk * atan(1.0_rk)
    real(kind=rk)            :: u, v
    call random_number(u)
    call random_number(v)
    rand_norm_std = sqrt(-2 * log(u)) * cos(2 * pi * v)  ! sqrt(-2 * log(u)) * sin(2 * pi * v)
  end function rand_norm_std

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from the specified normal distribution.
  !!
  real(kind=rk) function rand_norm(mean, variance)
    implicit none
    real(kind=rk), intent(in) :: mean, variance
    rand_norm = rand_norm_std() * variance + mean
  end function rand_norm

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from the specified log-normal distribution.
  !!
  !!  @f[ \text{mean} = e^\left(\mu+\frac{\sigma^2}{2}\right)                @f]
  !!  @f[ \text{median} = e^\mu                                              @f]
  !!  @f[ \text{variance} = \left[e^{\sigma^2}-1\right] e^{2\mu+\sigma^2}    @f]
  real(kind=rk) function rand_log_norm(mu, sd)
    implicit none
    real(kind=rk), intent(in) :: mu, sd
    rand_log_norm = exp(rand_norm_std() * sd + mu)
  end function rand_log_norm

end module mrffl_stats

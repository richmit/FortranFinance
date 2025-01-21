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
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik, zero_epsilon
  implicit none
  private

  public  :: mean_and_variance
  public  :: rand_int, rand_real                                                ! Function wrapper on built in PRNG
  public  :: resample_tail, resample_head                                       ! Use rand_int
  public  :: rand_norm_std_box, rand_norm_std_probit, rand_norm_std_probit_clip ! Use: rand_real -- fundamental normal generators
  public  :: rand_norm_std                                                      ! Use: rand_norm_std_*
  public  :: rand_norm, rand_log_norm                                           ! Use: rand_norm_std
  public  :: probit
  public  :: geometric_brownian_motion, zero_clipped_brownian_motion

contains

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random integer in U([optional_lower_bound,upper_bound)) -- optional_lower_bound is 0 if missing.
  !!
  !! @param upper_bound  Upper bound for random number
  !! @param lower_bound  Lower bound for random number
  !!
  integer(kind=ik) function rand_int(upper_bound, optional_lower_bound)
    implicit none
    integer(kind=ik),           intent(in) :: upper_bound
    integer(kind=ik), optional, intent(in) :: optional_lower_bound
    integer(kind=ik)                       :: lower_bound = 0
    real(kind=rk)                          :: r
    if (present(optional_lower_bound)) lower_bound = optional_lower_bound
    if (lower_bound > upper_bound) then
       error stop "ERROR(rand_int): lower_bound > upper_bound!"
    end if
    call random_number(r) ! Random Number in [0, 1)
    rand_int = lower_bound + int(r * (upper_bound - lower_bound), kind=ik)
  end function rand_int

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random real value in U([optional_lower_bound,upper_bound)) -- optional_lower_bound is 0 if missing.
  !!
  !! @param upper_bound  Upper bound for random number
  !! @param lower_bound  Lower bound for random number
  !!
  real(kind=rk) function rand_real(upper_bound, optional_lower_bound)
    implicit none
    real(kind=rk),           intent(in) :: upper_bound
    real(kind=rk), optional, intent(in) :: optional_lower_bound
    real(kind=rk)                       :: lower_bound = 0
    real(kind=rk)                       :: r
    if (present(optional_lower_bound)) lower_bound = optional_lower_bound
    if (lower_bound > upper_bound) then
       error stop "ERROR(rand_real): lower_bound > upper_bound!"
    end if
    call random_number(r) ! Random Number in [0, 1)
    rand_real = lower_bound + r * (upper_bound - lower_bound)
  end function rand_real

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from among the last tail_length elements of data.
  !!
  !! @param data         The data set -- a rank 1 array.
  !! @param tail_length  Number of elements to consider for resample.
  !!
  real(kind=rk) function resample_tail(data, tail_length)
    implicit none
    real(kind=rk), intent(in)    :: data(:)
    integer(kind=ik), intent(in) :: tail_length
    integer(kind=ik)             :: i
    i = ubound(data, 1, kind=ik) - rand_int(min(tail_length, int(size(data), kind=ik)))
    i = max(i, lbound(data, 1, kind=ik))
    i = min(i, ubound(data, 1, kind=ik))
    resample_tail = data(i)
  end function resample_tail

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from among the first head_length elements of data.
  !!
  !! @param data         The data set -- a rank 1 array.
  !! @param head_length  Number of elements to consider for resample.
  !!
  real(kind=rk) function resample_head(data, head_length)
    implicit none
    real(kind=rk), intent(in)    :: data(:)
    integer(kind=ik), intent(in) :: head_length
    integer(kind=ik)             :: i
    i = lbound(data, 1, kind=ik) + rand_int(min(head_length, size(data, kind=ik)))
    i = max(i, lbound(data, 1, kind=ik))
    i = min(i, ubound(data, 1, kind=ik))
    resample_head = data(i)
  end function resample_head

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return mean & variance of elements in an array.
  !!
  !! @param mean      Holds the mean upon exit.
  !! @param variance  Holds the variance (standard deviation squared) upon exit.
  !! @param data      The data set -- a rank 1 array.
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
  !! This function simply calls the preferred rand_norm_std_* function.  Currently that's rand_norm_std_probit().
  !!
  real(kind=rk) function rand_norm_std()
    implicit none
    rand_norm_std = rand_norm_std_probit()
  end function rand_norm_std

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from the standard normal distribution using the Box-Muller transform.
  !!
  !! Reference:
  !!   Box, G. E. P., and Mervin E. Muller. 1958. "A Note on the Generation of Random Normal Deviates." The Annals of Mathematical Statistics 29 (2): 610-11.
  !!
  real(kind=rk) function rand_norm_std_box()
    implicit none
    real(kind=rk), parameter :: pi = 4.0_rk * atan(1.0_rk)
    real(kind=rk)            :: u, v
    logical, save            :: cached_valid = .false.
    real(kind=rk), save      :: cached_value = 0.0_rk
    if (cached_valid) then
       rand_norm_std_box = cached_value
       cached_valid      = .false.
    else
       call random_number(u)
       call random_number(v)
       rand_norm_std_box = sqrt(-2 * log(u)) * cos(2 * pi * v)
       cached_value      = sqrt(-2 * log(u)) * sin(2 * pi * v)
       cached_valid      = .true.
    end if
  end function rand_norm_std_box

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from the standard normal distribution in [-5.612, 5.612] using the Probit function.
  !!
  real(kind=rk) function rand_norm_std_probit_clip()
    implicit none
    real(kind=rk) :: u
    do
       call random_number(u)
       if ((u > zero_epsilon) .and. (u < (1.0_rk - zero_epsilon))) then
          rand_norm_std_probit_clip = probit(u)
          return
       end if
    end do
  end function rand_norm_std_probit_clip

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from the standard normal distribution using the Probit function.
  !!
  real(kind=rk) function rand_norm_std_probit()
    implicit none
    real(kind=rk) :: u
    do
       call random_number(u)
       if (u < 1.0_rk) then
          rand_norm_std_probit = probit(u)
          return
       end if
    end do
  end function rand_norm_std_probit

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return random value from the specified normal distribution.
  !!
  !! @param mean      Mean of the distribution
  !! @param variance  Variance (standard deviation squared) of the distribution
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
  !!
  !! @param mu     Mean of the distribution
  !! @param sigma  Standard deviation of the distribution
  !!
  real(kind=rk) function rand_log_norm(mu, sd)
    implicit none
    real(kind=rk), intent(in) :: mu, sd
    rand_log_norm = exp(rand_norm_std() * sd + mu)
  end function rand_log_norm

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Probit function -- i.e. inverse of standard normal CDF.
  !!    Return $z_p$ such that @f$ P(\mathcal{N}(0, 1)<=z_p) = p @f$
  !!
  !!  Reference:
  !!    Wichura, Michael J. 1988. "Algorithm AS 241: The Percentage Points of the Normal Distribution." Journal of the Royal Statistical Society. Series C (Applied Statistics) 37 (3): 477-84.
  !!
  !! @param p  Probablity in @f$ (0,1) $f$.
  !! @param x  The value at which to evaluate the polynomial
  !!
  real(kind=rk) function probit(p)
    implicit none
    real(kind=rk), intent(in) :: p
    real(kind=rk)             :: q
    real(kind=rk)             :: r
    real(kind=rk), parameter  :: const1 = 0.180625_rk
    real(kind=rk), parameter  :: const2 = 1.600000_rk
    real(kind=rk), parameter  :: split1 = 0.425000_rk
    real(kind=rk), parameter  :: split2 = 5.000000_rk
    real(kind=rk), parameter  :: r1_numr(8) = [ &  ! Coefficients for p close to 0.5
         2.50908092873012267270e+03_rk, 3.34305755835881281050e+04_rk, 6.72657709270087008530e+04_rk, 4.59219539315498714570e+04_rk, &
         1.37316937655094611250e+04_rk, 1.97159095030655144270e+03_rk, 1.33141667891784377450e+02_rk, 3.38713287279636660800e+00_rk  ]
    real(kind=rk), parameter  :: r1_dnom(8) = [ &
         5.22649527885285456100e+03_rk, 2.87290857357219426740e+04_rk, 3.93078958000927106100e+04_rk, 2.12137943015865958670e+04_rk, &
         5.39419602142475110770e+03_rk, 6.87187007492057908300e+02_rk, 4.23133307016009112520e+01_rk, 1.00000000000000000000e+00_rk  ]
    real(kind=rk), parameter  :: r2_numr(8) = [ & ! Coefficients for p not near 0.5, 0, or 1
         7.74545014278341407640e-04_rk, 2.27238449892691845833e-02_rk, 2.41780725177450611770e-01_rk, 1.27045825245236838258e+00_rk, &
         3.64784832476320460504e+00_rk, 5.76949722146069140550e+00_rk, 4.63033784615654529590e+00_rk, 1.42343711074968357734e+00_rk  ]
    real(kind=rk), parameter  :: r2_dnom(8) = [ &
         1.05075007164441684324e-09_rk, 5.47593808499534494600e-04_rk, 1.51986665636164571966e-02_rk, 1.48103976427480074590e-01_rk, &
         6.89767334985100004550e-01_rk, 1.67638483018380384940e+00_rk, 2.05319162663775882187e+00_rk, 1.00000000000000000000e+00_rk  ]
    real(kind=rk), parameter  :: r3_numr(8) = [ & ! Coefficients for p close 0 or 1
         2.01033439929228813265e-07_rk, 2.71155556874348757815e-05_rk, 1.24266094738807843860e-03_rk, 2.65321895265761230930e-02_rk, &
         2.96560571828504891230e-01_rk, 1.78482653991729133580e+00_rk, 5.46378491116411436990e+00_rk, 6.65790464350110377720e+00_rk  ]
    real(kind=rk), parameter  :: r3_dnom(8) = [ &
         2.04426310338993978564e-15_rk, 1.42151175831644588870e-07_rk, 1.84631831751005468180e-05_rk, 7.86869131145613259100e-04_rk, &
         1.48753612908506148525e-02_rk, 1.36929880922735805310e-01_rk, 5.99832206555887937690e-01_rk, 1.00000000000000000000e+00_rk  ]
    if (p <= 0.0_rk) then             ! $ p\in(\infty, 0] $
       probit = -huge(p)
    else if (1.0_rk <= p) then        ! $ p\in[1, \infty) $
       probit = huge(p)
    else
       q = p - 0.5_rk
       if(abs(q) <= split1) then      ! $ p\in[0.075, 0.925] $
          r = const1 - q * q
          probit = q * poly_eval(r1_numr, r) / poly_eval(r1_dnom, r)
       else                           ! $ NOT p\in[0.075, 0.925] $
          if (q < 0.0_rk) then            ! $ p\in[0, 0.075) $
             r = p
          else                            ! $ p\in(0.925, 1])
             r = 1.0_rk - p
          end if
          r = sqrt(-log(abs(r)))          ! p in ( 1.3887943865e-11, 0.999999999986)
          if (r <= split2) then
             r = r - const2
             probit = poly_eval(r2_numr, r) / poly_eval(r2_dnom, r)
          else                            ! p is close to 0 or 1
             r = r - split2
             probit = poly_eval(r3_numr, r) / poly_eval(r3_dnom, r)
          end if
          if (q < 0.0_rk) then
             probit = - probit
          end if
       end if
    end if
  end function probit

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Evaluate a univariate polynomial.
  !!
  !! Used by probit.  Not exported from the module.
  !!
  !! The polynomial is ordered in the natural way with the highest coefficient first in the array:
  !!   @f[ p=\sum_{k=1}^{d+1} p_kx^{1+d-k} @f]
  !! Note that $k$ in the above formula is the index in the array `p`.
  !!
  !! @param p  A $d+1$ element, rank 1 array holding the coefficients of a polynomial
  !! @param x  The value at which to evaluate the polynomial
  !!
  real(kind=rk) function poly_eval(p, x)
    implicit none
    real(kind=rk), intent(in) :: p(:)
    real(kind=rk), intent(in) :: x
    integer i
    poly_eval = p(1)
    do i = 2,size(p)
       poly_eval = poly_eval * x + p(i)
    end do
  end function poly_eval

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute a set of random steps in a GBM process.
  !!
  !! @param s0          Initial value of an asset
  !! @param mu          Mean gain for the assest over 1 unit of time
  !! @param sigma       Standard deviation of gain for the assest over 1 unit of time
  !! @param step_values A rank 1 array which will hold the values of the asset over time
  !!
  subroutine geometric_brownian_motion(step_values, s0, mu, sigma)
    real(kind=rk), intent(out) :: step_values(:)
    real(kind=rk), intent(in)  :: s0, mu, sigma
    real(kind=rk)              :: step_width, m, s, sum
    integer                    :: i, num_steps
    num_steps = size(step_values)
    step_width = 1.0_rk / (num_steps-1)
    m = (mu - 0.5 * sigma ** 2) * step_width
    s = sigma * sqrt(step_width)
    sum = 0
    do i=1,num_steps
       sum = sum + rand_norm(m, s)
       step_values(i) = s0 * exp(sum)
    end do
  end subroutine geometric_brownian_motion

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Compute a set of random steps in a zero clipped browinan motion.
  !!
  !! @param s0          Initial value of an asset
  !! @param mu          Mean gain for the assest over 1 unit of time
  !! @param sigma       Standard deviation of gain for the assest over 1 unit of time
  !! @param step_values A rank 1 array which will hold the values of the asset over time
  !!
  subroutine zero_clipped_brownian_motion(step_values, s0, mu, sigma)
    real(kind=rk), intent(out) :: step_values(:)
    real(kind=rk), intent(in)  :: s0, mu, sigma
    real(kind=rk)              :: step_width, m, s
    integer                    :: i, num_steps
    num_steps = size(step_values)
    step_width = 1.0_rk / (num_steps-1)
    m = mu * step_width
    s = sigma * sqrt(step_width)
    step_values(1) = s0
    do i=2,num_steps
       if (abs(step_values(i-1)) < zero_epsilon) then
          step_values(i) = 0.0_rk
       else
          step_values(i) = max(step_values(i-1) * (1 + rand_norm(m, s)), 0.0_rk)
       end if
    end do
  end subroutine zero_clipped_brownian_motion

end module mrffl_stats

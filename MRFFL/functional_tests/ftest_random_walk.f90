! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      ftest_random_walk.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-20
!! @brief     Demonstrate geometric_brownian_motion & zero_clipped_brownian_motion.@EOL
!! @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
!! @std       F2023
!! @see       https://github.com/richmit/FortranFinance
!! @copyright
!!  @parblock
!!  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
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
program ftest_random_walk
  use :: mrffl_config, only: rk
  use :: mrffl_stats,  only: geometric_brownian_motion, zero_clipped_brownian_motion

  implicit none (type, external)

  integer, parameter :: num_runs  = 10000
  integer, parameter :: num_steps = 365
  real(kind=rk)      :: s0 = 1.0_rk, mu = 0.04_rk, sigma = 0.18_rk
  real(kind=rk)      :: walk1(num_steps), walk2(num_steps)
  integer            :: i, j
  integer            :: out_io_unit

  open(newunit=out_io_unit, file='ftest_random_walk.txt', form='formatted', action='write')
  write (unit=out_io_unit, fmt='(a10,a10,a15,a15)') "sim", "step", "val1", "val2"
  do i=1,num_runs
     call geometric_brownian_motion(walk1, s0, mu, sigma)
     call zero_clipped_brownian_motion(walk2, s0, mu, sigma)
     do j=1,num_steps
        write (unit=out_io_unit, fmt='(i10,i10,f15.5,f15.5)') i, j, walk1(j), walk2(j)
     end do
  end do
  close(unit=out_io_unit, status='keep')

end program ftest_random_walk

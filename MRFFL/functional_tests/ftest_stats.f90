! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      ftest_stats.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-01
!! @brief     Test mrffl_stats.@EOL
!! @std       F2023
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
program ftest_stats
  use mrffl_config, only: rk=>mrfflrk
  use mrffl_stats,  only: rand_norm, rand_log_norm, rand_norm_std_box, rand_norm_std_probit, rand_norm_std_probit_clip, &
       &                  mean_and_variance, resample_tail, resample_head

  implicit none (type, external)


  integer, parameter :: num_runs = 10000
  real(kind=rk)      :: a(7) = [1,2,3,4,5,6,7]
  real(kind=rk)      :: m, v
  integer            :: lim = 5
  integer            :: i
  integer            :: out_io_unit

  print *
  call mean_and_variance(m, v, a)
  print *, m, v

  print *
  do i=1,10
     print *, "tail ", resample_tail(a, lim)
  end do

  print *
  do i=1,10
     print *, "head ", resample_head(a, lim)
  end do

  print *
  open(newunit=out_io_unit, file='ftest_stats_rand_norm203.txt', form='formatted', action='write')
  write (unit=out_io_unit, fmt='(a20)') "norm203"
  do i=1,num_runs
     write (unit=out_io_unit, fmt='(f20.5)') rand_norm(20.0_rk, 3.0_rk)
  end do
  close(unit=out_io_unit, status='keep')

  print *
  open(newunit=out_io_unit, file='ftest_stats_rand_log_norm.txt', form='formatted', action='write')
  write (unit=out_io_unit, fmt='(a20)') "log_norm"
  do i=1,num_runs
     write (unit=out_io_unit, fmt='(f20.5)') rand_log_norm(2.0_rk, 0.5_rk)
  end do
  close(unit=out_io_unit, status='keep')

  print *
  open(newunit=out_io_unit, file='ftest_stats_rand_norm_std.txt', form='formatted', action='write')
  write (unit=out_io_unit, fmt='(a20,a20,a20)') "z_box", "z_probit", "z_probit_clip"
  do i=1,num_runs
     write (unit=out_io_unit, fmt='(f20.5,f20.5,f20.5)') rand_norm_std_box(), rand_norm_std_probit(), rand_norm_std_probit_clip()
  end do
  close(unit=out_io_unit, status='keep')

end program ftest_stats

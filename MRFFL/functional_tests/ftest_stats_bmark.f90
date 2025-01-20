! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      ftest_stats_bmark.f90
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
program ftest_stats_bmark
  use mrffl_config, only: rk=>mrfflrk
  use mrffl_stats

  integer, parameter :: num_runs = 100000000
  real(kind=rk)      :: sum
  integer            :: i, clock_rate, clock_max, clock_start, clock_end

  call system_clock(count_rate=clock_rate)
  call system_clock(count_max=clock_max)

  call system_clock(clock_start)
  sum = 0
  do i=1,num_runs
     sum = sum + rand_norm_std_probit()
  end do
  call system_clock(clock_end)
  print '(a40,f10.4)', "rand_norm_std_probit time: ", (clock_end-clock_start)/real(clock_rate, rk)

  call system_clock(clock_start)
  sum = 0
  do i=1,num_runs
     sum = sum + rand_norm_std_probit_clip()
  end do
  call system_clock(clock_end)
  print '(a40,f10.4)', "rand_norm_std_probit_clip time: ", (clock_end-clock_start)/real(clock_rate, rk)

  call system_clock(clock_start)
  sum = 0
  do i=1,num_runs
     sum = sum + rand_norm_std_box()
  end do
  call system_clock(clock_end)
  print '(a40,f10.4)', "rand_norm_std_box time: ", (clock_end-clock_start)/real(clock_rate, rk)

end program ftest_stats_bmark

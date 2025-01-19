! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      utest_life_table_print.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit tests for life_table_print subroutine.@EOL
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
program utest_life_table_print
  use, intrinsic :: iso_fortran_env, only: output_unit
  use mrffl_config, only: ik=>mrfflik
  use mrffl_life_table
  use mrffl_prt_sets
  implicit none

  print '(a)', "usss_f_qx_dat"
  call life_table_print(output_unit, prt_ALL, usss_f_qx_dat, 100000_ik)
  print '(a)', "usss_f_lx_dat"
  call life_table_print(output_unit, prt_ALL, usss_f_lx_dat,      0_ik)
  print '(a)', "usss_m_qx_dat"
  call life_table_print(output_unit, prt_ALL, usss_m_qx_dat, 100000_ik)
  print '(a)', "usss_m_lx_dat"
  call life_table_print(output_unit, prt_ALL, usss_m_lx_dat,      0_ik)
  print '(a)', "uscdc_w_f_lx_dat"
  call life_table_print(output_unit, prt_ALL, uscdc_w_f_lx_dat,   0_ik)
  print '(a)', "uscdc_w_f_lx_dat"
  call life_table_print(output_unit, prt_ALL, uscdc_w_f_lx_dat,   0_ik)
  print '(a)', "uscdc_w_lx_dat"
  call life_table_print(output_unit, prt_ALL, uscdc_w_lx_dat,     0_ik)
  print '(a)', "uscdc_lx_dat"
  call life_table_print(output_unit, prt_ALL, uscdc_lx_dat,       0_ik)

end program utest_life_table_print

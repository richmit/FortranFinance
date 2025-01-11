! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      test_life_table.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: mrffl_us_taxes.@EOL
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
program test_life_table
  use, intrinsic:: iso_c_binding
  use mrffl_life_table
  use mrffl_prt_sets
  implicit none

  print '(a)', repeat("=", 115)

  print '(a55,f30.10,f30.10)', "survivors(-10,  usss_f_qx_dat, 100000): ", survivors(-10,  usss_f_qx_dat, 100000), survivors(-10,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(-10,  usss_m_qx_dat, 100000): ", survivors(-10,  usss_m_qx_dat, 100000), survivors(-10,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "survivors(-1,   usss_f_qx_dat, 100000): ", survivors(-1,   usss_f_qx_dat, 100000), survivors(-1,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(-1,   usss_m_qx_dat, 100000): ", survivors(-1,   usss_m_qx_dat, 100000), survivors(-1,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "survivors(0,    usss_f_qx_dat, 100000): ", survivors(0,    usss_f_qx_dat, 100000), survivors(0,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(0,    usss_m_qx_dat, 100000): ", survivors(0,    usss_m_qx_dat, 100000), survivors(0,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "survivors(1,    usss_f_qx_dat, 100000): ", survivors(1,    usss_f_qx_dat, 100000), survivors(1,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(1,    usss_m_qx_dat, 100000): ", survivors(1,    usss_m_qx_dat, 100000), survivors(1,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "survivors(10,   usss_f_qx_dat, 100000): ", survivors(10,   usss_f_qx_dat, 100000), survivors(10,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(10,   usss_m_qx_dat, 100000): ", survivors(10,   usss_m_qx_dat, 100000), survivors(10,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "survivors(100,  usss_f_qx_dat, 100000): ", survivors(100,  usss_f_qx_dat, 100000), survivors(100,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(100,  usss_m_qx_dat, 100000): ", survivors(100,  usss_m_qx_dat, 100000), survivors(100,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "survivors(119,  usss_f_qx_dat, 100000): ", survivors(119,  usss_f_qx_dat, 100000), survivors(119,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(119,  usss_m_qx_dat, 100000): ", survivors(119,  usss_m_qx_dat, 100000), survivors(119,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "survivors(120,  usss_f_qx_dat, 100000): ", survivors(120,  usss_f_qx_dat, 100000), survivors(120,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(120,  usss_m_qx_dat, 100000): ", survivors(120,  usss_m_qx_dat, 100000), survivors(120,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "survivors(1000, usss_f_qx_dat, 100000): ", survivors(1000, usss_f_qx_dat, 100000), survivors(1000, usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "survivors(1000, usss_m_qx_dat, 100000): ", survivors(1000, usss_m_qx_dat, 100000), survivors(1000, usss_m_lx_dat, 0)

  print '(a)', repeat("=", 115)

  print '(a55,f30.10,f30.10)', "probability_of_death(-10,  usss_f_qx_dat, 100000): ", probability_of_death(-10,  usss_f_qx_dat, 100000), probability_of_death(-10,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(-10,  usss_m_qx_dat, 100000): ", probability_of_death(-10,  usss_m_qx_dat, 100000), probability_of_death(-10,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "probability_of_death(-1,   usss_f_qx_dat, 100000): ", probability_of_death(-1,   usss_f_qx_dat, 100000), probability_of_death(-1,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(-1,   usss_m_qx_dat, 100000): ", probability_of_death(-1,   usss_m_qx_dat, 100000), probability_of_death(-1,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "probability_of_death(0,    usss_f_qx_dat, 100000): ", probability_of_death(0,    usss_f_qx_dat, 100000), probability_of_death(0,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(0,    usss_m_qx_dat, 100000): ", probability_of_death(0,    usss_m_qx_dat, 100000), probability_of_death(0,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "probability_of_death(1,    usss_f_qx_dat, 100000): ", probability_of_death(1,    usss_f_qx_dat, 100000), probability_of_death(1,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(1,    usss_m_qx_dat, 100000): ", probability_of_death(1,    usss_m_qx_dat, 100000), probability_of_death(1,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "probability_of_death(10,   usss_f_qx_dat, 100000): ", probability_of_death(10,   usss_f_qx_dat, 100000), probability_of_death(10,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(10,   usss_m_qx_dat, 100000): ", probability_of_death(10,   usss_m_qx_dat, 100000), probability_of_death(10,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "probability_of_death(100,  usss_f_qx_dat, 100000): ", probability_of_death(100,  usss_f_qx_dat, 100000), probability_of_death(100,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(100,  usss_m_qx_dat, 100000): ", probability_of_death(100,  usss_m_qx_dat, 100000), probability_of_death(100,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "probability_of_death(119,  usss_f_qx_dat, 100000): ", probability_of_death(119,  usss_f_qx_dat, 100000), probability_of_death(119,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(119,  usss_m_qx_dat, 100000): ", probability_of_death(119,  usss_m_qx_dat, 100000), probability_of_death(119,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "probability_of_death(120,  usss_f_qx_dat, 100000): ", probability_of_death(120,  usss_f_qx_dat, 100000), probability_of_death(120,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(120,  usss_m_qx_dat, 100000): ", probability_of_death(120,  usss_m_qx_dat, 100000), probability_of_death(120,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "probability_of_death(1000, usss_f_qx_dat, 100000): ", probability_of_death(1000, usss_f_qx_dat, 100000), probability_of_death(1000, usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "probability_of_death(1000, usss_m_qx_dat, 100000): ", probability_of_death(1000, usss_m_qx_dat, 100000), probability_of_death(1000, usss_m_lx_dat, 0)

  print '(a)', repeat("=", 115)

  print '(a55,f30.10,f30.10)', "died(-10,  usss_f_qx_dat, 100000): ", died(-10,  usss_f_qx_dat, 100000), died(-10,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(-10,  usss_m_qx_dat, 100000): ", died(-10,  usss_m_qx_dat, 100000), died(-10,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "died(-1,   usss_f_qx_dat, 100000): ", died(-1,   usss_f_qx_dat, 100000), died(-1,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(-1,   usss_m_qx_dat, 100000): ", died(-1,   usss_m_qx_dat, 100000), died(-1,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "died(0,    usss_f_qx_dat, 100000): ", died(0,    usss_f_qx_dat, 100000), died(0,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(0,    usss_m_qx_dat, 100000): ", died(0,    usss_m_qx_dat, 100000), died(0,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "died(1,    usss_f_qx_dat, 100000): ", died(1,    usss_f_qx_dat, 100000), died(1,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(1,    usss_m_qx_dat, 100000): ", died(1,    usss_m_qx_dat, 100000), died(1,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "died(10,   usss_f_qx_dat, 100000): ", died(10,   usss_f_qx_dat, 100000), died(10,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(10,   usss_m_qx_dat, 100000): ", died(10,   usss_m_qx_dat, 100000), died(10,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "died(100,  usss_f_qx_dat, 100000): ", died(100,  usss_f_qx_dat, 100000), died(100,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(100,  usss_m_qx_dat, 100000): ", died(100,  usss_m_qx_dat, 100000), died(100,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "died(119,  usss_f_qx_dat, 100000): ", died(119,  usss_f_qx_dat, 100000), died(119,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(119,  usss_m_qx_dat, 100000): ", died(119,  usss_m_qx_dat, 100000), died(119,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "died(120,  usss_f_qx_dat, 100000): ", died(120,  usss_f_qx_dat, 100000), died(120,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(120,  usss_m_qx_dat, 100000): ", died(120,  usss_m_qx_dat, 100000), died(120,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "died(1000, usss_f_qx_dat, 100000): ", died(1000, usss_f_qx_dat, 100000), died(1000, usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "died(1000, usss_m_qx_dat, 100000): ", died(1000, usss_m_qx_dat, 100000), died(1000, usss_m_lx_dat, 0)

  print '(a)', repeat("=", 115)

  print '(a55,f30.10,f30.10)', "life_expectancy(-10,  usss_f_qx_dat, 100000): ", life_expectancy(-10,  usss_f_qx_dat, 100000), life_expectancy(-10,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(-10,  usss_m_qx_dat, 100000): ", life_expectancy(-10,  usss_m_qx_dat, 100000), life_expectancy(-10,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "life_expectancy(-1,   usss_f_qx_dat, 100000): ", life_expectancy(-1,   usss_f_qx_dat, 100000), life_expectancy(-1,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(-1,   usss_m_qx_dat, 100000): ", life_expectancy(-1,   usss_m_qx_dat, 100000), life_expectancy(-1,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "life_expectancy(0,    usss_f_qx_dat, 100000): ", life_expectancy(0,    usss_f_qx_dat, 100000), life_expectancy(0,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(0,    usss_m_qx_dat, 100000): ", life_expectancy(0,    usss_m_qx_dat, 100000), life_expectancy(0,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "life_expectancy(1,    usss_f_qx_dat, 100000): ", life_expectancy(1,    usss_f_qx_dat, 100000), life_expectancy(1,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(1,    usss_m_qx_dat, 100000): ", life_expectancy(1,    usss_m_qx_dat, 100000), life_expectancy(1,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "life_expectancy(10,   usss_f_qx_dat, 100000): ", life_expectancy(10,   usss_f_qx_dat, 100000), life_expectancy(10,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(10,   usss_m_qx_dat, 100000): ", life_expectancy(10,   usss_m_qx_dat, 100000), life_expectancy(10,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "life_expectancy(100,  usss_f_qx_dat, 100000): ", life_expectancy(100,  usss_f_qx_dat, 100000), life_expectancy(100,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(100,  usss_m_qx_dat, 100000): ", life_expectancy(100,  usss_m_qx_dat, 100000), life_expectancy(100,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "life_expectancy(119,  usss_f_qx_dat, 100000): ", life_expectancy(119,  usss_f_qx_dat, 100000), life_expectancy(119,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(119,  usss_m_qx_dat, 100000): ", life_expectancy(119,  usss_m_qx_dat, 100000), life_expectancy(119,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "life_expectancy(120,  usss_f_qx_dat, 100000): ", life_expectancy(120,  usss_f_qx_dat, 100000), life_expectancy(120,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(120,  usss_m_qx_dat, 100000): ", life_expectancy(120,  usss_m_qx_dat, 100000), life_expectancy(120,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "life_expectancy(1000, usss_f_qx_dat, 100000): ", life_expectancy(1000, usss_f_qx_dat, 100000), life_expectancy(1000, usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy(1000, usss_m_qx_dat, 100000): ", life_expectancy(1000, usss_m_qx_dat, 100000), life_expectancy(1000, usss_m_lx_dat, 0)

  print '(a)', repeat("=", 115)

  print '(a55,f30.10,f30.10)', "mortality_rate(-10,  usss_f_qx_dat, 100000): ", mortality_rate(-10,  usss_f_qx_dat, 100000), mortality_rate(-10,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(-10,  usss_m_qx_dat, 100000): ", mortality_rate(-10,  usss_m_qx_dat, 100000), mortality_rate(-10,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "mortality_rate(-1,   usss_f_qx_dat, 100000): ", mortality_rate(-1,   usss_f_qx_dat, 100000), mortality_rate(-1,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(-1,   usss_m_qx_dat, 100000): ", mortality_rate(-1,   usss_m_qx_dat, 100000), mortality_rate(-1,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "mortality_rate(0,    usss_f_qx_dat, 100000): ", mortality_rate(0,    usss_f_qx_dat, 100000), mortality_rate(0,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(0,    usss_m_qx_dat, 100000): ", mortality_rate(0,    usss_m_qx_dat, 100000), mortality_rate(0,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "mortality_rate(1,    usss_f_qx_dat, 100000): ", mortality_rate(1,    usss_f_qx_dat, 100000), mortality_rate(1,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(1,    usss_m_qx_dat, 100000): ", mortality_rate(1,    usss_m_qx_dat, 100000), mortality_rate(1,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "mortality_rate(10,   usss_f_qx_dat, 100000): ", mortality_rate(10,   usss_f_qx_dat, 100000), mortality_rate(10,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(10,   usss_m_qx_dat, 100000): ", mortality_rate(10,   usss_m_qx_dat, 100000), mortality_rate(10,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "mortality_rate(100,  usss_f_qx_dat, 100000): ", mortality_rate(100,  usss_f_qx_dat, 100000), mortality_rate(100,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(100,  usss_m_qx_dat, 100000): ", mortality_rate(100,  usss_m_qx_dat, 100000), mortality_rate(100,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "mortality_rate(119,  usss_f_qx_dat, 100000): ", mortality_rate(119,  usss_f_qx_dat, 100000), mortality_rate(119,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(119,  usss_m_qx_dat, 100000): ", mortality_rate(119,  usss_m_qx_dat, 100000), mortality_rate(119,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "mortality_rate(120,  usss_f_qx_dat, 100000): ", mortality_rate(120,  usss_f_qx_dat, 100000), mortality_rate(120,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(120,  usss_m_qx_dat, 100000): ", mortality_rate(120,  usss_m_qx_dat, 100000), mortality_rate(120,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "mortality_rate(1000, usss_f_qx_dat, 100000): ", mortality_rate(1000, usss_f_qx_dat, 100000), mortality_rate(1000, usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "mortality_rate(1000, usss_m_qx_dat, 100000): ", mortality_rate(1000, usss_m_qx_dat, 100000), mortality_rate(1000, usss_m_lx_dat, 0)

  print '(a)', repeat("=", 115)

  print '(a55,f30.10,f30.10)', "person_years(-10,  usss_f_qx_dat, 100000): ", person_years(-10,  usss_f_qx_dat, 100000), person_years(-10,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(-10,  usss_m_qx_dat, 100000): ", person_years(-10,  usss_m_qx_dat, 100000), person_years(-10,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "person_years(-1,   usss_f_qx_dat, 100000): ", person_years(-1,   usss_f_qx_dat, 100000), person_years(-1,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(-1,   usss_m_qx_dat, 100000): ", person_years(-1,   usss_m_qx_dat, 100000), person_years(-1,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "person_years(0,    usss_f_qx_dat, 100000): ", person_years(0,    usss_f_qx_dat, 100000), person_years(0,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(0,    usss_m_qx_dat, 100000): ", person_years(0,    usss_m_qx_dat, 100000), person_years(0,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "person_years(1,    usss_f_qx_dat, 100000): ", person_years(1,    usss_f_qx_dat, 100000), person_years(1,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(1,    usss_m_qx_dat, 100000): ", person_years(1,    usss_m_qx_dat, 100000), person_years(1,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "person_years(10,   usss_f_qx_dat, 100000): ", person_years(10,   usss_f_qx_dat, 100000), person_years(10,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(10,   usss_m_qx_dat, 100000): ", person_years(10,   usss_m_qx_dat, 100000), person_years(10,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "person_years(100,  usss_f_qx_dat, 100000): ", person_years(100,  usss_f_qx_dat, 100000), person_years(100,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(100,  usss_m_qx_dat, 100000): ", person_years(100,  usss_m_qx_dat, 100000), person_years(100,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "person_years(119,  usss_f_qx_dat, 100000): ", person_years(119,  usss_f_qx_dat, 100000), person_years(119,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(119,  usss_m_qx_dat, 100000): ", person_years(119,  usss_m_qx_dat, 100000), person_years(119,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "person_years(120,  usss_f_qx_dat, 100000): ", person_years(120,  usss_f_qx_dat, 100000), person_years(120,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(120,  usss_m_qx_dat, 100000): ", person_years(120,  usss_m_qx_dat, 100000), person_years(120,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "person_years(1000, usss_f_qx_dat, 100000): ", person_years(1000, usss_f_qx_dat, 100000), person_years(1000, usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "person_years(1000, usss_m_qx_dat, 100000): ", person_years(1000, usss_m_qx_dat, 100000), person_years(1000, usss_m_lx_dat, 0)

  print '(a)', repeat("=", 235)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(-10,  usss_f_qx_dat, 100000): ", probability_of_survival_1(-10,  usss_f_qx_dat, 100000), probability_of_survival_n(-10,  1, usss_f_qx_dat, 100000), 1-probability_of_death(-10,  usss_f_qx_dat, 100000), probability_of_survival_1(-10,  usss_f_lx_dat, 0), probability_of_survival_n(-10,  1, usss_f_lx_dat, 0), 1-probability_of_death(-10,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(-10,  usss_m_qx_dat, 100000): ", probability_of_survival_1(-10,  usss_m_qx_dat, 100000), probability_of_survival_n(-10,  1, usss_m_qx_dat, 100000), 1-probability_of_death(-10,  usss_m_qx_dat, 100000), probability_of_survival_1(-10,  usss_m_lx_dat, 0), probability_of_survival_n(-10,  1, usss_m_lx_dat, 0), 1-probability_of_death(-10,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(-1,   usss_f_qx_dat, 100000): ", probability_of_survival_1(-1,   usss_f_qx_dat, 100000), probability_of_survival_n(-1,   1, usss_f_qx_dat, 100000), 1-probability_of_death(-1,   usss_f_qx_dat, 100000), probability_of_survival_1(-1,   usss_f_lx_dat, 0), probability_of_survival_n(-1,   1, usss_f_lx_dat, 0), 1-probability_of_death(-1,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(-1,   usss_m_qx_dat, 100000): ", probability_of_survival_1(-1,   usss_m_qx_dat, 100000), probability_of_survival_n(-1,   1, usss_m_qx_dat, 100000), 1-probability_of_death(-1,   usss_m_qx_dat, 100000), probability_of_survival_1(-1,   usss_m_lx_dat, 0), probability_of_survival_n(-1,   1, usss_m_lx_dat, 0), 1-probability_of_death(-1,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(0,    usss_f_qx_dat, 100000): ", probability_of_survival_1(0,    usss_f_qx_dat, 100000), probability_of_survival_n(0,    1, usss_f_qx_dat, 100000), 1-probability_of_death(0,    usss_f_qx_dat, 100000), probability_of_survival_1(0,    usss_f_lx_dat, 0), probability_of_survival_n(0,    1, usss_f_lx_dat, 0), 1-probability_of_death(0,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(0,    usss_m_qx_dat, 100000): ", probability_of_survival_1(0,    usss_m_qx_dat, 100000), probability_of_survival_n(0,    1, usss_m_qx_dat, 100000), 1-probability_of_death(0,    usss_m_qx_dat, 100000), probability_of_survival_1(0,    usss_m_lx_dat, 0), probability_of_survival_n(0,    1, usss_m_lx_dat, 0), 1-probability_of_death(0,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(1,    usss_f_qx_dat, 100000): ", probability_of_survival_1(1,    usss_f_qx_dat, 100000), probability_of_survival_n(1,    1, usss_f_qx_dat, 100000), 1-probability_of_death(1,    usss_f_qx_dat, 100000), probability_of_survival_1(1,    usss_f_lx_dat, 0), probability_of_survival_n(1,    1, usss_f_lx_dat, 0), 1-probability_of_death(1,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(1,    usss_m_qx_dat, 100000): ", probability_of_survival_1(1,    usss_m_qx_dat, 100000), probability_of_survival_n(1,    1, usss_m_qx_dat, 100000), 1-probability_of_death(1,    usss_m_qx_dat, 100000), probability_of_survival_1(1,    usss_m_lx_dat, 0), probability_of_survival_n(1,    1, usss_m_lx_dat, 0), 1-probability_of_death(1,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(10,   usss_f_qx_dat, 100000): ", probability_of_survival_1(10,   usss_f_qx_dat, 100000), probability_of_survival_n(10,   1, usss_f_qx_dat, 100000), 1-probability_of_death(10,   usss_f_qx_dat, 100000), probability_of_survival_1(10,   usss_f_lx_dat, 0), probability_of_survival_n(10,   1, usss_f_lx_dat, 0), 1-probability_of_death(10,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(10,   usss_m_qx_dat, 100000): ", probability_of_survival_1(10,   usss_m_qx_dat, 100000), probability_of_survival_n(10,   1, usss_m_qx_dat, 100000), 1-probability_of_death(10,   usss_m_qx_dat, 100000), probability_of_survival_1(10,   usss_m_lx_dat, 0), probability_of_survival_n(10,   1, usss_m_lx_dat, 0), 1-probability_of_death(10,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(100,  usss_f_qx_dat, 100000): ", probability_of_survival_1(100,  usss_f_qx_dat, 100000), probability_of_survival_n(100,  1, usss_f_qx_dat, 100000), 1-probability_of_death(100,  usss_f_qx_dat, 100000), probability_of_survival_1(100,  usss_f_lx_dat, 0), probability_of_survival_n(100,  1, usss_f_lx_dat, 0), 1-probability_of_death(100,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(100,  usss_m_qx_dat, 100000): ", probability_of_survival_1(100,  usss_m_qx_dat, 100000), probability_of_survival_n(100,  1, usss_m_qx_dat, 100000), 1-probability_of_death(100,  usss_m_qx_dat, 100000), probability_of_survival_1(100,  usss_m_lx_dat, 0), probability_of_survival_n(100,  1, usss_m_lx_dat, 0), 1-probability_of_death(100,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(119,  usss_f_qx_dat, 100000): ", probability_of_survival_1(119,  usss_f_qx_dat, 100000), probability_of_survival_n(119,  1, usss_f_qx_dat, 100000), 1-probability_of_death(119,  usss_f_qx_dat, 100000), probability_of_survival_1(119,  usss_f_lx_dat, 0), probability_of_survival_n(119,  1, usss_f_lx_dat, 0), 1-probability_of_death(119,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(119,  usss_m_qx_dat, 100000): ", probability_of_survival_1(119,  usss_m_qx_dat, 100000), probability_of_survival_n(119,  1, usss_m_qx_dat, 100000), 1-probability_of_death(119,  usss_m_qx_dat, 100000), probability_of_survival_1(119,  usss_m_lx_dat, 0), probability_of_survival_n(119,  1, usss_m_lx_dat, 0), 1-probability_of_death(119,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(120,  usss_f_qx_dat, 100000): ", probability_of_survival_1(120,  usss_f_qx_dat, 100000), probability_of_survival_n(120,  1, usss_f_qx_dat, 100000), 1-probability_of_death(120,  usss_f_qx_dat, 100000), probability_of_survival_1(120,  usss_f_lx_dat, 0), probability_of_survival_n(120,  1, usss_f_lx_dat, 0), 1-probability_of_death(120,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(120,  usss_m_qx_dat, 100000): ", probability_of_survival_1(120,  usss_m_qx_dat, 100000), probability_of_survival_n(120,  1, usss_m_qx_dat, 100000), 1-probability_of_death(120,  usss_m_qx_dat, 100000), probability_of_survival_1(120,  usss_m_lx_dat, 0), probability_of_survival_n(120,  1, usss_m_lx_dat, 0), 1-probability_of_death(120,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(1000, usss_f_qx_dat, 100000): ", probability_of_survival_1(1000, usss_f_qx_dat, 100000), probability_of_survival_n(1000, 1, usss_f_qx_dat, 100000), 1-probability_of_death(1000, usss_f_qx_dat, 100000), probability_of_survival_1(1000, usss_f_lx_dat, 0), probability_of_survival_n(1000, 1, usss_f_lx_dat, 0), 1-probability_of_death(1000, usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10,f30.10,f30.10,f30.10,f30.10)', "probability_of_survival_1(1000, usss_m_qx_dat, 100000): ", probability_of_survival_1(1000, usss_m_qx_dat, 100000), probability_of_survival_n(1000, 1, usss_m_qx_dat, 100000), 1-probability_of_death(1000, usss_m_qx_dat, 100000), probability_of_survival_1(1000, usss_m_lx_dat, 0), probability_of_survival_n(1000, 1, usss_m_lx_dat, 0), 1-probability_of_death(1000, usss_m_lx_dat, 0)

  print '(a)', repeat("=", 235)

  print '(a55,f30.10,f30.10)', "total_person_years(-10,  usss_f_qx_dat, 100000): ", total_person_years(-10,  usss_f_qx_dat, 100000), total_person_years(-10,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(-10,  usss_m_qx_dat, 100000): ", total_person_years(-10,  usss_m_qx_dat, 100000), total_person_years(-10,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "total_person_years(-1,   usss_f_qx_dat, 100000): ", total_person_years(-1,   usss_f_qx_dat, 100000), total_person_years(-1,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(-1,   usss_m_qx_dat, 100000): ", total_person_years(-1,   usss_m_qx_dat, 100000), total_person_years(-1,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "total_person_years(0,    usss_f_qx_dat, 100000): ", total_person_years(0,    usss_f_qx_dat, 100000), total_person_years(0,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(0,    usss_m_qx_dat, 100000): ", total_person_years(0,    usss_m_qx_dat, 100000), total_person_years(0,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "total_person_years(1,    usss_f_qx_dat, 100000): ", total_person_years(1,    usss_f_qx_dat, 100000), total_person_years(1,    usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(1,    usss_m_qx_dat, 100000): ", total_person_years(1,    usss_m_qx_dat, 100000), total_person_years(1,    usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "total_person_years(10,   usss_f_qx_dat, 100000): ", total_person_years(10,   usss_f_qx_dat, 100000), total_person_years(10,   usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(10,   usss_m_qx_dat, 100000): ", total_person_years(10,   usss_m_qx_dat, 100000), total_person_years(10,   usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "total_person_years(100,  usss_f_qx_dat, 100000): ", total_person_years(100,  usss_f_qx_dat, 100000), total_person_years(100,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(100,  usss_m_qx_dat, 100000): ", total_person_years(100,  usss_m_qx_dat, 100000), total_person_years(100,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "total_person_years(119,  usss_f_qx_dat, 100000): ", total_person_years(119,  usss_f_qx_dat, 100000), total_person_years(119,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(119,  usss_m_qx_dat, 100000): ", total_person_years(119,  usss_m_qx_dat, 100000), total_person_years(119,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "total_person_years(120,  usss_f_qx_dat, 100000): ", total_person_years(120,  usss_f_qx_dat, 100000), total_person_years(120,  usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(120,  usss_m_qx_dat, 100000): ", total_person_years(120,  usss_m_qx_dat, 100000), total_person_years(120,  usss_m_lx_dat, 0)

  print '(a55,f30.10,f30.10)', "total_person_years(1000, usss_f_qx_dat, 100000): ", total_person_years(1000, usss_f_qx_dat, 100000), total_person_years(1000, usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "total_person_years(1000, usss_m_qx_dat, 100000): ", total_person_years(1000, usss_m_qx_dat, 100000), total_person_years(1000, usss_m_lx_dat, 0)

  print '(a)', repeat("=", 115)

  print '(a55,f30.10,f30.10)', "age_all_dead(-10,  usss_f_qx_dat, 100000): ", age_all_dead(usss_f_qx_dat, 100000), age_all_dead(usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "age_all_dead(-10,  usss_m_qx_dat, 100000): ", age_all_dead(usss_m_qx_dat, 100000), age_all_dead(usss_m_lx_dat, 0)

  print '(a)', repeat("=", 115)

  print '(a55,f30.10,f30.10)', "life_expectancy_at_birth(-10,  usss_f_qx_dat, 100000): ", life_expectancy_at_birth(usss_f_qx_dat, 100000), life_expectancy_at_birth(usss_f_lx_dat, 0)
  print '(a55,f30.10,f30.10)', "life_expectancy_at_birth(-10,  usss_m_qx_dat, 100000): ", life_expectancy_at_birth(usss_m_qx_dat, 100000), life_expectancy_at_birth(usss_m_lx_dat, 0)

  print '(a)', repeat("=", 115)

end program test_life_table

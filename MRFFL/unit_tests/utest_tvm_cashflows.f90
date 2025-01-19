! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      utest_cashflows.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: mrffl_cashflows.@EOL
!! @keywords  finance fortran monte carlo cashflows cashflow time value of money tvm cashflows taxes stock market
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
program utest_cashflows
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_cashflows
  use mrffl_prt_sets
  implicit none
  real(kind=rk)    :: i = 4.0
  real(kind=rk)    :: cfm(6,7), cf(6), fv(6), pv(6)
  integer(kind=ik) :: status

  print "(a)", repeat("=", 100)
  cf = [-80000,-500,4500,5500,4500,130000]
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  print "(a)", repeat("=", 100)
  cf = [ 0, 100, 200, 300, 400, 500]
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  print "(a)", repeat("=", 100)
  cf = 0
  call make_cashflow_vector_delayed_arithmetic_annuity(cf, 100.0_rk, 100.0_rk, 1_ik, 0_ik, status)
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  print "(a)", repeat("=", 100)
  cf = [ 100, 200, 300, 400, 500, 0]
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  print "(a)", repeat("=", 100)
  cf = 0
  call make_cashflow_vector_delayed_arithmetic_annuity(cf, 100.0_rk, 100.0_rk, 0_ik, 1_ik, status)
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  print "(a)", repeat("=", 100)
  cf = 0
  call make_cashflow_vector_delayed_geometric_annuity(cf, 10.0_rk, 100.0_rk, 0_ik, 1_ik, status)
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  print "(a)", repeat("=", 100)
  cf = 0
  call make_cashflow_vector_delayed_geometric_annuity(cf, 10.0_rk, 100.0_rk, 3_ik, 1_ik, status)
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  print "(a)", repeat("=", 100)
  cf = 0
  call make_cashflow_vector_delayed_level_annuity(cf, 100.0_rk, 3_ik, 1_ik, status)
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  print "(a)", repeat("=", 100)
  cf = 0
  call make_cashflow_vector_delayed_lump(cf, 100.0_rk, 3_ik, status)
  call cashflow_vector_value_print(cf, i, pv, fv, status, prt_ALL)

  cfm(:,1) = [-80000,-500,4500,5500,4500,130000]
  call make_cashflow_vector_delayed_arithmetic_annuity(cfm(:,2), 100.0_rk, 100.0_rk, 1_ik, 0_ik, status)
  call make_cashflow_vector_delayed_arithmetic_annuity(cfm(:,3), 100.0_rk, 100.0_rk, 0_ik, 1_ik, status)
  call make_cashflow_vector_delayed_geometric_annuity(cfm(:,4), 10.0_rk, 100.0_rk, 0_ik, 1_ik, status)
  call make_cashflow_vector_delayed_geometric_annuity(cfm(:,5), 10.0_rk, 100.0_rk, 3_ik, 1_ik, status)
  call make_cashflow_vector_delayed_level_annuity(cfm(:,6), 100.0_rk, 3_ik, 1_ik, status)
  call make_cashflow_vector_delayed_lump(cfm(:,7), 100.0_rk, 3_ik, status)
  call cashflow_matrix_value_print(cfm, i, pv, fv, status, prt_ALL)

end program utest_cashflows


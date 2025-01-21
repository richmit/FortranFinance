! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      cashflow_vs_tvm_solver.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-28
!! @brief     Solving a simple uneven cash flow problem.@EOL
!! @keywords  tvm time value money future present cashflow
!! @std       F2023
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
!!
!!  In this example we solve a classic, uneven cashflow TVM problem.  First we use the MRFFL cashflow module much the way the
!!  problem might be solved on a financial calculator.  Then, just for fun, we do it by hand using the MRFFL tvm module.
!!
!!  The TVM way of approaching the problem is more verbose, but it is also a bit more dynamic.  For example if you are receiving
!!  cashflows from an outside source, the TVM approach allows us to incrementally provide PF & FV as we receive more data.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program cashflow_vs_tvm_solver
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_tvm
  use mrffl_var_sets
  use mrffl_prt_sets
  use mrffl_cashflows
  implicit none

  integer(kind=ik), parameter :: years = 5

  real(kind=rk)    :: cfv(years+1) = [-80000,-500,4500,5500,4500,130000]
  real(kind=rk)    :: fvv(years+1),  pvv(years+1)
  real(kind=rk)    :: i = 13.0
  real(kind=rk)    :: s_i, s_pv, s_fv, s_n, s_a, pv_sum, fv_sum = 0
  integer(kind=ik) :: s_d, status

  print "(a)", repeat("=", 100)
  print *, "Here we use the manually constructed cashflow to compute PV/FV"
  call cashflow_vector_pv_fv_print(cfv, i, pvv, fvv, status, prt_ALL)
  print "(a30,i15)", "cashflow_matrix_pv_fv status: ", status
  print *
  print "(a)", repeat("=", 100)

  s_i    = 13
  s_n    = years

  pv_sum = 0
  fv_sum = 0
  print *, "Now we repeat the exercise but use TVM solvers instead of the cashflow module"
  print *
  print "(a6,5(a15),2(a8))", "time", "n", "i", "pv", "fv", "a", "d",  "status"
  do s_d=0,years
     s_a  = cfv(s_d+1)
     s_pv = 0
     s_fv = 0
     call tvm_delayed_lump_sum_solve(s_n, s_i, s_pv, s_fv, s_a, s_d, var_fv+var_pv, status)
     print "(i6,5(f15.4),2(i8))",s_d,  s_n, s_i, s_pv, s_fv, s_a, s_d,  status
     pv_sum = pv_sum + s_pv
     fv_sum = fv_sum + s_fv
  end do
  print *
  print "(a6,f15.4)", "PV", pv_sum
  print "(a6,f15.4)", "FV", fv_sum

end program cashflow_vs_tvm_solver

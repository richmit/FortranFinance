! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      loan_level_payments.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-13
!! @brief     Standard loan problem solved with multiple cashflows.@EOL
!! @keywords  mrffl finance
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
!! @filedetails   
!!
!!  The typical approach to solving loan problems is to equate the present value of the principal with the present value of the
!!  payment cashflow stream in a single equation and solve for the payment.  With only one principal payment occurring at the
!!  beginning and a payment stream consisting of a level annuity certain, this is pretty simple.  Unfortunately things get much
!!  more complex if the payment structure (for principal or repayment) changes from this baseline.  These more difficult
!!  situations can be easily handled by considering the problem as multiple cashflow streams.  In order to illustrate how to
!!  approach such problems using a multiple cashflow methodology, this program applies the technique to solve the familiar
!!  problem of the simple loan.
!!
!!  We can model a loan as a pair of parallel cashflows: 
!!    - Money paid by the lender (i.e. the principal) -- a negative cash flow from the lender's perspective
!!    - Money paid to the lender (i.e. loan payments) -- a positive cash flow from the lender's perspective
!!  These two cash flows should have equal magnitude PV & FV, but of opposite sign.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program loan_level_payments
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_tvm
  use mrffl_var_sets
  use mrffl_prt_sets
  use mrffl_cashflows
  implicit none

  real(kind=rk)    :: n  = 10  
  real(kind=rk)    :: i  = 7

  real(kind=rk)    :: a_pv, a_fv, a_a, p_pv, p_fv 
  integer(kind=ik) :: a_d, a_e, p_d

  integer(kind=ik) :: status

  real(kind=rk)    :: cfm(11,2), fvv(11),  pvv(11)

  ! First, we print out the term and interest.
  print "(a)", repeat("=", 111)
  print "(a60,f15.4)", "n: ", n
  print "(a60,f15.4)", "i: ", i

  ! Now we compute the PV & FV of the principal cashflow.
  print "(a)", repeat("=", 111)
  p_pv = -1000000
  p_d = 0
  call tvm_lump_sum_solve(n, i, p_pv, p_fv, var_fv, status)
  print "(a60,i15)", "tvm_lump_sum_solve status: ", status
  print "(a60,f15.4)", "Loan PV: ", p_pv
  print "(a60,f15.4)", "Loan FV: ", p_fv

  print "(a)", repeat("=", 111)
  ! Now we compute the PV & FV of the repayment cashflow stream.
  a_pv = -p_pv
  a_d = 1
  a_e = 0
  call tvm_delayed_level_annuity_solve(n, i, a_pv, a_fv, a_a, a_d, a_e, var_fv+var_a, status)
  print "(a60,i15)", "tvm_level_annuity_solve status: ", status
  print "(a60,f15.4)", "Annuity PV: ", a_pv
  print "(a60,f15.4)", "Annuity FV: ", a_fv
  print "(a60,f15.4)", "Annuity A: ",  a_a

  ! Just for fun, let's create cashflow sequences for everything, and print it out.

  ! First the principal cashflow.  Note we use the variables we used with the TVM solver.
  print "(a)", repeat("=", 111)
  call make_cashflow_vector_delayed_lump(cfm(:,1), p_pv, p_d, status)
  print "(a60,i15)", "make_cashflow_vector_delayed_lump status: ", status

  ! Next the repayment cashflow.  Note we use the variables we used with the TVM solver.
  print "(a)", repeat("=", 111)
  call make_cashflow_vector_delayed_level_annuity(cfm(:,2), a_a, a_d, a_e, status)
  print "(a60,i15)", "make_cashflow_vector_delayed_level_annuity status: ", status

  ! Now we print it out.  Notice the PV & FV of the combined cashflow series is zero.
  print "(a)", repeat("=", 111)
  call cashflow_matrix_value(cfm, i, pvv, fvv, status)
  print "(a60,i15)", "cashflow_matrix_value status: ", status
  print "(a60,f15.4)", "cashflow_matrix_value Sum: ",   sum(cfm)
  print "(a60,f15.4)", "cashflow_matrix_value PV Sum: ", sum(pvv)
  print "(a60,f15.4)", "cashflow_matrix_value FV Sum: ", sum(fvv)
  print "(a)", repeat("=", 111)

  call cashflow_matrix_value_print(cfm, i, pvv, fvv, status, prt_ALL)
  print "(a)", repeat("=", 111)

end program loan_level_payments

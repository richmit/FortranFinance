! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      loan_up_down_payments.f90
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
!!  This program extends the examples from loan_level_payments.f90 and loan_geometric_payments.f90 to a unequal, non-standard
!!  annuity designed to make payments round cent values.
!!
!!  One way to amortize a loan is to round all payments but the last one up to the nearest penny, and then adjust the last
!!  payment lower to accommodate the difference -- this last payment is rounded DOWN to the nearest cent.  This insures that the
!!  lender will not loose more than a fractional cent on the entire transaction, and the borrower don't pay more than the agreed
!!  upon rate (this second condition is required by law in many jurisdictions).  Note this method is unsuitable for very long
!!  term loans as it may shorten the overall term.
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

  integer, parameter :: years = 10

  real(kind=rk)    :: n  = years
  real(kind=rk)    :: i  = 7

  real(kind=rk)    :: a_pv = 1000000
  real(kind=rk)    :: a_fv
  real(kind=rk)    :: a_a
  integer(kind=ik) :: a_d = 1
  integer(kind=ik) :: a_e = 0

  real(kind=rk)    :: p_pv = -1000000
  real(kind=rk)    :: p_fv
  integer(kind=ik) :: p_d = 0

  real(kind=rk)    :: a_final

  integer(kind=ik) :: status

  real(kind=rk)    :: cfm(years+1,2), fvv(years+1),  pvv(years+1)

  print "(a)", repeat("=", 111)
  print "(a60,f15.4)", "n: ", n
  print "(a60,f15.4)", "i: ", i

  ! First we find the PV & FV for the principal.
  print "(a)", repeat("=", 111)
  call tvm_lump_sum_solve(n, i, p_pv, p_fv, var_fv, status)
  print "(a60,i15)", "tvm_lump_sum_solve status: ", status
  print "(a60,f15.4)", "Loan PV: ", p_pv
  print "(a60,f15.4)", "Loan FV: ", p_fv

  ! Now we solve for the payment (a in the annuity) and the fv
  print "(a)", repeat("=", 111)
  call tvm_delayed_level_annuity_solve(n, i, a_pv, a_fv, a_a, a_d, a_e, var_fv+var_a, status)
  print "(a60,i15)", "tvm_level_annuity_solve status: ", status
  print "(a60,f15.4)", "Annuity PV: ", a_pv
  print "(a60,f15.4)", "Annuity FV: ", a_fv
  print "(a60,f15.4)", "Annuity A: ",  a_a

  ! Now we round a_a UP to the nearest cent.
  print "(a)", repeat("=", 111)
  a_a = ceiling(100*a_a)
  a_a = a_a / 100
  print "(a60,f15.4)", "Rounded Up Annuity A: ",  a_a

  ! Now we find PV & FV for an annuity with the rounded payment that ends one period early
  print "(a)", repeat("=", 111)
  call tvm_delayed_level_annuity_solve(n, i, a_pv, a_fv, a_a, a_d, a_e+1_ik, var_fv+var_pv, status)
  print "(a60,i15)", "tvm_level_annuity_solve status: ", status
  print "(a60,f15.4)", "Rounded (n-1) Annuity PV: ", a_pv
  print "(a60,f15.4)", "Rounded (n-1) Annuity FV: ", a_fv

  ! The final payment needs to be the difference between the principal FV and the rounded annuity FV
  print "(a)", repeat("=", 111)
  a_final = floor(-(p_fv+a_fv)*100)
  a_final = a_final / 100
  print "(a60,f15.4)", "Final Payment: ",  a_final

  ! Now we construct the cashflows so we can print a nice table

  ! We start with the principal
  print "(a)", repeat("=", 111)
  call make_cashflow_vector_delayed_lump(cfm(:,1), p_pv, p_d, status)
  print "(a60,i15)", "make_cashflow_vector_delayed_lump status: ", status

  ! Next we add the rounded up
  print "(a)", repeat("=", 111)
  call make_cashflow_vector_delayed_level_annuity(cfm(:,2), a_a, a_d, a_e+1_ik, status)
  print "(a60,i15)", "make_cashflow_vector_delayed_level_annuity status: ", status

  ! Finally we add the last payment
  cfm(years+1, 2) = a_final

  ! Now we print the cashflow
  call cashflow_matrix_pv_fv_print(cfm, i, pvv, fvv, status, prt_ALL)
  print "(a)", repeat("=", 111)

end program loan_level_payments

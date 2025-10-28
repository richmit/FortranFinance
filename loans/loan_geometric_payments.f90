! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      loan_geometric_payments.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-13
!! @brief     Geometric load payments with multiple cashflows.@EOL
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
!!  This program extends the example from loan_level_payments.f90 to geometric payments.  Not much changes in the flow except the
!!  annuity type.
!!
!!  If you are curious about how such a loan might come about, then consider the following scenario:
!!
!!  A business needs a 1M load.  They wish to make annual payments, and to pay down the loan as quickly as possible.  At the end
!!  of the year they can afford to pay 95K.  The business has been experiencing 11% revenue growth for the last 5 years with
!!  projections showing that to continue.  Based on growth projections, they wish to increase loan payments by 10% per year.  We
!!  wish to extend them the loan, and make 7%.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program loan_geometric_payments
  use :: mrffl_config, only: rk
  use :: mrffl_cashflows, only: make_cashflow_vector_delayed_lump, make_cashflow_vector_delayed_geometric_annuity, cashflow_matrix_pv_fv
  use :: mrffl_prt_sets,  only: prt_ALL
  use :: mrffl_tvm,       only: tvm_delayed_geometric_annuity_solve, tvm_lump_sum_solve
  use :: mrffl_var_sets,  only: var_fv, var_a, var_n

  implicit none (type, external)

  real(kind=rk)    :: n
  real(kind=rk)    :: i  = 7

  real(kind=rk)    :: a_pv = 1000000
  real(kind=rk)    :: a_fv
  real(kind=rk)    :: a_g = 10
  real(kind=rk)    :: a_a = 95000
  integer          :: a_d = 1
  integer          :: a_e = 0

  real(kind=rk)    :: p_pv = -1000000
  real(kind=rk)    :: p_fv

  integer          :: status

  real(kind=rk), allocatable    :: cfm(:,:), fvv(:),  pvv(:)

  ! First we solve for the number of years and future value of our loan.
  print "(a)", repeat("=", 111)
  call tvm_delayed_geometric_annuity_solve(n, i, a_g, a_pv, a_fv, a_a, a_d, a_e, var_n+var_fv, status)
  print "(a60,i15)", "tvm_level_annuity_solve status: ", status
  print "(a60,f15.4)", "Annuity n: ", n
  print "(a60,f15.4)", "Annuity FV: ", a_fv
  print "(a)", "From this result we know the loan term needs to be just about 10 years."

  ! Instead of using an odd term, we decide on an even 10 year term.
  ! So we must copute the initial loan payment, and loan FV
  print "(a)", repeat("=", 111)
  n = ceiling(n)
  call tvm_delayed_geometric_annuity_solve(n, i, a_g, a_pv, a_fv, a_a, a_d, a_e, var_a+var_fv, status)
  print "(a60,i15)", "tvm_level_annuity_solve status: ", status
  print "(a60,f15.4)", "Annuity a: ", a_a
  print "(a60,f15.4)", "Annuity FV: ", a_fv

  ! Next we check our work by solving for the FV of the lump sum.
  print "(a)", repeat("=", 111)
  call tvm_lump_sum_solve(n, i, p_pv, p_fv, var_fv, status)
  print "(a60,i15)", "tvm_lump_sum_solve status: ", status
  print "(a60,f15.4)", "Loan FV: ", p_fv

  ! Allocate space for our cashflow matrix and the PV/FV vectors.  We don't check for allocation errors. ;)
  allocate(cfm(nint(n)+1,2))
  allocate(fvv(nint(n)+1))
  allocate(pvv(nint(n)+1))

  ! Now we populate a cashflow matrix with our two cashflows.
  print "(a)", repeat("=", 111)
  call make_cashflow_vector_delayed_lump(cfm(:,1), p_pv, 0, status)
  print "(a60,i15)", "make_cashflow_vector_delayed_lump status: ", status
  call make_cashflow_vector_delayed_geometric_annuity(cfm(:,2), a_g, a_a, a_d, a_e, status)
  print "(a60,i15)", "make_cashflow_vector_delayed_level_annuity status: ", status

  ! We can check our work by making sure our cashflows sum to zero.
  print "(a)", repeat("=", 111)
  call cashflow_matrix_pv_fv(cfm, i, pvv, fvv, status)
  print "(a60,i15)", "cashflow_matrix_pv_fv status: ", status
  print "(a60,f15.4)", "cashflow_matrix_pv_fv PV Sum: ", sum(pvv)
  print "(a60,f15.4)", "cashflow_matrix_pv_fv FV Sum: ", sum(fvv)

  ! Finally we an print out our cashflows.
  print "(a)", repeat("=", 111)
  call cashflow_matrix_pv_fv(cfm, i, pvv, fvv, status, prt_o=prt_ALL)

  print "(a)", repeat("=", 111)

end program loan_geometric_payments

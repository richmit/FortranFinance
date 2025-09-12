! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      retire_cashflow.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-16
!! @brief     Retirement scenario approach with MRFFL's cashflow module.@EOL
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
!!  In this program we consider a retirement scenario using MRFFL's cashflow module.
!!
!!  Retirement scenario:
!!    - Savings: 2M
!!    - Pension: Starts in 5 years at 24k per year and then grows with inflation
!!    - Annual expenses: 90k including taxes
!!    - Total life expectancy: 25 years
!!    - Inflation: 3%
!!    - Investment growth rate: 3%
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program cashflow_retire
  use mrffl_config,    only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_cashflows, only: make_cashflow_vector_delayed_lump, make_cashflow_vector_delayed_level_annuity, &
       &                     make_cashflow_vector_delayed_geometric_annuity, cashflow_matrix_pv_fv_print
  use mrffl_prt_sets,  only: prt_ALL

  implicit none (type, external)

  integer, parameter :: years = 25
  real(kind=rk)      :: cfi = 4.0
  real(kind=rk)      :: cfm(years+1,3),  fv(years+1),  pv(years+1)
  integer(kind=ik)   :: status

  ! First we add a cashflow for our savings
  print "(a)", repeat("=", 126)
  call make_cashflow_vector_delayed_lump(cfm(:,1), 2.0e6_rk, 0_ik, status)
  print "(a30,i15)", "make_cashflow_vector_delayed_lump status: ", status

  ! Now we add cashflows for the pension (growing with inflation)
  call make_cashflow_vector_delayed_level_annuity(cfm(:,2), 2.4e4_rk, 5_ik, 0_ik, status)
  print "(a30,i15)", "make_cashflow_vector_delayed_level_annuity status: ", status

  ! Last we add cashflows for our expenses (growing with inflation)
  call make_cashflow_vector_delayed_geometric_annuity(cfm(:,3), 3.0_rk, -9e4_rk, 1_ik, 0_ik, status)
  print "(a30,i15)", "make_cashflow_vector_delayed_level_annuity status: ", status

  ! Finally we print it all out.
  print "(a)", repeat("=", 126)
  call cashflow_matrix_pv_fv_print(cfm, cfi, pv, fv, status, prt_ALL)
  print "(a30,i15)", "cashflow_matrix_pv_fv status: ", status
  print "(a)", repeat("=", 126)

end program cashflow_retire

! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      roth_conversion.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-16
!! @brief     Value of a roth conversion.@EOL
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
!!  Scenario:
!!    - The time frame from this experiment is 10 years
!!    - We pay our expenses and regular taxes with income
!!    - Our tax rate will be low for the first 5 years, and higher for the next 5.
!!    - We convert part of an ira into a roth ira at time zero.
!!      - This conversion bumps up taxes that year to the higher rate.
!!      - We pay the conversion taxes with the brokerage account
!!    - We withdraw everything at the end of 10 years
!!      - We use the higher tax rate for this single withdrawal from the ira.
!!        If the numbers were larger, then we would need to consider a higher rate or a phased withdrawal strategy.
!!    - We assume we pay capital gains every year on gains brokerage account.  This is an overestimate of tax.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
program roth_conversion
  use :: mrffl_config,      only: rk
  use :: mrffl_percentages, only: percentage_of, add_percentage, percentage_to_fraction
  use :: mrffl_cashflows,   only: make_cashflow_vector_delayed_lump, add_intrest_to_cashflow_vector, &
       &                          add_multi_intrest_to_cashflow_vector, cashflow_matrix_pv_fv
  use :: mrffl_prt_sets,    only: prt_ALL
  !
  implicit none (type, external)
  ! Arguments
  integer, parameter :: years            = 10
  integer, parameter :: conversion_years = 5
  ! Local Variables
  real(kind=rk) :: apr                = 4.0_rk
  real(kind=rk) :: retirement_tax     = 17.0_rk
  real(kind=rk) :: conversion_tax     = 15.0_rk
  real(kind=rk) :: conv_size          = 50000.0_rk
  real(kind=rk) :: initial_ira        = 200000.0_rk
  real(kind=rk) :: initial_brokerage  = 100000.0_rk
  real(kind=rk) :: rate               = 0.0_rk
  real(kind=rk) :: cf(years+1,3),  fv(years+1),  pv(years+1), vapr(years)
  integer       :: status
  real(kind=rk) :: ncv, wcv

  print "(a)", repeat("=", 126)
  cf = 0

  call make_cashflow_vector_delayed_lump(cf(:,1), initial_ira, 0, status)
  call add_intrest_to_cashflow_vector(cf(:,1), apr, status)
  cf(size(cf,1),1) = cf(size(cf,1),1) - percentage_of(sum(cf(:,1)), retirement_tax)

  cf(:,2) = 0

  call make_cashflow_vector_delayed_lump(cf(:,3), initial_brokerage, 0, status)
  vapr(1:conversion_years)  = add_percentage(apr, -conversion_tax)
  vapr(conversion_years+1:) = add_percentage(apr, -retirement_tax)
  call add_multi_intrest_to_cashflow_vector(cf(:,3), vapr, status)

  call cashflow_matrix_pv_fv(cf, rate, pv, fv, status, prt_o=prt_ALL)

  ncv = sum(fv)

  print "(a)", repeat("=", 126)
  cf = 0

  call make_cashflow_vector_delayed_lump(cf(:,1), initial_ira-conv_size, 0, status)
  call add_intrest_to_cashflow_vector(cf(:,1), apr, status)
  cf(size(cf,1),1) = cf(size(cf,1),1) - percentage_of(sum(cf(:,1)), retirement_tax)

  call make_cashflow_vector_delayed_lump(cf(:,2), conv_size, 0, status)
  call add_intrest_to_cashflow_vector(cf(:,2), apr, status)

  call make_cashflow_vector_delayed_lump(cf(:,3), initial_brokerage, 0, status)
  vapr(1:conversion_years)  = add_percentage(apr, -conversion_tax)
  vapr(conversion_years+1:) = add_percentage(apr, -retirement_tax)
  call add_multi_intrest_to_cashflow_vector(cf(:,3), vapr, status)

  call cashflow_matrix_pv_fv(cf, rate, pv, fv, status, prt_o=prt_ALL)

  wcv = sum(fv)
  print "(a)", repeat("=", 126)

  print *
  print "(a25,f20.2)", "No conversion:", ncv
  print "(a25,f20.2)", "Conversion:",    wcv
  print "(a25,f20.2)", "Delta:",         wcv-ncv
  print *
  print "(a)", repeat("=", 126)

end program roth_conversion

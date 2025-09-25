! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      irr.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-19
!! @brief     Demonstrate how to compute irr.@EOL
!! @keywords  tvm time value money future present cashflow
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
program irr
  use :: mrffl_config,    only: rk
  use :: mrffl_cashflows, only: cashflow_vector_total_pv, cashflow_vector_irr, cashflow_matrix_total_pv, cashflow_matrix_irr, &
       &                        cashflow_vector_pv_fv_print, cashflow_matrix_pv_fv_print
  use :: mrffl_prt_sets,  only: prt_ALL

  implicit none (type, external)

  integer,          parameter :: years = 3
  real(kind=rk)               :: cfv(years+1) = [-125, 50, 60, 70]
  real(kind=rk)               :: cfm(years+1, 2) = reshape([-125, 0, 0, 0, 0, 50, 60, 70], [years+1, 2])
  real(kind=rk)               :: fvv(years+1),  pvv(years+1)
  real(kind=rk)               :: i
  integer                     :: status

  print "(a)", repeat("=", 111)
  i = 4
  call cashflow_vector_pv_fv_print(cfv, i, pvv, fvv, status, prt_ALL)
  print "(a30,i15)", "cashflow_matrix_pv_fv status: ", status
  print *
  print '(a10,f20.5)', "total pv:", cashflow_vector_total_pv(cfv, i)
  call cashflow_vector_irr(cfv, i, status)
  print '(a10,f20.5,i10)', "irr:", i, status
  print "(a)", repeat("=", 111)

  print "(a)", repeat("=", 111)
  i = 4
  call cashflow_matrix_pv_fv_print(cfm, i, pvv, fvv, status, prt_ALL)
  print "(a30,i15)", "cashflow_matrix_pv_fv status: ", status
  print *
  print '(a10,f20.5)', "total pv:", cashflow_matrix_total_pv(cfm, i)
  call cashflow_matrix_irr(cfm, i, status)
  print '(a10,f20.5,i10)', "irr:", i, status
  print "(a)", repeat("=", 111)

end program irr

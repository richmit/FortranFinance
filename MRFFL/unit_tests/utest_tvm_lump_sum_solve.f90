! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      utest_tvm_lump_sum_solve.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: utest_tvm_lump_sum_solve from mrffl_tvm.@EOL
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
program utest_tvm_lump_sum_solve
  use :: mrffl_config,   only: rk
  use :: mrffl_tvm,      only: tvm_lump_sum_solve
  use :: mrffl_var_sets, only: var_pv, var_fv, var_i, var_n

  implicit none (type, external)

  real(kind=rk)    :: i, fv, pv, n
  integer          :: status

  print "(a)", repeat("=", 111)
  print "(a3,a7,4(a20),a7)", "TM", "stat", "n", "i", "pv", "fv", "var"

  call setem()
  print "(a3,a7,4(f20.4),i7)", "", "", n, i, pv, fv, 0

  print "(a)", repeat("=", 111)
  call setem()
  pv = -1
  print "(a3,i7,4(f20.4),i7)", "BF", status, n, i, pv, fv, var_pv
  call tvm_lump_sum_solve(n, i, pv, fv, var_pv, status)
  print "(a3,i7,4(f20.4),i7)", "AF", status, n, i, pv, fv, var_pv

  print "(a)", repeat("=", 111)
  call setem()
  fv = -1
  print "(a3,i7,4(f20.4),i7)", "BF", status, n, i, pv, fv, var_fv
  call tvm_lump_sum_solve(n, i, pv, fv, var_fv, status)
  print "(a3,i7,4(f20.4),i7)", "AF", status, n, i, pv, fv, var_fv

  print "(a)", repeat("=", 111)
  call setem()
  i = -1
  print "(a3,i7,4(f20.4),i7)", "BF", status, n, i, pv, fv, var_i
  call tvm_lump_sum_solve(n, i, pv, fv, var_i, status)
  print "(a3,i7,4(f20.4),i7)", "AF", status, n, i, pv, fv, var_i

  print "(a)", repeat("=", 111)
  call setem()
  n = -1
  print "(a3,i7,4(f20.4),i7)", "BF", status, n, i, pv, fv, var_n
  call tvm_lump_sum_solve(n, i, pv, fv, var_n, status)
  print "(a3,i7,4(f20.4),i7)", "AF", status, n, i, pv, fv, var_n
  print "(a)", repeat("=", 111)

  contains
    subroutine setem()
      i = 10
      fv = 1331
      pv = 1000
      n = 3
      status = -1
  end subroutine setem
end program utest_tvm_lump_sum_solve

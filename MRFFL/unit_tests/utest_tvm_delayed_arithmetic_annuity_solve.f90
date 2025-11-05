! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      utest_tvm_delayed_arithmetic_annuity_solve.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: utest_tvm_delayed_arithmetic_annuity_solve from mrffl_tvm.@EOL
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
program utest_tvm_delayed_arithmetic_annuity_solve
  use :: mrffl_config,    only: rk
  use :: mrffl_tvm,       only: tvm_delayed_arithmetic_annuity_solve
  use :: mrffl_var_sets,  only: var_pv, var_fv, var_i, var_n, var_a, var_NONE, var_q
  use :: mrffl_cashflows, only: cashflow_matrix_cmp
  implicit none (type, external)

  real(kind=rk)    :: n, i, q, pv, fv, a
  integer          :: d, e, status
  integer          :: k

  print "(a)", repeat("=", 119)
  print "(a3,a5,6(a15),3(a7))", "BF", "stat", "n", "i", "q", "pv", "fv", "a", "d", "e", "var"

  print "(a)", repeat("=", 119)
  call setem(1)
  a = -1
  fv = -1
  pv = -1
  print "(a3,i5,6(f15.4),3(i7),a10)", "BF", status, n, i, q, pv, fv, a, d, e, var_a+var_fv+var_pv, "too many"
  call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_a+var_fv+var_pv, status)
  print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_a+var_fv+var_pv

  print "(a)", repeat("=", 119)
  call setem(1)
  print "(a3,i5,6(f15.4),3(i7),a10)", "BF", status, n, i, q, pv, fv, a, d, e, var_NONE, "none"
  call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_NONE, status)
  print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_NONE

  print "(a)", repeat("=", 119)

  !! All possible combos
  !! n n+i n+q n+pv n+fv n+a
  !! i i+q i+pv i+fv i+a
  !! q q+pv q+fv q+a
  !! pv pv+fv pv+a
  !! fv fv+a
  !! a
  !!

  do k=1,2

     print "(a)", repeat("=", 119)
     call setem(k)
     n = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_n
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_n, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_n

     print "(a)", repeat("=", 119)
     call setem(k)
     n = -1
     i = -1
     print "(a3,i5,6(f15.4),3(i7),a10)", "BF", status, n, i, q, pv, fv, a, d, e, var_n+var_i, "bad: n+i"
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_n+var_i, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_n+var_i

     print "(a)", repeat("=", 119)
     call setem(k)
     n = -1
     q = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_n+var_q
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_n+var_q, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_n+var_q

     print "(a)", repeat("=", 119)
     call setem(k)
     n = -1
     pv = -1
     print "(a3,i5,6(f15.4),3(i7),a10)", "BF", status, n, i, q, pv, fv, a, d, e, var_n+var_pv, "bad: n+pv"
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_n+var_pv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_n+var_pv

     print "(a)", repeat("=", 119)
     call setem(k)
     n = -1
     fv = -1
     print "(a3,i5,6(f15.4),3(i7),a10)", "BF", status, n, i, q, pv, fv, a, d, e, var_n+var_fv, "bad: n+fv"
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_n+var_fv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_n+var_fv

     print "(a)", repeat("=", 119)
     call setem(k)
     n = -1
     a = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_n+var_a
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_n+var_a, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_n+var_a

     print "(a)", repeat("=", 119)
     call setem(k)
     i = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_i
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_i, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_i

     print "(a)", repeat("=", 119)
     call setem(k)
     i = -1
     q = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_i+var_q
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_i+var_q, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_i+var_q

     print "(a)", repeat("=", 119)
     call setem(k)
     i = -1
     pv = -1
     print "(a3,i5,6(f15.4),3(i7), a10)", "BF", status, n, i, q, pv, fv, a, d, e, var_i+var_pv, "bad: i+pv"
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_i+var_pv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_i+var_pv

     print "(a)", repeat("=", 119)
     call setem(k)
     i = -1
     fv = -1
     print "(a3,i5,6(f15.4),3(i7),a10)", "BF", status, n, i, q, pv, fv, a, d, e, var_i+var_fv, "bad: i+fv"
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_i+var_fv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_i+var_fv

     print "(a)", repeat("=", 119)
     call setem(k)
     i = -1
     a = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_i+var_a
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_i+var_a, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_i+var_a

     print "(a)", repeat("=", 119)
     call setem(k)
     q = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_q
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_q, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_q

     print "(a)", repeat("=", 119)
     call setem(k)
     q = -1
     pv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_q+var_pv
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_q+var_pv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_q+var_pv

     print "(a)", repeat("=", 119)
     call setem(k)
     q = -1
     fv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_q+var_fv
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_q+var_fv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_q+var_fv

     print "(a)", repeat("=", 119)
     call setem(k)
     q = -1
     a = -1
     print "(a3,i5,6(f15.4),3(i7),a10)", "BF", status, n, i, q, pv, fv, a, d, e, var_q+var_a, "bad: q+a"
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_q+var_a, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_q+var_a

     print "(a)", repeat("=", 119)
     call setem(k)
     pv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_pv
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_pv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_pv

     print "(a)", repeat("=", 119)
     call setem(k)
     pv = -1
     fv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_pv+var_fv
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_pv+var_fv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_pv+var_fv

     print "(a)", repeat("=", 119)
     call setem(k)
     pv = -1
     a = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_pv+var_a
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_pv+var_a, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_pv+var_a

     print "(a)", repeat("=", 119)
     call setem(k)
     fv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_fv
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_fv, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_fv

     print "(a)", repeat("=", 119)
     call setem(k)
     pv = -1
     a = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_fv+var_a
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_fv+var_a, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_fv+var_a

     print "(a)", repeat("=", 119)
     call setem(k)
     a = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, q, pv, fv, a, d, e, var_a
     call tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, var_a, status)  !
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, q, pv, fv, a, d, e, var_a

  end do

  print "(a)", repeat("=", 119)
contains
  subroutine setem(k)
    integer, intent(in) :: k
    if      (k==1) then
       ! Case: d=1  e=0, i>0
       n        =   9
       i        =   10
       q        =   100
       pv       =   7701.1690671979450_rk
       fv       =   18158.953820000024_rk
       a        =   1000
       d        =   1
       e        =   0
       status   =  -1
    else if (k==2) then
       ! Case: d=0  e=1, i>0
       n        =   9
       i        =   10
       q        =   100
       pv       =   8471.2859739177366_rk
       fv       =   19974.849202000038_rk
       a        =   1000
       d        =   0
       e        =   1
       status   =  -1
    end if
    ! MJR TODO NOTE <2024-12-18T14:19:02-0600> setem: Add more d/e cases.
    ! MJR TODO NOTE <2024-12-18T14:19:02-0600> setem: Add i<0 cases
    ! MJR TODO NOTE <2024-12-18T14:19:02-0600> setem: Add q<0 cases
  end subroutine setem
end program utest_tvm_delayed_arithmetic_annuity_solve

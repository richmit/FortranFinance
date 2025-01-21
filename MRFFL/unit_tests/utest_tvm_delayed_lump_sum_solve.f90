! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      utest_tvm_delayed_lump_sum_solve.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: utest_tvm_delayed_lump_sum_solve from mrffl_tvm.@EOL
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
program utest_tvm_delayed_lump_sum_solve
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_tvm
  use mrffl_var_sets

  real(kind=rk)    :: i, fv, pv, a, n
  integer(kind=ik) :: d, status
  integer          :: k

  print "(a)", repeat("=", 121)
  print "(a3,a7,5(a20),a4,a7)", "BF", "stat", "n", "i", "pv", "fv", "a", "d", "var"

  print "(a)", repeat("=", 121)
  call setem(1)
  status = -1
  a = -1
  pv = -1
  fv = -1
  print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_a+var_fv+var_pv
  call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_a+var_fv+var_pv, status)
  print "(a3,i7,5(f20.4),i4,i7,a)", "AF", status, n, i, pv, fv, a, d, var_a+var_fv+var_pv, " ERROR tmv"

  print "(a)", repeat("=", 121)
  call setem(1)
  status = -1
  print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_g
  call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_g, status)
  print "(a3,i7,5(f20.4),i4,i7,a)", "AF", status, n, i, pv, fv, a, d, var_g, " ERROR unk"

  !! Var Combos:
  !! NONE
  !! n, n+i, n+pv, n+fv, n+a
  !! i, i+pv, i+fv, i+a
  !! pv, pv+fv, pv+a
  !! fv, fv+a
  !! a

  do k=1,4
     print "(a)", repeat("=", 121)
     call setem(k)
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_NONE
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_NONE, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_NONE

     print "(a)", repeat("=", 121)
     call setem(k)
     n = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_n
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_n, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_n

     print "(a)", repeat("=", 121)
     call setem(k)
     n = -1
     i = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_n+var_i
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_n+var_i, status)
     print "(a3,i7,5(f20.4),i4,i7,a)", "AF", status, n, i, pv, fv, a, d, var_n+var_i, " ERROR d=0"

     print "(a)", repeat("=", 121)
     call setem(k)
     n = -1
     pv = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_n+var_pv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_n+var_pv, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_n+var_pv

     print "(a)", repeat("=", 121)
     call setem(k)
     n = -1
     fv = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_n+var_fv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_n+var_fv, status)
     print "(a3,i7,5(f20.4),i4,i7,a)", "AF", status, n, i, pv, fv, a, d, var_n+var_fv, " bad:n+fv"

     print "(a)", repeat("=", 121)
     call setem(k)
     n = -1
     a = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_a+var_n
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_a+var_n, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_a+var_n

     print "(a)", repeat("=", 121)
     call setem(k)
     i = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_i
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_i, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_i

     print "(a)", repeat("=", 121)
     call setem(k)
     i = -1
     pv = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_i+var_pv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_i+var_pv, status)
     print "(a3,i7,5(f20.4),i4,i7,a)", "AF", status, n, i, pv, fv, a, d, var_i+var_pv, " ERROR d=n"

     print "(a)", repeat("=", 121)
     call setem(k)
     i = -1
     fv = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_i+var_fv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_i+var_fv, status)
     print "(a3,i7,5(f20.4),i4,i7,a)", "AF", status, n, i, pv, fv, a, d, var_i+var_fv, " ERROR d=0"

     print "(a)", repeat("=", 121)
     call setem(k)
     i = -1
     a = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_a+var_i
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_a+var_i, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_a+var_i

     print "(a)", repeat("=", 121)
     call setem(k)
     pv = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_pv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_pv, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_pv

     print "(a)", repeat("=", 121)
     call setem(k)
     pv = -1
     a = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_a+var_pv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_a+var_pv, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_a+var_pv

     print "(a)", repeat("=", 121)
     call setem(k)
     pv = -1
     fv = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_fv+var_pv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_fv+var_pv, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_fv+var_pv

     print "(a)", repeat("=", 121)
     call setem(k)
     fv = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_fv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_fv, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_fv

     print "(a)", repeat("=", 121)
     call setem(k)
     fv = -1
     a = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_a+var_fv
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_a+var_fv, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_a+var_fv

     print "(a)", repeat("=", 121)
     call setem(k)
     a = -1
     print "(a3,i7,5(f20.4),i4,i7)", "BF", status, n, i, pv, fv, a, d, var_a
     call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, var_a, status)
     print "(a3,i7,5(f20.4),i4,i7)", "AF", status, n, i, pv, fv, a, d, var_a
  end do
  print "(a)", repeat("=", 121)
contains
  subroutine setem(k)
    integer, intent(in) :: k
    if (k==1) then
       ! Case: d=0, i>0
       i = 10
       pv = 1100.0000000000000
       fv = 1610.5100000000004
       a  = 1100
       n = 4
       d = 0
       status = -1
    else if (k==2) then
       ! Case: d=1, i>0
       i = 10
       pv = 1000.0000000000000
       fv = 1464.1000000000004
       a = 1100
       n = 4
       d = 1
       status = -1
    else if (k==3) then
       ! Case: 1<d<n-1, i>0
       i = 10
       pv = 909.09090909090901
       fv = 1331.0000000000002
       a = 1100
       n = 4
       d = 2
       status = -1
    else if (k==4) then
       ! Case: d=n-1, i>0
       i = 10
       pv = 826.44628099173531
       fv = 1210.0000000000000
       a = 1100
       n = 4
       d = 3
       status = -1
    else if (k==5) then
       ! Case: d=n, i>0
       i = 10
       pv = 751.31480090157754
       fv = 1100.0000000000000
       a = 1100
       n = 4
       d = 4
       status = -1
    end if
  end subroutine setem
end program utest_tvm_delayed_lump_sum_solve

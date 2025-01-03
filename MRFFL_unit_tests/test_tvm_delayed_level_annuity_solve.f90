! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      test_tvm_delayed_level_annuity_solve.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: test_tvm_delayed_level_annuity_solve from mrffl_tvm.@EOL
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
program test_tvm_delayed_level_annuity_solve
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_tvm
  use mrffl_var_sets
  implicit none

  real(kind=rk)    :: n, i, pv, fv, a
  integer(kind=ik) :: d, e
  integer(kind=ik) :: status
  integer          :: k

  print "(a)", repeat("=", 98)
  print "(a3,a5,5(a15),3(a5))", "BF", "stat", "n", "i", "pv", "fv", "a", "d", "e", "var"

  ! MJR TODO NOTE: Add cases for variable errors (too many or unknown)
  do k=1,7
     print "(a)", repeat("=", 98)
     call setem(k)
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_NONE
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_NONE, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_NONE

     print "(a)", repeat("=", 98)
     call setem(k)
     a = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_a
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_a, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_a

     print "(a)", repeat("=", 98)
     call setem(k)
     a = -1
     i = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_a+var_i
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_a+var_i, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_a+var_i

     print "(a)", repeat("=", 98)
     call setem(k)
     a = -1
     pv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_a+var_pv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_a+var_pv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_a+var_pv

     print "(a)", repeat("=", 98)
     call setem(k)
     a = -1
     fv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_a+var_fv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_a+var_fv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_a+var_fv

     print "(a)", repeat("=", 98)
     call setem(k)
     a = -1
     n = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_a+var_n
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_a+var_n, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_a+var_n

     print "(a)", repeat("=", 98)
     call setem(k)
     n = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_n
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_n, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_n

     print "(a)", repeat("=", 98)
     call setem(k)
     n = -1
     i = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_n+var_i
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_n+var_i, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_n+var_i
     ! Need i<-100%, and -100%<i<0 cases for above

     print "(a)", repeat("=", 98)
     call setem(k)
     n = -1
     pv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_n+var_pv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_n+var_pv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_n+var_pv

     print "(a)", repeat("=", 98)
     call setem(k)
     n = -1
     fv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_n+var_fv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_n+var_fv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_n+var_fv

     print "(a)", repeat("=", 98)
     call setem(k)
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_i
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_i, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_i

     print "(a)", repeat("=", 98)
     call setem(k)
     i = -1
     fv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_i+var_fv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_i+var_fv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_i+var_fv

     print "(a)", repeat("=", 98)
     call setem(k)
     i = -1
     pv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_i+var_pv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_i+var_pv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_i+var_pv

     print "(a)", repeat("=", 98)
     call setem(k)
     fv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_fv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_fv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_fv

     print "(a)", repeat("=", 98)
     call setem(k)
     pv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_pv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_pv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_pv

     print "(a)", repeat("=", 98)
     call setem(k)
     fv = -1
     pv = -1
     print "(a3,i5,5(f15.4),3(i5))", "BF", status, n, i, pv, fv, a, d, e, var_fv+var_pv
     call tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, var_fv+var_pv, status)
     print "(a3,i5,5(f15.4),3(i5))", "AF", status, n, i, pv, fv, a, d, e, var_fv+var_pv
  end do
  print "(a)", repeat("=", 98)
contains
  subroutine setem(k)
    integer, intent(in) :: k
    if (k==1) then
       ! d=0, e>1, i>0
       n        =  7
       i        =  10
       pv       =  1909.0909090909101        
       fv       =  3720.2781000000027 
       a        =  1000
       d        =  0
       e        =  6
       status   =  -1
    else if (k==2) then
       ! d=1, e>1, i>0
       n        =  7
       i        =  10
       pv       =  1735.5371900826456        
       fv       =  3382.0710000000063     
       a        =  1000
       d        =  1
       e        =  5
       status   =  -1
    else if (k==3) then
       ! d>1, e>1, i>0
       n        =  7
       i        =  10
       pv       =  1434.3282562666486
       fv       =  2795.1000000000013     
       a        =  1000
       d        =  3
       e        =  3
       status   =  -1
    else if (k==4) then
       ! d>1, e=0, i>0
       n        =  7
       i        =  10
       pv       =  1077.6320482844853        
       fv       =  2100.0000000000018     
       a        =  1000
       d        =  6
       e        =  0
       status   =  -1
    else if (k==5) then
       ! d>1, e=1, i>0
       n        =  7
       i        =  10
       pv       =  1185.3952531129339        
       fv       =  2310.0000000000032     
       a        =  1000
       d        =  5
       e        =  1
       status   =  -1
    else if (k==6) then
       ! d>1, e=1, -100<i<0
       n        =  7
       i        =  -10
       pv       =  3575.1852040019489        
       fv       =  1709.9999999999993     
       a        =  1000
       d        =  5
       e        =  1
       status   =  -1
    else if (k==7) then
       ! d>1, e>1, -100<i<0
       n        =  7
       i        =  -10
       pv       =  2895.9
       fv       =  1385.1
       a        =  1000
       d        =  3
       e        =  3
       status   =  -1
    end if

    ! MJR TODO NOTE <2024-12-17T23:31:30-0600> setem: Add more -100<i<0 cases
    ! MJR TODO NOTE <2024-12-17T23:31:40-0600> setem: Add i<-100 cases and fix solver to handle them if possible
    ! MJR TODO NOTE <2024-12-17T23:31:57-0600> setem: Add d=0 & e=0 case

    ! ! d>1, e>1, i<-100%
    ! n        =  7
    ! i        =  -110
    ! pv       =  9000000      
    ! fv       =  -0.9
    ! a        =  1000
    ! d        =  3
    ! e        =  3
    ! status   =  -1
  end subroutine setem
end program test_tvm_delayed_level_annuity_solve





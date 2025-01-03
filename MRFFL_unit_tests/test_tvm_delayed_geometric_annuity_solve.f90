! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      test_tvm_delayed_geometric_annuity_solve.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: test_tvm_delayed_geometric_annuity_solve from mrffl_tvm.@EOL
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
program test_tvm_delayed_geometric_annuity_solve
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_tvm
  use mrffl_var_sets

  real(kind=rk)    :: n, i, g, pv, fv, a
  integer(kind=ik) :: d, e, status
  integer          :: k

  print "(a)", repeat("=", 119)
  print "(a3,a5,6(a15),3(a7))", "BF", "stat", "n", "i", "g", "pv", "fv", "a", "d", "e", "var"

  print "(a)", repeat("=", 119)
  call setem(1)
  print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_a+var_fv+var_pv
  call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_a+var_fv+var_pv, status)
  print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_a+var_fv+var_pv

  print "(a)", repeat("=", 119)
  call setem(1)
  print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_pmt
  call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_pmt, status)
  print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_pmt

  do k=1,4
     print "(a)", repeat("=", 119)
     call setem(k)
     a = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_a
     call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_a, status)
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_a

     print "(a)", repeat("=", 119)
     call setem(k)
     a = -1
     pv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_a+var_pv
     call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_a+var_pv, status)
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_a+var_pv

     print "(a)", repeat("=", 119)
     call setem(k)
     a = -1
     fv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_a+var_fv
     call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_a+var_fv, status)
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_a+var_fv

     print "(a)", repeat("=", 119)
     call setem(k)
     n = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_n
     call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_n, status)
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_n

     print "(a)", repeat("=", 119)
     call setem(k)
     n = -1
     fv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_n+var_fv
     call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_n+var_fv, status)
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_n+var_fv

     print "(a)", repeat("=", 119)
     call setem(k)
     fv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_fv
     call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_fv, status)
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_fv

     print "(a)", repeat("=", 119)
     call setem(k)
     pv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_pv
     call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_pv, status)
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_pv

     print "(a)", repeat("=", 119)
     call setem(k)
     fv = -1
     pv = -1
     print "(a3,i5,6(f15.4),3(i7))", "BF", status, n, i, g, pv, fv, a, d, e, var_fv+var_pv
     call tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, var_fv+var_pv, status)
     print "(a3,i5,6(f15.4),3(i7))", "AF", status, n, i, g, pv, fv, a, d, e, var_fv+var_pv
  end do
  print "(a)", repeat("=", 119)
contains
  subroutine setem(k)
    integer, intent(in) :: k
    if      (k==1) then
       ! Case: d=1  e=0, i/=g, i>0
       n        =  15
       i        =   4
       g        =   3
       pv       =   13491.599717769010        
       fv       =   24297.608890615225     
       a        =   1000
       d        =   1
       e        =   0
       status   =  -1
    else if (k==2) then
       ! Case: d=1  e=0, i/=g, -100<i<0
       n        =  15
       i        =   -4
       g        =   3
       pv       =   26771.722696416953     
       fv       =   14512.586239140792        
       a        =   1000
       d        =   1
       e        =   0
       status   =  -1
    else if (k==3) then
       ! Case: d=1  e=0, i=g, i>0
       n        =  15
       i        =   4
       g        =   4
       pv       =   14423.076923076922        
       fv       =   25975.146714042068     
       a        =   1000
       d        =   1
       e        =   0
       status   =  -1
    else if (k==4) then
       ! Case: d=1  e=0, i/=g, -100<i<0
       n        =  15
       i        =   -4
       g        =   -4
       pv       =   15625.000000000002        
       fv       =   8470.0996853267043     
       a        =   1000
       d        =   1
       e        =   0
       status   =  -1
    else if (k==1) then
       ! Case: d=1  e=0, i<-100
       ! MJR TODO NOTE <2024-12-18T14:18:37-0600> setem: Add i<-100 cases, correct solver if possible
       n        =  5
       i        =   -110
       g        =   3
       pv       =   -102591.51099999959        
       fv       =   1.0259151099999999     
       a        =   1
       d        =   1
       e        =   0
       status   =  -1
    end if
    ! MJR TODO NOTE <2024-12-18T14:19:02-0600> setem: Add more d/e cases.
  end subroutine setem
end program test_tvm_delayed_geometric_annuity_solve

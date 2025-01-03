! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      test_tvm12.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: mrffl_tvm12.@EOL
!! @keywords  finance fortran monte carlo tvm12 cashflow time value of money tvm tvm12 taxes stock market
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
program test_tvm12
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik, mrfflcnfmt, mrfflctfmt
  use mrffl_tvm12
  use mrffl_var_sets
  use mrffl_prt_sets

  integer(kind=ik) :: n, status
  real(kind=rk)    :: i, pv, fv, pmt

  mrfflcnfmt = "f10.4"
  mrfflctfmt = "a10"

  ! MJR TODO NOTE <2024-12-16T14:01:45-0600> retire: Add solve for i case where i<-100 (for both end & beginning)
  ! MJR TODO NOTE <2024-12-16T14:01:45-0600> retire: Add solve for i case where -100<i<0 (for both end & beginning)
  ! MJR TODO NOTE <2024-12-16T14:01:45-0600> retire: Add solve for n case where n would be negative (for both end & beginning)
  ! MJR TODO NOTE <2024-12-16T14:01:45-0600> retire: Add solve for i known and zero
  ! MJR TODO NOTE <2024-12-16T14:01:45-0600> retire: Add solve for i known and -1
  ! MJR TODO NOTE <2024-12-16T14:01:45-0600> retire: Add solve for n known and non-positive

  print "(a)", repeat("=", 108)
  print "(a7,4(a20),3(a7))", "n", "i", "pv", "pmt", "fv", "p_tim", "var", "status"

  print "(a)", repeat("=", 108)
  call set_em_up_end()
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_end, 0, status
  print "(a)", repeat("=", 108)

  call set_em_up_end()
  n = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_end, var_n, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_end, var_n, status

  call set_em_up_end()
  i = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_end, var_i, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_end, var_i, status

  call set_em_up_end()
  pv = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_end, var_pv, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_end, var_pv, status

  call set_em_up_end()
  pmt = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_end, var_pmt, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_end, var_pmt, status

  call set_em_up_end()
  fv = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_end, var_fv, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_end, var_fv, status
  print "(a)", repeat("=", 108)

  call tvm12_print(n, i, pv, pmt, fv, pmt_at_end, prt_ALL)

  print "(a)", repeat("=", 108)
  print "(a7,4(a20),3(a7))", "n", "i", "pv", "pmt", "fv", "p_tim", "var", "status"

  print "(a)", repeat("=", 108)
  call set_em_up_beg()
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_beginning, 0, status
  print "(a)", repeat("=", 108)

  call set_em_up_beg()
  n = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_beginning, var_n, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_beginning, var_n, status

  call set_em_up_beg()
  i = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_beginning, var_i, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_beginning, var_i, status

  call set_em_up_beg()
  pv = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_beginning, var_pv, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_beginning, var_pv, status

  call set_em_up_beg()
  pmt = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_beginning, var_pmt, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_beginning, var_pmt, status

  call set_em_up_beg()
  fv = -1
  call tvm12_solve(n, i, pv, pmt, fv, pmt_at_beginning, var_fv, status)
  print "(i7,4(f20.5),3(i7))", n, i, pv, pmt, fv, pmt_at_beginning, var_fv, status
  print "(a)", repeat("=", 108)

  call tvm12_print(n, i, pv, pmt, fv, pmt_at_end, prt_ALL)

contains
  subroutine set_em_up_end()
    i      = 0.04
    pv     = 1000.0
    pmt    =  -392.38339254851439
    n      = 3
    fv     = 100.0
    status = 0
  end subroutine set_em_up_end
  subroutine set_em_up_beg()
    i      = 0.04
    pv     = 1000.0
    pmt    =   -377.29172392869191     
    n      = 3
    fv     = 100.0
    status = 0
  end subroutine set_em_up_beg
end program test_tvm12

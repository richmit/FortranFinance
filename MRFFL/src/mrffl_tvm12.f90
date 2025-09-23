! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_tvm12.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     TVM solver with functionality similar to financial calculators.@EOL
!! @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!


!##################################################################################################################################
!> Provides a TVM solver with functionality similar to modern financial calculators.
!!
!! Most financial calculators use the following relationship to implement TVM functionality:
!! @f[ 0=PV+(1+i\cdot b)\cdot\frac{PMT}{i}\cdot\left(1-\frac{1}{(1+i)^n}\right)+\frac{FV}{(1+i)^n} @f]
!!
!! This single equation defines several relationships, and can be used to solve a great many different kinds of TVM problems.
!!
!! The modular approach taken in tvm.f90 and cashflows.f90 is a far more flexible to and powerful way to solve TVM problems;
!! however, the simplicity and familiarity of the classical calculator approach is sometimes more comfortable and direct.
!!
module mrffl_tvm12
  use mrffl_config,   only: rk=>mrfflrk, cnfmt=>mrfflcnfmt, ctfmt=>mrfflctfmt, zero_epsilon
  use mrffl_bitset,   only: bitset_intersectp, bitset_subsetp
  use mrffl_var_sets, only: var_i, var_n, var_pv, var_fv, var_pmt
  use mrffl_solver,   only: multi_bisection
  ! use mrffl_solver_ne, only: multi_bisection
  use mrffl_prt_sets, only: prt_param, prt_table, prt_title
  implicit none (type, external)
  private

  ! Quite a lot of code depends on the values being 1 & 0.  Do not change them!
  integer,          parameter, public  ::  pmt_at_beginning = 1
  integer,          parameter, public  ::  pmt_at_end       = 0

  public :: tvm12_solve, tvm12_print
  public :: var_i, var_n, var_pv, var_fv, var_pmt

contains

  !------------------------------------------------------------------------------------------------------------------------------
  !> Solve TVM Equation.
  !!
  !!  Value returned in status:
  !!  -  0 - Everything worked
  !!  -  1 - ERROR(tvm_solve): n==0!
  !!  -  2 - ERROR(tvm_solve): n<0!
  !!  -  3 - ERROR(tvm_solve): Unsupported value for pmt_time (must be one of pmt_at_beginning or pmt_at_end)
  !!  -  4 - ERROR(tvm_solve): Unsupported value for action!
  !!  -  5 - ERROR(tvm_solve): Unable to solve for i
  !!  -  6 - ERROR(tvm_solve): Can not solve for n!
  !!  -  7 - ERROR(tvm_solve): i near -1!
  !!  -  8 - ERROR(tvm_solve): i near zero!
  !!  -  9 - Reserved
  !!
  !! WARNING: Solving for i is not entirely reliable.  Only values between -1 and 1 may be found.  Additionally it is
  !! possible a solution in that range might not be found.
  !!
  !! @param n         Number of compounding periods
  !! @param i         Interest as a fraction (not a percentage)
  !! @param pv        Present Value
  !! @param pmt       Payment
  !! @param fv        Future Value
  !! @param pmt_time  Payments at beginning or end of period.  Allowed parameters: pmt_at_beginning or pmt_at_end
  !! @param unknown   The unknown variable to solve for.  Allowed parameters: var_pmt var_i, var_n, var_pv, or var_fv.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 3001-3032.
  subroutine tvm12_solve(n, i, pv, pmt, fv, pmt_time, unknown, status)
    integer,          intent(inout) :: n
    real(kind=rk),    intent(inout) :: i, pv, pmt, fv
    integer,          intent(in)    :: pmt_time, unknown
    integer,          intent(out)   :: status
    real (kind=rk)                  :: ip1tn, tmp1, tmp2, islvivl0(3), islvivl1(3), r_dat(4)
    integer                         :: i_dat(2)
    if (unknown /= var_n) then
       if (n == 0) then
          status = 3001 ! "ERROR(tvm_solve): n==0!"
          return
       end if
       if (n < 0) then
          status = 3002 ! "ERROR(tvm_solve): n<0!"
          return
       end if
    end if
    if (unknown /= var_i) then
       if (abs(i) < zero_epsilon) then
          status = 3003 ! "ERROR(tvm_solve): i near zero!"
          return
       else if (abs(1+i) < zero_epsilon) then
          status = 3004 ! "ERROR(tvm_solve): i near -1!"
          return
       end if
    end if
    r_dat = [ i, pv, pmt, fv ]
    i_dat = [ n, pmt_time ]
    islvivl0  = [ 0.0_rk+zero_epsilon, -100.0_rk+zero_epsilon,            -99999.0_rk]
    islvivl1  = [          99999.0_rk,    0.0_rk-zero_epsilon, -100.0_rk-zero_epsilon]
    ip1tn = (1+i) ** n
    if (pmt_time == pmt_at_beginning) then
       if (unknown == var_pmt) then
          pmt = i * (fv + ip1tn * pv) / (i + 1 - (1+i) * ip1tn)
          status = 0
       else if (unknown == var_i) then
          call multi_bisection(i, islvivl0, islvivl1, i_slv_func, 1.0e-5_rk, 1.0e-5_rk, 1000, status, .false.)
          !call multi_bisection(i, islvivl0, islvivl1, i_slv_func, r_dat, i_dat, 1.0e-5_rk, 1.0e-5_rk, 1000, status, .false.)
          if (status /= 0) then
             status = 3005 ! "ERROR(tvm_solve): Unable to solve for i!"
          end if
       else if (unknown == var_n) then
          tmp1 = ((-1 - i) * pmt - pv * i) / ((-1 - i) * pmt + fv * i)
          tmp2 = 1 + i
          if ((tmp1 < zero_epsilon) .or. (tmp2 < zero_epsilon)) then
             status = 3006 ! "ERROR(tvm_solve): Can not solve for n!"
          else
             n = nint(-log(tmp1) / log(tmp2))
             status = 0
          end if
       else if (unknown == var_pv) then
          pv = ((pmt * (1 + i) - fv * i) * (1 + i) ** (-n) - pmt * (1 + i)) / i
          status = 0
       else if (unknown == var_fv) then
          fv = (((-1 - i) * pmt - pv * i) * ip1tn + pmt * (1 + i)) / i
          status = 0
       end if
    else if (pmt_time == pmt_at_end) then
       if (unknown == var_pmt) then
          pmt = -i / (ip1tn - 1) * (fv + ip1tn * pv)
          status = 0
       else if (unknown == var_i) then
          call multi_bisection(i, islvivl0, islvivl1, i_slv_func, 1.0e-5_rk, 1.0e-5_rk, 1000, status, .false.)
          !call multi_bisection(i, islvivl0, islvivl1, i_slv_func, r_dat, i_dat, 1.0e-5_rk, 1.0e-5_rk, 1000, status, .false.)
          if (status /= 0) then
             status = 3007 ! "ERROR(tvm_solve): Unable to solve for i!"
          end if
       else if (unknown == var_n) then
          tmp1 = (-pv * i - pmt) / (fv * i - pmt)
          tmp2 = 1 + i
          if ((tmp1 < zero_epsilon) .or. (tmp2 < zero_epsilon)) then
             status = 3008 ! "ERROR(tvm_solve): Can not solve for n!"
          else
             n = nint(-log(tmp1) / log(tmp2))
             status = 0
          end if
       else if (unknown == var_pv) then
          pv = ((-fv * i + pmt) / ip1tn - pmt) / i
          status = 0
       else if (unknown == var_fv) then
          fv = ((-pv * i - pmt) * ip1tn + pmt) / i
          status = 0
       else
          status = 3009 ! "ERROR(tvm_solve): Unsupported value for unknown!"
       end if
    else
       status = 3010 ! "ERROR(tvm_solve): Unsupported value for pmt_time (must be one of pmt_at_beginning or pmt_at_end)"
    end if
  contains
    real(kind=rk) function i_slv_func(x)
      implicit none (type, external)
      real(kind=rk), intent(in) :: x
      i_slv_func = (((-pmt_time * pmt + fv) * x - pmt) * (1 + x) ** (-n) + (pmt_time * pmt + pv) * x + pmt) / x
    end  function i_slv_func
  end subroutine tvm12_solve


  ! real(kind=rk) function i_slv_func(x, r_dat, i_dat)
  !   implicit none (type, external)
  !   real(kind=rk),    intent(in) :: x
  !   real(kind=rk),    intent(in) :: r_dat(:)
  !   integer,          intent(in) :: i_dat(:)
  !   integer                      :: n, pmt_time
  !   real(kind=rk)                :: i, pv, pmt, fv
  !   i        = r_dat(1)
  !   pv       = r_dat(2)
  !   pmt      = r_dat(3)
  !   fv       = r_dat(4)
  !   n        = i_dat(1)
  !   pmt_time = i_dat(2)
  !   i_slv_func = (((-pmt_time * pmt + fv) * x - pmt) * (1 + x) ** (-n) + (pmt_time * pmt + pv) * x + pmt) / x
  ! end  function i_slv_func


  !------------------------------------------------------------------------------------------------------------------------------
  !> Print TVM Problem (variables and/or table)
  !!
  !! @param n            Number of compounding periods
  !! @param i            Interest as a fraction (not a percentage)
  !! @param pv           Present Value
  !! @param pmt          Payment
  !! @param fv           Future Value
  !! @param pmt_time     Payments at beginning or end of period.  Allowed parameters: pmt_at_beginning or pmt_at_end
  !! @param print_out    Set made from the following constants: prt_param, prt_table, prt_title
  subroutine tvm12_print(n, i, pv, pmt, fv, pmt_time, print_out)
    implicit none (type, external)
    integer,          intent(in) :: n, pmt_time, print_out
    real(kind=rk),    intent(in) :: i, pv, fv, pmt
    real (kind=rk)               :: tot_pmt, cur_pv
    integer                      :: k

    if ((pmt_time /= pmt_at_beginning) .and. (pmt_time /= pmt_at_end)) then
       stop "ERROR(tvm_solve): Unsupported value for pmt_time (must be one of pmt_at_beginning or pmt_at_end)"
    end if
    if (bitset_intersectp(prt_param+prt_table, print_out)) then
       print *, ""
    end if
    if (bitset_subsetp(prt_param, print_out)) then
       print "(a20,i30)",   "n:", n
       print "(a20,f30.8)", "i:", i
       print "(a20,f30.8)", "pv:", pv
       print "(a20,f30.8)", "fv:", fv
       print "(a20,f30.8)", "pmt:", pmt
       if (pmt_time == pmt_at_beginning) then
          print "(a20,a30)", "pmt_time:", "BEGIN"
       else
          print "(a20,a30)", "pmt_time:", "END"
       end if
    end if
    if (bitset_intersectp(prt_param+prt_table, print_out)) then
       print *
    end if
    if (bitset_subsetp(prt_table, print_out)) then
       if (bitset_subsetp(prt_title, print_out)) print "(a8,2(1x,"//ctfmt//"))", "period", "cur_pv", "tot_pmt"
       cur_pv = pv
       tot_pmt = 0
       do k=0,n
          if (pmt_time == pmt_at_beginning) then
             print "(i8,2(1x,"//cnfmt//"))", k, cur_pv, tot_pmt
             tot_pmt = tot_pmt + pmt
             cur_pv = cur_pv + pmt
             cur_pv = cur_pv + cur_pv * i
          else if (pmt_time == pmt_at_end) then
             print "(i8,2(1x,"//cnfmt//"))", k, cur_pv, tot_pmt
             cur_pv = cur_pv + cur_pv * i
             tot_pmt = tot_pmt + pmt
             cur_pv = cur_pv + pmt
          end if
       end do
    end if
    if (bitset_intersectp(prt_param+prt_table, print_out)) then
       print *, ""
    end if
  end subroutine tvm12_print

end module mrffl_tvm12

! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_tvm.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     Time value of money solvers.@EOL
!! @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
!! @std       F2023
!! @see       https://github.com/richmit/FortranFinance/MRFFL
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

!------------------------------------------------------------------------------------------------------------------------------
!> Solvers for TVM problems involving lump sums, level (fixed) annuities, and geometric (growing) annuities.
!! 
!! @par Definitions and Notation
!!
!!   - Annuity in advance, annuity due: Payment at beginning of each period. Ex: Rent and subscription fees.
!!   - Ordinary annuity, annuity in arrears, annuity-immediate: Payment at the end of each period.  Ex: Mortgages and car loans.
!!   - Annuity certain, guaranteed annuity: The number of payments is known in advance
!!   - Deferred annuity: An annuity that begins payments only after a number of periods.
!!   - Early end annuity: An annuity that that terminates before the number of periods.  This can be because the annuity
!!     was contingent; however, it is more frequently an artificial condition used to base the PV/FV computation on a different
!!     term than that for which the annuity was intended.
!!   - Annuity payment growth:
!!     - fixed, level: all payments are the same
!!     - growing, geometric: payments grow geometrically
!!     - arithmatic: payments grow linearly
!!
!! In working with people from different places and from many different fields, I have discovered some of the above terminology is
!! used differently or becomes confusing depending upon the audience.  To avoid such issues, I follow the following rules in this
!! package:
!!
!!  - I avoid the adjective "fixed" with regard to annuities because it is confused with the adjectives "certain" & "guaranteed".
!!    As such, "fixed annuities" are called "level annuities" in this package.
!!
!!  - I avoid the term "annuity-immediate" because many confuse it with annuities that are not "deferred" -- i.e. pay immediately
!!    upon purchase.  As such I use the phrase
!!
!!  - I avoid the adjective "guaranteed" and "guarantee" with regard to annuities because they are confused with the term "non
!!    contingent". As such I use the word "certain".  In this module all annuities are certain, so it is save to assume an
!!    annuity is certain when it's not explicity specified.
!! 
!!  - I avoid the adjective "growing" with regard to annuities simply because it is too generic and offends my mathematical sense
!!    of precision. Arithmetic annuities are "growing" too?!? Right?!?  As such I use the term "geometric".
!! 
!! @par Approach
!! 
!! It's common practice in financial problem solving to produce a single set of TVM equations for a particular scenario, and then
!! solve the resulting equation(s).  For example a loan might be thought of as a single cashflow stream starting with a negative
!! cashflow for the principal followed by sequence of negative, equal cash flows for the payments with the overall condition that
!! the cashflow stream has a future value of zero.  As an aside, this is the fundamental "TVM Equation" used by modern financial
!! calculators -- so this approach can lead to single equations that are broadly applicable to many problems.
!!
!! Most TVM problems may be broken down into distinct components corresponding to well known, fundamental TVM problems.  It is
!! frequently possible to solve the overall problem by solving these smaller, well known problems in isolation. For example a
!! loan can be thought of as two cashflow sequences: An ordinary lump sum and aj ordinary annuity certain -- we simply require
!! that each of these two cashflows have equal future values.
!!
!! This module encourages this second method of solving TVM problems by providing very generic TVM solvers for some very common
!! cashflow patterns (various forms of annuities & lump sums).  This allows one to mix and match the solvers as required applying
!! them to the components of a typical TVM problem.
!!
!! @par Annuities
!!
!! The annuity solvers in this module are uniquely flexible.  In most software annuities are defined as a fixed number of periods
!! with a single payment occurring in each period such that all payments occur either at the beginning (ordinary annuities) or
!! end (annuities due) of the period.  Instead of associating payments with periods, this module ties them directly to the
!! *boundaries* between periods -- an n period annuity has n+1 boundaries (one at time zero, one at time n, and n-1 between
!! periods).
!!
!! In this module the number of periods (n) and the boundaries at which the payments start (d) and end (e) are each free
!! variables in the definition of an annuity. This allows us to handle ordinary annuities, annuities due, differed annuities, and
!! truncated annuities with just a single solver!  In addition we can use this flexibility to base PV/FV computations on a term
!! different from the natural term of the annuity -- for example we can compute the PV for an annuity due on n-1 periods instead
!! of n periods.  Using this notation, we can obtain typical annuities like so:
!!
!!   - An N payment ordinary annuity:   n=N, d=1, e=0
!!   - An N payment annuity due:        n=N, d=0, e=1
!!       Note that PV/FV are normally computed for N periods for these types of annuities; however, the last payment is at the
!!       beginning of the last period.  This leaves an "empty" period at the end.  Sometimes it is preferable to base the
!!       computations on N-1 periods instead.  To do this, use N-1 as the period count and set e=0.
!!   - For a delayed ordinary annuity that starts paying in period k, set d=k
!!   - For a delayed annuity due that starts paying in period k, set d=k-1
!!
!! @par Lump Sums
!!
!! Like the annuity solvers, the lump sum solver in this module is also uniquely flexible.  Instead of the lump sum always being
!! paid at the beginning of the first period, this library allows one to specify the payment on any period boundary.  
!!
!!   - For an "ordinary lump sum" use d=0
!!     Note that some packages call this "a lump sum with payment mode BEGIN"
!!     Because this is a very common case, a "helper" function exists to make this easier: tvm_lump_sum_solve
!!   - For a "lump sum due" use d=1
!!     Note that some packages call this "a lump sum with payment mode END"
!!
!! @par Solvers
!!
!! First we should define what we mean by a "solver".  The TVM problems this module works with are each governed by a pair of
!! formulas (one for fv and one for pv) in terms of n, i, g, q, & a.  Most of the time we want pv & fv, so we just evaluate the
!! formulas.  Sometimes we already know pv and fv and want another variable.  The "solvers" in this package provide an interface
!! through which we may ask for any two variables to be found with respect to the others.
!!
!! The primary solver functions are:
!!
!!   - tvm_delayed_lump_sum_solve()
!!   - tvm_delayed_level_annuity_solve()
!!   - tvm_delayed_geometric_annuity_solve()
!!   - tvm_delayed_arithmetic_annuity_solve()
!!
!! These functions operate in largely the same manner.  The first 5 or 6 arguments are variables in the TVM equations.  These
!! first arguments are all ~intent(inout)~ arguments -- they hold known values upon entry and hold solved values upon exit.
!! These arguments are followed by the delay (d) argument.  For annuities this is followed by the early end argument.  Next is
!! the ~unknown~ argument that specifies what variables we wish to solve for.  Lastly is a ~status~ argument used to return
!! errors.
!! 
!!                                                                              delay
!!                                                                              |  early end
!!                                                        ---- variables ----   |  |
!!                   tvm_delayed_lump_sum_solve(          n, i,    pv, fv, a,   d, |  unknowns, status)
!!                   tvm_delayed_level_annuity_solve(     n, i,    pv, fv, a,   d, e, unknowns, status)
!!                   tvm_delayed_geometric_annuity_solve( n, i, g, pv, fv, a,   d, e, unknowns, status)
!!                   tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a,   d, e, unknowns, status)
!!                                                                                    |         |
!!                                                                                    |         Used to return error codes
!!                                                                                    Specifies unknown variables
!!
!! Every one of these solvers works with *two* equations (one for pv and one for fv).  As a system of two equations, we can
!! normally solve for two unknowns.  The unknowns we wish to find are specified in the ~unknowns~ argument using a sum of
!! variable constants.  For example, we would request that PV and FV be found with a sum like this:
!!
!!                                     var_pv + var_fv
!!
!! For the lump sum solver there are 15 possible combinations of 1 or 2 variables:
!!
!!   var_n, var_n+var_i, var_n+var_pv, var_n+var_fv, var_n+var_a, var_i, var_i+var_pv, var_i+var_fv, 
!!   var_i+var_a, var_pv, var_pv+var_fv, var_pv+var_a, var_fv, var_fv+var_a, var_a
!!
!! For the lump sum solver any of the combinations may be solved.  For others there are limitations.  See the documentation for the
!! specific solver you are using.
!!
!! Note that if we only solve for one variable there is a possibility that the resulting collection of variable values may be
!! inconsistent with the underlying equations.  So if you solve for only one variable, make sure all of the other variables
!! contain valid values.  Each solver subroutine finishes by calling a corresponding consistency checker routine to insure
!! all the values lead to consistent equations.  These routines are exported from the package:
!!
!!   - tvm_delayed_lump_sum_check()
!!   - tvm_delayed_level_annuity_check()
!!   - tvm_delayed_geometric_annuity_check()
!!   - tvm_delayed_arithmetic_annuity_check()
!!
!! Aside from not taking the ~unknowns~ argument, the consistency checkers follow the same argument pattern as the solvers.
!!
!! Solving for n.
!!
!! Logically the value for n is the number of periods -- a positive integer.  For these routines n is allowed to be any real
!! value so that we can solve for the variable and produce consistent results.  If an integer value is required: 
!!   -# First solve for n. 
!!   -# Transform n to an integer in an appropriate way (perhaps ceiling for a loan). 
!!   -# Using this integer n, now solve for two other variables in the equations (perhaps a & i for a loan).  
!!      You will now have a consistent set of equations with an
!!      integer number of periods with the two variables solved for in the last step being adjusted.
!!
!! @par Error handling
!!
!! This module is intended to be used as a subsystem in larger software packages.  As such it never STOPs -- it simply reports
!! error conditions back to the caller.  Subroutines in this module report errors via an integer parameter named "status".  If
!! this parameter is zero after a call, then no error conditions were encountered.  The error codes are allocated in blocks to
!! each subroutine so that no subroutines share common error codes.  This is intended to assist the caller in ascertaining where
!! the problem occurred.  In the code each error code is documented with a comment where the status value is set. i.e. search for
!! the error code, and you will find the description.  
!!
!! @par Other Approaches & References
!!
!! For experience more like a financial calculator, see the module tvm12.
!!
!! For cashflow problems, see the module cashflows.  Note it is easy to model the kinds of cashflows in this module with the
!! cashflows module -- for things like amortization tables.
!!
module mrffl_tvm
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik, zero_epsilon
  use mrffl_bitset, only: bitset_size, bitset_minus, bitset_subsetp, bitset_not_subsetp, bitset_not_intersectp
  use mrffl_var_sets, only: var_NONE, var_a, var_p, var_i, var_g, var_n, var_pv, var_fv, var_q
  use mrffl_percentages, only: p2f => percentage_to_fraction, f2p => fraction_to_percentage
  use mrffl_solver, only: multi_bisection
  implicit none  
  private

  real(kind=rk),    parameter         :: consistent_epsilon = 1.0e-3_rk !< Used to check equation consistency

  ! Primary TVM equation solvers
  public :: tvm_delayed_lump_sum_solve, tvm_delayed_level_annuity_solve, tvm_delayed_geometric_annuity_solve, tvm_delayed_arithmetic_annuity_solve

  ! Primary TVM equation consistency checkers
  public :: tvm_delayed_lump_sum_check, tvm_delayed_level_annuity_check, tvm_delayed_geometric_annuity_check, tvm_delayed_arithmetic_annuity_check

  ! Helper solvers for common special cases of the primary TVM solvers above
  public :: tvm_lump_sum_solve

  ! Related computational routines
  public :: tvm_geometric_annuity_sum_a, fv_from_pv_n_i, tvm_delayed_annuity_num_payments

contains

  !----------------------------------------------------------------------------------------------------------------------------
  !> compute future value from present value (pv), number of periods (n), and an intrest rate (i).
  real(kind=rk) pure elemental function fv_from_pv_n_i(pv, n, i)
    integer(kind=ik), intent(in) :: n
    real(kind=rk),    intent(in) :: pv, i
    fv_from_pv_n_i = pv * (1+i) ** n
  end function fv_from_pv_n_i

  !----------------------------------------------------------------------------------------------------------------------------
  !> Sum the payments from a geometric annuity.
  !! Ex: Sum of inflation adjusted payments.
  !!
  !! The payment sequence is the same no matter when it starts.  The first non-zero payment is A, and the
  !! last non-zero payment is A(1+i)^{n-1}:
  !!
  !! @f[ \begin{array}{ll}
  !!       V_1 & = A            \\
  !!       V_2 & = A(1+g)       \\
  !!           &    ...         \\
  !!       V_m & = A(1+g)^{m-1} \\
  !!           &    ...         \\
  !!       V_n & = A(1+g)^{n-1} \\
  !!  \end{array} @f]
  !!
  !! Formulas
  !! @f[ \begin{array}{ll}
  !!    P_{sum} & = P\cdot\left(\frac{(i+1)^{n}-1}{i}\right) \\ 
  !!  \end{array} @f]
  !!
  !! @param n         Number of compounding periods (WARNING: Unlike elsewhere in this module, this is an INTEGER not a REAL)
  !! @param g         Payment growth rate as a percentage.
  !! @param a         First payment (Annuity)
  !!
  real(kind=rk) pure function tvm_geometric_annuity_sum_a(n, g, a)
    integer(kind=ik), intent(in) :: n
    real(kind=rk),    intent(in) :: g, a
    real(kind=rk)                :: gq
    if (n < 1) then
       tvm_geometric_annuity_sum_a = 0
    else
       gq = p2f(g)    
       tvm_geometric_annuity_sum_a = a * ((1+gq)**n-1) / gq
       ! tvm_geometric_annuity_sum_a = 0
       ! do j=0,n-1
       !    tvm_geometric_annuity_sum_a = tvm_geometric_annuity_sum_a +  a * (1+gq)**j
       ! end do
    end if
  end function tvm_geometric_annuity_sum_a

  !----------------------------------------------------------------------------------------------------------------------------
  !> Return the number of payments given the period count (n), delay (d), and early end (e).
  !! 
  !! Examples: 
  !!     d      0 1 2 3 4 5 6 7 8 9
  !!     period 0 1 2 3 4 5 6 7 8 9
  !!     e      9 8 7 6 5 4 3 2 1 0
  !!              |           |   |
  !!         d=1 -+      e=2 -+   +- n=9   -> num_payments = 1+n-e-d = 7
  !! 
  !! @param n         Number of periods.
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !!
  integer(kind=ik) pure function tvm_delayed_annuity_num_payments(n, d, e)
    integer(kind=ik), intent(in) :: n, d, e
    tvm_delayed_annuity_num_payments = 1+n-e-d
  end function tvm_delayed_annuity_num_payments

  !----------------------------------------------------------------------------------------------------------------------------
  !> Solve for TVM parameters for a lump sum.
  !!
  !! This is a simple wrapper that calls See tvm_delayed_lump_sum_solve().
  !!
  !! @param n         Number of compounding periods
  !! @param i         Annual growth rate as a percentage.
  !! @param pv        Present Value
  !! @param fv        Future Value
  !! @param unknowns  What variable to solve for.  
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 1193-1224.
  !!
  subroutine tvm_lump_sum_solve(n, i, pv, fv, unknowns, status)
    implicit none
    real(kind=rk),    intent(inout) :: n, i, pv, fv
    integer(kind=ik), intent(in)    :: unknowns
    integer(kind=ik), intent(out)   :: status
    integer(kind=ik), parameter     :: allowed_vars = var_n + var_i + var_pv + var_fv
    real(kind=rk)                   :: a
    if (bitset_not_subsetp(unknowns, allowed_vars)) then
       status = 1193 ! "ERROR(tvm_lump_sum_solve): Unknown unknowns!"
       return
    end if
    if (bitset_size(unknowns) > 1) then
       status = 1194 ! "ERROR(tvm_lump_sum_solve): Too many unknowns!"
       return
    end if
    a = pv
    if (bitset_subsetp(var_pv, unknowns)) then
       call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, 0_ik, var_pv+var_a, status)
    else
       call tvm_delayed_lump_sum_solve(n, i, pv, fv, a, 0_ik, unknowns, status)
    end if
  end subroutine tvm_lump_sum_solve

  !----------------------------------------------------------------------------------------------------------------------------
  !> Solve for TVM parameters for a generalized lump sum.
  !!
  !! The equations this solver uses are as follows:
  !! @f[ \mathit{fv} = a \left(1+q \right)^{n -d} @f]
  !! @f[ \mathit{pv} = a \left(1+q \right)^{-d}   @f]
  !!
  !! Normally TVM software assumes a lump sum cashflow occurs at the beginning of the first period.  This solver allows the
  !! cashflow to occur at the beginning or end of any period.
  !!
  !! This solver can find any combination of 1 or 2 of the following variables: n, i, pv, fv, & a.
  !! 
  !! @param n         Number of compounding periods
  !! @param i         Annual growth rate as a percentage.
  !! @param pv        Present Value
  !! @param fv        Future Value
  !! @param a         The size of the cashflow
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param unknowns  What variables to solve for.  
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 1161-1192.
  !!
  subroutine tvm_delayed_lump_sum_solve(n, i, pv, fv, a, d, unknowns, status)
    real(kind=rk),    intent(inout) :: n, i, pv, fv, a
    integer(kind=ik), intent(in)    :: d, unknowns
    integer(kind=ik), intent(out)   :: status
    integer(kind=ik), parameter     :: allowed_vars = var_n + var_i + var_pv + var_fv + var_a
    integer                         :: num_unknowns
    real(kind=rk)                   :: iq
    num_unknowns = bitset_size(unknowns)
    if (num_unknowns > 2) then
       status = 1161 ! "ERROR(tvm_delayed_lump_sum_solve): Too many unknowns!"
    else if (bitset_not_subsetp(unknowns, allowed_vars)) then
       status = 1162 ! "ERROR(tvm_delayed_lump_sum_solve): Unknown unknowns!"
    else
       if (num_unknowns > 0) then
          if (bitset_subsetp(var_a, unknowns)) then             ! a is unknown
             if (bitset_subsetp(var_i, unknowns)) then          ! a & i are unknown
                i = f2p((pv / fv) ** (-1 / n) - 1)
             end if
             iq = p2f(i)
             if      (bitset_subsetp(var_pv, unknowns)) then    ! a & pv are unknown
                pv = fv / (1 + iq) ** n
             else if (bitset_subsetp(var_fv, unknowns)) then    ! a & fv are unknown
                fv = pv * (1 + iq) ** n
             else if (bitset_subsetp(var_n, unknowns)) then     ! a & n are unknown
                n = -log(pv / fv) / log(1 + iq)
             end if
             a = fv * (1 + iq) ** (d - n)
          else if (bitset_subsetp(var_n, unknowns)) then        ! a is known. n is unknown
             if (bitset_subsetp(var_i, unknowns)) then          ! a is known. n & i are unknown
                if (abs(d) < zero_epsilon) then
                   status = 1163 ! "ERROR(tvm_delayed_lump_sum_solve): Can not solve for i with n unknown and d=0!"
                   return
                end if
                i = f2p((pv / a) ** (-1.0_rk / d) - 1)
             end if
             iq = p2f(i)
             if      (bitset_subsetp(var_pv, unknowns)) then    ! a is known. n & pv are unknown
                pv = a * (1 + iq) ** (-d)
             else if (bitset_subsetp(var_fv, unknowns)) then    ! a is known. n & pv are unknown
                status = 1164 ! "ERROR(tvm_delayed_lump_sum_solve): Can not solve for fv with n unknown and d/=n!"
                return
             end if
             n = (d * log(1 + iq) + log(fv / a)) / log(1 + iq)
          else if (bitset_subsetp(var_i, unknowns)) then          ! a & n are known. i is unknown. 
             if      (bitset_subsetp(var_pv, unknowns)) then      ! a, n, & fv are known. i & pv are unknown  
                if (abs(d-n) < zero_epsilon) then
                   status = 1165 ! "ERROR(tvm_delayed_lump_sum_solve): Can not solve for pv with i unknown and d=n!"
                   return
                else
                   pv = a * ((fv / a) ** (-1 / (d - n))) ** (-d)
                end if
             else if (bitset_subsetp(var_fv, unknowns)) then      ! a, n, & pv are known. i & fv are unknown  
                if (abs(d) < zero_epsilon) then
                   status = 1166 ! "ERROR(tvm_delayed_lump_sum_solve): Can not solve for fv with i unknown and d=0!"
                   return
                else
                   if (abs(d-n) < zero_epsilon) then
                      fv = a
                   else
                      fv = (pv / a) ** ((d - n) / d) * a
                   end if
                end if
             end if
             if (abs(d-n) < zero_epsilon) then
                i = f2p(exp(-log(pv / a) / n) - 1)
             else
                i = f2p(exp(-log(fv / a) / (d - n)) - 1)
             end if
          else
             iq = p2f(i)
             if (bitset_subsetp(var_fv, unknowns)) then         ! a, i, & n are known. fv is unknown. pv may be unknown.
                fv = a * (1 + iq) ** (-d + n)
             end if
             if (bitset_subsetp(var_pv, unknowns)) then         ! a, i, & n are known. pv is unknown. fv may be unknown.
                pv = a * (1 + iq) ** (-d)
             end if
          end if
       end if
       call tvm_delayed_lump_sum_check(n, i, pv, fv, a, d, status) ! Return status set
    end if
  end subroutine tvm_delayed_lump_sum_solve

  !----------------------------------------------------------------------------------------------------------------------------
  !> Check the TVM parameters for a generalized lump sum.
  !!
  !! See tvm_delayed_lump_sum_solve() for more information regarding generalized lump sums.
  !!
  !! @param n       Number of compounding periods
  !! @param i       Annual growth rate as a percentage.
  !! @param pv      Present Value
  !! @param fv      Future Value
  !! @param a       The size of the cashflow
  !! @param d       Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param status  Returns status of computation. 0 if everything worked. Range: 0 & 1129-1160.
  !!
  subroutine tvm_delayed_lump_sum_check(n, i, pv, fv, a, d, status)
    real(kind=rk),    intent(in)  :: n, i, pv, fv, a
    integer(kind=ik), intent(in)  :: d
    integer(kind=ik), intent(out) :: status
    real(kind=rk)                 :: iq
    if (d < 0) then
       status = 1129 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (d<0)!"
    else if (.not. ieee_is_finite(n)) then
       status = 1130 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (n is infinite)!"
    else if (ieee_is_nan(n)) then
       status = 1131 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (n is NaN)!"
    else if (d > nint(n, ik)) then
       status = 1132 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (d>n)!"
    else if (n < zero_epsilon) then
       status = 1133 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (n<0)!"
    else if (n < 1) then
       status = 1134 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (n<1)!"
    else if (.not. ieee_is_finite(i)) then
       status = 1135 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (i is infinite)!"
    else if (ieee_is_nan(i)) then
       status = 1136 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (i is NaN)!"
    else if (abs(i) < zero_epsilon) then
       status = 1137 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (i is 0%)!"
    else if (abs(100+i) < zero_epsilon) then
       status = 1138 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (i is -100%)!"
    else if (.not. ieee_is_finite(pv)) then
       status = 1139 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (pv is infinite)!"
    else if (ieee_is_nan(pv)) then
       status = 1140 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (pv is NaN)!"
    else if (.not. ieee_is_finite(fv)) then
       status = 1141 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (fv is infinite)!"
    else if (ieee_is_nan(fv)) then
       status = 1142 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (fv is NaN)!"
    else if (.not. ieee_is_finite(a)) then
       status = 1143 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (a is infinite)!"
    else if (ieee_is_nan(a)) then
       status = 1144 ! "ERROR(tvm_delayed_lump_sum_check): Parameters inconsistent (a is NaN)!"
    else
       iq = p2f(i)
       if (abs(a * (1 + iq) ** (n - d) - fv) > consistent_epsilon) then
          status = 1145 ! "ERROR(tvm_delayed_lump_sum_check): Inconsistent parameters (fv equation)!"
       else if (abs(a / (1 + iq) ** d - pv) > consistent_epsilon) then
          status = 1146 ! "ERROR(tvm_delayed_lump_sum_check): Inconsistent parameters (pv equation)!"
       else
          status = 0
       end if
    end if
  end subroutine tvm_delayed_lump_sum_check

  !----------------------------------------------------------------------------------------------------------------------------
  !> Solve for TVM parameters for a level annuity.
  !!
  !! The equations this solver uses are as follows:
  !! @f[ \mathit{fv} = \frac{\left(-\left(1+i \right)^{d}+\left(1+i \right)^{n +1-d}\right) a}{i}  @f]
  !! @f[ \mathit{pv} = \frac{\left(-\left(1+i \right)^{-n +d}+\left(1+i \right)^{1-d}\right) a}{i} @f]
  !!
  !! This routine is capable of searching for any combination of 1 or 2 of the following variables: n, i, pv, fv, & a
  !! 
  !! Solving for i.
  !! When possible a closed for solution is used.  In some cases (var_i+var_fv, var_i+var_pv, var_i+var_n when d or e is greater
  !! than 1) bisection is used to numerically solve for i.  When bisection is used, this routine will search for values for i
  !! that are within the following intervals (in the order listed):
  !!      [zero_epsilon, 99999], [-100+zero_epsilon, -zero_epsilon], and [-99999, -100-zero_epsilon]
  !! In other words, it can find values of i that are between -99999 and 99999 and at least zero_epsilon away from 0% and 100%.
  !! If the value for i is outside these ranges, then no solution will be found.  It is also possible that even if a solution
  !! exists inside these ranges that the search algorithm may fail.  If solutions exist in more than one range, then the first
  !! one found is used.  
  !!
  !! @warning  Solutions are unreliable when i is less than -100%.  
  !! 
  !! @param n         Number of compounding periods
  !! @param i         Annual growth rate as a percentage.  When solving for this value only POSITIVE values are found.
  !! @param pv        Present Value
  !! @param fv        Future Value
  !! @param a         Payment (Annuity)
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param unknowns  What variable(s) to solve for.  Should be a sum of var_? constants.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 1097-1128.
  !!
  subroutine tvm_delayed_level_annuity_solve(n, i, pv, fv, a, d, e, unknowns, status)
    real(kind=rk),    intent(inout) :: n, i, pv, fv, a
    integer(kind=ik), intent(in)    :: d, e
    integer(kind=ik), intent(in)    :: unknowns
    integer(kind=ik), intent(out)   :: status
    integer(kind=ik), parameter     :: allowed_vars = var_n + var_i + var_pv + var_fv + var_a
    integer                         :: num_unknowns
    real(kind=rk)                   :: islvivl0(3), islvivl1(3), iq
    num_unknowns = bitset_size(unknowns)
    if (num_unknowns > 2) then
       status = 1097 ! "ERROR(tvm_delayed_level_annuity_solve): Too many unknowns!"
    else if (bitset_not_subsetp(unknowns, allowed_vars)) then
       status = 1098 ! "ERROR(tvm_delayed_level_annuity_solve): Unknown unknowns!"
    else
       if (num_unknowns > 0) then
          islvivl0  = [ 0.0_rk+zero_epsilon, -100.0_rk+zero_epsilon,            -99999.0_rk]
          islvivl1  = [          99999.0_rk,    0.0_rk-zero_epsilon, -100.0_rk-zero_epsilon]
          if (bitset_subsetp(var_a, unknowns)) then          ! a is unknown
             if (bitset_subsetp(var_i, unknowns)) then       ! a & i are unknown
                i = f2p((fv / pv) ** (1 / n) - 1)
             end if
             iq = p2f(i)
             if      (bitset_subsetp(var_pv, unknowns)) then ! a & pv are unknown
                pv = (1 + iq) ** (-n) * fv
             else if (bitset_subsetp(var_fv, unknowns)) then ! a & fv are unknown
                fv = pv * (1 + iq) ** n
             else if (bitset_subsetp(var_n, unknowns)) then  ! a & n are unknown
                n = -log(pv / fv) / log(1 + iq)
             end if
             a = -fv * iq / ((1 + iq) ** e - (1 + iq) ** (1 + n - d))
          else if (bitset_subsetp(var_n, unknowns)) then    ! a is known. n is unknown
             if (bitset_subsetp(var_i, unknowns)) then      ! a is known. n & i are unknown
                if      ((d==0) .and. (e==0)) then
                   i = f2p(-(fv - pv) * a / (a - pv) / fv)
                else if ((d==0) .and. (e==1)) then
                   i = f2p(-(fv - pv) * a / (a * fv - a * pv - fv * pv))
                else if ((d==1) .and. (e==0)) then
                   i = f2p(a * (fv - pv) / fv / pv)
                else if ((d==1) .and. (e==1)) then
                   i = f2p((fv - pv) * a / pv / (a + fv))
                else
                   call multi_bisection(i, islvivl0, islvivl1, sf_i_no_n, 1.0d-5, 1.0d-5, 1000, status, .false.)
                   if (status /= 0) then
                      status = 1099 ! "ERROR(tvm_delayed_level_annuity_solve): i solver failed for unknown n case!"
                      return
                   end if
                end if
             end if
             iq = p2f(i)
             if      (bitset_subsetp(var_pv, unknowns)) then      ! a is known. n & pv are unknown
                pv = (1 + iq) ** (1 - d) * a * fv / (a * (1 + iq) ** e + fv * iq)
             else if (bitset_subsetp(var_fv, unknowns)) then      ! a is known. n & pv are unknown
                fv = (1 + iq) ** e * a * pv / ((1 + iq) ** (1.0_rk - d) * a - iq * pv)
             end if
             n = (log(1 + iq) * d - log(1 + iq) + log((a * exp(log(1 + iq) * e) + fv * iq) / a)) / log(1 + iq)
          else
             if (bitset_subsetp(var_i, unknowns)) then          ! a & n are known. i is unknown
                if (bitset_subsetp(var_fv, unknowns)) then      ! a, n, & pv are known. i & fv are unknown  
                   call multi_bisection(i, islvivl0, islvivl1, sf_i_no_fv, 1.0d-7, 1.0d-7, 1000, status, .false.)
                   if (status /= 0) then
                      status = 1100 ! "ERROR(tvm_delayed_level_annuity_solve): i solver failed for unknown fv case!"
                      return
                   end if
                else if (bitset_subsetp(var_pv, unknowns)) then      ! a, n, & fv are known. i & pv are be unknown.
                   call multi_bisection(i, islvivl0, islvivl1, sf_i_no_pv, 1.0d-7, 1.0d-7, 1000, status, .false.)
                   if (status /= 0) then
                      status = 1101 ! "ERROR(tvm_delayed_level_annuity_solve): i solver failed for unknown pv case!"
                      return
                   end if
                else ! a, n, pv, & fv are known. i is unknown.
                   i = f2p((fv / pv) ** (1 / n) - 1)
                end if
             end if
             iq = p2f(i)
             if (bitset_subsetp(var_fv, unknowns)) then           ! a, i, & n are known. fv is unknown
                fv = 1 / iq * (-(1 + iq) ** e + (1 + iq) ** (1 + n - d)) * a
             end if
             if (bitset_subsetp(var_pv, unknowns)) then           ! a, i, & n are known. pv is unknown
                pv = (-(1 / (1 + iq)) ** (n - e + 1) * (1 + iq) / iq + (1 / (1 + iq)) ** d * (1 + iq) / iq) * a
             end if
          end if
       end if
       call tvm_delayed_level_annuity_check(n, i, pv, fv, a, d, e, status) ! Return status set
    end if
  contains
    real(kind=rk) function sf_i_no_n(i)
      implicit none
      real(kind=rk), intent(in) :: i
      real(kind=rk)             :: iq
      iq = p2f(i)
      sf_i_no_n = (a * (1 + iq) ** e + fv * iq) * pv - (1 + iq) ** (1 - d) * a * fv
    end function sf_i_no_n
    real(kind=rk) function sf_i_no_fv(i)
      implicit none
      real(kind=rk), intent(in) :: i
      real(kind=rk)             :: iq
      iq = p2f(i)
      sf_i_no_fv = (-(1 / (1 + iq)) ** (n - e + 1) * (1 + iq) / iq + (1 / (1 + iq)) ** d * (1 + iq) / iq) * a - pv
    end function sf_i_no_fv
    real(kind=rk) function sf_i_no_pv(i)
      implicit none
      real(kind=rk), intent(in) :: i
      real(kind=rk)             :: iq
      iq = p2f(i)
      sf_i_no_pv = 1 / iq * (-(1 + iq) ** e + (1 + iq) ** (1 + n - d)) * a - fv
    end function sf_i_no_pv
  end subroutine tvm_delayed_level_annuity_solve
  
  !----------------------------------------------------------------------------------------------------------------------------
  !> Check TVM parameters for a level annuity.
  !! @param n         Number of compounding periods
  !! @param i         Annual growth rate as a percentage.  When solving for this value only POSITIVE values are found.
  !! @param pv        Present Value
  !! @param fv        Future Value
  !! @param a         Payment (Annuity)
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 1065-1096.
  !!
  subroutine tvm_delayed_level_annuity_check(n, i, pv, fv, a, d, e, status)
    real(kind=rk),    intent(in)  :: n, i, pv, fv, a
    integer(kind=ik), intent(in)  :: d, e
    integer(kind=ik), intent(out) :: status
    real(kind=rk)                 :: iq
    if (d < 0) then
       status = 1065 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (d<0)!"
    else if (e < 0) then
       status = 1066 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (e<0)!"
    else if (.not. ieee_is_finite(n)) then
       status = 1067 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (n is infinite)!"
    else if (ieee_is_nan(n)) then
       status = 1068 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (n is NaN)!"
    else if (d > nint(n, ik)) then
       status = 1069 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (d>n)!"
    else if (e > nint(n, ik)) then
       status = 1070 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (e>n)!"
    else if ((d+e) > nint(n, ik)) then
       status = 1071 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (d+e>n)!"
    else if (n < zero_epsilon) then
       status = 1072 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (n<0)!"
    else if (n < 1) then
       status = 1073 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (n<1)!"
    else if (.not. ieee_is_finite(i)) then
       status = 1074 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (i is infinite)!"
    else if (ieee_is_nan(i)) then
       status = 1075 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (i is NaN)!"
    else if (abs(i) < zero_epsilon) then
       status = 1076 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (i is 0%)!"
    else if (abs(100+i) < zero_epsilon) then
       status = 1077 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (i is -100%)!"
    else if (.not. ieee_is_finite(pv)) then
       status = 1078 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (pv is infinite)!"
    else if (ieee_is_nan(pv)) then
       status = 1079 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (pv is NaN)!"
    else if (.not. ieee_is_finite(fv)) then
       status = 1080 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (fv is infinite)!"
    else if (ieee_is_nan(fv)) then
       status = 1081 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (fv is NaN)!"
    else if (.not. ieee_is_finite(a)) then
       status = 1082 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (a is infinite)!"
    else if (ieee_is_nan(a)) then
       status = 1083 ! "ERROR(tvm_delayed_level_annuity_check): Parameters inconsistent (a is NaN)!"
    else
       iq = p2f(i)
       if (abs(1 / iq * (-(1 + iq) ** e + (1 + iq) ** (1 + n - d)) * a - fv) > consistent_epsilon) then
          status = 1084 ! "ERROR(tvm_delayed_level_annuity_solve): Inconsistent parameters (fv equation)!"
       else if (abs((-(1 / (1 + iq)) ** (n - e + 1) * (1 + iq) / iq + (1 / (1 + iq)) ** d * (1 + iq) / iq) * a - pv) > consistent_epsilon) then
          status = 1085 ! "ERROR(tvm_delayed_level_annuity_solve): Inconsistent parameters (pv equation)!"
       else
          status = 0
       end if
    end if
  end subroutine tvm_delayed_level_annuity_check

  !----------------------------------------------------------------------------------------------------------------------------
  !> Solve for TVM parameters for a guaranteed geometric annuity.
  !!
  !! The equations this solver uses are as follows:
  !! @f[ \mathit{fv} =  a \frac{\left(1+g \right)^{1+n -e -d} \left(1+i \right)^{e}-\left(1+i \right)^{1+n -d}}{g -i}  @f]
  !! @f[ \mathit{pv} =  a \frac{\left(1+g \right) \left(\frac{1+g}{1+i}\right)^{n -e}-\left(\frac{1+g}{1+i}\right)^{d} \left(1+i \right)}{(g-i)(1+g)^{-d}} @f]
  !!
  !! This routine can solve for var_n, var_n+var_fv, and any combination of two or fewer of the following: var_a, var_fv, var_pv
  !!
  !! Use cases:
  !!  1: Q: In retirement bill needs $75,000 dollars per year adjusted for 3% inflation, he has $1,000,000 invested today, and he
  !!        expects to see a 5% return on investments.  How long before he runs out of money?
  !!     A: Set pv=1000000, fv=0, a=75000, i=5, g=3.  Solve for n.
  !!
  !! @param n         Number of compounding periods
  !! @param i         Discount rate as a percentage.
  !! @param g         Payment growth rate as a percentage.
  !! @param pv        Present Value
  !! @param fv        Future Value
  !! @param a         First payment (Annuity)
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param unknowns  What variables to solve for.  
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 1033-1064.
  !!
  subroutine tvm_delayed_geometric_annuity_solve(n, i, g, pv, fv, a, d, e, unknowns, status)
    real(kind=rk),    intent(inout) :: n, i, g, pv, fv, a
    integer(kind=ik), intent(in)    :: d, e
    integer(kind=ik), intent(in)    :: unknowns
    integer(kind=ik), intent(out)   :: status
    integer(kind=ik), parameter     :: allowed_vars = var_n + var_pv + var_fv + var_a
    integer                         :: num_unknowns
    real(kind=rk)                   :: iq, gq, iq1, gq1, giq
    num_unknowns = bitset_size(unknowns)
    if (num_unknowns > 2) then
       status = 1033 ! "ERROR(tvm_delayed_geometric_annuity_solve): Too many unknowns!"
    else if (bitset_not_subsetp(unknowns, allowed_vars)) then
       status = 1034 ! "ERROR(tvm_delayed_geometric_annuity_solve): Unknown unknowns!"
    else
       iq = p2f(i)
       gq = p2f(g)    
       iq1 = 1+iq
       gq1 = 1+gq
       giq = gq-iq
       if (num_unknowns > 0) then
          if (abs(i-g) < zero_epsilon) then ! i=g case          
             if (bitset_subsetp(var_a, unknowns)) then              ! a is unknown
                if      (bitset_subsetp(var_pv, unknowns)) then      ! a & pv are unknown
                   pv = fv * iq1 ** (-n)
                else if (bitset_subsetp(var_fv, unknowns)) then      ! a & fv are unknown
                   fv = pv * iq1 ** n
                else if (bitset_subsetp(var_n, unknowns)) then       ! a & gq are unknown
                   status = 1035 ! "ERROR(tvm_delayed_geometric_annuity_solve): Unsupported value for unknown (var_a+var_n)!"
                end if
                a = -fv * iq1 ** (d - n) / (d - n + e - 1)
             else if (bitset_subsetp(var_n, unknowns)) then              ! n is unknown
                if      (bitset_subsetp(var_fv, unknowns)) then      ! n & fv are unknown
                   fv = pv * iq1 ** ((iq1 ** d * pv + a * (-1 + d + e)) / a)
                ! else if (bitset_subsetp(var_i, unknowns)) then      ! n & i are unknown                   
                !    ! MJR TODO NOTE <2024-12-19T15:50:59-0600> tvm_delayed_geometric_annuity_solve: Add iterative solver for i here.
                else if (num_unknowns > 1) then                      ! n & something otherthan fv are unknown
                   status = 1036 ! "ERROR(tvm_delayed_geometric_annuity_solve): Unsupported value for unknown (only var_fv supported with var_n)!"
                end if
                n = ((iq1) ** (-d) * a * d + iq1 ** (-d) * a * e - a * iq1 ** (-d) + pv) / iq1 ** (-d) / a
             else ! pv and/or fv might be unknown
                if (bitset_subsetp(var_fv, unknowns)) then              ! fv unknown.  pv might be unknown
                   fv = a * iq1 ** (-d + n) * (-d + n - e + 1)
                end if
                if (bitset_subsetp(var_pv, unknowns)) then              ! pv unknown.  fv might be unknown
                   pv = (-d + n - e + 1) * iq1 ** (-d) * a
                end if
             end if
          else ! i!=g case
             if (bitset_subsetp(var_a, unknowns)) then              ! a is unknown
                if      (bitset_subsetp(var_pv, unknowns)) then      ! a & pv are unknown
                   pv = fv * iq1 ** d * gq1 ** e * (-(gq1) ** (n - e + 1) * iq1 ** (-n + e) + iq1 ** (1 - d) * gq1 ** d) / (iq1 ** n * iq1 * gq1 ** (d + e) - gq1 ** n * iq1 ** (d + e) * gq1)
                else if (bitset_subsetp(var_fv, unknowns)) then      ! a & fv are unknown
                   fv = (-(gq1) ** n * gq1 * iq1 ** (d + e + n) + gq1 ** (d + e) * iq1 ** (2 * n) * iq1) * pv / (iq1 ** n * iq1 * gq1 ** (d + e) - gq1 ** n * iq1 ** (d + e) * gq1)
                else if (bitset_subsetp(var_n, unknowns)) then       ! a & gq are unknown
                   status = 1037 ! "ERROR(tvm_delayed_geometric_annuity_solve): Unsupported value for unknown (var_a+var_n)!"
                end if
                a = fv * giq / (gq1 ** (-d + n - e + 1) * iq1 ** e - iq1 ** (1 + n - d))
             else if    (bitset_subsetp(var_n, unknowns)) then              ! n is unknown
                if      (bitset_subsetp(var_fv, unknowns)) then      ! n & fv are unknown
                   fv = 1 / giq * (gq1 ** (1 / (log(gq1) - log(iq1)) * (d * log(iq1) + log((a * iq1 * iq1 ** (-d) + giq * pv) / a) - log(iq1))) * iq1 ** e - iq1 ** ((log((a * iq1 * iq1 ** (-d) + giq * pv) / a) + (d - e - 1) * log(iq1) + e * log(gq1)) / (log(gq1) - log(iq1)))) * a
                else if (num_unknowns > 1) then                      ! n & something otherthan fv are unknown
                   status = 1038 ! "ERROR(tvm_delayed_geometric_annuity_solve): Unsupported value for unknown (only var_fv supported with var_n)!"
                end if
                n = ((log(gq1) - log(iq1)) * e + (d - 1) * log(gq1) + log(1 / a * (a * iq * iq1 ** (-d) + a * iq1 ** (-d) + pv * gq - pv * iq))) / (log(gq1) - log(iq1))
             else ! pv and/or fv might be unknown
                if (bitset_subsetp(var_fv, unknowns)) then              ! fv unknown.  pv might be unknown
                   fv = 1 / giq * (gq1 ** (-d + n - e + 1) * iq1 ** e - iq1 ** (1 + n - d)) * a
                end if
                if (bitset_subsetp(var_pv, unknowns)) then              ! pv unknown.  fv might be unknown
                   pv = (gq1 * (gq1 / iq1) ** (n - e) - (gq1 / iq1) ** d * iq1) * gq1 ** (-d) * a / giq
                end if
             end if
          end if
       end if
       call tvm_delayed_geometric_annuity_check(n, i, g, pv, fv, a, d, e, status)
    end if
  end subroutine tvm_delayed_geometric_annuity_solve

  !----------------------------------------------------------------------------------------------------------------------------
  !> Check TVM parameters for a guaranteed geometric annuity.
  !!
  !! @param n         Number of compounding periods
  !! @param i         Discount rate as a percentage.
  !! @param g         Payment growth rate as a percentage.
  !! @param pv        Present Value
  !! @param fv        Future Value
  !! @param a         First payment (Annuity)
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 1001-1032.
  !!
  subroutine tvm_delayed_geometric_annuity_check(n, i, g, pv, fv, a, d, e, status)
    real(kind=rk),    intent(in)    :: n, i, g, pv, fv, a
    integer(kind=ik), intent(in)    :: d, e
    integer(kind=ik), intent(out)   :: status
    real(kind=rk)                   :: iq, gq
    if (d < 0) then
       status = 1001 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (d<0)!"
    else if (e < 0) then
       status = 1002 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (e<0)!"
    else if (.not. ieee_is_finite(n)) then
       status = 1003 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (n is infinite)!"
    else if (ieee_is_nan(n)) then
       status = 1004 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (n is NaN)!"
    else if (d > nint(n, ik)) then
       status = 1005 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (d>n)!"
    else if (e > nint(n, ik)) then
       status = 1006 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (e>n)!"
    else if ((d+e) > nint(n, ik)) then
       status = 1007 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (d+e>n)!"
    else if (n < zero_epsilon) then
       status = 1008 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (n<0)!"
    else if (n < 1) then
       status = 1009 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (n<1)!"
    else if (.not. ieee_is_finite(i)) then
       status = 1010 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (i is infinite)!"
    else if (ieee_is_nan(i)) then
       status = 1011 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (i is NaN)!"
    else if (abs(i) < zero_epsilon) then
       status = 1012 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (i is 0%)!"
    else if (abs(100+i) < zero_epsilon) then
       status = 1013 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (i is -100%)!"
    else if (.not. ieee_is_finite(g)) then
       status = 1014 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (g is infinite)!"
    else if (ieee_is_nan(g)) then
       status = 1015 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (g is NaN)!"
    else if (abs(g) < zero_epsilon) then
       status = 1016 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (g is 0%)!"
    else if (abs(100+g) < zero_epsilon) then
       status = 1017 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (g is -100%)!"
    else if (.not. ieee_is_finite(pv)) then
       status = 1018 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (pv is infinite)!"
    else if (.not. ieee_is_finite(fv)) then
       status = 1019 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (fv is infinite)!"
    else if (ieee_is_nan(pv)) then
       status = 1020 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (pv is NaN)!"
    else if (ieee_is_nan(fv)) then
       status = 1021 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (fv is NaN)!"
    else if (.not. ieee_is_finite(a)) then
       status = 1022 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (a is infinite)!"
    else if (ieee_is_nan(a)) then
       status = 1023 ! "ERROR(tvm_delayed_geometric_annuity_check): Parameters inconsistent (a is NaN)!"
    else
       iq = p2f(i)
       gq = p2f(g)    
       if (abs(i-g) < zero_epsilon) then ! i=g case
          if (abs(a * (1 + iq) ** (-dble(d) + dble(n)) * (-d + n - e + 1) - fv) > consistent_epsilon) then
             status = 1024 ! "ERROR(tvm_delayed_geometric_annuity_check): Inconsistent parameters (i=g & fv equation)!"
          else if (abs((-d + n - e + 1) * (1 + iq) ** (-d) * a - pv) > consistent_epsilon) then
             status = 1025 ! "ERROR(tvm_delayed_geometric_annuity_check): Inconsistent parameters (i=g & pv equation)!"
          else
             status = 0
          end if
       else ! i!=g case
          if (abs(1 / (gq - iq) * ((1 + gq) ** (-d + n - e + 1) * (1 + iq) ** e - (1 + iq) ** (1 + n - d)) * a - fv) > consistent_epsilon) then
             status = 1026 ! "ERROR(tvm_delayed_geometric_annuity_check): Inconsistent parameters (i!=g & fv equation)!"
          else if (abs(((1 + gq) * ((1 + gq) / (1 + iq)) ** (n - e) - ((1 + gq) / (1 + iq)) ** d * (1 + iq)) * (1 + gq) ** (-d) * a / (gq - iq) - pv) > consistent_epsilon) then
             status = 1027 ! "ERROR(tvm_delayed_geometric_annuity_check): Inconsistent parameters (i!=g & pv equation)!"
          else
             status = 0
          end if
       end if
    end if
  end subroutine tvm_delayed_geometric_annuity_check

  !----------------------------------------------------------------------------------------------------------------------------
  !> Solve for TVM parameters for a guaranteed arithmetic annuity.
  !!
  !! The equations this solver uses are as follows:
  !! @f[ \mathit{fv} = \frac{\left(1+i \right)^{1+n -d} \left(a i +q \right)-\left(1+i \right)^{e} \left(\left(\left(1+n -e -d \right) q +a \right) i +q \right)}{i^{2}} @f]
  !! @f[ \mathit{pv} = \frac{\left(\left(\left(d -1-n +e \right) q -a \right) i -q \right) \left(\frac{1}{1+i}\right)^{n -e}+\left(a i +q \right) \left(1+i \right) \left(\frac{1}{1+i}\right)^{d}}{i^{2}} @f]
  !!
  !! This routine can solve one or two variables except for the following combinations:
  !!    var_n+var_i, var_n+var_q, var_n+var_pv, var_n+var_fv, var_i+var_pv, var_i+var_fv, var_q+var_a
  !!
  !! @param n         Number of compounding periods
  !! @param i         Discount rate as a percentage.
  !! @param q         Payment growth rate (added at each payment)
  !! @param pv        Present Value
  !! @param fv        Future Value
  !! @param a         First payment (Annuity)
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param unknowns  What variables to solve for.  
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 4129-4160.
  !!
  subroutine tvm_delayed_arithmetic_annuity_solve(n, i, q, pv, fv, a, d, e, unknowns, status)
    real(kind=rk),    intent(inout) :: n, i, q, pv, fv, a
    integer(kind=ik), intent(in)    :: d, e
    integer(kind=ik), intent(in)    :: unknowns
    integer(kind=ik), intent(out)   :: status
    integer(kind=ik)                :: cur_unk, lst_unk
    real(kind=rk)                   :: iq, iq1, epd, n1
    cur_unk = unknowns
    lst_unk = cur_unk
    iq  = p2f(i)
    iq1 = 1+iq
    epd = e + d
    n1 = n + 1
    do 
       if (bitset_subsetp(var_i, cur_unk) .and. bitset_not_intersectp(cur_unk, var_fv+var_pv+var_n)) then      ! U=i,  K=fv+pv+n ?=q+a
          iq  = (fv / pv) ** (1 / n) - 1
          i   = f2p(iq)
          iq1 = 1+iq
          cur_unk = bitset_minus(cur_unk, var_i)
       end if
       if (bitset_subsetp(var_n, cur_unk) .and. bitset_not_intersectp(cur_unk, var_fv+var_pv+var_i)) then      ! U=n,  K=fv+pv+i ?=q+a
          n = log(fv / pv) / log(iq1)
          n1 = n + 1
          cur_unk = bitset_minus(cur_unk, var_n)
       end if
       if (bitset_subsetp(var_pv, cur_unk) .and. bitset_not_intersectp(cur_unk, var_fv+var_i+var_n)) then      ! U=pv, K=fv+i+n ?=q+a
          pv = fv / iq1 ** n
          cur_unk = bitset_minus(cur_unk, var_pv)
       end if
       if (bitset_subsetp(var_fv, cur_unk) .and. bitset_not_intersectp(cur_unk, var_pv+var_i+var_n)) then      ! U=fv, K=pv+i+n ?=q+a
          fv = pv * iq1 ** n
          cur_unk = bitset_minus(cur_unk, var_fv)
       end if
       if (bitset_subsetp(var_a, cur_unk) .and. bitset_not_intersectp(cur_unk, var_i+var_n+var_q+var_fv)) then ! U=a,  K=i+n+q+fv ?=pv
          a = iq1 ** d * (iq1 ** (n1 - d) * q + q * (-1 + (epd - n1) * iq) * iq1 ** e - fv * iq ** 2) / iq / (iq1 ** epd - iq1 ** (n1))
          cur_unk = bitset_minus(cur_unk, var_a)
       end if
       if (bitset_subsetp(var_q, cur_unk) .and. bitset_not_intersectp(cur_unk, var_i+var_a+var_pv+var_n)) then ! U=q,  K=i+a+pv+n ?=fv
          q = iq * (a * iq1 ** epd + pv * iq * iq1 ** (d + n) - a * iq1 ** n * iq1) / ((-1 + (epd - n1) * iq) * iq1 ** epd + iq1 ** (n1))
          cur_unk = bitset_minus(cur_unk, var_q)
       end if
       if (bitset_subsetp(var_pv, cur_unk) .and. bitset_not_intersectp(cur_unk, var_n+var_q+var_a+var_i)) then ! U=pv, K=n+q+a+i ?=fv
          pv = ((((epd - n1) * q - a) * iq - q) * (1 / iq1) ** (n - e) + (a * iq + q) * iq1 * (1 / iq1) ** d) / iq ** 2
          cur_unk = bitset_minus(cur_unk, var_pv)
       end if
       if (bitset_subsetp(var_fv, cur_unk) .and. bitset_not_intersectp(cur_unk, var_n+var_q+var_a+var_i)) then ! U=fv, K=n+q+a+i ?=pv
          fv = (iq1 ** (n1 - d) * (a * iq + q) - iq1 ** e * (((n1 - e - d) * q + a) * iq + q)) / iq ** 2
          cur_unk = bitset_minus(cur_unk, var_fv)
       end if
       if (cur_unk == lst_unk) then
          exit
       end if
       lst_unk = cur_unk
    end do
    if (bitset_size(cur_unk) > 0) then
       status = 4129 ! "ERROR(tvm_delayed_arithmetic_annuity_solve): Unable to solve for some variables!"
    else
       call tvm_delayed_arithmetic_annuity_check(n, i, q, pv, fv, a, d, e, status)
    end if
  end subroutine tvm_delayed_arithmetic_annuity_solve

  !----------------------------------------------------------------------------------------------------------------------------
  !> Check TVM parameters for a guaranteed arithmatic annuity.
  !!
  !! @param n         Number of compounding periods
  !! @param i         Discount rate as a percentage.
  !! @param q         Payment growth rate (added at each payment)
  !! @param pv        Present Value
  !! @param fv        Future Value
  !! @param a         First payment (Annuity)
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 4097-4128.
  !!
  subroutine tvm_delayed_arithmetic_annuity_check(n, i, q, pv, fv, a, d, e, status)
    real(kind=rk),    intent(in)    :: n, i, q, pv, fv, a
    integer(kind=ik), intent(in)    :: d, e
    integer(kind=ik), intent(out)   :: status
    real(kind=rk)                   :: iq
    if (d < 0) then
       status = 4097 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (d<0)!"
    else if (e < 0) then
       status = 4098 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (e<0)!"
    else if (.not. ieee_is_finite(n)) then
       status = 4099 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (n is infinite)!"
    else if (ieee_is_nan(n)) then
       status = 4100 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (n is NaN)!"
    else if (d > nint(n, ik)) then
       status = 4101 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (d>n)!"
    else if (e > nint(n, ik)) then
       status = 4102 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (e>n)!"
    else if ((d+e) > nint(n, ik)) then
       status = 4103 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (d+e>n)!"
    else if (n < zero_epsilon) then
       status = 4104 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (n<0)!"
    else if (n < 1) then
       status = 4105 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (n<1)!"
    else if (.not. ieee_is_finite(i)) then
       status = 4106 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (i is infinite)!"
    else if (ieee_is_nan(i)) then
       status = 4107 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (i is NaN)!"
    else if (abs(i) < zero_epsilon) then
       status = 4108 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (i is 0%)!"
    else if (abs(100+i) < zero_epsilon) then
       status = 4109 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (i is -100%)!"
    else if (.not. ieee_is_finite(q)) then
       status = 4110 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (q is infinite)!"
    else if (ieee_is_nan(q)) then
       status = 4111 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (q is NaN)!"
    else if (abs(q) < zero_epsilon) then
       status = 4112 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (q is 0%)!"
    else if (.not. ieee_is_finite(pv)) then
       status = 4113 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (pv is infinite)!"
    else if (.not. ieee_is_finite(fv)) then
       status = 4114 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (fv is infinite)!"
    else if (ieee_is_nan(pv)) then
       status = 4115 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (pv is NaN)!"
    else if (ieee_is_nan(fv)) then
       status = 4116 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (fv is NaN)!"
    else if (.not. ieee_is_finite(a)) then
       status = 4117 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (a is infinite)!"
    else if (ieee_is_nan(a)) then
       status = 4118 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Parameters inconsistent (a is NaN)!"
    else
       iq = p2f(i)
       if (abs(((1 + iq) ** (1 + n - d) * (a * iq + q) - (1 + iq) ** e * (((1 + n - e - d) * q + a) * iq + q)) / iq ** 2 - fv) > consistent_epsilon) then
          status = 4119 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Inconsistent parameters (i!=g & fv equation)!"
       else if (abs(((((d - 1 - n + e) * q - a) * iq - q) * (1 / (1 + iq)) ** (n - e) + (a * iq + q) * (1 + iq) * (1 / (1 + iq)) ** d) / iq ** 2 - pv) > consistent_epsilon) then
          status = 4120 ! "ERROR(tvm_delayed_arithmetic_annuity_check): Inconsistent parameters (i!=g & pv equation)!"
       else
          status = 0
       end if
    end if
  end subroutine tvm_delayed_arithmetic_annuity_check

end module mrffl_tvm


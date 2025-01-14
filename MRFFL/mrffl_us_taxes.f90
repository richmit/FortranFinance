! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_us_taxes.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-21
!! @brief     Compute Taxes for USA.@EOL
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
!------------------------------------------------------------------------------------------------------------------------------
!> Compute Taxes for USA.
!!
module mrffl_us_taxes
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik, zero_epsilon
  use mrffl_percentages, only: percentage_of, percentage_of_total
  use mrffl_tvm, only: fv_from_pv_n_i
  implicit none  
  private                  

  integer(kind=ik), parameter, public :: seed_tax_year                     = 2024 ! The year the following constants hold
  real(kind=rk), parameter, public    :: std_tax_deduction_single          = 14600
  real(kind=rk), parameter, public    :: std_tax_deduction_joint           = 29200
  real(kind=rk), parameter, public    :: std_tax_deduction_separately      = 14600
  real(kind=rk), parameter, public    :: std_tax_deduction_head            = 21900
  real(kind=rk), parameter, public    :: tax_bracket_rates(7)              = [   10,    12,     22,     24,     32,     35,     37]
  real(kind=rk), parameter, public    :: tax_bracket_breaks_single(7)      = [11600, 47150, 100525, 191950, 243725, 609350, 609351]
  real(kind=rk), parameter, public    :: tax_bracket_breaks_joint(7)       = [23200, 94300, 201050, 383900, 487450, 731200, 731201]
  real(kind=rk), parameter, public    :: tax_bracket_breaks_head(7)        = [16550, 63100, 100500, 191950, 243700, 609350, 609350]
  real(kind=rk), parameter, public    :: tax_bracket_breaks_separately(7)  = [11600, 47150, 100525, 191950, 243725, 365600, 365601]

      ! 2024 US Tax Brackets
      ! 
      ! | Tax rate | Single filer         | Married filing jointly | Married filing separately | Head of household    |
      ! |----------+----------------------+------------------------+---------------------------+----------------------|
      ! |      10% |       $0 to $11,600  |       $0 to  $23,200   |       $0 to  $11,600      |       $0 to  $16,550 |
      ! |      12% |  $11,601 to $47,150  |  $23,201 to  $94,300   |  $11,601 to  $47,150      |  $16,551 to  $63,100 |
      ! |      22% |  $47,151 to $100,525 |  $94,301 to $201,050   |  $47,151 to $100,525      |  $63,101 to $100,500 |
      ! |      24% | $100,526 to $191,950 | $201,051 to $383,900   | $100,526 to $191,950      | $100,501 to $191,950 |
      ! |      32% | $191,951 to $243,725 | $383,901 to $487,450   | $191,951 to $243,725      | $191,951 to $243,700 |
      ! |      35% | $243,726 to $609,350 | $487,451 to $731,200   | $243,726 to $365,600      | $243,701 to $609,350 |
      ! |      37% | $609,351 or more     | $731,201 or more       | $365,601 or more          | $609,350 or more     |
      ! 

  public :: tax, projected_tax, max_bracket, effective_tax_rate

contains

  !------------------------------------------------------------------------------------------------------------------------------
  !> Find the bracket val belongs in given an array of bracket end points.
  !! 
  !! brackets is an array consisting of bracket endpoints @f$ [ e_1, e_2, ..., e_{n-1}, e_{n} ] @f$
  !! such that @f$ e_1 < e_2 < ... < e_{n-1} < e_n @f$.
  !!
  !! The endpoints define a set of intervals:  
  !!                @f$ (-\infty, e1], (e_1, e_2], ..., (e_{n-2}, e_{n-1}], (e_{n-1}, e_n), [e_n, \infity) @f$
  !! Note the second to last interval is open -- the rest are half open.
  !! We enumerate these intervals from 0 up to n.  
  !!
  !! This function returns the index for the interval containing val.
  !! 
  integer(kind=ik) function max_bracket(val, brackets)
    real(kind=rk), intent(in) :: val
    real(kind=rk), intent(in) :: brackets(:)         
    integer(kind=ik)          :: i
    if (val <= brackets(1)) then
       max_bracket = 1_ik
    else if (val >= brackets(size(brackets))) then
       max_bracket = size(brackets, kind=ik)
    else
       max_bracket = 0_ik
       do i=size(brackets, kind=ik)-1_ik,1_ik,-1_ik
          if (val > brackets(i)) then
             max_bracket = i+1_ik
             exit
          end if
       end do
    end if
  end function max_bracket

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute tax given value, bracket breaks, and bracket rates.
  !! 
  !! see max_bracket() for a description of breaks.  The rates array are the tax rates for each bracket.
  !! 
  real(kind=rk) function tax(val, breaks, rates)
    real(kind=rk),    intent(in) :: val
    real(kind=rk),    intent(in) :: breaks(:), rates(:)         
    real(kind=rk)                :: val_in_bracket, last_break, cur_break
    integer                      :: idx
    if (val < zero_epsilon) then
       tax = 0
    else
       last_break = 0
       tax  = 0
       do idx=1,size(rates)-1
          cur_break = breaks(idx)
          val_in_bracket = min(val, cur_break) - last_break
          last_break = cur_break
          if (val_in_bracket > 0) then
             tax = tax + percentage_of(val_in_bracket, rates(idx))
          end if
       end do
       if (val >= breaks(size(breaks))) then
          tax = tax + percentage_of(1 + val - breaks(size(breaks)), rates(idx))
       end if
    end if
  end function tax

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute effective tax rate given value, bracket breaks, and bracket rates.
  !! 
  !! see max_bracket() for a description of breaks.  The rates array are the tax rates for each bracket.
  !! 
  real(kind=rk) function effective_tax_rate(val, breaks, rates)
    real(kind=rk),    intent(in) :: val
    real(kind=rk),    intent(in) :: breaks(:), rates(:)         
    if (val < zero_epsilon) then
       effective_tax_rate = 0
    else
       effective_tax_rate = percentage_of_total(val, tax(val, breaks, rates))
    end if
  end function effective_tax_rate

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute projected effective tax rate with tax() by adjusting the breaks into the future for inflation.
  !!
  !! If year is in the past with respect to our tax information, then -1 is returned.
  !!
  real(kind=rk) function projected_tax(val, breaks, rates, year, inflation)
    real(kind=rk),    intent(in) :: val, inflation
    integer(kind=ik), intent(in) :: year
    real(kind=rk),    intent(in) :: breaks(:), rates(:)         
    if (year < seed_tax_year) then
       projected_tax = -1
    else if (year < seed_tax_year) then
       projected_tax = tax(val, breaks, rates)
    else
       projected_tax = tax(val, fv_from_pv_n_i(breaks, year-seed_tax_year, inflation), rates)
    end if
  end function projected_tax

end module mrffl_us_taxes
